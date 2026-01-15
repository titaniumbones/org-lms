;; [[file:org-lms.org::*Package intro][Package intro:1]]
;;; org-lms -- Summary
;;;
;;; Commentary:
;;; Library to facilitate marking assignments and interacting
;;; with the Canvas LMS (https://canvas.instructure.com/) via its
;;; JSON API (https://canvas.instructure.com/doc/api/).
;;;
;;; Functionality is still rough and design is idiosyncratic. I hope to
;;; one day design a more robusti nterface but... who know? 

;;; Code:
;; Package intro:1 ends here

;; [[file:org-lms.org::*Dependencies][Dependencies:1]]
;; require the dependencies
(require 'org) ;; the source of all good!
(require 'org-attach) ;; for attaching files to emails
(require 'cl-lib) ;; may not be necessary anymore in newer Emacsen
(require 'org-mime) ;; Unfortunately I require this somewhat outdated library for mailing
(require 'dash) ;; modern syntax
(require 'ts) ;; easy time manipulation
(require 'oc) ;; citations
(require 'oc-csl) ;; csl citaiont processor
(require 'citeproc) ;; citeproc dependency
(require 'ox-canvashtml) ;; new canvas html processor (experimental)
;; (require 's) ;; modern strings
;; (require 'org-ql) ;; faster, easier query syntax
;;(require 'ov) ;; for grade overlays
;; Dependencies:1 ends here

;; [[file:org-lms.org::*Define obsolete functions][Define obsolete functions:1]]
(define-obsolete-function-alias 'org-lms-send-subtree-with-attachments
    'org-lms--send-subtree-with-attachments "a pretty long time ago")
(define-obsolete-function-alias 'org-lms-mail-all-undone 
    'org-lms-mail-all "a pretty long time ago")
(define-obsolete-function-alias 'org-lms-parse-assignment 
    'org-lms-post-assignment "2021-06-20" "calling this`parse` was misleading")
;; Define obsolete functions:1 ends here

;; [[file:org-lms.org::*basic variables that most users will need to set][basic variables that most users will need to set:1]]
;; variables
;; most of these are used for canvas interactions...

(defvar org-lms-courses nil
  "Alist in which each car is a symbol, and each cdr is a plist.

  Value of this variable must be set beforeusing the library. The
  plist should include at least the following attributes in order
  to match the local definition with the courses on canvas:

  - `:coursnum' 
  - `:name'
  - `:semester'
  ")

(defcustom org-lms-baseurl nil
  "Baseurl for canvas API queries. 
    Should have the form \"https://canvas.instance.at.school/api/v1/\"."
  :type '(string)
  )

(defcustom org-lms-public-baseurl nil
  "Baseurl for canvas API queries. 
    Should have the form \"https://canvas.instance.at.school/api/v1/\"."
  :type '(string)
  )

(defcustom org-lms-token nil
  "Secret oauth token for Canvas. DO NOT SHARE THIS INFO.
    Probably customize is a rotten place to put this!"
  :type '(string))

(defvar-local org-lms-course nil
  "Locally-set variable representing the local course.")

(defvar-local org-lms-local-assignments nil
  "List of assignments for the current course. 

    Intended to be updated automatically somehow, but for now just
    being set in grading page")

(defvar-local org-lms-merged-assignments nil
  "Buffer-local plist of assignments in this course, merging cnavas and local info. 

    Intended to be set automatically. Should always be buffer-local")

(defvar-local org-lms-local-students nil
  "Buffer-local plist of students in this course, using local csv file. 

    Intended to be set automatically. Should always be buffer-local")

(defvar-local org-lms-merged-students nil
  "Buffer-local plist of students in this course, merging cnavas and local info. 

    Intended to be set automatically. Should always be buffer-local")
(defcustom ol-make-headings-final-hook nil
  "list of functions to run just after a heading has been created"
  :safe t)

(defcustom org-lms-citeproc-doi-prefix
  "https://dx.doi.org/"
  "Local DOI resolver for student bibliography links")

;; in the syllabus, "citations" are actually full bibliography entries.
;; therefore, don't wrap them in links that will create useless HTML
;; and probably also don't create a full biblipgraphy. 
(defcustom org-lms-citeproc-fmt-alist
  ;; oops, requires dash library!!
  (and (boundp 'citeproc-fmt--html-alist)
       (--map-when (eq  (car it) 'cited-item-no)
                   '(cited-item-no
                     . ;; (lambda (x y) (concat "<a href=\"#slide-bibliography\">" x "</a>"))
                     (lambda (x y) x))
                   citeproc-fmt--html-alist))
  "Alist matching CSL properties to lambda functions that wrap the property values
  in HTML tags; or nil, if CITEPROC-FMT--HTML-ALIST is not defined."
  :group 'org-export-re-reveal
  :type '(alist :key-type symbol :value-type function ))

(defvar org-lms-attach-ignore "^\\.pdf-view-restore$\\|^\\.~lock\\.\\|\\.zip$" 
  "regexp of file names to ignore.  a bit clumsy but hey.")

(defvar org-lms-grading-table
  '(("A+" . 92)
    ("A" . 87)
    ("A-" . 83)
    ("B+" . 78)
    ("B" . 75)
    ("B-" . 72)
    ("C+" . 68)
    ("C" . 65)
    ("C-" . 62)
    ("D+" . 58)
    ("D" . 55)
    ("D-" . 52))
  "Quick lookup table for marks so that they get properly translated to canvas. Should it be a hash table? Who cares?")
;; basic variables that most users will need to set:1 ends here

;; [[file:org-lms.org::*Variables used internally to keep code (sort of) clean][Variables used internally to keep code (sort of) clean:1]]

;; Variables used internally to keep code (sort of) clean:1 ends here

;; [[file:org-lms.org::*Read global values of org file][Read global values of org file:1]]
(defun org-lms-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun org-lms-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-lms-global-props key))))
;; Read global values of org file:1 ends here

;; [[file:org-lms.org::*Reading keywords in org files][Reading keywords in org files:1]]
;; john kitchin's version
;; (defun org-lms-get-keyword (key &optional buffer)

;;   (org-element-map (org-element-parse-buffer) 'keyword
;;     (lambda (k)
;;       (when (string= key (org-element-property :key k))
;;         (org-element-property :value k))) 
;;     nil t))


(defun org-lms-get-keyword (key &optional file)
  (save-excursion
    (let ((result nil)
          (buf (current-buffer))
          )
      
      (if file 
          (setq buf (find-file-noselect file)))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (let ((setup (org-element-map
                           (org-element-parse-buffer)
                           'keyword
                         (lambda (k)
                           (when (string= "SETUPFILE" (org-element-property :key k))
                             (org-element-property :value k)))
                         nil t)))
            (setq result
                  (or
                   (org-element-map (org-element-parse-buffer) 'keyword
                     (lambda (k)
                       (when (string= key (org-element-property :key k))
                         (setq result  (org-element-property :value k)))
                       result) 
                     nil t)
                   (and setup
                        (org-lms-get-keyword key setup ))
                   ))))))))

;; nicolas g's version
;; (defun org-lms-get-keyword (key)
;;   "Get value of keyword, whether or not it's been defined by org. 

;; Look for a keyword statement of the form 
;; #+KEYWORD: 

;; and return either the last-declared value of the keyword, or the
;; value of the current headline's property of the same name."

;;   (let ((case-fold-search t)
;;         (regexp (format "^[ \t]*#\\+%s:" key))
;;         (result nil))
;;     (org-with-point-at 1
;;       (while (re-search-forward regexp nil t)
;;         (let ((element (org-element-at-point)))
;;           (when (eq 'keyword (org-element-type element))
;;             (push (org-element-property :value element) result)))))
;;     (or (org-entry-get nil key) (car result)))
;;   )



(defun org-lms-set-keyword (tag value)
  "Set filetag TAG to VALUE.
        If VALUE is nil, remove the filetag."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "#\\+%s:" tag) (point-max) 'end)
        ;; replace existing filetag
        (progn
          (beginning-of-line)
          (kill-line)
          (when value
            (insert (format "#+%s: %s" tag value))))
      (goto-char (point-min))
      ;; add new filetag
      (if (looking-at "^$") 		;empty line
          ;; at beginning of line
          (when value
            (insert (format "#+%s: %s\n" tag value)))
        ;; at end of some line, so add a new line
        (when value
          (insert (format "#+%s: %s\n" tag value)))))))
;; Reading keywords in org files:1 ends here

;; [[file:org-lms.org::*CSV Parsers][CSV Parsers:1]]
;; Helper Functions

;; I'm using hte namespace `org-lms~' for these internal helper functions.
;; At some liater date should figure out and implement approved best
;; oractices. 

;; CSV Parsers
;; Student information (name, email, etc) is exported from excel or blackboard in the form
;; of a CSV file.  These two functions parse such files

(defun org-lms~parse-csv-file (file)
  "Transforms FILE into a list.
 Each element of the returned value is itself a list
containing all the elements from one line of the file.
This fn was stolen from somewhere on the web, and assumes
that the file ocntains no header line at the beginning"
  (interactive
   (list (read-file-name "CSV file: ")))
  (let ((buf (find-file-noselect file))
        (result nil))
    (with-current-buffer buf
      (goto-char (point-min))
      ;; (let ((header (buffer-substring-no-properties
      ;;              (line-beginning-position) (line-end-position))))
      ;;   (push ))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          ;; (let templist (split-string line ",")
          ;;      ;;(print templist)
          ;;      ;; (push (cons (car templist) (nth 1 templist) ) result)
          ;;      )
          (push (cons (nth 0 (split-string line ",")) (nth 1 (split-string line ","))) result)
          )
        (forward-line 1)))
    (reverse result)))

(defun org-lms~parse-plist-symbol-csv-file (file)
  "Transforms csv FILE into a list of plists.
Like `parse-csv-file' but each line of the original file is
turned into a plist. Returns a list of plists. Column header
strings are transformed into downcased single-word keys, e.g.
\"First Name\" becomes \":firstname\". Assumes that the first
line of the csv file is a header containing field names. Clumsily
coded, but works."
  (interactive
   (list (read-file-name "CSV file: ")))
  (message "here i am w/ %s" file)
  (let (;; (buf (find-file-noselect file))
        (result nil))
    (with-temp-buffer
      (if (file-exists-p (expand-file-name file)) (insert-file-contents (expand-file-name file)))
      (goto-char (point-min))
      (let ((header-props
             (split-string  (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)) ","))
            )
       (message "CSV PARSER: headerprops ;; %s" (buffer-string))
        (while (not (eobp))
          (let ((line  (split-string (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position)) ","))
                (count 0)
                (new-plist '()))
            (while (< count (length line))
              (message "here in loop w count %s of " count (length line))
              (setq new-plist (plist-put new-plist
                                         (intern (concat ":"
                                                         (downcase
                                                          (replace-regexp-in-string "\"" ""
                                                                                    (replace-regexp-in-string
                                                                                     "[[:space:]]" ""
                                                                                     (nth count header-props))))))
                                         (if (not (equal (nth count line) "false"))
                                             (replace-regexp-in-string "\"" "" 
                                                                       (nth count line))
                                           "")))
              (setq count (1+ count)))
            (push  new-plist result)
            (forward-line 1))))
      ;; (message "PARSER: result -- %s" result)
      (cdr (reverse result)))))
(defun org-lms~parse-plist-csv-file (file)
  "Transforms csv FILE into a list of plists.
Like `parse-csv-file' but each line of the original file is turned 
into a plist.  Returns a list of plists. Assumes that the first line
of the csv file is a header containing field names.  Clumsily coded, 
but works."
  (interactive
   (list (read-file-name "CSV file: ")))
  (let ((buf (find-file-noselect file))
        (result nil))
    (with-current-buffer buf
      (goto-char (point-min))
      (let ((header-props
             (split-string  (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)) ","))
            )
        ;; (message "CSV PARSER: headerprops ;; %s" header-props)
        (while (not (eobp))
          (let ((line  (split-string (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position)) ","))
                (count 0)
                (new-plist '()))
            (while (< count (length line))
              (setq new-plist (plist-put new-plist
                                         (intern
                                          (replace-regexp-in-string "\"" ""
                                                                    (replace-regexp-in-string
                                                                     "[[:space:]]" ""
                                                                     (nth count header-props))))
                                         (if (not (equal (nth count line) "false"))
                                             (replace-regexp-in-string "\"" "" 
                                                                       (nth count line))
                                           "")))
              (setq count (1+ count)))
            (push  new-plist result)
            (forward-line 1))))
      ;; (message "PARSER: result -- %s" result)
      (cdr (reverse result)))))
;; CSV Parsers:1 ends here

;; [[file:org-lms.org::*Miscellaneous Helper functions][Miscellaneous Helper functions:1]]
;; Element tree navigation
;; not sure but I don't think I use this anymore
;; also trying to avoid relying on parental properties
;; remove in future
(defun org-lms~get-parent-headline ()
  "Acquire the parent headline & return. Used by`org-lms-make-headlines' and `org-lms-attach'"
  (save-excursion
    (org-up-heading-safe)
    (nth 4 (org-heading-components))
    ;;(org-mark-subtree)
    ;;(re-search-backward  "^\\* ")
    ;;(nth 4 (org-heading-components))
    ))
(defun org-lms-safe-pget (list prop)

  (if (plist-get list prop)
       
      (plist-get list prop)
    ""))

(defun oln2s (num)
  (cond
   ((numberp num)
    (number-to-string num))
   ((stringp num )
    num)
   (num
    (format "%s" num))
   (t
    "")))

;;copied and modified from https://github.com/jorendorff/dotfiles/blob/master/.emacs
;; should be replaced by emacs-kv
(defun org-lms-plist-to-alist (ls)
  "Convert a plist to an alist. Primarily for old color-theme themes."
  (let ((result nil))
    (while ls
      (add-to-list 'result (cons (intern (substring  (symbol-name (car ls)) 1 )) (cadr ls)))
      (setq ls (cddr ls)))
    result))
;; Miscellaneous Helper functions:1 ends here

;; [[file:org-lms.org::*JSON helpers and wrappers][JSON helpers and wrappers:1]]
;; number-to-string was driving me crazy 


(defmacro ol-jsonwrapper (fn &rest args)
  "Run FN with ARGS, but first set `json.el' vars to `org-lms' defaults.
Allows org-lms functions to easily parse json consistently. The org-lms
default values are:
`json-array-type': 'list
`json-object-type': 'plist
`json-false': nil
`json-key-type': 'keyword"
  
  `(let ((json-array-type 'list)
         (json-object-type 'plist)
         (json-key-type 'keyword)
         (json-false nil)
         (json-encoding-pretty-print nil))
     (,fn ,@args)
     )

  )

(defun ol-write-json-plists (metalist)
  "Work around json bug with lists of plists (METALIST)."
  (ol-jsonwrapper 
   (lambda ()
     (let ((result "["))
       (cl-loop for s in metalist
                do
                (setq result (concat result
                                     (json-encode-plist s) "," )))
       (concat result "]")))
   )
  )

;; this isn't necessary actually!
(defun ol-write-json-alists (metalist)
  "Work around json bug with lists of plists (METALIST)."
  (ol-jsonwrapper 
   (lambda ()
     (let ((result "["))
       (cl-loop for s in metalist
                do
                (setq result (concat result
                                     (json-encode-alist s) "," )))
       (concat result "]")))
   )
  )
;; JSON helpers and wrappers:1 ends here

;; [[file:org-lms.org::*Read-lines: Belongs up with the utility functions][Read-lines: Belongs up with the utility functions:1]]
;; stolen from xah, http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun org-lms~read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))
;; Read-lines: Belongs up with the utility functions:1 ends here

;; [[file:org-lms.org::lms-process][lms-process]]
(defun org-lms-process-props () 
"retrieve all properties in a headline, then downcase and standardize the key names so that they are convenient to use with `let-alist`"
(cl-loop for (key . value) in (org-entry-properties)
         collect
         (cons (intern
                (replace-regexp-in-string
                 "^org_lms_" "ol_"
                 (downcase key)))
               (if (string= "nil" value)
                   nil
                 value ))))

;; i don't think this is used? b it's cool 
(defun org-lms-propertize-response-data (response-data)
   "write a variable value to a headline property. MUNGED-VAR is a dot-variable set by `let-alist`, 
which see for more details"
   (let ((propDictionary
          '((:id .  "CANVASID")
            (:published . "OL_PUBLISH")
            (:html_url . "CANVAS_HTML_URL")
            (:submission_url . "CANVAS_SUBMISSION_URL")
            (:submissions_download_url . "SUBMISSIONS_DOWNLOAD_URL:")
            (:grading_standard_id . "GRADING_STANDARD_ID")
            (:submission_types . "CANVAS_SUBMISSION_TYPES")
            (:grading_type . "GRADING_TYPE"))))
     (cl-loop for (k . v) in propDictionary
              do
              (if (plist-get response-data k)
                  (progn
                    (message "yup, got prop %s" k)
                    (org-set-property v (format "%s" (plist-get response-data k))))
                (message "nope, no prop %s" k))
              ;; collect
              ;; `(,k . ,(plist-get response-data k))
              )
            
   ))

(defun org-lms-generic-set-properties (data isgroup)
  "sets prperties for all groups "
                                
  )
;; lms-process ends here

;; [[file:org-lms.org::*Deal with timestamps][Deal with timestamps:1]]
(require 'ts)
(defun o-l-date-to-timestamp (date)
  "use ts.el date parse functions return an ISO-compatible
timestamp for transmission to Canvas via API. DATE is a string,
usually of the form `2019-09-26`, but optionally including a full time."

  (ts-format "%Y-%m-%dT%H:%M:%S%:z" (ts-parse-fill 'end date )))
;; Deal with timestamps:1 ends here

;; [[file:org-lms.org::*Generic get-valid-subtree function.][Generic get-valid-subtree function.:1]]
(defun org-lms--get-valid-subtree ()
  "Return the Org element for a valid Hugo post subtree.
The condition to check validity is that the EXPORT_FILE_NAME
property is defined for the subtree element.
As this function is intended to be called inside a valid Hugo
post subtree, doing so also moves the point to the beginning of
the heading of that subtree.
Return nil if a valid Hugo post subtree is not found.  The point
will be moved in this case too."
  (catch 'break
    (while :infinite
      (let* ((entry (org-element-at-point))
             (fname (org-string-nw-p (org-element-property :EXPORT_FILE_NAME entry)))
             level)
        (when fname
          (throw 'break entry))
        ;; Keep on jumping to the parent heading if the current
        ;; entry does not have an EXPORT_FILE_NAME property.
        (setq level (org-up-heading-safe))
        ;; If no more parent heading exists, break out of the loop
        ;; and return nil
        (unless level
          (throw 'break nil))))))
;; Generic get-valid-subtree function.:1 ends here

;; [[file:org-lms.org::*Filter attachments][Filter attachments:1]]
(defun org-lms-attach-file-list ()
  "get the filtered attachments list" 
  (seq-filter (lambda (a)
                (not (string-match org-lms-attach-ignore a)))
              (org-attach-file-list (org-attach-dir t))))
;; Filter attachments:1 ends here

;; [[file:org-lms.org::*Letter Grade to numerical value][Letter Grade to numerical value:1]]
(defun org-lms-calculate-grade (initial)
  "Infer canvas-style grade from numerical, percentage, or letter grade."
  (let* ((weight (or
                 (org-entry-get (point) "ASSIGNMENT_WEIGHT" t)
                 "0.1"))
        (trimmed (s-trim initial))
        (matched (or
                  (and (stringp trimmed)  (string=  trimmed "pass") "pass")
                  (map-elt org-lms-grading-table initial nil 'equal)
                  (and (string= (s-right 1 trimmed) "%")
                       (s-numeric (s-left -1 trimmed))
                       (string-to-number (s-left -1 trimmed)))
                  (string-to-number trimmed)
                  0)))
    (if  (and (stringp matched) (string= matched "pass"))
        matched
      (string-to-number (format "%0.2f" 
                                (* matched (string-to-number weight)))))))
;; Letter Grade to numerical value:1 ends here

;; [[file:org-lms.org::*Expand templates in titles][Expand templates in titles:1]]
(defun org-lms-quick-template-expand (unparsed)
(if (string-match "\\(^.*\\){{{\\([-A-Za-z0-9_]+\\)}}}\\(.*\\)$"  
                  unparsed)
    (let* ((key (downcase (match-string 2 unparsed)))
           (prefix (match-string-no-properties 1 unparsed))
           (suffix (match-string-no-properties 3 unparsed))
           (value (match-string-no-properties 0 unparsed))
           ;; (args (pcase (match-string-no-properties 3)
	   ;;         (`nil nil)
	   ;;         (a (org-macro-extract-arguments
	   ;;     	(replace-regexp-in-string
	   ;;     	 "[ \t\r\n]+" " " (org-trim a))))))
           (m (list 'macro (list :key key
		                 :value value
		                 :args nil
		                 :begin 0
		                 :end 10
		                 ;;:post-blank post-blank
                                 ))))
      (concat prefix 
              (org-macro-expand m org-macro-templates)
              suffix))
  unparsed))
;; Expand templates in titles:1 ends here

;; [[file:org-lms.org::*Basic "request" function][Basic "request" function:1]]
;; talking to canvas via API v1: https://canvas.instructure.com/doc/api/ 

(defun org-lms-canvas-request (query &optional request-type request-params file)
  "Send QUERY to `org-lms-baseurl' with http request type REQUEST-TYPE.
  Optionally send REQUEST-PARAMS as JSON data, and write results to FILE, which should be a full path.  

  Returns a user-error if `org-lms-token' is unset, or if data payload is nil. Otherwise return a parsed json data payload, with the following settings wrapping `json-read':

    `json-array-type' 'list
    `json-object-type' 'plist
    `json-key-type' 'symbol
    maybe key-type needs to be keyword though! Still a work in progress.
    "
  (message "LISP PARAMS: %s" request-params)
  (unless request-type (setq request-type "GET"))
  (let ((canvas-payload nil)
        (canvas-err nil)
        (canvas-status nil)
        (json-params (json-encode request-params))
        (target (concat org-lms-baseurl query))
        ;;(request-backend 'url-retrieve)
        ;;(request-coding-system 'no-conversion)
        )
    (message (concat target "   " request-type))
    ;; (message "%s" `(("Authorization" . ,(concat "Bearer " org-lms-token))))
    (message "PARAMS: %s" json-params)
    (if org-lms-token
        (progn (setq thisrequest
                     (request
                      target
                      
                      :type request-type
                      :headers `(("Authorization" . ,(concat "Bearer " org-lms-token))
                                 ("Content-Type" . "application/json")
                                 )
                      :sync t
                      ;;:data   (if  json-params (encode-coding-string json-params 'utf-8)  nil) ;; (or data nil)
                      :data   (if  json-params json-params  nil)
                      ;;:encoding 'no-conversion
                      :encoding 'utf-8
                      :parser (lambda ()
                                (if (and (boundp 'file) file) (write-region (buffer-string) nil file))
                                (ol-jsonwrapper json-read))
                      :success (cl-function
                                (lambda (&key data &allow-other-keys)
                                  (message "SUCCESS: %S" data)
                                  ;;(message "SUCCESS!!")
                                  (setq canvas-payload data)
                                  canvas-payload
                                  ))
                      :error (cl-function (lambda ( &key error-thrown data status &allow-other-keys )
                                            (setq canvas-err error-thrown)
                                            (message "ERROR: %s" error-thrown)))))
               (unless (request-response-data thisrequest)                                   
                 (message (format "NO PAYLOAD: %s" canvas-err)) )
               (or (request-response-data thisrequest) thisrequest) )
      (user-error "Please set a value for for `org-lms-token' in order to complete API calls"))))
;; Basic "request" function:1 ends here

;; [[file:org-lms.org::*Getters][Getters:1]]
(defun org-lms-get-courseids (&optional file)
    "Get list of JSON courses and produce a simplified list with just ids and names, for convenience.
  Optionally write JSON output to FILE."
    (let ((result (org-lms-get-courses file)))
      (cl-loop for course in result
               collect
               `(,(plist-get course :id) ,(format "#+ORG_LMS_COURSEID: %s" (plist-get course :id)) ,(plist-get course :name) ))))

  (defun org-lms-get-courses (&optional file)
    "Get full list of JSON courses, optionally writing to FILE."
    (org-lms-canvas-request "courses" "GET" `(("include" . "term")) (if file (expand-file-name file))))

  (defun org-lms-get-single-course (&optional courseid file)
    "Get the current Canvas JSON object representing the coures with id COURSEID."
(setq courseid (or courseid
                       (org-lms-get-keyword "ORG_LMS_COURSEID")
                       (plist-get org-lms-course)))
    (org-lms-canvas-request (format "courses/%s" courseid) "GET" nil file))

  (defun org-lms-infer-course (&optional course recordp)
    "Attempt to infer Canvas ID of a local COURSE and return that object.
    \(using the information we already have.\)
    Optionally RECORDP the keyword.
    But RECORDP isn't actually implemented yet and for some reason 
    this fn returns a course object not a ocursid!"
    (unless course
      (setq course org-lms-course))

    (let ((canvas-courses (org-lms-get-courses))
          (coursenum (plist-get course :coursenum))
          (shortname (plist-get course :shortname))
          (semester (plist-get course :semester))
          (result nil)
          )
      (cl-loop for can in-ref canvas-courses
            do
            ;;(prin1 can)
            (let ((course-code (plist-get can :sis_course_id)))
              ;; (message "COURSECODE %s" course-code)
              (if (and
                   course-code
                   (string-match coursenum  course-code )
                   (string-match semester course-code))
                  (progn
                    (plist-put can :shortname
                               shortname)
                    (plist-put can :coursenum coursenum)
                    (plist-put can :semester semester)
                    (setq result can)
                    (org-lms-set-keyword "ORG_LMS_COURSE" (plist-get result :id))))))
      (or result
          (user-error "No course in Canvas matches definition of %s" course))))
;; Getters:1 ends here

;; [[file:org-lms.org::*Setter][Setter:1]]
(defun org-lms-post-syllabus (&optional courseid subtreep)
  "Post  syllabus to course"
  (interactive)
  (setq courseid (or courseid
                     (org-lms-get-keyword "ORG_LMS_COURSEID")
                     (plist-get org-lms-course :id)))
  ;; (cl-flet ((org-html--build-meta-info
  ;;              (lambda (&rest args) "")))
  ;;     ;; (prin1 (symbol-function  'org-html--build-meta-info))
  ;; )
  (let* (;;(org-export-with-toc nil)
         ;;(org-export-with-smart-quotes nil)
         (org-html-postamble nil)
         (org-html-preamble nil)
         (org-html-xml-declaration nil)
         (org-html-head-include-scripts nil)
         (org-html-head-include-default-style nil)
         (org-html-klipsify-src nil)
         (org-export-with-title nil)
         (citeproc-fmt--doi-link-prefix
           "https://doi-org.myaccess.library.utoronto.ca/")
         (citeproc-fmt--formatters-alist
          `((html . ,(citeproc-formatter-create
	              :rt (citeproc-formatter-fun-create org-lms-citeproc-fmt-alist)
	              :bib #'citeproc-fmt--html-bib-formatter))))
         (atext (org-export-as 'canvas-html subtreep nil t))
         (is_public (or (org-lms-get-keyword "IS_PUBLIC") t))
         (license (or (org-lms-get-keyword "LICENSE") "cc_by_nc_sa"))
         (default_view (or (org-lms-get-keyword "DEFAULT_VIEW" )"syllabus"))
         (grading_standard_id (or (org-lms-get-keyword "GRADING_STANDARD_ID") 15 ))
         
         ;;(response (org-lms-get-single-course courseid))
         (data-structure `(("course" . (
                                         ("syllabus_body" . ,atext)
                                        ("is_public" . ,is_public)
                                        ("grading_standard_id" . ,grading_standard_id)
                                        ("license" . ,license)
                                        ;;("default_view" . ,default_view)
                                        ("license" . ,license)
                                        ))))
         (response (org-lms-canvas-request
                    (format  "courses/%s" courseid) "PUT" data-structure ))
         )
    (write-region (json-encode data-structure) nil "~/syl.json")
    ;;(setq response)
    
    (message "Response: %s" response)
    ( if response
        (org-lms-set-keyword "SYLLABUS_URL" (format "%s/%s/assignments/syllabus"
                                                    ;;org-lms-baseurl
                                                    ;;fix this!
                                                    "https://q.utoronto.ca/courses"
                                                    courseid))) 
    ))
;; Setter:1 ends here

;; [[file:org-lms.org::*Custom Gradebook Columns][Custom Gradebook Columns:1]]
(defun org-lms-post-gb-column (title &optional columnid position teachernotes courseid)
    (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
    (org-lms-canvas-request
     (format "courses/%s/custom_gradebook_columns%s" courseid (if columnid (concat "/" columnid) "")) (if columnid "PUT" "POST") 
     `(("column[title]" . ,title)
       ;;,(if position ("column[position]" . position))
       ;;,(if teachernotes ("column[teacher_ notes]" . teachernotes))
       ))
    )

(defun org-lms-get-gb-column-data (columnid &optional courseid)
                        (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
                        (org-lms-canvas-request
                         (format "courses/%s/custom_gradebook_columns/%s/data" courseid columnid) "GET" nil 
                         )
                        )

(defun org-lms-get-gb-columns ( &optional courseid)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request
   (format "courses/%s/custom_gradebook_columns/" courseid) "GET" nil 
   )
  )


(defun org-lms-post-gb-column-data ( data &optional courseid)
  "Post DATA to custom grading columns in the gradebook for COURSEID.
Data should be a list of 3-cell alists, in which the values of `column_id',
`user_id', and `example_content' are set for each entity."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request
   (format "courses/%s/custom_gradebook_column_data" courseid ) "PUT" data 
   )
  )
;; Custom Gradebook Columns:1 ends here

;; [[file:org-lms.org::*Getters][Getters:1]]
(defun org-lms-get-students (&optional courseid sections)
    "Retrieve Canvas student data for course with id COUSEID"
    (let* ((courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
;; (courseid (plist-get course :id))
           (result
            (org-lms-canvas-request (format "courses/%s/users" courseid) "GET"
                                    '(("enrollment_type" . ("student"))
                                      ("include" . ("email" "enrollments"))
                                      ("per_page" . 500 )))))
      (message "RESULTS: number of students: %s" (length result) )
      ;;(with-temp-file "students-canvas.json" (insert result))
      (when sections
        (let ((counter 0))
          (setq result (-filter
                        (lambda (student)
                          (setq counter (1+ counter))
                          (let* ((enrollments (plist-get student :enrollments))
                                 (student-sections
                                  (--map (plist-get it :course_section_id) enrollments)))
                            (-intersection sections student-sections)))
                        result)))
        (message "RESULTS: filtered students: %s" (length result)))
      (cl-loop for student in-ref result
               do
               (if (string-match "," (plist-get student :sortable_name))
                   (let ((namelist  (split-string (plist-get student :sortable_name) ", ")))
                     (plist-put student :lastname (car namelist) )
                     (plist-put student :firstname (cadr namelist)))))
      result))

  (defun org-lms-get-all-users (&optional courseid)
  "Retrieve all users from the course with id COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
    (org-lms-canvas-request (format "courses/%s/users" courseid) "GET" '(("per_page" . 500))))

  (defun org-lms-get-single-user (studentid &optional courseid)
    (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
    (org-lms-canvas-request (format "courses/%s/users/%s" courseid  studentid) "GET"))

  (defun org-lms-find-local-user (id)
    (let* ((result nil))
      (cl-loop for s in org-lms-merged-students
               if (equal id (number-to-string (plist-get s :id)))
               do
               (setq result s))
      result))
;; Getters:1 ends here

;; [[file:org-lms.org::*Transformer -- merging student lists][Transformer -- merging student lists:1]]
;; fix broken symbol not keyword assignment!!!
(defun org-lms-merge-student-lists (&optional local canvas)
  "Merge student lists, optionally explicity named as LOCAL and CANVAS."

  (unless local
    (setq local (org-lms-get-local-students))
    )
  (unless canvas
    (setq canvas (org-lms-get-students)))

  ;;(message "%s" local)
 (if local 
  (cl-loop for c in-ref canvas
        do (let* ((defn c)
                  (email (plist-get defn :email)))
             (cl-loop for l in-ref local
                   if (string=  email  (plist-get l :email))
                   do
                   (progn 
                     (plist-put defn :github (plist-get l :github))
                     (if (plist-get l :nickname)
                         (progn
                           (plist-put defn :nickname (plist-get l :nickname))
                           (plist-put defn :short_name (plist-get l :nickname))))
                     (unless (plist-get c :firstname)
                       (plist-put defn :firstname (plist-get l :firstname)))
                     (unless (plist-get c :lastname)
                       (plist-put defn :lastname (plist-get l :lastname)))
                     
                 )))))
  (with-temp-file "students-merged.json" (insert  (ol-write-json-plists canvas)))
  canvas)
;; Transformer -- merging student lists:1 ends here

;; [[file:org-lms.org::*Getter -- get all pages][Getter -- get all pages:1]]
(defun org-lms-get-all-pages () 
"get all pages as a list of plists"
(interactive)
(org-lms-canvas-request
 (format "courses/%s/pages" (org-lms-get-keyword "ORG_LMS_COURSEID"))
 nil nil))

(defun org-lms-collect-page-links ()
  (let* ((pages (org-lms-get-all-pages))
         (orgList 
          (cl-loop for p in pages
                   concat (format "- [[%s][%s]]\n" (plist-get p :html_url)(plist-get p :title))
                   )))
    orgList))
;; Getter -- get all pages:1 ends here

;; [[file:org-lms.org::*Setter -- create page][Setter -- create page:1]]
(defun org-lms-post-page ()
  "Extract page data from HEADLINE.
  HEADLINE is an org-element object."
  (interactive)

  (let-alist (org-lms-process-props)
    (message "title: %s, roles: %s, published: %s, url: %s" .item .editing_roles .ol_publish .canvas_short_url)
    (let* ((canvas-page-url (org-entry-get nil "CANVAS_PAGE_URL"))
           (org-html-checkbox-type 'unicode )  ;; canvas strips checkbox inputs
           ;;(subtype (if (equal (org-entry-get nil "PAGE_TYPE") "canvas") "online_upload" "none"))
           )
      ;; (message "canvas evals to %s" (if canvasid "SOMETHING " "NOTHING" ))
      (let* ((org-export-with-tags nil)
             (page-params `(("wiki_page" .
                             (("title" .  ,(identity .item) )
                              ("body" . ,(org-export-as 'canvas-html t nil t))
                              ("editing_roles" . ,(or .editing_roles "teachers"))
                              ("published" . ,(if (and .ol_publish
                                                       (not (string= .ol_publish "nil")))
                                                  "true" nil) )))))
             (request-url (format "courses/%s/pages%s"
                                  (org-lms-get-keyword "ORG_LMS_COURSEID")
                                  (if .canvas_short_url
                                    (concat  "/" .canvas_short_url) "")))
             (response
              (org-lms-canvas-request request-url
                                      (if .canvas_short_url "PUT" "POST")
                                      page-params
                                      ))
             (response-data (or response nil))
             )
        ;; (message "request url: %s" request-url)

        ;; (message "HERE COMES THE PARAMS %s" response-data )
        ;; (prin1 (assq-delete-all "page[description]" page-params))
        (if (plist-get response-data :url)
            (progn
              (message "received response-data")
              (org-set-property "CANVASID" (format "%s"(plist-get response-data :page_id)))
              (org-set-property "CANVAS_PAGE_URL" (format "%s"(plist-get response-data :url)))
              (org-set-property "OL_PUBLISH" (format "%s" (plist-get response-data :published)))
              (org-set-property "CANVAS_HTML_URL" (format "%s"(plist-get response-data :html_url)))
              (org-set-property "CANVAS_SHORT_URL" (format "%s"(plist-get response-data :url)))
              (org-set-property "CANVAS_EDITING_ROLES" (format "%s" (plist-get response-data :editing_roles)))
              ))
        ;; (message "PAGE_TYPE is canvas %s" (equal "canvas" (org-entry-get nil "PAGE_TYPE")))
        ;; (message "RESPONSE IS %s" response)
        (if (plist-get response-data :html_url)
            (browse-url (plist-get response-data :html_url)))
        response))))
;; Setter -- create page:1 ends here

;; [[file:org-lms.org::*Getters][Getters:1]]
(defun org-lms-file-post-request (query   request-params path)
  "Send QUERY to `org-lms-baseurl' with http request type POST
  Also send REQUEST-PARAMS as JSON data.  

  Returns a user-error if `org-lms-token' is unset, or if data payload is nil. 
  Otherwise return a parsed json data payload, with the following settings 
  wrapping `json-read':

    `json-array-type' 'list
    `json-object-type' 'plist
    `json-key-type' 'symbol
    maybe key-type needs to be keyword though! Still a work in progress.
    "
  (let ((canvas-payload nil)
        (canvas-err nil)
        (canvas-status nil)
        (json-params (json-encode request-params))
        ;;(params )
        (target (concat org-lms-baseurl query))
        (request-backend 'url-retrieve )
        )
    (if org-lms-token
        (progn
          (setq thisrequest
                (request
                  target
                  :type "POST"
                  :headers `(("Authorization" . ,(concat "Bearer " org-lms-token))
                             ;: ("Content-Type" . "application/json")
                             )
                  :sync t
                  ;;:data   json-params ;; (or data nil)
                  :params request-params 
                  ;;:encoding 'no-conversion
                  :parser (lambda ()
                            ;; (if (and (boundp 'file) file)
                            ;;     (write-region (buffer-string) nil file))
                            (ol-jsonwrapper json-read  ))
                  :success (cl-function
                            (lambda (&key data &allow-other-keys)
                              (message "FIle Info regrieved: %S" data)
                              ;;(message "SUCCESS!!")
                              ;;(setq canvas-payload data)
                              data
                              ))
                  :error (cl-function (lambda ( &key error-thrown data status &allow-other-keys )
                                        (setq canvas-err error-thrown)
                                        (message "ERROR: %s" error-thrown)))))
               (unless (request-response-data thisrequest)                                   
                 (message (format "NO PAYLOAD: %s" canvas-err))
                 (message "Full response: %s" thisrequest))
               (request-response-data thisrequest) )
      (user-error "Please set a value for for `org-lms-token' in order to complete API calls"))))

(defun org-lms-post-new-file (filepath &optional endpoint folder courseid)
  "Upload FILEPATH to canvas storage. Default ENDPOINT is `/courses/COURSEID/files`
but there are other possible endpoints; see the API for details. Optionally organize into FOLDER."
  (interactive)
  ;; main loop
  (let* ((courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
         (endpoint (or endpoint (format "courses/%s/files" courseid)))         
         ;;(storageinfo )
         (fileinfo)
         (allinfo)
         (storageinfo)
         (name (file-name-nondirectory filepath))
         (params `(("name" . ,name)))
         (formstring ""))
    ;; note -- just updated to map-put! from map-put, but that  didn't seem to
    ;; work
    ;; it's all ok now that I'm using map-insert instead  
    (when folder (setq params (map-insert params "parent_folder_path" folder )))
    (setq fileinfo (org-lms-file-post-request
                     endpoint
                     params
                     filepath))
    (message "%s" fileinfo)
    (if fileinfo
        (org-lms-upload-file-to-storage filepath fileinfo))
    ;; (if fileinfo
    ;;     (progn 
    ;;       (setq storageinfo (org-lms-upload-file-to-storage filepath fileinfo))
    ;;       (message "storageninfo: %s" storageinfo)
    ;;       (if  (and  storageinfo (> 0  (length storageinfo )))
    ;;           (progn (setq storageinfo (map-merge
    ;;                                     'plist fileinfo
    ;;                                     (when
    ;;                                         (and  storageinfo (> 0  (length storageinfo )))
    ;;                                       (ol-jsonwrapper json-read-from-string storageinfo))))
    ;;                  storageinfo)
    ;;         (message "CURL DID NOT SUCCEED")
    ;;         storageinfo))
    ;;   (message "FILEINFO DID NOT SUCCEED")
    ;;   nil)
    ))


(defun org-lms-upload-file-to-storage (filepath fileinfo)
  "using a canvas file upload response, upload a file to the file storage."
  (interactive)
  (message "uploading file. fileinfo: %s" fileinfo)
  (let* ((upload-url (map-elt fileinfo :upload_url ))
         (params-plist (map-elt fileinfo :upload_params))
         (params-alist (org-lms-plist-to-alist params-plist))
         (canvas-payload)
         (canvas-err )
         (formstring ""))
    (cl-loop for prop in params-alist
             do
             (setq formstring (concat formstring "-F '" (symbol-name (car prop))
                                      "=" (format "%s" (cdr prop)) "' ")))
    (setq formstring (concat formstring " -F 'file=@" filepath "' 2> /dev/null"))
    (let* ((thiscommand  (concat "curl '"
                                 upload-url
                                 "' " formstring))
           (curlres  (shell-command-to-string thiscommand))
           (file_id (if (> (length curlres) 0 )
                        (format "%s"
                                (plist-get
                                 (ol-jsonwrapper json-read-from-string curlres) :id )))))
      (message "upload curl command response: %s" curlres)
      ;;(f-write-text thiscommand 'utf-8 "~/src/org-grading/filecurlcommand.sh")
      curlres
      )))
;; Getters:1 ends here

;; [[file:org-lms.org::*Getters][Getters:2]]
(defun org-lms-get-folders (&optional courseid)
  (unless courseid
    (setq courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))

  (org-lms-canvas-request (format "courses/%s/folders" courseid) "GET"))

(defun org-lms-get-single-folder (folderid &optional courseid)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")
                     ))
  (org-lms-canvas-request (format "courses/%s/folders/%s" courseid groupid) "GET"))

(defun org-lms-map-folder-from-name (name)
  (interactive)
  (let* ((folders (org-lms-get-folders))
         (match (or (--first (string= (plist-get it :name) name) folders )
                    (org-lms-set-folder `((name . ,name))))))
    (plist-get match :id) ;;(plist-get it :id)
    ;;(org-lms-set-assignment-group `((name . ,name))))
    ))

(defun org-lms-get-files (&optional courseid)
  (unless courseid
    (setq courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request (format "courses/%s/files" courseid) "GET" '(("include" . "content_details" ))))

(defun org-lms-get-single-module-item (itemid moduleid &optional courseid)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")
                     ))
  (org-lms-canvas-request (format "courses/%s/modules/%s/items/%s" courseid moduleid itemid) "GET" '(("include" . "content_details" ))))
;; Getters:2 ends here

;; [[file:org-lms.org::*Setters][Setters:1]]
(defun org-lms-set-folder (params)
  "Create a folder from params"
  (interactive)

  (let* ((canvasid (plist-get params  "CANVASID"))
         )
    (let* (
           (response
            (org-lms-canvas-request (format "courses/%s/folders"
                                            (org-lms-get-keyword "ORG_LMS_COURSEID")
                                            (if canvasid
                                                (format  "/%s" canvasid) ""))
                                    (if canvasid "PUT" "POST")
                                    params))
           (response-data (or response nil)))
      response)))
(defun org-lms-set-file (item module &optional canvasid)
  "create a module item from an item definition"
  (let* ((params `(("module_item" . ,item )))
         (response
          (org-lms-canvas-request (format "courses/%s/modules/%s/items"
                                          (org-lms-get-keyword "ORG_LMS_COURSEID")
                                          module
                                          (if canvasid
                                              (format  "/%s" canvasid) ""))
            (if canvasid "PUT" "POST")
            params)))
    (response-data (or response nil))
    ))
;; Setters:1 ends here

;; [[file:org-lms.org::*Getters][Getters:1]]
(defun org-lms-get-modules (&optional courseid)
  (unless courseid
    (setq courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))

  (org-lms-canvas-request (format "courses/%s/modules" courseid) "GET"))

(defun org-lms-get-single-module (moduleid &optional courseid)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")
                     ))
  (let ((params '(("include" . ("items")))))
    (org-lms-canvas-request (format "courses/%s/modules/%s" courseid moduleid) "GET" params)))

(defun org-lms-map-module-from-name (name)
  (interactive)
  (let* ((modules (org-lms-get-modules))
         (match (or (--first (string= (plist-get it :name) name) modules )
                    (org-lms-set-module `((name . ,name))))))
    (plist-get match :id) ;;(plist-get it :id)
    ;;(org-lms-set-assignment-group `((name . ,name))))
    ))

(defun org-lms-get-module-items (moduleid &optional courseid)
  (unless courseid
    (setq courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request (format "courses/%s/modules/%s/items" courseid moduleid) "GET" '(("include" . "content_details" ))))

(defun org-lms-get-single-module-item (itemid moduleid &optional courseid)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")
                     ))
  (org-lms-canvas-request (format "courses/%s/modules/%s/items/%s" courseid moduleid itemid) "GET" '(("include" . "content_details" ))))
;; Getters:1 ends here

;; [[file:org-lms.org::*Setters][Setters:1]]
;; -🔰in front of course documents\\
;; -💯in front of assessments\\
;; 📖in front of core texts\\
;; -📝in front of handouts that require activity\\
;; -👁 in front of videos (recorded lessons or outside videos)\\
;; -✍🏼in front of asynchronous independent tasks students are assigned


(defun org-lms-set-module (params)
  "Create a module from params"
  (interactive)
  (message "module params: %s" params)
  (let* ((canvasid (map-elt params "moduleid" nil 'equal))
         (org-html-checkbox-type 'unicode )
         (course (org-lms-get-keyword "ORG_LMS_COURSEID"))
         (assignment-params  `(("module" . ,params)))
         (response
          (org-lms-canvas-request (format "courses/%s/modules%s"
                                          course
                                          (if canvasid
                                              (format  "/%s" canvasid) ""))
                                  (if canvasid "PUT" "POST")
                                  assignment-params))
         (response-data (or response nil)))
    (if response-data
    (browse-url (format "%s/courses/%s/modules#%s"
                        org-lms-public-baseurl course
                        (plist-get response-data :id) )))
    response))

(defun org-lms-module-delete (id)
  "Delete the given module from canvas (careful!)"
  (org-lms-canvas-request (format "courses/%s/modules/%s"
                                  (org-lms-get-keyword "ORG_LMS_COURSEID")
                                  id
                                  )
                            "DELETE" 
                          )
  )
;; just acopy of assignment-grou-pfrom-headline.  oos!
(defun org-lms-module-from-headline ()
  "Create a Module from HEADLINE.
  HEADLINE is an org-element object."
  (interactive)
  (let* ((name  (nth 4 (org-heading-components)) )
         (position (org-entry-get nil "POSITION"))
         (moduleid (or (org-entry-get nil "MODULE_ID")
                       (and (org-entry-get nil "MODULE")
                            (org-lms-map-module-from-name (org-entry-get nil "MODULE")))))
         (published (org-entry-get nil "PUBLISHED"))
         (params `(("name" . ,name))))
    (when position (add-to-list  'params `("position" .  ,position)))
    (when moduleid (add-to-list  'params `("moduleid" .  ,moduleid)))
    (when published (add-to-list  'params `("published" .  ,published)))
        (let* ((response (org-lms-set-module params))
           (response-data (or response nil)))
      (if (plist-get response-data :id)
          (progn
            (message "received module response-data %s" response-data)
            (org-set-property "MODULE_ID" (format "%s"(plist-get response-data :id)))
            (org-set-property "MODULE" (format "%s"(plist-get response-data :name)))
            (org-set-property "PUBLISHED" (format "%s"(plist-get response-data :published)))
            (org-set-property "POSITION" (format "%s"(plist-get response-data :position)))
            
            )
        (message "did not receive module group response-data"))
      response)
    ;; (message "Please ensure that MODULE and MODULE_ITEM_TYPE are both set")
    ))


(defun org-lms-match-emoji (category)
  (string= (downcase category)
           (downcase (org-property-get nil "ORG_LMS_CATEGORY"))
           ))



(defcustom org-lms-emojify t
  "whether or not to insert emoji in module item titles")


(defun org-lms-set-module-item (item module &optional canvasid)
  "create a module item from an item definition, then open modules page at item modules"
  (let* ((params `(("module_item" . ,item )))
         (course (org-lms-get-keyword "ORG_LMS_COURSEID"))
         response)
    (message "MODULEPARAMS: %s" item)
    (message "MODULEJSON: %s" (json-encode item))
    
    (setq response
     (org-lms-canvas-request (format "courses/%s/modules/%s/items%s"
                                     course
                                     module
                                     (if canvasid
                                         (format  "/%s" canvasid) ""))
       (if canvasid "PUT" "POST")
       params))
    
    ;; this opens at the *module*. Would it be better to link directly to the 
    ;; item itself at e.g. context_module_item_MODULEITEMID?
    ;; in which case replace 'module' with
    ;; (format "context_module_item_%s" (plist-get response-data :id)
    (browse-url (format "%s/courses/%s/modules#%s" org-lms-public-baseurl course module ))

    (or  response (request-response-error-thrown response) "Something's wrong")
    ))

(defcustom org-lms-emoji-alist
  '(("Assignment" . "💯")
    ("Coursedoc" . "🔰")
    ("Reading" . "📖")
    ("Task" . "✍🏼" )
    ("Quiz" . "✍🏼" )
    ("Discussion" . "❓")
    ("Video" . "👀")
    ("Urgent" . "❗"))
  "Assoc between emoji and predicates")


(defun org-lms-module-item-from-headline ()
  "Extract module data from HEADLINE.
  HEADLINE is an org-element object."
  (interactive)
  (let* ((canvasid (org-entry-get nil "CANVASID"))
         (cat (or  (org-entry-get nil "ORG_LMS_CATEGORY" t)
                   (org-entry-get nil "MODULE_ITEM_TYPE" t)))
         (emoji (when (and  org-lms-emojify cat)
                  (concat  (alist-get cat  org-lms-emoji-alist  nil  nil 'string=) " ")))
         (name  (concat emoji  (nth 4 (org-heading-components))) )
         (position (org-entry-get nil "MODULE_POSITION"))
         (moduleid (or (org-entry-get nil "MODULE_ID" t)
                       (org-lms-map-module-from-name (org-entry-get nil "MODULE" t))))
         (moduleitemtype (or
                          (org-entry-get nil "MODULE_ITEM_TYPE" t)
                          (let ((assignmentp  (org-entry-get nil "CANVAS_SUBMISSION_URL"))
                                (pagep (org-entry-get nil "CANVAS_PAGE_URL") ))
                            (cond (assignmentp "Assignment")
                                  (pagep "Page")))))
         (moduleitemid (org-entry-get nil "MODULE_ITEM_ID"))
         (externalurl (org-entry-get nil "MODULE_ITEM_EXTERNAL_URL"))
         (pageurl (org-entry-get nil "CANVAS_PAGE_URL"))
         (content_id (or 
                      (org-entry-get nil "CANVAS_ID")
                      (org-entry-get nil "CONTENT_ID"))) ;; a bit risky keeping thesethe same
         (newtab t)
         (published  (org-entry-get nil "MODULE_ITEM_PUBLISHED") )
         ;; rules...
         (params `(("title" . ,name)
                   ("type" . ,moduleitemtype)
                   ("new_tab" . t)
                   ("published" . ,published))))
    (when canvasid (add-to-list 'params  `("content_id" . ,(string-to-number canvasid))))
    (when position (add-to-list  'params `("position" .  ,position)))
    (when pageurl (add-to-list  'params `("page_url" .  ,pageurl)))
    (when externalurl (add-to-list  'params `("external_url" .  ,externalurl)))
    ;; (when newtab (add-to-list  'params `("new_tab" .  ,newtab)))
    (when moduleitemid (add-to-list 'params `("module_item_id" . ,moduleitemid)))
    (if (and moduleid (or moduleitemtype pageurl ))
        (let* ((response (org-lms-set-module-item params moduleid moduleitemid))
               (response-data (or response nil))
               (html_url (plist-get response-data :html_url))
               (external_url (plist-get response-data :external_url)))
          
          (if (plist-get response-data :id)
              (progn
                (message "received module response-data")
                (org-set-property "MODULE_ITEM_ID" (format "%s"(plist-get response-data :id)))
                (org-set-property "MODULE_POSITION" (format "%s"(plist-get response-data :position)))

                ;; actually this doesn't get sent back for some reason? huh
                ;;(org-set-property "MODULE_ITEM_NEW_TAB" (format "%s"(plist-get response-data :new_tab)))
                ;; ditto here
                (org-set-property "CONTENT_ID" (format "%s"(or (plist-get response-data :content_id) canvasid "") ))
                (org-set-property "MODULE_ITEM_PUBLISHED" (format "%s"(plist-get response-data :published)))
                )
            (message "did not receive module item response-data"))
          response)
      (message "Please ensure that MODULE and MODULE_ITEM_TYPE are both set"))))
;; Setters:1 ends here

;; [[file:org-lms.org::*Assignments][Assignments:1]]
(defun org-lms-get-assignments (&optional courseid)
  (unless courseid
    (setq courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))

  (org-lms-canvas-request (format "courses/%s/assignments" courseid) "GET"))

(defun org-lms-get-single-assignment (assignmentid &optional courseid)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request (format "courses/%s/assignments/%s" courseid assignmentid) "GET"))



(defun org-lms-merge-assignment-values (&optional local canvas)
  (unless local
    (setq local org-lms-local-assignments ))
  (unless canvas
    (setq canvas (org-lms-get-assignments)))
  (message "LOCALLLLL")
  ;; (prin1 local)
  ;; (prin1 canvas)
  (let ((result '()))
    (cl-loop for l in-ref local
          do (let* ((defn (cdr l))
                    (name (plist-get defn :name)))
               (message "LLLLLLLLL")
               ;; (prin1 l)
               ;; (prin1 (plist-get (cdr l) :name))
               ;; (prin1 name)
               (dolist (c canvas)
                 (message "CCCCCCCC")
                 ;;(message "Printing canvas defn of %s" (plist-get c :name))
                 ;;(prin1 c)
                 (if (equal
                      name  (plist-get c :name))
                     (progn
                       (message "MADE ITI N")
                       (plist-put defn :canvasid (plist-get c :id))
                       (plist-put defn :html_url (plist-get c :html_url))
                       (plist-put defn :submissions_download_url (plist-get c :submissions_download_url))
                       (message "DEFN")
                       (prin1 defn)

                       (add-to-list 'result `(,(car l) .  ,defn)))))))
    result))
;; Assignments:1 ends here

;; [[file:org-lms.org::*Submissions][Submissions:1]]
(defun org-lms-get-submissions (&optional courseid)
  "get all submisisons in a COURSE (rarely used)."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request (format "courses/%s/students/submissions" courseid) "GET" `(("per_page" . 100))))

(defun org-lms-get-assignment-submissions ( assignmentid &optional courseid)
  "Get all submisisons belonging to ASSIGNMENTID in optional COURSE."

  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request
   (format "courses/%s/assignments/%s/submissions/" courseid assignmentid ) "GET" `(("per_page" . 100))))

(defun org-lms-get-single-submission (studentid assignmentid &optional courseid quizp)
  "Retrieve a single sugmission from canvas.
STUDENTID identifies the student, ASSIGNMENTID the assignment, and COURSEID the course. If QUIZP is non-nil, the assignment is a quiz"
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request
   (format "courses/%s/%s/%s/submissions/%s" courseid 
           (if  quizp "quizzes" "assignments")  assignmentid studentid)
   "GET"))
;; Submissions:1 ends here

;; [[file:org-lms.org::*Attachments][Attachments:1]]
(defun org-lms-get-canvas-attachments (&optional quizp)
  (interactive) 
  (let* ((assid
          (save-excursion 
            (org-up-heading-safe)
            (org-entry-get (point) "ASSIGNMENTID")))
         (quizp (save-excursion (org-up-heading-safe) (org-entry-get (point) "IS_QUIZ")))
         (org-lms-merged-students (or org-lms-merged-students (org-lms-get-students)))
         (studentid (or (org-entry-get (point) "STUDENTID") (org-entry-get (point) "ID")))
         (submission (org-lms-get-single-submission studentid assid quizp))
         (student (org-lms-find-local-user studentid)))
         (message "Submission: %s" submission)
    (cl-loop for attachment in (plist-get submission :attachments)
             do
             (message "%s%s" (plist-get student :lastname)
                      (plist-get student :firstname) )
             (let* ((downloadurl (plist-get attachment :url))
                    (filename
                     (format "%s%s_%s%s_%s_%s"
                             (downcase (plist-get student :lastname))
                             (downcase (plist-get student :firstname))
                             (if (plist-get submission :late)
                                 "late_" "")
                             studentid   (org-lms-safe-pget attachment :studentid)
                             (plist-get attachment :display_name)))
                    (f (request-response-data
                        (request
                         downloadurl
                                :sync t
                         :parser 'buffer-string )))
                    (fullpath (expand-file-name filename (org-entry-get (point) "ORG_LMS_ASSIGNMENT_DIRECTORY"))))
               (message "attachment exists")
               ;;(prin1 f)
               ;;(message "STUDENT %s" (or (plist-get attachment :late) "NOPE"))
               (if (file-exists-p fullpath)
                   (message "file %s already exists, not downloading" filename)
               (let ((coding-system-for-write 'no-conversion))
                   (with-temp-file fullpath
                   ;; (set-buffer-multibyte nil)
                     (insert (string-as-multibyte f))
                     ;; (encode-coding-string contents 'utf-8 nil (current-buffer))
                     )))
               (unwind-protect
                   (condition-case err
                       (org-attach-attach (expand-file-name
                                           filename
                                           (org-entry-get
                                            (point) "ORG_LMS_ASSIGNMENT_DIRECTORY")))
                     ('error (message "Caught exception while attaching %s: [%s]"filename err)))
                 (message "Cleaning up attach...")))))
  )
;; Attachments:1 ends here

;; [[file:org-lms.org::*Assignment Groups][Assignment Groups:1]]
(defun org-lms-get-assignment-groups (&optional courseid)
  (unless courseid
    (setq courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))

  (org-lms-canvas-request (format "courses/%s/assignment_groups" courseid) "GET"))

(defun org-lms-get-single-assignment-group (groupid &optional courseid)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")
                     ))
  (org-lms-canvas-request (format "courses/%s/assignment_groups/%s" courseid groupid) "GET"))
;; Assignment Groups:1 ends here

;; [[file:org-lms.org::*Assignments][Assignments:1]]
(defun org-lms-post-assignment ()
  "Extract assignment data from HEADLINE.
  HEADLINE is an org-element object."
  (interactive)

  (let* ((canvasid (org-entry-get nil "CANVASID"))
         (duedate (org-entry-get nil "DUE_AT"))
         (org-html-checkbox-type 'unicode )  ;; canvas stirps checkbox inputs
         (pointspossible (if (org-entry-get nil "ASSIGNMENT_WEIGHT") (* 100 (string-to-number (org-entry-get nil "ASSIGNMENT_WEIGHT")))))
         (gradingtype (or  (org-entry-get nil "GRADING_TYPE") "letter_grade"))
         (subtype (or (and (org-entry-get nil "CANVAS_SUBMISSION_TYPES")
                           (replace-regexp-in-string "(\\([^()]*\\))" "\\1"
                                                     (org-entry-get nil "CANVAS_SUBMISSION_TYPES")  ))
                      (if (equal (org-entry-get nil "ASSIGNMENT_TYPE")
                                 "canvas")
                          "online_upload" "none")))
         ;;  (org-entry-get nil "DUE_AT"))
         (publish (org-entry-get nil "OL_PUBLISH"))
         (group (org-entry-get nil "ASSIGNMENT_GROUP" t))
         (omit (org-entry-get nil "ASSIGNMENT_OMIT"))
         (position (org-entry-get nil "ASSIGNMENT_POSITION"))
         (reflection (org-entry-get nil "OL_HAS_REFLECTION"))
         (reflection-id (org-entry-get nil "OL_REFLECTION_ID"))
         (org-export-with-tags nil)
         (assignment-params `(("name" .  ,(nth 4 (org-heading-components)) )
                              ("description" . ,(org-export-as 'canvas-html t nil t))
                              ("due_at" . ,(o-l-date-to-timestamp
                                            (or duedate
                                                (format-time-string "%Y-%m-%d"
                                                                    (time-add (current-time) (* 7 24 3600) )) ) ))
                              ;;`("due_at"   . ,(o-l-date-to-timestamp duedate))
                              
                              ("submission_types" . ,subtype)
                              ("grading_type" . ,gradingtype)
                              ("grading_standard_idcomment" . 458)
                              ("points_possible" . ,(or pointspossible 10))
                              ("published" . ,(if publish t nil) )
                              
                              ))
         response finalparams)
    ;; (message "canvas evals to %s" (if canvasid "SOMETHING " "NOTHING" ))
    ;;(prin1 canvasid)
    (when group
      (add-to-list 'assignment-params `("assignment_group_id" . ,(org-lms-map-assignment-group-from-name group))))
    (when position
      (add-to-list  'assignment-params `("position" . ,position)))
    (when omit
      (add-to-list 'assignment-params `("omit_from_final_grade" ,
                                        omit)))
    (setq finalparams `(("assignment" .  ,assignment-params)))
    (setq response
          (org-lms-canvas-request (format "courses/%s/assignments%s"
                                          (org-lms-get-keyword "ORG_LMS_COURSEID");; (plist-get org-lms-course :id)
                                          (if canvasid
                                              (format  "/%s" canvasid) "")
                                          )
            (if canvasid "PUT" "POST")
            finalparams
            ))
    (setq response-data (or response nil))
    (message "response data is non-nil %s" response-data)
          ;; (message "HERE COMES THE PARAMS %s" (request-response-data response) )
          ;; (prin1 (assq-delete-all "assignment[description]" assignment-params))



    (if (plist-get response-data :id)
        (progn
          (message "received assignment response-data")
          (org-set-property "DUE_AT"
                            (ts-format
                             "%Y-%m-%d" ;; wrong time zone so ignore for now "%Y-%m-%dT%H:%M:%S%:z"
                             (ts-parse (plist-get response-data :due_at)))                            )
          (org-set-property "CANVASID" (format "%s"(plist-get response-data :id)))
          (org-set-property "OL_PUBLISH" (format "%s"(plist-get response-data :published)))
          (org-set-property "ORG_LMS_CATEGORY" "Assignment")
          (org-set-property "CANVAS_HTML_URL" (format "%s"(plist-get response-data :html_url)))
          (org-set-property "CANVAS_SUBMISSION_URL" (format "%s" (plist-get response-data :submissions_download_url)))
          (org-set-property "SUBMISSIONS_DOWNLOAD_URL" (format "%s"(plist-get response-data :submissions_download_url)))
          (org-set-property "GRADING_STANDARD_ID" (format "%s"(plist-get response-data :grading_standard_id)))
          (org-set-property "CANVAS_SUBMISSION_TYPES" (format "%s"(plist-get response-data :submission_types)))
          (org-set-property "GRADING_TYPE" (format "%s"(plist-get response-data :grading_type)))
          (org-set-property "CANVASID" (format "%s"(plist-get response-data :id)))
          
          (if reflection 
              (let* ((reflection-params `(("assignment" .
                                           (("name" .  ,(concat  (nth 4 (org-heading-components)) " Reflection Questions") )
                                            ("description" . ,(org-export-as 'html t nil t))
                                            ,(if duedate
                                                 `("due_at"   . ,(o-l-date-to-timestamp duedate))
                                               )
                                            ("submission_types" . "none")
                                            ("grading_type" . ,gradingtype)
                                            ("grading_standard_idcomment" . 458)
                                            ("points_possible" . 1)
                                            ("published" . ,(if publish t nil) )))))
                     (reflection-response
                      (org-lms-canvas-request (format "courses/%s/assignments%s"
                                                      (org-lms-get-keyword "ORG_LMS_COURSEID")
                                                      (if reflection-id
                                                          (format  "/%s" reflection-id) "")
                                                      )
                        (if reflection-id "PUT" "POST")
                        assignment-params
                        )))
                (if (and reflection-response (plist-get reflection-response :id))
                    (progn
                      (message "received reflection response-data")
                      (org-set-property "OL_REFLECTION_ID" (format "%s" (plist-get response-data :id)))))))))

    response))



(defun org-lms-post-assignment-and-save (&optional file)
  "First post the assignment, then save the value to FILE."
  (interactive)
  (unless file (setq file (expand-file-name "assignments.el")))
  (org-lms-post-assignment)
  (org-lms-save-assignment-map file))
;; Assignments:1 ends here

;; [[file:org-lms.org::*Assignments][Assignments:2]]
(defun org-lms-assignment-update ()
  "remove previous year's properties to make updating easier."
  (interactive)
  (cl-map 'list  (lambda (prop)
                   (org-entry-delete (point) prop))
          '("CANVASID" "CANVAS_HTML_URL" "CANVAS_SUBMISSION_URL" "SUBMISSIONS_DOWNLOAD_URL"))
  )

(defun org-lms-assignment-update-all ()
  (interactive)
  (org-map-entries #'org-lms-assignment-update "assignment"))
;; Assignments:2 ends here

;; [[file:org-lms.org::*Assignment Groups][Assignment Groups:1]]
(defun org-lms-assignment-group-from-headline ()
  "Extract assignment group data from HEADLINE.
  HEADLINE is an org-element object."
  (interactive)
  (let* ((canvasid (org-entry-get nil "GROUP_ID"))
         (name  (nth 4 (org-heading-components)) )
         (position (org-entry-get nil "GROUP_POSITION"))
         (weight (org-entry-get nil "GROUP_WEIGHT"))
         ;; rules...

         (params `((name . ,name))))
    (when canvasid (add-to-list 'params `("CANVASID" . ,canvasid)))
    (when position (add-to-list 'params `("position" . ,(string-to-number position))))
    (when weight (add-to-list  'params `("group_weight" . ,(string-to-number weight))))

    (let* ((response (org-lms-set-assignment-group params))
           (response-data (or response nil)))
      
      (if (plist-get response-data :id)
          (progn
            (message "received assignment group response-data")
            (org-set-property "GROUP_ID" (format "%s"(plist-get response-data :id)))
            (org-set-property "GROUP_POSITION" (format "%s"(plist-get response-data :position)))
            (org-set-property "GROUP_WEIGHT" (format "%s"(plist-get response-data :group_weight)))
            )
        (message "did not receive assignment group response-data"))
      response)))

(defun org-lms-set-assignment-group (params)
  "Create an asignment group from params"
  (interactive)

  (let* ((canvasid (or  (plist-get params  "CANVASID")
                        (alist-get "CANVASID" params  nil nil #'equal)))
         (org-html-checkbox-type 'unicode )  ;; canvas stirps checkbox inputs
         
         )
    ;; (message "canvas evals to %s" (if canvasid "SOMETHING " "NOTHING" ))
    ;;(prin1 canvasid)
    (let* ((org-export-with-tags nil)
           (assignment-params  params 
                               )
  
           (response
            (org-lms-canvas-request (format "courses/%s/assignment_groups%s"
                                            (org-lms-get-keyword "ORG_LMS_COURSEID")
                                            (if canvasid
                                                (format  "/%s" canvasid) "")
                                            )
                                    (if canvasid "PUT" "POST")
                                    assignment-params
                                    ))
           (response-data (or response nil)))
      response)))
;; Assignment Groups:1 ends here

;; [[file:org-lms.org::*Assignment Groups][Assignment Groups:2]]
(defun org-lms-map-assignment-group-from-name (name)
  (interactive)
  (let* ((groups (org-lms-get-assignment-groups))
         (match (or (--first (string= (plist-get it :name) name) groups )
                    (org-lms-set-assignment-group `((name . ,name)))))
         )
    (plist-get match :id) ;;(plist-get it :id)
    ;;(org-lms-set-assignment-group `((name . ,name)))
    ))


;;(org-lms-set-assignment-group `((name . "Tests")))
;; Assignment Groups:2 ends here

;; [[file:org-lms.org::*Setters -- headlineto announcement][Setters -- headlineto announcement:1]]
;; huh is this deprecated?
;; doesn't seem to be used at all 
(defun org-lms-post-announcement (payload &optional courseid apipath)
  "Create new announcement using PAYLOAD a data in course COURSEID."
  (setq courseid (or courseid
                     (org-lms-get-keyword "ORG_LMS_COURSEID")
                     (plist-get org-lms-course))
        apipath (or apipath "courses"))
  (org-lms-canvas-request
   (format "%s/%s/discussion_topics" apipath courseid) "POST" payload))

;; announcements

(defun org-lms-headline-to-announcement (&optional courseid file apipath)
  ""
  (interactive)
  (setq courseid (or courseid
                     (org-lms-get-keyword "ORG_LMS_COURSEID")
                     (plist-get org-lms-course))
        apipath (or apipath "courses"))
  ;; (cl-flet ((org-html--build-meta-info
  ;;            (lambda (&rest args) ""))))
  (let* ((org-export-with-toc nil)
         (org-export-with-smart-quotes nil)
         (org-html-postamble nil)
         (org-html-preamble nil)
         (org-html-xml-declaration nil)
         (org-html-head-include-scripts nil)
         (org-html-head-include-default-style nil)
         ;;(atext (org-export-as 'html t))
         (atitle (org-lms-quick-template-expand
                  (nth 4 (org-heading-components))))
         (org-html-klipsify-src nil)
         (org-export-with-title nil)
         ;;(courseid (plist-get course :id))
         (atext (org-export-as 'canvas-html t nil t))
         (response nil)
         (oldid (org-entry-get (point) "ORG_LMS_ANNOUNCEMENT_ID"))
         (section (org-entry-get (point) "OL_SECTION_ID" t))
         (params `(("title" . ,atitle)
                   ("message" . ,atext)
                   ("is_published" . (not (org-entry-get (point) "ORG_LMS_WITHHOLD")))
                   ("is_announcement" . t))))
    (when (and section (not (string-equal apipath "groups")))
      (add-to-list 'params `("specific_sections" . ,(s-split " " section))  ))
    ;; (message "BUILDMETA DEFN")
    ;; (prin1 (symbol-function  'org-html--build-meta-info))
    ;; (message "%s" atext)

    (if oldid
        (progn
          (message "already added!")
          (setq response ;;(request-response-data) 
                (org-lms-canvas-request
                 (format  "%s/%s/discussion_topics/%s" apipath courseid oldid) "PUT"
                 params)))

      (setq response ;;(request-response-data)
            (org-lms-canvas-request
             (format  "%s/%s/discussion_topics" apipath courseid) "POST"
             params)))
    (cl-loop for (k v) on response
             do
             (message "%s %S" k v))
    (org-entry-put (point) "ORG_LMS_ANNOUNCEMENT_ID" (format "%s" (plist-get response :id)))
    (org-entry-put (point) "ORG_LMS_ANNOUNCEMENT_URL" (format "%s" (plist-get response :url)))
    (org-entry-put (point) "ORG_LMS_POSTED_AT" (format "%s" (plist-get response :posted_at)))

    (if (plist-get response :url) 
        (browse-url (plist-get response :url)))
    response))
;; Setters -- headlineto announcement:1 ends here

;; [[file:org-lms.org::*Sections][Sections:1]]
(defun org-lms-get-sections (&optional courseid)
"Retrieve list of course sections" 
  (unless courseid
    (setq courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))

  (org-lms-canvas-request (format "courses/%s/sections" courseid) "GET"))

(defun org-lms-get-single-section (sectionid &optional courseid)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")
                     ))
  (let ((params '(("include" . ( "students" "total_students" "enrollments" "email")))))
    (org-lms-canvas-request (format "courses/%s/sections/%s" courseid sectionid) "GET" params)))



(defun org-lms-map-section-from-name (name)
  (interactive)
  (let* ((sections (org-lms-get-sections))
         (match (or (--first (string= (plist-get it :name) name) sections )
                    ;; (org-lms-set-section `((name . ,name)))
                    )))
    (plist-get match :id) ;;(plist-get it :id)
    ;;(org-lms-set-assignment-group `((name . ,name))))
    ))

(defun org-lms-get-section-students (sectionid &optional courseid)
  "get all students in section; if sectionid is a string, first map from section list"
  (interactive)
  (let* ((section (or (and (or  (numberp sectionid) (s-numeric sectionid))  sectionid) (org-lms-map-section-from-name sectionid)))
        (students (plist-get  (org-lms-get-single-section section courseid) :students)))
    ;; (--sort (plist-get it :sortable_name)
    ;;         (plist-get  (org-lms-get-single-section section courseid) :students))
    (cl-loop for student in-ref students
            do
            (if (string-match "," (plist-get student :sortable_name))
                (let ((namelist  (split-string (plist-get student :sortable_name) ", ")))
                  (plist-put student :lastname (car namelist) )
                  (plist-put student :firstname (cadr namelist)))))
    (cl-sort students
             'string-lessp :key (lambda (student) (plist-get student :sortable_name)))))

(defun org-lms-get-multiple-section-students (sectionslist &optional courseid)
  "collect and merge student lists from sections"
  (--mapcat (org-lms-get-section-students it) sectionslist))
;; Sections:1 ends here

;; [[file:org-lms.org::*Getter -- get-grading-standards][Getter -- get-grading-standards:1]]
(defun org-lms-get-grading-standards (&optional courseid)
    "Retrieve Canvas grading standards for course with id COUSEID"
    (let* ((courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
           (result
            (org-lms-canvas-request (format "courses/%s/grading_standards" courseid) "GET" )))
      result))
;; Getter -- get-grading-standards:1 ends here

;; [[file:org-lms.org::*Making and getting groupsets (group categories)][Making and getting groupsets (group categories):1]]
(defun org-lms-get-group-categories (&optional courseid)
  "we will need these in order to create and get groups, actually"
  (interactive)
  (setq courseid (or courseid
                       (org-lms-get-keyword "ORG_LMS_COURSEID")
                       (plist-get org-lms-course)))
  (let* ((response nil))
    (setq response ;;(request-response-data) 
          (org-lms-canvas-request
           (format  "%s/%s/group_categories" "courses" courseid ) "GET"))
    response))

(defun org-lms-group-category-from-name (name) 
  (let* ((categories (org-lms-get-group-categories))
         (matching
          (seq-filter
              (lambda (group)
                (string= name 
                         (plist-get group :name)))
              categories)))
    (if matching 
        (plist-get (car matching) :id)
        ;;(plist-get matching :id)
      (message "whoops"))))
;; Making and getting groupsets (group categories):1 ends here

;; [[file:org-lms.org::*Making and getting groups][Making and getting groups:1]]
(defun org-lms-get-groups-in-category (category &optional courseid)
  
  (let ((request)
        (courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))) 
    (setq request (org-lms-canvas-request
                   (format  "group_categories/%s/groups"  category ) "GET"))
    request))
(cl-defun org-lms-set-group-in-category (name category &key students id description)
  (let ((method (if id "PUT" "POST"))
        (params `((name . ,name)))
        (request))
    (if students (add-to-list 'params  `(members . ,students)))
    (if description (add-to-list 'params  `(description . ,description)))
    (setq request (org-lms-canvas-request
                   (if id 
                       (format "groups/%s" id)
                     (format "group_categories/%s/groups" category))
                   method
                   params))
    request))
;; Making and getting groups:1 ends here

;; [[file:org-lms.org::*Group Announcements][Group Announcements:1]]
(defun org-lms-headline-to-group-announcement ()
  (interactive)
  (let ((groupid (org-entry-get (point)  "GROUP_ID" t)))
    (org-lms-headline-to-announcement groupid nil "groups")))
;; Group Announcements:1 ends here

;; [[file:org-lms.org::*Some miscellaneous section functions][Some miscellaneous section functions:1]]
(defun org-lms-get-multiple-section-ids (sectionRE)
  (seq-map
   (lambda (section) (plist-get section :id))
   (seq-filter
    (lambda (section)
      (string-match
       sectionRE
       (plist-get section :name)))
    (org-lms-get-sections ))))
;; (org-lms-get-multiple-section-ids "0301\\|0401\\|0501\\|0601")
;; Some miscellaneous section functions:1 ends here

;; [[file:org-lms.org::his108-students][his108-students]]
(defun org-lms-students-from-multiple-sections (sectionRE)
(seq-mapcat
 (lambda (sectionid)
   (plist-get  (org-lms-get-single-section sectionid) :students))
 (seq-map
  (lambda (section) (plist-get section :id))
  (seq-filter
   (lambda (section)
     (string-match
      sectionRE
    (plist-get section :name)))
 (org-lms-get-sections )))))
;; (seq-map (lambda (s) (plist-get s :id))

;;  )
;; his108-students ends here

;; [[file:org-lms.org::*Principal headline-making functions][Principal headline-making functions:1]]
;; MAIN ORG-LMS UTILITY FUNCTIONS

;; attaching files to subtrees
;; looks like this is unuesed.  
(defun org-lms-attach () 
  "Interactively attach a file to a subtree. 

Assumes that the parent headline is the name of a subdirectory,
and that the current headline is the name of a student. Speeds up file choice."
  (interactive)
  (let ((lms-att-dir
         (org-entry-get (point) "ORG_LMS_ASSIGNMENT_DIRECTORY" t)
         
         ;; (save-excursion
         ;;   (org-up-heading-safe)
         ;;   ())
         ))
    (message lms-att-dir)
    ;; (read-file-name
    ;;  (concat  "File for student " (nth 4 (org-heading-components)) ":")
    ;;  (expand-file-name lms-att-dir))
    (if lms-att-dir
        (org-attach-attach (read-file-name
                            (concat  "File for student " (nth 4 (org-heading-components)) ":")
                            (concat  (expand-file-name lms-att-dir) "/")))
      (message "Warning: no such directory %s; not attaching file" lms-att-dir))
    )
  ;; (if (save-excursion
  ;;       )
  ;;     (org-attach-attach (read-file-name
  ;;                         (concat  "File for student " (nth 4 (org-heading-components)) ":")
  ;;                         (org-lms~get-parent-headline) ))
  ;;   (message "Warning: no such directory %s; not attaching file" (org-lms~get-parent-headline)))
  )

;; This doesn't work because org-attach doesn't have a map per se
;; instead this would need to modify `org-attach-commands`
;; also, you'd only want to do that if org-grading were active I guess
;; this feels a bit fragile
;;(define-key 'org-attach-map (kbd "s p") #'projectile-pt)

(defun org-lms-make-headings (a students &optional ignore-attachments)
  "Create a set of headlines for grading.

A is a plist describing the assignment. STUDENTS is now assumed
to be a plist, usually generated by
`org-lms~parse-plist-csv-file' but eventually perhaps read
directly from Canvas LMS. UPDATE: seems to work well with
`org-lms-merged-students'

Canvas LMS allows for export of student information; the
resultant csv file has a certain shape, bu this may all be irrelevant now."
  (message "running org-lms-make-headings")
  (unless students
    (setq students (org-lms-get-students)))
  (save-excursion
    (goto-char (point-max))
    ;; (message "students=%s" students)
    ;; (mapcar (lambda (x)))
    (let* ((body a)
           ;; rewrite this part wit horg-process-props? 
           ;; nmaybe not possible as written.
           (aname (plist-get body :name))
           (org-id (plist-get body :org-id))
           (atitle (format "[[id:%s][%s]]"
                           org-id
                           (plist-get body :name)))
           (quizp (plist-get body :is-quiz))
           (number (plist-get body :assignment_number))
           (assignmentid (or (format "%s" (plist-get body :canvasid)) ""))
           (directory (plist-get body :directory ))
           (weight (plist-get body :assignment-weight ))
           (grade-type (plist-get body :grade-type ))
           (assignment-weight (plist-get body :assignment-weight))
           (assignment-type (plist-get body :assignment-type))
           (email-response (plist-get body :email-response))
	   (canvas-response (plist-get body :canvas-response))
           (basecommit (or (plist-get body :basecommit) "none"))
           (repo-basename (or  (plist-get body :repo-basename) ""))
           (grading-type (or (plist-get body :grading-type) "letter_grade"))
           (courseid (or (plist-get body :courseid) (org-lms-get-keyword "ORG_LMS_COURSEID")) 
                     ;; (if  (and  (boundp 'org-lms-course) (listp org-lms-course))
                     ;;     (number-to-string (plist-get org-lms-course :id))
                     ;;   nil)
                     )
           (template (plist-get body :rubric)))
      ;; (message "car assignment successful: %s" template)
      (insert (format "\n* %s :ASSIGNMENT:" atitle))
      (org-set-property "ASSIGNMENT_WEIGHT" assignment-weight)
      (org-set-property "ASSIGNMENTID" assignmentid)
      (org-set-property "ASSIGNMENT_NAME" aname)
      (and directory (org-set-property "ORG_LMS_ASSIGNMENT_DIRECTORY" directory))
      (org-set-property "BASECOMMIT" basecommit)
      (org-set-property "GRADING_TYPE" grading-type)
      (org-set-property "IS_QUIZ" (format "%s" quizp))
      ;;(org-set-property "NUMBER" number)
      ;;(org-set-property "ORG_LMS_EMAIL_RESPONSE" "t")
      ;;(org-set-property "ORG_LMS_CANVAS_RESPONSE" "t")
      (org-set-property "ORG_LMS_EMAIL_COMMENTS" "t")
      (org-set-property "ORG_LMS_CANVAS_COMMENTS" "t")
      (make-directory directory t)
      (goto-char (point-max))
      (let* (( afiles (if (file-exists-p directory)
                          (directory-files directory  nil ) nil))
             (json-array-type 'list)
             (json-object-type 'plist)
             (json-key-type 'keyword)
             (json-false nil)
             ;; this crufty garbage needs to be fixed. 
             ;;(prs (if (string= assignment-type "github") (json-read-file "./00-profile-pr.json")))
             )
        (mapcar (lambda (stu)
                  ;;(message "%s" stu)
                  (let* ((fname (plist-get stu :firstname))
                         (lname (plist-get stu :lastname))
                         (nname (or  (unless (equal  (plist-get stu :nickname) nil)
                                       (plist-get stu :nickname)) fname))
                         (email (or (plist-get stu :email) ""))
                         (coursenum (if  (and  (boundp 'org-lms-course) (listp org-lms-course))
                                        (plist-get org-lms-course :coursenum)
                                      nil))

                         (github (or  (plist-get stu :github) ""))
                         (id (or (number-to-string (plist-get stu :id)) ""))
                         (props 
                          `(("GRADE" . "0")
                            ("CHITS" . "0")
                            ("NICKNAME" . ,nname)
                            ("FIRSTNAME" . ,fname)
                            ("LASTNAME" . ,lname)
                            ("MAIL_TO" . ,email)
                            ("GITHUB" . ,github)
                            ("ORG_LMS_REPO_BASENAME" . ,repo-basename)
                            ("STUDENTID" . ,id)
                            ("GRADE_URL" . ,(concat org-lms-public-baseurl "courses/" courseid
                                                    "/gradebook/speed_grader?assignment_id=" assignmentid  "&student_id=" id))
                            ("COURSEID" . ,courseid)
                            ("BASECOMMIT" . ,basecommit) ;; it would be better to keep this in the parent
                            ("ORG_LMS_ASSIGNMENT_DIRECTORY" . ,directory)
                            ;; ("MAIL_CC" . "matt.price@utoronto.ca")
                            ("MAIL_REPLY" . "matt.price@utoronto.ca")
                            ("MAIL_SUBJECT" .
                             ,(format "%sComments on Assignment \"%s\" (%s %s)"
                                      (if coursenum
                                          (format "[%s] " coursenum)
                                        "")
                                      aname nname lname ))
                            ))
                         )
                    ;; (message "COURSENUM: %s" coursenum)
                    (insert (format "\n** %s %s\n" nname lname))
                    (org-todo 'todo) 
                    (dolist (p props)
                      (org-set-property (car p ) (cdr p)))
                    (insert (or template ""))
                    (if weight (insert (format "This assignment is worth *%s percent* of your mark and is graded as a letter grade. Please see ... for more details.\n"
                                               (* 100   (if (numberp weight) weight (string-to-number weight))))))


                    ;; Gather student assignments, if possible
                    ;; method depends on assignment type
                    ;; (message "SUBMISSIONTYPE %s" assignment-type)
                    (cond
                     ((equal assignment-type "github")
                      (org-set-property "LOCAL_REPO"
                                        (expand-file-name
                                         github
                                         ;; old way
                                         ;; (concat repo-basename "-" github)
                                         directory))

                      ;; this is some weird shit I used to do.  Time to fix it maybe.
                      ;; instead use a control vocabulary to find appropriate branches

                      ;; anyway as of 2019, not currently in use.

                      ;; hard-coded!!!!
                      ;; shouldn't this use ol-json-wrapper?
                      (let* ((json-array-type 'list)
                             (json-object-type 'plist)
                             (json-key-type 'keyword)
                             (json-false nil)
                             (prs  '() ;; (json-read-file "./01-profile-pr.json")
                                   ))
                        ;; (message "MADE IT INTO LOOP for student with ID %s" github)
                        (if prs
                            ;; (message "%s" prs)
                            (dolist (pull prs) ;; need to update this I guless
                              ;; (message "%s: %s"github  pull)
                              
                              (if (string= (plist-get pull :githubid) github)
                                  (progn
                                    (org-set-property "COMMENTS_PR" (plist-get pull :url))
                                    (let ((s (or (plist-get pull :status) "")))
                                      (org-set-property "TEST_STATUS" s)
                                      (cond
                                       ((string= "fail" s)
                                        (insert "\nYour repository did not pass all required tests."))
                                       ((string= "pass" s)
                                        (insert "\nYour repository passed all required tests for the basic asisgnment!"))
                                       ((string= "reflection" s)
                                        (insert "\nYour repository passed all tests, including the reflection checks!")))
                                      (insert (concat "\nThere may be further comments in your github repo: " (plist-get pull :url) )))
                                    ))
                              ))
                        ))
                     ;; if assignment is handed in on canvas, getstudent work as attachments     
                     ((and (equal assignment-type "canvas") (not ignore-attachments))
                      ;; (message "SUBTYPE IS CANVAS")
                      (ignore-errors
                        (with-local-quit
                          (org-lms-get-canvas-attachments))))
                     
                     ;; otherwise, look for existing files with approximately matching names in the appropriate directory.  
                     (t
                      (let* ((fullnamefiles
                              (remove-if-not
                               (lambda (f) (string-match (concat "\\\(" fname "\\\)\\\([^[:alnum:]]\\\)*" lname) f)) afiles))
                             (nicknamefiles
                              (remove-if-not
                               (lambda (f) (string-match (concat "\\\(" nname "\\\)\\\([^[:alnum:]]\\\)*" lname) f)) afiles)))
                        ;;(message "fullnamefiles is: %s" fullnamefiles)
                        (if afiles
                            (cond
                             (fullnamefiles
                              ;; (if fullnamefiles)
                              (dolist (thisfile fullnamefiles)
                                ;;(message "value of thisfile is: %s" thisfile)
                                ;;(message "%s %s" (buffer-file-name) thisfile)
                                ;;(message "value being passed is: %s"(concat (file-name-directory (buffer-file-name)) assignment "/" thisfile) )
                                (org-attach-attach
                                 (concat (file-name-directory (buffer-file-name))
                                         directory "/" thisfile) )
                                (message "Attached perfect match for %s %s" fname lname)))
                             (nicknamefiles
                              (dolist (thisfile nicknamefiles)
                                ;; (if t)
                                ;; (progn) 
                                (org-attach-attach (concat (file-name-directory (buffer-file-name)) assignment "/" thisfile) )
                                (message "No perfect match; attached likely match for %s (%s) %s" fname nname lname)))

                             (t 
                              (message "No files match name of %s (%s) %s" fname nname lname)))
                          (message "warning: no directory %s, not attaching anything" directory)))
                      ;; other cases
                      )
                     )

                    ;; (condition-case nil

                    ;;   (error (message "Unable to attach file belonging to student %s" nname )))
                    (save-excursion
                      (org-back-to-heading)
                      ;;(org-mark-subtree);;

                      (org-cycle nil))
                    ))
                students)
        (run-hooks 'ol-make-headings-final-hook)
        )) 
    (org-cycle-hide-drawers 'all)))
;; Principal headline-making functions:1 ends here

;; [[file:org-lms.org::*OBSOLETE github-specific function][OBSOLETE github-specific function:1]]
;; org make headings, but for github assignments
(defun org-lms-make-headings-from-github (assignments students)
  "Create a set of headlines for grading.

ASSIGNMENTS is an alist in which the key is the assignment title,
and the value is itslef a plist with up to three elements. The
first is the assignment base name, the second is a list of files
to attach, and the third is the grading template. STUDENTS is now
assumed to be a plist, usually generated by
`org-lms~parse-plist-csv-file'. Relevant field in the plist are
First, Last, Nickname, Email, github.

The main innovations vis-a-vis `org-lms-make-headings` are
the structure of the the alist, and the means of attachment
"
  ;;(message "%s" assignments)
  (save-excursion
    (goto-char (point-max))
    (message "students=%s" students)
    (mapcar (lambda (x)
              (let* ((title (car x))
                     (v (cdr x))
                     (template (plist-get v :template))
                     (basename (plist-get v :basename))
                     (filestoget (plist-get v :files))
                     (prs (if (plist-get v :prs)
                              (org-lms~read-lines (plist-get v :prs))
                            nil))
                     )
                (insert (format "\n* %s :ASSIGNMENT:" title))
                ;;(let (( afiles (directory-files (concat title  )   nil ))))
                (mapcar (lambda (stu)
                          (let* ((fname (plist-get stu 'First))
                                 (lname (plist-get stu 'Last))
                                 (nname (or  (plist-get stu 'Nickname) fname))
                                 (email (plist-get stu 'Email))
                                 (github (plist-get stu 'github))
                                 (afiles (ignore-errors
                                           (directory-files
                                            (concat title "/" basename "-" github ))))
                                 
                                 )
                            (message "afiles is: %s" afiles )
                            ;;(message  "pliste gets:%s %s %s %s" fname lname nname email)
                            (insert (format "\n** %s %s" (if (string= nname "")
                                                          fname
                                                        nname) lname))
                            (org-todo 'todo)
                            (insert template)
                            (org-set-property "GRADE" "0")
                            (org-set-property "CHITS" "0")
                            (org-set-property "NICKNAME" nname)
                            (org-set-property "FIRSTNAME" fname)
                            (org-set-property "LASTNAME" lname)
                            (org-set-property "MAIL_TO" email)
                            (org-set-property "GITHUB" github)
                            (org-set-property "LOCAL_REPO" (concat title "/" basename "-" github "/" ))
                            (if prs
                                (mapcar (lambda (url)
                                          (message "inside lambda")
                                          (if (string-match github url)
                                              (progn
                                                (message "string matched")
                                                ;; one thought would be to add all comments PR's to this
                                                ;; but that would ocmplicate the logic for opening the PR URL
                                                ;; automatically
                                                ;; (org-set-property "COMMENTS_PR"
                                                ;;                   (concat (org-get-entry (point) "COMMENTS_PR") " " url))
                                                (org-set-property "COMMENTS_PR" url)
                                                (insert (concat "\nPlease see detailed comments in your github repo: " url))
                                                )))
                                        prs)
                              )
                            ;; (org-set-property "MAIL_CC" "matt.price@utoronto.ca")
                            (org-set-property "MAIL_REPLY" "matt.price@utoronto.ca")
                            (org-set-property "MAIL_SUBJECT"
                                              (format "Comments on %s Assignment (%s %s)"
                                                      (mwp-org-get-parent-headline) nname lname ))
                            
                            ;;   (error (message "Unable to attach file belonging to student %s" nname )))
                            (save-excursion
                              (org-mark-subtree)
                              (org-cycle nil))
                            ))students) ) ) assignments)))
;; OBSOLETE github-specific function:1 ends here

;; [[file:org-lms.org::*Attachments][Attachments:1]]
;; stolen from gnorb, but renamed to avoid conflicts
(defun org-lms~attachment-list (&optional id)
  "Get a list of files (absolute filenames) attached to the
  current heading, or the heading indicated by optional argument ID."
  (when (featurep 'org-attach)
    (let* ((attach-dir (save-excursion
                         (when id
                (org-id-goto id))
                         (org-attach-dir t)))
           (files
            (mapcar
             (lambda (f)
               (expand-file-name f attach-dir))
             (org-attach-file-list attach-dir))))
      files)))
;; Attachments:1 ends here

;; [[file:org-lms.org::*Sending Subtrees][Sending Subtrees:1]]
;; temp fix for gh
(defun org-lms~mail-text-only ()
  "org-mime-subtree and HTMLize"
  (interactive)
  (org-mark-subtree)
  (save-excursion
    (org-mime-org-subtree-htmlize)
    (message-send-and-exit)
    )
  )

;; mail integration. Only tested with mu4e.
(defun org-lms--send-subtree-with-attachments ()
  "org-mime-subtree and HTMLize"
  (interactive)
  ;; (org-mark-subtree)
  (let ((attachments (org-lms-attach-file-list)))
    (save-excursion
      
      (org-lms-mime-org-subtree-htmlize attachments)
      ;; (org-mime-org-subtree-htmlize attachments)
      )))

;; defunkt
;; Sending Subtrees:1 ends here

;; [[file:org-lms.org::*Mail and Post Multiple Trees][Mail and Post Multiple Trees:1]]
(cl-defun org-lms-return-all-assignments (&optional (send-all nil) (also-mail nil) (post-to-lms t) )
  "By default mail all subtrees 'READY' to student recipients, unless SEND-ALL is non-nil.
In that case, send all marked 'READY' or 'TODO'."
  (interactive)
  (message "Returning all READY subtrees to students")
  
  (let* ((ol-status-org-msg org-msg-mode)
        
        (send-condition
         (if send-all
             `(or (string= (org-element-property :todo-keyword item) "READY")
                  (string= (org-element-property :todo-keyword item) "TODO") )
           `(string= (org-element-property :todo-keyword item) "READY")
           )))
    (if ol-status-org-msg (org-msg-mode))
    (org-map-entries 
     #'ol-return-just-one)
    (if ol-status-org-msg (org-msg-mode)))
  (org-cycle-hide-drawers 'all))


(cl-defun ol-return-just-one (&optional (also-mail nil) (post-to-lms t))
  ;; (print (nth 0 (org-element-property :todo-keyword item)))
  (interactive)
  (let ((also-mail (org-entry-get nil "ORG_LMS_EMAIL_COMMENTS" t))
        (post-to-lms (org-entry-get nil "ORG_LMS_CANVAS_COMMENTS" t)))
    
    (when (string= (nth 2 (org-heading-components) ) "READY")
      (when post-to-lms (org-lms-put-single-submission-from-headline))
      (when also-mail  (save-excursion
                         ;;(org-lms-mime-org-subtree-htmlize )
                         (org-lms--send-subtree-with-attachments)
                         (sleep-for 1)
                         ;; (message-send-and-exit)
                         ))
      (org-todo "SENT"))))
;; should get rid of this & just add a flag to ~org-lms-mail-all~
(defun org-lms-return-all-undone ()
  (interactive)
  "Mail all subtrees marked 'TODO' to student recipients."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (item)
      ;; (print (nth 0 (org-element-property :todo-keyword item)))
      (when (string= (org-element-property :todo-keyword item) "TODO")
        (save-excursion
          (goto-char (1+ (org-element-property :begin item)) )
          ;;(print "sending")
          ;;(print item)
          (save-excursion
            (org-lms-send-missing-subtree)
            (message-send-and-exit))
          (org-todo ))))))

(cl-defun org-lms-post-all-grades (&optional (send-all nil) (also-mail nil) (post-to-lms t) )
  "By default post all  'READY' to student recipients, unless SEND-ALL is non-nil.
In that case, send all marked 'READY' or 'TODO'."
  (interactive)
  (message "Returning all READY subtrees to students")
  
  (let* ((ol-status-org-msg org-msg-mode)
         
         (send-condition
          (if send-all
              `(or (string= (org-element-property :todo-keyword item) "READY")
                   (string= (org-element-property :todo-keyword item) "TODO") )
            `(string= (org-element-property :todo-keyword item) "READY")
            )))
    (org-map-entries 
     #'ol-post-just-one-grade))
  (org-cycle-hide-drawers 'all))

(cl-defun ol-post-just-one-grade ()
  "post only the grade for current headline"
  (interactive)
  (when (string= (nth 2 (org-heading-components) ) "READY")
    (org-lms-put-single-grade-from-headline)
    (org-todo "SENT")))

;; doesn't seem to actually be used... 
(defun org-lms-send-missing-subtree ()
  "org-mime-subtree and HTMLize"
  (interactive)
  (org-mark-subtree)
  (let ((attachments (mwp-org-attachment-list))
        (subject  (mwp-org-get-parent-headline)))
    ;;(insert "Hello " (nth 4 org-heading-components) ",\n")
    (org-mime-subtree)
    (insert "\nBest,\nMP.\n")
    (message-goto-body)
    (insert "Hello,\n\nI have not received a paper from you, and ma sending this email just to let you know.\n\n")
    (insert "At this point I have marked all the papers I know about. If 
you have not received a grade for work that you have handed in,
 please contact me immediately and we can resolve the situation!.\n\n")
    (org-mime-htmlize)
    ;; this comes from gnorb
    ;; I will reintroduce it if I want to reinstate questions.
    ;; (map-y-or-n-p
    ;;  ;; (lambda (a) (format "Attach %s to outgoing message? "
    ;;  ;;                    (file-name-nondirectory a)))
    ;; (lambda (a)
    ;;   (mml-attach-file a (mm-default-file-encoding a)
    ;;                    nil "attachment"))
    ;; attachments
    ;; '("file" "files" "attach"))
    ;; (message "Attachments: %s" attachments)
    (dolist (a attachments) (message "Attachment: %s" a) (mml-attach-file a (mm-default-file-encoding a) nil "attachment"))
    (message-goto-to)
    ))
;; Mail and Post Multiple Trees:1 ends here

;; [[file:org-lms.org::*Even more Mail -- org-mime rewrite functions.][Even more Mail -- org-mime rewrite functions.:1]]
;; ;; more helpers
(defun org-lms-mime-org-subtree-htmlize (&optional attachments)
  "Create an email buffer of the current subtree.
The buffer will contain both html and in org formats as mime
alternatives.

Need to remove link from header
The following headline properties can determine the headers.\n* subtree heading
   :PROPERTIES:
   :MAIL_SUBJECT: mail title
   :MAIL_TO: person1@gmail.com
   :MAIL_CC: person2@gmail.com
   :MAIL_BCC: person3@gmail.com
   :END:

The cursor is left in the TO field."
  (interactive)
  (save-excursion
    ;; (funcall org-mime-up-subtree-heading)
    (cl-flet ((mp (p) (org-entry-get nil p org-mime-use-property-inheritance)))
      (let* ((org-export-with-broken-links t)
             (file (buffer-file-name (current-buffer)))
             (subject (or (mp "MAIL_SUBJECT")
                          (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]"
                                                   "\\1"
                                                   (nth 4 (org-heading-components)))))
             (to (mp "MAIL_TO"))
             (cc (mp "MAIL_CC"))
             (bcc (mp "MAIL_BCC"))
             (addressee (or (mp "NICKNAME") (mp "FIRSTNAME") ) )
             ;; Thanks to Matt Price for improving handling of cc & bcc headers
             (other-headers (cond
                             ((and cc bcc) `((cc . ,cc) (bcc . ,bcc)))
                             (cc `((cc . ,cc)))
                             (bcc `((bcc . ,bcc)))
                             (t nil)))
             (subtree-opts (when (fboundp 'org-export--get-subtree-options)
			     (org-export--get-subtree-options)))
	     (org-export-show-temporary-export-buffer nil)
	     (org-major-version (string-to-number
				 (car (split-string  (org-release) "\\."))))
	     (org-buf  (save-restriction
			   (org-narrow-to-subtree)
			   (let ((org-export-preserve-breaks org-mime-preserve-breaks)
                                 )
			     (cond
			      ((= 8 org-major-version)
			       (org-org-export-as-org
			        nil t nil
			        (or org-mime-export-options subtree-opts)))
			      ((= 9 org-major-version)
			       (org-org-export-as-org
			        nil t nil t
			        (or org-mime-export-options subtree-opts)))))))
	     (html-buf (save-restriction
			 (org-narrow-to-subtree)
			 (org-html-export-as-html
			  nil t nil t
			  (or org-mime-export-options subtree-opts))))
	     ;; I wrap these bodies in export blocks because in org-mime-compose
	     ;; they get exported again. This makes each block conditionally
	     ;; exposed depending on the backend.
	     (org-body (prog1
			   (with-current-buffer org-buf
			     ;; (format "#+BEGIN_EXPORT org\n%s\n#+END_EXPORT"
				   ;;   (buffer-string))
           (buffer-string))
			 (kill-buffer org-buf)))
	     (html-body (prog1
			    (with-current-buffer html-buf
			      (format "#+BEGIN_EXPORT html\n%s\n#+END_EXPORT"
				      (buffer-string))
            ;; (buffer-string)
            )
			  (kill-buffer html-buf)))
	     ;; (body (concat org-body "\n" html-body))
       (body org-body))
	(save-restriction
	  (org-narrow-to-subtree)
	  (org-lms-mime-compose body file to subject other-headers
			            (or org-mime-export-options subtree-opts)
                                    addressee))
        (if (eq org-mime-library 'mu4e)
        (advice-add 'mu4e~switch-back-to-mu4e-buffer :after
                    `(lambda ()
                       (switch-to-buffer (get-buffer ,(buffer-name) ))
                       (advice-remove 'mu4e~switch-back-to-mu4e-buffer "om-temp-advice"))
                    '((name . "om-temp-advice"))))
        (dolist (a attachments)  (mml-attach-file a (mm-default-file-encoding a) nil "attachment"))

	(message-goto-to)
        (message-send-and-exit)
        ))))

(defun org-lms-mime-compose (body file &optional to subject headers opts addressee)
  "Create mail BODY in FILE with TO, SUBJECT, HEADERS and OPTS."
  (when org-mime-debug (message "org-mime-compose called => %s %s" file opts))
  (setq body (format "Hello%s, \n\nAttached are the comments from your assignment.\n%s\nBest,\nMP.\n----------\n" (if addressee (concat " " addressee) "")  (replace-regexp-in-string "\\`\\(\\*\\)+.*$" "" body)))
  (let* ((fmt 'html)
	 ;; we don't want to convert org file links to html
	 (org-html-link-org-files-as-html nil)
	 ;; These are file links in the file that are not images.
	 (files
	  (if (fboundp 'org-element-map)
	      (org-element-map (org-element-parse-buffer) 'link
		(lambda (link)
		  (when (and (string= (org-element-property :type link) "file")
			     (not (string-match
				   (cdr (assoc "file" org-html-inline-image-rules))
				   (org-element-property :path link))))
		    (org-element-property :path link))))
	    (message "Warning: org-element-map is not available. File links will not be attached.")
	    '())))
    (unless (featurep 'message)
      (require 'message))
    (cl-case org-mime-library
      (mu4e
       (mu4e~compose-mail to subject headers nil))
      (t
       (message-mail to subject headers nil)))
    (message-goto-body)
    (cl-labels ((bhook (body fmt)
		       (let ((hook 'org-mime-pre-html-hook))
			 (if (> (eval `(length ,hook)) 0)
			     (with-temp-buffer
			       (insert body)
			       (goto-char (point-min))
			       (eval `(run-hooks ',hook))
			       (buffer-string))
			   body))))
      (let* ((org-link-file-path-type 'absolute)
	     (org-export-preserve-breaks org-mime-preserve-breaks)
	     (plain (org-mime--export-string body 'org))
	     ;; this makes the html self-containing.
	     (org-html-htmlize-output-type 'inline-css)
	     ;; this is an older variable that does not exist in org 9
	     (org-export-htmlize-output-type 'inline-css)
	     (html-and-images
	      (org-mime-replace-images
	       (org-mime--export-string (bhook body 'html) 'html opts)
	       file))
	     (images (cdr html-and-images))
	     (html (org-mime-apply-html-hook (car html-and-images))))
	;; If there are files that were attached, we should remove the links,
	;; and mark them as attachments. The links don't work in the html file.
	(mapc (lambda (f)
		(setq html (replace-regexp-in-string
			    (format "<a href=\"%s\">%s</a>"
				    (regexp-quote f) (regexp-quote f))
			    (format "%s (attached)" (file-name-nondirectory f))
			    html)))
	      files)
	(insert (org-mime-multipart plain html)
		(mapconcat 'identity images "\n"))
	;; Attach any residual files
	(mapc (lambda (f)
		(when org-mime-debug (message "attaching: %s" f))
		(mml-attach-file f))
	      files)))))
;; Even more Mail -- org-mime rewrite functions.:1 ends here

;; [[file:org-lms.org::*Even more Mail -- org-mime rewrite functions.][Even more Mail -- org-mime rewrite functions.:2]]

;; Even more Mail -- org-mime rewrite functions.:2 ends here

;; [[file:org-lms.org::*Set Grades and Overlays][Set Grades and Overlays:1]]
(require 'ov)
(defvar-local org-lms-grade-overlays '()
  "array of currently existing grading overlays")
;; still imperfect, but good enough for me.
(defun org-lms-overlay-headings ()
  "Show grades at end of headlines that have a 'GRADE' property. If file keyword 'OL_USE_CHITS' is non-nil, also add a 'CHItS:' overlay."
  (interactive)
  (require 'ov)

  (let ((chits (org-lms-get-keyword "OL_USE_CHITS")))
    (org-map-entries
     (lambda ()
       (when (org-entry-get (point) "GRADE")
         (ov-clear (- (line-end-position) 1)
                   (+ 0 (line-end-position)))
         (setq ov (make-overlay (- (line-end-position) 1)
                                (+ 0 (line-end-position))))
         (setq character (buffer-substring (- (line-end-position) 1) (line-end-position)))
         (overlay-put
          ov 'display
          (format  "%s  GRADE: %s %s" character (org-entry-get (point) "GRADE")
                   (if chits (org-entry-get (point) "CHITS") "")))
         (overlay-put ov 'name "grading")
         (message "%s" (overlay-get ov "name"))))))
  )

(defun org-lms-overlay-current-heading ()
  "Show grades at end of headlines that have a 'GRADE' property. If file keyword 'OL_USE_CHITS' is non-nil, also add a 'CHItS:' overlay."
  (interactive)
  (save-excursion
    (let ((grade (org-entry-get (point) "GRADE")))
      (when grade
        (org-back-to-heading)
        (let* ((gradestring (format  "GRADE: %s%s" grade
                                     ;; causing a hangup with parsing sometimes
                                     ;; (if (org-lms-get-keyword "OL_USE_CHITS")  (format " %s" (org-entry-get (point) "CHITS")) "")
                                     ""
                                     ))
               (end (line-end-position))
               (character (buffer-substring (- end 1) end))
               (gradeov (ov-in 'name "grading" (line-beginning-position) end)))
          (if gradeov
              (setq gradeov (car gradeov))
            (setq gradeov (ov (- end 1) end 'name "grading" 'grading "true")))
          (ov-set gradeov 'display (concat character " " gradestring))
          
          ;; (setq thisov (make-overlay (- end 1)
          ;;                            (+ 0 end)))

          ;; (overlay-put
          ;;  thisov 'display
          ;;  )
          ;; (overlay-put thisov 'name "grading")
          ;; (overlay-put thisov 'grading "true")
          (message "%s" (ov-val gradeov 'name)))))))

(defun org-lms-clear-overlays ()
  "if the overlays become annoying at any point"
  (interactive)
  (ov-clear))

(defvar ol-grade-regex  "- \\*?Grade:?\\*?\\( ::\\)? ?\\([^[:space:]]+\\) *"
  "regular expression matching grade lines." )

(defun org-lms-set-grade ()
  "set grade property for current heading on basis of \"- Grade :: \" line.

  Use with caution."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (save-excursion
      (org-back-to-heading)
      (while (re-search-forward ol-grade-regex nil t )
        (let ((mark (or (match-string 2) 0)))
          (message "mark is: %s" mark)
          (if (string= mark "Pass")
              (setq mark "pass"))
          (org-set-property "GRADE" mark)
          (org-todo "READY"))
        (org-lms-overlay-current-heading))
      (org-back-to-heading)
      (org-cycle)))
  (progn  (org-next-visible-heading 1)
        (org-show-entry)
        (org-hide-drawer-all)))

(defun org-lms-set-all-grades ()
  "set grade property for all headings on basis of \"- Grade :: \" line.

  Use with caution."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ol-grade-regex nil t )
      (org-set-property "GRADE" (or (match-string 2) 0))
      ;; (save-excursion
      ;;   (org-back-to-heading)
      ;;   (org-set-property)
      ;;   (org-element-at-point))
      ))
  (org-lms-overlay-headings)

  )

(defun org-lms-set-all-grades-boolean ()
  "set grade property for all headings on basis of \"- Grade :: \" line.

  Use with caution."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ol-grade-regex nil t )
      (let ((grade (match-string 1)))
        (if (or (string-match "pass" (downcase grade)) (string-match "1" grade ))
            (progn (message grade)
                   (org-set-property "GRADE" "pass"))
          ))

      ;;(org-set-property "GRADE" (match-string 1))
      ;; (save-excursion
      ;;   (org-back-to-heading)
      ;;   (org-set-property)
      ;;   (org-element-at-point))
      ))
  (org-lms-overlay-headings)
  ;;(org-lms-overlay-headings)

  )
;; Set Grades and Overlays:1 ends here

;; [[file:org-lms.org::*More grading, for pass/fail][More grading, for pass/fail:1]]
;; helper function to set grades easily. Unfinished.
(defun org-lms-pass ()
  "set the current tree to pass"
  
  (interactive)
  (org-set-property "GRADE" "1")
  ;;(ov-clear)
  (org-lms-overlay-headings)
  )

(defun org-lms-chit ()
  "set the current tree to one chit"
  
  (interactive)
  (org-set-property "CHITS" "1")
  (ov-clear)
  (org-lms-overlay-headings)
  )
;; More grading, for pass/fail:1 ends here

;; [[file:org-lms.org::*Grade Report Tables][Grade Report Tables:1]]
(defun org-lms-generate-tables ()
  "Generate a *grade report* buffer with a summary of the graded assignments
Simultaneously write results to results.csv in current directory."
  (interactive)

  (let ((students (org-lms-get-students))
        (assignments '())
        (chits (org-lms-get-keyword "OL_USE_CHITS")))

    ;; hack! having trouble with this
    (cl-loop for s in-ref students
             do (plist-put s :grades '()))
    ;;get assignments
    (let ((org-use-tag-inheritance nil))
      (org-map-entries
       (lambda ()
         (add-to-list 'assignments
                      (org-id-get-create)
                      ;;(nth 4 (org-heading-components))
                      t))
       "ASSIGNMENT"))
    
    ;;loop over entries
    ;; this should be improved, returning a plist to be looped over
    (dolist (assignment assignments)
      (save-excursion
        (org-open-link-from-string (format "[[id:%s]]" assignment)) ;; jump to assignment
        (org-map-entries        ;; map over entries
         (lambda ()
           (let* ((heading (nth 4 (org-heading-components)))
                  (email (org-entry-get (1+ (point)) "MAIL_TO" )))
             ;; loop over students, find the right one
             (cl-loop for s in-ref students
                      if (string= (plist-get s :email) email)
                      do
                      (let* ((grades (plist-get s :grades))
                             (g (org-entry-get (point) "GRADE")))
                        (cond
                         ((string= g "1") (setq g "Pass"))
                         ;; this needs to be figured out. I want this in p/f booleans but not for 0 grades in non-booleans
                         ((string= g "0") (setq g "Fail"))
                         )
                        (add-to-list 'grades `(,heading . ,g))
                        
                        ;; (if chits
                        ;;     (add-to-list 'grades `(,(concat assignment " Chits") . ,(org-entry-get (point) "CHITS"))))
                        (plist-put s :grades grades)))))
         nil 'file 'comment)))
    ;; there's gotta be a bette way!
    (cl-loop for s in-ref students
             do (let ((grades (plist-get s :grades)))
               (plist-put s :grades (reverse grades))))
    (message "Students = %s" students)
    
    (let* ((columns (cl-loop for a in assignments
                             collect (save-excursion
                                       (org-open-link-from-string (format "[[id:%s]]" a))
                                       (org-entry-get (point) "ASSIGNMENT_NAME" ))
                                         ;; if chits
                                         ;; collect (concat a " Chits")
                                         ))
           (tableheader (append '("Student" "First" "Nick" "Last" "Student #" "email") columns))
           (rows (cl-loop for s in students
                     collect
                     ;; (message "%s" s)
                     (let* ((grades (plist-get s :grades))
                            (row (append `(,(plist-get s :name)
                                           ,(plist-get s :firstname)
                                           ,(plist-get s :nickname)
                                           ,(plist-get s :lastname)
                                           ,(plist-get s :integration_id) ;; check to be sure this is right
                                           ,(plist-get s :email)
                                           )
                                         (cl-loop for c in columns
                                                  collect (cdr (assoc c grades))))))
                       (message "%s" row)
                       row)
                     )))
      (message "%s %s" (length rows) (length students)) (message "%s" tableheader)
      (cl-loop for h in-ref tableheader
               do
               (if (string-match "chits" (downcase h) )
                   (setq h "Chits")))
      
      (setq gradebook
            (append (list  tableheader
                           'hline)
                    rows))

      (write-region (orgtbl-to-csv gradebook nil) nil "results3.csv"))

    
    
    ;; I would like to put the gradebook IN the buffer but I can't figure out
    ;; a wayt odo it without killing 
    ;; (org-open-ling-from-string "[[#gradebook]]")
    ;;(let ((first-child (car (org-element-contents (org-element-at-point)))))  (when (eq )))
    (let ((this-buffer-name  (buffer-name)))
      (switch-to-buffer-other-window "*grade report*")
      (erase-buffer)
      (org-mode)
      
      (insert (orgtbl-to-orgtbl gradebook nil))
      (insert "\n\n* Grade reports\n")

      (cl-loop for s in students
               do
               (message "%s" s)
               (let* ((grades (plist-get s :grades))
                      (fname (plist-get s :firstname))
                      (lname (plist-get s :lastname))
                      (nname (or  (unless (equal  (plist-get s :nickname) nil)
                                    (plist-get s :nickname)) fname))
                      (email (plist-get s :email))
                      (coursenum (if  (and  (boundp 'org-lms-course) (listp org-lms-course))
                                     (plist-get org-lms-course :coursenum)
                                   ""))
                      (github (or  (plist-get s :github) ""))
                      ;; (id (or (number-to-string (plist-get s :id)) ""))
                      (props 
                       `(("NICKNAME" . ,nname)
                         ("FIRSTNAME" . ,fname)
                         ("LASTNAME" . ,lname)
                         ("MAIL_TO" . ,email)
                         ("GITHUB" . ,github)
                         ;; ("STUDENTID" . ,id)
                         ("MAIL_REPLY" . "matt.price@utoronto.ca")
                         ("MAIL_SUBJECT" .
                          ,(format "%s Grades Summary"
                                   (if coursenum
                                       (format "[%s] " coursenum)
                                     ""))))))
                 ;; (message "COURSENUM: %s" coursenum)
                 (insert (format "** TODO %s %s" nname lname))
                 ;; (org-todo 'todo)
                 (cl-loop for g in grades
                          do
                          (insert (concat "\n" "- " (car g) " :: " (cdr g) "\n"))
                          (dolist (p props)
                            (org-set-property (car p ) (cdr p))))
                 (save-excursion
                   (org-back-to-heading)
                   (org-cycle nil))
                 )
               )
      
      (pop-to-buffer this-buffer-name)))
  ;;(pop-to-buffer nil)
  )

;; try writing reports for each students
;; Grade Report Tables:1 ends here

;; [[file:org-lms.org::*Github-related helper functions][Github-related helper functions:1]]
;; helper functions for github repos
(defun org-lms~open-student-repo ()
  (interactive)
  (find-file-other-window (org-entry-get (point) "LOCAL_REPO" )))

(defun org-lms~open-attachment-or-repo () 
  (interactive)
  (let* ((attach-dir (org-attach-dir t))
         (files (org-lms-attach-file-list)))
    (if (> (length files) 0 )
        (org-attach-open)
      (org-lms~open-student-repo)
      )))
;; Github-related helper functions:1 ends here

;; [[file:org-lms.org::*Setters - post submissions from headlines][Setters - post submissions from headlines:1]]
(defun org-lms-put-single-grade-from-headline (&optional studentid assignmentid courseid)
  "Get grade only (!) from student headline and post to Canvas LMS.
If STUDENTID, ASSIGNMENTID and COURSEID are omitted, their values
will be extracted from the current environment, as the GRADE alwyas will be"
  (interactive)
  ;; main loop
  (let* ((courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
         (assignmentid (or assignmentid (save-excursion (org-up-heading-safe) (org-entry-get (point) "ASSIGNMENTID"))))
         (studentid (or studentid (org-entry-get (point) "STUDENTID")))
         (grade (org-entry-get (point) "GRADE"))
         (returnval '()))
    ;; loop over attachments
    
    (let* ((grade-params `(("submission" . (("posted_grade" . ,(org-lms-calculate-grade grade))))))
           (comment-response ;;(request-response-data)
            (org-lms-canvas-request
             (format "courses/%s/assignments/%s/submissions/%s" courseid assignmentid studentid)
             "PUT" grade-params)))
      (org-entry-put nil "ORG_LMS_SPEEDGRADER_URL"
                     (format
                      "[[https://q.utoronto.ca/courses/%s/gradebook/speed_grader?assignment_id=%s#{\"student_id\":%s}]]"
                      courseid assignmentid studentid))
      
      (message "%s" (plist-get  (car (plist-get comment-response
                                                :submission_comments)) :id))
      (message "NO PROBLEMS HERE")
      ;; (message "Response: %s" comment-response )
      comment-response)))

;; not that if you want to update a comment instead, it's necessary to use a different request mechanism

(defun org-lms-put-single-submission-from-headline (&optional studentid assignmentid courseid)
  "Get comments from student headline and post to Canvas LMS.
If STUDENTID, ASSIGNMENTID and COURSEID are omitted, their values
will be extracted from the current environment. Note the
commented out `dolist' macro, which will upload attachments to
canvas. THis process is potentially buggy and seems likely to
lead to race conditions and duplicated uploads and comments. Still
working on this."
  (interactive)
  ;;(setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (unless assignmentid
    (setq assignmentid (save-excursion (org-up-heading-safe)
                                       (org-entry-get (point) "ASSIGNMENTID"))))
  (unless studentid (setq studentid (org-entry-get (point)  "STUDENTID")))
  ;; main loop
  (let* ((courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
         (grade (org-entry-get (point) "GRADE"))
         (comments (let*((org-export-with-toc nil)
                         ;;(atext (org-export-as 'html t))
                         ;;(atitle (nth 4 (org-heading-components)))
                         (org-ascii-text-width 23058430000))
                     (org-export-as 'ascii t nil t)))
         (returnval '()))
    ;; loop over attachments
    (dolist (a  (org-lms-attach-file-list))
      (let* ((path (expand-file-name a (org-attach-dir t) )))
        (unless (file-directory-p path)
          (let* ((fileinfo (org-lms-canvas-request
                            (format "courses/%s/assignments/%s/submissions/%s/comments/files"
                                    courseid assignmentid studentid)
                            "POST" `(("name" . ,a)) ) ;; (request-response-data )
                           )
                 (al (org-lms-plist-to-alist (plist-get fileinfo :upload_params)))
                 (formstring ""))
            (cl-loop for prop in al
                     do
                     (setq formstring (concat formstring "-F '" (symbol-name (car prop))
                                              "=" (format "%s" (cdr prop)) "' ")))
            (setq formstring (concat formstring " -F 'file=@" path "' 2> /dev/null"))
            (let* ((thiscommand  (concat "curl '"
                                         (plist-get fileinfo :upload_url)
                                         "' " formstring))
                   (curlres  (shell-command-to-string thiscommand))
                   (file_id (if (> (length curlres) 0 ) (format "%s" (plist-get (ol-jsonwrapper json-read-from-string curlres) :id )))))
              (message "CURLRES: %s" curlres)
              
              (if file_id (progn
                            (setq returnval (add-to-list 'returnval file_id))
                            ;; this needs to be fixed up still -- only saves last
                            (org-entry-put (point) "ORG_LMS_ATTACHMENT_URL"
                                           file_id))))))))
    (let* ((grade-params `(("submission" . (("posted_grade" . ,(org-lms-calculate-grade grade))))
                           ("comment" . (("text_comment" . ,comments)
                                         ;; EDIT 2018=11-07 -- untested switch from alist to plist
                                         ("file_ids" . ,returnval)
                                         ;; alas, doesn't seem to update the previous comment! drat
                                         ;; it's a different API endpoint
                                         ;; this is a todo item
                                         ("id" . ,(or (org-entry-get nil "OL_COMMENT_ID" ) nil))))))
           (comment-response ;;(request-response-data)
            (org-lms-canvas-request
             (format "courses/%s/assignments/%s/submissions/%s" courseid assignmentid studentid)
             "PUT" grade-params)))
      (org-entry-put nil "ORG_LMS_SPEEDGRADER_URL"
                     (format
                      "[[https://q.utoronto.ca/courses/%s/gradebook/speed_grader?assignment_id=%s#{\"student_id\":%s}]]"
                      courseid assignmentid studentid))
      (org-entry-put nil "OL_COMMENT_ID"
                     (format "%s"
                             (plist-get  (car (plist-get comment-response
                                                         :submission_comments)) :id))  )
      (message "%s" (plist-get  (car (plist-get comment-response
                                                :submission_comments)) :id))
      (message "NO PROBLEMS HERE")
      ;; (message "Response: %s" comment-response )
      comment-response)))
;; Setters - post submissions from headlines:1 ends here

;; [[file:org-lms.org::*Set up local environment][Set up local environment:1]]
;;deprectaed!!!!!!
(defun org-lms-setup ()
  "Merge  defs and students lists, and create table for later use.

`org-lms-course', `org-lms-local-assignments' and other org-lms
variables must be set or errors wil lresult."
  (setq org-lms-merged-students (org-lms-merge-student-lists))
  (setq org-lms-merged-assignments (org-lms-merge-assignment-values))
  (org-lms-assignments-table org-lms-merged-assignments)
  )

(defun org-lms-setup-grading (&optional courseid assignmentsfile)
  "Parse assignments buffer and students lists, and create table for later use.

`org-lms-course', `org-lms-local-assignments' and other org-lms
variables must be set or errors will result."
  (setq org-lms-merged-students (org-lms-merge-student-lists))
  ;;(setq org-lms-merged-assignments (org-lms-merge-assignment-values))
  (setq assignments (org-lms-map-assignments (org-lms-get-keyword "ORG_LMS_ASSIGNMENTS")))
  (setq org-lms-merged-assignments assignments)
  (org-lms-assignments-table assignments))

(defun org-lms-setup-ta-grading (sectionslist &optional courseid assignmentsfile)
  "Parse assignments buffer & student lists, and create table for later use.
Restrict student list to your tutorials."
  (setq org-lms-merged-students
        (org-lms-merge-student-lists
         nil ;;
         (org-lms-get-multiple-section-students sectionslist courseid)
         ;;(org-lms-get-students courseid sectionslist)
         ))
  (setq assignments (org-lms-map-assignments (org-lms-get-keyword "ORG_LMS_ASSIGNMENTS")))
  (setq org-lms-merged-assignments assignments)
  (org-lms-assignments-table assignments))

(defun org-lms-get-local-csv-students (&optional csv)
  (unless csv
    (setq csv "./students.csv"))
  (org-lms~parse-plist-symbol-csv-file csv)
  )

(defun org-lms-get-local-json-students (&optional jfile)
  (unless jfile
    (setq jfile "./students-local.json"))
  (ol-jsonwrapper json-read-file jfile))



(defcustom org-lms-get-student-function 'org-lms-get-local-json-students
  "function to use to get students"
  :type 'function)

(defun org-lms-get-local-students (&optional file)
  (unless file
    (setq file "./students-local.json"))
  (if (file-exists-p file) 

      (apply org-lms-get-student-function (list file))))
;; Set up local environment:1 ends here

;; [[file:org-lms.org::*Assignments Table][Assignments Table:1]]
(defun org-lms-assignments-table (&optional assignments students)
  "Return a 2-dimensional list suitable whose contents are org-mode table cells.

Intnded to be used in a simpe src block with :results header `value raw table'. 
Resultant links allow quick access to the canvas web interface as well as the make-headings commands."
  (unless assignments
    (setq assignments org-lms-merged-assignments))
  (unless students
    (setq students org-lms-merged-students))
  ;;(message "MERGED ASSIGNMENTS")
  ;;(prin1 assignments)
  (let* ((cid (org-lms-get-keyword "ORG_LMS_COURSEID"))
         (make-headlines-string "")
         (table-header '(("Name (upload here)" "Download URL" Inspect "Make Headers") hline))
         )
    (append '(("Name (upload here)" "Download URL" Inspect "Make Headers") hline)
            (cl-loop for i in assignments
                     collect `( ,(format "%s"
                                         (if (plist-get (cdr i) :html_url)
                                             (concat "[[" (org-lms-safe-pget (cdr i) :html_url) "][" (org-lms-safe-pget (cdr i) :name) "]]")
                                           (org-lms-safe-pget (cdr i) :name)) ) 
                                ,(format "%s"
                                         (if (plist-get (cdr i) :submissions_download_url)
                                             (concat "[[" (org-lms-safe-pget (cdr i) :submissions_download_url) "][Download Submissions]]")
                                           " ")
                                         )
                                ,(format
                                  "%s"
                                  (if (plist-get (cdr i) :canvasid)
                                      (concat  "[[elisp:(org-lms-canvas-inspect \"courses/"
                                               (format "%s" cid)
                                               "/assignments/"
                                               (format "%s" (org-lms-safe-pget (cdr i) :canvasid))
                                               "\")][Inspect Original JSON]]")
                                    " "))
                                ;; "Inspect Original JSON"
                                ,(format "[[%s][%s]]"
                                         (concat "elisp:(org-lms-make-headings (alist-get '"
                                                 (symbol-name (car i))
                                                 " org-lms-merged-assignments) org-lms-merged-students)"
                                                 ) 
                                         "Make Headlines"))))

    ))
;; Assignments Table:1 ends here

;; [[file:org-lms.org::*Transformer -- map org file to json][Transformer -- map org file to json:1]]
(defun org-lms-map-assignments (&optional file )
    "turn a buffer of assignment objects into a plist with relevant info enclosed."

    (let ((old-buffer (current-buffer)))
      (save-window-excursion
        (with-temp-buffer 
          (if file (find-file file) ;; (insert-file-contents (expand-file-name file))
            old-buffer;; (insert-buffer-substring-no-properties old-buffer)
            )
          ;; (insert-file-contents file)
          (org-mode)
          (let* ((id (org-lms-get-keyword "ORG_LMS_COURSEID"))
                 (results '())
                 (org-use-tag-inheritance nil)
                 )
            (message "BUFFER STRING SHOULD BE: %s" (buffer-string))
            (setq results 
                  (org-map-entries
                   (lambda ()
                     (let* ((rubric )
                            (name (nth 4 (org-heading-components)))
                            (a-symbol (intern (or (org-entry-get nil  "ORG_LMS_ANAME") 
                                                  (replace-regexp-in-string "[ \n\t]" "" name)))))
                       (setq rubric  (car (org-map-entries
                                           (lambda ()
                                             (let ((e (org-element-at-point )))
                                               ;; in case at some point we would rather have thewhole element (scary)
                                               ;; (org-element-at-point)
                                               (buffer-substring-no-properties
                                                (org-element-property :contents-begin e)
                                                (-  (org-element-property :contents-end e) 1))
                                               ))
                                           "rubric" 'tree))  )
                       ;; hopefully nothing broeke here w/ additions <2018-11-16 Fri>
                       `(,a-symbol .  (:courseid ,id :canvasid ,(org-entry-get nil "CANVASID")
                                                 :due-at ,(org-entry-get nil "DUE_AT") :html_url ,(org-entry-get nil "CANVAS_HTML_URL")
                                                 :name ,(nth 4 (org-heading-components)  ) 
                                                 :submission_type ,(or (org-entry-get nil "SUBMISSION_TYPE") "online_upload") 
                                                 :published ,(org-entry-get nil "OL_PUBLISH")
                                                 :submission_url ,(org-entry-get nil "CANVAS_SUBMISSION_URL")
                                                 :basecommit ,(org-entry-get nil "BASECOMMIT")
                                                 :org_lms_email_comments ,(org-entry-get nil "ORG_LMS_MAIL_COMMENTS")
                                                 :org_lms_canvas_comments ,(org-entry-get nil "ORG_LMS_CANVAS_COMMENTS")
                                                 :assignment_number ,(org-entry-get nil "ORG_LMS_NUMBER")
                                                 :grade_type "letter_grade" ;; oops fix this!
                                                 :assignment_weight ,(org-entry-get nil "ASSIGNMENT_WEIGHT")
                                                 :assignment-type ,(org-entry-get nil "ASSIGNMENT_TYPE")
                                                 :org-id ,(org-id-get-create)
                                                 :directory ,(or (org-entry-get nil "OL_DIRECTORY")
                                                                 (downcase
                                                                  (replace-regexp-in-string "[\s]" "-" name )))
                                                 :rubric ,rubric)))
                     ) "assignment"))
            ;;(message "RESULT IS: %s" results)
            results)))) )

  (defun org-lms-save-assignment-map (&optional file)
    "Map assignments and save el object to FILE, \"assignments.el\" by default."
    (interactive)
    (unless file (setq file (expand-file-name "assignments.el")))
    (let ((output (org-lms-map-assignments)))
      (with-temp-file (expand-file-name "assignments.el")

        (prin1 output (current-buffer))  )) )

(defun org-lms-read-assignment-map (&optional file)
  "Read assignments map from optional FILE, `assignments.el' by default."
  (unless file (setq file (expand-file-name "assignments.el")))
(with-temp-buffer
  (insert-file-contents (expand-file-name file))
  (cl-assert (eq (point) (point-min)))
  (read (current-buffer)))
)
;; Transformer -- map org file to json:1 ends here

;; [[file:org-lms.org::*Creator -- making assignments][Creator -- making assignments:1]]
;; (defun my-org-element-create (title)
;;   (interactive)
;;   (let* ((email t)
;;         (canvas t)
;;         (type "canvas")
;;         (weight "0.10")
;;         (submission "(online_upload)")
;;         (publish "hello")
;;         (standard nil)
;;         (level 1)
;;         (tags  )
;;         (export (replace-regexp-in-string "[ ,::]" "-" (downcase title)))
;;         )
;;     (message "hello")
;;     ;;(org-insert-heading-after-current)
;;     (org-element-create 'headline
;;                         (list :raw-value title :title title :level level :ORG_LMS_EMAIL_COMMENTS email
;;                               :ORG_LMS_CANVAS_COMMENTS canvas :ASSIGNMENT_TYPE type
;;                               :EXPORT_FILE_NAME export :GRADING_STANDARD_ID standard
;;                               :PUBLISH t :OL_PUBLISH t :ASSIGNMENT_WEIGHT weight)
;;                         )
;;     ))
;; (setq temp-el (my-org-element-create "my title"))
;; (org-element-interpret-data temp-el)


;; (replace-regexp-in-string "[ ,::]" "-" (downcase "My TItle"))

;; (org-ml-build-headline :title title :level level :ORG_LMS_EMAIL_COMMENTS email
;;                        :ORG_LMS_CANVAS_COMMENTS canvas :ASSIGNMENT_TYPE type
;;                        :EXPORT_FILE_NAME export :GRADING_STANDARD_ID standard
;;                        :PUBLISH t :OL_PUBLISH t :ASSIGNMENT_WEIGHT weight)

(defun org-lms-assignment-add-headline-create (title)
"Template for making an assignment with default values"
  (interactive "sAssignment title: ")
  (let* ((email "t")
         (canvas "t")
         (type "canvas")
         (weight "0.10")
         (submission "(online_upload)")
         (publish "hello")
         (standard "nil")
         (position "nil")
         (group "")
         (level 1)
         (dueat (format-time-string "%Y-%m-%d" (time-add (current-time) (* 7 24 2600))))
         (export (replace-regexp-in-string "[ ,::]" "-" (downcase title)))
         (allprops (list "ORG_LMS_EMAIL_COMMENTS" email
                         "ORG_LMS_CANVAS_COMMENTS"  canvas "ASSIGNMENT_TYPE" type
                         "DUE_AT" dueat
                         "EXPORT_FILE_NAME" export "GRADING_STANDARD_ID" standard
                         "PUBLISH" "t" "OL_PUBLISH" "t" "ASSIGNMENT_WEIGHT" weight
                         "ASSIGNMENT_GROUP" group "ASSIGNMENT_POSITION" position
                         "ASSIGNMENT_OMIT" nil ))
         )
    ;;(org-insert-heading-after-current)
    (org-insert-heading nil nil t)
    (insert title)
    (cl-loop for (propname value) on allprops by 'cddr
             do
             (org-entry-put nil propname value))
    (org-set-tags "assignment")
    (while (< 1 (nth 1 (org-heading-components))) (org-promote-subtree))
    ))

;; (my-org-headline-create "test")
;; Creator -- making assignments:1 ends here

;; [[file:org-lms.org::*First, let's define a ~let=plist~ macro that should make our lives easier][First, let's define a ~let=plist~ macro that should make our lives easier:1]]
;; let-plist definition taken fro
;; https://github.com/alphapapa/emacs-package-dev-handbook/issues/7#issuecomment-475969263
(defvar let-plist--symbol-prefix "$")

(defun let-plist--remove-prefix (symbol)
  "Return SYMBOL, without an initial prefix.
Which prefix in particular is `let-plist--symbol-prefix'."
  (let ((name (symbol-name symbol))
        (regexp (eval `(rx bos ,let-plist--symbol-prefix) t)))
    (if (string-match regexp name)
        (intern (replace-match "" nil nil name))
      symbol)))

(defun let-plist--list-to-sexp (list var)
  "Turn symbols LIST into recursive calls to `plist-get' on VAR."
  `(plist-get ,(if (cdr list)
                   (let-plist--list-to-sexp (cdr list) var)
                 var)
              ',(car list)))

(defun let-plist--access-sexp (symbol variable)
  "Return a sexp used to access SYMBOL inside VARIABLE."
  (let* ((clean (let-plist--remove-prefix symbol))
         (name (symbol-name clean))
         (regexp (eval `(rx bol ,let-plist--symbol-prefix) t)))
    (if (string-match regexp name)
        clean
      (let-plist--list-to-sexp
       (mapcar #'intern (thread-last (eval `(rx ,let-plist--symbol-prefix) t)
                          (split-string name)
                          (nreverse)))
       variable))))

(defun let-plist--deep-var-search (data)
  "Return alist of symbols inside DATA that start with
`let-plist--symbol-prefix'. Perform a deep search and return an alist where each
car is the symbol, and each cdr is the same symbol without the
`let-plist--symbol-prefix'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data))
          (regexp (eval `(rx bol ,let-plist--symbol-prefix) t)))
      (when (string-match regexp name)
        ;; Return the cons cell inside a list, so it can be appended
        ;; with other results in the clause below.
        (list (cons data (intern (replace-match "" nil nil name)))))))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.  See Bug#24641.
    (let-plist--deep-var-search (cadr data)))
   (t (append (let-plist--deep-var-search (car data))
              (let-plist--deep-var-search (cdr data))))))

(defmacro let-plist (plist &rest body)
  (declare (indent 1))
  (let ((plist-var (gensym)))
    `(let ((,plist-var ,plist))
       (let ,(cl-loop for (dotvar . var) in (delete-dups (let-plist--deep-var-search body))
                      collect `(,dotvar ,(let-plist--access-sexp dotvar plist-var)))
         ,@body))))


;; alternative implementation, with some broken parts
;; First, let's define a ~let=plist~ macro that should make our lives easier:1 ends here

;; [[file:org-lms.org::*exporting lectures to reveal presentations and uploading to Quercus in a Lecture Folder][exporting lectures to reveal presentations and uploading to Quercus in a Lecture Folder:1]]
;; copied directly from ox-hugo;
;;this function is therefore copyright Kaushal Modi
(defun org-lms--get-valid-subtree (&optional pred)
  "Return the Org element for a valid subtree.
The default condition to check validity is that the EXPORT_FILE_NAME
property is defined for the subtree element, but optional argument
PRED will override that.  

this function is intended to be called inside a valid org-lms subtree, 
doing so also moves the point to the beginning of
the heading of that subtree.

Return nil if a valid org-lms subtree is not found.  The point
will be moved in this case too."
  (catch 'break
    (while :infinite
      (let* ((entry (org-element-at-point))
             (fname (org-string-nw-p (org-element-property :EXPORT_FILE_NAME entry)))
             level)
        ;;(cl-flet)  
        (when (if pred (funcall pred entry)
                fname)
          (throw 'break entry))
          ;; Keep on jumping to the parent heading if the current
          ;; entry does not have an EXPORT_FILE_NAME property.

        (setq level (org-up-heading-safe))
          ;; If no more parent heading exists, break out of the loop
          ;; and return nil

        (unless level
          (throw 'break nil))))))

(defun org-lms-export-reveal-wim-to-html (&optional all-subtrees async visible-only noerror)
  "Export the current subtree/all subtrees/current file to a Canvas file.

This is an Export \"What I Mean\" function:

- If the current subtree has the \"EXPORT_FILE_NAME\" property, export
  that subtree.
- If the current subtree doesn't have that property, but one of its
  parent subtrees has, then export from that subtree's scope.
- If none of the subtrees have that property (or if there are no Org
  subtrees at all), but the Org #+title keyword is present,
  export the whole Org file as a post with that title (calls
  `org-huveal-export-to-html' with its SUBTREEP argument set to nil).

- If ALL-SUBTREES is non-nil, export all valid Hugo post subtrees
  \(that have the \"EXPORT_FILE_NAME\" property) in the current file
  to multiple Markdown posts.
- If ALL-SUBTREES is non-nil, and again if none of the subtrees have
  that property (or if there are no Org subtrees), but the Org #+title
  keyword is present, export the whole Org file.

- If the file neither has valid Hugo post subtrees, nor has the
  #+title present, throw a user error.  If NOERROR is non-nil, use
  `message' to display the error message instead of signaling a user
  error.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

If ALL-SUBTREES is nil, return output file's name.
If ALL-SUBTREES is non-nil, and valid subtrees are found, return
a list of output files.
If ALL-SUBTREES is non-nil, and valid subtrees are not found,
return the output file's name (exported using file-based
approach)."
  (interactive "P")
  (let ((f-or-b-name (if (buffer-file-name)
                         (file-name-nondirectory (buffer-file-name))
                       (buffer-name)))
        (org-re-reveal-single-file t)
        (org-reveal-single-file t))
    (save-window-excursion
      (save-restriction
        (widen)
        (save-excursion
          ;; oops, this looks pretty buggy, will make an md file.  need to correct
          ;;
          (if all-subtrees
              (let (ret (iteration 0)
                    )
                (setq org-hugo--subtree-count 0)
                (setq ret (org-map-entries
                           (lambda ()
                             (setq iteration (+ 1 iteration))
                             (message "iteration: %s" iteration)
                             (org-lms-export-reveal-wim-to-html
                              nil async visible-only noerror)
                             (message "%s" (or (org-entry-get (point) "CUSTOM_ID")
                                               (org-entry-get (point) "ID")))
                             ;; (sleep-for 6)
                             )
                           ;; Export only the subtrees where
                           ;; EXPORT_FILE_NAME property is not
                           ;; empty.
                           "EXPORT_FILE_NAME<>\"\""))
                (if ret
                    (message "[org-lms] Exported %d subtree%s from %s"
                             org-hugo--subtree-count
                             (if (= 1 org-hugo--subtree-count) "" "s")
                             f-or-!b-name)
                  ;; If `ret' is nil, no valid Hugo subtree was found.
                  ;; So call `org-lms-export-reveal-wim-to-html' directly.  In
                  ;; that function, it will be checked if the whole
                  ;; Org file can be exported.
                  (setq ret (org-lms-export-reveal-wim-to-html
                             nil async visible-only noerror)))
                (setq org-hugo--subtree-count nil) ;Reset the variable
                ret)
              
            ;; Upload only the current subtree
            (ignore-errors
              (org-back-to-heading :invisible-ok))
            (let* ((subtree (org-lms--get-valid-subtree))
                   (info (org-combine-plists
                          (org-export--get-export-attributes
                           're-reveal subtree visible-only)
                          (org-export--get-buffer-attributes)
                          (org-export-get-environment 're-reveal subtree)))
                   (exclude-tags (plist-get info :exclude-tags))
                   is-commented is-excluded matched-exclude-tag do-export)
              ;; (message "[org-hugo-export-wim-to-md DBG] exclude-tags = %s" exclude-tags)
              (if subtree
                  (progn
                    ;; If subtree is a valid Hugo post subtree, proceed ..
                    (setq is-commented (org-element-property :commentedp subtree))

                    (let ((all-tags (let ((org-use-tag-inheritance t))
                                      (org-hugo--get-tags))))
                      (when all-tags
                        (dolist (exclude-tag exclude-tags)
                          (when (member exclude-tag all-tags)
                            (setq matched-exclude-tag exclude-tag)
                            (setq is-excluded t)))))

                    ;; (message "[current subtree DBG] subtree: %S" subtree)
                    ;; (message "[current subtree DBG] is-commented:%S, tags:%S, is-excluded:%S"
                    ;;          is-commented tags is-excluded)
                    (let ((title (org-element-property :title subtree)))
                      (cond
                       (is-commented
                        (message "[org-lms] `%s' was not exported as that subtree is commented"
                                 title))
                       (is-excluded
                        (message "[org-lms] `%s' was not exported as it is tagged with an exclude tag `%s'"
                                 title matched-exclude-tag))
                       (t
                        ;; commenting this ount as well until I can manage all-subtrees as a case
                        (if (numberp org-hugo--subtree-count)
                            (progn
                              (setq org-hugo--subtree-count (1+ org-hugo--subtree-count))
                              (message "[org-lms] %d/ Exporting `%s' .." org-hugo--subtree-count title))
                          (message "[org-lms] Exporting `%s' .." title))


                        ;; Get the current subtree coordinates for
                        ;; auto-calculation of menu item weight, page
                        ;; or taxonomy weights.
                        ;; there might be some similar values that it would be worth
                        ;; putting in here.  Maybe!
                        ;; (when (or
                        ;;        ;; Check if the menu front-matter is specified.
                        ;;        (or
                        ;;         (org-entry-get nil "EXPORT_HUGO_MENU" :inherit)
                        ;;         (save-excursion
                        ;;           (goto-char (point-min))
                        ;;           (let ((case-fold-search t))
                        ;;             (re-search-forward "^#\\+hugo_menu:.*:menu" nil :noerror))))
                        ;;        ;; Check if auto-calculation is needed
                        ;;        ;; for page or taxonomy weights.
                        ;;        (or
                        ;;         (let ((page-or-taxonomy-weight (org-entry-get nil "EXPORT_HUGO_WEIGHT" :inherit)))
                        ;;           (and (stringp page-or-taxonomy-weight)
                        ;;                (string-match-p "auto" page-or-taxonomy-weight)))
                        ;;         (save-excursion
                        ;;           (goto-char (point-min))
                        ;;           (let ((case-fold-search t))
                        ;;             (re-search-forward "^#\\+hugo_weight:.*auto" nil :noerror)))))
                        ;;   (setq org-hugo--subtree-coord
                        ;;         (org-hugo--get-post-subtree-coordinates subtree)))
                        (setq do-export t)))))
                ;; If not in a valid subtree, check if the Org file is
                ;; supposed to be exported as a whole, in which case
                ;; #+title has to be defined *and* there shouldn't be
                ;; any valid Hugo post subtree present.
                (setq org-hugo--subtree-count nil) ;Also reset the subtree count
                ;; having trouble with this code I think
                (let ((valid-subtree-found 
                       (catch 'break
                         (org-map-entries
                          (lambda ()
                            (throw 'break t))
                          ;; Only map through subtrees where
                          ;; EXPORT_FILE_NAME property is not
                          ;; empty.
                          "EXPORT_FILE_NAME<>\"\""))
                       )
                      err msg)
                  (if valid-subtree-found
                      (setq msg "Point is not in a valid Hugo post subtree; move to one and try again")
                    (let ((title (save-excursion
                                   (goto-char (point-min))
                                   (let ((case-fold-search t))
                                     (re-search-forward "^#\\+title:" nil :noerror)))))
                      (if title
                          (setq do-export t)
                        (setq err t)
                        (setq msg (concat "The file neither contains a valid Hugo post subtree, "
                                          "nor has the #+title keyword")))))
                  (unless do-export
                    (let ((error-fn (if (or (not err)
                                            noerror)
                                        #'message
                                      #'user-error)))
                      (apply error-fn
                             (list
                              (format "%s: %s" f-or-b-name msg)))))))
              (when do-export
                (let ((org-re-reveal-single-file t)
                      (exported-file
                       (org-re-reveal-export-to-html async subtree visible-only nil
                                                     '(:org-re-reveal-single-file t :re-reveal-single-file t :reveal-single-file t))))
                  (if exported-file
                      (let ((file-info)
                            (file-location)
                            (file-view-in-canvas)
                            (file-folder (or (org-entry-get (point) "ORG_LMS_FILE_FOLDER")
                                             "Lectures")))
                        (setq file-info
                              (json-read-from-string (org-lms-post-new-file exported-file nil file-folder ))
                              file-location (format "%s%s" org-lms-public-baseurl (alist-get   'preview_url file-info))
                              file-actual-preview (and file-location
                                                       (replace-regexp-in-string
                                                        "\\(/.*/\\)\\(file_preview\\?\\)\\(.*\\)"
                                                        "\\1"
                                                        file-location)))
                        (message "PREVIEW: %s" file-location)
                        (when file-location
                          (org-entry-put (point) "CANVASID"
                                        (format "%s" (alist-get 'id file-info)))
                          (org-entry-put (point) "ORG_LMS_PREVIEW_URL"
                                         file-actual-preview)
                          (org-entry-put (point) "ORG_LMS_FILE_URL"
                                         file-location))
                        file-info
                        )))
                
                ))))))))
;; exporting lectures to reveal presentations and uploading to Quercus in a Lecture Folder:1 ends here

;; [[file:org-lms.org::*Debugging][Debugging:1]]
(defun org-lms-inspect-object (method url headers)
    (restclient-http-do method url headers
     ))
;; Debugging:1 ends here

;; [[file:org-lms.org::*Debugging][Debugging:2]]

;; Debugging:2 ends here

;; [[file:org-lms.org::*MAYBE Canvas-inspect][MAYBE Canvas-inspect:1]]
(defun org-lms-canvas-inspect (query &optional request-type request-params)
  "Send QUERY to `org-lms-baseurl' with http request type `type', using `org-lms-token' to authenticate.

Return an error if `org-lms-oauth' is unset. Otherwise return a list whose car is a parsed json
payload and whose cdr is an error message. The data payload will be a list, produced by `json-read' 
with thefollowing settings:

`json-array-type' 'list
`json-object-type' 'plist
`json-key-type' 'symbol

maybe key-type needs to be keyword though! Still a work in progress.
"
  (unless request-type
    (setq request-type "GET"))
  (let ((canvas-payload nil)
        (canvas-err nil)
        (canvas-status nil)

        )
    ;; (message (concat org-lms-baseurl query))
    ;; (message (concat "Bearer " org-lms-token))
    ;; (message "%s" `(("Authorization" . ,(concat "Bearer " org-lms-token))))
    (if org-lms-token
        (progn
          (setq thisrequest
                (request
                 (concat org-lms-baseurl query)
                 :type request-type
                 :headers `(("Authorization" . ,(concat "Bearer " org-lms-token)))
                 :sync t
                 :data (if  request-params request-params nil)
                 :parser 'buffer-string
                 :success (cl-function
                           (lambda (&key data &allow-other-keys)
                             (setq canvas-payload data)
                             (when data
                               (with-current-buffer (get-buffer-create "*request demo*")
                                 (erase-buffer)
                                 (insert data)
                                 (pop-to-buffer (current-buffer))
                                 (json-mode)
                                 (json-mode-beautify))))
                           )
                 :error (cl-function (lambda (&rest args  &key error-thrown &allow-other-keys)
                                       (setq canvas-err error-thrown)
                                       (message "ERROR: %s" error-thrown)))
                 ))
          ;; (message "pPAYLOAD: %s" canvas-payload)
          (if (request-response-data thisrequest)
              canvas-payload
            (error (format "NO PAYLOAD: %s" canvas-err)))
          ) 
      (user-error "Please set a value for for `org-lms-token' in order to complete API calls"))))
;; MAYBE Canvas-inspect:1 ends here

;; [[file:org-lms.org::*Paste zoom recording links][Paste zoom recording links:1]]
(defun org-lms-munge-zoom-link ()
  "Turn the zoom link from an email into an org list item"
  (interactive)
  (let ((today (format-time-string "%B %d"))
        (s (or (and (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))
               (current-kill 0 t))))
    (insert   (replace-regexp-in-string "^.*?\\(https://.*?\\)\\s-+(?\\(Passcode: \\)\\(.*?\\))? ?$" ;; "^.*?\\(https://.*?\\)\\s-+\\((?Passcode: \\)\\(.*?\\)) ?$" 
                                        (concat "- [[\\1][Recording from " today ":]] (\\2 *\\3*)") s))))
;; Paste zoom recording links:1 ends here

;; [[file:org-lms.org::*Completely ad-hoc function designed exclusively for my own purposes][Completely ad-hoc function designed exclusively for my own purposes:1]]
(defun org-lms-visit-gradebook ()
  "visit the online gradebook location"
  (interactive)
  (let* ((id (org-entry-get (point) "STUDENTID") )
         (assignment (org-entry-get (point) "ASSIGNMENTID" t ))
         (course (org-entry-get (point) "COURSEID" t))
         (url (concat org-lms-public-baseurl "courses/" course
                      "/gradebook/speed_grader?assignment_id=" assignment "&student_id=" id)))
    (browse-url url)))
;; Completely ad-hoc function designed exclusively for my own purposes:1 ends here

;; [[file:org-lms.org::*Completely ad-hoc function designed exclusively for my own purposes][Completely ad-hoc function designed exclusively for my own purposes:2]]
(defun org-lms-announcement-wim ()
  "move point to top level subtree, then, since we want 
to keep announcement creation super-lightweight, *always* export that 
headline. Need to decide whether this is the best course of action of course.  gaah."
  (interactive)
  ;; don't test here because we'll actually do it below.
  ;;(if (string= (org-lms-get-keyword "ORG_LMS_SECTION") "announcement"))
  (save-excursion
    (let ((subtree (org-lms--get-valid-subtree)))
      (org-lms-headline-to-announcement)
      )))

(defun org-lms-subtree-to-slack-wim ()
  "don't move point to top level subtree, since we want 
to keep announcement creation super-lightweight. *always*  
copy that subtree as slack text for posting to slack."
  (interactive)
    (save-excursion
    (let (;;(subtree (org-lms--get-valid-subtree))
          )
      (org-mark-subtree)
      (org-slack-export-to-clipboard-as-slack)
      )))

(defun isassignment (entry)
  (member "assignment" (org-get-tags )))
(defun org-lms-assignment-wim ()
  "post current asisgnment to org-lms as an assignment, using wim criteria"
  (interactive)
  (save-window-excursion
    (widen)
    (save-excursion
      (let* (
             (subtree (org-lms--get-valid-subtree 'isassignment)))
        (if subtree
            (progn
              (org-lms-post-assignment-and-save)              )
          (message "Couldn't find a valid subtree!!! Not posted."))
        ))))

;; TODO: detect if grade should be boolean or normal
(defun org-lms-grades-wim ()
  "set grade in current subtree, set state to ready, and advance to next grade"
  (interactive)
  (org-lms-set-grade)
  ;;(org-todo "READY")
  ;;(org-forward-heading-same-level 1)
  )

(defun org-lms-wim-wim ()
  "test for org-lms-section, then perform the appropriate wim function"
  (interactive)
  (pcase (org-lms-get-keyword "ORG_LMS_SECTION")
    ((pred (string= "announcement")) (org-lms-announcement-wim))
    ((pred (string= "assignment")) (org-lms-assignment-wim))
    ((pred (string= "slides")) (org-lms-slides-wim))
    ((pred (string= "grades")) (org-lms-grades-wim))
    ((pred (string= "lecture")) (org-lms-export-reveal-wim-to-html))
    ((pred (string= "syllabus")) (org-lms-post-syllabus))
    (- (progn (message "no section foind, please set the \"ORG_LMS_SECTION\" keyword.") nil)))
  )
;; Completely ad-hoc function designed exclusively for my own purposes:2 ends here

;; [[file:org-lms.org::*Preliminary mode defn][Preliminary mode defn:1]]
;; Minor mode definition. I'm not really using it right now, but it
;; might be a worthwhile improvement.
(defvar org-lms-keymap-prefix (kbd "C-c o")
        "`org-lms-mode` keymap prefix")

(defun org-lms-get-and-open-attachment ()
  "look for attachments; if they don't exist, try to fetch them, and
if that succeeds, open them"
  (interactive)
  (unless (and (org-attach-dir)
               (org-attach-file-list (org-attach-dir)))
    (org-lms-get-canvas-attachments))
  (if (and (org-attach-dir)
           (org-attach-file-list (org-attach-dir)))
      (org-attach-open)
    (message "unable to retrieve attachments from canvas lms")))

(define-minor-mode org-lms-mode
  "a mode to get my grading and other lms interacitons in order"
  :init-value nil
  :global nil
  :keymap  (let ((map (make-sparse-keymap))) 
             (define-key map (kbd "C-c C-x C-g") 'org-lms-set-grade )
             (define-key map (kbd "C-c <f12>") 'org-lms-wim-wim)
             (define-key map (kbd "C-c o a") 'org-lms-post-assignment)
             (define-key map (kbd "C-c o p") 'org-lms-post-page)
             (define-key map (kbd "C-c o n") 'org-lms-headline-to-announcement)
             (define-key map (kbd "C-c o i") 'org-lms-module-item-from-headline)
             (define-key map (kbd "C-c o o") 'org-lms-get-and-open-attachment)
             (define-key map (kbd "C-c o z") 'org-lms-munge-zoom-link)
             map )
  :lighter " LMS"
  ;;(mwp-toggle-macros)
  (if org-lms-mode
      (progn
        (add-hook 'org-ctrl-c-ctrl-c-final-hook 'org-lms-wim-wim))

    (remove-hook 'org-ctrl-c-ctrl-c-hinal-hook 'org-lms-wim-wim))
  )
;; Preliminary mode defn:1 ends here

;; [[file:org-lms.org::*library closing][library closing:1]]
;; Optional migration module
(require 'org-lms-migration nil t)

(provide 'org-lms)
;;; org-lms ends here
;; Quiz API Functions:1 starts here

;; [[file:org-lms.org::*Quiz API Functions][Quiz API Functions:1]]

;; Quiz Management Functions
(defun org-lms-get-quizzes (&optional courseid)
  "Retrieve all quizzes for course with id COURSEID."
  (unless courseid
    (setq courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request (format "courses/%s/quizzes" courseid) "GET"))

(defun org-lms-get-single-quiz (quizid &optional courseid)
  "Retrieve single quiz with QUIZID from course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request (format "courses/%s/quizzes/%s" courseid quizid) "GET"))

(defun org-lms-post-quiz ()
  "Create or update a quiz from current headline properties."
  (interactive)
  (let* ((canvasid (org-entry-get nil "QUIZ_ID"))
         (courseid (org-lms-get-keyword "ORG_LMS_COURSEID"))
         (title (nth 4 (org-heading-components)))
         (description (org-lms-export-quiz-description))
         (quiz-type-raw (or (org-entry-get nil "QUIZ_TYPE") "assignment"))
         (quiz-type (if (member quiz-type-raw '("practice_quiz" "assignment" "graded_survey" "survey"))
                        quiz-type-raw
                      (progn
                        (message "Warning: Invalid QUIZ_TYPE '%s'. Using 'assignment' instead. Valid types: practice_quiz, assignment, graded_survey, survey" quiz-type-raw)
                        "assignment")))
         (time-limit (org-entry-get nil "TIME_LIMIT"))
         (allowed-attempts (org-entry-get nil "ALLOWED_ATTEMPTS"))
         (scoring-policy (or (org-entry-get nil "SCORING_POLICY") "keep_highest"))
         (shuffle-answers (org-entry-get nil "SHUFFLE_ANSWERS"))
         (show-correct-answers (org-entry-get nil "SHOW_CORRECT_ANSWERS"))
         (due-date (org-entry-get nil "DUE_AT"))
         (unlock-date (org-entry-get nil "UNLOCK_AT"))
         (lock-date (org-entry-get nil "LOCK_AT"))
         (published (org-entry-get nil "OL_PUBLISH"))
         (points-possible (or (org-entry-get nil "POINTS_POSSIBLE") "0"))
         
         (quiz-params `(("title" . ,title)
                       ("description" . ,description)
                       ("quiz_type" . ,quiz-type)
                       ("points_possible" . ,(string-to-number points-possible))
                       ("published" . ,(if published t nil)))))

    ;; Add optional parameters if they exist
    (when time-limit
      (add-to-list 'quiz-params `("time_limit" . ,(string-to-number time-limit))))
    (when allowed-attempts
      (add-to-list 'quiz-params `("allowed_attempts" . ,(string-to-number allowed-attempts))))
    (when scoring-policy
      (add-to-list 'quiz-params `("scoring_policy" . ,scoring-policy)))
    (when shuffle-answers
      (add-to-list 'quiz-params `("shuffle_answers" . ,(if (string= shuffle-answers "t") t nil))))
    (when show-correct-answers
      (add-to-list 'quiz-params `("show_correct_answers" . ,(if (string= show-correct-answers "t") t nil))))
    (when due-date
      (add-to-list 'quiz-params `("due_at" . ,(o-l-date-to-timestamp due-date))))
    (when unlock-date
      (add-to-list 'quiz-params `("unlock_at" . ,(o-l-date-to-timestamp unlock-date))))
    (when lock-date
      (add-to-list 'quiz-params `("lock_at" . ,(o-l-date-to-timestamp lock-date))))

    (let* ((final-params `(("quiz" . ,quiz-params)))
           (response (org-lms-canvas-request
                     (format "courses/%s/quizzes%s"
                            courseid
                            (if canvasid (format "/%s" canvasid) ""))
                     (if canvasid "PUT" "POST")
                     final-params)))
      
      (when (plist-get response :id)
        (org-set-property "QUIZ_ID" (format "%s" (plist-get response :id)))
        (org-set-property "QUIZ_HTML_URL" (format "%s" (plist-get response :html_url)))
        (org-set-property "QUIZ_MOBILE_URL" (format "%s" (plist-get response :mobile_url)))
        (org-set-property "QUIZ_PREVIEW_URL" (format "%s" (plist-get response :preview_url)))
        (org-set-property "QUIZ_TYPE" quiz-type)
        (org-set-property "ORG_LMS_CATEGORY" "Quiz"))
      
      response)))

;; Quiz Question Group Management Functions
(defun org-lms-get-quiz-question-groups (quizid &optional courseid)
  "Retrieve all question groups for quiz QUIZID in course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request (format "courses/%s/quizzes/%s/groups" courseid quizid) "GET"))

(defun org-lms-get-single-quiz-question-group (groupid quizid &optional courseid)
  "Retrieve single question group GROUPID from quiz QUIZID in course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request (format "courses/%s/quizzes/%s/groups/%s" courseid quizid groupid) "GET"))

(defun org-lms-post-quiz-question-group ()
  "Create or update a quiz question group and all its child questions."
  (interactive)
  (let* ((courseid (org-lms-get-keyword "ORG_LMS_COURSEID"))
         (quizid (org-entry-get nil "QUIZ_ID" t))
         (groupid (org-entry-get nil "QUESTION_GROUP_ID"))
         (group-name (nth 4 (org-heading-components)))
         (pick-count (or (org-entry-get nil "PICK_COUNT") "1"))
         (question-points (or (org-entry-get nil "QUESTION_POINTS") "1"))
         (position (org-entry-get nil "GROUP_POSITION"))
         
         (group-params `(("name" . ,group-name)
                        ("pick_count" . ,(string-to-number pick-count))
                        ("question_points" . ,(string-to-number question-points)))))

    ;; Add optional parameters if they exist
    (when position
      (add-to-list 'group-params `("position" . ,(string-to-number position))))

    ;; Create the group first
    (let* ((final-params `(("quiz_groups" . (,group-params))))
           (response (org-lms-canvas-request
                     (format "courses/%s/quizzes/%s/groups%s"
                            courseid quizid
                            (if groupid (format "/%s" groupid) ""))
                     (if groupid "PUT" "POST")
                     final-params))
           (question-responses '())
           (question-count 0))
      
      (message "Group response: %s" response)
      ;; Canvas API returns groups wrapped in quiz_groups array
      (let ((quiz-groups (plist-get response :quiz_groups)))
        (when quiz-groups
          (let ((group-data (if (listp quiz-groups) (car quiz-groups) quiz-groups)))
            (when (plist-get group-data :id)
              (org-set-property "QUESTION_GROUP_ID" (format "%s" (plist-get group-data :id)))
              (message "Set QUESTION_GROUP_ID to: %s" (plist-get group-data :id))
              (when (plist-get group-data :quiz_id)
                (org-set-property "QUIZ_ID" (format "%s" (plist-get group-data :quiz_id))))
              
              ;; Now process child questions within this group
              (message "Processing child questions for group...")
              (org-map-entries
               (lambda ()
                 (when (org-entry-get nil "QUESTION_TYPE")
                   (setq question-count (1+ question-count))
                   (let ((existing-question-id (org-entry-get nil "QUESTION_ID")))
                     (if existing-question-id
                         (message "Updating existing question %d to join group..." question-count)
                       (message "Creating new question %d in group..." question-count))
                     (let ((question-response (org-lms-post-quiz-question)))
                       (if (plist-get question-response :id)
                           (progn
                             (push question-response question-responses)
                             (message "Question %d %s successfully" 
                                     question-count 
                                     (if existing-question-id "updated" "created")))
                         (message "Failed to %s question %d" 
                                 (if existing-question-id "update" "create") 
                                 question-count))))))
               nil 'tree)))))
      
      ;; Return comprehensive response
      `(:group ,response 
        :questions ,(reverse question-responses)
        :question-count ,question-count))))

;; Quiz Question Management Functions
(defun org-lms-get-quiz-questions (quizid &optional courseid)
  "Retrieve all questions for quiz QUIZID in course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request (format "courses/%s/quizzes/%s/questions" courseid quizid) "GET"))

(defun org-lms-get-single-quiz-question (questionid quizid &optional courseid)
  "Retrieve single question QUESTIONID from quiz QUIZID in course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request (format "courses/%s/quizzes/%s/questions/%s" courseid quizid questionid) "GET"))

(defun org-lms-post-quiz-question ()
  "Create or update a quiz question from current headline properties."
  (interactive)
  (let* ((courseid (org-lms-get-keyword "ORG_LMS_COURSEID"))
         (quizid (org-entry-get nil "QUIZ_ID" t))
         (questionid (org-entry-get nil "QUESTION_ID"))
         (question-name (nth 4 (org-heading-components)))
         (question-text-body (org-lms-export-question-text))
         (question-text (if (and question-text-body 
                                (string-match-p "\\S-" question-text-body)
                                (not (string-match-p (regexp-quote question-name) question-text-body)))
                           (format "<p><strong>%s</strong></p>\n%s" question-name question-text-body)
                         (if (and question-text-body (string-match-p "\\S-" question-text-body))
                             question-text-body
                           (format "<p><strong>%s</strong></p>" question-name))))
         (question-type (or (org-entry-get nil "QUESTION_TYPE") "multiple_choice_question"))
         (points-possible (or (org-entry-get nil "QUESTION_POINTS") "1"))
         (correct-comments (org-entry-get nil "CORRECT_COMMENTS"))
         (incorrect-comments (org-entry-get nil "INCORRECT_COMMENTS"))
         (neutral-comments (org-entry-get nil "NEUTRAL_COMMENTS"))
         
         (question-params `(("question_name" . ,question-name)
                           ("question_text" . ,question-text)
                           ("question_type" . ,question-type)
                           ("points_possible" . ,(string-to-number points-possible)))))

    (message "DEBUG: Question name: %s" question-name)
    (message "DEBUG: Question text: %s" (substring question-text 0 (min 100 (length question-text))))

    ;; Add comments if they exist
    (when correct-comments
      (add-to-list 'question-params `("correct_comments" . ,correct-comments)))
    (when incorrect-comments
      (add-to-list 'question-params `("incorrect_comments" . ,incorrect-comments)))
    (when neutral-comments
      (add-to-list 'question-params `("neutral_comments" . ,neutral-comments)))

    ;; Handle answers based on question type
    (when (string= question-type "multiple_choice_question")
      (let ((answers (org-lms-extract-quiz-answers)))
        (when answers
          (add-to-list 'question-params `("answers" . ,answers)))))

    ;; Check if this question belongs to a question group
    (let ((groupid (org-entry-get nil "QUESTION_GROUP_ID" t)))
      (when groupid
        (add-to-list 'question-params `("quiz_group_id" . ,(string-to-number groupid)))))

    (let* ((final-params `(("question" . ,question-params)))
           (response (org-lms-canvas-request
                      (format "courses/%s/quizzes/%s/questions%s"
                              courseid quizid
                              (if questionid (format "/%s" questionid) ""))
                      (if questionid "PUT" "POST")
                      final-params)))
      
      (when (plist-get response :id)
        (org-set-property "QUESTION_ID" (format "%s" (plist-get response :id)))
        (org-set-property "QUESTION_TYPE" (format "%s" (plist-get response :question_type))))
      
      response)))

(defun org-lms-extract-quiz-answers ()
  "Extract quiz answers from the current subtree content.
Looks for answer choices in the format:
- [ ] Wrong answer
- [X] Correct answer
- [ ] Another wrong answer"
  (save-restriction
    (org-narrow-to-subtree)
    (let ((answers '())
          (answer-weight 100))
      (message "DEBUG: Searching for answers in content...")
      (goto-char (point-min))
      (message "DEBUG: Buffer content: %s" (buffer-substring-no-properties (point-min) (point-max)))
      (while (re-search-forward "^[ \t]*- \\[\\([X ]\\)\\] \\(.*\\)$" nil t)
        (let* ((checkbox-content (match-string 1))
               (is-correct (string= checkbox-content "X"))
               (answer-text (match-string 2))
               (weight (if is-correct answer-weight 0))
               (answer `(("answer_text" . ,answer-text)
                        ("answer_weight" . ,weight))))
          (message "DEBUG: Found checkbox [%s] with text '%s', weight %d" checkbox-content answer-text weight)
          (push answer answers)))
      (message "DEBUG: Total answers found: %d" (length answers))
      (reverse answers))))

;; Quiz export helper functions
(defun org-lms-export-quiz-description ()
  "Export quiz description excluding subheadings (questions)."
  (save-restriction
    (org-narrow-to-subtree)
    (save-excursion
      (org-back-to-heading)
      (let ((start (progn 
                     (forward-line 1)
                     (while (looking-at "^[ \t]*:")
                       (forward-line 1))
                     (point)))
            (end (save-excursion
                   (if (outline-next-heading)
                       (point)
                     (point-max)))))
        (let ((content (buffer-substring-no-properties start end)))
          (with-temp-buffer
            (insert content)
            (org-mode)
            (org-export-as 'canvas-html nil nil t)))))))

(defun org-lms-export-question-text ()
  "Export question text excluding answer choices (checkbox lists) and grading notes."
  (save-restriction
    (org-narrow-to-subtree)
    (save-excursion
      (org-back-to-heading)
      (let ((start (progn 
                     (forward-line 1)
                     (while (looking-at "^[ \t]*:")
                       (forward-line 1))
                     (point)))
            (end (save-excursion
                   ;; Find first subheading (including grading notes)
                   (let ((current-level (org-outline-level)))
                     (if (re-search-forward (format "^\\*\\{%d,\\}" (+ current-level 1)) nil t)
                         (progn (beginning-of-line) (point))
                       (point-max))))))
        (let ((content (buffer-substring-no-properties start end)))
          (with-temp-buffer
            (insert content)
            (org-mode)
            ;; Remove checkbox list items (answer choices)
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*- \\[[ xX]\\] .*$" nil t)
              (replace-match ""))
            ;; Clean up extra newlines
            (goto-char (point-min))
            (while (re-search-forward "\n\n\n+" nil t)
              (replace-match "\n\n"))
            (org-export-as 'canvas-html nil nil t)))))))

(defun org-lms-extract-grading-notes ()
  "Extract content from Grading Notes subheading (tagged with grading_note)."
  (save-excursion
    (org-back-to-heading)
    (let ((grading-notes ""))
      (save-restriction
        (org-narrow-to-subtree)
        ;; Look for grading_note tagged subheading
        (when (re-search-forward "^\\*+ .*:grading_note:" nil t)
          (let* ((notes-start (progn
                                (forward-line 1)
                                (point)))
                 (notes-end (save-excursion
                              ;; Find next heading at same or higher level
                              (if (re-search-forward "^\\*+ " nil t)
                                  (progn (beginning-of-line) (point))
                                (point-max))))
                 (notes-content (buffer-substring-no-properties notes-start notes-end)))
            ;; Clean up the content
            (setq grading-notes (string-trim notes-content)))))
      grading-notes)))

(defun org-lms-calculate-quiz-item-position ()
  "Calculate position of current quiz item within its quiz structure.
Returns 1-based position counting all quiz items (questions/groups) in the quiz."
  (save-excursion
    (let ((current-pos (point))
          (position 1))
      ;; Go to the quiz heading (find ancestor with QUIZ tag or QUIZ_ID)
      (while (and (org-up-heading-safe)
                  (not (or (member "QUIZ" (org-get-tags))
                          (org-entry-get nil "QUIZ_ID")
                          (org-entry-get nil "NEW_QUIZ_ID")))))
      
      ;; Now we're at the quiz level, count quiz items before current position
      (org-map-entries
       (lambda ()
         (when (< (point) current-pos)
           ;; Count if this is a quiz item (has QUESTION_TYPE or PICK_COUNT)
           (when (or (org-entry-get nil "QUESTION_TYPE")
                    (org-entry-get nil "PICK_COUNT"))
             (setq position (1+ position)))))
       nil 'tree)
      
      position)))

(defun org-lms-update-quiz-positions ()
  "Update POSITION properties for all quiz items in current quiz to match org structure."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    ;; Make sure we're at the quiz level
    (while (and (org-up-heading-safe)
                (not (or (member "QUIZ" (org-get-tags))
                        (org-entry-get nil "QUIZ_ID")
                        (org-entry-get nil "NEW_QUIZ_ID")))))
    
    (let ((update-count 0))
      (org-map-entries
       (lambda ()
         ;; Update position for quiz items (questions and groups)
         (when (or (org-entry-get nil "QUESTION_TYPE")
                  (org-entry-get nil "PICK_COUNT"))
           (let ((calculated-position (org-lms-calculate-quiz-item-position)))
             (org-set-property "POSITION" (format "%s" calculated-position))
             (setq update-count (1+ update-count))
             (message "Updated position for '%s' to %d" 
                     (nth 4 (org-heading-components)) 
                     calculated-position))))
       nil 'tree)
      
      (message "Updated %d quiz item positions" update-count))))

(defun org-lms-remove-quiz-ids ()
  "Remove QUESTION_GROUP_ID, QUIZ_ID, and QUESTION_ID properties from all headings in current subtree."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((removed-count 0))
      (org-map-entries
       (lambda ()
         (let ((removed-from-heading 0))
           (when (org-entry-get nil "QUESTION_GROUP_ID")
             (org-entry-delete nil "QUESTION_GROUP_ID")
             (setq removed-from-heading (1+ removed-from-heading)))
           (when (org-entry-get nil "QUIZ_ID")
             (org-entry-delete nil "QUIZ_ID")
             (setq removed-from-heading (1+ removed-from-heading)))
           (when (org-entry-get nil "QUESTION_ID")
             (org-entry-delete nil "QUESTION_ID")
             (setq removed-from-heading (1+ removed-from-heading)))
           (when (> removed-from-heading 0)
             (setq removed-count (1+ removed-count))
             (message "Removed %d properties from: %s" removed-from-heading (nth 4 (org-heading-components))))))
       nil 'tree)
      (message "Removed quiz ID properties from %d headings" removed-count))))

;; Quiz debugging utilities
(defun org-lms-debug-quiz-structure ()
  "Debug the current quiz structure showing groups and questions."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (message "=== Quiz Structure Debug ===")
    (message "Quiz: %s" (nth 4 (org-heading-components)))
    (message "Quiz ID: %s" (org-entry-get nil "QUIZ_ID"))
    
    (org-map-entries 
     (lambda ()
       (let ((level (org-outline-level))
             (heading (nth 4 (org-heading-components)))
             (pick-count (org-entry-get nil "PICK_COUNT"))
             (question-type (org-entry-get nil "QUESTION_TYPE"))
             (group-id (org-entry-get nil "QUESTION_GROUP_ID"))
             (question-id (org-entry-get nil "QUESTION_ID")))
         (cond
          (pick-count
           (message "  GROUP (level %d): %s" level heading)
           (message "    PICK_COUNT: %s" pick-count)
           (message "    GROUP_ID: %s" group-id))
          (question-type
           (message "    QUESTION (level %d): %s" level heading)
           (message "      QUESTION_TYPE: %s" question-type)
           (message "      QUESTION_ID: %s" question-id)
           (message "      INHERITED_GROUP_ID: %s" (org-entry-get nil "QUESTION_GROUP_ID" t))))))
     nil 'tree)))

;; Quiz utilities
(defun org-lms-post-quiz-with-questions ()
  "Post the current quiz, question groups, and all questions to Canvas."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (message "Creating quiz...")
    ;; First create the quiz
    (let ((quiz-response (org-lms-post-quiz))
          (question-count 0)
          (group-count 0)
          (question-responses '())
          (group-responses '()))
      (if (plist-get quiz-response :id)
          (progn
            (message "Quiz created successfully. Processing structure...")
            
            ;; Process groups and direct questions (no nesting needed)
            (org-map-entries 
             (lambda ()
               (cond
                ;; Handle question groups (they process their own children)
                ((org-entry-get nil "PICK_COUNT")
                 (setq group-count (1+ group-count))
                 (message "Creating question group %d..." group-count)
                 (let ((group-response (org-lms-post-quiz-question-group)))
                   (if (plist-get group-response :group)
                       (progn
                         (push group-response group-responses)
                         ;; Add child question count to total
                         (setq question-count (+ question-count (plist-get group-response :question-count)))
                         (setq question-responses (append question-responses (plist-get group-response :questions)))
                         (message "Question group %d created successfully with %d questions" 
                                 group-count (plist-get group-response :question-count)))
                     (message "Failed to create question group %d" group-count))))
                
                ;; Handle direct questions (not in groups) - only level 2 headings
                ((and (org-entry-get nil "QUESTION_TYPE")
                      (not (org-entry-get nil "PICK_COUNT" t))
                      (= (org-outline-level) 2))
                 (setq question-count (1+ question-count))
                 (message "Creating direct question %d..." question-count)
                 (let ((question-response (org-lms-post-quiz-question)))
                   (if (plist-get question-response :id)
                       (progn
                         (push question-response question-responses)
                         (message "Question %d created successfully" question-count))
                     (message "Failed to create question %d" question-count))))))
             nil 'tree)
            
            (message "Quiz creation complete: %d groups, %d questions added" group-count question-count)
            ;; Return summary
            `(:quiz ,quiz-response 
              :groups ,(reverse group-responses)
              :questions ,(reverse question-responses)
              :group-count ,group-count
              :question-count ,question-count))
        (message "Failed to create quiz")
        nil))))

(defun org-lms-quiz-from-headline ()
  "Create a quiz and all its questions from the current headline and subheadings."
  (interactive)
  (org-lms-post-quiz-with-questions))

(defun org-lms-quiz-template ()
  "Insert a template for a quiz with sample questions."
  (interactive)
  (let ((quiz-title (read-string "Quiz title: "))
        (due-date (format-time-string "%Y-%m-%d" (time-add (current-time) (* 7 24 3600)))))
    (insert (format "* %s :QUIZ:\n" quiz-title))
    (org-entry-put nil "QUIZ_TYPE" "assignment")
    (org-entry-put nil "TIME_LIMIT" "30")
    (org-entry-put nil "ALLOWED_ATTEMPTS" "1")
    (org-entry-put nil "SCORING_POLICY" "keep_highest")
    (org-entry-put nil "SHUFFLE_ANSWERS" "t")
    (org-entry-put nil "SHOW_CORRECT_ANSWERS" "t")
    (org-entry-put nil "DUE_AT" due-date)
    (org-entry-put nil "POINTS_POSSIBLE" "10")
    (org-entry-put nil "OL_PUBLISH" "nil")
    (insert "\nThis is the quiz description. You can include instructions here.\n\n")
    
    ;; Add sample direct question
    (insert "** Direct Question (not in group)\n")
    (org-entry-put nil "QUESTION_TYPE" "multiple_choice_question")
    (org-entry-put nil "QUESTION_POINTS" "2")
    (org-entry-put nil "CORRECT_COMMENTS" "Excellent! That's the correct answer.")
    (org-entry-put nil "INCORRECT_COMMENTS" "Not quite right. Please review the material.")
    (insert "\nWhat is 2 + 2?\n\n")
    (insert "- [ ] 3\n")
    (insert "- [X] 4\n")
    (insert "- [ ] 5\n")
    (insert "- [ ] 6\n\n")
    
    ;; Add sample question group
    (insert "** Math Problems (Question Group)\n")
    (org-entry-put nil "PICK_COUNT" "2")
    (org-entry-put nil "QUESTION_POINTS" "3")
    (org-entry-put nil "GROUP_POSITION" "1")
    (insert "\nThis group will randomly select 2 questions from the pool below.\n\n")
    
    (insert "*** Addition Problem\n")
    (org-entry-put nil "QUESTION_TYPE" "multiple_choice_question")
    (insert "\n- [ ] 7\n")
    (insert "- [X] 8\n")
    (insert "- [ ] 9\n")
    (insert "- [ ] 10\n\n")
    
    (insert "*** Subtraction Problem\n")
    (org-entry-put nil "QUESTION_TYPE" "multiple_choice_question")
    (insert "\n- [ ] 2\n")
    (insert "- [X] 3\n")
    (insert "- [ ] 4\n")
    (insert "- [ ] 5\n\n")
    
    (insert "*** Multiplication Problem\n")
    (org-entry-put nil "QUESTION_TYPE" "multiple_choice_question")
    (insert "\n- [ ] 10\n")
    (insert "- [X] 12\n")
    (insert "- [ ] 14\n")
    (insert "- [ ] 16\n\n")))

(defun org-lms-insert-short-answer-question ()
  "Insert a new short-answer question header with appropriate structure at point."
  (interactive)
  (let ((question-title (read-string "Question title: "))
        (question-points (read-string "Question points (default 1): " nil nil "3")))
    (insert (format "*** %s\n" question-title))
    (org-entry-put nil "QUESTION_TYPE" "short_answer_question")
    (org-entry-put nil "QUESTION_POINTS" question-points)
    (org-entry-put nil "POSITION" "1")
    (org-entry-put nil "CORRECT_COMMENTS" "Good answer!")
    (org-entry-put nil "INCORRECT_COMMENTS" "Please review this topic.")
    (insert "\nEnter your question text here.\n\n")
    (insert "**** Grading Notes                                       :grading_note:\n")
    (insert "Enter grading instructions for this question here.\n\n")))

(defun org-lms-insert-essay-question ()
  "Insert a new essay question header with appropriate structure at point."
  (interactive)
  (let ((question-title (read-string "Question title: "))
        (question-points (read-string "Question points (default 5): " nil nil "5")))
    (insert (format "*** %s\n" question-title))
    (org-entry-put nil "QUESTION_TYPE" "essay_question")
    (org-entry-put nil "QUESTION_POINTS" question-points)
    (org-entry-put nil "POSITION" "1")
    (org-entry-put nil "CORRECT_COMMENTS" "Excellent essay!")
    (org-entry-put nil "INCORRECT_COMMENTS" "Please review the essay requirements.")
    (insert "\nEnter your essay question text here.\n\n")
    (insert "**** Grading Notes                                       :grading_note:\n")
    (insert "Enter grading criteria and rubric for this essay question here.\n\n")))

(defun org-lms-delete-quiz-question-by-id (question-id quiz-id &optional course-id)
  "Delete quiz question QUESTION-ID from quiz QUIZ-ID in Canvas."
  (setq course-id (or course-id (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-canvas-request 
   (format "courses/%s/quizzes/%s/questions/%s" course-id quiz-id question-id) 
   "DELETE"))

(defun org-lms-delete-current-quiz-question ()
  "Delete the current quiz question from Canvas and remove the heading from buffer."
  (interactive)
  (let ((question-id (org-entry-get nil "QUESTION_ID"))
        (quiz-id (org-entry-get nil "QUIZ_ID" t))
        (question-type (org-entry-get nil "QUESTION_TYPE"))
        (heading (nth 4 (org-heading-components))))
    (if (and question-type question-id quiz-id)
        (when (y-or-n-p (format "Delete question '%s' (ID: %s) from Canvas and buffer? " heading question-id))
          (message "Deleting question from Canvas...")
          (let ((response (org-lms-delete-quiz-question-by-id question-id quiz-id)))
            (if response
                (progn
                  (message "Question deleted from Canvas successfully")
                  ;; Delete the entire subtree from buffer
                  (org-back-to-heading)
                  (org-cut-subtree)
                  (message "Question removed from buffer"))
              (message "Failed to delete question from Canvas"))))
      (message "Current heading is not a quiz question or missing required IDs"))))

;; New Quizzes API Functions
;; New Quiz request helper function
(defun org-lms-new-quiz-request (query &optional request-type request-params)
  "Send QUERY to New Quizzes API with correct base URL."
  (unless request-type (setq request-type "GET"))
  (let* ((new-quiz-baseurl (replace-regexp-in-string "/api/v1/$" "/api/quiz/v1/" org-lms-baseurl))
         (canvas-payload nil)
         (canvas-err nil)
         (canvas-status nil)
         (json-array-type 'list)
         (json-object-type 'plist)  
         (json-key-type 'keyword)
         (json-false nil)
         (json-params (json-encode request-params))
         (target (concat new-quiz-baseurl query)))
    (message "New Quiz target: %s" target)
    (message "New Quiz params: %s" json-params)
    (if org-lms-token
        (progn (setq thisrequest
                     (request
                      target
                      :type request-type
                      :sync t
                      :data json-params
                      :encoding 'utf-8
                      :headers `(("Authorization" . ,(concat "Bearer " org-lms-token))
                                ("Content-Type" . "application/json"))
                      :parser (lambda ()
                                (ol-jsonwrapper json-read))
                      :success (cl-function
                                (lambda (&key data response &allow-other-keys)
                                  (message "NEW QUIZ SUCCESS: %S" data)
                                  (message "Response status: %s" (request-response-status-code response))
                                  (setq canvas-payload data)))
                      :error (cl-function
                              (lambda (&key error-thrown data status response &allow-other-keys)
                                (setq canvas-err error-thrown)
                                (message "NEW QUIZ ERROR: %s" error-thrown)
                                (message "Error status: %s" status)
                                (message "Error data: %s" data)
                                (when response
                                  (message "Response status code: %s" (request-response-status-code response))
                                  (message "Response data: %s" (request-response-data response)))))))
               (unless (request-response-data thisrequest)
                 (message "NO PAYLOAD: %s" canvas-err))
               (or (request-response-data thisrequest) thisrequest))
      (user-error "You must set org-lms-token in order to make Canvas API requests"))))

;; New Quiz Management Functions
(defun org-lms-post-new-quiz ()
  "Create or update a new quiz from current headline properties."
  (interactive)
  (let* ((assignment-id (org-entry-get nil "NEW_QUIZ_ID"))
         (courseid (org-lms-get-keyword "ORG_LMS_COURSEID"))
         (title (nth 4 (org-heading-components)))
         (assignment-group-id (or (org-entry-get nil "ASSIGNMENT_GROUP_ID") "1"))
         (points-possible (or (org-entry-get nil "POINTS_POSSIBLE") "0"))
         (grading-type (or (org-entry-get nil "GRADING_TYPE") "points"))
         (due-date (org-entry-get nil "DUE_AT"))
         (unlock-date (org-entry-get nil "UNLOCK_AT"))
         (lock-date (org-entry-get nil "LOCK_AT"))
         (published (org-entry-get nil "OL_PUBLISH"))
         
         (quiz-inner-params `(("title" . ,title)
                             ("assignment_group_id" . ,(string-to-number assignment-group-id))
                             ("points_possible" . ,(string-to-number points-possible))
                             ("grading_type" . ,grading-type))))
    (message "DEBUG: Course ID = %s" courseid)
    (message "DEBUG: Assignment ID = %s" assignment-id)
    (message "DEBUG: Title = %s" title)
    (unless courseid
      (user-error "Course ID not found! Make sure ORG_LMS_COURSEID is set in your setup file"))

    ;; Add optional parameters if they exist
    (when due-date
      (add-to-list 'quiz-inner-params `("due_at" . ,due-date)))
    (when unlock-date
      (add-to-list 'quiz-inner-params `("unlock_at" . ,unlock-date)))
    (when lock-date
      (add-to-list 'quiz-inner-params `("lock_at" . ,lock-date)))
    (when published
      (add-to-list 'quiz-inner-params `("published" . ,(if (string= published "t") t nil))))

    ;; Add quiz settings if they exist
    (let ((quiz-settings '()))
      (when-let ((time-limit (org-entry-get nil "TIME_LIMIT")))
        (push `("session_time_limit_in_seconds" . ,(* 60 (string-to-number time-limit))) quiz-settings)
        (push `("has_time_limit" . t) quiz-settings))
      (when-let ((allowed-attempts (org-entry-get nil "ALLOWED_ATTEMPTS")))
        (push `("multiple_attempts" . t) quiz-settings)
        (push `("allowed_attempts" . ,(string-to-number allowed-attempts)) quiz-settings))
      (when-let ((shuffle-answers (org-entry-get nil "SHUFFLE_ANSWERS")))
        (push `("shuffle_answers" . ,(string= shuffle-answers "t")) quiz-settings))
      (when quiz-settings
        (add-to-list 'quiz-inner-params `("quiz_settings" . ,quiz-settings))))

    ;; Wrap in quiz object as required by New Quizzes API
    (let* ((quiz-params `(("quiz" . ,quiz-inner-params)))
           (response (org-lms-new-quiz-request
                     (format "courses/%s/quizzes%s"
                            courseid
                            (if assignment-id (format "/%s" assignment-id) ""))
                     (if assignment-id "PATCH" "POST")
                     quiz-params)))
      (message "DEBUG: Final quiz params = %s" quiz-params)
      (message "New Quiz response: %s" response)
      (when (plist-get response :id)
        (org-set-property "NEW_QUIZ_ID" (format "%s" (plist-get response :id)))
        (when (plist-get response :html_url)
          (org-set-property "NEW_QUIZ_HTML_URL" (plist-get response :html_url))))
      response)))

(defun org-lms-get-new-quiz (assignment-id &optional courseid)
  "Retrieve new quiz ASSIGNMENT-ID in course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-new-quiz-request (format "courses/%s/quizzes/%s" courseid assignment-id) "GET"))

(defun org-lms-delete-new-quiz (assignment-id &optional courseid)
  "Delete new quiz ASSIGNMENT-ID in course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-new-quiz-request (format "courses/%s/quizzes/%s" courseid assignment-id) "DELETE"))

;; New Quiz Item Bank Management Functions
(defun org-lms-post-new-quiz-item-bank ()
  "Create or update a new quiz item bank from current headline properties."
  (interactive)
  (let* ((bank-id (org-entry-get nil "ITEM_BANK_ID"))
         (courseid (org-lms-get-keyword "ORG_LMS_COURSEID"))
         (name (nth 4 (org-heading-components)))
         (description (org-lms-export-quiz-description))
         
         (bank-params `(("name" . ,name)
                       ("description" . ,description))))

    (let ((response (org-lms-new-quiz-request
                     (format "courses/%s/item_banks%s"
                            courseid
                            (if bank-id (format "/%s" bank-id) ""))
                     (if bank-id "PATCH" "POST")
                     bank-params)))
      (message "Item Bank response: %s" response)
      (when (plist-get response :id)
        (org-set-property "ITEM_BANK_ID" (format "%s" (plist-get response :id))))
      response)))

;; New Quiz Item Management Functions
(defun org-lms-post-new-quiz-item ()
  "Create or update a new quiz item from current headline properties."
  (interactive)
  (let* ((item-id (org-entry-get nil "NEW_ITEM_ID"))
         (courseid (org-lms-get-keyword "ORG_LMS_COURSEID"))
         (assignment-id (org-entry-get nil "NEW_QUIZ_ID" t))
         (question-type (org-entry-get nil "QUESTION_TYPE"))
         (question-text (org-lms-export-question-text))
         (question-points (or (org-entry-get nil "QUESTION_POINTS") "1"))
         (question-position (org-lms-calculate-quiz-item-position))
         (correct-comments (org-entry-get nil "CORRECT_COMMENTS"))
         (incorrect-comments (org-entry-get nil "INCORRECT_COMMENTS"))
         
         ;; Map classic question types to new quiz item types
         (item-type (cond
                     ((string= question-type "multiple_choice_question") "multiple_choice")
                     ((string= question-type "short_answer_question") "essay_question")
                     ((string= question-type "essay_question") "essay_question")
                     ((string= question-type "true_false_question") "true_false")
                     (t "multiple_choice")))
         
         ;; Build properties (remove duplicated fields that are now at top level)
         (properties `(("title" . ,(nth 4 (org-heading-components)))
                      ("points_possible" . ,(string-to-number question-points))))
         
         ;; Build interaction data based on question type
         (interaction-data (cond
                           ((string= item-type "multiple_choice")
                            `(("prompt" . ,question-text)))
                           ((string= item-type "essay_question")
                            `(("rce" . t)
                              ("essay" . :json-null)
                              ("word_count" . t)
                              ("file_upload" . :json-false)
                              ("spell_check" . t)
                              ("word_limit_max" . "1000")
                              ("word_limit_min" . "0")
                              ("word_limit_enabled" . t)))
                           (t `(("prompt" . ,question-text)))))
         
         ;; Initialize scoring data based on question type
         (scoring-data (cond
                       ((string= item-type "essay_question") 
                        (let ((grading-notes (org-lms-extract-grading-notes)))
                          `(("value" . ,(if (and grading-notes (not (string-empty-p grading-notes)))
                                           grading-notes
                                         "")))))
                       (t '()))))

    ;; Add feedback if available
    (when (or correct-comments incorrect-comments)
      (let ((feedback '()))
        (when correct-comments
          (push `("correct" . ,correct-comments) feedback))
        (when incorrect-comments
          (push `("incorrect" . ,incorrect-comments) feedback))
        (push `("feedback" . ,feedback) properties)))

    ;; Add answer choices for multiple choice questions
    (when (string= item-type "multiple_choice")
      (let ((answers (org-lms-extract-quiz-answers)))
        (message "DEBUG: Extracted answers = %s" answers)
        (when answers
          (let ((choices (mapcar (lambda (answer)
                                 `(("id" . ,(format "choice_%d" (cl-position answer answers)))
                                   ("position" . ,(+ (cl-position answer answers) 1))
                                   ("itemBody" . ,(format "<p>%s</p>" (alist-get "answer_text" answer nil nil #'string=)))))
                               answers))
                (correct-choices (cl-remove-if-not (lambda (a) 
                                                   (> (alist-get "answer_weight" a 0 nil #'string=) 0)) 
                                                 answers)))
            (message "DEBUG: Choices = %s" choices)
            (message "DEBUG: Correct choices = %s" correct-choices)
            (push `("choices" . ,choices) interaction-data)
            ;; Set scoring data for multiple choice
            (when correct-choices
              (setq scoring-data `(("value" . ,(format "choice_%d" (cl-position (car correct-choices) answers))))))))))

    ;; Build the entry (actual question content) using correct New Quiz format
    (let* ((interaction-type-slug (cond
                                   ((string= item-type "multiple_choice") "choice")
                                   ((string= item-type "essay_question") "essay")
                                   ((string= item-type "true_false") "true_false")
                                   (t "choice")))
           
           ;; Build properties with correct structure
           (entry-properties (cond
                             ((string= item-type "multiple_choice")
                              `(("shuffleRules" . (("choices" . (("toLock" . [])
                                                                 ("shuffled" . t)))))
                                ("varyPointsByAnswer" . :json-false)))
                             ((string= item-type "essay_question")
                              '())
                             (t '())))
           
           ;; Build feedback structure
           (feedback-obj '())
           
           (entry `(("interaction_type_slug" . ,interaction-type-slug)
                   ("title" . ,(nth 4 (org-heading-components)))
                   ("item_body" . ,question-text)
                   ("calculator_type" . "none")
                   ("interaction_data" . ,interaction-data)
                   ("properties" . ,entry-properties)
                   ("scoring_data" . ,(if scoring-data scoring-data '()))
                   ("scoring_algorithm" . "Equivalence")
                   ("feedback" . ,feedback-obj))))

      ;; Add feedback if available
      (when (or correct-comments incorrect-comments)
        (when correct-comments
          (push `("correct" . ,correct-comments) feedback-obj))
        (when incorrect-comments
          (push `("incorrect" . ,incorrect-comments) feedback-obj))
        (setf (alist-get "feedback" entry nil nil #'string=) feedback-obj))

      ;; Build the outer item structure
      (let ((item `(("entry_type" . "Item")
                   ("points_possible" . ,(string-to-number question-points))
                   ("position" . ,question-position)
                   ("entry" . ,entry))))

        ;; Wrap in item object as required by New Quiz Items API
        (let* ((final-params `(("item" . ,item))))
          (message "DEBUG: Calculated position = %s" question-position)
          (message "DEBUG: Entry = %s" entry)
          (message "DEBUG: Item = %s" item)
          (message "DEBUG: Final params = %s" final-params)
          (let ((response (org-lms-new-quiz-request
                           (format "courses/%s/quizzes/%s/items%s"
                                   courseid assignment-id
                                   (if item-id (format "/%s" item-id) ""))
                           (if item-id "PATCH" "POST")
                           final-params)))
            (message "New Quiz Item response: %s" response)
            (when (plist-get response :id)
              (org-set-property "NEW_ITEM_ID" (format "%s" (plist-get response :id))))
            ;; Always update POSITION property to reflect calculated position
            (org-set-property "POSITION" (format "%s" question-position))
            response))))))

(defun org-lms-get-new-quiz-items (assignment-id &optional courseid)
  "Retrieve all items for new quiz ASSIGNMENT-ID in course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-new-quiz-request (format "courses/%s/quizzes/%s/items" courseid assignment-id) "GET"))

(defun org-lms-delete-new-quiz-item (item-id assignment-id &optional courseid)
  "Delete new quiz item ITEM-ID from quiz ASSIGNMENT-ID in course COURSEID."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
  (org-lms-new-quiz-request 
   (format "courses/%s/quizzes/%s/items/%s" courseid assignment-id item-id) 
   "DELETE"))

;; New Quiz comprehensive workflow function
(defun org-lms-post-new-quiz-with-items ()
  "Post the current new quiz and all items to Canvas."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (message "Creating new quiz...")
    ;; First create the new quiz
    (let ((quiz-response (org-lms-post-new-quiz))
          (item-count 0)
          (item-responses '()))
      (if (plist-get quiz-response :id)
          (progn
            (message "New quiz created successfully. Processing items...")
            
            ;; Process items (no groups in new quizzes, just direct items)
            (org-map-entries 
             (lambda ()
               (when (and (org-entry-get nil "QUESTION_TYPE")
                         (>= (org-outline-level) 2))
                 (setq item-count (1+ item-count))
                 (message "Creating item %d..." item-count)
                 (let ((item-response (org-lms-post-new-quiz-item)))
                   (if (plist-get item-response :id)
                       (progn
                         (push item-response item-responses)
                         (message "Item %d created successfully" item-count))
                     (message "Failed to create item %d" item-count)))))
             nil 'tree)
            
            (message "New quiz creation complete: %d items added" item-count)
            `(:quiz ,quiz-response :items ,(reverse item-responses) :item-count ,item-count))
        (message "Failed to create new quiz")
        nil))))

;; Conversion helper functions
(defun org-lms-convert-quiz-to-new-quiz ()
  "Convert current classic quiz heading to new quiz format."
  (interactive)
  (when (org-entry-get nil "QUIZ_ID")
    (let ((quiz-id (org-entry-get nil "QUIZ_ID"))
          (assignment-group-id (or (org-entry-get nil "ASSIGNMENT_GROUP_ID") "1")))
      ;; Set new quiz properties
      (org-set-property "NEW_QUIZ_TYPE" "new_quiz")
      (org-set-property "ASSIGNMENT_GROUP_ID" assignment-group-id)
      (unless (org-entry-get nil "GRADING_TYPE")
        (org-set-property "GRADING_TYPE" "points"))
      
      ;; Convert question type properties for child questions
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "QUESTION_ID")
           (org-set-property "ORIGINAL_QUESTION_ID" (org-entry-get nil "QUESTION_ID"))
           (org-delete-property "QUESTION_ID")))
       nil 'tree)
      
      (message "Quiz converted to new quiz format. Use org-lms-post-new-quiz-with-items to create."))))

;; Delete helper function for new quiz items
(defun org-lms-delete-current-new-quiz-item ()
  "Delete the current new quiz item from Canvas and remove the heading from buffer."
  (interactive)
  (let ((item-id (org-entry-get nil "NEW_ITEM_ID"))
        (assignment-id (org-entry-get nil "NEW_QUIZ_ID" t))
        (question-type (org-entry-get nil "QUESTION_TYPE"))
        (heading (nth 4 (org-heading-components))))
    (if (and question-type item-id assignment-id)
        (when (y-or-n-p (format "Delete item '%s' (ID: %s) from Canvas and buffer? " heading item-id))
          (message "Deleting item from Canvas...")
          (let ((response (org-lms-delete-new-quiz-item item-id assignment-id)))
            (if response
                (progn
                  (message "Item deleted from Canvas successfully")
                  ;; Delete the entire subtree from buffer
                  (org-back-to-heading)
                  (org-cut-subtree)
                  (message "Item removed from buffer"))
              (message "Failed to delete item from Canvas"))))
      (message "Current heading is not a new quiz item or missing required IDs"))))

(defun org-lms-new-quiz-template ()
  "Insert a template for a new quiz with sample items."
  (interactive)
  (let ((quiz-title (read-string "New quiz title: "))
        (assignment-group-id (read-string "Assignment group ID (default 1): " nil nil "1"))
        (due-date (format-time-string "%Y-%m-%d" (time-add (current-time) (* 7 24 3600)))))
    (insert (format "* %s :NEW_QUIZ:\n" quiz-title))
    (org-entry-put nil "NEW_QUIZ_TYPE" "new_quiz")
    (org-entry-put nil "ASSIGNMENT_GROUP_ID" assignment-group-id)
    (org-entry-put nil "GRADING_TYPE" "points")
    (org-entry-put nil "TIME_LIMIT" "30")
    (org-entry-put nil "ALLOWED_ATTEMPTS" "1")
    (org-entry-put nil "SHUFFLE_ANSWERS" "t")
    (org-entry-put nil "DUE_AT" due-date)
    (org-entry-put nil "POINTS_POSSIBLE" "10")
    (org-entry-put nil "OL_PUBLISH" "nil")
    (insert "\nThis is the new quiz description. You can include instructions here.\n\n")
    
    ;; Add sample multiple choice item
    (insert "** Multiple Choice Question\n")
    (org-entry-put nil "QUESTION_TYPE" "multiple_choice_question")
    (org-entry-put nil "QUESTION_POINTS" "2")
    (org-entry-put nil "POSITION" "1")
    (org-entry-put nil "CORRECT_COMMENTS" "Excellent! That's the correct answer.")
    (org-entry-put nil "INCORRECT_COMMENTS" "Not quite right. Please review the material.")
    (insert "\nWhat is 2 + 2?\n\n")
    (insert "- [ ] 3\n")
    (insert "- [X] 4\n")
    (insert "- [ ] 5\n")
    (insert "- [ ] 6\n\n")
    
    ;; Add sample short answer item
    (insert "** Short Answer Question\n")
    (org-entry-put nil "QUESTION_TYPE" "short_answer_question")
    (org-entry-put nil "QUESTION_POINTS" "3")
    (org-entry-put nil "POSITION" "2")
    (org-entry-put nil "CORRECT_COMMENTS" "Good answer!")
    (org-entry-put nil "INCORRECT_COMMENTS" "Please provide more detail.")
    (insert "\nExplain the significance of the year 1492 in European history.\n\n")))

;; Quiz API Functions:1 ends here

;; library closing:1 ends here
