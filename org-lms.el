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

;; require the dependencies
(require 'org) ;; the source of all good!
(require 'org-attach) ;; for attaching files to emails
(require 'cl) ;; may not be necessary anymore in newer Emacsen
(require 'org-mime) ;; Unfortunately I require this somewhat outdated library for mailing
(require 'dash) ;; modern syntax
(require 'ts) ;; easy time manipulation
;; (require 's) ;; modern strings
;; (require 'org-ql) ;; faster, easier query syntax
;;(require 'ov) ;; for grade overlays

(define-obsolete-function-alias 'org-lms-send-subtree-with-attachments
    'org-lms~send-subtree-with-attachments)
(define-obsolete-function-alias 'org-lms-mail-all-undone 
    'org-lms-mail-all)

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
    "Buffer-local plist of students in this course, merging cnavas and local info. 

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

(defun org-lms-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun org-lms-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-lms-global-props key))))

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
      ;; add new filetag
      (if (looking-at "^$") 		;empty line
          ;; at beginning of line
          (when value
            (insert (format "#+%s: %s" tag value)))
        ;; at end of some line, so add a new line
        (when value
          (insert (format "\n#+%s: %s" tag value)))))))

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

;; stolen from xah, http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun org-lms~read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

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

(require 'ts)
(defun o-l-date-to-timestamp (date)
  "use ts.el date parse functions return an ISO-compatible
timestamp for transmission to Canvas via API. DATE is a string,
usually of the form `2019-09-26`, but optionally including a full time."

  (ts-format "%Y-%m-%dT%H:%M:%S%:z" (ts-parse-fill 'end date )))

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
  (declare (indent defun))
  (unless request-type (setq request-type "GET"))
  (let ((canvas-payload nil)
        (canvas-err nil)
        (canvas-status nil)
        (json-params (json-encode request-params))
        (target (concat org-lms-baseurl query))
        ;; (request-coding-system 'no-conversion)
        ;; (request-conding-system 'no-conversion)
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
                      :data   (if  json-params json-params  nil) ;; (or data nil)
                      ;;:encoding 'no-conversion
                      :parser (lambda ()
                                (if (and (boundp 'file) file) (write-region (buffer-string) nil file))
                                (ol-jsonwrapper json-read))
                      :success (cl-function
                                (lambda (&key data &allow-other-keys)
                                  ;;(message "SUCCESS: %S" data)
                                  (message "SUCCESS!!")
                                  (setq canvas-payload data)
                                  canvas-payload
                                  ))
                      :error (cl-function (lambda ( &key error-thrown data status &allow-other-keys )
                                            (setq canvas-err error-thrown)
                                            (message "ERROR: %s" error-thrown)))))
               (unless (request-response-data thisrequest)                                   
                 (message (format "NO PAYLOAD: %s" canvas-err)) )
               (request-response-data thisrequest) )
      (user-error "Please set a value for for `org-lms-token' in order to complete API calls"))))

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
      (loop for can in-ref canvas-courses
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
  (let* ((org-export-with-toc nil)
         ;;(org-export-with-smart-quotes nil)
         (org-html-postamble nil)
         (org-html-preamble nil)
         (org-html-xml-declaration nil)
         (org-html-head-include-scripts nil)
         (org-html-head-include-default-style nil)
         (org-html-klipsify-src nil)
         (org-export-with-title nil)
         (atext (org-export-as 'html subtreep nil t))
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
                                        ("default_view" . ,default_view)
                                        ("license" . ,license)
                                        ))))
         (response (org-lms-canvas-request
                    (format  "courses/%s" courseid) "PUT" data-structure ))
         )
    (write-region (json-encode data-structure) nil "/home/matt/syl.json")
    ;;(setq response)
    (message "Response: %s" response)
    response
    ))

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

(defun org-lms-get-students (&optional courseid)
    "Retrieve Canvas student data for course with id COUSEID"
    (let* ((courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
;; (courseid (plist-get course :id))
           (result
            (org-lms-canvas-request (format "courses/%s/users" courseid) "GET"
                                    '(("enrollment_type" . ("student"))
                                      ("include" . ("email"))
                                      ("per_page" . 500 )))))
      ;;(message "RESULTS")
      ;;(with-temp-file "students-canvas.json" (insert result))
      (loop for student in-ref result
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
  (loop for c in-ref canvas
        do (let* ((defn c)
                  (email (plist-get defn :email)))
             (loop for l in-ref local
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
                              ("body" . ,(org-export-as 'html t nil t))
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

;; (defun org-lms-post-page ()
  ;; "Extract page data from HEADLINE.
  ;; HEADLINE is an org-element object."
  ;; (interactive)

  ;; (let* ((canvasid (org-entry-get nil "CANVASID"))
  ;;        (canvas-page-url (org-entry-get nil "CANVAS_PAGE_URL"))
  ;;        ;; (duedate (org-entry-get nil "DUE_AT"))
  ;;        (org-html-checkbox-type 'unicode )  ;; canvas stirps checkbox inputs
  ;;        ;; (pointspossible (if (org-entry-get nil "PAGE_WEIGHT") (* 100 (string-to-number (org-entry-get nil "PAGE_WEIGHT")))))
  ;;        (editing-roles  (or  (org-entry-get nil "CANVAS_EDITING_ROLES") "teachers"))
  ;;        (subtype (if (equal (org-entry-get nil "PAGE_TYPE") "canvas") "online_upload" "none"))
  ;;        ;;( (org-entry-get nil "DUE_AT"))
  ;;        (publish (org-entry-get nil "OL_PUBLISH")))
  ;;   ;; (message "canvas evals to %s" (if canvasid "SOMETHING " "NOTHING" ))
  ;;   ;;(prin1 canvasid)
  ;;   (let* ((org-export-with-tags nil)
  ;;          (page-params `(("wiki_page" .
  ;;                                (("title" .  ,(nth 4 (org-heading-components)) )
  ;;                                 ("body" . ,(org-export-as 'html t nil t))
  ;;                                 ;; ("submission_types" . ,subtype)
  ;;                                 ;; ("grading_type" . ,gradingtype)
  ;;                                 ;; ("grading_standard_idcomment" . 458)
  ;;                                 ("editing_roles" . ,editing-roles)
  ;;                                 ;; ("points_possible" . ,(or pointspossible 10))
  ;;                                 ("published" . ,(if publish t nil) )))))

  ;;          (response
  ;;           (org-lms-canvas-request (format "courses/%s/pages%s"
  ;;                                           (org-lms-get-keyword "ORG_LMS_COURSEID");; (plist-get org-lms-course :id)
  ;;                                           (if canvas-page-url
  ;;                                               (format  "/%s" canvas-page-url) "")
  ;;                                           )
  ;;                                   (if canvas-page-url "PUT" "POST")
  ;;                                   page-params
  ;;                                   ))
  ;;          (response-data (or response nil))
  ;;          )
  ;;     ;; (message "HERE COMES THE PARAMS %s" (request-response-data response) )
  ;;     ;; (prin1 (assq-delete-all "page[description]" page-params))
  ;;     (if (plist-get response-data :url)
  ;;         (progn
  ;;           (message "received response-data")
  ;;           (org-set-property "CANVASID" (format "%s"(plist-get response-data :page_id)))
  ;;           (org-set-property "CANVAS_PAGE_URL" (format "%s"(plist-get response-data :url)))
  ;;           (org-set-property "OL_PUBLISH" (format "%s"(plist-get response-data :published)))
  ;;           (org-set-property "CANVAS_HTML_URL" (format "%s"(plist-get response-data :html_url)))
  ;;           (org-set-property "CANVAS_EDITING_ROLES" (format "%s" (plist-get response-data :editing_roles)))
  ;;           ;; (org-set-property "SUBMISSIONS_DOWNLOAD_URL" (format "%s"(plist-get response-data :submissions_download_url)))
  ;;           ;; (org-set-property "GRADING_STANDARD_ID" (format "%s"(plist-get response-data :grading_standard_id)))
  ;;           ;; (org-set-property "CANVAS_SUBMISSION_TYPES" (format "%s"(plist-get response-data :submission_types)))
  ;;           ;; (org-set-property "GRADING_TYPE" (format "%s"(plist-get response-data :grading_type)))
  ;;           ;; (org-set-property "CANVASID" (format "%s"(plist-get response-data :id)))

  ;;           ) )
  ;;     ;; (message "PAGE_TYPE is canvas %s" (equal "canvas" (org-entry-get nil "PAGE_TYPE")))
  ;;     ;; (message "RESPONSE IS %s" response)
  ;;     (if (plist-get response-data :html_url)
  ;;         (browse-url (plist-get response-data :html_url)))
  ;;     response)))

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
    (loop for l in-ref local
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

(defun org-lms-get-submissions (&optional courseid)
  "get all submisisons in a COURSE (rarely used)."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request (format "courses/%s/students/submissions" courseid) "GET"))

(defun org-lms-get-assignment-submissions ( assignmentid &optional courseid)
  "Get all submisisons belonging to ASSIGNMENTID in optional COURSE."

  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request
   (format "courses/%s/assignments/%s/submissions/" courseid assignmentid ) "GET"))

(defun org-lms-get-single-submission (studentid assignmentid &optional courseid)
  "Retrieve a single sugmission from canvas.
STUDENTID identifies the student, ASSIGNMENTID the assignment, and COURSEID the course."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request
   (format "courses/%s/assignments/%s/submissions/%s" courseid assignmentid studentid) "GET"))

(defun org-lms-get-canvas-attachments ()
  (interactive) 
  (let* ((assid
          (save-excursion 
            (org-up-heading-safe)
            (org-entry-get (point) "ASSIGNMENTID")
            ))
         (studentid (or (org-entry-get (point) "STUDENTID") (org-entry-get (point) "ID")))
         (submission (org-lms-get-single-submission studentid assid))
         (student (org-lms-find-local-user studentid))
         )
         (message "Submission: %s" submission)
    (cl-loop for attachment in (plist-get submission :attachments)
             do
             (message "%s%s"(downcase (plist-get student :lastname))
                      (downcase (plist-get student :firstname)) )
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

(defun org-lms-parse-assignment ()
  "Extract assignment data from HEADLINE.
  HEADLINE is an org-element object."
  (interactive)

  (let* ((canvasid (org-entry-get nil "CANVASID"))
         (duedate (org-entry-get nil "DUE_AT"))
         (org-html-checkbox-type 'unicode )  ;; canvas stirps checkbox inputs
         (pointspossible (if (org-entry-get nil "ASSIGNMENT_WEIGHT") (* 100 (string-to-number (org-entry-get nil "ASSIGNMENT_WEIGHT")))))
         (gradingtype (or  (org-entry-get nil "GRADING_TYPE") "letter_grade"))
         (subtype (if (equal (org-entry-get nil "ASSIGNMENT_TYPE") "canvas") "online_upload" "none"))
         ;;( (org-entry-get nil "DUE_AT"))
         (publish (org-entry-get nil "OL_PUBLISH"))
         (reflection (org-entry-get nil "OL_HAS_REFLECTION"))
         (reflection-id (org-entry-get nil "OL_REFLECTION_ID")))
    ;; (message "canvas evals to %s" (if canvasid "SOMETHING " "NOTHING" ))
    ;;(prin1 canvasid)
    (let* ((org-export-with-tags nil)
           (assignment-params `(("assignment" .
                                 (("name" .  ,(nth 4 (org-heading-components)) )
                                  ("description" . ,(org-export-as 'html t nil t))
                                  ,(if duedate
                                       ;;`("due_at"   . ,(concat duedate  "T23:59:59-04:00"))
                                       `("due_at"   . ,(o-l-date-to-timestamp duedate))
                                     )
                                  ("submission_types" . ,subtype)
                                  ("grading_type" . ,gradingtype)
                                  ("grading_standard_idcomment" . 458)
                                  ("points_possible" . ,(or pointspossible 10))
                                  ("published" . ,(if publish t nil) )))))

           (response
            (org-lms-canvas-request (format "courses/%s/assignments%s"
                                            (org-lms-get-keyword "ORG_LMS_COURSEID");; (plist-get org-lms-course :id)
                                            (if canvasid
                                                (format  "/%s" canvasid) "")
                                            )
                                    (if canvasid "PUT" "POST")
                                    assignment-params
                                    ))
           (response-data (or response nil))
           )
      ;; (message "HERE COMES THE PARAMS %s" (request-response-data response) )
      ;; (prin1 (assq-delete-all "assignment[description]" assignment-params))
      (if (plist-get response-data :id)
          (progn
            (message "received assignment response-data")
            (org-set-property "CANVASID" (format "%s"(plist-get response-data :id)))
            (org-set-property "OL_PUBLISH" (format "%s"(plist-get response-data :published)))
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
                                                   ;;`("due_at"   . ,(concat duedate  "T23:59:59-04:00"))
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
                        (org-set-property "OL_REFLECTION_ID" (format "%s" (plist-get response-data :id)))))))
            
            
            ))

      
      ;; (message "ASSIGNMENT_TYPE is canvas %s" (equal "canvas" (org-entry-get nil "ASSIGNMENT_TYPE")))
      ;; (message "RESPONSE IS %s" response)
      response)))



(defun org-lms-post-assignment-and-save (&optional file)
  "First post the assignment, then save the value to FILE."
  (interactive)
  (unless file (setq file (expand-file-name "assignments.el")))
  (org-lms-parse-assignment)
  (org-lms-save-assignment-map file))

;; huh is this deprecated?
  ;; doesn't seem to be used at all 
(defun org-lms-post-announcement (payload &optional courseid)
  "Create new announcement using PAYLOAD a data in course COURSEID."
    (setq courseid (or courseid
                       (org-lms-get-keyword "ORG_LMS_COURSEID")
                       (plist-get org-lms-course)))
    (org-lms-canvas-request
     (format "courses/%s/discussion_topics" courseid) "POST" payload))

;; announcements

(defun org-lms-headline-to-announcement (&optional courseid file)
  ""
  (interactive)
  (setq courseid (or courseid
                       (org-lms-get-keyword "ORG_LMS_COURSEID")
                       (plist-get org-lms-course)))
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
         (atitle (nth 4 (org-heading-components)))
         (org-html-klipsify-src nil)
         (org-export-with-title nil)
         ;;(courseid (plist-get course :id))
         (atext (org-export-as 'html t nil t))
         (response nil)
         (oldid (org-entry-get (point) "ORG_LMS_ANNOUNCEMENT_ID"))
         )
    ;; (message "BUILDMETA DEFN")
    ;; (prin1 (symbol-function  'org-html--build-meta-info))
    ;; (message "%s" atext)
    (if oldid
        (progn
          (message "already added!")
          (setq response ;;(request-response-data) 
                (org-lms-canvas-request
                 (format  "courses/%s/discussion_topics/%s" courseid oldid) "PUT"
                 `(("title" . ,atitle)
                   ("message" . ,atext)
                   ("is_published" . t)
                   ("is_announcement" . t)))))

      (setq response ;;(request-response-data)
            (org-lms-canvas-request
             (format  "courses/%s/discussion_topics" courseid) "POST"
             `(("title" . ,atitle)
               ("message" . ,atext)
               ("is_published" . t)
               ("is_announcement" . t)))))
    (cl-loop for (k v) on response
             do
             (message "%s %S" k v))
    (org-entry-put (point) "ORG_LMS_ANNOUNCEMENT_ID" (format "%s" (plist-get response :id)))
    (org-entry-put (point) "ORG_LMS_ANNOUNCEMENT_URL" (format "%s" (plist-get response :url)))
    (org-entry-put (point) "ORG_LMS_POSTED_AT" (format "%s" (plist-get response :posted_at)))

    (if (plist-get response :url) 
        (browse-url (plist-get response :url)))
    response))

(defun org-lms-get-grading-standards (&optional courseid)
    "Retrieve Canvas grading standards for course with id COUSEID"
    (let* ((courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID")))
           (result
            (org-lms-canvas-request (format "courses/%s/grading_standards" courseid) "GET" )))
      result))

(defun org-lms-map-assignments (&optional file )
    "turn a buffer of assignment objects into a plist with relevant info enclosed."

    (let ((old-buffer (current-buffer)))
      (with-temp-buffer 
        (if file (insert-file-contents (expand-file-name file))
          (insert-buffer-substring-no-properties old-buffer))
        ;; (insert-file-contents file)
        (org-mode)
        (let* ((id (org-lms-get-keyword "ORG_LMS_COURSEID"))
               (results '())
               (org-use-tag-inheritance nil)
               )
         ;; (message "BUFFER STRING SHOULD BE: %s" (buffer-string))
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
                                             )) "rubric" 'tree))  )
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
                                               :assignment-type ,(org-entry-get nil "ASSIGNMENT_TYPE")
                                               :directory ,(or (org-entry-get nil "OL_DIRECTORY")
                                                               (downcase
                                                                (replace-regexp-in-string "[\s]" "-" name )))
                                               :rubric ,rubric)))
                                               ) "assignment"))
          ;;(message "RESULT IS: %s" results)
          results))) )

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
                         (atitle (nth 4 (org-heading-components)))
                         (org-ascii-text-width 23058430000))
                     (org-export-as 'ascii t nil t)))
         (returnval '()))
    ;; loop over attachments
    (dolist (a (org-attach-file-list (org-attach-dir t)))
      (let* ((path (expand-file-name a (org-attach-dir t) ))
             (fileinfo (org-lms-canvas-request
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
                                       file_id))))))
    (let* ((grade-params `(("submission" . (("posted_grade" . ,grade)))
                           ("comment" . (("text_comment" . ,comments)
                                         ;; EDIT 2018=11-07 -- untested switch from alist to plist
                                         ("file_ids" . ,returnval)
                                         ;; alas, doesn't seem to update the previous comment! drat
                                         ("id" . (or (org-entry-get nil "OL_COMMENT_ID" ) nil)))) ))
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

(defun org-lms-make-headings (a students)
  "Create a set of headlines for grading.

A is a plist describing the assignment. STUDENTS is now assumed
to be a plist, usually generated by
`org-lms~parse-plist-csv-file' but eventually perhaps read
directly from Canvas LMS. UPDATE: seems to work well with
`org-lms-merged-students'

Canvas LMS allows for export of student information; the
resultant csv file has a certain shape, bu this may all be irrelevant now."
  (message "running org-lms-make-headings")
  (save-excursion
    (goto-char (point-max))
    ;; (message "students=%s" students)
    ;; (mapcar (lambda (x)))
    (let* ((body a)
           ;; rewrite this part wit horg-process-props? 
           ;; nmaybe not possible as written. 
           (atitle (plist-get body :name ))
           (number (plist-get body :assignment_number))
           (assignmentid (or (format "%s" (plist-get body :canvasid)) ""))
           (directory (plist-get body :directory ))
           (weight (plist-get body :assignment-weight ))
           (grade-type (plist-get body :grade-type ))
           (assignment-type (plist-get body :assignment-type))
           ;; (email-response (plist-get body :email-response))
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
      (org-set-property "ASSIGNMENTID" assignmentid)
      (org-set-property "ORG_LMS_ASSIGNMENT_DIRECTORY" directory)
      (org-set-property "BASECOMMIT" basecommit)
      (org-set-property "GRADING_TYPE" grading-type)
      (org-set-property "NUMBER" number)
      (make-directory directory t)
      (goto-char (point-max))
      (let* (( afiles (if (file-exists-p directory)
                          (directory-files directory  nil ) nil))
             (json-array-type 'list)
             (json-object-type 'plist)
             (json-key-type 'keyword)
             (json-false nil)
             ;; this crufty garbage needs to be fixed. 
             (prs (if (string= assignment-type "github") (json-read-file "./00-profile-pr.json"))))
        (mapcar (lambda (stu)
                  ;;(message "%s" stu)
                  (let* ((fname (plist-get stu :firstname))
                         (lname (plist-get stu :lastname))
                         (nname (or  (unless (equal  (plist-get stu :nickname) nil)
                                       (plist-get stu :nickname)) fname))
                         (email (plist-get stu :email))
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
                                      atitle nname lname ))
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
                     ((equal assignment-type "canvas")
                      ;; (message "SUBTYPE IS CANVAS")
                      (org-lms-get-canvas-attachments))
                     
                     ;; otherwise, look for existing files with approximately matching names in the appropriate directory.  
                     (t
                      (let* ((fullnamefiles (remove-if-not (lambda (f) (string-match (concat "\\\(" fname "\\\)\\\([^[:alnum:]]\\\)*" lname) f)) afiles))
                             (nicknamefiles (remove-if-not (lambda (f) (string-match (concat "\\\(" nname "\\\)\\\([^[:alnum:]]\\\)*" lname) f)) afiles)))
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
(defun org-lms~send-subtree-with-attachments ()
  "org-mime-subtree and HTMLize"
  (interactive)
  ;; (org-mark-subtree)
  (let ((attachments (org-lms~attachment-list)))
    (save-excursion
      (org-lms-mime-org-subtree-htmlize attachments))
    ))

;; defunkt
(defun org-lms-send-subtree-with-attachments ()
  "org-mime-subtree and HTMLize"
  (interactive)
  (org-mark-subtree)
  (let ((attachments (mwp-org-attachment-list))
        (subject  (mwp-org-get-parent-headline)))
    ;;(insert "Hello " (nth 4 org-heading-components) ",\n")
    (org-mime-subtree)
    (insert "\nBest,\nMP.\n")
    (message-goto-body)
    (insert "Hello,\n\nAttached are the comments from your assignment.\n\n")
    (insert "At this point I have marked all the papers I know about. If 
you have not received a grade for work that you have handed in,
 please contact me immediately and we can resolve the situation!.\n\n")
    ;; (message "subject is" )
    ;; (message subject)
    ;;(message-to)
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

;;; NOTE I may have broken this via SEND-ALL condition !!! 2018-11-08
(cl-defun org-lms-return-all-assignments (&optional (send-all nil) (also-mail nil) (post-to-lms t) )
  "By default mail all subtrees 'READY' to student recipients, unless SEND-ALL is non-nil.
In that case, send all marked 'READY' or 'TODO'."
  (interactive)
  (message "Mailing all READY subtrees to students")
  
  (let* ((ol-status-org-msg org-msg-mode)
        
        (send-condition
         (if send-all
             `(or (string= (org-element-property :todo-keyword item) "READY")
                  (string= (org-element-property :todo-keyword item) "TODO") )
           `(string= (org-element-property :todo-keyword item) "READY")
           )))
    (if ol-status-org-msg (org-msg-mode))
    (org-map-entries 
     #'ol-send-just-one)
    (if ol-status-org-msg (org-msg-mode)))
  (org-cycle-hide-drawers 'all)
  )


(cl-defun ol-send-just-one (&optional (also-mail nil) (post-to-lms t))
  ;; (print (nth 0 (org-element-property :todo-keyword item)))
  (interactive)
  (let ((also-mail (org-entry-get nil "ORG_LMS_EMAIL_COMMENTS" t))
        (post-to-lms (org-entry-get nil "ORG_LMS_CANVAS_COMMENTS" t)))
    
    (when (string= (nth 2 (org-heading-components) ) "READY")
      (when post-to-lms (org-lms-put-single-submission-from-headline))
      (when also-mail  (save-excursion
                         ;;(org-lms-mime-org-subtree-htmlize )
                         (org-lms~send-subtree-with-attachments)
                         (sleep-for 1)
                         ;; (message-send-and-exit)
                         ))
      (org-todo "SENT"))))
;; should get rid of this & just add a flag to ~org-lms-mail-all~
(defun org-lms-mail-all-undone ()
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
          (org-todo )
          ))
      )
    ))




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

;; more helpers
(defun org-lms-mime-org-subtree-htmlize (&optional attachments)
  "Create an email buffer of the current subtree.
The buffer will contain both html and in org formats as mime
alternatives.

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
      (let* ((file (buffer-file-name (current-buffer)))
             (subject (or (mp "MAIL_SUBJECT") (nth 4 (org-heading-components))))
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
  (require 'ov)

  (let ((chits (org-lms-get-keyword "OL_USE_CHITS")))
    (save-excursion
      (org-back-to-heading)
      
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
        (message "%s" (overlay-get ov "name")))))
  )

(defun org-lms-clear-overlays ()
  "if the overlays become annoying at any point"
  (interactive)
  (ov-clear))

;; (defun org-lms-set-grade (grade)
;;   "set grade property at point and regenerate overlays"
;;   (interactive "sGrade:")
;;   (org-set-property "GRADE" grade)
;;   (org-lms-clear-overlays)
;;   (org-lms-overlay-headings) )

(defvar ol-grade-regex  "- \\*?Grade:?\\*?\\( ::\\)? ?\\(.+\\)"
  "regular expression matching grade lines." )

(defun org-lms-set-grade ()
  "set grade property for all headings on basis of \"- Grade :: \" line.

  Use with caution."
  (interactive)
  (save-restriction 
    (org-narrow-to-subtree)
  (save-excursion
    (org-back-to-heading)
    (while (re-search-forward ol-grade-regex nil t )
      (let ((mark (or (match-string 2) 0)))

        (if (string= mark "Pass")
            (setq mark "pass"))
        (org-set-property "GRADE" mark)
        (org-todo "READY"))
      )))
  (org-lms-overlay-headings) 

  )

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
         (add-to-list 'assignments (nth 4 (org-heading-components)) t))
       "ASSIGNMENT"))
    
    ;;loop over entries
    ;; this should be improved, returning a plist to be looped over
    (dolist (assignment assignments)
      (save-excursion
        (org-open-link-from-string (format "[[%s]]" assignment)) ;; jump to assignment
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
                         ;;((string= g "0") (setq g "Fail"))
                         )
                        (add-to-list 'grades `(,assignment . ,g))
                        
                        (if chits
                            (add-to-list 'grades `(,(concat assignment " Chits") . ,(org-entry-get (point) "CHITS"))))
                        (plist-put s :grades grades)))))
         nil 'file 'comment)))
    ;; there's gotta be a bette way!
    (cl-loop for s in-ref students
             do (let ((grades (plist-get s :grades)))
               (plist-put s :grades (reverse grades))))
    (message "Students = %s" students)
    
    (let* ((columns (cl-loop for a in assignments
                                         collect a
                                         if chits
                                         collect (concat a " Chits")))
           (tableheader (append '("Student" "First" "Nick" "Last" "Student #" "email") columns))
           (rows (cl-loop for s in students
                     collect
                     ;; (message "%s" s)
                     (let* ((grades (plist-get s :grades))
                            (row (append `(,(plist-get s :name)
                                           ,(plist-get s :firstname)
                                           ,(plist-get s :nickname)
                                           ,(plist-get s :lastname)
                                           ,(plist-get s :sis_user_id) ;; check to be sure this is right
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

;; helper functions for github repos
(defun org-lms~open-student-repo ()
  (interactive)
  (find-file-other-window (org-entry-get (point) "LOCAL_REPO" )))

(defun org-lms~open-attachment-or-repo () 
  (interactive)
  (let* ((attach-dir (org-attach-dir t))
         (files (org-attach-file-list attach-dir)))
    (if (> (length files) 0 )
        (org-attach-open)
      (org-lms~open-student-repo)
      )))

(defun org-lms-inspect-object (method url headers)
    (restclient-http-do method url headers
     ))



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

(defun org-lms-announcement-wim ()
  "move point to top level subtree, then, since we want 
to keep announcement creation super-lightweight, *always* export that 
headline."
  (interactive)
  (if (string= (org-lms-get-keyword "ORG_LMS_SECTION") "announcement")
      (save-excursion
        (let ((subtree (org-lms--get-valid-subtree)))
          (org-lms-headline-to-announcement)
          ))))

(defun mwp-subtree-to-slack-wim ()
  "move point to top level subtree, then, since we want 
to keep announcement creation super-lightweight, *always*  
copy that subtree as slack text for posting to slack."
  (interactive)
    (save-excursion
    (let ((subtree (org-lms--get-valid-subtree)))
      (org-mark-subtree)
      (org-slack-export-to-clipboard-as-slack)
      )))


(defun org-lms-assignment-wim ()
  "post current asisgnment to org-lms, using wim criteria"
  (interactive)
  (save-window-excursion
    (widen)
    (save-excursion
      (let ((subtree (org-lms--get-valid-subtree)))
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
  (org-todo "READY")
  (org-forward-heading-same-level 1))

(defun org-lms-wim-wim ()
  "test for org-lms-section, then perform the appropriate wim function"
  (interactive)
  (pcase (org-lms-get-keyword "ORG_LMS_SECTION")
    ("announcement" (org-lms-announcement-wim))
    ("assignment" (org-lms-assignment-wim))
    ("slides" (org-lms-slides-wim))
    ("grades" (org-lms-grades-wim))
    )
  )

;; Minor mode definition. I'm not really using it right now, but it
;; might be a worthwhile improvement.
(defun o-l-set-grade ()
(call-interactively (org-set-property "GRADE"))) 
(define-minor-mode org-lms-mode
  "a mode to get my grading and other lms interacitons in order"
  :init-value nil
  :global nil
  :keymap  (let ((map (make-sparse-keymap))) 
             (define-key map (kbd "C-c C-x C-g") 'o-l-set-grade )
             map )
  :lighter " Mark"
  (mwp-toggle-macros)
  (if org-lms-mode
      (progn
        (add-hook 'org-ctrl-c-ctrl-c-final-hook 'org-lms-wim-wim))

    (remove-hook 'org-ctrl-c-ctrl-c-hinal-hook 'org-lms-wim-wim))
  )

(provide 'org-lms)
;;; org-lms ends here
