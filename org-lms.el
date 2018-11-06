;;; package -- Summary

;;; Commentary: A collection of functions to facilitate grading papers
;;; and assignments. It is currently somewhat inflexible and assumes a
;;; very specific workflow; I'd be interested to know whether it's of
;;; use to anyoneelse.

;;; Code:

;; require the dependencies
(require 'org) ;; the source of all good!
(require 'org-attach) ;; for attaching files to emails
(require 'cl) ;; may not be necessary anymore in newer Emacsen
(require 'ov) ;; for grade overlays


;; variables
;; most of these are used for canvas interactions...

(defvar org-lms-courses
  '((wildwater .  (:name "Wildwater" :coursenum "NEW271" :assignments () :etc ())))
  "The idea is to be able to set this easily and store it somehow.")

(defcustom org-lms-baseurl nil
  "baseurl for canvas API queries. 
Should have the form \"https://canvas.instance.at.school/api/v1\"."
  :type '(string)
  )
(defcustom org-lms-token nil
  "secret oauth token for Canvas. DO NOT SHARE THIS INFO.
Probably customize is a rotten place to put this!"
  :type '(string))

(defvar-local org-lms-course nil
  "to be used in initialization src blocks for now ")

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
(defun org-lms~get-parent-headline ()
  "Acquire the parent headline & return. Used by`org-lms-make-headlines' and `org-lms-attach'"
  (save-excursion
    (org-up-heading-safe)
    (nth 4 (org-heading-components))
    ;;(org-mark-subtree)
    ;;(re-search-backward  "^\\* ")
    ;;(nth 4 (org-heading-components))
    ))

;; Minor mode definition. I'm not really using it right now, but it
;; might be a worthwhile improvement. 
(define-minor-mode org-lms-mode
  "a mode to get my grading in order"
  ;;:keymap (kbd "C-c C-x C-g" . (call-interactively (org-set-property "GRADE")))
  :lighter " Mark"
  )

;; mail integration. Only tested with mu4e.
(defun org-lms~send-subtree-with-attachments ()
  "org-mime-subtree and HTMLize"
  (interactive)
  (org-mark-subtree)
  (let ((attachments (org-lms~attachment-list))
        )
    (save-excursion
      (org-lms-mime-org-subtree-htmlize attachments))
    ))


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



;; MAIN ORG-LMS UTILITY FUNCTIONS

;; attaching files to subtrees
;; looks like this is unuesed.  
(defun org-lms-attach () 
  "Interactively attach a file to a subtree. 

Assumes that the parent headline is the name of a subdirectory,
and that the current headline is the name of a student. Speeds up file choice."
  (interactive)
  (if (file-exists-p org-lms~get-parent-headline )
      (org-attach-attach (read-file-name
                          (concat  "File for student " (nth 4 (org-heading-components)) ":")
                          (org-lms~get-parent-headline) ))
    (message "Warning: no such directory %s; not attaching file" org-lms~get-parent-headline)))


(defun org-lms-make-headings (a students)
  "Create a set of headlines for grading.

A is a plist describing the assignment. STUDENTS is now assumed to
be a plist, usually generated by `org-lms~parse-plist-csv-file' but eventually
perhaps read directly from Canvas LMS.

Canvas LMS allows for export of student information; the
resultant csv file (all of this may be irrelevant now"
  (message "running org-lms-make-headings")
  (save-excursion
    (goto-char (point-max))
    ;; (message "students=%s" students)
    ;; (mapcar (lambda (x)))
    (let* ((body a)
           (atitle (plist-get body :name ))
           (assignmentid (or (format "%s" (plist-get body :canvasid)) ""))
           (directory (plist-get body :directory ))
           (weight (plist-get body :weight ))
           (grade-type (plist-get body :grade-type ))
           (storage (plist-get body :storage-type))
           (template (let ((output ""))
                       (dolist (item  (plist-get body :rubric-list) output)
                         (setq output (concat output
                                              (format "- *%s* :: \n" item))))))
           ;; (template (plist-get 'rubric-list body))
           )
      (message "BODY:\n%s\n%s\n%s\n%s/BODY" body atitle directory weight)
      ;; (message "car assignment successful: %s" template)
      (insert (format "\n* %s :ASSIGNMENT:" atitle))
      (org-set-property "ASSIGNMENTID" assignmentid)
      (org-set-property "ORG_LMS_ASSIGNMENT_DIRECTORY" directory)
      (goto-char (point-max))
      (let (( afiles (if (file-exists-p directory)
                         (directory-files directory  nil ) nil)))
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
                         (courseid (if  (and  (boundp 'org-lms-course) (listp org-lms-course))
                                       (number-to-string (plist-get org-lms-course :id))
                                      nil))
                         (github (or  (plist-get stu :github) ""))
                         (id (or (number-to-string (plist-get stu :id)) ""))
                         (props 
                          `(("GRADE" . "0")
                            ("GRADE" . "0")
                            ("CHITS" . "0")
                            ("NICKNAME" . ,nname)
                            ("FIRSTNAME" . ,fname)
                            ("LASTNAME" . ,lname)
                            ("MAIL_TO" . ,email)
                            ("GITHUB" . ,github)
                            ("ID" . ,id)
                            ("COURSEID" . ,courseid)
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
                    (insert template)
                    (insert (format "This assignment is worth *%s percent* of your mark and is graded as a letter grade. Please see ... for more details.\n"
                                    (* 100  weight)))
                    (dolist (p props)
                      (org-set-property (car p ) (cdr p)))

                    ;; TODO: this should be converted to a (cond...) that works differnetly
                    ;; with different assignment types
                    ;; try to attach files, if possible
                    (cond
                     ((eq storage "github")
                      ;; git stuff
                      )
                     ((eq storage "file")
                      ;; directory stuff
                      )
                     (t
                      ;; other cases
                      )
                     )
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
                    ;; (condition-case nil
                    
                    ;;   (error (message "Unable to attach file belonging to student %s" nname )))
                    (save-excursion
                      (org-back-to-heading)
                      ;;(org-mark-subtree);;
                      
                      (org-cycle nil))
                    ))
                students)) ) 
   )
  (org-cycle-hide-drawers 'all))


;; stolen from xah, http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun org-lms~read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

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
  (message "%s" assignments)
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
                                 (afiles (ignore-errors (directory-files (concat title "/" basename "-" github ))))
                                 
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
;; Mailing functions

(defun org-lms-mail-all ()
  (interactive)
  "Mail all subtrees marked 'READY' to student recipients."
  (message "Mailing all READY subtrees to students")
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (item)
      ;; (print (nth 0 (org-element-property :todo-keyword item)))
      (when (string= (org-element-property :todo-keyword item) "READY")
        (save-excursion
          (goto-char (org-element-property :begin item))
          ;;(print "sending")
          ;;(print item)
          (save-excursion
            (forward-char)
            ;; (save-)
            (org-lms~send-subtree-with-attachments)
            ;; added this line
            ;; (if (fboundp 'mu4e-compose-mode)
            ;;     (mu4e-compose-mode))
            )
          (org-todo "SENT")
          ))
      )
    )
  (org-cycle-hide-drawers 'all))



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
          (org-todo "TODO")
          ))
      )
    ))


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

;; still imperfect, but good enough for me.  
(defun org-lms-overlay-headings ()
  "Show grades at end of headlines that have a 'GRADE' property."
  (interactive)
  (require 'ov)

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
        (format  "%s  GRADE: %s CHITS: %s" character (org-entry-get (point) "GRADE") (org-entry-get (point) "CHITS")))
       (overlay-put ov 'name "grading")
       (message "%s" (overlay-get ov "name")))))
  )

(defun org-lms-clear-overlays ()
    "if the overlays become annoying at any point"
    (ov-clear)
    
    )

(defun org-lms-set-grade (grade)
  "set grade property at point and regenerate overlays"
  (interactive "sGrade:")
  (org-set-property "GRADE" grade)
  (org-lms-clear-overlays)
  (org-lms-overlay-headings) )


(defun org-lms-set-all-grades ()
  "set grade property for all headings on basis of \"- Grade :: \" line.

  Use with caution."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "- Grade :: \\(.+\\)" nil t )
      (org-set-property "GRADE" (match-string 1))
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
    (while (re-search-forward "- \\(.*\\)Grade\\(.*\\) :: \\(.+\\)" nil t )
      (let ((grade (match-string 3)))
        (if (string-match "pass" grade)
            (progn (message grade)
                   (org-set-property "GRADE" "1"))
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

(defun org-lms-generate-tables ()
  "Generate a *grade report* buffer with a summary of the graded assignments
Simultaneously write results to results.csv in current directory."
  (interactive)
  (setq assignments '())
  (setq students '())

  ;;get assignments
  (let ((org-use-tag-inheritance nil))
    (org-map-entries
     (lambda ()
       (add-to-list 'assignments (nth 4 (org-heading-components)) t))
     "ASSIGNMENT"))

  ;; get student names as list of cons cells
  (let ((org-use-property-inheritance nil))
    (org-map-entries
     (lambda ()
       (add-to-list 'students (cons (nth 4 (org-heading-components)) '()) t))
     "MAIL_TO={utoronto.ca}"))
  ;;loop over entries
  ;; this should be improved, returning a plist to be looped over
  (dolist (assignment assignments)
    (save-excursion
      ;; jump to assignment
      (org-open-link-from-string (format "[[%s]]" assignment))
      ;; map over entries
      (org-map-entries
       (lambda ()
         (let* ((student (car (assoc (nth 4 (org-heading-components)) students))))
           (when student
             (setf (cdr (assoc student students))
                   (append (cdr (assoc student students))
                           (list (org-entry-get (point) "GRADE")))))))
       nil 'tree)))

  (setq gradebook
        (append (list  (append '("Student") assignments)
                       'hline)
                students))

  (write-region (orgtbl-to-csv gradebook nil) nil "results3.csv")

   
  ;; I would like to put the gradebook IN the buffer but I can't figure out
  ;; a wayt odo it without killing 
  ;; (org-open-ling-from-string "[[#gradebook]]")
  ;;(let ((first-child (car (org-element-contents (org-element-at-point)))))  (when (eq )))
  (let ((this-buffer-name  (buffer-name)))
    (switch-to-buffer-other-window "*grade report*")
    (erase-buffer)
    (org-mode)
    
    (insert (orgtbl-to-orgtbl gradebook nil))
    (pop-to-buffer this-buffer-name))
  ;;(pop-to-buffer nil)
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



;; more helpers
(defun org-lms-mime-org-subtree-htmlize (attachments)
  "Create an email buffer of the current subtree.
The buffer will contain both html and in org formats as mime
alternatives.

The following headline properties can determine the headers.
* subtree heading
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

;; talking to canvas

(defun org-lms-canvas-request (query &optional request-type request-params)
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
    (message (concat org-lms-baseurl query))
    (message (concat "Bearer " org-lms-token))
    (message "%s" `(("Authorization" . ,(concat "Bearer " org-lms-token))))
    (if org-lms-token
        (progn
          (setq thisrequest
                (request
                 (concat org-lms-baseurl query)
                 :type request-type
                 :headers `(("Authorization" . ,(concat "Bearer " org-lms-token)))
                 :sync t
                 :params (if  request-params request-params nil)
                 :parser (lambda ()
                           (let* ((json-array-type 'list)
                                  (json-object-type 'plist)
                                  ;; (json-key-type 'symbol
                                  ;;                )
                                  )
                             (json-read)))
                 :success (cl-function
                           (lambda (&key data &allow-other-keys)
                             (setq canvas-payload data)
                             ))
                 :error (cl-function (lambda (&rest args  &key error-thrown &allow-other-keys)
                                       (setq canvas-err error-thrown)
                                       (message "ERROR: %s" error-thrown)))
                 ))
          ;; (message "pPAYLOAD: %s" canvas-payload)
          ;;canvas
          (if (request-response-data thisrequest)
              canvas-payload
            (error (format "NO PAYLOAD: %s" canvas-err)))
          ) 
      (user-error "Please set a value for for `org-lms-token' in order to complete API calls"))))


(defun org-lms-canvas-json-request (query &optional request-type request-data)
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
    (message (concat org-lms-baseurl query))
    ;;(message (concat "Bearer " org-lms-token))
    ;;(message "%s" `(("Authorization" . ,(concat "Bearer " org-lms-token))))
    (if org-lms-token
        (progn
          (setq thisrequest
                (request
                 (concat org-lms-baseurl query)
                 :type request-type
                 :headers `(("Authorization" . ,(concat "Bearer " org-lms-token)) ("Content-Type" . "application/json"))
                 :sync t
                 :data (json-encode  request-data)
                 :parser (lambda ()
                           (let* ((json-array-type 'list)
                                  (json-object-type 'plist)
                                  ;; (json-key-type 'symbol
                                  ;;                )
                                  )
                             (json-read)))
                 :success (cl-function
                           (lambda (&key data &allow-other-keys)
                             (setq canvas-payload data)
                             ))
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

(defun org-lms-get-courses ()
  (org-lms-canvas-request "courses" "GET"))

(defun org-lms-get-courseid (&optional course)
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
          (prin1 can)
          (let ((course-code (plist-get can :sis_course_id)))
            (message "COURSECODE %s" course-code)
            (if (and
                 course-code
                 (string-match coursenum  course-code )
                 (string-match semester course-code))
                (progn
                  (plist-put can :shortname
                             shortname)
                  (plist-put can :coursenum coursenum)
                  (plist-put can :semester semester)
                  (setq result can)))))
    result))

(defun org-lms-get-students (&optional course)
  (unless course
    (setq course org-lms-course))
  (let* ((courseid (plist-get course :id))
         (result
         (org-lms-canvas-request (format "courses/%s/users" courseid) "GET"
                                 '(("enrollment_type[]" . "student")
                                   ("include[]" . "email")))))
    (message "RESULTS")
    (prin1 result)
    (loop for student in-ref result
          do
          (if (string-match "," (plist-get student :sortable_name))
              (let ((namelist  (split-string (plist-get student :sortable_name) ", ")))
                (plist-put student :lastname (car namelist) )
                (plist-put student :firstname (cadr namelist)))))
    result))


(defun org-lms-get-all-users (&optional course)
  ;; (unless course
  ;;   (setq course org-lms-course))
  (prin1 course)
  (let ((courseid (plist-get course :id)))
    (message "COUSEID %s" courseid)
    (org-lms-canvas-request (format "courses/%s/users" courseid) "GET")))

(defun org-lms-get-assignments (&optional course)
  (unless course
    (setq course org-lms-course))
  (let ((courseid (plist-get course :id)))
    (org-lms-canvas-request (format "courses/%s/assignments" courseid) "GET")))

(defun org-lms-get-submissions (&optional course)
  (unless course
    (setq course org-lms-course))
  (let ((courseid (plist-get course :id)))
    (org-lms-canvas-request (format "courses/%s/students/submissions" courseid) "GET")))

(defun org-lms-get-assignment-submissions ( assignmentid &optional course)
  (unless course
    (setq course org-lms-course))
  (let ((courseid (plist-get course :id)))
    (org-lms-canvas-request
     (format "courses/%s/assignments/%s/submissions/" courseid assignmentid ) "GET")))

(defun org-lms-get-single-submission (studentid assignmentid &optional course)
  (unless course
    (setq course org-lms-course))
  (let ((courseid (plist-get course :id)))
    (org-lms-canvas-request
     (format "courses/%s/assignments/%s/submissions/%s" courseid assignmentid studentid) "GET")))

(defun org-lms-put-single-submission-from-headline (&optional studentid assignmentid  course)
  "Get comments from student headline and post to Canvas LMS.

If STUDENTID, ASSIGNMENTID and COURSEID are omitted, their values
will be extracted from the current environment. Note the
commented out `dolist' macro, which will upload attachments to
cnavas. THis process is potentially buggy and seems likely to
lead to race conditions and duplicated uploadsand comments. Still
working on this."
  
  (unless course
    (setq course org-lms-course))
  (unless assignmentid
    (setq assignmentid (save-excursion 
                         (org-up-heading-safe)
                         (org-entry-get (point) "ASSIGNMENTID"))))
  (unless studentid
    (setq studentid (org-entry-get (point)  "ID")))
  (let* ((courseid (plist-get course :id))
         (grade (org-entry-get (point) "GRADE"))
         (comments (let*((org-export-with-toc nil)
                         (org-export-with-smart-quotes nil)
                         (org-html-postamble nil)
                         (org-html-preamble nil)
                         (org-html-xml-declaration nil)
                         (org-html-head-include-scripts nil)
                         (org-html-head-include-default-style nil)
                         ;;(atext (org-export-as 'html t))
                         (atitle (nth 4 (org-heading-components)))
                         (org-html-klipsify-src nil)
                         (org-export-with-title nil))
                     (org-export-as 'ascii t nil t)))
         (returnval '()))
    ;; (dolist (a (org-attach-file-list (org-attach-dir t)))
      
    ;;   (let* (
    ;;          (path (expand-file-name a (org-attach-dir t) ))
    ;;          (fileinfo
    ;;           (org-lms-canvas-request
    ;;            (format "courses/%s/assignments/%s/submissions/%s/comments/files"
    ;;                    courseid assignmentid studentid)
    ;;            "POST" `(("name" . ,a)) )))
    ;;     (message "WHAAAAT?")
    ;;     (prin1  (plist-get  fileinfo :upload_params))
    ;;     (let ((al (gcr/plist-to-alist (plist-get fileinfo :upload_params)))
    ;;           (formstring ""))
    ;;       (cl-loop for prop in al
    ;;                do
    ;;                (setq formstring (concat formstring "-F '" (symbol-name (car prop))
    ;;                                         "=" (format "%s" (cdr prop)) "' ")))
    ;;       (setq formstring (concat formstring " -F 'file=@" path "' 2> /dev/null"))
    ;;       (let* ((thiscommand  (concat "curl '"
    ;;                                (plist-get fileinfo :upload_url)
    ;;                                "' " formstring))
    ;;              (curlres  (shell-command-to-string thiscommand
    ;;                                                 )))
    ;;         ;;(message "NO PROBLEMS HERE")
    ;;         ;;(prin1 (json-read-from-string  curlres))
    ;;         (setq returnval (add-to-list 'returnval  (alist-get 'id (json-read-from-string curlres)))))
          
    ;;       ;; (request
    ;;       ;;  (plist-get fileinfo :upload_url)
    ;;       ;;  :params al
    ;;       ;;  :files `((,a . )))
    ;;       )
    ;;     )
    ;;   )
    ;; first have to submit the graded doc
    (org-lms-canvas-request
     (format "courses/%s/assignments/%s/submissions/%s" courseid assignmentid studentid)
     "PUT"
     `(("submission[posted_grade]" . ,grade)
       ("comment[text_comment]" . ,comments)
       ;; ("comment[file_ids]" . ,returnval)
       ))

    ))

;;copied and modified from https://github.com/jorendorff/dotfiles/blob/master/.emacs
(defun org-lms-plist-to-alist (ls)
  "Convert a plist to an alist. Primarily for old color-theme themes."
  (let ((result nil))
    (while ls
      (add-to-list 'result (cons (intern (substring  (symbol-name (car ls)) 1 )) (cadr ls)))
      (setq ls (cddr ls)))
    result))

(defun org-lms-canvas-file-upload (url params)
  
  )
(defun org-lms-post-announcement (payload &optional course)
  (unless course
    (setq course org-lms-course))
  (let ((courseid (plist-get course :id)))
    (org-lms-canvas-request (format "courses/%s/discussion_topics" courseid) "POST" payload))
  )

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
                 :params (if  request-params request-params nil)
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

(defun org-lms-merge-assignment-values (&optional local canvas)
  (unless local
    (setq local org-lms-local-assignments ))
  (unless canvas
    (setq canvas (org-lms-get-assignments)))
  ;; (message "LOCALLLLL")
  ;; (prin1 local)
  ;; (prin1 canvas)
  (let ((result '()))
    (loop for l in-ref local
          do (let* ((defn (cdr l))
                    (name (plist-get defn :name)))
               ;; (message "LLLLLLLLL")
               ;; (prin1 l)
               ;; (prin1 (plist-get (cdr l) :name))
               ;; (prin1 name)
               (dolist (c canvas)
                 (message "CCCCCCCC")
                 (prin1 (plist-get c :name))
                 (prin1 c)
                 (if (equal
                      name  (plist-get c :name))
                     (progn
                       (message "MADE ITI N")
                       (plist-put defn :canvasid (plist-get c :id))
                       (plist-put defn :html_url (plist-get c :html_url))
                       (plist-put defn :submissions_download_url (plist-get c :submissions_download_url))
                       (message "DEFN")
                       (prin1 defn)
                       )))
               (add-to-list 'result `(,(car l) .  ,defn))))
    result))

;; fix broken symbol not keyword assignment!!!
(defun org-lms-merge-student-lists (&optional local canvas)
  (unless local
    (setq local (org-lms-get-local-students))
    )
  (unless canvas
    (setq canvas (org-lms-get-students)))
  (message "%s" local)
  (loop for c in-ref canvas
        do (let* ((defn c)
                  (email (plist-get defn :email)))
             (dolist (l  local)
               (if (equal
                    email  (plist-get l :email))
                   (progn 
                     (plist-put defn 'github (plist-get l 'github))
                     (if (plist-get l :nickname)
                         (progn
                           (plist-put defn :nickname (plist-get l :nickname))
                           (plist-put defn :short_name (plist-get l :nickname))))
                     (unless (plist-get c :firstname)
                       (plist-put defn :firstname (plist-get l :firstname)))
                     (unless (plist-get c :lastname)
                       (plist-put defn :lastname (plist-get l :lastname)))
                     )))))
  canvas)

(defun org-lms-assignments-table (&optional assignments students)
  "Return a 2-dimensional list suitable whose contents are org-mode table cells.

Intnded to be used in a simpe src block with :results header `value raw table'. 
Resultant links allow quick access to the canvas web interface as well as the make-headings
commangs."
  (unless assignments
    (setq assignments org-lms-merged-assignments))
  (unless students
    (setq students org-lms-merged-students))
  (message "MERGED ASSIGNMENTS")
  (prin1 assignments)
  (let* ((cid (plist-get org-lms-course :id))
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
                                               (number-to-string cid)
                                               "/assignments/"
                                               (number-to-string (org-lms-safe-pget (cdr i) :canvasid))
                                               "\")][Inspect Original JSON]]")
                                    " "))
                                                                  ;; "[[elisp:(org-lms-canvas-inspect \"courses/%s/assignments/%s\") ][%s]]"
                                  ;; cid
                                  ;; (org-lms-safe-pget (cdr i) :canvasid)
                                  ;; "courses/64706/assignments/88373"
                                  ;; (org-lms-safe-pget
                                  ;; org-lms-course
                                  ;; :canvasid)
                                ;; "Inspect Original JSON"
                                ,(format "[[%s][%s]]"
                                         (concat "elisp:(org-lms-make-headings (alist-get '"
                                                 (symbol-name (car i))
                                                 " org-lms-merged-assignments) org-lms-merged-students)"
                                                 ) 
                                         "Make Headlines"))))

    ))

(defun org-lms-safe-pget (list prop)
  (if (plist-get list prop)
      (plist-get list prop)
    ""))

(defun org-lms-setup ()
  "Merge  defs and students lists, and create table for later use.

`org-lms-course', `org-lms-local-assignments' and other org-lms
variables must be set or errors wil lresult."
  (setq org-lms-merged-students (org-lms-merge-student-lists))
  (setq org-lms-merged-assignments (org-lms-merge-assignment-values))
  (org-lms-assignments-table org-lms-merged-assignments)
  )
(defun org-lms-get-local-students (&optional csv)
  (unless csv
    (setq csv "./students.csv"))
  (org-lms~parse-plist-symbol-csv-file csv)
  )


;; announcements

(defun org-lms-headline-to-announcement (&optional course)
  (interactive)
  (unless course
    (setq course org-lms-course))
  (cl-flet ((org-html--build-meta-info
             (lambda (&rest args) "")))
    ;; (prin1 (symbol-function  'org-html--build-meta-info))
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
           (courseid (plist-get course :id))
           (atitle (nth 4 (org-heading-components)))
           (atext (org-export-as 'html t nil t))
           (response nil)
           (oldid (org-entry-get (point) "ORG_LMS_ANNOUNCEMENT_ID"))
           )
      ;; (message "BUILDMETA DEFN")
      ;; (prin1 (symbol-function  'org-html--build-meta-info))
      (message "%s" atext)
      (if oldid
          (progn
            (message "already added!")
            (setq response
                  (org-lms-canvas-json-request
                   (format  "courses/%s/discussion_topics/%s" courseid oldid) "PUT"
                   `(("title" . ,atitle)
                     ("message" . ,atext)
                     ("is_published" . t)
                     ("is_announcement" . t)))))
        
        (setq response
              (org-lms-canvas-json-request
               (format  "courses/%s/discussion_topics" courseid) "POST"
               `(("title" . ,atitle)
                 ("message" . ,atext)
                 ("is_published" . t)
                 ("is_announcement" . t))))
        (org-entry-put (point) "ORG_LMS_ANNOUNCEMENT_ID" (format "%s" (plist-get response :id)))
        (org-entry-put (point) "ORG_LMS_ANNOUNCEMENT_URL" (format "%s" (plist-get response :url)))
        
        )
      (browse-url (plist-get response :url))
      response)))


(provide 'org-lms)
;;; org-lms ends here
