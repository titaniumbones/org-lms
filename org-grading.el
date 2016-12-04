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


;; Helper Functions

;; I'm using hte namespace `o-g-' for these internal helper functions.
;; At some liater date should figure out and implement approved best
;; oractices. 

;; CSV Parsers
;; Student information (name, email, etc) is exported from excel or blackboard in the form
;; of a CSV file.  These two functions parse such files

(defun o-g-parse-csv-file (file)
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

(defun o-g-parse-plist-csv-file (file)
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
      (let ((header-props (split-string  (buffer-substring-no-properties
                                          (line-beginning-position) (line-end-position)) ","))
            )
        ;;(message (format "header is: %s" header-props)) ;;(print header)
        (while (not (eobp))
          (let ((line  (split-string (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position)) ","))
                (count 0)
                (new-plist '()))
            ;;            ;;(print line)
            (while (< count (length line))
              (print (nth count header-props))
              (print (nth count line))
              (setq new-plist (plist-put new-plist  (intern (nth count header-props)) (nth count line)))
              (setq count (1+ count)))
            (push  new-plist result)
            (forward-line 1))))
      (cdr (reverse result)))))

;; Element tree navigation

(defun o-g-get-parent-headline ()
  "Acquire the parent headline & return. Used by`org-grading-make-headlines' and `org-grading-attach'"
  (save-excursion
    (org-mark-subtree)
    (re-search-backward  "^\\* ")
    (nth 4 (org-heading-components))))

;; Minor mode definition. I'm not really using it right now, but it
;; might be a worthwhile improvement. 
(define-minor-mode org-grading-mode
  "a mode to get my grading in order"
  ;;:keymap (kbd "C-c C-x C-g" . (call-interactively (org-set-property "GRADE")))
  :lighter " Mark"
  )
;; refers to an obsolete function I can't remember
(add-hook 'org-grading-mode-hook
          (lambda ()
            (add-hook 'org-metareturn-hook 'mwp-insert-grade-template nil 'make-local
                      )))
(add-hook 'org-grading-mode-hook 'org-contacts-setup-completion-at-point)

;; mail integration. Only tested with mu4e.
(defun o-g-send-subtree-with-attachments ()
  "org-mime-subtree and HTMLize"
  (interactive)
  (org-mark-subtree)
  (let ((attachments (o-g-attachment-list))
        (subject  (mwp-org-get-parent-headline)))
    (org-mime-send-subtree)
    (insert "\nBest,\nMP.\n")
    (message-goto-body)
    (insert "Hello,\n\nAttached are the comments from your assignment.\n")
    (org-mime-htmlize)
    (dolist (a attachments)  (mml-attach-file a (mm-default-file-encoding a) nil "attachment"))
    (message-goto-to)
    ))


;; stolen from gnorb, but renamed to avoid conflicts
(defun o-g-attachment-list (&optional id)
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



;; MAIN ORG-GRADING UTILITY FUNCTIONS

;; attaching files to subtreeds
(defun org-grading-attach () 
  "Interactively attach a file to a subtree. 

Assumes that the parent headline is the name of a subdirectory,
and that the current headline is the name of a student. Speeds up file choice."
  (interactive)
  (org-attach-attach (read-file-name
                      (concat  "File for student " (nth 4 (org-heading-components)) ":")
                      (o-g-get-parent-headline) )))
(defun org-grading-attach ())

;; Used to create grading headlines for each assignment & student
(defun org-grading-make-headings (assignments students)
  "Create a set of headlines for grading.

ASSIGNMENTS is an alist in which the key is the assignment title,
and the value is the grading template. STUDENTS is now assumed to
be a plist, usually generated by `o-g-parse-plist-csv-file', whose
first element is the student name, and whose second is the
student email."
  (message "%s" assignments)
  (save-excursion
    (goto-char (point-max))
    (message "students=%s" students)
    (mapcar (lambda (x)
              (let ((assignment (car x))
                    (template (cdr x)))
                (insert (format "\n* %s :ASSIGNMENT:" assignment))
                (let (( afiles (directory-files assignment  nil )))
                  (mapcar (lambda (stu)
                            (let* ((fname (plist-get stu 'First))
                                   (lname (plist-get stu 'Last))
                                   (nname (or  (plist-get stu 'Nickname) fname))
                                   (email (plist-get stu 'Email))
                                   )
                              ;;(message  "pliste gets:%s %s %s %s" fname lname nname email)
                              (insert (format "\n** %s %s" nname lname))
                              (org-todo 'todo)
                              (insert template)
                              (org-set-property "GRADE" "0")
                              (org-set-property "CHITS" "0")
                              (org-set-property "NICKNAME" nname)
                              (org-set-property "MAIL_TO" email)
                              ;; (org-set-property "MAIL_CC" "matt.price@utoronto.ca")
                              (org-set-property "MAIL_REPLY" "matt.price@utoronto.ca")
                              (org-set-property "MAIL_SUBJECT"
                                                (format "Comments on %s Assignment (%s %s)"
                                                        (mwp-org-get-parent-headline) nname lname ))
                              ;; try to attach files, if possible
                              (let* ((fullnamefiles (remove-if-not (lambda (f) (string-match (concat "\\\(" fname "\\\)\\\([^[:alnum:]]\\\)*" lname) f)) afiles))
                                     (nicknamefiles (remove-if-not (lambda (f) (string-match (concat "\\\(" nname "\\\)\\\([^[:alnum:]]\\\)*" lname) f)) afiles)))
                                (message "fullnamefiles is: %s" fullnamefiles)
                                (if afiles
                                    (if fullnamefiles
                                        dolist (thisfile fullnamefiles)
                                        (message "value of thisfile is: %s" thisfile)
                                        (message "%s %s" (buffer-file-name) thisfile)
omwp                                        (message "value being passed is: %s"(concat (file-name-directory (buffer-file-name)) assignment "/" thisfile) )
                                        (org-attach-attach (concat (file-name-directory (buffer-file-name)) assignment "/" thisfile) )
                                        (message "Attached perfect match for %s" name)
                                      (dolist (thisfile nicknamefiles)
                                        (if t
                                            (progn 
                                              (org-attach-attach (concat (file-name-directory (buffer-file-name)) assignment "/" thisfile) )
                                              (message "No perfect match; attached likely match for %s" nname)))))
                                  (message "No files match name of %s" nname)))
                              ;; (condition-case nil
                              
                              ;;   (error (message "Unable to attach file belonging to student %s" nname )))
                              (save-excursion
                                (org-mark-subtree)
                                (org-cycle nil))
                              ))students)) ) ) assignments)))


;; Mailing functions

(defun org-grading-mail-all ()
  (interactive)
  "Mail all subtrees marked 'READY' to student recipients."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (item)
      ;; (print (nth 0 (org-element-property :todo-keyword item)))
      (when (string= (org-element-property :todo-keyword item) "READY")
        (save-excursion
          (goto-char (org-element-property :begin item))
          ;;(print "sending")
          ;;(print item)
          (save-excursion
            (mwp-send-subtree-with-attachments)
            ;; added this line
            ;; (if (fboundp 'mu4e-compose-mode)
            ;;     (mu4e-compose-mode))
            (message-send-and-exit))
          (org-todo "SENT")
          ))
      )
    ))
(defun org-grading-mail-all-undone ()
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
            (org-grading-send-missing-subtree)
            (message-send-and-exit))
          (org-todo "TODO")
          ))
      )
    ))

;; not currently used -- abandoned in favour of a definitions list
(defun org-grading-insert-grade-template ()
  "simply insert a short grading template after creation of level 2 headline.
I'm actualy not using this right now, but keeping temporarily until I'm sure it won't "
  (let ((element (org-element-at-point)))
    (save-excursion
      (when (and (org-element-type element)
                 (eq (org-element-property :level element) 2))
        (insert "
| Organization             |   |
| Clarity of Thesis        |   |
| Presentation of Evidence |   |
| Grammar and Spelling     |   |
| Style                    |   |
| Citations                |   |
| Further Comments         |   |
| Grade                    |   | 

")))))

(defun org-grading-send-subtree-with-attachments ()
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
(defun org-grading-send-missing-subtree ()
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
(defun org-grading-overlay-headings ()
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

(defun org-grading-clear-overlays ()
    "if the overlays become annoying at any point"
    (ov-clear)
    
    )

(defun org-grading-set-grade (grade)
  "set grade property at point and regenerate overlays"
  (interactive "sGrade:")
  (org-set-property "GRADE" grade)
  (org-grading-clear-overlays)
  (org-grading-overlay-headings) )

(defun org-grading-set-all-grades ()
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
  (org-grading-overlay-headings) 

  )

(defun org-grading-generate-tables ()
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
(defun org-grading-pass ()
  "set the current tree to pass"
  
  (interactive)
  (org-set-property "GRADE" "1")
  ;;(ov-clear)
  (org-grading-overlay-headings)
  )

(defun org-grading-chit ()
  "set the current tree to one chit"
  
  (interactive)
  (org-set-property "CHITS" "1")
  (ov-clear)
  (org-grading-overlay-headings)
  )

(provide 'org-grading)
;;; org-grading ends here
