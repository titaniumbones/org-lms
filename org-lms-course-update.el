;;; org-lms-course-update.el --- Year-to-year course update utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Matt Price

;; Author: Matt Price
;; Keywords: org, lms, canvas

;;; Commentary:

;; Convenience functions for detecting and fixing outdated content when
;; updating courses from one year to the next.  Detects:
;; - Links pointing to old course IDs or containing outdated year patterns
;; - Dates outside the current course date range
;;
;; Results are displayed in an occur-like buffer with jump-to-source
;; and optional direct editing capabilities.
;;
;; Usage:
;;   M-x org-lms-check-outdated-links
;;   M-x org-lms-check-outdated-dates
;;   M-x org-lms-check-course-updates  (combined check)
;;
;; Required file keywords:
;;   #+COURSE_START_DATE: 2025-09-01
;;   #+COURSE_END_DATE: 2025-12-15
;;   #+ORG_LMS_COURSEID: 123456  (existing keyword)

;;; Code:

(require 'org)
(require 'org-element)
(require 'ts)
(require 'dash)
(require 'cl-lib)

;; Soft require - org-lms should be loaded first for org-lms-get-keyword
(require 'org-lms nil t)
(declare-function org-lms-get-keyword "org-lms")

;;; Customization

(defgroup org-lms-course-update nil
  "Year-to-year course update utilities for org-lms."
  :group 'org-lms
  :prefix "org-lms-cu-")

(defcustom org-lms-cu-canvas-url-pattern
  "q\\.utoronto\\.ca/courses/\\([0-9]+\\)"
  "Regexp pattern to match Canvas course URLs and capture course ID.
The first capture group should match the course ID number."
  :type 'regexp
  :group 'org-lms-course-update)

(defcustom org-lms-cu-year-patterns
  '("/\\(20[0-9][0-9]\\)/"                                    ; /2024/ in URL paths
    "\\(20[0-9][0-9]\\)-\\(20[0-9][0-9]\\)"                   ; 2024-2025 academic year
    "\\b\\(Fall\\|Winter\\|Spring\\|Summer\\)\\s-*\\(20[0-9][0-9]\\)\\b") ; "Fall 2024"
  "List of regexp patterns that indicate year references in URLs or text.
Each pattern should have at least one capture group containing a year."
  :type '(repeat regexp)
  :group 'org-lms-course-update)

(defcustom org-lms-cu-buffer-name "*Course Update Checker*"
  "Name of the buffer for displaying course update check results."
  :type 'string
  :group 'org-lms-course-update)

(defcustom org-lms-cu-current-year nil
  "Current academic year for comparison.
If nil, uses the year from COURSE_START_DATE or current calendar year."
  :type '(choice (const :tag "Auto-detect" nil)
                 (integer :tag "Year"))
  :group 'org-lms-course-update)

;;; Internal Variables

(defvar-local org-lms-cu--issues nil
  "List of issues in current results buffer.")

(defvar-local org-lms-cu--check-type nil
  "Type of check performed: `links', `dates', or `all'.")

(defvar-local org-lms-cu--scope nil
  "Scope of check performed: `buffer', `directory', or `project'.")

(defvar-local org-lms-cu--source-files nil
  "List of source files that were checked.")

(defvar-local org-lms-cu--edit-mode nil
  "Non-nil when edit mode is active in results buffer.")

(defvar-local org-lms-cu--edit-overlays nil
  "List of overlays tracking edits in results buffer.")

;;; Scope Selection

(defun org-lms-cu--get-scope-files (scope)
  "Get list of org files for SCOPE.
SCOPE is one of: `buffer', `directory', `project'."
  (pcase scope
    ('buffer (list (buffer-file-name)))
    ('directory
     (directory-files default-directory t "\\.org$"))
    ('project
     (if (and (fboundp 'projectile-project-root)
              (projectile-project-root))
         (--filter (string-suffix-p ".org" it)
                   (projectile-project-files (projectile-project-root)))
       ;; Fallback to directory if projectile not available
       (directory-files default-directory t "\\.org$" t)))))

(defun org-lms-cu--prompt-for-scope ()
  "Interactively prompt user to select check scope."
  (let* ((choices '(("Current buffer" . buffer)
                    ("All org files in directory" . directory)
                    ("All org files in project" . project)))
         (choice (completing-read "Scope: " choices nil t)))
    (cdr (assoc choice choices))))

;;; Link Scanning Functions

(defun org-lms-cu--extract-links-from-buffer ()
  "Extract all http/https links from current org buffer with position info.
Returns list of plists with :url, :begin, :end, :line, :raw-link."
  (let ((links '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((type (org-element-property :type link)))
          (when (member type '("http" "https"))
            (push (list :url (org-element-property :raw-link link)
                        :begin (org-element-property :begin link)
                        :end (org-element-property :end link)
                        :line (line-number-at-pos (org-element-property :begin link))
                        :raw-link (org-element-property :raw-link link))
                  links)))))
    (nreverse links)))

(defun org-lms-cu--check-link-course-id (link-plist current-courseid)
  "Check if LINK-PLIST contains a Canvas URL with mismatched course ID.
CURRENT-COURSEID is the expected course ID from ORG_LMS_COURSEID.
Returns nil if OK, or a plist describing the issue if problematic."
  (let ((url (plist-get link-plist :url)))
    (when (and url current-courseid
               (string-match org-lms-cu-canvas-url-pattern url))
      (let ((found-id (match-string 1 url)))
        (unless (string= found-id current-courseid)
          (list :type "course-id"
                :message (format "Link contains course ID %s, expected %s"
                                 found-id current-courseid)
                :found found-id
                :expected current-courseid))))))

(defun org-lms-cu--check-link-year (link-plist current-year)
  "Check if LINK-PLIST contains outdated year references.
CURRENT-YEAR is the current academic year as a string.
Returns nil if OK, or a plist describing the issue if problematic."
  (let ((url (plist-get link-plist :url))
        (current-year-int (if (stringp current-year)
                              (string-to-number current-year)
                            current-year)))
    (when url
      (catch 'found
        (dolist (pattern org-lms-cu-year-patterns)
          (when (string-match pattern url)
            (let ((found-year (match-string 1 url)))
              ;; Handle term names like "Fall" - the year is in group 2
              (when (member found-year '("Fall" "Winter" "Spring" "Summer"))
                (setq found-year (match-string 2 url)))
              (when found-year
                (let ((found-year-int (string-to-number found-year)))
                  (when (< found-year-int current-year-int)
                    (throw 'found
                           (list :type "year"
                                 :message (format "Link contains year %s, current year is %s"
                                                  found-year current-year)
                                 :found found-year
                                 :expected current-year))))))))
        nil))))

(defun org-lms-cu-find-outdated-links (&optional file)
  "Find all outdated links in FILE or current buffer.
Returns list of issue plists suitable for results buffer."
  (let* ((file (or file (buffer-file-name)))
         (courseid (org-lms-get-keyword "ORG_LMS_COURSEID" file))
         (start-date (org-lms-get-keyword "COURSE_START_DATE" file))
         (current-year (or org-lms-cu-current-year
                           (when start-date
                             (format-time-string "%Y" (ts-unix (ts-parse start-date))))
                           (format-time-string "%Y")))
         (links (org-lms-cu--extract-links-from-buffer))
         (issues '()))
    (dolist (link links)
      (let ((issue-id (org-lms-cu--check-link-course-id link courseid))
            (issue-year (org-lms-cu--check-link-year link current-year)))
        (when issue-id
          (push (append (list :file file
                              :line (plist-get link :line)
                              :begin (plist-get link :begin)
                              :end (plist-get link :end)
                              :context (plist-get link :url))
                        issue-id)
                issues))
        (when issue-year
          (push (append (list :file file
                              :line (plist-get link :line)
                              :begin (plist-get link :begin)
                              :end (plist-get link :end)
                              :context (plist-get link :url))
                        issue-year)
                issues))))
    (nreverse issues)))

;;; Date Scanning Functions

(defun org-lms-cu--extract-timestamps-from-buffer ()
  "Extract all org timestamps from current buffer with position info.
Returns list of plists with :timestamp, :begin, :end, :line, :raw-value."
  (let ((timestamps '()))
    (org-element-map (org-element-parse-buffer) 'timestamp
      (lambda (ts)
        (push (list :timestamp ts
                    :begin (org-element-property :begin ts)
                    :end (org-element-property :end ts)
                    :line (line-number-at-pos (org-element-property :begin ts))
                    :raw-value (org-element-property :raw-value ts)
                    :year (org-element-property :year-start ts)
                    :month (org-element-property :month-start ts)
                    :day (org-element-property :day-start ts))
              timestamps)))
    (nreverse timestamps)))

(defun org-lms-cu--parse-course-dates (&optional file)
  "Parse COURSE_START_DATE and COURSE_END_DATE from FILE or current buffer.
Returns plist (:start TS-OBJECT :end TS-OBJECT) using ts.el, or nil if not set."
  (let ((start-str (org-lms-get-keyword "COURSE_START_DATE" file))
        (end-str (org-lms-get-keyword "COURSE_END_DATE" file)))
    (when (and start-str end-str)
      (list :start (ts-parse start-str)
            :end (ts-parse end-str)))))

(defun org-lms-cu--check-date-in-range (timestamp-plist course-dates)
  "Check if TIMESTAMP-PLIST falls within COURSE-DATES range.
Returns nil if OK, or a plist describing the issue if out of range."
  (when course-dates
    (let* ((year (plist-get timestamp-plist :year))
           (month (plist-get timestamp-plist :month))
           (day (plist-get timestamp-plist :day))
           (start-ts (plist-get course-dates :start))
           (end-ts (plist-get course-dates :end)))
      (when (and year month day start-ts end-ts)
        (let ((date-ts (ts-parse (format "%04d-%02d-%02d" year month day))))
          (cond
           ((ts< date-ts start-ts)
            (list :type "date-before"
                  :message (format "Date %04d-%02d-%02d is before course start %s"
                                   year month day
                                   (ts-format "%Y-%m-%d" start-ts))))
           ((ts> date-ts end-ts)
            (list :type "date-after"
                  :message (format "Date %04d-%02d-%02d is after course end %s"
                                   year month day
                                   (ts-format "%Y-%m-%d" end-ts))))))))))

(defun org-lms-cu-find-outdated-dates (&optional file)
  "Find all dates outside course date range in FILE or current buffer.
Returns list of issue plists suitable for results buffer."
  (let* ((file (or file (buffer-file-name)))
         (course-dates (org-lms-cu--parse-course-dates file))
         (timestamps (org-lms-cu--extract-timestamps-from-buffer))
         (issues '()))
    (unless course-dates
      (user-error "COURSE_START_DATE and COURSE_END_DATE keywords required"))
    (dolist (ts timestamps)
      (let ((issue (org-lms-cu--check-date-in-range ts course-dates)))
        (when issue
          (push (append (list :file file
                              :line (plist-get ts :line)
                              :begin (plist-get ts :begin)
                              :end (plist-get ts :end)
                              :context (plist-get ts :raw-value))
                        issue)
                issues))))
    (nreverse issues)))

;;; Results Buffer Mode

(defvar org-lms-cu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-lms-cu-goto-issue)
    (define-key map (kbd "o") #'org-lms-cu-goto-issue-other-window)
    (define-key map (kbd "n") #'org-lms-cu-next-issue)
    (define-key map (kbd "p") #'org-lms-cu-previous-issue)
    (define-key map (kbd "e") #'org-lms-cu-toggle-edit-mode)
    (define-key map (kbd "C-c C-c") #'org-lms-cu-apply-changes)
    (define-key map (kbd "g") #'org-lms-cu-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "M-g M-n") #'next-error)
    (define-key map (kbd "M-g M-p") #'previous-error)
    map)
  "Keymap for `org-lms-cu-mode'.")

(define-derived-mode org-lms-cu-mode special-mode "CourseUpdate"
  "Major mode for viewing and fixing outdated course content.

\\{org-lms-cu-mode-map}"
  :group 'org-lms-course-update
  (setq-local revert-buffer-function #'org-lms-cu--revert-buffer)
  (setq-local next-error-function #'org-lms-cu--next-error-function)
  (setq truncate-lines t))

;;; Results Buffer Population

(defun org-lms-cu--button-action (button)
  "Action for clicking a BUTTON in results buffer."
  (let ((issue (button-get button 'org-lms-cu-issue)))
    (when issue
      (let ((file (plist-get issue :file))
            (begin (plist-get issue :begin)))
        (find-file-other-window file)
        (goto-char begin)))))

(defun org-lms-cu--insert-issue (issue)
  "Insert formatted ISSUE into results buffer with navigation properties."
  (let* ((file (plist-get issue :file))
         (line (plist-get issue :line))
         (type (plist-get issue :type))
         (message (plist-get issue :message))
         (context (plist-get issue :context))
         (begin (plist-get issue :begin))
         (start (point)))
    ;; Insert file:line prefix
    (insert (propertize (format "%s:%d: "
                                (file-name-nondirectory file) line)
                        'face 'compilation-info
                        'org-lms-cu-file file
                        'org-lms-cu-line line
                        'org-lms-cu-begin begin
                        'org-lms-cu-issue issue))
    ;; Insert issue type
    (insert (propertize (format "[%s] " type)
                        'face 'compilation-warning))
    ;; Insert context (the problematic text) - this is the editable part
    (let ((context-start (point)))
      (insert context)
      (put-text-property context-start (point) 'org-lms-cu-editable t)
      (put-text-property context-start (point) 'org-lms-cu-issue issue))
    (insert "\n")
    ;; Insert message/explanation
    (insert (propertize (format "    %s\n" message)
                        'face 'font-lock-comment-face))
    ;; Make the whole entry a button
    (make-text-button start (point)
                      'action #'org-lms-cu--button-action
                      'org-lms-cu-issue issue
                      'follow-link t)))

(defun org-lms-cu--populate-results-buffer (issues check-type scope)
  "Populate current buffer with formatted ISSUES.
CHECK-TYPE is `links', `dates', or `all'.  SCOPE is the scope used."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Course Update Check: %s (%s)\n"
                                (pcase check-type
                                  ('links "Outdated Links")
                                  ('dates "Outdated Dates")
                                  ('all "All Issues"))
                                (symbol-name scope))
                        'face 'info-title-1))
    (insert (format "Found %d issue%s:\n\n"
                    (length issues)
                    (if (= 1 (length issues)) "" "s")))
    (insert (propertize "Keys: RET/o=goto  n/p=next/prev  e=edit  C-c C-c=apply  g=refresh  q=quit\n\n"
                        'face 'font-lock-comment-face))
    (setq org-lms-cu--issues issues)
    (setq org-lms-cu--check-type check-type)
    (setq org-lms-cu--scope scope)
    (if issues
        (dolist (issue issues)
          (org-lms-cu--insert-issue issue))
      (insert (propertize "No issues found!\n" 'face 'success)))
    (goto-char (point-min))))

;;; Navigation Functions

(defun org-lms-cu-goto-issue ()
  "Jump to the source location of issue at point."
  (interactive)
  (let ((file (get-text-property (point) 'org-lms-cu-file))
        (begin (get-text-property (point) 'org-lms-cu-begin)))
    (if (and file begin)
        (progn
          (find-file file)
          (goto-char begin))
      (user-error "No issue at point"))))

(defun org-lms-cu-goto-issue-other-window ()
  "Jump to issue source in other window, keeping results buffer visible."
  (interactive)
  (let ((file (get-text-property (point) 'org-lms-cu-file))
        (begin (get-text-property (point) 'org-lms-cu-begin)))
    (if (and file begin)
        (progn
          (find-file-other-window file)
          (goto-char begin))
      (user-error "No issue at point"))))

(defun org-lms-cu-next-issue ()
  "Move to next issue in results buffer."
  (interactive)
  (let ((pos (next-single-property-change (point) 'org-lms-cu-issue)))
    (if pos
        (goto-char pos)
      (user-error "No more issues"))))

(defun org-lms-cu-previous-issue ()
  "Move to previous issue in results buffer."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'org-lms-cu-issue)))
    (if pos
        (goto-char pos)
      (user-error "No previous issue"))))

(defun org-lms-cu--next-error-function (n &optional reset)
  "Move to Nth next issue.  Integrates with `next-error'.
If RESET is non-nil, start from the beginning."
  (when reset (goto-char (point-min)))
  (dotimes (_ (abs n))
    (if (> n 0)
        (org-lms-cu-next-issue)
      (org-lms-cu-previous-issue)))
  (org-lms-cu-goto-issue-other-window))

(defun org-lms-cu--revert-buffer (_ignore-auto _noconfirm)
  "Revert the results buffer by re-running the check."
  (org-lms-cu-refresh))

(defun org-lms-cu-refresh ()
  "Refresh the results buffer by re-running the check."
  (interactive)
  (when (and org-lms-cu--check-type org-lms-cu--scope)
    (let ((check-type org-lms-cu--check-type)
          (scope org-lms-cu--scope))
      (pcase check-type
        ('links (org-lms-check-outdated-links scope))
        ('dates (org-lms-check-outdated-dates scope))
        ('all (org-lms-check-course-updates scope))))))

;;; Edit Mode Implementation

(defun org-lms-cu-toggle-edit-mode ()
  "Toggle edit mode in results buffer.
When enabled, allows direct editing that propagates to source files."
  (interactive)
  (setq org-lms-cu--edit-mode (not org-lms-cu--edit-mode))
  (if org-lms-cu--edit-mode
      (progn
        (setq buffer-read-only nil)
        (org-lms-cu--setup-edit-overlays)
        (message "Edit mode enabled. Edit the highlighted URLs/dates, then C-c C-c to apply."))
    (setq buffer-read-only t)
    (org-lms-cu--remove-edit-overlays)
    (message "Edit mode disabled.")))

(defun org-lms-cu--setup-edit-overlays ()
  "Create overlays for editable regions (the context text of each issue)."
  (setq org-lms-cu--edit-overlays nil)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((editable-start (text-property-any (point) (line-end-position)
                                               'org-lms-cu-editable t)))
        (when editable-start
          (let* ((editable-end (or (next-single-property-change
                                    editable-start 'org-lms-cu-editable)
                                   (line-end-position)))
                 (issue (get-text-property editable-start 'org-lms-cu-issue))
                 (ov (make-overlay editable-start editable-end)))
            (overlay-put ov 'face '(:background "#ffffcc"))
            (overlay-put ov 'org-lms-cu-issue issue)
            (overlay-put ov 'org-lms-cu-original
                         (buffer-substring-no-properties editable-start editable-end))
            (push ov org-lms-cu--edit-overlays))))
      (forward-line 1))))

(defun org-lms-cu--remove-edit-overlays ()
  "Remove all edit tracking overlays."
  (dolist (ov org-lms-cu--edit-overlays)
    (delete-overlay ov))
  (setq org-lms-cu--edit-overlays nil))

(defun org-lms-cu-apply-changes ()
  "Apply all edits in results buffer back to source files."
  (interactive)
  (unless org-lms-cu--edit-mode
    (user-error "Not in edit mode. Press 'e' to enable editing"))
  (let ((changes-by-file (make-hash-table :test 'equal))
        (change-count 0))
    ;; Collect changes grouped by file
    (dolist (ov org-lms-cu--edit-overlays)
      (when (overlay-buffer ov)  ; overlay still valid
        (let* ((issue (overlay-get ov 'org-lms-cu-issue))
               (file (plist-get issue :file))
               (original (overlay-get ov 'org-lms-cu-original))
               (new-text (buffer-substring-no-properties
                          (overlay-start ov) (overlay-end ov))))
          (unless (string= original new-text)
            (cl-incf change-count)
            (puthash file
                     (cons (list :begin (plist-get issue :begin)
                                 :end (plist-get issue :end)
                                 :original original
                                 :replacement new-text)
                           (gethash file changes-by-file))
                     changes-by-file)))))
    (if (= change-count 0)
        (message "No changes to apply.")
      ;; Apply changes to each file (in reverse order to preserve positions)
      (maphash (lambda (file changes)
                 (with-current-buffer (find-file-noselect file)
                   (save-excursion
                     ;; Sort by position descending so changes don't affect later positions
                     (dolist (change (sort changes
                                           (lambda (a b)
                                             (> (plist-get a :begin)
                                                (plist-get b :begin)))))
                       (let ((begin (plist-get change :begin))
                             (end (plist-get change :end))
                             (replacement (plist-get change :replacement)))
                         (goto-char begin)
                         (delete-region begin end)
                         (insert replacement))))
                   (save-buffer)))
               changes-by-file)
      (message "Applied %d change%s to %d file%s."
               change-count (if (= 1 change-count) "" "s")
               (hash-table-count changes-by-file)
               (if (= 1 (hash-table-count changes-by-file)) "" "s"))
      ;; Exit edit mode and refresh
      (org-lms-cu-toggle-edit-mode)
      (org-lms-cu-refresh))))

;;; Entry Point Functions

(defun org-lms-cu--display-results (issues check-type scope)
  "Display ISSUES in results buffer.
CHECK-TYPE is `links', `dates', or `all'.  SCOPE is the scope used."
  (let ((buf (get-buffer-create org-lms-cu-buffer-name)))
    (with-current-buffer buf
      (org-lms-cu-mode)
      (org-lms-cu--populate-results-buffer issues check-type scope))
    (pop-to-buffer buf)
    (setq next-error-last-buffer buf)))

;;;###autoload
(defun org-lms-check-outdated-links (&optional scope)
  "Check for outdated Canvas links in org files.
SCOPE determines which files to check: `buffer', `directory', or `project'.
Interactively, prompts for scope."
  (interactive)
  (let* ((scope (or scope (org-lms-cu--prompt-for-scope)))
         (files (org-lms-cu--get-scope-files scope))
         (all-issues '()))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (condition-case err
              (let ((issues (org-lms-cu-find-outdated-links file)))
                (setq all-issues (append all-issues issues)))
            (error (message "Error checking %s: %s" file (error-message-string err)))))))
    (org-lms-cu--display-results all-issues 'links scope)))

;;;###autoload
(defun org-lms-check-outdated-dates (&optional scope)
  "Check for dates outside course date range in org files.
SCOPE determines which files to check: `buffer', `directory', or `project'.
Interactively, prompts for scope."
  (interactive)
  (let* ((scope (or scope (org-lms-cu--prompt-for-scope)))
         (files (org-lms-cu--get-scope-files scope))
         (all-issues '()))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (condition-case err
              (let ((issues (org-lms-cu-find-outdated-dates file)))
                (setq all-issues (append all-issues issues)))
            (error nil)))))  ; Silently skip files without course dates
    (org-lms-cu--display-results all-issues 'dates scope)))

;;;###autoload
(defun org-lms-check-course-updates (&optional scope)
  "Check for all types of outdated course content.
Combines link and date checks.  SCOPE determines which files to check:
`buffer', `directory', or `project'.  Interactively, prompts for scope."
  (interactive)
  (let* ((scope (or scope (org-lms-cu--prompt-for-scope)))
         (files (org-lms-cu--get-scope-files scope))
         (all-issues '()))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (condition-case nil
              (setq all-issues (append all-issues
                                       (org-lms-cu-find-outdated-links file)))
            (error nil))
          (condition-case nil
              (setq all-issues (append all-issues
                                       (org-lms-cu-find-outdated-dates file)))
            (error nil)))))
    (org-lms-cu--display-results all-issues 'all scope)))

(provide 'org-lms-course-update)
;;; org-lms-course-update.el ends here
