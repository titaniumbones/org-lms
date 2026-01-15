;;; org-lms-migration.el --- Course migration for org-lms -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Matt Price

;; Author: Matt Price
;; Keywords: org, lms, canvas, migration

;;; Commentary:

;; Course migration system for org-lms that integrates with Canvas LMS's
;; Content Migration API.  This module provides:
;;
;; - Canvas-first migration: Use Canvas to copy course content, then update
;;   org-mode properties with new Canvas IDs using the asset_id_mapping API
;; - Snapshot/rollback: Create snapshots before migration for safe rollback
;; - Multi-content support: Assignments, quizzes, modules, pages, etc.
;;
;; Main entry point:
;;   M-x org-lms-migrate-course-wizard
;;
;; Individual commands:
;;   M-x org-lms-migrate-prepare    - Create snapshot and validate
;;   M-x org-lms-migrate-execute    - Run Canvas migration
;;   M-x org-lms-migrate-update-ids - Apply ID mapping to org files
;;   M-x org-lms-migrate-verify     - Verify migration success
;;   M-x org-lms-migrate-rollback   - Restore from snapshot
;;
;; Canvas API Reference:
;;   https://canvas.instructure.com/doc/api/content_migrations.html

;;; Code:

;;;; Dependencies

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'json)

;; org-lms is required at runtime but we defer loading
;; to avoid dependency issues during byte-compilation
(declare-function org-lms-canvas-request "org-lms")
(declare-function org-lms-get-keyword "org-lms")
(defvar org-lms-baseurl)
(defvar org-lms-token)

(defun org-lms-mig--ensure-org-lms ()
  "Ensure org-lms is loaded, erroring if unavailable."
  (unless (featurep 'org-lms)
    (require 'org-lms)))

;; Optional dependencies
(require 'ts nil t)
(require 'dash nil t)

;;;; Customization

(defgroup org-lms-migration nil
  "Course migration utilities for org-lms."
  :group 'org-lms
  :prefix "org-lms-mig-")

(defcustom org-lms-mig-poll-interval 5
  "Seconds between migration status polls."
  :type 'integer
  :group 'org-lms-migration)

(defcustom org-lms-mig-poll-timeout 600
  "Maximum seconds to wait for migration completion."
  :type 'integer
  :group 'org-lms-migration)

(defcustom org-lms-mig-buffer-name "*Course Migration*"
  "Name of the buffer for displaying migration results."
  :type 'string
  :group 'org-lms-migration)

(defcustom org-lms-mig-snapshot-directory
  (expand-file-name "migration-snapshots" user-emacs-directory)
  "Directory to store migration snapshots."
  :type 'directory
  :group 'org-lms-migration)

;;;; Variables

(defvar org-lms-mig--current-state nil
  "Current migration state plist.
Structure:
  :migration-id       - Unique identifier for this migration
  :source-course-id   - Canvas ID of source course
  :dest-course-id     - Canvas ID of destination course
  :status             - pending|in-progress|completed|failed
  :canvas-migration-id - Canvas's migration ID
  :canvas-status      - Canvas migration workflow_state
  :snapshot           - Pre-migration snapshot data
  :asset-mapping      - Parsed asset_id_mapping from Canvas
  :updates            - List of property updates made
  :errors             - List of errors encountered
  :warnings           - List of warnings")

(defvar-local org-lms-mig--results nil
  "Migration results for display in results buffer.")

;;;; Property Registry

(defconst org-lms-mig--property-registry
  '((assignment . (:canvas-key "assignments"
                   :properties ("CANVASID")
                   :derived-props ("CANVAS_HTML_URL" "SUBMISSIONS_DOWNLOAD_URL")))
    (quiz . (:canvas-key "quizzes"
             :properties ("QUIZ_ID")
             :derived-props ("QUIZ_HTML_URL")))
    (new-quiz . (:canvas-key "assignments"  ; New Quizzes are assignment-backed
                 :properties ("NEW_QUIZ_ID")
                 :detect-prop "NEW_QUIZ_ID"))
    (question . (:canvas-key "quiz_questions"
                 :properties ("QUESTION_ID")))
    (question-group . (:canvas-key "quiz_groups"
                       :properties ("QUESTION_GROUP_ID")))
    (module . (:canvas-key "modules"
               :properties ("MODULE_ID")))
    (module-item . (:canvas-key "module_items"
                    :properties ("MODULE_ITEM_ID")))
    (page . (:canvas-key "pages"
             :properties ("CANVAS_PAGE_URL" "CANVAS_SHORT_URL")))
    (discussion . (:canvas-key "discussion_topics"
                   :properties ("ORG_LMS_ANNOUNCEMENT_ID")))
    (file . (:canvas-key "files"
             :properties ("CANVAS_FILE_ID")))
    (rubric . (:canvas-key "rubrics"
               :properties ("RUBRIC_ID")))
    (assignment-group . (:canvas-key "assignment_groups"
                         :properties ("GROUP_ID"))))
  "Registry mapping content types to Canvas API keys and org properties.")

;;;; Canvas Migration API Functions

(defun org-lms-mig-create (dest-course-id source-course-id &optional options)
  "Create a content migration from SOURCE-COURSE-ID to DEST-COURSE-ID.
OPTIONS is a plist that can include:
  :shift-dates     - Non-nil to enable date shifting
  :old-start-date  - Source course start date (ISO format)
  :new-start-date  - Destination course start date
  :old-end-date    - Source course end date
  :new-end-date    - Destination course end date
  :selective       - Non-nil to use selective import

Returns the migration response plist from Canvas."
  (org-lms-mig--ensure-org-lms)
  (let ((params `(("migration_type" . "course_copy_importer")
                  ("settings[source_course_id]" . ,source-course-id))))
    ;; Add date shift options if specified
    (when (plist-get options :shift-dates)
      (push '("date_shift_options[shift_dates]" . "true") params)
      (when-let ((old-start (plist-get options :old-start-date)))
        (push `("date_shift_options[old_start_date]" . ,old-start) params))
      (when-let ((new-start (plist-get options :new-start-date)))
        (push `("date_shift_options[new_start_date]" . ,new-start) params))
      (when-let ((old-end (plist-get options :old-end-date)))
        (push `("date_shift_options[old_end_date]" . ,old-end) params))
      (when-let ((new-end (plist-get options :new-end-date)))
        (push `("date_shift_options[new_end_date]" . ,new-end) params)))
    ;; Selective import
    (when (plist-get options :selective)
      (push '("selective_import" . "true") params))
    ;; Make the API call
    (org-lms-canvas-request
     (format "courses/%s/content_migrations" dest-course-id)
     "POST"
     params)))

(defun org-lms-mig-get-status (course-id migration-id)
  "Get the status of migration MIGRATION-ID in COURSE-ID.
Returns the ContentMigration object as a plist."
  (org-lms-mig--ensure-org-lms)
  (org-lms-canvas-request
   (format "courses/%s/content_migrations/%s" course-id migration-id)
   "GET"))

(defun org-lms-mig-get-asset-mapping (course-id migration-id)
  "Get the asset ID mapping for completed migration MIGRATION-ID.
Returns a plist mapping content types to alists of (old-id . new-id)."
  (org-lms-mig--ensure-org-lms)
  (org-lms-canvas-request
   (format "courses/%s/content_migrations/%s/asset_id_mapping" course-id migration-id)
   "GET"))

(defun org-lms-mig-poll-until-complete (course-id migration-id &optional timeout callback)
  "Poll migration status until complete or TIMEOUT seconds elapsed.
CALLBACK is called with status plist on each poll if provided.
Returns final status plist, or signals error on timeout/failure."
  (let ((timeout (or timeout org-lms-mig-poll-timeout))
        (start-time (float-time))
        (status nil)
        (done nil))
    (while (and (not done)
                (< (- (float-time) start-time) timeout))
      (setq status (org-lms-mig-get-status course-id migration-id))
      (let ((workflow-state (plist-get status :workflow_state)))
        (message "Migration status: %s (progress: %s%%)"
                 workflow-state
                 (or (plist-get status :completion) 0))
        (when callback
          (funcall callback status))
        (cond
         ((member workflow-state '("imported" "completed"))
          (setq done t))
         ((member workflow-state '("failed" "pre_process_error"))
          (error "Migration failed: %s" (plist-get status :migration_issues)))
         (t
          (sleep-for org-lms-mig-poll-interval)))))
    (unless done
      (error "Migration timed out after %d seconds" timeout))
    status))

;;;; Snapshot Functions

(defun org-lms-mig--generate-snapshot-id ()
  "Generate a unique snapshot identifier."
  (format "%s-%s"
          (format-time-string "%Y%m%d-%H%M%S")
          (substring (md5 (format "%s" (random))) 0 8)))

(defun org-lms-mig--collect-file-properties (file)
  "Collect all Canvas-related properties from FILE.
Returns a plist with :keywords and :headings."
  (org-lms-mig--ensure-org-lms)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (let ((keywords nil)
            (headings nil))
        ;; Collect file-level keywords
        (dolist (kw '("ORG_LMS_COURSEID" "COURSE_START_DATE" "COURSE_END_DATE"))
          (when-let ((val (org-lms-get-keyword kw)))
            (push (cons kw val) keywords)))
        ;; Collect heading properties
        (org-map-entries
         (lambda ()
           (let ((props nil)
                 (pos (point))
                 (heading (org-get-heading t t t t)))
             ;; Check all property types
             (dolist (type-entry org-lms-mig--property-registry)
               (let ((prop-names (plist-get (cdr type-entry) :properties)))
                 (dolist (prop prop-names)
                   (when-let ((val (org-entry-get nil prop)))
                     (push (cons prop val) props)))))
             ;; Also collect derived properties that contain course ID
             (dolist (prop '("CANVAS_HTML_URL" "SUBMISSIONS_DOWNLOAD_URL"
                            "QUIZ_HTML_URL" "ORG_LMS_ANNOUNCEMENT_URL"))
               (when-let ((val (org-entry-get nil prop)))
                 (push (cons prop val) props)))
             (when props
               (push (list :position pos
                           :heading heading
                           :properties (nreverse props))
                     headings)))))
        (list :file file
              :keywords (nreverse keywords)
              :headings (nreverse headings))))))

(defun org-lms-mig-create-snapshot (&optional scope)
  "Create a snapshot of current Canvas IDs for rollback.
SCOPE is one of: `buffer', `directory', `project'.
Returns the snapshot plist."
  (interactive)
  (let* ((scope (or scope 'buffer))
         (files (org-lms-mig--get-scope-files scope))
         (snapshot-id (org-lms-mig--generate-snapshot-id))
         (file-data nil))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (push (org-lms-mig--collect-file-properties file) file-data)))
    (let ((snapshot (list :id snapshot-id
                          :timestamp (current-time)
                          :scope scope
                          :files (nreverse file-data))))
      ;; Save snapshot to file
      (org-lms-mig--save-snapshot snapshot)
      (message "Created snapshot %s with %d files" snapshot-id (length file-data))
      snapshot)))

(defun org-lms-mig--save-snapshot (snapshot)
  "Save SNAPSHOT to the snapshot directory."
  (unless (file-directory-p org-lms-mig-snapshot-directory)
    (make-directory org-lms-mig-snapshot-directory t))
  (let ((file (expand-file-name
               (format "%s.el" (plist-get snapshot :id))
               org-lms-mig-snapshot-directory)))
    (with-temp-file file
      (insert ";;; Migration Snapshot\n")
      (insert (format ";;; Created: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S"
                                          (plist-get snapshot :timestamp))))
      (pp snapshot (current-buffer)))
    file))

(defun org-lms-mig--load-snapshot (snapshot-id)
  "Load snapshot with SNAPSHOT-ID from disk."
  (let ((file (expand-file-name
               (format "%s.el" snapshot-id)
               org-lms-mig-snapshot-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Skip comments
        (while (looking-at "^;;")
          (forward-line 1))
        (read (current-buffer))))))

(defun org-lms-mig-restore-snapshot (snapshot)
  "Restore org properties from SNAPSHOT.
SNAPSHOT can be a snapshot plist or a snapshot ID string."
  (interactive
   (list (completing-read
          "Snapshot to restore: "
          (directory-files org-lms-mig-snapshot-directory nil "\\.el$")
          nil t)))
  (when (stringp snapshot)
    (setq snapshot (org-lms-mig--load-snapshot
                    (replace-regexp-in-string "\\.el$" "" snapshot))))
  (unless snapshot
    (error "No snapshot found"))
  (let ((restored 0))
    (dolist (file-data (plist-get snapshot :files))
      (let ((file (plist-get file-data :file)))
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            ;; Restore keywords
            (dolist (kw (plist-get file-data :keywords))
              ;; Keywords are trickier - need to replace in-buffer
              ;; For now, just log them
              (message "Would restore keyword %s = %s in %s"
                       (car kw) (cdr kw) file))
            ;; Restore heading properties
            (dolist (heading-data (plist-get file-data :headings))
              (goto-char (plist-get heading-data :position))
              ;; Verify we're at the right heading
              (when (string= (org-get-heading t t t t)
                             (plist-get heading-data :heading))
                (dolist (prop (plist-get heading-data :properties))
                  (org-set-property (car prop) (cdr prop))
                  (cl-incf restored))))
            (save-buffer)))))
    (message "Restored %d properties from snapshot" restored)))

;;;; Scope Selection

(defun org-lms-mig--get-scope-files (scope)
  "Get list of org files for SCOPE.
SCOPE is one of: `buffer', `directory', `project'."
  (pcase scope
    ('buffer (list (buffer-file-name)))
    ('directory
     (directory-files default-directory t "\\.org$"))
    ('project
     (if (and (fboundp 'projectile-project-root)
              (projectile-project-root))
         (seq-filter (lambda (f) (string-suffix-p ".org" f))
                     (projectile-project-files (projectile-project-root)))
       ;; Fallback to directory
       (directory-files default-directory t "\\.org$" t)))))

(defun org-lms-mig--prompt-for-scope ()
  "Interactively prompt user to select scope."
  (let* ((choices '(("Current buffer" . buffer)
                    ("All org files in directory" . directory)
                    ("All org files in project" . project)))
         (choice (completing-read "Scope: " choices nil t)))
    (cdr (assoc choice choices))))

;;;; Property Update Functions

(defun org-lms-mig--parse-asset-mapping (raw-mapping)
  "Parse RAW-MAPPING from Canvas API into usable hash tables.
Returns a plist mapping canvas-key to hash-table of old-id -> new-id."
  (let ((result nil))
    (cl-loop for (key . val) on raw-mapping by #'cddr
             do (let ((ht (make-hash-table :test 'equal))
                      (key-str (symbol-name key)))
                  ;; val is an alist like ((\"old\" . \"new\") ...)
                  (dolist (pair val)
                    (puthash (car pair) (cdr pair) ht))
                  (setq result (plist-put result (intern key-str) ht))))
    result))

(defun org-lms-mig--lookup-new-id (mapping canvas-key old-id)
  "Look up the new ID for OLD-ID of type CANVAS-KEY in MAPPING."
  (when-let ((ht (plist-get mapping (intern canvas-key))))
    (gethash (if (numberp old-id) (number-to-string old-id) old-id) ht)))

(defun org-lms-mig--update-derived-url (url old-course-id new-course-id old-obj-id new-obj-id)
  "Update URL by replacing course ID and object ID.
Returns the updated URL string."
  (let ((result url))
    ;; Replace course ID in URL
    (when (and old-course-id new-course-id)
      (setq result (replace-regexp-in-string
                    (format "/courses/%s/" old-course-id)
                    (format "/courses/%s/" new-course-id)
                    result)))
    ;; Replace object ID if present
    (when (and old-obj-id new-obj-id)
      (setq result (replace-regexp-in-string
                    (format "/%s\\([^0-9]\\|$\\)" old-obj-id)
                    (format "/%s\\1" new-obj-id)
                    result)))
    result))

(defun org-lms-mig-update-heading-properties (mapping old-course-id new-course-id)
  "Update Canvas IDs for the heading at point using MAPPING.
OLD-COURSE-ID and NEW-COURSE-ID are used for URL updates.
Returns list of updates made."
  (let ((updates nil))
    ;; Iterate through all content types
    (dolist (type-entry org-lms-mig--property-registry)
      (let* ((type (car type-entry))
             (config (cdr type-entry))
             (canvas-key (plist-get config :canvas-key))
             (properties (plist-get config :properties))
             (derived-props (plist-get config :derived-props)))
        ;; Update primary properties
        (dolist (prop properties)
          (when-let ((old-id (org-entry-get nil prop)))
            (when-let ((new-id (org-lms-mig--lookup-new-id mapping canvas-key old-id)))
              (org-set-property prop new-id)
              (push (list :property prop
                          :old-value old-id
                          :new-value new-id
                          :type type)
                    updates))))
        ;; Update derived URL properties
        (dolist (prop derived-props)
          (when-let ((url (org-entry-get nil prop)))
            (let* ((primary-prop (car properties))
                   (old-obj-id (org-entry-get nil primary-prop))
                   (new-obj-id (org-lms-mig--lookup-new-id mapping canvas-key old-obj-id))
                   (new-url (org-lms-mig--update-derived-url
                             url old-course-id new-course-id old-obj-id new-obj-id)))
              (unless (string= url new-url)
                (org-set-property prop new-url)
                (push (list :property prop
                            :old-value url
                            :new-value new-url
                            :type 'derived)
                      updates)))))))
    (nreverse updates)))

(defun org-lms-mig-update-org-properties (mapping old-course-id new-course-id &optional scope)
  "Update all org properties using MAPPING.
OLD-COURSE-ID and NEW-COURSE-ID for URL regeneration.
SCOPE is one of: `buffer', `directory', `project'.
Returns list of all updates made."
  (let* ((scope (or scope 'buffer))
         (files (org-lms-mig--get-scope-files scope))
         (all-updates nil))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (let ((file-updates nil))
            ;; Update ORG_LMS_COURSEID keyword
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward
                     (format "^#\\+ORG_LMS_COURSEID:\\s-*%s\\s-*$" old-course-id)
                     nil t)
                (replace-match (format "#+ORG_LMS_COURSEID: %s" new-course-id))
                (push (list :file file
                            :type 'keyword
                            :property "ORG_LMS_COURSEID"
                            :old-value old-course-id
                            :new-value new-course-id)
                      file-updates)))
            ;; Update heading properties
            (org-map-entries
             (lambda ()
               (let ((updates (org-lms-mig-update-heading-properties
                               mapping old-course-id new-course-id)))
                 (when updates
                   (dolist (update updates)
                     (push (plist-put update :file file) file-updates))))))
            (when file-updates
              (save-buffer)
              (setq all-updates (append all-updates file-updates)))))))
    all-updates))

;;;; Link Update Functions

(defun org-lms-mig-update-links (old-course-id new-course-id &optional scope)
  "Update all Canvas links from OLD-COURSE-ID to NEW-COURSE-ID.
SCOPE is one of: `buffer', `directory', `project'.
Returns count of links updated."
  (let* ((scope (or scope 'buffer))
         (files (org-lms-mig--get-scope-files scope))
         (count 0)
         (pattern (format "\\(https?://[^/]+/courses/\\)%s\\(/\\|\\]\\|)\\)"
                          old-course-id)))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward pattern nil t)
              (replace-match (format "\\1%s\\2" new-course-id))
              (cl-incf count)))
          (when (buffer-modified-p)
            (save-buffer)))))
    count))

;;;; Verification Functions

(defun org-lms-mig-verify (mapping &optional scope)
  "Verify that migration completed successfully.
Check that all old IDs have mappings.
SCOPE is one of: `buffer', `directory', `project'.
Returns a plist with :success, :warnings, :errors."
  (let* ((scope (or scope 'buffer))
         (files (org-lms-mig--get-scope-files scope))
         (unmapped nil)
         (verified 0))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (dolist (type-entry org-lms-mig--property-registry)
               (let* ((config (cdr type-entry))
                      (canvas-key (plist-get config :canvas-key))
                      (properties (plist-get config :properties)))
                 (dolist (prop properties)
                   (when-let ((id (org-entry-get nil prop)))
                     ;; Check if this ID exists in the new course
                     ;; (For now, just verify the mapping exists)
                     (if (org-lms-mig--lookup-new-id mapping canvas-key id)
                         (cl-incf verified)
                       (push (list :file file
                                   :heading (org-get-heading t t t t)
                                   :property prop
                                   :value id)
                             unmapped)))))))))))
    (list :success (null unmapped)
          :verified-count verified
          :unmapped unmapped
          :warnings (when unmapped
                      (format "%d items could not be mapped" (length unmapped))))))

;;;; Results Buffer

(defvar org-lms-mig-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-lms-mig-results-goto-source)
    (define-key map (kbd "n") #'org-lms-mig-results-next)
    (define-key map (kbd "p") #'org-lms-mig-results-prev)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for migration results buffer.")

(define-derived-mode org-lms-mig-results-mode special-mode "MigResults"
  "Mode for displaying migration results.
\\{org-lms-mig-results-mode-map}"
  (setq buffer-read-only t))

(defun org-lms-mig-results-goto-source ()
  "Jump to the source location of the result at point."
  (interactive)
  (when-let ((result (get-text-property (point) 'mig-result)))
    (let ((file (plist-get result :file))
          (pos (plist-get result :position)))
      (when file
        (find-file-other-window file)
        (when pos
          (goto-char pos))))))

(defun org-lms-mig-results-next ()
  "Move to next result."
  (interactive)
  (let ((pos (next-single-property-change (point) 'mig-result)))
    (when pos (goto-char pos))))

(defun org-lms-mig-results-prev ()
  "Move to previous result."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'mig-result)))
    (when pos (goto-char pos))))

(defun org-lms-mig--display-results (updates errors warnings)
  "Display migration results in a buffer.
UPDATES is list of property updates made.
ERRORS is list of errors encountered.
WARNINGS is list of warnings."
  (let ((buf (get-buffer-create org-lms-mig-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-lms-mig-results-mode)
        ;; Header
        (insert (propertize "Migration Results\n" 'face 'bold))
        (insert (make-string 50 ?=) "\n\n")
        ;; Summary
        (insert (format "Updates:  %d\n" (length updates)))
        (insert (format "Errors:   %d\n" (length errors)))
        (insert (format "Warnings: %d\n\n" (length warnings)))
        ;; Errors
        (when errors
          (insert (propertize "ERRORS:\n" 'face 'error))
          (dolist (err errors)
            (insert (format "  - %s\n" err)))
          (insert "\n"))
        ;; Warnings
        (when warnings
          (insert (propertize "WARNINGS:\n" 'face 'warning))
          (dolist (warn warnings)
            (let ((start (point)))
              (insert (format "  - %s: %s = %s\n"
                              (plist-get warn :property)
                              (plist-get warn :value)
                              (or (plist-get warn :heading) "unknown")))
              (put-text-property start (point) 'mig-result warn)))
          (insert "\n"))
        ;; Updates
        (when updates
          (insert (propertize "UPDATES:\n" 'face 'success))
          (dolist (update updates)
            (let ((start (point)))
              (insert (format "  %s: %s -> %s\n"
                              (plist-get update :property)
                              (plist-get update :old-value)
                              (plist-get update :new-value)))
              (put-text-property start (point) 'mig-result update))))
        (goto-char (point-min))))
    (display-buffer buf)))

;;;; Interactive Commands

;;;###autoload
(defun org-lms-migrate-prepare (&optional scope)
  "Prepare for migration: create snapshot and validate files.
SCOPE is one of: `buffer', `directory', `project'."
  (interactive)
  (let ((scope (or scope (org-lms-mig--prompt-for-scope))))
    ;; Check for uncommitted git changes
    (when (and (buffer-file-name)
               (vc-backend (buffer-file-name)))
      (let ((default-directory (file-name-directory (buffer-file-name))))
        (when (string-match-p "Changes not staged\\|Changes to be committed"
                              (shell-command-to-string "git status"))
          (unless (y-or-n-p "Uncommitted git changes detected. Continue anyway? ")
            (user-error "Aborted - please commit changes first")))))
    ;; Create snapshot
    (let ((snapshot (org-lms-mig-create-snapshot scope)))
      (setq org-lms-mig--current-state
            (list :status 'prepared
                  :scope scope
                  :snapshot snapshot))
      (message "Migration prepared. Snapshot: %s" (plist-get snapshot :id)))))

;;;###autoload
(defun org-lms-migrate-execute (dest-course-id source-course-id &optional options)
  "Execute Canvas migration from SOURCE-COURSE-ID to DEST-COURSE-ID.
OPTIONS passed to `org-lms-mig-create'."
  (interactive
   (progn
     (org-lms-mig--ensure-org-lms)
     (list (read-string "Destination course ID: ")
           (or (org-lms-get-keyword "ORG_LMS_COURSEID")
               (read-string "Source course ID: ")))))
  ;; Update state
  (setq org-lms-mig--current-state
        (plist-put org-lms-mig--current-state :source-course-id source-course-id))
  (setq org-lms-mig--current-state
        (plist-put org-lms-mig--current-state :dest-course-id dest-course-id))
  (setq org-lms-mig--current-state
        (plist-put org-lms-mig--current-state :status 'in-progress))
  ;; Start migration
  (message "Starting Canvas migration from %s to %s..." source-course-id dest-course-id)
  (let ((response (org-lms-mig-create dest-course-id source-course-id options)))
    (let ((migration-id (plist-get response :id)))
      (setq org-lms-mig--current-state
            (plist-put org-lms-mig--current-state :canvas-migration-id migration-id))
      (message "Migration created with ID %s. Polling for completion..." migration-id)
      ;; Poll for completion
      (let ((final-status (org-lms-mig-poll-until-complete dest-course-id migration-id)))
        (setq org-lms-mig--current-state
              (plist-put org-lms-mig--current-state :canvas-status
                         (plist-get final-status :workflow_state)))
        (message "Migration completed. Fetching asset mapping...")
        ;; Get asset mapping
        (let ((mapping (org-lms-mig-get-asset-mapping dest-course-id migration-id)))
          (setq org-lms-mig--current-state
                (plist-put org-lms-mig--current-state :asset-mapping mapping))
          (message "Asset mapping retrieved. Ready to update org files."))))))

;;;###autoload
(defun org-lms-migrate-update-ids (&optional scope)
  "Apply asset mapping to org files.
SCOPE is one of: `buffer', `directory', `project'."
  (interactive)
  (unless (plist-get org-lms-mig--current-state :asset-mapping)
    (user-error "No asset mapping available. Run migration first"))
  (let* ((scope (or scope
                    (plist-get org-lms-mig--current-state :scope)
                    (org-lms-mig--prompt-for-scope)))
         (mapping (org-lms-mig--parse-asset-mapping
                   (plist-get org-lms-mig--current-state :asset-mapping)))
         (old-id (plist-get org-lms-mig--current-state :source-course-id))
         (new-id (plist-get org-lms-mig--current-state :dest-course-id)))
    ;; Update properties
    (message "Updating org properties...")
    (let ((updates (org-lms-mig-update-org-properties mapping old-id new-id scope)))
      (setq org-lms-mig--current-state
            (plist-put org-lms-mig--current-state :updates updates))
      ;; Update links
      (message "Updating Canvas links...")
      (let ((link-count (org-lms-mig-update-links old-id new-id scope)))
        (message "Updated %d properties and %d links" (length updates) link-count)
        ;; Verify and display results
        (let ((verification (org-lms-mig-verify mapping scope)))
          (setq org-lms-mig--current-state
                (plist-put org-lms-mig--current-state :status
                           (if (plist-get verification :success) 'completed 'completed-with-warnings)))
          (org-lms-mig--display-results
           updates
           (plist-get org-lms-mig--current-state :errors)
           (plist-get verification :unmapped)))))))

;;;###autoload
(defun org-lms-migrate-verify ()
  "Verify migration success and report issues."
  (interactive)
  (unless (plist-get org-lms-mig--current-state :asset-mapping)
    (user-error "No asset mapping available"))
  (let* ((scope (or (plist-get org-lms-mig--current-state :scope) 'buffer))
         (mapping (org-lms-mig--parse-asset-mapping
                   (plist-get org-lms-mig--current-state :asset-mapping)))
         (result (org-lms-mig-verify mapping scope)))
    (if (plist-get result :success)
        (message "Verification passed: %d items verified"
                 (plist-get result :verified-count))
      (message "Verification found issues: %s"
               (plist-get result :warnings)))
    result))

;;;###autoload
(defun org-lms-migrate-rollback ()
  "Rollback to pre-migration state."
  (interactive)
  (if-let ((snapshot (plist-get org-lms-mig--current-state :snapshot)))
      (when (y-or-n-p "Restore org files from snapshot? ")
        (org-lms-mig-restore-snapshot snapshot)
        (setq org-lms-mig--current-state
              (plist-put org-lms-mig--current-state :status 'rolled-back))
        (message "Rollback complete"))
    (user-error "No snapshot available for rollback")))

;;;###autoload
(defun org-lms-migrate-show-status ()
  "Display current migration status."
  (interactive)
  (if org-lms-mig--current-state
      (message "Migration status: %s\n  Source: %s\n  Dest: %s\n  Canvas ID: %s"
               (plist-get org-lms-mig--current-state :status)
               (plist-get org-lms-mig--current-state :source-course-id)
               (plist-get org-lms-mig--current-state :dest-course-id)
               (plist-get org-lms-mig--current-state :canvas-migration-id))
    (message "No migration in progress")))

;;;###autoload
(defun org-lms-migrate-course-wizard ()
  "Interactive wizard for course migration.
Guides through the complete migration process."
  (interactive)
  (org-lms-mig--ensure-org-lms)
  ;; Step 1: Get source course ID
  (let* ((source-id (or (org-lms-get-keyword "ORG_LMS_COURSEID")
                        (read-string "Source course ID: ")))
         ;; Step 2: Get destination course ID
         (dest-id (read-string "Destination course ID: "))
         ;; Step 3: Scope
         (scope (org-lms-mig--prompt-for-scope))
         ;; Step 4: Date shift options
         (shift-dates (y-or-n-p "Shift dates to new semester? "))
         (options nil))
    (when shift-dates
      (setq options
            (list :shift-dates t
                  :old-start-date (read-string "Old course start date (YYYY-MM-DD): ")
                  :new-start-date (read-string "New course start date (YYYY-MM-DD): ")
                  :old-end-date (read-string "Old course end date (YYYY-MM-DD): ")
                  :new-end-date (read-string "New course end date (YYYY-MM-DD): "))))
    ;; Confirm
    (unless (y-or-n-p (format "Migrate course %s -> %s? " source-id dest-id))
      (user-error "Migration cancelled"))
    ;; Execute workflow
    (org-lms-migrate-prepare scope)
    (org-lms-migrate-execute dest-id source-id options)
    (org-lms-migrate-update-ids scope)
    (message "Migration wizard complete!")))

(provide 'org-lms-migration)
;;; org-lms-migration.el ends here
