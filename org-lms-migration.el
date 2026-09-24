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
  (let ((params `((migration_type . "course_copy_importer")
                  (settings . ((source_course_id . ,source-course-id))))))
    ;; Add date shift options if specified
    (when (plist-get options :shift-dates)
      (let ((date-opts `((shift_dates . t))))
        (when-let ((old-start (plist-get options :old-start-date)))
          (push `(old_start_date . ,old-start) date-opts))
        (when-let ((new-start (plist-get options :new-start-date)))
          (push `(new_start_date . ,new-start) date-opts))
        (when-let ((old-end (plist-get options :old-end-date)))
          (push `(old_end_date . ,old-end) date-opts))
        (when-let ((new-end (plist-get options :new-end-date)))
          (push `(new_end_date . ,new-end) date-opts))
        (push `(date_shift_options . ,date-opts) params)))
    ;; Selective import
    (when (plist-get options :selective)
      (push '(selective_import . t) params))
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

;;;; Async Polling

(defvar org-lms-mig--poll-timer nil
  "Active timer for async migration polling.")

(defvar org-lms-mig--poll-state nil
  "State plist for the active async poll.
Keys: :course-id :migration-id :start-time :timeout
      :on-complete :on-error :on-progress")

(defun org-lms-mig--poll-tick ()
  "Timer callback that checks migration status once.
Reschedules itself or calls completion/error callbacks."
  (condition-case err
      (let* ((course-id (plist-get org-lms-mig--poll-state :course-id))
             (migration-id (plist-get org-lms-mig--poll-state :migration-id))
             (timeout (plist-get org-lms-mig--poll-state :timeout))
             (start-time (plist-get org-lms-mig--poll-state :start-time))
             (on-complete (plist-get org-lms-mig--poll-state :on-complete))
             (on-error (plist-get org-lms-mig--poll-state :on-error))
             (on-progress (plist-get org-lms-mig--poll-state :on-progress))
             (status (org-lms-mig-get-status course-id migration-id))
             (workflow-state (plist-get status :workflow_state)))
        (message "Migration status: %s (progress: %s%%)"
                 workflow-state
                 (or (plist-get status :completion) 0))
        (when on-progress
          (funcall on-progress status))
        (cond
         ((member workflow-state '("imported" "completed"))
          (org-lms-mig--poll-cancel)
          (when on-complete
            (funcall on-complete status)))
         ((member workflow-state '("failed" "pre_process_error"))
          (org-lms-mig--poll-cancel)
          (let ((msg (format "Migration failed: %s"
                             (plist-get status :migration_issues))))
            (if on-error
                (funcall on-error msg)
              (message "%s" msg))))
         ((>= (- (float-time) start-time) timeout)
          (org-lms-mig--poll-cancel)
          (let ((msg (format "Migration timed out after %d seconds" timeout)))
            (if on-error
                (funcall on-error msg)
              (message "%s" msg))))))
    (error
     (org-lms-mig--poll-cancel)
     (let ((on-error (plist-get org-lms-mig--poll-state :on-error)))
       (if on-error
           (funcall on-error (error-message-string err))
         (message "Migration poll error: %s" (error-message-string err)))))))

(defun org-lms-mig-poll-until-complete-async
    (course-id migration-id &optional timeout on-complete on-error on-progress)
  "Poll migration status asynchronously using a timer.
COURSE-ID and MIGRATION-ID identify the Canvas migration.
TIMEOUT defaults to `org-lms-mig-poll-timeout'.
ON-COMPLETE is called with the final status plist on success.
ON-ERROR is called with an error message string on failure/timeout.
ON-PROGRESS is called with the status plist on each poll tick."
  (when org-lms-mig--poll-timer
    (org-lms-mig--poll-cancel))
  (setq org-lms-mig--poll-state
        (list :course-id course-id
              :migration-id migration-id
              :start-time (float-time)
              :timeout (or timeout org-lms-mig-poll-timeout)
              :on-complete on-complete
              :on-error on-error
              :on-progress on-progress))
  (setq org-lms-mig--poll-timer
        (run-at-time 0 org-lms-mig-poll-interval #'org-lms-mig--poll-tick))
  (message "Async migration polling started (every %ds, timeout %ds)"
           org-lms-mig-poll-interval
           (or timeout org-lms-mig-poll-timeout)))

(defun org-lms-mig--poll-cancel ()
  "Cancel the active async poll timer."
  (when org-lms-mig--poll-timer
    (cancel-timer org-lms-mig--poll-timer)
    (setq org-lms-mig--poll-timer nil)))

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
              (org-lms-set-keyword (car kw) (cdr kw))
              (cl-incf restored))
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
RAW-MAPPING is a plist as returned by the Canvas API (via `ol-jsonwrapper'),
e.g. (:assignments (:12345 \"67890\" ...) :quizzes (:111 \"222\" ...)).
Returns a plist mapping canvas-key to hash-table of old-id -> new-id,
where both keys and values are strings."
  (let ((result nil))
    (cl-loop for (key val) on raw-mapping by #'cddr
             do (let ((ht (make-hash-table :test 'equal))
                      (key-str (replace-regexp-in-string
                                "^:" "" (symbol-name key))))
                  ;; val is a plist of (:old-id "new-id" ...) from JSON
                  (cl-loop for (old-key new-val) on val by #'cddr
                           do (puthash (replace-regexp-in-string
                                        "^:" "" (symbol-name old-key))
                                       new-val ht))
                  (setq result (plist-put result (intern key-str) ht))))
    result))

(defun org-lms-mig--lookup-new-id (mapping canvas-key old-id)
  "Look up the new ID for OLD-ID of type CANVAS-KEY in MAPPING."
  (when-let ((ht (plist-get mapping (intern canvas-key))))
    (gethash (if (numberp old-id) (number-to-string old-id) old-id) ht)))

(defun org-lms-mig--detect-org-course-id (&optional scope)
  "Detect the course ID actually referenced in org file URLs.
Looks for /courses/NNNNN/ patterns in CANVAS_HTML_URL properties.
Returns the most common course ID found, or nil."
  (let ((scope (or scope 'buffer))
        (id-counts (make-hash-table :test 'equal)))
    (dolist (file (org-lms-mig--get-scope-files scope))
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (dolist (prop '("CANVAS_HTML_URL" "QUIZ_HTML_URL"
                             "SUBMISSIONS_DOWNLOAD_URL" "CANVAS_SUBMISSION_URL"))
               (when-let ((url (org-entry-get nil prop)))
                 (when (string-match "/courses/\\([0-9]+\\)/" url)
                   (let ((id (match-string 1 url)))
                     (puthash id (1+ (gethash id id-counts 0)) id-counts))))))))))
    ;; Return the most common course ID
    (let ((best-id nil) (best-count 0))
      (maphash (lambda (id count)
                 (when (> count best-count)
                   (setq best-id id best-count count)))
               id-counts)
      best-id)))

(defun org-lms-mig--find-course-copy-mapping (course-id)
  "Find the asset mapping for the most recent completed course copy on COURSE-ID.
Returns the raw asset mapping plist, or nil if none found."
  (let* ((migrations (org-lms-canvas-request
                      (format "courses/%s/content_migrations" course-id)
                      "GET"))
         (completed (seq-filter
                     (lambda (m)
                       (and (member (plist-get m :workflow_state)
                                    '("imported" "completed"))
                            (equal (plist-get m :migration_type)
                                   "course_copy_importer")))
                     migrations)))
    (when completed
      (setq completed (sort completed
                           (lambda (a b)
                             (> (plist-get a :id) (plist-get b :id)))))
      (let ((migration-id (plist-get (car completed) :id)))
        (message "Found intermediate migration %s on course %s" migration-id course-id)
        (org-lms-mig-get-asset-mapping course-id migration-id)))))

(defun org-lms-mig--compose-raw-mappings (mapping-a mapping-b)
  "Compose two raw Canvas asset mappings: A then B.
MAPPING-A maps old->intermediate IDs, MAPPING-B maps intermediate->new IDs.
Returns a raw mapping plist that maps old->new IDs directly.
Both mappings are in the raw Canvas format (plist of keyword plists)."
  (let ((parsed-a (org-lms-mig--parse-asset-mapping mapping-a))
        (parsed-b (org-lms-mig--parse-asset-mapping mapping-b))
        (result nil))
    ;; For each content type in mapping A
    (cl-loop for (type-sym ht-a) on parsed-a by #'cddr
             do (let* ((type-key (symbol-name type-sym))
                       (ht-b (plist-get parsed-b type-sym))
                       (composed-entries nil))
                  (when ht-a
                    (maphash
                     (lambda (old-id intermediate-id)
                       (let ((new-id (when ht-b
                                       (gethash (if (numberp intermediate-id)
                                                    (number-to-string intermediate-id)
                                                  intermediate-id)
                                                ht-b))))
                         (push (cons (intern (concat ":" old-id))
                                     (or new-id intermediate-id))
                               composed-entries)))
                     ht-a))
                  (when composed-entries
                    ;; Convert back to raw plist format (:key1 val1 :key2 val2 ...)
                    (let ((raw-entries nil))
                      (dolist (entry composed-entries)
                        (push (cdr entry) raw-entries)
                        (push (car entry) raw-entries))
                      (setq result (plist-put result
                                             (intern (concat ":" type-key))
                                             raw-entries))))))
    result))

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

(defun org-lms-mig-update-heading-properties (mapping old-course-id new-course-id
                                                     &optional url-course-ids)
  "Update Canvas IDs for the heading at point using MAPPING.
OLD-COURSE-ID and NEW-COURSE-ID are used for URL updates.
URL-COURSE-IDS is an optional list of additional old course IDs that
may appear in URLs (from prior unmigrated copies).  All will be
replaced with NEW-COURSE-ID.
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
                   ;; Get the NEW primary ID (already updated above)
                   (new-obj-id (org-entry-get nil primary-prop))
                   ;; Extract the object ID currently in the URL
                   (url-obj-id (org-lms-mig--extract-url-object-id url))
                   ;; Build the new URL: replace all old course IDs and the object ID
                   (new-url url))
              ;; Replace all known old course IDs in the URL
              (dolist (old-cid (cons old-course-id (or url-course-ids '())))
                (when old-cid
                  (setq new-url (replace-regexp-in-string
                                 (format "/courses/%s/" old-cid)
                                 (format "/courses/%s/" new-course-id)
                                 new-url))))
              ;; Replace the object ID in the URL with the new primary ID
              (when (and url-obj-id new-obj-id
                         (not (string= url-obj-id new-obj-id)))
                (setq new-url (replace-regexp-in-string
                               (format "/%s\\([^0-9]\\|$\\)" (regexp-quote url-obj-id))
                               (format "/%s\\1" new-obj-id)
                               new-url)))
              (unless (string= url new-url)
                (org-set-property prop new-url)
                (push (list :property prop
                            :old-value url
                            :new-value new-url
                            :type 'derived)
                      updates)))))))
    (nreverse updates)))

(defun org-lms-mig--extract-url-object-id (url)
  "Extract the trailing object ID from a Canvas URL.
E.g. from \"https://host/courses/123/assignments/456\" returns \"456\".
From \"https://host/courses/123/quizzes/789?foo=1\" returns \"789\"."
  (when (string-match "/courses/[0-9]+/[^/]+/\\([0-9]+\\)" url)
    (match-string 1 url)))

(defun org-lms-mig-update-org-properties (mapping old-course-id new-course-id
                                                  &optional scope url-course-ids)
  "Update all org properties using MAPPING.
OLD-COURSE-ID and NEW-COURSE-ID for URL regeneration.
SCOPE is one of: `buffer', `directory', `project'.
URL-COURSE-IDS is an optional list of additional old course IDs
that may appear in URLs (from prior unmigrated copies).
Returns list of all updates made."
  (let* ((scope (or scope 'buffer))
         (files (org-lms-mig--get-scope-files scope))
         (all-updates nil))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (let ((file-updates nil))
            ;; Update ORG_LMS_COURSEID keyword — match old-course-id or any url-course-id
            (save-excursion
              (goto-char (point-min))
              (let ((ids-to-replace (cons old-course-id (or url-course-ids '()))))
                (dolist (old-id ids-to-replace)
                  (when old-id
                    (goto-char (point-min))
                    (when (re-search-forward
                           (format "^#\\+ORG_LMS_COURSEID:\\s-*%s\\s-*$" old-id)
                           nil t)
                      (replace-match (format "#+ORG_LMS_COURSEID: %s" new-course-id))
                      (push (list :file file
                                  :type 'keyword
                                  :property "ORG_LMS_COURSEID"
                                  :old-value old-id
                                  :new-value new-course-id)
                            file-updates))))))
            ;; Update heading properties
            (org-map-entries
             (lambda ()
               (let ((updates (org-lms-mig-update-heading-properties
                               mapping old-course-id new-course-id url-course-ids)))
                 (when updates
                   (dolist (update updates)
                     (push (plist-put update :file file) file-updates))))))
            (when file-updates
              (save-buffer)
              (setq all-updates (append all-updates file-updates)))))))
    all-updates))

;;;; Link Update Functions

(defun org-lms-mig-update-link-params (mapping &optional scope)
  "Update file IDs in link query parameters using MAPPING.
Finds patterns like ?preview=NNNNN or &verifier=...&preview=NNNNN
in Canvas URLs and replaces the file ID using the files asset mapping.
SCOPE is one of: `buffer', `directory', `project'.
Returns count of parameters updated."
  (let* ((scope (or scope 'buffer))
         (files (org-lms-mig--get-scope-files scope))
         (file-ht (plist-get mapping (intern "files")))
         (count 0)
         ;; Match ?preview=DIGITS or &preview=DIGITS inside org links
         (pattern "\\([?&]preview=\\)\\([0-9]+\\)"))
    (unless file-ht
      (message "WARNING: No files mapping found in asset mapping")
      (cl-return-from org-lms-mig-update-link-params 0))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward pattern nil t)
              (let* ((old-id (match-string 2))
                     (new-id (gethash old-id file-ht)))
                (when new-id
                  (replace-match (concat (match-string 1) new-id))
                  (cl-incf count)))))
          (when (buffer-modified-p)
            (save-buffer)))))
    count))

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
    (message "Migration create response: %S" response)
    (let ((migration-id (plist-get response :id)))
      (unless migration-id
        (user-error "Failed to create migration: no ID in response. Response: %S" response))
      (setq org-lms-mig--current-state
            (plist-put org-lms-mig--current-state :canvas-migration-id migration-id))
      (message "Migration created with ID %s. Polling for completion..." migration-id)
      ;; Poll for completion asynchronously so Emacs stays responsive
      (org-lms-mig-poll-until-complete-async
       dest-course-id migration-id nil
       ;; on-complete
       (lambda (final-status)
         (setq org-lms-mig--current-state
               (plist-put org-lms-mig--current-state :canvas-status
                          (plist-get final-status :workflow_state)))
         (message "Migration completed. Fetching asset mapping...")
         (let ((mapping (org-lms-mig-get-asset-mapping dest-course-id migration-id)))
           (setq org-lms-mig--current-state
                 (plist-put org-lms-mig--current-state :asset-mapping mapping))
           (message "Asset mapping retrieved.")
           ;; Auto-run update-ids if launched from wizard
           (when-let ((scope (plist-get org-lms-mig--current-state :wizard-scope)))
             (message "Automatically updating org IDs...")
             (org-lms-migrate-update-ids scope)
             (message "Migration wizard complete!"))))
       ;; on-error
       (lambda (err-msg)
         (setq org-lms-mig--current-state
               (plist-put org-lms-mig--current-state :status 'failed))
         (user-error "%s" err-msg))))))

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
         (new-id (plist-get org-lms-mig--current-state :dest-course-id))
         (url-course-ids (plist-get org-lms-mig--current-state :url-course-ids)))
    ;; Update properties
    (message "Updating org properties...")
    (let ((updates (org-lms-mig-update-org-properties
                    mapping old-id new-id scope url-course-ids)))
      (setq org-lms-mig--current-state
            (plist-put org-lms-mig--current-state :updates updates))
      ;; Update links — replace all old course IDs
      (message "Updating Canvas links...")
      (let ((link-count 0))
        (dolist (cid (cons old-id (or url-course-ids '())))
          (when cid
            (cl-incf link-count (org-lms-mig-update-links cid new-id scope))))
        ;; Update file IDs in link query parameters (e.g. ?preview=NNNNN)
        (message "Updating link parameters (preview IDs, etc.)...")
        (let ((param-count (org-lms-mig-update-link-params mapping scope)))
          (message "Updated %d properties, %d links, and %d link params"
                   (length updates) link-count param-count))
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
(defun org-lms-migrate-check-stale-dates (&optional scope)
  "Check for quiz/assignment dates that are in the past or before course start.
Reports any DUE_AT, UNLOCK_AT, or LOCK_AT properties with years
that don't match the current ORG_LMS_COURSEID course.
SCOPE is one of: `buffer', `directory', `project'."
  (interactive)
  (org-lms-mig--ensure-org-lms)
  (let* ((scope (or scope (org-lms-mig--prompt-for-scope)))
         (files (org-lms-mig--get-scope-files scope))
         (current-year (format-time-string "%Y"))
         (stale nil))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (dolist (prop '("DUE_AT" "UNLOCK_AT" "LOCK_AT"))
               (when-let ((val (org-entry-get nil prop)))
                 ;; Extract year from the date value
                 (when (string-match "\\`\\([0-9]\\{4\\}\\)" val)
                   (let ((year (match-string 1 val)))
                     (when (string< year current-year)
                       (push (list :file (file-name-nondirectory file)
                                   :heading (org-get-heading t t t t)
                                   :property prop
                                   :value val
                                   :year year)
                             stale)))))))))))
    (if (not stale)
        (message "No stale dates found.")
      (message "Found %d stale date(s):" (length stale))
      (dolist (s (nreverse stale))
        (message "  %s: %s %s = %s"
                 (plist-get s :file)
                 (plist-get s :heading)
                 (plist-get s :property)
                 (plist-get s :value))))
    stale))

;;;###autoload
(defun org-lms-migrate-fix-preview-ids (&optional scope)
  "Fix stale file IDs in ?preview= link parameters.
Uses the asset mapping from the current migration state.
If a chained mapping is available (for files from an older course),
it will be used automatically.
SCOPE is one of: `buffer', `directory', `project'."
  (interactive)
  (unless (plist-get org-lms-mig--current-state :asset-mapping)
    (user-error "No asset mapping available. Run migration first"))
  (let* ((scope (or scope (org-lms-mig--prompt-for-scope)))
         (mapping (org-lms-mig--parse-asset-mapping
                   (plist-get org-lms-mig--current-state :asset-mapping)))
         (count (org-lms-mig-update-link-params mapping scope)))
    (if (> count 0)
        (message "Updated %d preview IDs" count)
      ;; Direct mapping had no matches — try chaining through intermediate
      (message "Direct mapping found no matches. Trying chained mapping...")
      (let* ((source-id (plist-get org-lms-mig--current-state :source-course-id))
             (dest-id (plist-get org-lms-mig--current-state :dest-course-id))
             (intermediate-raw (org-lms-mig--find-course-copy-mapping source-id)))
        (if (not intermediate-raw)
            (message "No intermediate mapping found on course %s. Cannot fix preview IDs." source-id)
          (let* ((dest-raw (plist-get org-lms-mig--current-state :asset-mapping))
                 (chained-raw (org-lms-mig--compose-raw-mappings intermediate-raw dest-raw))
                 (chained (org-lms-mig--parse-asset-mapping chained-raw))
                 (chained-count (org-lms-mig-update-link-params chained scope)))
            (message "Updated %d preview IDs using chained mapping" chained-count)))))))


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
    ;; Store scope so the async callback can use it
    (setq org-lms-mig--current-state
          (plist-put org-lms-mig--current-state :wizard-scope scope))
    (org-lms-migrate-execute dest-id source-id options)
    (message "Migration started. update-ids will run automatically when Canvas finishes.")))

;;;###autoload
(defun org-lms-migrate-from-ui (dest-course-id source-course-id &optional scope)
  "Complete migration after a course copy done via the Canvas web UI.
Finds the latest completed migration on DEST-COURSE-ID, fetches
the asset mapping, and updates org files.
SOURCE-COURSE-ID is needed to update course ID keywords and links.
SCOPE is one of: `buffer', `directory', `project'.

If org files reference a course ID different from SOURCE-COURSE-ID
\(e.g. from an earlier migration that never updated local files),
this function will detect that and attempt to chain through
intermediate asset mappings to translate the old IDs to the
destination course."
  (interactive
   (progn
     (org-lms-mig--ensure-org-lms)
     (list (read-string "Destination course ID: ")
           (or (org-lms-get-keyword "ORG_LMS_COURSEID")
               (read-string "Source course ID: "))
           (org-lms-mig--prompt-for-scope))))
  ;; Detect the actual course ID referenced in org files
  (let ((org-course-id (org-lms-mig--detect-org-course-id scope)))
    (when (and org-course-id
               (not (string= org-course-id source-course-id))
               (not (string= org-course-id dest-course-id)))
      (message "NOTE: Org files reference course %s, not source %s. Will chain mappings."
               org-course-id source-course-id)))
  (message "Fetching content migrations for course %s..." dest-course-id)
  (let* ((migrations (org-lms-canvas-request
                      (format "courses/%s/content_migrations" dest-course-id)
                      "GET"))
         ;; Find the latest completed course_copy migration
         (completed (seq-filter
                     (lambda (m)
                       (and (member (plist-get m :workflow_state)
                                    '("imported" "completed"))
                            (equal (plist-get m :migration_type)
                                   "course_copy_importer")))
                     migrations)))
    (unless completed
      (user-error "No completed course copy migrations found on course %s" dest-course-id))
    ;; Sort by id descending to get the latest
    (setq completed (sort completed
                         (lambda (a b)
                           (> (plist-get a :id) (plist-get b :id)))))
    (let* ((migration (car completed))
           (migration-id (plist-get migration :id)))
      (message "Found migration %s (state: %s). Fetching asset mapping..."
               migration-id (plist-get migration :workflow_state))
      ;; Prepare snapshot first
      (org-lms-migrate-prepare scope)
      ;; Fetch the direct mapping (source -> dest)
      (let* ((mapping-raw (org-lms-mig-get-asset-mapping dest-course-id migration-id))
             (org-course-id (org-lms-mig--detect-org-course-id scope))
             (url-course-ids nil))
        (unless mapping-raw
          (user-error "No asset mapping returned for migration %s" migration-id))
        ;; If org URLs reference a different (older) course ID, we need to
        ;; replace that in URLs too.  Primary IDs (CANVASID etc.) are from
        ;; SOURCE-COURSE-ID and matched by the direct mapping; only URLs
        ;; are stale from the older course.
        (when (and org-course-id
                   (not (string= org-course-id source-course-id))
                   (not (string= org-course-id dest-course-id)))
          (message "NOTE: URLs reference course %s (not %s). Will replace both in URLs."
                   org-course-id source-course-id)
          (setq url-course-ids (list org-course-id)))
        ;; Set up state so update-ids can work
        ;; source-course-id stays as-is — primary IDs match this course
        (setq org-lms-mig--current-state
              (plist-put org-lms-mig--current-state :source-course-id source-course-id))
        (setq org-lms-mig--current-state
              (plist-put org-lms-mig--current-state :dest-course-id dest-course-id))
        (setq org-lms-mig--current-state
              (plist-put org-lms-mig--current-state :canvas-migration-id migration-id))
        (setq org-lms-mig--current-state
              (plist-put org-lms-mig--current-state :asset-mapping mapping-raw))
        (setq org-lms-mig--current-state
              (plist-put org-lms-mig--current-state :url-course-ids url-course-ids))
        (message "Asset mapping retrieved. Updating org files...")
        ;; Run the ID update
        (org-lms-migrate-update-ids scope)
        (message "Migration from UI complete!")))))

(provide 'org-lms-migration)
;;; org-lms-migration.el ends here
