;;; org-lms-migration-tests.el --- ERT tests for course migration module -*- lexical-binding: t -*-

;;; Commentary:
;; Unit tests for org-lms-migration.el.
;; Tests tagged :live-api require Canvas API access and are skipped in batch mode.
;;
;; Usage:
;;   M-x ert RET org-lms-mig-test- RET
;; or from shell:
;;   emacs --batch -l test/org-lms-migration-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'org)

;;; Setup

;; Add elpaca builds to load-path for batch mode
(let ((elpaca-builds (expand-file-name "~/.emacs.d/elpaca/builds/")))
  (when (file-directory-p elpaca-builds)
    (dolist (pkg-dir (directory-files elpaca-builds t "^[^.]"))
      (when (file-directory-p pkg-dir)
        (add-to-list 'load-path pkg-dir)))))

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir))
  (unless (fboundp 'org-lms-set-keyword)
    (load (expand-file-name "../org-lms.el" dir) nil t))
  (unless (fboundp 'org-lms-mig-create)
    (load (expand-file-name "../org-lms-migration.el" dir) nil t)))

;;; Helpers

(defmacro org-lms-mig-test-with-org-buffer (content &rest body)
  "Execute BODY in a temp org buffer with CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defmacro org-lms-mig-test-with-org-file (content &rest body)
  "Execute BODY with a temp org file containing CONTENT.
Binds `test-file' to the file path."
  (declare (indent 1))
  `(let ((test-file (make-temp-file "mig-test-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file test-file
             (insert ,content))
           ,@body)
       (when (file-exists-p test-file)
         (delete-file test-file)))))

;;; Test: Property Registry

(ert-deftest org-lms-mig-test-property-registry-structure ()
  "All registry entries should have :canvas-key and :properties."
  (dolist (entry org-lms-mig--property-registry)
    (let ((type (car entry))
          (config (cdr entry)))
      (should (symbolp type))
      (should (stringp (plist-get config :canvas-key)))
      (should (listp (plist-get config :properties)))
      (should (> (length (plist-get config :properties)) 0)))))

;;; Test: Collect Properties

(ert-deftest org-lms-mig-test-collect-properties ()
  "Should collect file keywords and heading properties from org file."
  (org-lms-mig-test-with-org-file
      (concat "#+ORG_LMS_COURSEID: 12345\n"
              "#+COURSE_START_DATE: 2025-01-06\n"
              "* Assignment One\n"
              ":PROPERTIES:\n"
              ":CANVASID: 99001\n"
              ":CANVAS_HTML_URL: https://q.utoronto.ca/courses/12345/assignments/99001\n"
              ":END:\n"
              "* Quiz One\n"
              ":PROPERTIES:\n"
              ":QUIZ_ID: 88001\n"
              ":END:\n")
    (let ((result (org-lms-mig--collect-file-properties test-file)))
      ;; Keywords
      (should (assoc "ORG_LMS_COURSEID" (plist-get result :keywords)))
      (should (string= "12345"
                        (cdr (assoc "ORG_LMS_COURSEID" (plist-get result :keywords)))))
      (should (assoc "COURSE_START_DATE" (plist-get result :keywords)))
      ;; Headings
      (should (>= (length (plist-get result :headings)) 2))
      ;; First heading has CANVASID
      (let* ((h1 (car (plist-get result :headings)))
             (props (plist-get h1 :properties)))
        (should (assoc "CANVASID" props))
        (should (string= "99001" (cdr (assoc "CANVASID" props))))))))

;;; Test: Snapshot Roundtrip

(ert-deftest org-lms-mig-test-snapshot-roundtrip ()
  "Save and load a snapshot; data should survive the roundtrip."
  (let ((org-lms-mig-snapshot-directory (make-temp-file "mig-snap-" t)))
    (unwind-protect
        (let* ((snapshot (list :id "test-snap-001"
                               :timestamp (current-time)
                               :source-course-id "12345"
                               :files (list (list :file "/tmp/fake.org"
                                                  :keywords '(("ORG_LMS_COURSEID" . "12345"))
                                                  :headings nil))))
               (saved-file (org-lms-mig--save-snapshot snapshot))
               (loaded (org-lms-mig--load-snapshot "test-snap-001")))
          (should (file-exists-p saved-file))
          (should loaded)
          (should (string= "test-snap-001" (plist-get loaded :id)))
          (should (string= "12345" (plist-get loaded :source-course-id)))
          (should (= 1 (length (plist-get loaded :files)))))
      (delete-directory org-lms-mig-snapshot-directory t))))

;;; Test: Restore Heading Properties

(ert-deftest org-lms-mig-test-restore-heading-properties ()
  "Restoring from snapshot should revert heading properties."
  (let ((org-lms-mig-snapshot-directory (make-temp-file "mig-snap-" t)))
    (unwind-protect
        (org-lms-mig-test-with-org-file
            (concat "#+ORG_LMS_COURSEID: 12345\n"
                    "* Assignment One\n"
                    ":PROPERTIES:\n"
                    ":CANVASID: 99001\n"
                    ":END:\n")
          ;; Collect original properties
          (let* ((original (org-lms-mig--collect-file-properties test-file))
                 (snapshot (list :id "restore-test"
                                 :timestamp (current-time)
                                 :source-course-id "12345"
                                 :files (list original))))
            (org-lms-mig--save-snapshot snapshot)
            ;; Modify the file — change CANVASID
            (with-current-buffer (find-file-noselect test-file)
              (goto-char (point-min))
              (org-next-visible-heading 1)
              (org-set-property "CANVASID" "CHANGED")
              (save-buffer)
              ;; Verify modification
              (should (string= "CHANGED" (org-entry-get nil "CANVASID"))))
            ;; Restore
            (org-lms-mig-restore-snapshot snapshot)
            ;; Verify restoration
            (with-current-buffer (find-file-noselect test-file)
              (revert-buffer t t)
              (goto-char (point-min))
              (org-next-visible-heading 1)
              (should (string= "99001" (org-entry-get nil "CANVASID"))))))
      (delete-directory org-lms-mig-snapshot-directory t))))

;;; Test: Restore Keywords

(ert-deftest org-lms-mig-test-restore-keywords ()
  "Restoring from snapshot should revert file-level keywords."
  (let ((org-lms-mig-snapshot-directory (make-temp-file "mig-snap-" t)))
    (unwind-protect
        (org-lms-mig-test-with-org-file
            (concat "#+ORG_LMS_COURSEID: 12345\n"
                    "* Heading\n")
          (let* ((original (org-lms-mig--collect-file-properties test-file))
                 (snapshot (list :id "kw-restore-test"
                                 :timestamp (current-time)
                                 :source-course-id "12345"
                                 :files (list original))))
            (org-lms-mig--save-snapshot snapshot)
            ;; Modify the keyword
            (with-current-buffer (find-file-noselect test-file)
              (org-lms-set-keyword "ORG_LMS_COURSEID" "99999")
              (save-buffer)
              (should (string= "99999" (org-lms-get-keyword "ORG_LMS_COURSEID"))))
            ;; Restore
            (org-lms-mig-restore-snapshot snapshot)
            ;; Verify keyword restored
            (with-current-buffer (find-file-noselect test-file)
              (revert-buffer t t)
              (should (string= "12345" (org-lms-get-keyword "ORG_LMS_COURSEID"))))))
      (delete-directory org-lms-mig-snapshot-directory t))))

;;; Test: Parse Asset Mapping

(ert-deftest org-lms-mig-test-parse-asset-mapping ()
  "Should parse Canvas asset mapping plist into hash tables."
  ;; Canvas API returns JSON objects as plists via ol-jsonwrapper
  ;; json-read with json-key-type 'keyword produces :1001 style keys
  (let* ((raw '(:assignments (:1001 "2001" :1002 "2002")
                :quizzes (:3001 "4001")))
         (parsed (org-lms-mig--parse-asset-mapping raw)))
    ;; Should have assignment and quiz tables
    (should (hash-table-p (plist-get parsed 'assignments)))
    (should (hash-table-p (plist-get parsed 'quizzes)))
    ;; Lookup values — keys are stored as strings without leading colon
    (should (string= "2001" (gethash "1001" (plist-get parsed 'assignments))))
    (should (string= "2002" (gethash "1002" (plist-get parsed 'assignments))))
    (should (string= "4001" (gethash "3001" (plist-get parsed 'quizzes))))))

;;; Test: Lookup New ID

(ert-deftest org-lms-mig-test-lookup-new-id ()
  "Should look up new IDs and handle both string and numeric inputs."
  (let* ((raw '(:assignments (:1001 "2001")))
         (mapping (org-lms-mig--parse-asset-mapping raw)))
    (should (string= "2001" (org-lms-mig--lookup-new-id mapping "assignments" "1001")))
    (should (string= "2001" (org-lms-mig--lookup-new-id mapping "assignments" 1001)))
    (should-not (org-lms-mig--lookup-new-id mapping "assignments" "9999"))
    (should-not (org-lms-mig--lookup-new-id mapping "nonexistent" "1001"))))

;;; Test: Update Derived URL

(ert-deftest org-lms-mig-test-update-derived-url ()
  "Should replace course ID and object ID in Canvas URLs."
  ;; Replace both course and object IDs
  (should (string= "https://q.utoronto.ca/courses/67890/assignments/2001"
                    (org-lms-mig--update-derived-url
                     "https://q.utoronto.ca/courses/12345/assignments/1001"
                     "12345" "67890" "1001" "2001")))
  ;; Replace only course ID
  (should (string= "https://q.utoronto.ca/courses/67890/assignments/1001"
                    (org-lms-mig--update-derived-url
                     "https://q.utoronto.ca/courses/12345/assignments/1001"
                     "12345" "67890" nil nil)))
  ;; No-op when IDs are nil
  (should (string= "https://q.utoronto.ca/courses/12345/assignments/1001"
                    (org-lms-mig--update-derived-url
                     "https://q.utoronto.ca/courses/12345/assignments/1001"
                     nil nil nil nil))))

;;; Test: Async Poll Cancel

(ert-deftest org-lms-mig-test-poll-cancel ()
  "Cancelling poll should clear timer and state."
  (let ((org-lms-mig--poll-timer (run-at-time 999 nil #'ignore)))
    (should org-lms-mig--poll-timer)
    (org-lms-mig--poll-cancel)
    (should-not org-lms-mig--poll-timer)))

;;; Test: Async Poll Setup

(ert-deftest org-lms-mig-test-async-poll-setup ()
  "Starting async poll should create timer and store state."
  (let ((org-lms-mig--poll-timer nil)
        (org-lms-mig--poll-state nil)
        (completed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'org-lms-mig-get-status)
                   (lambda (_course _mig)
                     '(:workflow_state "completed" :completion 100))))
          (org-lms-mig-poll-until-complete-async
           "12345" "mig-1" 30
           (lambda (_status) (setq completed t))
           nil nil)
          ;; Timer should be set
          (should org-lms-mig--poll-timer)
          (should (plist-get org-lms-mig--poll-state :course-id))
          ;; Let the timer fire once
          (sleep-for 1)
          ;; Should have completed since mock returns "completed"
          (should completed)
          (should-not org-lms-mig--poll-timer))
      ;; Cleanup in case test fails
      (org-lms-mig--poll-cancel))))

(provide 'org-lms-migration-tests)
;;; org-lms-migration-tests.el ends here
