;;; org-lms-image-tests.el --- ERT tests for Canvas inline image upload pipeline -*- lexical-binding: t -*-

;;; Commentary:
;; Sequential ERT tests for the inline image upload feature (Phases 1-6).
;; Runs against sandbox course 35724 (Matthew Price's Sandbox).
;; Test image: test/20100310_95.JPG
;;
;; Usage:
;;   M-x ert RET org-lms-image- RET
;; or from shell:
;;   emacs --batch -l test/org-lms-image-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'org)
(require 'ox-html)

;;; Setup

;; Load project files only if not already loaded (avoids clobbering session state)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir))
  (unless (fboundp 'org-lms-canvas-file-preview-url)
    (load (expand-file-name "../org-lms.el" dir) nil t))
  (unless (fboundp 'org-canvashtml-link)
    (load (expand-file-name "../ox-canvashtml.el" dir) nil t)))

;; Read token/baseurl from .env.sh only if not already set in this Emacs session
(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (env-file (expand-file-name "../.env.sh" dir)))
  (when (file-exists-p env-file)
    (with-temp-buffer
      (insert-file-contents env-file)
      (unless (stringp org-lms-token)
        (when (re-search-forward "OAUTHTOKEN=\"\\([^\"]+\\)\"" nil t)
          (setq org-lms-token (match-string 1))))
      (unless (stringp org-lms-baseurl)
        (when (re-search-forward "CANVASBASE=\"\\([^\"]+\\)\"" nil t)
          (setq org-lms-baseurl (concat (match-string 1) "/")))))))

;; Ensure baseurl ends with /api/v1/
(unless (and org-lms-baseurl (string-suffix-p "/api/v1/" org-lms-baseurl))
  (setq org-lms-baseurl "https://q.utoronto.ca/api/v1/"))

(defconst test-courseid 35724
  "Sandbox course ID for live API tests.")

(defconst test-image
  (expand-file-name "20100310_95.JPG"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to test image file.")

;; Store results between tests
(defvar test-last-upload-url nil
  "Canvas preview URL from the most recent upload test.")

(defvar test-last-cache nil
  "Cache alist from the most recent upload test.")

;;; Phase 1 Test: Canvas URL construction

(ert-deftest org-lms-image-test-01-canvas-file-preview-url ()
  "URL should be /courses/{id}/files/{id}/preview on q.utoronto.ca."
  (let ((url (org-lms-canvas-file-preview-url 12345 test-courseid)))
    (should (stringp url))
    (should (string= url "https://q.utoronto.ca/courses/35724/files/12345/preview"))))

;;; Phase 2 Tests: Cache load/save roundtrip

(ert-deftest org-lms-image-test-02-cache-roundtrip ()
  "Cache should persist across save/load cycle."
  (let* ((cache-file (make-temp-file "canvas-image-cache" nil ".el"))
         (test-cache '(("abc123" . "https://example.com/files/1/preview")
                       ("def456" . "https://example.com/files/2/preview")))
         loaded)
    (unwind-protect
        (progn
          ;; Write directly using the same format as org-lms-save-image-cache
          (with-temp-file cache-file
            (prin1 test-cache (current-buffer)))
          ;; Read it back
          (setq loaded (with-temp-buffer
                         (insert-file-contents cache-file)
                         (condition-case nil (read (current-buffer)) (error nil))))
          (should (equal loaded test-cache))
          (should (string= (alist-get "abc123" loaded nil nil #'string=)
                           "https://example.com/files/1/preview")))
      (when (file-exists-p cache-file)
        (delete-file cache-file)))))

;;; Phase 3 Test: Local image scanning

(ert-deftest org-lms-image-test-03-collect-local-images ()
  "Should find image link in a test org buffer."
  (let* ((image-path test-image)  ;; use constant set correctly at load time
         (org-content (format "#+TITLE: Test\n\n[[file:%s]]\n" image-path))
         (temp-org (make-temp-file "org-image-test" nil ".org"))
         result)
    (unwind-protect
        (progn
          (write-region org-content nil temp-org)
          (with-current-buffer (find-file-noselect temp-org)
            (setq result (org-lms-collect-local-images))
            (kill-buffer))
          (should (listp result))
          (should (= 1 (length result)))
          (should (string= (car result) image-path)))
      (when (file-exists-p temp-org)
        (delete-file temp-org)))))

;;; Phase 4 Test: Upload single image (live API call)

(ert-deftest org-lms-image-test-04-upload-image-to-canvas ()
  "Actually upload JPG to sandbox and get back a Canvas preview URL."
  :tags '(:live-api)
  (skip-unless (file-exists-p test-image))
  (skip-unless (stringp org-lms-token))
  (let* ((cache nil)
         (result (org-lms-upload-image-if-needed
                  test-image cache test-courseid "Test Images"))
         (canvas-url (car result))
         (updated-cache (cdr result)))
    (should (stringp canvas-url))
    (should (string-match "q\\.utoronto\\.ca/courses/35724/files/[0-9]+/preview"
                          canvas-url))
    ;; Store for deduplication test
    (setq test-last-upload-url canvas-url)
    (setq test-last-cache updated-cache)
    (message "Uploaded image. Canvas preview URL: %s" canvas-url)))

;;; Phase 4b Test: Deduplication (no re-upload)

(ert-deftest org-lms-image-test-05-upload-deduplication ()
  "Second upload of same file should return cached URL without API call."
  :tags '(:live-api)
  (skip-unless (stringp test-last-upload-url))
  (skip-unless (file-exists-p test-image))
  (let* ((result (org-lms-upload-image-if-needed
                  test-image test-last-cache test-courseid "Test Images"))
         (canvas-url (car result)))
    (should (string= canvas-url test-last-upload-url))
    (message "Deduplication OK: returned cached URL %s" canvas-url)))

;;; Phase 5+6 Test: Full pipeline — post a Canvas page with inline image

(ert-deftest org-lms-image-test-06-post-page-with-image ()
  "Post a test page to sandbox; image should be embedded as a Canvas preview URL."
  :tags '(:live-api)
  (skip-unless (file-exists-p test-image))
  (skip-unless (stringp org-lms-token))
  (let* ((image-path test-image)
         (org-content (format "#+TITLE: Image Test Page\n#+ORG_LMS_COURSEID: %d\n\n* Image Test Page\n:PROPERTIES:\n:OL_PUBLISH: true\n:END:\n\n[[file:%s]]\n\nThis page tests inline image upload.\n"
                              test-courseid image-path))
         (temp-org (make-temp-file "org-canvas-image-test" nil ".org"))
         response)
    (unwind-protect
        (progn
          (write-region org-content nil temp-org)
          (with-current-buffer (find-file-noselect temp-org)
            (org-mode)
            ;; Go to the first headline
            (goto-char (point-min))
            (re-search-forward "^\\* " nil t)
            (beginning-of-line)
            ;; Stub browse-url so the test doesn't try to open a browser
            (cl-letf (((symbol-function 'browse-url) #'ignore))
              (setq response (org-lms-post-page)))
            (kill-buffer))
          (should response)
          (let ((html-url (plist-get response :html_url)))
            (when html-url
              (message "Canvas page posted: %s" html-url)
              (message "Visually verify that the image renders at: %s" html-url))))
      (when (file-exists-p temp-org)
        (delete-file temp-org))
      ;; Clean up any cache file created
      (let ((cache (concat (file-name-directory temp-org) ".canvas-image-cache.el")))
        (when (file-exists-p cache)
          (delete-file cache))))))

;;; Layout QA constants and helpers

(defconst test-layout-org
  (expand-file-name "test-inline-image.org"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Persistent org file with all image layout variants for manual Canvas QA.")

(defun test-export-to-canvas-html (org-content)
  "Export ORG-CONTENT as canvas-html with juice mocked out.
Navigates to the first level-1 heading and exports that subtree.
Returns the HTML string."
  (let ((temp-org (make-temp-file "canvas-layout-test" nil ".org")))
    (unwind-protect
        (progn
          (write-region org-content nil temp-org)
          (with-current-buffer (find-file-noselect temp-org)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* " nil t)
            (beginning-of-line)
            (prog1
                (cl-letf (((symbol-function 'copy-file) #'ignore)
                          ((symbol-function 'call-process) (lambda (&rest _) 0)))
                  (org-export-as 'canvas-html t nil t))
              (kill-buffer))))
      (when (file-exists-p temp-org)
        (delete-file temp-org)))))

;;; Phase 7 Test: Float-right class propagation

(ert-deftest org-lms-image-test-07-float-right-class ()
  "#+ATTR_HTML :class float-right should appear on the figure wrapper div."
  (let* ((html (test-export-to-canvas-html
                "#+TITLE: Float Test\n\n* Float Test\n\n#+ATTR_HTML: :class float-right\n[[file:fake.jpg]]\n")))
    (should (stringp html))
    (should (string-match-p "class=\"figure float-right\"" html))))

;;; Phase 8 Test: Two-column flex layout classes

(ert-deftest org-lms-image-test-08-two-column-layout ()
  "canvas-2col and canvas-col special blocks should export with correct classes."
  (let* ((html (test-export-to-canvas-html
                (concat "#+TITLE: Two Col Test\n\n* Two Col Test\n\n"
                        "#+BEGIN_canvas-2col\n"
                        "#+BEGIN_canvas-col\nText.\n#+END_canvas-col\n"
                        "#+BEGIN_canvas-col\nMore text.\n#+END_canvas-col\n"
                        "#+END_canvas-2col\n"))))
    (should (stringp html))
    (should (string-match-p "class=\"canvas-2col\"" html))
    (should (string-match-p "class=\"canvas-col\"" html))))

;;; Phase 9 Test: Wide + narrow column classes

(ert-deftest org-lms-image-test-09-wide-narrow-columns ()
  "canvas-col-wide and canvas-col-narrow classes should both appear in output."
  (let* ((html (test-export-to-canvas-html
                (concat "#+TITLE: Wide Narrow\n\n* Wide Narrow\n\n"
                        "#+BEGIN_canvas-2col\n"
                        "#+BEGIN_canvas-col-wide\nWide.\n#+END_canvas-col-wide\n"
                        "#+BEGIN_canvas-col-narrow\nNarrow.\n#+END_canvas-col-narrow\n"
                        "#+END_canvas-2col\n"))))
    (should (stringp html))
    (should (string-match-p "class=\"canvas-col-wide\"" html))
    (should (string-match-p "class=\"canvas-col-narrow\"" html))))

;;; Phase 10 Test: Post full layout page to Canvas (live API)

(ert-deftest org-lms-image-test-10-post-layout-page ()
  "Post test/test-inline-image.org to Canvas sandbox; prints URL for visual QA.
After the first run, CANVAS_SHORT_URL is saved to the file so future
runs update the same page via PUT rather than creating a new one."
  :tags '(:live-api)
  (skip-unless (file-exists-p test-layout-org))
  (skip-unless (stringp org-lms-token))
  (let* (response)
    (with-current-buffer (find-file-noselect test-layout-org)
      (org-mode)
      (goto-char (point-min))
      (re-search-forward "^\\* " nil t)
      (beginning-of-line)
      (cl-letf (((symbol-function 'browse-url) #'ignore))
        (setq response (org-lms-post-page)))
      (when (buffer-modified-p)
        (save-buffer))
      (kill-buffer))
    (should response)
    (let ((html-url (plist-get response :html_url)))
      (when html-url
        (message "Canvas layout page posted: %s" html-url)
        (message "Visually verify all layouts at: %s" html-url)))))


;;; Phase 11 Test: Subtree-scoped image collection

(ert-deftest org-lms-image-test-11-collect-local-images-subtree ()
  "With SUBTREEP non-nil, only images inside the current subtree are collected."
  (let* ((org-content
          (format (concat "#+TITLE: Scope Test\n\n"
                          "* First\n\n[[file:%s]]\n\n"
                          "* Second\n\nNo image here.\n")
                  test-image))
         (temp-org (make-temp-file "org-image-scope" nil ".org"))
         in-first in-second)
    (unwind-protect
        (progn
          (write-region org-content nil temp-org)
          (with-current-buffer (find-file-noselect temp-org)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* First" nil t)
            (beginning-of-line)
            (setq in-first (org-lms-collect-local-images nil t))
            (goto-char (point-min))
            (re-search-forward "^\\* Second" nil t)
            (beginning-of-line)
            (setq in-second (org-lms-collect-local-images nil t))
            (kill-buffer))
          (should (equal in-first (list test-image)))
          (should (null in-second)))
      (when (file-exists-p temp-org)
        (delete-file temp-org)))))

;;; Helpers for offline post tests

(defconst test-stub-file-id 4242
  "Canvas file ID returned by the stubbed uploader in offline post tests.")

(defun test-post-headline-offline (org-content poster)
  "Write ORG-CONTENT to a temp org file, then call POSTER on its first headline.
Canvas network calls are stubbed: the file uploader returns
`test-stub-file-id', and `org-lms-canvas-request' records its arguments
instead of issuing a request.  Returns the request-params alist that
POSTER passed to `org-lms-canvas-request'."
  (let ((temp-org (make-temp-file "org-canvas-offline" nil ".org"))
        captured)
    (unwind-protect
        (progn
          (write-region org-content nil temp-org)
          (with-current-buffer (find-file-noselect temp-org)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* " nil t)
            (beginning-of-line)
            (cl-letf (((symbol-function 'browse-url) #'ignore)
                      ((symbol-function 'org-lms-post-new-file)
                       (lambda (&rest _)
                         (format "{\"id\": %d}" test-stub-file-id)))
                      ((symbol-function 'org-lms-canvas-request)
                       (lambda (_query &optional _type params &rest _)
                         (setq captured params)
                         (list :id 1
                               :url "https://q.utoronto.ca/courses/35724/discussion_topics/1"
                               :html_url "https://q.utoronto.ca/courses/35724/assignments/1"
                               :posted_at "2026-01-01T00:00:00Z"
                               :due_at "2026-01-08T00:00:00Z"
                               :published t))))
              (funcall poster))
            (set-buffer-modified-p nil)
            (kill-buffer))
          captured)
      (when (file-exists-p temp-org)
        (delete-file temp-org))
      (let ((cache (concat (file-name-directory temp-org) ".canvas-image-cache.el")))
        (when (file-exists-p cache)
          (delete-file cache))))))

(defun test-expected-preview-url ()
  "Canvas preview URL the stubbed uploader should produce for course 35724."
  (org-lms-canvas-file-preview-url test-stub-file-id test-courseid))

;;; Phase 12 Test: Announcement embeds uploaded image

(ert-deftest org-lms-image-test-12-announcement-embeds-canvas-url ()
  "An inline local image in an announcement is uploaded and linked by Canvas URL."
  (skip-unless (file-exists-p test-image))
  (let* ((params (test-post-headline-offline
                  (format (concat "#+TITLE: Ann Test\n#+ORG_LMS_COURSEID: %d\n\n"
                                  "* Announcement With Image\n\n[[file:%s]]\n\nBody text.\n")
                          test-courseid test-image)
                  #'org-lms-headline-to-announcement))
         (message-html (alist-get "message" params nil nil #'string=)))
    (should (stringp message-html))
    (should (string-match-p (regexp-quote (test-expected-preview-url)) message-html))
    (should-not (string-match-p "src=\"file:" message-html))))

;;; Phase 13 Test: Assignment embeds uploaded image

(ert-deftest org-lms-image-test-13-assignment-embeds-canvas-url ()
  "An inline local image in an assignment is uploaded and linked by Canvas URL."
  (skip-unless (file-exists-p test-image))
  (let* ((params (test-post-headline-offline
                  (format (concat "#+TITLE: Assn Test\n#+ORG_LMS_COURSEID: %d\n\n"
                                  "* Assignment With Image\n\n[[file:%s]]\n\nDo the thing.\n")
                          test-courseid test-image)
                  #'org-lms-post-assignment))
         (description (alist-get "description"
                                 (alist-get "assignment" params nil nil #'string=)
                                 nil nil #'string=)))
    (should (stringp description))
    (should (string-match-p (regexp-quote (test-expected-preview-url)) description))
    (should-not (string-match-p "src=\"file:" description))))


;;; Phase 14/15 Tests: ORG_LMS_WITHHOLD delays an announcement

;; Canvas has no draft state for announcements.  It ignores published=false at
;; create time and answers HTTP 400 to a PUT carrying it (verified against
;; course 35724, topic 3612569, 2026-09-21).  The only way to keep an
;; announcement off students' radar is delayed_post_at.

(ert-deftest org-lms-image-test-14-announcement-withhold-delays-posting ()
  "ORG_LMS_WITHHOLD sends a future delayed_post_at and leaves published true."
  (let* ((params (test-post-headline-offline
                  (format (concat "#+TITLE: Withheld\n#+ORG_LMS_COURSEID: %d\n\n"
                                  "* Withheld Announcement\n"
                                  ":PROPERTIES:\n:ORG_LMS_WITHHOLD: 2027-01-15\n:END:\n\nBody.\n")
                          test-courseid)
                  #'org-lms-headline-to-announcement))
         (delayed (alist-get "delayed_post_at" params nil nil #'string=)))
    (should (eq t (alist-get "published" params nil nil #'string=)))
    (should (stringp delayed))
    (should (string-prefix-p "2027-01-15" delayed))))

(ert-deftest org-lms-image-test-15-announcement-defaults-to-posting-now ()
  "Without ORG_LMS_WITHHOLD no delayed_post_at is sent."
  (let* ((params (test-post-headline-offline
                  (format (concat "#+TITLE: Live\n#+ORG_LMS_COURSEID: %d\n\n"
                                  "* Live Announcement\n\nBody.\n")
                          test-courseid)
                  #'org-lms-headline-to-announcement)))
    (should (eq t (alist-get "published" params nil nil #'string=)))
    (should-not (assoc "delayed_post_at" params))))

;;; Phase 16 Test: assignments are unpublished with a real JSON false

(ert-deftest org-lms-image-test-16-assignment-unpublished-is-json-false ()
  "Without OL_PUBLISH an assignment sends published=false, not null.
Canvas ignores a null, so nil would leave the publish state to chance."
  (let* ((params (test-post-headline-offline
                  (format (concat "#+TITLE: Draft\n#+ORG_LMS_COURSEID: %d\n\n"
                                  "* Draft Assignment\n\nBody.\n")
                          test-courseid)
                  #'org-lms-post-assignment))
         (assignment (alist-get "assignment" params nil nil #'string=)))
    (should (eq :json-false (alist-get "published" assignment nil nil #'string=)))))

;;; Phase 17 Test: rewritten images keep a meaningful alt attribute

(ert-deftest org-lms-image-test-17-rewritten-image-keeps-filename-alt ()
  "Rewriting a file: link to a Canvas preview URL must not make alt=\"preview\".
`org-html--format-image' derives a missing alt from the last path segment,
which is \"preview\" for every Canvas file URL."
  (skip-unless (file-exists-p test-image))
  (let* ((params (test-post-headline-offline
                  (format (concat "#+TITLE: Alt Test\n#+ORG_LMS_COURSEID: %d\n\n"
                                  "* Alt Test\n\n[[file:%s]]\n")
                          test-courseid test-image)
                  #'org-lms-headline-to-announcement))
         (message-html (alist-get "message" params nil nil #'string=)))
    (should (stringp message-html))
    (should (string-match-p "alt=\"20100310_95\\.JPG\"" message-html))
    (should-not (string-match-p "alt=\"preview\"" message-html))))


;;; Phase 18 Test: #+OPTIONS: tags:t must not leak tags into Canvas

(ert-deftest org-lms-image-test-18-file-options-cannot-reenable-tags ()
  "A file that sets `#+OPTIONS: tags:t' must still post without tags.
In-buffer `#+OPTIONS:' -- usually inherited from a shared `#+SETUPFILE:'
-- override both the global `org-export-with-tags' and `org-export-as''s
EXT-PLIST, so a plain `let' around the export is not enough."
  (let* ((params (test-post-headline-offline
                  (format (concat "#+TITLE: Tagged\n#+ORG_LMS_COURSEID: %d\n"
                                  "#+OPTIONS: tags:t\n\n"
                                  "* Parent Assignment\n\nIntro.\n\n"
                                  "** Finding Your Folder   :ATTACH:\n\nChild body.\n")
                          test-courseid)
                  #'org-lms-post-assignment))
         (description (alist-get "description"
                                 (alist-get "assignment" params nil nil #'string=)
                                 nil nil #'string=)))
    (should (stringp description))
    (should (string-match-p "Finding Your Folder" description))
    (should-not (string-match-p "ATTACH" description))))

(ert-deftest org-lms-image-test-19-announcement-also-suppresses-tags ()
  "Announcements suppress tags too; the function never bound the option at all."
  (let* ((params (test-post-headline-offline
                  (format (concat "#+TITLE: Tagged\n#+ORG_LMS_COURSEID: %d\n"
                                  "#+OPTIONS: tags:t\n\n"
                                  "* Parent Announcement\n\nIntro.\n\n"
                                  "** Sub Heading   :ATTACH:\n\nChild body.\n")
                          test-courseid)
                  #'org-lms-headline-to-announcement))
         (message-html (alist-get "message" params nil nil #'string=)))
    (should (stringp message-html))
    (should (string-match-p "Sub Heading" message-html))
    (should-not (string-match-p "ATTACH" message-html))))

;;; Run all tests if invoked as a batch script

(when noninteractive
  (ert-run-tests-batch-and-exit "org-lms-image-test-"))

;;; org-lms-image-tests.el ends here
