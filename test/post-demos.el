;;; Post the two demo subtrees to Canvas.  -*- lexical-binding: t -*-
;;; Token comes from the CANVAS_TOKEN environment variable.
(require 'org)
(require 'ox-html)
(add-to-list 'load-path "/Users/pricemat/src/org-grading")
(load "/Users/pricemat/src/org-grading/org-lms.el" nil t)
(load "/Users/pricemat/src/org-grading/ox-canvashtml.el" nil t)

(setq org-lms-baseurl "https://q.utoronto.ca/api/v1/"
      org-lms-token (getenv "CANVAS_TOKEN")
      org-canvas-html-css-file "/Users/pricemat/src/org-grading/canvas-styles.css"
      org-lms-request-timeout 120)

(unless (and org-lms-token (> (length org-lms-token) 20))
  (error "CANVAS_TOKEN is not set"))

(defun post-demo (file poster label)
  (with-current-buffer (find-file-noselect file)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "^\\* " nil t)
    (beginning-of-line)
    (let ((resp (cl-letf (((symbol-function 'browse-url) #'ignore))
                  (funcall poster))))
      (when (buffer-modified-p) (save-buffer))
      (kill-buffer)
      (princ (format "\n==== %s ====\nid:        %s\npublished: %S\nurl:       %s\nhtml_url:  %s\n"
                     label
                     (plist-get resp :id)
                     (plist-get resp :published)
                     (or (plist-get resp :url) "")
                     (or (plist-get resp :html_url) "")))
      resp)))

(post-demo "/Users/pricemat/src/org-grading/test/demo-announcement.org"
           #'org-lms-headline-to-announcement "ANNOUNCEMENT")
(post-demo "/Users/pricemat/src/org-grading/test/demo-assignment.org"
           #'org-lms-post-assignment "ASSIGNMENT")
