;;; org-lms-rubrics.el --- Rubric functions for org-lms -*- lexical-binding: t; -*-

;; This file provides functions for managing Canvas LMS rubrics through org-mode
;; It depends on org-lms.el and extends its functionality

;;; Code:

(require 'org-lms)
(require 'cl-lib)

;; Rubric data structure functions

(defun org-lms-rubric-headline-to-params ()
  "Convert the current org-mode headline and its subtree to a rubric parameter list.
Assumes the structure:
* Rubric Title                               :rubric:
  :PROPERTIES:
  :RUBRIC_ID: [optional existing ID]
  :POINTS_POSSIBLE: 10
  :END:
  ** Criterion 1 (5 points)
     :PROPERTIES:
     :CRITERION_ID: [optional existing ID]
     :POINTS: 5
     :END:
     - Full Marks (5 pts): Description for full marks
     - Partial Marks (3 pts): Description for partial
     - No Marks (0 pts): Description for no marks
  ** Criterion 2 (5 points)
     ..."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (nth 4 (org-heading-components)))
           (rubric-id (org-entry-get nil "RUBRIC_ID"))
           (points-possible (string-to-number (or (org-entry-get nil "POINTS_POSSIBLE") "0")))
           (free-form-criterion (org-entry-get nil "FREE_FORM_CRITERION"))
           (hide-points (or (org-entry-get nil "HIDE_POINTS") nil))
           (criteria '())
           (params '()))
      
      ;; Build the parameters list
      (setq params `(("title" . ,title)
                     ("points_possible" . ,points-possible)))
      
      ;; Add optional parameters if present
      (when free-form-criterion
        (push `("free_form_criterion" . ,free-form-criterion) params))
      
      (when hide-points
        (push `("hide_points" . ,(if (equal hide-points "nil") "false" "true")) params))
      
      ;; Process each criterion (subheading)
      (save-restriction
        (org-narrow-to-subtree)
        (let ((continue t))
          (org-forward-heading-same-level nil t)
          (while continue
            (when (org-at-heading-p)
              ;; Extract criterion information
              (let* ((criterion-name (nth 4 (org-heading-components)))
                     (criterion-id (org-entry-get nil "CRITERION_ID"))
                     (points (string-to-number (or (org-entry-get nil "POINTS") "0")))
                     (description (org-entry-get nil "DESCRIPTION"))
                     (ratings '())
                     (criterion-index (length criteria))
                     (criterion-params `(("description" . ,criterion-name)
                                         ("points" . ,points))))
                
                ;; Add criterion_id if available
                (when criterion-id
                  (push `("id" . ,criterion-id) criterion-params))
                
                ;; Add long description if available
                (when description
                  (push `("long_description" . ,description) criterion-params))
                
                ;; Parse ratings from the bullet points in the criterion
                (let ((ratings-text (org-get-entry))
                      (rating-index 0))
                  (with-temp-buffer
                    (insert ratings-text)
                    (goto-char (point-min))
                    (while (re-search-forward "- \\(.*?\\) (\\([0-9.]+\\) pts):\\(.*\\)$" nil t)
                      (let ((rating-title (match-string-no-properties 1))
                            (rating-points (string-to-number (match-string-no-properties 2)))
                            (rating-desc (string-trim (match-string-no-properties 3))))
                        (push `(("description" . ,rating-title)
                                ("points" . ,rating-points)
                                ("long_description" . ,rating-desc))
                              ratings)
                        (setq rating-index (1+ rating-index))))))
                
                ;; Add ratings to criterion
                (let ((rev-ratings (nreverse ratings)))
                  (push `("ratings" . ,rev-ratings) criterion-params))
                
                ;; Add criterion to criteria array with appropriate index
                (push criterion-params criteria)))
            
            ;; Move to next criterion or exit
            (setq continue (org-get-next-sibling)))))
      
      ;; Add criteria to parameters in the proper format
      (let ((rev-criteria (nreverse criteria)))
        (dotimes (i (length rev-criteria))
          (let ((criterion (nth i rev-criteria)))
            (dolist (param criterion)
              (let ((param-name (car param))
                    (param-value (cdr param)))
                (if (equal param-name "ratings")
                    ;; Handle ratings specially
                    (dotimes (j (length param-value))
                      (let ((rating (nth j param-value)))
                        (dolist (rating-param rating)
                          (let ((rating-name (car rating-param))
                                (rating-value (cdr rating-param)))
                            (push `(,(format "criteria[%d][ratings][%d][%s]" i j rating-name) . ,rating-value) params)))))
                  ;; Regular criterion parameters
                  (push `(,(format "criteria[%d][%s]" i param-name) . ,param-value) params))))))
      (message "RUBRICPARAMS: %s" params))
      ;; Return full params
      params)))

(defun org-lms-params-to-rubric-headline (params)
  "Convert a Canvas API rubric parameters list to an org-mode headline structure.
Will insert at current point."
  (let* ((title (alist-get 'title params))
         (rubric-id (alist-get 'id params))
         (points-possible (alist-get 'points_possible params))
         (free-form-criterion (alist-get 'free_form_criterion params))
         (hide-points (alist-get 'hide_points params))
         (criteria (or (alist-get 'data params) (alist-get 'criteria params))))

    ;; Insert main rubric heading
    (insert (format "* %s :rubric:\n" title))
    (insert ":PROPERTIES:\n")
    (when rubric-id
      (insert (format ":RUBRIC_ID: %s\n" rubric-id)))
    (insert (format ":POINTS_POSSIBLE: %s\n" points-possible))
    (when free-form-criterion
      (insert (format ":FREE_FORM_CRITERION: %s\n" free-form-criterion)))
    (when hide-points
      (insert (format ":HIDE_POINTS: %s\n" hide-points)))
    (insert ":END:\n\n")

    ;; Insert each criterion as a subheading
    (dolist (criterion criteria)
      (let ((desc (plist-get criterion :description))
            (criterion-id (plist-get criterion :id))
            (points (plist-get criterion :points))
            (long-desc (plist-get criterion :long_description))
            (ratings (plist-get criterion :ratings)))

        ;; Insert criterion heading
        (insert (format "** %s (%s points)\n" desc points))
        (insert ":PROPERTIES:\n")
        (when criterion-id
          (insert (format ":CRITERION_ID: %s\n" criterion-id)))
        (insert (format ":POINTS: %s\n" points))
        (when long-desc
          (insert (format ":DESCRIPTION: %s\n" long-desc)))
        (insert ":END:\n\n")

        ;; Insert each rating as a bullet item
        (dolist (rating ratings)
          (let ((rating-desc (plist-get rating :description))
                (rating-points (plist-get rating :points))
                (rating-long-desc (plist-get rating :long_description)))
            (insert (format "- %s (%s pts): %s\n" 
                            rating-desc 
                            rating-points 
                            (or rating-long-desc "")))))
        (insert "\n")))))

;; API functions

(defun org-lms-get-rubrics (&optional courseid)
  "Get all rubrics for the course with ID COURSEID.
If COURSEID is nil, it is retrieved from the ORG_LMS_COURSEID keyword or property."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request (format "courses/%s/rubrics" courseid) "GET"))

(defun org-lms-get-single-rubric (rubricid &optional courseid)
  "Get a single rubric with ID RUBRICID for the course with ID COURSEID.
If COURSEID is nil, it is retrieved from the ORG_LMS_COURSEID keyword or property.
Optionally includes assessments if INCLUDE_ASSESSMENTS is non-nil."
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (org-lms-canvas-request (format "courses/%s/rubrics/%s" courseid rubricid) "GET"))

(defun org-lms-set-rubric (&optional association-id association-type)
  "Create or update a rubric based on the current org-mode headline.
If ASSOCIATION-ID and ASSOCIATION-TYPE are provided, associate the rubric
with that object. ASSOCIATION-TYPE should be 'Assignment', 'Course', or 'Account'.

If the headline already carries `:RUBRIC_ID:', issues a PUT to update; otherwise
POST to create.  Refuses to update if Canvas reports the rubric as `read_only'
(used for grading) -- create a new rubric and re-associate instead."
  (interactive)
  (let* ((rubric-id (org-entry-get nil "RUBRIC_ID"))
         (courseid (or (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
         (method (if rubric-id "PUT" "POST"))
         (url (if rubric-id
                  (format "courses/%s/rubrics/%s" courseid rubric-id)
                (format "courses/%s/rubrics" courseid)))
         (api-params '()))

    ;; Locked-rubric guard: fail fast before doing any work if Canvas has
    ;; already marked this rubric read-only (typically after a grader has
    ;; assessed at least one student against it).
    (when rubric-id
      (let ((existing (ignore-errors
                        (org-lms-get-single-rubric rubric-id courseid))))
        (when (and existing
                   (or (eq (plist-get existing :read_only) t)
                       (equal (plist-get existing :read_only) "true")))
          (user-error
           (concat "Rubric %s is read-only on Canvas (already used for grading).\n"
                   "To make changes, create a new rubric (clear :RUBRIC_ID:)\n"
                   "and re-associate the assignment with it.")
           rubric-id))))
    
    ;; Build basic rubric parameters
    (let* ((title (nth 4 (org-heading-components)))
           (points-possible (string-to-number (or (org-entry-get nil "POINTS_POSSIBLE") "0")))
           (free-form-criterion (org-entry-get nil "FREE_FORM_CRITERION"))
           (hide-points (or (org-entry-get nil "HIDE_POINTS") nil)))
      
      ;; Add basic rubric parameters
      (push `("rubric[title]" . ,title) api-params)
      (push `("rubric[points_possible]" . ,points-possible) api-params)
      
      ;; Add optional parameters
      (when free-form-criterion
        (push `("rubric[free_form_criterion_comments]" . ,(if (equal free-form-criterion "nil") "false" "true")) api-params))
      
      (when hide-points
        (push `("rubric[hide_points]" . ,(if (equal hide-points "nil") "false" "true")) api-params))
      
      ;; Process each criterion (subheading)
      (save-restriction
        (org-narrow-to-subtree)
        (let ((criterion-count 0)
              (continue t))
          (org-forward-heading-same-level nil t)
          (while continue
            (when (org-at-heading-p)
              ;; Extract criterion information.  :LONG_DESCRIPTION: is the
              ;; canonical property name (matches Canvas's `long_description'
              ;; field); fall back to legacy `:DESCRIPTION:' if present so
              ;; existing rubrics keep working.
              (let* ((criterion-name (nth 4 (org-heading-components)))
                     (criterion-id (or (org-entry-get nil "CRITERION_ID") (format "_criterion%d" criterion-count)))
                     (points (string-to-number (or (org-entry-get nil "POINTS") "0")))
                     (description (or (org-entry-get nil "LONG_DESCRIPTION")
                                      (org-entry-get nil "DESCRIPTION")))
                     (outcome-id (org-entry-get nil "OUTCOME_ID")))

                (push `(,(format "rubric[criteria][%s][description]" criterion-id) . ,criterion-name) api-params)
                (push `(,(format "rubric[criteria][%s][points]" criterion-id) . ,points) api-params)
                (push `(,(format "rubric[criteria][%s][criterion_use_range]" criterion-id) . "false") api-params)

                ;; Add long description if available
                (when description
                  (push `(,(format "rubric[criteria][%s][long_description]" criterion-id) . ,description) api-params))

                ;; Optional learning-outcome alignment
                (when outcome-id
                  (push `(,(format "rubric[criteria][%s][learning_outcome_id]" criterion-id) . ,outcome-id) api-params))

                ;; Parse ratings from the bullet points in the criterion --
                ;; but skip entirely when this rubric is in free-form mode
                ;; (Canvas doesn't accept ratings for free-form rubrics).
                (let ((ratings-text (org-get-entry))
                      (rating-count 0))
                  (with-temp-buffer
                    (insert ratings-text)
                    (goto-char (point-min))
                    (unless (and free-form-criterion
                                 (not (equal free-form-criterion "nil")))
                    (while (re-search-forward "^[ \t]*- \\(.*?\\) (\\([0-9.]+\\) pts):[ \t]*\\(.*\\)$" nil t)
                      (let ((rating-title (match-string-no-properties 1))
                            (rating-points (string-to-number (match-string-no-properties 2)))
                            (rating-desc (string-trim (match-string-no-properties 3)))
                            (rating-id (format "_rating%d" rating-count)))
                        
                        ;; Add rating parameters - Canvas expects ratings with unique IDs
                        (push `(,(format "rubric[criteria][%s][ratings][%s][description]" 
                                               criterion-id rating-id) 
                                       . ,rating-title) api-params)
                        
                        (push `(,(format "rubric[criteria][%s][ratings][%s][points]" 
                                               criterion-id rating-id) 
                                       . ,rating-points) api-params)
                        
                        (push `(,(format "rubric[criteria][%s][ratings][%s][long_description]" 
                                               criterion-id rating-id) 
                                       . ,rating-desc) api-params)
                        
                        (setq rating-count (1+ rating-count)))))))

                (setq criterion-count (1+ criterion-count))))
            
            ;; Move to next criterion or exit
            (setq continue (org-get-next-sibling))))))
    
    ;; Add association which is REQUIRED for creation
    (setq association-type (or association-type "Course"))
    (setq association-id (or association-id courseid))
      
    ;; Add the required association parameters
    (push `("rubric_association[association_id]" . ,association-id) api-params)
    (push `("rubric_association[association_type]" . ,association-type) api-params)
    (push '("rubric_association[use_for_grading]" . "false") api-params)
    (push '("rubric_association[purpose]" . "bookmark") api-params)
    
    ;; Additional association parameters required by the API
    (push `("rubric_association[rubric_id]" . ,(or rubric-id "null")) api-params)
    (push '("rubric_association[title]" . "") api-params)
    (push '("rubric_association[hide_score_total]" . "false") api-params)
    
    ;; Debug message
    (message "Sending %s to %s with params: %S" method url api-params)

    ;; Make API request — POST to create, PUT to update.
    (let ((response (org-lms-canvas-request url method api-params)))
      ;; Update properties based on response
      (when response
        (message "Response: %S" response)
        ;; The response might be structured differently for create vs update
        (let ((rubric-id-val nil)
              (rubric-assoc (plist-get response :rubric_association)))
          
          ;; Try to extract the rubric ID from the response
          (cond
           ;; If there's a rubric_association with rubric_id
           ((and rubric-assoc (plist-get rubric-assoc :rubric_id))
            (setq rubric-id-val (plist-get rubric-assoc :rubric_id)))
           
           ;; If there's a direct rubric with id
           ((plist-get response :id)
            (setq rubric-id-val (plist-get response :id)))
           
           ;; If there's a nested rubric object
           ((and (plist-get response :rubric) (plist-get (plist-get response :rubric) :id))
            (setq rubric-id-val (plist-get (plist-get response :rubric) :id))))
          
          ;; Update the property if we got an ID
          (when rubric-id-val
            (org-entry-put nil "RUBRIC_ID" (format "%s" rubric-id-val))
            (message "Rubric ID %s successfully %s." 
                     rubric-id-val
                     (if rubric-id "updated" "created")))))
      response)))

(defun org-lms-associate-rubric-with-assignment (rubric-id assignment-id &optional courseid use-for-grading)
  "Associate an existing rubric with RUBRIC-ID to an assignment with ASSIGNMENT-ID.
If COURSEID is nil, it is retrieved from the ORG_LMS_COURSEID keyword or property.
If USE-FOR-GRADING is non-nil, the rubric will be used in grade calculations."
  (interactive "sRubric ID: \nsAssignment ID: ")
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (let* ((params `(("rubric_association[rubric_id]" . ,rubric-id)
                   ("rubric_association[association_id]" . ,assignment-id)
                   ("rubric_association[association_type]" . "Assignment")
                   ("rubric_association[purpose]" . "grading")
                   ("rubric_association[use_for_grading]" . ,(if use-for-grading "true" "false"))))
         (response (org-lms-canvas-request 
                    (format "courses/%s/rubric_associations" courseid) 
                    "POST" 
                    params)))
    (message "Rubric %s associated with assignment %s." rubric-id assignment-id)
    response))

(defun org-lms-insert-rubric-from-api (rubric-id &optional courseid)
  "Fetch a rubric with RUBRIC-ID from Canvas and insert it as an org-mode headline.
If COURSEID is nil, it is retrieved from the ORG_LMS_COURSEID keyword or property."
  (interactive "sRubric ID: ")
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (let* ((rubric (org-lms-get-single-rubric rubric-id courseid))
         (params (org-lms-plist-to-alist rubric)))
    (org-lms-params-to-rubric-headline params)))

(defun org-lms-display-rubrics (&optional courseid)
  "Display all rubrics for the course in a new buffer.
If COURSEID is nil, it is retrieved from the ORG_LMS_COURSEID keyword or property."
  (interactive)
  (setq courseid (or courseid (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
  (let ((rubrics (org-lms-get-rubrics courseid))
        (buffer (get-buffer-create "*Canvas Rubrics*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Canvas Rubrics\n\n")
      (dolist (rubric rubrics)
        (let ((params (org-lms-plist-to-alist rubric)))
          (org-lms-params-to-rubric-headline params)
          (insert "\n")))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun org-lms-test-create-rubric (&optional title)
  "Create a simple test rubric with a default structure for testing."
  (interactive)
  (let ((buffer (get-buffer-create "*Test Rubric*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (format "* %s :rubric:\n" (or title "Test Rubric")))
      (insert ":PROPERTIES:\n")
      (insert ":POINTS_POSSIBLE: 10\n")
      (insert ":END:\n\n")
      
      ;; First criterion
      (insert "** Content (6 points)\n")
      (insert ":PROPERTIES:\n")
      (insert ":POINTS: 6\n")
      (insert ":DESCRIPTION: Quality of content and analysis\n")
      (insert ":END:\n\n")
      (insert "- Excellent (6 pts): Thorough and insightful analysis\n")
      (insert "- Good (4 pts): Good analysis with some insights\n")
      (insert "- Satisfactory (2 pts): Basic analysis only\n")
      (insert "- Unsatisfactory (0 pts): Incomplete or incorrect analysis\n\n")
      
      ;; Second criterion
      (insert "** Organization (4 points)\n")
      (insert ":PROPERTIES:\n")
      (insert ":POINTS: 4\n")
      (insert ":DESCRIPTION: Logical organization and flow\n")
      (insert ":END:\n\n")
      (insert "- Excellent (4 pts): Well-organized with clear flow\n")
      (insert "- Good (3 pts): Generally well-organized\n") 
      (insert "- Satisfactory (2 pts): Some organizational issues\n")
      (insert "- Unsatisfactory (0 pts): Poorly organized and difficult to follow\n"))
    
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (message "Test rubric created. Use org-lms-set-rubric to create it on Canvas.")))

;;; ----- wim integration + assignment-side helpers -----

(defun org-lms-rubric-wim ()
  "wim dispatch entry for rubric headlines.
Validates that point is within a `:rubric:'-tagged subtree, then posts via
`org-lms-set-rubric'.  Routed via `org-lms-wim-wim' when the buffer's
`#+ORG_LMS_SECTION:' keyword is `rubric'."
  (interactive)
  (save-window-excursion
    (widen)
    (save-excursion
      (let ((subtree (org-lms--get-valid-subtree 'isrubric)))
        (if subtree
            (org-lms-set-rubric)
          (message "Point is not inside a :rubric:-tagged subtree; not posted."))))))

(defun org-lms-resolve-rubric-id-from-heading (heading &optional rubrics-file)
  "Return the :RUBRIC_ID: of the rubric headline whose title equals HEADING.
Searches RUBRICS-FILE (defaulting to `Rubrics.org' next to the syllabus, then
the project-root `Rubrics.org').  Returns nil if no match.  Signals an error
if the matching headline lacks a `:RUBRIC_ID:' property (i.e. has not yet been
posted to Canvas)."
  (let* ((candidates
          (delq nil
                (list rubrics-file
                      (and (org-lms-get-keyword "ORG_LMS_COURSEID")
                           (expand-file-name "Rubrics.org" default-directory))
                      (expand-file-name "Rubrics.org" default-directory))))
         (file (cl-find-if #'file-readable-p candidates))
         (rubric-id nil))
    (when file
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when (re-search-forward
                   (format "^\\* +%s\\(?:\\s-+:[a-zA-Z0-9_:]+:\\)?\\s-*$"
                           (regexp-quote heading))
                   nil t)
              (setq rubric-id (org-entry-get (point) "RUBRIC_ID"))
              (unless rubric-id
                (user-error
                 "Rubric headline %S found in %s but has no :RUBRIC_ID: property -- post it to Canvas first"
                 heading (file-name-nondirectory file))))))))
    rubric-id))

(defun org-lms-ensure-rubric-association-for-assignment ()
  "If the assignment headline at point declares a rubric, sync the association.
Reads `:RUBRIC_HEADING:' (preferred, resolves through Rubrics.org) or
`:RUBRIC_ID:' from the headline's PROPERTIES drawer, plus optional
`:RUBRIC_USE_FOR_GRADING:', `:RUBRIC_HIDE_POINTS:',
`:RUBRIC_HIDE_SCORE_TOTAL:'.

Idempotent: GETs existing rubric_associations for the assignment first; PUTs
to update an existing association, POSTs to create a new one.  Writes
`:RUBRIC_ASSOCIATION_ID:' back to the headline on success."
  (interactive)
  (let* ((courseid (or (org-lms-get-keyword "ORG_LMS_COURSEID") (plist-get org-lms-course)))
         (assignment-id (org-entry-get (point) "CANVASID"))
         (rubric-heading (org-entry-get (point) "RUBRIC_HEADING"))
         (rubric-id (or (org-entry-get (point) "RUBRIC_ID")
                        (and rubric-heading
                             (org-lms-resolve-rubric-id-from-heading rubric-heading))))
         (use-for-grading (or (org-entry-get (point) "RUBRIC_USE_FOR_GRADING") "true"))
         (hide-points (or (org-entry-get (point) "RUBRIC_HIDE_POINTS") "false"))
         (hide-score-total (or (org-entry-get (point) "RUBRIC_HIDE_SCORE_TOTAL") "false"))
         (existing-assoc-id (org-entry-get (point) "RUBRIC_ASSOCIATION_ID")))
    ;; No-op for assignments without a rubric reference (most of them).
    (when (and rubric-id assignment-id)
    (let* ((bool (lambda (v) (if (or (eq v t) (member (format "%s" v) '("t" "true" "yes"))) "true" "false")))
           (params `(("rubric_association[rubric_id]" . ,rubric-id)
                     ("rubric_association[association_id]" . ,assignment-id)
                     ("rubric_association[association_type]" . "Assignment")
                     ("rubric_association[purpose]" . "grading")
                     ("rubric_association[use_for_grading]" . ,(funcall bool use-for-grading))
                     ("rubric_association[hide_points]" . ,(funcall bool hide-points))
                     ("rubric_association[hide_score_total]" . ,(funcall bool hide-score-total))))
           (response
            (if existing-assoc-id
                (org-lms-canvas-request
                 (format "courses/%s/rubric_associations/%s" courseid existing-assoc-id)
                 "PUT" params)
              (org-lms-canvas-request
               (format "courses/%s/rubric_associations" courseid)
               "POST" params)))
           (assoc-id (plist-get response :id)))
      (when assoc-id
        (org-entry-put (point) "RUBRIC_ASSOCIATION_ID" (format "%s" assoc-id))
        (message "Rubric %s %s with assignment %s (association %s)"
                 rubric-id
                 (if existing-assoc-id "association updated" "associated")
                 assignment-id assoc-id))
      response))))

(provide 'org-lms-rubrics)
;;; org-lms-rubrics.el ends here
