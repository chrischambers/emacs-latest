;;; orr.el --- Foo -*- lexical-binding: t; -*-

(defvar orr-alias-map nil "Cache for alias mapping")

(defun orr/alias-map ()
  "Return all distinct titles and aliases in the Org-roam database."
  (let* ((results (org-roam-db-query
                   [:select [title id] :from nodes
                    :union :select [alias node-id] :from aliases]))
        (map (make-hash-table :test #'equal :size (length results))))
    (dolist (i results)
      (puthash (car i) (cadr i) map))
    map))
(setq orr-alias-map (orr/alias-map))

(defun orr/lookup (title-or-alias)
  (gethash title-or-alias orr-alias-map))

(defun my/inspect-node-for-id-at-point ()
  "Helper for quickly printing the node for an id"
  (interactive)
  (let* ((pair (bounds-of-thing-at-point 'uuid))
         (start (car pair))
         (end (cdr pair))
         (id (buffer-substring-no-properties start end))
         (node (org-roam-node-from-id id)))
    (inspect node)
    node))

(defun my/find-node-for-id-at-point ()
  "Helper for quickly navigating to the node for an id"
  (interactive)
  (find-file (org-roam-node-file (my/inspect-node-for-id-at-point))))

(leader "x" '(:ignore t :which-key "extras"))
(leader "xi" 'my/inspect-node-for-id-at-point)
(leader "xf" 'my/find-node-for-id-at-point)

;; backlink
;; - destination node
;; - source node
;; - source node pos
;; - container pos

;; ALTERNATIVELY, instead of calculating the container section on the fly, you
;; could cache them in the org-roam.db. This could potentially make previewing a
;; bit faster?
(cl-defstruct (orr-backlink (:constructor orr-backlink-create)
                            (:copier nil))
  file-title file
  source-node target-node
  point
  heading-pos heading-title
  link-description
  properties)

;; We can then group by container pos, sort by source node pos, and iterate
(defclass orr-backlink-container-section (magit-section)
  ((keymap :initform 'orr/mode-map)
   (backlinks :initform nil)
   (target-node :initform nil)))

(cl-defun orr/get-backlinks (nodes &key unique)
  "Return the backlinks for NODES.

 When UNIQUE is nil, show all positions where references are found.
 When UNIQUE is t, limit to unique sources."
  (let* ((ids (seq--into-vector (-map #'org-roam-node-id nodes)))
         (node-ids (make-set))
         (sql (if unique
                  ;; TODO: Not done yet
                  [:select :distinct [source dest pos properties heading-pos]
                   :from links
                   :where (in dest $v1)
                   :and (= type "id")
                   :group :by source
                   :having (funcall min pos)]
                [:select [l:source l:dest l:pos l:properties l:heading-pos l:heading-title l:link-description f:title f:file]
                 :from [(as links l) (as files f) nodes]
                 :where (in dest $v1)
                 :and (= l:source nodes:id)
                 :and (= nodes:file f:file)
                 :and (= l:type "id")
                 :order :by [f:title l:heading-pos]]))
         (backlinks (org-roam-db-query sql ids)))
    (dolist (link backlinks)
      (cl-destructuring-bind (source-id dest-id _ _ _ _ _ _ _) link
        (set-add node-ids source-id)
        (set-add node-ids dest-id)))
    (let ((mapping (my/populate-nodes-from-ids (set-members node-ids))))
      (cl-loop for link in backlinks collect
               (cl-destructuring-bind (source-id
                                       dest-id
                                       pos
                                       properties
                                       heading-pos
                                       heading-title
                                       link-description
                                       file-title
                                       file) link
                 (orr-backlink-create
                  :file-title file-title
                  :file file
                  :source-node (gethash source-id mapping)
                  :target-node (gethash dest-id mapping)
                  :point pos
                  :heading-pos heading-pos
                  :heading-title heading-title
                  :link-description link-description
                  :properties properties))))))

(defun org-roam-refactor4-main (nodes)
  (let ((new-buffer (get-buffer-create org-roam-refactoring2-buffer-name))
        (ids (-map #'org-roam-node-id nodes)))
    (switch-to-buffer new-buffer)
    (setq org-roam-buffer-current-nodes nodes
          org-roam-buffer-current-directory org-roam-directory)
    (org-roam-refactoring4-render)
    (-map #'my/add-link-overlay-with-id ids)))

(defvar orr-backlink-filter nil
  "If set (buffer-local?), should be a predicate taking a single backlink.")
(put 'orr-backlink-filter 'permanent-local t)

(defun org-roam-refactor4 ()
  "Easily update all the backlinks for a given node/nodes."
  (interactive)
  (let* ((link (orr/get-link-at-point))
         (parent-id (orr/get-id-from-parent-section))
         (nodes
          (cond
           (link
            (let* ((initial-input (when link (orr/org-link-get-description link)))
                   (nodes (my/org-roam-node-read-multiple initial-input
                                                          nil
                                                          nil
                                                          'require-match)))
              (-map #'cdr nodes)))
           (parent-id (list (org-roam-node-from-id parent-id)))
           (t
            (let* ((nodes (my/org-roam-node-read-multiple nil
                                                          nil
                                                          nil
                                                          'require-match)))
              (-map #'cdr nodes))))))
    (org-roam-refactor4-main nodes)))

(customize-set-variable 'orr4-mode-sections (list #'orr4-backlinks-section))

(defun org-roam-refactoring4-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-roam-refactoring2-mode)
    (setq-local default-directory org-roam-buffer-current-directory)
    (setq-local org-roam-directory org-roam-buffer-current-directory)
    (org-roam-buffer-set-header-line-format
     (mapconcat
      (lambda (i) (->> i org-roam-node-title (format "\"%s\"")))
      org-roam-buffer-current-nodes
      ", "))
    (insert (propertize
             (format "Filter: =%s=" orr-backlink-filter)
             'font-lock-face 'org-verbatim))
    (insert ?\n)
    (magit-insert-section (org-roam)
      (magit-insert-heading)
      (dolist (section orr4-mode-sections)
        (pcase section
          ((pred functionp)
           (funcall section org-roam-buffer-current-nodes))
          (`(,fn . ,args)
           (apply fn (cons org-roam-buffer-current-nodes args)))
          (_
           (user-error "Invalid `orr4-mode-sections' specification")))))
    (run-hooks 'org-roam-buffer-postrender-functions)
    (goto-char 0)))

(cl-defstruct (orr-file (:constructor orr-file-create)
                        (:copier nil))
  title path)

(defun orr--accumulate-backlinks (bl acc)
  (let* ((file (orr-file-create :title (orr-backlink-file-title bl)
                                :path (orr-backlink-file bl)))
         (new-title (orr-file-title file))
         (new-heading-pos (orr-backlink-heading-pos bl)))
    (pcase acc
      (`((,file2 ((,current-pos ,backlinks) . ,positions)) . ,rest)
       (if (string= (orr-file-title file2) new-title)
           (if (= current-pos new-heading-pos)
               (cons (list file (cons (list current-pos (cons bl backlinks)) (cdadar acc))) rest)
             (cons (list file (cons (list new-heading-pos (list bl)) (cadar acc))) rest))
         (cons (list file (list (list new-heading-pos (list bl)))) acc))))))

(defun orr/filter-backlinks (backlinks &optional show-backlink-p)
  (if show-backlink-p
      (-filter show-backlink-p backlinks)
      backlinks))

(defun orr/backlinks-grouped (nodes &optional unique show-backlink-p)
  (let* ((backlinks (orr/get-backlinks nodes :unique unique))
         (backlinks (orr/filter-backlinks backlinks show-backlink-p))
         (reduced (-reduce-r-from
                   #'orr--accumulate-backlinks
                   `((,(orr-file-create :title "foo" :path "~/foo.txt") ((1 (foo)))))
                   backlinks))
         (result (-drop-last 1 reduced)))
    result))

;; (inspect (orr/backlinks-grouped (list (org-roam-node-from-id "608130E1-5C55-41CF-AAC8-AB1DE5ABC788"))))

(defun orr/-make-bl-p (f re)
  (lambda (bl) (s-matches? re (funcall f bl))))

(defun orr-backlink-filter-active (f)
  (pcase f
    (`(heading ,text) (orr/-make-bl-p #'orr-backlink-heading-title text))
    (`(link-description ,text) (orr/-make-bl-p #'orr-backlink-link-description text))
    (`(file-title ,text) (orr/-make-bl-p #'orr-backlink-file-title text))
    (f f)))

(cl-defun orr4-backlinks-section (nodes &key (unique nil) (show-backlink-p nil))
  "The backlinks section for NODES.

When UNIQUE is nil, show all positions where references are found.
When UNIQUE is t, limit to unique sources.

When SHOW-BACKLINK-P is not null, only show backlinks for which
this predicate is not nil."
  (let ((show-backlink-p (or show-backlink-p (orr-backlink-filter-active orr-backlink-filter))))
    (when-let ((grouped-backlinks (orr/backlinks-grouped nodes unique show-backlink-p)))
      ;; (inspect grouped-backlinks t)
      ;;(magit-insert-section (orr-backlink-container)
      (magit-insert-section (org-roam-backlinks)
        (magit-insert-heading "Backlinks:")
        (dolist (group grouped-backlinks)
          (orr4-build-sections group)))
      (insert ?\n)
      (org-latex-preview '(16))
      (org-toggle-inline-images))))

(defun orr-preview-function ()
  "Return the preview content at point.

This function returns all contents under the current headline, up
to the next headline."
  (let ((beg (save-excursion
               (org-roam-end-of-meta-data t)
               (point)))
        (end (save-excursion
               (org-next-visible-heading 1)
               (point))))
    (s-trim-right (buffer-substring-no-properties beg end))))

(defun orr-preview-get-contents (file pt)
  "Get preview content for FILE at PT."
  (save-excursion
    (org-roam-with-temp-buffer file
      (org-with-wide-buffer
       (goto-char pt)
       (let ((s (funcall #'orr-preview-function)))
         (dolist (fn org-roam-preview-postprocess-functions)
           (setq s (funcall fn s)))
         s)))))

(setq orr-relative-link-re
      "\\[\\[\\(\\.?\\./\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]")

(defun orr-fontify-like-in-org-mode (s path)
  (let ((dir (f-slash (f-parent path))))
    (with-temp-buffer
      (insert (s-replace-regexp orr-relative-link-re (format "[[%s\\1]]" dir) s))
      (let ((org-ref-buffer-hacked t))
        (org-mode)
        (setq-local org-fold-core-style 'overlays)
        (font-lock-ensure)
        (buffer-string)))))

(defun orr4-build-sections (group)
  (pcase group
    (`(,file ,matches)
     (magit-insert-section section (orr4-file-section)
       (let ((path (orr-file-path file))
             (title (orr-file-title file)))
         (oset section file path)
         (magit-insert-heading (propertize title 'font-lock-face 'org-roam-title))
         ;; (insert ?\n)
         (dolist (match matches)
           (pcase match
             (`(,heading-pos ,backlinks)
              (magit-insert-section section (orr4-heading-section)
                (let* ((first-backlink (car backlinks))
                       (properties (orr-backlink-properties first-backlink))
                       (source-node (orr-backlink-source-node first-backlink))
                       (points (-map #'orr-backlink-point backlinks))
                       (outline (if-let ((outline (plist-get properties :outline)))
                                    (mapconcat #'org-link-display-format outline " > ")
                                  "Top")))
                  (oset section heading-pos heading-pos)
                  (oset section points points)
                  (oset section file path)
                  (insert
                   (concat
                    (propertize (org-roam-fontify-like-in-org-mode
                                 (format "  %s"
                                         (propertize (or (orr-backlink-heading-title first-backlink) "")
                                                     'font-lock-face
                                                     'org-roam-preview-heading))))
                    (format " (%s)" (propertize outline
                                                'font-lock-face
                                                'org-roam-olp))))
                  ;; (insert (concat (propertize (format "  %s" (orr-backlink-heading-title (car backlinks)))
                  ;;                             'font-lock-face 'org-roam-header-line)
                  ;;                 (format " (%s)"
                  ;;                         (propertize outline 'font-lock-face 'org-roam-olp)))))
                  (magit-insert-heading)
                  ;; (insert ?\n)
                  (magit-insert-section section (orr4-preview-section)
                    ;; (message "%s:%s" (f-filename (org-roam-node-file source-node)) (point))
                    (insert (orr-fontify-like-in-org-mode
                             (orr-preview-get-contents
                              (org-roam-node-file source-node)
                              heading-pos)
                             (org-roam-node-file source-node))
                            "\n")
                    (oset section file (org-roam-node-file source-node))
                    (oset section heading-pos heading-pos)
                    (oset section points points)
                    (insert ?\n)))
                ))))
         )))))

(defun orr-buffer-refresh ()
  (interactive)
  (org-roam-refactor4-main org-roam-buffer-current-nodes))

(defun orr--filter-clear ()
  (interactive)
  (setq-local orr-backlink-filter nil)
  (orr-buffer-refresh))

(defun orr--filter-lambda (form)
  (interactive "X")
  (setq-local orr-backlink-filter form)
  (orr-buffer-refresh))

(defun orr--filter-function (f)
  (interactive "a")
  (setq-local orr-backlink-filter f)
  (orr-buffer-refresh))

(defun orr--filter-heading (re)
  (interactive "sHeading: ")
  (setq-local orr-backlink-filter `(heading ,re))
  (orr-buffer-refresh))

(defun orr--filter-file-title (re)
  (interactive "sFile Title: ")
  (setq-local orr-backlink-filter `(file-title ,re))
  (orr-buffer-refresh))

(defun orr--filter-link-description (re)
  (interactive "sLink Description: ")
  (setq-local orr-backlink-filter `(link-description ,re))
  (orr-buffer-refresh))

(transient-define-prefix orr-filter ()
    ["Filter:\n"
     ("c" "clear" orr--filter-clear)
     ("f" "file title" orr--filter-file-title)
     ("F" "function" orr--filter-function)
     ("h" "heading" orr--filter-heading)
     ("l" "link description" orr--filter-link-description)
     ("L" "lambda" orr--filter-lambda)])

(defun orr--replace-link-destination ()
  (interactive)
  (when-let ((section (magit-current-section))
             (file (oref section file))
             (points (oref section points))
             (buffer (or (find-buffer-visiting file)
                         (find-file-noselect file))))
    (save-excursion
      (with-current-buffer buffer
        (cl-loop for pos in points
                 do
                 (goto-char pos)
                 (org-roam-refactor-replace-link-destination))))))

(defun orr--replace-link-description ()
  (interactive)
  (message "not done yet!"))

(transient-define-prefix orr-refactor ()
    ["Refactor:\n"
     ("l" "location" orr--replace-link-destination)
     ("d" "description" orr--replace-link-description)])

(defvar orr/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map (kbd "f") 'orr-filter)
    (define-key map (kbd "r") 'orr-refactor)
    (define-key map [C-return]  'org-roam-buffer-visit-thing)
    (define-key map (kbd "C-m") 'org-roam-buffer-visit-thing)
    (define-key map [remap revert-buffer] 'orr-buffer-refresh)
    (define-key map (kbd "SPC") nil)
    map))

(defvar orr/file-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map orr/mode-map)
    (define-key map [remap org-roam-buffer-visit-thing] 'orr/file-visit)
    map))

(defvar orr/heading-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map orr/mode-map)
    (define-key map [remap org-roam-buffer-visit-thing] 'orr/heading-visit)
    map))

(defclass orr4-file-section (magit-section)
  ((keymap :initform 'orr/file-mode-map)
   (file :initform nil)))

(defclass orr4-heading-section (magit-section)
  ((keymap :initform 'orr/heading-mode-map)
   (file :initform nil)
   (heading-pos :initform nil)
   (points :initform nil)))

(defvar orr/preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-preview-map)
    (define-key map (kbd "f") 'orr-filter)
    (define-key map (kbd "r") 'orr-refactor)
    (define-key map [remap org-roam-buffer-visit-thing] 'orr/preview-visit)
    (define-key map (kbd "SPC") nil)
    map))

(defclass orr4-preview-section (magit-section)
  ((keymap :initform 'orr/preview-map)
   (file :initform nil)
   (heading-pos :initform nil)
   (points :initform nil)))

(defun orr/buffer-file-at-point (&optional assert)
  "Return the file at point in the current `org-roam-mode' based buffer.
If ASSERT, throw an error."
  (if-let ((file (magit-section-case
                   (org-roam-node-section (org-roam-node-file (oref it node)))
                   (org-roam-grep-section (oref it file))
                   (org-roam-preview-section (oref it file))
                   (orr4-file-section (oref it file))
                   (orr4-heading-section (oref it file))
                   (orr4-preview-section (oref it file))
                   (t (cl-assert (derived-mode-p 'org-roam-mode))))))
      file
    (when assert
      (user-error "No file at point"))))
(advice-add #'org-roam-buffer-file-at-point :override #'orr/buffer-file-at-point)

;; ------------------------------------------------------------------------------
(defun orr--visit (file point &optional other-window)
  (let ((buf (find-file-noselect file))
        (display-buffer-fn (if other-window
                               #'switch-to-buffer-other-window
                             #'pop-to-buffer-same-window)))
    (funcall display-buffer-fn buf)
    (with-current-buffer buf
      (widen)
      (goto-char point))
    (when (org-invisible-p) (org-show-context))
    buf))

(defun orr/file-visit (file &optional other-window)
  (interactive (list (org-roam-buffer-file-at-point 'assert)
                     current-prefix-arg))
  (orr--visit file 1 other-window))

(defun orr/heading-visit (file heading-pos &optional other-window)
  (interactive (list (org-roam-buffer-file-at-point 'assert)
                     (oref (magit-current-section) heading-pos)
                     current-prefix-arg))
  (orr--visit file heading-pos other-window))

(defun orr/preview-visit (file points &optional other-window)
  (interactive (list (org-roam-buffer-file-at-point 'assert)
                     (oref (magit-current-section) points)
                     current-prefix-arg))
  (let ((point (orr--nearest-point-to (point) points)))
    (orr--visit file point other-window)))

;; (inspect (orr/get-backlinks org-roam-buffer-current-nodes))

(local-leader org-roam-refactoring2-mode-map
  "R" '(orr--filter-clear :which-key "reset filters"))

(defun flibbity ()
  (interactive)
  (let* ((current-section (magit-current-section))
         (heading-pos (oref current-section heading-pos))
         (points (oref current-section points))
         ;; WRONG: This is the start of the block, not the start of the heading
         (actual-heading-pos (save-excursion (magit-section-backward) (point)))
         (new-points (-map (lambda (p) (+ p (- heading-pos) actual-heading-pos)) points)))
    (message "%s" new-points)
    (goto-char (car new-points))))

;; Maybe this approach is flawed: all we need to do is divide the preview area
;; into n block ranges, where n is the amount of points, such that each block
;; range maps to one point?

(defun orr--nearest-point-to (point points)
  (let* ((differences (-map (-compose #'abs (-partial #'- point)) points))
         (mapping (-zip-pair differences points))
         (map (make-hash-table)))
    (pcase-dolist (`(,diff . ,point) mapping) (puthash diff point map))
    (gethash (apply #'min differences) map)))

(defun org-roam-refactor-current-node ()
  (interactive)
  (org-roam-refactor4-main (list (org-roam-node-at-point))))

(local-leader org-mode-map
  "wB"   '(org-roam-refactor-current-node :which-key "refactor"))
