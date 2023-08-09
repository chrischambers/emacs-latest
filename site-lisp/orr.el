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

(defun my/orrg ()
  "Helper for quickly printing the node for an id"
  (interactive)
  (let* ((pair (bounds-of-thing-at-point 'uuid))
         (start (car pair))
         (end (cdr pair))
         (id (buffer-substring-no-properties start end))
         (node (org-roam-node-from-id id)))
    (message "%s" node)
    node))

(defun my/orrg2 ()
  "Helper for quickly navigating to the node for an id"
  (interactive)
  (find-file (org-roam-node-file (my/orrg))))

(leader ";" 'my/orrg)
(leader "'" 'my/orrg2)

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
  properties)

;; We can then group by container pos, sort by source node pos, and iterate
(defclass orr-backlink-container-section (magit-section)
  ((keymap :initform 'orr/mode-map)
   (backlinks :initform nil)
   (target-node :initform nil))
  )

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
                [:select [l:source l:dest l:pos l:properties l:heading-pos l:heading-title f:title f:file]
                 :from [(as links l) (as files f) nodes]
                 :where (in dest $v1)
                 :and (= l:source nodes:id)
                 :and (= nodes:file f:file)
                 :and (= l:type "id")
                 :order :by [f:title l:heading-pos]]))
         (backlinks (org-roam-db-query sql ids)))
    (dolist (link backlinks)
      (cl-destructuring-bind (source-id dest-id _ _ _ _ _ _) link
        (set-add node-ids source-id)
        (set-add node-ids dest-id)))
    (let ((mapping (my/populate-nodes-from-ids (set-members node-ids))))
      (cl-loop for link in backlinks collect
               (cl-destructuring-bind (source-id dest-id pos properties heading-pos heading-title file-title file) link
                 (orr-backlink-create
                  :file-title file-title
                  :file file
                  :source-node (gethash source-id mapping)
                  :target-node (gethash dest-id mapping)
                  :point pos
                  :heading-pos heading-pos
                  :heading-title heading-title
                  :properties properties))))))

(defun org-roam-refactor4 ()
  "Easily update all the backlinks for a given node/nodes."
  (interactive)
  (let* ((link (orr/get-link-at-point))
         (parent-id (orr/get-id-from-parent-section))
         (nodes (cond
                 (link (let* ((initial-input (when link (orr/org-link-get-description link)))
                              (nodes (my/org-roam-node-read-multiple initial-input nil nil 'require-match)))
                         (-map #'cdr nodes)))
                 (parent-id (list (org-roam-node-from-id parent-id)))
                 (t (let* ((nodes (my/org-roam-node-read-multiple nil nil nil 'require-match)))
                      (-map #'cdr nodes)))))
         (new-buffer (get-buffer-create org-roam-refactoring2-buffer-name))
         (ids (-map #'org-roam-node-id nodes)))
    (switch-to-buffer new-buffer)
    (setq org-roam-buffer-current-nodes nodes
          org-roam-buffer-current-directory org-roam-directory)
    (org-roam-refactoring4-render)
    (-map #'my/add-link-overlay-with-id ids)))

(customize-set-variable 'orr4-mode-sections (list #'orr4-backlinks-section))

(defun org-roam-refactoring4-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-roam-refactoring2-mode)
    (setq-local default-directory org-roam-buffer-current-directory)
    (setq-local org-roam-directory org-roam-buffer-current-directory)
    (org-roam-buffer-set-header-line-format
     (mapconcat #'org-roam-node-title org-roam-buffer-current-nodes ", "))
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

(defun orr/backlinks-grouped (nodes &optional unique)
  (let* ((backlinks (orr/get-backlinks nodes :unique unique))
         (reduced (-reduce-r-from
                   #'orr--accumulate-backlinks
                   `((,(orr-file-create :title "foo" :path "~/foo.txt") ((1 (foo))))) backlinks))
         (result (-drop-last 1 reduced)))
    result))

;; (inspect (orr/backlinks-grouped (list (org-roam-node-from-id "608130E1-5C55-41CF-AAC8-AB1DE5ABC788"))))

(cl-defun orr4-backlinks-section (nodes &key (unique nil) (show-backlink-p nil))
  "The backlinks section for NODES.

When UNIQUE is nil, show all positions where references are found.
When UNIQUE is t, limit to unique sources.

When SHOW-BACKLINK-P is not null, only show backlinks for which
this predicate is not nil."
  (when-let ((grouped-backlinks (orr/backlinks-grouped nodes unique)))
    (inspect grouped-backlinks t)
    (magit-insert-section (org-roam-backlinks)
      (magit-insert-heading "Backlinks:")
      (dolist (group grouped-backlinks)
        (orr4-build-sections group)))
      (insert ?\n)))

(defun orr-preview-function ()
  "Return the preview content at point.

This function returns the all contents under the current
headline, up to the next headline."
  (let ((beg (save-excursion
               (org-roam-end-of-meta-data t)
               (point)))
        (end (save-excursion
               (org-next-visible-heading 1)
               (point))))
    (string-trim (buffer-substring-no-properties beg end))))

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

(defun orr4-build-sections (group)
  (pcase group
    (`(,file ,matches)
     (magit-insert-section section (orr4-file-section)
       (let ((path (orr-file-path file))
             (title (orr-file-title file)))
         (insert (propertize title 'font-lock-face 'org-roam-title))
         (oset section file path)
         (insert ?\n)
         (dolist (match matches)
           (pcase match
             (`(,heading-pos ,backlinks)
              (magit-insert-section section (orr4-heading-section)
                (oset section heading-pos heading-pos)
                (oset section file path)
                (let* ((properties (orr-backlink-properties (car backlinks)))
                       (outline (if-let ((outline (plist-get properties :outline)))
                                    (mapconcat #'org-link-display-format outline " > ")
                                  "Top")))
                  (insert (concat (propertize (org-roam-fontify-like-in-org-mode
                                               (format "  %s" (orr-backlink-heading-title (car backlinks)))))
                                  (format " (%s)"
                                          (propertize outline 'font-lock-face 'org-roam-olp)))))
                ;; (insert (concat (propertize (format "  %s" (orr-backlink-heading-title (car backlinks)))
                ;;                             'font-lock-face 'org-roam-header-line)
                ;;                 (format " (%s)"
                ;;                         (propertize outline 'font-lock-face 'org-roam-olp)))))
                (magit-insert-heading)
                (insert ?\n)
                (let* ((backlink (car backlinks))
                       (source-node (orr-backlink-source-node backlink))
                       (point (orr-backlink-point backlink)))
                  (magit-insert-section section (orr4-preview-section)
                    (insert (org-roam-fontify-like-in-org-mode
                             (orr-preview-get-contents
                              (org-roam-node-file source-node)
                              heading-pos))
                            "\n")
                    ;; (dolist (backlink backlinks)
                    ;;   )
                    (oset section file (org-roam-node-file source-node))
                    (oset section point point)
                    (insert ?\n)))
                ))))
         )))))

(defvar orr/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [C-return]  'org-roam-buffer-visit-thing)
    (define-key map (kbd "C-m") 'org-roam-buffer-visit-thing)
    (define-key map [remap revert-buffer] 'org-roam-buffer-refresh)
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
   (heading-pos :initform nil)))

(defvar orr/preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-preview-map)
    (define-key map [remap org-roam-buffer-visit-thing] 'orr/preview-visit)
    (define-key map (kbd "SPC") nil)
    map))

;; TODO
(defclass orr4-preview-section (magit-section)
  ((keymap :initform 'orr/preview-map)
   (file :initform nil)
   (point :initform nil))
  "A `magit-section' used by `org-roam-mode' to contain preview content.
The preview content comes from FILE, and the link as at POINT.")

(defun orr/node-at-point (&optional assert)
  "Return the node at point.
If ASSERT, throw an error if there is no node at point.
This function also returns the node if it has yet to be cached in the
database. In this scenario, only expect `:id' and `:point' to be
populated."
  (or (magit-section-case
        (org-roam-node-section (oref it node))
        (org-roam-preview-section (save-excursion
                                    (magit-section-up)
                                    (org-roam-node-at-point)))
        (t (org-with-wide-buffer
            (while (not (or (org-roam-db-node-p)
                            (bobp)
                            (eq (funcall outline-level)
                                (save-excursion
                                  (org-roam-up-heading-or-point-min)
                                  (funcall outline-level)))))
              (org-roam-up-heading-or-point-min))
            (when-let ((id (org-id-get)))
              (org-roam-populate
               (org-roam-node-create
                :id id
                :point (point)))))))
      (and assert (user-error "No node at point"))))

(advice-add #'org-roam-node-at-point :override #'orr/node-at-point)

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

(defun orr/preview-visit (file point &optional other-window)
  "Visit FILE at POINT and return the visited buffer.
With OTHER-WINDOW non-nil do so in another window.
In interactive calls OTHER-WINDOW is set with
`universal-argument'."
  (interactive (list (org-roam-buffer-file-at-point 'assert)
                     (oref (magit-current-section) point)
                     current-prefix-arg))
  (orr--visit file point other-window))
