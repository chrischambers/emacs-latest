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
  ((keymap :initform 'org-roam-node-map)
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
                 :and (= l:type "id")]))
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

(defun orr--sort-alist (x y)
  (pcase (cons x y)
    (`(((,file1 . ,pos1) . ,nodes1) . ((,file2 . ,pos2) . ,nodes2))
     (let ((f1 (file-name-base file1))
           (f2 (file-name-base file2)))
       (if (string= f1 f2) (< pos1 pos2) (string< f1 f2))))))

(defun orr/backlinks-grouped-sorted (nodes &optional unique)
  (let* ((set-backlinks (orr/get-backlinks nodes))
         (grouped
          (-group-by (lambda (bl)
                       (let ((node (orr-backlink-source-node bl)))
                         (cons (org-roam-node-file node) (orr-backlink-heading-pos bl))))
                     set-backlinks))
         (sorted (sort grouped #'orr--sort-alist)))
    sorted))

;; (inspect (orr/backlinks-grouped-sorted (list (org-roam-node-from-id "608130E1-5C55-41CF-AAC8-AB1DE5ABC788"))))

(cl-defun orr4-backlinks-section (nodes &key (unique nil) (show-backlink-p nil))
  "The backlinks section for NODES.

When UNIQUE is nil, show all positions where references are found.
When UNIQUE is t, limit to unique sources.

When SHOW-BACKLINK-P is not null, only show backlinks for which
this predicate is not nil."
  (when-let ((grouped-backlinks (orr/backlinks-grouped-sorted nodes unique)))
    (magit-insert-section (org-roam-backlinks)
      (magit-insert-heading "Backlinks:")
      (dolist (group grouped-backlinks)
        (pcase group
          (`((,file . ,heading-pos) . ,backlinks)
           (orr4-build-sections file heading-pos backlinks)
           ))))
      (insert ?\n)))

(defun orr4-build-sections (file heading-pos backlinks)
  (magit-insert-section section (orr4-file-section)
      (insert (propertize (orr-backlink-file-title (car backlinks)) 'font-lock-face 'org-roam-title))
      (oset section file file)
      (insert ?\n)
      (magit-insert-section section (orr4-heading-section)
      (oset section heading-pos heading-pos)
        (let* ((properties (orr-backlink-properties (car backlinks)))
               (outline (if-let ((outline (plist-get properties :outline)))
                            (mapconcat #'org-link-display-format outline " > ")
                          "Top")))
      (insert (concat (propertize (format "  %s" (orr-backlink-heading-title (car backlinks)))
                                  'font-lock-face 'org-roam-header-line)
                      (format " (%s)"
                              (propertize outline 'font-lock-face 'org-roam-olp)))))
        (magit-insert-heading)
    ;; (magit-insert-section section (orr4-preview-section)
    ;;   (insert (org-roam-fontify-like-in-org-mode
    ;;            (org-roam-preview-get-contents (org-roam-node-file source-node) point))
    ;;           "\n")
    ;;   (oset section file (org-roam-node-file source-node))
    ;;   (oset section point point)
    ;;   (insert ?\n))
    )))

(defclass orr4-file-section (magit-section)
  ((keymap :initform 'org-roam-node-map)
   (file :initform nil)))

(defclass orr4-heading-section (magit-section)
  ((keymap :initform 'org-roam-node-map)
   (heading-pos :initform nil)))

;; TODO
(defclass orr4-preview-section (magit-section)
  ((keymap :initform 'org-roam-preview-map)
   (file :initform nil)
   (point :initform nil))
  "A `magit-section' used by `org-roam-mode' to contain preview content.
The preview content comes from FILE, and the link as at POINT.")
