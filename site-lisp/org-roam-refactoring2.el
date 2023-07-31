;;; org-roam-refactoring2.el --- Common commands for moving/renaming links -*- lexical-binding: t; -*-

(require 'org-roam-node)
(require 'dash)

(customize-set-variable 'org-roam-mode-sections (list #'orr-backlinks-section))

(defcustom org-roam-refactoring2-buffer-name "*org-roam-refactoring2*"
  "The name of the special org-roam-refactoring buffer"
  :group 'org-roam-refactoring
  :type 'string)

(defvar org-roam-refactoring2-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap revert-buffer] 'org-roam-refactoring-buffer-refresh)
    (general-def '(motion normal) org-roam-refactoring-mode-map
      "q" #'org-roam-refactoring-kill-buffer-ask)
    map)
  "Keymap for `org-roam-refactoring2-mode'")

(define-derived-mode org-roam-refactoring2-mode org-roam-mode "org-roam-refactoring"
  :group 'org-roam-refactoring
  (add-hook 'completion-at-point-functions 'org-roam-refactoring-completion-at-point nil 'local)
  (face-remap-add-relative 'header-line 'org-roam-header-line))

(defun org-roam-refactoring-buffer-refresh ()
  (interactive)
  (cl-assert (derived-mode-p 'org-roam-refactoring-mode))
  (save-excursion (org-roam-refactoring-render)))

(defun org-roam-refactoring-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-roam-refactoring2-mode)
    (setq-local default-directory org-roam-buffer-current-directory)
    (setq-local org-roam-directory org-roam-buffer-current-directory)
    (org-roam-buffer-set-header-line-format
     (org-roam-node-title org-roam-buffer-current-node))
    (magit-insert-section (org-roam)
      (magit-insert-heading)
      (dolist (section org-roam-mode-sections)
        (pcase section
          ((pred functionp)
           (funcall section org-roam-buffer-current-node))
          (`(,fn . ,args)
           (apply fn (cons org-roam-buffer-current-node args)))
          (_
           (user-error "Invalid `org-roam-mode-sections' specification")))))
    (run-hooks 'org-roam-buffer-postrender-functions)
    (goto-char 0)))

(cl-defun orr-backlinks-get (nodes &key unique)
  "Return the backlinks for NODE.

 When UNIQUE is nil, show all positions where references are found.
 When UNIQUE is t, limit to unique sources."
  (let* ((ids (seq--into-vector (-map #'org-roam-node-id nodes)))
         (sql (if unique
                  [:select :distinct [source dest pos properties]
                   :from links
                   :where (in dest $v1)
                   :and (= type "id")
                   :group :by source
                   :having (funcall min pos)]
                [:select [source dest pos properties]
                 :from links
                 :where (in dest $v1)
                 :and (= type "id")]))
         (backlinks (org-roam-db-query sql ids)))
    (cl-loop for backlink in backlinks
             collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
                       (org-roam-populate
                        (org-roam-backlink-create
                         :source-node (org-roam-node-create :id source-id)
                         :target-node (org-roam-node-create :id dest-id)
                         :point pos
                         :properties properties))))))

(cl-defun orr-node-insert-section (&key source-node point properties)
  "Insert section for a link from SOURCE-NODE to some other node.
The other node is normally `org-roam-buffer-current-node'.

SOURCE-NODE is an `org-roam-node' that links or references with
the other node.

POINT is a character position where the link is located in
SOURCE-NODE's file.

PROPERTIES (a plist) contains additional information about the
link.

Despite the name, this function actually inserts 2 sections at
the same time:

1. `org-roam-node-section' for a heading that describes
   SOURCE-NODE. Acts as a parent section of the following one.

2. `org-roam-preview-section' for a preview content that comes
   from SOURCE-NODE's file for the link (that references the
   other node) at POINT. Acts a child section of the previous
   one."
  (magit-insert-section section (org-roam-node-section)
    (let ((outline (if-let ((outline (plist-get properties :outline)))
                       (mapconcat #'org-link-display-format outline " > ")
                     "Top")))
      (insert (concat (propertize (org-roam-node-title source-node)
                                  'font-lock-face 'org-roam-title)
                      (format " (%s)"
                              (propertize outline 'font-lock-face 'org-roam-olp)))))
    (magit-insert-heading)
    (oset section node source-node)
    (magit-insert-section section (org-roam-preview-section)
      (insert (org-roam-fontify-like-in-org-mode
               (org-roam-preview-get-contents (org-roam-node-file source-node) point))
              "\n")
      (oset section file (org-roam-node-file source-node))
      (oset section point point)
      (insert ?\n))))

(cl-defun orr-backlinks-section (nodes &key (unique nil) (show-backlink-p nil))
  "The backlinks section for NODES.

When UNIQUE is nil, show all positions where references are found.
When UNIQUE is t, limit to unique sources.

When SHOW-BACKLINK-P is not null, only show backlinks for which
this predicate is not nil."
  (when-let ((backlinks
              (seq-sort #'org-roam-backlinks-sort
                        (orr-backlinks-get nodes :unique unique))))
    (magit-insert-section (org-roam-backlinks)
      (magit-insert-heading "Backlinks:")
      (dolist (backlink backlinks)
        (when (or (null show-backlink-p)
                  (and (not (null show-backlink-p))
                       (funcall show-backlink-p backlink)))
          (orr-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink))))
      (insert ?\n))))

(defun my/org-roam-node-read-multiple (&optional initial-input filter-fn sort-fn require-match prompt)
  "Read and return an `org-roam-node'.
    INITIAL-INPUT is the initial minibuffer prompt value.
    FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
    and when nil is returned the node will be filtered out.
    SORT-FN is a function to sort nodes. See `org-roam-node-read-sort-by-file-mtime'
    for an example sort function.
    If REQUIRE-MATCH, the minibuffer prompt will require a match.
    PROMPT is a string to show at the beginning of the mini-buffer, defaulting to \"Node: \""
  (let* ((nodes (org-roam-node-read--completions filter-fn sort-fn))
         (prompt (or prompt "Node: "))
         (matches (completing-read-multiple
                   prompt
                   (lambda (string pred action)
                     (if (eq action 'metadata)
                         `(metadata
                           ;; Preserve sorting in the completion UI if a sort-fn is used
                           ,@(when sort-fn
                               '((display-sort-function . identity)
                                 (cycle-sort-function . identity)))
                           (annotation-function
                            . ,(lambda (title)
                                 (funcall org-roam-node-annotation-function
                                          (get-text-property 0 'node title))))
                           (category . org-roam-node))
                       (complete-with-action action nodes string pred)))
                   nil require-match initial-input 'org-roam-node-history)))
    (dolist (m matches)
      (message "%s" m))
    (-map (lambda (m) (assoc m nodes)) matches)))

(defun org-roam-refactor2 ()
  "Easily update all the backlinks for a given node."
  (interactive)
  (let* ((link (orr/get-link-at-point))
         (parent-id (orr/get-id-from-parent-section))
         (node (cond
                (link (let* ((initial-input (when link (orr/org-link-get-description link)))
                             (node (org-roam-node-read initial-input nil nil 'require-match)))
                        node))
                (parent-id (org-roam-node-from-id parent-id))
                (t (let* ((node (org-roam-node-read nil nil nil 'require-match)))
                     node))))
         (new-buffer (get-buffer-create org-roam-refactoring2-buffer-name))
         (id (org-roam-node-id node)))
    (switch-to-buffer new-buffer)
    (setq org-roam-buffer-current-node node
          org-roam-buffer-current-directory org-roam-directory)
    (org-roam-refactoring-render)
    (my/add-link-overlay-with-id id)
    (add-hook 'kill-buffer-hook #'org-roam-buffer--persistent-cleanup-h nil t)))

(defun org-roam-refactor3 ()
  "Easily update all the backlinks for a given node/nodes."
  (interactive)
  (let* ((link (orr/get-link-at-point))
         (parent-id (orr/get-id-from-parent-section))
         (nodes (cond
                 (link (let* ((initial-input (when link (orr/org-link-get-description link)))
                              (nodes (my/org-roam-node-read-multiple initial-input nil nil 'require-match)))
                         nodes))
                 (parent-id (list (org-roam-node-from-id parent-id)))
                 (t (let* ((nodes (my/org-roam-node-read-multiple nil nil nil 'require-match)))
                      nodes))))
         (nodes (-map #'cdr nodes))
         (new-buffer (get-buffer-create org-roam-refactoring2-buffer-name))
         (ids (-map #'org-roam-node-id nodes)))
    (switch-to-buffer new-buffer)
    (setq org-roam-buffer-current-nodes nodes
          org-roam-buffer-current-directory org-roam-directory)
    (org-roam-refactoring2-render)
    (-map #'my/add-link-overlay-with-id ids)))

(defun org-roam-refactoring2-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-roam-refactoring2-mode)
    (setq-local default-directory org-roam-buffer-current-directory)
    (setq-local org-roam-directory org-roam-buffer-current-directory)
    (org-roam-buffer-set-header-line-format
     (mapconcat #'org-roam-node-title org-roam-buffer-current-nodes ", "))
    (magit-insert-section (org-roam)
      (magit-insert-heading)
      (dolist (section org-roam-mode-sections)
        (pcase section
          ((pred functionp)
           (funcall section org-roam-buffer-current-nodes))
          (`(,fn . ,args)
           (apply fn (cons org-roam-buffer-current-nodes args)))
          (_
           (user-error "Invalid `org-roam-mode-sections' specification")))))
    (run-hooks 'org-roam-buffer-postrender-functions)
    (goto-char 0)))

(defun orr-preview-function ()
  "Return the preview content at point.

This function returns the all contents under the current
headline, up to the next headline."
  (let ((beg (save-excursion
               (if (org-current-level)
                   (org-back-to-heading-or-point-min t)
                 (org-roam-end-of-meta-data t))
               (point)))
        (end (save-excursion
               (org-next-visible-heading 1)
               (point))))
    (string-trim (buffer-substring-no-properties beg end))))

(customize-set-variable 'org-roam-preview-function #'orr-preview-function)
