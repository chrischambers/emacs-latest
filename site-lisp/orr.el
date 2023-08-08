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
  source-node target-node
  point headline-pos properties)

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
                  [:select :distinct [source dest pos properties heading-pos]
                   :from links
                   :where (in dest $v1)
                   :and (= type "id")
                   :group :by source
                   :having (funcall min pos)]
                [:select [source dest pos properties heading-pos]
                 :from links
                 :where (in dest $v1)
                 :and (= type "id")]))
         (backlinks (org-roam-db-query sql ids)))
    (dolist (link backlinks)
      (cl-destructuring-bind (source-id dest-id _ _ _) link
        (set-add node-ids source-id)
        (set-add node-ids dest-id)))
    (let ((mapping (my/populate-nodes-from-ids (set-members node-ids))))
      (cl-loop for link in backlinks collect
               (cl-destructuring-bind (source-id dest-id pos properties headline-pos) link
                 (orr-backlink-create
                  :source-node (gethash source-id mapping)
                  :target-node (gethash dest-id mapping)
                  :point pos
                  :headline-pos headline-pos
                  :properties properties))))))
