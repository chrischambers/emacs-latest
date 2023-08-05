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
