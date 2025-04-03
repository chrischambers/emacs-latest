;;; cljh.el --- -*- lexical-binding: t; -*-

;; Utility Functions:
;; --------------------------------------------------------------------------
(defun cljh-node-display (node)
  (when node
    (substring-no-properties (treesit-node-text node))))

(defun cljh-str-lit-display (node)
  (when node
    (->> node
         treesit-node-text
         substring-no-properties
         (s-chop-left 1)
         (s-chop-right 1))))

(defun cljh-node-info (node)
  (when node
    (list
     (treesit-node-start node)
     (treesit-node-end node)
     (cljh-node-display node)
     node)))

(defun cljh-corresponding-test-name (name)
  (format "%s-test" name))

(defun cljh-select-when-car (f xs)
  (-map #'cdr (-filter (lambda (p) (funcall f (car p))) xs)))

(defun cljh-select-when-car-eql (sym xs)
  (cljh-select-when-car (lambda (x) (eql x sym)) xs))

(defun cljh-node-type-equal? (node type)
  (equal (treesit-node-type node) type))

(defun cljh-symbol? (node)
  (cljh-node-type-equal? node "sym_lit"))

(defun cljh-str? (node)
  (cljh-node-type-equal? node "str_lit"))

(defun cljh-vector? (node)
  (cljh-node-type-equal? node "vec_lit"))

(defun cljh-list? (node)
  (cljh-node-type-equal? node "list_lit"))

(defun cljh-direct-child? (node)
  (lambda (n) (member n (treesit-children-node node))))

(defun cljh-filter-for-type (xs type)
  (-filter (lambda (n) (cljh-node-type-equal? n type)) xs))

(defun cljh-filter-children-for-type (node type)
  (cljh-filter-for-type (treesit-node-children node) type))

;; Node Operations:
;; --------------------------------------------------------------------------
(defun cljh-def-node? (node)
  (when-let* ((node (treesit-node-child node 1))
              (name (treesit-node-child-by-field-name node "name")))
    (when (cljh-symbol? node)
      (string-match-p "^.?def$" (treesit-node-text name)))))

(defun cljh-defn-node? (node)
  "Includes `defn', `>defn' `defmacro', ..."
  (when-let* ((node (treesit-node-child node 1))
              (name (treesit-node-child-by-field-name node "name")))
    (when (cljh-symbol? node)
      (string-match-p "^.?def.+" (treesit-node-text name)))))

(defun cljh-arity (defn-node)
  (seq-let [_ _ &rest parts] (treesit-node-children defn-node)
    (if (equal (treesit-node-type (first parts)) "vec_lit")
        1
      (length (cljh-filter-for-type parts "list_lit")))))

(defun cljh-defn-params (node)
  (let ((children (treesit-node-children node)))
    (cljh-node-display
     (if (= (cljh-arity node) 1)
         (car (cljh-filter-for-type children "vec_lit"))
       (when-let* ((list-nodes (cljh-filter-for-type children "list_lit"))
                   (last-node (car (last list-nodes))))
         (car (cljh-filter-children-for-type last-node "vec_lit")))))))

(defun cljh-defn-docstring (node)
  (when-let ((results (cljh-filter-children-for-type node "str_lit")))
    (cljh-str-lit-display (car results))))

(defun cljh-defn-body (node)
  (when-let* ((list-nodes (cljh-filter-children-for-type node "list_lit"))
              (last-node (car (last list-nodes))))
    (if (= (cljh-arity node) 1)
        (cljh-node-display last-node)
      (when-let* ((list-nodes (cljh-filter-children-for-type last-node "list_lit"))
                  (last-node (car (last list-nodes))))
        (cljh-node-display last-node)))))

(defun cljh-def-name (node)
  (-> node
      (treesit-node-child 2)
      (treesit-node-child-by-field-name "name")
      treesit-node-text
      substring-no-properties))

;; Position Operations:
;; --------------------------------------------------------------------------
(defun cljh-def-node-at (pos)
  (interactive "d")
  (let* ((node (treesit-node-at pos))
         (parent-defn (treesit-parent-until node #'cljh-def-node? t)))
    parent-defn))

(defun cljh-defn-node-at (pos)
  (interactive "d")
  (let* ((node (treesit-node-at pos))
         (parent-defn (treesit-parent-until node #'cljh-defn-node? t)))
    parent-defn))

(defun cljh-defn-name-at (pos)
  (interactive "d")
  (let* ((node (cljh-defn-node-at pos))
         (name (cljh-def-name node)))
    (message "%s" name)
    (substring-no-properties name)))

;; Buffer Operations:
;; --------------------------------------------------------------------------
(defun cljh-test-names-in-buffer ()
  (let* ((node (treesit-buffer-root-node))
         (query '((list_lit :anchor (sym_lit name: (_) @form-name)
                            :anchor (sym_lit name: (_) @name)
                            (:equal "deftest" @form-name)) @entire-form))
         (results (-partition 3 (treesit-query-capture node query)))
         (results (-map (lambda (triple)
                          (let ((entire-form (cdr (car triple)))
                                (name-form (cdr (nth 2 triple))))
                            (list
                             (treesit-node-start entire-form)
                             (treesit-node-end entire-form)
                             (cljh-node-display name-form))))
                        results)))
    results))

(defun cljh-test-name-in-buffer? (name)
  (when-let* ((tests (cljh-test-names-in-buffer))
              (matches (-filter (lambda (xs) (equal (nth 2 xs) name)) tests))
              (result (car matches)))
    result))

(defun cljh-defn (node)
  (when (cljh-defn-node? node)
    (let* ((name (cljh-def-name node))
           (docstring (cljh-defn-docstring node))
           (params (cljh-defn-params node))
           (result
            `((name ,name)
              (params ,params))))
      (if docstring
          (cons `(docstring ,docstring) result)
        result))))

;; New
(defun cljh--node-depth-from-impl (buffer-root root node depth)
  (cond
   ((equal node root) depth)
   ((equal node buffer-root) nil)
   (t (cljh--node-depth-from-impl
       buffer-root
       root
       (treesit-node-parent node)
       (1+ depth)))))

(defun cljh-node-depth-from (root node)
  "Compute the depth of NODE from the root."
  (cljh--node-depth-from-impl (treesit-buffer-root-node) root node 0))

(defun my/treesit-query-capture-with-depth (root depth query)
  "Run `treesit-query-capture` but only return nodes at most `DEPTH` from the root."
  (seq-filter
   (lambda (p)
     (when-let ((node-depth (cljh-node-depth-from root (cdr p))))
       (<= node-depth depth)))
   (treesit-query-capture root query)))

(defun cljh-top-level-requires ()
  (my/treesit-query-capture-with-depth
   (treesit-buffer-root-node)
   1
   '((list_lit :anchor (sym_lit name: _) @name
               (:equal @name "require")) @l)))
