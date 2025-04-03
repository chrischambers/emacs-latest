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
  (seq-let [_ _ &rest parts] (treesit-node-children defn-node 'named)
    (if (cljh-filter-for-type parts "vec_lit")
        1
      (length (cljh-filter-for-type parts "list_lit")))))

(defun cljh-arity-overloaded? (defn-node)
  "A function has arity overloading if, at its top-level, it doesn't
contain a vector form."
  (seq-let [_ _ &rest parts] (treesit-node-children defn-node 'named)
    (not (cljh-filter-for-type parts "vec_lit"))))

(defun cljh--extract-as-impl (children previous-as?)
  (cond
   ((null children) nil)
   (previous-as? (cljh-node-display (car children)))
   ((string-equal
     (treesit-node-text
      (treesit-node-child-by-field-name (car children) "name")) "as")
    (cljh--extract-as-impl (cdr children) t))
   (t (cljh--extract-as-impl (cdr children) nil))))

(defun cljh-extract-as (node)
  (cljh--extract-as-impl (treesit-node-children node 'named) nil))

(defun cljh--parse-params-impl (children vec-count map-count)
  (if (null children)
      nil
    (let* ((first (car children))
           (rest (cdr children))
           (type (treesit-node-type first)))
      (cond
       ((string-equal type "sym_lit")
        (if (string-equal
             (treesit-node-text
              (treesit-node-child-by-field-name first "name")) "&")
            nil
          (cons
           (cljh-node-display first)
           (cljh--parse-params-impl rest vec-count map-count))))

       ((string-equal type "vec_lit")
        (let ((as (cljh-extract-as first))
              (name (format "v%s" vec-count)))
          (if as
              (cons as (cljh--parse-params-impl rest vec-count map-count))
            (cons name (cljh--parse-params-impl rest (1+ vec-count) map-count)))))

       ((string-equal type "map_lit")
        (let ((as (cljh-extract-as first))
              (name (format "m%s" map-count)))
          (if as
              (cons as (cljh--parse-params-impl rest vec-count map-count))
            (cons name (cljh--parse-params-impl rest vec-count (1+ map-count))))))))))

(defun cljh-parse-params (node)
  (cljh--parse-params-impl (treesit-node-children node 'named) 1 1))

(defun cljh-defn-params (node)
  (let ((children (treesit-node-children node)))
    (cljh-parse-params
     (if (not (cljh-arity-overloaded? node))
         (car (cljh-filter-for-type children "vec_lit"))
       (when-let* ((list-nodes (cljh-filter-for-type children "list_lit"))
                   (last-node (car (last list-nodes))))
         (car (cljh-filter-children-for-type last-node "vec_lit")))))))

(defun cljh-defn-docstring (node)
  (when-let ((results (cljh-filter-children-for-type node "str_lit")))
    (cljh-str-lit-display (car results))))

(defun cljh-defn-body (node)
  (when-let* ((list-nodes (cljh-filter-children-for-type node "list_lit")))
    (if (not (cljh-arity-overloaded? node))
        (-map #'cljh-node-display list-nodes)
      (when-let* ((list-nodes (cljh-filter-children-for-type (car (last list-nodes)) "list_lit")))
        (-map #'cljh-node-display list-nodes)))))

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
(defun cljh-ns-name ()
  (let* ((node (treesit-buffer-root-node))
         (first-child (treesit-node-child node 0 'named))
         (children (treesit-node-children first-child 'named)))
    (when (string-equal
           (treesit-node-text (treesit-node-child-by-field-name
                               (car children) "name"))
           "ns")
      (cljh-node-display
       (treesit-node-child-by-field-name
        (nth 1 children) "name")))))

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
              (test-name (cljh-corresponding-test-name name))
              (matches (-filter (lambda (xs) (equal (nth 2 xs) test-name)) tests))
              (result (car matches)))
    result))

(defun cljh-defn (node)
  (when (cljh-defn-node? node)
    (let* ((name (cljh-def-name node))
           (docstring (cljh-defn-docstring node))
           (params (cljh-defn-params node))
           (body (cljh-defn-body node))
           (result
            `((name ,name)
              (params ,params)
              (body ,body))))
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

(defun cljh-nth-level-forms (depth form-name)
  (-map #'cljh-node-info
        (cljh-select-when-car-eql
         'list
         (my/treesit-query-capture-with-depth
          (treesit-buffer-root-node)
          depth
          `((list_lit :anchor (sym_lit name: _) @name
                      (:equal @name ,form-name)) @list)))))

(defun cljh-top-level-requires ()
  (cljh-nth-level-forms 1 "require"))

(defun cljh-squeeze-blank-lines-around (&optional posn)
  (interactive "d")
  (let ((posn (or posn (point))))
    (save-excursion
      (goto-char posn)
      (delete-blank-lines))))

(defun cljh-delete-top-level-requires (&optional buffer)
  (interactive "b")
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (dolist (parts (cljh-top-level-requires))
        (seq-let [start end] parts
          (delete-region start end)
          (cljh-squeeze-blank-lines-around start))))))

(defun cljh-replace-ns (replacement)
  (when (derived-mode-p 'clojure-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^(ns " nil t)
        (re-search-backward "(")
        (let ((start (point))
              (end (progn (forward-sexp) (point))))
          (replace-region-contents start end (lambda () replacement))
          (cljh-delete-top-level-requires)
          (save-buffer))))))

(defun cljh-consolidate-ns (&rest libspecs)
  (when (derived-mode-p 'clojure-mode)
    (let* ((file-name (buffer-file-name (current-buffer)))
           (libspecs (-map (lambda (ls) (format "'%s'" ls)) libspecs))
           (cmd (concat "clj_consolidate_requires " file-name " " (s-join " " libspecs)))
           (new-ns (s-trim (shell-command-to-string cmd))))
      (when new-ns
        (cljh-replace-ns new-ns)))))

(defun cljh-read-multiple-strings (prompt)
  "Read multiple strings from the minibuffer until an empty string is entered.
Returns them as a list to be used in an interactive call."
  (let ((strings '())
        (input nil))
    (while (not (string-empty-p (setq input (read-string prompt))))
      (push input strings))
    (nreverse strings)))

(defun update-clojure-namespace (&rest libspecs)
  "Consolidates top-level requires (and provided libspecs) into `ns' form."
  (interactive (cljh-read-multiple-strings "Provide libspecs (RET to finish): "))
  (apply #'cljh-consolidate-ns libspecs))

(setq cljh-test-template
      "
(deftest %s
    (testing \"it satisfies its spec\"
      (is (successful? (check `%s))))
    (testing \"it returns correct values for known inputs\"
      (are [%s expected] (= expected (%s %s))
        %s
        )))")

(defun cljh-test-jump ()
  (interactive)
  (let* ((current-fn (cljh-defn-node-at (point)))
         (ns (cljh-ns-name))
         (name (cljh-def-name current-fn))
         (test-name (cljh-corresponding-test-name name))
         (params (cljh-defn-params current-fn))
         (param-string (s-join " " params))
         (_ (projectile-toggle-between-implementation-and-test))
         (test-found? (cljh-test-name-in-buffer? name)))
    (if test-found?
        (let ((start-posn (car test-found?)))
          (goto-char start-posn))
      (progn
        (cljh-consolidate-ns
         "[clojure.test :as test :refer [are deftest is testing]]"
         "[respeced.test :refer [check successful?]]"
         (format "[%s :refer [%s]]" ns name))
        (goto-char (point-max))
        (insert
         (format cljh-test-template
                 test-name
                 name
                 param-string
                 name
                 param-string
                 (s-join " " (-repeat (1+ (length params)) "1"))))
        (call-interactively #'apheleia-format-buffer)))))
