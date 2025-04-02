;;; cljh.el --- -*- lexical-binding: t; -*-

;; Utility Functions:
;; --------------------------------------------------------------------------
(defun cljh-node-display (node)
  (substring-no-properties (treesit-node-text node)))

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

(defun cljh-filter-children-for-type (node type)
  (--filter (equal (treesit-node-type it) type) (treesit-node-children node)))

;; Node Operations:
;; --------------------------------------------------------------------------
(defun cljh-def-node? (node)
  (treesit-query-capture
   node
   '((list_lit
      (sym_lit name: (_) @name)
      (:match  "^.?def" @name)
      (_) @body))))

(defun cljh-defn-node? (node)
  "Includes `defn', `>defn' `defmacro', ..."
  (treesit-query-capture
   node
   '((list_lit
      (sym_lit name: (_) @name)
      (:match  "^.?def.+" @name)
      (_) @body))))

(defun cljh-arity (defn-node)
  (seq-let [_ _ &rest parts] (treesit-node-children defn-node)
    (if (equal (treesit-node-type (first parts)) "vec_lit")
        1
      (length (--filter (equal (treesit-node-type it) "list_lit") parts)))))

;; You'll still need to interrogate by type - position doesn't work - arbitrary
;; docstrings
(defun cljh-defn-params (node)
  (let ((children (treesit-node-children node)))
    (cljh-node-display
     (if (= (cljh-arity node) 1)
         (car (--filter (equal (treesit-node-type it) "vec_lit") children))
       (let* ((list-nodes (--filter (equal (treesit-node-type it) "list_lit") children))
              (last-node (car (last list-nodes)))
              (children (treesit-node-children last-node)))
         (car (--filter (equal (treesit-node-type it) "vec_lit") children)))))))

(defun cljh-def-name (node)
  (-> node
      (treesit-node-child 2)
      (treesit-node-child-by-field-name "name")
      treesit-node-text))

;; Position Operations:
;; --------------------------------------------------------------------------
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

;;; New
(defun cljh-str-lit-display (node)
  (->> node
       treesit-node-text
       substring-no-properties
       (s-chop-left 1)
       (s-chop-right 1)))

(defun cljh--node-equal? (node type)
  (equal (treesit-node-type node) type))

(defun cljh-str? (node)
  (cljh--node-equal? node "str_lit"))

(defun cljh-vector? (node)
  (cljh--node-equal? node "vec_lit"))

(defun cljh-list? (node)
  (cljh--node-equal? node "list_lit"))

(defun cljh--display-defn-params (sym-lits)
  (->> sym-lits
       (-map (-compose #'cljh-node-display #'cdr))
       (-remove (lambda (x) (equal x "&")))))

(defun cljh-direct-child? (node)
  (lambda (n) (member n (treesit-children-node node))))

;; strategy idea: get bounds of defn /excluding final list (i.e. body)
;; and bound the query search to that.
;; You then check for:
;; - the first vec_lit form, if present, and use that, parsing appropriately, or
;; - the final list_lit's first vec_lit


;; Another idea: create fn checking for multiple-arity? dispatch on that in all
;; cases where it matters. By default, use the final arity params/body.

;; cljh-arity: returns a number

(defun cljh--process-defn-params (defn-node)
  (when-let ((last-match
              (last
               (treesit-query-capture
                defn-node
                '((list_lit (vec_lit (_)) @v
                            :anchor (list_lit) :anchor @l)
                  (:pred call? (cljh-direct-child? defn-node) @l))
                nil nil t))))
    (cljh-node-display (car last-match))))

(defun cljh--process-defn-parts (parts acc)
  (if (not parts) acc
    (seq-let [first &rest rest] parts
      (cond
       ((cljh-str? first)
        (cljh--process-defn-parts
         rest
         (cons `(docstring ,(cljh-str-lit-display first)) acc)))

       ((cljh-vector? first)
        (cons `(params ,(cljh--process-defn-params first)) acc))

       ((cljh-list? first)
        (let ((params (treesit-query-capture
                       (treesit-node-parent first)
                       '((list_lit (vec_lit (_) @v :anchor))))))
          (cons `(params ,(cljh--display-defn-params params)) acc)))

       (t acc)))))

(defun cljh-parse-defn (node)
  (seq-let [_ name &rest parts] (treesit-node-children node 'named)
    (let ((results `((name ,(cljh-node-display name)))))
      (cljh--process-defn-parts parts results))))
