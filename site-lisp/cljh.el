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

;; Doesn't handle multiple-arity, or keyword / rest params very well yet
(defun cljh-defn-params (node)
  (when-let ((results (treesit-filter-child
                       node
                       (lambda (n) (equal (treesit-node-type n) "vec_lit"))))
             (results (-> results
                          car
                          (treesit-query-capture '((sym_lit name: (_) @name))))))
    (->> results
         (-map (-compose #'cljh-node-display #'cdr)))))

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
