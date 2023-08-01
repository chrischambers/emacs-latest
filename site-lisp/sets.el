;;; sets.el --- A simple set implementation using hash-tables internally -*- lexical-binding: t; -*-

(defconst set-missing-member :missing-set-member
  "The sentinel object for missing set members.")

(defun make-set (&rest args)
  "Creates a set-like structure using a hash-table."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (arg args)
      (set-add set arg))
    set))

(defun set-length (set)
  "Returns length of SET."
  (length (set-members set)))

(defun set-add (set x)
  "ADDS X TO SET."
  (puthash x t set))

(defun set-remove (set x)
  "Removes X from SET."
  (remhash x set))

(defun set-p (set)
  "True if SET is a set."
  (hash-table-p set))
(defalias 'set? #'set-p)

(defun set-member-p (set x)
  "True if X is in SET."
  (not (eq set-missing-member
           (gethash x set set-missing-member))))
(defalias 'set-member? #'set-member-p)

(defun set-members (set)
  "Returns a sequence of the members of SET."
  (hash-table-keys set))
