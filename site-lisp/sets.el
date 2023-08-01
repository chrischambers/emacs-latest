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

(defun copy-set (set)
  "Returns a copy of SET."
  (copy-hash-table set))

(describe "a set"
  :var (set)

  (before-each
    (setq set (make-set :a :b 1 nil 'c)))
  (after-each
    (setq set nil))

  (it "can be correctly identified as a set"
    (dolist (i '(a 1 :b "c"))
      (expect (set? i) :not :to-be t))
    (expect (set? set) :to-be t))

  (describe "membership"
    (it "accurately reports whether elements are members"
      (dolist (i (set-members set))
        (expect (set-member? set i) :to-be t))
      (dolist (i '(2 b "d" :c))
        (expect (set-member? set i) :not :to-be t)))
    (it "handles nil correctly"
      (expect (set-member? set nil) :to-be t)
      (set-remove set nil)
      (expect (set-member? set nil) :not :to-be t)
      (set-add set nil)
      (expect (set-member? set nil) :to-be t)))

  (describe "`copy-set'"
    (it "creates a new set with identical members"
      (let ((copy (copy-set set)))
        (expect (set-members copy) :to-equal (set-members set)))))

  (describe "invariants"
    (it "is unchanged when a duplicate member is added"
      (let ((copy (copy-set set)))
        (set-add copy :a)
        (expect (set-members copy) :to-equal (set-members set))))
    (it "is unchanged if a non-member is removed"
      (let ((copy (copy-set set)))
        (set-remove copy 'foobar)
        (expect (set-members copy) :to-equal (set-members set)))))

  (describe "has a length (`set-length')"
    (it "reports the correct length"
      (expect (set-length set) :to-equal 5))
    (it "is correct when elements are added/removed"
      (dolist (i '(2 3))
        (set-add set i))
      (expect (set-length set) :to-equal 7)
      (set-remove set 1)
      (expect (set-length set) :to-equal 6))))
