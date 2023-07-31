;;; org-roam-refactoring2.el --- Common commands for moving/renaming links -*- lexical-binding: t; -*-

(require 'org-roam-node)
(require 'dash)

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
