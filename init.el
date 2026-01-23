;; Bootstrap straight.el package manager.
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications '(watch-files find-when-checking))
(setq straight-cache-autoloads nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'gcmh)
(gcmh-mode 1)
(straight-use-package 'use-package)
(straight-use-package 'org)
(org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(put 'dired-find-alternate-file 'disabled nil)
