;;; org-roam-refactoring.el --- Common commands for moving/renaming links -*- lexical-binding: t; -*-

(require 'org-roam-node)
(require 'dash)

(defun org-roam-refactor/files-linked-to-id (node-id)
  (let ((links
         (org-roam-db-query
          [:select :distinct [n:file]
                   :from [(as links l), (as nodes n)]
                   :where (and (= l:dest $s1) (= l:type "id") (= n:id l:source))]
          node-id)))
    (-flatten links)))

(defun org-roam-refactor/get-files-linked-to ()
  (let* ((node (org-roam-node-read))
         (node-id (org-roam-node-id node)))
    (org-roam-refactor/files-linked-to-id node-id)))

;; ------------------------------------------------------------------------------
;; How to get an actual node object: extract id, invoke =org=roam-node-from-id=

(defun wibble ()
  (interactive)
 (let* ((node (org-roam-node-read))
         (node-id (org-roam-node-id node))
         (links (org-roam-db-query
          [:select :distinct [n:id]
                   :from [(as links l), (as nodes n)]
                   :where (and (= l:dest $s1) (= l:type "id") (= n:id l:source))]
          node-id))
         (first (car (-flatten links))))
    (org-roam-node-from-id first)))

;; Alternatively, use the extracted body to avoid the needless database query check:
(defun wibble ()
  (interactive)
  (let* ((node (org-roam-node-read))
         (node-id (org-roam-node-id node))
         (links (org-roam-db-query
          [:select :distinct [n:id]
                   :from [(as links l), (as nodes n)]
                   :where (and (= l:dest $s1) (= l:type "id") (= n:id l:source))]
          node-id))
         (first (car (-flatten links))))
    (org-roam-populate (org-roam-node-create :id first))))
;; ------------------------------------------------------------------------------
(defun wibble ()
  (interactive)
  (let* ((to-node (org-roam-node-read))
         (node-id (org-roam-node-id to-node))
         (links (org-roam-db-query
                 [:select :distinct [n:id]
                          :from [(as links l), (as nodes n)]
                          :where (and (= l:dest $s1) (= l:type "id") (= n:id l:source))]
                 node-id))
         (first (car (-flatten links)))
         (from-node (org-roam-populate (org-roam-node-create :id first))))
    (progn
      (org-roam-node-visit from-node)
      (message "%s" from-node))))

;; ------------------------------------------------------------------------------
; source: file link is located in
; dest: where link is going to
(defun wibble ()
  (let* ((to-node (org-roam-node-read))
         (node-id (org-roam-node-id to-node))
         (links (org-roam-db-query
          [:select :distinct [n:id, l:pos]
                   :from [(as links l), (as nodes n)]
                   :where (and (= l:dest $s1) (= l:type "id") (= n:id l:source))]
          node-id))
         (first (car links)))
    (seq-let [id pos] first
         (org-roam-populate (org-roam-node-create :id id)))))
;; ------------------------------------------------------------------------------
;; Potentially Useful:
(defun org-roam-link-replace-all ()
  "Replace all \"roam:\" links in buffer with \"id:\" links."
  (interactive)
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (org-roam-link-replace-at-point))))

(defun my/print-link (link)
  (message "%s" link))

;; "Run fns over all links in the current buffer."
;; (org-roam-db-map-links (list #'my/print-link))

;; (link (:type id
;;        :path B062AB89-D7F8-44B1-A9CB-AA675FDF264B
;;        :format bracket
;;        :raw-link id:B062AB89-D7F8-44B1-A9CB-AA675FDF264B
;;        :application nil
;;        :search-option nil
;;        :begin 2250
;;        :end 2299
;;        :contents-begin 2293
;;        :contents-end 2296
;;        :post-blank 1
;;        :parent (paragraph (:begin 2224
;;        :end 2366
;;        :contents-begin 2224
;;        :contents-end 2365
;;        :post-blank 1
;;        :post-affiliated 2224
;;        :mode nil
;;        :granularity element
;;        :cached t
;;        :parent (section (:begin 1904
;;        :end 2567
;;        :contents-begin 1904
;;        :contents-end 2566
;;        :robust-begin 1904
;;        :robust-end 2564
;;        :post-blank 1
;;        :post-affiliated 1904
;;        :mode section
;;        :granularity element
;;        :cached t
;;        :parent (headline (:raw-value LXC Usage
;;        :begin 1891
;;        :end 2567
;;        :pre-blank 0
;;        :contents-begin 1904
;;        :contents-end 2566
;;        :robust-begin 1906
;;        :robust-end 2564
;;        :level 2
;;        :priority nil
;;        :tags nil
;;        :todo-keyword nil
;;        :todo-type nil
;;        :post-blank 1
;;        :footnote-section-p nil
;;        :archivedp nil
;;        :commentedp nil
;;        :post-affiliated 1891
;;        :title LXC Usage
;;        :mode nil
;;        :granularity element
;;        :cached t
;;        :parent (headline (:raw-value <<<Linux Containers>>> / <<<LXC>>> / <<<LXCFS>>> / <<<LXD>>>
;;        :begin 1189
;;        :end 2567
;;        :pre-blank 1
;;        :contents-begin 1253
;;        :contents-end 2566
;;        :robust-begin 1255
;;        :robust-end 2564
;;        :level 1
;;        :priority nil
;;        :tags nil
;;        :todo-keyword nil
;;        :todo-type nil
;;        :post-blank 1
;;        :footnote-section-p nil
;;        :archivedp nil
;;        :commentedp nil
;;        :post-affiliated 1189
;;        :title <<<Linux Containers>>> / <<<LXC>>> / <<<LXCFS>>> / <<<LXD>>>
;;        :mode nil
;;        :granularity element
;;        :cached t
;;        :parent (org-data (:begin 1
;;        :contents-begin 1
;;        :contents-end 2644
;;        :end 2644
;;        :robust-begin 161
;;        :robust-end 2642
;;        :post-blank 0
;;        :post-affiliated 1
;;        :path /Users/diomedes/Dropbox/notes/#old/3-resources/operating_systems/linux/containers.org
;;        :mode org-data
;;        :ID 74AFCEF1-7C44-4446-B66B-AD5732BF5B36
;;        :ROAM_ALIASES LXCFS LXD LXC containerisation jails jail containers "OS-level virtualisation"
;;        :CATEGORY containers
;;        :cached t
;;        :org-element--cache-sync-key nil))
;;        :org-element--cache-sync-key nil))
;;        :org-element--cache-sync-key nil))
;;        :org-element--cache-sync-key nil))
;;        :org-element--cache-sync-key nil))))

(defun org-roam-refactor/buffer-get-links ()
  "Collect all links in the current buffer.

Implementation stolen from org-roam-db-map-links."
  (let ((results '()))
    (org-with-point-at 1
      (while (re-search-forward org-link-any-re nil :no-error)
        (backward-char)
        (let* ((begin (match-beginning 0))
               (element (org-element-context))
               (type (org-element-type element))
               link bounds)
          (cond
           ((eq type 'link)
            (setq link element))
           ((and (member type org-roam-db-extra-links-elements)
                 (not (member-ignore-case (org-element-property :key element)
                                          (cdr (assoc type org-roam-db-extra-links-exclude-keys))))
                 (setq link (save-excursion
                              (goto-char begin)
                              (save-match-data (org-element-link-parser)))))))
          (when link
            (push link results)))))
  results))

(defun org-roam-refactor/buffer-get-links-to-id (id)
  (let ((links (org-roam-refactor/buffer-get-links)))
    (--filter (s-contains? id (org-element-property :path it) id) links)))

(defun org-roam-refactor/get-links-to-id (id)
  (let* ((links (org-roam-db-query
          [:select :distinct [n:id]
                   :from [(as links l), (as nodes n)]
                   :where (and (= l:dest $s1) (= l:type "id") (= n:id l:source))]
          id)))
    (->> links
         -flatten
         (--map (org-roam-populate (org-roam-node-create :id it))))))

;; (->> "1B58213E-EB80-4452-9809-0B107DEE0031"
;;      org-roam-refactor/get-links-to-id
;;      (--map (org-roam-node-file it))
;;      -distinct)

;; C-. - embark act
;; M-. - embark dwim
;; COLLECT or OCCUR: this is the sort of buffer you want for renaming

;; NEXT: Function which takes a link (not a buffer with links) and replaces it
;; with another link. Optionally, update the label text. Needs to open the
;; buffer if it's not open, modify it, save it, and close it if was opened
;; exclusively for this purpose.

;;; ------------------------------------------------------------------------------

(defcustom org-roam-refactoring-buffer-name "*org-roam-refactoring*"
  "The name of the special org-roam-refactoring buffer"
  :group 'org-roam-refactoring
  :type 'string)

(defun my/org-roam-aliases ()
  ;; (mapcar (lambda (node) `(,(org-roam-node-title node) . ,node)) (org-roam-node-list)))
  ;; (mapcar #'org-roam-node-title (org-roam-node-list)))
  (mapcar (lambda (node) `(,(org-roam-node-title node) .
                      ,(format "[[%s][%s]]"
                               (org-roam-node-id node)
                               (org-roam-node-title node))))
          (org-roam-node-list)))

(defun org-roam-refactoring-completion-at-point ()
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end (my/org-roam-aliases) . nil )))

(defun my/org-roam-id-find (id &optional markerp pos)
  (let* ((rows (org-roam-db-query
                [:select [n:file, n:pos]
                         :from [(as nodes n)]
                         :where (= n:id $s1)] id))
         (row (car rows))
         (file (car row))
         (pos (if pos pos (cadr row)))
         (pair (cons file pos)))
    (if markerp
        (unwind-protect
            (let ((buffer (or (find-buffer-visiting file)
                              (find-file-noselect file))))
              (with-current-buffer buffer
                (move-marker (make-marker) pos buffer))))
      pair)))

(advice-add #'org-id-find :override #'my/org-roam-id-find)

(defun my/marker-for-parent-headline (id pos)
  (pcase (my/org-roam-id-find id nil pos)
    (`(,file . ,pos)
     (unwind-protect
         (let ((buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))))
           (with-current-buffer buffer
             (goto-char pos)
             (org-back-to-heading-or-point-min t)
             (move-marker (make-marker) (point) buffer)))))))

(defun org-transclusion-add-org-id-at-point (link plist)
  "Return a list for Org-ID LINK object and PLIST.
    Return nil if not found."
  (when (string= "id@point" (org-element-property :type link))
    (let* ((pair (s-split ":@" (org-element-property :path link)))
           (id (car pair))
           (pos (string-to-number (cadr pair)))
           (mkr (my/marker-for-parent-headline id pos))
           (payload '(:tc-type "org-id")))
      (if mkr
          (append payload (org-transclusion-content-org-marker mkr plist))
        (message
         (format "No transclusion done for this ID. Ensure it works at point %d, line %d"
                 (point) (org-current-line)))
        nil))))

(with-eval-after-load 'org-transclusion
  (push #'org-transclusion-add-org-id-at-point org-transclusion-add-functions))

(defvar org-roam-refactoring-mode-map nil "Keymap for `org-roam-refactoring-mode'")

(define-derived-mode org-roam-refactoring-mode org-mode "org-roam-refactoring"
  (add-hook 'completion-at-point-functions 'org-roam-refactoring-completion-at-point nil 'local))
;; ---------------------------------------------------------------------------

(defun org-roam-refactoring-kill-buffer-ask ()
  (interactive)
  (when-let ((buffer (get-buffer org-roam-refactoring-buffer-name)))
    (kill-buffer-ask buffer)))

(defun orr-refresh-transclusion ()
  (interactive)
  (if (org-transclusion-within-transclusion-p)
      (org-transclusion-refresh)))

(defun orr-edit-transclusion ()
  (interactive)
  (if (org-transclusion-within-transclusion-p)
      (org-transclusion-live-sync-start)))

(progn
  (setq org-roam-refactoring-mode-map (make-sparse-keymap))
  (general-def '(motion normal) org-roam-refactoring-mode-map
    "q" #'org-roam-refactoring-kill-buffer-ask
    "e" #'orr-edit-transclusion
    "r" #'orr-refresh-transclusion))

;; ---------------------------------------------------------------------------
(defun orr/get-link-at-point ()
  (when (derived-mode-p 'org-mode)
    (let ((node (org-element-context)))
      (when (orr/org-element-is-link? node)
        node))))

(defun orr/org-element-is-link? (node)
  (string= (org-element-type node) "link"))

(defun orr/org-element-get-bounds (node)
  (list (org-element-begin node)
        (org-element-end node)))

(defun orr/org-link-get-raw-link (node)
  (org-element-property :raw-link node))

(defun orr/org-link-get-description (node)
  (buffer-substring-no-properties
   (org-element-property :contents-begin node)
   (org-element-property :contents-end node)))

(defun orr/change-link-target-with-id:link (node id)
  (let ((link (if (s-starts-with? "id:" id) id (format "id:%s" id))))
    (org-link-make-string link (orr/org-link-get-description node))))

(defun orr/change-link-description (node description)
  (org-link-make-string (orr/org-link-get-raw-link node) description))

;; save-excursion
;; org-open-link-from-string "id@point:...
(defun orr/replace-link-target-with-id:link ()
  (interactive)
  (when-let* ((node (orr/get-link-at-point))
              (remove (orr/org-element-get-bounds node))
              (id (org-roam-node-id (org-roam-node-read)))
              (new-link (orr/change-link-target-with-id:link node id)))
    (when remove (apply #'delete-region remove))
    (insert new-link)))

(defun orr/replace-link-description ()
  (interactive)
  (when-let* ((node (orr/get-link-at-point))
              (remove (orr/org-element-get-bounds node))
              (old-description (orr/org-link-get-description node))
              (description (read-string "New description: " old-description))
              (new-link (orr/change-link-description node description)))
    (when remove (apply #'delete-region remove))
    (insert new-link)))

;; ---------------------------------------------------------------------------

(defun orr/org-roam-row-to-transclusion-block (row)
  (pcase row
    (`(,title ,source ,pos ,dest ,file)
     (let ((link (format "[[id@point:%s:@%s][%s]]"
                         source
                         pos
                         title)))
       (format "#+transclude: %s :level 2 :exclude-elements \"drawer keyword\"\n" link)))))

(defun orr/org-roam-row-to-link-text (row)
  (cl-destructuring-bind (title id pos search-term _) row
    (format "* [[id@point:%s:@%s][%s]] (%s)"
            id
            pos
            title
            pos)))

(defun orr/render-org-roam-row (row)
  (format "%s\n %s"
          (orr/org-roam-row-to-link-text row)
          (orr/org-roam-row-to-transclusion-block row)))

(defun orr/-get-id-from-node-property (node)
  (when (string= (org-element-property :key node) "ID")
    (org-element-property :value node)))

(defun orr/get-id-from-parent-section ()
  (let* ((section (org-ml-parse-this-section))
         (ids (org-element-map
                  section
                  'node-property
                #'orr/-get-id-from-node-property))
         (id (car ids)))
    id))

(defun my/link-overlay (link id)
  (when-let* ((link (or link (orr/get-org-link)))
              (bounds (-update-at 1 #'1- (orr/org-element-get-bounds link))))
    (when (string= (org-element-property :path link) id)
      (let ((overlay (apply #'make-overlay bounds)))
        (overlay-put overlay 'face (cons 'background-color "darkolivegreen"))))))

(defun my/add-link-overlay-with-id (id)
  (org-element-map (org-element-parse-buffer) 'link (lambda (n) (my/link-overlay n id))))

(defun org-roam-refactor ()
  "Easily update all the backlinks for a given node."
  (interactive)
  (let* ((link (orr/get-link-at-point))
         (parent-id (orr/get-id-from-parent-section))
         (id (cond
              (link (let* ((initial-input (when link (orr/org-link-get-description link)))
                           (node (org-roam-node-read initial-input nil nil 'require-match))
                           (id (org-roam-node-id node)))
                      id))
              (parent-id parent-id)
              (t (let* ((node (org-roam-node-read nil nil nil 'require-match))
                        (id (org-roam-node-id node)))
                   id))))
         (new-buffer (get-buffer-create org-roam-refactoring-buffer-name))
         (rows (org-roam-db-query
                (format
                 "SELECT n.title, l.source, l.pos, l.dest, n.file
                     FROM nodes as n, links as l
                     WHERE n.id = l.source AND l.type = '\"id\"'
                     AND l.dest = '\"%s\"'
                     ORDER BY n.title" id))))
    (switch-to-buffer new-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-roam-refactoring-mode)
      (insert (s-join "\n" (mapcar #'orr/render-org-roam-row rows)))
      (goto-char (point-min))
      (org-transclusion-add-all)
      (my/add-link-overlay-with-id id))))
