;;; Some simple tools and utilities

(use-package async
  :demand t
  :init
  (async-bytecomp-package-mode 1))

(use-package elscreen
  :disabled t
  :config
  (setq elscreen-display-tab t)
  (elscreen-start))

;; Not working well with ivy-switch-buffer
;; and other commands.
(use-package elscreen-buffer-group
  :disabled
  :after elscreen)

(use-package perspective
  :init
  (persp-mode)
  ;; (defun my/persp-switch-to-buffer()
  ;;   (interactive)
  ;;   (completing-read "Switch to buffer: " (remove nil (mapcar 'buffer-name (persp-buffers (persp-curr))))))
  :custom
  (persp-mode-prefix-key (kbd "C-."))
  :config
  ;; (define-key perspective-map (kbd ".") 'my/persp-switch-to-buffer)
  (when (eq pm-completion-system 'ivy)
    (bind-key "C-x b" #'persp-counsel-switch-buffer)
    (bind-key "M-o" #'persp-counsel-switch-buffer)))

(use-package which-key
  :config
  (which-key-mode)
  :delight)

(use-package avy
  :bind (("C-;" . avy-goto-word-1)
         ("C-c ;" . avy-goto-word-1)
         ("C-:" . avy-goto-char)
         ("M-g g" . avy-goto-line)
         ("C-'" . avy-isearch)))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Anzu
(use-package anzu
  :init
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  :delight)

(use-package undo-tree
  :config
  (setq global-undo-tree-mode t)
  (setq undo-tree-visualizer-diff t))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-SPC" . set-rectangular-region-anchor)))

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(use-package saveplace
  :custom
  (save-place-file "~/.emacs.d/saveplace")
  :config
  (save-place-mode 1))

(use-package imenu-anywhere
  :bind ("C-c C-/" . imenu-anywhere))

(use-package emamux
  :custom
  (global-set-key (kbd "C-c m") emamux:keymap)
  (emamux:completing-read-type 'ido)
  :bind (("C-c m m" . emamux:run-command)
         ("C-c m q" . emamux:close-panes)
         ("C-c m s" . emamux:send-command)
         ("C-c m n" . emamux:new-window)
         ("C-c m h" . emamux:split-window)))

(use-package god-mode
  :bind (("<escape>" . god-local-mode)
         :map god-local-mode-map
         ("i" . god-local-mode)
         ("." . repeat)
         ("z" . scroll-down-command)))

(use-package xclip
  :config
  (xclip-mode 1))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-alist
   '((?s aw-swap-window "Swap Windows")
     (?2 aw-split-window-vert "Split Window Vertically")
     (?3 aw-split-window-horz "Split Window Horizontally")
     (?? aw-show-dispatch-help)))
  :config
  (ace-window-display-mode 1)
  :bind ("C-o" . ace-window))

(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode)
  :config
  (dtrt-indent-mode t)
  (setq dtrt-indent-verbosity 0)
  :delight)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (smartparens-mode t)
  (require 'smartparens-config)
  ;; when you press RET, the curly braces automatically
  ;; add another newline
  (sp-with-modes '(c-mode c++-mode)
                 (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
                 (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                           ("* ||\n[i]" "RET")))))

(use-package paredit
  :disabled
  :hook (emacs-lisp-mode . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-c = -" . er/contract-region)
         ("C-c = =" . er/mark-symbol)
         ("C-c = f" . er/mark-defun)))

(use-package yasnippet                                                                                                                       
  :commands yas-expand                                                                                                                       
  :config                                                                                                                                    
  (yas-reload-all)                                                                                                                           
  (yas-minor-mode))

(use-package crux
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-k" . crux-smart-kill-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("M-L" . crux-duplicate-current-line-or-region)
         ("C-c M-;" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c I" . crux-find-user-init-file)
         ("C-c C-k" . crux-kill-line-backwards)))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))

(use-package default-text-scale
  :config
  (default-text-scale-mode 1))

(use-package nlinum-relative
  :hook (prog-mode . nlinum-relative-mode))

(provide 'tools-setup)
