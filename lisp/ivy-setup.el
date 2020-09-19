;;; Setup for ivy/counsel.

(use-package ivy
  :bind(("C-c C-r" . ivy-resume)
        ("C-c <down>" . ivy-push-view)
        ("C-c <up>" . ivy-pop-view))
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-extra-directories nil) ;; do not show ../  and ./
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  :delight ivy-mode)

(use-package swiper
  :requires ivy
  :after ivy
  :bind("M-s i" . swiper))

(use-package counsel
  :requires ivy
  :after ivy
  :bind(("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("M-j" . counsel-find-file)
        ("M-J" . find-file-other-window)
        ("M-o" . ivy-switch-buffer)
        ("M-O" . counsel-switch-buffer-other-window)
        ("M-y" . counsel-yank-pop)
        ("C-c h f" . counsel-describe-function)
        ("C-c h v" . counsel-describe-variable)
        ("C-c h i" . counsel-info-lookup-symbol)
        ("C-c h u" . counsel-unicode-char)
        ("C-c /" . counsel-imenu)
        ("C-c ." . counsel-bookmark)
        ("C-x C-r" . counsel-recentf)
        :map read-expression-map
        ("C-r" . counsel-expression-history)))

(use-package ivy-prescient
  :requires (prescient ivy)
  :custom
  (ivy-prescient-sort-commands
   '(:not swiper ivy-switch-buffer counsel-switch-buffer))
  (ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-enable-filtering t)
  (ivy-prescient-enable-sorting t)
  :config
  (ivy-prescient-mode 1))

(use-package ivy-posframe
  :requires ivy
  :custom
  (ivy-posframe-parameters
   '((left-fringe . 2)
     (right-fringe . 2)
     (internal-border-width . 2)))
  (ivy-posframe-height-alist
   '((swiper . 15)
     (swiper-isearch . 15)
     (t . 10)))
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper . nil)
          (t . ivy-posframe-display-at-window-center)))
  ;; posframe misses the default font sometimes
  ;; so set is explicitly but this will still not work
  ;; when the font is changed interactively, need to
  ;; check and add a hook.
  (setq ivy-posframe-font (elt (query-font (face-attribute 'default :font)) 0))
  (ivy-posframe-mode 1)
  :delight ivy-posframe-mode)

(use-package ivy-rich
  :requires ivy
  :ensure t
  :custom
  (ivy-rich-path-style 'abbreviate)
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (ivy-rich-mode 1))

 (use-package icomplete
   :config
   (icomplete-mode 1))

(use-package counsel-gtags
  :requires counsel
  :hook (c-mode c++-mode)
  :bind (("M-." . counsel-gtags-dwim)
         ("M-*" . counsel-gtags-go-backwards))
  :delight 'counsel-gtags-mode)

(provide 'ivy-setup)
