;;; Setting up helm and related packages.

(use-package helm
  :bind(
        ("M-x" . helm-M-x)
        ("M-y" . helm-show-kill-ring)
        ("M-j" . helm-find-files)
        ("M-o" . helm-mini)
        ("C-x C-r" . helm-recentf)
        ("C-c /" . helm-imenu)
        :map helm-map
        ([tab] . helm-execute-persistent-action) ; rebind tab to run persistent action
        ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
        ("C-z" . helm-select-action)) ; list actions using C-z
  :config
  (progn
    (require 'helm-config)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-x h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-split-window-in-side-p       t ; open helm buffer inside current window
          helm-move-to-line-cycle-in-source t ; move to end or beginning when reaching top or bottom of source.
          helm-ff-search-library-in-sexp    t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-buffer-max-length nil)         ; do not clip buffer name in helm-mini (can also set to 60)

    (helm-mode 1)
    (helm-autoresize-mode t)
    ;; (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
    ;; (setq helm-apropos-fuzzy-match t)
    ;; (setq helm-lisp-fuzzy-completion t)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    ;; Beautify
    (set-face-attribute 'helm-selection nil
                    :background nil
                    :foreground nil
                    :bold t
                    :italic nil
                    :underline nil)

    (set-face-attribute 'helm-source-header nil
                        :background nil
                        :foreground nil
                        :underline nil
                        :bold nil
                        :height 1.3
                        :family "Hack"))
  :diminish helm-mode)

(use-package helm-swoop
  :bind ("M-s i" . helm-swoop))

(use-package helm-cscope
  :config
  (add-hook 'c-mode-common-hook 'helm-cscope-mode)
  (add-hook 'helm-cscope-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
              (local-set-key (kbd "M-@") 'helm-cscope-find-calling-this-function)
              (local-set-key (kbd "M-c") 'helm-cscope-find-this-symbol)
              (local-set-key (kbd "M-,") 'helm-cscope-pop-mark)
              (local-set-key (kbd "C-c C-f") 'helm-cscope-find-this-file))))

(provide 'helm-setup)
