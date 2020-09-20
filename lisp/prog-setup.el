;;; Setup specific to different programming languages
;;; TODO: Break-up in separte files

(use-package emacs
  :init

  (defun my/common-prog-settings()
    "Setup common settings for all programming."
    (when window-system (linum-mode 1))
    (local-set-key (kbd "RET") 'newline-and-indent)
    (subword-mode 1)
    (setq-default indent-tabs-mode nil)
    (setq compilation-scroll-output t))

  (defun my/c-prog-settings()
    "Specific settings for C/C++."
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
    ;; Flycheck
    (setq flycheck-gcc-language-standard "c++11")
    (setq flycheck-clang-language-standard "c++11")
    ;; Avoid indentation at namespace
    (defconst my-cc-style
      '("cc-mode"
        (c-offsets-alist . ((innamespace . [0])))))

    (c-add-style "my-cc-mode" my-cc-style)
    (hs-minor-mode)
    (setq-default c-default-style "stroustrup"
                  c-basic-offset 2)
    ;; Need this otherwise not working
    ;; on work computer
    (setq c-basic-offset 4)
    )

  (defun my/elisp-prog-settings()
    "Specific settings for Elisp."
    ;; (require 'smartparens-config)
    ;; (sp-use-smartparens-bindings)
    ;; (smartparens-strict-mode)
    (prettify-symbols-mode))

  :hook ((prog-mode       . my/common-prog-settings)
         (c-mode          . my/c-prog-settings)
         (c++-mode        . my/c-prog-settings)
         (emacs-lisp-mode . my/elisp-prog-settings)))

(provide 'prog-setup)
