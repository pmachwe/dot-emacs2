;;; Setup the LSM mode

(use-package lsp-mode
  ;; :ensure-system-package
  ;; ((pyls . "pip install python-language-server[all]")
  ;;  (jedi-language-server . "pip install jedi-language-server"))
  :hook ((python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-to-list 'lsp-enabled-clients 'clangd)
  :commands lsp)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)

;; if you are helm user
(use-package helm-lsp
  :if (eq pm-completion-system 'helm)
  :commands helm-lsp-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package ivy-lsp
  :straight (lsp-ivy :host github :repo "emacs-lsp/lsp-ivy")
  :if (eq pm-completion-system 'ivy)
  :commands lsp-ivy-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol))

(use-package consult-lsp
  :straight (consult-lsp :type git :host github :repo "gagbo/consult-lsp" :protocol ssh)
  :if (eq pm-completion-system 'selectrum)
  :commands (consult-lsp-symbols consult-lsp-diagnostics consult-lsp-file-symbols)
  :config (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;;; NOTE: pyls is extremely slow atleast on windows
;;;       but jedi is reasonable
(use-package lsp-jedi
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(provide 'lsp-setup)
