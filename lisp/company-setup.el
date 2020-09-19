;;; Setting up company-mode

(use-package company
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  (company-transformers
   '(company-sort-by-backend-importance
     company-sort-prefer-same-case-prefix
     company-sort-by-occurrence))
  :config
  (company-tng-configure-default)
  :hook (after-init . global-company-mode))

(use-package company-prescient)

(use-package company-shell
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

(provide 'company-setup)
