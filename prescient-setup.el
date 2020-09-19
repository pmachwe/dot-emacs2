;;; Settings for prescient mode.
;;; This is used by both ivy and company.

(use-package prescient
  :custom
  (prescient-history-length 50)
  (prescient-save-file "~/.emacs.d/prescient-items")
  (prescient-filter-method '(fuzzy initialism regexp))
  :config
  (prescient-persist-mode 1))
