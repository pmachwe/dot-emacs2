;;; Settings for selectrum.

(use-package selectrum
  :config
  (selectrum-mode +1)
  :bind (("M-j" . find-file)
         ("M-o" . switch-to-buffer)))

(use-package selectrum-prescient
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)

  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

(provide 'selectrum-setup)
