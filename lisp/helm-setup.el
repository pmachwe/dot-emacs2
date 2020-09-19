;;; Setting up helm and related packages.

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-s i" . helm-swoop)))

(provide 'helm-setup)
