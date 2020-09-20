;;; Setup for python
;;; Mostly taken from: https://realpython.com/emacs-the-best-python-editor
;;; TODO: use-package not accepting :ensure-system-package keyword, debug

(use-package elpy
  :hook python-mode)

(use-package python-mode 
  :mode ("\\.py\\'" . python-mode) 
  :interpreter ("python" . python-mode) 
  ;; :ensure-system-package
  ;; ((elpy . "pip install elpy")
  ;;  (jedi . "pip install jedi")
  ;;  (rope . "pip install rope")
  ;;  (black . "pip install black")
  ;;  (rope . "pip install rope"))
  :config
  ;; Enable elpy
  (elpy-enable)

  ;; Use IPython for REPL
  (cond
   ((executable-find "ipython")
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil)
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter"))
  ((executable-find "ipython")
   (setq python-shell-interpreter "ipython"))
  ((executable-find "ipython3")
   (setq python-shell-interpreter "ipython3"))))

;; Can update to flycheck from default flymake
(use-package flycheck
  :disabled
  :hook python-mode
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Jupyter notebook control within Emacs
;; TODO: Need to evaluate
(use-package ein
  :disabled
  :hook python-mode)

(provide 'python-setup)
