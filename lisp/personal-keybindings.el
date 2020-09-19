;;; Setup personal keybindings
;;; TODO: Setup a minor mode

(use-package emacs
  :bind (("M-h" . backward-kill-word)
         ("S-<f5>" . eshell)
         ("C-c M-!" . eshell-command)
         ("M-k" . kill-buffer-and-window)
         ("M-K" . kill-buffer)
         ("C-c ," . highlight-symbol-at-point)
         ("C-c C-," . unhighlight-regexp)
         ("C-M-o" . mode-line-other-window))
  
  :init
  ;; Preferred way as per documentation
  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Backspace-invokes-help.html
  (keyboard-translate ?\C-h ?\C-?)

  ;; Help-map is not a defun and hence does not get mapped
  ;; through the bind mapping above
  (global-set-key (kbd "C-c h") help-map)
  (global-set-key (kbd "C-c h i") 'emacs-index-search)
  (global-set-key (kbd "C-c h I") 'info))

(provide 'personal-keybindings)
