;;; Setting up auto-complete mode.

(use-package auto-complete
  :config
  (setq ac-use-menu-map t)
  (add-to-list 'ac-dictionary-directories 
               (expand-file-name "~/.emacs.d/elpa/auto-complete-20150618.1949/dict"))
  (setq ac-comphist-file
        (expand-file-name "~/.emacs.d/ac-comphist.dat"))
  (ac-config-default)                                      
  (ac-flyspell-workaround) ; auto-complete does not work with flyspell
  (add-to-list 'ac-modes 'shell-mode)
  :diminish 'auto-complete-mode)

(provide 'auto-complete-setup)
