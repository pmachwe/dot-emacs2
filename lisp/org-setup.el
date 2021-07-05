;;; Setup orgmode.

(use-package org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-indent-mode t)
  (setq org-use-sub-superscripts nil)
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)))
  (setq org-confirm-babel-evaluate nil) ;; Always evaluate
  (setq org-src-fontify-natively t) ;; Beautify within code blocks
  (setq org-src-tab-acts-natively t)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  ;; (require 'org-tempo)
  ;; (setq org-structure-template-alist
  ;;  '(("s" "#+BEGIN_SRC\n?\n#+END_SRC")
  ;;    ("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
  ;;    ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
  ;;    ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE")
  ;;    ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE")
  ;;    ("V" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM")
  ;;    ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER")
  ;;    ("C" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
  ;;    ("I" "#+INCLUDE: %file ?")))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.20))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.10))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
   '(org-document-title ((t (:underline t :weight bold :height 1.3)))))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(use-package htmlize
  :after org
  :hook org-mode)

(use-package org-tree-slide
  :hook org-mode
  :bind (:map org-mode-map
              ("[f8]" . org-tree-slide-mode)
              ("[S-f8]" . org-tree-slide-skip-done)))

(use-package org-bullets
  :disabled
  :after org
  :hook (org-mode . org-bullets-mode)
  :if window-system)

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package emacs
  :hook (org-mode . (lambda ()
                      (turn-on-auto-fill)
                      (org-indent-mode t)
                      (setq adaptive-fill-mode t))))

(provide 'org-setup)
