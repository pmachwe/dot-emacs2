;;; Setup for ediff mode

(use-package ediff
  :commands (ediff ediff-files)
  :config
  (defmacro csetq (variable value)
    `(funcall (or (get ',variable 'custom-set)
                  'set-default)
              ',variable ,value))
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain)
  (csetq ediff-split-window-function 'split-window-horizontally)
  (csetq ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

  (defcustom my/ediff-search-done nil
  "Save if ediff-search is done.")

(defcustom my/ediff-search-buf 1
  "Save which buffer was searched.")

;; For call-interactively part, the only way
;; to go back is to have a hook because it
;; does not wait for isearch to complete.
(defun my/ediff-search (buf repeat)
  (setq my/ediff-search-done t)
  (setq my/ediff-search-buf buf)
  (other-window buf)
  (if repeat
      (progn
        (search-forward-regexp isearch-string)
        (my/ediff-search-back))
    (call-interactively 'isearch-forward-regexp)))

(defun my/ediff-jump-to-diff (buf-no)
  (if (eq buf-no 2)
      (ediff-jump-to-difference (ediff-diff-at-point 'B))
    (ediff-jump-to-difference (ediff-diff-at-point 'A))))

(defun my/ediff-search-back()
  (if my/ediff-search-done
      (progn
        (other-window (- 3 my/ediff-search-buf))
        (my/ediff-jump-to-diff my/ediff-search-buf)))
  (setq my/ediff-search-done nil))

(add-hook 'isearch-mode-end-hook 'my/ediff-search-back)

(defun my/ediff-search-A ()
  (interactive)
  (my/ediff-search 1 nil))

(defun my/ediff-search-B ()
  (interactive)
  (my/ediff-search 2 nil))

(defun my/ediff-search-repeat()
  (interactive)
  (my/ediff-search my/ediff-search-buf t))

(defun my/ediff-setup-keys()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "sA" 'my/ediff-search-A)
  (define-key ediff-mode-map "sB" 'my/ediff-search-B)
  (define-key ediff-mode-map "sa" 'my/ediff-search-A)
  (define-key ediff-mode-map "sb" 'my/ediff-search-B)
  (define-key ediff-mode-map "ss" 'my/ediff-search-repeat))

(add-hook 'ediff-mode-hook 'my/ediff-setup-keys))

(provide 'ediff-setup)

