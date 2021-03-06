;;; Setup for perforce
;;; TODO: Use locate-dominating-file function

(use-package p4
  :disabled t
  :hook prog-mode
  :commands (p4-edit p4-add p4-diff p4-sync p4-resolve p4-change p4-changes p4-revert)
  :config
  ;; Make sure to use the true filenames
  ;; otherwise perforce gets confused
  (setq find-file-visit-truename 't)
  (defun my/get-client-from-config()
    "Find .p4config upwards"
    (interactive)
    (let (file ref-path)
      (setq file (find-file-in-heirarchy (buffer-file-name) ".p4config"))
      (if (file-exists-p file)
          (progn 
                                        ;          (message "Found file %s" file)
            (find-file file)
            (goto-char 1)
            (search-forward "=")
            (setq ref-path (buffer-substring-no-properties (point) (line-end-position)))
            (message "Your ref-path is %s" ref-path)
            (kill-buffer (current-buffer))
            ref-path) ; return ref-path name
        nil)))

  (defun my/set-client()
    "Set P4CLIENT environment variable"
    (interactive)
    (let (ref-path)
      (progn
        (setq ref-path (my/get-client-from-config))
        (setenv "P4CLIENT" ref-path))))

  (require 'p4)
  (defadvice p4-call-command (before my-set-p4-client())
    (my/set-client))
  (ad-activate 'p4-call-command))


(provide 'p4-setup)
