;; -*- lexical-binding: t; -*-

(use-package recentf
  :init (recentf-mode +1)
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-etc-directory))
  )

(provide 'setup-recentf)
;;; setup-recentf.el ends here
