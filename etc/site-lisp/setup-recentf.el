;; -*- lexical-binding: t; -*-

(use-package recentf
    :init (recentf-mode +1)
    :config
    (setq recentf-max-saved-items 500
          recentf-max-menu-items 15
          recentf-auto-cleanup 'never))

(provide 'setup-recentf)
;;; setup-recentf.el ends here
