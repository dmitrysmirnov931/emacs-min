;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status)
  :config (add-hook 'with-editor-mode-hook #'evil-insert-state))

(provide 'setup-magit)
;;; setup-magit.el ends here
