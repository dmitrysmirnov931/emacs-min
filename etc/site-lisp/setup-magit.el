;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :config (add-hook 'with-editor-mode-hook #'evil-insert-state))

(provide 'setup-magit)
;;; setup-magit.el ends here
