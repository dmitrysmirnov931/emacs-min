;; -*- lexical-binding: t; -*-

(use-package diff-hl
  :ensure t
  :defer t
  :hook ((find-file . global-diff-hl-mode)
	 (find-file . diff-hl-flydiff-mode)
	 (find-file . diff-hl-margin-mode)
	 (magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh)
	 (dired-mode . diff-hl-dir-mode))
  :custom
  (diff-hl-side 'left)
  :config
  (global-diff-hl-mode))

(provide 'setup-diff-hl)
;;; setup-diff-hl.el ends here
