;; -*- lexical-binding: t; -*-

(use-package diff-hl
  :ensure t
  :defer t
  :hook ((find-file . global-diff-hl-mode)
	 (find-file . diff-hl-flydiff-mode)
	 (find-file . diff-hl-margin-mode)
	 (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
	 (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'left))

(provide 'setup-diff-hl)
;;; setup-diff-hl.el ends here
