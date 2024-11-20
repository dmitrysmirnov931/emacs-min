;; -*- lexical-binding: t; -*-

(use-package dired
  :hook (dired-mode . (lambda ()
			(auto-revert-mode)
			(hl-line-mode)))
  :config
  (setq dired-ls-F-marks-symlinks t
	dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-dwim-target t
	dired-auto-revert-buffer t
	dired-hide-details-hide-symlink-targets nil
	dired-create-destination-dirs 'ask
	dired-clen-confirm-killing-deleted-buffers nil
	dired-listing-switches "-lah --group-directories-first"
	dired-kill-when-opening-new-dired-buffer t))

(use-package diredfl
  :ensure t
  :defer t
  :hook (dired-mode . diredfl-mode))

(provide 'setup-dired)
;;; setup-dired.el ends here
