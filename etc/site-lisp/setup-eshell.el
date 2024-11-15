;; -*- lexical-binding: t; -*-

(use-package eshell
  :preface
  (defun my--eshel-other-window ()
    "Open a `eshell' in a new window"
    (interactive)
    (let ((buf (eshell)))
      (switch-to-buffer (other-buffer buf))
      (switch-to-buffer-other-window buf)))
  :init
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (eshell/alias "ls" (concat ls "--color"))
	      (eshell/alias "ll" (concat ls "--AlFh --color"))
	      (eshell/alias "ff" "find-file $1")
	      (eshell/alias "e"  "find-file-other-window $1")
	      (eshell/alias "d"  "dired $1")))
  :config
  (setq eshell-scroll-to-bottom-on-input 'this)
  (setq eshell-scroll-to-bottom-on-output nil)
  (setq eshell-prefer-list-functions nil)
  (setq eshell-error-if-no-glob t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)
  (setq eshell-destroy-buffer-when-process-dies t))

(provide 'setup-eshell)
;;; setup-eshell.el ends here
