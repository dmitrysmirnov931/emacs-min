;; -*- lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes)
  (modus-themes-org-blocks 'gray-background)
  :config
  (setq modus-themes-common-palette-overrides
	'((fringe                      bg-main)
	  (bultin                      fg-main)
	  (constant                    fg-main)
	  (variable                    fg-main)))
  (setq modus-themes-italic-constructs nil
	modus-themes-bold-constructs   nil)

  (modus-themes-load-theme 'modus-operandi-tinted))

(provide 'setup-themes)
;;; setup-themes.el ends here
