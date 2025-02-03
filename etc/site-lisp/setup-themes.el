;; -*- lexical-binding: t; -*-

;; (use-package modus-themes
;;   :ensure t
;;   :custom
;;   (modus-themes-custom-auto-reload t)
;;   (modus-themes-disable-other-themes)
;;   (modus-themes-org-blocks 'gray-background)
;;   :config
;;   (setq modus-themes-common-palette-overrides
;; 	'((fringe                      bg-main)
;; 	  (bultin                      fg-main)
;; 	  (constant                    fg-main)
;; 	  (variable                    fg-main)
;; 	  (name                        fg-main)
;; 	  (identifier                  fg-main)
;; 	  (docmarkup                   fg-main)
;; 	  (keyword                     blue)
;; 	  (type                        blue)
;; 	  (fnname                      blue)
;; 	  (string                      green)
;; 	  ))

;;   (setq modus-themes-italic-constructs nil
;; 	modus-themes-bold-constructs   t)

;;   (modus-themes-load-theme 'modus-vivendi))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

(provide 'setup-themes)
;;; setup-themes.el ends here
