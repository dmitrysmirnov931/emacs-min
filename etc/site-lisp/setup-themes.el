;; -*- lexical-binding: t; -*-

;; (use-package modus-themes
;;   :ensure t
;;   :custom
;;   (modus-themes-custom-auto-reload t)
;;   (modus-themes-disable-other-themes)
;;   (modus-themes-org-blocks 'gray-background)
;;   :config
;;   ;; (setq modus-themes-common-palette-overrides
;;   ;; 	'((variable                  fg-main)
;;   ;; 	  (builtin                   fg-main)
;;   ;; 	  (constant                  fg-main)))

;;   (modus-themes-load-theme 'modus-vivendi))

;; (use-package ef-themes
;;   :ensure t
;;   :config
;;   (ef-themes-load-theme 'ef-dark))

(use-package kaolin-themes
  :ensure t
  :config (load-theme 'kaolin-dark t))

(provide 'setup-themes)
;;; setup-themes.el ends here
