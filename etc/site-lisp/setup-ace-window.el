;; -*- lexical-binding: t; -*-

(use-package ace-window
  :ensure t
  :defer t
  :bind (("s-o" . ace-window)
	 ("s-p" . ace-delete-other-windows))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'setup-ace-window)
;;; setup-ace-window.el ends here
