;; -*- lexical-binding: t; -*-

(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-cycle t))

(use-package vertico-directory
  :defer t
  :bind (:map vertico-map
	      ("RET"   . vertico-directory-enter)
	      ("DEL"   . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(provide 'setup-vertico)
;;; setup-vertico.el ends here
