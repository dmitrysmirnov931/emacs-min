;; -*- lexical-binding: t; -*-

(use-package affe
  :ensure t
  :defer t
  :bind (("M-s d" . affe-find)
	 ("M-s g" . affe-grep))
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package consult
  :ensure t
  :defer t
  :bind (("C-x b"   . consult-buffer)
	 ("C-x r b" . consult-bookmark)
	 ("C-x p b" . consult-project-buffer)
	 ("M-g o"   . consult-outline)
	 ("M-g i"   . consult-imenu)
	 ("M-g I"   . consult-imenu-multi)
	 ("M-s e"   . consult-recent-file)
	 ("M-s G"   . consult-git-grep)
	 ("M-s r"   . consult-ripgrep)
	 ("M-s l"   . consult-line)
	 ("M-s L"   . consult-line-multi))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  :init
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package consult-eglot
  :ensure t
  :bind (("M-s s" . consult-eglot-symbols))
  :defer t
  :after consult eglot)

(provide 'setup-consult)
;;; setup-consult.el ends here
