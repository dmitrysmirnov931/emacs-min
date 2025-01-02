;; -*- lexical-binding: t; -*-

(use-package eldoc-box
  :ensure t
  :after eglot
  :custom
  (eldoc-idle-delay 1.0)
  (eldoc-box-hover-mode nil)
  :config
  (add-hook 'haskell-mode-hook #'eldoc-box-hover-at-point-mode t))

(provide 'setup-eldoc)
;;; setup-eldoc.el ends here
