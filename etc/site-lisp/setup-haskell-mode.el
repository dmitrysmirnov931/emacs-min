;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode)))

(provide 'setup-haskell-mode)
;;; setup-haskell-mode.el ends here
