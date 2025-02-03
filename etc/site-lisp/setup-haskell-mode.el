;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :ensure t
  :defer t
  :bind (("C-c h" . haskell-hoogle))
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-hoogle-command nil))

(provide 'setup-haskell-mode)
;;; setup-haskell-mode.el ends here
