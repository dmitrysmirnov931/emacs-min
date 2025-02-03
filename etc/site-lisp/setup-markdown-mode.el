;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(provide 'setup-markdown-mode)
;;; setup-markdown-mode.el ends here
