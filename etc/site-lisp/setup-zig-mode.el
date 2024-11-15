;; -*- lexical-binding: t; -*-

(use-package zig-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

(provide 'setup-zig-mode)
;;; setup-zig-mode.el ends here
