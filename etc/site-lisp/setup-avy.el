;; -*- lexical-binding: t; -*-

(use-package avy
  :ensure t
  :defer t
  :bind (("s-j" . avy-goto-char-timer))
  :custom
  (avy-all-windows 'all-frames))

(provide 'setup-avy)
;;; setup-avy.el ends here
