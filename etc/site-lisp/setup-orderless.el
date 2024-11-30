;; -*- lexical-binding: t; -*-

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(provide 'setup-orderless)
;;; setup-orderless.el ends here
