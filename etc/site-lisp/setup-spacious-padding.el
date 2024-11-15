;; -*- lexical-binding: t; -*-

(use-package spacious-padding
  :ensure t
  :defer t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(
     :internal-border-width 15
     :header-line-width 4
     :mode-line-width 4
     :tab-width 4
     :right-divider-width 30
     :scroll-bar-width 8
     :fringe-width 8))
  (spacious-padding-subtle-mode-line
   '(
     :mode-line-active 'default
     :mode-line-inactive vertical-border)))

(provide 'setup-spacious-padding)
;;; setup-spacious-padding.el ends here
