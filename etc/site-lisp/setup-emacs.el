;; -*- lexical-binding: t; -*-

(use-package emacs
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (electric-pair-mode t)
  (which-key-mode t)
  (show-paren-mode t)
  (global-auto-revert-mode t)
  :custom
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (use-short-answers t)
  (flymake-fringe-indicator-position 'right-fringe)
  (display-line-numbers-type 'visual)
  (ffap-machine-p-known 'reject)
  (global-text-scale-adjust-resizes-frames nil)
  ;; show-paren
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  ;; compilation buffer
  (compilation-scroll-output t)
  (compilation-always-kill t)
  ;; minibuffer
  (resize-mini-windows t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; vc
  (version-control t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (vc-annotate-display-mode 'scale)
  ;; backups
  (backup-directory-alist `(("." . "~/.config/emacs/saves")))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  ;; scrolling
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  ;; isearch
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (search-whitespace-regexp ".*?")
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
  :config
  (put 'narrow-to-region 'disabled nil))

(provide 'setup-emacs)
;;; setup-emacs.el ends here
