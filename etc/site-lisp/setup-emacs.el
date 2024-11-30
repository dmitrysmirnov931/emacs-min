;; -*- lexical-binding: t; -*-

(use-package emacs
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (electric-pair-mode t)
  (which-key-mode t)
  (show-paren-mode t)
  (global-auto-revert-mode t)
  (savehist-mode t)
  (line-number-mode t)
  (column-number-mode t)
  (delete-selection-mode t)
  :custom
  (read-process-output-max (* 1024 1024))
  (history-delete-duplicates t)
  (line-spacing 1)
  (which-function-update-delay 0.5)
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (use-short-answers t)
  (flymake-fringe-indicator-position 'right-fringe)
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
  ;; backups
  (backup-directory-alist `(("." . (expand-file-name "saves" no-littering-var-directory))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  ;; scrolling
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position 't)
  (auto-window-vscroll nil)
  ;; isearch
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (search-whitespace-regexp ".*?")
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
  :config
  (setq-default mode-line-format (delq 'mode-line-modes mode-line-format))

  (put 'narrow-to-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil))

(provide 'setup-emacs)
;;; setup-emacs.el ends here
