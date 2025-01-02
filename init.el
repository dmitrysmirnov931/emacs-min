;; -*- lexical-binding: t -*-

(setf (getenv "EDITOR") "emacsclient")
(set-face-attribute 'default nil :family "TX-02" :height 150)

;; bootstrap package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archive-priorities
      '(("gnu"           . 99)
        ("nongnu"        . 80)
        ("melpa-stable"  . 60)
        ("melpa"         . 40))
      package-install-upgrade-built-in t)
(when package-enable-at-startup (package-initialize))
(unless package-archive-contents (package-refresh-contents))
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; bootstrap use-package
(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-vc-prefer-newest t)
  (use-package-expand-minimally t))

(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode))

(use-package no-littering
  :ensure t
  :config
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(add-to-list 'load-path (expand-file-name "site-lisp" no-littering-etc-directory))
(require 'setup-emacs)
(require 'setup-keymaps)
(require 'setup-dired)
(require 'setup-recentf)
(require 'setup-eshell)
(require 'setup-zig-mode)
(require 'setup-haskell-mode)
(require 'setup-pyvenv)
(require 'setup-eglot)
(require 'setup-eldoc)
(require 'setup-themes)
(require 'setup-diff-hl)
(require 'setup-evil)
(require 'setup-vertico)
(require 'setup-corfu)
(require 'setup-consult)
(require 'setup-embark)
(require 'setup-wgrep)
(require 'setup-orderless)
(require 'setup-marginalia)
(require 'setup-avy)
(require 'setup-magit)
(require 'setup-ace-window)
(require 'setup-denote)
(require 'setup-org)
(require 'setup-spacious-padding)
;; (require 'setup-nerd-icons)
(require 'setup-casual-suite)
