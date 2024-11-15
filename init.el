;; -*- lexical-binding: t -*-

(setf (getenv "EDITOR") "emacsclient")

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

;; bootstrap use-package
(use-package use-package
    :ensure nil 
    :custom
    (use-package-verbose t)
    (use-package-expand-minimally t))

(use-package no-littering :ensure t)

(add-to-list 'load-path (expand-file-name "site-lisp" no-littering-etc-directory))
(require 'base)
(require 'setup-zig-mode)
(require 'setup-haskell-mode)
(require 'setup-eglot)
(require 'setup-diminish)
(require 'setup-modus-themes)
(require 'setup-diff-hl)
(require 'setup-spacious-padding)
(require 'setup-vi-tilde-fringe)
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
(require 'setup-keymaps)
