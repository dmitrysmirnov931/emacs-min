;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "Tangle and compile code-blocks ithe current buffer is init.org"
  (when (equal (buffer-file-name)
	       (expand-file-name (concat user-emacs-directory "init.org")))
    (let ((prog-mode-hook nil))
	(org-babel-tangle)
	(byte-compile-file (concat user-emacs-directory "init.el")))))
(add-hook 'after-save-hook 'tangle-init)

(require 'package)
(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-vc-prefer-newest t)
  (use-package-expand-minimally t)
  :config
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
                   (allow-no-window . t))))

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

(set-face-attribute 'default nil :family "PragmataPro" :height 170)
(set-face-attribute 'fixed-pitch nil :family "PragmataPro" :height 170)
(set-face-attribute 'variable-pitch nil :family "PragmataPro" :height 170)

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
  (delete-selection-mode t)
  (global-hl-line-mode t)
  (line-number-mode t)
  (column-number-mode t)
  :custom
  (truncate-lines t)
  (read-process-output-max (* 1024 1024))
  (history-delete-duplicates t)
  (line-spacing 1)
  (which-function-update-delay 0.5)
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (use-short-answers t)
  (ffap-machine-p-known 'reject)
  (global-text-scale-adjust-resizes-frames nil)
  (scroll-conservatively most-positive-fixnum)
  (eldoc-echo-area-use-multiline-p nil)
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
  ;; isearch
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (search-whitespace-regexp ".*?")
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
  (minibuffer-setup-hook . (lambda () (electric-pair-mode -1)))
  (minibuffer-exit-hook . (lambda () (electric-pair-mode t)))
  :config
  (setq frame-title-format
	(list (format "%s %%S: %%j " (system-name))
		'(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
  (setq-default mode-line-format (delq 'mode-line-modes mode-line-format))
  (put 'narrow-to-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired
  :hook (dired-mode . (lambda ()
                              (auto-revert-mode)
                              (hl-line-mode)))
  :config
  (setq dired-ls-F-marks-symlinks t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-dwim-target t
        dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil
        dired-create-destination-dirs 'ask
        dired-clen-confirm-killing-deleted-buffers nil
        dired-listing-switches "-lah --group-directories-first"
        dired-kill-when-opening-new-dired-buffer t))

(use-package recentf
  :init (recentf-mode +1)
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)))

(use-package eshell
  :config
  (setq eshell-scroll-to-bottom-on-input 'this
        eshell-scroll-to-bottom-on-output nil
        eshell-prefer-list-functions nil
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-destroy-buffer-when-process-dies t))

(use-package zig-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

(use-package pyvenv
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t
  :bind (("C-c h" . haskell-hoogle))
  :hook (haskell-mode-hook . interactive-haskell-mode)
  :custom
  (haskell-hoogle-command nil)
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode)))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions)
              ("C-c R" . xref-find-references))
  :hook ((python-mode python-ts-mode csharp-mode csharp-ts-mode zig-mode zig-ts-mode haskell-mode haskell-ts-mode ) . eglot-ensure)
  :custom
  (completion-category-overrides '((eglot (styles orderless))))
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-confirm-server-initiated-edits nil)
  ;; (eglot-ignored-server-capabilities '(:hoverProvider :inlayHintProvider))
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :config
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio" "--verbose")))
  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . ("dotnet" "/Users/dmitry.arthurovich.smirnov@bidbax.no/omnisharp/OmniSharp.dll" "-lsp")))
  (add-to-list 'eglot-server-programs '((zig-mode zig-ts-mode)       . ("zls"))))

(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-cycle t))

(use-package vertico-directory
  :defer t
  :bind (:map vertico-map
      	      ("RET"   . vertico-directory-enter)
      	      ("DEL"   . vertico-directory-delete-char)
      	      ("M-DEL" . vertico-directory-delete-word))
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-separator ?\s)
  (corfu-preview-current t)
  (corfu-preselect-first t)
  (corfu-history-mode 1)
  (corfu-count 15)			;
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-quit-no-match t corfu-quit-at-boundary t corfu-auto nil)))
  ;; Avoid press RET twice in shell
  ;; https://github.com/minad/corfu#completing-in-the-eshell-or-shell
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)

  ;; Completion in eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode))))

(use-package corfu-popupinfo
  :defer t
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package corfu-quick
  :defer t
  :after corfu
  :bind (:map corfu-map
              ("M-q" . corfu-quick-complete)
              ("C-q" . corfu-quick-insert)))

(use-package consult
  :ensure t
  :defer t
  :bind (("C-x b"   . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-g o"   . consult-outline)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ("M-s e"   . consult-recent-file)
         ("M-s G"   . consult-git-grep)
	 ("M-s d"   . consult-find)
	 ("M-s f"   . consult-flymake)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  :init
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package consult-eglot
  :ensure t
  :bind (("M-s s" . consult-eglot-symbols))
  :defer t
  :after consult eglot)

(use-package embark
  :ensure t
  :defer t
  :bind (("s-." . embark-act))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t 
  :defer t
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

(use-package wgrep
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :defer t
  :bind (("s-j" . avy-goto-char-timer))
  :custom
  (avy-style 'de-bruijn)
  (avy-all-windows 'all-frames))

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-x g" . magit-status)
   ("C-c b" . magit-blame))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package ace-window
  :ensure t
  :defer t
  :bind (("s-o" . ace-window)
         ("s-p" . ace-delete-other-windows)
	   ("s-[" . ace-delete-window))
  :custom
  (aw-minibuffer-flag t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package evil
  :ensure t
  :defer t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-Y-yank-to-eol t)
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-fu)
  (evil-mode-line-format nil)
  (evil-want-fine-undo t)
  :hook (after-init . evil-mode)
  :preface
  (defun my-save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :config
  (add-hook 'org-mode-hook
            (lambda () (setq evil-auto-indent nil)))
  (with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'my-save-and-kill-this-buffer))

(use-package evil-collection
  :ensure t
  :defer t
  :after evil
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-unimpaired-p nil))

(use-package evil-commentary
  :after evil
  :ensure t
  :defer t
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-escape
  :after evil
  :ensure t
  :defer t
  :hook (evil-mode . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1))

(use-package undo-fu
  :defer t
  :ensure t)

(use-package diff-hl
  :ensure t
  :defer t
  :hook ((find-file . global-diff-hl-mode)
	 (find-file . diff-hl-flydiff-mode)
	 (find-file . diff-hl-margin-mode)
	 (magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh)
	 (dired-mode . diff-hl-dir-mode))
  :custom
  (diff-hl-side 'left)
  :config
  (global-diff-hl-mode))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi-tinted t))

(use-package parrot
  :ensure t
  :custom
  (parrot-mode t))

(use-package ibuffer-project
  :ensure t
  :defer t
  :hook (ibuffer-hook . (lambda () (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
			  (unless (eq ibuffer-sorting-mode 'project-file-relative) (ibuffer-do-sort-by-project-file-relative)))))

(use-package breadcrumb
  :init (breadcrumb-mode t)
  :defer t
  :ensure t)

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)
  (add-hook 'org-mode-hook (lambda () (setq-local electric-pair-inhibit-predicate
                                                                    `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t) (python . t))))

(use-package denote
  :ensure t
  :defer t
  :bind (("C-c n n" . denote)
         ("C-c n r" . denote-region)
         ("C-c n f" . denote-open-or-create))
  :custom
  (denote-directory (expand-file-name "~/org/denote/"))
  (denote-save-buffers nil)
  (denote-known-keywords '("work" "read"))
  (denote-infer-keywords t)
  (denote-soft-keywords t)
  (denote-prompts '(title keywords))
  (denote-rename-confirmations '(rewrite-fron-matter modify-file-name))
  (denote-date-prompt-use-org-read-date t)
  (denote-date-format nil)
  (denote-backlinks-show-context t)
  :config
  (denote-rename-buffer-mode 1)
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                   '("n" "New note (with denote.el)" plain
                     (file denote-last-path)
                     #'denote-org-capture
                         :no-save t
                         :immediate-finish nil
                         :kill-buffer t
                         :jump-to-captured t))))

(defun my--eshell-other-window ()
  "Open a `eshell' in a new window"
  (interactive)
  (let ((buf (project-eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window unbind)))


(unbind-key "s-t")
(global-set-key (kbd "s-t")     'vertico-suspend)
(global-set-key (kbd "C-,")     'duplicate-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-x") 'kill-current-buffer)
(global-set-key (kbd "C-x p E") 'my--eshell-other-window)
(global-set-key (kbd "C-c n c") 'org-capture)
(global-set-key (kbd "C-c n a") 'org-agenda)
(global-set-key (kbd "M-p")     'previous-history-element)
(global-set-key (kbd "M-n")     'next-history-element)
(global-set-key (kbd "C-c s")   'scratch-buffer)
