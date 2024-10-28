;;; -*- lexical-binding: t -*-
(use-package emacs
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode -1)
  (electric-pair-mode t)
  (which-key-mode t)
  (show-paren-mode t)
  (global-auto-revert-mode t)
  (global-display-line-numbers-mode t)
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  (show-paren-delay 0)
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (use-short-answers t)
  (inhibit-startup-screen t)
  (flymake-fringe-indicator-position 'right-fringe)
  (display-line-numbers-type 'visual)
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
  ;; dired
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
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
  (when (file-exists-p custom-file)
    (load custom-file))

  (defun my-duplicate-line ()
    "Duplicate current line"
    (interactive)
    (let ((column (- (point) (point-at-bol)))
	  (line (let ((s (thing-at-point 'line t)))
		  (if s (string-remove-suffix "\n" s) ""))))
      (move-end-of-line 1)
      (newline)
      (insert line)
      (move-beginning-of-line 1)
      (forward-char column)))

  (global-set-key (kbd "C-,") 'my-duplicate-line)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-x C-x") 'kill-current-buffer)

  (put 'narrow-to-region 'disabled nil))

(use-package no-littering :ensure t)

(use-package diminish
  :ensure t
  :config
  (diminish 'which-key-mode)
  (diminish 'eldoc-mode))

(use-package kuronami-theme
  :ensure t
  :config
  (load-theme 'kuronami t))

(use-package diff-hl
  :ensure t
  :defer t
  :hook ((find-file . global-diff-hl-mode)
	 (find-file . diff-hl-flydiff-mode)
	 (find-file . diff-hl-margin-mode)
	 (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
	 (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'left))

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

(use-package vi-tilde-fringe
  :ensure t
  :defer t
  :hook (prog-mode-hook . vi-tilde-fringe-mode))

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

(use-package undo-fu
  :defer t
  :ensure t)

(use-package evil-collection
  :ensure t
  :diminish evil-collection-unimpaired-mode
  :defer t
  :after evil
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-unimpaired-p nil))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :ensure t
  :defer t
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :ensure t
  :defer t
  :hook (evil-mode . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1))

(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-cycle t))

(use-package vertico-directory
  :defer t
  :after vertico
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-separator ?\s)
  (corfu-preview-current t)
  (corfu-preselect-first t)
  (corfu-history-mode 1)
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-quit-no-match t
						     corfu-quit-at-boundary t
						     corfu-auto nil)))
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

(use-package affe
  :ensure t
  :defer t
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package wgrep
  :ensure t
  :defer t)

(use-package consult
  :ensure t
  :defer t
  :bind (("C-c M-x" . consult-mode-command)
	 ([remap Info-search] . consult-info)
	 ("C-x b" . consult-buffer)
	 ("C-x r b" . consult-bookmark)
	 ("C-x p b" . consult-project-buffer)
	 ("M-g o" . consult-outline)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ("M-s d" . affe-find)
	 ("M-s g" . affe-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi))
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

(use-package embark
  :ensure t
  :defer t
  :bind
  (("s-." . embark-act))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t 
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil))

(use-package zig-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
	      ("C-c r" . eglot-rename)
	      ("C-c R" . xref-find-references))
  :hook ((( python-mode python-ts-mode csharp-mode csharp-ts-mode zig-mode ) . eglot-ensure))
  :custom
  (completion-category-overrides '((eglot (styles orderless))))
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :inlayHintProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider))
  :config
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio" "--verbose")))
  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . ("dotnet" "~/omnisharp/OmniSharp.dll" "-lsp")))
  (add-to-list 'eglot-server-programs '((zig-mode zig-ts-mode)       . ("zls"))))

(use-package consult-eglot
  :ensure t
  :bind (:map eglot-mode-map
	      ("C-c s" . consult-eglot-symbols))
  :after consult eglot)

(use-package python-black
  :ensure t)

(use-package python-isort
  :ensure t)

(use-package ruff-format
  :ensure t)

(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

(use-package avy
  :ensure t
  :defer t
  :bind
  (("s-j" . avy-goto-char-timer))
  :custom
  (avy-all-windows 'all-frames))

(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status)
  :config (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package ace-window
  :ensure t
  :defer t
  :bind (("s-o" . ace-window)
	 ("s-p" . ace-delete-other-windows))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package denote
  :ensure t
  :defer t
  :bind (("C-c n n" . denote)
	 ("C-c n c" . denote-region))
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

(use-package eat
  :ensure t
  :config
  (setq eat-kill-buffer-on-exit t
	eat-enable-yank-to-terminal t
	eat-enable-directory-tracking t
	eat-enable-shell-command-history t
	eat-enable-shell-prompt-annotation t
	eat-term-scrollback-size nil)
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

;; (setq-default mode-line-format (delq 'mode-line-modes mode-line-format))
