;;; -*- lexical-binding: t -*-
(use-package emacs
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (which-key-mode t)
  (global-hl-line-mode t)
  (column-number-mode t)

  (setq show-paren-delay 0)
  (show-paren-mode t)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (setq ring-bell-function 'ignore)
  (setq frame-resize-pixelwise t)
  (setq use-short-answers t)
  (setq inhibit-startup-screen t)
  (setq enable-recursive-minibuffers t)
  (setq scroll-margin 0
	scroll-conservatively 101
	scroll-preserve-screen-position t
	auto-window-vscroll nil)

  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq flymake-fringe-indicator-position 'right-fringe)

  (setq backup-directory-alist `(("." . "~/.config/emacs/saves"))
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t)

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

  (put 'narrow-to-region 'disabled nil))


(use-package no-littering
  :ensure t)

(use-package base16-theme
  :ensure
  :config
  (load-theme 'base16-sandcastle t))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
	evil-undo-system 'undo-fu
        evil-mode-line-format nil)
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
  :ensure t)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :ensure t
  :config (evil-commentary-mode t))

(use-package evil-escape
  :after evil
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.1)
  (evil-escape-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package vertico
  :ensure t
  :config
  (setq vertico-count 10
        vertico-cycle t)
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico-map
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package corfu
  :ensure t
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-separtor ?\s)
  :init
  (global-corfu-mode))

(use-package affe
  :ensure t
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package consult
  :ensure t
  :bind (("C-c M-x" . consult-mode-command)
         ([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)
         ("C-x r b" . consult-bookmark)
	 ("C-x p b" . consult-project-buffer)
         ("M-g g" . consult-goto-line)
	 ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . affe-find)
         ("M-s c" . consult-locate)
         ("M-s g" . affe-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t 
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil))

(use-package eldoc-box
  :ensure t)

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c R" . xref-find-references))
  :hook ((( python-mode python-ts-mode ) . eglot-ensure))
  :config
  (setq completion-category-overrides '((eglot (styles orderless))))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  ;; (add-to-list 'eglot-ignored-server-capabilities :hoverProvider)
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eldoc-echo-area-use-multiline-p nil))

(use-package consult-eglot
  :ensure t
  :bind (:map eglot-mode-map
	      ("C-c s" . consult-eglot-symbols))
  :after consult eglot)

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode t))

(use-package avy
  :ensure t
  :bind
  (("s-j" . avy-goto-char-timer))
  :config
  (setq avy-all-windows 'all-frames))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
	'(
	  :internal-border-width 15
	  :header-line-width 4
	  :mode-line-width 4
	  :tab-width 4
	  :right-divider-width 30
	  :scroll-bar-width 8
	  :fringe-width 8)
	spacious-padding-subtle-mode-line
	'(
	  :mode-line-active 'default
	  :mode-line-inactive vertical-border))
  (spacious-padding-mode 1))

(use-package multiple-cursors
  :ensure t
  :bind (("C-s-c C-s-c" . mc/edit-lines)
	 ("C->"         . mc/mark-next-like-this)
	 ("C-<"         . mc/mark-previous-like-this)
	 ("C-c C-<"         . mc/mark-next-like-this)))


;; (setq-default mode-line-format (delq 'mode-line-modes mode-line-format))
