;; -*- lexical-binding: t; -*-

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

(provide 'setup-evil)
;;; setup-evil.el ends here
