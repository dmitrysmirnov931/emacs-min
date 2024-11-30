;; -*- lexical-binding: t; -*-

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

(provide 'setup-corfu)
;;; setup-corfu.el ends here
