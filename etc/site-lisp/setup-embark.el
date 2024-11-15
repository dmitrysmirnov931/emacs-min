;; -*- lexical-binding: t; -*-

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

(provide 'setup-embark)
;;; setup-embark.el ends here
