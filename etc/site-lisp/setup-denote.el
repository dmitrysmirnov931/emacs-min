;; -*- lexical-binding: t; -*-

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

(provide 'setup-denote)
;;; setup-denote.el ends here
