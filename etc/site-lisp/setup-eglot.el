;; -*- lexical-binding: t; -*-

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
	      ("C-c r" . eglot-rename)
	      ("C-c R" . xref-find-references))
  :hook (( python-mode python-ts-mode csharp-mode csharp-ts-mode zig-mode ) . eglot-ensure)
  :custom
  (completion-category-overrides '((eglot (styles orderless))))
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-ignored-server-capabilities '(:hoverProvider :inlayHintProvider))
  :config
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio" "--verbose")))
  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . ("dotnet" "~/omnisharp/OmniSharp.dll" "-lsp")))
  (add-to-list 'eglot-server-programs '((zig-mode zig-ts-mode)       . ("zls"))))

(provide 'setup-eglot)
;;; setup-eglot.el ends here
