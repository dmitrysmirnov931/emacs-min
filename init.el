(require 'org)
(require 'org-tempo)
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-formats
   '((mark modified read-only locked " " (name 18 18 :left :elide) " "
	   (size 9 -1 :right) " " (mode 16 16 :left :elide) " "
	   project-file-relative)))
 '(package-selected-packages
   '(affe avy breadcrumb cape comment-tags consult-eglot corfu diminish
	  eldoc-box embark-consult evil-collection evil-commentary
	  ibuffer-project magit marginalia no-littering orderless
	  org-bullets org-modern org-super-agenda python-pytest pyvenv
	  vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
