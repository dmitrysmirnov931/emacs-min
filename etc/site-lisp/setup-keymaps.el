;; -*- lexical-binding: t; -*-

;; misc
(unbind-key "s-t")
(global-set-key (kbd "C-,")     'duplicate-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-x") 'kill-current-buffer)

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c R") 'xref-find-references))


(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "RET")   'vertico-directory-enter)
  (define-key vertico-map (kdb "DEL")   'vertico-directory-delete-char)
  (define-key vertico-map (kdb "M-DEL") 'vertico-directory-delete-word))

(with-eval-after-load 'consult
  (global-set-key (kbd "C-x b")   'consult-buffer)
  (global-set-key (kbd "C-x r b") 'consult-bookmark)
  (global-set-key (kbd "C-x p b") 'consult-project-buffer)
  (global-set-key (kbd "M-g o")   'consult-outline)
  (global-set-key (kbd "M-g i")   'consult-imenu)
  (global-set-key (kbd "M-g I")   'consult-imenu-multi)
  (global-set-key (kbd "M-s e")   'consult-recent-file)
  (global-set-key (kbd "M-s G")   'consult-git-grep)
  (global-set-key (kbd "M-s r")   'consult-ripgrep)
  (global-set-key (kbd "M-s l")   'consult-line)
  (global-set-key (kbd "M-s L")   'consult-line-multi))

(with-eval-after-load 'affe
  (global-set-key (kbd "M-s d") 'affe-find)
  (global-set-key (kbd "M-s g") 'affe-grep))

(with-eval-after-load 'consult-eglot
  (define-key eglot-mode-map (kbd "C-c s") 'consult-eglot-symbols))

(with-eval-after-load 'embark
  (global-set-key (kbd "s-.") 'embar-act))

(with-eval-after-load 'avy
  (global-set-key (kbd "s-j") 'avy-goto-char-timer))

(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status))

(with-eval-after-load 'ace-window
  (global-set-key (kbd "s-o") 'ace-window)
  (global-set-key (kbd "s-p") 'ace-delete-other-windows))

(with-eval-after-load 'denote
  (global-set-key (kbd "C-c n n") 'denote)
  (global-set-key (kbd "C-c n c") 'denote-region)
  (global-set-key (kbd "C-c n f") 'denote-open-or-create))

(provide 'setup-keymaps)
;;; setup-keymaps.el ends here
