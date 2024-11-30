;; -*- lexical-binding: t; -*-

(defun my--eshell-other-window ()
  "Open a `eshell' in a new window"
  (interactive)
  (let ((buf (project-eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(defun my--keyboard-quit-dwim ()
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)


(unbind-key "s-t")
(global-set-key (kbd "C-g")     'my--keyboard-quit-dwim)
(global-set-key (kbd "C-,")     'duplicate-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-x") 'kill-current-buffer)
(global-set-key (kbd "C-x p E") 'my--eshell-other-window)
(global-set-key (kbd "C-c n c") 'org-capture)
(global-set-key (kbd "C-c n a") 'org-agenda)

(provide 'setup-keymaps)
;;; setup-keymaps.el ends here
