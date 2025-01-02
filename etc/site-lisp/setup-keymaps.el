;; -*- lexical-binding: t; -*-

(defun my--eshell-other-window ()
  "Open a `eshell' in a new window"
  (interactive)
  (let ((buf (project-eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))


(unbind-key "s-t")
(global-set-key (kbd "C-,")     'duplicate-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-x") 'kill-current-buffer)
(global-set-key (kbd "C-x p E") 'my--eshell-other-window)
(global-set-key (kbd "C-c n c") 'org-capture)
(global-set-key (kbd "C-c n a") 'org-agenda)
(global-set-key (kbd "M-p")     'previous-history-element)
(global-set-key (kbd "M-n")     'next-history-element)
(global-set-key (kbd "C-c s")   'scratch-buffer)

(provide 'setup-keymaps)
;;; setup-keymaps.el ends here
