;; -*- lexical-binding: t; -*-

(unbind-key "s-t")
(global-set-key (kbd "C-,")     'duplicate-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-x") 'kill-current-buffer)

(provide 'setup-keymaps)
;;; setup-keymaps.el ends here
