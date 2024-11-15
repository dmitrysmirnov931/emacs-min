;; -*- lexical-binding: t; -*-

(defun my-duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
	(line (let ((s (thing-at-point 'line t)))
		(if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(unbind-key "s-t")
(global-set-key (kbd "C-,") 'my-duplicate-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-x") 'kill-current-buffer)

(provide 'setup-keymaps)
;;; setup-keymaps.el ends here
