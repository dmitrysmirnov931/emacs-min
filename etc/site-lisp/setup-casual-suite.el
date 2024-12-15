;; -*- lexical-binding: t; -*-

(use-package casual-suite
  :ensure t
  :config
  (keymap-set dired-mode-map "C-c c" #'casual-dired-tmenu)
  (keymap-set ibuffer-mode-map "C-c c" #'casual-ibuffer-tmenu)
  (keymap-set reb-mode-map "C-c c" #'casual-re-builder-tmenu))

(provide 'setup-casual-suite)
;;; setup-casual-suite.el ends here
