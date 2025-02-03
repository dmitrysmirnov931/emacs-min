;; -*- lexical-binding: t; -*-

(use-package ultra-scroll
  :ensure t
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(provide 'setup-ultra-scroll)
;;; setup-ultra-scroll.el ends here
