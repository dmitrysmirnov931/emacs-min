;;; -*- lexical-binding: t -*-

(let ((original-gc-cons-threshold gc-cons-threshold))
    (setq
     gc-cons-threshold most-positive-fixnum
     read-process-output-max (* 1024 1024 4) ; 4mb
     which-func-update-delay 1.0
     use-file-dialog nil
     use-dialog-box nil
     inhibit-startup-screen t
     inhibit-compacting-font-caches t
     inhibit-startup-echo-area-message user-login-name
     inhibit-startup-buffer-menu t
     inhibit-x-resources t
     initial-buffer-choice nil
     frame-inhibit-implied-resize t
     initial-major-mode 'fundamental-mode
     message-log-max 16384
     package-enable-at-startup t
     use-package-enable-imenu-support 1
     load-prefer-newer noninteractive)
    (set-face-attribute 'default nil :family "Berkeley Mono" :height 170 :weight 'normal)
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (advice-add #'display-startup-screen :override #'ignore)
    (add-hook 'emacs-startup-hook
              (lambda nil
                  (setq gc-cons-threshold original-gc-cons-threshold))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'emacs-startup-hook
          (lambda nil
              (when (and custom-file
                         (file-exists-p custom-file))
                  (load custom-file nil 'nomessage))))

;; Native-comp
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))
