(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      inhibit-default-init t
      inhibit-message t
      inhibit-redisplay t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message ""
      inhibit-startup-message nil
      inhibit-startup-screen t
      inhibit-compacting-font-caches t
      inhibit-x-resources t
      load-prefer-newer noninteractive
      native-comp-async-report-warnings-errors nil
      native-comp-deferred-compilation t
      package-native-compile t
      site-run-file nil
      initial-major-mode 'fundamental-mode
      frame-inhibit-implied-resize t
      message-log-max 16384
      package-enable-at-startup t
      use-file-dialog nil
      use-dialog-box nil)

(push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)
(push '(fullscreen . maximized) initial-frame-alist)

(add-hook 'emacs-startup-hook
          (lambda nil
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold (* 1024 1024 64)
                  inhibit-redisplay nil
                  inhibit-message nil)))

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)
;; (add-to-list 'default-frame-alist '(undecorated-round . t))

(startup-redirect-eln-cache
 (convert-standard-filename
  (expand-file-name  "var/eln-cache/" user-emacs-directory)))
