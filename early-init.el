;;; -*- lexical-binding: t -*-
(set-face-attribute 'default nil :family "Iosevka" :height 210 :weight 'normal)

(defvar my-gc-cons-threshold (* 16 1024 1024)
  "The value of 'gc-cons-threshold' after Emacs startup")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold my-gc-cons-threshold)))
(setq read-process-output-max (* 256 1024))
(setq which-func-update-delay 1.0)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Native-comp
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))
