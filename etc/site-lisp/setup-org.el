;; -*- lexical-binding: t; -*-

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)
  (add-hook 'org-mode-hook (lambda () (setq-local electric-pair-inhibit-predicate
						  `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t) (python . t))))

(provide 'setup-org)
;;; setup-org.el ends here
