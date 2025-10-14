;; -*- lexical-binding: t; -*-
(org-babel-load-file "~/.emacs.d/config.org")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package lean4-mode
  :commands lean4-mode
  :straight (lean4-mode :type git :host github
                        :repo "leanprover-community/lean4-mode"
                        :files ("*.el" "data")))

;; (setq warning-minimum-level :emergency)

(use-package dash)
;; (use-package lsp-mode)
;; (setq lsp-ui-doc-show-with-cursor 't)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-delay 0)
(use-package magit-section)

(add-to-list 'load-path "~/.emacs.d/lean4-mode")
(require 'lean4-mode)

(my-leader-def 'lean4-mode-map
  "?" 'lsp-ui-doc-mode
  )


;;(require 'cl)
(defun quail-jk (key idx) 
  (let ((curpos (point)))
    (quail-delete-region)
    (evil-normal-state)
    (print curpos)
    (run-at-time 0.002 nil (lambda () (goto-char (- curpos 2))))
    (throw 'quail-tag nil))
  )


(use-package corfu
  :init
  (global-corfu-mode)
  :custom 
  (setq corfu-auto        t
	corfu-auto-delay  0 
	corfu-auto-prefix 2))

(use-package lsp-mode
  :after company)

(use-package lsp-ui)
;; (add-hook 'lean4-mode-hook (lambda () (quail-define-rules 
;; 				       ("jk" quail-jk))))
;; (use-package orderless
;;   :custom
;;   (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
;;   (completion-styles '(orderless partial-completion basic))
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles partial-completion)))))

;; (lambda ()
;;   (setq-local completion-category-defaults
;;               (assoc-delete-all 'lsp-capf completion-category-defaults)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(setq lsp-prefer-capf t)
(setq company-idle-delay 0)

(add-to-list 'company-backends 'company-elisp)
(add-to-list 'company-backends 'company-elisp)
(global-company-mode)

