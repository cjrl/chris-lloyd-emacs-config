;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-fix-flycheck-ld-library-path t)

(straight-use-package '(cond-let :type git :host github :repo "tarsius/cond-let"))
(require 'cond-let)
(straight-use-package '(transient :type git :host github :repo "magit/transient"))
;; (eval-after-load "transient" '(debug))

(use-package org)

(use-package solarized-theme
  :straight t
  :config
  (load-theme 'solarized-dark t))


(org-babel-load-file "~/.emacs.d/config.org")

;; Lean4 Performance Configuration for High-End Machines
;; Optimized for 32GB RAM + powerful GPU
;; Place this in your init.el or load it separately

;;; ============================================================================
;;; EMACS CORE PERFORMANCE SETTINGS
;;; ============================================================================

;; Increase garbage collection threshold dramatically (default is ~800KB)
;; Set to 100MB during normal operation, 1GB during startup
(setq gc-cons-threshold 100000000) ; 100MB
(setq gc-cons-percentage 0.6)

;; Increase during startup, then reset
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.6)))

;; Increase the amount of data Emacs reads from processes
(setq read-process-output-max (* 1024 1024 4)) ; 4MB (default is 4KB)

;; Maximum output from a single LSP response
(setq lsp-response-timeout 60) ; Give LSP more time for complex operations

;;; ============================================================================
;;; LSP-MODE PERFORMANCE SETTINGS
;;; ============================================================================

(with-eval-after-load 'lsp-mode
  ;; Increase LSP file watch limit
  ;; (setq lsp-file-watch-threshold 10000)
  
  ;; Disable some expensive features that may not be needed
  ;; (setq lsp-enable-file-watchers t) ; Keep enabled but with high threshold
  ;; (setq lsp-enable-text-document-color nil) ; Disable unless you need it
  
  ;; Aggressive lens and semantic tokens
  (setq lsp-lens-enable t)
  ;; (setq lsp-semantic-tokens-enable t)
  
  ;; Speed up modeline updates
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-headerline-breadcrumb-enable t)
  
  ;; Increase completion timeout
  (setq lsp-completion-provider :none) ; We'll configure company separately
  ;; (setq lsp-completion-show-detail t)
  ;; (setq lsp-completion-show-kind t)
  
  ;; Aggressive idle delays (make LSP update faster)
  (setq lsp-idle-delay 0.5) 
  
  ;; Enable aggressive logging for debugging (disable if not needed)
  (setq lsp-log-io nil) ; Set to t only for debugging
  
  ;; Increase workspace settings
  (setq lsp-restart 'auto-restart)
  (setq lsp-session-file (expand-file-name ".lsp-session-v1" user-emacs-directory))
  
  ;; Enable all features - we have the resources
  ;; (setq lsp-enable-snippet t)
  ;; (setq lsp-enable-folding t)
  ;; (setq lsp-enable-imenu t)
  ;; (setq lsp-enable-on-type-formatting t)
  ;; (setq lsp-enable-indentation t))
  )
;;; ============================================================================
;;; LEAN4-MODE SPECIFIC SETTINGS
;;; ============================================================================

;; (setq lean4-extra-arguments '("-M" "24000" "-j" "12"))

  (with-eval-after-load 'lean4-mode
    ;; Increase Lean server memory limits
    ;; Lean uses LEAN_MEMORY environment variable (in MB)
    ;; (setenv "LEAN_MEMORY" "8192") ; 8GB for Lean server
    
    ;; Number of parallel worker threads (use more on powerful machines)
    ;; (setenv "LEAN_THREADS" "8") ; Adjust based on your CPU cores

    ;; (setenv "LEAN_MEMORY" "16000")  ;; 16GB in MB
    ;; (setenv "LEAN_THREADS" "8"))
    (setq lean4-memory-limit 24000)
    
    ;; Aggressive info buffer updates
    ;; (setq lean4-info-window-height-fraction 0.3)
    ;; (setq lean4-idle-delay 0.1) ; Update goal buffer very quickly
    
    ;; Enable all Lean4 features
    ;; (setq lean4-show-lake-env-when-opening-files t)
    
    ;; Increase typeinfo delay
    ;; (setq lean4-typeinfo-delay 0.1) ; Show type info quickly
    
    ;; LSP settings specific to Lean4
    (setq lsp-lean4-lake-env-timeout 60) ; Give lake more time
    (setq lsp-lean4-server-log-level "warning") ; Reduce logging overhead
    )

;;; ============================================================================
;;; COMPANY-MODE PERFORMANCE SETTINGS
;;; ============================================================================

  (with-eval-after-load 'company
    ;; Aggressive completion settings
    (setq company-minimum-prefix-length 2) ; Start completing after 1 char
    ;; (setq company-idle-delay 0.0) ; Show completions immediately
    ;; (setq company-tooltip-idle-delay 0.0) ; Show tooltip immediately
    
    ;; Increase candidate limits
    ;; (setq company-tooltip-limit 5) ; Show more candidates
    ;; (setq company-candidates-length 1000) ; Cache more candidates
    
    ;; Performance optimizations
    (setq company-async-timeout 10) ; Give async backends more time
    (setq company-async-wait 0.1)
    
    ;; Better completion experience
    (setq company-tooltip-align-annotations t)
    (setq company-require-match nil)
    (setq company-selection-wrap-around t)
    
    ;; Transformers for better sorting
    ;; (setq company-transformers '(company-sort-by-occurrence
    ;; 				 company-sort-by-backend-importance))
    
    ;; Make company faster by disabling some features if needed
    ;; (setq company-dabbrev-downcase nil)
    ;; (setq company-dabbrev-ignore-case t)
    )

;;; ============================================================================
;;; FLYCHECK PERFORMANCE (if using)
;;; ============================================================================

  (with-eval-after-load 'flycheck
    ;; Aggressive checking
    (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
    (setq flycheck-idle-change-delay 2) ; Check after 0.5s of idle
    (setq flycheck-idle-buffer-switch-delay 2)
    
    ;; Display errors faster
    (setq flycheck-display-errors-delay 2))

;;; ============================================================================
;;; ADDITIONAL EMACS OPTIMIZATIONS
;;; ============================================================================

;; Faster font rendering
;; (setq inhibit-compacting-font-caches t)

;; Reduce cursor lag
(setq auto-window-vscroll nil)

;; Faster scrolling
;; (setq fast-but-imprecise-scrolling t)
;; (setq scroll-conservatively 101)
;; (setq scroll-margin 0)
;; (setq scroll-preserve-screen-position t)

;; Don't use dialog boxes (they're slow)
(setq use-dialog-box nil)

;; Increase undo limits (we have RAM to spare)
(setq undo-limit 800000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 120000000)

;; Bidirectional text performance (if not using RTL languages)
;; (setq-default bidi-display-reordering nil)
;; (setq bidi-inhibit-bpa t)

;;; ============================================================================
;;; LEAN4 ADDITIONAL TWEAKS
;;; ============================================================================

;; If you're using eglot instead of lsp-mode, use these:
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(lean4-mode . ("lake" "serve" "--")))
;;   (setq eglot-events-buffer-size 0) ; Disable events buffer for performance
;;   (setq eglot-sync-connect nil)
;;   (setq eglot-autoshutdown t))

;;; ============================================================================
;;; NATIVE COMPILATION (if using Emacs 28+)
;;; ============================================================================

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-speed 2)) ; Maximum optimization

;;; ============================================================================
;;; LEAN-SPECIFIC LAKE/BUILD SETTINGS
;;; ============================================================================

;; Set additional environment variables for maximum Lean performance
(setenv "LEAN_CC" "clang") ; Use clang if available (often faster)
(setenv "LEAN_CXX" "clang++")

;; If you want Lean to use more aggressive compilation
;; (setenv "LEAN_OPTS" "-Dpp.deepTerms=true -Dpp.deepTypes=true")

;;; ============================================================================
;;; DEBUGGING HELPERS
;;; ============================================================================

;; Uncomment these to diagnose performance issues:
;; (setq lsp-log-io t) ; Log all LSP communication
;; (setq lean4-server-log-level "trace") ; Maximum Lean logging

;; Add this to see GC statistics
;; (setq garbage-collection-messages t)

;;; ============================================================================
;;; HOOK EXAMPLE
;;; ============================================================================

;; Example hook to ensure settings are applied when opening Lean files
(add-hook 'lean4-mode-hook
          (lambda ()
            ;; (lsp-deferred)
            ;; Ensure company is active
            (company-mode 1)
            ;; Local performance settings
	    ))

(with-eval-after-load 'lean4-mode
  (defun lean4--server-cmd ()
    "Return Lean server command with memory and thread limits."
    (condition-case nil
	(if (string-version-lessp 
	     (car (process-lines (lean4-get-executable "lake") "--version")) 
	     "3.1.0")
	    ;; Old lake or no lake: use lean --server directly
	    `(,(lean4-get-executable lean4-executable-name) 
	      "--server" 
	      "-M" ,(number-to-string lean4-memory-limit)
	      "-j" "12")
	  ;; New lake: use lake serve with -- to pass args to lean server
	  `(,(lean4-get-executable "lake") 
	    "serve" 
	    "--"
	    "-M" ,(number-to-string lean4-memory-limit)
	    "-j" "12"))
      (error `(,(lean4-get-executable lean4-executable-name) 
	       "--server" 
	       "-M" ,(number-to-string lean4-memory-limit)
	       "-j" "12")))))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
