(setq package-enable-at-startup nil)

(setq inhibit-startup-message t)    ; Traditional way
(setq inhibit-startup-screen t)

(defun display-startup-echo-area-message ()
  (message ""))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


(setq frame-title-format "")
(add-to-list 'default-frame-alist '(drag-internal-border . t))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; If you are on Linux/GTK, you might also want to hide the tool-bar explicitly:
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(setq package-enable-at-startup nil)
