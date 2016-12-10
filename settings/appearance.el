;; Disable menu tool and scroll bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Dont't beep. Dont't visible-bell.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Highlight current line
(global-hl-line-mode 1)

;; Show line number
(global-linum-mode 1)

;; Set font
(when window-system
  (setq my/default-font "-*-Monaco-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (setq my/presentation-font "-*-Monaco-normal-normal-normal-*-21-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font my/default-font))

;; Default theme
(defun use-presentation-theme ()
  (interactive)
  (when (boundp 'my/presentation-font)
    (set-face-attribute 'default nil :font my/presentation-font)))

(defun use-default-theme ()
  (interactive)
  (load-theme 'atom-one-dark t)
  (when (boundp 'my/default-font)
    (set-face-attribute 'default nil :font my/default-font)))

(defun toggle-presentation-mode ()
  (interactive)
  (if (string= (frame-parameter nil 'font) my/default-font)
      (use-presentation-theme)
    (use-default-theme)))


(global-set-key (kbd "C-<f9>") 'toggle-presentation-mode)

(use-default-theme)

;; Dont't defer screen updates when performing operations
(setq redisplay-dont-paust t)

;; org-mode colors
(setq org-todo-keyword-faces
      '(
	("INPR" . (:foreground "yellow" :weight bold))
	("DONE" . (:foreground "green" :weight bold))
	("IMPEDED" . (:foreground "red" :weight bold))
	))


;; Highlight matching parentheses when the poing is on them
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'appearance)
