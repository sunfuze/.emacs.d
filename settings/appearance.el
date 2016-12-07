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

;; Default theme
(defun use-default-theme ()
  (interactive)
  (when (boundp 'my/default-font)
    (set-face-attribute 'default nil :font my/default-font)))

;; Presentation theme
(defun use-presentation-theme ()
  (interactive)
  (when (boundp 'my/presentation-font)
    (set-face-attribute 'default nil :font my/presentation-font)))

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
;; Load theme
(load-theme 'atom-one-dark t)

;; Highlight matching parentheses when the poing is on them
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'appearance)
