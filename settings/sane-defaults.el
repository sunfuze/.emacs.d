;; Allow cutting and pasting uses the system clipboard
(setq x-select-enable-clipboard t)

;; Auto refresh buffers when file has been changed outside
(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Encode as UTF-8
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Automatically copy text slected with the mouse
(setq mouse-drag-copy-region t)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Set line width to 80 characters
(setq fill-column 80)
(set-default 'fill-column 80)

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration by Ctrl-c left/right
(winner-mode 1)

;; Show empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Move through camel case words
(global-subword-mode 1)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Allow minibuffer commands while in minibuffer
(setq enable-recursive-minibuffers t)

;; Make less gc
(setq gc-cons-threshold 20000000)

;; Represent undo-history as an actual tree 
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Sentences do not need double spaces to end
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; No electric indent
(setq electric-indent-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Create parent directories if they do not exist
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
	       (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Add hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)

;; Smart show paren
(defadvice show-paren-function (around fix-show-paren-function activate)
   "Highlight enclosing parens."
   (cond ((looking-at-p "\\s(") ad-do-it)
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     ad-do-it))))
;; Set expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Set multiple cursors

;; Rename file and buffer
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(provide 'sane-defaults)