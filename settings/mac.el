(require 'dash)

;; mac friendly font
(when window-system
  (setq my/default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (setq my/presentation-font "-apple-Monaco-medium-normal-normal-*-21-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font my/default-font))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")


;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Open files
(defun mac-open-current-file ()
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(global-set-key (kbd "C-c C-S-o") 'mac-open-current-file)


;; fix osx weirdness with magit avatars
(setq-default magit-revision-use-gravatar-kludge t)

(provide 'mac)
