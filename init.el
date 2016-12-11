;; Set personal infomation

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name "Fuze Sun"
      user-mail-address "sunfuze.1989@gmail.com")
;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Open emacs init file
(defun open-my-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f8>") 'open-my-init-file)

;; Disable backup file
(setq make-backup-files nil)

;; Set path to dependencies
(setq settings-dir
 (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)

;; Write backup files to own direcotry
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Make backups of files
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Set up package
(require 'setup-package)
;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(magit
     ;; Program languages
     ;;  Elixir related
     alchemist
     elixir-mode
     ;;  HTML/CSS
     emmet-mode
     htmlize
     css-eldoc
     tagedit
     rainbow-mode
     ;;  Javascript
     js2-mode
     js2-refactor
     nodejs-repl
     tern
     tern-auto-complete
     coffee-mode
     ;; Yaml
     yaml-mode
     ;; Emacs lisp
     elisp-slime-nav
     ;; Elisp utilities
     dash
     ;; Emacs enhance
     undo-tree
     smartparens
     guide-key
     fill-column-indicator
     browse-kill-ring
     hungry-delete
     smooth-scrolling
     expand-region
     highlight-escape-sequences
     visual-regexp
     multiple-cursors
     ;; Dired mode enhance
     dired-details
     ;; Ido mode enhance
     ido-vertical-mode
     ido-at-point
     ido-ubiquitous
     ;; Beauty org ui
     org-bullets
     ;; M-x and isearch alter
     swiper
     counsel
     ;; Shell command completion
     shell-command
     ;; Code checking
     flycheck
     flycheck-pos-tip
     flycheck-mix
     ;; Code snippets
     yasnippet
     ;; Services manager
     prodigy
     ;; Emacs color theme
     atom-one-dark-theme
     ;; Minor display
     diminish)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Mac Customize
(when is-mac (require 'mac))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Load apperance
(require 'appearance)

;; Setup env variables from user shell
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Setup Extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-html-mode)

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'elixir-mode '(require 'setup-elixir-mode))

;; load stuff on demand
(autoload 'auto-complete-mode "auto-complete" nil t)
(eval-after-load 'flycheck '(require 'setup-flycheck))

;; map files to modes
(require 'mode-mappings)

;; setup key bindings
(require 'key-bindings)

;; Elisp go-to-definition with m-. and back again with m-.
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

;; Service Manager
(require 'prodigy)
(global-set-key (kbd "C-x M-m") 'prodigy)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (coffee-mode rainbow-mode diminish zenburn-theme yaml-mode visual-regexp undo-tree tern-auto-complete tagedit smooth-scrolling smex smartparens shell-command prodigy org-bullets org nodejs-repl magit js2-refactor ido-vertical-mode ido-ubiquitous ido-at-point hungry-delete htmlize highlight-escape-sequences guide-key flycheck-pos-tip flycheck-mix flx-ido fill-column-indicator expand-region exec-path-from-shell emmet-mode elisp-slime-nav dired-details css-eldoc counsel browse-kill-ring atom-one-dark-theme alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
