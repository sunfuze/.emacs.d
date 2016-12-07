;; Turn off mouse interface eayly in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Open emacs init file
(defun open-my-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-my-init-file)

;; Disable backup file
(setq make-backup-files nil)

;; Set path to dependencies
(setq settings-dir
     
 (expand-file-name "settings" user-emacs-directory))
(setq third-parts-dir
      (expand-file-name "third-parts" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path third-parts-dir)

;; Add third parts projects to load path
(dolist (project (directory-files third-parts-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

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
     alchemist
     elixir-mode
     yaml-mode
     undo-tree
     smartparens
     dash
     emmet-mode
     htmlize
     css-eldoc
     nodejs-repl
     elisp-slime-nav
     flycheck
     flycheck-pos-tip
     flycheck-mix
     guide-key
     fill-column-indicator
     elisp-slime-nav
     browse-kill-ring
     hungry-delete
     prodigy
     atom-one-dark-theme
     js2-mode
     tern
     tern-auto-complete
     dired-details
     js2-refactor
     tagedit
     expand-region
     highlight-escape-sequences
     visual-regexp
     expand-region
     multiple-cursors
     flx-ido
     ido-vertical-mode
     ido-at-point
     ido-ubiquitous
     yasnippet
     shell-command
     swiper
     counsel
     smooth-scrolling
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


;; Lets start with a smattering of sanity
(require 'sane-defaults)


;; Setup env variables from user'shell
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-html-mode)

(require 'prodigy)
(global-set-key (kbd "C-x M-m") 'prodigy)

;; Default setup of smartparens-config
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(dolist (it '(css-mode-hook
	      js-mode-hook
	      emmet-mode
	      java-mode
	      markdown-mode
	      elixir-mode))
  (add-hook it 'turn-on-smartparens-mode))

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))




;; language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'elixir-mode '(require 'setup-elixir-mode))


;; load stuff on demand
(autoload 'auto-complete-mode "auto-complete" nil t)
(eval-after-load 'flycheck '(require 'setup-flycheck))

;; Load stuff on demand
(eval-after-load 'flycheck '(require 'setup-flycheck))

;; map files to modes
(require 'mode-mappings)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c r") 'vr/replace)


;; fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)
(global-set-key (kbd "C-c y") 'browse-kill-ring)


;; smart m-x is smart
;; (require 'smex)
;; (smex-initialize)

;; setup key bindings
(require 'key-bindings)

;; Misc
(when is-mac (require 'mac))

;; Load apperance
(require 'appearance)

;; elisp go-to-definition with m-. and back again with m-.
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
