(defun custom-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)

(require 'elixir-mode)
(require 'alchemist)
(require 'flycheck-mix)

(defun my/elixir-mode-hook ()
  (alchemist-mode +1)
  (yas/minor-mode +1)
  (smartparens-mode +1)
  (flyspell-prog-mode))

(defun my/erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-go-to-jump-back))

(add-hook 'elixir-mode-hook 'my/elixir-mode-hook)
(add-hook 'erlang-mode-hook 'my/erlang-mode-hook)

(defun my/alchemist-test-report-mode-hook ()
  (text-scale-set -2))
(add-hook 'alchemist-test-report-mode-hook 'my/alchemist-test-report-mode-hook)

(define-key alchemist-test-report-mode-map "h" #'highlight-phrase)

(defun my/alchemist-iex-mode-hook ()
  (text-scale-set -2)
  (company-mode -1))
(add-hook 'alchemist-iex-mode-hook 'my/alchemist-iex-mode-hook)

;; scratch pad buffer
(defun my/alchemist-create-scratch-buffer ()
  "Open a buffer in elixir/alchemist mode; use `C-c a v q` for
evaluating the expressions in Elixir"
  (interactive)
  (switch-to-buffer "*elixir scratch*")
  (elixir-mode)
  (alchemist-mode))
;; bind the scratch pad to C-c a i s, why 'i'? Dunno, it has something to do with
;; evaluating stuff, I guess.
(define-key alchemist-mode-keymap (kbd "i s") 'my/alchemist-create-scratch-buffer)

(defun mg/alchemist-run-credo-on-project ()
  "Run credo on project"
  (interactive)
  (alchemist-mix-execute "credo"))
(define-key alchemist-mode-keymap (kbd "p c") 'my/alchemist-run-credo-on-project)

;; Open a new line with a pipe on control return
(defun my/open-new-line-with-pipe ()
     "open a new line with a pipe"
     (interactive)
     (progn
       (newline)
       (insert "|> ")
       (indent-according-to-mode)))
(define-key elixir-mode-map [(control return)] #'my/open-new-line-with-pipe)

;; require my elixir yasnippets
;; (require 'mg-elixir-snippets)

(provide 'setup-elixir-mode)

;;; elixir-setup.el ends here
