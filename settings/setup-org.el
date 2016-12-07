(require 'org)
(setq org-src-fontify-natively t)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map (kbd "C-c n") 'org-capture)

(setq org-agenda-files '("~/Dropbox/agenda"))
(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'setup-org)
