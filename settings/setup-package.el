(require 'package)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))

(setq package-pinned-packages '())

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  (dolist (it packages)
    (when  (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

(defun require-package (package &optional min-version no-refresh)
  "Install given Package, optionally requiring MIN-VERSION.
if NO-REFRESH is no-nil, the available package lists will not be 
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
	(package-install package)
      (progn
	(package-refresh-contents)
	(require-package package min-version t)))))
(provide 'setup-package)
