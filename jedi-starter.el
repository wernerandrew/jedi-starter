(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar local-packages
  '(auto-complete epc jedi projectile))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p nil) nil p)) packages)))

;; Adapted from:
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

;; Ensure that PATH is taken from shell
;; Set true if not relying on Jedi server install
(defvar jedi-config:use-system-python nil)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			    "[ \t\n]*$" ""
			      (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(add-hook
 'after-init-hook
 '(lambda ()
    ;; Auto-complete
    (require 'auto-complete-config)
    (ac-config-default)

    ;; Jedi
    (require 'jedi)
    ;; Only manually see in function tooltip
    (setq jedi:get-in-function-call-delay 10000000)
    ;; Hook up to autocomplete
    (add-to-list 'ac-sources 'ac-source-jedi-direct)
    (add-hook 'python-mode-hook 'jedi:setup)

    ;; Jedi customizations
    (defvar vcs-root-sentinel ".git")

    (defun buffer-project-root (buf sentinel)
      (let* ((buf-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
	     (project-root (vc-find-root buf-dir sentinel)))
	(if project-root
	    (expand-file-name project-root)
	  nil)))

    (defun my-custom-jedi-setup ()
      (let ((project-root (buffer-project-root (current-buffer) vcs-root-sentinel))
	    (default-jedi-server-command jedi:server-command))
	(when jedi-config:use-system-python
	  (set-exec-path-from-shell-PATH)
	  (make-local-variable 'jedi:server-command)
	  (set 'jedi:server-command
	       (list (executable-find "python") ;; may need help if running from GUI
		     (cadr default-jedi-server-command))))
	(when project-root
	  (make-local-variable 'jedi:server-args)
	  (set 'jedi:server-args (list "--sys-path" project-root)))))

    ;; Include active python and sys.path
    (add-hook 'python-mode-hook 'my-custom-jedi-setup)
    ;; some custom keybindings
    (add-hook 'python-mode-hook
	      '(lambda ()
		 (local-set-key (kbd "M-.") 'jedi:goto-definition)
		 (local-set-key (kbd "M-,") 'jedi:get-in-function-call)))
    ))
