;; Package housekeeping

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

;; This delightful bit adapted from:
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

(defvar jedi-config:use-system-python nil
  "Set to non-nil if not using jedi:install-server.
Will use system python and active environment for Jedi server.")

(defvar jedi-config:add-system-virtualenv t
  "Set to non-nil to also point Jedi towards the active $VIRTUAL_ENV, if any")

;; Ensure that PATH is taken from shell
;; Necessary on some environments if not relying on Jedi server install
;; Taken from: http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable

;; Small helper to scrape text from shell output
(defun get-shell-output (cmd)
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Helper to get virtualenv from shell
(defun get-active-virtualenv ()
  (get-shell-output "$SHELL -c 'echo $VIRTUAL_ENV'"))


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

    ;; Configuration helpers
    (defun buffer-project-root (buf sentinel)
      (let* ((buf-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
	     (project-root (vc-find-root buf-dir sentinel)))
	(if project-root
	    (expand-file-name project-root)
	  nil)))

    (defun jedi-config:setup-server-args ()
      ;; little helper macro
      (defmacro add-args (arg-list arg-name arg-value)
        `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
      ;; and here we go
      (let
          ((project-root
            (buffer-project-root (current-buffer) vcs-root-sentinel)))
        (make-local-variable 'jedi:server-args)
        (when project-root
          (message (format "Adding system path: %s" project-root))
          (add-args jedi:server-args "--sys-path" project-root))
        (when jedi-config:add-system-virtualenv
          (message (format "Adding system virtualenv: %s" (get-active-virtualenv)))
          (add-args jedi:server-args "--virtual-env" (get-active-virtualenv)))))

    ;; Use system python
    (defun jedi-config:maybe-use-system-python ()
      (when jedi-config:use-system-python
        (set-exec-path-from-shell-PATH)
        (make-local-variable 'jedi:server-command)
        (set 'jedi:server-command
             (list (executable-find "python") ;; may need help if running from GUI
                   (cadr default-jedi-server-command)))))

    ;; Set options
    ;; Buffer-specific server options
    (add-hook 'python-mode-hook
              (lambda ()
                (jedi-config:setup-server-args)
                (jedi-config:maybe-use-system-python)))

    ;; And custom keybindings
    (add-hook 'python-mode-hook
	      '(lambda ()
		 (local-set-key (kbd "M-.") 'jedi:goto-definition)
		 (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
                 (local-set-key (kbd "M-?") 'jedi:show-doc)
                 (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))
    ))
