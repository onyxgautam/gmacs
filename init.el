(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(let ((file-name-handler-alist nil)
      (orgfile (expand-file-name "gmacs.org" user-emacs-directory))
      (elfile (expand-file-name "gmacs.el" user-emacs-directory)))
  (if (or (not (file-exists-p elfile))
          (file-newer-than-file-p orgfile elfile))
      (progn
        ;; (require 'org)
        (org-babel-load-file orgfile))
    (load-file elfile)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(let ((personalfile (expand-file-name "personal.el" user-emacs-directory)))
  (when (file-exists-p personalfile)
    (load-file personalfile)))
(put 'downcase-region 'disabled nil)
