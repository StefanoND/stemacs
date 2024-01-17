;;; init.el -*-coding: utf-8-unix; lexical-binding: t; -*-

;; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "config/"))

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar rag--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; set home and emacs directories
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory "dotfiles/emacs/.stemacs.d/"))

;; load all .el files
(load (locate-user-emacs-file "packages.el") nil :nomessage)
(load (locate-user-emacs-file "configs.el") nil :nomessage)
(load (locate-user-emacs-file "custom.el") nil :nomessage)

;; DO NOT MODIFY ANITHING ABOVE THIS LINE



;; DO NOT MODIFY ANITHING BELOW THIS LINE

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-indent visual-fill-column visual-fill forge smartparens rainbow-mode rainbow-delimiters ivy doom-modeline command-log-mode cl-libify catppuccin-theme))
 '(safe-local-variable-values '((projectile-project-run-cmd . "npm start"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
