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
(load (locate-user-emacs-file "configs.el") nil :nomessage)
(load (locate-user-emacs-file "packages.el") nil :nomessage)
(load (locate-user-emacs-file "custom.el") nil :nomessage)

;; DO NOT MODIFY ANITHING ABOVE THIS LINE



;; DO NOT MODIFY ANITHING BELOW THIS LINE

;;; init.el ends here
