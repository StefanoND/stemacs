;;; configs.el -*-coding: utf-8-unix; lexical-binding: t; -*-

(require 'cl-libify)

;; DO NOT MODIFY ANITHING ABOVE THIS LINE

;; If using Chemacs2 and Native Compilation, uncomment this line
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; Disable startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room

(menu-bar-mode -1)      ; Disable menu bar

;; Set up visible bell
(setq visible-bell t)

;; Set font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120)

;; Set theme
;; (load-theme 'tango-dark)
(load-theme 'catppuccin :no-confirm)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; DO NOT MODIFY ANITHING BELOW THIS LINE

(provide 'configs)

;;; configs.el ends here
