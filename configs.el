;;; configs.el -*-coding: utf-8-unix; lexical-binding: t; -*-

(require 'cl-libify)

;; DO NOT MODIFY ANITHING ABOVE THIS LINE

(defvar stf/default-font-size 120)
(defvar stf/fixed-font-size 120)
(defvar stf/variable-font-size 120)

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

;; Default font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height stf/default-font-size)
;; Default font
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono" :height stf/fixed-font-size)
;; Default font
(set-face-attribute 'variable-pitch nil :font "Iosevka Nerd Font" :height stf/variable-font-size :weight 'regular)

;; Set theme
(load-theme 'catppuccin t)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Git status
(global-set-key (kbd "C-M-;") 'magit-status)

(setq org-startup-indented t)

;; DO NOT MODIFY ANITHING BELOW THIS LINE

(provide 'configs)

;;; configs.el ends here
