;;; init.el --- Summary
;;; user init file
;;; -*- no-byte-compile: t -*-
;;; Commentary:
;;; This file mostly contains two things:
;;; 1. A list of required packages to be downloaded if missing
;;; and the settings for the 'package library
;;; 2. requires for other files that orchestrate functionality
;;; at another layer (the dotfile directory)
;;; Code:

(add-to-list 'load-path " ~/local/share/emacs/24.4/lisp/")
(add-to-list 'load-path
             (expand-file-name "dotfiles" user-emacs-directory)
             )

(require 'misc-config)
(with-library 'proxy-config)

;; Package

(require 'package)
(add-to-list 'package-archives
             ;; '("marmalade" . "http://marmalade-repo.org/packages/")
             ;; '("melpa-stable" . "http://stable.melpa.org/packages/")
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; ;; paredit to match lisp parentheses
;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;; (add-hook 'geiser-mode-hook 'enable-paredit-mode)

;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss'" . scss-mode))
(setq scss-compile-at-save nil)

;; CSS
(setq css-indent-offset 2)

;; ;; Smalltalk
;; (use-package gnu-smalltalk-mode)

;; ;; xterm-color.el

(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
        (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))
 
(progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
       (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region))

(provide 'init)
;;; init.el ends here
