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

;; Package
(require 'package)
(add-to-list 'package-archives
             ;; '("marmalade" . "http://marmalade-repo.org/packages/")
             ;;'("melpa-stable" . "http://stable.melpa.org/packages/")
             '("melpa" . "http://melpa.org/packages/")
             t)
(package-initialize)

(setq package-list '(solarized-theme
                     auto-complete
                     auto-compile
                     cargo
                     company
                     csv-mode
                     dante
                     elm-mode
                     elpy
                     ess
                     ess-R-data-view
                     exec-path-from-shell
                     flycheck
                     flycheck-rust
                     haskell-mode
                     hyde
                     js2-mode
                     json-mode
                     nvm
                     magit
                     markdown-mode
                     nix-buffer
                     nix-mode
                     nixos-options
                     company-nixos-options
                     nix-sandbox
                     pickle
                     psc-ide
                     psci
                     purescript-mode
                     python-mode
                     racer
                     rainbow-delimiters
                     rainbow-mode
                     repl-toggle
                     rust-mode
                     sws-mode
                     use-package
                     virtualenv
                     yaml-mode
                     web-mode
                     writeroom-mode))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Misc config
(add-to-list 'load-path " ~/local/share/emacs/24.4/lisp/")
(add-to-list 'load-path
             (expand-file-name "dotfiles" user-emacs-directory)
             )
(require 'misc-config)
(with-library 'proxy-config)

;; Flycheck -- global syntax check (needed for hlint)
(use-package flycheck
             :ensure t
             :init (global-flycheck-mode))

;; Company -- text completion
(use-package company
  :ensure t)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; Git
(require 'magit)

;; sql
;;(require 'sql-config)

;; Gherkin
(require 'pickle)
(add-to-list 'auto-mode-alist '("\\.feature\\'" . pickle-mode))

;; Nix
(use-package nix-mode
  :ensure t
  :commands nix-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode))
  :config
  (add-hook 'nix-mode-hook #'rainbow-delimiters-mode))

;; Haskell
(require 'haskell-daniel-config)

;; JavaScript
(require 'javascript-config)

;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . json-mode))

;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
             '("\\.md$" . markdown-mode))

;; Jekyll
(require 'hyde)

;; PureScript
(load-library "purescript-config.el")

;; Python
(elpy-enable)

;; Rust
(require 'rust-config)

;; rainbow-mode for CSS
(require 'rainbow-mode)

;; rainbow-delimiters for elisp
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss'" . scss-mode))
(setq scss-compile-at-save nil)

;; ;; Speedbar
;; (require 'speedbar)
;; (speedbar-add-supported-extension ".hs")

;; xterm-color.el

(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
        (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))
 
(progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
       (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-prompt-read-only t)
 '(comint-use-prompt-regexp t)
 '(company-ghc-show-info t)
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")))
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(hindent-reformat-buffer t)
 '(hindent-style "johan-tibell")
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (ess-R-data-view ess confluence elpy racer hyde intero flycheck-rust exec-path-from-shell cargo auto-compile pickle jade-mode yaml-mode writeroom-mode web-mode virtualenv use-package sws-mode solarized-theme repl-toggle rainbow-mode rainbow-delimiters python-mode psci psc-ide nvm nix-mode markdown-mode magit json-mode js2-mode hindent flycheck-haskell etags-table elm-mode csv-mode company-ghc auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
