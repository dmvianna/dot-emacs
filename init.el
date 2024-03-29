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
;; (require 'package)
;; (add-to-list 'package-archives
;;              ;; '("marmalade" . "http://marmalade-repo.org/packages/")
;;              ;;'("melpa-stable" . "http://stable.melpa.org/packages/")
;;              '("melpa" . "https://melpa.org/packages/")
;;              t)
;; (package-initialize)

;; (setq package-list '(solarized-theme
;;                      alchemist
;; 		     all-the-icons
;;                      auto-complete
;;                      auto-compile
;;                      cargo
;;                      company
;;                      consult
;;                      consult-flycheck
;;                      csv-mode
;;                      dante
;;                      dash-at-point
;;                      dhall-mode
;;                      dockerfile-mode
;;                      docker-compose-mode
;;                      ein
;;                      elixir-mode
;;                      elm-mode
;;                      elpy
;;                      envrc
;;                      ess
;;                      exec-path-from-shell
;;                      flycheck
;;                      flycheck-mypy
;;                      flycheck-rust
;;                      ;; racket-lang
;;                      ac-geiser
;;                      geiser
;;                      racket-mode
;;                      scheme-complete
;;                      scribble-mode
;;                      ;; / racket-lang
;;                      selectrum
;;                      haskell-mode
;;                      htmlize
;;                      hyde
;;                      js2-mode
;;                      json-mode
;;                      neotree
;;                      nvm
;;                      magit
;;                      markdown-mode
;;                      markdown-preview-mode
;;                      memoize
;;                      nix-buffer
;;                      nix-mode
;;                      nixos-options
;;                      company-nixos-options
;;                      nix-sandbox
;;                      paredit
;;                      php-mode
;;                      pickle
;;                      polymode
;;                      poly-R
;;                      poly-rst
;;                      psc-ide
;;                      psci
;;                      purescript-mode
;;                      pyenv-mode
;;                      pyenv-mode-auto
;;                      python-black
;;                      python-mode
;;                      racer
;;                      rainbow-delimiters
;;                      rainbow-mode
;;                      restclient
;;                      company-restclient
;;                      repl-toggle
;;                      rust-mode
;;                      sql-indent
;;                      sws-mode
;;                      smartparens
;;                      use-package
;;                      virtualenv
;;                      yaml-mode
;;                      web-mode
;;                      writeroom-mode))

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (dolist (package package-list)
;;   (when (not (package-installed-p package))
;;     (package-install package)))

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

(use-package flyspell
  :custom
  (ispell-program-name "hunspell"))

;; Company -- text completion
(use-package company
  :ensure t)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; enable lsp-mode
(use-package lsp-mode
  :config
  (custom-set-variables
   '(lsp-idle-delay 0.5)
   '(lsp-enable-symbol-highlighting t)
   '(lsp-enable-snippet nil)  ;; Not supported by company capf, which is the recommended company backend
   ;;; python
   '(lsp-pyls-plugins-flake8-enabled t)
     ;;; haskell
   '(haskell-completion-backend 'lsp)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-enable-hindent-style 'fundamental)
   '(haskell-indent-spaces 2)
   '(haskell-process-args-ghci "ghci")
   '(haskell-process-type 'stack-ghci)
   '(haskell-stylish-on-save 't)
   '(haskell-tags-on-save t)
   '(lsp-modeline-code-actions-enable nil)
   )

  (lsp-register-custom-settings
   ;;; python
   '(
     ("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)
     )
   )
  :hook
  (lambda ()
    ((python-mode . lsp))
    ((haskell-mode . lsp))
    ((haskell-literate-mode . lsp))
    )
  )
(use-package lsp-ui
  :commands lsp-ui-mode
  :hook 'haskell-lsp-ui-mode-hook
  :config
  (custom-set-variables
   '(lsp-ui-sideline-show-diagnostics t)
   '(lsp-ui-sideline-show-hover t)
   '(lsp-ui-sideline-show-code-actions t)
   '(lsp-ui-sideline-update-mode t)
   '(lsp-ui-peek-enable t)
   '(lsp-ui-peek-show-directory t)
   '(lsp-ui-doc-enable t)
   '(lsp-ui-doc-position 'bottom)
   '(lsp-ui-doc-show-with-cursor t)
   '(lsp-ui-doc-show-with-mouse t)
   '(lsp-ui-imenu-auto-refresh t)
   '(lsp-ui-imenu-window-width 40)
   )
  )

;; Git
(use-package magit)

;; sql

;;; indent sql emacs-style
(add-hook 'sql-mode-hook 'sqlind-minor-mode)
(with-library 'sql-config)

;; Gherkin
(use-package pickle
  :init
  (add-to-list 'auto-mode-alist '("\\.feature\\'" . pickle-mode)))

;; Nix
(use-package nix-config)

;; Agda
;; (if (executable-find "agda")
;;     (use-package agda-config))

;; Dhall
(setq dhall-format-command nil)

;; Elixir
(use-package elixir-mode
  :hook alchemist)

;; Elm
(setq elm-format-on-save t)

;; Haskell
;; using lsp
(use-package lsp-haskell)
(use-package haskell-config
  :hook haskell)

;; JavaScript
(use-package javascript-config)

;; JSON
(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . json-mode)))

;; Markdown
(use-package markdown-mode
  :hook flyspell
  :init
  (add-to-list 'auto-mode-alist
               '("\\.md$" . markdown-mode)))

;; Jekyll
(use-package hyde)

;; Jupyter
;; (use-package ein)
;; (use-package ein-notebook)
;; (use-package ein-subpackages)

;; PureScript
(use-package purescript-config)

;; Python
;; handled by lsp
;; (use-package python-config)

;; Racket
(use-package racket-mode)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(defun turn-on-paredit () (paredit-mode t))
(add-hook 'racket-mode-hook 'turn-on-paredit)
;; (use-package geiser-mode)

;; Rust
;; (require 'rust-config)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(use-package rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; rainbow-mode for CSS
(use-package rainbow-mode)

;; rainbow-delimiters for elisp
(use-package rainbow-delimiters
  :hook (emacs-lisp geiser))

;; paredit to match lisp parentheses
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'geiser-mode-hook 'enable-paredit-mode)

;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss'" . scss-mode))
(setq scss-compile-at-save nil)

;; CSS
(setq css-indent-offset 2)

;; R
(load "ess-autoloads")

;; Smalltalk
(use-package gnu-smalltalk-mode)

;; Smartparens
(smartparens-global-mode t)

;; ;; Speedbar
;; (require 'speedbar)
;; (speedbar-add-supported-extension ".hs")

;; xterm-color.el

;; add direnv support to buffers independently
(envrc-global-mode)

;; mail
(use-package mail-config)

;; personal details
(use-package personal-details)

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
 '(custom-safe-themes
   '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" default))
 '(ein:output-area-inlined-images t)
 '(haskell-complete-module-preferred
   '("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord"))
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-stylish-on-save t)
 '(hindent-reformat-buffer t)
 '(hindent-style "johan-tibell")
 '(org-agenda-files nil)
 '(package-selected-packages
   '(envrc dash-at-point python-black sql-indent php-mode dockerfile-mode ein htmlize paredit flycheck flycheck-mypy helm elpy racer hyde intero flycheck-rust exec-path-from-shell cargo auto-compile pickle jade-mode yaml-mode writeroom-mode web-mode virtualenv use-package sws-mode solarized-theme repl-toggle rainbow-mode rainbow-delimiters python-mode psci psc-ide nvm nix-mode markdown-mode magit json-mode js2-mode hindent flycheck-haskell etags-table elm-mode csv-mode company-ghc auto-complete))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

