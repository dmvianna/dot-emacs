;;; Emacs ---  This is my configuration file.
;;; Commentary:
;;; Code:

;; Misc config
(add-to-list 'load-path " ~/local/share/emacs/24.4/lisp/")
(add-to-list 'load-path "~/.emacs.d/dotfiles")
(require 'misc-config)
(with-library 'proxy-config)

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
                     company
                     company-ghc
                     csv-mode
                     elm-mode
                     etags-table
                     flycheck
                     flycheck-haskell
                     ghc
                     haskell-mode
                     js2-mode
                     json-mode
                     magit
                     markdown-mode
                     nix-mode
                     psc-ide
                     psci
                     purescript-mode
                     python-mode
                     rainbow-delimiters
                     rainbow-mode
                     repl-toggle
                     virtualenv
                     yaml-mode
                     writeroom-mode))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Git
;; (require 'magit)
;; (global-set-key (kbd "C-g") 'magit-status)

;; Haskell
(load-library "haskell-daniel-config.el")

;; ;; highlight-symbol
;; (add-to-list 'load-path "~/.emacs.d/idle-highlight/")
;; (load-library "idle-highlight.el")
;; (require 'idle-highlight-mode)
;; (add-hook 'text-mode-hook (lambda () (idle-highlight-mode t)))
;; (add-hook 'text-mode-hook (lambda () (idle-highlight-mode t)))
;; (setq idle-highlight-idle-time 0.5)

;; JavaScript

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js2-mode))

;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . json-mode))

;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
             '("\\.md$" . markdown-mode))

;; Nix
(require 'nix-mode)

;; PureScript
(load-library "purescript-config.el")

;; Python
(require 'python-mode)

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
