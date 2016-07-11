;;; Emacs ---  This is my configuration file.
;;; Commentary:
;;; Code:

;; Misc config
(add-to-list 'load-path " ~/local/share/emacs/24.4/lisp/")
(add-to-list 'load-path "~/.emacs.d/dotfiles")
(load-library "misc-config.el")
(load-library "proxy-config.el")

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
                     flycheck
                     ghc
                     haskell-mode
                     js2-mode
                     json-mode
                     magit
                     markdown-mode
                     nix-mode
                     python-mode
                     virtualenv
                     yaml-mode
                     writeroom-mode))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Haskell
(load-library "haskell-config.el")

;; Override haskell-mode's BS.
(global-set-key (kbd "M-n") 'next-error)
(define-key interactive-haskell-mode-map (kbd "M-n") 'next-error)
(setenv "PATH" (concat (getenv "PATH") ":~/.local/bin"))
(setq exec-path (append exec-path '("~/.local/bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Must review everything below this line

;;;; Syntax checking (Flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;; Autocompletion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;; Tagging
(require 'etags-table)
(setq etags-table-search-up-depth 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PureScript

(require 'purescript-mode)
(add-hook 'purescript-mode-hook 'turn-on-purescript-indent)

;; Flycheck PureScript
(eval-after-load 'flycheck
  '(flycheck-purescript-setup))

;; PureScript IDE
(customize-set-variable 'psc-ide-executable "~/.local/bin/psc-ide-server")
(require 'psc-ide)
(add-hook 'psc-ide-mode 'company-mode)

;; PuresScript repl (emacs-psci)
(add-hook 'purescript-mode-hook 'inferior-psci-mode)
(require 'repl-toggle)
(require 'psci)
(add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))

;;;;;;;;;;;;;;;;;;;;
;;; Python

;; Default virtualenv to "py" -- may be useful when reinstalling jedi
;; (require 'virtualenvwrapper)
;; (setq venv-location "/R/.virtualenv/py")
;; (setq python-environment-directory venv-location)
;; (venv-initialize-interactive-shells)
;; (venv-initialize-eshell)

(add-hook 'python-mode-hook
          (if (equal (getenv "VIRTUAL_ENV") nil)
              (venv-workon "py"))
          )

;; Python autocompletion and doc browsing
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; Python directory tree browser
;; (eval-after-load "python"
;;   '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;; (add-hook 'jedi-mode-hook 'jedi-direx:setup)

;; Get Python version
(setq python_version
      (string-to-number
      (substring
       (shell-command-to-string "python --version")
       7 -3)))
;; Is it a new version? (Boolean)
(setq new_python (>= python_version 2.7))

;; Enable Git
;; (require 'magit)
;; (global-set-key (kbd "C-g") 'magit-status)

;; Enable web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\..{1,2}ss\\'" . web-mode))

;; JavaScript autocompletion and introspection
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'tern-mode-hook 'auto-complete-mode)
(eval-after-load 'tern
  '(progn
  (require 'tern-auto-complete)
  (tern-ac-setup)))

;; Enable ES6
;; disable jshint since we prefer eslint
(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; JSX syntax highlighting

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; Stylus and jade modes
;;(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode")
    (require 'sws-mode)
    (require 'jade-mode)
    (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))

;; xterm-color.el

(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
        (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))
 
(progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
       (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region))




(provide 'init)
;;; init.el ends here
