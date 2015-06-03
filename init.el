;;; Emacs ---  This is my configuration file.
;;; Commentary:
;;; Code:

(global-unset-key (kbd "C-z"))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(set-frame-font "Inconsolata-16")
(require 'uniquify) ;; Inbuilt - Display sane file names
(setq uniquify-buffer-name-style 'forward)
(require 'ido) ;; Inbuilt - Finding files made easier
(ido-mode t)
(global-auto-revert-mode 1) ;; Reload files that have been changed

(require 'windmove)
(windmove-default-keybindings 'shift)
(global-set-key (kbd "C-e") 'eshell)

;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        
    (defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
    (setq backup-directory-alist
        `((".*" . ,emacs-tmp-dir)))
    (setq auto-save-file-name-transforms
        `((".*" ,emacs-tmp-dir t)))
    (setq auto-save-list-file-prefix
        emacs-tmp-dir)

;; Set tab width to 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Mouse
(mouse-wheel-mode t)
(xterm-mouse-mode t)

;; Proxy

(setq url-proxy-services
   '(("no_proxy" . "^//(localhost|127.\.*|parrot.bbs.bunnings.com.au)/")
     ("http" . "itchy.internal.bunnings.com.au:8080")
     ("https" . "itchy.internal.bunnings.com.au:8080")))

;; Package
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             ;; '("marmalade" . "http://marmalade-repo.org/packages/")
             '("melpa-stable" . "http://stable.melpa.org/packages/")
             ;; '("melpa" . "http://melpa.org/packages/")
             t)

(defadvice package-compute-transaction
  (before package-compute-transaction-reverse (package-list requirements) activate compile)
    "reverse the requirements"
    (setq requirements (reverse requirements))
    (print requirements))


;; Solarized theme

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'solarized-dark t)

;;; Find things quickly
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
;; be generous with garbage collection to improve speed
(setq gc-cons-threshold 20000000)
;; isearch flx style (flx-isearch)
(global-set-key (kbd "C-M-s") 'flx-isearch-forward)
(global-set-key (kbd "C-M-r") 'flx-isearch-backward)


;;;; Syntax checking (Flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


;;; Haskell

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(custom-set-variables '(haskell-process-type 'cabal-repl))

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

;; IPython

;; IPython shell:

(if new_python 
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  )

(if nil ;new_python
    (progn
      (require 'ein)
      (setq-default py-python-command-args '("--colors" "DarkBG"))
      (setq ein:use-auto-complete t)
      )
  nil)

;; Enable Git
(require 'magit)
(global-set-key (kbd "C-g") 'magit-status)

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

(provide 'init)
;;; init.el ends here
