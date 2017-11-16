;;; Haskell -- Summary:
;;;
;;; This is where I configure most of my
;;; Haskell development environment
;;;
;;; Commentary:
;;; Code:

(setenv "PATH" (concat (getenv "PATH") ":~/.local/bin"))
(setq exec-path (append exec-path '("~/.local/bin")))
(custom-set-variables '(haskell-process-type 'stack-ghci))

;;;; Haskell syntax check
;; (require 'flycheck)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(require 'hs-lint)
(defun my-haskell-mode-hook ()
  (local-set-key "\C-cl" 'hs-lint))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
     (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
     ))

;;;; Haskell Autocompletion
(require 'ghc)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(require 'company)
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

;;;; Haskell-mode indenting and navigation
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)
(custom-set-variables
 '(haskell-process-suggest-haskell-docs-imports t)
 '(hindent-reformat-buffer t)
 '(hindent-style "johan-tibell")
 '(haskell-complete-module-preferred
   '("Data.ByteString"
     "Data.ByteString.Lazy"
     "Data.Conduit"
     "Data.Function"
     "Data.List"
     "Data.Map"
     "Data.Maybe"
     "Data.Monoid"
     "Data.Ord")))
;; (custom-set-variables '(haskell-tags-on-save t))

;;;; stylish-haskell
(custom-set-variables
 '(haskell-stylish-on-save t))

(provide 'haskell-daniel-config)

;;; haskell-daniel-config.el ends here

