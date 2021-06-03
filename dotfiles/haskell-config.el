;;; Haskell -- Summary:
;;;
;;; This is where I configure most of my
;;; Haskell development environment
;;;
;;; Commentary:
;;; Code:


(use-package lsp-haskell
  :defer t
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (lsp)))
  (add-hook 'haskell-literate-mode-hook #'lsp))

(use-package lsp-mode
  :hook (prog-mode . lsp-mode)
  :config
  ;; This is to make `lsp-mode' work with `direnv' and pick up the correct
  ;; version of GHC.
  ;; (advice-add 'lsp :before #'direnv-update-environment)
  (custom-set-variables
   '(haskell-completion-backend 'lsp)
   '(haskell-enable-hindent-style 'fundamental)
   '(haskell-indent-spaces 2)
   '(haskell-process-args-ghci "ghci")
   '(haskell-process-type 'stack-ghci)
   '(haskell-stylish-on-save 't)
   '(lsp-modeline-code-actions-enable nil)
   )
  )

(use-package lsp-ui
  :hook (prog-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom))

;; (eval-after-load 'haskell-mode
;;   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;   (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;   (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;   (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;   (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;   (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;   )


(provide 'haskell-config)

;;; haskell-daniel-config.el ends here

