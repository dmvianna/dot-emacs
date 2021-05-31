;;; Haskell -- Summary:
;;;
;;; This is where I configure most of my
;;; Haskell development environment
;;;
;;; Commentary:
;;; Code:

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  )

(add-hook 'dante-mode-hook
          '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                 '(warning . haskell-hlint)
                                                 )
             )
          )

(custom-set-variables
 '(haskell-completion-backend 'lsp)
 '(haskell-enable-hindent-style 'fundamental)
 '(haskell-indent-spaces 2)
 '(haskell-process-args-ghci "ghci")
 '(haskell-process-type 'stack-ghci)
 '(haskell-stylish-on-save 't)
 '(dante-repl-command-line '("cabal" "new-repl" dante-target))
 )

(provide 'haskell-config)

;;; haskell-daniel-config.el ends here

