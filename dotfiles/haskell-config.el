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

;;(setq dante-repl-command-line '("cabal" "new-repl" dante-target))

;;;; stylish-haskell
(custom-set-variables
 '(haskell-stylish-on-save t))

(provide 'haskell-config)

;;; haskell-daniel-config.el ends here

