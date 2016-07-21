
;; PureScript

(require 'purescript-mode)

;; PureScript IDE
(customize-set-variable 'psc-ide-executable "~/.local/bin/psc-ide-server")
(require 'psc-ide)
(add-hook 'purescript-mode-hook
          (lambda ()
            (psc-ide-mode)
            (company-mode)
            (flycheck-mode)
            (turn-on-purescript-indentation)))

;; ;; PuresScript repl (emacs-psci)
;; (add-hook 'purescript-mode-hook 'inferior-psci-mode)
;; (require 'repl-toggle)
;; (require 'psci)
;; (customize-set-variable 'psci-file-path "~/.local/bin/psci") ;; useless
;; (add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))

(provide 'purescript-config)

;; purescript-config.el ends here
