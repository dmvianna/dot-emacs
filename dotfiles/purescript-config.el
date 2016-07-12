
;; PureScript

(require 'flycheck)
(require 'company)
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

(provide 'purescript-config)

;; purescript-config.el ends here
