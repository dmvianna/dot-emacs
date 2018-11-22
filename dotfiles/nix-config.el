;;; nix-daniel-config.el -- Summary:
;;;
;;; Development environment for nix and nixos configuration files.
;;;
;;; Commentary:
;;; Code:

(use-package nix-mode
  :ensure t
  :commands nix-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode))
  :config
  (add-hook 'nix-mode-hook #'company-mode)
  (add-hook 'nix-mode-hook #'rainbow-delimiters-mode))

;; hint nixos options
(add-to-list 'company-backends 'company-nixos-options)
;; find executables in current nix shell
(setq flycheck-command-wrapper-function
        (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
      flycheck-executable-find
        (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

(provide 'nix-config)
