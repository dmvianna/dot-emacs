;;; python-config.el -- Summary:
;;;
;;; This is where I configure most of my
;;; Python development environment
;;;
;;; Commentary:
;;; Code:

(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

(remove-hook 'elpy-modules 'elpy-module-flymake)
(elpy-enable)

(provide 'python-config)
