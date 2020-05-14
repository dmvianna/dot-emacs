;;; python-config.el -- Summary:
;;;
;;; This is where I configure most of my
;;; Python development environment
;;;
;;; Commentary:
;;; `flycheck-mypy` provides mypy support (static type validation in Python).
;;; This does not work with flymake (used by elpy).
;;; `elpy` provides a lot of IDE support, including running both `flake8` and
;;; `pylint` via flymake. I wasn't able to run either alongside mypy on
;;; flycheck, but they seem to work well in parallel flycheck and flymake
;;; processes.
;;;
;;; Everything hinges on selecting a `pyvenv-workon` *prior* to opening any
;;; Python file. After that, one has to restart emacs and try again.
;;;
;;; Code:


(pyenv-mode)
(use-package pyenv-mode-auto)
(use-package flycheck-mypy)
(if (eq system-type 'darwin)
    (setq elpy-rpc-python-command "~/.pyenv/shims/python3")
  ;; else do nothing
  )
(use-package python-black
 ;; :demand t
 :after python)
(elpy-enable)

(provide 'python-config)
