;;; Package --- Summary
;;; Commentary:
;;; Misc config
;;; Code:

(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

(column-number-mode t)
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

;;; Tagging
;; (require 'etags-table)
;; (setq etags-table-search-up-depth 10)
;; (setq tags-revert-without-query 1) ;; Stop annoying tag reversal queries

(require 'windmove)
(windmove-default-keybindings 'shift)
(global-set-key (kbd "C-e") 'eshell)

;;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `(("." ,temporary-file-directory t)))
(setq auto-save-list-file-prefix
      temporary-file-directory)

;;; Set tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;;; Mouse
(mouse-wheel-mode t)
(xterm-mouse-mode t)

;;; use-package
(require 'use-package)

;;; Only load if library present
(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

(provide 'misc-config)

;;; change capitalisation
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; auto-byte-compile
(setq load-prefer-newer t)
(package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; exec path from shell
(exec-path-from-shell-initialize)

;;; misc-config.el ends here
