;; Misc config

(add-to-list 'load-path "~/.emacs.d/elpa/")
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
(setq tags-revert-without-query 1) ;; Stop annoying tag reversal queries

(require 'windmove)
(windmove-default-keybindings 'shift)
(global-set-key (kbd "C-e") 'eshell)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Set tab width to 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Mouse
(mouse-wheel-mode t)
(xterm-mouse-mode t)

;; ;;; Find things quickly
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)
;; ;; be generous with garbage collection to improve speed
;; (setq gc-cons-threshold 20000000)
;; ;; isearch flx style (flx-isearch)
;; (global-set-key (kbd "C-M-s") 'flx-isearch-forward)
;; (global-set-key (kbd "C-M-r") 'flx-isearch-backward)

(provide 'misc-config)

;; misc-config.el ends here
