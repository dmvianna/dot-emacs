;; Misc config

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

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

;; ;; Tagging
;; (require 'etags-table)
;; (setq etags-table-search-up-depth 10)
;; (setq tags-revert-without-query 1) ;; Stop annoying tag reversal queries

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

;; Only load if library present
(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

(provide 'misc-config)

;; misc-config.el ends here
