;;; agda-config.el -- Summary:
;;;
;;; This is where I configure most of my
;;; Agda development environment
;;;
;;; Commentary:
;;;
;;; Code:

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

(add-hook 'agda-mode-hook
          (lambda ()
            ;; default to mononoki
            (set-face-attribute 'default nil
		                            :family "mononoki"
		                            :height 120
		                            :weight 'normal
		                            :width  'normal)
            (set-frame-parameter (window-frame) 'background-mode 'light)
            (enable-theme 'solarized)))

(provide 'agda-config)
