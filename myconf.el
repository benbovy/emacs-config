;; My Emacs Confiuration on top of Prelude
;;
;;

;; transparent background (not needed on OSX)
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(when (not (eq system-type 'darwin))
  (add-hook 'window-setup-hook 'on-after-init))


;; standard keys for cut/copy/paste
(cua-mode 1)


;; disable guru mode
(setq prelude-guru nil)


;; disable flyspell
;; (setq prelude-flyspell nil)

;; set en_US as default dictionary
(setq ispell-dictionary "en_US")


;; additional packages not provided by Prelude
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)
(setq prelude-packages (append '(
                                 sr-speedbar
                                 nginx-mode
                                 ) prelude-packages))
(prelude-install-packages)


;; sr-speedbar settings
(require 'sr-speedbar)
(global-set-key (kbd "C-c b") 'sr-speedbar-toggle)
(autoload 'sr-speedbar-open "sr-speedbar" "Open the in-frame speedbar" t)
(eval-after-load 'sr-speedbar
  '(progn
     (setq speedbar-hide-button-brackets-flag t
           speedbar-show-unknown-files t
           speedbar-smart-directory-expand-flag t
           speedbar-directory-button-trim-method 'trim
           speedbar-use-images nil
           speedbar-indentation-width 2
           speedbar-use-imenu-flag t
           speedbar-file-unshown-regexp "flycheck-.*"
           sr-speedbar-width 40
           sr-speedbar-width-x 40
           sr-speedbar-auto-refresh nil
           sr-speedbar-skip-other-window-p t
           sr-speedbar-right-side nil)

     (add-hook 'speedbar-reconfigure-keymaps-hook
               '(lambda ()
                  (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
                  (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
                  (define-key speedbar-mode-map [left] 'speedbar-contract-line)))
     )
  )


(provide 'myconf)
;;; myconf.el ends here
