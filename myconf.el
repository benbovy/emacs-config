;;; myconf --- My Emacs Configuration on top of Prelude.
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; -- additional packages not provided by Prelude
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)
(setq prelude-packages (append '(
                                 sr-speedbar
                                 nginx-mode
                                 linum-off
                                 ) prelude-packages))
(prelude-install-packages)


;; -- transparent background (not needed on OSX)
(defun on-after-init ()
  "redefine default background"
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(when (not (eq system-type 'darwin))
  (add-hook 'window-setup-hook 'on-after-init))


;; -- split windows preferably
;;(setq split-width-threshold nil)  ;; vertical split.
;;(setq split-width-threshold 1)    ;; horizontal-split


;; -- disable system bip
(setq visible-bell t)


;; -- standard keys for cut/copy/paste
(cua-mode 1)


;; -- line numbers
;;(require 'linum-off)   ;; v0.1 bug, doesn't load
;;(global-linum-mode 1)

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                            (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
    (setq linum-format 'linum-format-func))

(custom-set-faces '(linum ((t (:foreground "#6F6F6F" :background "#3F3F3F" :box nil)))))

;;(setq linum-format "%5d ")


;; -- disable guru mode
(setq prelude-guru nil)


;; -- disable flyspell
;; (setq prelude-flyspell nil)


;; -- set en_US as default dictionary
(setq ispell-dictionary "en_US")


;; -- additional search engines
;; TODO: wordreference search
;;(require 'prelude-core)
;;(Prelude-install-search-engine "wordreference"     "http://www.google.com/search?q="              "Google: ")


;; -- sr-speedbar settings
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
                  (define-key speedbar-mode-map [C-S-up] 'speedbar-up-directory)
                  (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
                   (define-key speedbar-mode-map [left] 'speedbar-contract-line)))
     )
    )


;;  -- flycheck settings
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save))

(defun flycheck-buffer-and-list-errors()
  "execute command flycheck-buffer and flycheck-list-errors"
  (interactive)
  (flycheck-buffer)
  (flycheck-list-errors)
  (other-window 1)
  )

(defun flycheck-mode-keys()
  "key map for flycheck-mode"
  (local-set-key (kbd "<f7>") 'flycheck-buffer-and-list-errors)
  )
(add-hook 'flycheck-mode-hook 'flycheck-mode-keys)


;; -- magit settings
(defvar pc:magit-from-buffer
  "Buffer from where magit-status were last called."
  nil)

(defun pc:magit-status-buffer-switch (buf)
    "replacement for `magit-status-buffer-switch-function'.
`magit-status' does not split windows (switch to magit buffer
in stead). Also store the current buffer to switch back to it when
 quitting.
TODO: store the whole frame config instead?"
  (setq pc:magit-from-buffer (current-buffer))
  (switch-to-buffer buf)
  )

(setq magit-status-buffer-switch-function 'pc:magit-status-buffer-switch)

(defun pc:magit-quit-window (&optional kill-buffer)
  "r eplacement for \"q\" keybinding in magit.
Bury the current (magit) buffer and switch to original buffer.
With a prefix argument, kill the magit buffer instead."
  (interactive "P")
  (if kill-buffer (kill-buffer) (bury-buffer))
  (switch-to-buffer pc:magit-from-buffer)
  )

(defun magit-mode-keys()
  "key map for exiting  magit-mode"
  (define-key magit-mode-map (kbd "q") 'pc:magit-quit-window)
  )
(add-hook 'magit-mode-hook 'magit-mode-keys)


(provide 'myconf)
;;;  myconf.el ends here
