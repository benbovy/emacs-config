;;; general.el --- My Emacs Configuration on top of Prelude.
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; -- additional packages not provided by Prelude
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; (add-to-list 'package-archives
;;              '("marmalade" .
;;                "http://marmalade-repo.org/packages/"))
(prelude-require-packages '(nginx-mode
                            company-web
                            linum
                            linum-off
                            editorconfig
                            multi-term
                            ranger
                            use-package))


;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))


;; -- General settings for GUI (close to terminal settings)
(set-face-attribute 'default nil
                    :family "Menlo" :height 120 :weight 'extra-light)
(setq-default line-spacing 0.1)


;; -- transparent background (not needed on OSX)
(defun on-after-init ()
  "Redefine default background."
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(when (not (eq system-type 'darwin))
  (add-hook 'window-setup-hook 'on-after-init))


;; -- don't show menu bar nor scroll bar
(menu-bar-mode 0)
(scroll-bar-mode 0)


;; -- split windows preferably
;;(setq split-width-threshold nil)  ;; vertical split.
;;(setq split-width-threshold 1)    ;; horizontal-split
;;(setq split-height-threshold 200)


;; -- custom ace-window shortcut (<s-w>, i.e., Command-w doesn't work in terminal)
(global-set-key (kbd "M-p") 'ace-window)


;; -- disable system bip
(setq visible-bell t)


;; -- standard keys for cut/copy/paste
(cua-mode 1)


;; -- line numbers in left margin (+ auto off)
(require 'linum-off)
(require 'linum)

(global-linum-mode 0)
(global-set-key (kbd "C-x n") 'linum-mode)

;; separate line numbers from text
;; fancy but very slow (that's why limum mode is disabled by default)
(defvar-local linum-format-fmt nil)

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func(line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
  (setq linum-format 'linum-format-func))


;; -- disable guru mode
(require 'prelude-custom)
(setq prelude-guru nil)


;; -- disable flyspell
;; (setq prelude-flyspell nil)


;; -- set en_US as default dictionary
(require 'ispell)
(setq ispell-dictionary "en_US")


;;  -- flycheck settings
(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(setq flycheck-indication-mode 'left-fringe)
(setq-default flycheck-emacs-lisp-load-path 'inherit)

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

;; -- web-mode settings
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  )


;; -- multi-term settings
(require 'multi-term)
(setq multi-term-program "/usr/local/bin/bash")


;; -- Git gutter
(prelude-require-packages '(git-gutter))
(require 'git-gutter)
(require 'zenburn-theme)
(require 'diff-hl)

(global-diff-hl-mode -1)    ;; git-gutter works better in terminal than diff-hl
(global-git-gutter-mode +1)
(git-gutter:linum-setup)

(custom-set-variables
 '(git-gutter:modified-sign "!")
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-"))

;; copied from zenburn colors set for diff-hl
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   `(git-gutter:modified ((t (:foreground ,zenburn-blue
                              :background ,zenburn-blue-2))))
   `(git-gutter:added ((t (:foreground ,zenburn-green+1
                           :background ,zenburn-green-1))))
   `(git-gutter:deleted ((t (:foreground ,zenburn-red+1
                             :background ,zenburn-red-1))))
   )
)

;; -- magit color fix for terminal: remove when emacs 26 (true color support)
 (custom-theme-set-faces
  'zenburn
  `(magit-diff-added ((t (:background "#005f5f" :foreground "#ddffdd"))))
;;  `(magit-diff-added-highlight ((t (:background "#3F5F3F" :foreground "#cceecc"))))
;;  `(magit-diff-removed ((t (:background "#5f005f" :foreground "#ffdddd"))))
;;  `(magit-diff-removed-highlight ((t (:background "#663333" :foreground "#eecccc"))))
 )


;; -- counsel projectile
(prelude-require-package 'counsel-projectile)
(require 'counsel-projectile)
(counsel-projectile-mode +1)


;; -- calfw
(prelude-require-packages '(calfw calfw-org))
(require 'calfw)
(require 'calfw-org)


(provide 'general)
;;;  general.el ends here
