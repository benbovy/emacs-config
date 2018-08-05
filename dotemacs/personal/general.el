;;; general.el --- My Emacs Configuration on top of Prelude.
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; -- additional packages not provided by Prelude
(prelude-require-packages '(editorconfig
                            ranger
                            use-package
                            smex))


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

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))


;; -- disable system bip
(setq visible-bell t)


;; -- standard keys for cut/copy/paste
(cua-mode 1)


;; -- split windows preferably
;;(setq split-width-threshold nil)  ;; vertical split.
;;(setq split-width-threshold 1)    ;; horizontal-split
;;(setq split-height-threshold 200)


;; -- custom ace-window shortcut (<s-w>, i.e., Command-w doesn't work in terminal)
(use-package ace-window
  :bind* ("M-o" . ace-window))


;; -- disable advanced purist mode
(use-package prelude-custom
  :config
  (setq prelude-guru nil))


;; -- line numbers
(prelude-require-packages '(linum linum-off))

(use-package linum
  :bind ("C-x n" . linum-mode)
  :config
  (global-linum-mode 0))

(defvar-local linum-format-fmt nil)

(defun linum-format-func (line)
  "Add separator between LINE numbers and text."
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(defun linum-set-format-fmt ()
  "Dynamically compute linum-format.  Fancy but still slow!"
  (setq-local linum-format-fmt
              (let ((w (length (number-to-string
                                (count-lines (point-min) (point-max))))))
                (concat "%" (number-to-string w) "d"))))

(use-package linum
  :unless window-system
  :init
  (add-hook 'linum-before-numbering-hook 'linum-set-format-fmt)
  :config
  (setq linum-format 'linum-format-func)
  )


;; -- spell checker
(use-package prelude-custom
  :disabled
  :config
  (setq prelude-flyspell nil))

(use-package ispell
  :config
  (setq ispell-dictionary "en_US"))

(use-package flyspell
  :diminish (flyspell-mode . " ⓢ"))


;;  -- linter
(use-package flycheck
  :bind ("<f7>" . flycheck-buffer-and-list-errors)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (defun flycheck-buffer-and-list-errors ()
    "Execute command flycheck-buffer and flycheck-list-errors."
    (interactive)
    (flycheck-buffer)
    (flycheck-list-errors)
    (other-window 1)
    )
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-indication-mode 'left-fringe)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :diminish (flycheck-mode . " Ⓢ")
  )


;; -- web
(prelude-require-packages '(nginx-mode company-web typescript-mode))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  )

(use-package js2-mode
  :config
  (setq js-indent-level 2)
  )

(setq js-indent-level 2)

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)
  )


;; -- slideshow (remark)
(prelude-require-package 'remark-mode)


;; -- multi-term
(prelude-require-package 'multi-term)
(use-package multi-term
  :config
  (setq multi-term-program "/usr/local/bin/bash"))


;; -- git / diff
(prelude-require-package 'git-gutter)

(require 'zenburn-theme)

(use-package diff-hl
  :config
  (global-diff-hl-mode -1))

(use-package git-gutter
  :after (diff-hl)
  :requires (linum)
  :config
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup)

  (custom-set-variables
   '(git-gutter:modified-sign "!")
   '(git-gutter:added-sign "+")
   '(git-gutter:deleted-sign "-"))

  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; copied from zenburn-theme colors set for diff-hl
     `(git-gutter:modified ((t (:foreground ,zenburn-blue
                                            :background ,zenburn-blue-2))))
     `(git-gutter:added ((t (:foreground ,zenburn-green+1
                                         :background ,zenburn-green-1))))
     `(git-gutter:deleted ((t (:foreground ,zenburn-red+1
                                           :background ,zenburn-red-1))))))
  )

;; (use-package magit
;;   :config
;;   ;; -- magit color fix for terminal: remove when emacs 26 (true color support)
;;   (custom-theme-set-faces
;;    'zenburn
;;    `(magit-diff-added ((t (:background "#005f5f" :foreground "#ddffdd"))))))


;; -- counsel projectile
(prelude-require-package 'counsel-projectile)
(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))


;; -- calfw
(prelude-require-packages '(calfw calfw-org))


;; -- cmake projects
(prelude-require-package 'rtags)
(prelude-require-package 'cmake-ide)

(use-package cmake-ide
  :custom
  (cmake-ide-build-pool-dir "~/.emacs-cmake-ide")
  (cmake-ide-build-pool-use-persistent-naming t)
  :init
  (require 'rtags)
  (add-hook 'after-init-hook #'cmake-ide-setup)
  )


;; -- whitespace
(use-package whitespace
  :commands whitespace-mode
  :init
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode t)
  (setq whitespace-global-modes
        '(c-mode c++-mode lb-datalog-mode java-mode emacs-lisp-mode
                 shell-script-mode sh-mode python-mode))
  ;; c hook
  (add-hook 'c-mode-common-hook '(lambda ()
                                   (interactive)
                                   (whitespace-mode 0)
                                   (setq-local whitespace-line-column 90)
                                   (whitespace-mode 1)))
  :diminish (whitespace-mode . " ⓦ")
  )

;; -- smartparens
(use-package smartparens
  :diminish (smartparens-mode . " ⓟ"))


;; -- expand-region
(use-package expand-region
  :bind (("C-c SPC" . er/expand-region))
  )


;; -- smart-mode-line
;; TODO: tweak powerline theme colors with zenburn
;; (prelude-require-package 'smart-mode-line)
;; (prelude-require-package 'smart-mode-line-powerline-theme)
;; (use-package smart-mode-line
;;   :custom
;;   (sml/no-confirm-load-theme t)
;;   (sml/theme 'powerline)
;;   :init
;;   (add-hook 'after-init-hook #'sml/setup)
;;   )


(provide 'general)
;;;  general.el ends here
