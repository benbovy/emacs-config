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
                                 jinja2-mode
                                 python-django
                                 company-web
                                 linum-off
                                 editorconfig
                                 multi-term
                                 package-utils
                                 ranger
                                 multi-term
                                 flycheck-pyflakes
                                 elpy
                                 pyvenv
                                 ) prelude-packages))
(prelude-install-packages)


;; -- transparent background (not needed on OSX)
(defun on-after-init ()
  "redefine default background"
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(when (not (eq system-type 'darwin))
  (add-hook 'window-setup-hook 'on-after-init))


;; -- don't show menu bar (os-x)
(menu-bar-mode 0)

;; -- split windows preferably
;;(setq split-width-threshold nil)  ;; vertical split.
;;(setq split-width-threshold 1)    ;; horizontal-split
;;(setq split-height-threshold 200)


;; -- disable system bip
(setq visible-bell t)


;; -- standard keys for cut/copy/paste
(cua-mode 1)


;; -- iedit + keybinding that works on a terminal
(prelude-require-packages '(iedit))
(require 'iedit)
(global-set-key (kbd "C-c ;") 'iedit-mode)


;; -- wgrep + zenburn compatible colors
(prelude-require-packages '(wgrep))
(require 'wgrep)
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   `(wgrep-face ((t (:foreground ,zenburn-blue+1))))
   `(wgrep-delete-face ((t (:foreground ,zenburn-red+1))))
   `(wgrep-file-face ((t (:background ,zenburn-blue-5
                          :foreground ,zenburn-blue+1))))
   `(wgrep-reject-face ((t (:background ,zenburn-red-4
                            :foreground ,zenburn-red+1))))
   `(wgrep-done-face ((t (:background "#555511"
                          :foreground ,zenburn-yellow))))
   )
)


;; -- line numbers
(require 'linum-off)   ;; v0.1 bug, doesn't load
(global-linum-mode 1)

(global-set-key (kbd "C-x n") 'linum-mode)

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
(setq flycheck-check-indication-mode '(left-fringe))

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

;; --- handle tmux's xterm-keys
;; put the following line in your ~/.tmux.conf:
;;   setw -g xterm-keys on
(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
        (while (<= x 8)
          ;; shift
          (if (= x 2)
              (setq tkey "S-"))
          ;; alt
          (if (= x 3)
              (setq tkey "M-"))
          ;; alt + shift
          (if (= x 4)
              (setq tkey "M-S-"))
          ;; ctrl
          (if (= x 5)
              (setq tkey "C-"))
          ;; ctrl + shift
          (if (= x 6)
              (setq tkey "C-S-"))
          ;; ctrl + alt
          (if (= x 7)
              (setq tkey "C-M-"))
          ;; ctrl + alt + shift
          (if (= x 8)
              (setq tkey "C-M-S-"))

          ;; arrows
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
          ;; home
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d H" x)) (kbd (format "%s<home>" tkey)))
          ;; end
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d F" x)) (kbd (format "%s<end>" tkey)))
          ;; page up
          (define-key key-translation-map (kbd (format "M-[ 5 ; %d ~" x)) (kbd (format "%s<prior>" tkey)))
          ;; page down
          (define-key key-translation-map (kbd (format "M-[ 6 ; %d ~" x)) (kbd (format "%s<next>" tkey)))
          ;; insert
          (define-key key-translation-map (kbd (format "M-[ 2 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
          ;; delete
          (define-key key-translation-map (kbd (format "M-[ 3 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
          ;; f1
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d P" x)) (kbd (format "%s<f1>" tkey)))
          ;; f2
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d Q" x)) (kbd (format "%s<f2>" tkey)))
          ;; f3
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d R" x)) (kbd (format "%s<f3>" tkey)))
          ;; f4
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d S" x)) (kbd (format "%s<f4>" tkey)))
          ;; f5
          (define-key key-translation-map (kbd (format "M-[ 15 ; %d ~" x)) (kbd (format "%s<f5>" tkey)))
          ;; f6
          (define-key key-translation-map (kbd (format "M-[ 17 ; %d ~" x)) (kbd (format "%s<f6>" tkey)))
          ;; f7
          (define-key key-translation-map (kbd (format "M-[ 18 ; %d ~" x)) (kbd (format "%s<f7>" tkey)))
          ;; f8
          (define-key key-translation-map (kbd (format "M-[ 19 ; %d ~" x)) (kbd (format "%s<f8>" tkey)))
          ;; f9
          (define-key key-translation-map (kbd (format "M-[ 20 ; %d ~" x)) (kbd (format "%s<f9>" tkey)))
          ;; f10
          (define-key key-translation-map (kbd (format "M-[ 21 ; %d ~" x)) (kbd (format "%s<f10>" tkey)))
          ;; f11
          (define-key key-translation-map (kbd (format "M-[ 23 ; %d ~" x)) (kbd (format "%s<f11>" tkey)))
          ;; f12
          (define-key key-translation-map (kbd (format "M-[ 24 ; %d ~" x)) (kbd (format "%s<f12>" tkey)))
          ;; f13
          (define-key key-translation-map (kbd (format "M-[ 25 ; %d ~" x)) (kbd (format "%s<f13>" tkey)))
          ;; f14
          (define-key key-translation-map (kbd (format "M-[ 26 ; %d ~" x)) (kbd (format "%s<f14>" tkey)))
          ;; f15
          (define-key key-translation-map (kbd (format "M-[ 28 ; %d ~" x)) (kbd (format "%s<f15>" tkey)))
          ;; f16
          (define-key key-translation-map (kbd (format "M-[ 29 ; %d ~" x)) (kbd (format "%s<f16>" tkey)))
          ;; f17
          (define-key key-translation-map (kbd (format "M-[ 31 ; %d ~" x)) (kbd (format "%s<f17>" tkey)))
          ;; f18
          (define-key key-translation-map (kbd (format "M-[ 32 ; %d ~" x)) (kbd (format "%s<f18>" tkey)))
          ;; f19
          (define-key key-translation-map (kbd (format "M-[ 33 ; %d ~" x)) (kbd (format "%s<f19>" tkey)))
          ;; f20
          (define-key key-translation-map (kbd (format "M-[ 34 ; %d ~" x)) (kbd (format "%s<f20>" tkey)))

          (setq x (+ x 1))
          ))
      )
  )


;; -- web-mode settings
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'"))
      )


;; -- Python: use elpy instead of anaconda-mode
;(elpy-enable)
;(add-hook 'elpy-mode-hook (lambda () (anaconda-mode -1)))


;; -- Trick to select conda environment within emacs (using elpy or anaconda-mode)
;;    (see http://emacs.stackexchange.com/questions/20092/using-conda-environments-in-emacs)
;;    emacs must be run from a conda env activated!!
(if (getenv "CONDA_PREFIX")
    (progn
     (setq conda-env-dir (file-name-directory (directory-file-name (getenv "CONDA_PREFIX"))))
     (message "Conda environments directory: %s" conda-env-dir)
     (setenv "WORKON_HOME" conda-env-dir)
     (pyvenv-mode 1)
     (defalias 'conda-workon 'pyvenv-workon)
    )
)

;; -- multi-term settings
(setq multi-term-program "/bin/bash")


;; -- switch back fn- left/right on OS-X
(setq mac-option-modifier 'super )
(setq mac-command-modifier 'meta )
(define-key global-map [home] 'back-to-indentation)
(define-key global-map [end] 'end-of-line)

;; -- make C left/right the same as fn- left/right (use on keyboard w/ numpad)
(global-set-key (kbd "<C-left>") 'back-to-indentation)
(global-set-key (kbd "<C-right>") 'end-of-line)


;; -- Git gutter
(prelude-require-packages '(git-gutter))
(require 'git-gutter)

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

;; -- counsel projectile
(prelude-require-packages '(counsel-projectile))
(require 'counsel-projectile)
(counsel-projectile-mode +1)


(provide 'myconf)
;;;  myconf.el ends here
