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


;; -- General settings for GUI (close to terminal settings)
(set-face-attribute 'default nil
                    :family "Menlo" :height 120 :weight 'extra-light)
(setq-default line-spacing 0.1)


;; -- transparent background (not needed on OSX)
(defun on-after-init()
  "redefine default background"
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(when (not (eq system-type 'darwin))
  (add-hook 'window-setup-hook 'on-after-init))


;; -- don't show menu bar nor scroll bar (os-x)
(menu-bar-mode 0)
(scroll-bar-mode 0)


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


;; -- make windmove work in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;; -- custom ace-window shortcut (<s-w>, i.e., Command-w doesn't work in terminal)
(global-set-key (kbd "M-p") 'ace-window)


;; -- line numbers in left margin (+ auto off)
(require 'linum-off)
(global-linum-mode 1)

(global-set-key (kbd "C-x n") 'linum-mode)

;; separate line numbers from text
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
(setq prelude-guru nil)


;; -- disable flyspell
;; (setq prelude-flyspell nil)


;; -- set en_US as default dictionary
(setq ispell-dictionary "en_US")


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


;; -- avy
(prelude-require-packages '(avy))
(require 'avy)
(global-set-key (kbd "M-g") 'avy-goto-word-or-subword-1)


;; -- org config
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/refile.org")

(setq org-agenda-files '("~/Dropbox/org"))

(global-set-key (kbd "C-c c") 'org-capture)

;; refile to current org file or agenda org files (max 9 deep levels)
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(defvar date_added "\n  :PROPERTIES:\n  :DATE_ADDED: %U")

(setq org-capture-templates
      `(("t" "todo" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* TODO %?" date_added))
        ("n" "note" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* %? :note:" date_added))
        ("e" "event" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* %? %^T :event:" date_added))
        ("m" "message" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* TODO contact %? :msg:" date_added))
        ("b" "bank transfer" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* TODO transfer %? to (account: ) :bank:" date_added))))

(setq org-tag-alist (quote ((:startgroup)
                            ("@GFZ" . ?G)
                            ("@Home" . ?H)
                            ("@Berlin" . ?B)
                            (:endgroup)
                            ("perso" . ?p)
                            ("admin" . ?a)
                            ("bank" . ?b)
                            ("urgent" . ?u)
                            ("idea" . ?i)
                            ("canceled". ?d)
                            ("note" . ?n)
                            ("music" . ?m)
                            ("roscoe" . ?r))))

(setq org-agenda-custom-commands
      '(("r" "Tasks to refile"
         ((agenda "" nil)
          (tags "refile"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))))))


(provide 'myconf)
;;;  myconf.el ends here
