;;; search-replace.el --- search and replace configuration
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'use-package)


;; -- iedit + keybinding that works on a terminal
(prelude-require-package 'iedit)

(use-package iedit
  :bind ("C-c ;" . iedit-mode))


;; -- wgrep + zenburn compatible colors
(prelude-require-package 'wgrep)

(require 'wgrep)
(require 'zenburn-theme)

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


;; -- avy
(prelude-require-package 'avy)

(use-package avy
  :bind ("M-g" . avy-goto-word-or-subword-1))


(provide 'search-replace)
;;;  search-replace.el ends here
