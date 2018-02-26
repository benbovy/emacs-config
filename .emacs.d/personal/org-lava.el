;;; org-lava.el --- My Org configuration
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'use-package)

(require 'zenburn-theme)
(require 'windmove)

;; note: this module use functions defined in personal/preload/org-lava-utils.el

(use-package org
  :bind ("C-c c" . org-capture)
  :init
  ;; -- make windmove work in org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  :config
  (setq org-modules '(org-habit))

  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file "~/Dropbox/org/refile.org")
  (setq org-agenda-files '("~/Dropbox/org"))

  ;; (setq org-enforce-todo-dependencies t)  ;; bug with zenburn: bad color

  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))

  ;; refile up to 3 levels in current/agenda files
  (setq org-refile-targets (quote ((nil :maxlevel . 3)
                                   (org-agenda-files :maxlevel . 3))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)")
          (sequence "NEXT(n)" "WAITING(w@/!)" "|" "CANCELED(c@)")))

  (setq org-use-fast-todo-selection t)

  (zenburn-with-color-variables
    (setq org-todo-keyword-faces
          `(("WAITING" :foreground ,zenburn-orange :weight bold)
            ("NEXT" :foreground ,zenburn-blue :weight bold)
            ("CANCELED" :foreground ,zenburn-green :weight bold))))

  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(org-agenda-done ((t (:foreground ,zenburn-fg-1))))))  ;; bug in zenburn?

  (setq org-tag-alist (quote ((:startgroup)
                              ("@GFZ" . ?G)
                              ("@Home" . ?H)
                              ("@Berlin" . ?B)
                              (:endgroup)
                              ("admin" . ?a)
                              ("bank" . ?b)
                              ("note" . ?n)
                              ("music" . ?m)
                              ("shop" . ?s))))

  )


(use-package org-habit
  :config
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 4))


(use-package org-capture
  :config
  (defvar org-lava-date_added "\n  :PROPERTIES:\n  :DATE_ADDED: %U\n  :END:\n")

  (setq org-capture-templates
        `((" " "simple entry" entry (file "~/Dropbox/Org/refile.org")
           "* %?")
          ("p" "new project" entry (file "~/Dropbox/Org/projects.org")
           "* %? %^g")
          ("t" "todo" entry (file "~/Dropbox/Org/refile.org")
           ,(concat "* TODO %?" org-lava-date_added))
          ("n" "note" entry (file "~/Dropbox/Org/refile.org")
           ,(concat "* %? :note:" org-lava-date_added))
          ("e" "event" entry (file "~/Dropbox/Org/refile.org")
           ,(concat "* %? %^T :event:" org-lava-date_added))
          ("m" "message" entry (file "~/Dropbox/Org/refile.org")
           ,(concat "* TODO contact %? :msg:" org-lava-date_added))
          ("b" "bank transfer" entry (file "~/Dropbox/Org/refile.org")
           ,(concat "* TODO transfer %? :bank:" org-lava-date_added))
          ("s" "shop" entry (file "~/Dropbox/Org/refile.org")
           ,(concat "* TODO buy :shop:" org-lava-date_added))))
  )

(use-package org-agenda
  :config
  (setq org-agenda-custom-commands
        '(("c" "Custom agenda view"
           ((agenda "" nil)
            (tags "refile"
                  ((org-agenda-overriding-header "Tasks to refile")
                   (org-tags-match-list-sublevels nil)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next tasks")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down effort-up priority-down category-keep))))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting tasks")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down priority-down category-keep))))
            (tags "+LEVEL=1-TODO=\"CANCELED\""
                  ((org-agenda-overriding-header "Stuck projects")
                   (org-agenda-files '("~/Dropbox/org/projects.org"))
                   (org-agenda-skip-function 'org-lava-skip-non-stuck-projects)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
            (tags "+LEVEL=1"
                  ((org-agenda-overriding-header "All other projects")
                   (org-agenda-files '("~/Dropbox/org/projects.org"))
                   (org-agenda-skip-function 'org-lava-skip-stuck-projects)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
            (tags "+idea+LEVEL=1"
                  ((org-agenda-overriding-header "Five randomly selected ideas")
                   (org-agenda-max-entries 5)
                   (org-agenda-cmp-user-defined (org-lava-compare-randomly))
                   (org-lava-compare-random-refresh  t)
                   (org-agenda-sorting-strategy '(user-defined-up))))
            ))))
  )


(provide 'org-lava)
;;;  org-lava.el ends here
