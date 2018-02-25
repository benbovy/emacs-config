;;; org-conf.el --- My Org configuration -*- lexical-binding: t -*-
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-habit)
(require 'org-capture)

;; -- custom functions

;; from http://doc.norang.ca/org-mode.html
(defun lava-org-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (has-next ))
        (save-excursion
          (forward-line 1)
          (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
            (unless (member "WAITING" (org-get-tags-at))
              (setq has-next t))))
        (if has-next
            next-headline
          nil)) ; a stuck project, has subtasks but no next task
      )))

(defun lava-org-skip-stuck-projects ()
  "Skip trees that are stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (has-next ))
        (save-excursion
          (forward-line 1)
          (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
            (unless (member "WAITING" (org-get-tags-at))
              (setq has-next t))))
        (if has-next
            nil ; a stuck project, has subtasks but no next task
          next-headline))
      )))


;; from https://emacs.stackexchange.com/questions/24270/org-mode-pick-random-task-from-custom-agenda-view
(defvar lava-org-compare-random-refresh nil
  "Whether `lava-org-compare-randomly' should refresh its keys.")

(defun lava-org-compare-get-marker (entry)
  "Return the marker for ENTRY."
  (get-text-property 1 'org-marker entry))

(defun lava-org-compare-randomly-update-sort-key (entry table generator)
  "Return sort key for ENTRY in TABLE, generating it with GENERATOR if necessary."
  (let* ((marker (lava-org-compare-get-marker entry))
         (hash-key `(,(marker-buffer marker) . ,(marker-position marker))))
    (or (gethash hash-key table)
        (puthash hash-key (funcall generator entry) table))))

;;(setq table (make-hash-table :test #'equal))
;;(setq generator 'random)

(defun lava-org-compare-randomly-by (generator)
  "Return a random comparator using GENERATOR."
  (let ((table (make-hash-table :test #'equal)))
    (lambda (x y)
      (when lava-org-compare-random-refresh
        (clrhash table)
        (setq lava-org-compare-random-refresh nil))
      (let ((x-val (lava-org-compare-randomly-update-sort-key x table generator))
            (y-val (lava-org-compare-randomly-update-sort-key y table generator)))
        (cond
         ((= x-val y-val)  nil)
         ((< x-val y-val)   -1)
         ((> x-val y-val)   +1))))))

(defun lava-org-compare-randomly ()
  "Return a comparator implementing a random shuffle."
  (lava-org-compare-randomly-by (lambda (_) (random))))


;; -- configuration

(setq org-modules '(org-habit))

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/refile.org")

(setq org-agenda-files '("~/Dropbox/org"))

(global-set-key (kbd "C-c c") 'org-capture)

; also text search (C-a s) in archive files
;(setq org-agenda-text-search-extra-files '(agenda-archives))

; force complete child tasks first (bug with zenburn: bad color)
;(setq org-enforce-todo-dependencies t)

;(setq org-log-done (quote time))   ; log defined below in org-todo-keywords
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

(setq org-habit-show-habits-only-for-today nil)
(setq org-habit-graph-column 50)
(setq org-habit-preceding-days 4)

; refile up to 3 levels in current/agenda files
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

; some colors not defined properly in zenburn
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   `(org-agenda-done ((t (:foreground ,zenburn-fg-1))))))

(defvar lava-org-date_added "\n  :PROPERTIES:\n  :DATE_ADDED: %U\n  :END:\n")

(setq org-capture-templates
      `((" " "simple entry" entry (file "~/Dropbox/Org/refile.org")
         "* %?")
        ("p" "new project" entry (file "~/Dropbox/Org/projects.org")
         "* %? %^g")
        ("t" "todo" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* TODO %?" lava-org-date_added))
        ("n" "note" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* %? :note:" lava-org-date_added))
        ("e" "event" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* %? %^T :event:" lava-org-date_added))
        ("m" "message" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* TODO contact %? :msg:" lava-org-date_added))
        ("b" "bank transfer" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* TODO transfer %? :bank:" lava-org-date_added))
        ("s" "shop" entry (file "~/Dropbox/Org/refile.org")
         ,(concat "* TODO buy :shop:" lava-org-date_added))))

(setq org-tag-alist (quote ((:startgroup)
                            ("@GFZ" . ?G)
                            ("@Home" . ?H)
                            ("@Berlin" . ?B)
                            (:endgroup)
                            ("perso" . ?p)
                            ("pixcie" . ?x)
                            ("admin" . ?a)
                            ("bank" . ?b)
                            ("urgent" . ?u)
                            ("idea" . ?i)
                            ("canceled". ?d)
                            ("note" . ?n)
                            ("music" . ?m)
                            ("roscoe" . ?r)
                            ("shop" . ?s))))

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
                 (org-agenda-skip-function 'lava-org-skip-non-stuck-projects)
                 (org-agenda-sorting-strategy
                  '(category-keep))))
          (tags "+LEVEL=1"
                ((org-agenda-overriding-header "All other projects")
                 (org-agenda-files '("~/Dropbox/org/projects.org"))
                 (org-agenda-skip-function 'lava-org-skip-stuck-projects)
                 (org-agenda-sorting-strategy
                  '(category-keep))))
          (tags "+idea+LEVEL=1"
                ((org-agenda-overriding-header "Five randomly selected ideas")
                 (org-agenda-max-entries 5)
                 (org-agenda-cmp-user-defined (lava-org-compare-randomly))
                 (lava-org-compare-random-refresh  t)
                 (org-agenda-sorting-strategy '(user-defined-up))))
          ))))


;; -- make windmove work in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


(provide 'org-conf)
;;;  org-conf.el ends here
