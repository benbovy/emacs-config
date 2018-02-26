;;; org-lava-utils.el --- Org utils -*- lexical-binding: t -*-
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'org)

;; from http://doc.norang.ca/org-mode.html
(defun org-lava-skip-non-stuck-projects ()
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

(defun org-lava-skip-stuck-projects ()
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
(defvar org-lava-compare-random-refresh nil
  "Whether `org-lava-compare-randomly' should refresh its keys.")

(defun org-lava-compare-get-marker (entry)
  "Return the marker for ENTRY."
  (get-text-property 1 'org-marker entry))

(defun org-lava-compare-randomly-update-sort-key (entry table generator)
  "Return sort key for ENTRY in TABLE, generating it with GENERATOR if necessary."
  (let* ((marker (org-lava-compare-get-marker entry))
         (hash-key `(,(marker-buffer marker) . ,(marker-position marker))))
    (or (gethash hash-key table)
        (puthash hash-key (funcall generator entry) table))))

(defun org-lava-compare-randomly-by (generator)
  "Return a random comparator using GENERATOR."
  (let ((table (make-hash-table :test #'equal)))
    (lambda (x y)
      (when org-lava-compare-random-refresh
        (clrhash table)
        (setq org-lava-compare-random-refresh nil))
      (let ((x-val (org-lava-compare-randomly-update-sort-key x table generator))
            (y-val (org-lava-compare-randomly-update-sort-key y table generator)))
        (cond
         ((= x-val y-val)  nil)
         ((< x-val y-val)   -1)
         ((> x-val y-val)   +1))))))

(defun org-lava-compare-randomly ()
  "Return a comparator implementing a random shuffle."
  (org-lava-compare-randomly-by (lambda (_) (random))))


(provide 'org-lava-utils)
;;;  org-lava-utils.el ends here
