;;; python.el --- Python configuration
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; -- Elpy
;; (prelude-require-package 'elpy)
;; (require 'elpy)
;; (elpy-enable)
;; disable anaconda-mode
;; (add-hook 'elpy-mode-hook (lambda () (anaconda-mode 0)))

;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")

;; disable flymake used by elpy (we use flycheck instead)
;; (when (require 'flycheck nil t)
;;    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))


;; anaconda-mode doc
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)


;; disable which-function in Python mode
;; really bad performance for big files with many functions
(add-hook 'python-mode-hook (lambda () (which-function-mode -1)))


;; -- Conda
;; Trick to select conda environment within emacs
;; (see http://emacs.stackexchange.com/questions/20092/using-conda-environments-in-emacs)
;; emacs must be run from a conda env activated!!
;; TODO: by default select conda base environment if none is activated?
(prelude-require-package 'pyvenv)
(require 'pyvenv)

(defvar conda-env-dir)

(if (getenv "CONDA_PREFIX")
    (progn
     (setq conda-env-dir (file-name-directory (directory-file-name (getenv "CONDA_PREFIX"))))
     (message "Conda environments directory: %s" conda-env-dir)
     (setenv "WORKON_HOME" conda-env-dir)
     (pyvenv-mode 1)
     (defalias 'conda-workon 'pyvenv-workon)
     (pyvenv-activate (getenv "CONDA_PREFIX"))
     (message "Conda environment %s is activated" (getenv "CONDA_PREFIX"))
    )
)


(provide 'python)
;;;  python.el ends here
