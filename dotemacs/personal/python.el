;;; python.el --- Python configuration
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'use-package)


;; -- Elpy
(prelude-require-package 'elpy)

(use-package elpy
  :disabled
  :init
  (add-hook 'elpy-mode-hook (lambda () (anaconda-mode 0)))
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(use-package elpy
  :disabled
  :requires flycheck
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))


;; anaconda-mode doc
(add-hook 'python-mode 'anaconda-eldoc-mode)


;; disable which-function in Python mode
;; really bad performance for big files with many functions
(add-hook 'python-mode-hook (lambda () (which-function-mode -1)))


;; -- Conda
(prelude-require-package 'pyvenv)

(defvar conda-env-dir)

(use-package pyvenv
  :config
  ;; Trick to select conda environment within emacs
  ;; (see http://emacs.stackexchange.com/questions/20092/using-conda-environments-in-emacs)
  ;; emacs must be run from a conda env activated!!
  ;; TODO: by default select conda base environment if none is activated?
  ;; TODO: doesn't work with base environment
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
  )


;; -- pytest
(prelude-require-package 'python-pytest)
(use-package python-pytest)


(provide 'python)
;;;  python.el ends here
