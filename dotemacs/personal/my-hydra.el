;;; my-hydra.el --- custom Hydras.
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(prelude-require-package 'hydra)

(use-package smartparens
  :ensure t
  :bind (("M-s" . hydra-smartparens/body))
  :custom
  ;; disable some annoying key-bindings as hydra is used instead
  (sp-override-key-bindings
   '(("C-<left>" . nil)
     ("C-<right>" . nil)
     ("M-s" . nil)))
  :init
  (defhydra hydra-smartparens (:hint nil)
    "
.: Smartparens :.

_q_ quit

^Navigation^               ^Barf/Slurp^                   ^Wrap^
-------------------------------------------------------------------------------
_<left>_   backward        _C-<left>_    slurp forward    _(_ wrap with ( )
_<right>_  forward         _C-<right>_   barf forward     _[_ wrap with [ ]
_<up>_     up forward      _C-S-<left>_  slurp backward   _{_ wrap with { }
_<down>_   down forward    _C-S-<right>_ barf backward    _\"_ wrap with \" \"
_S-<up>_   up backward     ^^                             _'_ wrap with ' '
_S-<down>_ down backward   ^^                             _\\_ wrap with \\ \\
_a_        beginning       ^^                             _`_ wrap with ` `
_e_        end             ^^                             _u_ unwrap
^^                         ^^                             _U_ unwrap (backward)

^Kill^           ^Misc^
-------------------------------------------------------------------------------
_w_ copy        _j_ join
_k_ kill        _s_ split
^^              _t_ transpose (swap)"
    ("q" nil)
    ;; Navigation
    ("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("<left>" sp-backward-sexp)
    ("<right>" sp-forward-sexp)
    ("<up>" sp-up-sexp)
    ("<down>" sp-down-sexp)
    ("S-<up>" sp-backward-up-sexp)
    ("S-<down>" sp-backward-down-sexp)
    ;; Barfing/slurping
    ("C-<right>" sp-forward-slurp-sexp)
    ("C-<left>" sp-forward-barf-sexp)
    ("C-S-<left>" sp-backward-slurp-sexp)
    ("C-S-<right>" sp-backward-barf-sexp)
    ;; Wrapping
    ("(" (lambda (a) (interactive "P") (sp-wrap-with-pair "(")))
    ("[" (lambda (a) (interactive "P") (sp-wrap-with-pair "[")))
    ("{" (lambda (a) (interactive "P") (sp-wrap-with-pair "{")))
    ("\"" (lambda (a) (interactive "P") (sp-wrap-with-pair "\"")))
    ("'" (lambda (a) (interactive "P") (sp-wrap-with-pair "'")))
    ("\\" (lambda (a) (interactive "P") (sp-wrap-with-pair "\\")))
    ("`" (lambda (a) (interactive "P") (sp-wrap-with-pair "`")))
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp))
  )


(use-package ace-window
  :ensure t
  :bind (("M-w" . hydra-window/body))
  :init
  (defhydra hydra-window (:hint nil)
    "
.: Windows :.

_q_ quit

^Navigatqion^      ^Resize^                    ^Split^
-------------------------------------------------------------------------------
_<left>_  ←        _p_   enlarge horizontal    _v_ vertical
_<down>_  ↓        _m_   shrink horizontal     _h_ horizontal
_<up>_    ↑        _C-p_ enlarge vertical
_<right>_ →        _C-m_ shrink vertical

^Delete^                ^Undo/Redo^       ^Misc^
-------------------------------------------------------------------------------
_k_ delete current      _z_ undo          _a_ select (ace)
_o_ delete others       _y_ redo          _f_ toggle follow mode
"
    ("q" nil)
    ;; Navigation
    ("<left>" windmove-left)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("<right>" windmove-right)
    ;; Resize
    ("p" enlarge-window-horizontally)
    ("m" shrink-window-horizontally)
    ("C-p" enlarge-window)
    ("C-m" shrink-window)
    ;; Split
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     )
    ("h" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     )
    ;; Delete
    ("k" delete-window)
    ("o" delete-other-windows)
    ;; Undo/Redo
    ("z" (progn
           (winner-undo)
           (setq this-command 'winner-undo))
     )
    ("y" winner-redo)
    ;; Misc
    ("f" follow-mode :color blue)
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))))
  )


(provide 'my-hydra)
;;;  my-hydra.el ends here
