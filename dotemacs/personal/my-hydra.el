;;; my-hydra.el --- custom Hydras.
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'use-package)

(prelude-require-package 'hydra)


(use-package hydra
  :defer 0.5
  :bind (("M-w" . hydra-window/body)
         ("M-s" . hydra-smartparens/body)
         ("M-m" . hydra-magit/body)
         ("M-y" . hydra-yasnippet/body)
         ("M-f" . hydra-flycheck/body)
         ("M-F" . hydra-flyspell/body))
  )


(defhydra hydra-magit (:color blue :hint nil)
  "
.: Magit :.

_q_ quit

^New/Clone^       ^Actions^
^---------^-------^-------^----------------------------------------------------
_i_ init          _s_ status
_c_ clone         _b_ blame
  "
  ("q" nil)
  ;; New/Clone
  ("i" magit-init)
  ("c" magit-clone)
  ("s" magit-status)
  ("b" magit-blame)
  )


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
  ("s" sp-split-sexp)
  )


(defhydra hydra-window (:hint nil)
  "
.: Windows :.

_q_ quit

^Navigation^       ^Resize^                    ^Split^
-------------------------------------------------------------------------------
_<left>_  ←        _p_   enlarge horizontal    _v_ vertical
_<down>_  ↓        _m_   shrink horizontal     _h_ horizontal
_<up>_    ↑        _C-p_ enlarge vertical
_<right>_ →        _C-m_ shrink vertical

^Delete^                ^Undo/Redo^       ^Misc^
-------------------------------------------------------------------------------
_k_ delete current      _z_ undo          _a_ select (ace)
_o_ delete others       _y_ redo          _f_ toggle follow mode
^^                      ^^                _s_ swap
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
                   'hydra-window/body)))
  ("s" ace-swap-window)
  )


(defhydra hydra-yasnippet (:color blue :hint nil)
  "
.: Yasnippet :.

_q_ quit

^Mode^                    ^Actions^
^----^--------------------^-------^--------------------------------------------
_g_ toggle global mode    _i_ insert snippet
_m_ toogle minor mode     _n_ new snippet
  "
  ("q" nil)
  ;; mode
  ("g" yas-global-mode)
  ("m" yas-minor-mode)
  ;; actions
  ("i" ivy-yasnippet)
  ("n" yas-new-snippet)
  )


(defhydra hydra-flycheck (:color blue :hint nil)
  "
.: Flycheck :.

_q_ quit

^Mode^            ^Navigation^          ^Checker^
^----^------------^----------^----------^-------^-------------------------------
_m_ toggle mode   _<up>_   previous     _s_ select
^^                _<down>_ next         _d_ disable
^^                _l_      list         _?_ describe
^^                ^^                    _v_ verify setup
  "
  ("q" nil)
  ;; Mode
  ("m" flycheck-mode)
  ;; Navigation
  ("<up>" flycheck-previous-error :color pink)
  ("<down>" flycheck-next-error :color pink)
  ("l" flycheck-buffer-and-list-errors)
  ;; Checker
  ("s" flycheck-select-checker)
  ("d" flycheck-disable-checker)
  ("?" flycheck-describe-checker)
  ("v" flycheck-verify-setup)
  )


(defhydra hydra-flyspell (:color blue :hint nil)
  "
.: Flyspell :.

_q_ quit

^Mode^            ^Navigation^         ^Checker^
^----^------------^----------^---------^-------^-------------------------------
_m_ toggle mode   _<up>_   previous     _f_ run flycheck for current buffer
^^                _<down>_ next         _b_ run ispell for current buffer
^^                _c_      current      _r_ run ispell for current region
^^                ^^                    _d_ select dictionary
  "
  ("q" nil)
  ;; Mode
  ("m" flyspell-mode)
  ;; Navigation
  ("<up>" flyspell-correct-previous :color pink)
  ("<down>" flyspell-correct-next :color pink)
  ("c" flyspell-correct-word-generic)
  ;; Checker
  ("f" flyspell-buffer :color pink)
  ("b" ispell)
  ("r" ispell-region)
  ("d" ispell-change-dictionary :color pink)
  )


(provide 'my-hydra)
;;;  my-hydra.el ends here
