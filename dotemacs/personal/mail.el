;;; mail.el --- My Emacs Configuration on top of Prelude.
;;;
;;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(prelude-require-packages '(smtpmail bbdb))

(require 'use-package)

(use-package mu4e
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-get-mail-command "mbsync gmail")
  (setq mu4e-update-interval 300)   ;; 5 mins

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq mu4e-maildir "~/Maildir")
  (setq mu4e-drafts-folder "/gmail/drafts")
  (setq mu4e-sent-folder "/gmail/sent")
  (setq mu4e-trash-folder "/gmail/trash")

  (setq mu4e-maildir-shortcuts
        '(("/gmail/inbox"  . ?i)
          ("/gmail/sent"   . ?s)))
  )


(provide 'mail)
;;;  mail.el ends here
