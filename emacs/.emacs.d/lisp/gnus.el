(setq gnus-select-method '(nnml ""))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-startup-file (recker/path 'docs "emacs/newsrc"))

(setq gnus-use-dribble-file nil)

(setq gnus-asynchronous t)

(setq gnus-use-cache t)

(setq gnus-home-directory (recker/path 'docs "emacs/gnus")
      nnfolder-directory (recker/path 'docs "emacs/gnus/Mail/archive")
      message-directory (recker/path 'docs "emacs/gnus/Mail")
      nndraft-directory (recker/path 'docs "emacs/gnus/Drafts")
      gnus-cache-directory (recker/path 'docs "emacs/gnus/cache"))

(setq gnus-secondary-select-methods '())

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "personal"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port "imaps")
		      (nnimap-stream ssl)
		      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
		      (nnmail-expiry-wait immediate)))

(setq gnus-message-archive-group nil)

(use-package bbdb
  :ensure t
  :config (setq bbdb-file (recker/path 'docs "emacs/bbdb.el"))
  :init
  (bbdb-mua-auto-update-init 'message)
  (setq bbdb-mua-auto-update-p 'query)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

(setq smtpmail-smtp-service 587
      smtpmail-smtp-user "alex@reckerfamily.com"
      smtpmail-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it)

(add-to-list 'auth-sources (recker/path 'docs "emacs/authinfo.gpg"))
