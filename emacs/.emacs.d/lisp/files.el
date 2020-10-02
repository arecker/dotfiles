(defun recker/path (dir &optional subpath)
  (let* ((macos-p (string-equal system-type "darwin"))
	 (dir (pcase dir
		('home "~")
		('desktop (if macos-p "~/Desktop" "~/desktop"))
		('docs (if macos-p "~/Documents" "~/docs"))
		('pics (if macos-p "~/Pictures" "~/pics"))
		('public (if macos-p "~/Public" "~/public"))
		('src "~/src")
		('emacs user-emacs-directory)
		(_ (error (format "no %s directory!" dir)))))
	 (subdir (or subpath "")))
    (expand-file-name (concat (file-name-as-directory dir) subpath))))

(setq default-directory (recker/path 'home))

(setq make-backup-files nil
      auto-save-default nil)

(global-auto-revert-mode 1)

(setq custom-file (recker/path 'emacs "lisp/custom.el"))

;; file registers
(set-register ?c '(file . "/ssh:console:/home/alex"))
(set-register ?d `(file . ,(recker/path 'desktop)))
(set-register ?e `(file . ,(recker/path 'emacs)))
(set-register ?f '(file . "/ssh:alex@archive.local:/mnt/"))
(set-register ?l `(file . ,(recker/path 'emacs "local.el")))
(set-register ?m `(file . ,(recker/path 'docs "mixtape.org")))
(set-register ?o `(file . ,(recker/path 'docs "opsat.org")))

;; bookmarks
(require 'bookmark)
(setq bookmark-save-flag 1
      bookmark-default-file (recker/path 'docs "emacs/bookmarks.el"))

(defun recker/ido-bookmark-jump (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))

(global-set-key (kbd "C-x r b") 'recker/ido-bookmark-jump)

;; dired
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq dired-listing-switches "-lh")
(setq dired-use-ls-dired nil)
(setq dired-clean-confirm-killing-deleted-buffers nil)

(use-package projectile
  :ensure t
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :init (projectile-mode t))
