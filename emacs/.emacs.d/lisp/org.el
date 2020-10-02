(defun recker/org-scratch ()
  "Open a org mode *scratch* pad."
  (interactive)
  (switch-to-buffer "*org scratch*")
  (org-mode)
  (insert "#+TITLE: Org Scratch\n\n"))

(defun recker/today ()
  "Open today's journal entry."
  (interactive)
  (let* ((target
	  (recker/path 'src (format-time-string "blog/entries/%Y-%m-%d.md"))))
    (find-file target)))

(global-set-key (kbd "C-c n") 'recker/org-scratch)

(global-set-key (kbd "C-c t") 'recker/today)

(setq org-publish-project-alist '())

(setq org-confirm-babel-evaluate nil)

(setq org-cycle-separator-lines 1)

(setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(setq org-attach-directory (recker/path 'docs "attachments/"))

(setq org-attach-archive-delete 't)

(setq org-capture-templates '())

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files (list (recker/path 'docs)))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-start-with-follow-mode t)

(setq org-agenda-tag-filter-preset '("-ARCHIVE"))

(setq org-agenda-custom-commands '())

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ruby . t)
   (shell . t)))

(setq org-publish-project-alist '())
