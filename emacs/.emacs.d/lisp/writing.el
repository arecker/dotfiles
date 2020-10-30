(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"
	      markdown-fontify-code-blocks-natively t))

(use-package writegood-mode
  :ensure t
  :init
  (add-hook 'org-mode-hook 'writegood-mode)
  (add-hook 'markdown-mode-hook 'writegood-mode))

(setq ispell-personal-dictionary (recker/path 'docs "emacs/ispell.dict"))

(setq ispell-program-name (executable-find "ispell"))

(use-package dictionary :ensure t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'text-mode-hook #'(lambda () (flyspell-mode t)))

(use-package rst
  :mode (("\\.rst$" . rst-mode)))

(defun recker/filename-to-alt (filename)
  "Convert a filepath to an HTML alt attribute."
  (let ((name (file-name-sans-extension filename))
	(chars '(?_ ?- ?/)))
    (dolist (char chars)
      (setf name (subst-char-in-string char ?\s name)))
    name))

(defun recker/insert-figure (filename caption)
  "Insert an HTML figure and caption."
  (interactive "sFilename: 
sCaption: ")
  (message "%s" caption)
  (let* ((src (format "/images/%s" filename))
	 (alt (recker/filename-to-alt filename))
	 (img (format "<a href=\"%s\">\n<img alt=\"%s\" src=\"%s\"/>\n</a>" src alt src))
	 (figcaption (if (not (string-equal caption ""))
			 (format "<figcaption>\n<p>%s</p>\n</figcaption>" caption))))
    (insert
     (if (string-equal caption "")
	 (format "<figure>\n%s\n</figure>" img)
       (format "<figure>\n%s\n%s\n</figure>" img figcaption)))))

(defun recker/today ()
  "Open today's journal entry."
  (interactive)
  (let* ((target
	  (recker/path 'src (format-time-string "blog/entries/%Y-%m-%d.md")))
	 (frontmatter
	  (format-time-string "---\ntitle:\nbanner: %Y-%m-%d.jpg\n---\n\n")))
    (if (file-exists-p target)
	(find-file target)
      (progn (find-file target)
	     (insert frontmatter)))))

