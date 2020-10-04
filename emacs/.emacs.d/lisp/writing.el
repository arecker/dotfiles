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
