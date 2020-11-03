(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Add kernel style
	    (c-add-style
	     "linux-tabs-only"
	     '("linux" (c-offsets-alist
			(arglist-cont-nonempty
			 c-lineup-gcc-asm-reg
			 c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook (lambda ()
			 (setq indent-tabs-mode t)
			 (setq show-trailing-whitespace t)
			 (c-set-style "linux-tabs-only")))

(use-package slime
  :ensure t
  :defer t
  :config (setq inferior-lisp-program (executable-find "sbcl")))

(use-package d-mode
  :ensure t
  :defer t
  :mode "\\.d\\'")

(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "\\Dockerfile\\'")

(use-package groovy-mode
  :ensure t
  :defer t
  :mode "\\Jenkinsfile\\'")

(use-package haskell-mode
  :ensure t
  :defer t
  :mode "\\.hs\\'")

(use-package emmet-mode
  :ensure t
  :defer t
  :init (setq emmet-preview-default nil)
  :config (add-hook 'sgml-mode-hook 'emmet-mode))

(use-package go-mode
  :ensure t
  :defer t
  :config (let ((govet (flycheck-checker-get 'go-vet 'command)))
	    (when (equal (cadr govet) "tool")
	      (setf (cdr govet) (cddr govet)))))

(setq js-indent-level 2)

(use-package jsonnet-mode
  :ensure t
  :defer t
  :mode ("\\.jsonnet\\'" "\\.libsonnet\\'"))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package terraform-mode
  :ensure t
  :defer t
  :mode "\\.tf\\'")

(use-package lua-mode
  :ensure t
  :defer t
  :mode ("\\.lua\\'" "\\.p8\\'"))

(setq ruby-deep-indent-paren nil)
