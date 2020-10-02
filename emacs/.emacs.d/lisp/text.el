(setq indent-tabs-mode nil)

(use-package editorconfig
  :ensure t
  :defer t
  :config (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(defun recker/unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical line."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(delete-selection-mode t)

(global-set-key (kbd "C-c r") 'replace-string)

(global-set-key (kbd "C-c l") 'sort-lines)

(use-package swiper
  :ensure t
  :bind ("C-c s" . swiper))
