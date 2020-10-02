(menu-bar-mode 0)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package rich-minority
  :ensure t
  :init (rich-minority-mode 1)
  :config (setq rm-blacklist ""))
