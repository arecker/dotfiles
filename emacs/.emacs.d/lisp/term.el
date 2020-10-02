(defun recker/ansi-term ()
  (interactive)
  (ansi-term "/bin/bash"))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-x t") 'recker/ansi-term)
