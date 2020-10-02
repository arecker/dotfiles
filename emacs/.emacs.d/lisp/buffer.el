(defun recker/purge-buffers ()
  "Delete all buffers, except for *scratch*."
  (interactive)
  (mapc #'(lambda (b) (unless (string= (buffer-name b) "*scratch*") (kill-buffer b))) (buffer-list)))

(global-set-key (kbd "C-x C-k k") 'kill-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x P") 'recker/purge-buffers)
