(setq-default recker/regular-font (if (recker/macos-p)
				      "Monaco 16"
				    "Inconsolata 13"))

(setq-default recker/large-font (if (recker/macos-p)
				    "Monaco 20"
				  "Inconsolata 18"))

(when (display-graphic-p)
  (set-frame-font recker/regular-font nil t))

(defun recker/toggle-font-size ()
  (interactive)
  (unless (display-graphic-p)
    (error "Running in -nw mode, font won't work"))
  (let* ((current-font-obj (face-attribute 'default :font))
	 (current-font (format "%s %s"
			       (font-get current-font-obj :family)
			       (font-get current-font-obj :size)))
	 (desired-font (if (string-equal current-font recker/regular-font)
			   recker/large-font
			 recker/regular-font)))
    (set-frame-font (progn (message "Changing font to %s" desired-font) desired-font) t t)))

(global-set-key (kbd "C-c C-x f") 'recker/toggle-font-size)
