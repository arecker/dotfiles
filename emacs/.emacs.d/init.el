(defun recker/load (file)
  "Load a elisp file from ~/.emacs.d/lisp."
  (let ((filename (format "%s.el" file)))
    (load-file (expand-file-name filename "~/.emacs.d/lisp/"))))

(setq user-full-name "Alex Recker"
      user-mail-address "alex@reckerfamily.com")

(recker/load "package")
(recker/load "os")
(recker/load "interface")
(recker/load "files")
(recker/load "font")
(recker/load "text")
(recker/load "code")
(recker/load "term")
(recker/load "buffer")
(recker/load "scratch")
(recker/load "git")
(recker/load "pass")
(recker/load "org")
(recker/load "writing")
(recker/load "gnus")
(recker/load "local")

(setq tetris-score-file (recker/path 'docs "emacs/tetris-scores"))
