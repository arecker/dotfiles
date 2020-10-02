(defun recker/macos-p ()
  "Returns T if running on MacOS."
  (string-equal system-type "darwin"))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))
