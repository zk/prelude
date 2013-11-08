;;; smart-mode-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "smart-mode-line" "smart-mode-line.el" (21099
;;;;;;  13446 0 0))
;;; Generated autoloads from smart-mode-line.el

(autoload 'sml/setup "smart-mode-line" "\
Setup the mode-line, or revert it.

If argument is a non-positive integer, revert any changes made.
Otherwise, setup the mode-line.

This should be called after any themes have been applied, which
is why it is better to add as an `after-init-hook' than to be
called straight from your init file.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; smart-mode-line-autoloads.el ends here
