(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

(defun my/->string (str)
  (cond
   ((stringp str) str)
   ((symbolp str) (symbol-name str))))

(defun my/->mode-hook (name)
  "Turn mode name into hook symbol"
  (intern (replace-regexp-in-string "\\(-mode\\)?\\(-hook\\)?$"
                                    "-mode-hook"
                                    (my/->string name))))

(defun my/->mode (name)
  "Turn mode name into mode symbol"
  (intern (replace-regexp-in-string "\\(-mode\\)?$"
                                    "-mode"
                                    (my/->string name))))

(defun my/turn-on (&rest mode-list)
  "Turn on the given (minor) modes."
  (dolist (m mode-list)
    (funcall (my/->mode m) +1)))

;; Key Setup

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-use-native-fullscreen nil)

;;(set-fringe-style 1)

(toggle-scroll-bar 0)
(global-hl-line-mode -1)
(key-chord-mode -1)

;; Global Key Overrides

(global-set-key (kbd "M-SPC") 'helm-mini)
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-j") 'save-buffer)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-<backspace>") 'kill-region)
(global-set-key (kbd "M-\\") 'kill-whitespace)
(global-set-key (kbd "M-x") 'smex)



;; L&F

(defvar prelude-theme-dir (expand-file-name "themes" prelude-dir))
(add-to-list 'load-path prelude-theme-dir)
(disable-theme 'zenburn)
(load-theme 'tango-dark t)


;; Editor

(require 'git-gutter-fringe)
(global-git-gutter-mode +1)

(require 'auto-complete)
(setq ac-auto-show-menu t
      ac-quick-help-delay 0.5
      ac-use-fuzzy t)

(global-auto-complete-mode +1)
(global-set-key (kbd "M-/") 'ac-expand)

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;;

;; elisp

(define-key emacs-lisp-mode-map (kbd "C-o") 'eval-defun)


;; Clojure

(defun my/cider-mode-hooks ()
  "Clojure specific setup code that should only be run when we
  have a CIDER REPL connection"
  (cider-turn-on-eldoc-mode))

;;(add-hook 'cider-mode-hook 'my/cider-mode-hooks)

(defun my/cider-repl-mode-hooks ()
  (my/turn-on 'paredit
              'rainbow-delimiters
              'highlight-parentheses))

(add-hook 'cider-repl-mode-hook 'my/cider-repl-mode-hooks)
