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
(global-set-key (kbd "RET") 'newline-and-indent)



;; L&F

(defvar prelude-theme-dir (expand-file-name "themes" prelude-dir))
(add-to-list 'load-path prelude-theme-dir)
(disable-theme 'zenburn)
(load-theme 'tango-dark t)


;; Editor

(setq prelude-whitespace nil)

(require 'git-gutter-fringe)
(global-git-gutter-mode +1)

(require 'auto-complete)
(setq ac-auto-show-menu t
      ac-quick-help-delay 0.5
      ac-use-fuzzy t)

(defun my/ac-start-and-complete ()
  (interactive)
  (ac-start))

(global-auto-complete-mode +1)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-M-/") 'my/ac-start-and-complete)

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)


;; Filesystem

(require 'helm)
(require 'helm-projectile)

(defun my/helm-projectile ()
  (interactive)
  (helm :sources '(helm-source-projectile-files-list
                   helm-source-projectile-buffers-list)
        :buffer "*helm projectile*"
        :candidate-number-limit 50
        :prompt (projectile-prepend-project-name "pattern: ")))

(global-set-key (kbd "M-t") 'my/helm-projectile)


(require 'ido)
(define-key ido-file-completion-map (kbd "C-w") 'backward-kill-word)

;; Clean up whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)


;; Windows

(require 'window-numbering)
(window-numbering-mode 1)


;; elisp

(define-key emacs-lisp-mode-map (kbd "C-o") 'eval-defun)


;; Clojure

(define-key clojure-mode-map (kbd "C-o") 'cider-eval-expression-at-point)
(define-key clojure-mode-map (kbd "C-j") 'save-buffer)
(define-key clojure-mode-map (kbd "RET") 'paredit-newline)

(defun my/clojure-mode-hooks ()
  (my/turn-on 'paredit-mode 'rainbow-delimiters))

(add-hook 'clojure-mode-hook 'my/clojure-mode-hooks)

(defun my/cider-mode-hooks ()
  "Clojure specific setup code that should only be run when we
  have a CIDER REPL connection"
  (cider-turn-on-eldoc-mode))

(defun my/cider-run-tests ()
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (cider-interactive-eval (format "(load-file \"%s\") (run-tests)" (buffer-file-name))))

(require 'clojure-test-mode)
(define-key clojure-test-mode-map (kbd "C-c C-,") 'my/cider-run-tests)

(defun open-lein-dep ()
  (interactive)
  (flet ((parse-name-parts
          (name)
          (let ((pos (string-match (regexp-quote "/") name)))
            (if (numberp pos)
                (let* ((parts (split-string name "/"))
                       (first (car parts))
                       (second (car (cdr parts))))
                  (list first second))
              (list name name))))

         (name-path
          (parts)
          (concat (replace-regexp-in-string (regexp-quote ".") "/" (car parts))
                  "/"
                  (car (cdr parts))))

         (construct-jar-path
          (name-path lib version)
          (concat
           "~/.m2/repository/"
           name-path "/"
           version "/"
           lib "-" version ".jar")))
    (let ((dep-str (thing-at-point 'list)))
      (string-match "\\[\\([^\s]+\\)\s+\"\\([^\]]+\\)\"" dep-str)
      (let ((name (match-string 1 dep-str))
            (version (match-string 2 dep-str)))
        (when (and name version)
          (let* ((name-parts (parse-name-parts name))
                 (lib (car (cdr name-parts)))
                 (npath (name-path name-parts))
                 (jar-path (construct-jar-path npath lib version)))
            (find-file (file-truename jar-path))))))))

(define-clojure-indent
  (or 0)
  (and 0)
  (-> 0)
  (->> 0)
  (str 0)
  (merge 0)
  (= 0))


;; Paredit

(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") 'save-buffer)
(define-key paredit-mode-map (kbd "C-w") 'paredit-backward-kill-word)

;;(add-hook 'cider-mode-hook 'my/cider-mode-hooks)

(defun my/cider-repl-mode-hooks ()
  (my/turn-on 'paredit
              'rainbow-delimiters
              'highlight-parentheses))

(add-hook 'cider-repl-mode-hook 'my/cider-repl-mode-hooks)

(setq prelude-guru nil)


;; CSS

(setq css-indent-offset 2)
(setq js-indent-level 2)

(defun my/scss-mode-hooks ()
  (my/turn-on 'smartparens-mode 'auto-complete-mode))

(add-hook 'scss-mode-hook 'my/scss-mode-hooks)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;(require 'smooth-scrolling)
(setq scroll-margin 5)


(require 'flx-ido)
(flx-ido-mode +1)