(defun tv-require (feature &optional filename noerror)
  (message "Loading %s..." (symbol-name feature))
  (condition-case err
      (if (require feature filename noerror)
          (message "Loading %s done" (symbol-name feature))
        (message "Loading %s Failed" (symbol-name feature)))
    (error
     (signal 'error (list feature (car err) (cadr err))))))

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


;; Repos

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

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

;;(global-set-key (kbd "M-/") 'hippie-expand)

;; Filesystem

(require 'helm)
(require 'helm-projectile)

(defun my/helm-projectile ()
  (interactive)
  (helm-occur-init-source)
  (setq helm-multi-occur-buffer-list (list (buffer-name (current-buffer))))
  (helm :sources '(helm-source-projectile-files-list
                   ;;helm-source-projectile-buffers-list
                   helm-source-moccur)
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

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(require 'clojure-mode)

(define-key clojure-mode-map (kbd "C-o") 'cider-eval-defun-at-point)
(define-key clojure-mode-map (kbd "C-j") 'save-buffer)
(define-key clojure-mode-map (kbd "RET") 'paredit-newline)

(defun my/clojure-mode-hooks ()
  (my/turn-on 'paredit-mode 'rainbow-delimiters))

(add-hook 'clojure-mode-hook 'my/clojure-mode-hooks)

(defun my/cider-mode-hooks ()
  "Clojure specific setup code that should only be run when we
  have a CIDER REPL connection"
  (cider-turn-on-eldoc-mode))

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
  (= 0)
  (not= 0))

(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))

(setq cider-auto-select-error-buffer nil)


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

(define-key clojure-mode-map (kbd "C-c C-j") 'cider-restart)





;; CSS

(setq css-indent-offset 2)
(setq js-indent-level 2)

(defun my/scss-mode-hooks ()
  (my/turn-on 'smartparens-mode 'auto-complete-mode 'flycheck-mode))

(add-hook 'scss-mode-hook 'my/scss-mode-hooks)

(add-hook 'css-mode-hook 'my/scss-mode-hooks)

(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]+"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(defun hexcolor-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolor-keywords))

(add-hook 'css-mode-hook 'hexcolor-add-to-font-lock)


;; ORG

(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;(require 'smooth-scrolling)
(setq scroll-margin 5)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'flx-ido)
(flx-ido-mode +1)


;; Ruby

(require 'ruby-mode)
(define-key ruby-mode-map (kbd "M-q") 'indent-region)


(setq org-src-fontify-natively t)

(setq ido-everywhere t)


;; Company Mode

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
   `(company-tooltip-common-selection ((t (:inherit font-lock-keyword-face))))))



;; Don't split windows automatically
(setq pop-up-windows nil)

(defun nrepl-reup ()
  (interactive)
  (cider-interactive-eval "(user/reup)"))

(global-set-key (kbd "C-c C-o") 'nrepl-reup)

(global-set-key (kbd "M-o") 'helm-occur)
(define-key prelude-mode-map (kbd "M-o") 'helm-occur)

(setq helm-follow-mode-persistent t)
(helm-occur-init-source)

(helm-attrset 'follow 1 helm-source-occur)
(helm-attrset 'follow 1 helm-source-moccur)

(require 'thingatpt)

(defun find-clojurescript-tag ()
  (interactive)
  (find-tag (car (last (split-string (thing-at-point 'symbol) "/")))))

(defun find-clojurescript-tag-other-window ()
  (interactive)
  (find-tag-other-window (car (last (split-string (thing-at-point 'symbol) "/")))))

(define-key clojure-mode-map (kbd "M-i") 'find-clojurescript-tag)
(define-key clojure-mode-map (kbd "M-I") 'find-clojurescript-tag-other-window)

;; Don't visit tags table after regen
(defun zk-regenerate-tags ()
  "Regenerate the project's [e|g]tags."
  (interactive)
  (if (boundp 'ggtags-mode)
      (progn
        (let* ((ggtags-project-root (projectile-project-root))
               (default-directory ggtags-project-root))
          (ggtags-ensure-project)
          (ggtags-update-tags t)))
    (let* ((project-root (projectile-project-root))
           (tags-exclude (projectile-tags-exclude-patterns))
           (default-directory project-root)
           (tags-file (expand-file-name projectile-tags-file-name))
           (command (format projectile-tags-command tags-file tags-exclude))
           shell-output exit-code)
      (with-temp-buffer
        (setq exit-code
              (call-process-shell-command command nil (current-buffer))
              shell-output
              (s-trim (buffer-substring (point-min) (point-max)))))
      (unless (zerop exit-code)
        (error shell-output)))))

;; (add-hook 'clojure-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'zk-regenerate-tags nil 'make-it-local)))

(setq backup-directory-alist `(("." . "~/.em-saves")))

(set-face-attribute 'default nil :height 120)
(set-default-font "Monaco")
(setq-default line-spacing 0)

;; Prevent CTRL-Z from minimizing Emacs
(global-set-key (kbd "C-Z") nil)


;; Winner undo / cancel

(global-set-key (kbd "M-[") 'winner-undo)
(define-key prog-mode-map (kbd "M-[") 'winner-undo)
(global-set-key (kbd "M-]") 'winner-redo)

(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              "*esh command on file*"
                              ))

(when (tv-require 'winner)
  (defvar winner-boring-buffers-regexp "\\*[hH]elm.*")
  (defun winner-set1 (conf)
    ;; For the format of `conf', see `winner-conf'.
    (let* ((buffers nil)
           (alive
            ;; Possibly update `winner-point-alist'
            (cl-loop for buf in (mapcar 'cdr (cdr conf))
                     for pos = (winner-get-point buf nil)
                     if (and pos (not (memq buf buffers)))
                     do (push buf buffers)
                     collect pos)))
      (winner-set-conf (car conf))
      (let (xwins)                      ; to be deleted

        ;; Restore points
        (dolist (win (winner-sorted-window-list))
          (unless (and (pop alive)
                       (setf (window-point win)
                             (winner-get-point (window-buffer win) win))
                       (not (or (member (buffer-name (window-buffer win))
                                        winner-boring-buffers)
                                (string-match winner-boring-buffers-regexp
                                              (buffer-name (window-buffer win))))))
            (push win xwins)))          ; delete this window

        ;; Restore marks
        (letf (((current-buffer)))
              (cl-loop for buf in buffers
                       for entry = (cadr (assq buf winner-point-alist))
                       for win-ac-reg = (winner-active-region)
                       do (progn (set-buffer buf)
                                 (set-mark (car entry))
                                 (setf win-ac-reg (cdr entry)))))
        ;; Delete windows, whose buffers are dead or boring.
        ;; Return t if this is still a possible configuration.
        (or (null xwins)
            (progn
              (mapc 'delete-window (cdr xwins)) ; delete all but one
              (unless (one-window-p t)
                (delete-window (car xwins))
                t))))))

  (defalias 'winner-set 'winner-set1))

(winner-mode 1)
