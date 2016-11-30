;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; font settings
(set-face-attribute 'default nil :font "Source Code Pro") 

;; modes
(electric-indent-mode 0)

;; global keybindings
(global-unset-key (kbd "C-z"))

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c M-/") 'uncomment-region)


;; themes
(use-package solarized-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized-dark t)

(add-to-list 'exec-path "/Users/bcarlson/.sdkman/candidates/scala/current/bin")
(add-to-list 'exec-path "/Users/bcarlson/.sdkman/candidates/sbt/current/bin")
(add-to-list 'exec-path "/Users/bcarlson/.sdkman/candidates/maven/current/bin")
(add-to-list 'exec-path "/usr/local/bin/")

(setenv "PATH" (mapconcat 'identity exec-path path-separator))
(setenv "MAVEN_OPTS" "-Djava.awt.headless=true")

(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

;; (use-package flx-ido
;;   :demand
;;   :init
;;   (setq
;;    ido-enable-flex-matching t
;;    ;; C-d to open directories
;;    ;; C-f to revert to find-file
;;    ido-show-dot-for-dired nil
;;    ido-enable-dot-prefix t)
;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (flx-ido-mode 1))

(use-package emojify)

(require 'helm)
(require 'helm-config)
(use-package helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "M-x") 'undefined)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)


;; (use-package jdee)
(use-package mvn)

(use-package ensime 
  :ensure t
  :pin melpa-stable)
(setq ensime-use-helm t)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package magit
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("C-x g" . magit-status)))

(use-package magithub
  :after magit)

(use-package magit-gitflow)

(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

(use-package emmet-mode)

(bind-key "C-x C-g" 'goto-line)

;; Mac keybindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )


(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

(use-package editorconfig
  :ensure t
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1))
  (add-hook 'editorconfig-mode-hook
            (add-hook 'before-save-hook
                      (lambda () (if indent-tabs-mode
                                     (tabify (point-min) (point-max))
                                   (untabify (point-min) (point-max)))))))



(use-package alchemist)

(defcustom
  scala-mode-prettify-symbols
  '(("->" . ?→)
    ("<-" . ?←)
    ("=>" . ?⇒)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("==" . ?≡)
    ("!=" . ?≠)
    ;; implicit https://github.com/chrissimpkins/Hack/issues/214
    ("+-" . ?±))
  "Prettify symbols for scala-mode.")

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
       (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode) 
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x _") 'split-window-vertically)
(global-set-key (kbd "C-c z") (lambda ()  (interactive) (ansi-term "/bin/zsh")))

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(add-hook 'scala-mode-hook
          (lambda ()
            (electric-indent-local-mode +1)
            (show-paren-mode)
            (smartparens-mode)
            (yas-minor-mode)
            (setq prettify-symbols-alist scala-prettify-symbols-alist)
            (prettify-symbols-mode)
;            (company-mode)
            (scala-mode:goto-start-of-code)))

(add-hook 'java-mode-hook
          (lambda () 
            (ensime)))

(add-hook 'heml-mode-hook
          (lambda ()
            (emmet-mode)))


;; This stuff is to ansi-colorize the compilation buffer after a rails test so the terminal colors come through.
(define-derived-mode ansi-compilation-mode compilation-mode "ansi compilation"
  "Compilation mode that understands ansi colors."
  (require 'ansi-color)
  (toggle-read-only 0)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun colorize-compilation (one two)
  "ansi colorize the compilation buffer."
  (ansi-compilation-mode)
 )

(setq compilation-finish-function 'colorize-compilation)

;; Compilation Mode ;;
(require 'compile)
(add-hook 'compilation-mode (lambda () (next-error-follow-minor-mode 1)))
(add-hook 'compilation-mode-hook 
          (lambda () 
            (setenv "MAVEN_OPTS" "-Djava.awt.headless=true")))
(add-hook 'compilation-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(
               ("^\\[WARNING\\]" . compilation-warning-face)
               ("^\\[ERROR\\]" . compilation-error-face)
               ("^\\[INFO\\].*$" . compilation-info-face)
               ("^.*Exception$" . compilation-error-face)
               ("^.*Caused by\\:.*$" . compilation-error-face)
               ("^\\s-\\(at\\s-[^(]+\\)\(\\([^\\:\n]+\\)\)" . compilation-error-face)))))
(add-hook 'compilation-filter-hook
          ;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
          (lambda ()
            (progn
              (toggle-read-only)
              (ansi-color-apply-on-region compilation-filter-start (point))
              (toggle-read-only))))
(setq compilation-error-regexp-alist-alist
      '((mvn-scala-warn . ("^\\(\\[WARNING\\]\\)\\s-\\(/.+?\\)\\:\\([[:digit:]]+\\)\\:\\(.*?\\)$" 2 3 nil 1 nil (1 compilation-warning-face) (4 compilation-info-face)))
        (mvn-scala-err . ("^\\(\\[ERROR\\]\\)\\s-\\(/.+?\\)\\:\\([[:digit:]]+\\)\\:\\(.*?\\)$" 2 3 nil 2 nil (1 compilation-error-face) (4 compilation-info-face)))
        (mvn-java-err . ("^\\(\\[ERROR\\]\\)\\s-\\(/[^\\:\n]+\\)[^[:digit:]]+\\([[:digit:]]+\\),\\([[:digit:]]+\\)\\]\\(.*\\)$" 2 3 4 2 nil (1 compilation-error-face) (5 compilation-error-face)))
        (mvn-java-warning . ("^\\(\\[WARNING\\]\\)\\s-\\(/[^\\:\n]+\\)[^[:digit:]]+\\([[:digit:]]+\\),\\([[:digit:]]+\\)\\]\\(.*\\)$" 2 3 4 1 nil (1 compilation-warning-face) (5 compilation-warning-face)))
        (scalastyle-warning . ("^warning\\s-file=\\([^[:space:]]+\\).*line=\\([[:digit:]]+\\)\\s-column=\\([[:digit:]]+\\)" 1 2 3 1))
        (mvn-pom-warning . ("\\(\\[WARNING\\]\\).*,\s\\(/[^,]+\\),\sline\s\\([[:digit:]]+\\),\scolumn\s\\([[:digit:]]+\\)" 2 3 4 1 nil (1 compilation-warning-face)))
        (jvm-exception . ("^\\s-\\(at\\s-[^(]+\\)\(\\([^\\:\n]+\\)\\:\\([[:digit:]]+\\)\)" 2 3 nil 2 nil (1 compilation-error-face) (2 compilation-info-face)))))

(setq compilation-error-regexp-alist '(mvn-scala-warn mvn-scala-err mvn-java-err mvn-java-warning jvm-exception scalastyle-warning mvn-pom-warning))

;;; Shut up compile saves
(setq compilation-ask-about-save nil)
;;; Don't save *anything*
(setq compilation-save-buffers-predicate '(lambda () nil))

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (helm-projectile alchemist editorconfig helm-config magithub mvn emacs-websocket multi-term MultiTerm magit-gitflow magit-flow flymd markdown multiple-cursors solarized-theme use-package undo-tree smartparens projectile popup-imenu magit highlight-symbol helm goto-chg ensime emmet-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
