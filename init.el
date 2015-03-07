;; TODO check all leader keys are defined, download mark complete
;; workgroups2
;; ws butler
;; clean anindent
;; helm swoop
;; parens packages

;;;_. =================================================
;;;_. Unicode Encoding
;;;_. =================================================
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;;;_. =================================================
;;;_. Package initialization
;;;_. =================================================
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq message-log-max 16384)

(require 'use-package)

;;;_. =================================================
;;;_. Initialization
;;;_. =================================================
(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq molokai-theme-kit t)
(load-theme 'molokai t)

(require 'utility)
(require 'init-misc) ;; misc
(require 'init-gui-frames) ;; supress dialog and startup, disable toolbar and scrollbar, opacity
(require 'init-recentf) ;; performance optimizations for recentf
(require 'init-uniquify) ;; uniquify buffers with same name
(require 'init-fonts) ;; allow resize of fonts
(require 'init-modeline) ;; compact mode-line
(require 'init-linum-mode) ;; enable line numbering

(require 'init-cc-mode)

;; TODO move to file
(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(define-key global-map (kbd "RET") 'newline-and-indent)
(defalias 'yes-or-no-p 'y-or-n-p)

;;;_. =================================================
;;;_. PACKAGES
;;;_. =================================================
(setq evil-symbol-word-search t)
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :config
  (progn
    (require 'init-evil)
    (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
    (evil-mode t)
    (add-hook 'c-mode-common-hook
      (function (lambda ()
                  (setq evil-shift-width 3))))
  )
)

(use-package evil-surround
  :ensure t
  :config
  (progn
    (global-evil-surround-mode t)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
  )
)

(use-package evil-matchit
  :ensure t
  :config
  (progn
    (global-evil-matchit-mode t)
  )
)

(use-package evil-nerd-commenter
  :ensure t
  :commands (evilnc-comment-or-uncomment-lines
             evilnc-copy-and-comment-lines
            )
  :config
  (progn
    (evilnc-default-hotkeys)
  )
)

(use-package evil-args
  :ensure t
  :config
  (progn
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)
  )
)

(use-package evil-search-highlight-persist
  :ensure t
  :config
  (progn
    (global-evil-search-highlight-persist t)
  )
)

(use-package evil-leader
  :ensure t
  :init
  (setq evil-leader/in-all-states t)
  (setq evil-leader/leader ",")
  :config
  (progn
    (evil-leader/set-key
      ","  'evilnc-comment-or-uncomment-lines
      "al" 'align-regexp
      "cb" 'evilcvn-change-symbol-in-whole-buffer
      "cd" 'evilcvn-change-symbol-in-defun
      "dj" 'dired-jump ;; open the dired from current file
      "eb" 'eval-buffer
      "em" 'erase-message-buffer
      "fl" 'cp-filename-line-number-of-current-buffer
      "fn" 'cp-filename-of-current-buffer
      "fp" 'cp-fullpath-of-current-buffer
      "gf" 'helm-gtags-find-tag
      "gr" 'helm-gtags-find-rtag
      "gt" 'helm-gtags-dwim
      "hb" 'helm-back-to-last-point
      "hr" 'helm-recentf
      "hs" 'helm-swoop
      "hd" 'describe-function
      "hv" 'describe-variable
      "hf" 'find-function
      "hh" 'highlight-symbol-at-point
      "hq" 'highlight-symbol-query-replace
      "ma" 'mc/mark-all-like-this-in-defun
      "md" 'mc/mark-all-like-this-dwim
      "ms" 'mc/mark-all-symbols-like-this-in-defun
      "mw" 'mc/mark-all-words-like-this-in-defun
      "mf" 'mark-defun
      "rw" 'rotate-windows
      "sc" 'shell-command
      "sl" 'sort-lines
      "so" 'sos
      "srr" 'sr-speedbar-refresh-toggle
      "srt" 'sr-speedbar-toggle
      "ss" 'evil-surround-edit
      "tm" 'get-term
      "vu" '(lambda () (interactive) (scroll-other-window '-))
      "vv" 'scroll-other-window
    )
    (global-evil-leader-mode t)
  )
)

(use-package sos
  :ensure t
  :commands (sos sos-answer)
  :config
  (progn
    (define-key sos-mode-map (kbd "C-c C-o") 'sos-answer)
  )
)

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (progn
    (require 'init-helm)
  )
)

(use-package helm-gtags
  :disabled nil
  :ensure t
  :config
  (require 'init-helm-gtags)
)

(use-package ws-butler
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'ws-butler-mode)
)

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
)

(use-package window-numbering
  :ensure t
  :commands switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
)


;;;_ , yasnippet
(use-package yasnippet
  :ensure t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (hook-into-modes #'(lambda () (yas-minor-mode 1))
                   '(haskell-mode-hook
                     coq-mode-hook
                     org-mode-hook
                     ruby-mode-hook
                     message-mode-hook
                     gud-mode-hook
		     c-mode-hook
		     c++-mode-hook
                     erc-mode-hook))
  :config
  (progn
    (yas-load-directory "~/.emacs.d/snippets/")

    (bind-key "C-i" 'yas-next-field-or-maybe-expand yas-keymap)

    (defun yas-new-snippet (&optional choose-instead-of-guess)
      (interactive "P")
      (let ((guessed-directories (yas-guess-snippet-directories)))
        (switch-to-buffer "*new snippet*")
        (erase-buffer)
        (kill-all-local-variables)
        (snippet-mode)
        (set (make-local-variable 'yas-guessed-modes)
             (mapcar #'(lambda (d)
                         (intern (yas-table-name (car d))))
                     guessed-directories))
        (unless (and choose-instead-of-guess
                     (not (y-or-n-p "Insert a snippet with useful headers? ")))
          (yas-expand-snippet "\
# -*- mode: snippet -*-
# name: $1
# --
$0"))))

    (bind-key "C-c y TAB" 'yas-expand)
    (bind-key "C-c y n" 'yas-new-snippet)
    (bind-key "C-c y f" 'yas-find-snippets)
    (bind-key "C-c y r" 'yas-reload-all)
    (bind-key "C-c y v" 'yas-visit-snippet-file)
    (yas-reload-all)
))

(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol-at-point highlight-symbol-query-replace)
)

(use-package semantic
  :commands (semantic-mode helm-semantic-or-imenu)
  :idle (semantic-mode)
  :config
  (progn
    (global-semanticdb-minor-mode 1)
    ;;(global-semantic-idle-scheduler-mode 1)
    (semantic-mode 1)
    (global-semantic-stickyfunc-mode)
  )
)

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'global-company-mode)
    (setq company-require-match nil)
    (if (fboundp 'evil-declare-change-repeat)
	(mapc #'evil-declare-change-repeat
	      '(company-complete-common
		company-select-next
		company-select-previous
		company-complete-selection
		company-complete-number
		)))
  )
  :config
  (progn
    (add-to-list 'company-backends 'company-cmake)
    ;; can't work with TRAMP
    (setq company-backends (delete 'company-ropemacs company-backends))
    (setq company-backends (delete 'company-capf company-backends))

    ;; Semantic and company is super slow.  It reparses everything
    (setq company-backends (delete 'company-semantic company-backends))

    ;; I don't like the downcase word in company-dabbrev
    ;; for languages use camel case naming convention
    (setq company-dabbrev-downcase nil)
    (setq company-show-numbers t)
    (setq company-begin-commands '(self-insert-command))
    (setq company-idle-delay 0.2)
    (setq company-clang-insert-arguments nil)
    ;;(setq company-auto-complete t)
  )
)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(ebrowse-root-class ((t (:foreground "dark orange" :weight bold))))
  '(evil-search-highlight-persist-highlight-face ((t (:background "dark olive green"))))
)

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
