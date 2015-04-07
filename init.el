;; bugs
;;  emacsclientw instances don't get closed.  is C-x # the only way?  is it the right way?
;;  maybe prevent runemacs from running if the two servers are already started
;; pressing "X" on top right won't close emacs the correct way when server is loaded.  override hook?  disable this button if possible?

;; 
; TODO check all leader keys are defined, download mark complete
;; workgroups2
;; ws butler
;; clean anindent
;; helm swoop
;; parens packages

;; checkout semantic-refactor -- seems really useful but early stages
;;  https://github.com/tuhdo/semantic-refactor/blob/master/srefactor-demos/demos.org

;; switch to trunk, dev
;; open current file in perforce - doesn't seem possible
;; enable camel case for Evil??  maybe
;; check out ctags
;; get familiar with shell and console
;; grep files in different locations?  like a project?
;; jump to component?  maybe by making bookmarks when running a script?
;; rotate windows
;; leverage a lot of interesting code for the compile command wiki
;;   http://emacswiki.org/emacs/CompileCommand

;; http://stackoverflow.com/a/9661665 (gtags-find-file)

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
(add-to-list 'load-path (expand-file-name "~/.emacs.d/defuns"))

;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(setq frame-title-format "%b") ;; show buffer as the frame title

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq molokai-theme-kit t)
(load-theme 'molokai t)

(require 'utility)

(require 'nicompilation) ;; compilation setup for build services
(global-set-key (kbd "<f5>") 'my-compile)

;; Note: To get this to work, I needed to create a client spec and set
;;  Environment varialbe P4CONFIG=[path to config]
;;  *IMPORTANT* Also byte compile p4.el for performance improvment
;; ===P4Config file===
;; P4CLIENT=YOURP4CLIENTNAME
;; P4USER=yourP4UserName
;; P4PORT=perforce:1666
(require 'p4)

(require 'init-misc) ;; misc
(require 'init-gui-frames) ;; supress dialog and startup, disable toolbar and scrollbar, opacity
(require 'init-recentf) ;; performance optimizations for recentf
(require 'init-uniquify) ;; uniquify buffers with same name
(require 'init-fonts) ;; allow resize of fonts
(require 'init-modeline) ;; compact mode-line
(require 'init-linum-mode) ;; enable line numbering

(require 'init-cc-mode)

(require 'package-mode)

;; TODO move to file
(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(define-key global-map (kbd "RET") 'newline-and-indent)
(defalias 'yes-or-no-p 'y-or-n-p)


(setq auto-mode-alist
      (append '(("package$" . package-mode)
                ("\\.mak$" . makefile-mode)
                ) auto-mode-alist))

;;;_. =================================================
;;;_. Emacs Server (to allow emacsclient)
;;;_.  For the clients, use emacsclientw.exe [-f servername]
;;;_. =================================================
(require 'server)
(unless (server-running-p)
  (server-start))

;; Workaround to behave like a daemon on Windows
;;  http://emacs-fu.blogspot.com/2009/03/windows-and-daemons.html
;; (if (eq system-type 'windows-nt)
    (progn
      (defun hide-instead-of-kill ()
        "Hide window instead of killing so we can use it as a daemon"
        (interactive)                                                                                     
        (server-edit)
        (make-frame-invisible nil t))                                                                     
      (when (server-running-p server-name)
        (global-set-key (kbd "C-x C-c") 'hide-instead-of-kill)))
;; )


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

    ;;;; Enable emacs bindings when in insert mode
    ;; remove all keybindings from insert-state keymap
    (setcdr evil-insert-state-map nil)
    ;; but [escape] should switch back to normal state
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

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
      "c/" 'replace-slash-toggle
      "dj" 'dired-jump ;; open the dired from current file
      "eb" 'eval-buffer
      "em" 'erase-message-buffer
      "fl" 'cp-filename-line-number-of-current-buffer
      "fn" 'cp-filename-of-current-buffer
      "fp" 'cp-fullpath-of-current-buffer
      "gf" 'helm-gtags-find-tag
      "gr" 'helm-gtags-find-rtag
      "gs" 'helm-gtags-find-symbol
      "gt" 'helm-gtags-dwim
      "gof" 'helm-gtags-find-tag-other-window
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
      "nf" 'move-buf-to-new-frame
      "nn" 'visit-next-file-with-base-name 
      "rw" 'rotate-windows
      "sc" 'shell-command
      "sl" 'sort-lines
      "sg" 'helm-google-suggest
      "so" 'sos
      "sp" 'documentation-std
      "sw" 'helm-wikipedia-suggest
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
  :config
  (window-numbering-mode t)
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
    (setq yas-snippet-dirs "~/.emacs.d/snippets/")
    (yas-reload-all)
))

(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol-at-point highlight-symbol-query-replace)
)

(use-package fic-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'fic-mode)
)

(use-package semantic
  :commands (semantic-mode helm-semantic-or-imenu)
  :defer 3
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

