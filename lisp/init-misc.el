(setq-default
  bookmark-default-file "~/.emacs.d/.bookmarks.el"
  buffers-menu-max-size 30
  case-fold-search t
  compilation-scroll-output t
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain
  grep-highlight-matches t
  grep-scroll-output t
  indent-tabs-mode nil
  line-spacing 0.2
  mouse-yank-at-point t
  set-mark-command-repeat-pop t
  tooltip-delay 1.5
  truncate-lines nil
  truncate-partial-width-windows nil
  ;; no annoying beep on errors
  visible-bell t
  ;; Highlight trailing whitespace
  show-trailing-whitespace t
  ;; Vim smooth scrolling
  scroll-margin 5
  scroll-conservatively 9999
  scroll-step 1
  )

;; Make underscore part of a word
(add-hook 'c++-mode-hook (lambda() (modify-syntax-entry ?_ "w" c++-mode-syntax-table)))
(add-hook 'c-mode-hook (lambda() (modify-syntax-entry ?_ "w" c-mode-syntax-table)))
(add-hook 'lisp-mode-hook (lambda() (modify-syntax-entry ?_ "w" lisp-mode-syntax-table)))
(add-hook 'makefile-mode-hook (lambda() (modify-syntax-entry ?_ "w" makefile-mode-syntax-table)))
(add-hook 'package-mode-hook (lambda() (modify-syntax-entry ?_ "w" package-mode-syntax-table)))

;; Remove buffer still has clients error due to using C-x k with client-server
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'init-misc)
