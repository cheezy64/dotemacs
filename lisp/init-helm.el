;; TODO - remap find-tag to use ggtags
;;        check out helm-swoop

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; PACKAGE: helm
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-config)
(require 'helm-grep)

;; ========================================
;; Configuration 
;; ========================================
(setq
  helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
  helm-quick-update t ; do not display invisible candidates
  helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
  helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
  helm-candidate-number-limit 500 ; limit the number of displayed canidates
  helm-ff-file-name-history-use-recentf t
  helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
  helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non-nil
                                         ; useful in helm-mini that lists buffers
  )

;; adds man pages to default lookup of text at point
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; ========================================
;; Hooks
;; ========================================
;; use helm to list eshell history
(add-hook 'eshell-mode-hook
  #'(lambda ()
      (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; ========================================
;; Overriding key bindings
;; ========================================
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")   'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>") 'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")	'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")	'helm-grep-mode-jump-other-window-backward)

;; override some keys from helm-config
(define-key helm-command-map (kbd "s")    nil)
(define-key helm-command-map (kbd "s g") 'helm-google-suggest)
(define-key helm-command-map (kbd "s w") 'helm-wikipedia-suggest)

;; show help-command with Helm
(define-key 'help-command (kbd "C-f")	'helm-apropos)
(define-key 'help-command (kbd "r")	'helm-info-emacs)
(define-key 'help-command (kbd "C-l")	'helm-locate-library)

;; show minibuffer history with Helm
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

;; remap
(define-key global-map [remap find-tag]		'helm-etags-select)
(define-key global-map [remap list-buffers]	'helm-buffers-list)

;; ========================================
;; Custom key bindings
;; ========================================
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(bind-key (kbd "C-c h")		'helm-command-prefix)
(bind-key (kbd "C-c h o")	'helm-occur)
(bind-key (kbd "C-c h x")	'helm-register)
(bind-key (kbd "C-x b")		'helm-mini)
(bind-key (kbd "C-x C-f")	'helm-find-files)
(bind-key (kbd "M-x")		'helm-M-x)
(bind-key (kbd "M-y")		'helm-show-kill-ring)

(helm-mode t) ; Use helm everywhere!

(provide 'init-helm)
