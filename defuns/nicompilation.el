;; http://stackoverflow.com/a/14096693
;; (defun parent-directory (dir)
;;   (unless (equal "/" dir)
;;     (file-name-directory (directory-file-name dir))))

;; (defun find-file-in-heirarchy (current-dir fname)
;;   "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR" 
;;   (let ((file (concat current-dir fname))
;;         (parent (parent-directory (expand-file-name current-dir))))
;;     (if (file-exists-p file)
;;         file
;;       (when parent
;;         (find-file-in-heirarchy parent fname))))) ;; FIXME infinite recursion caused by parent-directory function

;; (defun find-dir-with-filename (current-dir fname)
;;   "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR and returns the directory containing FNAME" 
;; (let ((file (find-file-in-heirarchy current-dir fname)))
;;   (when file
;;     (file-name-directory file))
;;   )
;; )
(defun find-dir-with-filename (current-dir fname)
  (locate-dominating-file current-dir fname))

(defun mxbs-eval-in-dir (filename apply-it)
  "Given a function and its parameters in a list, applies that
     function to those parameters in the nearest directory with a
     build services package file"
  (let ((save-directory default-directory)
        (this-directory (find-dir-with-filename default-directory filename)))
 
    (when this-directory
      (setq default-directory this-directory)
      (apply (car apply-it) (cdr apply-it))
      (setq default-directory save-directory))))
 
(defun mxbs-eval-in-package-dir (apply-it)
  "Given a function and its parameters in a list, applies that
     function to those parameters in the nearest directory with a
     build services package file"
  (mxbs-eval-in-dir "package" apply-it))
 
(setq compile-command "setupEnv.bat & make")
(defun my-compile (command)
  "Start compilation in nearest parent directory with a 'package'
     file, or current directory if no package file found."
  (interactive
   (list
    (let ((default-command (eval compile-command)))
      (read-string "Compile command: " default-command
                   ;; if the command above is the first history command,
                   ;; skip it so you don't see it twice, but only if
                   ;; there is compile history
                   (if (boundp 'compile-history)
                       (if (equal (car compile-history) default-command)
                           '(compile-history . 1)
                         'compile-history)
                     ())))))
   
   ;; update the global "last compile command"
   (unless (equal command (eval compile-command))
     (setq compile-command command))
 
   ;; finally, call compile
   (mxbs-eval-in-package-dir (list 'compile command)))

(provide 'nicompilation)
