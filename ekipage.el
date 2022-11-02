;;; ekipage.el --- Package manager  -*- lexical-binding: t -*-

(defcustom ekipage-base-dir
  (expand-file-name "straight" user-emacs-directory)
  "Directory for ekipage data."
  :type 'string)

(defun ekipage-melpa-retrieve (package)
  "Look up the MELPA recipe for PACKAGE."
  (let ((melpa-repo (expand-file-name "repos/melpa/recipes/" ekipage-base-dir)))
    (with-temp-buffer
      (condition-case nil
          (insert-file-contents-literally
           (expand-file-name (symbol-name package) melpa-repo))
        (file-missing nil)
        (:success
         (cl-destructuring-bind (name . recipe) (read (current-buffer))
           ;; TODO Ensure *-pkg.el is included in :files, may be implicit since MELPA always recreates it
           ;; TODO Normalize MELPA style recipe
           (cons name recipe)))))))

(defvar ekipage-default-files
  '("*.el" "lisp/*.el"
    "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude
     ".dir-locals.el" "lisp/.dir-locals.el"
     "test.el" "tests.el" "*-test.el" "*-tests.el"
     "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
  "Default value for the `:files' directive in recipes.")

(defun ekipage-calc-symlinks (files src-dir)
  "Determines the files used to build the package from its `:files' directive.
Returns a list of (SRCPATH . DST-SUBDIR) pairs where SRCPATH is a file
relative to SRC-DIR that should be copied the DST-SUBDIR subdirectory
of the destination build directory."
  (cond
   ((not files) (setq files ekipage-default-files))
   ((eq (car files) :defaults)
    (setq files (append ekipage-default-files (cdr files)))))
  (let ((default-directory src-dir)
        (stack `(("" t . ,files)))
        result)
    (while stack
      (pcase (car stack)
        (`(,_ ,_) (pop stack))
        (`(,dir t (,(and (pred stringp) subdir) . ,srcpaths) . ,rest)
         (setq stack `((,(concat dir subdir "/") t . ,srcpaths)
                       (,dir t . ,rest) . ,(cdr stack))))
        (`(,dir t (:exclude . ,srcpaths) . ,rest)
         (setq stack `((,dir nil . ,srcpaths) (,dir t . ,rest) . ,(cdr stack))))
        (`(,dir ,inc ,(and (pred stringp) srcpath) . ,_)
         (let ((xs (file-expand-wildcards srcpath)))
           (if inc
               (let ((new (mapcar
                           (lambda (x) (cons (file-name-nondirectory x) dir)) xs)))
                 (setq result (nconc new result)))
             (cl-remove-if (lambda (x) (memq x xs)) result)))
         (pop (cddar stack)))
        (_ (signal 'bad-files-directive files))))
    result))

(let* ((recipe (ekipage-melpa-retrieve 'hotfuzz))
       (cps (ekipage-calc-symlinks
             (append (plist-get recipe :files)
                     '(:defaults ("hej" "test/*.el")))
             (expand-file-name "repos/hotfuzz" ekipage-base-dir))))
  cps)

(defun ekipage-use-package (recipe)
  "Do thing.

RECIPE is a MELPA style recipe"
  )
