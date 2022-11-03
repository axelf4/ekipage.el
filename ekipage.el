;;; ekipage.el --- Package manager  -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))

(defcustom ekipage-base-dir
  (expand-file-name "straight" user-emacs-directory)
  "Directory for ekipage data."
  :type 'string)

(defun ekipage-melpa-retrieve (package)
  "Look up the MELPA recipe for PACKAGE."
  (let ((melpa-repo (expand-file-name "repos/melpa/recipes" ekipage-base-dir)))
    (with-temp-buffer
      (condition-case nil
          (insert-file-contents-literally
           (expand-file-name (symbol-name package) melpa-repo))
        (file-missing nil)
        (:success
         (cl-destructuring-bind (name . recipe) (read (current-buffer))
           (when-let ((files (plist-get recipe :files)))
             ;; Ensure *-pkg.el is included (may be implicit since MELPA then creates it)
             (plist-put recipe :files (append files (list (format "%s-pkg.el" package)))))
           ;; TODO Normalize MELPA style recipe, e.g. default to git fetcher
           (cons name recipe)))))))

(defun ekipage-gnu-elpa-retrieve (package)
  (let ((elpa-repo (expand-file-name "repos/gnu-elpa-mirror" ekipage-base-dir)))
    (when (file-exists-p (expand-file-name (symbol-name package) elpa-repo))
      (list package :fetcher 'github :repo (format "emacs-straight/%s" package)
            :files '("*" (:exclude ".git"))))))

;; TODO Inline?
(defun ekipage--normalize-recipe (melpa-style-recipe)
  (cl-destructuring-bind (package . recipe)
      (if (listp melpa-style-recipe)
          melpa-style-recipe
        (or (ekipage-melpa-retrieve melpa-style-recipe)
            (ekipage-gnu-elpa-retrieve melpa-style-recipe)))
    (plist-put recipe :package (symbol-name package))
    recipe))

(defun ekipage--dependencies (package)
  "Return the dependencies of the PACKAGE."
  (let* ((build-dir (expand-file-name package
                                     (expand-file-name "build" ekipage-base-dir)))
         (default-directory build-dir)
         (case-fold-search t))
    (with-temp-buffer
      (condition-case nil
          ;; Either from <PACKAGE>-pkg.el metadata
          (insert-file-contents-literally (concat package "-pkg.el"))
        (:success (eval (nth 4 (read (current-buffer)))))
        (file-missing
         ;; Or the <PACKAGE>.el preamble
         (and (ignore-error file-missing
                (insert-file-contents-literally (concat package ".el")))
              (re-search-forward
               "[[:space:]]*;+[[:space:]]*Package-Requires:[[:space:]]*" nil t)
              (looking-at "(")
              (read (current-buffer))))))))

(defconst ekipage-default-files
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
   ((null files) (setq files ekipage-default-files))
   ((eq (car files) :defaults)
    (setq files (append ekipage-default-files (cdr files)))))
  (let ((default-directory src-dir)
        (stack (list (cons "" files)))
        exclude result)
        (while stack
          (pcase (car stack)
            (`(,dir ,(and (pred stringp) srcpath) . ,_)
             (let ((xs (file-expand-wildcards srcpath)))
               (if exclude
                   (cl-remove-if (lambda (x) (memq x xs)) result)
                 (let ((new (mapcar
                             (lambda (x) (cons (file-name-nondirectory x) dir)) xs)))
                   (setq result (nconc new result)))))
             (pop (cdar stack)))
            (`(,_) (setq exclude nil) (pop stack))
            ;; Nested SUBDIR/:exclude inside of :exclude: Flatten
            ((and (guard exclude) `(,_ (,_ . ,srcpaths) . ,rest))
             (setf (cdar stack) (append srcpaths rest)))
            (`(,dir (,(and (pred stringp) subdir) . ,srcpaths) . ,rest)
             (setq stack `((,(concat dir subdir "/") . ,srcpaths)
                           (,dir . ,rest) . ,(cdr stack))))
            (`(,dir (:exclude . ,srcpaths) . ,rest)
             (setq exclude t
                   stack `((,dir . ,srcpaths) (,dir . ,rest) . ,(cdr stack))))
            (_ (signal 'bad-files-directive files))))
        result))

(defun ekipage--activate-package (recipe)
  "Activate the package built by RECIPE."
  (let* ((package (plist-get recipe :package))
         (build-dir (expand-file-name package
                                      (expand-file-name "build" ekipage-base-dir)))
         (autoloads-file (expand-file-name (format "%s-autoloads.el" package) build-dir)))
    (message "Activating: %S" recipe)
    (cl-pushnew build-dir load-path)
    ;; TODO Cache autoloads
    (load autoloads-file nil t)))

(defconst ekipage-ignored-dependencies
  '(emacs cl-lib cl-generic nadvice seq)
  "Packages to ignore.")

;;;###autoload
(defun ekipage-use-package (melpa-style-recipe)
  "Do thing.
RECIPE is a MELPA style recipe"
  (cond
   ((memq ; TODO
     (if (listp melpa-style-recipe) (car melpa-style-recipe) melpa-style-recipe)
     ekipage-ignored-dependencies)
    nil)
   (t (let* ((recipe (ekipage--normalize-recipe melpa-style-recipe))
             (package (plist-get recipe :package)))
        (unless recipe
          (error "Recipe for package %s not found." melpa-style-recipe))

        (dolist (dep (ekipage--dependencies package))
          (message "Using dependency: %S" dep)
          (ekipage-use-package (if (consp dep) (car dep) dep)))

        ;; TODO Clone if not available
        (ekipage--activate-package recipe)
        ))))

;; (let* ((recipe (ekipage--normalize-recipe (ekipage-melpa-retrieve 'magit)))
;;        (package (plist-get recipe :package))
;;        (cps (ekipage-calc-symlinks
;;              ;; (append '(:defaults ("hej" "test/*.el")))
;;              (plist-get recipe :files)
;;              (expand-file-name "repos/magit" ekipage-base-dir)))
;;        (deps (ekipage--dependencies package)))
;;   (message "dependencies: %s" deps)
;;   cps)
