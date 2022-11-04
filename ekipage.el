;;; ekipage.el --- Emacs Lisp package manager  -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))

(defgroup ekipage nil "An Emacs Lisp package manager." :group 'applications)

(defcustom ekipage-base-dir
  (expand-file-name "straight" user-emacs-directory)
  "Directory for ekipage data."
  :type 'string)

(defun ekipage--melpa-retrieve (package)
  "Look up the MELPA recipe for PACKAGE."
  (let ((melpa-repo (expand-file-name "repos/melpa/recipes" ekipage-base-dir)))
    (with-temp-buffer
      (condition-case nil
          (insert-file-contents-literally
           (expand-file-name (symbol-name package) melpa-repo))
        (file-missing nil)
        (:success
         (cl-destructuring-bind (name . recipe) (read (current-buffer))
           (when-let ((tail (cdr (plist-member recipe :files))))
             ;; Ensure *-pkg.el is included (may be implicit as MELPA always creates it)
             (setcar tail (append (car tail) (list (format "%s-pkg.el" name)))))
           ;; TODO Normalize MELPA style recipe, e.g. default to git fetcher
           (cons name recipe)))))))

(defun ekipage--gnu-elpa-retrieve (package)
  (let ((elpa-repo (expand-file-name "repos/gnu-elpa-mirror" ekipage-base-dir)))
    (when (file-exists-p (expand-file-name (symbol-name package) elpa-repo))
      (list package :fetcher 'github :repo (format "emacs-straight/%s" package)
            :files '("*" (:exclude ".git"))))))

;; TODO Remove and require user recipes to include :package key
(defun ekipage--normalize-recipe (melpa-style-recipe)
  (cl-destructuring-bind (package . recipe)
      (if (listp melpa-style-recipe)
          melpa-style-recipe
        (or (ekipage--melpa-retrieve melpa-style-recipe)
            (ekipage--gnu-elpa-retrieve melpa-style-recipe)
            (error "Recipe for package %s not found" melpa-style-recipe)))
    (setq recipe (plist-put recipe :package (symbol-name package)))
    recipe))

(defun ekipage--dependencies (package build-dir)
  "Return the dependencies of PACKAGE."
  (let ((default-directory build-dir)
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

(defun ekipage--calc-symlinks (files src-dir)
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
               (setq result
                     (if exclude (cl-delete-if (lambda (x) (memq x xs)) result)
                       (nconc (mapcar (lambda (x) (cons x dir)) xs) result))))
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

;; TODO Use repos var to unify mono-repo repository in use
(let ((repos (make-hash-table)))
  (cl-defun ekipage--repo-dir ((&key fetcher repo url &allow-other-keys))
    "Return the repository directory for the package built by RECIPE."
    (expand-file-name
       (pcase fetcher
         ((or 'nil 'github 'gitlab)
          (substring repo (1+ (string-match-p "/" repo))))
         ('git (string-remove-suffix ".git" (file-name-nondirectory url))))
     (expand-file-name "repos" ekipage-base-dir))))

(defun ekipage--clone-package (_recipe)
  (error "TODO"))

(defun ekipage--build-package (package build-dir)
  "Byte-compiles PACKAGE located in BUILD-DIR.
Returns the generated autoloads such that they can be loaded with `eval'."
  (let* ((default-directory build-dir) ;; TODO Remove?
         (emacs (concat invocation-directory invocation-name))
         (program (format "(let ((default-directory %S))
  (normal-top-level-add-subdirs-to-load-path)
  (byte-recompile-directory %S 0 'force))"
                          (concat build-dir "/..") build-dir))
         (args (list "-Q" "--batch" "--eval" program))

         (autoloads-file (concat package "-autoloads.el")))
    ;; TODO Capture output
    (apply #'call-process emacs nil nil nil args)
    ;; Generate autoloads
    (make-directory-autoloads build-dir autoloads-file)

    ;; Read generated autoloads
    ;; TODO Find output files overriden with generated-autoload-file
    (with-temp-buffer
      (insert-file-contents-literally autoloads-file)
      ;; Normalize the $# reader macro
      (let ((load-file-name autoloads-file) result)
        (ignore-error end-of-file
          (while t (push (read (current-buffer)) result)))
        ;; Fake the autoloads forms being load:ed as usual when eval:ing
        `(let ((load-file-name ,autoloads-file) (load-in-progress t))
           ,@(nreverse result))))))

;; TODO Inline?
(defun ekipage--activate-package (recipe &optional autoloads)
  "Activate the package built by RECIPE."
  (let* ((package (plist-get recipe :package))
         (build-dir (expand-file-name package
                                      (expand-file-name "build2" ekipage-base-dir)))
         (autoloads-file (expand-file-name (concat package "-autoloads.el") build-dir)))
    (message "Activating: %S" recipe)
    (cl-pushnew build-dir load-path)
    (if autoloads
        (eval autoloads) ;; Cache autoloads to avoid loading file
      (load autoloads-file nil t))))

(defvar ekipage--package-cache
  ;; TODO Allow checking if modified using find(1)
  (let ((cache-file (expand-file-name "cache" ekipage-base-dir)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents cache-file)
          ;; TODO Validate ekipage and Emacs version, etc.
          (let ((cache (read (current-buffer))))
            (unless (hash-table-p cache) (signal 'malformed cache))
            cache)
          ;; TODO Remove any packages found in modified directory
          )
      (malformed (delete-file cache-file) (make-hash-table))
      (file-missing (make-hash-table)))) ; Proceed with default value
  "Map of symbols of up-to-date packages to (RECIPE DEPENDENCIES . AUTOLOADS) tuples.")

(defun ekipage--write-cache ()
  (with-temp-file (expand-file-name "cache" ekipage-base-dir)
    (let (print-length print-level)
      (print ekipage--package-cache (current-buffer)))))

(add-hook 'after-init-hook #'ekipage--write-cache)

(defconst ekipage--ignored-dependencies '(emacs cl-lib cl-generic nadvice seq)
  "Packages to ignore.")

;;;###autoload
(cl-defun ekipage-use-package
    (melpa-style-recipe
     &aux (name (if (listp melpa-style-recipe) (car melpa-style-recipe) melpa-style-recipe)))
  "Do thing.
RECIPE is a MELPA style recipe"
  (cond
   ((and (symbolp melpa-style-recipe) (memq name ekipage--ignored-dependencies))
    nil)
   ;; If in cache and not modified: Just activate
   ((pcase (gethash name ekipage--package-cache)
      ;; TODO Check that recipe is the same
      (`(,recipe ,deps . ,autoloads)
       (dolist (dep deps) (ekipage-use-package (if (consp dep) (car dep) dep)))
       (ekipage--activate-package recipe autoloads)
       t)))
   (t (let* ((recipe (ekipage--normalize-recipe melpa-style-recipe))
             (package (plist-get recipe :package))
             (repo-dir (ekipage--repo-dir recipe))
             (build-dir (expand-file-name
                         package (expand-file-name "build2" ekipage-base-dir)))
             deps autoloads)
        (unless (file-exists-p repo-dir) (ekipage--clone-package recipe))

        (delete-directory build-dir t)
        (make-directory build-dir t)
        (cl-loop with default-directory = build-dir
                 for (srcfile . dst-dir)
                 in (ekipage--calc-symlinks (plist-get recipe :files) repo-dir) do
                 (make-symbolic-link
                  (expand-file-name srcfile repo-dir)
                  (if (string= dst-dir "") "./" (make-directory dst-dir t) dst-dir) t))

        ;; TODO Have to recurse into deps after files copied to build directory and before actually building
        (setq deps (ekipage--dependencies package build-dir))
        (dolist (dep deps)
          (message "Using dependency: %S" dep)
          (ekipage-use-package (if (consp dep) (car dep) dep)))

        ;; Build package
        (message "Compiling %s..." package)
        (setq autoloads (ekipage--build-package package build-dir))

        (ekipage--activate-package recipe autoloads)

        (puthash name `(,recipe ,deps . ,autoloads) ekipage--package-cache)
        (when after-init-time (ekipage--write-cache))))))

(let* ((recipe (ekipage--normalize-recipe (ekipage-melpa-retrieve 'magit)))
       (package (plist-get recipe :package))
       (cps (ekipage--calc-symlinks
             ;; (append '(:defaults ("hej" "test/*.el")))
             (plist-get recipe :files)
             (expand-file-name "repos/magit" ekipage-base-dir)))
       (build-dir (expand-file-name
                   package (expand-file-name "build" ekipage-base-dir)))
       (deps (ekipage--dependencies package build-dir)))
  (message "dependencies: %s" deps)
  (message "repo: %s" (ekipage--repo-dir recipe))
  cps)
