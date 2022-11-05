;;; ekipage.el --- Emacs Lisp package manager  -*- lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))

(defgroup ekipage nil "An Emacs Lisp package manager." :group 'applications)

(defcustom ekipage-base-dir (expand-file-name "ekipage" user-emacs-directory)
  "Absolute path to the ekipage data directory."
  :type 'string)

(defun ekipage--melpa-retrieve (package)
  "Look up the MELPA recipe for PACKAGE."
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents-literally
         (file-name-concat ekipage-base-dir "repos" "melpa" "recipes"
                           (symbol-name package)))
      (file-missing nil)
      (:success
       (cl-destructuring-bind (name . recipe) (read (current-buffer))
         (when-let ((tail (cdr (plist-member recipe :files))))
           ;; Ensure *-pkg.el is included (may be implicit as MELPA always creates it)
           (setcar tail (append (car tail) (list (format "%s-pkg.el" name)))))
         ;; TODO Normalize MELPA style recipe, e.g. default to git fetcher
         (cons name recipe))))))

(defun ekipage--gnu-elpa-retrieve (package)
  (when (file-exists-p
         (file-name-concat
          ekipage-base-dir "repos" "gnu-elpa-mirror" (symbol-name package)))
    (list package :fetcher 'github :repo (format "emacs-straight/%s" package)
          :files '("*" (:exclude ".git")))))

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

(defconst ekipage--default-files
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
   ((null files) (setq files ekipage--default-files))
   ((eq (car files) :defaults)
    (setq files (append ekipage--default-files (cdr files)))))
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
(let ((_repos (make-hash-table :test #'equal)))
  (cl-defun ekipage--repo-dir ((&key fetcher repo url &allow-other-keys))
    "Return the repository directory for the package built by RECIPE."
    (file-name-concat
     ekipage-base-dir "repos"
     (pcase fetcher
       ((or 'nil 'github 'gitlab)
        (substring repo (1+ (string-match-p "/" repo))))
       ('git (string-remove-suffix ".git" (file-name-nondirectory url)))
       (_ (error "Unknown :fetcher %s in recipe" fetcher))))))

(defun ekipage--clone-package (_recipe)
  (error "TODO"))

(defcustom ekipage-byte-compilation-buffer "*ekipage-byte-compilation*"
  "Name of the byte-compilation log buffer, or nil to discard output."
  :type '(choice (string :tag "Buffer name")
                 (const :tag "Discard output" nil)))

(defun ekipage--build-package (package build-dir)
  "Byte-compile PACKAGE inside of BUILD-DIR.
Returns the generated autoloads loadable via `eval'."
  (let* ((default-directory build-dir)
         (emacs (concat invocation-directory invocation-name))
         (expr (format "(let ((default-directory %S))
  (normal-top-level-add-subdirs-to-load-path)
  (byte-recompile-directory %S 0 t t))"
                       (file-name-directory build-dir) build-dir))
         (autoloads-file (concat package "-autoloads.el")))
    ;; Byte-compile in subprocess to have a clean environment
    (call-process emacs nil ekipage-byte-compilation-buffer nil
                  "-Q" "--batch" "--eval" expr)
    ;; Generate autoloads
    (make-directory-autoloads build-dir autoloads-file)
    ;; Read generated autoloads
    ;; TODO Find output files overridden with generated-autoload-file
    (with-temp-buffer
      (insert-file-contents-literally autoloads-file)
      ;; Normalize the $# reader macro
      (let ((load-file-name autoloads-file) result)
        (ignore-error end-of-file
          (while t (push (read (current-buffer)) result)))
        ;; Fake the autoloads forms being load:ed as usual when eval:ing
        `(let ((load-file-name ,autoloads-file) (load-in-progress t))
           ,@(nreverse result))))))

(declare-function texinfo-format-buffer-1 "texinfmt")
(cl-defun ekipage--make-info (build-dir &aux has-info enable-local-variables
                                        find-file-hook)
  "Compile Info files from Texinfo sources.
Returns whether any output files were produced."
  (dolist (texi-file (directory-files build-dir t "\\.texi\\(?:nfo\\)?$" t) has-info)
    (require 'texinfmt)
    (with-current-buffer (find-file-noselect texi-file)
      (condition-case err
          (cl-destructuring-bind (info-file . indices) (texinfo-format-buffer-1)
            (Info-tagify)
            (Info-split)
            (call-process "install-info" nil ekipage-byte-compilation-buffer nil
                          info-file (file-name-concat build-dir "dir")))
        (:success (setq has-info t))
        ((error file-missing)
         (message "Error during texi2info on %s: %S" texi-file err)
         (set-buffer-modified-p nil))))))

;; TODO Inline?
(defun ekipage--activate-package (recipe autoloads)
  "Activate the package built by RECIPE."
  (let* ((package (plist-get recipe :package))
         (build-dir (file-name-concat ekipage-base-dir "build" package)))
    (cl-pushnew build-dir load-path :test #'string=) ; TODO Skip later activations to use push
    (eval autoloads))) ;; Cache autoloads to avoid loading file

(defun ekipage--modified-packages (pkg-cache)
  "Find packages in PKG-CACHE with modified repositories using find(1)."
  (let* ((repos (make-hash-table :test #'equal))
         (tests
          (cl-loop
           for k being the hash-keys of pkg-cache using (hash-values v) nconc
           (pcase v
             (`(,recipe ,timestamp ,_deps . ,_autoloads)
              (let ((repo (ekipage--repo-dir recipe)))
                (puthash repo k repos)
                (list "-o" "-path" (concat repo "/*")
                      "-newermt" (format-time-string "%F %T" timestamp)
                      "-printf" "%H\n" "-prune"))))))
         (args (nconc (cl-loop for k being the hash-keys of repos collect k)
                      (list "-name" ".git" "-prune") tests))
         (default-directory (file-name-concat ekipage-base-dir "repos"))
         result)
    (mapc (lambda (repo) (when-let ((package (gethash repo repos)))
                           (push package result)
                           (remhash repo repos))) ; Remove to skip duplicates
          (when tests (apply #'process-lines "find" args)))
    result))

(defcustom ekipage-check-for-modifications '(check-on-save)
  "How to check for modifications.")

(defvar ekipage--cache
  (let ((cache-file (file-name-concat ekipage-base-dir "cache")))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents cache-file)
          (let ((cache-emacs-version (read (current-buffer)))
                (cache (read (current-buffer)))
                (modified-dir (file-name-concat ekipage-base-dir "modified")))
            (unless (and (equal cache-emacs-version emacs-version)
                         (hash-table-p cache))
              (signal 'malformed cache))
            (if (memq 'find-at-startup ekipage-check-for-modifications)
                (dolist (package (ekipage--modified-packages cache))
                  (remhash package cache))
              ;; Remove any packages recorded in "modified" directory
              (dolist (modified-pkg
                       (when (file-exists-p modified-dir)
                         (directory-files modified-dir nil
                                          directory-files-no-dot-files-regexp t)))
                (cl-loop
                 for k being the hash-keys of cache using (hash-values v) until
                 (cl-destructuring-bind ((&key package &allow-other-keys) . rest) v
                   ;; TODO Fetch package from repository name
                   (when (string= package modified-pkg) (remhash k cache) t)))))
            cache))
      ((malformed end-of-file) (delete-file cache-file) (make-hash-table))
      (file-missing (make-hash-table)))) ; Proceed with default value
  "Map of up-to-date packages to their build information.
The values are (RECIPE TIMESTAMP DEPENDENCIES . AUTOLOADS) tuples, or
just (RECIPE) if the package does not get built.")

(defun ekipage--write-cache ()
  (with-temp-file (file-name-concat ekipage-base-dir "cache")
    (let (print-length print-level)
      (print emacs-version (current-buffer))
      (print ekipage--cache (current-buffer))))
  (delete-directory (file-name-concat ekipage-base-dir "modified") t))

(define-minor-mode ekipage-check-modifications-mode
  "Mode that records modifications to package repositories managed by ekipage.el."
  :global t
  (if ekipage-check-modifications-mode
      (add-hook 'before-save-hook #'ekipage--register-modification)
    (remove-hook 'before-save-hook #'ekipage--register-modification)))
(ekipage-check-modifications-mode)

(defun ekipage--register-modification ()
  "If the current buffer visits a file in managed repository, mark it modified."
  (when-let* (buffer-file-name
              (repos-dir (file-name-as-directory
                          (file-name-concat ekipage-base-dir "repos")))
              ((string-prefix-p repos-dir buffer-file-name
                                (file-name-case-insensitive-p repos-dir)))
              (slash-pos (string-match-p "/" buffer-file-name (length repos-dir)))
              (repo (substring buffer-file-name (length repos-dir) slash-pos)))
    (make-empty-file (file-name-concat ekipage-base-dir "modified" repo) t)))

(defconst ekipage--ignored-dependencies '(emacs cl-lib cl-generic nadvice seq)
  "Packages to ignore.")

(defvar ekipage--packages-in-use (make-hash-table)
  "The set of packages that are loaded in this Emacs session.")

;;;###autoload
(cl-defun ekipage-use-package
    (melpa-style-recipe &key no-build
     &aux (name (if (listp melpa-style-recipe) (car melpa-style-recipe) melpa-style-recipe)))
  "Do thing.
RECIPE is a MELPA style recipe."
  (and (symbolp melpa-style-recipe) (memq name ekipage--ignored-dependencies)
       (cl-return-from ekipage-use-package))
  ;; If already built: Just activate
  (pcase (gethash name ekipage--cache)
    ;; TODO Check that recipe is the same
    (`(,_recipe) (cl-return-from ekipage-use-package)) ; no-build package
    (`(,recipe ,_timestamp ,deps . ,autoloads)
     (dolist (dep deps) (ekipage-use-package (if (consp dep) (car dep) dep)))
     (ekipage--activate-package recipe autoloads)
     (cl-return-from ekipage-use-package)))

  (let* ((recipe (ekipage--normalize-recipe melpa-style-recipe))
         (package (plist-get recipe :package))
         (repo-dir (ekipage--repo-dir recipe))
         (build-dir (file-name-concat ekipage-base-dir "build" package))
         deps autoloads)
    (unless (file-exists-p repo-dir) (ekipage--clone-package recipe))

    (if no-build
        (puthash name (list recipe) ekipage--cache)
      (delete-directory build-dir t)
      (make-directory build-dir t)
      ;; Symlink files used to build the package
      (cl-loop
       with default-directory = build-dir
       for (srcfile . dst-dir)
       in (ekipage--calc-symlinks (plist-get recipe :files) repo-dir) do
       (make-symbolic-link
        (file-name-concat repo-dir srcfile)
        (if (string= dst-dir "") "./" (make-directory dst-dir t) dst-dir) t))
      ;; Ensure dependencies are built
      (setq deps (ekipage--dependencies package build-dir))
      (dolist (dep deps) (ekipage-use-package (if (consp dep) (car dep) dep)))
      ;; Build the package
      (message "Compiling %s..." package)
      (setq autoloads (ekipage--build-package package build-dir))
      (ekipage--make-info build-dir)

      (ekipage--activate-package recipe autoloads)

      (puthash name `(,recipe ,(time-add nil 1) ,deps . ,autoloads) ekipage--cache))

    (if after-init-time (ekipage--write-cache)
      (add-hook 'after-init-hook #'ekipage--write-cache))))

(provide 'ekipage)
;;; ekipage.el ends here
