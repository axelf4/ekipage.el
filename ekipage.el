;;; ekipage.el --- Emacs Lisp package manager  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(declare-function cl-delete-if "cl-seq")

(defgroup ekipage nil "An Emacs Lisp package manager." :group 'applications)

(defcustom ekipage-base-dir (expand-file-name "ekipage" user-emacs-directory)
  "Absolute path to the ekipage data directory."
  :type 'string)

(cl-defun ekipage--dependencies
    (package build-dir &aux (default-directory build-dir) (case-fold-search t))
  "Return the dependencies of PACKAGE."
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
             "^;+[[:space:]]*Package-Requires[[:space:]]*:[[:space:]]*\\(.*\\(?:\n;+\\(?:\t\\|[ \t]\\{2,\\}\\).+\\)*\\)" nil t)
            (read (replace-regexp-in-string
                   "\n;+[ \t]+" " " (match-string-no-properties 1) t t)))))))

(defconst ekipage--default-files
  '("*.el" "lisp/*.el"
    "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude
     ".dir-locals.el" "lisp/.dir-locals.el"
     "test.el" "tests.el" "*-test.el" "*-tests.el"
     "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
  "Default value for the `:files' recipe directive.")

(defun ekipage--expand-files (files src-dir)
  "Determine the files used to build the package from its `:files' directive.
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
                 (if exclude (cl-delete-if (lambda (x) (member x xs)) result :key #'car)
                   (nconc (mapcar (lambda (x) (cons x dir)) xs) result))))
         (pop (cdar stack)))
        (`(,_) (setq exclude nil) (pop stack))
        ;; Flatten nested SUBDIR/:exclude inside of :exclude
        ((and (guard exclude) `(,_ (,_ . ,srcpaths) . ,rest))
         (setf (cdar stack) (append srcpaths rest)))
        (`(,dir (,(and (pred stringp) subdir) . ,srcpaths) . ,rest)
         (setq stack `((,(concat dir subdir "/") . ,srcpaths)
                       (,dir . ,rest) . ,(cdr stack))))
        (`(,dir (:exclude . ,srcpaths) . ,rest)
         (setq exclude t
               stack `((,dir . ,srcpaths) (,dir . ,rest) . ,(cdr stack))))
        (_ (error "Invalid :files directive `%S'" files))))
    result))

(defun ekipage--built-in-retrive (package)
  "Return a dummy recipe for built-in PACKAGE, otherwise nil."
  (defvar package--builtins)
  (require 'finder-inf)
  (when (assq package package--builtins)
    (list package :fetcher 'built-in)))

(defun ekipage--melpa-retrieve (package)
  "Look up the MELPA recipe for PACKAGE."
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents-literally
         (file-name-concat
          (ekipage-use-package '(melpa :fetcher github :repo "melpa/melpa") :no-build t)
          "recipes" (symbol-name package)))
      (file-missing nil)
      (:success
       (cl-destructuring-bind (name . recipe) (read (current-buffer))
         (when-let ((tail (cdr (plist-member recipe :files))))
           ;; Ensure *-pkg.el is included (may be implicit as MELPA always creates it)
           (setcar tail (nconc (car tail) (list (format "%s-pkg.el" name)))))
         (cons name recipe))))))

(defun ekipage--gnu-elpa-retrieve (package)
  "Look up the GNU Elpa recipe for PACKAGE."
  (when (file-exists-p
         (file-name-concat
          (ekipage-use-package
           '(gnu-elpa-mirror :fetcher github :repo "emacs-straight/gnu-elpa-mirror") :no-build t)
          (symbol-name package)))
    (list package :fetcher 'github :repo (format "emacs-straight/%s" package)
          :files '("*" (:exclude ".git")))))

(defun ekipage--normalize-recipe (melpa-style-recipe)
  (cl-destructuring-bind (package &rest recipe &key fetcher local-repo repo url &allow-other-keys)
      (if (consp melpa-style-recipe)
          (copy-sequence melpa-style-recipe)
        (or (ekipage--built-in-retrive melpa-style-recipe)
            (ekipage--melpa-retrieve melpa-style-recipe)
            (ekipage--gnu-elpa-retrieve melpa-style-recipe)
            (error "Recipe for package `%s' not found" melpa-style-recipe)))
    (setq recipe (plist-put recipe :package (symbol-name package)))
    (when-let
        (((null local-repo))
         (repo (pcase fetcher
                 ((or 'nil 'github 'gitlab)
                  (substring repo (1+ (string-match-p "/" repo))))
                 ('git (string-remove-suffix ".git" (file-name-nondirectory url)))
                 ('built-in)
                 (_ (symbol-name package)))))
      (setq recipe (plist-put recipe :local-repo repo)))
    recipe))

(defcustom ekipage-byte-compilation-buffer "*ekipage-byte-compilation*"
  "Name of the byte-compilation log buffer, or nil to discard output."
  :type '(choice (string :tag "Buffer name")
                 (const :tag "Discard output" nil)))

(cl-defun ekipage--recipe-git-url ((&key package fetcher repo url &allow-other-keys))
  (pcase fetcher
    ('git url)
    ('github (format "https://github.com/%s.git" repo))
    ('gitlab (format "https://gitlab.com/%s.git" repo))
    (_ (error "Unknown fetcher %S for package %s" fetcher package))))

(cl-defun ekipage--clone-package
    ((&whole recipe &key package local-repo branch &allow-other-keys) repo-dir)
  (message "Cloning %s into %s..." package local-repo)
  (make-directory repo-dir t)
  (let ((url (ekipage--recipe-git-url recipe)))
    (apply #'call-process "git" nil ekipage-byte-compilation-buffer nil
           ;; Use "origin" regardless of clone.defaultRemoteName from user config
           "clone" "--origin" "origin" "--depth" "1" "--no-single-branch"
           `(,@(when branch (list "--branch" branch))
             "--" ,url ,repo-dir))))

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
            (ignore-error error (Info-split))
            (call-process "install-info" nil ekipage-byte-compilation-buffer nil
                          info-file (file-name-concat build-dir "dir")))
        (:success (setq has-info t))
        ((error file-missing)
         (message "Error during texi2info on %s: %S" texi-file err)
         (set-buffer-modified-p nil))))))

(defcustom ekipage-check-for-modifications '(find-at-startup)
  "How to check for modifications."
  :type '(set (const :tag "Use find(1) at startup" find-at-startup)
              (const :tag "Fee" check-on-save))
  :initialize #'custom-initialize-changed
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (memq 'check-on-save value)
             (add-hook 'before-save-hook #'ekipage--register-modification)
           (remove-hook 'before-save-hook #'ekipage--register-modification))))

(defvar ekipage--cache
  (let ((cache-file (file-name-concat ekipage-base-dir "cache")))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents cache-file)
          (let ((cache-emacs-version (read (current-buffer)))
                (cache (read (current-buffer))))
            (if (and (equal cache-emacs-version emacs-version)
                     (hash-table-p cache))
                cache
              (delete-file cache-file)
              (make-hash-table))))
      ((file-missing end-of-file) (make-hash-table))))
  "Map of up-to-date packages to their build information.
The values are (RECIPE ORIGINAL DEPENDENCIES TIMESTAMP . AUTOLOADS)
tuples. For clone-only packages AUTOLOADS is nil.")

(defun ekipage--modified-packages ()
  "Find packages in `ekipage--cache' with modified repositories using find(1)."
  (let* ((repos (make-hash-table :test 'equal))
         (tests
          (cl-loop
           for k being the hash-keys of ekipage--cache using (hash-values v) nconc
           (cl-destructuring-bind
               ((&key local-repo &allow-other-keys) _original _deps timestamp &rest) v
             (when local-repo
               (puthash local-repo (cons k (gethash local-repo repos)) repos)
               (list "-o" "-path" (concat local-repo "*")
                     "-newermt" (format-time-string "%F %T" timestamp)
                     "-printf" "%H\n" "-prune")))))
         (args (nconc (hash-table-keys repos)
                      (list "-name" ".git" "-prune") tests))
         (default-directory (file-name-concat ekipage-base-dir "repos"))
         result)
    (mapc (lambda (repo) (when-let ((packages (gethash repo repos)))
                           (setq result (nconc packages result))
                           (remhash repo repos))) ; Remove to skip duplicates
          (when tests (apply #'process-lines "find" args)))
    result))

(defun ekipage--invalidate-dependents ()
  "Invalidate packages in CACHE that transitively depend on stale packages."
  (while (let (changed) ; Fixpoint calculation
           (maphash
            (cl-function
             (lambda (package (_recipe _original deps . rest))
               (unless (cl-loop
                        for dep in deps for dep-pkg = (if (consp dep) (car dep) dep)
                        always (gethash dep-pkg ekipage--cache))
                 (remhash package ekipage--cache) (setq changed t))))
            ekipage--cache)
           changed)))

(when-let* ; Invalidate stale packages in cache
    ((modified-dir (file-name-concat ekipage-base-dir "modified"))
     (modified
      (if (memq 'find-at-startup ekipage-check-for-modifications)
          (ekipage--modified-packages)
        ;; Invalidate any packages recorded in "modified" directory
        (mapcan
         (lambda (modified-repo)
           (cl-loop
            for k being the hash-keys of ekipage--cache using (hash-values v)
            if (string= (plist-get (car v) :local-repo) modified-repo) collect k))
         (when (file-exists-p modified-dir)
           (directory-files
            modified-dir nil directory-files-no-dot-files-regexp t))))))
  ;; TODO Check if recipes from modified recipe repository changed
  (dolist (package modified) (remhash package ekipage--cache))
  (ekipage--invalidate-dependents))

(defun ekipage--write-cache ()
  (with-temp-file (file-name-concat ekipage-base-dir "cache")
    (let (print-length print-level)
      (print emacs-version (current-buffer))
      (print ekipage--cache (current-buffer))))
  (delete-directory (file-name-concat ekipage-base-dir "modified") t))

(defun ekipage--register-modification ()
  "If the current buffer visits a file in a managed repository, mark it modified."
  (when-let* (buffer-file-name
              (repos-dir (file-name-as-directory
                          (file-name-concat ekipage-base-dir "repos")))
              ((string-prefix-p repos-dir buffer-file-name
                                (file-name-case-insensitive-p repos-dir)))
              (slash-pos (string-match-p "/" buffer-file-name (length repos-dir)))
              (repo (substring buffer-file-name (length repos-dir) slash-pos)))
    (make-empty-file (file-name-concat ekipage-base-dir "modified" repo) t)))

(defvar ekipage--activated-packages (make-hash-table :test 'eq)
  "The set of packages that are loaded in this Emacs session.")

(defun ekipage--activate-package (name build-dir autoloads)
  "Activate the package built by RECIPE."
  (unless (gethash name ekipage--activated-packages)
    (puthash name t ekipage--activated-packages)
    (push build-dir load-path)
    ;; TODO Add build-dir to Info-directory-list once it is initialized
    (eval autoloads))) ; Cache autoloads to avoid loading file

;;;###autoload
(cl-defun ekipage-use-package
    (melpa-style-recipe
     &key no-build
     &aux (name (if (consp melpa-style-recipe) (car melpa-style-recipe) melpa-style-recipe)))
  "Make the package specified by MELPA-STYLE-RECIPE available in this session.
Returns the directory containing the built package."
  (interactive "SPackage to use: ")
  (pcase (gethash name ekipage--cache)
    ;; If manually specified recipe differs from cache: Rebuild
    (`(,recipe ,(pred (not (equal melpa-style-recipe))) . ,_)
     ;; Invalidate other packages built from repository in old recipe
     ;; regardless of whether repository details changed.
     (let ((old-repo (plist-get recipe :local-repo)))
       (maphash (cl-function
                 (lambda (k ((&key local-repo &allow-other-keys) . rest))
                   (when (string= local-repo old-repo) (remhash k ekipage--cache))))
                ekipage--cache)))
    ;; If already built: Just activate
    ((and `(,recipe ,_original ,deps ,_timestamp . ,autoloads)
          (let repo (plist-get recipe :local-repo))
          (or (guard (null repo))
              (let (and repo-dir (pred #'file-exists-p))
                (file-name-concat ekipage-base-dir "repos" repo)))
          (or (guard (null autoloads)) ; clone-only package
              (let (and build-dir (pred #'file-exists-p))
                (file-name-concat ekipage-base-dir "build"
                                  (plist-get recipe :package)))))
     (pcase-dolist ((or `(,dep ,_) dep) deps) (ekipage-use-package dep))
     (when build-dir (ekipage--activate-package name build-dir autoloads))
     (cl-return-from ekipage-use-package (or build-dir repo-dir))))
  ;; About to rebuild: Invalidate packages that may later use this package
  (when (gethash name ekipage--cache)
    (when (gethash name ekipage--activated-packages)
      (error "Rebuild of already loaded package `%s' required. \
Impossible as it is to unload a package you will have to restart Emacs before proceeding." name))
    (remhash name ekipage--cache) (ekipage--invalidate-dependents))

  (let* ((recipe (ekipage--normalize-recipe melpa-style-recipe))
         (package (plist-get recipe :package))
         (repo (plist-get recipe :local-repo))
         (repo-dir (when repo (file-name-concat ekipage-base-dir "repos" repo)))
         (build-dir (file-name-concat ekipage-base-dir "build" package))
         deps autoloads)
    (if repo
        (unless (file-exists-p repo-dir) (ekipage--clone-package recipe repo-dir))
      (setq no-build t))

    (unless no-build
      (delete-directory build-dir t)
      (make-directory build-dir t)
      ;; Symlink files used to build the package
      (cl-loop
       with default-directory = build-dir for (srcfile . dst-dir)
       in (ekipage--expand-files (plist-get recipe :files) repo-dir) do
       (make-symbolic-link
        (file-name-concat repo-dir srcfile)
        (if (string-empty-p dst-dir) "./" (make-directory dst-dir t) dst-dir) t))
      ;; Ensure dependencies are built
      (setq deps (ekipage--dependencies package build-dir))
      (pcase-dolist ((or `(,dep ,_) dep) deps) (ekipage-use-package dep))
      ;; Build the package
      (message "Compiling %s..." package)
      (setq autoloads (ekipage--build-package package build-dir))
      (ekipage--make-info build-dir)

      (ekipage--activate-package name build-dir autoloads))

    (puthash
     name `(,recipe ,melpa-style-recipe ,deps ,(time-add nil 1) . ,autoloads)
     ekipage--cache)
    (if after-init-time (ekipage--write-cache)
      (add-hook 'after-init-hook #'ekipage--write-cache))
    (if no-build repo-dir build-dir)))

(provide 'ekipage)
;;; ekipage.el ends here
