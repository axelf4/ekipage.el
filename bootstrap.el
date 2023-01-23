;;; bootstrap.el --- Initializes ekipage.el  -*- lexical-binding: t -*-

(require 'bytecomp)

(let* ((bootstrap.el (file-truename load-file-name))
       (ekipage.el (expand-file-name "ekipage.el"
                                     (file-name-directory bootstrap.el))))
  (byte-recompile-file ekipage.el nil 0)
  (load (file-name-sans-extension ekipage.el) nil t nil t))

(ekipage-use-package '(ekipage :fetcher github :repo "axelf4/ekipage") :no-build t)
