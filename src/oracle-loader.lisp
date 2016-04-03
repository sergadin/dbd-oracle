;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          oracle-loader.lisp
;;;; Purpose:       Foreign library loader for CLSQL Oracle interface
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:dbd.oracle)

(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (ext:getenv (string var))
  #+(or cmu scl)
  (cdr (assoc (string var) ext:*environment-list* :test #'equalp
              :key #'string))
  #+lispworks (lw:environment-variable (string var))
  #+ccl (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var))


(defparameter *oracle-home*
  (let ((oracle-home (getenv "ORACLE_HOME")))
    (when oracle-home
      (parse-namestring (concatenate 'string oracle-home "/"))))
  "Pathname of ORACLE_HOME as set in user environment.")

(defparameter *oracle-client-library-filenames*
  (list "libclntsh" "oci"))

(defvar *oracle-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the Oracle client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *oracle-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defvar *foreign-library-search-paths* nil
  "The list of pathnames where OCI library will be searched in
  addition to ORACLE_HOME locations.")

(defvar *foreign-library-types*
  (list #+(or windows) "dll")
  "List of libraries types used in addition to these defined by CFFI.")


(defun find-and-load-foreign-library (filenames &key module supporting-libraries (errorp t))
  "Attempt to load a foreign library. This will search for any of the filenames, as
well as any of the filenames in any of the *foreign-library-search-paths*"
  (setq filenames (if (listp filenames) filenames (list filenames)))

  (flet ((try-load (testpath)
           (handler-case
               (uffi:load-foreign-library testpath
                                          :module module
                                          :supporting-libraries supporting-libraries)
             (error #| (c) (warn "~A" c) nil)))) ;; |# nil)))) ;
    (or
     (loop for type in (append *foreign-library-types* (uffi:foreign-library-types))
           thereis
           (loop for name in filenames
                 for pn = (make-pathname :name name :type type)
                 thereis (or
                          (try-load pn)
                          (loop for search-path in *foreign-library-search-paths*
                                thereis (try-load (merge-pathnames pn search-path))))))
     (when errorp
       (error "Couldn't load foreign librar~@P ~{~S~^, ~}. (searched ~S: ~S)"
              (length filenames) filenames
              '*foreign-library-search-paths* *foreign-library-search-paths*)))))


(defun oracle-library-loaded ()
  *oracle-library-loaded*)

(defun oracle-load-foreign ()
  (when *oracle-home*
    (dolist (dir-name '("lib" "bin" ""))
      (pushnew (merge-pathnames *oracle-home* (make-pathname :name dir-name))
               *foreign-library-search-paths*
               :test #'equal)))
  (find-and-load-foreign-library *oracle-client-library-filenames*
                                 :module "dbd-oracle"
                                 :supporting-libraries *oracle-supporting-libraries*)
  (setq *oracle-library-loaded* t))
