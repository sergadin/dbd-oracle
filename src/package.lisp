(in-package :cl-user)

(defpackage dbd.oracle
  (:use :cl
        :dbi
        :dbi.driver
        :dbi.error)
  (:export #:*foreign-library-types*
           #:*foreign-library-search-paths*
           #:with-reusable-query)
  (:shadowing-import-from :dbi.driver
                          :disconnect))
