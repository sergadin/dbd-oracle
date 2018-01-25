(in-package :dbd.oracle)

(defmacro with-reusable-query ((var connection sql) &body body)
  (let ((query-var (gensym "QUERY-")))
    `(let ((,var (dbi:prepare ,connection ,sql)))
       (setf (slot-value (query-prepared ,var) 'reusablep) t)
       (unwind-protect
            (progn ,@body)
         (dbd.oracle::release-resources ,var)))))
