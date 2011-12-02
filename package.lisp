;;;; package.lisp

(defpackage #:fcgiclient
  (:export :do-request
           :do-some
           :start-request
           :get-result)
  (:use #:cl))

