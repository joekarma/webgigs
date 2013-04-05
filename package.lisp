;;;; package.lisp

(restas:define-module #:webgigs
  (:use #:cl #:restas #:alexandria)
  (:export #:start-server
           #:with-yaclml-output-to-string))

(defpackage #:webgigs.db
  (:use #:cl #:alexandria)
  (:export #:define-schema
           #:mark-link-as-favourite
           #:store-parsed-link
           #:store-gig-links-for-all-categories
           #:get-all-stored-links))

(defpackage #:webgigs.core
  (:use #:cl #:alexandria)
  (:export #:get-gig-links-for-all-categories
           #:get-gig-links-for-category))
