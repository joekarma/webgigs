(in-package :webgigs.db)

;;; Utilities

(defmacro defdbfun2 (name (&rest args) &body fun)
  `(defun ,name ,args
     (pomo:with-connection '("craigslist" "joekarma" "" "localhost")
       ,@fun)))

(defmacro with-alist-accessors ((&rest accessors) alist &body body)
  (once-only (alist)
    `(let ,(mapcar (lambda (accessor) `(,accessor (assoc-value ,alist ,(intern (symbol-name accessor) :keyword)))) accessors)
       ,@body)))

;;; Code

(defdbfun2 define-schema ()
  (restart-case
      (pomo:execute (:create-table gigs
                                   ((id :type bigint :primary-key t)
                                    (category :type string)
                                    (title :type string)
                                    (city :type string)
                                    (favourite :type boolean)
                                    (notes :type (or string pomo:db-null))
                                    (fetch-time :type bigint))))
    (drop-table-and-retry ()
      :report "Delete the job-posts table then create it again."
      (pomo:execute (:drop-table :gigs))
      (invoke-restart (find-restart 'swank::retry)))))

(defdbfun mark-link-as-favourite (link-id)
  (pomo:execute (:update :gigs :set :favourite t :where (:= :id link-id))))

(defdbfun store-parsed-link (link)
  (with-alist-accessors (id category city title fetch-time favourite) link
    (handler-case
        (pomo:execute (:insert-into :gigs :set :id id :category category :city city :title title :favourite favourite
                                    :fetch-time (or fetch-time (get-universal-time))))
      (cl-postgres-error:unique-violation ()
        (pomo:execute (:update :gigs :set :category category :city city :title title :fetch-time (or fetch-time (get-universal-time))
                               :where (:= :id id)))))))

(defun store-gig-links-for-all-categories ()
  (mapcar #'store-parsed-link (webgigs.core:get-gig-links-for-all-categories)))

(defdbfun get-all-stored-links ()
  (pomo:query (:order-by (:select :* :from :gigs)
                         (:desc :id))
              :alists))
