(in-package :webgigs.core)

(interpol:enable-interpol-syntax)

(defvar *interesting-categories* '("cpg" "web" "sof" "eng" "med"))

(defun get-gig-links-for-all-categories ()
  (loop :for community :in *interesting-categories*
        :nconc (get-gig-links-for-category community)))

(defun get-gig-links-for-category (category)
  (let* ((page (chtml:parse (drakma:http-request (format nil #?"http://vancouver.en.craigslist.ca/${category}/"))
                           (cxml-stp:make-builder)))
         (links (stp-query:query "span.pl a" page)))
    (sort (mapcar (lambda (link)
                    (parse-link ($:html link)))
                  links)
          #'>
          :key (lambda (link) (alexandria:assoc-value link :id)))))

(defun parse-link (link)
  (ppcre:register-groups-bind (city category id title)
      ("/([^/]+)/([^/]+)/(\\d+)\\.html\".*?>([^<]+)" link)
    `((:id . ,(parse-integer id))
      (:category . ,category)
      (:city . ,city)
      (:title . ,title))))


