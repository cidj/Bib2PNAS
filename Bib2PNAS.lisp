(defclass article ()
  ((abstract
    :initarg :abstract
    :initform " "
    :accessor abstract)
   (author 
    :initarg :author
    :initform " "
    :accessor author)
   (publisher 
    :initarg :publisher
    :initform " "
    :accessor publisher)
   (doi
    :initarg :doi
    :initform " "    
    :accessor doi)
   (file
    :initarg :file
    :initform " "
    :accessor file)
   (isbn 
    :initarg :isbn
    :initform " "
    :accessor isbn)
   (issn 
    :initarg :issn
    :initform " "
    :accessor issn)
   (journal
    :initarg :journal
    :initform " "
    :accessor journal)
   (keywords 
    :initarg :keywords
    :initform " "
    :accessor keywords)
   (anumber
    :initarg :anumber 
    :initform " "
    :accessor anumber)
   (pages 
    :initarg :pages 
    :initform " "
    :accessor pages)
   (pmid 
    :initarg :pmid 
    :initform " "
    :accessor pmid)
   (title 
    :initarg :title
    :initform " "
    :accessor title)
   (volume 
    :initarg :volume 
    :initform " "
    :accessor volume)
   (url 
    :initarg :url 
    :initform " "
    :accessor url)
   (month 
    :initarg :month 
    :initform " "
    :accessor month)
   (year 
    :initarg :year 
    :initform " "
    :accessor year)))

(defun write-pnas (reference stream)
  (format stream "\\bibitem{")
  (format stream "~a" (citename (author reference) (year reference)))
  (format stream "} ")
  (format stream "~a" (nameform (author reference) 5))
  (format stream " (")
  (format stream "~a" (year reference))
  (format stream ") ")  
  (format stream "~a" (title reference))
  (format stream " {\\it ")
  (format stream "~a" (journal reference))
  (format stream "} ")
  (format stream "~a" (volume reference))
  (format stream ":")
  (format stream "~a" (pages reference))
  (format stream "."))

(defgeneric exportstyle (reference style stream))
(defmethod exportstyle ((reference article) style stream)
  (cond ((string= style "pnas") (write-pnas reference stream))
	(t (error "Not defined reference!"))))

(defun string-eql (x y)
  (let ((xl (length x))
	(yl (length y)))
    (if (>= xl yl)
	(string= x y :start1 0 :end1 yl)
	nil)))
(defun deal (line)
  (if (find "{" line :test #'string=)
      (let ((head (position "{" line :test #'string= :from-end nil))
	    (tail (position "}" line :test #'string= :from-end t)))
	(subseq line (1+ head)  tail))
      (let ((head (position "=" line :test #'string= :from-end nil)))
	(subseq line (1+ head)))))
(defun clear-title (title)
  (let* ((x (deal title))
	 (y (length x)))
    (subseq x 1 (1- y))))
(defun title-dot (title)
  (if (char/= #\. (elt title (1- (length title))))
      (concatenate 'string title ".")
      title))
(defun deal-title (line)
  (title-dot
   (clear-title line)))

(defun deal-pages (pages)
  (setf pages (deal pages))
  (remove #\- pages :count 1))

(defun deal-journal (journal)
  (setf journal (deal journal))
  (remove #\. journal))

(defun split-by-string (substr string)
    (loop for i = 0 then (+ j (length substr))
          as j = (search substr string :start2 i)
          collect (string-trim "." (string-trim " " (subseq string i j)))
          while j))

(defun abr-givenname (name)
	   (let ((a (search ". " name))
		 (b (search "-" name)))
	     (cond (a (concatenate 'string (subseq name 0 1) (subseq name (+ 2 a) (+ 3 a))))
		   (b (concatenate 'string (subseq name 0 1) "-" (subseq name (1+ b) (+ 2 b))))    
		   (t (subseq name 0 1)))))
(defun abr-name (name)
  (concatenate 'string (car (split-by-string "," name)) " " (abr-givenname (cadr (split-by-string "," name)))))
(defun conlist (list)
	   (with-output-to-string (out)
	    (loop for i in list
		do 
		 (if (string= i (car list)) 
		     (format out "~a" i)
		     (format out ", ~a" i)))))	
#|
(defun nameform (string)
  (conlist
   (loop for i in (split-by-string " and " string)
      collect (abr-name i))))
|#
(defun nameform (string n)
  (let* ((list (split-by-string " and " string)) 
	 (first	(abr-name (car list))))
	 (if (> (length list) n)
	     (concatenate 'string first ", et al.")
	     (conlist
	      (loop for i in list
		 collect (abr-name i))))))

(defun citename (authorname year)
  (let* ((list (split-by-string " and " authorname))
         (first (car (split-by-string "," (car list)))))
         (concatenate 'string first year)))
  
  
(defparameter *x* nil)
(with-open-file (stream "MyCollection.bib" :direction :input)
  (when stream
    (do ((line (read-line stream nil) (read-line stream nil)))
	((null line))
      (cond ((string-eql line "@article")(defparameter *a* (make-instance 'article)))
	    ((string-eql line "abstract")(setf (abstract *a*) (deal line)))
	    ((string-eql line "author")(setf (author *a*) (deal line)))
        ((string-eql line "publisher")(setf (publisher *a*) (deal line)))
	    ((string-eql line "doi")(setf (doi *a*) (deal line)))
	    ((string-eql line "file")(setf (file *a*) (deal line)))
	    ((string-eql line "isbn")(setf (isbn *a*) (deal line)))
	    ((string-eql line "issn")(setf (issn *a*) (deal line)))
	    ((string-eql line "journal")(setf (journal *a*) (deal-journal line)))
	    ((string-eql line "keywords")(setf (keywords *a*) (deal line)))
	    ((string-eql line "number")(setf (anumber *a*) (deal line)))
	    ((string-eql line "pages")(setf (pages *a*) (deal-pages line)))
	    ((string-eql line "pmid")(setf (pmid *a*) (deal line)))
	    ((string-eql line "title")(setf (title *a*) (deal-title line)))
	    ((string-eql line "volume")(setf (volume *a*) (deal line)))
	    ((string-eql line "url")(setf (url *a*) (deal line)))
	    ((string-eql line "month")(setf (month *a*) (deal line)))
	    ((string-eql line "year")(setf (year *a*) (deal line)))
	    ((string-eql line "}")(push *a* *x*))
	    (t (if (string-eql line "@")
		   (progn (format t "~a" line)
			  (error "Only articles are accepted!~%You need to delete other cites and run program once more or there are mistakes in the result."))
		   (format t "Not defined property: ~a" line)))))))
(defparameter *x* (nreverse *x*))

(with-open-file (stream "PNASstyle.txt" :direction :output :if-exists :append)
  (dolist (x *x*)
    (progn
      (exportstyle x "pnas" stream)
      (format stream "~%~%"))))
