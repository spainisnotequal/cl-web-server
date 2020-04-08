;;;; server.lisp

(in-package #:cl-web-server)


;;; ----------------
;;; Start the server
;;; ----------------

(defvar *acceptor* nil)

(defun start-server (&optional (port 5000))
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  (setf *acceptor*
        (hunchentoot:start (make-instance 'routes-acceptor ; "routes-acceptor" from the "easy-routes" library
                                          :port port))))

(start-server)


;;; -----------------------
;;; Connect to the database
;;; -----------------------

;; define connection parameters

(defvar *database-url* nil) ; If the database is in Heroku, use: (defvar *database-url* (heroku-getenv "DATABASE_URL"))
(defvar *local-db-params* (list "mydb" "lispuser" "lisppassword" "localhost"))

(defun db-params ()
  "Heroku database url format is postgres://username:password@host/database_name. If we are testing on localhost, use the db-parameters from *local-db-params*."
  (if *database-url*
      (let* ((url (second (cl-ppcre:split "//" *database-url*)))
	     (user (first (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
	     (password (second (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
	     (host (first (cl-ppcre:split "/" (second (cl-ppcre:split "@" url)))))
	     (database (second (cl-ppcre:split "/" (second (cl-ppcre:split "@" url))))))
	(list database user password host))
      *local-db-params*))

;; test the connection
(with-connection (db-params)
  (query (:select (:+ 1 2)) :single))


;;; -----------------------------------------------------------------
;;; Macro to define the model (the dao-class and the CRUD operations)
;;; -----------------------------------------------------------------

;; "symb" funtion copied from: https://github.com/mck-/heroku-cl-example/blob/master/src/util/general-utils.lisp
;; (he adapted it from "On Lisp", page 58)
;; I need to put it inside an "eval-when" expression in order to use it later in
;; this same file, as it's pointed in: https://stackoverflow.com/a/49922152/9466271
(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defun symb (a b)
    (intern (format nil "~a-~a" (symbol-name a) (symbol-name b)))))

;;  (copied from: https://kuomarc.wordpress.com/2012/05/13/12-steps-to-build-and-deploy-common-lisp-in-the-cloud-and-comparing-rails/)
(defmacro defmodel (name slot-definitions)
  `(progn
     ;; Define the dao-class and create the table
     (defclass ,name ()
       ((id :col-type serial :reader ,(symb name 'id))
	,@slot-definitions)		      
       (:metaclass dao-class)
       (:keys id))
     (with-connection (db-params)
       (unless (table-exists-p ',name)
	 (execute (dao-table-definition ',name))))
     
     ;; Create
     (defmacro ,(symb name 'create) (&rest args)
       `(with-connection (db-params)
	  (make-dao ',',name ,@args)))
     
     ;; Get-all
     (defun ,(symb name 'get-all) ()
       (with-connection (db-params)
	 (select-dao ',name)))

     ;; Get (by id)
     (defun ,(symb name 'get) (id)
       (with-connection (db-params)
	 (get-dao ',name id)))

     ;; Select
     (defmacro ,(symb name 'select) (sql-test &optional sort)
       `(with-connection (db-params)
	  (select-dao ',',name ,sql-test ,sort)))
     
     ;; Update
     (defun ,(symb name 'update) (,name)
       (with-connection (db-params)
	 (update-dao ,name)))
     
     ;; Delete
     (defun ,(symb name 'delete) (,name)
       (with-connection (db-params)
	 (delete-dao ,name)))))


;;; --------------------------------------------
;;; Create a table and make some CRUD operations
;;; --------------------------------------------

;; Define the model and create the table
(defmodel furniture
    ((name   :col-type string  :initarg :name   :accessor furniture-name)
     (colour :col-type string  :initarg :colour :accessor furniture-colour)
     (stock  :col-type integer :initarg :stock  :accessor furniture-stock)))

;; Create
(furniture-create :name "bed"   :colour "white" :stock 1)
(furniture-create :name "desk"  :colour "brown" :stock 3)
(furniture-create :name "lamp"  :colour "black" :stock 5)
(furniture-create :name "chair" :colour "grey"  :stock 7)

;; Read
(furniture-get-all)
(furniture-get 1)
(furniture-select (:= 'name "lamp")) ; returns a list with every dao whose name is "lamp"

;; Update
(let ((lamp (furniture-get 2)))
  (setf (furniture-stock lamp) 55)
  (furniture-update lamp))

(destructuring-bind (lamp &rest rest) (furniture-select (:= 'name "lamp"))
  (setf (furniture-stock lamp) 99)
  (furniture-update lamp))

;; Delete
(furniture-delete (furniture-get 24))


;;; ----------
;;; Add routes
;;; ----------

;; Decorator to set the "Content-Tyoe" as JSON data
(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

;; Define routes

(defroute get-all ("/api/furniture" :method :get
                                    :decorators (@json)) ()
  (json-response :status hunchentoot:+http-ok+
                 :data (furniture-get-all)
                                        ; :error "Not applicable here, but for demonstration..."
                 ))

(defroute get-furniture ("/api/furniture/:id" :method :get
                                              :decorators (@json)) ()
  (json-response :status hunchentoot:+http-ok+
                 :data (furniture-get id)
                                        ; :error "Not applicable here, but for demonstration..."
                 ))
