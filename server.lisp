;;;; server.lisp

(in-package #:cl-web-server)

;;; ----------------
;;; Start the server
;;; ----------------

(defvar *acceptor* nil)

(defun start-server (&optional (port 5000))
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                          :port port
                                          :document-root (uiop:parse-unix-namestring "./www/")))))

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

;; get all data from the "gold_price" table
(with-connection (db-params)
  (query (:select '* :from 'gold_price)))

;;; ------------------------------------
;;; Create a table and insert some items
;;; ------------------------------------

;; Create a (dao-)class, that represents the table we want to create
(defclass furniture ()
  ((id     :col-type serial                   :reader furniture-id)
   (name   :col-type string  :initarg :name   :accessor furniture-name)
   (colour :col-type string  :initarg :colour :accessor furniture-colour)
   (stock  :col-type integer :initarg :stock  :accessor furniture-stock))
  (:metaclass dao-class) 
  (:keys id))

;; Create the table
(with-connection
    (db-params)
           (execute (dao-table-definition 'furniture)))

;; Insert some items into the table
(with-connection (db-params)
  (make-dao 'furniture :name "desk" :colour "brown" :stock 3))
(with-connection (db-params)
  (make-dao 'furniture :name "lamp" :colour "black" :stock 1))
(with-connection (db-params)
  (make-dao 'furniture :name "chair" :colour "grey" :stock 2))

;;; ---------------------------------------------------
;;; Write the CRUD (Create, Read, Update, Delete) logic
;;; ---------------------------------------------------

;;  (copied from: https://kuomarc.wordpress.com/2012/05/13/12-steps-to-build-and-deploy-common-lisp-in-the-cloud-and-comparing-rails/)
(defmacro furniture-create (&rest args)
   `(with-connection (db-params)
      (make-dao 'furniture ,@args)))

(defun furniture-get-all ()
   (with-connection (db-params)
     (select-dao 'furniture)))

(defun furniture-get (id)
   (with-connection (db-params)
     (get-dao 'furniture id)))

(defmacro furniture-select (sql-test &optional sort)
   `(with-connection (db-params)
      (select-dao 'furniture ,sql-test ,sort)))

(defun furniture-update (furniture)
   (with-connection (db-params)
     (update-dao furniture)))

(defun furniture-delete (furniture)
   (with-connection (db-params)
     (delete-dao furniture)))

;; test some of those macros and functions
(furniture-create :name "bed" :colour "white" :stock 1)
(furniture-get-all)
(furniture-get 1)
(furniture-select (:= 'name "lamp")) ; returns a list with every dao whose name is "lamp"

(let ((lamp (furniture-get 2)))
  (setf (furniture-stock lamp) 55)
  (furniture-update lamp))

(destructuring-bind (lamp &rest rest) (furniture-select (:= 'name "lamp"))
  (setf (furniture-stock lamp) 99)
  (furniture-update lamp))

(furniture-delete (furniture-get 4))
