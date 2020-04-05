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
(postmodern:with-connection (db-params)
  (postmodern:query (:select (:+ 1 2)) :single))

;; get all data from the "gold_price" table
(postmodern:with-connection (db-params)
  (postmodern:query (:select '* :from 'gold_price)))
