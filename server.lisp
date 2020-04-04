;;;; server.lisp

(in-package #:cl-web-server)

(defvar *acceptor* nil)

(defun start-server (&optional (port 5000))
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                          :port port
                                          :document-root (uiop:parse-unix-namestring "./www/")))))
