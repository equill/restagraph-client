(in-package :restagraph-client)

(defclass rg-server ()
  ((protocol :initarg :protocol
             :reader protocol
             :initform "http"
             :type string)
   (hostname :initarg :hostname
             :reader hostname
             :initform "localhost"
             :type string)
   (port :initarg :port
         :reader port
         :initform 4950
         :type integer)))
