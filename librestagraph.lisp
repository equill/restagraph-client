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

(defun ingest-csv (filepath)
  "Parse data from a CSV file."
  (with-open-file (infile filepath)
    (read-csv:parse-csv infile)))

(defun ensure-resource-exists (server resourcetype uid)
  "Ensure a resource of this type exists with this UID on that server.
   'Resourcetype' = path to the resource, so an account on Twitter would be:
   `/Websites/Twitter/ACCOUNT/Accounts`"
  (declare (type rg-server server)
           (type string resourcetype uid))
  (let ((url (format nil "~A://~A:~A/raw/v1~A/~A"
                     (protocol server)
                     (hostname server)
                     (port server)
                     resourcetype
                     uid)))
    (log:info "Attempting to query URL '~A'" url)
    (multiple-value-bind (body status-code)
      (drakma:http-request url)
      (declare (ignore body))
      (if (equal 200 status-code)
          ;; It's there, so we're good.
          (progn
            (log:info "Present; no action to take.")
            t)
          ;; It's not there. Create it.
          (let ((create-url (format nil "~A://~A:~A/raw/v1~A"
                                    (protocol server)
                                    (hostname server)
                                    (port server)
                                    resourcetype)))
            (log:info "Absent; attempting to create it via URL '~A', with UID '~A'" create-url uid)
            (multiple-value-bind (body status-code)
              (drakma:http-request create-url
                                   :method :POST
                                   :parameters `(("uid" . ,uid)))
              (if (equal 201 status-code)
                  t
                  (log:warn "Failed with message ~A" body))))))))

(defun ensure-link-exists (server source relationship target)
  "Ensure this relationship exists."
  (declare (type rg-server server)
           (type string source relationship target))
  (let* ((target-elements (cl-ppcre:split "/" target))
         (target-type (car (last target-elements 2)))
         (target-uid (car (last target-elements)))
         (check-url (format nil "~A://~A:~A/raw/v1/~A/~A/~A/~A"
                            (protocol server)
                            (hostname server)
                            (port server)
                            source
                            relationship
                            target-type
                            target-uid)))
    (log:info "Checking for presence of the relationship with URL '~A'" check-url)
    (multiple-value-bind (body status-code)
      (drakma:http-request check-url)
      (declare (ignore body))
      (if (equal 200 status-code)
          ;; It's there, so we're good.
          (progn
            (log:info "Present; no action to take.")
            t)
          ;; It's not there. Create it.
          (let ((create-url (format nil "~A://~A:~A/raw/v1~A/~A"
                                    (protocol server)
                                    (hostname server)
                                    (port server)
                                    source
                                    relationship)))
            (log:info "Absent; attempting to create it via URL '~A', with target '~A'" create-url target)
            (multiple-value-bind (body status-code)
              (drakma:http-request create-url
                                   :method :POST
                                   :parameters `(("target" . ,target)))
              (if (equal 201 status-code)
                  t
                  (log:warn "Failed with message ~A" body))))))))
