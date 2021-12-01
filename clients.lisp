(in-package :cl-user)


;; Client functions that call on library functions

;;; Create and tag users of a site, from a CSV file

(defun create-and-tag-user (server sitename username tags)
  "Ensure the presence of a website, a user profile on it and a list of tags, and associate them all."
  (declare (type restagraph-client:rg-server server)
           (type string sitename username)
           (type list tags))
  (format t "Ensuring we have site ~A, user ~A exists on it, and we have tags ~{~A~^, ~} associated with that user."
          sitename username tags)
  ;; Site
  (log:info "Ensuring website '~A' exists." sitename)
  (restagraph-client:ensure-resource-exists server "/Websites" sitename)
  ;; Account
  (log:info "Ensuring user '~A' exists on site '~A'" username sitename)
  (restagraph-client:ensure-resource-exists
    server
    (format nil "/Websites/~A/ACCOUNTS/Accounts" sitename)
    username)
  (log:info "Linking the account back to the site.")
  (restagraph-client:ensure-link-exists
    server
    (format nil "/Websites/~A/ACCOUNTS/Accounts/~A" sitename username)
    "BELONGS_TO"
    (format nil "/Websites/~A" sitename))
  ;; Tags
  (mapcar #'(lambda (tag)
              (log:debug "Ensuring tag '~A' exists." tag)
              (restagraph-client:ensure-resource-exists server "/Tags" tag)
              (log:debug "Tagging user '~A' with '~A' username tag" username tag)
              (restagraph-client:ensure-link-exists
                server
                (format nil "/Websites/~A/ACCOUNTS/Accounts/~A" sitename username)
                "TAGS"
                (format nil "/Tags/~A" tag)))
          tags))

(defun process-line (server line)
  "For a single line, make the necessary updates to Webcat."
  (declare (type restagraph-client:rg-server server))
  (let ((sitename (first line))
        (username (second line))
        (tags (cl-ppcre:split ":" (third line))))
    (format t "Processing site ~A, user ~A , and tags ~{~A~^, ~}."
            sitename username tags)
    (create-and-tag-user server sitename username tags)))

(defparameter *webcat*
  (make-instance 'restagraph-client:rg-server :hostname "192.0.2.1" :port 4965))

(defun create-and-tag-users-from-file (server filepath)
  "Create and tag all accounts in a CSV file.
  File format:
  - site
  - user ID
  - tags (colon-separated list)
  Ensures presence of the site, username and tags, and connects accounts back to their site."
  (mapcar #'(lambda (line) (process-line server line))
          (restagraph-client:ingest-csv filepath)))
