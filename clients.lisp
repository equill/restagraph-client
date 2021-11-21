(in-package :cl-user)


;; Client functions that call on library functions

;;; Create and tag users of a site, from a CSV file

(defun process-line (server line)
  "For a single line, make the necessary updates to Webcat."
  (declare (type rg-server server))
  (let ((sitename (first line))
        (username (second line))
        (tags (cl-ppcre:split ":" (third line))))
    (format t "Ensuring we have site ~A, user ~A exists on it, and we have tags ~{~A~^, ~} associated with that user."
            sitename username tags)
    ;; Site
    (log:info "Ensuring website '~A' exists." sitename)
    (ensure-resource-exists server "Websites" sitename)
    ;; Account
    (log:info "Ensuring user '~A' exists on site '~A'" username sitename)
    (ensure-resource-exists server
                            (format nil "/Websites/~A/ACCOUNTS/Accounts" sitename)
                            username)
    (log:info "Linking the account back to the site.")
    (ensure-link-exists server
                        (format nil "/Websites/~A/ACCOUNTS/Accounts/~A" sitename username)
                        "BELONGS_TO"
                        (format nil "/Websites/~A" sitename))
    ;; Tags
    (mapcar #'(lambda (tag)
                (log:debug "Ensuring tag '~A' exists." tag)
                (ensure-resource-exists server "Tags" tag)
                (log:debug "Tagging user '~A' with '~A' username tag" username tag)
                (ensure-link-exists server
                                    (format nil "/Websites/~A/ACCOUNTS/Accounts/~A" sitename username)
                                    "TAGS"
                                    (format nil "/Tags/~A" tag)))
            tags)))

(defparameter *webcat*
  (make-instance 'rg-server :port 4965))

(defun create-and-tag-users (server filepath)
  "Create and tag all accounts in a CSV file.
  File format:
  - site
  - user ID
  - tags (colon-separated list)
  Ensures presence of the site, username and tags, and connects accounts back to their site."
  (mapcar #'(lambda (line) (process-line server line))
          (ingest-csv filepath)))
