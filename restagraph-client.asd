;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(asdf:defsystem
  #:restagraph-client
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "Library of client functions for interacting with a Restagraph server."
  :depends-on (#:drakma
               #:read-csv
               #:cl-ppcre
               #:log4cl)
  :components ((:file "package")
               (:file "librestagraph")
               (:file "clients")))
