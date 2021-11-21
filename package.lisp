;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(defpackage restagraph-client
  (:use
    #:cl)
  (:export
    rg-server
    ingest-csv
    ensure-resource-exists
    ensure-link-exists))
