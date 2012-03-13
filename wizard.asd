(asdf:defsystem #:wizard
  :version      "0.0.1"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "metacyclic codegenerator"
  :depends-on   (#:cl-mysql
                 #:cl-fad
                 #:cl-store
                 #:restas
                 #:restas-directory-publisher
                 #:cl-json
                 #:cl-ppcre
                 #:cl-smtp
                 #:cl-mime
                 #:arnesi
                 #:closer-mop
                 #:drakma)
  :serial        t
  :components   ((:file "lib")
                 (:file "ent")
                 (:file "genobj")
                 (:file "iface")
                 (:file "places")
                 (:file "gen")
                 (:file "obj")
                 (:file "grid")
                 (:file "searching")
                 (:file "grid-fltr")
                 (:file "render")
                 (:file "show-linear-elt")
                 (:file "perm")
                 (:file "defmodule")
                 (:file "suppmapp")
                 (:file "register")
                 (:file "init")
                 (:file "save-restore")
                 (:static-file "migration.lisp")
                 (:static-file "README")
                 (:static-file "wizard.asd")
                 (:module "tpl"
                          :serial t
                          ;; :pathname ""
                          :components ((:static-file "about.htm")
                                       (:static-file "contacts.htm")
                                       (:static-file "main.htm")
                                       (:static-file "root.htm")
                                       (:static-file "services.htm")
                                       (:static-file "templates.htm")))))
