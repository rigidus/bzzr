(in-package #:WIZARD)

(defmacro a-fld (name obj)
  `(if (equal val :clear)
       ""
       (funcall
        (intern
         (format nil "A-~A" ,name)
         (find-package "WIZARD"))
        ,obj)))

;; (print (macroexpand-1 '(a-fld namefld val)))

(defun show-fld-helper (captfld tplfunc namefld valuefld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (funcall tplfunc (list :name namefld
                                            :value valuefld)))))


(defun show-linear-elt (typedata val namefld captfld permfld)
  "intermediate dispatcher (-ext)"
  (show-linear-elt-ext (intern (format nil "~{~A~^.~}" typedata) :keyword) val namefld captfld permfld))


(defgeneric show-linear-elt-ext (typedata val namefld captfld permfld))

(defmethod show-linear-elt-ext (typedata val namefld captfld permfld)
  (error "no applicable method SHOW-LINEAR-ELT for ~A" (type-of typedata)))

(defmethod show-linear-elt-ext ((typedata (eql :bool)) val namefld captfld permfld)
  (show-fld-helper captfld #'tpl:flagupd namefld (a-fld namefld val)))

(defmethod show-linear-elt-ext ((typedata (eql :num)) val namefld captfld permfld)
  (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))




(defmethod show-linear-elt-ext ((typedata (eql :str)) val namefld captfld permfld)
  (if (check-perm (a-update permfld) (cur-user) (intern (format nil "LINEAR-FLD-~A-~A" typedata namefld) :keyword))
      ;; update
      (tpl:fld (list :fldname captfld
                     :fldcontent (tpl:strupd
                                  (list :name namefld
                                        :value (IF (EQUAL VAL :CLEAR)
                                                   ""
                                                   (slot-value val (intern namefld :wizard)))))))
      ;; else
      (if (check-perm (a-view permfld) (cur-user) (intern (format nil "LINEAR-FLD-~A-~A" typedata namefld) :keyword))
          ;; view
          (tpl:fld (list :fldname captfld
                         :fldcontent (tpl:strview
                                      (list :name namefld
                                            :value (IF (EQUAL VAL :CLEAR)
                                                       ""
                                                       (slot-value val (intern namefld :wizard)))))))
          ;; else - none
          (if (and (boundp '*dbg*) *dbg*)
              (format nil "<br/>~%Permisson denied for fld [~A] <br/>~%"
                      namefld)
              ""))))


(defmethod show-linear-elt-ext ((typedata (eql :txt)) val namefld captfld permfld)
  (if (check-perm (a-update permfld) (cur-user) (intern (format nil "LINEAR-FLD-~A-~A" typedata namefld) :keyword))
      ;; update
      (tpl:fld (list :fldname captfld
                     :fldcontent (tpl:txtupd
                                  (list :name namefld
                                        :value (IF (EQUAL VAL :CLEAR)
                                                   ""
                                                   (slot-value val (intern namefld :wizard)))))))
      ;; else
      (if (check-perm (a-view permfld) (cur-user) (intern (format nil "LINEAR-FLD-~A-~A" typedata namefld) :keyword))
          ;; view
          (tpl:fld (list :fldname captfld
                         :fldcontent (tpl:txtview
                                      (list :name namefld
                                            :value (IF (EQUAL VAL :CLEAR)
                                                       ""
                                                       (slot-value val (intern namefld :wizard)))))))
          ;; else - none
          (if (and (boundp '*dbg*) *dbg*)
              (format nil "<br/>~%Permisson denied for fld [~A] <br/>~%"
                      namefld)
              ""))))



(defmethod show-linear-elt-ext ((typedata (eql :pswd)) val namefld captfld permfld)
  (show-fld-helper captfld #'tpl:pswdupd namefld (a-fld namefld val)))

(defmethod show-linear-elt-ext ((typedata (eql :interval)) val namefld captfld permfld)
  (let ((val (a-fld namefld val)))
    (if (equal 'INTERVAL (type-of val))
        (tpl:fld
         (list :fldname captfld
               :fldcontent (tpl:intervalupd (list :name namefld
                                                  :valuebegin (decode-date (interval-begin val))
                                                  :valueend   (decode-date (interval-end val))))))
        (tpl:fld
         (list :fldname captfld
               :fldcontent (tpl:intervalupd (list :name namefld
                                                  :valuebegin ""
                                                  :valueend   "")))))))

(defmethod show-linear-elt-ext ((typedata (eql :date)) val namefld captfld permfld)
  (let ((val (a-fld namefld val)))
    (if (or (null val)
            (equal "" val))
        (tpl:fld
         (list :fldname captfld
               :fldcontent (tpl:dateupd (list :name namefld
                                              :value ""))))
        (tpl:fld
         (list :fldname captfld
               :fldcontent (tpl:dateupd (list :name namefld
                                              :value (decode-date val))))))))


(defmethod show-linear-elt-ext ((typedata (eql :list-of-keys.supplier-status)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (getf *supplier-status* (a-fld namefld val)))))))

(defmethod show-linear-elt-ext ((typedata (eql :list-of-keys.offer-status)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (getf *offer-status* (a-fld namefld val)))))))

(defmethod show-linear-elt-ext ((typedata (eql :list-of-keys.resource-types)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (getf *resource-types* (a-fld namefld val)))))))

(defmethod show-linear-elt-ext ((typedata (eql :list-of-keys.tender-status)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (getf *tender-status* (a-fld namefld val)))))))

(defmethod show-linear-elt-ext ((typedata (eql :link.builder)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))

(defmethod show-linear-elt-ext ((typedata (eql :link.category)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))

(defmethod show-linear-elt-ext ((typedata (eql :link.supplier)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (let ((it (a-fld namefld val)))
                                                 (if (null it) "" (a-name it))))))))

(defmethod show-linear-elt-ext ((typedata (eql :link.tender)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))

(defmethod show-linear-elt-ext ((typedata (eql :link.tender-resource)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :value (format nil "~A | (a-name (a-resource (a-fld namefld val)))"

                                                       (IF (EQUAL VAL :CLEAR)
                                                           ""
                                                           ;; (a-tender-resource val)
                                                           "here: show-linear-elt if nil - debug it!"
                                                           ;; (FUNCALL (INTERN (FORMAT NIL "A-~A" NAMEFLD) (FIND-PACKAGE "WIZARD"))
                                                           ;;          VAL)
                                                           )

                                                       ))))))


(defmethod show-linear-elt-ext ((typedata (eql :link.supplier-resource-price)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :name namefld
                                        :value (a-name (a-fld namefld val)))))))

(defmethod show-linear-elt-ext ((typedata (eql :link.resource)) val namefld captfld permfld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (tpl:strview (list :name namefld
                                        :value (a-name (a-fld namefld val)))))))
