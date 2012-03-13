(in-package :wizard)

(define-condition condition-404-Not-Found (error)
  ((text
    :initarg :text
    :accessor text))
  (:report (lambda (condition stream)
             (format stream "404 Not Found :: [~a]"
                     (text condition)))))


(defmacro check-perm-for-cur-user-with-dbg (perm obj &body if-not-dbg)
  `(if (check-perm ,perm (cur-user) ,obj)
       (call-next-method)
       (if (and (boundp '*dbg*) *dbg*)
           (format nil "<br/>~%Permisson [~A]=[~A] denied for :~A [~A] <br/>~%"
                   (bprint (a-perm ,obj))
                   (bprint (perm-check (a-perm ,obj) (cur-user) ,obj))
                   (string-downcase (type-of ,obj))
                   (if (slot-exists-p obj 'value)
                       (a-value ,obj)
                       (a-title ,obj)))
           ,@if-not-dbg)))

;; activate
;; example-json
;; show-acts

(defclass wizard-render () ()) ;; default
(defclass action-render () ()) ;; actions : none, tpl, grid, linear, announce, yamap, file.
(defclass grid-render   () ()) ;; grids   : fld, btn.
(defclass linear-render () ()) ;; linears : fld, btn, popbtn, grid



(setf *default-render-method* (mi 'wizard-render))

;; RENDER DISPATCHER (ACTS)
(defmethod restas:render-object ((designer wizard-render) (acts list))
  (let* ((personal  (let ((userid (hunchentoot:session-value 'userid)))
                      (if (null userid)
                          (tpl:loginform)
                          (tpl:logoutform (list :user (a-name (gethash userid *USER*))
                                                :login (a-login (gethash userid *USER*))
                                                :usertype (string-downcase (type-of (gethash userid *USER*)))
                                                :userid userid))))))
    ;; Рендерить вложенные объекты надо до передачи в шаблон, если есть необходимость в побочных эффектах,
    ;; реализуемых через динамические переменные и влияющих на отображение других ключей шаблона (например *popups*)
    (let ((in-render (loop :for act :in acts :collect
                            (restas:render-object (mi 'action-render) act))))
      (tpl:root
       (list
        :mapkey         *mapkey*
        :personal       personal
        :right          (if (eql 1 (length (request-list))) ;; main page
                            (tpl:right)
                            "")
        :navpoints      (menu)
        :searchcategory (aif (hunchentoot:post-parameter "searchcategory") it "")
        :searchstring   (aif (hunchentoot:post-parameter "searchstring") it "")
        :content        (format nil "~{~A~}" in-render))))))


;; RENDER DISPATCHER :AROUND (ACTS) - error handling
(defmethod restas:render-object :around ((designer wizard-render) (acts list))
  (block 404-return-message
    (handler-bind ((condition-404-Not-Found
                    #'(lambda (c)
                        (invoke-restart 'restart-404-Not-Found (text c)))))
      (restart-case (progn
                      (hunchentoot:start-session) ;; before start-session
                      (call-next-method))
        (restart-404-Not-Found (message)
          (return-from 404-return-message
            (progn
              (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
              (setf (hunchentoot:return-code*) 404)
              ;; TODO: Научить hunch вместе с 404-ым кодом отдавать произвольное сообщение
              message
              )))))))


;; ACT: default (error)
(defmethod restas:render-object ((designer action-render) (obj action))
  (error "no applicable render method for subclass of ACTION: ~A" (type-of obj)))


;; ACT: none
(defmethod restas:render-object ((designer action-render) (obj none))
  (tpl:content-block
   (list :title (a-title obj)
         :content "Раздел находится в разработке")))



;; ACT: tpl
(defmethod restas:render-object ((designer action-render) (obj tpl))
  (tpl:content-block
   (list :title (a-title obj)
         :content (format nil "~A" (funcall (a-val obj))))))


;; ACT: grid --> grid-render (fld, btn)
(defmethod restas:render-object ((designer action-render) (obj grid))
  (let ((grid-id  (gensym "J"))
        (pager-id (gensym "P"))
        (col-titles)
        (col-models)
        (col-replace)) ;; <-- не используется, но возможно пригодится в будущем
    (loop :for infld :in (a-fields obj) :collect
       (multiple-value-bind (col-title col-model)
           (restas:render-object (mi 'grid-render) infld) ;; <-dispatcher
         (unless (null col-title)
           (push col-title col-titles)
           (push col-model col-models))))
    (tpl:content-block
     (list :title   (a-title obj)
           :content (tpl:gridview
                     (list :idgrid grid-id
                           :idpager pager-id
                           :json (replace-all
                                  (json:encode-json-to-string
                                   `(("url" . ,(format nil "/~A~A" (a-grid obj) (if (a-param-id obj) (format nil "/~A" (cur-page-id)) ""))) ;; absolute uri
                                     ("datatype" . "json")
                                     ("colNames" . ,(reverse col-titles))
                                     ("colModel" . ,(reverse col-models))
                                     ("rowNum" . 10)
                                     ("rowList" . (10 20 30))
                                     ("pager" . ,(format nil "#~A" pager-id))
                                     ("sortname" . "id")
                                     ("viewrecords" . t)
                                     ("sortorder" . "desc")
                                     ("height" . ,(aif (a-height obj) it "180"))
                                     ("editurl" . "/edit_url")
                                     ("gridComplete" . "-=|=-")
                                     ("caption" . ,(a-title obj))
                                     ))
                                  "\"-=|=-\"" ;; замена после кодирования в json - иначе никак не вставить js :)
                                  (format nil "
function(){
  var ids = jQuery(\"#~A\").jqGrid('getDataIDs');
  for(var i=0;i < ids.length;i++){
    var cl = ids[i];
    ~{~A~%~}
  }
}
" grid-id (loop :for (in-name btn-str) :in (reverse col-replace) :collect
             (format nil "jQuery(\"#~A\").jqGrid('setRowData',ids[i],{~A: ~A});"
                     grid-id in-name btn-str))))))))))


(defmethod restas:render-object :around ((designer action-render) (obj grid))
  (check-perm-for-cur-user-with-dbg (a-perm obj) obj ""))


;; ACT: blk
(defmethod restas:render-object ((designer action-render) (obj blk))
  (tpl:blk
   (list :title (a-title obj)
         :contents (format nil "~{~A~}"
                           (loop :for infld :in (a-contents obj) :collect
                              (restas:render-object (mi 'action-render) infld)))))) ;; <-- dispatcher

(defmethod restas:render-object :around ((designer action-render) (obj blk))
  (check-perm-for-cur-user-with-dbg (a-perm obj) obj ""))


;; ACT: linear
(defmethod restas:render-object ((designer action-render) (obj linear))
  (let ((*val*  (a-val obj)))
    (declare (special *val*))
    (tpl:frmobj
     (list :content (format nil "~{~A~}"
                            (loop :for infld :in (a-fields obj) :collect
                               (restas:render-object (mi 'linear-render) infld))))))) ;; <-- dispatcher

(defmethod restas:render-object :around ((designer action-render) (obj linear))
  (check-perm-for-cur-user-with-dbg (a-perm obj) obj ""))


;; ACT: popbtn
(defmethod restas:render-object ((designer action-render) (obj popbtn)) ;; Только внутри action-render иначе будут вложенные теги <form>!
  (tpl:popbtnlin (list :id (a-name obj)
                       :width (a-width obj)
                       :height (a-height obj)
                       :content  (format nil "~{~A~}"
                                         (loop :for item :in (a-actions obj)
                                            :collect (restas:render-object (mi 'action-render) item)))
                       :value (a-value obj))))

(defmethod restas:render-object :around ((designer action-render) (obj popbtn))
  (check-perm-for-cur-user-with-dbg (a-perm obj) obj ""))


;; ACT: announce
(defmethod restas:render-object ((designer action-render) (obj announce))
  (format nil "~{~A~}"
          (loop :for (id . announce) :in (funcall (a-val obj)) :append
             (list
              (tpl:postannounce (list :title (a-title announce)
                                      :date (a-date announce)
                                      :photoannounce (or (a-announce-photo announce) "")
                                      :announce (a-announce announce)
                                      :id id))))))


;; ACT: post
(defmethod restas:render-object ((designer action-render) (obj post))
  (let ((val (funcall (a-val obj))))
    (if (null val)
        "404 Not Found Post"
        (tpl:posttext (list :title (a-title val)
                            :text (a-text val)
                            :date (a-date val)
                            :phototext (or (a-text-photo val) ""))))))


;; ACT: yamap
(defmethod restas:render-object ((designer action-render) (obj yamap))
  (tpl:map (list :title (a-title obj)
                 :center (a-center-coord obj)
                 :scale (a-scale obj)
                 :placemarks (format nil "~{~A~}"
                                     (mapcar #'(lambda (point)
                                                 (tpl:placemark
                                                  (list :title (a-title point)
                                                        :link  (a-link  point)
                                                        :coord (a-coord point)
                                                        :descr (a-descr point))))
                                             (a-mark-points obj))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRID-RENDER ;;;;;;;;;;;;;;;;



;; GRID: default (error)
(defmethod restas:render-object ((designer grid-render) (obj t))
  (error "no applicable render method for grid: ~A" (type-of obj)))


;; FLD in GRID


(defmethod restas:render-object ((designer grid-render) (obj fld))
  (values
   (a-title obj)
   `(("name"     . ,(a-name obj))
     ("index"    . ,(a-name obj))
     ("width"    . ,(a-width obj))
     ("align"    . ,(if (equal '(:num) (a-typedata obj)) "center" "left"))
     ("sortable" . t)
     ("editable" . nil))))

(defmethod restas:render-object :around ((designer grid-render) (obj fld))
  (check-perm-for-cur-user-with-dbg (a-show obj) obj nil))


;; BTN in GRID


(defmethod restas:render-object ((designer grid-render) (obj btn))
  (values
   ""
   `(("name"     . "")
     ("index"    . "")
     ("width"    . ,(a-width obj))
     ("align"    . "center")
     ("sortable" . nil)
     ("editable" . nil))))

(defmethod restas:render-object :around ((designer grid-render) (obj btn))
  ;; При выводе в гриде столбцов содержащих кнопки проверяются права на строчку а не на столбец
  (call-next-method))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LINEAR-RENDER ;;;;;;;;;;;;;;


;; LINEAR: default (error)
(defmethod restas:render-object ((designer linear-render) (obj t))
  (error "no applicable render method for linear: ~A" (type-of obj)))


(defmethod restas:render-object ((designer linear-render) (obj fld))
  (let ((val       (funcall      *val*))
        (namefld   (a-name       obj))
        (captfld   (a-title      obj))
        (permfld   (mi 'perm :update (a-update obj) :view (a-view obj) :show (a-show obj)))
        (typedata  (a-typedata   obj)))
    (declare (ignore permfld))
    (show-linear-elt typedata val namefld captfld permfld)))


(defmethod restas:render-object ((designer linear-render) (obj btn))
  (tpl:btnlin (list :name (a-name obj) :value (a-value obj))))

(defmethod restas:render-object :around ((designer linear-render) (obj btn))
  (check-perm-for-cur-user-with-dbg (a-perm obj) obj ""))


(defmethod restas:render-object ((designer linear-render) (obj popbtn))
  (tpl:popbtnlin (list :id (a-name obj)
                       :width (a-width obj)
                       :height (a-height obj)
                       :content  (format nil "~{~A~}"
                                         (loop :for item :in (a-actions obj)
                                            :collect (restas:render-object (mi 'action-render) item)))
                       :value (a-value obj))))

(defmethod restas:render-object :around ((designer action-render) (obj popbtn))
  (check-perm-for-cur-user-with-dbg (a-perm obj) obj ""))


(defmethod restas:render-object ((designer linear-render) (obj grid))
  (restas:render-object (mi 'action-render) obj))


(defmethod restas:render-object ((designer linear-render) (obj file))
  (tpl:fld
   (list :fldname (a-value obj)
         :fldcontent (tpl:fileupd (list :name (a-name obj))))))
