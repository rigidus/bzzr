(in-package #:wizard)

(defmacro def~fld ((name &key update show view xref (width 200)) &body body)
  (declare (ignore body))
  (let (initargs)
    (when update  (setf (getf initargs :update) update))
    (when view    (setf (getf initargs :view) view))
    (when show    (setf (getf initargs :show) show))
    (when xref    (setf (getf initargs :xref) xref))
    (when width   (setf (getf initargs :width) width))
    `(mi '~fld :title ',name ,@initargs)))

(defmacro def~btn ((title perm &key (width 200)) &body act)
  (let (initargs)
    (when width   (setf (getf initargs :width) width))
    `(mi '~btn :title ,title :perm ,perm ,@initargs :act ',@act)))

(defmacro def~upl ((file perm name) &body body)
  (declare (ignore body))
  `(mi '~upl :title ',file :perm ,perm :name ,name))

(defmacro def~pop ((title perm &key (height 100)  (width 200)) &body actions)
  (let (initargs)
    (when height  (setf (getf initargs :height) height))
    (when width   (setf (getf initargs :width)  width))
    `(mi '~pop :title ,title :perm ,perm ,@initargs :actions (list ,@actions))))

(defmacro def~grd ((title perm entity val &key (height 100)) &body fields)
  (let (initargs)
    (when height  (setf (getf initargs :height) height))
    `(mi '~grd :title ,title :perm ,perm :entity ',entity :val ',val ,@initargs :fields (list ,@fields))))

(defmacro def~blk ((title perm) &body contents)
  `(mi '~blk :title ,title :perm ,perm :contents (list ,@contents)))

(defmacro def~lin ((title perm entity val) &body fields)
  `(mi '~lin :title ,title :perm ,perm :entity ',entity :val ',val :fields (list ,@fields)))

(defmacro def~tpl ((tpl) &body val)
  `(mi '~tpl :title ,tpl :val ',@val))

(defmacro def~map ((yamap) &body val)
  `(mi '~map :title ,yamap :val ',@val))

(defmacro def~nop ((none) &body body)
  (declare (ignore body))
  `(mi '~nop :title ,none))

(defmacro def~pst ((post entity val) &body fields)
  `(mi '~pst :title ,post :entity ',entity :val ',val :fields (list ,@fields)))

(defmacro def~ann ((announce entity val) &body fields)
  `(mi '~ann :title ,announce :entity ',entity :val ',val :fields (list ,@fields)))


(defmacro def~plc ((name url &key navpoint) &body actions)
  "TODO: -controllers-"
  (flet ((multi-grid (grid-elt)
           (loop :for permis :in (cadr grid-elt)
              :collect (let ((code (copy-tree (cddr grid-elt)))
                             (field-incf 0))
                         (setf (cadar code) (car permis))
                         (loop :for g-fld :in (cddr code)
                            :when (eql (car g-fld) 'def~btn)
                            :do (setf (cadadr g-fld) (cadr permis))
                            :when (eql (car g-fld) 'def~btn)
                            :do (incf field-incf
                                      (get-key-arg (cadr g-fld) :width)))
                         (if (cadr permis)
                             `(def~grd ,@code)
                             (progn
                               (incf (get-key-arg (cadadr code) :width)
                                     field-incf)
                               `(def~grd
                                    ,@(loop :for g-fld :in code
                                         :unless (eql (car g-fld) 'def~btn)
                                         :collect g-fld))))))))
    (flet ((body-change (x)
             (cond ((eql 'multi-post-grid (car x)) (multi-grid x)) ; разъединил для примера
                   ((eql 'multi-tend-grid (car x)) (multi-grid x)) ; разъединил для примера
                   (t (list x)))))
      ;;
      `(let ((rs (list :place ',name :url ,url)))
         ,(when navpoint
                `(progn
                   (nconc rs (list :navpoint ,navpoint))
                   (when (boundp '-navpoints-) ;; if exists special var -navpoints- — save navpoint!
                     (nconc -navpoints- (list (list :link ,url :title ,navpoint))))))
;;;      (nconc rs (list :actions (list 'quote (list ,@actions))))                 ; оригинал, для сравнения
         (nconc rs (list :actions (list 'quote (list ,@(mapcan #'body-change actions)))))
         rs))))


(defmacro def~asm (&body places)
  "TODO: -ajax-data-set-"
  `(let ((-navpoints- (list 'dymmy)))
     (declare (special -navpoints-)) ;; special for menu
     (let ((-places- (list ,@(loop :for item :in places :collect item))))
       (defparameter *places* (remove-if #'null -places-))
       (defparameter *navpoints* (cdr -navpoints-)))))

