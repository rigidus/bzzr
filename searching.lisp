(in-package #:WIZARD)

(restas:define-route search-page ("/search")
  (let ((session (hunchentoot:start-session)))
    (declare (ignore session))
    (list
     (mi 'tpl :title "Результаты поиска:" :perm :ALL
         :val (lambda () "Задан пустой поисковый запрос!")))))


(defmethod searching ((category (eql :supplier)) text)
  (format nil "<br/><br/>~{~A~}"
          (mapcar #'(lambda (x)
                      (format nil "<a href=\"/supplier/~A\">~A</a><br/><br/>"
                              (car x)
                              (a-name (cdr x))))
                  (remove-if-not #'(lambda (x)
                                     (and
                                      (equal 'supplier (type-of (cdr x)))
                                      (search text (string-downcase (string-trim '(#\Space #\Tab #\Newline) (a-name (cdr x)))))
                                      ))
                                 (cons-hash-list *USER*)))))


(defmethod searching ((category (eql :map)) text)
  (let ((results)
        (points))
    (loop :for (id . supplier) :in (remove-if-not #'(lambda (x) (equal 'supplier (type-of (cdr x)))) (cons-hash-list *user*)) :collect
       (loop :for supplier-resource :in (a-resources supplier) :collect
          (let ((resource (a-resource supplier-resource)))
            (unless (null resource)
              (if (search text (string-downcase (string-trim '(#\Space #\Tab #\Newline) (a-name resource))))
                  (progn
                    (push (cons id supplier) results)
                    (return)))))))
    (setf points
          (mapcar #'(lambda (x)
                      (mi 'yapoint
                          :title (a-name (cdr x))
                          :link  (format nil "/supplier/~A" (car x))
                          :descr (format nil "~A" (a-actual-address (cdr x)))
                          :coord (geo-coder (a-actual-address (cdr x)))))
                  results))
    (format nil "~A<br></br>~{~A~}"
            (restas:render-object (mi 'action-render) (mi 'yamap
                                                          :center-coord "30.313622, 59.937720"
                                                          :mark-points points))
            (mapcar #'(lambda (x)
                        (format nil "<a href=\"/supplier/~A\">~A</a><br>~A<br/><br/>"
                                (car x)
                                (a-name (cdr x))
                                (a-actual-address (cdr x))))
                    results)
            )))


(defmethod searching ((category (eql :resource)) text)
  (let ((results))
    (loop :for (id . supplier) :in (remove-if-not #'(lambda (x) (equal 'supplier (type-of (cdr x)))) (cons-hash-list *user*)) :collect
       (loop :for supplier-resource :in (a-resources supplier) :collect
          (let ((resource (a-resource supplier-resource)))
            (unless (null resource)
              (if (search text (string-downcase (string-trim '(#\Space #\Tab #\Newline) (a-name resource))))
                  (progn
                    (push (cons id supplier) results)
                    (return)))))))
    (format nil "<br/><br/>~{~A~}"
            (mapcar #'(lambda (x)
                        (format nil "<a href=\"/supplier/~A\">~A</a><br>~A<br/><br/>"
                                (car x)
                                (a-name (cdr x))
                                (a-actual-address (cdr x))))
                    results)
            )))


(restas:define-route search-page/post ("/search" :method :post)
  (let ((session (hunchentoot:start-session)))
    (declare (ignore session))
    (list
     (mi 'tpl :title "Результаты поиска" :perm :ALL
         :val (lambda ()
                (let* ((text      (string-downcase (string-trim '(#\Space #\Tab #\Newline) (cdr (car (form-data))))))
                       (category  (cdr (cadr (form-data)))))
                  (cond ((= 0 (length text))
                         (if (string= "map" category)
                             (restas:render-object (mi 'action-render)
                                                   (mi 'yamap
                                                       :center-coord "30.313622, 59.937720"))
                             "Задан пустой поисковый запрос"))
                        ((> 3 (length text)) "Слишком короткий поисковый запрос")
                        (t  (let ((results   (cond  ((string= "supplier" category) (searching :supplier text))
                                                    ((string= "map"      category) (searching :map text))
                                                    ((string= "resource" category) (searching :resource text))
                                                    ((string= "all" category)      (concatenate 'list
                                                                                                (searching :supplier text)))
                                                    (t (format nil "~A" (bprint category))))))
                              (if (null results)
                                  "Ничего не найдено"
                                  results))))))))))


(restas:define-route newtender/post ("/newtender" :method :post)
  (if (equal 'builder (type-of (cur-user)))
      (let* ((id     (hash-table-count *TENDER*))
             (owner  (cur-user))
             (tender (setf (gethash id *TENDER*)
                           (mi 'TENDER
                               :name      (cdr (ASSOC "NAME" (FORM-DATA) :test #'equal))
                               :status    :unactive
                               :owner     owner
                               :all       (cdr (ASSOC "ALL" (FORM-DATA) :test #'equal))
                               :claim     (cdr (ASSOC "CLAIM" (FORM-DATA) :test #'equal))
                               :analize   (cdr (ASSOC "ANALIZE" (FORM-DATA) :test #'equal))
                               :interview (cdr (ASSOC "INTERVIEW" (FORM-DATA) :test #'equal))
                               :result    (cdr (ASSOC "RESULT" (FORM-DATA) :test #'equal))
                               ))))
        ;; Связываем с владельцем
        (setf (a-tenders owner)
              (append (a-tenders owner)
                      (list tender)))
        ;; Редирект
        (redirect
         (format nil "/tender/~A" id)))
      ;; else
      (list
       (mi 'tpl :title "Недостаточно прав" :perm :all
           :val (lambda ()
                  "Только залогиненные застройщики могут объявлять тендер!")))))
