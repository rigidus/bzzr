(in-package #:wizard)

(restas:define-route suppmap-page ("/suppmap")
  (let ((session (hunchentoot:start-session)))
    (declare (ignore session))
    (list
     (mi 'tpl :title "Карта поставщиков" :perm :all
         :val (named-lambda suppmap ()
                (funcall (find-symbol "SUPPMAP" 'tpl)
                         (list :yamap
                               (restas:render-object (mi 'action-render)
                                                     (mi 'yamap
                                                         :center-coord "30.313622, 59.937720")))))))))


(restas:define-route suppmap-page/post ("/suppmap" :method :post)
  (let ((session (hunchentoot:start-session)))
    (declare (ignore session))
    (list
     (mi 'tpl :title "Результаты поиска" :perm :all
         :val (named-lambda suppmap ()
                (funcall (find-symbol "SUPPMAP" 'tpl)
                         (list :yamap
                               (suppmap/searching (hunchentoot:post-parameter "resource")
                                                  (hunchentoot:post-parameter "center")
                                                  ))))))))

(defun suppmap/searching (resource-str object-addr-str)
  (let ((results))
    (loop :for (id . supplier) :in (remove-if-not #'(lambda (x) (equal 'supplier (type-of (cdr x)))) (cons-hash-list *user*)) :collect
       (loop :for supplier-resource :in (a-resources supplier) :collect
          (let ((resource (a-resource supplier-resource)))
            (unless (null resource)
              (if (search resource-str (string-downcase (string-trim '(#\Space #\Tab #\Newline) (a-name resource))))
                  (progn
                    (push (cons id supplier) results)
                    (return)))))))
    (setf results (subseq results 0 15))
    (let* ((object-center (geo-coder object-addr-str))
           (center (mi 'yapoint
                    :title ""
                    :link ""
                    :descr object-addr-str
                    :coord object-center))
           (points (append (mapcar #'(lambda (x)
                                      (mi 'yapoint
                                          :title (a-name (cdr x))
                                          :link  (format nil "/supplier/~A" (car x))
                                          :descr (format nil "~A" (a-actual-address (cdr x)))
                                          :coord (geo-coder (a-actual-address (cdr x)))))
                                  results)
                           (list center))))
      (format nil "~A<br></br>~{~A~}"
              (tpl:map (list :center object-center
                             :scale "10"
                             :placemarks (format nil "~{~A~}"
                                                 (mapcar #'(lambda (point)
                                                             (tpl:placemark
                                                              (list :title (a-title point)
                                                                    :link  (a-link  point)
                                                                    :coord (a-coord point)
                                                                    :descr (a-descr point))))
                                                         points))))
              (mapcar #'(lambda (x)
                          (format nil "<a href=\"/supplier/~A\">~A</a><br>~A<br/><br/>"
                                  (car x)
                                  (a-name (cdr x))
                                  (a-actual-address (cdr x))))
                      results)
              ))))
