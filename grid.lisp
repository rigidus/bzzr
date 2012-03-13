(in-package #:WIZARD)

(defun json-assembly (cur-page total-page rows-per-page rows)
  "example call: (json-assembly 1 2 2 '( (1 \"one\" \"two\") (2 \"three\" \"fourth\")))"
  (json:encode-json-to-string
   `(("page"    . ,cur-page)
     ("total"   . ,total-page)
     ("records" . ,rows-per-page)
     ("rows"    . ,(loop :for row :in rows :collect
                      `(("id"   . ,(car row))
                        ("cell" . ,(cdr row))))))))


(defmethod get-accessor-perm ((infld fld))
  (let ((perm  (a-show infld))
        (symb  (find-symbol (format nil "A-~A" (a-name infld)) (find-package "WIZARD")))
        (accessor))
    (cond ((equal '(:bool)                             (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (aif (funcall symb x) "да" "нет")))))
          ((equal '(:str)                              (a-typedata infld))
           (let ((tmp (a-xref infld)))
             (if (null tmp)
                 (setf accessor (lambda (x) (format nil "~A" (funcall symb x))))
                 (setf accessor (lambda (x) (format nil "<a href=\"/~A/%|id|%\">~A</a>"
                                                    tmp
                                                    (funcall symb x)))))))
          ((equal '(:text)                             (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (aif (funcall symb x) it "")))))
          ((equal '(:num)                              (a-typedata infld))
           (setf accessor (lambda (x)
                            (let ((val (funcall symb x)))
                              (if (realp val)
                                  (format nil "~F" val)
                                  (format nil "~A" val))))))
          ((equal '(:link resource)                    (a-typedata infld))
           (let ((tmp (a-xref infld)))
             (if (null tmp)
                 (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x)))))
                 (setf accessor (lambda (x) (format nil "<a href=\"/~A/%|id|%\">~A</a>"
                                                    tmp
                                                    (a-name (funcall symb x)))))))
           ;; (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x)))))
           )
          ((equal '(:link tender)                      (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x))))))
          ((equal '(:list-of-keys tender-status)       (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (getf *tender-status* (funcall symb x))))))
          ((equal '(:list-of-keys offer-status)        (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (getf *offer-status* (funcall symb x))))))
          ((equal '(:link builder)                     (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x))))))
          ((equal '(:link supplier)                    (a-typedata infld))
           (let ((tmp (a-xref infld))) ;; for offer page (block "Заявки)
             (if (null tmp)
                 (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x)))))
                 (setf accessor (lambda (x) (format nil "<a href=\"/~A/%|id|%\">~A</a>"
                                                     tmp
                                                     (a-name (funcall symb x))))))))
          ((equal '(:link tender-resource)             (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (a-name (a-resource (funcall symb x)))))))
          ((equal '(:list-of-keys resource-types)      (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (getf *resource-types* (funcall symb x))))))
          ((equal '(:interval)                         (a-typedata infld))
           (setf accessor (lambda (x)  (let ((val (funcall symb x)))
                                         (format nil "~A-~A"
                                                 (decode-date (interval-begin val))
                                                 (decode-date (interval-end val)))))))
          (t ;; default - print unknown type message in grid field
           (let ((err-str (format nil "unk typedata: ~A" (a-typedata infld))))
             (setf accessor (lambda (x)
                              (declare (ignore x))
                              err-str)))))
    (values
     accessor
     perm)))


(defmethod get-accessor-perm ((infld btn))
  (let* ((perm      (a-perm  infld))
         (btn       (a-name  infld))                     ;; тут важно чтобы вычисление происходило вне лямбды
         (value     (a-value infld))                     ;; тут важно чтобы вычисление происходило вне лямбды
         (accessor  (lambda (x)
                      (declare (ignore x))
                      (format nil "<form method='post'><input type='submit' name='~A~~%|id|%' value='~A' /></form>"
                              btn
                              value))))
    (values accessor perm)))


(defmethod get-accessor-perm ((infld popbtn))
  (let* ((perm      (a-perm  infld))
         (btn       (a-name  infld))                ;; тут важно чтобы вычисление происходило вне лямбды
         (value     (a-value infld))                 ;; тут важно чтобы вычисление происходило вне лямбды
         (accessor  (lambda (x)
                      (declare (ignore x))
                      (format nil "<input type='button' name='~A~~%|id|%' value='~A' onclick='ShowHide(\"~A\")' />"
                              btn
                              value
                              btn))))
    (values accessor perm)))


;; (defmethod get-accessor-perm ((infld calc))
;;    (let* ((perm      (getf infld :perm))
;;           (calc      (getf infld :cacl))                  ;; тут важно чтобы вычисление происходило вне лямбды
;;           (func      (getf infld :func))                  ;; тут важно чтобы вычисление происходило вне лямбды
;;           (accessor  (getf infld :func)))
;;      (push (cons accessor perm) field-cons)))


(defmethod get-accessor-perm (infld)
  (error (format nil "defmethod (get-accessor-perm (~A) not implemented)" (type-of infld))))


(defun pager (val fields page rows-per-page)
  ;; Полезная отладка, чтобы определить, какой контроллер вызывает ошибку
  ;; (error (format nil "~A" val))) ;;
  (let* ((rows             (funcall val))
         (cnt-rows         (length rows))
         (slice-cons)      ;; many of (id . #<object>)
         (field-cons))     ;; manu of (#<function-accessor> . plist-permlist)
    ;; если запрос с поиском - отфильтровываем из rows объекты, не подходящие под фильтр
    (when (equal "true" (hunchentoot:get-parameter "_search"))
      (flet ((fltr (x) (grid-fltr-op
                        (intern (string-upcase (hunchentoot:get-parameter "searchOper")) :keyword)
                        (hunchentoot:get-parameter "searchString")
                        (funcall (intern (format nil "A-~A" (hunchentoot:get-parameter "searchField")) :wizard)
                                 (cdr x)))))
      (setf rows (remove-if-not #'fltr rows))))
    ;; slice-cons (overloop) - выбираем те объекты, которые попадают на страницу
    (loop :for num :from (* page rows-per-page) :below (* (+ 1 page) rows-per-page) :do
       (let ((row (nth num rows)))
         (unless (null row)
           (push (nth num rows) slice-cons))))
    ;; field-cons (innerloop) - получаем для каждого поля функцию-accessor и права
    (loop :for infld :in fields :collect
       (multiple-value-bind (accessor perm)
           (get-accessor-perm infld)
       (push (cons accessor perm) field-cons)))
    ;; Применяем к каждому выбранному на первом шаге объекту функции-accessor-ы полученные на втором
    (let ((result
           (loop :for (id . obj) :in (reverse slice-cons) :collect
              (list* id
                     (loop :for (accessor . perm) :in (reverse field-cons) :collect
                        (if (check-perm perm (cur-user) obj)
                            (replace-all (funcall accessor obj) "%|id|%" (format nil "~A" id))
                            (if (and (boundp '*dbg*) *dbg*) "permdenied-grid-pager" ""))
                        )))))
      (values result cnt-rows))))


(defun example-json (val fields)
  "Выясняем страницу и кол-во строк в ней, вызываем pager и формируем вывод с помощью json-assembly"
  (let* ((page            (- (parse-integer (hunchentoot:get-parameter "page")) 1))
         (rows-per-page   (parse-integer (hunchentoot:get-parameter "rows"))))
    (multiple-value-bind (slice cnt-rows)
        (pager val fields page rows-per-page)
      (json-assembly  (+ page 1)  (ceiling cnt-rows rows-per-page)  (length slice) slice))))
