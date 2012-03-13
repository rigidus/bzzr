(in-package #:wizard)

(connect :user "root" :password *db-password* :database *db-name*)
(query "SET NAMES utf8")

(query "TRUNCATE TABLE `user`")

;; (query
;;  "CREATE TABLE IF NOT EXISTS `user` (
;;   `id` int(11) NOT NULL AUTO_INCREMENT,
;;   `entity` varchar(255) NOT NULL,
;;   `login` varchar(255) NOT NULL,
;;   `password` varchar(255) NOT NULL,
;;   `email` varchar(255) NOT NULL,
;;   `person` varchar(255) NOT NULL,
;;   `phone` varchar(255) NOT NULL,
;;   `fax` varchar(255) NOT NULL,
;;   `site` varchar(255) NOT NULL,
;;   `city` varchar(255) NOT NULL,
;;   `distinct` varchar(255) NOT NULL,
;;   `metro` varchar(255) NOT NULL,
;;   `name` varchar(255) NOT NULL,
;;   `rightform` varchar(255) NOT NULL,
;;   `status` varchar(255) NOT NULL,
;;   `juridical-address` varchar(255) NOT NULL,
;;   `actual-address` varchar(255) NOT NULL,
;;   `inn` varchar(255) NOT NULL,
;;   `kpp` varchar(255) NOT NULL,
;;   `ogrn` varchar(255) NOT NULL,
;;   `bank-name` varchar(255) NOT NULL,
;;   `bik` varchar(255) NOT NULL,
;;   `corresp-account` varchar(255) NOT NULL,
;;   `client-account` varchar(255) NOT NULL,
;;   `affiliates` int(11) NOT NULL,
;;   `resources` int(11) NOT NULL,
;;   `price-elts` int(11) NOT NULL,
;;   `offers` int(11) NOT NULL,
;;   `sales` int(11) NOT NULL,
;;   `tenders` varchar(255) NOT NULL,
;;   PRIMARY KEY (`id`)
;; ) ENGINE=MyISAM  DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RESOURCES & CATEGORYES


(defun categoryes-and-resources ()
  "Переносим ресурсы и категории"
  (clrhash *CATEGORY*)
  (clrhash *RESOURCE*)
  ;; Забираем все единицы измерения
  (let ((measures (make-hash-table :test #'equal)))
    (with-query-select ("SELECT |:::| FROM `jos_gt_measure_unit`"
                        ("id" "name"))
      (setf (gethash id measures) name))

    ;; ------------------------------
    ;; Забираем ресурсы
    (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_resource`")
                        ("id" "name" "unit_id" "group_id" "type"))
      ;; Создаем ресурс, не заполняя его категории
      (setf (gethash id *RESOURCE*)
            (mi 'RESOURCE
                :name name
                :resource-type (if (equal 1 type) :material :machine)
                :unit (gethash unit_id measures))))
    ;; Забираем категории
    (with-query-select ("SELECT |:::| FROM `jos_gt_resource_shgroup`"
                        ("id" "name" "type" "parent_id"))
      (setf (gethash id *CATEGORY*)
            (mi 'CATEGORY
                :name name
                ;; здесь parent еще числовой
                :parent parent_id)))
    ;; Связываем категории в дерево - здесь parent становится категорией, и слот child-categoryes становится валидным
    (maphash #'(lambda (key category)
                 (let ((parent-category (gethash (a-parent category) *CATEGORY*)))
                   (setf (a-parent category) parent-category)
                   (when parent-category
                     (append-link (a-child-categoryes parent-category) category))))
             *CATEGORY*)
    ;; Забираем связи и связываем ресурсы с категориями и категории с ресурсами
    (with-query-select ("SELECT |:::| FROM `jos_gt_resource_shgroup_bind`"
                        ("id" "group_id" "resource_id"))
      (let ((category (gethash group_id *CATEGORY*))
            (resource (gethash resource_id *RESOURCE*)))
        (if (or (null category)
                (null resource))
            ;; (format t "~%link category [~A] and resource [~A] not exists" group_id resource_id)
            nil
            ;; else
            (progn
              (append-link (a-resources category) resource)
              (append-link (a-categoryes resource) category)))))))


  ;;   ;; -------------------------------
  ;;   ;; Забираем сырые данные по категориям из базы
  ;;   (with-query-select ("SELECT |:::| FROM `jos_gt_resource_group`"
  ;;                       ("id" "name" "type" "parent_id"))
  ;;     (let ((save-group-id   id)
  ;;           (save-group-name name)
  ;;           (this-category   (setf (gethash id *CATEGORY*)
  ;;                                  (make-instance 'CATEGORY
  ;;                                                 :name name
  ;;                                                 ;; здесь parent еще числовой
  ;;                                                 :parent parent_id))))
  ;;       ;; Забираем ресурсы, принадлежащие этой категории
  ;;       (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_resource` WHERE `group_id` = '~A'" id)
  ;;                           ("id" "name" "unit_id" "group_id" "type"))
  ;;         ;; Создаем ресурс, связывая его с категорией
  ;;         (let ((this-resource (setf (gethash id *RESOURCE*)
  ;;                                    (make-instance 'RESOURCE
  ;;                                                   :name name
  ;;                                                   :category this-category
  ;;                                                   :resource-type (if (equal 1 type) :material :machine)
  ;;                                                   :unit (gethash unit_id measures)))))
  ;;           ;; Выдаем warning если в базе связь ресурс-категория не совпадает со связью категория-ресурс
  ;;           (unless (equal save-group-id group_id)
  ;;             (format t "~&warn: not equal link to category (~A | ~A) and resource (~A | ~A)"
  ;;                     save-group-id
  ;;                     save-group-name
  ;;                     id
  ;;                     name))
  ;;           ;; Добавляем этот ресурс в категорию, т.е. связываем категорию с ресурсом
  ;;           (append-link (a-resources this-category) this-resource)
  ;;           ;; TODO: Тут  нужно еще связать ресурс с ценой через справочник
  ;;           t)t)t)t)t)
  ;; ;; Связываем категории в дерево - здесь parent становится категорией, и слот child-categoryes становится валидным
  ;; (maphash #'(lambda (key category)
  ;;              (let ((parent-category (gethash (a-parent category) *CATEGORY*)))
  ;;                (setf (a-parent category) parent-category)
  ;;                (when parent-category
  ;;                  (append-link (a-child-categoryes parent-category) category))))
  ;;          *CATEGORY*)
  ;; t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ADMIN, BUILDERS, SUPPLIERS and EXPERTS


(defun users ()
  ;; Очистка
  (clrhash *USER*)
  ;; Нулевой - админ
  (setf (gethash 0 *user*) (mi 'admin :login "admin" :password "admin" :name "admin"))
  ;;
  (let ((hash-city    (make-hash-table :test #'equal))
        (company_type (make-hash-table :test #'equal)))
    ;; Получаем города и записываем их в хэш-таблицу
    (with-query-select ("SELECT |:::| FROM `jos_gt_city`"
                        ("id" "name"))
      (setf (gethash id hash-city) name))
    ;; Идентификаторы всех поставщиков предзаносим в company_type
    (with-query-select ("SELECT |:::| FROM `jos_gt_company_group_bind`"
                          ("company_id" "group_id"))
        (when (< group_id 5)
          (setf (gethash company_id company_type) 1)))
    ;; Забираем все компании
    (with-query-select ("SELECT |:::| FROM `jos_gt_company`"
                        ("id" "juridical_address_id" "actual_address_id" "head_id" "details_id" "name" "email" "site" "is_diligent"))
      (let ((company_id id))
        ;; Для каждой собираем все адреса, телефоны и прочее
        (let ((juridical-address) (actual-address) (contacts) (heads) (divisions)
              (inn*) (ogrn*) (bank-name*) (bik*) (correspondent_account*) (sattlement_account*))
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_address` WHERE `id`='~A'" juridical_address_id)
                              ("city_id" "street" "house"))
            (setf juridical-address (format nil "~A ~A ~A" (gethash city_id hash-city) street house)))
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_address` WHERE `id`='~A'" actual_address_id)
                              ("city_id" "street" "house"))
            (setf actual-address (format nil "~A ~A ~A" (gethash city_id hash-city) street house)))
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_employee` WHERE `id`='~A'" head_id)
                              ("second_name" "name" "patronymic" "post" "phone" "email" "user_id"))
            (setf heads (format nil "~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] "
                                post second_name name patronymic phone email)))
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_details` WHERE `id`='~A'" details_id)
                              ("inn" "ogrn" "bank" "bik" "correspondent_account" "sattlement_account"))
            (setf inn* inn)
            (setf ogrn* ogrn)
            (setf bank* bank)
            (setf bik* bik)
            (setf correspondent_account* correspondent_account)
            (setf sattlement_account* sattlement_account))
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_division` WHERE `company_id`='~A'" company_id)
                              ("city_id" "name" "post_index" "street" "house" "office" "phone"))
            (push (format nil "~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~]"
                          (gethash city_id hash-city) name post_index street house office phone)
                  divisions)
            t)
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_phone` WHERE `company_id`='~A'" company_id)
                              ("number"))
            (append-link contacts number)
            t)
          ;; to *USER*
          (if (gethash company_id company_type)
              ;; Поставщик
              (progn
                ;; (format t "~%[SUPPLIER]: ~A | ~A" name company_id)
                (let ((supplier (setf (gethash company_id *USER*)
                                      (make-instance 'SUPPLIER
                                                     :login (symbol-name (gensym "LOGIN"))
                                                     :password (symbol-name (gensym "PASSWORD"))
                                                     :name (format nil "~A" name)
                                                     :email email
                                                     :site site
                                                     ;; :heads heads
                                                     :inn inn*
                                                     :ogrn ogrn*
                                                     :bank-name bank-name*
                                                     :bik bik*
                                                     :corresp-account correspondent_account*
                                                     :client-account sattlement_account*
                                                     :status (if (equal 1 is_diligent)  :fair  :unfair)
                                                     :juridical-address juridical-address
                                                     :actual-address actual-address
                                                     ;; :contacts contacts
                                                     ))))
                  ;; Сохраняем в таблицу `users`
                  (query
                   (format nil "INSERT INTO `ktopostavlyaet`.`user` (
 `id`,
 `entity`,
 `login`,
 `password`,
 `name`,
 `status`,
 `city`,
 `distinct`,
 `metro`,
 `juridical-address`,
 `actual-address`,
 `phone`,
 `fax`,
 `email`,
 `site`,
 `inn`,
 `kpp`,
 `ogrn`,
 `bank-name`,
 `bik`,
 `corresp-account`,
 `client-account`,
 `rightform`,
 `person`
) VALUES (
 ~A,
 'supplier',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]',
 '~@[~A~]'
); SELECT LAST_INSERT_ID();"
                           company_id
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-login supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-password supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-name supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-status supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-city supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-distinct supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-metro supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-juridical-address supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-actual-address supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-phone supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-fax supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-email supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-site supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-inn supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-kpp supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-ogrn supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-bank-name supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-bik supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-corresp-account supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-client-account supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-rightform supplier))))
                           (format nil "~@[~A~]" (strip (format nil "~@[~A~]" (a-person supplier))))
                           ))
                  ;; Адреса филиалов
                  ;; (mapcar #'(lambda (x)
                  ;;             (let ((supplier-affiliate (push-hash *supplier-affiliate* 'supplier-affiliate
                  ;;                                         :owner supplier
                  ;;                                         :address x)))
                  ;;               (append-link (a-affiliates supplier) supplier-affiliate))
                  ;;             (let ((supplier-affiliate (push-hash *supplier-affiliate* 'supplier-affiliate
                  ;;                                         :owner supplier
                  ;;                                         :address x)))
                  ;;               (append-link (a-affiliates supplier) supplier-affiliate)))
                  ;;         divisions)
                  ;; Это поставщик, значит у него могут быть ресурсы
                  (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_resource` WHERE `company_id`='~A'" company_id)
                                      ("id" "resource_id" "price"))
                    (let ((supplier-resource (setf (gethash id *SUPPLIER-RESOURCE*)
                                                   (make-instance 'SUPPLIER-RESOURCE
                                                                  :owner supplier
                                                                  :resource (gethash resource_id *RESOURCE*)
                                                                  :price price))))
                      (append-link (a-resources supplier) supplier-resource)))
                  )
                company_id)
              ;; else
              ;; Застройщик
              (progn
                ;; (format t "~%[BUILDER]: ~A" name )
                (setf (gethash company_id *USER*)
                      (make-instance 'BUILDER
                                     :login (symbol-name (gensym "LOGIN"))
                                     :password (symbol-name (gensym "PASSWORD"))
                                     :name (format nil "~A" name)
                                     :email email
                                     :site site
                                     :juridical-address juridical-address
                                     :actual-address actual-address
                                     ;; :contacts contacts
                                     ))
                nil))))))
  ;; Эксперты
  ;; (loop :for i :from 1 :to 9 :do
  ;;    (push-hash *USER* 'EXPERT
  ;;      :name (format nil "Эксперт-~A" i)
  ;;      :login (format nil "exp~A" i)
  ;;      :password (format nil "exp~A" i)))
  ;; Застройщик
  (setf (gethash 9999999 *USER*)
        (mi 'BUILDER
            :name "builder"
            :login "builder"
            :password "builder"
            ))
  )

;; (users)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TENDERS


(defun tenders ()
  "Тендеры"
  (clrhash *TENDER*)
  (clrhash *TENDER-RESOURCE*)
  (clrhash *OFFER*)
  (clrhash *OFFER-RESOURCE*)
  ;; Забираем все тендеры
  (with-query-select ("SELECT |:::| FROM `jos_gt_tender`"
                      ("id" "company_id" "name" "tender_begin" "tender_end" "order_begin" "order_end" "process_begin" "process_end"
                            "talking_begin" "talking_end" "resulting_begin" "resulting_end" "total" "pricetype" "pricesource"
                            "coverage" "status_id"))
    (let* ((save-tender-id id)
           (builder (gethash company_id *USER*))           ;; Отыскиваем компанию, которой принадлежит тендер, по company_id
           (this-tender (setf (gethash id *TENDER*)        ;; Создаем тендер, связывая его с владельцем
                              (make-instance 'TENDER
                                             :name name
                                             :owner builder
                                             :all        (make-interval :begin tender_begin    :end tender_end)
                                             :claim      (make-interval :begin order_begin     :end order_end)
                                             :analize    (make-interval :begin process_begin   :end process_end)
                                             :interview  (make-interval :begin talking_begin   :end talking_end)
                                             :result     (make-interval :begin resulting_begin :end resulting_end)
                                             :status (ecase status_id (1 :unactive) (2 :active) (3 :cancelled) (4 :finished))))))

      ;; Связываем владельца с созданным тендером
      (append-link (a-tenders builder) this-tender)
      ;; Забираем ресурсы тендера
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_resource` WHERE `tender_id` = ~A" save-tender-id)
                          ("id" "tender_id" "resource_id" "quantity" "price" "pricedate" "comments" "deliver" "is_basis"))
        ;; Создаем объекты tender-resource, связанные с тендером
        (let ((this-tender-resource (setf (gethash id *TENDER-RESOURCE*)
                                          (make-instance 'TENDER-RESOURCE
                                                         :tender this-tender
                                                         :resource (gethash resource_id *RESOURCE*)
                                                         :quantity quantity
                                                         :price price
                                                         :price-date pricedate
                                                         :comment comments
                                                         :delivery (if (equal deliver 1) T NIL)
                                                         :basic   (if (equal is_basis 1) T NIL)
                                                         ))))
          ;; Связываем тендер с созданным tender-resource
          (append-link (a-resources this-tender) this-tender-resource)
          t))
      ;; Забираем поставщиков тендера
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_supplier` WHERE `tender_id` = ~A" save-tender-id)
                          ("id" "company_id" "is_invited"))  ;;  в этой таблице is_invited - приглашен на собеседование, им потом можно allow_modify
        ;; Связываем их с тендером
        (append-link (a-suppliers this-tender) (gethash company_id *USER*)))
      ;; Забираем приглашения поставщикам на этот тендер
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_offer` WHERE `tender_id` = ~A" save-tender-id)
                          ("id" "tender_id" "company_id" "has_been_read"))
        (let ((supplier (gethash company_id *USER*)))
          (if (not (equal (type-of supplier) 'supplier))
              (format t "~%warn: company is not supplier ~A | ~A ::: ~A"
                      (type-of supplier)
                      (a-name supplier)
                      company_id)
              ;; ELSE
              (let ((this-offer (setf (gethash id *OFFER*)
                                      (make-instance 'OFFER
                                                     :owner supplier
                                                     :tender this-tender
                                                     :status :sended ;; не паримся так как статусы уже отличаются
                                                     ))))
                ;; Связываем тендер c приглашением
                (append-link (a-offers this-tender) this-offer)
                ;; Связываем поставщика с приглашением
                (append-link (a-offers supplier) this-offer)
                ;; Забираем заявки поставщиков, которые поставщики отправили в ответ на каждое приглашение (один к одному)
                (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_order` WHERE `offer_id` = ~A" id)
                                    ("id" "offer_id" "status" "has_been_read" "allow_modify"))
                  ;; Забираем ресурсы, которые привязаны к заявке
                  (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_order_resource` WHERE `order_id` = ~A" id)
                                      ("id" "order_id" "tender_resource_id" "quantity" "price" "comments"
                                            "deliver" "deliver_cost" "marked" "price2" "place"))
                    ;; Создаем offer-resource
                    (let ((this-offer-resource (setf (gethash id *OFFER-RESOURCE*)
                                                     (make-instance 'OFFER-RESOURCE
                                                                    :offer this-offer
                                                                    :tender-resource (gethash tender_resource_id *TENDER-RESOURCE*)
                                                                    :quantity quantity
                                                                    :price price
                                                                    :price-result (aif price2 price2 0)
                                                                    :comment comments
                                                                    :delivery (if (equal 1 deliver) T NIL)
                                                                    :delivery-price deliver_cost
                                                                    :marked marked
                                                                    :rank place))))
                      ;; Связывать с tender-resouce не надо (!)
                      ;; А вот с offer свяжем
                      (append-link (a-resources this-offer) this-offer-resource)
                      ))))))))))


(defun get-post-items-from-dir (section)
  (loop
     :for html-file
     :in  (sort (directory (path (format nil "~A/*.htm" section)))
                (lambda (x y)
                  (string-greaterp (pathname-name x) (pathname-name y))))
     :collect (let ((content  (read-file-into-string html-file))
                    (fmt-tpl  "(?s)<~A>(.*)</~A>")
                    (tags     '(title date announce-photo announce text-photo text))
                    (result   (mi 'post-item :section section)))
                (loop :for tag :in tags :do
                   (let ((extract (extract (string-downcase (format nil fmt-tpl tag tag)) content)))
                     (setf extract (replace-all extract (string-downcase (format nil "<~A>" (symbol-name tag))) ""))
                     (setf extract (replace-all extract (string-downcase (format nil "</~A>" (symbol-name tag))) ""))
                     (setf (slot-value result tag)
                           (string-trim '(#\Space #\Tab #\Newline) extract))))
                result)))

(defun posts ()
  (clrhash *POST-ITEM*)
  (let ((last-id (block post-fnd-block
                  (loop :for id :from 0 :do
                     (multiple-value-bind (result present)
                         (gethash id *POST-ITEM*)
                       (unless present
                         (return-from post-fnd-block id)))))))
    (mapcar #'(lambda (x)
                (setf (gethash last-id *POST-ITEM*) x)
                (setf last-id (+ 1 last-id)))
            (append (get-post-items-from-dir "ivent")
                    (get-post-items-from-dir "techno")
                    (get-post-items-from-dir "news")
                    (get-post-items-from-dir "tenders")))))

(posts)
(categoryes-and-resources)
(users)
;; (tenders)

(defparameter *SALE* (make-hash-table :test #'equal))

;; (let ((o (push-hash *SALE* 'SALE
;;            :title     "Акция от компании \"Невастрой\""
;;            :owner     (gethash 2 *USER*)
;;            :announce  "Комплект включает в себя материалы, значительный набор крепежа, полный ассортимент торцевых шпонок. Доставка бесплатно")))
;;   (append-link (a-sales (gethash 2 *USER*)) o))

;; (let ((o (push-hash *SALE* 'SALE
;;            :title     "Сезонное предложение от \"КирпичПром\""
;;            :owner     (gethash 2 *USER*)
;;            :announce  "При покупке 5000 шт. Вам предоставляется еще 100 шт. бесплатно")))
;;   (append-link (a-sales (gethash 2 *USER*)) o))

;; (let ((o (push-hash *SALE* 'SALE
;;            :title     "Распродажа со складов \"ГипроРусь\""
;;            :owner     (gethash 2 *USER*)
;;            :announce  "Беспрецендентное предложени - гипсокартон по низкой цене. До 3000 листов в одни руки. Скидки постоянным покупателям. Самовывоз. Оплата по безналу.")))
;;   (append-link (a-sales (gethash 2 *USER*)) o))



(defun passwords ()
  (passwd)
  (restas:define-route passwords ("/passwords7")
    (format nil "<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" /></head><body><pre>~A</pre></body></html>"
            (read-file-into-string (path "passwd.txt")))))

(passwords)


(defun start-wizard ()
  (restas:start '#:wizard :port 8081 #|:address "localhost"|#))

(defun stop-wizard ()
  ;; (restas:stop-all)  ;; err: symbol not found!
  )

(start-wizard)

(restas:debug-mode-on)

