(in-package :wizard)

;;;;;;;;;;;;;;;
(defun copy-resources (res-path &key (static-files t) directories
		       &aux (syspath (get-system-path)))
  (when static-files
    (flet ((copy-static-file (path &aux new-path)
	     (setf new-path (translate-pathname path
						(merge-pathnames "**/*.*" syspath)
						(merge-pathnames "**/*.*" res-path)))
	     (ensure-directories-exist (get-dir new-path))
	     (alexandria:copy-file path new-path))
	   )
      (map-components +system-name+
		      #'(lambda (cmp)
			  (when (typep cmp 'asdf:static-file)
			    (copy-static-file (asdf:component-pathname cmp)))))))
  (when directories
    (flet ((copy-res-directory (relative-path &aux old-path new-path)
	     (setf old-path (cl-fad:pathname-as-directory
			     (merge-pathnames relative-path syspath)))
	     (setf new-path (translate-pathname old-path
						(merge-pathnames "**/*.*" syspath)
						(merge-pathnames "**/*.*" res-path)))
	     (copy-directory old-path new-path)))
      (dolist (dir directories)
	(copy-res-directory dir)))))
;;;;;;;;;;;
;; Пример использования (скопирует все static-file компоненты и указанные директории
;; в папку для ресурсов):
;; WIZARD> (copy-resources "/home/someuser/tmp/resexp/" :directories '("img" "some-dir"))
;;;;;;;;;;;;;;;;;

(defun destroy-other-threads ()
  (mapc #'sb-thread:destroy-thread
	(remove sb-thread:*current-thread* (sb-thread:list-all-threads))))

(defun restore-wizard ()
  (setf *basedir* (get-resources-path  (get-core-path)))
  (start-wizard))

;; (defun save-wizard (file &aux (res-path (get-resources-path file)))
;;   (pushnew #'(lambda ()
;; 	       (stop-wizard)
;; 	       (disconnect)
;;                ;;; Needed to add directories with resources
;; 	       (copy-resources res-path
;; 			       :directories '(
;; 					      "analitic"
;; 					      "css"
;; 					      "doc"
;; 					      "img"
;; 					      "ivent"
;; 					      "js"
;; 					      "laws"
;; 					      "lib.lisp"
;; 					      "materili"
;; 					      "news"
;; 					      "techno"
;; 					      "tenders"
;; 					      "tpl"
;; 					      "video"
;; 					      "wizard.asd"
;; 					      ))
;; 	       (asdf:clear-configuration))
;; 	  sb-ext:*save-hooks*)

;;   ;;; Need rewriting
;;   ;(ignore-errors (swank:stop-server 4005))

;;   (destroy-other-threads)
;;   (with-open-file (stream (make-pathname :defaults file
;; 					 :directory (pathname-directory (get-dir file))
;; 					 :name (concatenate 'string "run-" (pathname-name file))
;; 					 :type "sh")
;; 			  :direction :output
;; 			  :if-does-not-exist :create
;; 			  :if-exists :supersede)
;;     (princ "rlwrap ./wizard-serv --eval \"(progn (wizard::restore-wizard) (swank:create-server :port 4005))\""
;; 	   stream))
;;   (sb-ext:save-lisp-and-die file :executable t))
