(in-package #:WIZARD)

(with-defclass (~act ())
  (title "")
  (perm :all)
  (val nil)
  (entity nil)
  (fields nil))


(with-defclass (~nop (~act)))

(with-defclass (~lin (~act)))

(with-defclass (~grd (~act))
  (grid nil)
  (param-id nil)
  (height "180"))

(with-defclass (~blk (~act))
  (contents nil))

(with-defclass (~tpl (~act)))

(with-defclass (~ann (~act)))

(with-defclass (~pst (~act))
  (date "")
  (announce-photo nil)
  (announce "")
  (text-photo nil)
  (text ""))

(with-defclass (~upl (~act))
  (value "")
  (name ""))

(with-defclass (~map (~act))
  (center-coord "")
  (mark-points nil))


(with-defclass (~fld ())
  (name "")
  (title "")
  (typedata '(:str))
  (width 200)
  (xref nil)
  (update nil)
  (view nil)
  (show nil))

(with-defclass (~btn ())
  (name "")
  (title "")
  (width 200)
  (value "")
  (perm :all)
  (act))

(with-defclass (~pop ())
  (name "")
  (title "")
  (top 200)
  (left 400)
  (width 200)
  (height 400)
  (value "")
  (perm :all)
  (actions ""))
