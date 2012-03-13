(in-package #:WIZARD)

(defmethod grid-fltr-op ((op (eql :eq)) etalon sample)
  "равно"
  (equal (string-downcase (string-trim '(#\Space #\Tab #\Newline) etalon))
         (string-downcase (string-trim '(#\Space #\Tab #\Newline) sample))))

(defmethod grid-fltr-op ((op (eql :ne)) etalon sample)
  "не равно"
  (not (grid-fltr-op :eq etalon sample)))

(defmethod grid-fltr-op ((op (eql :bw)) etalon sample)
  "начинается с"
  (equal 0 (search (string-downcase (string-trim '(#\Space #\Tab #\Newline) etalon))
                   (string-downcase(string-trim '(#\Space #\Tab #\Newline) sample)))))

(defmethod grid-fltr-op ((op (eql :bn)) etalon sample)
  "не начинается с"
  (not (grid-fltr-op :bw etalon sample)))

(defmethod grid-fltr-op ((op (eql :ew)) etalon sample)
  "заканчивается на"
  (grid-fltr-op :bw (reverse etalon) (reverse sample)))

(defmethod grid-fltr-op ((op (eql :en)) etalon sample)
  "не заканчивается на"
  (not (grid-fltr-op :ew etalon sample)))

(defmethod grid-fltr-op ((op (eql :cn)) etalon sample)
  "содержит"
  (if (search (string-downcase (string-trim '(#\Space #\Tab #\Newline) etalon))
              (string-downcase (string-trim '(#\Space #\Tab #\Newline) sample)))
      t
      nil))

(defmethod grid-fltr-op ((op (eql :nc)) etalon sample)
  "не содержит"
  (not (grid-fltr-op :cn etalon sample)))





