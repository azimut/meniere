;;;; meniere.lisp

(in-package #:meniere)

;; http://stevelosh.com/blog/2016/08/playing-with-syntax/
(defmacro mulf (place factor)
  `(setf ,place (* ,place ,factor)))

(defmacro divf (place divisor)
  `(setf ,place (/ ,place ,divisor)))

;; Placeholder for overtone.lisp elsewhere
(defun note (n)
  (cm:keynum n))

(defun midihz (n)
  (cm:hertz n))

;; https://rosettacode.org/wiki/Map_range#Common_Lisp
(defun map-range (a1 a2 b1 b2 s)
  (+ b1
     (/ (* (- s a1)
	   (- b2 b1))
	(- a2 a1))))
