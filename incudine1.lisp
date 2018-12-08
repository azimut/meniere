(in-package #:meniere)

;; Source:
;; https://soundcloud.com/kruhft/incudine1

(defparameter s1env (make-envelope '(0 1 0) '(.2 1)))

(define-vug s1 (f a e g d q)
  (* a
     (envelope e g d #'free)
     (bpf (+ (- (phasor f (coerce (random 1.0) 'double-float)) 0.5)
             (- (phasor f (coerce (random 1.0) 'double-float)) 0.5))
          f q)))

(dsp! synth1 (f a (e envelope) g d q)
  (let ((s (s1 f a e g d q)))
    (out s s)))

#+nil
(progn
  (defparameter minor #(0 2 3 5 7 8 10 12))
  (defun minor (n)
    (+ n (aref minor (random (length minor)))))

  (defun f (midi)
    (* (expt 2 (/ (- midi 69) 12)) 440f0))

  (defun x (n b a time)
    (let ((b2 (1+ (random b))))
      (synth1 (f (minor n))
              (random a)
              s1env
              .3 b2 8)
      (at (+ time #[b2 beat])
          #'x n b a (+ time #[b2 beat]))))

  (defun incudine1 ()
    (setf (bpm *tempo*) 120)
    (x 48 8 .5 (now))
    (at (+ (now) #[8 beat]) #'x 60 8 .5 (+ (now) #[8 beat])))
  
  (incudine1))
