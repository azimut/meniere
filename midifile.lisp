(in-package #:meniere)
;; TODO:
;; - go-track loop end check for outer range.
;; - function that returns pairs (50 .5) per measure
;; - everything can be probably generalized more
;;

(defvar *qnotes* (make-hash-table)
  "Hash table used by (push-midi-note) that stores the state of each key.")
(defvar *tempo-cutoff* .02
  "drops inferred silences that last less than this")

(defun get-time (mf &optional (unit :seconds))
  (declare (midifile:input-stream mf)
           ((member :seconds :beats :time)))
  (case unit
    (:seconds (midifile:event-seconds mf))
    (:beats   (midifile:event-beats mf))
    (:time    (midifile:event-time mf))))

(defun go-track (to-track mf)
  "Skip ahead TO-TRACK"
  (declare (unsigned-byte to-track) (midifile:input-stream mf))
  (let* ((number-of-tracks (1- (midifile:number-of-tracks mf)))
         (current-track    (midifile:current-track mf))
         (to-track         (max (min to-track number-of-tracks) 0)))
    (loop :repeat (- to-track current-track)
       :do (midifile:next-track mf))))

(defun get-notes (filename &optional (track-number 0))
  "Just return the midi notes, useful with bars without chords.
   > (get-notes *mf*)
   (60 64 69 34 59 30 49 59 29)"
  (declare (string filename))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (loop
       :for ev = (midifile:read-event mf)
       :while ev
       :when (and (= ev 144)
                  (= track-number (midifile:current-track mf))
                  (plusp (midifile:message-data2 mf)))
       :collect (midifile:message-data1 mf))))

(defun push-midi-note
    (note time-seconds velocity &optional start-time-p coerce)
  "helper for (get-notes-duration), returns back the note
   only when the note is released otherwise returns NIL"
  (when coerce
    (setf time-seconds (coerce time-seconds 'single-float)))
  (let ((current (gethash note *qnotes*)))
    ;; NOTE: some midi files have two hits without release between them,
    ;;       avoiding those for now
    (if current
        (when (zerop velocity)
          ;; Remove cache for note
          (setf (gethash note *qnotes*) NIL)
          (let ((result (list note (- time-seconds (cadr current)))))
            (when start-time-p
              (appendf result (list (cadr current))))
            result))
        (when (plusp velocity)
          (setf (gethash note *qnotes*)
                (list note time-seconds))
          NIL))))

(defun get-notes-durations
    (filename
     &optional (track-number 0) start-time-p (coerce t) (unit :seconds))
  "get notes and duration as pairs, useful for bars with only one note
   > (get-notes-duration *mf*)
   ((50 .2) (60 .2) (90 1.3))"
  (declare (string filename))
  (clrhash *qnotes*)
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (remove-if
     #'null
     (loop
        :for ev = (midifile:read-event mf)
        :while ev
        :with previous-time = 0f0
        :when (and (= ev 144)
                   (= track-number (midifile:current-track mf)))
        :collect
          (let ((seconds (get-time mf unit)))
            (when (not (= previous-time seconds))
              (setf previous-time seconds))
            (push-midi-note (midifile:message-data1 mf)
                            seconds
                            (midifile:message-data2 mf)
                            start-time-p
                            coerce))))))

(defun get-notes-durations-chords
    (filename
     &optional (track-number 0) start-time-p (coerce t) (unit :seconds))
  "sorts and groups (get-notes-duration) to get the notes grouped in chords
   > (meniere:get-notes-durations-chords *mf* 1)
   (((41 48 53) (0.23645833 0.23645833 0.23645833))
    ((41 48 53) (0.23645833 0.23645833 0.23645833)))"
  (declare (string filename))
  (remove-if
   #'null
   (loop
      :for (note duration time)
      :in  (sort (get-notes-durations filename track-number T coerce unit)
                 #'< :key #'caddr)
      :with queue
      :with last-time
      :collect
      (cond ((not queue)
             (prog1 NIL
               (setf last-time time)
               (setf queue (list note duration))))
            ((and queue (= last-time time))
             (prog1 NIL
               (let ((notes     (append (ensure-list (car  queue))
                                        (list note)))
                     (durations (append (ensure-list (cadr queue))
                                        (list duration))))
                 (setf queue (list notes durations)))))
            ((and queue (not (= last-time time)))
             (let ((result queue))
               (when start-time-p
                 (appendf result (list last-time)))
               (setf last-time time)
               (setf queue `(,note ,duration))
               result))))))

(defun get-notes-chords
    (filename
     &optional (track-number 0) (unit :seconds))
  "get notes grouped by time they were triggered
   > (get-notes-list *mf*)
   ((60 62 65) (60 62 69) (79)"
  (declare (string filename))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (remove-if
     #'null
     (loop
        :for ev = (midifile:read-event mf)
        :while ev
        :with queue = '()
        :when (and (= 144 ev)
                   (= track-number (midifile:current-track mf))
                   (> (midifile:message-data2 mf) 0))
        :collect
          (let* ((time (get-time mf unit))
                 (now  (list (midifile:message-data1 mf)
                             time)))
            (cond ((not queue)
                   (push now queue) NIL)
                  ((and queue (= time (cadar queue)))
                   (push now queue) NIL)
                  ;; Time to return the queue
                  ((and queue (not (= time (cadar queue))))
                   (prog1 (mapcar #'car queue)
                     (setf queue NIL)
                     (push now queue)))))))))

(defun get-notes-durations-chords-silences
    (filename
     &optional (track-number 0) start-time-p (coerce t) (unit :seconds))
  "Returns pais of notes/durations on the current TRACK-NUMBER.
   Including any silence period. As a pair with 0 as the midi-note
   and the duration of the silence.
   > (get-notes-durations-chords-silences *mf*)
   ((40 0.49895832) (42 0.49895835) (43 0.49895835) (47 0.49895835)
    (45 0.49895835) (43 0.49895835) (38 0.99895835) (40 0.4989581))"
  (declare (string filename))
  (loop
     :for (notes durations time)
     :in  (get-notes-durations-chords filename track-number T coerce unit)
     :with next-time
     :appending
       (let ((current
              (if start-time-p
                  (list notes durations time)
                  (list notes durations))))
         (cond
           ;; First run
           ((not next-time)
            (setf next-time
                  (+ time (extremum (ensure-list durations) #'>)))
            (list current))
           ;; Silence in between notes
           ((and next-time
                 (< next-time time)
                 ;; NOTE: Hacks! basically we skip silences too small product
                 ;;       of the flaky math
                 (> (- time next-time) *tempo-cutoff*))
            (let* ((zero-duration (- time next-time))
                   (zero-start    (- time zero-duration)))
              (setf next-time
                    (+ time (extremum (ensure-list durations) #'>)))
              (if start-time-p
                  (list (list 0 zero-duration zero-start) current)
                  (list (list 0 zero-duration) current))))
           ;; No silence in between (?
           (t
            (setf next-time
                  (+ time (extremum (ensure-list durations) #'>)))
            (list current))))))

;;--------------------------------------------------

(defun group-by-measure
    (mf
     &optional (track-number 0) measure-length (unit :seconds) (silence T))
  "returns 2 values as lists.
   First list of values are the notes per MEASURE_LENGTH in
   seconds if provided, if not is assumed 4/4 using the tempo
   of the miditrack.
   Second list of values is for the durations.
   > (group-by-measure *mf* 2 1)
   ((40 42 43 47) (45 43) (40 42))
   ((0.49895832 0.49895835 0.49895835 0.49895835)
    ((0.9989586 0.9989586) (0.9989586 0.9989586)))"
  (loop
     :for (notes durations)
     :in (if silence
             (get-notes-durations-chords-silences mf track-number NIL T unit)
             (get-notes-durations-chords          mf track-number NIL T unit))
     :with acc-notes
     :with acc-durations
     :with ret-notes
     :with ret-durations
     :with quarter-time = (first (get-midi-tempo mf))
     :with measure-length = (if (null measure-length)
                                ;; Assume 4/4
                                (* 4 quarter-time)
                                measure-length)
     :summing (if (listp durations)
                  (first durations)
                  durations) :into total-duration
     :do
       (cond ((> total-duration measure-length)
              (setf total-duration (if (listp durations)
                                       (first durations)
                                       durations))
              ;;
              (push (reverse acc-notes) ret-notes)
              (push (reverse acc-durations) ret-durations)
              (setf acc-notes     NIL)
              (setf acc-durations NIL)
              ;;
              (push notes     acc-notes)
              (push durations acc-durations))
             (t
              (push notes     acc-notes)
              (push durations acc-durations)))
     :finally (return (values (reverse ret-notes)
                              (reverse ret-durations)))))

(defun get-measures-pair
    (mf
     &optional (track-number 0) (n-measures 4) measure-length (unit :seconds) (silence T))
  "Returns a list of pairs of notes and durations of the midi file MF
   and TRACK-NUMBER. Up to N-MEASURES measure by MEASURE-LENGTH.
   Re-arrange output of group-by-measure to make it easier to
   shuffle measures around.
   > (get-measures-pair *mf* 2 2 1)
   (((40 42 43 47) (0.49895832 0.49895835 0.49895835 0.49895835))
    ((45 43 38)    (0.49895835 0.49895835 0.99895835)))"
  (multiple-value-bind (notes durations)
      (group-by-measure mf track-number measure-length unit silence)
    (loop
       :for measure-notes :in notes
       :for measure-durations :in durations
       :repeat n-measures
       :collect (list measure-notes
                      measure-durations))))

;;--------------------------------------------------

(defun get-midi-signature (filename &optional (track-number 0))
  "Prints the signature changes on the score.
   > (get-midi-signature \"something.mid\")
   Signature: 4/4 - Clocks per tick: 24 - Number of 1/32 per 24 clocks: 8"
  (declare (string filename))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (loop
       :for ev = (midifile:read-event mf)
       :while ev
       :when (and (midifile::meta-event-p mf)
                  ;; Check if it is a "Time signature" event
                  ;; https://www.csie.ntu.edu.tw/~r92092/ref/midi/#timesig
                  (= (midifile:message-data1 mf) #x58)
                  (= (midifile:message-data2 mf) #x04))
       :do
         (format T "Signature: ~d/~d - "
                 (aref (midifile::input-stream-buffer mf) 3)
                 (expt 2 (aref (midifile::input-stream-buffer mf) 4)))
       ;;
         (format T "Clocks per tick: ~d - Number of 1/32 per 24 clocks: ~d~%"
                 (aref (midifile::input-stream-buffer mf) 5)
                 (aref (midifile::input-stream-buffer mf) 6)))))

(defun get-midi-tempo (filename &optional (track-number 0))
  "returns a list of seconds per quarter note, one per tempo change"
  (declare (string filename))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (loop
       :for ev = (midifile:read-event mf)
       :while ev
       :when (and (midifile::meta-event-p mf)
                  ;; Check if it is a "Set Tempo" event
                  ;; https://www.csie.ntu.edu.tw/~r92092/ref/midi/#timesig
                  (= (midifile:message-data1 mf) #x51)
                  (= (midifile:message-data2 mf) #x03))
       :collect
         (/ (midifile::microseconds-per-quarter-note mf)
            1e+6))))
