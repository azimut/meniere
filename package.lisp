;;;; package.lisp

(uiop:define-package #:meniere
    (:use #:cl
          #:incudine
          #:incudine.vug
          #:incudine.util)
  (:import-from #:alexandria
                #:appendf
                #:ensure-list
                #:extremum)
  (:export #:dsp-square
           #:dsp-pulse
           #:dsp-pluck
           #:prophet
           #:green
           #:keen
           #:bass
           #:ixi-kick
           #:ixi-snare
           #:get-notes
           #:get-notes-durations
           #:get-notes-durations-chords
           #:get-notes-chords
           #:get-notes-durations-chords-silences
           #:group-by-measure
           #:get-measures-pair))
