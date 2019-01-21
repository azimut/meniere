;;;; package.lisp

(uiop:define-package #:meniere
  (:use #:cl
        #:incudine
        #:incudine.vug
        #:incudine.util)
  (:import-from #:alexandria
                #:appendf
                #:last-elt
                #:ensure-list
                #:extremum)
  (:export #:dsp-square
           #:dsp-pulse
           #:dsp-prophet
           #:dsp-green
           #:dsp-keen
           #:dsp-bass
           #:dsp-ixi-kick
           #:dsp-ixi-snare
           ;;
           #:dsp-pluck
           ;;
           #:p
           #:pa
           #:off-with-the-beats
           #:fpress
           #:fsens
           #:fpitch
           #:fchorus-toggle
           #:fchorus
           #:freverb-toggle
           #:freverb
           #:freverb-preset
           #:fp
           #:all-piano
           #:try-sounds
           #:fg
           #:fpan
           #:all-pan
           #:fstart
           #:fload
           #:*synth*
           #:*settings*
           ;;
           #:get-notes
           #:get-notes-durations
           #:get-notes-durations-chords
           #:get-notes-chords
           #:get-notes-durations-chords-silences
           #:group-by-measure
           #:get-measures-pair))
