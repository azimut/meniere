;;;; package.lisp

(uiop:define-package #:meniere
    (:use #:cl
          #:incudine
          #:incudine.vug
          #:incudine.util)
  (:export #:dsp-square
           #:dsp-pulse
           #:dsp-pluck
           #:prophet
           #:green
           #:keen
           #:bass
           #:ixi-kick
           #:ixi-snare))