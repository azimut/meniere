;;;; meniere.asd

(asdf:defsystem #:meniere
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "Specify license here"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:swank
               #:incudine
               #:alexandria
               #:cm)
  :components ((:file "package")
               (:file "pluck")
               (:file "meniere")
               (:file "fluidsynth")
               (:file "midifile")))
