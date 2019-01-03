;;;; meniere.asd

(asdf:defsystem #:meniere
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "Specify license here"
  :version "0.0.2"
  :serial t
  :depends-on (#:swank
               #:incudine
               #:alexandria)
  :components ((:file "package")
               (:file "pluck")
               (:file "meniere")
               (:file "midifile")))
