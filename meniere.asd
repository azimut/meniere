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
               #:cl-ppcre
               #:alexandria
               #:cm)
  :components ((:file "package")
               (:file "meniere")
               (:file "buffers")
               (:file "midifile")))

(asdf:defsystem #:meniere/dsp
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "Specify license here"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere)
  :components ((:file "dsp")))

(asdf:defsystem #:meniere/pluck
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "Specify license here"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere)
  :components ((:file "pluck")))

(asdf:defsystem #:meniere/csound
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "Specify license here"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere
               #:usocket)
  :components ((:file "csound-udp")))

(asdf:defsystem #:meniere/fluidsynth
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "Specify license here"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere
               #:incudine-fluidsynth)
  :components ((:file "fluidsynth")))
