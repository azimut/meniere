;;;; meniere.asd

(asdf:defsystem #:meniere
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license "GPL-3.0"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:swank
               #:str
               #:incudine
               #:cl-ppcre
               #:cl-arrows
               #:alexandria
               #:cm)
  :components ((:file "package")
               (:file "meniere")
               (:file "cm")
               (:file "buffers")
               (:file "midifile")))

(asdf:defsystem #:meniere/dsp
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license "GPL-3.0"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere)
  :components ((:file "dsp")))

(asdf:defsystem #:meniere/foxdot
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "GPL-3.0"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere
               #:cl-lex
               #:verbose
               #:yacc)
  :components ((:file "foxdot-grammar")
               (:file "foxdot-play")))

(asdf:defsystem #:meniere/pluck
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "GPL-3.0"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere)
  :components ((:file "pluck")))

(asdf:defsystem #:meniere/csound-udp
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "GPL-3.0"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere
               #:usocket)
  :components ((:file "csound-udp")))

(asdf:defsystem #:meniere/fluidsynth
  :description "collection of incudine dsps"
  :author "Azimut <azimut.github@protonmail.com>"
  :license  "GPL-3.0"
  :homepage "https://github.com/azimut/meniere/"
  :version "0.0.7"
  :serial t
  :depends-on (#:meniere
               #:incudine-fluidsynth)
  :components ((:file "fluidsynth")))

(asdf:defsystem #:meniere/csound-live-code
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:meniere
               #:cl-lex
               #:yacc
               #:numcl
               #:serapeum
               #:bit-smasher)
  :components ((:file "csound-live-code")))
