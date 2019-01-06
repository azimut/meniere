## About meniere

Collection of [incudine](http://incudine.sourceforge.net/) dsps and helpers.

## midifile.lisp

### General Midi Info

#### `Midi Tempo`
As with partitures, there is a tempo on .mid files. And the tempo can change a lot in the score.
So far I haven't seen it change the time returned though.

#### `Midi Tracks`
Starting from 0 there can be many tracks. Either one per stave or more.

#### `Midi Events`
List of midi events in hexadecimal (see http://www.onicos.com/staff/iz/formats/midi-event.html)
144 - 0x90 - Chan 1 Note on
176 - 0xB0 - Chan 1 Control mode change
192 - 0xC0 - Chan 1 Program change
255 - 0xFF - Sys real time sys reset

#### `Midi Notes`
There are no explicit silence/rests on the midi format

### About (group-by-measure-pair)

In order to use the (group-measures-pair) function you need to know
the midi song bar length. Ex: 4/4 or 6/8.  Then provide the
MEASURE-LENGTH. It varies depending the UNIT you use.

#### Example
```
(defparameter *notes* nil)
(setf *notes* (subseq (get-measures-pair *mf* 10 2 1) 6))
(let ((measures (make-heap *notes*)))
  (defun f (time)
    (let* ((measure (next measures))
           (notes (first measure))
           (durations (second measure)))
      (play-midi-arp time notes 50 durations 0 (d2o durations)))
    (aat (+ time #[2 b]) #'f it)))
```
