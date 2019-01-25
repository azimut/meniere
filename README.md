# About meniere

Collection of [incudine](http://incudine.sourceforge.net/) dsps and helpers. Depends on [CM](https://github.com/ormf/cm/).

## cm.lisp

Common Music helpers that provide constructors for patterns that mimic the `(make-*)` that Common Music 3 has.

```
> (next (make-cycle '(c d e)) 6)
(C D E C D E)
> (next (make-weighting '(c (d .1))) 6)
(C C C C D C)
> (next (make-line '(1 2 3 4)) 6)
(1 2 3 4 4 4)
> (next (make-graph `((c 3) (g 3) (e ,(make-cycle '(1 2))))) 6)
(C E C E G E)
```

## buffers.lisp
Helpers to handle incudine buffer objects. They are all build around caching and reusing buffers when possible. Default buffer dsp! has some filters build-in.

```lisp
(bbufer-load "/home/user/song.wav")
(bbplay "song.wav" :lpf 200 :pan .1)
```

You can manually create subsets of a buffer, to let's say play a `word` from dialogue sample.

```lisp
(bbufer-load "/home/user/speech.wav")
(put-phrase "welcome" "speech.wav" 1 2)
(put-phrase "change" "speech.wav" 9 4)
(word-play "welcome")
```

You can make an instrument which will interpolate the note filling the blanks for notes missing.

```lisp
(make-instrument 'drum "/home/sendai/Downloads/sample/OH/")
(push-note 'drum 32 "kick_OH_F_9.wav")
(play-instrument 'drum 39)
```

## fluidsynth.lisp
Shortcuts to control and use fluidsynth with incudine. Including `(p)` and `(pa)`, helpers to play a note/chord or arpeggio, useful on a temporal recursion scenario.

### Examples
```
> (fload "/home/user/myfont.sf2")
> (p (now) :c4 60 1 0)
> (p (now) '(60 63 67) 60 1 0)
> (pa (now) '(60 58) 60 1 4 '(0 .25))
> (pa (now) 60 60 1 4 '(0 .25))
```

## midifile.lisp

Helpers to retrieve midi `notes` and `rhythms` from midi files using incudine's midifile package.

### General Midi Info

#### `Midi Tempo`
As with partitures, there is a tempo on .mid files. And the tempo can change a lot in the score. So far I haven't seen it change the time returned though.

#### `Midi Tracks`
Starting from 0 there can be many tracks. Either one per stave or more.

#### `Midi Events`
List of midi events in hexadecimal (from [www.onicos.com](http://www.onicos.com/staff/iz/formats/midi-event.html))

```
144 - 0x90 - Chan 1 Note on
176 - 0xB0 - Chan 1 Control mode change
192 - 0xC0 - Chan 1 Program change
255 - 0xFF - Sys real time sys reset
```

#### `Midi Notes`
There are no explicit silence/rests on the midi format

### About (group-by-measure-pair)

In order to use the (group-measures-pair) function you need to know
the midi song bar length. Ex: 4/4 or 6/8.  Then provide the
MEASURE-LENGTH. It varies depending the UNIT you use.

#### `Example`
```lisp
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
