[![Build Status](https://travis-ci.org/azimut/meniere.svg?branch=master)](https://travis-ci.org/azimut/meniere)

# About meniere

Collection of [incudine](http://incudine.sourceforge.net/) dsps and helpers. Depends on [CM](https://github.com/ormf/cm/).

## csound-udp.lisp

Basic helpers to connect and trigger events against a started instance of csound listening for commands on [UDP](https://csound.com/docs/manual/udpserver.html)/10000. More in particular it provides helpers for [csound-live-code](https://github.com/kunstmusik/csound-live-code/).

```
$ csound livecode.csd
```

```
> (ql:quickload :meniere/csound)
> (in-package :meniere)
> (csound-socket-connect)
> (csound-chnset 0 "Bass.pan")
> (clc "Bass" 60 30 1)
```

## csound-live-code.lisp

Depends on [numcl](https://github.com/numcl/numcl) and his deps [constantfold](https://github.com/numcl/constantfold), [specialized-function](https://github.com/numcl/specialized-function), [gtype](https://github.com/numcl/gtype) and [trivia](https://github.com/guicho271828/trivia/) from github at the time of writting this

Tries to re-implement some of the logic of [csound-live-code](https://github.com/kunstmusik/csound-live-code/) in lisp/incudine.

It provides a parser to do some hexabeat "math".
```
> (hexpitch "x93 + 2 ~ xadf>>2")
#<cycle {1008DE9BC3}>
#(3 2 2 3 2 2 3 3 1 1 1 0 1 0 1 1 0 1 1 1)
(CONCATENATE 'VECTOR (ADD #*10010011 2) (ROLL #*101011011111 (- 2)))
```
You can use said cycle like this:
```lisp
(let ((p1 (hexpitch "xd11c")))
         (defun f (time)
           (when (next p1)
             (p time 60 60 1 0))
           (aat (+ time #[1 b]) #'f it)))
```
Or use `(hexplay)` that is a macro that uses `(hexpitchc)` that caches on a hash the cycle created. You can add spaces to use the same beat twice without reuse them.
```lisp
(defun f (time)
  (hexplay "xd11c"
    (p time 60 60 1 0))
  (aat (+ time #[1 b]) #'f it))
```

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

Shortcuts to control and use incudine-fluidsynth.

Includes `(p)` and `(pa)`, helpers to play a note/chord or arpeggio, useful on a temporal recursion scenario.

```
> (fload "/home/user/myfont.sf2")
> (p (now) :c4 60 1 0)
> (p (now) '(60 63 67) 60 1 0)
> (pa (now) '(60 58) 60 1 4 '(0 .25))
> (pa (now) 60 60 1 4 '(0 .25))
```

## foxdot-grammar.lisp & foxdot-play.lisp

These helpers give you a fast way to load foxdot samples and playthem as it's `play()` function does.

Use `(fx-load-all)` to load all the samples from `*FX-PATH*`. Use `fx-pats` to return a list from the pattern passed. Use `fx-play` to parse the pattern string returned by it.

Example:
```
(setf (bpm *tempo*) 120)

(destructuring-bind (p1 p2 p3 p4)
    (fx-pats "<x X ><[--] ><(_v)__><  o >")
  (defun d1 (time)
    (fx-play (cm:next p1) :amp .1)
    (fx-play (cm:next p2) :amp .1)
    (fx-play (cm:next p3) :amp .2)
    (fx-play (cm:next p4) :amp .1 :downsamp 8 :rate 2.5 :lpf 200)
    (aat (+ time #[1 b]) #'d1 it)))

(aat (tempo-sync #[1 b]) #'d1 it)
(defun p1 ())
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
128 - 0x80 - Chan 1 Note off
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
