(in-package #:meniere)
;;
;; Description: Loaders and players for foxdot sounds
;;

(defvar *fx-samples* (make-hash-table)
  "Hash with array of buffers.")
(defvar *fx-path* "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/"
  "Directory where to search for FoxDot samples, overwrite as needed.")

;; lib/Buffers.py
(defvar *fx-non-alpha-sounds*
  '(("semicolon"    . ":")
    ("ampersand"    . "&")
    ("asterix"      . "*")
    ("at"           . "@")
    ("bar"          . "|")
    ("caret"        . "^")
    ("colon"        . ":")
    ("dollar"       . "$")
    ("equals"       . "=")
    ("exclamation"  . "!")
    ("forwardslash" . "/")
    ("greaterthan"  . ">")
    ("hash"         . "#")
    ("hyphen"       . "-")
    ("lessthan"     . "<")
    ("percent"      . "%")
    ("plus"         . "+")
    ("question"     . "?")
    ("tilde"        . "~")
    ("backslash"    . "\\")
    ("1"            . "1")
    ("2"            . "2")
    ("3"            . "3")
    ("4"            . "4"))
  "alist of sample folder name to symbol string used in fx-play")

(defun fx-guess-path (path)
  (if (uiop:absolute-pathname-p path)
      path
      (concatenate 'string *fx-path* path)))

(defun fx-clear (&optional global-clear)
  "NOTE: to free buffers from memory use (clean-buffers)"
  (clrhash *fx-samples*)
  (when global-clear
    (clean-buffers))
  t)

(defun fx-load (path key)
  "loads sounds in PATH into the *FX-SAMPLES* hash cache under KEY
   VALUE is and extendable array that keeps each sample for KEY
   PATH can be a .wav file or a directory"
  (let ((resolved-path (fx-guess-path path))
        (key (if (stringp key) (char key 0) key)))
    (cond ((uiop:directory-exists-p resolved-path)
           (remhash key *fx-samples*)
           (loop :for file :in (uiop:directory-files ;; ensure trailing slash
                                (uiop:directory-exists-p resolved-path))
                 :do (fx-load file key)))
          ((uiop:file-exists-p resolved-path)
           (let ((buffer (bbuffer-load resolved-path)))
             (alexandria:if-let ((buffers (gethash key *fx-samples*)))
               (vector-push-extend buffer buffers)
               (let ((init (make-array 1 :initial-contents `(,buffer)
                                         :adjustable T :fill-pointer 1)))
                 (setf (gethash key *fx-samples*) init)))))
          (t (error "PATH provides not valid...")))))

(defun fx-load-all ()
  "loads all sounds in *FX-PATH* into *FX-SAMPLES*"
  (loop :for dir :in (uiop:subdirectories *fx-path*)
        :for letter := (alexandria:lastcar (pathname-directory dir))
        :if (str:alphap letter)
        :do (fx-load (merge-pathnames dir "lower") letter)
            (fx-load (merge-pathnames dir "upper") (str:upcase letter)))
  (loop :for dir :in (uiop:subdirectories (merge-pathnames *fx-path* "_"))
        :for letter := (alexandria:lastcar (pathname-directory dir))
        :for letter-symbol := (cdr (assoc letter *fx-non-alpha-sounds* :test #'equal))
        :do (fx-load dir letter-symbol)))

(defun resolve-patterns-in-list (list)
  (declare (type list list))
  (if (some #'cm:pattern? list)
      (resolve-patterns-in-list (mapcar #'cm:next list))
      list))

(defun fx-play (key &rest rest &key (sample 0) (dur 1) &allow-other-keys)
  (etypecase key
    (cm::pattern
      (apply #'fx-play (cm:next key) rest)) ;; when this happens?
    (string
     (apply #'fx-play (char key 0) rest))
    (character
     (alexandria:when-let* ((buffers   (gethash key *fx-samples*))
                            (n-buffers (fill-pointer buffers))
                            (buffer    (aref buffers (mod sample n-buffers))))
       (remf rest :sample)
       (remf rest :dur)
       (apply #'bbplay buffer rest)))
    (list
     (alexandria:when-let* ((_        key)
                            (sounds   (resolve-patterns-in-list key))
                            (n-sounds (length sounds))
                            (offsets  (loop :repeat n-sounds
                                            :for i :from 0d0 :by (/ (coerce dur 'double-float)
                                                                    n-sounds)
                                            :summing i :into total
                                            :collect total)))
       (mapcar (lambda (n o)
                 (declare (type double-float o))
                 (apply #'at
                        (+ (now) (calc-beats o))
                        #'fx-play
                        n
                        rest))
               key
               offsets)))))
