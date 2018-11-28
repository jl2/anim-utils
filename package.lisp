;;;; package.lisp

(defpackage #:anim-utils
  (:use #:cl #:j-utils)
  (:export 
   #:interpolate

   #:make-transition-value
   #:transition-value-current
   #:transition-value-lower
   #:transition-value-upper
   #:transition-value-scale
   #:transition-value-advance-value
   #:gv
   #:deep-copy

   #:make-movie

   #:read-mp3-file

   #:mp3-file-left-channel
   #:mp3-file-right-channel
   #:mp3-file-samples
   #:mp3-file-sample-rate
   #:mp3-file-channels
   #:mp3-file-mpg123-type
   #:mp3-file-duration-in-seconds

   #:animated-var
   #:reset-var
   #:step-var
   #:make-animated-var
   #:var-val
   #:vv
   #:val
   #:buckets
   #:offset
   #:animated-var-val
   #:animated-var-buckets
   #:animated-var-offset
   ))

