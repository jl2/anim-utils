;;;; package.lisp

(defpackage #:anim-utils
  (:use #:cl)
  (:export 
   #:interpolate
   #:fix-directory

   #:make-transition-value
   #:transition-value-current
   #:transition-value-lower
   #:transition-value-upper
   #:transition-value-scale
   #:transition-value-advance-value
   #:gv
   #:map-val
   #:deep-copy

   #:make-movie

   #:random-between

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
   #:val
   #:buckets
   #:offset
   #:animated-var-val
   #:animated-var-buckets
   #:animated-var-offset
   ))

