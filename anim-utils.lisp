;;;; anim-utils.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:anim-utils)

(defstruct transition-value
  (current 0.0d0 :type double-float)
  (lower 1 :type (unsigned-byte 32))
  (upper 2 :type (unsigned-byte 32))
  (scale 1.0d0 :type double-float))

(defstruct animated-var
  (val 0.0d0 :type double-float)
  (buckets '() :type list)
  (offset 0.0d0 :type double-float))

(defun reset-var (var)
  (with-slots (offset) var
    (setf offset 0.0d0)))

(declaim (ftype (cl:function (animated-var
                              bordeaux-fft:complex-sample-array 
                              bordeaux-fft:complex-sample-array) double-float) step-value)
         (inline step-var))

(defun step-var (val scale left-fft-data right-fft-data)
  (declare (type animated-var val)
           (type bordeaux-fft:complex-sample-array left-fft-data right-fft-data))
  (the double-float 
       (with-slots (offset buckets) val
         (incf offset
               (loop for idx in buckets
                  summing (if (< idx 0)
                              (aref left-fft-data (- idx))
                              (aref right-fft-data idx))
                  into total
                  finally (return (* scale (abs total))))))))

(defun var-val (val)
  (with-slots (offset val) val
    (+ offset val)))

(defun vv (val)
  (+ (animated-var-offset val) (animated-var-val val)))

(defgeneric deep-copy (object))

(defmethod deep-copy ((object transition-value))
  (make-transition-value :current (transition-value-current object)
                         :lower (transition-value-lower object)
                         :upper (transition-value-upper object)
                         :scale (transition-value-scale object)))

(defmethod deep-copy ((object animated-var))
  (make-animated-var :val (animated-var-val object)
                     :buckets (copy-list (animated-var-buckets object))
                     :offset (animated-var-offset object)))

(defun gv (tv)
  (transition-value-current tv))

(defun transition-value-advance-value (trans fft-data)
  (with-slots (current lower upper scale) trans
  (incf current
        (loop for idx from lower below upper
           summing (aref fft-data idx) into total
           finally (return (* scale (/ (abs total) (- upper lower))))))))

(defun interpolate (a b cur-step steps &optional (looping nil))
  "Linearly interpolate between a and b over a number of steps.
   If looping is t, interpolates between a and b when cur-step is less than 
   steps/2, and between b and a when cur-step is greater than steps/2."
  (if (not looping)
      (let ((da (/ (- b a) steps)))
        (+ a (* da cur-step)))
      (if (< cur-step (/ steps 2))
          (let ((da (/ (- b a) (/ steps 2))))
            (+ a (* da cur-step)))
          (let ((da (/ (- a b) (/ steps 2))))
            (+ b (* da (- cur-step (/ steps 2)) ))))))

(defstruct mp3-file
  (left-channel)
  (right-channel)
  (samples)
  (sample-rate 44100 :type (unsigned-byte 32))
  (channels 2 :type (unsigned-byte 32))
  (mpg123-type 208 :type (unsigned-byte 32)))

(defun mp3-file-duration-in-seconds (mp3)
  "Compute the duration of an mp3-file in seconds."
  (with-slots (samples channels sample-rate) mp3
    (/ (length samples) 
       (* channels sample-rate))))

(defun read-mp3-file (fname)
  "Read the specified mp3 file into an mp3-file structure."
  (multiple-value-bind
        (samples sample-rate channels mt)
      (mpg123:decode-mp3-file fname :character-encoding :utf-8)
    
    (let* ((samples-per-channel (/ (length samples) channels))
           (left-channel (make-array samples-per-channel
                                     :element-type 'bordeaux-fft:complex-sample
                                     :initial-element (coerce 0.0 'bordeaux-fft:complex-sample)))
           (right-channel (make-array samples-per-channel
                                      :element-type 'bordeaux-fft:complex-sample
                                      :initial-element (coerce 0.0 'bordeaux-fft:complex-sample))))
      (loop for i below samples-per-channel
         do
           (let ((left-raw (/ (aref samples (* 2 i)) 32768.0))
                 (right-raw (/ (aref samples (+ 1 (* 2 i))) 32768.0)))
             
             (setf (aref left-channel i)
                   (coerce left-raw 'bordeaux-fft:complex-sample))
             (setf (aref right-channel i)
                   (coerce right-raw 'bordeaux-fft:complex-sample))))
      (make-mp3-file :samples samples
                     :left-channel left-channel
                     :right-channel right-channel
                     :sample-rate sample-rate
                     :channels channels
                     :mpg123-type mt))))

(defun make-movie (&key directory file-name
                     (mp3-name nil)
                     (bit-rate (* 24 1024 1024))
                     (temp-name "tmpmovie.mpg")
                     (file-template "frame%08d")
                     (image-type "png")
                     (remove-temp t))
  "Run ffmpeg to create a movie with audio."
  (if (probe-file temp-name)
      (delete-file temp-name))

  (let ((movie-command
         (format nil 
                 "ffmpeg -r 30 -i \"~a~a.~a\" -b ~a -q 4 \"~a~a\""
                 directory file-template image-type bit-rate directory temp-name))
        (audio-command
         (format nil
                 "ffmpeg -i \"~a~a\" -i \"~a~a\" -codec copy -shortest \"~a\""
                 directory temp-name mp3-name directory file-name))
        (mv-command
         (format nil "mv \"~a~a\" \"~a~a\"" directory temp-name directory file-name)))
    
    (format t "~a~%" movie-command)
    (uiop:run-program movie-command)

    (if (probe-file file-name)
        (delete-file file-name))

    (if mp3-name
        (progn 
          (format t "~a~%" audio-command)
          (uiop:run-program audio-command))
        (progn
          (format t "~a~%" mv-command)
          (uiop:run-program mv-command)))

    (if (and remove-temp (probe-file file-name))
        (delete-file temp-name))))
