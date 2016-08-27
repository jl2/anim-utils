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
       (incf (animated-var-offset val)
             (loop for idx in (animated-var-buckets val)
                summing (if (< idx 0)
                            (aref left-fft-data (- idx))
                            (aref right-fft-data idx))
                into total
                finally (return (* scale (abs total)))))))

(defgeneric deep-copy (object))

(defmethod deep-copy ((object transition-value))
  (make-transition-value :current (transition-value-current object)
                         :lower (transition-value-lower object)
                         :upper (transition-value-upper object)
                         :scale (transition-value-scale object)))


(defun gv (tv)
  (transition-value-current tv))

(defun transition-value-advance-value (trans fft-data)
  (incf (transition-value-current trans)
     (loop for idx from (transition-value-lower trans) below (transition-value-upper trans)
        summing (aref fft-data idx) into total
        finally (return (* (transition-value-scale trans) (/ (abs total) (- (transition-value-upper trans) (transition-value-lower trans))))))))

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
  (/ (length (mp3-file-samples mp3)) 
     (* (mp3-file-channels mp3) (mp3-file-sample-rate mp3))))

(defun next-power-of-2 (num)
  (loop
     for power from 0
     for cn = num then (floor (/ cn 2))
     until (< cn 2)
     finally (return (ash 1 (+ 1 power)))))


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
                     (bit-rate (* 4 2014))
                     (temp-name "tmpmovie.mpg")
                     (image-type "png")
                     (remove-temp t))
  "Run ffmpeg to create a movie with audio."
  (if (probe-file temp-name)
      (delete-file temp-name))

  (let ((movie-command
         (format nil 
                 "ffmpeg -r 30 -i \"~aframe%05d.~a\" -b ~a -q 4 \"~a\""
                 directory image-type bit-rate temp-name))
        (audio-command
         (format nil
                 "ffmpeg -i \"~a\" -i \"~a\" -codec copy -shortest \"~a\""
                 temp-name mp3-name file-name))
        (mv-command
         (format nil "mv \"~a\" \"~a\"" temp-name file-name)))
    
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

    (if remove-temp
        (delete-file temp-name))))

(defun random-between (min-val max-val)
  (+ min-val (random (- max-val min-val))))

(defun map-val (x xmin xmax new-xmin new-xmax)
  "map a value from the range xmin,xmax to the range new-xmin,new-xmax"
  (+ (* (/ (- x xmin) (- xmax xmin)) (- new-xmax new-xmin)) new-xmin))

(defun fix-directory (directory-name)
  "Make sure directory exists and has a / at the end."
  (ensure-directories-exist
   (if (char=  #\/ (aref directory-name (- (length directory-name) 1)))
       directory-name 
       (concatenate 'string directory-name "/"))))
