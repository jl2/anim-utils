;;;; mp3-utils.asd

(asdf:defsystem #:anim-utils
  :description "Describe mp3-utils here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:j-utils
               #:mpg123-ffi
               #:bordeaux-fft)
  :serial t
  :components ((:file "package")
               (:file "anim-utils")))

