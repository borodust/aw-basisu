(cl:defpackage :aw-basisu.example
  (:use :cl)
  (:export #:run))
(cl:in-package :aw-basisu.example)


(cffi:define-foreign-library (aw-basisu-blob
                              :search-path (asdf:system-relative-pathname
                                            :aw-basisu/example
                                            "src/lib/build/desktop/"))
  (:windows "basisu.clawed.dll")
  (:unix "libbasisu.clawed.so"))


(defvar *basisu-initialized* nil)
(defvar *etc1-global-selector-codebook* nil)

(defun init-basisu ()
  (unless *basisu-initialized*
    (cffi:use-foreign-library aw-basisu-blob)
    (%basisu:transcoder-init)

    (setf
     *etc1-global-selector-codebook*
     (iffi:make-intricate-instance
      '%basisu:etc1-global-selector-codebook
      '%basisu::uint32-t %basisu:+g-global-selector-cb-size+
      '(claw-utils:claw-pointer %basisu::uint32-t) %basisu:+g-global-selector-cb+))

    (setf *basisu-initialized* t)))


(defmacro with-transcoder ((transcoder-var file-path) &body body)
  (alexandria:with-gensyms (data data-ptr encoded)
    `(iffi:with-intricate-instance
         (,transcoder-var %basisu:ktx2-transcoder
                          '(claw-utils:claw-pointer %basisu::etc1-global-selector-codebook)
                          *etc1-global-selector-codebook*)
       (with-open-file (,encoded ,file-path
                                 :direction :input
                                 :element-type '(unsigned-byte 8))
         (let ((,data (cffi:make-shareable-byte-vector (file-length ,encoded))))
           (read-sequence ,data ,encoded)
           (cffi:with-pointer-to-vector-data (,data-ptr ,data)
             (%basisu:init
              '(claw-utils:claw-pointer %basisu::ktx2-transcoder) ,transcoder-var
              '(claw-utils:claw-pointer :void) ,data-ptr
              '%basisu::uint32-t (length ,data)))
           ,@body)))))


(defun start-transcoding (transcoder)
  (%basisu:start-transcoding
   '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder))


(defun transcoding-width (transcoder)
  (%basisu:get-width
   :const
   '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder))


(defun transcoding-height (transcoder)
  (%basisu:get-height
   :const
   '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder))


(defun transcoding-format (transcoder)
  (%basisu:get-format
   :const
   '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder))


(defun run ()
  (init-basisu)
  (with-transcoder (transcoder (asdf:system-relative-pathname
                                :aw-basisu/example
                                "example/brick_1.ktx2"))
    (start-transcoding transcoder)
    (format t "~&Format: ~A~&Width: ~A~&Height: ~A"
            (transcoding-format transcoder)
            (transcoding-width transcoder)
            (transcoding-height transcoder))))
