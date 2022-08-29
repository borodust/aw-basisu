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

(defun init-basisu ()
  (unless *basisu-initialized*
    (cffi:use-foreign-library aw-basisu-blob)
    (%basisu:transcoder-init)
    (setf *basisu-initialized* t)))


(defmacro with-transcoder ((transcoder-var file-path) &body body)
  (alexandria:with-gensyms (data data-ptr encoded)
    `(iffi:with-intricate-instance
         (,transcoder-var %basisu:ktx2-transcoder)
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


(defun transcoding-level-count (transcoder)
  (%basisu:get-levels
   :const
   '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder))


(defun transcoding-layer-count (transcoder)
  (max 1 (%basisu:get-layers
          :const
          '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder)))


(defun transcoding-face-count (transcoder)
  (%basisu:get-faces
   :const
   '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder))


(defun make-image-level-info ()
  (iffi:make-intricate-instance '%basisu::ktx2-image-level-info))


(defun destroy-image-level-info (image-level-info)
  (iffi:destroy-intricate-instance '%basisu::ktx2-image-level-info image-level-info))


(defun update-transcoding-level-info (transcoder image-level-info level-idx layer-idx face-idx)
  (%basisu::get-image-level-info
   :const
   '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder
   '(claw-utils:claw-pointer %basisu::ktx2-image-level-info) image-level-info
   '%basisu::uint32-t level-idx
   '%basisu::uint32-t layer-idx
   '%basisu::uint32-t face-idx))


(defmacro do-image-levels ((level-info-var transcoder) &body body)
  (alexandria:once-only (transcoder)
    (alexandria:with-gensyms (level-idx layer-idx face-idx)
      `(let ((,level-info-var (make-image-level-info)))
         (unwind-protect
              (loop for ,level-idx from 0 below (transcoding-level-count ,transcoder)
                    do (loop for ,layer-idx from 0 below (transcoding-layer-count ,transcoder)
                             do (loop for ,face-idx from 0 below (transcoding-face-count ,transcoder)
                                      do (progn
                                           (update-transcoding-level-info ,transcoder
                                                                          ,level-info-var
                                                                          ,level-idx
                                                                          ,layer-idx
                                                                          ,face-idx)
                                           ,@body))))
           (destroy-image-level-info ,level-info-var))))))


(defun image-level-index (level-info)
  (iffi:intricate-slot-value level-info
                             '%basisu::ktx2-image-level-info
                             '%basisu:m-level-index))


(defun image-level-layer-index (level-info)
  (iffi:intricate-slot-value level-info
                             '%basisu::ktx2-image-level-info
                             '%basisu:m-layer-index))


(defun image-level-face-index (level-info)
  (iffi:intricate-slot-value level-info
                             '%basisu::ktx2-image-level-info
                             '%basisu:m-face-index))


(defun image-level-width (level-info)
  (iffi:intricate-slot-value level-info
                             '%basisu::ktx2-image-level-info
                             '%basisu:m-width))


(defun image-level-height (level-info)
  (iffi:intricate-slot-value level-info
                             '%basisu::ktx2-image-level-info
                             '%basisu:m-height))


(defstruct (compressed-image
            (:constructor %make-compressed-image))
  internal-format
  width
  height
  block-width
  block-height
  y-blocks
  x-blocks
  qwords-per-block
  data)


(defun make-compressed-image (format width height)
  (let* ((format (%basisu:get-basisu-texture-format
                  '%basisu::transcoder-texture-format format))
         (block-width (%basisu:get-block-width
                       '%basisu::texture-format format))
         (block-height (%basisu:get-block-height
                        '%basisu::texture-format format))
         (qwords-per-block (%basisu:get-qwords-per-block
                            '%basisu::texture-format format))
         (x-blocks (floor (/ (+ width block-width -1) block-width)))
         (y-blocks (floor (/ (+ height block-height -1) block-height))))
    (%make-compressed-image
     :internal-format format
     :width width
     :height height
     :block-width block-width
     :block-height block-height
     :qwords-per-block qwords-per-block
     :x-blocks x-blocks
     :y-blocks y-blocks
     :data (cffi:make-shareable-byte-vector (* x-blocks y-blocks
                                               (* qwords-per-block 8))))))

(defun compressed-image-block-count (compressed-image)
  (* (compressed-image-y-blocks compressed-image)
     (compressed-image-x-blocks compressed-image)))


(defun transcode-image (transcoder image-level format)
  (let ((compressed-image (make-compressed-image format
                                                 (image-level-width image-level)
                                                 (image-level-height image-level))))
    (cffi:with-pointer-to-vector-data (data-ptr (compressed-image-data compressed-image))
      (%basisu::transcode-image-level
       '(claw-utils:claw-pointer %basisu::ktx2-transcoder) transcoder
       '%basisu::uint32-t (image-level-index image-level)
       '%basisu::uint32-t (image-level-layer-index image-level)
       '%basisu::uint32-t (image-level-face-index image-level)
       '(claw-utils:claw-pointer :void) data-ptr
       '%basisu::uint32-t (compressed-image-block-count compressed-image)
       '%basisu::transcoder-texture-format format
       '%basisu::uint32-t 0             ; decode flags
       '%basisu::uint32-t 0
       '%basisu::uint32-t 0
       :int -1
       :int -1
       '(claw-utils:claw-pointer %basisu::ktx2-transcoder-state) (cffi:null-pointer)))
    compressed-image))


(defun run ()
  (init-basisu)
  (with-transcoder (transcoder (asdf:system-relative-pathname
                                :aw-basisu/example
                                "example/brick_1.ktx2"))
    (start-transcoding transcoder)
    (format t "~&Format: ~A"
            (transcoding-format transcoder))

    (let (compressed-images)
      (do-image-levels (level transcoder)
        (format t "~&[~{~A~^,~}] = Width: ~A, Height: ~A"
                (list (image-level-index level)
                      (image-level-layer-index level)
                      (image-level-face-index level))
                (image-level-width level)
                (image-level-height level))
        (push (transcode-image transcoder level :etc2-rgba) compressed-images))
      compressed-images)))
