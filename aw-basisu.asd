(asdf:defsystem :aw-basisu
  :description "Bindings to Basis Universal transcoder library"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:aw-basisu-bindings))


(asdf:defsystem :aw-basisu/wrapper
  :description "Wrapper over Basis Universal transcoder library"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :cffi :claw :claw-utils)
  :serial t
  :components ((:file "src/claw")
               (:module :basisu-includes :pathname "src/lib/basisu/transcoder/")))


(asdf:defsystem :aw-basisu/example
  :description "AW-BASISU example"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :cffi :iffi :aw-basisu)
  :serial t
  :components ((:file "example/example")))
