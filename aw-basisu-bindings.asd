;; Generated by :claw at 2021-12-19T20:45:38.812121Z
(asdf:defsystem #:aw-basisu-bindings
  :description "Bindings generated by aw-basisu"
  :author "CLAW"
  :license "Public domain"
  :defsystem-depends-on (:trivial-features)
  :depends-on (:uiop :cffi :iffi :claw-utils)
  :components
  ((:file "bindings/x86_64-pc-linux-gnu" :if-feature
    (:and :x86-64 :linux))
   (:file "bindings/aarch64-linux-android" :if-feature
    (:and :aarch64 :android))
   (:file "bindings/x86_64-w64-mingw32" :if-feature
    (:and :x86-64 :windows))))
#-(:or (:and :x86-64 :windows)(:and :aarch64 :android)(:and :x86-64 :linux))
(warn "Current platform unrecognized or unsupported by aw-basisu-bindings system")