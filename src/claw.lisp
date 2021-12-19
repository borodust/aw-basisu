(claw:defwrapper (:aw-basisu
                  (:system :aw-basisu/wrapper)
                  (:headers "basisu_transcoder.h")
                  (:includes :basisu-includes)
                  (:include-definitions "^BASISD_.*"
                                        "^basist::basis_.*"
                                        "^basist::basisu_.*"
                                        "^basist::ktx2_*"

                                        "etc1_global_selector_codebook"
                                        "g_global_selector"

                                        "get_qwords_per_block"
                                        "get_block_width"
                                        "get_block_height"
                                        "get_bytes_per_block")
                  (:exclude-definitions "^std::basic_string"
                                        "^std::__")
                  (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                            ((:and :aarch64 :android) "aarch64-linux-android")
                            ((:and :x86-64 :windows) "x86_64-w64-mingw32"))
                  (:persistent t :depends-on (:claw-utils))
                  (:language :c++))
  :in-package :%basisu
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :with-adapter (:static
                 :path "src/lib/adapter/adapter.cxx")
  :override-types ((:string claw-utils:claw-string)
                   (:pointer claw-utils:claw-pointer))
  :symbolicate-names (:in-pipeline
                      (:by-removing-prefixes "basist::"
                                             "basisu::"
                                             "BASISD_")
                      (:by-removing-prefixes "basisu_" "basis_" "cTF")))
