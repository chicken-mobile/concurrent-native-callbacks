(module concurrent-native-callbacks-compile-time
    (cncb-transformer
     generate-entry-point
     unstash-and-execute
     foreign-type-declaration)
  (import scheme 
          (chicken base)
          (chicken string)
          (chicken syntax)
          (chicken format)
          (chicken port))
  (include "cncb-compile-time.scm"))
