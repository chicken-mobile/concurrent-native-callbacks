(module concurrent-native-callbacks-compile-time
    (cncb-transformer
     generate-entry-point
     unstash-and-execute
     foreign-type-declaration)
  (import (scheme) (chicken base))
  (include "cncb-compile-time.scm"))
