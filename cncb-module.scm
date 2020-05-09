(module concurrent-native-callbacks (dispatcher
				     dispatcher?
				     dispatcher-id
				     dispatcher-thread
				     dispatcher-input-fileno
				     dispatcher-output-fileno
				     dispatch
				     dispatcher-add!
				     dispatcher-terminate!
				     define-concurrent-native-callback
				     define-synchronous-concurrent-native-callback)
				     
  (import scheme (chicken base))
  (include "cncb.scm") )
