(use concurrent-native-callbacks lolevel srfi-18)

(define-concurrent-native-callback (foo (int x)) default 
  (print "foo: " x))

(define-synchronous-concurrent-native-callback 
  (bar (int x) (int r)) default 
  (print "bar: " x)
  (* x x))

(print "started")
(thread-sleep! 1000000)
(print "timeout")
(return-to-host)			; should not execute
