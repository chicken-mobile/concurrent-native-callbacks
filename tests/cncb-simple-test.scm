(use concurrent-native-callbacks lolevel srfi-18)

(define-concurrent-native-callback (foo (int x))
  (print "foo: " x))

(define-synchronous-concurrent-native-callback (bar (int t) (int x)) int
  (let ((r (* x x)))
    (print "bar(" t "): " x " -> " r)
    r))

(thread-start!
 (lambda ()
   (print "starting spin ...")
   (do () (#f) 
     (print* "_")
     (thread-sleep! 0.25))))

(print "dispatching...")
(dispatch)
(print "done")
(return-to-host)
