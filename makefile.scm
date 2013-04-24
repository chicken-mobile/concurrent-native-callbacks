(use make shell matchable files)

(shell-verbose #t)

(define (build args)
  (make (("concurrent-native-callbacks.so" 
	  ("cncb.scm"
	   "cncb-module.scm"
	   "twiddle.c"
	   "concurrent-native-callbacks-compile-time.import.so")
	  (run (csc -sJ -O2 cncb-module.scm
		    -C -g -k			;XXX
		    -C -Wno-unused-result
		    -o concurrent-native-callbacks.so)) )
	 ("concurrent-native-callbacks.import.so"
	  ("concurrent-native-callbacks.so")
	  (run (csc -s -O2 -d0 concurrent-native-callbacks.import.scm)))
	 ("concurrent-native-callbacks-compile-time.so"
	  ("cncb-compile-time.scm" 
	   "cncb-compile-time-module.scm")
	  (run (csc -sJ -O2 cncb-compile-time-module.scm
		    -o concurrent-native-callbacks-compile-time.so)))
	 ("concurrent-native-callbacks-compile-time.import.so"
	  ("concurrent-native-callbacks-compile-time.so")
	  (run (csc -s -O2 -d0
		    concurrent-native-callbacks-compile-time.import.scm)))
	 ("test1" 
	  ("simple-test.c"
	   "cncb-simple-test.scm"
	   "concurrent-native-callbacks.import.so")
	  (run (csc -k -C -g simple-test.c -o test1
		    -C -Wno-unused-result ;-C "-O0"
		    cncb-simple-test.scm -e
		    -L$HOME/lib -lchicken -lpthread)))
	 ("test2" ("hammering.c" 
		   "cncb-simple-test.scm"
		   "concurrent-native-callbacks.import.so")
	  (run (csc -k -C -g hammering.c -o test2
		    -C -Wno-unused-result ;-C -O0
		    cncb-simple-test.scm -e
		    -L$HOME/lib -lchicken -lpthread))))
    args))

(for-each
 (match-lambda
   ("clean"
    (run (rm -f *.so *.import.* test1 test2 a.out *.o)))
   ("all"
    (build
     '("concurrent-native-callbacks.so"
       "concurrent-native-callbacks.import.so"
       "concurrent-native-callbacks-compile-time.so"
       "concurrent-native-callbacks-compile-time.import.so"
       "test1"
       "test2")))
   (arg (build (list arg))))
 (let ((args (command-line-arguments)))
   (if (null? args)
       (list "all")
       args)))
