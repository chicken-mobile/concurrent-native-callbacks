(import shell make)

(make (("test1" 
	("simple-test.c"
	 "cncb-simple-test.scm")
	(run (csc
	      simple-test.c -o test1
	      cncb-simple-test.scm -e
	      -L -lpthread)))
       ("test2" ("hammering.c" 
		 "cncb-simple-test.scm")
	(run (csc
	      hammering.c -o test2
	      cncb-simple-test.scm -e
	      -L -lpthread))))
  '("test1" "test2"))

(run (./test1))
(run (./test2 10 10))
