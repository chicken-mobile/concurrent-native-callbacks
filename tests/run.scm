(import shell make)

(make (("test1" 
	("simple-test.c"
	 "cncb-simple-test.scm")
	(run (csc			-k -C -g
	      simple-test.c -o test1
	      cncb-simple-test.scm -e
	      -lpthread)))
       ("test2" ("hammering.c" 
		 "cncb-simple-test.scm")
	(run (csc			-k -C -g
	      hammering.c -o test2
	      cncb-simple-test.scm -e
	      -lpthread))))
  '("test1" "test2"))

(run (./test1))
(run (./test2 10 10))
