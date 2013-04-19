;;;; compile-time part of cncb


(use data-structures srfi-1 matchable ports)


(define (cncb-transformer x r c sync)
  (let ((%d (r 'd))
	(%a (r 'a))
	(%begin (r 'begin))
	(%dispatcher-add! (r 'dispatcher-add!))
	(%quote (r 'quote))
	(%define (r 'define))
	(%dispatcher (r 'dispatcher))
	(%lambda (r 'lambda)))
    (match x
      ;;XXX change to "((name dispatcher-id) args ...)", id optional
      ((_ (name args ...) dispatcher-id body ...)
       `(,%begin
	 (,%define ,%d (,%dispatcher (,%quote ,dispatcher-id)))
	 ,(generate-entry-point r name %d args sync)
	 (,%dispatcher-add!
	  ,%d
	  (,%quote ,name)
	  (,%lambda
	   (,%d ,%a) 
	   ,(unstash-and-execute r args %a body sync))))))))

(define (generate-entry-point r name dispvar t/a sync)
  (let ((cvar (gensym "cvar"))
	(mutex (gensym "mutex"))
	(infd (gensym "infd"))
	(outfd (gensym "outfd"))
	(data (gensym "data"))
	(ptr (gensym "ptr"))
	(%begin (r 'begin))
	(%define-foreign-variable (r 'define-foreign-variable))
	(%dispatcher-argument-input-fileno (r 'dispatcher-argument-input-fileno))
	(%dispatcher-result-output-fileno (r 'dispatcher-result-output-fileno))
	(%foreign-declare (r 'foreign-declare)))
    ;; - by convention, in a synchronous call the last argument is a pointer to the result value
    (define (stash-arguments args)
      (let ((size (gensym "size")))
	(with-output-to-string
	  (lambda ()
	    ;; add ptr to callback-name and cvar-ptr
	    (printf "int ~a = sizeof(void *) * 2;~%char *~a, *~a;~%" 
	      size data ptr)
	    (for-each
	     (match-lambda
	       ((type arg)
		(printf "~a += sizeof ~a;~%" size arg)))
	     args)
	    ;;XXX result of malloc(3) not checked
	    (printf "~a = (char *)malloc(~a);~%~a = ~a;~%"
	      data size ptr data)
	    (printf "*((char **)~a) = \"~a\";~%~a += sizeof(char *);~%" 
	      ptr name ptr)
	    (when sync
	      (printf "*((void **)~a) = &~a;~%~a += sizeof(void *);~%" 
		ptr cvar ptr))
	    (for-each
	     (match-lambda
	       ((type arg)
		(printf "memcpy(~a, &~a, sizeof ~a);~%~a += sizeof ~a;~%"
		  ptr arg arg ptr arg)))
	     args)))))
    `(,%begin
      (,%foreign-declare
       "#include <pthread.h>\n"
       ,(conc "static int " infd "," outfd ";\n")
       ,(if sync
	    (conc
	     "pthread_cond_t " cvar " = PTHREAD_COND_INITIALIZER;\n"
	     "pthread_mutex_t " mutex " = PTHREAD_MUTEX_INITIALIZER;\n")
	    "")
       ,(conc "void " name "("
	      (string-intersperse 
	       (map (match-lambda
		      ((type arg)
		       (foreign-type-declaration type (symbol->string arg))))
		    t/a)
	       ",")
	      "){\n")
       ,(if sync
	    (conc "pthread_mutex_lock(&" mutex ");\n")
	    "")
       ,(stash-arguments t/a)
       ;; no locking, unless size of data block exceeds PIPE_BUF
       ;;XXX should check for this, even if it is unlikely
       ,(conc "printf(\"writing to fd %d\\n\", " outfd ");\n") ;XXX
       ,(conc "write(" outfd ", " data ", " ptr " - " data ");\n")
       ,(if sync
	   (conc "pthread_cond_wait(&" cvar ", &" mutex ");\n")
	   "")
       ,(conc "free(" data "); }\n"))
      (,%define-foreign-variable ,infd int)
      (,%define-foreign-variable ,outfd int)
      (set! ,outfd (,%dispatcher-result-output-fileno ,dispvar))
      (set! ,infd (,%dispatcher-argument-input-fileno ,dispvar)))))


(define (unstash-and-execute r t/a ptr body sync)
  (let ((%let (r 'let))
	(%extract_argument_ptr (r 'extract_argument_ptr))
	(%foreign-lambda* (r 'foreign-lambda*))
	(%begin (r 'begin))
	(ptrvar (gensym "pptr")))
    `(,%let ((,ptrvar (,%extract_argument_ptr ptr)))
	    (,%let
	     ,(map (match-lambda
		     ((type arg) 
		      `(,arg ((,%foreign-lambda* 
			       ,type
			       (((c-pointer (c-pointer ,type)) ptr))
			       "return(*((*ptr)++));")
			      ,ptrvar))))
		   t/a)
	     ,@(if sync
		   (let ((type (car (last t/a))))
		     `(((,%foreign-lambda* 
			 void
			 (((c-pointer (c-pointer ,type)) ptr)
			  (,type val)
			  (int index))
			 "**(ptr + index + 2) = val;")
			,ptrvar (,%begin ,@body) ,(length t/a))))
		   body)))))


;; foreign-type conversion - taken from c-backend.scm

(define (foreign-type-declaration type target)
  (let ((err (lambda () (syntax-error "illegal foreign type" type)))
	(str (lambda (ts) (string-append ts " " target))) )
    (case type
      ((scheme-object) (str "C_word"))
      ((char byte) (str "C_char"))
      ((unsigned-char unsigned-byte) (str "unsigned C_char"))
      ((unsigned-int unsigned-integer) (str "unsigned int"))
      ((unsigned-int32 unsigned-integer32) (str "C_u32"))
      ((int integer bool) (str "int"))
      ((size_t) (str "size_t"))
      ((int32 integer32) (str "C_s32"))
      ((integer64) (str "C_s64"))
      ((unsigned-integer64) (str "C_u64"))
      ((short) (str "short"))
      ((long) (str "long"))
      ((unsigned-short) (str "unsigned short"))
      ((unsigned-long) (str "unsigned long"))
      ((float) (str "float"))
      ((double number) (str "double"))
      ((c-pointer nonnull-c-pointer scheme-pointer nonnull-scheme-pointer)
       (str "void *"))
      ((c-string-list c-string-list*) "C_char **")
      ((blob nonnull-blob u8vector nonnull-u8vector) (str "unsigned char *"))
      ((u16vector nonnull-u16vector) (str "unsigned short *"))
      ((s8vector nonnull-s8vector) (str "char *"))
      ((u32vector nonnull-u32vector) (str "unsigned int *"))
      ((s16vector nonnull-s16vector) (str "short *"))
      ((s32vector nonnull-s32vector) (str "int *"))
      ((f32vector nonnull-f32vector) (str "float *"))
      ((f64vector nonnull-f64vector) (str "double *"))
      ((pointer-vector nonnull-pointer-vector) (str "void **"))
      ((nonnull-c-string c-string nonnull-c-string* c-string* symbol) 
       (str "char *"))
      ((nonnull-unsigned-c-string nonnull-unsigned-c-string* 
				  unsigned-c-string unsigned-c-string*)
       (str "unsigned char *"))
      ((void) (str "void"))
      (else
       (cond #;((and (symbol? type) 
		   (##sys#hash-table-ref foreign-type-table type))
	      => (lambda (t)
		   (foreign-type-declaration (if (vector? t) (vector-ref t 0) t) target)) )
	     ((string? type) (str type))
	     ((list? type)
	      (let ((len (length type)))
		(cond 
		 ((and (= 2 len)
		       (memq (car type)
			     '(pointer nonnull-pointer c-pointer 
				       nonnull-c-pointer) ) )
		  (foreign-type-declaration
		   (cadr type)
		   (string-append "*" target)) )
		 ((and (= 2 len)
		       (eq? 'ref (car type)))
		  (foreign-type-declaration
		   (cadr type)
		   (string-append "&" target)) )
		 ((and (> len 2)
		       (eq? 'template (car type)))
		  (str
		   (string-append 
		    (foreign-type-declaration (cadr type) "")
		    "<"
		    (string-intersperse
		     (map (cut foreign-type-declaration <> "") (cddr type))
		     ",")
		    "> ") ) )
		 ((and (= len 2) (eq? 'const (car type)))
		  (string-append
		   "const " 
		   (foreign-type-declaration (cadr type) target)))
		 ((and (= len 2) (eq? 'struct (car type)))
		  (string-append
		   "struct "
		   (->string (cadr type)) " " target))
		 ((and (= len 2) (eq? 'union (car type)))
		  (string-append "union " (->string (cadr type)) " " target))
		 ((and (= len 2) (eq? 'enum (car type)))
		  (string-append "enum " (->string (cadr type)) " " target))
		 ((and (= len 3) 
		       (memq (car type) 
			     '(instance nonnull-instance)))
		  (string-append (->string (cadr type)) "*" target))
		 ((and (= len 3) (eq? 'instance-ref (car type)))
		  (string-append (->string (cadr type)) "&" target))
		 ((and (>= len 3) (eq? 'function (car type)))
		  (let ((rtype (cadr type))
			(argtypes (caddr type))
			(callconv (optional (cdddr type) "")))
		    (string-append
		     (foreign-type-declaration rtype "")
		     callconv
		     " (*" target ")("
		     (string-intersperse
		      (map (lambda (at)
			     (if (eq? '... at) 
				 "..."
				 (foreign-type-declaration at "") ) )
			   argtypes) 
		      ",")
		     ")" ) ) )
		 (else (err)) ) ) )
	     (else (err)) ) ) ) ) )
