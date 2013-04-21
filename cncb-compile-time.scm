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
    (define (expand name args dispatcher-id rtype body)
      `(,%begin
	(,%define ,%d (,%dispatcher (,%quote ,dispatcher-id)))
	,(generate-entry-point r name %d args sync rtype)
	(,%dispatcher-add!
	 ,%d
	 (,%quote ,name)
	 (,%lambda
	  (,%d ,%a) 
	  ,(unstash-and-execute r args %a body sync)))))
    (if sync
	(match x
	  ((_ ((name dispatcher-id) args ...) rtype body ...)
	   (expand name args dispatcher-id rtype body))
	  ((_ (name args ...) rtype body ...)
	   (expand name args 'default rtype body)))
	(match x
	  ((_ ((name dispatcher-id) args ...) body ...)
	   (expand name args dispatcher-id #f body))
	  ((_ (name args ...) body ...)
	   (expand name args 'default #f body))))))

(define (generate-entry-point r name dispvar t/a sync rtype)
  (let ((cvar (gensym "cvar"))
	(mutex (gensym "mutex"))
	(infd (gensym "infd"))
	(outfd (gensym "outfd"))
	(data (gensym "data"))
	(ptr (gensym "ptr"))
	(result (symbol->string (gensym "result")))
	(result-arg (gensym "arg"))
	(%begin (r 'begin))
	(%define-foreign-variable (r 'define-foreign-variable))
	(%dispatcher-argument-output-fileno (r 'dispatcher-argument-output-fileno))
	(%dispatcher-result-input-fileno (r 'dispatcher-result-input-fileno))
	(%foreign-declare (r 'foreign-declare)))
    (define (stash-arguments args)
      (let ((size (gensym "size")))
	(with-output-to-string
	  (lambda ()
	    ;; add ptr to callback-name and cvar-ptr
	    ;; we align to 8-byte, which should be sufficient
	    (printf "int ~a = 4 * sizeof(void *);~%char *~a, *~a;~%" size data ptr)
	    (when sync
	      ;; by convention, in a synchronous call the last argument is a
	      ;; pointer to the result value
	      (set! args (append args `(((c-pointer ,rtype) ,result-arg))))
	      (printf "~a, *~a = &~a;~%printf(\"r: %p\\n\", ~a);~%"
		(foreign-type-declaration rtype result #t)
		result-arg result result-arg))
	    (for-each
	     (match-lambda
	       ((type arg)
		(printf "~a += 2 * sizeof(void *);~%" size)))
	     args)
	    ;;XXX result of malloc(3) not checked
	    (printf "~a = (char *)malloc(~a);~%~a = ~a;~%" data size ptr data)
	    (printf "*((char **)~a) = \"~a\";~%~a += 2 * sizeof(void *);~%" ptr name ptr)
	    (when sync
	      (printf "*((void **)~a) = &~a;~%" ptr cvar))
	    ;; one dummy ptr when no cvar needed
	    (printf "~a += 2 * sizeof(void *);~%" ptr) 
	    (for-each
	     (match-lambda
	       ((type arg)
		(printf "memcpy(~a, &~a, sizeof ~a);~%~a += 2 * sizeof(void *);~%" 
		  ptr arg arg ptr)))
	     args)))))
    `(,%begin
      (,%foreign-declare
       "#include <pthread.h>\n"
       ,(conc "static int " infd "," outfd ";\n")
       ,(if sync
	    (conc
	     "static pthread_cond_t " cvar " = PTHREAD_COND_INITIALIZER;\n"
	     "static pthread_mutex_t " mutex " = PTHREAD_MUTEX_INITIALIZER;\n")
	    "")
       ,(conc (if sync (foreign-type-declaration rtype "") "void")
	      " " name "("
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
       ,(conc "write(" outfd ", &" data ", sizeof(void *));\n")
       ,(if sync
	   (conc "pthread_cond_wait(&" cvar ", &" mutex ");\n")
	   "")
       ,(if sync
	    (conc "return " result ";\n")
	    "")
       "}\n")
      (,%define-foreign-variable ,infd int)
      (,%define-foreign-variable ,outfd int)
      (set! ,outfd (,%dispatcher-argument-output-fileno ,dispvar))
      (set! ,infd (,%dispatcher-result-input-fileno ,dispvar)))))


(define (unstash-and-execute r t/a ptr body sync)
  (let ((%let (r 'let))
	(%foreign-lambda* (r 'foreign-lambda*))
	(%begin (r 'begin))
	(%free (r 'free))
	(%begin0 (r 'begin0))
	(%advance! (r 'advance!))
	(ptrvar (gensym "pptr")))
    `(,%let ((,ptrvar ((,%foreign-lambda* c-pointer ((c-pointer ptr)) "return(ptr);")
		       ,ptr))		; copy pointer because of pointer-mutation below
	     (,%advance! 
	      (,%foreign-lambda* 
	       void ((scheme-object ptr))
	       "C_set_block_item(ptr, 0, C_block_item(ptr, 0) + 2 * sizeof(void *));")))
	    (,%advance! ,ptrvar)	; cbname
	    (,%advance! ,ptrvar)	; cvar
	    (,%let
	     ,(map (match-lambda
		     ((type arg) 
		      `(,arg (,%begin0
			      ((,%foreign-lambda* 
				,type
				(((c-pointer ,type) ptr))
				,(string-append
				  (foreign-type-declaration type "val")
				  " = *ptr;\nreturn(val);"))
			       ,ptrvar)
			      (,%advance! ,ptrvar)))))
		   t/a)
	     ,@(if sync
		   (let ((type (car (last t/a))))
		     `(((,%foreign-lambda* 
			 void
			 (((c-pointer (c-pointer ,type)) ptr)
			  (c-pointer buf)
			  (,type val)
			  (int index))
			 "*(*ptr) = val;"
			 "pthread_cond_signal(*((pthread_cond_t **)buf + 2));")
			,ptrvar ,ptr (,%begin ,@body) ,(length t/a))))
		   body))
	     (,%free ,ptr))))


;; foreign-type conversion - taken from c-backend.scm

(define (foreign-type-declaration type target #!optional result?)
  (let ((err (lambda () (syntax-error "illegal foreign type" type)))
	(badresult (lambda () (syntax-error "invalid native callback result type" type)))
	(str (lambda (ts) (string-append ts " " target))) )
    ;;XXX "badresult" not used - statically allocated data would be ok.
    ;;    this is still rather error-prone.
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
