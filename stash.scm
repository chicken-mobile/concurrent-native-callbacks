;;;; stashing code


;; "result" must be ptr to actual result value type
(define (stash-arguments cbname args result outfd)
  (let ((data (gensym "data"))
	(ptr (gensym "ptr"))
	(size (gensym "size"))
	(cvar (gensym "cvar"))
	(mutex (gensym "mutex")))
    (with-output-to-string
      (lambda ()
	(when result
	  (printf "pthread_cond_t ~a = PTHREAD_COND_INITIALIZER;~%" cvar)
	  (printf "pthread_mutex_t ~a = PTHREAD_MUTEX_INITIALIZER;~%" mutex)
	  (printf "pthread_mutex_lock(~a);~%" mutex))
	;; add ptr to callback-name, cvar-ptr and result-ptr
	(printf "int ~a = sizeof(void *) * 3;~%char ~a, ~a;~%" size data ptr)
	(for-each
	 (lambda (arg)
	   (printf "~a += sizeof ~a;~%" size arg))
	 args)
	(printf "~a = (char *)malloc(~a);~%assert(~a);~%~a = ~a~%"
	  data size data ptr data)
	(printf "*((char **)~a) = \"~a\";~%~a += sizeof(char *);~%" ptr cbname ptr)
	(printf "*((void **)~a) = &~a;~%~a += sizeof(void *);~%" ptr cvar ptr)
	(printf "*((void **)~a) = &~a;~%~a += sizeof(void *);~%" ptr result ptr)
	(for-each
	 (lambda (arg)
	   (printf "memcpy(~a, &~a, sizeof ~a);~%~a += sizeof ~a;~%" ptr arg arg ptr arg))
	 args)
	;; no locking, unless size of data block exceeds PIPE_BUF
	;;XXX should check for this, even if it is unlikely
	(printf "write(~a, ~a, ~a - ~a);~%" outfd data (- ptr data))
	(when result
	  (printf "pthread_cond_wait(&~a, &~a);~%" cvar mutex)
	  (printf "free(~a);~%" data)
	  (printf "return *~a;~%" result))))))
