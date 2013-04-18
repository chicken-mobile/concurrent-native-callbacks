;;;; concurrent native callbacks


(use typed-records matchable srfi-69 srfi-1 posix bind)


#|XXX
(import-for-syntax chicken matchable)

(begin-for-syntax
 (require-extension cnbc-compile-time))

(bind-file* "twiddle.c")
|#


(define-typed-record dispatcher
  (id : symbol)
  (thread : thread)
  (callbacks : (list-of (pair fixnum ((struct dispatcher) c-pointer -> * boolean))))
  (argument-input-fileno : fixnum)
  (argument-output-fileno : fixnum)
  (result-input-fileno : fixnum)
  (result-output-fileno : fixnum) )


(define dispatcher-table (make-hash-table eq?))

(define (dispatcher id)
  (define (dthread) (make-thread dispatch))
  (define (dispatch)
    (let* ((disp (thread-specific (current-thread)))
	   (in (dispatcher-argument-input-fileno disp))
	   (out (dispatcher-result-output-fileno disp)))
      (let loop ()
	(let ((input (read_message in)))
	  (unless (##sys#null-pointer? input) ; aborts dispatcher
	    (let ((cbname (extract_callback_name input)))
	      (cond ((alist-ref cbname (dispatcher-callbacks disp)) =>
		     (lambda (cb)
		       (cb disp input)))
		    (else
		     (warning "callback not found" cbname id)))
	      (loop)))
	  (file-close in)
	  (file-close out)
	  (file-close (dispatcher-argument-output-fileno disp))
	  (file-close (dispatcher-result-input-fileno disp))))))
  (or (hash-table-ref/default dispatcher-table id #f)
      (let ((t (dthread)))
	(let-values (((in1 out1) (create-pipe))
		     ((in2 out2) (create-pipe)))
	  (let ((disp (make-dispatcher id t '() in1 out1 in2 out2)))
	    (thread-specific-set! t disp)
	    (thread-start! t)
	    disp)))))		   

(define (dispatcher-add! disp name cb)
  (dispatcher-callbacks-set!
   disp
   (alist-update! name cb (dispatcher-callbacks disp))))

(define (dispatcher-terminate! disp)
  (send_termination_signal 
   (dispatcher-argument-input-fileno disp)))

(define-syntax define-concurrent-native-callback
  (cnbc-transformer #f))

(define-syntax define-synchronous-concurrent-native-callback
  (cnbc-transformer #t))

(define (synchronous-return argptr result)
  (trigger_return argptr))
