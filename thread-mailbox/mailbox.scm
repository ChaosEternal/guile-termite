

(library (thread-mailbox mailbox)
(export make-empty-mailbox
	mailbox-rewind!
	mailbox-rewound?
	mailbox-send
	mailbox-next-value-or-receive
	mailbox-receive
	mailbox-next-value
	mailbox-extract-and-rewind
        )
(import (ice-9 q)
	(rnrs (6))
	(guile) ;; throw, set-cdr!, catch 
        (srfi srfi-18)
	(srfi srfi-2)
	(srfi srfi-34))

;; (use-modules (ice-9 q)
;; 	     (srfi srfi-18)
;; 	     (srfi srfi-2)
;; 	     (srfi srfi-34))

;; make an empty mailbox, with 2 place-holders
(define (make-mbox) (sync-q!
		     '((place-holder
			place-holder2)))) 
(define (make-empty-mailbox)
  (let* ((mutex (make-mutex))
	 (read-cond-var (make-condition-variable))
	 (mbox (make-mbox))
	 (cursor (car mbox)))
    (letrec*
	([next-value-or-receive (lambda (receive? timeout)
		      (if (null? (cddr cursor))
			  (begin
			    (if (not (apply mutex-lock! 
					    (cons mutex 
						  (if timeout (cons timeout '()) '()) ;; work around for bug of mutex-lock!
						  )))
				(throw 'timeout-reached))
			    (if (null? (cddr cursor))
				(begin
				  (if (not
				       (apply mutex-unlock!
					      (cons mutex (cons read-cond-var
								(if timeout (cons timeout '()) '()) ;; workaround of bug of mutex-unlock!
								))))
				      (throw 'timeout-reached))
				  (next-value-or-receive receive? timeout))))
			  (let ([result (caddr cursor)]
				[ncursor (cdr cursor)])
			    (if receive?
				(if (null? (cddr ncursor))
				      (begin (mutex-lock! mutex)
					     (let ((tail (cdr ncursor)))
					       (set-cdr! ncursor (cddr ncursor))
					       (if (null? (cdr tail))
						   (set-cdr! mbox tail)))
					     (mutex-unlock! mutex))
				      (set-cdr! ncursor (cddr ncursor))
				      )
				(set! cursor ncursor))
			    result)))]
	 [rewind (lambda () (set! cursor (car mbox)))]
	 [rewound? (lambda () (eq? (car cursor) (car (car mbox))))]
	 [extract-and-rewind (lambda (timeout)
			       (if (rewound?)
				   (next-value-or-receive #t timeout)
				   (let ((value (cadr cursor)))
				     (begin
				       (if (null? (cddr cursor))
					   (begin (mutex-lock! mutex timeout)
						  (let ((tail (cdr cursor)))
						    (set-cdr! cursor (cddr cursor))
						    (if (null? (cdr tail))
							(set-cdr! mbox tail)))
						  (mutex-unlock! mutex))
					   (set-cdr! cursor (cddr cursor)))
				       (set! cursor (car mbox)))
				     value))
			       )] 
	 [send (lambda (message)
		 (begin
		   (mutex-lock! mutex)
		   (enq! mbox message)
		   (condition-variable-signal! read-cond-var)
		   (mutex-unlock! mutex)))]
	 )
	(lambda (x . y)
	  (dynamic-wind
	      (lambda () #t)
	      (lambda ()
		(case x
		  ((rewind) (apply rewind y))
		  ((rewound?) (apply rewound? y))
		  ((extract-and-rewind) (apply extract-and-rewind y))
		  ((next-value-or-receive) (apply next-value-or-receive y))
		  ((send) (apply send y))
		  ))
	      (lambda ()
		(if (eq? (mutex-state mutex) (current-thread))
		    (mutex-unlock! mutex)))))
	)))
   
(define (mailbox-rewind! mbox)
  (mbox 'rewind))

(define (mailbox-rewound? mbox)
  (mbox 'rewound?))

(define (mailbox-send mbox message)
  (mbox 'send message))

(define (mailbox-next-value-or-receive mbox receive? . timeout-and-default)
  (cond
   ((null? timeout-and-default) (mailbox-next-value-or-receive 
				 mbox
				 receive?
				 #f))
   ;;((and (time? (car timeout-and-default)) (not (eq? #f (car timeout-and-default))))
   ;;  (raise 'invalid-timeout))
   ((null? (cdr timeout-and-default)) 
    (mbox 
     'next-value-or-receive receive? (car timeout-and-default)))
   (else (catch 'timeout-reached
		(lambda () 
		  (mailbox-next-value-or-receive 
		   mbox
		   receive?
		   (car timeout-and-default)))
		(lambda (key . args) 
		  (cadr timeout-and-default))))))

(define (mailbox-receive mbox . timeout-and-default)
  (apply mailbox-next-value-or-receive 
	 (cons mbox (cons #t timeout-and-default))))

(define (mailbox-next-value mbox . timeout-and-default)
  (apply mailbox-next-value-or-receive
	 (cons mbox (cons #f timeout-and-default))))

(define (mailbox-extract-and-rewind mbox . timeout)
  (if (null? timeout)
      (mailbox-extract-and-rewind mbox #f)
      (mbox 'extract-and-rewind (car timeout))))
)
;; (define mymbox (make-empty-mailbox))

;; (for-each (lambda (x) (mailbox-send mymbox x)) (iota 100))
;; (let lp ()
;;   (display (mailbox-receive mymbox))
;;   (lp))