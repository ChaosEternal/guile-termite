

(library (thread-mailbox thread-mailbox)
(export thread-mailbox-make-and-set!
	thread-mailbox-get
	thread-mailbox-rewound?
	thread-send
	thread-receive
	thread-mailbox-next
	thread-mailbox-rewind!
	thread-mailbox-extract-and-rewind
        )
(import (ice-9 q)
	(rnrs (6))
	(guile) ;; throw, set-cdr!, catch 
        (srfi srfi-18)
	(srfi srfi-2)
	(srfi srfi-34)
	(thread-mailbox mailbox))

(define (thread-mailbox-make-and-set! . thread)
  (if (null? thread)
      (thread-mailbox-make-and-set! )
      (thread-specific-set! (car thread) (make-empty-mailbox))))

(define (thread-mailbox-get . thread)
  (if (null? thread)
      (thread-mailbox-get (current-thread))
      (let ((mbox (thread-specific-get (car thread))))
	(if (not mbox)
	    (throw 'thread-mailbox-nonexists "thread mailbox not exists!")
	    mbox))))

(define (thread-mailbox-rewound?)
  (mailbox-rewound? (thread-mailbox-get)))

(define (thread-send thread message)
  (mailbox-send (thread-mailbox-get thread) message))

(define (thread-receive . timeout-and-default)
  (apply mailbox-receive (cons (thread-mailbox-get)
			       timeout-and-default)))

(define (thread-mailbox-next . timeout-and-default)
  (apply mailbox-next-value (cons (thread-mailbox-get)
				  timeout-and-default)))

(define (thread-mailbox-rewind!)
  (mailbox-rewind! (thread-mailbox-get)))

(define (thread-mailbox-extract-and-rewind . timeout)
  (apply mailbox-extract-and-rewind (thread-mailbox-get) timeout))
)
