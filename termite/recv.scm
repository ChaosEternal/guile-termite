
(library (termite recv)
(export recv)
(import (termite match-action)
	(rnrs (6))
	(guile) ;; throw, set-cdr!, catch 
        (srfi srfi-18)
	(srfi srfi-2)
	(srfi srfi-34))

(define-syntax  recv
  (syntax-rules (after)
    ((_ e1 e2 ... (after tmout on-tmout) ) (do-recv  tmout on-tmout e1 e2 ... ))
    ((_ e1 e2 ... ) (recv e1 e2 ... (after #f #f) ))
    )
)

(define-syntax do-recv
  (syntax-rules ()
    ((_ tmout #f e2 e3 ...)
     (let lp ((msg (thread-receive tmout)))
       (match/action 
	(thread-mailbox-extract-and-rewind)
	(lp (thread-mailbox-next tmout))
	msg
	e2
	e3
	...
	)))
    ((_ tmout on-tmout e2 e3 ...)
     (catch 'timeout-reached
       (lambda (do-recv tmout #f e2 e3 ...))
       (lambda () on-tmout)
       ))))

)
