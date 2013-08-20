
(use-modules (thread-mailbox thread-mailbox)
	     (srfi srfi-18))

(define t1 (current-thread))
(thread-mailbox-make-and-set!)

(define (sender)
  (begin

    (let lp ((i 0))
      (thread-send t1 i)
;      (usleep 100000)
      (lp (+ 1 i)))
    ))

(define t2 (make-thread (lambda () (sender))))
;(display 'here1)
(thread-start! t2)

(let recv ()
  (display (thread-receive))
  (newline)
  (recv))
