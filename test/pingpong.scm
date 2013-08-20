
(use-modules (thread-mailbox thread-mailbox)
	     (srfi srfi-18))

(define t1 (current-thread))
(thread-mailbox-make-and-set!)

(define (pong)
  (begin

;    (display "here\n")
;    (display (current-thread))
    (thread-send t1 (thread-receive))
    (pong)))

(define t2 (make-thread (lambda () (pong))))
(thread-mailbox-make-and-set! t2)
;(display 'here1)
(thread-start! t2)

(define (ping i)
  (let ((ct (current-time)))
    (thread-send t2 (cons i ct))
    (display (cons i ct))
    (display (thread-receive))
    (newline)
    (ping (+ 1 i))))

(ping 0)
