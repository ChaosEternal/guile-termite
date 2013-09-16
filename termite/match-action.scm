;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010, 2011, 2012 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (termite match-action)
  #:export (
	    match/action
            ))

(define error (@@ (ice-9 match) error))
;; syntax: match/action on-success on-fail exp clause1 clause2 ...
;; in compare to original match, match/action takes two more arguments:
;; on-success: code to insert when a clause matches, #f means do nothing
;; on-failure: code to execute when all pattern fail, #f will raise exception as usual

(define-syntax match/action
  (syntax-rules ()
    ((match/action)
     (match-syntax-error "missing match expression"))
    ((match/action on-success on-failure atom)
     (match-syntax-error "no match clauses"))
    ((match/action on-success on-failure (app ...) (pat . body) ...)
     (let ((v (app ...)))
       (match-next/action on-success on-failure v ((app ...) (set! (app ...))) (pat . body) ...)))
    ((match/action on-success on-failure #(vec ...) (pat . body) ...)
     (let ((v #(vec ...)))
       (match-next/action on-success on-failure v (v (set! v)) (pat . body) ...)))
    ((match/action on-success on-failure atom (pat . body) ...)
     (let ((v atom))
       (match-next/action on-success on-failure v (atom (set! atom)) (pat . body) ...)))
    ))

(define-syntax on-succ-helper
  (syntax-rules ()
    ((_ #f e1 ...)
     (begin e1 ...))
    ((_ #t e1 ...)
     (begin e1 ...))
    ((_ e1 e2 ...)
     (begin e1 e2 ...))))

(define-syntax on-fail-helper
  (syntax-rules ()
    ((_ #f) 
     (error 'match "no matching pattern"))
    ((_ e1)
     e1)))

(define-syntax match-next/action
  (syntax-rules (=>)
    ;; no more clauses, the match failed
    ((match-next/action on-success on-failure v g+s)
     (on-fail-helper on-failure))
    ;; named failure continuation
    ((match-next/action on-success on-failure v g+s (pat (=> failure) . body) . rest)
     (let ((failure (lambda () (match-next/action on-success on-failure v g+s . rest))))
       ;; match-one analyzes the pattern for us
       ((@@ (ice-9 match) match-one) v pat g+s ((@@ (ice-9 match) match-drop-ids) (on-succ-helper on-success . body)) (failure) ())))
    ;; anonymous failure continuation, give it a dummy name
    ((match-next/action on-success on-failure v g+s (pat . body) . rest)
     (match-next/action on-success on-failure v g+s (pat (=> failure) . body) . rest))))


