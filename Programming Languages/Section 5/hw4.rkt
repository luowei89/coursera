
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

;; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (sub1 n)))))

;; Problem 5
(define funny-number-stream 
  (letrec ([f (lambda (x) (cons (if (= 0 (remainder x 5)) (- x) x)
                                (lambda () (f (add1 x)))))])
    (lambda () (f 1))))

;; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons x 
                                (lambda () (f (if (string=? "dan.jpg" x) "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

;; Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x)))
                                (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

;; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                (lambda () (f (add1 n)))))])
    (lambda () (f 0))))

;; Problem 9
(define (vector-assoc v vec)
  (letrec ([helper (lambda (i) 
                     (if (= i (vector-length vec))
                         #f
                         (let ([vi (vector-ref vec i)])
                           (cond 
                             [(not (pair? vi)) (helper (add1 i))]
                             [(equal? (car vi) v) vi]
                             [#t (helper (add1 i))]))))])
    (helper 0)))

;; Problem 10
(define (cached-assoc xs n)
  (letrec([memo (make-vector n #f)]
          [index 0]
          [f (lambda (v) 
               (let ([ans (vector-assoc v memo)])
                 (if ans 
                     ans
                     (let ([new-ans (assoc v xs)])
                       (begin
                         (vector-set! memo index new-ans)
                         (set! index (remainder (add1 index) n))
                         new-ans)))))])
    f))

;; Challenge Problem
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([e e1]
              [f (lambda (x) (if (<= x e2) #t (f x)))])
       (f e))]))
