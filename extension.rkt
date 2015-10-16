#lang racket

(provide (all-defined-out))

;; Increment
(define (++ i)
  (+ i 1))

;; Decrement
(define (-- i)
  (- i 1))

;; Is integer odd?
(define (odd? i)
  (= (remainder i 2) 1))

;; Is integer even?
(define (even? i)
  (= (remainder i 2) 0))

;; Get x of 2D-vector
(define (vec-x v)
  (vector-ref v 0))

;; Get y of 2D-vector
(define (vec-y v)
  (vector-ref v 1))

;; Get length of 2D-vector
(define (vec-length v)
  (sqrt (+ (sqr (vec-x v)) (sqr (vec-y v)))))

;; Add two 2D-vectors
(define (vec-add x y)
  (vector (+ (vec-x x) (vec-x y))
          (+ (vec-y x) (vec-y y))))

;; Check if element is in list
(define (list-contains? li e)
  (cond [(empty? li) #f]
        [(equal? (car li) e) #t]
        [else (list-contains? (cdr li) e)]))

;; Flatten list
(define (flat . li)
  (foldr (lambda (x y)
           (append (if (list? x) (apply flat x) (list x)) y))
         (list)
         li))

;; Cartesian product
(define (cartesian . lis)
  (foldr (lambda (lix liy)
           (apply append (map (lambda (x)
                                (map (lambda (y)
                                       (cons x y))
                                     liy))
                              lix)))
         (list null) lis))

;; Cartesian product yielding vectors
(define (vec-cartesian . lis)
  (map list->vector (apply cartesian lis)))

;; Reverse function for vectors
(define (reverse-vector v)
  (list->vector (reverse (vector->list v))))
