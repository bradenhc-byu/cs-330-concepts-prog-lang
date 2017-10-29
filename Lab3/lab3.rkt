#lang racket

; curry2
; takes a two parameter function and returns a curried version
;     of that function
(define (curry2 func)
  (lambda (x) (lambda (y) (func x y))))

; convertFC
; takes a list of integers representing degrees in Fahrenheit
;     and returns a list of the same values in Celsius
(define (convertFC temps)
  (map (lambda (x) (* (/ 5 9) (- x 32))) temps))

(define (my-and x y)
  (and x y))

; check-temps1
; prelim function for recursion
; return true if the list temps contains integers between
;     5 and 95 inclusive
(define (check-temps1 temps)
  (foldr my-and #t (map (lambda (x) (and (>= x 5) (<= x 95))) temps)))

; check-temps
; return true if the list temps contains integers between
;     the user specified bounds of low and high
(define (check-temps temps low high)
  (foldr my-and #t (map (lambda (x) (and (>= x low) (<= x high))) temps)))

(define (convert-place x y)
  (+ (* x 10) y))

; convert
; return an integer comprised of the list of digits in
;     reverse order
(define (convert digits)
  (foldr (lambda (x y) (+ (* 10 y) x)) 0 digits))

; duple
; takes a list of elements and returns a list containing tuples of
; duplicates of each value
(define (duple lst)
  (map (lambda (x) (list x x)) lst))

; average : returns the average of a list lst of numbers
;     using only higher order functions
(define (average lst)
  (foldr (lambda (x y) (/ (+ x y) 2)) (first lst) (rest lst)))

                                                
; is-larger?
; returns true if the first element of a list is larger than any of
;     its subsequent numbers
;(define (is-larger? lst)
;  (if (= (length lst) 1)
;      false
;      (or (> (first lst) (first (rest lst)))
;          (is-larger? (rest lst)))))

; eliminate-larger
; takes a list of integers and removes any that are
;     larger than any of subsequent numbers
(define (eliminate-larger lst)
  (if (empty? lst)
      empty
      (local [(define r (eliminate-larger (rest lst)))]
        (if (is-larger? r (first lst))
            r
            (append (list (first lst)) r)))))
                                                
; is-larger?
; returns true if the first element of a list is larger than any of
;     its subsequent numbers
(define (is-larger? lst num)
  (if (empty? lst)
      false
      (or (> num (first lst))
          (is-larger? (rest lst) num))))

