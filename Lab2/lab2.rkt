#lang racket

; check-temps1
; prelim function for recursion
; return true if the list temps contains integers between
;     5 and 95 inclusive
(define (check-temps1 temps)
  (if (empty? temps)
      true
      (and (>= (first temps) 5)
           (and (<= (first temps) 95)
                (check-temps1 (rest temps))))))

; check-temps
; return true if the list temps contains integers between
;     the user specified bounds of low and high
(define (check-temps temps low high)
  (if (empty? temps)
      true
      (and (>= (first temps) low)
           (and (<= (first temps) high)
                (check-temps (rest temps) low high)))))

; convert
; return an integer comprised of the list of digits in
;     reverse order
(define (convert digits)
  (convert-rec digits 0))

; recursive helper function for the convert function
; uses the level to determine at which power of 10 the digit
;     should be placed in the returned integer
(define (convert-rec digits level)
  (if (empty? digits)
      0
      (+ (* (expt 10 level) (first digits))
         (convert-rec (rest digits) (+ level 1)))))

; duple
; takes a list of elements and returns a list containing tuples of
; duplicates of each value
(define (duple lst)
  (if (empty? lst)
      empty
      (append (list (list (first lst) (first lst))) (duple (rest lst)))))


; len : list of anything -> number
; return the length of the list
(define (len lst)
  (if (empty? lst)
      0
      (+ 1 (len (rest lst)))))

; average : returns the average of a list lst of numbers
; calls a seperate recursive function
(define (average lst)
  (average-rec lst 0 (len lst)))

; recursively finds the sum of a list of numbers
(define (average-rec lst sum len)
  (if (empty? lst)
      (/ sum len)
      (average-rec (rest lst) (+ sum (first lst)) len)))

; convertFC
; takes a list of integers representing degrees in Fahrenheit
;     and returns a list of the same values in Celsius
(define (convertFC temps)
  (if (empty? temps)
      empty
      (append (list (* (/ 5 9) (- (first temps) 32))) (convertFC (rest temps)))))

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

; get-nth
; returns the nth element of the list
(define (get-nth lst n)
  (get-nth-rec lst n 0))

; recursively get the nth element (tracked with variable
;     passed into recursive function
(define (get-nth-rec lst n pos)
  (if (= n pos)
      (first lst)
      (get-nth-rec (rest lst) n (+ pos 1))))

; find-item
; determines the index of an item in a list
; returns the index if found, -1 otherwise
(define (find-item lst target)
  (cond
    [(empty? lst)-1]
    [(= (first lst) target) 0]
    [else (local [(define r (find-item (rest lst) target))]
          (if (= r -1) -1 (+ 1 r)))]))
      
