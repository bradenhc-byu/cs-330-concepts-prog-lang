#lang racket

(define (sum-coins pennies nickels dimes quarters)
  (+ (* 1 pennies)
     (* 5 nickels)
     (* 10 dimes)
     (* 25 quarters))
 )

(define (degrees-to-radians angle)
  (* 3.1415 (/ angle 180))
 )

(define (sign x)
  (if (< x 0) -1 (if (= x 0) 0 1))
 )

(define (new-sin angle type)
  (if (symbol=? type 'degrees) (sin (degrees-to-radians angle)) (sin angle)) 
 )
