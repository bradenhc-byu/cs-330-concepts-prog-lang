#lang racket

; default-params
; takes a function f and a list of values whose
;     length corresponds with the arity of f
; returns the result of f
(define (default-parms f values)
  (lambda args
    (apply f (append args (list-tail values (length args))))))

; type-params
; take a function f and a list types of type predicates of the same
;     length as the arity of f
; returns a function that behaves exactly like f but first
;     checks to make sure the parameter types match that of
;     f
(define (type-parms f types)
  (lambda args
    (if (validate args types) 
        (apply f args)
        (writeln "got an error"))))

; validate
; takes the parameter values passed into a function created by
;     type-params and determines whether they are valid parameters
; returns true if parameters are valid, false otherwise
(define (validate args types)
  (if (empty? types)
      (empty? args)
      (and ((first types) (first args)) (validate (rest args) (rest types)))))

; chain-dec
; chains the decorators together (for testing purposes)
(define chain-dec (default-parms
         (type-parms
          +
          (list number? number?))
         (list 0 0)))

; degrees-to-radians
; converts the given angle from degree to radians
(define (degrees-to-radians angle)
  (* 3.1415 (/ angle 180))
 )

; new-sin
; sin function devolped in the first assignment
; returns the sin of the angle given the type
(define (new-sin angle type)
  (if (symbol=? type 'degrees) (sin (degrees-to-radians angle)) (sin angle)) 
 )


; new-sin2
; Uses decorators to verify the parameter values for
;     the sin function, also setting default values if
;     no other parameters are given
(define new-sin2 (default-parms
                   (type-parms
                    new-sin
                    (list number? symbol?))
                   (list 0 'radians)))