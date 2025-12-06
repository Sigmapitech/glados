; Complex Lisp Examples
; This file demonstrates advanced Lisp programming patterns

; Factorial function using recursion
(define factorial
  (lambda (n)
    (if (eq? n 0)
        1
        (* n (factorial (- n 1))))))

; Fibonacci sequence
(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

; Sum of a range from a to b
(define sum-range
  (lambda (a b)
    (if (< b a)
        0
        (+ a (sum-range (+ a 1) b)))))

; Power function (x^n)
(define power
  (lambda (x n)
    (if (eq? n 0)
        1
        (if (eq? n 1)
            x
            (* x (power x (- n 1)))))))

; Check if a number is even
(define is-even
  (lambda (n)
    (eq? (mod n 2) 0)))

; Check if a number is odd
(define is-odd
  (lambda (n)
    (eq? (mod n 2) 1)))

; Absolute value
(define abs
  (lambda (n)
    (if (< n 0)
        (- 0 n)
        n)))

; Max of two numbers
(define max
  (lambda (a b)
    (if (< a b)
        b
        a)))

; Min of two numbers
(define min
  (lambda (a b)
    (if (< a b)
        a
        b)))

; GCD (Greatest Common Divisor) using Euclidean algorithm
(define gcd
  (lambda (a b)
    (if (eq? b 0)
        a
        (gcd b (mod a b)))))

; LCM (Least Common Multiple)
(define lcm
  (lambda (a b)
    (div (* a b) (gcd a b))))

; Count down from n to 0
(define countdown
  (lambda (n)
    (if (eq? n 0)
        0
        (countdown (- n 1)))))

; Ackermann function (extremely recursive)
(define ackermann
  (lambda (m n)
    (if (eq? m 0)
        (+ n 1)
        (if (eq? n 0)
            (ackermann (- m 1) 1)
            (ackermann (- m 1) (ackermann m (- n 1)))))))

; Nested conditionals and computations
(define complex-calc
  (lambda (x y z)
    (if (< x y)
        (if (< y z)
            (* x (+ y z))
            (- (* x y) z))
        (if (< x z)
            (+ (* x y) (* y z))
            (- (+ x y) z)))))

; Higher-order function: apply a function twice
(define twice
  (lambda (f x)
    (f (f x))))

; Increment function
(define inc
  (lambda (n)
    (+ n 1)))

; Decrement function
(define dec
  (lambda (n)
    (- n 1)))

; Double a number
(define double
  (lambda (n)
    (* n 2)))

; Triple a number
(define triple
  (lambda (n)
    (* n 3)))

; Square a number
(define square
  (lambda (n)
    (* n n)))

; Cube a number
(define cube
  (lambda (n)
    (* n (* n n))))

; Sum of squares
(define sum-of-squares
  (lambda (a b)
    (+ (square a) (square b))))

; Pythagorean theorem checker (checks if a^2 + b^2 = c^2)
(define is-pythagorean
  (lambda (a b c)
    (eq? (+ (square a) (square b)) (square c))))

; Collatz sequence step (3n+1 problem)
(define collatz
  (lambda (n)
    (if (eq? n 1)
        1
        (if (is-even n)
            (collatz (div n 2))
            (collatz (+ (* 3 n) 1))))))

; Compose two single-argument functions
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

; Church numerals simulation - successor
(define succ
  (lambda (n)
    (+ n 1)))

; Predecessor (limited to non-negative)
(define pred
  (lambda (n)
    (if (eq? n 0)
        0
        (- n 1))))

; Sign function
(define sign
  (lambda (n)
    (if (< n 0)
        (- 0 1)
        (if (eq? n 0)
            0
            1))))

; Distance between two numbers
(define distance
  (lambda (a b)
    (abs (- a b))))

; Clamp a value between min and max
(define clamp
  (lambda (value minval maxval)
    (max minval (min value maxval))))

; Linear interpolation
(define lerp
  (lambda (a b t)
    (+ a (* t (- b a)))))

; Average of two numbers
(define average
  (lambda (a b)
    (div (+ a b) 2)))

; Sum of first n natural numbers
(define sum-n
  (lambda (n)
    (if (eq? n 0)
        0
        (+ n (sum-n (- n 1))))))

; Product of first n natural numbers (factorial alternative)
(define product-n
  (lambda (n)
    (if (eq? n 0)
        1
        (* n (product-n (- n 1))))))

; Exponentiation by squaring (faster power)
(define fast-power
  (lambda (x n)
    (if (eq? n 0)
        1
        (if (eq? n 1)
            x
            (if (is-even n)
                (square (fast-power x (div n 2)))
                (* x (fast-power x (- n 1))))))))

; Test expressions
(factorial 5)
(fib 7)
(sum-range 1 10)
(power 2 10)
(gcd 48 18)
(lcm 12 18)
(ackermann 2 2)
(is-pythagorean 3 4 5)
(sum-of-squares 3 4)
(twice double 5)
(complex-calc 5 10 3)
