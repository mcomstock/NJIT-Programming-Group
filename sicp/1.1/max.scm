;;
;; Exercise 1.1
;;

;; 10
;; 12
;; 8
;; 3
;; 6
;;
;;
;; 19
;; #f
;; 4 (the condition is true)
;; 16 (the second condition is true)
;; 6 (the condition is true)
;; 16 (the second condition is true)

;;
;; Exercise 1.2
;;

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))
;; Result: -37/150

;;
;; Exercise 1.3
;;

(define (square x) (* x x))
(define (sum-of-squares a b) (+ (square a) (square b)))
(define (larger-sum-of-squares a b c)
  (if (> a b)
      (if (> b c)
          (sum-of-squares a b)
          (sum-of-squares a c))
      (if (> a c)
          (sum-of-squares a b)
          (sum-of-squares b c))))
;; Alternative list approach
;; (define (sorttwo a b) (if (> a b) (list a b) (list b a)))
;; (define (larger-sum-of-squares a b c)
;;   (define sorted (sorttwo a b))
;;   (sum-of-squares (car sorted)
;;                   (car (sorttwo
;;                         (cadr sorted)
;;                         c))))
(display (larger-sum-of-squares 1 2 3))
(newline)
(display (larger-sum-of-squares 3 2 1))
(newline)
(display (larger-sum-of-squares 3 1 2))
(newline)
(display (larger-sum-of-squares 2 1 3))
(newline)
(newline)

;;
;; Exercise 1.4
;;

;; The expression results in a + b if b is positive and a - b otherwise.
;; In other words, a + |b|

;;
;; Exercise 1.5
;;

;; Applicative
;; (test 0 (p)) = (if (= 0 0) (0 (p))) = 0
;; Normal
;; (test 0 (p)) = (test 0 (p)) after expanding (p), should infinite loop

;;
;; Exercise 1.6
;;

;; The new-if function is not a special form. If applicative order is used, then both arguments to
;; new-if will need to be evaluated, leading to infinite recursion into the square-iter function.

;;
;; Exercise 1.7
;;

;; Original
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Error for floating point numbers is usually measured relative to the magnitude of the values in
;; question rather than an absolute quantity (relative vs. absolute error). For large enough values,
;; accurate enough numbers might not be representable due to the resolution of the numbers, so it
;; will just loop infinitely.

;; (display (sqrt 1.0e100)) ;; infinite loop
;; (newline)
(display (sqrt 0.0001)) ;; very bad
(newline)

;; Fix
(define (rel-sqrt-iter guess prev x)
  (if (rel-good-enough? guess prev)
      guess
      (rel-sqrt-iter (improve guess x)
                 guess
                 x)))
(define (rel-good-enough? guess prev)
  (< (abs (- guess prev)) (* 1e-10 guess)))
(define (rel-sqrt x)
  (rel-sqrt-iter 1.0 0.0 x))

(display (rel-sqrt 1.0e100))
(newline)
(display (rel-sqrt 0.0001))
(newline)
(newline)

;;
;; Exercise 1.8
;;

(define (cbrt-iter guess prev x)
  (if (rel-good-enough? guess prev)
      guess
      (cbrt-iter (cbrt-improve guess x)
                 guess
                 x)))
(define (cbrt-improve y x)
  (/ (+ (/ x
           (* y y))
        (* 2 y))
     3))
(define (cbrt x)
  (cbrt-iter 1.0 0.0 x))

(display (cbrt 1.0e120))
(newline)
(display (cbrt 1e-12))
(newline)
