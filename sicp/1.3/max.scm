;;
;; Exercise 1.29
;;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (simpson f a b n)
  (define h (/ (- a b) n))
  (define (term1 k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (* (term1 k) (f (+ a (* k h)))))
  (define (next k) (+ k 1))
  (* (/ h 3) (sum term 0 next n)))

(define (cube x) (* x x x))

;; This method is much more accurate

(display (simpson cube 0.0 1.0 100))
;; 0.24999999999999992
(newline)
(display (simpson cube 0.0 1.0 1000))
;; 0.2500000000000003
(newline)
(newline)

;;
;; Exercise 1.30
;;

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;;
;; Exercise 1.31
;;

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define product product-iter)

(define (factorial n)
  (define (term x) x)
  (define (next x) (+ 1 x))
  (product term 1 next n))

(define (pi-approx n)
  (define (term x) (/ (* x (+ x 2)) (* (+ x 1) (+ x 1))))
  (define (next x) (+ x 2))
  (* (product term 2.0 next (+ n 2)) 4))

(display (factorial 1))
(newline)
(display (factorial 2))
(newline)
(display (factorial 3))
(newline)
(display (factorial 4))
(newline)
(display (factorial 5))
(newline)
(display (factorial 6))
(newline)
(newline)

(display (pi-approx 1))
(newline)
(display (pi-approx 10))
(newline)
(display (pi-approx 100))
(newline)
(display (pi-approx 1000))
(newline)
(newline)

;;
;; Exercise 1.32
;;

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define accumulate accumulate-iter)

(define (factorial n)
  (define (term x) x)
  (define (next x) (+ 1 x))
  (accumulate * 1 term 1 next n))

(define (pi-approx n)
  (define (term x) (/ (* x (+ x 2)) (* (+ x 1) (+ x 1))))
  (define (next x) (+ x 2))
  (* (accumulate * 1 term 2.0 next (+ n 2)) 4))

(display (factorial 1))
(newline)
(display (factorial 2))
(newline)
(display (factorial 3))
(newline)
(display (factorial 4))
(newline)
(display (factorial 5))
(newline)
(display (factorial 6))
(newline)
(newline)

(display (pi-approx 1))
(newline)
(display (pi-approx 10))
(newline)
(display (pi-approx 100))
(newline)
(display (pi-approx 1000))
(newline)
(newline)

;;
;; Exercise 1.33
;;

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (square-check x n)
  (let ((sq (remainder (* x x) n)))
    (if (and (not (= x 1))
             (not (= x (- n 1)))
             (= sq 1))
        0
        sq)))

(define (expmod-check base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-check (expmod-check base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod-check base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-check a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-mr? n times)
  (cond ((< n 0) (fast-prime-mr? (abs n) times))
        ((< n 2) #f)
        ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime-mr? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime-mr? n 1000))

(define (sum-prime-squares a b)
  (define (term x) (* x x))
  (define (next x) (+ x 1))
  (filtered-accumulate + 0 term a next b prime?))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-rel-prime n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (define (filter x) (= (gcd x n) 1))
  (filtered-accumulate * 1 term 2 next (- n 1) filter))

(display (sum-prime-squares 0 10))
(newline)

(display (product-rel-prime 10))
(newline)
(display (product-rel-prime 11))
(newline)
(newline)

;;
;; Exercise 1.34
;;

(define (f g)
  (g 2))

;; (f f)
;; First evaluates the argument f and gets the function definition
;; Substitute the function to get (f 2)
;; Applies 2 to f to get (2 2)
;; Error because 2 is not a function

;;
;; Exercise 1.35
;;

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Golden ratio: phi^2 = phi + 1
;; Divide both sides by phi to obtain phi = 1 + 1/phi

(display (fixed-point
          (lambda (phi) (+ 1 (/ 1 phi)))
          1.0))
(newline)

;;
;; Exercise 1.36
;;

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point-print
 (lambda (x) (/ (log 1000) (log x)))
 2)
(newline)
(fixed-point-print
 (lambda (x) (/ (+ x (/ (log 1000) (log x)) ) 2))
 2)
(newline)
(newline)

;; Takes about 35 steps without average damping and about 9 steps with average damping

;;
;; Exercise 1.37
;;

(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0) res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

(display (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    12))
(newline)

(define (cont-frac-rec n d k)
  (define (iter i)
    (if (= i k) (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(display (cont-frac-rec (lambda (i) 1.0)
                        (lambda (i) 1.0)
                        12))
(newline)
(newline)

;; Seems that k=12 is enough to get 4 decimal places of accuracy

;;
;; Exercise 1.38
;;

(define (euler-d i)
  (if (= (modulo i 3) 2) (* 2 (ceiling (/ i 3)))
      1))

(display (cont-frac (lambda (i) 1.0)
                    euler-d
                    100))
(newline)
(newline)

;;
;; Exercise 1.39
;;

(define (tan-cf x k)
  (define (tn i)
    (if (= i 1) x (- (* x x))))
  (define (td i)
    (- (* 2 i) 1))
  (cont-frac tn td k))

(display (tan-cf (atan 1.0) 100))
(newline)
(display (tan-cf (atan -.5) 100))
(newline)
(newline)

;;
;; Exercise 1.40
;;

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c)))

(display (newtons-method (cubic 1 1 1) 1))
(newline)
(newline)

;;
;; Exercise 1.41
;;

(define (double f)
  (lambda (x) (f (f x))))
(define (inc i) (+ 1 i))

(display ((double inc) 1))
(newline)
(display (((double (double double)) inc) 5))
(newline)
(newline)

;; (double double) -> double 2x -> 4x
;; (double (double double)) -> double 4x -> 16x
;; Adds 16 to 5 -> 21

;;
;; Exercise 1.42
;;

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(display ((compose square inc) 6))
(newline)
(newline)

;;
;; Exercise 1.43
;;

(define (repeated f n)
  (define (iter res i)
    (if (= i 1) res
        (iter (compose f res) (- i 1))))
  (iter f n))

(display ((repeated square 2) 5))
(newline)
(newline)

;;
;; Exercise 1.44
;;

(define sdx 0.1)
(define (smooth f)
  (lambda (x) (/ (+ (f x)
                    (f (+ x sdx))
                    (f (- x sdx)))
                 3)))

(display ((smooth square) 0))
(newline)
(display (((repeated smooth 5) square) 0))
(newline)
(newline)

;;
;; Exercise 1.45
;;

;; I looked it up. Average damp floor(log2(n)) times.

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nth-root x n)
  (fixed-point
   ((repeated average-damp
              (floor (/ (log n) (log 2))))
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

(display (nth-root 4 2))
(newline)
(display (nth-root 32 5))
(newline)
(display (nth-root 823543 7))
(newline)
(newline)

;;
;; Exercise 1.46
;;

(define (iterative-improve test improve)
  (define (iter guess)
    (if (test guess) guess
        (iter (improve guess))))
  iter)

(define (sqrt-ii x)
  ((iterative-improve
    (lambda (a) (< (abs (- (square a) x)) tolerance))
    (lambda (guess) (average guess (/ x guess))))
   1.0))

(define (fixed-point-ii f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve
    (lambda (a) (close-enough? (f a) a))
    f)
   first-guess))

(define (sqrt-fii x)
  (fixed-point-ii (average-damp (lambda (y) (/ x y))) 1.0))

(display (sqrt-ii 25))
(newline)
(display (sqrt-fii 25))
(newline)
