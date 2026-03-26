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
 (lambda (x) (/ (log 1000) (log x))))
(newline)
(fixed-point-print
 (lambda (x) (/ (log 1000) (log x))))
(newline)

;; TODO finish and test the above (second one needs to use averaging)
