;;
;; Exercise 1.9
;;

;; First (recursive):
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; Second (iterative):
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;;
;; Exercise 1.10
;;

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; Values

;; First: 1024
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; ... = 2^10
(display (A 1 10))
(newline)

;; Second: 65536
;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; ... = 2^2^4 = 2^2^2^2
(display (A 2 4))
(newline)

;; Third: 65536
;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 (A 1 (A 1 1)))
;; (A 2 (A 1 2))
;; (A 2 (A 0 (A 1 1)))
;; (A 2 (A 0 2))
;; (A 2 4)
;; = Second case
(display (A 3 3))
(newline)
(newline)

;; Definitions
;; f(n) = 2n
;; g(n) = 2^n
;; h(n) = 2^^n (tetration, 2^2^...^2 n times)

;;
;; Exercise 1.11
;;

(define (f1.11-rec n)
  (if (< n 3)
      n
      (+ (f1.11-rec (- n 1))
         (* 2 (f1.11-rec (- n 2)))
         (* 3 (f1.11-rec (- n 3))))))

(define (f1.11-iter a b c count)
  (if (= count 0)
      c
      (f1.11-iter (+ a (* 2 b) (* 3 c))
                  a
                  b
                  (- count 1))))

(define (f1.11 n)
  (f1.11-iter 2 1 0 n))

(display (f1.11-rec 5))
(newline)
(display (f1.11-rec 10))
(newline)
(display (f1.11-rec 12))
(newline)
(display (f1.11 5))
(newline)
(display (f1.11 10))
(newline)
(display (f1.11 12))
(newline)
(newline)

;;
;; Exercise 1.12
;;

(define (pascal row idx)
  (cond ((<= row 1) 1)
        ((<= idx 1) 1)
        ((>= idx row) 1)
        (else (+ (pascal (- row 1) (- idx 1))
                 (pascal (- row 1) idx)))))

(display (pascal 5 1))
(newline)
(display (pascal 5 2))
(newline)
(display (pascal 5 3))
(newline)
(display (pascal 5 4))
(newline)
(display (pascal 5 5))
(newline)
(newline)

;;
;; Exercise 1.13
;;

;; Base cases:
;; fib(0) = 0, (phi^0 - psi^0)/sqrt(5) = 0
;; fib(1) = 1, (phi^1 - psi^1)/sqrt(5) = ((1 + sqrt(5)) - (1 - sqrt(5)))/2sqrt(5) = 2sqrt(5)/2sqrt(5) = 1

;; Induction
;; fib(n-2) = (phi^(n-2) - psi^(n-2))/sqrt(5)
;; fib(n-1) = (phi^(n-1) - psi^(n-1))/sqrt(5)
;; fib(n) = fib(n-2) + fib(n-1)
;;        = (phi^(n-2) - psi^(n-2))/sqrt(5) + (phi^(n-1) - psi^(n-1))/sqrt(5)
;; Golden ratio: phi^2 = phi + 1, so phi^n = phi^(n-1) + phi^(n-1)
;; True for conjugate: psi^2 = (1 - 2sqrt(5) + 5) / 4 = (4 + 2 - 2sqrt(5)) / 4 = 1 + (1 - sqrt(5))/2 = 1 + psi
;; Thus fib(n) = (phi^n + psi^n) / sqrt(5)

;;
;; Exercise 1.14
;;

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(display (count-change 11))
(newline)
(newline)

;; (cc 11 5)
;;   (cc -39 5) = 0
;;   (cc 11 4)
;;     (cc -14 4) = 0
;;     (cc 11 3)
;;       (cc 1 3)
;;         (cc -9 3) = 0
;;         (cc 1 2)
;;           (cc 1 1)
;;             (cc 0 1) = 1
;;             (cc 1 0) = 0
;;       (cc 11 2)
;;         (cc 6 2)
;;           (cc 1 2)
;;             (cc -4 2) = 0
;;             (cc 1 1)
;;               (cc 0 1) = 1
;;               (cc 1 0) = 0
;;           (cc 6 1)
;;             (cc 5 1)
;;               (cc 4 1)
;;                 (cc 3 1)
;;                   (cc 2 1)
;;                     (cc 1 1)
;;                       (cc 0 1) = 1
;;                       (cc 1 0) = 0
;;                     (cc 1 0) = 0
;;                   (cc 2 0) = 0
;;                 (cc 3 0) = 0
;;               (cc 4 0) = 0
;;             (cc 5 0) = 0
;;           (cc 6 0) = 0
;;         (cc 11 1)
;;           (cc 10 1)
;;             (cc 9 1)
;;               (cc 8 1)
;;                 (cc 7 1)
;;                   (cc 6 1)
;;                     (cc 5 1)
;;                       (cc 4 1)
;;                         (cc 3 1)
;;                           (cc 2 1)
;;                             (cc 1 1)
;;                               (cc 0 1) = 1
;;                             (cc 1 0) = 0
;;                           (cc 2 0) = 0
;;                         (cc 3 0) = 0
;;                       (cc 4 0) = 0
;;                     (cc 5 0) = 0
;;                   (cc 6 0) = 0
;;                 (cc 7 0) = 0
;;               (cc 8 0) = 0
;;             (cc 9 0) = 0
;;           (cc 10 0) = 0
;;         (cc 11 0) = 0

;; Analysis
;; Time:
;;   n^5 is an upper bound if we let the problem size be reduced by 1 for each type of coin
;;   (n/50)^5 is a lower bound if we let the problem size be reduced by 50 for each type of coin
;;   so theta(n^5)
;; Space:
;;   Depth for each sub-tree is proportional to n depending on the coin, so theta(n)

;;
;; Exercise 1.15
;;

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(display (sine 12.15))
(newline)
(newline)

;; a.
;; (sine 12.15)
;;   (p (sine (4.05)))
;;     (p (p (sine 1.35)))
;;       (p (p (p (sine 0.45))))
;;         (p (p (p (p (sine 0.15)))))
;;           (p (p (p (p (p (sine 0.05))))))
;; 5 times

;; b.
;; Each iteration step divides the problem size by 3, so the space and time growth are logarithmic
;; in a.

;;
;; Exercise 1.16
;;

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

(define (fast-expt-i b n)
  (fast-expt-iter 1 b n))

(display (fast-expt 3 8))
(newline)
(display (fast-expt-i 3 8))
(newline)
(display (fast-expt 3 9))
(newline)
(display (fast-expt-i 3 9))
(newline)
(display (fast-expt 3 10))
(newline)
(display (fast-expt-i 3 10))
(newline)
(newline)

;;
;; Exercise 1.17
;;

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (fast-mul a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- b 1))))))

(display (fast-mul 7 3))
(newline)
(display (fast-mul 7 4))
(newline)
(display (fast-mul 8 3))
(newline)
(display (fast-mul 8 4))
(newline)
(newline)

;;
;; Exercise 1.18
;;

(define (fast-mul-iter q a b)
  (cond ((= b 0) q)
        ((even? b) (fast-mul-iter q (double a) (halve b)))
        (else (fast-mul-iter (+ q a) a (- b 1)))))

(define (fast-mul-i a b)
  (fast-mul-iter 0 a b))

(display (fast-mul-i 7 3))
(newline)
(display (fast-mul-i 7 4))
(newline)
(display (fast-mul-i 8 3))
(newline)
(display (fast-mul-i 8 4))
(newline)
(newline)

;;
;; Exercise 1.19
;;

;; Using matrix-vector multiplication:
;; [ q+p q ] [ a ] = [ 1 1 ] [ a ] = [ a + b ]
;; [ q   p ] [ b ] = [ 1 0 ] [ b ]   [ a     ]
;; square the matrix:
;; [ (q+p)^2+q^2 q(q+p)+pq ]
;; [ q(q+p)+pq   p^2+q^2   ]
;; so p' = p^2 + q^2
;;    q' = q(q+p)+pq = q^2+2pq

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))       ; compute p'
                   (+ (* q q) (* 2 (* p q))) ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(display (fib 3))
(newline)
(display (fib 4))
(newline)
(display (fib 5))
(newline)
(display (fib 6))
(newline)
(display (fib 7))
(newline)
(display (fib 8))
(newline)
(display (fib 9))
(newline)
(display (fib 10))
(newline)
(newline)

;;
;; Exercise 1.20
;;

;; Normal order
;; 0 (gcd 206 40)
;; 1 (gcd 40 (remainder 206 40))
;; 2 (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; 3 (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; 4 (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; 1: evaluates b = (remainder 206 40) (1)
;; 2: evaluates b = (remainder 40 (remainder 206 40)) (2)
;; 3: evaluates b = (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (4)
;; 4: evaluates b = (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (7)
;; bs: 14
;; Then evaluates a = (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;; a: 4
;; total: 18

;; Applicative order:
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remaidner (40 6)))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; (gcd 2 (remainder 4 2))
;; (gcd 2 0)
;; 2
;; total: 4

;;
;; Exercise 1.21
;;

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(display (smallest-divisor 199))
(newline)
(display (smallest-divisor 1999))
(newline)
(display (smallest-divisor 19999))
(newline)
(newline)

;;
;; Exercise 1.22
;;

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (cdr (gettimeofday))))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (cdr (gettimeofday)) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (iter start)
    (when (<= start end)
      (timed-prime-test start)
      (iter (+ start 1))))
  (iter start))

;; (search-for-primes 1000 1100)
;; (newline)
;; (search-for-primes 10000 10100)
;; (newline)
;; (search-for-primes 100000 100100)
;; (newline)
;; (newline)

;; The time resolution on my Mac is not that good, so it's difficult to tell

;;
;; Exercise 1.23
;;

;; I'm not doing it because of the timing issue, but I doubt it's much faster since we aren't
;; actually skipping iterations.

;;
;; Exercise 1.24
;;

;; Same issue, but this one probably works

;;
;; Exercise 1.25
;;

;; Since the remainder function is used at each iteration, the numbers will get very large. The
;; large numbers could lead to slower results or runtime errors.

;;
;; Exercise 1.26
;;

;; Now expmod has to be evaluated twice (once for each argument to the *) when it was previously
;; only evaluated once (as the argument to square). I think he would be right for normal order
;; evaluation, however. So now we have O(n) = 2 O(n/2) which expands to 1 + 2 + 4 + 8 + ... + logn,
;; which can be solved as log(2^n) = O(n).

;;
;; Exercise 1.27
;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (carmichael-test n)
  (define (iter a)
    (or (>= a n)
        (and (= a (expmod a n n))
             (iter (+ a 1)))))
  (iter 1))

(display (carmichael-test 561))
(newline)
(display (carmichael-test 1729))
(newline)
(display (carmichael-test 2465))
(newline)
(display (carmichael-test 2821))
(newline)
(display (carmichael-test 6601))
(newline)
(newline)

;;
;; Exercise 1.28
;;

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
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime-mr? n (- times 1)))
        (else #f)))

(display (fast-prime-mr? 9 1000))
(newline)
(display (fast-prime-mr? 10 1000))
(newline)
(display (fast-prime-mr? 7 1000))
(newline)
(display (fast-prime-mr? 11 1000))
(newline)

;; Gets them all
(display (fast-prime-mr? 561 1000))
(newline)
(display (fast-prime-mr? 1729 1000))
(newline)
(display (fast-prime-mr? 2465 1000))
(newline)
(display (fast-prime-mr? 2821 1000))
(newline)
(display (fast-prime-mr? 6601 1000))
(newline)
