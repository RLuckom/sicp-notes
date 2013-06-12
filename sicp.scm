(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 2 4)

(define (pow num p)
  (if (= p 0)
      1
      (* num (pow num (- p 1)))))

(pow 2 16)


(define (rec-f n)
  (if (< n 3)
      n
      (+ (rec-f (- n 1))
         (* 2 (rec-f (- n 2)))
         (* 3 (rec-f (- n 3))))))

(rec-f 7)

(define (it-f n )
  (define (itf-h a b c i)
    (cond ((< n 3) n)
          ((> i n) b)
          (else (itf-h (+ a
                          (* 2 b)
                          (* 3 c))
                       a
                       b
                       (+ i 1)))))
  (itf-h 4 2 1 3))

(define (pascal n)
  (define (pascal-helper triangle c)
    (if (= c 0)
        triangle
        (pascal-helper (cons (make-row (car triangle))
                             triangle)
                       (- c 1))))
  (pascal-helper (list (list 1)) n))

(define (make-row last-row)
  (define (row-helper row accum)
    (if (eq? row '())
      accum
      (row-helper (cdr row) (cons (+ (car row)
                                     (if (eq? (cdr row) '()) 
                                       0
                                       (cadr row))) 
                                  accum))))
  (row-helper last-row (cons 1 '())))

(car (list (list 1 '())))

(define (recur-pascal n)
  (if (= n 0)
    (list 1)
    (make-row (recur-pascal (- n 1)))))


(cdr '(1))

(make-row (list 1 1))

(recur-pascal 100)



(pascal 23)

(it-f 7)


(define (cube x)
  (* x x x))

(cube 2)

(define (p x) 
  (- (* 3 x) (* 4 (cube x))))

;;log(n) space and time

(/ (/ (/ (/ (/ 12.15 3) 3) 3) 3) 3)

;;1.16
;;

(even? 3)

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 2 10)


(define (superfast-expt a b n)
  (cond ((= n 0) a)
        ((even? n) (superfast-expt (square a) b (/ n 2)))
        (else (superfast-expt (* a b) b (- n 1)))))

(define (ex b n) (superfast-expt 1 b n))

(ex 2 5)

(define (double x) (+ x x))

(define (fast-mult x n)
  (cond ((= n 1) x)
        ((even? n) (fast-mult (double x) (/ n 2)))
        (else (+ x (fast-mult x (- n 1))))))

(fast-mult 5 5)

(define (superfast-mult start odd n)
  (cond ((= n 1) (+ start odd))
        ((even? n) (superfast-mult (double start) odd (/ n 2)))
        (else (superfast-mult start (+ odd start) (- n 1)))))

(define (mult x y) (superfast-mult x 0 y))

(mult 3 6)

1 1

(define (fib-iter-h x g n)
  (if (= n 0) g
    (fib-iter-h (+ x g) x (- n 1))))

(define (fib-iter x) (fib-iter-h 1 1 x))


(fib-iter 1)

(define (fib-test a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-test a
                   b
                   (+ p q)
                   (+ q q p)
                   (/ count 2)))
        (else (fib-test (+ (* b q) (* a q) (* a p))
                        (+ (* a q) (* b p))
                        p
                        q
                        (- count 1)))))

(define (ez-fib x) (fib-test 1 0 0 1 x))

(ez-fib 10)


;; a <- aq + bq + ap
;; b <- bp + aq
;;
;; fib(2, 3)
;;
;;3 5
;;
;; 1 * 2 + 1 * 3 + 0 * 2
;;
;; 1 * 3 + 0 * 2
;;
;; 1 1 2 3 5 8 13 21 34
;;
;; 1 * 5 + 2 * 5 + 1 * 3
;;
;; 1 * 5 + 1 * 5 + 1 * 3
;; 1 * 3 + 1 * 5

(define (gcdf a b)
  (if (= b 0) a
      (gcdf b (remainder a b))))

(gcdf 10 15)

;; 1.20
;;
;; (gcd 206 40)
;;
;;normal-order
;;
;;(gcd 206 40)
;;
;;(gcd 40 (remainder 206 40))
;;
;;(gcd 6 (remainder 40 6))
;;
;;(gcd 4 (remainder 6 4))
;;
;;(gcd 2 (remainder 4 2))
;;
;;(gcd 206 40)
;;
;;(if (= 0 40) 206 (gcd 40 (remainder 206 40))
;;
;;Ended up looking it up. The answer is that it performs 18 evaluations of
;;remainder in the normal-order evaluation, 14 of which are in the conditional
;;predicate tests. My mistake was forgetting that even when the value of b was
;;evaluated in the conditional test, it was still unevaluated in the rest of the
;;expression. Therefore, the subsequent calls to gcd included
;;increasingly-nested remainder call, each of which had to be evaluated in the
;;predicate and possibly the result.

(define (smallest-divisor a)
  (prime? a 2))


(define (prime? n test)
  (cond ((> (square test) (square n)) n)
        ((= (remainder n test) 0) test)
        (else (prime? n (+ test 1)))))

(smallest-divisor 199)

(smallest-divisor 1999)

(smallest-divisor 19999)


(remainder (pow 5 13) 13)

(define (expmod2 base ex m)
  (cond ((= ex 0) 1)
        ((even? ex)
         (remainder (square (expmod base (/ ex 2) m)) m))
        (else (remainder (* base (expmod base (- ex 1) m)) m))))

(random 5)

(define (fermat-test n)
  (define (try-it m)
    (= (expmod m n n) m))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 9)

(define (test-prime candidate times)
  (cond ((= times 0) true)
        ((fermat-test candidate) (test-prime candidate (- times 1)))
        (else false)))

(smallest-divisor 19999)

(test-prime 13 13)

(define (runtime) (/ (current-inexact-milliseconds) 1000))

(runtime)

(define (is-prime n)
  (= (prime? n 2) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n time)
  (if (is-prime n)
    (report-prime (- (runtime) time))
    0))

(define (report-prime time)
  (display " *** ")
  (display time))

(timed-prime-test 1999)

(define (search-for-primes n)
  (define (run h)
    (timed-prime-test h)
    (if (< n h) 0
      (run (+ h 1))))
  (run 2))

(search-for-primes 100000)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))

;;1.29

(define (yk f a k h)
  (f (+ a (* k h))))

(yk cube 0 1000.0 (/ 1.0 1000.0))

(/ 1.0 1000)

(define (term-a num a f h)
  (define (term n)
    (cond ((= n 0) (yk f a n h))
          ((even? n) (+ (* 2 (yk f a n h)) (term (- n 1))))
          (else (+ (* 4 (yk f a n h)) (term (- n 1))))))
  (term num))

(define (integral f a b n)
  (* (/ (/ (- b a) n) 3.0)
     (term-a n a f (/ (- b a) n))))

(integral cube 0.0 1.0 10000)

(define (expmod base ex m)
  (remainder (fast-expt base ex) m))

(define (cube x) (* x x x))

(expmod 5 0 20)

(expmod2 5 0 20)

(define (sum-iter term a next b)
  (define (iter result a)
    (if (> a b) result
      (iter (+ result 
               (term a))
            (next a))))
  (iter 0 a))

(sum-iter (lambda (a) a) 0 (lambda (a) (+ a 1)) 10)

(+ 10 9 8 7 6 5 4 3 2 1)

(define (accumulate combiner null term a next b)
  (if (> a b) null 
      (combiner (term a) (accumulate combiner null term (next a) next b))))

(define (iter-accum combiner null term a next b)
  (define (iter n a)
    (if (> a b) n
      (iter (combiner n (term a)) (next a))))
  (iter null a))

(accumulate (lambda (x y) (+ x y)) 0 (lambda (x) x) 0 (lambda (x) (+ x 1)) 10)

(iter-accum + 0 (lambda (x) x) 0 (lambda (x) (+ x 1)) 10)

(define (accum-filter pred combiner null term a next b)
  (cond ((> a b) null)
        ((pred a) (combiner (term a) (accum-filter pred
                                                   combiner
                                                   null
                                                   term
                                                   (next a)
                                                   next
                                                   b)))
        (else (accum-filter pred combiner null term (next a) next b))))

(+ 2 4 6 8 10)

(accum-filter (lambda (x) (even? x))
              +
              0
              (lambda (x) x)
              0
              (lambda (x) (+ x 1))
              10)

(define (Y f)
  ((lambda (x) (lambda (v) (f (x x)) v)) 
   (lambda (x) (lambda (v) (f (x x)) v))))

(define (sqrt-helper f)
     (lambda (x)
         ((lambda (z)
            (if (< (abs (- x (* z z))) 0.0001) z
              (f x (/ (+ z (/ x z)) 2)))) 
          1)))

(define (fixer next eps a)
  (define f (x)
    (if (< (abs (- (next x) x)) eps) x
      (f (next x))))
  (f a)

((sqrt-helper 25) 4)

((Y sqrt-helper) 12)

((lambda (x) (+ x 1)) 1)

(define (sqrtt x)
  (define (g y)
    (if (< (abs (- x (* y y))) 0.0001) y
      (g (/ (+ y (/ x y)) 2))))
  (g 1.0))

(sqrtt 25)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00000001))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next) next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(/ 1.0 (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0))

(define (cont-frac n d k)
  (define (frac-help n d x)
    (if (> x k) 1
      (/ (n x) (+ (d x) (frac-help n d (+ x 1))))))
  (frac-help n d 1))

(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 50)

(define (deriv g dx)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))

((deriv cube 0.00001) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g 0.00001) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrtt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a (* x x)) (* b x) c)))

(newtons-method (cubic 2 2 2) 1)

(define (double f)
  (lambda (x)
    (f (f x))))

((double (lambda (x) (+ x 1))) 1)

(define (compose f g)
  (lambda (x) 
    (f (g x))))

((compose (lambda (x) (* x x)) (lambda (x) (+ x 1))) 6)

(define (repeated f x)
  (define (h f n)
    (if (= n 1) f
      (compose f (h f (- n 1)))))
  (h f x))

((repeated (lambda (x) (* x x)) 2) 5)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x 0.00001)) (f (+ x 0.00001))) 2)))

(define (n-smooth f n)
  ((repeated smooth n) f))

((n-smooth (lambda (x) x) 8) 1)

(define (make-rat n d)
  (define (r x y)
    (let ((g (gcdf x y)))
      (cons (/ x g) (/ y g))))
  (cond ((and (< 0 n) (< 0 d)) (r (* -1 n) (* -1 d)))
        ((or (< 0 n) (< 0 d)) (r (* -1 (abs n)) (abs d)))
        (else (r n d))))

(make-rat -3 1)

(make-rat -3 -1)

(make-rat 3 1)

(make-rat 9 -3)

(define (make-segment point-a point-b)
  (cons point-a point-b))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((s (start-segment segment))
        (e (end-segment segment)))
  (make-point (/ (+ (x-point s) (x-point e)) 2)
              (/ (+ (y-point s) (y-point e)) 2))))

(midpoint-segment (make-segment (make-point -1.0 1.0) (make-point 1.0 -2.0)))

(define (make-rect bottom-left width height)
  (let ((x (x-point bottom-left))
        (y (y-point bottom-left)))
  (cons (make-segment bottom-left (make-point (+ width x) y))
        (make-segment (make-point x (+ y height)) 
              (make-point (+ x width) (+ y height))))))

(perimeter (make-rect2 1.0 1.0))

(define (perimeter rect)
  (+ (* 2 (height rect)) (* 2 (width rect))))

(define (area rect)
  (* (height rect) (width rect)))

(define (height rect)
  (abs (- (cdr (cdr (cdr rect))) (cdr (car (car rect))))))

(define (width rect)
  (abs (- (car (cdr (car rect))) (car (car (car rect))))))

(define (make-rect2 height width)
  (cons height width))

(define (height rect)
  (car rect))

(define (width rect)
  (cdr rect))

(define (c x y)
  (lambda (m) (m x y)))

(define (cr c)
  (c (lambda (p q) p)))

(define (cd c)
  (c (lambda (p q) q)))

(cr (c 7 8))

(cd (c 7 8))


(define (cc a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(cc 1 2)

(remainder 123 2)

(define (ch pred nextx nexty firsty)
(lambda (x)
  (define (c2 x y)
    (if (pred x) y
      (c2 (nextx x) (nexty y))))
  (c2 x firsty)))

(define (car2 x)
  ((ch (lambda (z) (= (remainder z 2) 1))
       (lambda (z) (/ z 2))
       (lambda (z) (+ z 1))
       0) x))

(define (cdr3 x)
  ((ch (lambda (z) (not (= (remainder z 3) 0)))
       (lambda (z) (/ z 3))
       (lambda (z) (+ z 1))
       0) x))

(car2 (cc 5 8))

(cdr3 (cc 5 8))

(define church-one (lambda (f) (lambda (x) (f x))))

(define church-two (lambda (f) (lambda (x) (f (f x)))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (car interval))

(define (lower-bound interval) (cdr interval))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(define (span-zero? interval)
  (< (* (lower-bound interval)
        (upper-bound interval))
     0))

(define (div-interval x y)
  (if (span-zero? interval) (error "Spans zero")
    (div-int x y)))

(error "?")

(define (make-center-percent c p)
  (cons (+ c (* c p)) (- c (* c p))))

(define (percent interval)
  (let ((hw (/ (- (upper-bound interval) (lower-bound interval)) 2)))
    (/ hw (- (upper-bound interval) hw))))

(percent (make-center-percent 10 0.1))

(define (last-pair l)
  (if (= (length l) 1) l
    (last-pair (cdr l))))

(last-pair (list 23 72 149 34))

(define (rev l)
  (define (r l a)
    (if (null? l) a
      (r (cdr l) (cons (car l) a))))
  (r l '()))

(rev (list 1 4 9 16 25))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define (no-more? coins)
  (null? coins))

(define (same-parity x . r)
  (define (sp p l a)
    (cond ((null? l) (rev a))
          ((= p (remainder (car l) 2)) (sp p (cdr l) (cons (car l) a)))
          (else (sp p (cdr l) a))))
  (sp (remainder x 2) r (list x)))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

(define (square-list items)
  (if (null? items) '()
    (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4 5 6))

(define (square-list2 items)
  (map square items))

(square-list2 (list 1 2 3 4 5 6))

(define (fe f l)
  (if (map f l) 1
    '()))

(fe (lambda (x) (newline) (display x) (newline)) (list 1 3 4 7 87))

(cons (list 2 3) (list 3 4 (list 5 6)))

(car (cdaddr (list 1 3 (list 5 7) 9)))

(caar (list (list 7)))

(cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)

(cons x y)

(list x y)

(define (rev l)
  (define (rrev k a)
    (cond ((null? k) a)
          ((not (pair? (car k))) (rrev (cdr k) (cons (car k) a)))
          (else (rrev (cdr k) 
                      (if (not (null? a)) (list (rrev (car k) '()) a)
                        (rrev (car k) '()))))))
  (rrev l '()))

(rev (list (list (list 1 2) (list 3 4)) (list 6 7) 9))

(define (fringe tree)
  (define (f t a)
    (cond ((null? t) a)
          ((pair? t) (f (cdr t) (f (car t) a)))
          (else (append a (list t)))))
  (f tree '()))

(fringe (list (list (list 1 2) (list 3 4)) (list 6 7) 9))

(define (make-mobile left right)
  (cons left right))

(define (make-branch len lb)
  (cons len lb))

(define (get-len branch)
  (car branch))

(define (get-lb branch)
  (cdr branch))
(define (get-left m)
  (car m))
(define (get-right m)
  (cdr m))
(define (branch? b)
  (not (pair? (get-left b))))
(define (mobile? m)
  (and (pair? (get-left m)) (pair? (get-right m))))

(define (leaf? l)
  (not (pair? (get-right l))))

(define (total-lb m)
  (define (h k a)
    (cond ((leaf? k) (+ a (get-lb k)))
          ((branch? k) (h (get-lb k) a))
          (else (h (get-right k) (h (get-left k) a)))))
  (h m 0))

a

(define (balanced? m)
  (cond ((null? m) (= 1 1))
        ((and (mobile? m) 
              (not (= (* (get-len (get-left m)) (total-lb (get-left m)))
                      (* (get-len (get-right m)) (total-lb (get-right m))))))
         (= 2 1))
        ((branch? m) (= 1 1))
        (else (and (balanced? (get-left m))
                   (balanced? (get-right m))))))

(pair? '())

(mobile? (list (list 2 4 5 ) (list 5)))

(make-mobile ((make-branch 3 
                        (make-mobile (make-branch 2 1) 
                                        (make-branch 5 6)))
              (make-branch 4 9)))

(total-lb (make-mobile (make-branch 3 (make-mobile (make-branch 4 5) 
                                                   (make-branch 4 5)))
                       (make-branch 4 5)))

(get-right (make-branch 4 (make-mobile (make-branch 2 2) (make-branch 2 2))))

(cdr (cons 1 3))

(balanced? (make-mobile (make-branch 2 4) 
                        (make-branch 2 (make-mobile (make-branch 2 2)
                                                    (make-branch 2 2)))))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) 
                    (square-tree (cdr tree))))))

(square-tree (list 5 (list 5 (list (list 7 8) 6))))

(define (square-tree-map tree)
  (map (lambda (t) (if (pair? t) (square-tree-map t)
                     (square t)))
       tree))

(square-tree-map (list 5 (list 5 (list (list 7 8) 6))))

(define (tree-map f t)
  (define (tm t)
    (map (lambda (t) (if (pair? t) (tm t)
                       (f t)))
    t))
  (tm t))

(define (tt tree) (tree-map square tree))

(tt (list 5 (list 5 (list (list 7 8) 6))))

(define (subsets s)
  (if (null? s) (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (l) (append (list (car s)) l)) rest)))))

(subsets (list 1 2 3))

(define (accumulate op init l)
  (if (null? l) init
    (op (car l)
        (accumulate op init (cdr l)))))

(accumulate + 0 (list 1 2 3 4 5))

(define (m f seq)
  (accumulate (lambda (x y) (cons (f x) y)) '() seq))

(m (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (a seq1 seq2)
  (accumulate cons seq2 seq1))

(a (list 1 2 3 4) (list 5 6 7 8))

(define (l seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(l (list 1 2 3 4 5))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (* (+ this-coeff higher-terms)
                                                   x))
              0
              coefficient-sequence))

(horner-eval 2 (list 0 1 2))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (fringe t)))

(count-leaves (list 4 (list 3 (list (list 5 6) 7))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3)
             (list 1 2 3))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(matrix-*-vector (list (list 1 2 3 4) (list 4 5 6 6) (list 7 8 9 10))
                 (list 1 2 3 4))

(define (transpose m)
  (accumulate-n cons '() m))


(transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix (list (list 1 1) (list 1 1))
                 (list (list 1 1) (list 1 0)))

(define (fl op initial seq)
  (define (iter result rest)
    (if (null? rest) result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial seq))

(accumulate / 1 (list 1 2 3))

(/ 3 (/ 2 (/ 1)))

(fl / 1 (list 1 2 3)) 

(/ 1 (/ 2 (/ 3)))

(accumulate list '() (list 1 2 3))

(fl list '() (list 1 2 3))

(define (r seq)
  (accumulate (lambda (x y) (append y (list x))) '() seq))

(r (list 1 2 3))

(define (lr seq)
  (fl (lambda (x y) (cons y x)) '() seq))

(lr (list 1 2 3))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval n i)
  (define (enum x accum)
    (if (= x i) accum
      (enum (- x 1) (cons x accum))))
  (enum (- n 1) '()))

(enumerate-interval 8 5)

(define (unique-pairs n)
  (flatmap (lambda (i)
               (map 
                 (lambda (x) (list i x)) 
                 (enumerate-interval i)))
               (enumerate-interval n)))

(flatmap (lambda (i) 
   (map (lambda (x) (list i x)) (enumerate-interval i))) 
 (list 1 2))

(unique-pairs 7)

(define (triples n target)
  (filter (lambda (x) (= (accumulate + 0 x) target)) (combos n 3)))

(define (combos n d)
  (define (f m accum)
    (if (= m 1) accum
        (f (- m 1) 
           (flatmap (lambda (x)
                      (map (lambda (y) (cons y x)) 
                           (enumerate-interval n (car x))))
                    accum))))
  (map reverse (f d (map list (enumerate-interval n 0)))))

(triples 6 9)
