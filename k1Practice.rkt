#lang racket
;practce for first Kontrolno


(define (devides? x y)
  (if (= (remainder y x) 0) #t #f))

(define (factorise n)
  (define (helper curr res leftOver)
    (cond
    [(= leftOver 1) res]
    [(devides? curr leftOver) (helper curr (cons curr res) (quotient leftOver curr))]
    [else (helper (+ 1 curr) res leftOver)]))
  (reverse (helper 2 '() n))
  )

(factorise 12356498)

(equal? (factorise 2) '(2))
(equal? (factorise 6) '(2 3))
(equal? (factorise 13) '(13))
(equal? (factorise 123) '(3 41))
(equal? (factorise 152) '(2 2 2 19))
(equal? (factorise 12356498) '(2 7 11 19 41 103))

;week 2

; 1



(define (fib-recursive n)
  (cond
    [(zero? n) 0]
    [(= 1 n) 1]
    [else (+ (fib-recursive (- n 1)) (fib-recursive (- n 2)))]
    )
  )

(define (fib-iterative n)
  (define (helper n0 n1 res i)
    (cond
      [(zero? n) n0]
      [(= 1 n) n1]
      [(>= i n) res]
      [else (helper n1 (+ n1 n0) (+ n1 n0) (+ i 1))]))
  (helper 0 1 0 1)
  )
 
(= (fib-recursive 12) 144)
(= (fib-iterative 12) 144)

; 2
(define (help-galena new lost goal)
  (let ([net (- new lost)])
  (define (helper curr res)
    (if (>= curr goal) res (helper (+ curr net) (+ res 1))))
    (helper 0 -1))  
  )

;(help-galena 100000 10000 910000)
(= (help-galena 100000 10000 910000) 10)

; 4

(define (rev n)
  (define (helper  from to )
    (cond
      [(zero? from) to]
      [else (helper (quotient from 10) (+ (remainder from 10) (* to 10)))]
      )
    ) 
  (helper n 0)
  )

(= (rev 0) 0)
(= (rev 123) 321)
(= (rev 987654321) 123456789)
(= (rev 12000) 21)

; 5


(define (my-recursive-pow x n)
  (define (helper res currN)
    (if (zero? currN) res (helper (* res x) (- currN 1))))
  (helper 1 n)
  )

(= (my-recursive-pow 2 5) 32)
(= (my-recursive-pow 15 3) 3375)

; 6

(define (prime? n)
  (define (helper curr)
    (cond
      [(= n 1) #f]
      [(> curr (- n 1)) #t]
      [(zero? (remainder n curr)) #f]
      [else (helper (+ curr 1))]))
  (helper 2))

(equal? (prime? 1) #f)
(equal? (prime? 2) #t)
(equal? (prime? 3) #t)
(equal? (prime? 6) #f)
(equal? (prime? 42) #f)
(equal? (prime? 61) #t)

; 7

(define (increasing-digits? x)
  (cond
    [(zero? (quotient x 10)) #t]
    [(< (remainder x 10) (remainder (quotient x 10) 10)) #f]
    [else (increasing-digits? (quotient x 10))])
  )

(equal? (increasing-digits? 1244) #t)
(equal? (increasing-digits? 12443) #f)

; 8

(define (palindromes-between a b)
  (define (isPalindromes x)
    (if (= x (rev x)) #t #f))
  (define (helper curr res)
    (cond
      [(> curr b) res]
      [(isPalindromes curr) (helper (+ curr 1) (+ res 1))]
      [else (helper (+ curr 1) res)]))
  (if (< a b) (helper a 0) (palindromes-between b a)))

(= (palindromes-between 1 101) 19)
(= (palindromes-between 1 100) 18)
(= (palindromes-between 100 1) 18)


; 9



(define (sum-first-special-numbers n d)
  (define (findNextPrime x)
    (if (prime? (+ 1 x)) (+ 1 x) (findNextPrime (+ 1 x))))
  (define (containsDigit x d)
    (cond
      [(= d (remainder x 10)) #t]
      [(= (quotient x 10) 0) #f]
      [else (containsDigit (quotient x 10) d)]))
  (define (helper curr remaining res)
    (cond
      [(<= remaining 0) res]
      [(containsDigit curr d) (helper (findNextPrime curr) (- remaining 1) (+ res curr))]
      [else  (helper (findNextPrime curr) remaining res)]))
  (helper 1 n 0)
  )


(= (sum-first-special-numbers 5 2) 392)
(= (sum-first-special-numbers 5 3) 107)
(= (sum-first-special-numbers 10 3) 462)

; 10

(define (automorphic? n)
  (let ([pow (my-recursive-pow n 2)])
    (define (helper currNumber currPow)
      (cond
        [(not (= (remainder currNumber 10) (remainder currPow 10))) #f]
        [(and (< currNumber 10) (= (remainder currNumber 10) (remainder currPow 10))) #t]
        [(zero? currNumber) #f]
        [else (helper (quotient currNumber 10) (quotient currPow 10))]))
    (helper n pow)
    ))

(equal? (automorphic? 3) #f)
(equal? (automorphic? 10) #f)
(equal? (automorphic? 5) #t)
(equal? (automorphic? 25) #t)
(equal? (automorphic? 76) #t) 
(equal? (automorphic? 890625) #t) 
(equal? (automorphic? 625) #t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)


; 11


(define (subnumber? sub num)
  (define (getLastNDigits num n)
    (remainder num (my-recursive-pow 10 n)))
  (define (countDigits n)
    (define (helper curr res)
      (if (zero? curr) res (helper (quotient curr 10) (+ 1 res))))
    (helper n 0))
  (define (helper curr)
    (cond
      [(< (countDigits curr) (countDigits sub)) #f]
      [(= (getLastNDigits curr (countDigits sub)) sub) #t]
      [else (helper (quotient curr 10))]))
  (helper num))  

(equal? (subnumber? 123 5123783) #t)
(equal? (subnumber? 0 0) #t)
(equal? (subnumber? 10 101) #t)
(equal? (subnumber? 101 101) #t)
(equal? (subnumber? 10 0) #f)
(equal? (subnumber? 1253 5123783) #f)
(equal? (subnumber? 12 0) #f)

; 12
 (define (sameDigits x)
    (cond
      [(= (remainder (quotient x 10) 10) (remainder x 10)) (sameDigits (quotient x 10))]
      [else (if (< x 10) #t #f)])
    )

(define (sumDigits x)
    (define (helper curr res)
      (if (= curr 0) res (helper (quotient curr 10) (+ res (remainder curr 10)))))
    (helper x 0)
    )

(sumDigits 11)

(define (digital-root x)
  (cond
    [(< x 10) x]
    [else (digital-root (sumDigits x))])
  )

(= (digital-root 5) 5)
(= (digital-root 39) 3)
(= (digital-root 493193) 2)

; 13
(define (remove-first-occurrence n x)
  (cond
    [(= n x) 0]
    [(< n 10) n]
    [(= (remainder n 10) x) (quotient n 10)]
    [else (+ (* 10 (remove-first-occurrence (quotient n 10) x)) (remainder n 10))]
    )
  )

(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)

; week 3

; 3

(define (apply-n f n)
  (define (helper cnt res)
    (if (> cnt n)
        res
        (helper (+ 1 cnt) (f res)))
    )
  (λ (x) (helper 1 x))
  )

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)

; 4

(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (cool-expression n)
  (accumulate-i + 0 2 n (λ (x) (* x x x)) (λ (x) (+ x 3)))
  )

(= (cool-expression 11) 1976)
(= (cool-expression 15) 4720)

; 5

(define (factorial n)
  (accumulate-i * 1 1 n (λ (x) x) (λ (x) (+ x 1))))

(= (factorial 5) 120)
(= (factorial 8) 40320)

; 6

(define (prime2? x)
  (if (= x 1)
      #f
  (accumulate-i (λ (x1 x2) (and x1 x2)) #t 2 (- x 1) (λ (dev) (not (= (remainder x dev) 0))) (λ (a) (+ 1 a)))))

(equal? (prime2? 1) #f)
(equal? (prime2? 2) #t)
(equal? (prime2? 3) #t)
(equal? (prime2? 6) #f)
(equal? (prime2? 42) #f)
(equal? (prime2? 61) #t)

; 7

(define (all? a b f)
  (accumulate-i (λ (x y) (and x y)) #t a b (λ (x) (f x)) (λ (x) (+ x 1))))

(equal? (all? 100 999 (λ (x) (< x 1000))) #t)
(equal? (all? 1 100 odd?) #f)

; 8

(define (any? a b f)
(accumulate-i (λ (x y) (or x y)) #f a b (λ (x) (f x)) (λ (x) (+ x 1))
  ))

(equal? (any? 1001 1500 (λ (x) (< x 1000))) #f)
(equal? (any? 1 100 odd?) #t)

; 9

(define (argmin f a b)
  (accumulate-i (λ (x y) (if (< (f x) (f y)) x y)) a a b (λ (x) x) (λ (x) (+ x 1))) 
  )

(define (mod7 x) (remainder x 7))
(= (argmin mod7 45 50) 49)

; 10

(define (count-digits n)
  (if (zero? n)
      1
  (accumulate-i + 0 1 n (λ (x) 1) (λ (x) (* x 10))))
  )

(= (count-digits 12345) 5)
(= (count-digits 0) 1)

; 11

#|
(define (any? a b pred?)
  (accumulate (λ (x y) (or x y)) #f a b pred? add1)
  )
|#

(define (count-pairs a b n)
  (accumulate + 0 a b (λ (x) (if (any? x b (λ (y) (= (+ x y) n))) 1 0)) add1)
  )

(= (count-pairs 1 10 14) 4)

; week 4

;12

(define (get-ascending from xs)
  (define (helper curr res)
    (if (or (= (+ 1 curr) (length xs)) (> (list-ref xs curr) (list-ref xs (+ curr 1))))
        res
        (helper (+ curr 1) (cons (list-ref xs (+ 1 curr)) res))
        ))
    (if (> from (- (length xs) 1)) 
        '()
        (cons (list-ref xs from) (reverse (helper from '())))))

#|
(get-ascending 4 '(1 5 2 3 1 5 6 7 7 1 5))

                
               (define (longest-ascending-sub xs)
               (define (helper curr from maxLn best)
                 (cond 
                   [(>= (+ 1 (length curr)) (length xs)) (list-ref xs curr)]
                   [(> (length curr) (maxLn)) (helper (get-ascending (+ 1 (from)) xs) (+ 1 from) (length curr) curr)]
                   [else (helper (get-ascending (+ 1 (from)) xs) (+ 1 from) maxLn best)]))
               (helper (get-ascending 0 xs) 0 0 '()))


(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))

;11

(define (devides? x y)
  (if (= (remainder y x) 0) #t #f))

(define (factorise n)
  (define (helper curr res leftOver)
    (cond
    [(= leftOver 1) res]
    [(devides? curr leftOver) (helper curr (cons curr res) (quotient leftOver curr))]
    [else (helper (+ 1 curr) res leftOver)]))
  (reverse (helper 2 '() n))
  )

(factorise 12356498)

(equal? (factorise 2) '(2))
(equal? (factorise 6) '(2 3))
(equal? (factorise 13) '(13))
(equal? (factorise 123) '(3 41))
(equal? (factorise 152) '(2 2 2 19))
(equal? (factorise 12356498) '(2 7 11 19 41 103))
|#

; 10

(define (concat xs1 xs2)
  (if (empty? xs1)
      xs2
      (cons (car xs1) (concat (cdr xs1) xs2))
      )
  )

(cons (cons '(1) '(2 3 4)) '(2 3 4))

(equal? (concat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

;9
#|
(define (insert-at n at xs)
  (cond
    [(zero? at) (cons n xs)]
    [(empty? xs) (list n)]
    [else (cons (car xs) (insert-at n (- at 1) (cdr xs)))]))

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
|#
; 8

(define (kth-max-negative xs)
  (λ (k) (list-ref (remove-duplicates(sort xs <)) (- k 1)))) 


(= ((kth-max-negative '(-1)) 1) -1)
(= ((kth-max-negative '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-negative '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)

; 7

(define (my-reverse-foldl xs)
  (foldl (λ (x ys) (cons x ys)) '() xs))

 (my-reverse-foldl '(1 2 3 4 5))

;week 5

; 1

(take '(1 2 3 4) 0)
(take '(1 2 3 4) 2)
(drop '(1 2 3 4) 0)
(drop '(1 2 3 4) 2)

(define (insert-at x position xs)
  (flatten (cons (take xs (- position 1)) (cons x (drop xs (- position 1)))))  
  )

 (insert-at 0 2 '(10 20 30)) '(10 0 20 30)

(equal? (insert-at 0 1 '(10 20 30)) '(0 10 20 30))
(equal? (insert-at 0 2 '(10 20 30)) '(10 0 20 30))
(equal? (insert-at 0 3 '(10 20 30)) '(10 20 0 30))
(equal? (insert-at 0 4 '(10 20 30)) '(10 20 30 0))
(equal? (insert-at 0 1 '()) '(0))

; 2


(define (get-sublist start end xs)
  (take (drop xs start) (- end 1)))

(get-sublist 2 6 '(1 2 2 3 1 5 6 7 7))

(equal? (get-sublist 2 6 '(1 2 2 3 1 5 6 7 7)) '(2 3 1 5 6))

; 3

(define (count-occurrences xs ys)
  (define (helper curr res)
  (cond  
    [(< (length curr) (length xs)) res]  
    [(equal? (take curr (length xs)) xs) (helper (cdr curr) (+ 1 res))]
    [else (helper (drop curr (- (length xs) 1)) res)]) ) 
    (helper ys 0))
(count-occurrences '(1 5) '(1 5 2 3 1 5 6 7 7 1 5))
(= (count-occurrences '(1 5) '(1 5 2 3 1 5 6 7 7 1 5)) 3)
(= (count-occurrences '(5 5) '(5 5 5 3 1 5 6 7 5 5 5)) 4)
(= (count-occurrences '(6 6) '(2 2)) 0)

; 4



