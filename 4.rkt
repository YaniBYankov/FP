#lang racket
(define (sort-list xs)
  (λ (p) (sort xs p)))

(equal? ((sort-list '("one" "two" "0" "five" "" "one hundred" "onehundred")) (λ (s1 s2) (< (string-length s1) (string-length s2)))) '("" "0" "one" "two" "five" "onehundred" "one hundred"))


(define (my-length xs)
          (apply + (map (λ (x) 1) xs)))

(define (my-length-fold xs)
  (foldl (λ (x acc) (add1 acc )) 0 xs))

(= (my-length '()) 0)
(= (my-length '(1 2 5 6 4 8)) 6)

(= (my-length-fold '()) 0)
(= (my-length-fold '(1 2 5 6 4 8)) 6)

(define (get-smallest xs)
  (apply min xs))
(= (get-smallest '(1 2 5)) 1)
(= (get-smallest '(2 1 5)) 1)
(= (get-smallest '(2 1 -1 5)) -1)


(define (remove-first x xs)
  (remq x xs))



(equal? (remove-first 1 '(1 1 1 2)) '(1 1 2))
(equal? (remove-first 1 '(2 5 6)) '(2 5 6))
(equal? (remove-first 1 '(1)) '())
(equal? (remove-first 1 '(2 1)) '(2))
(equal? (remove-first "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN" "RNN"))

;(equal? (remove-all 1 '(1 1 1 2)) '(2))
;(equal? (remove-all 1 '(2 5 6)) '(2 5 6))
;(equal? (remove-all 1 '(1)) '())
;(equal? (remove-all 1 '(1 2 1 1)) '(2))
;(equal? (remove-all "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

(define (num-to-xs num)
  (define (helper current result)
    (cond
      [(zero? current) '()]
      [else (helper (quotient current 10) (cons '((remainder current 10)) result))]
      )
    )
  (if (zero? num) '(0) (helper num '()))
  )
(equal? (num-to-xs 123) '(1 2 3))
(equal? (num-to-xs 123456789) '(1 2 3 4 5 6 7 8 9))

;(= (xs-to-num '(1 2 3)) 123)
;(= (xs-to-num '(1 2 3 4 5 6 7 8 9)) 123456789)

