#lang racket
; Примерна тема за първо контролно (2021/22 г.)
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

#|
Задача 1. Едно естествено число наричаме свършено, ако е с 2 по-малко от сумата на всичките си делители по-малки от него.

а) (3 т.) Да се реализира функция done?, която проверява дали дадено число е свършено.

б) (7 т.) Да се реализира функция sum-almost-done, която по подадени естествени числа a и b намира сумата на всички числа в интервала [a; b], които са по-близко до свършено число, отколкото до краищата на интервала.

Примери:

(done? 20) → #t
(done? 28) → #f

(sum-almost-done 5 24) → 153 ; сумата на числата от 13 до 21
|#



(define (findAllDiv n)
  (define (helper curr res)
         (cond
           [(>= curr n) res]
           [(zero? (remainder n curr)) (helper (+ 1 curr) (cons curr res))]
           [else (helper (+ 1 curr) res)]))
  (helper 1 '())
  )

(define (done? n)
  (= (- (apply + (findAllDiv n)) 2) n))
(done? 20)
(done? 28)

(define (findSmallestDoneIn a b)
  (define (helper a b curr)
  (cond
    [(>= curr b) -1]
    [(done? curr) curr]
    [else (helper a b (+ 1 curr))]))
  (helper a b a))

(define (findLargestDoneIn a b)
  (define (helper a b curr)
  (cond
    [(<= curr b) -1]
    [(done? curr) curr]
    [else (helper a b (- curr 1))]))
  (helper b a b))


(findSmallestDoneIn 5 24)
(findLargestDoneIn 5 24)


(define (sum-almost-done a b)
  (define (shouldAdd x)
    (cond
      [(and (<= x (findLargestDoneIn a b)) (>= x (findSmallestDoneIn a b))) x]
      [else (if (and (< (- (findSmallestDoneIn a b) x) (- x a)) (< (- x (findLargestDoneIn a b)) (- b x))) x 0)]))
  (accumulate-i + 0 a b (λ (x) (shouldAdd x)) (λ (x) (+ 1 x))))
(sum-almost-done 5 24)