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

#|
Задача 2. (10 т.) Разглеждаме стекова машина, която представя паметта си като списък от числа и символи и приема списък от инструкции, които интерпретира по следния начин:

ако поредната инструкция е число или символ, то се добавя на върха на стека
ако поредната инструкция е функция, тя се прилага над всички числа в стека (допуска се, че функцията приема само един параметър), променяйки стойностите им в стека
ако поредната инструкция е наредена двойка от операция (двуместна функция) и число n, то горните две числа на стека се изваждат и обратно на върха на стека се записва резултат от прилагането на операцията над тях. Прилагането се повтаря до изчерпване на стека или достигане до символ, но не повече от n пъти.
всички останали инструкции се игнорират.
Да се реализира функция run-machine, която връща като резултат списък, представящ паметта на машината след последователно обработване на всички инструкции. Първоначално машината се инициализира с празен стек.

Пример:

(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))                       → (6 5 4 3 a 2 x 1)
(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) → (45 a 2 x 1)
|#

(define (applyPair op l n)
    (define (helper cnt res)
      (cond
      [(zero? (length l)) res]
      [(zero? cnt) res]
      [(symbol? (car res)) res]
      [(and (>= (length l) 2) (symbol? (list-ref res 1))) res]
      [else (helper (- cnt 1) (cons (op (car res) (list-ref res 1)) (cddr res)))]))
  (helper n l)
    )


(define (getFirstNonNumber l)
 
  (define (helper curr res)
    (if (and (>= (length curr) 1) (number? (car curr))) (helper (cdr curr) (+ res 1)) res)  
  )
   (if (list? l)
       (- (helper l 0) 1)
       0
       )
 )

(define (run-machine l)
  
  (define (helper curr res )
    (cond
      [(> curr (- (length l) 1)) res] 
      [(procedure? (list-ref l curr)) (helper (+ curr 1) (map (λ (x) (if (number? x) ((list-ref l curr) x) x)) res))]  
      [(pair? (list-ref l curr)) (helper (+ curr 1) (applyPair (car (list-ref l curr)) res (cdr (list-ref l curr))))] 
      [else (helper (+ curr 1) (cons (list-ref l curr) res))]
))
  (helper 0 '()))

(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5)))

#|
Задача 3. (10 т.) Казваме, че един списък е подсписък на друг, ако елементите на първия списък се срещат непосредствено последователно във втория.
Например, '(2 4) не е подсписък на '(1 2 3 4 5), но '(2 3 4) е. Казваме, че един списък от числа a се мажорира от списъка b,
ако двата списъка са с еднаква дължина n и ai ≤ bi за всяко i ∈ [0; n). Списък от списъци ll наричаме мажорен,
ако е вярно, че li се мажорира от подсписък на li+1 за всеки два съседни списъка li и li+1 в ll.
Да се реализира функция is-major?, която проверява дали даден списък от списъци от числа е мажорен.

Примери:

(is-major? '((1 3) (4 2 7) (2 5 4 3 9 12))) → #t
(is-major? '((1 3) (4 2 7) (2 5 3 3 9 12))) → #f
Бонус: (5 т.) Да се реализира функция find-longest-major, която намира най-дългия мажорен подсписък на даден списък от списъци от числа.|#

(define (is-majoring? a b)
  (cond
   [(not (equal? (length a) (length b))) #f]
   [(and (zero? (length a)) (zero? (length b))) #t]
   [(> (car a) (car b)) #f]
   [else (is-majoring? (cdr a) (cdr b))])) 

(define (extract l  from n)
  (define (helper from cnt res)
    (if (>= cnt n) 
        res
        (helper (+ from 1) (+ cnt 1) (cons (list-ref l from) res))))
  (reverse (helper from 0 '())))

(define (is-sub-majoring? a b)
  (define (helper from)
    (cond
      [(> from (- (length b) (length a))) #f]
      [(is-majoring? a (extract b from (length a))) #t] 
      [else (helper (+ from 1))]) 
    )
  (helper 0)
  )

(define (is-major? l)
  (accumulate-i (λ (x y) (and x y)) #t 0 (- (length l) 2) (λ (x) (if (is-sub-majoring? (list-ref l x) (list-ref l (+ x 1))) #t #f)) (λ (x) (+ x 1))) 
  )
(is-major? '((1 3) (4 2 7) (2 5 4 3 9 12) (4 2 7 3 5 5 4 12 12))) 
(is-major? '((1 3) (4 2 7) (2 5 3 3 9 12)))
