#lang r5rs
; scheme list operations in functional programming style


; basic definitions

; higher order procedure that lets you apply a selector procedure on its parameters
(define (cons x y)
  (lambda (select)
    (select x y)))

; provides a selector procedure (first item) to the cons procedure to be applied
(define (car pair)
  (pair (lambda (x y) x)))

; provides a selector procedure (second item) to the cons procedure to be applied
(define (cdr pair)
  (pair (lambda (x y) y)))

; cons in message passing style
(define (cons2 x y)
  (define (dispatch message)
    (cond ((= message 0) x)
          ((= message 1) y)
          (else (display "bad message"))))
  dispatch)

; replace define dispatch with lambda(message)

(define (car2 pair)
  (pair 0))

(define (cdr2 pair)
  (pair 1))


; list operations

; test for empty list
(define (null?? l)
  (eq? l '()))

; get length of a list (recursive)
(define (len l)
  (if (eq? l '())
      0
      (+ 1 (len (cdr l)))))

; get length of a list (imperative)
(define (len2 l)
  (define (iter l sum)
    (if (eq? l '())
        sum
        (iter (cdr l) (+ 1 sum))))
  (iter l 0))

; get n-th item of a list
(define (get-item l n)
  (if (eq? l '())
      (display "index out of range\n")
      (if (= n 0)
          (car l)
          (get-item (cdr l) (- n 1)))))

; append items of list2  to list1
(define (append2 l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

; append items of any number of lists to list1
(define (append . lists)
  (define (helper lists)
    (cond((null? lists)
          '())
         ((null? (car lists))
          (helper (cdr lists)))
         (else
          (cons (caar lists)
                (helper (cons (cdar lists)
                              (cdr lists)))))))
  (helper lists))
         
; maps a procedure over list items         
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))

; filters list to given condition
(define (filter condi l)
  (if (null? l)
      '()
      (if (condi (car l))
          (cons (car l) (filter condi (cdr l)))
          (filter condi (cdr l)))))

; accumulate list items with operation (recursive)
(define (accumulate operation initial l)
  (if (null? l)
      initial
      (operation (car l) (accumulate operation initial (cdr l)))))

; accumulate list items with operation (iterative)
(define (accumulate2 operation initial l)
  (define (iter operation result l)
    (if (null? l)
        result
        (iter operation (operation result (car l)) (cdr l))))
  (iter operation initial l))

; get list length with by using accum procedure
(define (len3 l)
  (accumulate (lambda (x y) (+ 1 y)) 0 l))

; reverse a list
(define (reverse l)
  (define (helper list acc)
    (if (null? list)
        acc
        (helper (cdr list) (cons (car list) acc))))
  (helper l '()))


; find object in list
(define (memq sym l)
  (cond ((null? l)
         #f)
        ((eq? sym (car l))
         #t)
        (else
         (memq sym (cdr l)))))
         
; remove first occurence of member
(define (rember sym l)
  (cond ((null? l)
         '())
        ((eq? sym (car l))
         (cdr l))
        (else
         (cons (car l) (rember sym (cdr l))))))
    
