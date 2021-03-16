#lang r5rs

; set operations

; sets can be implemented as 1)unordered lists or 2)ordered lists

(define empty-set '())
(define empty? null?)

; 1) sets as unordered lists

; check if element in set O(n)
(define (element? x set)
  (cond ((empty? set) #f)
        ((eq? x (car set)) #t)
        (else (element? x (cdr set)))))

; join element to set
(define (join x set)
  (if (element? x set)
      set
      (cons x set)))

; intersection of two sets O(n^2)
(define (intersection set1 set2)
  (cond ((or (empty? set1) (empty? set2))
         '())
        ((element? (car set1) set2)
         (cons (car set1) (intersection (cdr set1) set2)))
        (else
         (intersection (cdr set1) set2))))

; union of two sets
(define (union set1 set2)
  (cond ((empty? set1)
         set2)
        ((element? (car set1) set2)
         (union (cdr set1) set2))
        (else
         (cons (car set1) (union (cdr set1) set2)))))


; 2) sets as ordered lists

; check if element in set O(n)
(define(element_o? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element_o? x (cdr set)))))

; intersection of two sets O(n)
(define (intersec_o set1 set2)
  (cond ((or (empty? set1) (empty? set2))
         '())
        ((= (car set1) (car set2))
         (cons (car set1) (intersec_o (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (intersec_o (cdr set1) set2))
        (else
         (intersec_o set1 (cdr set2)))))
