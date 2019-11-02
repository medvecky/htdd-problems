;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dropn-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; dropn-starter.rkt

; 
; PROBLEM:
; 
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by dropping every nth element from lox.
; 
; (dropn (list 1 2 3 4 5 6 7) 2) should produce (list 1 2 4 5 7)
; 



;; (listof X) Natural -> (listof X)
;;  produces the list formed by dropping every nth element from lox

(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))
(check-expect (dropn (list "a" "b" "c" "d" "e" "f" "g") 2) (list "a" "b" "d" "e" "g"))

; (define (dropn lox n) empty) ; stub

#;
(define (skip1 lox0)
  (local [(define (skip1 lox acc)
            (cond [(empty? lox) (... acc)]
                  [else
                   (... acc
                        (first lox)
                        (skip1 (rest lox)
                               (... acc (first lox))))]))]
    
    (skip1 lox0 ...)))


(define (dropn lox0 n)
  ;; acc is Natural; how many elements of lox to keep before next skip
  ;; (dropn (list 1 2 3 4 5 6 7) 2)
  ;; (dropn (list   2 3 4 5 6 7) 1)
  ;; (dropn (list     3 4 5 6 7) 0)
  ;; (dropn (list       4 5 6 7) 2)
  ;; (dropn (list         5 6 7) 1)
  ;; (dropn (list           6 7) 0)
  ;; (dropn (list             7) 2)
  ;; (dropn (list              ) 1) 
  (local [(define (skip1 lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (zero? acc)
                        (skip1 (rest lox) n)
                        (cons (first lox)
                              (skip1 (rest lox) (sub1 acc))))]))]
    (skip1 lox0 n)))