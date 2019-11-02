;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname average-tr-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; average-starter.rkt

; 
; PROBLEM:
; 
; Design a function called average that consumes (listof Number) and produces the
; average of the numbers in the list.
; 


;; (listof Number) -> Number
;; produces the average of the numbers in the list.

(check-expect (avg-lox empty) 0)
(check-expect (avg-lox (list 1 2 3 4 5)) 3)
(check-expect (avg-lox (list 1 2 3 4 5 6)) 3.5)
  
; (define (avg-lox lox) 0) ; stub

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

(define (avg-lox lox0)
  ;; cnt: Number; how many numbers so far 
  ;; sum: Number; sum of numbers so far  
  ;;
  ;; (average (list 2 3 4)  0 0)
  ;; (average (list   3 4)  1 2)
  ;; (average (list     4)  2 5)
  ;; (average (list      )  3 9) 
  (local [(define (avg-lox1 lox counter sum)
            (cond [(and (empty? lox) (> counter 0)) (/ sum counter)]
                  [(empty? lox) 0]
                  [else
                   (avg-lox1 (rest lox) (add1 counter) (+ (first lox) sum))]))]
    (avg-lox1 lox0 0 0)))