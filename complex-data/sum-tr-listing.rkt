;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sum-tr-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; sum-tr-starter.rkt

; 
; PROBLEM:
; 
; (A) Consider the following function that consumes a list of numbers and produces
;     the sum of all the numbers in the list. Use the stepper to analyze the behavior 
;     of this function as the list gets larger and larger. 
;     
; (B) Use an accumulator to design a tail-recursive version of sum.
; 


;; (listof Number) -> Number
;; produce sum of all elements of lon
(check-expect (sum empty) 0)
(check-expect (sum (list 2 4 5)) 11)

#;
(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))



(define (sum lon0)
  ;; acc: Number; the sum of the element of lon0 seen so far
  ;; (sum (list 2 4 5))
  ;; (sum (list 2 4 5) 0)
  ;; (sum (list   4 5) 2)
  ;; (sum (list     5) 6)
  ;; (sum (list      ) 11)
  (local[(define (sum lon acc)
           (cond
             [(empty? lon) acc]
             [else
              ; (... acc
              ;  (first lon)
              (sum (rest lon)
                   (+ acc (first lon)))]))]
    (sum lon0 0)))



(sum (list 1 2 3 4 5))