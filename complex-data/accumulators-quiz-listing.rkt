;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname accumulators-quiz-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;  PROBLEM 1:
;  
;  Assuming the use of at least one accumulator, design a function that consumes a list of strings,
;  and produces the length of the longest string in the list. 
;  


;; (listof String) -> Number
;; produces the length of the longest string in the list

(check-expect (max-string-length empty) 0)
(check-expect (max-string-length (list "a" "abc")) 3)
(check-expect (max-string-length (list "a" "abc" "abcdef" "" "xx")) 6)

;(define (max-string-length lox) 0); stub

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


(define (max-string-length lox0)
  (local [(define (max-string-length1 lox acc)
            (cond [(empty? lox) acc]
                  [else
                   (if (> (string-length (first lox)) acc)
                        (max-string-length1 (rest lox) (string-length (first lox)))
                        (max-string-length1 (rest lox) acc))]))]
    (max-string-length1 lox0 0)))


;  PROBLEM 2:
;  
;  The Fibbonacci Sequence https://en.wikipedia.org/wiki/Fibonacci_number is 
;  the sequence 0, 1, 1, 2, 3, 5, 8, 13,... where the nth element is equal to 
;  n-2 + n-1. 
;  
;  Design a function that given a list of numbers at least two elements long, 
;  determines if the list obeys the fibonacci rule, n-2 + n-1 = n, for every 
;  element in the list. The sequence does not have to start at zero, so for 
;  example, the sequence 4, 5, 9, 14, 23 would follow the rule. 
;  


;; (listof Number) -> Boolean
;; produce true if all the numbers in the list follow the fibbonaci rule
;; ASSUME: list is at least two elements long
;; NOTE: if list is only 2 elements long, always produce true
(check-expect (fib? (list 4 5 9 14 23)) true)
(check-expect (fib? (list 0 1 1 2 3 5 8 13)) true)
(check-expect (fib? (list 0 2 4 5 6)) false)
(check-expect (fib? (list 9 8 7 6 5 4 3 2 1)) false)

(define (fib? lon0)
  (local [(define (fib lon n-2 n-1)
            (cond [(empty? lon) true]
                  [else
                   (if (= (first lon) (+ n-2 n-1))
                       (fib (rest lon) n-1 (first lon))
                       false)]))]

    (fib lon0 (- (first lon0) 1) (- (first lon0) (- (first lon0) 1)))))




;  PROBLEM 3:
;  
;  Refactor the function below to make it tail recursive.  
;  


;; Natural -> Natural
;; produces the factorial of the given number
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)
(check-expect (fact 5) 120)


#;
(define (fact n)
  (cond [(zero? n) 1]
        [else 
         (* n (fact (sub1 n)))]))


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


(define (fact n)
  (local [(define (fact n1 acc)
            (cond [(zero? n1) acc]
                  [else
                   (fact (- n1 1) (* acc n1))]))]
    (fact n 1)))



;  PROBLEM 4:
;  
;  Recall the data definition for Region from the Abstraction Quiz. Use a worklist 
;  accumulator to design a tail recursive function that counts the number of regions 
;  within and including a given region. 
;  So (count-regions CANADA) should produce 7



(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))
          
          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))
          
          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else 
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))


;; Region -> Natural
;; produce the amount of regions in the given tree
(check-expect (count BC) 3)
(check-expect (count VICTORIA) 1)
(check-expect (count CANADA) 7)

(define (count r)
  (local [(define (fn-for-region todo r rsf)
            (fn-for-lor (append (region-subregions r) todo) (add1 rsf)))
       
          (define (fn-for-lor todo rsf)
            (cond [(empty? todo) rsf]
                  [else 
                   (fn-for-region (rest todo) (first todo) rsf)]))]
    
    (fn-for-region empty r 0)))
