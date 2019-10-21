;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fractals-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

;; fractals-starter.rkt

; 
; PROBLEM: 
; 
; Design a function that consumes a number and produces a Sierpinski
; triangle of that size. Your function should use generative recursion.
; 
; One way to draw a Sierpinski triangle is to:
; 
;  - start with an equilateral triangle with side length s
;  
;      .
;      
;  - inside that triangle are three more Sierpinski triangles
;      .
;      
;  - and inside each of those... and so on
;  
; So that you end up with something that looks like this:
;    
; 
;    
; 
; .
;    
; Note that in the 2nd picture above the inner triangles are drawn in 
; black and slightly smaller just to make them clear. In the real
; Sierpinski triangle they should be in the same color and of side
; length s/2. Also note that the center upside down triangle is not
; an explicit triangle, it is simply formed from the other triangles.
; 
; 


(define CUTOFF 2)

;; Number -> Image
;; produce a Sierpinski Triangle of the give size
(check-expect (stri CUTOFF) (triangle CUTOFF "outline" "red"))
(check-expect (stri (* CUTOFF 2))
             (overlay (triangle (* CUTOFF 2) "outline" "red")
             (local [(define  sub (triangle CUTOFF "outline" "red"))]
               (above sub
                      (beside sub sub)))))

; (define (stri s) (square 0 "solid" "black")) ; stub

#;
(define (genrec-fn d)
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d 
              (genrec-fn (next-problem d)))]))

; 
; PROBLEM:
; Three part termination argument
; 
; Base case: (<= s CUTOFF)
; 
; Reduction step: (/ s 2)
; 
; Argument that repeated application of reduction step will eventually 
; reach the base case:
; As long as cutoff is > 0 and s starts >= 0 repeated division by 2
; will eventually be less than cutoff. 



(define (stri s)
  (cond [(<= s CUTOFF)
         (triangle s "outline" "red")]
        [else
         (overlay (triangle s "outline" "red")
                  (local [(define sub (stri (/ s 2)))]
                    (above sub
                           (beside sub sub))))]))


; 
; PROBLEM:
; 
; Design a function to produce a Sierpinski carpet of size s.
; 
; Here is an example of a larger Sierpinski carpet.
; 
; .
; 


;; Number -> Image
;; produce a Sierpinski carpet of the give size
(check-expect (scarpet CUTOFF) (square CUTOFF "outline" "red"))
(check-expect (scarpet (* CUTOFF 3))
              (overlay (square (* 3 CUTOFF) "outline" "red")
                       (local [(define sub (square CUTOFF "outline" "red"))
                               (define blk (square CUTOFF "solid" "black"))]
                         (above
                          (beside sub sub sub)
                          (beside sub blk sub)
                          (beside sub sub sub)))))
                       
;(define (scarpet s) (square 0 "solid" "black")) ; stub

#;
(define (genrec-fn d)
  (cond [(<= s CUTOFF) (trivial-answer d)]
        [else
         (... d 
              (genrec-fn (next-problem d)))]))


; 
; PROBLEM:
; Three part termination argument
; 
; Base case: (<= s CUTOFF)
; 
; Reduction step: (/ s 3)
; 
; Argument that repeated application of reduction step will eventually 
; reach the base case:
; As long as cutoff is > 0 and s starts >= 0 repeated division by 3
; will eventually be less than cutoff. 



(define (scarpet s)
  (cond [(<= s CUTOFF)
         (square s "outline" "red")]
        [else
         (overlay (square s "outline" "red")
                  (local [(define sub (scarpet (/ s 3)))
                          (define blk (square (/ s 3) "solid" "black"))]
                    (above
                     (beside sub sub sub)
                     (beside sub blk sub)
                     (beside sub sub sub))))]))



