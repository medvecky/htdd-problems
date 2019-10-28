;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname genrec-quiz-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/list)

;  PROBLEM 1:
;  
;  In the lecture videos we designed a function to make a Sierpinski triangle fractal. 
;  
;  Here is another geometric fractal that is made of circles rather than triangles:
;  
;  .
;  
;  Design a function to create this circle fractal of size n and colour c.
;  


(define CUT-OFF 5)

;; Natural String -> Image
;; produce a circle fractal of size n and colour c
(check-expect (cicrcle-fractal 5 "red") (circle 5 "outline" "red"))
(check-expect (cicrcle-fractal 10 "red") (overlay
                                          (circle 10 "outline" "red")
                                          (beside (circle 5 "outline" "red")
                                                  (circle 5 "outline" "red"))))
;(define (circle-fractal n c) empty-image) ; stub

#;
(define (genrec-fn d)
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d 
              (genrec-fn (next-problem d)))]))

(define (cicrcle-fractal n c)
  (cond [(<= n CUT-OFF) (circle n "outline" c)]
        [else
         (local [(define sub (cicrcle-fractal (/ n 2) c))]
           (overlay (circle n "outline" c)
                    (beside sub sub)))]))



;  PROBLEM 2:
;  
;  Below you will find some data definitions for a tic-tac-toe solver. 
;  
;  In this problem we want you to design a function that produces all 
;  possible filled boards that are reachable from the current board. 
;  
;  In actual tic-tac-toe, O and X alternate playing. For this problem
;  you can disregard that. You can also assume that the players keep 
;  placing Xs and Os after someone has won. This means that boards that 
;  are completely filled with X, for example, are valid.
;  
;  Note: As we are looking for all possible boards, rather than a winning 
;  board, your function will look slightly different than the solve function 
;  you saw for Sudoku in the videos, or the one for tic-tac-toe in the 
;  lecture questions. 
;  


;; Value is one of:
;; - false
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by false) or has and "X" or an "O"

(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; Board is (listof Value)
;; a board is a list of 9 Values
(define B0 (list false false false
                 false false false
                 false false false))

(define B1 (list false "X"   "O"   ; a partly finished board
                 "O"   "X"   "O"
                 false false "X")) 

(define B2 (list "X"  "X"  "O"     ; a board where X will win
                 "O"  "X"  "O"
                 "X" false "X"))

(define B3 (list "X" "O" "X"       ; a board where Y will win
                 "O" "O" false
                 "X" "X" false))


(define B4 (list "X" "O" "X"
                 "O" "O" "X"
                 "X" "X" "O"))

(define B5 (list "X" "O" "X"
                 "O" false "X"
                 "X" "X" "O"))

(define B6 (list "X" "O" "X"
                 "O" false "X"
                 "X" "X" false))

(define B7 (list "X" "O" "X"
                 "O" "O" "X"
                 "O" "X" "O"))
 
(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else 
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))


#;
(define (count-boards bd)
  (local [(define (count--board bd)
            (if (trivial-solution? bd)
                (score-board bd)
                (count--lob (next-boards bd))))
          (define (count--lob lob) 
            (cond [(empty? lob) 0]
                  [else
                   (+ (count--board (first lob))
                      (count--lob (rest lob)))]))]
    (count--board bd)))


;; Board -> Number
;; Produce number of achievable boards from consumed board

;(check-expect (count-boards B4) 0)
(check-expect (count-boards B5) 2)
(check-expect (count-boards B6) 4)
(check-expect (count-boards B1) 8)

(define (count-boards bd)
  (local [(define (count--board bd)
            (if (trivial-solution? bd)
                1
                (count--lob (next-boards bd))))
          (define (count--lob lob) 
            (cond [(empty? lob) 0]
                  [else
                   (+ (count--board (first lob))
                      (count--lob (rest lob)))]))]
    (count--board bd)))

;; Board -> Bolean
;; Produce true if board is filled
(check-expect (trivial-solution? B4) true)
(check-expect (trivial-solution? B3) false)
(check-expect (trivial-solution? B2) false)
(check-expect (trivial-solution? B0) false)
; (define (trivial-solution? b) false) ; stub

(define (trivial-solution? b)
  (andmap (λ(v) (not (boolean? v))) b))

;; Board -> (listof Board)
;; produce a list of all boards
;; which can be achieved from consumed board

(check-expect (next-boards B0)
              (list
               (list "X" false false
                     false false false
                     false false false)
               (list "O" false false
                     false false false
                     false false false)))

(check-expect (next-boards (list "X" false false
                                 false false false
                                 false false false))
              (list
               (list "X"  "X" false
                     false false false
                     false false false)
               (list "X" "O" false
                     false false false
                     false false false)))

; (define (next-boards b) empty) ; stub

(define (next-boards b)
  (fill-x-o (find-blank b) b))

;; Board -> Number
;; Produce first blank field's number

(check-expect (find-blank B0) 0)
(check-expect (find-blank B2) 7)

; (define (find-blank b) -1) ; stub

(define (find-blank b)
  (cond [(empty? b) (error "The board didn't have a blank space.")]
        [else
         (if (false? (first b))
             0
             (+ 1 (find-blank (rest b))))]))

;; Position Board -> (listof Board)
;; Produce a list of boards
;; were the field with consumed position filled with X and O
(check-expect (fill-x-o 0 B0)
              (list
               (cons "X" (rest B0))
               (cons "O" (rest B0))))

(check-expect (fill-x-o 1 B0)
              (list
               (cons (first B0) (cons "X" (rest (rest B0))))
               (cons (first B0) (cons "O" (rest (rest B0))))))

; (define (fill-x-o p b) empty) ; stub

(define (fill-x-o p b)
  (local [(define (filler n)
            (cond
              [(= n 0) (list-set b p "X")]
              [(= n 1) (list-set b p "O")]))]
    (build-list 2 filler)))

;  PROBLEM 3:
;  
;  Now adapt your solution to filter out the boards that are impossible if 
;  X and O are alternating turns. You can continue to assume that they keep 
;  filling the board after someone has won though. 
;  
;  You can assume X plays first, so all valid boards will have 5 Xs and 4 Os.
;  
;  NOTE: make sure you keep a copy of your solution from problem 2 to answer 
;  the questions on edX.
;  



;; Board -> Number
;; Produce number of achievable boards from consumed board

;(check-expect (count-boards B4) 0)
(check-expect (count-valid-boards B5) 1)
(check-expect (count-valid-boards B6) 1)
(check-expect (count-valid-boards B1) 3)

(define (count-valid-boards bd)
  (local [(define (count--board bd)
            (if (trivial-solution? bd)
                (if (valid-board?  bd)
                    1
                    0)
                (count--lob (next-boards bd))))
          (define (count--lob lob) 
            (cond [(empty? lob) 0]
                  [else
                   (+ (count--board (first lob))
                      (count--lob (rest lob)))]))]
    (count--board bd)))


;; Board -> Boolean
;; Produce true when Board is valid
;; contains 5 X and 4 0

(check-expect (valid-board? B4) true)
(check-expect (valid-board? B7) false)

; (define (valid-board? b) false) ; stub

(define (valid-board? b)
  (and (= 4 (count-on-board "O" b)) (= 5 (count-on-board "X" b))))

;; Symbol Board -> Number
;; Produce a number of the consumed symbol on consumed board

(check-expect (count-on-board "X" B4) 5)
(check-expect (count-on-board "O" B4) 4)
; (define (count-on-board s b) 0) ; stub

(define (count-on-board s b)
  (cond
    [(empty? b) 0]
    [else (+
           (if (eq? (first b) s)
               1
               0)
           (count-on-board s (rest b)))]))
