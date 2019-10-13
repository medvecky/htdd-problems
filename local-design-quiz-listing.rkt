;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local-design-quiz-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; local-design-quiz.rkt


; Problem 1:
; 
; Suppose you have rosters for players on two opposing tennis team, and each
; roster is ordered by team rank, with the best player listed first. When both 
; teams play, the best players of each team play one another,
; and the second-best players play one another, and so on down the line. When
; one team has more players than the other, the lowest ranking players on
; the larger team do not play.
; 
; Design a function that consumes two rosters, and produces true if all players 
; on both teams will play if the teams play each other. 
; No marks will be given to solution that do not use a cross product table. 
; 


;; Player is String
;; interp.  the name of a tennis player.
(define P0 "Maria")
(define P2 "Serena")

#;
(define (fn-for-player p)
  (... p))



;; Roster is one of:
;; - empty
;; - (cons Player Roster)
;; interp.  a team roster, ordered from best player to worst.
(define R0 empty)
(define R1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define R2 (list "Maria" "Nadia" "Elena" "Anastasia" "Svetlana"))

#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else 
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))



(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; interp.  a match between player p1 and player p2, with same team rank
(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))

#;
(define (fn-for-match m)
  (... (match-p1 m) (match-p2 m)))



;; ListOfMatch is one of:
;; - empty
;; - (cons Match ListOfMatch)
;; interp. a list of matches between one team and another.
(define LOM0 empty)
(define LOM1 (list (make-match "Eugenie" "Maria")
                   (make-match "Gabriela" "Nadia")))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-match (first lom))
              (fn-for-lom (rest lom)))]))


;; Roster Roster -> Boolean
;; produce true if in consumed roasters is equal number of players
(check-expect (roasters-match? empty empty) true)
(check-expect (roasters-match? (list "A") empty) false)
(check-expect (roasters-match? empty (list "A") ) false)
(check-expect (roasters-match? (list "A") (list "B")) true)
(check-expect (roasters-match? (list "A" "B") (list "C")) false)
(check-expect (roasters-match? (list "C") (list "A" "B")) false)
(check-expect (roasters-match? (list "C" "D") (list "A" "B")) true)
; (define (roasters-match? ra rb ) false) ; stub
; <use template for Roster>
#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else 
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))

(define (roasters-match? ra  rb)
  (cond [(and (empty? ra) (empty? rb)) true]
        [(empty? ra) false]
        [(empty? rb) false]
        [else 
         (roasters-match? (rest ra) (rest rb))]))


; Problem 2:
; 
; Now write a function that, given two teams, produces the list of tennis matches
; that will be played. 
; 
; Assume that this function will only be called if the function you designed above
; produces true. In other words, you can assume the two teams have the same number
; of players. 
; 


;; Roaster Roster -> ListOfMatch
;; produce list of matches based on consumed roasters
(check-expect (make-matches empty empty) empty)
(check-expect (make-matches (list "A") (list "B")) (list (make-match "A" "B")))
(check-expect (make-matches (list "A" "B") (list "C" "D")) (list (make-match "A" "C") (make-match "B" "D")))
;(define (make-matches ra rb) empty) ; stub
; <use template for Roster>
#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else 
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))

(define (make-matches ra rb)
  (cond [(and (empty? ra) (empty? rb)) empty]
        [else 
         (cons (make-match (first ra) (first rb))
               (make-matches (rest ra) (rest rb)))]))


