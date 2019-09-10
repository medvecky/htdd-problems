;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spinning-bears-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; spinning-bears-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)

; PROBLEM:
; 
; In this problem you will design another world program. In this program the changing 
; information will be more complex - your type definitions will involve arbitrary 
; sized data as well as the reference rule and compound data. But by doing your 
; design in two phases you will be able to manage this complexity. As a whole, this problem 
; will represent an excellent summary of the material covered so far in the course, and world 
; programs in particular.
; 
; This world is about spinning bears. The world will start with an empty screen. Clicking
; anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
; but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the 
; screen, a new upright bear appears and starts spinning.
; 
; So each bear has its own x and y position, as well as its angle of rotation. And there are an
; arbitrary amount of bears.
; 
; To start, design a world that has only one spinning bear. Initially, the world will start
; with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
; world will replace the old bear with a new bear at the new spot. You can do this part 
; with only material up through compound. 
; 
; Once this is working you should expand the program to include an arbitrary number of bears.
; 
; Here is an image of a bear for you to use:  .



;; Spinning spaceships

;; =================
;; Constants:

(define SPACE-SHIP .)
(define WIDTH 600)
(define HEIGHT 400)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define SPEED 0.1)
(define ROTATIONSTEP 20)
(define MTS (overlay (rectangle WIDTH HEIGHT "solid" "black") (empty-scene WIDTH HEIGHT)))

;; =================
;; Data definitions:

(define-struct spaceship (x y r))
;; SpaceShip is (make-spaceship Natural[0,WIDTH] Natural[0,HEIGHT] Integer)
;;   the x is x coordinate of ship pixels
;;   the y is y coordinate of ship pixels
;;   the r rotation angle argument uses for rotate function
(define SS1 (make-spaceship 10 270 90))
(define SS2 (make-spaceship 100 100 400))
#;
(define (fn-for-spaceship ss)
  (... (spaceship-x ss)    ; Natural[0, WIDTH]
       (spaceship-y ss)    ; Natural[0, HEIGHT]
       (spaceship-r ss)))  ; INteger 
;; Template rules used:
;;  - compound: 3 fields

;; ListOfSpaceShip is one of:
;;  - empty
;;  - (cons SpaceShip ListOfSpaceships)
;; interp. a list spaceshops
(define LOSS1 empty)
(define LOSS2 (cons SS1 (cons SS2 empty)))

#;
(define (fn-for-loss loss)
  (cond [(empty? loss) (...)]
        [else
         (... (fn-for-spaceship (first loss))
              (fn-for-loss (rest loss)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Spaceship ListOfSpaceShips)
;;  - reference: (first loss) is SpaceShip
;;  - self-reference: (rest loss) is ListOfSpcaceShip


;; =================
;; Functions:

;; ListOfSpaceShip -> ListOfSpaceShip
;; start the world with (main empty)
;; 
(define (main loss)
  (big-bang loss                                          ; ListOfSpaceShip
            (on-tick   update-listofspaceships SPEED)     ; ListOfSpaceShip -> ListOfSpaceShip
            (to-draw   render-list-of-spaceship)          ; ListOfSpaceShip -> Image
            (on-mouse  handle-mouse)))                    ; SpaceShip Integer Integer MouseEvent -> SpaceShip


;; ListOfSpaceShips -> ListOfSpaceships
;; produce List of spaceships with updated rotation angles

(check-expect (update-listofspaceships empty) empty)
(check-expect (update-listofspaceships (cons (make-spaceship 10 10 0) empty)) (cons (make-spaceship 10 10 20)  empty))
(check-expect (update-listofspaceships (cons (make-spaceship 100 100 90) (cons (make-spaceship 10 10 0) empty))) (cons (make-spaceship 100 100 110) (cons (make-spaceship 10 10 20) empty)))
;(define (update-listofspaceships loss) empty) ;stub
;<use template for ListOfSpaceShip>

(define (update-listofspaceships loss)
  (cond [(empty? loss) empty]
        [else
         ( cons (next-spaceship (first loss))
              (update-listofspaceships (rest loss)))]))

;; SpaceShip -> SpaceShip
;; produce the next SpaceShip

;; SpaceShip -> SpaceShip
;; produce the next spaceship with new postion
;; and rotation angle
(check-expect (next-spaceship (make-spaceship 0 0 0)) (make-spaceship 0 0 20))
(check-expect (next-spaceship (make-spaceship 100 100 360)) (make-spaceship 100 100 20))
;(define (next-spaceship ss) (make-spaceship 0 0)) ; stub

;; <use template from SpaceShip>
#;
(define (fn-for-spaceship ss)
  (... (spaceship-x ss)    ; Natural[0, WIDTH]
       (spaceship-y ss)    ; Natural[0, HEIGHT]
       (spaceship-r ss)))  ; Integer

(define (next-spaceship ss)
    (make-spaceship
        (spaceship-x ss)
        (spaceship-y ss)
        (modulo (+ (spaceship-r ss) ROTATIONSTEP) 360)))


;; SpaceShip -> Image
;; render Space ship  with position and rotation
(check-expect (render-spaceship (make-spaceship 100 110 45) MTS) (place-image (rotate 45 SPACE-SHIP) 100 110 MTS))
;(define (render ss) MTS) ; stub
;; <use template from SpaceShip>
#;
(define (fn-for-spaceship ss)
  (... (spaceship-x ss)    ; Natural[0, WIDTH]
       (spaceship-y ss)    ; Natural[0, HEIGHT]
       (spaceship-r ss)))  ; Natural

(define (render-spaceship ss img)
  (place-image (rotate (spaceship-r ss) SPACE-SHIP) (spaceship-x ss) (spaceship-y ss) img))


;; ListOfSpaceShip -> Image
;; Render all spaceships from lists

(check-expect (render-list-of-spaceship empty) MTS)
(check-expect (render-list-of-spaceship (cons (make-spaceship 50 50 0) empty)) (place-image SPACE-SHIP 50 50 MTS))
(check-expect (render-list-of-spaceship (cons (make-spaceship 50 50 0)(cons (make-spaceship 100 100 0) empty))) (place-image SPACE-SHIP 50 50 (place-image SPACE-SHIP 100 100 MTS)))
;(define (render-list-of-spaceship loss) MTS) ;stub
; <template for ListOfSpaceShip>


(define (render-list-of-spaceship loss)
  (cond [(empty? loss) MTS]
        [else
          (render-spaceship (first loss) (render-list-of-spaceship (rest loss)))]))



;; MouseEvent -> ListOfSpaceShip
;; Add Spaceship with X and Y position
(check-expect (handle-mouse empty 50 50 "button-down") (cons (make-spaceship 50 50 0) empty))
(check-expect (handle-mouse (cons (make-spaceship 50 50 0) empty) 100 100 "button-down") (cons (make-spaceship 100 100 0) (cons (make-spaceship 50 50 0) empty)))


;(define (handle-mouse loss x y me) empty) ; stub


(define (handle-mouse loss x y me)
  (cond [(mouse=? me "button-down") (cons (make-spaceship x y 0) loss)]
        [else
          loss]))

