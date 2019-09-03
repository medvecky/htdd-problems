;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-ship-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; water-balloon-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)

;; Space ship

;; =================
;; Constants:
(define SPACE-SHIP .)
(define WIDTH 600)
(define HEIGHT 400)
(define CTR-Y (/ HEIGHT 2))
(define SPEED 0.1)
(define ROTATIONSTEP 20)
(define MOVINGSTEP 10)
(define MTS (overlay (rectangle WIDTH HEIGHT "solid" "black") (empty-scene WIDTH HEIGHT)))
;; =================
;; Data definitions:

(define-struct spaceship (x r))
;; SpaceShip is (make-spaceship Natural[0,WIDTH] Integer)
;;   the x is x coordinate of ship pixels
;;   the r rotation angle argument uses for rotate function
(define SS1 (make-spaceship 10 270))
(define SS2 (make-spaceship 10 400))
#;
(define (fn-for-spaceship ss)
  (... (spaceship-x ss)    ; Natural[0, WIDTH]
       (spaceship-r ss)))  ; Integer
;; Template rules used:
;;  - compound: 2 fields

;; =================
;; Functions:

;; SpaceShip -> SpaceShip
;; start the world with ...
;; (main (make-spaceship 0 0))
(define (main ss)
  (big-bang ss                                   ; SpaceShip
            (on-tick   next-spaceship SPEED)     ; SpaceShip -> SpaceShip
            (to-draw   render)                   ; SpaceShip -> Image
            (on-key    handle-key)))             ; SpaceShip KeyEvent -> SpaceShip

;; SpaceShip -> SpaceShip
;; produce the next spaceship with new postion
;; and rotation angle
(check-expect (next-spaceship (make-spaceship 0 0)) (make-spaceship 10 20))
(check-expect (next-spaceship (make-spaceship 10 360)) (make-spaceship 20 20))
;(define (next-spaceship ss) (make-spaceship 0 0)) ; stub

;; <use template from SpaceShip>
#;
(define (fn-for-spaceship ss)
  (... (spaceship-x ss)    ; Natural[0, WIDTH]
       (spaceship-r ss)))  ; Integer

(define (next-spaceship ss)
    (make-spaceship
        (+ (spaceship-x ss) MOVINGSTEP)
        (modulo (+ (spaceship-r ss) ROTATIONSTEP) 360)))

;; SpaceShip -> Image
;; render Space ship  with position and rotation
(check-expect (render (make-spaceship 100 45)) (place-image (rotate -45 SPACE-SHIP) 100 CTR-Y MTS))
;(define (render ss) MTS) ; stub
;; <use template from SpaceShip>
#;
(define (fn-for-spaceship ss)
  (... (spaceship-x ss)    ; Natural[0, WIDTH]
       (spaceship-r ss)))  ; Natural

(define (render ss)
  (place-image (rotate (- (spaceship-r ss)) SPACE-SHIP) (spaceship-x ss) CTR-Y MTS))

;; KeyEvent -> SpaceShip
;; Reset SpaceShip to initial state
(check-expect (handle-key  SS1 " ") (make-spaceship 0 0))
(check-expect (handle-key SS1 "a") SS1)

;(define (handle-key ss ke) (make-spaceship 0 0)) ; stub
(define (handle-key ss ke)
  (cond [(key=? ke " ") (make-spaceship 0 0)]
        [else 
           ss]))