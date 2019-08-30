;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; traffic-light-starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a traffic light. 
; 
; Your program should show a traffic light that is red, then green, 
; then yellow, then red etc. For this program, your changing world 
; state data definition should be an enumeration.
; 
; Here is what your program might look like if the initial world 
; state was the red traffic light:
; .
; Next:
; .
; Next:
; .
; Next is red, and so on.
; 
; To make your lights change at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, (on-tick next-color 1) 
; then big-bang will wait 1 second between calls to next-color.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Note: If you want to design a slightly simpler version of the program,
; you can modify it to display a single circle that changes color, rather
; than three stacked circles. 
; 


;; Traffic light simulator

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define SPEED 2)
(define MTS (overlay (rectangle WIDTH HEIGHT "solid" "black") (empty-scene WIDTH HEIGHT)))

;; =================
;; Data definitions:

;; LightState is one of:
;;  - "red"
;;  - "yellow"
;;  - "green"
;; interp. the color of a traffic light

;; <examples are redundant for enumerations>
 
#;
(define (fn-for-light-state ls)
  (cond [(string=? "red" ls) (...)]
        [(string=? "yellow" ls) (...)]
        [(string=? "green" ls) (...)]))
;; Template rules used:
;;  - one of: 3 cases
;;  - atomic distinct: "red"
;;  - atomic distinct: "yellow"
;;  - atomic distinct: "green"



;; =================
;; Functions:

;; LightState -> LightState
;; start the world with (main "red")
;; 
(define (main ls)
  (big-bang ls                           ; LightState
            (on-tick   next-state SPEED) ; LightState -> LightState
            (to-draw   render)))         ; LightState -> Image
          
;; LightState -> LightState
;; produce the next state
(check-expect (next-state "red") "green")
(check-expect (next-state "green") "yellow")
(check-expect (next-state "yellow") "red")
;(define (next-state ls) "black") ; stub
;<template from LightState>
(define (next-state ls)
  (cond [(string=? "red" ls) "green"]
        [(string=? "yellow" ls) "red"]
        [(string=? "green" ls) "yellow"]))

;; LightState -> Image
;; render  traffic light on scene
(check-expect (render "green") (overlay (rectangle 100 300 "outline" "white")
            (place-image (above (circle 49 "outline" "red")
                   (circle 49 "outline" "yellow")
                   (circle 49 "solid" "green")) CTR-X CTR-Y MTS)))
(check-expect (render "yellow") (overlay (rectangle 100 300 "outline" "white")
            (place-image (above (circle 49 "outline" "red")
                   (circle 49 "solid" "yellow")
                   (circle 49 "outline" "green")) CTR-X CTR-Y MTS)))
(check-expect (render "red") (overlay (rectangle 100 300 "outline" "white")
            (place-image (above (circle 49 "solid" "red")
                   (circle 49 "outline" "yellow")
                   (circle 49 "outline" "green")) CTR-X CTR-Y MTS)))
;(define (render ls) MTS) ; stub
(define (render ls)
  (place-image (show-trafic-light ls)CTR-X CTR-Y MTS))
;; LightState -> Image
;; Produces traffic light image with provided state
(check-expect (show-trafic-light "green") (overlay (rectangle 100 300 "outline" "white")
                   (above (circle 49 "outline" "red")
                   (circle 49 "outline" "yellow")
                   (circle 49 "solid" "green"))))
(check-expect (show-trafic-light "yellow") (overlay (rectangle 100 300 "outline" "white")
                   (above (circle 49 "outline" "red")
                   (circle 49 "solid" "yellow")
                   (circle 49 "outline" "green"))))
(check-expect (show-trafic-light "red") (overlay (rectangle 100 300 "outline" "white")
                   (above (circle 49 "solid" "red")
                   (circle 49 "outline" "yellow")
                   (circle 49 "outline" "green"))))
;(define (show-trafic-light ls) MTS) ; stub
(define (show-trafic-light ls)
  (overlay (rectangle 100 300 "outline" "white")
            (above (circle 49 (if (string=? ls "red") "solid"  "outline") "red")
                   (circle 49 (if (string=? ls "yellow") "solid"  "outline") "yellow")
                   (circle 49 (if (string=? ls "green") "solid"  "outline") "green"))))

