;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-animation-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; countdown-animation starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a simple countdown. 
; 
; Your program should display a simple countdown, that starts at ten, and
; decreases by one each clock tick until it reaches zero, and stays there.
; 
; To make your countdown progress at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, 
; (on-tick advance-countdown 1) then big-bang will wait 1 second between 
; calls to advance-countdown.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Once you are finished the simple version of the program, you can improve
; it by reseting the countdown to ten when you press the spacebar.
; 


;; Animated 10 - 0 countdown
;; =================
;; Constants:
(define WIDTH 600)
(define HEIGHT 400)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define SPEED 1)
(define MTS (overlay (rectangle WIDTH HEIGHT "solid" "black") (empty-scene WIDTH HEIGHT)))
;; =================
;; Data definitions:

;; CountdownCounter is Number
;; number what shoCountdownCounter on countdown animation
(define CC1 10) ; high edge
(define CC2 5)  ; middle
(define CC3 0)  ; low edge

#;
(define (fn-for-count-down cc)
  (... cc))

;; Template rules used:
;; - atomic-distinct: Number


;; =================
;; Functions:

;; CountdownCounter -> CountdownCounter
;; start the world with (main 10)
(define (main cc)
  (big-bang  cc                                     ; CountdownCounter
            (on-tick   advance-countdown SPEED)     ; CountdownCounter -> CountdownCounter
            (to-draw   render)                      ; CountdownCounter -> Image
            (on-key    handle-key)))                ; CountdownCounter KeyEvent -> CountdownCounter
            

;; CountdownCounter -> CountdownCounter
;; produce the next countdown value by decrising current value on 1
(check-expect (advance-countdown 3) 2)
(check-expect (advance-countdown 0) 0)
; (define (next-countdown-value cc) 5) ; stub
; <use template from CountdownCounter>

(define (advance-countdown cc)
  (cond [(> cc 0) (- cc 1)]
        [else cc]))

;; CountdownCounter -> Image
;; render countdown image on scene with appropriate number 
(check-expect (render 5) (place-image (text (number->string 5) 250 "green") CTR-X CTR-Y MTS))
;(define (render cc) MTS) ; stub
; <use template from CountdownCounter>
(define (render cc)
   (place-image (text (number->string cc) 250 "green") CTR-X CTR-Y MTS))

;; CountdownCounter KeyEvent -> CountdownCounter
;; Reset Countdown to 10 when space key is pressed
(check-expect (handle-key 0 " ") 10)
(check-expect (handle-key 0 "a") 0)
(check-expect (handle-key 10 " ") 10)
(check-expect (handle-key 10 "a") 10)
;(define (handle-key cc ke) 5) ; stub
(define (handle-key cc ke)
  (cond [(key=? ke " ") 10]
        [else 
           cc]))