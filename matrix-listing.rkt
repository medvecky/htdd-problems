;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname matrix-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt

; 
; PROBLEM:
; 
; Design a simple interactive animation of rain falling down a screen. Wherever we click,
; a rain drop should be created and as time goes by it should fall. Over time the drops
; will reach the bottom of the screen and "fall off". You should filter these excess
; drops out of the world state - otherwise your program is continuing to tick and
; and draw them long after they are invisible.
; 
; In your design pay particular attention to the helper rules. In our solution we use
; these rules to split out helpers:
;   - function composition
;   - reference
;   - knowledge domain shift
;   
;   
; NOTE: This is a fairly long problem.  While you should be getting more comfortable with 
; world problems there is still a fair amount of work to do here. Our solution has 9
; functions including main. If you find it is taking you too long then jump ahead to the
; next homework problem and finish this later.
; 
; 


;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 5)

(define DROP (ellipse 4 8 "solid" "light green"))

(define MTS (rectangle WIDTH HEIGHT "solid" "black"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)      ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops 0.01)        ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops)))    ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position

(check-expect (handle-mouse empty 10 11 "button-down") (cons (make-drop 10 11) empty))
(check-expect (handle-mouse (cons (make-drop 10 11) empty) 20 21 "button-down") (cons (make-drop 20 21) (cons (make-drop 10 11) empty)))
;(define (handle-mouse lod x y mevt) empty) ; stub
(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else
          lod]))



;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops

(check-expect (next-drops (cons (make-drop 10 11) (cons (make-drop 50 (+ 1 HEIGHT)) (cons (make-drop 100 100) empty)))) (cons (make-drop 10 12) (cons (make-drop 100 101) empty)))
;(define (next-drops lod)empty) ; stub
;<use template for ListOfDrop>

(define (next-drops lod)
  (cond [(empty? lod) empty]
        [else
         (filter-list-of-drops (update-drops lod))]))




;; ListOfDrops -> ListOfDrops
;; produce ListWithUpdated drops

(check-expect (update-drops empty) empty)
(check-expect (update-drops (cons (make-drop 1 2) empty)) (cons (make-drop 1 3) empty))
(check-expect (update-drops (cons (make-drop 1 2) (cons (make-drop 3 4) empty))) (cons (make-drop 1 3) (cons (make-drop 3 5) empty)))

;(define (update-drops lod) empty) ; stub
; <use template for list of drops>

(define (update-drops lod)
  (cond [(empty? lod) empty]
        [else
         ( cons (update-drop (first lod))
              (update-drops (rest lod)))]))



;; Drop -> Drop
;; produce drop with updated y coordinate

(check-expect (update-drop (make-drop 5 5)) (make-drop 5 6))
;(define (update-drop d) d) ; stub

(define (update-drop d)
  (make-drop (drop-x d) (+ 1 (drop-y d))))

;; ListOfDrop -> ListOfDrop
;; produce ListOfDrop without already falled drops

(check-expect (filter-list-of-drops empty) empty)
(check-expect (filter-list-of-drops (cons (make-drop 10 10) empty)) (cons (make-drop 10 10) empty))
(check-expect (filter-list-of-drops (cons (make-drop 10 HEIGHT) empty)) (cons (make-drop 10 HEIGHT) empty))
(check-expect (filter-list-of-drops (cons (make-drop 10 (+ HEIGHT 1)) empty)) empty)
(check-expect (filter-list-of-drops (cons (make-drop 10 10) (cons (make-drop 20 20) empty))) (cons (make-drop 10 10) (cons (make-drop 20 20) empty)))
(check-expect (filter-list-of-drops (cons (make-drop 10 (+ HEIGHT 1)) (cons (make-drop 20 20) empty))) (cons (make-drop 20 20) empty))
(check-expect (filter-list-of-drops (cons (make-drop 10 10) (cons (make-drop 20 (+ HEIGHT 1)) empty))) (cons (make-drop 10 10) empty))
(check-expect (filter-list-of-drops (cons (make-drop 10 (+ HEIGHT 1)) (cons (make-drop 20 (+ HEIGHT 1)) empty))) empty)

;(define (filter-list-of-drops lod) empty) ;stup
;<use template for ListOfDrop>

(define (filter-list-of-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (drop-is-out? (first lod))
              (filter-list-of-drops (rest lod))
              (cons (first lod) (filter-list-of-drops (rest lod))))]))

;; Drop -> Boolean
;; produce true if Drop is out of bottom screen border

(check-expect (drop-is-out? (make-drop 100 100)) false)
(check-expect (drop-is-out? (make-drop 100 HEIGHT)) false)
(check-expect (drop-is-out? (make-drop 100 (+ 1 HEIGHT))) true)
;(define (drop-is-out? d) true) ; stup
;<use template for drop>
(define (drop-is-out? d)
  (> (drop-y d) HEIGHT))

;; ListOfDrop -> Image
;; Render the drops onto MTS

(check-expect (render-drops empty) MTS)
(check-expect (render-drops (cons (make-drop 50 50) empty)) (place-image DROP 50 50 MTS))
(check-expect (render-drops (cons (make-drop 50 50) (cons (make-drop 100 100)  empty))) (place-image DROP 50 50 (place-image DROP 100 100 MTS)))

;(define (render-drops lod) MTS) ; stub

;<use template for ListOfDrops>

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-image DROP (drop-x (first lod)) (drop-y (first lod))
              (render-drops (rest lod)))]))

