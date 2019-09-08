;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname alternative-tuition-graph-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; alternative-tuition-graph-starter.rkt

; 
; Consider the following alternative type comment for Eva's school tuition 
; information program. Note that this is just a single type, with no reference, 
; but it captures all the same information as the two types solution in the 
; videos.
; 
; (define-struct school (name tuition next))
; ;; School is one of:
; ;;  - false
; ;;  - (make-school String Natural School)
; ;; interp. an arbitrary number of schools, where for each school we have its
; ;;         name and its tuition in USD
; 
; (A) Confirm for yourself that this is a well-formed self-referential data 
;     definition.
; 
; (B) Complete the data definition making sure to define all the same examples as 
;     for ListOfSchool in the videos.
; 
; (C) Design the chart function that consumes School. Save yourself time by 
;     simply copying the tests over from the original version of chart.
; 
; (D) Compare the two versions of chart. Which do you prefer? Why?
; 


;; Constants:

(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE   1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")

;; Data definitions:

(define-struct school (name tuition next))
;; School is one of:
;;  - false
;;  - (make-school String Natural School)
;; interp. an arbitrary number of schools, where for each school we have its
;;         name and its tuition in USD

(define S0 false)
(define S1 (make-school "School1" 27797 false))
(define S2 (make-school "School2" 23300 S1)) 
(define S3 (make-school "School3" 28500 S2))

(define (fn-for-school s)
  (cond [(= s false) (...)]
        [else
         (... (fn-for-school-compound s)
              (fn-for-school s))]))
;;Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: false
;;  - compound  school

;; Functions:

;; ListOfSchool -> Image
;; produce bar chart showing names and tuitions of consumed school

(check-expect (chart S0) (square 0 "solid" "white"))
(check-expect (chart S1) (beside
                          (overlay/align "center" "bottom"
                                         (rotate 90 (text "School1" FONT-SIZE FONT-COLOR))
                                         (rectangle BAR-WIDTH (* 27797 Y-SCALE) "outline" "black")
                                         (rectangle BAR-WIDTH (* 27797 Y-SCALE) "solid" BAR-COLOR))
                          (square 0 "solid" "white")))
(check-expect (chart S2) (beside/align "bottom"
                                       (overlay/align "center" "bottom"
                                                      (rotate 90 (text "School2" FONT-SIZE FONT-COLOR))
                                                      (rectangle BAR-WIDTH (* 23300 Y-SCALE) "outline" "black")
                                                      (rectangle BAR-WIDTH (* 23300 Y-SCALE) "solid" BAR-COLOR))
                                       (overlay/align "center" "bottom"
                                                      (rotate 90 (text "School1" FONT-SIZE FONT-COLOR))
                                                      (rectangle BAR-WIDTH (* 27797 Y-SCALE) "outline" "black")
                                                      (rectangle BAR-WIDTH (* 27797 Y-SCALE) "solid" BAR-COLOR)
                                       (square 0 "solid" "white"))))

;(define (chart s) (square 0 "solid" "white")) ; stub
(define (chart s)
  (cond [(boolean? s) (square 0 "solid" "white")]
        [else
        (beside/align "bottom"
                       (make-bar s)
                       (chart (school-next s)))]))

;; School -> Image
;; produce the bar for a single school in the barchart
(check-expect (make-bar S1) (overlay/align "center" "bottom"
                                           (rotate 90 (text "School1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 27797 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 27797 Y-SCALE) "solid" BAR-COLOR)))
(check-expect (make-bar S2) (overlay/align "center" "bottom"
                                           (rotate 90 (text "School2" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 23300 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 23300 Y-SCALE) "solid" BAR-COLOR)))
;(define (make-bar s) (square 0 "solid" "white")) ; stub
;<template for school>
(define (make-bar s)
   (overlay/align "center" "bottom"
                 (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" "black")
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR)))
  
  