;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname max-exits-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;; max-exits-from-starter.rkt

; 
; PROBLEM:
; 
; Using the following data definition, design a function that produces the room with the most exits 
; (in the case of a tie you can produce any of the rooms in the tie).
; 


;; Data Definitions: 

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

; .
 
(define H1 (make-room "A" (list (make-room "B" empty))))

; .
 
(define H2 
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-)) 


; .

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))
           


; .

(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

(define H5
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -D- -E- -F- -A-)))
           (-D- (make-room "D" empty))
           (-E- (make-room "E"  empty))
           (-F- (make-room "F" empty)))
    -A-))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist, 
;;           context-preserving accumulator what rooms have we already visited

(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)))) ; (... (room-name r))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty)))

;; Room -> Room
;; produces the room with the most exits
(check-expect (max-exits-from H1) (make-room "A" (list (make-room "B" empty))))
(check-expect (max-exits-from H2)  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))-0-))
(check-expect (max-exits-from H4)  (shared ((-0-
                                             (make-room
                                              "A"
                                              (list
                                               -3-
                                               (make-room
                                                "D"
                                                (list -10-)))))
                                            (-10-
                                             (make-room
                                              "E"
                                              (list
                                               (make-room "F" '())
                                               -0-)))
                                            (-3-
                                             (make-room
                                              "B"
                                              (list
                                               (make-room "C" (list -3-))
                                               -10-))))
                                     -0-))
(check-expect (max-exits-from H5)  (shared ((-0-
                                             (make-room
                                              "C"
                                              (list
                                               (make-room "D" '())
                                               (make-room "E" '())
                                               (make-room "F" '())
                                               (make-room
                                                "A"
                                                (list
                                                 (make-room
                                                  "B"
                                                  (list -0-))))))))
                                     -0-))
;(define (max-exits-from r0) (make-room "Stub" empty)) ; stub

(define (max-exits-from r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; rsf is Room; result so far
  (local [(define (fn-for-room r todo visited rsf) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (if (> (length (room-exits r)) (length (room-exits rsf))) 
                    (fn-for-lor (append (room-exits r) todo)
                                (cons (room-name r) visited)
                                r)
                    (fn-for-lor (append (room-exits r) todo)
                                (cons (room-name r) visited)
                                rsf))))
          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited
                                rsf)]))]
    (fn-for-room r0 empty empty r0)))