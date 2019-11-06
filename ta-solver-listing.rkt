;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt



;  PROBLEM 1:
;  
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people. 
;  
;  Design a data definition for Chirper, including a template that is tail recursive and avoids 
;  cycles. 
;  
;  Then design a function called most-followers which determines which user in a Chirper Network is 
;  followed by the most people.
;  


(define-struct user (name verified? follow))
;; User is (make-user String Boolean (listof User))
;; interp. user in Chirper network with name, verified or not status and list of followed users

(define U1 (make-user "A" true (list (make-user "B" false empty)
                                     (make-user "C" true empty))))
(define U2
  (shared ((-A- (make-user "A" true  (list -C- -B-)))
           (-B- (make-user "B" true  (list -E- -D- -G-)))
           (-C- (make-user "C" true  (list -G- -D-)))
           (-D- (make-user "D" false (list -F- -H-)))
           (-E- (make-user "E" false (list -D-)))
           (-F- (make-user "F" true  (list -D-)))
           (-G- (make-user "G" false (list -B-)))
           (-H- (make-user "H" true  (list -E-))))
    -A-))

(define (fn-for-chirper user)
  ;; todo is (listof User); worklist accumulator
  ;; visited is (listof String); list of user's names seen so far
  (local [(define (fn-for-user u todo visited)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-follow u)
                                    todo)
                            (cons (user-name u) visited))))  ;(... (user-name u) (user-verified? u)
          (define (fn-for-lou todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-user (first todo) (rest todo) visited)]))]
    (fn-for-user user empty empty)))

;; User -> User
;; produce user in a Chirper Network that is followed by the most people
(check-expect (most-followers U1) (make-user "B" false empty))
(check-expect (most-followers U2) (shared ((-A- (make-user "A" true  (list -C- -B-)))
                                           (-B- (make-user "B" true  (list -E- -D- -G-)))
                                           (-C- (make-user "C" true  (list -G- -D-)))
                                           (-D- (make-user "D" false (list -F- -H-)))
                                           (-E- (make-user "E" false (list -D-)))
                                           (-F- (make-user "F" true  (list -D-)))
                                           (-G- (make-user "G" false (list -B-)))
                                           (-H- (make-user "H" true  (list -E-))))
                                    -D-))

;(define (most-followers u) u)         ;Stub

(define (most-followers user)
  ;; todo is (listof User); worklist accumulator
  ;; visited is (listof String); list of user's names seen so far
  ;; rsf is (listof Follows); count of follows seen so far
  (local [(define-struct follows (user count))
          ;; Visites is (make-follows User Natural)
          ;; interp. user with count of how many other users follow him
          (define (fn-for-user u todo visited rsf)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited rsf)
                (fn-for-lou (append (user-follow u)
                                    todo)
                            (cons (user-name u) visited)
                            (add-users (user-follow u) rsf))))
          
          (define (fn-for-lou todo visited rsf)
            (cond [(empty? todo) (max-visited-user rsf)]
                  [else
                   (fn-for-user (first todo) (rest todo) visited rsf)]))

          (define (add-users lou rsf)
            (foldr add-user rsf lou))

          (define (add-user u rsf)         ;; (listof User) (listof Visites) -> (listof Visites)
            (local [(define try (update (user-name u) rsf))]
              (if (false? try)
                  (cons (make-follows u 1) rsf)
                  try)))

          (define (update name rsf)         ;; String (listof Visites) -> (listof Visites)
            (cond [(empty? rsf) false]
                  [else
                   (if (string=? name (user-name (follows-user (first rsf))))
                       (cons (make-follows (follows-user (first rsf)) (add1 (follows-count (first rsf)))) (rest rsf))
                       (update name (rest rsf)))]))

          (define (max-visited-user rsf)
            (follows-user (foldr (lambda (u1 u2)
                                   (if (> (follows-count u1) (follows-count u2))
                                       u1
                                       u2))
                                 (first rsf) (rest rsf))))]
    (fn-for-user user empty empty empty)))

;  PROBLEM 2:
;  
;  In UBC's version of How to Code, there are often more than 800 students taking 
;  the course in any given semester, meaning there are often over 40 Teaching Assistants. 
;  
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write 
;  a program to do it for us! 
;  
;  Below are some data definitions for a simplified version of a TA schedule. There are some 
;  number of slots that must be filled, each represented by a natural number. Each TA is 
;  available for some of these slots, and has a maximum number of shifts they can work. 
;  
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their 
;  maximum shifts. If no such schedules exist, produce false. 
; 
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define ERIKA   (make-ta "Erika"   1 (list 1 3 7 9)))
(define RYAN    (make-ta "Ryan"    1 (list 1 8 10)))
(define REECE   (make-ta "Reece"   1 (list 5 6)))
(define GORDON  (make-ta "Gordon"  2 (list 2 3 9)))
(define DAVID   (make-ta "David"   2 (list 2 8 9)))
(define KATIE   (make-ta "Katie"   1 (list 4 6)))
(define AASHISH (make-ta "Aashish" 2 (list 1 10)))
(define GRANT   (make-ta "Grant"   2 (list 1 11)))
(define RAEANNE (make-ta "Raeanne" 2 (list 1 11 12)))

(define ALEX    (make-ta "Alex"    1 (list 7)))
(define ERIN    (make-ta "Erin"    1 (list 4)))

(define START-TAs      (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE))
(define START-TAs+ALEX (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ALEX))
(define START-TAs+ERIN (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ERIN))
(define SLOTS (list 1 2 3 4 5 6 7 8 9 10 11 12))


(define NOODLE-TAs (list SOBA UDON RAMEN))

(define (fn-for-tas tas)              ; (listof TA) -> X
  (cond [(empty? tas) (...)]
        [else
         (... (fn-for-ta (first tas))
              (fn-for-tas (rest tas)))]))

(define (fn-for-ta ta)                ; TA -> Y 
  (... (ta-name ta)          ;String
       (ta-max ta)           ;Natural
       (fn-for-losl (ta-avail ta))))

(define (fn-for-losl losl)            ; (listof Slot) -> Z
  (cond [(empty? losl) (...)]
        [else
         (... (first losl)   ;Natural
              (fn-for-losl (rest losl)))]))


(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)



;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4)) 
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)
(check-expect (schedule-tas START-TAs SLOTS) false)
(check-expect (schedule-tas START-TAs+ALEX SLOTS) false)
(check-expect (schedule-tas START-TAs+ERIN SLOTS) false)

;(define (schedule-tas tas slots) empty) ;stub

(define (schedule-tas tas0 slots0)
  ;; rsf is (listof Assignment); result so far accumulator
  (local [(define (schedule-tas tas slots rsf)
            (if (empty? slots)
                rsf
                (schedule-lotas tas (valid-tas tas (first slots)) slots rsf)))

          (define (schedule-lotas tas lotas slots rsf)                   
            (cond [(empty? lotas) false]
                  [else
                   (local [(define try (schedule-tas (update-tas tas (first lotas)) (rest slots) (update-rsf (first lotas) (first slots) rsf tas0)))]
                     (if (false? try)
                         (schedule-lotas tas (rest lotas) slots rsf)
                         try))]))

          (define (valid-tas tas slot)                  ;;(listof TA) Natural -> (listof String)
            (cond [(empty? tas) empty]
                  [else
                   (if (member? slot (ta-avail (first tas)))
                       (cons (ta-name (first tas)) (valid-tas (rest tas) slot))
                       (valid-tas (rest tas) slot))]))

          (define (update-tas tas ta)                  ;;(listof TA) String -> (listof TA)
            (if (string=? (ta-name (first tas)) ta)
                (if (= (ta-max (first tas)) 1)
                    (rest tas)
                    (cons (make-ta ta (sub1 (ta-max (first tas))) (ta-avail (first tas))) (rest tas)))
                (cons (first tas) (update-tas (rest tas) ta))))
          
          (define (update-rsf ta slot rsf tas)         ;;String Natural (listof Assignment) (listof TA) -> (listof Assignment)
            (if (string=? (ta-name (first tas)) ta)
                (cons (make-assignment (first tas) slot) rsf)
                (update-rsf ta slot rsf (rest tas))))]
    (schedule-tas tas0 slots0 empty)))


