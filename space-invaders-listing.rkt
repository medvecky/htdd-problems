;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-listing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define EMPTY-FRAME (rectangle WIDTH HEIGHT "outline" "black"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissiles is one of:
;; - empty
;; (cons Missile ListOfMissiles)
;; interpr. a list of missiles

(define LOM1 empty)
(define LOM2 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile ListOfMissile)
;;  - reference: (first lom) is Missile 
;;  - self-reference: (rest lom) is ListOfMissiles


;; ListOfInvaders is one of:
;; - empty
;; (cons Invader ListOfInvaders)
;; interpr. a list of invaders

(define LOI1 empty)
(define LOI2 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvaders)
;;  - reference: (first loi) is Invader
;;  - self-reference: (rest loi) is ListOfinvaders


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty (make-tank 150 0)))
;; 
(define (main game)
  (big-bang game                          ; game
            (on-tick   update-game)       ; Game -> Game
            (to-draw   render)            ; Game -> Image
            ;(stop-when ...)              ; Game -> Boolean
            (on-key handle-key)           ; GameKeyEvent -> Game
            (on-release handle-release))) ; GameKeyEvent -> Game

;; Game -> Game
;; produce the next game state
(check-expect (update-game (make-game empty empty (make-tank 100 0))) (make-game empty empty (make-tank 100 0)))
(check-expect (update-game (make-game empty empty (make-tank 100 1))) (make-game empty empty (make-tank 102 1)))
(check-expect (update-game (make-game empty empty (make-tank 100 -1))) (make-game empty empty (make-tank 98 -1)))
(check-expect (update-game (make-game empty (list (make-missile 100 100)) (make-tank 100 -1)))
              (make-game empty (list (make-missile 100 90)) (make-tank 98 -1)))
(check-expect (update-game (make-game empty (list (make-missile 100 100) (make-missile 150 150)) (make-tank 100 -1)))
              (make-game empty (list (make-missile 100 90) (make-missile 150 140)) (make-tank 98 -1)))
;(define (update-game game) game) ; stub
; <use template for game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))))

(define (update-game g)
  (make-game (game-invaders g) (remove-flew-out-missiles (update-lom (game-missiles g))) (next-tank (game-tank g))))


;; Game -> Image
;; render the game elements 
(check-expect (render (make-game empty empty (make-tank 100 0)))
                       (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render (make-game empty (list (make-missile 100 100)) (make-tank 100 0)))
               (place-image MISSILE 100 100 
                            (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))
(check-expect (render (make-game empty (list (make-missile 100 100) (make-missile 150 150)) (make-tank 100 0)))
               (place-image MISSILE 100 100 (place-image MISSILE 150 150
                                                         (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))

;(define (render g) BACKGROUND) ; stub
; <use template for game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define (render g)
    (render-lom (game-missiles g) (render-tank (game-tank g))))
 



;; Tank -> Tank
;; produce next state of tank

(check-expect (next-tank (make-tank 100 0)) (make-tank 100 0))
(check-expect (next-tank (make-tank 100 -1)) (make-tank 98 -1))
(check-expect (next-tank (make-tank 100 1)) (make-tank 102 1))
(check-expect (next-tank (make-tank (+ 0 ( / (image-width TANK) 2)) -1)) (make-tank (+ 0 ( / (image-width TANK) 2)) -1))
(check-expect (next-tank (make-tank (- WIDTH ( / (image-width TANK) 2)) 1)) (make-tank (- WIDTH ( / (image-width TANK) 2)) 1))
;(define (next-tank t) (make-tank 0 0)) ; stub

; <use template for tank>
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define (next-tank t)
  (if (or (= 0 (tank-dir t)) (tank-not-movable? t)) 
      t
      (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t) )))

;; Tank -> Boolean
;; produce true if tank can't be moved in consumed dirrection
(check-expect (tank-not-movable? (make-tank 100 1)) false)
(check-expect (tank-not-movable? (make-tank (+ 0 ( / (image-width TANK) 2)) -1)) true)
(check-expect (tank-not-movable? (make-tank (- WIDTH ( / (image-width TANK) 2)) 1)) true)
;(define (tank-not-movable? t) false) ; stub

; <use template for tank>
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define (tank-not-movable? t)
  (or (and (= (tank-dir t) 1)
           (>= (tank-x t) (- WIDTH ( / (image-width TANK) 2))))
      (and (= (tank-dir t) -1)
           (<= (tank-x t) (/ (image-width TANK) 2)))))

;; Tank -> Image
;; render tank and place it on background
(check-expect (render-tank (make-tank 100 0)) (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
;(define (render-tank t) BACKGROUND) ;stub
; <use template for tank>
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; Game -> Game
;; Handle arrows key press
(check-expect (handle-key (make-game empty empty (make-tank 100 0)) "left") (make-game empty empty (make-tank 100 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 100 0)) "right") (make-game empty empty (make-tank 100 1)))
(check-expect (handle-key (make-game empty empty (make-tank 100 1)) " ") (make-game empty (list (make-missile 100 (- HEIGHT (image-height TANK)))) (make-tank 100 1)))
;(define (handle-key game ke) game) ; stub
; <use template for game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define (handle-key game ke)
  (cond
    [(key=? ke "right" )
     (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) 1))]
     [(key=? ke "left" )
     (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) -1))]
      [(key=? ke " ")
      (make-game (game-invaders game)
                 (cons (make-missile (tank-x (game-tank game)) (- HEIGHT (image-height TANK))) (game-missiles game))
                 (game-tank game))] 
     [else game]))

;; Game -> Game
;; Handle arrows key release 
(check-expect (handle-release (make-game empty empty (make-tank 100 -1)) "left") (make-game empty empty (make-tank 100  0)))
(check-expect (handle-release (make-game empty empty (make-tank 100 1)) "right") (make-game empty empty (make-tank 100 0)))
;(define (handle-release game ke) game) ; stub
; <use template for game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define (handle-release game ke)
  (cond
    [(key=? ke "right" )
     (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) 0))]
     [(key=? ke "left" )
     (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) 0))]
     [else game]))

;; Missile -> Missile
;; produce next missile state
(check-expect (next-missile (make-missile 100 150)) (make-missile 100 140))
;(define (next-missile m) m) ; stub
; <use template for missile>
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))
(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissiles -> ListOfMissiles
;; update all missile in list one  by one
(check-expect (update-lom empty) empty)
(check-expect (update-lom (list (make-missile 100 100))) (list (make-missile 100 90)))
(check-expect (update-lom (list (make-missile 100 100) (make-missile 150 150))) (list (make-missile 100 90) (make-missile 150 140)))
;(define (update-lom lom) lom) ; stub
;<use template for ListOfMissiles>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (update-lom lom)
  (cond [(empty? lom) empty]
        [else
         (cons (next-missile (first lom))
              (update-lom (rest lom)))]))

;; ListOfMissiles -> Image
;; render list of missiles
(check-expect (render-lom empty  BACKGROUND) BACKGROUND)
(check-expect (render-lom (list (make-missile 100 100)) BACKGROUND) (place-image MISSILE 100 100 BACKGROUND))
(check-expect (render-lom (list (make-missile 100 100) (make-missile 150 150)) BACKGROUND) (place-image MISSILE 100 100 (place-image MISSILE 150 150 BACKGROUND)))
;(define (render-lom lom img) BACKGROUN) ;stub
;<use template for ListOfMissiles>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (render-lom lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
              (render-lom (rest lom) img))]))

;; ListOfMissiles -> ListOfMissiles
;; removes flew out missiles from list
(check-expect (remove-flew-out-missiles empty) empty)
(check-expect (remove-flew-out-missiles (list (make-missile 100 100))) (list (make-missile 100 100)))
(check-expect (remove-flew-out-missiles (list (make-missile 100 -1))) empty)
(check-expect (remove-flew-out-missiles (list (make-missile 100 100) (make-missile 150 150))) (list (make-missile 100 100) (make-missile 150 150)))
(check-expect (remove-flew-out-missiles (list (make-missile 100 -1) (make-missile 150 150))) (list (make-missile 150 150)))
(check-expect (remove-flew-out-missiles (list (make-missile 100 100) (make-missile 150 -1))) (list (make-missile 100 100)))
(check-expect (remove-flew-out-missiles (list (make-missile 100 -1) (make-missile 150 -1))) empty)
; (define (remove-flew-out-missiles lom) lom) ;stub

; <use template for ListOfMissiles>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (remove-flew-out-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (missile-flew-out? (first lom))
             (remove-flew-out-missiles (rest lom))
             (cons (first lom) (remove-flew-out-missiles (rest lom))))]))


;; Missile -> Boolean
;; produce true if missile flew out of the game field
(check-expect (missile-flew-out? (make-missile 100 150)) false)
(check-expect (missile-flew-out? (make-missile 100 0)) false)
(check-expect (missile-flew-out? (make-missile 100 -1)) true)
; (define (missile-flew-out? m) false) ; stub
; <use template for Missile>
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define (missile-flew-out? m)
  (< (missile-y m) 0))
