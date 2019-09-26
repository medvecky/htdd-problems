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

(define INVADE-RATE 200)

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
            (stop-when game-end?)         ; Game -> Boolean
            (on-key handle-key)           ; GameKeyEvent -> Game
            (on-release handle-release))) ; GameKeyEvent -> Game

;; Game -> Game
;; produce the next game state
; <use template for game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))))

(define (update-game g)
  (new-invader (remove-destroyed-items (remove-flew-out-missiles-from-game (update-all-game-elements-position g)))))


;;Game -> Game
;; produce new game without destroyed invaders and missiles

(check-expect (remove-destroyed-items (make-game
                                       (list (make-invader 100 100 50))
                                       (list (make-missile 100 100))
                                       (make-tank 150 0)))
              (make-game
               empty
               empty
               (make-tank 150 0)))

(check-expect (remove-destroyed-items (make-game
                                       (list (make-invader 150 150 50))
                                       (list (make-missile 100 100))
                                       (make-tank 150 0)))
              (make-game
               (list (make-invader 150 150 50))
               (list (make-missile 100 100))
               (make-tank 150 0)))
; (define (remove-destroyed-items g) g) ; stub

;<use template for game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define (remove-destroyed-items g)
  (make-game
   (remove-destroyed-invaders
    (game-invaders g)
    (game-missiles g))
   (remove-destroyed-missiles
    (game-missiles g)
    (game-invaders g))
   (game-tank g)))


;; Game -> Game
;; produce new game with new positions of all elements
(check-expect (update-all-game-elements-position (make-game empty (list (make-missile 100 100) (make-missile 150 150)) (make-tank 100 -1)))
              (make-game empty (list (make-missile 100 90) (make-missile 150 140)) (make-tank 98 -1)))
; (define (update-all-game-elements-position g) g) ;stub
; <use template for game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define (update-all-game-elements-position g)
  (make-game (update-loi (game-invaders g)) (update-lom (game-missiles g)) (next-tank (game-tank g))))

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
(check-expect (render (make-game
                       (list (make-invader 100 100 1) (make-invader 150 150 1))
                       (list (make-missile 100 100) (make-missile 150 150))
                       (make-tank 100 0)))
              (place-image INVADER 100 100 
                           (place-image INVADER 150 150 
                                         (place-image MISSILE 100 100
                                                      (place-image MISSILE 150 150
                                                                   (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))

;(define (render g) BACKGROUND) ; stub
; <use template for game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define (render g)
    (render-loi (game-invaders g) (render-lom (game-missiles g) (render-tank (game-tank g)))))
 



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

;; ListOfInvaders -> Image
;; render list of invaders
(check-expect (render-loi empty BACKGROUND) BACKGROUND)
(check-expect (render-loi (list (make-invader 150 150 1)) BACKGROUND) (place-image INVADER 150 150 BACKGROUND))
(check-expect (render-loi (list (make-invader 150 150 1) (make-invader 100 100 1)) BACKGROUND) (place-image
                                                                                                INVADER 150 150
                                                                                                (place-image
                                                                                                 INVADER 100 100 BACKGROUND)))
; (define (render-loi loi image) BACKGROUND) ; stub
; <use template for ListOfInvaders>
; use template for ListOfInvaders
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define (render-loi loi img)
  (cond [(empty? loi) img]
        [else
         (place-image
          INVADER
          (invader-x (first loi))
          (invader-y (first loi))
          (render-loi (rest loi) img))]))
                      


;;Game -> Game
;; produce game without flew out missile
(check-expect (remove-flew-out-missiles-from-game (make-game empty
                                                             (list (make-missile 100 100) (make-missile 150 -1))
                                                             (make-tank 150 0)))
              (make-game empty
                         (list (make-missile 100 100))
                         (make-tank 150 0)))
              
; (define (remove-flew-out-missiles-from-game g) g) ; stub
; <use template for Game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define (remove-flew-out-missiles-from-game g)
  (make-game (game-invaders g)
             (remove-flew-out-missiles (game-missiles g))
             (game-tank g)))

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

;; ListOfInvaders -> ListOfInvaders
;; Produce ListOfInvaders with updated X and Y positions

(check-expect (update-loi empty) empty)
(check-expect (update-loi (list (make-invader 150 150 1))) (list (make-invader 151.5 151.5 1)))
(check-expect (update-loi (list (make-invader 150 150 1) (make-invader 0 150 -1))) (list (make-invader 151.5 151.5 1) (make-invader -1.5 151.5 1)))
; (define (update-loi loi) loi) ; stub
; use template for ListOfInvaders
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define (update-loi loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
              (update-loi (rest loi)))]))


;;Game -> Game
;; add invader to game's invaders list

(define (new-invader g)
  (if (< (random 10000) INVADE-RATE)
      (make-game (cons (make-invader (random WIDTH) 0 (get-direction 2)) (game-invaders g)) (game-missiles g) (game-tank g))
      g))

;; Invader -> Invader 
;; Update innvader x and y position
(check-expect (next-invader (make-invader 150 150 1)) (make-invader 151.5 151.5 1))
(check-expect (next-invader (make-invader 150 150 -1)) (make-invader 148.5 151.5 -1))
(check-expect (next-invader (make-invader 1 150 -1)) (make-invader -0.5 151.5 1))
(check-expect (next-invader (make-invader WIDTH 150 1)) (make-invader (+ WIDTH 1.5) 151.5 -1))
;(define (next-invader i) i) ; stub
; <use template for invader>
#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

(define (next-invader i)
  (check-invader-position (make-invader
                           (+ (* (invader-dx i) INVADER-X-SPEED) (invader-x i))
                           (+ INVADER-Y-SPEED (invader-y i))
                           (invader-dx i))))

;; Invader -> Invader
;; check invader direction and change it if invader reach field side
(check-expect (check-invader-position (make-invader 100 100 1)) (make-invader 100 100 1))
(check-expect (check-invader-position (make-invader 0 100 -1)) (make-invader 0 100 1))
(check-expect (check-invader-position (make-invader -1 100 -1)) (make-invader -1 100 1))
(check-expect (check-invader-position (make-invader WIDTH 100 1)) (make-invader WIDTH 100 -1))
(check-expect (check-invader-position (make-invader (+ WIDTH 1) 100 1)) (make-invader (+ WIDTH 1) 100 -1))
; (define (check-invader-position i) i) ; stub
; <use template for invader>
#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

(define (check-invader-position i)
  (cond [(<= (invader-x i) 0)
         (make-invader (invader-x i) (invader-y i) 1)]
        [(>= (invader-x i) WIDTH)
         (make-invader (invader-x i) (invader-y i) -1)]
        [else i]))

;; Integer -> Integer
;; produce random 1 or -1 for invader direction
(check-range  (get-direction 2) -1 1)
;(define (get-direction x) -2) ; stub
(define (get-direction x)
  (if (= (random x) 1)
      1
      -1))

;; ListOfMissile ListOfInvaders -> ListOfMissiles
;; produce ListOfMissiles without destroyed items
(check-expect (remove-destroyed-missiles empty empty) empty)
(check-expect (remove-destroyed-missiles empty (list (make-invader 100 100 1) (make-invader 150 150 1))) empty)
(check-expect (remove-destroyed-missiles (list (make-missile 50 50)) (list (make-invader 100 100 1) (make-invader 150 150 1))) (list (make-missile 50 50)))
(check-expect (remove-destroyed-missiles (list (make-missile 100 100)) (list (make-invader 100 100 1) (make-invader 150 150 1))) empty)
(check-expect (remove-destroyed-missiles (list (make-missile 150 150)) (list (make-invader 100 100 1) (make-invader 150 150 1))) empty)
(check-expect (remove-destroyed-missiles (list
                                          (make-missile 50 50)
                                          (make-missile 70 70))
                                         (list
                                          (make-invader 100 100 1)
                                          (make-invader 150 150 1)))
              (list
               (make-missile 50 50)
               (make-missile 70 70)))
(check-expect (remove-destroyed-missiles (list
                                          (make-missile 100 100)
                                          (make-missile 70 70))
                                         (list
                                          (make-invader 100 100 1)
                                          (make-invader 150 150 1)))
              (list
               (make-missile 70 70)))
(check-expect (remove-destroyed-missiles (list
                                          (make-missile 50 50)
                                          (make-missile 150 150))
                                         (list
                                          (make-invader 100 100 1)
                                          (make-invader 150 150 1)))
              (list
               (make-missile 50 50)))
(check-expect (remove-destroyed-missiles (list
                                          (make-missile 100 100)
                                          (make-missile 150 150))
                                         (list
                                          (make-invader 100 100 1)
                                          (make-invader 150 150 1)))
              empty)

; (define (remove-destroyed-missiles lom loi) lom) ; stub
; <use template for ListOfMissiles>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (remove-destroyed-missiles lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom] 
        [else
         (if (missile-destroyed? (first lom) loi)
             (remove-destroyed-missiles (rest lom) loi)
             (cons (first lom) (remove-destroyed-missiles (rest lom) loi)))]))

;; ListOfInaders ListOfMissiles -> ListOfInvaders
;; produce LilstOfInvaders without destroyed items
(check-expect (remove-destroyed-invaders empty empty) empty)
(check-expect (remove-destroyed-invaders empty (list (make-missile 100 100) (make-missile 150 150))) empty)
(check-expect (remove-destroyed-invaders (list (make-invader 50 50 1)) (list (make-missile 100 100) (make-missile 150 150))) (list (make-invader 50 50 1)))
(check-expect (remove-destroyed-invaders (list (make-invader 100 100 1)) (list (make-missile 100 100) (make-missile 150 150))) empty)
(check-expect (remove-destroyed-invaders (list (make-invader 150 150 1)) (list (make-missile 100 100) (make-missile 150 150))) empty)
(check-expect (remove-destroyed-invaders (list
                                          (make-invader 50 50 1)
                                          (make-invader 70 70 1))
                                         (list
                                          (make-missile 100 100)
                                          (make-missile 150 150)))
              (list
               (make-invader 50 50 1)
               (make-invader 70 70 1)))
(check-expect (remove-destroyed-invaders (list
                                          (make-invader 100 100 1)
                                          (make-invader 70 70 1))
                                         (list
                                          (make-missile 100 100)
                                          (make-missile 150 150)))
              (list
               (make-invader 70 70 1)))
(check-expect (remove-destroyed-invaders (list
                                          (make-invader 50 50 1)
                                          (make-invader 150 150 1))
                                         (list
                                          (make-missile 100 100)
                                          (make-missile 150 150)))
              (list
               (make-invader 50 50 1)))
(check-expect (remove-destroyed-invaders (list
                                          (make-invader 100 100 1)
                                          (make-invader 150 150 1))
                                         (list
                                          (make-missile 100 100)
                                          (make-missile 150 150)))
              empty)
; (define (remove-destroyed-invaders loi lom) loi) ; stub
; <use template for ListOfInvaders>
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))


(define (remove-destroyed-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi] 
        [else
         (if (invader-destroyed? (first loi) lom)
             (remove-destroyed-invaders (rest loi) lom)
             (cons (first loi) (remove-destroyed-invaders (rest loi) lom)))]))

;; Mssile ListOfInvadeders -> Boolean
;; produce true if missile destroyed by invader explosure

(check-expect (missile-destroyed? (make-missile 100 100) empty) false)
(check-expect (missile-destroyed? (make-missile 100 100) (list (make-invader 150 150 1))) false)
(check-expect (missile-destroyed? (make-missile 100 100) (list (make-invader 110 98 1))) true)
(check-expect (missile-destroyed? (make-missile 100 100)
                                  (list (make-invader 150 150 1)
                                        (make-invader 110 98 1))) true)
(check-expect (missile-destroyed? (make-missile 100 100)
                                  (list (make-invader 110 98 1)
                                        (make-invader 150 150 1))) true)
(check-expect (missile-destroyed? (make-missile 100 100)
                                  (list (make-invader 110 98 1)
                                        (make-invader 110 98 1))) true)
(check-expect (missile-destroyed? (make-missile 100 100)
                                  (list (make-invader 150 150 1)
                                        (make-invader 150 150 1))) false)

; (define (missile-destroyed? m loi) true) ; stub
; <use template for ListOfInvaders>
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define (missile-destroyed? m loi)
  (cond [(empty? loi) false]
        [else
         (if (missile-meet-invader? (first loi) m)
             true
              (missile-destroyed? m (rest loi)))]))




;; Invader ListOfMissiles -> Boolean
;; produce true if invader destroyed by any missile from list
(check-expect (invader-destroyed? (make-invader 100 100 1) empty) false)
(check-expect (invader-destroyed? (make-invader 100 100 1) (list (make-missile 150 150))) false)
(check-expect (invader-destroyed? (make-invader 100 100 1) (list (make-missile 110 98))) true)
(check-expect (invader-destroyed? (make-invader 100 100 1)
                                  (list (make-missile 150 150)
                                        (make-missile 110 98))) true)
(check-expect (invader-destroyed? (make-invader 100 100 1)
                                  (list (make-missile 110 98)
                                        (make-missile 150 150))) true)
(check-expect (invader-destroyed? (make-invader 100 100 1)
                                  (list (make-missile 110 98)
                                        (make-missile 110 98))) true)
(check-expect (invader-destroyed? (make-invader 100 100 1)
                                  (list (make-missile 150 150)
                                        (make-missile 150 150))) false)
; (define (invader-destroyed? i lom) true) ; stub
; <use template for ListOfMissiles>
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (invader-destroyed? i lom)
  (cond [(empty? lom) false]
        [else
         (if (missile-meet-invader? i (first lom))
             true
              (invader-destroyed? i (rest lom)))]))

;; Invader Missile -> Boolean
;; produce true if missile meets invader
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 150 150)) false)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 110 110)) true)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 111 110)) false)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 110 111)) false)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 109 111)) false)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 109 109)) true)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 110 109)) true)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 109 95)) true)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 95 95)) true)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 80 80)) false)
(check-expect (missile-meet-invader? (make-invader 100 100 1) (make-missile 100 80)) false)
; (define (invider-destroyed-by-missile? i m) true) ; stub
; <use template for invader>
#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

(define (missile-meet-invader? i m)
 (and (<= (abs (- (invader-x i) (missile-x m))) 10)
      (<= (abs (- (invader-y i) (missile-y m))) 10)))


;; Game -> Boolean
;; produce true if one of invader passed through the game field
(check-expect (game-end? (make-game (list (make-invader 150 150 1)) empty (make-tank 150 0))) false)
(check-expect (game-end? (make-game (list (make-invader 150 HEIGHT 1)) empty (make-tank 150 0))) true)
; (define (game-end? g) true);
; <use template for Game>
#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))
(define (game-end? g)
  (invader-passed? (game-invaders g)))

;; ListOfInvaders -> Boolean
;; produce true if one of invader passed through the game field
(check-expect (invader-passed? empty) false)
(check-expect (invader-passed? (list (make-invader 150 150 1))) false)
(check-expect (invader-passed? (list (make-invader 150 HEIGHT 1))) true)
(check-expect (invader-passed? (list (make-invader 150 (+ 1 HEIGHT) 1))) true)
(check-expect (invader-passed? (list
                                (make-invader 150 150 1)
                                (make-invader 100 100 1)))
              false)
(check-expect (invader-passed? (list
                                (make-invader 150 HEIGHT 1)
                                (make-invader 100 100 1)))
              true)
(check-expect (invader-passed? (list
                                (make-invader 150 150 1)
                                (make-invader 100 HEIGHT 1)))
              true)
(check-expect (invader-passed? (list
                                (make-invader 150 HEIGHT 1)
                                (make-invader 100 HEIGHT 1)))
              true)
; (define (invader-passed? loi) true) ; stub
; <use template for ListOfInvaders>
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define (invader-passed? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (invader-passed? (rest loi)))]))
