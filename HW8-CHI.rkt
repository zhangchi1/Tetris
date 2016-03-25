;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW8-CHI) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;;;;;;;;;;;Problem Set 8 By Max Rais and Chi Zhang;;;;;;;;;;;;;
(require 2htdp/image)
(require 2htdp/universe)
;;;;Problem 1;;;;

;;Sort-n: [List-of Number] [Number Number-> Boolean]->[List-of Number]
;;Sort-s: [List-of String] [String String-> Boolean]->[List-of String]

;;Sort-IRs: [List-of IRs] [IRs IRs-> Boolean]->[List-of IRs]

;;;;Problem 2;;;;
(define-struct toy (name description bought sold))
;;A Toy is (make-toy string string number number)
;;Name is the name of the toy
;;Description is the description of the toy
;;bought is the acquisition price and sold is the recommened sales price

;;Examples
(define toy1 (make-toy "lego" "a lego" 10 20))
(define toy2 (make-toy "toy" "a toy" 20 50))

;;A list of toys (LOTS) is one of:
;;empty
;;(cons toy lots)

;;Examples
(define lots1 empty)
(define lots2 (list toy1 toy2))
(define lots3 (list (make-toy "balls" "ball" 10 10) toy1))

;;Template:
(define (lots-tempt a-lot)
  (cond [(empty? a-lot)...]
        [else (...(first a-lot)...(lots-tempt (rest a-lot))...)]))
;;eliminate-exp: Number Lots -> Lots
;;removes all toys from the given list 
;;whose sales price is above the given number
(define (eliminate-exp ua lots)
  (cond
    [(empty? lots) empty]
    [else (if (< (toy-sold (first lots)) ua)
              (cons (first lots)
                    (eliminate-exp ua (rest lots)))
              (eliminate-exp ua (rest lots)))]))

;;Tests
(check-expect (eliminate-exp 25 lots1) empty)
(check-expect (eliminate-exp 25 lots2) (list toy1))
(check-expect (eliminate-exp 50 lots2) (list toy1))
(check-expect (eliminate-exp 60 lots2) (list toy1 toy2))

;;recall: String Lots -> Lots
;removes all instances of toys with the given name from a given lots
(define (recall name lots)
  (filter (λ (toy) (not (string=? (toy-name toy) name))) lots))

;;Tests
(check-expect (recall "lego" lots1) empty)
(check-expect (recall "lego" lots2) (list toy2))
(check-expect (recall "something" lots2) (list toy1 toy2))


;;helper function:
;;compare-string: Los String-> Boolean
(define (compare-string los str)
      (cond [(empty? los) false]
            [else (or (string=? str (first los)) 
                      (compare-string (rest los) str))]))

;;Examples Tests:
(define (get-name toy) (toy-name toy))
(define lon1 (map get-name lots2))
(define lon2 (map get-name lots3))
(check-expect (compare-string lon1 "toy") true)
(check-expect (compare-string empty "toy") false)

;;selection : Lon Lon -> Lon
;;Takes two lons and produces a third with toys 
;;that share a name between the two
(define (selection 1st-lon 2nd-lon)
  (filter (λ (n) (compare-string 1st-lon n)) 2nd-lon))

;Tests
(check-expect (selection empty lon2) empty)
(check-expect (selection lon1 lon2) (list "lego"))

;;;;Problem 3;;;;

;;1.
(λ (x) (< x 10))

;;2.
(λ (x y) (number->string (* x y)))

;;3.
(λ (p img) (place-image (circle 3 "solid" 'red)
                        (posn-x p)
                        (posn-y p)
                        img))
;;4.
;4.
(define-struct ir [name price])
; An IR is (make-ir String Number)
;name is the name of an inventory record 
;price is the price of an inventory record 

;;Loir is one of:
;;empty
;;(cons ir loir)
(λ (lir) (foldr = (ir-price (first lir)) (map get-price lir)))
;get-price : Ir -> Number
(define (get-price ir)
  (ir-price ir))

;;5.
(λ (n) (if (even? n) 0 1))


;;;;Problem 4;;;;

;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
 
;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))
 
;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))
 
;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.

;; A List of Numbers (LON) is one of:
;; - empty
;; - (cons Number LON)
 
;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;;;Constants
(define PIXELS/GRID 20)
(define BOARD-WIDTH 10) ; in grid cells
(define BOARD-HEIGHT 20) ; in grid cells
(define BOARD 
  (empty-scene (* PIXELS/GRID BOARD-WIDTH)
               (* PIXELS/GRID BOARD-HEIGHT)))

;"O" Block
(define OIMAGE (square 20 "solid" "green"))
(define O1 (make-block 4 21 "green"))
(define O2 (make-block 5 21 "green"))
(define O3 (make-block 4 20 "green"))
(define O4 (make-block 5 20 "green"))
(define BSETO (list O1 O2 O3 O4))
(define CENTERO (make-posn 4.5 20.5))
(define TETRAO (make-tetra CENTERO BSETO))

;"I" Block
(define IIMAGE (square 20 "solid" "blue"))
(define I1 (make-block 3 20 "blue"))
(define I2 (make-block 4 20 "blue"))
(define I3 (make-block 5 20 "blue"))
(define I4 (make-block 6 20 "blue"))
(define BSETI (list I1 I2 I3 I4))
(define CENTERI (make-posn 4.5 20.5))
(define TETRAI (make-tetra CENTERI BSETI))

;"L" Block
(define LIMAGE (square 20 "solid" "purple"))
(define L1 (make-block 4 20 "purple"))
(define L2 (make-block 5 20 "purple"))
(define L3 (make-block 6 20 "purple"))
(define L4 (make-block 6 21 "purple"))
(define BSETL (list L1 L2 L3 L4))
(define CENTERL (make-posn (block-x L2) (block-y L2)))
(define TETRAL (make-tetra  CENTERL BSETL))

;"J" Block
(define JIMAGE (square 20 "solid" "cyan"))
(define J1 (make-block 4 21 "cyan"))
(define J2 (make-block 4 20 "cyan"))
(define J3 (make-block 5 20 "cyan"))
(define J4 (make-block 6 20 "cyan"))
(define BSETJ (list J1 J2 J3 J4))
(define CENTERJ (make-posn (block-x J3) (block-y J3)))
(define TETRAJ (make-tetra CENTERJ BSETJ ))

;"T" Block
(define TIMAGE (square 20 "solid" "orange"))
(define T1 (make-block 4 20 "orange"))
(define T2 (make-block 5 20 "orange"))
(define T3 (make-block 5 21 "orange"))
(define T4 (make-block 6 20 "orange"))
(define BSETT (list T1 T2 T3 T4))
(define CENTERT (make-posn (block-x T2) (block-y T2)))
(define TETRAT (make-tetra  CENTERT BSETT))

;"Z" Block
(define ZIMAGE (square 20 "solid" "pink"))
(define Z1 (make-block 4 21 "pink"))
(define Z2 (make-block 5 21 "pink"))
(define Z3 (make-block 5 20 "pink"))
(define Z4 (make-block 6 20 "pink"))
(define BSETZ (list Z1 Z2 Z3 Z4)) 
(define CENTERZ (make-posn (block-x Z3) (block-y Z3)))
(define TETRAZ (make-tetra CENTERZ BSETZ))

;"S" Block
(define SIMAGE (square 20 "solid" "red"))
(define S1 (make-block 4 20 "red"))
(define S2 (make-block 5 20 "red"))
(define S3 (make-block 5 21 "red"))
(define S4 (make-block 6 21 "red"))
(define BSETS (list S1 S2 S3 S4))
(define CENTERS (make-posn (block-x S2) (block-y S2)))
(define TETRAS (make-tetra CENTERS BSETS))

;;;Logic Functions:

;;new-tetra : Number -> Tetra
;;Produces a new tetra based on a given random number from 0-6
(define (new-tetra n)
  (cond
    [(= n 0) TETRAO]
    [(= n 1) TETRAI]
    [(= n 2) TETRAL]
    [(= n 3) TETRAJ]
    [(= n 4) TETRAT]
    [(= n 5) TETRAZ]
    [(= n 6) TETRAS]))

;EXAMPLES/TESTS:
(check-expect (new-tetra 0) TETRAO)
(check-expect (new-tetra 1) TETRAI)
(check-expect (new-tetra 2) TETRAL)
(check-expect (new-tetra 3) TETRAJ)
(check-expect (new-tetra 4) TETRAT)
(check-expect (new-tetra 5) TETRAZ)
(check-expect (new-tetra 6) TETRAS)

;;Defines our initial world
(define INITIAL-WORLD (make-world (new-tetra (random 6)) empty))
;Some example worlds
(define world1 (make-world TETRAS BSETS))
(define world2 (make-world TETRAS empty))
;an example full row
(define a-full-row (list (make-block 0 0 'red)
                         (make-block 1 0 'red)
                         (make-block 2 0 'red)
                         (make-block 3 0 'red)
                         (make-block 4 0 'red)
                         (make-block 5 0 'red)
                         (make-block 6 0 'red)
                         (make-block 7 0 'red)
                         (make-block 8 0 'red)
                         (make-block 9 0 'red)))

;;move-tetra : Tetra Number Bset -> Tetra
;;Makes a new tetra with moved center coordinates and block coordinates
(define (move-tetra tetra x-dir pile)
  (make-tetra (move-center (tetra-center tetra) x-dir) 
              (move-bset (tetra-blocks tetra) x-dir pile)))

;EXAMPLES/TESTS:
(check-expect (move-tetra TETRAO 0 empty) (make-tetra (make-posn 4.5 19.5)
                                                      (list
                                                       (make-block 4 20 "green")
                                                       (make-block 5 20 "green")
                                                       (make-block 4 19 "green")
                                                       (make-block 5 19 "green"))))
(check-expect (move-tetra TETRAI 1 empty) (make-tetra (make-posn 3.5 19.5)
                                                      (list
                                                       (make-block 2 19 "blue")
                                                       (make-block 3 19 "blue")
                                                       (make-block 4 19 "blue")
                                                       (make-block 5 19 "blue"))))
(check-expect (move-tetra TETRAJ -1 empty) (make-tetra (make-posn 6 19)
                                                       (list
                                                        (make-block 5 20 "cyan")
                                                        (make-block 5 19 "cyan")
                                                        (make-block 6 19 "cyan")
                                                        (make-block 7 19 "cyan"))))
(check-expect (move-tetra (make-tetra (make-posn 5 0)
                                      (list 
                                       (make-block 4 0 "orange")
                                       (make-block 5 0 "orange")
                                       (make-block 5 1 "orange")
                                       (make-block 6 0 "orange"))) 0 empty) 
              (make-tetra (make-posn 5 -1)
                          (list 
                           (make-block 4 0 "orange")
                           (make-block 5 0 "orange")
                           (make-block 5 1 "orange")
                           (make-block 6 0 "orange"))))

(check-expect (move-tetra (make-tetra (make-posn 5 0)
                                      (list 
                                       (make-block 3 7 "red")
                                       (make-block 4 7 "red")
                                       (make-block 4 6 "red")
                                       (make-block 5 6 "red"))) 0 (list 
                                                                      (make-block 5 5 "green")))
              (make-tetra (make-posn 5 -1)
                                      (list 
                                       (make-block 3 7 "red")
                                       (make-block 4 7 "red")
                                       (make-block 4 6 "red")
                                       (make-block 5 6 "red"))))
                                                      
;;move-bset : Bset Number Bset : -> Bset
;;Moves the given bset according to the movement of the tetra
(define (move-bset bset x-dir pile)
  (local ((define (move block)
            (make-block (- (block-x block) x-dir)
                        (- (block-y block) 1)
                        (block-color block))))
    (cond
      [(or (empty? bset)
           (hit-bottom? bset pile)) bset]
      [else (map move bset)])))

;EXAMPLES/TESTS
(check-expect (move-bset BSETO 0 empty)
              (list
               (make-block 4 20 "green")
               (make-block 5 20 "green")
               (make-block 4 19 "green")
               (make-block 5 19 "green")))

(check-expect (move-bset BSETI 1 empty)
              (list
               (make-block 2 19 "blue")
               (make-block 3 19 "blue")
               (make-block 4 19 "blue")
               (make-block 5 19 "blue")))

(check-expect (move-bset BSETJ -1 empty)
              (list
               (make-block 5 20 "cyan")
               (make-block 5 19 "cyan")
               (make-block 6 19 "cyan")
               (make-block 7 19 "cyan")))

(check-expect (move-bset (list 
                          (make-block 4 0 "orange")
                          (make-block 5 0 "orange")
                          (make-block 5 1 "orange")
                          (make-block 6 0 "orange")) 0 empty)
              (list 
                           (make-block 4 0 "orange")
                           (make-block 5 0 "orange")
                           (make-block 5 1 "orange")
                           (make-block 6 0 "orange")))

(check-expect (move-bset (list (make-block 3 7 "red")
                               (make-block 4 7 "red")
                               (make-block 4 6 "red")
                               (make-block 5 6 "red")) 0 (list (make-block 5 5 "green")))
              (list 
               (make-block 3 7 "red")
               (make-block 4 7 "red")
               (make-block 4 6 "red")
               (make-block 5 6 "red")))
              
                           
;;move-center : Posn Number -> Posn
;;Moves the given center (as a posn) according the movement of the tetra
(define (move-center center x-dir)
  (make-posn (- (posn-x center) x-dir) (- (posn-y center) 1)))

;EXAMPLES/TESTS
(check-expect (move-center (make-posn 0 0) 0)
              (make-posn 0 -1))

(check-expect (move-center (make-posn 0 0) 1)
              (make-posn -1 -1))

(check-expect (move-center (make-posn 0 0) -1)
              (make-posn 1 -1))

;;tetra-hit-bottom? : Tetra Bset -> Boolean
;;helper function to determine if a tetra has hit the screen or pile
(define (tetra-hit-bottom? tetra pile)
  (hit-bottom? (tetra-blocks tetra) pile))

;EXAMPLES/TESTS
(check-expect (tetra-hit-bottom? TETRAO empty) false)

(check-expect (tetra-hit-bottom? TETRAL (list (make-block 5 19 "green"))) true)

(check-expect (tetra-hit-bottom? (make-tetra (make-posn 5 0)
                                      (list 
                                       (make-block 4 0 "orange")
                                       (make-block 5 0 "orange")
                                       (make-block 5 1 "orange")
                                       (make-block 6 0 "orange"))) empty) true)

;;hit-bottom? : Bset Bset -> Boolean
;;determines if the first given Bset hits the ground or the top of the other Bset
(define (hit-bottom? bset pile)
  (cond
    [(empty? bset) false]
    [(or 
      (block-hit-bottom? (first bset))
      (block-hit-pile? (first bset) pile)) true]
    [else (hit-bottom? (rest bset) pile)]))

;EXAMPLES/TESTS
(check-expect (hit-bottom? BSETO empty) false)

(check-expect (hit-bottom? BSETL (list (make-block 5 19 "green"))) true)

(check-expect (hit-bottom? (list 
                            (make-block 4 0 "orange")
                            (make-block 5 0 "orange")
                            (make-block 5 1 "orange")
                            (make-block 6 0 "orange")) empty) true)

;;block-hit-bottom? : Block -> Boolean
;;determines if a specific block in a Bset has hit the bottom of the screen
(define (block-hit-bottom? block)
  (= (block-y block) 0))

;EXAMPLES/TESTS
(check-expect (block-hit-bottom? L1) false)

(check-expect (block-hit-bottom? (make-block 4 0 "orange")) true)

;;block-hit-pile? : Block Bset -> Boolean
;;determines if a specific block in a Bset has hit the top of a given pile
(define (block-hit-pile? block pile)
  (cond
    [(empty? pile) false]
    [(and
      (= (block-y block) (add1 (block-y (first pile))))
      (<= (block-x block) (block-x (first pile)))
      (>= (block-x block) (block-x (first pile))))
     true]
    [else (block-hit-pile? block (rest pile))]))

;EXAMPLES/TESTS/
(check-expect (block-hit-pile? L1 empty) false)

(check-expect (block-hit-pile? L1 (list (make-block 5 19 "green"))) false)

(check-expect (block-hit-pile? L1 (list (make-block 4 19 "green"))) true)

(check-expect (block-hit-pile? L1 (list (make-block 3 19 "green") (make-block 4 19 "green"))) true)

;;tetra-cant-move-left? : Tetra Bset -> Boolean
;;helper function to determine if a tetra can not move to the left
(define (tetra-cant-move-left? tetra pile)
  (cant-move-left? (tetra-blocks tetra) pile))

;EXAMPLES/TESTS
(check-expect (tetra-cant-move-left? TETRAS empty) false)

(check-expect (tetra-cant-move-left? (make-tetra (make-posn .5 5.5)
                                                (list
                                                 (make-block 0 6 "green")
                                                 (make-block 1 6 "green")
                                                 (make-block 0 5 "green")
                                                 (make-block 1 5 "green"))) empty) true)

(check-expect (tetra-cant-move-left? (make-tetra (make-posn 2.5 5.5)
                                                (list
                                                 (make-block 2 6 "green")
                                                 (make-block 3 6 "green")
                                                 (make-block 2 5 "green")
                                                 (make-block 3 5 "green"))) 
                                    (list
                                     (make-block 1 5 "green"))) 
              true)

;;tetra-cant-move-right? : Tetra Bset -> Boolean
;;helper function to determine if a tetra can not move to the left
(define (tetra-cant-move-right? tetra pile)
  (cant-move-right? (tetra-blocks tetra) pile))

;EXAMPLES/TESTS
(check-expect (tetra-cant-move-right? TETRAS empty) false)

(check-expect (tetra-cant-move-right? (make-tetra (make-posn 8.5 5.5)
                                                (list
                                                 (make-block 8 6 "green")
                                                 (make-block 9 6 "green")
                                                 (make-block 8 5 "green")
                                                 (make-block 9 5 "green"))) empty) true)

(check-expect (tetra-cant-move-right? (make-tetra (make-posn 2.5 5.5)
                                                (list
                                                 (make-block 2 6 "green")
                                                 (make-block 3 6 "green")
                                                 (make-block 2 5 "green")
                                                 (make-block 3 5 "green"))) 
                                    (list
                                     (make-block 4 5 "green"))) 
              true)

;;cant-move-left? : Bset Bset -> Boolean
;;determines if the first given Bset can not move left by checking
;;if it hits the wall or the second given bset
(define (cant-move-left? bset pile)
  (cond
    [(empty? bset) false]
    [(or
      (block-cant-move-left? (first bset))
      (block-hit-left-pile? (first bset) pile)) true]
    [else (cant-move-left? (rest bset) pile)]))

;EXAMPLES/TESTS
(check-expect (cant-move-left? BSETL empty) false)

(check-expect (cant-move-left? (list
                                      (make-block 0 6 "green")
                                      (make-block 1 6 "green")
                                      (make-block 0 5 "green")
                                      (make-block 1 5 "green")) empty) true)

(check-expect (cant-move-left? (list
                                      (make-block 2 6 "green")
                                      (make-block 3 6 "green")
                                      (make-block 2 5 "green")
                                      (make-block 3 5 "green")) 
                                    (list
                                     (make-block 1 5 "green"))) 
              true)


;;block-cant-move-left? : Block -> Boolean
;;determines if a specific block in a Bset can not move left by checking if it hits the wall
(define (block-cant-move-left? block)
  (= (block-x block) 0))

;EXAMPLES/TESTS
(check-expect (block-cant-move-left? L1) false)

(check-expect (block-cant-move-left? (make-block 0 6 "green")) true)

;;block-hit-left-pile? : Block Bset -> Boolean
;;determines if a specific block can not move left by checking
;;if it hits another block in the given bset
(define (block-hit-left-pile? block pile)
  (cond
    [(empty? pile) false]
    [(and 
      (= (block-x block) (add1 (block-x (first pile))))
      (<= (block-y block) (add1 (block-y (first pile))))
      (>= (block-y block) (block-y (first pile))))
     true]
    [else (block-hit-left-pile? block (rest pile))]))

;EXAMPLES/TESTS
(check-expect (block-hit-left-pile? L1 empty) false)

(check-expect (block-hit-left-pile? (make-block 2 4 "green") (list (make-block 1 5 "green"))) false)

(check-expect (block-hit-left-pile? (make-block 2 5 "green") (list (make-block 1 5 "green"))) true)


;;cant-move-right? : Bset Bset -> Boolean
;;determines if the first given Bset can not move right by checking
;;if it hits the wall or the second given bset
(define (cant-move-right? bset pile)
  (cond
    [(empty? bset) false]
    [(or 
      (block-cant-move-right? (first bset))
      (block-hit-right-pile? (first bset) pile)) true]
    [else (cant-move-right? (rest bset) pile)]))

;EXAMPLES/TESTS
(check-expect (cant-move-right? BSETL empty) false)

(check-expect (cant-move-right? (list
                                      (make-block 8 6 "green")
                                      (make-block 9 6 "green")
                                      (make-block 8 5 "green")
                                      (make-block 9 5 "green")) empty) true)

(check-expect (cant-move-right? (list
                                      (make-block 2 6 "green")
                                      (make-block 3 6 "green")
                                      (make-block 2 5 "green")
                                      (make-block 3 5 "green")) 
                                    (list
                                     (make-block 4 5 "green"))) 
              true)

;;block-cant-move-right? : Block -> Boolean
;;determines if a specific block in a Bset can not move right by checking if it hits the wall
(define (block-cant-move-right? block)
  (= (block-x block) 9))

;EXAMPLES/TESTS
(check-expect (block-cant-move-right? L1) false)

(check-expect (block-cant-move-right? (make-block 9 6 "green")) true)

;block-hit-right-pile? : Block Bset -> Boolean
;determines if a specific block can not move right by checking
;if it hits another block in the given bset
(define (block-hit-right-pile? block pile)
  (cond
    [(empty? pile) false]
    [(and 
      (= (block-x block) (sub1 (block-x (first pile))))
      (<= (block-y block) (add1 (block-y (first pile))))
      (>= (block-y block) (block-y (first pile))))
     true]
    [else (block-hit-right-pile? block (rest pile))]))

;EXAMPLES/TESTS
(check-expect (block-hit-right-pile? L1 empty) false)

(check-expect (block-hit-right-pile? (make-block 0 4 "green") (list (make-block 1 5 "green"))) false)

(check-expect (block-hit-right-pile? (make-block 2 5 "green") (list (make-block 3 5 "green"))) true)

;;cant-rotate? : Tetra Bset -> Boolean
;;Determines if a given tetra can rotate or not
(define (cant-rotate? tetra pile)
  (or (tetra-cant-move-left? tetra pile)
      (tetra-cant-move-right? tetra pile)
      (tetra-hit-bottom? tetra pile)))

;EXAMPLES/TESTS
(check-expect (cant-rotate? (world-tetra world1) (world-pile world1)) true)
(check-expect (cant-rotate? (world-tetra world2) (world-pile world2)) false)

;;rotate-tetra-ccw : Tetra -> Tetra
;;helper function to rotate a tetra ccw
(define (rotate-tetra-ccw tetra)
  (make-tetra (tetra-center tetra)
              (bset-rotate-ccw (tetra-center tetra) (tetra-blocks tetra))))

;EXAMPLES/TESTS
(check-expect (rotate-tetra-ccw TETRAS) 
              (make-tetra (tetra-center TETRAS)
              (bset-rotate-ccw (tetra-center TETRAS) 
                               (tetra-blocks TETRAS))))

;;bset-rotate-ccw : Posn Bset -> Bset
;;rotates given bset ccw around given center
(define (bset-rotate-ccw center bset)
  (cond
    [(empty? bset) empty]
    [else (cons (block-rotate-ccw center (first bset))
                (bset-rotate-ccw center (rest bset)))]))

;EXAMPLES/TESTS
(check-expect (bset-rotate-ccw CENTERO BSETO)
 (cons
 (make-block 4 20 "green")
 (cons
  (make-block 4 21 "green")
  (cons
   (make-block 5 20 "green")
   (cons (make-block 5 21 "green") empty)))))

;;rotate-tetra-cw : Tetra -> Tetra
;;helper function to rotate a tetra cw
(define (rotate-tetra-cw tetra)
  (make-tetra (tetra-center tetra)
              (bset-rotate-cw (tetra-center tetra) (tetra-blocks tetra))))

;EXAMPLES/TESTS
(check-expect (rotate-tetra-cw TETRAS)
(make-tetra
 (make-posn 5 20)
 (cons
  (make-block 5 21 "red")
  (cons
   (make-block 5 20 "red")
   (cons
    (make-block 6 20 "red")
    (cons (make-block 6 19 "red") empty))))))


;;bset-rotate-cw : Posn Bset -> Bset
;;rotates given bset cw around given center
(define (bset-rotate-cw center bset)
  (cond
    [(empty? bset) empty]
    [else (cons (block-rotate-cw center (first bset))
                (bset-rotate-cw center (rest bset)))]))

;EXAMPLES/TESTS
(check-expect (bset-rotate-cw CENTERS BSETS)
(cons
 (make-block 5 21 "red")
 (cons
  (make-block 5 20 "red")
  (cons
   (make-block 6 20 "red")
   (cons (make-block 6 19 "red") empty)))))
                             
;; block-rotate-ccw : Posn Block -> Block
;; Rotate the block 90 counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))

;EXAMPLES/TESTS:
(check-expect (block-rotate-ccw CENTERS S1)
              (make-block 5 19 "red"))

;; block-rotate-cw : Posn Block -> Block
;; Rotate the block 90 clockwise around the posn.
(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))

;EXAMPLES/TESTS
(check-expect (block-rotate-cw CENTERS S1)
              (make-block 5 21 "red"))

;;Bset->ylist: BSet -> LON
;;Maps given BSet into a list of numbers based on y-values of blocks
(define (Bset->ylist pile)
  (local ((define (make-y block) (block-y block)))
    (map make-y pile)))

;EXAMPLES/TESTS:
(check-expect (Bset->ylist (world-pile world1)) (list 20 20 21 21))
(check-expect (Bset->ylist (world-pile world2)) empty)

;;remove-row: Bset Number -> Bset
;;removes row at given y value
(define (remove-row pile y)
  (filter (λ (block) (not (= (block-y block) y))) pile))

;EXAMPLES/TESTS:
(check-expect (remove-row (world-pile world1) 20) (list S3 S4))
(check-expect (remove-row (world-pile world1) 5) (list S1 S2 S3 S4))
(check-expect (remove-row (world-pile world2) 20) empty)

;;lower-blocks: Bset  Number -> Bset
;;Lowers all blocks in a given Bset (above the given row) by one row
(define (lower-blocks pile y)
  (cond [(empty? pile) empty]
        [(> (block-y (first pile)) y)
         (cons
          (make-block (block-x (first pile))
                      (sub1 (block-y (first pile)))
                      (block-color (first pile))) 
          (lower-blocks (rest pile) y))]
        [else (cons (first pile) (lower-blocks (rest pile) y))]))

;EXAMPLES/TESTS:
(check-expect (lower-blocks (world-pile world1) 10) (list (make-block 4 19 "red")
                                                          (make-block 5 19 "red")
                                                          (make-block 5 20 "red")
                                                          (make-block 6 20 "red")))

(check-expect (lower-blocks (world-pile world1) 22) (list S1 S2 S3 S4))
(check-expect (lower-blocks (world-pile world2) 22) empty)

;;adjust-blocks: Bset Number -> Bset
;;Both removes the full row and lowers blocks above the row
(define (adjust-blocks pile complete-row)
  (lower-blocks (remove-row pile complete-row) complete-row))

;EXAMPLES/TESTS
(check-expect (adjust-blocks (world-pile world1) 20) (list (make-block 5 20 "red")
                                                           (make-block 6 20 "red")))
(check-expect (adjust-blocks (world-pile world2) 22) empty)

;count-number: Number LON -> Number
;Determines how many times a given number appears in a given lon


;EXAMPLES/TESTS:
;(check-expect (count-number 3 empty) 0)
;(check-expect (count-number 3 (list 1 2 3 4 3)) 2)

;;full-row: LON -> Number
;;Takes list of y values. Determines if any row is full. 
;;If so, returns the row's y value. If none are full, returns -1
(define (full-row x ylist)
 (local ((define (count-number x ylist)
  (cond [(empty? ylist) 0]
        [(= x (first ylist)) (add1 (count-number x (rest ylist)))]
        [else (count-number x (rest ylist))])))
  (cond [(or (empty? ylist)
             (= x BOARD-HEIGHT)) -1]
        [(>= (count-number x ylist) BOARD-WIDTH) x]
        [else (full-row (add1 x) ylist)])))

;EXAMPLES/TESTS:
(check-expect (full-row 0 (list 1 2 2 3 5 4 4 4 4 6 7 4 4 4 2 3 4 4 9 4)) 4)
(check-expect (full-row 0 (list 1 2 3 7 7 8 3 5 6 9 9)) -1)
(check-expect (full-row 0 empty) -1)

;;check-pile: Bset -> Bset
;;Checks for and any full rows. If there are any, it removes them and lowers the rest of the bset
(define (check-pile pile)
  (cond [(> (full-row 0 (Bset->ylist pile)) -1)
         (check-pile (adjust-blocks pile (full-row 0 (Bset->ylist pile))))]
        [else pile]))

;EXAMPLES/TESTS:
(check-expect (check-pile (world-pile world1)) BSETS)

(check-expect (check-pile a-full-row) empty)

;;;Rendering Functions

;;world->image : World -> Image
;; draws the world (tetra and pile)
(define (world->image world)
  (tetra+image (world-tetra world)
               (bset+image (world-pile world)
                           BOARD)))

;EXAMPLES/TESTS
(check-expect (world->image INITIAL-WORLD)
               (tetra+image (world-tetra INITIAL-WORLD)
               (bset+image (world-pile INITIAL-WORLD)
                           BOARD)))

;;place-image/grid : Image Number Number Image -> Image
;; Just like place-image, except in grid coordinates
(define (place-image/grid img1 x y img2)
  (place-image img1
               (* (+ x 1/2) PIXELS/GRID)
               (* (- BOARD-HEIGHT (+ y 1/2)) PIXELS/GRID)
               img2))

;EXAMPLES/TESTS
(check-expect (place-image/grid SIMAGE 10 10 BOARD)
      (place-image SIMAGE 210 190 BOARD))

;;tetra+image : Tetra Image -> Image
;; Draws the tetra onto the provided image
(define (tetra+image tetra image)
  (bset+image (tetra-blocks tetra) image))

;EXAMPLES/TESTS
(check-expect (tetra+image TETRAS BOARD)
            (bset+image (tetra-blocks TETRAS) BOARD))  

;;bset+image : Bset Image -> Image
;; Draws the bset onto the provided image
(define (bset+image bset image)
  (cond
    [(empty? bset) image]
    [else 
     (place-image/grid (overlay
                        (square 20 "outline" 'black)
                        (square 20 "solid" (block-color (first bset))))
                       (block-x (first bset))
                       (block-y (first bset))
                       (bset+image (rest bset) image))]))

;EXAMPLES/TESTS
(check-expect (bset+image BSETS BOARD)
            (place-image/grid (overlay
                        (square 20 "outline" 'black)
                        (square 20 "solid" (block-color (first BSETS))))
                       (block-x (first BSETS))
                       (block-y (first BSETS))
                       (bset+image (rest BSETS) BOARD)))
(check-expect (bset+image empty BOARD)
              BOARD)

;;;Big Bang Functions

;;next-world : World -> World
;;takes the world and returns the next world
(define (next-world world)
  (cond
    [(tetra-hit-bottom? (world-tetra world) (world-pile world)) 
     (make-world (new-tetra (random 7)) 
                 (append (tetra-blocks (world-tetra world)) (world-pile world)))]
    [(> (full-row 0 (Bset->ylist (world-pile world))) -1)
     (make-world (new-tetra (random 7))
                 (adjust-blocks (world-pile world) (full-row 0 (Bset->ylist (world-pile world)))))]
    [else (make-world (move-tetra (world-tetra world) 0 (world-pile world)) 
                      (world-pile world))]))

;EXAMPLES/TESTS
(check-random (next-world world1) 
              (make-world (new-tetra (random 7)) 
                 (append (tetra-blocks (world-tetra world1))
                         (world-pile world1))))

(check-expect (next-world world2)
              (make-world (move-tetra (world-tetra world2) 0 (world-pile world2))
                          (world-pile world2)))

(check-random (next-world (make-world TETRAS a-full-row))
              (make-world (new-tetra (random 7)) empty))

;;world-game-over? : World -> Boolean
;;helper fuction to determine if game is over or not
(define (world-game-over? world)
  (game-over? (world-pile world)))

;EXAMPLES/TESTS
(check-expect (world-game-over? world1)
  (game-over? (world-pile world1)))

;;game-over? : Bset -> Boolean
;;determines whether game is over (pile being above BOARD-HEIGHT)
(define (game-over? pile)
  (cond
    [(empty? pile) false]
    [(<= BOARD-HEIGHT (block-y (first pile))) true]
    [else (game-over? (rest pile))]))

;EXAMPLES/TESTS
(check-expect (game-over? empty) false)
(check-expect (game-over? BSETS) true)
(check-expect (game-over? (list (make-block 1 1 "red") 
                                (make-block 100 100 "red") empty)) true)

;;end-game-image : World -> Image
;;produces a "game over" and "score" image based on the given world
(define (end-game-image world)
  (place-image (text "GAME OVER" 26 'red)
               100 180
               (place-image  (text 
                              (string-append "Score: "
                                             (number->string (world-get-score world)))
                              20 'red)
               100 220
               BOARD)))

;EXAMPLES/TESTS
(check-expect (end-game-image world1)
  (place-image (text "GAME OVER" 26 'red)
               100 180
               (place-image (text 
                              (string-append "Score: "
                              (number->string (world-get-score world1)))
                              20 'red)
               100 220 BOARD))) 

;;world-get-score : World -> Number
;;helper function to determine the score
(define (world-get-score world)
  (get-score (world-pile world)))

;EXAMPLES/TESTS
(check-expect (world-get-score world1)
  (get-score (world-pile world1)))

;;get-score : Bset -> Number
;;determines the score by adding up the blocks in a given pile
(define (get-score pile)
  (cond
    [(empty? pile) 0]
    [else (add1 (get-score (rest pile)))]))

;EXAMPLES/TESTS
(check-expect (get-score empty) 0)
(check-expect (get-score BSETS) 4) 

;; Key-Handler : World Keyevent -> World
;; To respond to keystrokes


(define (key-handler world key)
  (cond
    [(string=? key "left")
     (if (tetra-cant-move-left? (world-tetra world) (world-pile world))
         world
         (make-world (move-tetra (world-tetra world) 1 (world-pile world)) 
                     (world-pile world)))]
    [(string=? key "right")
     (if (tetra-cant-move-right? (world-tetra world) (world-pile world))
         world
         (make-world (move-tetra (world-tetra world) -1 (world-pile world)) 
                     (world-pile world)))]
    [(string=? key "a") 
     (if (cant-rotate? (world-tetra world) (world-pile world))
         world
         (make-world (rotate-tetra-ccw (world-tetra world)) (world-pile world)))]
    [(string=? key "s")
     (if (cant-rotate? (world-tetra world) (world-pile world))
         world
         (make-world (rotate-tetra-cw (world-tetra world)) 
                                    (world-pile world)))]
    [else world]))

;EXAMPLES/TESTS:
(check-expect (key-handler world1 "left") world1)
(check-expect (key-handler world2 "left")
              (make-world (move-tetra (world-tetra world2) 1 (world-pile world2))
                          (world-pile world2)))
(check-expect (key-handler world1 "right") world1)
(check-expect (key-handler world2 "right")
              (make-world (move-tetra (world-tetra world2) -1 (world-pile world2))
                          (world-pile world2)))
(check-expect (key-handler world1 "a") world1)
(check-expect (key-handler world2 "a")
              (make-world (rotate-tetra-ccw (world-tetra world2)) (world-pile world2)))
(check-expect (key-handler world1 "s") world1)
(check-expect (key-handler world2 "s")(make-world (rotate-tetra-cw (world-tetra world2)) 
                                    (world-pile world2)))
(check-expect (key-handler world1 "q") world1)

;;Main: World->Big Bang
(define (main w)
  (big-bang w
          (on-tick next-world 1/3)
          (to-draw world->image)
          (on-key key-handler)
          (stop-when world-game-over? end-game-image)))
(main INITIAL-WORLD)