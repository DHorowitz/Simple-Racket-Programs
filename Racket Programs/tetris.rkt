;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Dylan Horowitz
; 11/16/2015
; Simple Tetris

(require 2htdp/image)
(require 2htdp/universe)

;constants
(define WIDTH 10) ;Maximum number of horizontal blocks
(define HEIGHT WIDTH) ;Maximum number of vertical blocks
 
; graphical constants 
(define SIZE 20) ; blocks are square 
(define BLOCK ; they are rendered as red squares with black rims
  (overlay (rectangle SIZE SIZE "solid" "red")
           (rectangle SIZE SIZE "outline" "black")))


(define SCENE-SIZE (* WIDTH SIZE))
(define MT-SCENE
  (overlay (rectangle (- SCENE-SIZE 1) (- SCENE-SIZE 1) "solid" "white")
           (rectangle SIZE SIZE "outline" "black")))

; A Tetris is (make-tetris Block Landscape)
(define-struct tetris (block landscape))

; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)

; Block is (make-block N N)
(define-struct block (x y))


 
; interpretation given (make-tetris (make-block x y) (list b1 b2 ...))
; (x,y) is the logical position of the dropping block, while
; b1, b2, etc are the positions of the resting blocks 
; A logical position (x,y) determines how many SIZEs the block is
;   from the left—x—and from the top—y.

; Data examples
(define landscape0 '())
(define block-dropping (make-block (/ SIZE 2) (/ SIZE 2)))
(define tetris0 (make-tetris block-dropping landscape0))
(define tetris0-drop (make-tetris (make-block 0 SIZE) landscape0))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape1 (list block-landed))
(define landscape2 (list block-on-block block-landed))
(define tetris1 (make-tetris block-dropping landscape2))


; bottom?: tetris -> boolean
; checks to see if the currently - moving block has hit the bottom of the screen
(define (bottom? game)
  (cond
    [(empty? (tetris-landscape game)) (>= (block-y (tetris-block game)) (- SCENE-SIZE SIZE))]
    [else
     (cond
       [(and (= (block-x (first (tetris-landscape game))) (block-x (tetris-block game))) (= (block-y (first (tetris-landscape game))) (+ SIZE (block-y (tetris-block game))))) #true]
       [else (bottom? (make-tetris (tetris-block game) (rest (tetris-landscape game))))])]))

(check-expect (bottom? tetris0) #false)
(check-expect (bottom? tetris1) #false)
(check-expect (bottom? (make-tetris (make-block 5 SCENE-SIZE) landscape0)) #true)

; leftmost?: block -> boolean
; checks if the currently moving block is in the leftmost spot or not
(define (leftmost? b)
  (<= (block-x b) (/ SIZE 2)))

; rightmost?: block -> boolean
; checks if the currently moving block is in the rightmost spot or not
(define (rightmost? b)
  (>= (block-x b) 180))

; intersect?:  tetris -> boolean
; checks if the currently moving block would move into an already-existing block
(define (intersect? t)
  (cond
    [(empty? (tetris-landscape t)) #false]
    [(cons? (tetris-landscape t))
     (cond
       [(and (= (block-y (tetris-block t)) (block-y (first (tetris-landscape t)))) (= (block-x (tetris-block t)) (block-x (first (tetris-landscape t))))) #true]
       [else (intersect? (make-tetris (tetris-block t) (rest (tetris-landscape t))))])]))


; user-move-block: tetris -> tetris
; moves a block in the indicated direction
(define (user-move-block r k)
  (cond
    [(string=? "left" k)
     (cond
       [(leftmost? (tetris-block r)) (make-tetris (tetris-block r) (tetris-landscape r))]
       [(intersect? (make-tetris (make-block (- (block-x (tetris-block r)) SIZE) (block-y (tetris-block r))) (tetris-landscape r))) (make-tetris (tetris-block r) (tetris-landscape r))]
       [else (make-tetris (make-block (- (block-x (tetris-block r)) SIZE) (block-y (tetris-block r))) (tetris-landscape r))])]
    [(string=? "right" k)
     (cond
       [(rightmost? (tetris-block r)) (make-tetris (tetris-block r) (tetris-landscape r))]
       [(intersect? (make-tetris (make-block (+ (block-x (tetris-block r)) SIZE) (block-y (tetris-block r))) (tetris-landscape r))) (make-tetris (tetris-block r) (tetris-landscape r))]
       [else (make-tetris (make-block (+ SIZE (block-x (tetris-block r))) (block-y (tetris-block r))) (tetris-landscape r))])]))

; move-block: tetris -> tetris
; moves a block in a game of tetris
(define (move-block game)
  (cond
    [(bottom? game) (move-block (make-tetris (make-block (/ SIZE 2) (/ SIZE 2)) (cons (tetris-block game) (tetris-landscape game))))]
    [else (make-tetris (make-block (block-x (tetris-block game)) (+ SIZE (block-y (tetris-block game)))) (tetris-landscape game))]))


(define (tetris-main r)
  (big-bang tetris0
            (to-draw tetris-render)
            (on-tick tetris-tick r)
            (on-key user-move-block)
            (stop-when over?)))

; tetris-render: tetris -> image
; turns a tetris struct into an image
(define (tetris-render r)
  (cond
    [(empty? (tetris-landscape r)) (place-image BLOCK (block-x (tetris-block r)) (block-y (tetris-block r)) MT-SCENE)]
    [else (place-image BLOCK (block-x (first (tetris-landscape r))) (block-y (first (tetris-landscape r))) (tetris-render (make-tetris (tetris-block r) (rest (tetris-landscape r)))))]))

(define (tetris-tick r)
  (move-block r))

; over?: tetris -> boolean
; determines if the game is over
(define (over? r)
  (= 8 (number-in-row (tetris-landscape r) (block-x (tetris-block r)))))

; number-in-row: landscape number (row) -> number
; determines the number of blocks in a single row
(define (number-in-row l row)
  (cond
    [(empty? l) 0]
    [(cons? l)
     (local [(define count-of-rest (number-in-row (rest l) row))]
       (cond
         [(= row (block-x (first l))) (+ 1 count-of-rest)]
         [else count-of-rest]))]))



