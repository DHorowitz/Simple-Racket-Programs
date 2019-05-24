;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101 Sections 03 and 04
; Fall 2015
; Assign 2
; Dylan Horowitz
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300)

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (im x y dx dy))
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. empty, or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
;   These will help you test bounce conditions.

; here's one of my ball CONSTANTS, which you may use or modify
; if you like to define the rest.
(define BALL-LEFT 
  (make-ball (circle 10 "solid" "green")
             (+ RADIUS 4) (/ HEIGHT 2) -3 3)) 

(define BALL-RIGHT 
  (make-ball (circle 20 "solid" "red")
             (- (- WIDTH RADIUS) 1) (/ HEIGHT 2) -2 2)) 

(define BALL-TOP
  (make-ball (circle 25 "solid" "teal")
             (/ WIDTH 2) (+ RADIUS 1) -8 8))

(define BALL-BOTTOM 
  (make-ball (circle 15.6 "solid" "yellow")
             (/ WIDTH 4) (- HEIGHT RADIUS) -4 4))


; Define INIT-LOB to be a list-of-balls:
; You will use this to be the initial state of the world.
; I've defined it to be the empty list, but you should define it
; to contain the four example ball CONSTANTS you just defined. 
(define INIT-LOB (cons BALL-LEFT (cons BALL-RIGHT (cons BALL-TOP (cons BALL-BOTTOM '()))))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
; Use these to help you get started with the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fun-for-ball : ball -> ???
; Template for a function that consumes a ball
;(define (fun-for-ball b) 
;  (...(ball-im b)...
;   ...(ball-x b)...(ball-y b)...
;   ...(ball-dx b)...(ball-dy b)...))


; fun-for-lob list-of-balls -> ???
; Template for a function that consumes a list-of-balls
;(define (fun-for-list-of-balls lob)
;  (cond
;    [(empty? lob)...] 
;    [else (...(fun-for-ball (first lob))...
;           ...(fun-for-lob (rest lob))...)]))
;
;(define (fun-for-list-of-balls lob)
;  (cond
;    [(empty? lob) '()] 
;    [else ((fun-for-ball (first lob))
;           (fun-for-lob (rest lob)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the functions below, in order. I've supplied the
; signature, purpose statement, and header for each function.
;
; You provide the check-expect examples, and using the appropriate
; template, complete the function bodies.
;
; I recommend you proceed in order, and complete each function,
; with passing tests, before going on to the next.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; top-edge? : ball -> boolean
; determines whether the ball reached the top edge of scene
;(define (top-edge? b)...)

(define (top-edge? b)
  (<= (ball-y b) (/ (image-width (ball-im b)) 2)))

(check-expect (top-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) RADIUS -1 1)) #true)
(check-expect (top-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) (- RADIUS 10) -1 1)) #true)
(check-expect (top-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal") WIDTH (+ RADIUS 50) -1 1)) #false)

; bottom-edge? : ball -> boolean
; determines whether the ball reached the bottom edge of scene
;(define (bottom-edge? b) ...)

(define (bottom-edge? b)
  (>= (ball-y b) (- HEIGHT (/ (image-width (ball-im b)) 2))))

(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) (- HEIGHT RADIUS) -1 1)) #true)
(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) HEIGHT -1 1)) #true)
(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) RADIUS -1 1)) #false)

; left-edge? : ball -> boolean
; determines whether the ball reached the left edge of scene
;(define (left-edge? b) ...)

(define (left-edge? b)
  (<= (ball-x b) (/ (image-width (ball-im b)) 2)))

(check-expect (left-edge? (make-ball (circle RADIUS "solid" "teal") RADIUS RADIUS -1 1)) #true)
(check-expect (left-edge? (make-ball (circle RADIUS "solid" "teal") 0 RADIUS -1 1)) #true)
(check-expect (left-edge? (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) RADIUS -1 1)) #false)

; right-edge? : ball -> boolean
; determines whether the ball reached the right edge of scene
;(define (right-edge? b) ...)

(define (right-edge? b)
  (>= (ball-x b) (- WIDTH (/ (image-width (ball-im b)) 2))))

(check-expect (right-edge? (make-ball (circle RADIUS "solid" "teal") WIDTH RADIUS -1 1)) #true)
(check-expect (right-edge? (make-ball (circle RADIUS "solid" "teal") (- WIDTH RADIUS) RADIUS -1 1)) #true)
(check-expect (right-edge? (make-ball (circle RADIUS "solid" "teal") RADIUS RADIUS -1 1)) #false)

; reverse-up-down : ball -> ball
; reverse ball's up-down direction   
;(define (reverse-up-down b) ...)

(define (reverse-up-down b)
    (make-ball (ball-im b) (ball-x b)
               (+ (ball-y b) (* (* (ball-dy b) -1) (quotient RADIUS 5)))
               (ball-dx b) (* (ball-dy b) -1)))

(check-expect (reverse-up-down
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) RADIUS -1 1))
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) 20 -1 -1))

; reverse-left-right : ball -> ball
; reverse ball's left-right direction   
;(define (reverse-left-right b) ...)

(define (reverse-left-right b)
  (make-ball (ball-im b)
             (+ (ball-x b) (* (* (ball-dx b) -1) (quotient RADIUS 5)))
             (ball-y b) (* -1 (ball-dx b)) (ball-dy b)))

(check-expect (reverse-left-right
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) RADIUS -1 1))
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (+ 5(/ WIDTH 2)) RADIUS 1 1))

; bounce-up-down : ball -> ball
; changes direction of given ball if it hit the top or bottom edge
;(define (bounce-up-down b)
;  (cond
;    [(top-edge? b) ...]
;    [(bottom-edge? b) ...]
;    [else ...]))

(define (bounce-up-down b)
  (cond
    [(top-edge? b) (reverse-up-down b)]
    [(bottom-edge? b) (reverse-up-down b)]))

(check-expect (bounce-up-down
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) RADIUS -1 1))
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) 20 -1 -1))

; bounce-left-right : ball -> ball
; changes direction of given ball if it hit the left or right edge
;(define (bounce-left-right b)
;  (cond
;    [(left-edge? b) ...]
;    [(right-edge? b) ...]
;    [else ...]))

(define (bounce-left-right b)
  (cond
    [(left-edge? b) (reverse-left-right b)]
    [(right-edge? b) (reverse-left-right b)]
    [else (make-ball (ball-im b)
             (+ (ball-x b) (* (ball-dx b) (quotient RADIUS 5)))
             (+ (ball-y b) (* (ball-dy b) (quotient RADIUS 5)))
             (ball-dx b) (ball-dy b))]))

(check-expect (bounce-left-right
              (make-ball (circle (+ RADIUS 4) "solid" "teal") RADIUS RADIUS -1 1))
              (make-ball (circle (+ RADIUS 4) "solid" "teal") 30 RADIUS 1 1))

; move-ball : ball -> ball
; moves the given ball by its dx and dy amounts
;(define (move-ball b)
;    (cond
;      [(or (top-edge? b) (bottom-edge? b)) ...]
;      [else ...]))

(define (move-ball b)
  (cond
    [(or (top-edge? b) (bottom-edge? b)) (bounce-up-down b)]
    [else (bounce-left-right b)]))

(check-expect (move-ball
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) RADIUS -1 1))
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) 20 -1 -1))

(check-expect (move-ball
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) RADIUS -1 1))
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) 20 -1 -1))

(check-expect (move-ball
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (/ WIDTH 2) (+ 10 RADIUS) -1 1))
              (make-ball (circle (+ RADIUS 4) "solid" "teal") (- (/ WIDTH 2) 5) 40 -1 1))


; move-list-of-balls : list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
;(define (move-list-of-balls lob)
;  (cond
;    [(empty? lob) '()] 
;    [else ((move-ball (first lob))
;           (move-list-of-balls (rest lob)))]))

(define (move-list-of-balls lob)
  (cond
    [(empty? lob) '()]
    [else (cons (move-ball (first lob)) (move-list-of-balls (rest lob)))]))


; render-ball : ball image -> image
; renders given ball b on given background bg
;(define (render-ball b bg)
;  (... (ball-im b) ... (ball-x b) ... (ball-y b) ... bg ...))

(define (render-ball b bg)
  (place-image (ball-im b) (ball-x b) (ball-y b) bg))
  
; render-balls : list-of-balls -> image 
; produces image of each ball at each given current position on
; background.
; (Yes, I provided this function for you! You shouldn't have to
;  touch it if you've correctly implemented the functions above.)
(define (render-balls lob) 
  (cond [(empty? lob) BACKGROUND]
        [else (render-ball (first lob)
                           (render-balls (rest lob)))]))

; Here's the big-bang expression!
; It uses INIT-LOB as the initial state of the world.
; Once you've implemented move-list-of-balls, uncomment on-tick below.
(big-bang INIT-LOB
          (on-tick move-list-of-balls 1/28) 
          (to-draw render-balls))
