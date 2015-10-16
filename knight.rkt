#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "extension.rkt")
  
;; Constants
(define size 40)
(define horizontal 5)
(define vertical 5)
(define hints? #t)
(define hintkey " ")

;; Game state structure
(struct game (pos history hint))

;; Graphics setup
(define knight (circle (/ size 4) 'solid (color 255 50 80)))
(define board (letrec ([setup-board (lambda (surface x y)
                                      (let ([surface (place-image (square size 'solid (if (even? (+ x y)) 'black 'white))
                                                                  (+ (* x size) (/ size 2))
                                                                  (+ (* y size) (/ size 2))
                                                                  surface)])
                                        (if (< x 7)
                                            (setup-board surface (++ x) y)
                                            (if (< y 7)
                                                (setup-board surface 0 (++ y))
                                                surface))))])
                (setup-board (empty-scene (* size horizontal) (* size vertical)) 0 0)))

;; Calculate possible fields
(define (possible-fields state)
  (filter (lambda (v)
              (and (not (list-contains? (game-history state) v))
                   (>= (vec-x v) 0)
                   (< (vec-x v) horizontal)
                   (>= (vec-y v) 0)
                   (< (vec-y v) vertical)))
          (map (lambda (v)
                 (vec-add (game-pos state) v))
               (let ([vecs (vec-cartesian '(1 -1) '(2 -2))])
                 (append vecs (map reverse-vector vecs))))))

;; Calculate optimal fields
(define (hinted-fields state)
  (if (game-hint state)
      (foldl (lambda (new bests)
               (if (not (null? bests))
                   (let ([newlen (length (possible-fields (struct-copy game state (pos new))))]
                         [bestlen (length (possible-fields (struct-copy game state (pos (car bests)))))])
                     (cond [(< newlen bestlen) (list new)]
                           [(= newlen bestlen) (cons new bests)]
                           [else bests]))
                   (list new)))
             null (possible-fields state))
      null))

;; Move knight on click
(define (move-to state x y event)
  (if (equal? event "button-up")
      (let ([click (vector (quotient x size) (quotient y size))])
        (cond [(null? (game-pos state))
               (struct-copy game state (pos click))]
              [(list-contains? (possible-fields state) click)
               (struct-copy game state
                            [pos click]
                            [history (cons (game-pos state) (game-history state))])]
              [else state]))
      state))

;; Enable hints
(define (show-hints state event)
  (if (and hints? (equal? event hintkey))
      (struct-copy game state (hint (not (game-hint state))))
      state))

;; Highlight field
(define (highlight-field r g b)
  (lambda (field surface)
     (place-image (square size 'solid (color r g b 150))
                  (+ (* (vec-x field) size) (/ size 2))
                  (+ (* (vec-y field) size) (/ size 2))
                  surface)))

;; Draw function
(define (render state)
  (if (vector? (game-pos state))
      (place-image knight
                   (+ (* (vec-x (game-pos state)) size) (/ size 2))
                   (+ (* (vec-y (game-pos state)) size) (/ size 2))
                   (foldl (highlight-field 0 255 0)
                          (foldl (highlight-field 0 0 255)
                                 (foldl (highlight-field 255 0 0)
                                        board (game-history state))
                                 (possible-fields state))
                          (hinted-fields state)))
      board))

;; Main event handler
(big-bang (game null null hints?)
          (on-mouse move-to)
          (on-release show-hints)
          (to-draw render))
