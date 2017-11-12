#lang racket

; http://www.cs.toronto.edu/~radford/csc121.S17/week6.pdf

; Plot random walk.
; Support library, and plotting, in racket follows.
;
; random_walk <- function (steps) {
;   position <- numeric(steps+1)
;   for (i in 1:steps) {
;     if (runif(1) < 0.5)
;       position[i+1] <- position[i] + 1
;     else
;       position[i+1] <- position[i] - 1
;   }
;   position
; }
; plot(random_walk(200))
; plot(random_walk(200))
; plot(random_walk(200))


#| Simple indexed plotting. |#

(require "plot.rkt")


#| Simple time series library. |#

#;(repeat-times f seed times)
; produces list of length ‘times’ with:
#;(list seed (f seed) (f (f seed)) ...)

(module Series racket (provide repeat-times)
  (define (repeat-times f seed times)
    (if (zero? times)
        '()
        (list* seed (repeat-times f (f seed) (sub1 times))))))


#| Use the libraries. |#

(require 'Series)

; Plot three random walks:

#;(step n) ; → n ± 1.

(define (step position)
  #;(if (zero? (random 2)) (+ position 1) (- position 1))
  #;(+ position (if (zero? (random 2)) +1 -1))
  #;(if (zero? (random 2)) (add1 position) (sub1 position))
  ((if (zero? (random 2)) add1 sub1) position))
(plot (repeat-times step 0 200))
(plot (repeat-times step 0 200))
(plot (repeat-times step 0 200))


; csc121 pseudo-random number illustration:
; > nxt <- 1; series <- c()
; > for (i in 1:200) { nxt <- (nxt * 17) %% 31; series <- c(series,nxt) }
; > plot(series)

(define (next n) (remainder (* n 17) 31))
(define series (repeat-times next 1 200))
(plot series)
