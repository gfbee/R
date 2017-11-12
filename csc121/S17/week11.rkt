#lang racket ; http://www.cs.toronto.edu/~radford/csc121.S17/week11.pdf

; > colours <- c("red","blue","red","red","green","blue")
; > print(tcol <- table(colours))
; colours
;  blue green red
;     2     1   3
; > names(tcol)
; [1] "blue"  "green" "red"

(module Table racket (provide table (rename-out [dict-keys names]))

  (require data/splay-tree data/order)

  (define (make-table) (make-splay-tree datum-order))
  (define (dict-update! dict key updater failure-value)
    (dict-set! dict key (updater (if (dict-has-key? dict key)
                                     (dict-ref dict key)
                                     failure-value))))
  (define table
    (case-lambda [(s) (define result (make-table))
                      (for ([e s]) (dict-update! result e add1 0))
                      result]
                 [(s1 s2) (table (map list s1 s2))])))

(require 'Table)

(define colors '("red" "blue" "red" "red" "green" "blue"))

(define colors-table (table colors))
(names colors-table)

(require "plot.rkt")

(plot colors-table)

; > colours <- c("red","blue","red","red","green","blue")
; > shapes <- c("round","round","square","square","square","round")
; > table(colours,shapes)
;        shapes
; colours round square
;    blue     2      0
;   green     0      1
;     red     1      2
