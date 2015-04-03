#lang racket/gui

(require "../main.rkt")

(define frame
  (new frame% [label "The Frame"] [width 400] [height 400]))

(define table
  (new table%
       [parent frame]
       [content
        (for/list ([i (in-range 5)])
          (for/list ([j (in-range 5)])
            (cond [(even? (+ i j)) (format "i: ~v, j: ~v" i j)]
                  [else (new button% [parent null-panel] [label "hi!"]
                             [callback (λ (x y) (displayln "hi!"))])])))]
       ))
