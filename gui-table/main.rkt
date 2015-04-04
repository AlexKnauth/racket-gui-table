#lang racket

(provide table% cell-content/c panel? area?)

(require racket/gui/base)

;; cell-content means one of:
;;  - (λ (parent) (U gui-object cell-content))
;;  - String
(define cell-content/c
  (recursive-contract
   (or/c [panel? . -> . (or/c area? cell-content/c)]
         string?)
   #:chaperone))

(define panel? (is-a?/c panel%))
(define area? (is-a?/c area<%>))

(define (cell-content-proc content)
  (cond [(or ((procedure-arity-includes/c 1) content) (string? content))
         (define (content-proc parent)
           (cell-content->obj content #:parent parent))
         content-proc]
        [else (error 'cell-content-proc "expected cell-content/c, given: ~v" content)]))

(define (cell-content->obj content #:parent parent)
  (cond [(area? content)
         (unless (object=? (send content get-parent) parent)
           (error 'cell-content->obj "parent doesn't match"))
         content]
        [((procedure-arity-includes/c 1) content) (cell-content->obj (content parent) #:parent parent)]
        [(string? content) (string->message content #:parent parent)]
        [else
         (error 'cell-content->obj "expected (or/c area? cell-content/c), given: ~v" content)]))

(define table%
  (class vertical-panel%
    (init content
          [style '(vscroll)]
          [cell-alignment '(center center)]
          [min-column-width 0]
          [min-row-height   0]
          )
    
    (super-new [style style])
    
    (define horizontal-panel (new horizontal-panel%
                                  [parent this]
                                  [style style]))
    
    (define proc-rows
      (cell-content-rows->proc-rows content))
    
    (define/public (get-columns)
      (for/list ([column-panel (in-list (send horizontal-panel get-children))])
        (for/list ([cell-panel (in-list (send column-panel get-children))])
          (match-define (list cell) (send cell-panel get-children))
          cell)))
    
    (define/public (get-rows)
      (columns->rows (get-columns)))
    
    (define obj-columns
      (for/list ([proc-column (in-list (rows->columns proc-rows))])
        (define column-panel (new vertical-panel% [parent horizontal-panel] [min-width min-column-width]))
        (define obj-column
          (for/list ([content-proc (in-list proc-column)])
            (define cell-panel (new vertical-panel%
                                    [parent column-panel]
                                    [style '(border)]
                                    [alignment cell-alignment]
                                    [stretchable-height #f]))
            (define cell (content-proc cell-panel))
            cell))
        obj-column))

    (for ([obj-row (in-list (columns->rows obj-columns))])
      (define row-height
        (apply max min-row-height
               (for/list ([cell (in-list obj-row)])
                 (send cell min-height))))
      (for ([cell (in-list obj-row)])
        (send cell min-height row-height)))

    ))


(define (rows->columns rows)
  (apply map list rows))

(define (columns->rows columns)
  (apply map list columns))

(define (cell-content-rows->proc-rows content)
  (for/list ([row (in-list content)])
    (for/list ([content (in-list row)])
      (cell-content-proc content))))


(define (string->message s #:parent parent)
  (new message%
       [label s]
       [parent parent]))

