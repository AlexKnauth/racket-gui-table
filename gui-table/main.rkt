#lang racket

(provide table% null-panel)

(require racket/gui/base)

(define table%
  (class vertical-panel%
    (init content
          [style '(vscroll)]
          [cell-alignment '(center center)]
          [min-column-width 0]
          [min-row-height   0]
          [text-fields? #f])
    
    (super-new [style style])
    
    (define horizontal-panel (new horizontal-panel%
                                  [parent this]
                                  [style style]))
    
    (field [rows
            (local [(define (any->object x)
                      (cond [(object? x) x]
                            [(string? x)
                             (cond [text-fields?
                                    (string->text-field x #:parent null-panel)]
                                   [else
                                    (string->message x #:parent null-panel)])]
                            [else (any->object (~v x))]))]
              (for/list ([row (in-list content)])
                (for/list ([cell-content row])
                  (any->object cell-content))))])
    
    (define/public (change-column-panels filter-proc)
      (send* horizontal-panel (change-children filter-proc)))
    
    (define/public (change-column-lists filter-proc)
      (let* ([column-panels (send horizontal-panel get-children)]
             [column-lists (for/list ([column (in-list column-panels)])
                             (send column get-children))]
             [new-column-lists (filter-proc column-lists)])
        (for ([column-panel (in-list column-panels)]
              [column-list  (in-list new-column-lists)])
          (send* column-panel (change-children (lambda (children) column-list))))))
    
    (define (rows->columns rows)
      (apply map list rows))
    
    (define (columns->rows columns)
      (apply map list columns))
    
    (define columns (rows->columns rows))
    
    (define/public (change-row-lists filter-proc)
      (change-column-lists (compose1 rows->columns filter-proc columns->rows)))
    
    (for ([column (in-list columns)])
      (define column-panel (new vertical-panel% [parent horizontal-panel] [min-width min-column-width]))
      (for ([cell-content (in-list column)]
            [row-num (in-naturals)])
        (local [(define current-row (list-ref rows row-num))
                (define row-height
                  (apply max min-row-height
                         (for/list ([cell-content_0 (in-list current-row)])
                           (send cell-content_0 min-height))))
                (define cell-panel (new vertical-panel%
                                        [parent column-panel]
                                        [style '(border)]
                                        [alignment cell-alignment]
                                        [stretchable-height #f]))]
          (send* cell-content
            (reparent cell-panel)
            (min-height row-height)))))
    ))

(define (string->text-field s #:parent parent #:label [label #f])
  (new text-field%
       [init-value s]
       [parent parent]
       [label label]))

(define (string->message s #:parent parent)
  (new message%
       [label s]
       [parent parent]))

(define null-frame (new frame% [label "null-frame"] [width 100] [height 100]))
(define null-panel (new panel% [parent null-frame]))

