#lang racket

(provide table%)

(require racket/gui/base)

;; cell-content means one of:
;;  - (λ (parent) (U gui-object cell-content))
;;  - String

(define (cell-content-proc content)
  (cond [(or (procedure? content) (string? content))
         (define (content-proc parent)
           (cell-content->obj content #:parent parent))
         content-proc]
        [else (error 'cell-content-proc "expected (or/c procedure? string?), given: ~v" content)]))

(define (cell-content->obj content #:parent parent)
  (cond [(object? content)
         (unless (object=? (send content get-parent) parent)
           (error 'cell-content->obj "parent doesn't match"))
         content]
        [(procedure? content) (cell-content->obj (content parent) #:parent parent)]
        [(string? content) (string->message content #:parent parent)]
        [else
         (error 'cell-content->obj "expected (or/c object? procedure? string?), given: ~v" content)]))

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
      (for/list ([row (in-list content)])
        (for/list ([content row])
          (cell-content-proc content))))
    
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
    
    (define proc-columns (rows->columns proc-rows))
    
    (define/public (change-row-lists filter-proc)
      (change-column-lists (compose1 rows->columns filter-proc columns->rows)))
    
    (define obj-columns
      (for/list ([proc-column (in-list proc-columns)])
        (define column-panel (new vertical-panel% [parent horizontal-panel] [min-width min-column-width]))
        (define obj-column
          (for/list ([content-proc (in-list proc-column)]
                     [row-num (in-naturals)])
            (define current-proc-row (list-ref proc-rows row-num))
            (define cell-panel (new vertical-panel%
                                    [parent column-panel]
                                    [style '(border)]
                                    [alignment cell-alignment]
                                    [stretchable-height #f]))
            (define obj-cell (content-proc cell-panel))
            obj-cell))
        obj-column))
    (define obj-rows (columns->rows obj-columns))
    (for ([obj-row (in-list obj-rows)])
      (define row-height
        (apply max min-row-height
               (for/list ([obj-cell (in-list obj-row)])
                 (send obj-cell min-height))))
      (for ([cell (in-list obj-row)])
        (send cell min-height row-height)))
    ))

(define (string->message s #:parent parent)
  (new message%
       [label s]
       [parent parent]))

