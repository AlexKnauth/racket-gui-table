#lang scribble/manual

@(require (for-label gui-table
                     racket
                     racket/gui/base
                     ))

@title{gui-table}

@defmodule[gui-table]

@defclass[table% vertical-panel% ()]{
@defconstructor/auto-super[([content (listof (listof cell-content/c))]
                            [style (listof (or/c 'border 'deleted
                                                 'hscroll 'auto-hscroll
                                                 'vscroll 'auto-vscroll))
                                   '(vscroll)]
                            [cell-alignment (list/c (or/c 'left 'center 'right)
                                                    (or/c 'top 'center 'bottom))
                                            '(center center)]
                            [min-column-width (and/c real? (not/c negative?))
                                              0]
                            [min-row-height (and/c real? (not/c negative?))
                                            0]
                            )]{
An example use:
@(racketblock
  (define frame
    (new frame% [label "The Frame"] [width 400] [height 400]))
  (define my-table
    (new table%
         [parent frame]
         [content
          (list (list (λ (parent) (new message% [label "Hi!"] [parent parent]))
                      (λ (parent) (new message% [label "Hello!"] [parent parent])))
                (list (λ (parent) (new button% [label "I am a button!"] [parent parent]
                                       [callback (λ (b e) (displayln "You pressed the button!"))]))
                      (λ (parent) (new message% [label "Bye!"] [parent parent]))))]))
  (send frame show #t))
}
}

@defthing[cell-content/c
          chaperone-contract?
          #:value (recursive-contract
                   (or/c [panel? . -> . (or/c area? cell-content/c)]
                         string?)
                   #:chaperone)]{
Represents a cell in a @racket[table%].

It should either be a function of one argument, or a string.

If it's a function, then it should produce a gui object, and the argument to the
function should be passed as the parent to that object. 

An example would be:
@(racketblock
  (λ (parent)
    (new message% [label "Hello!"] [parent parent])))

If it's a string, then it is converted to a procedure that produces a
@racket[message%] object with the given string and the given parent.
}

@defproc[(panel? [v any/c]) boolean?]{
Returns true if @racket[v] is a @racket[panel%].
}

@defproc[(area? [v any/c]) boolean?]{
Returns true if @racket[v] is an @racket[area<%>].
}

