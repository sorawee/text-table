#lang scribble/manual
@(require racket/sandbox
          scribble/example
          (for-label text-table
                     racket/base
                     racket/format))

@title{text-table}
@author{Laurent Orseau}

@(define my-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base '(require text-table))))

@defmodule[text-table]{
 A simple package to display utf-8 textual tables.}

To install:

@verbatim{raco pkg install text-table}

See the example in the main submodule of the @filepath{main.rkt} file.
You can observe the results by running:

@verbatim{racket -l text-table}

@examples[
 #:eval my-eval
;; Minimalistic example:
(displayln
 (table->string
  '((a b c d e f gggg h)
    (123 456 77 54 1  5646547987 41 1)
    (111 22 3333 44 5 6 7 8888))))

;; With more bells and whistles
(displayln
 (table->string
  '((a b c d e f gggg h)
    (123 456 77 54 1  5646547987 41 1)
    (111 22 3333 44 5 6 7 8888))
  #:border-style 'double
  #:framed? #f
  #:row-sep? #t
  #:align '(left center center center center center center right)))
 ]

@defproc[(table->string
          [table (listof list?)]
          [#:->string to-string procedure? ~a]
          [#:border-style border-style
           (or/c 'single 'space 'space-single 'rounded 'double 'latex)
           'single]
          [#:framed? framed? boolean? #t]
          [#:row-sep? row-sep? boolean? #t]
          [#:align align
           (or/c (listof (or/c 'left 'center 'right))
                 (or/c 'left 'center 'right))
           'left])
         string?]{
 Accepts a table specified as a list of lists, and returns a string
 representing the table. The lists must all be of the same length.

 The @racket[to-string] procedure is used to convert cell values to
 strings.

 The @racket[border-style] specifies the style of lines to be used
 in drawing the table.

 When @racket[framed?] is @racket[#true], a frame is drawn around the
 outside of the table.

 When @racket[row-sep?] is false, no separators are drawn between the
 rows.

 The @racket[align] specification indicates how the contents of the
 cells are to be aligned within their cells. A single-symbol specification
 applies to all cells, or a list of symbols of the same length as the
 rows can be applied in order to specify the alignment of each column
 independently. When @racket[align] is a list, it is trimmed to the length
 of the columns if it is too long, or the last element of the list is used
 for the remaining columns if it is too short.
}

@defproc[(simple-table->string
          [table (listof list?)]
          [#:->string to-string procedure? ~a]
          [#:border-style border-style
           (or/c 'single 'space 'space-single 'rounded 'double 'latex)
           'space]
          [#:framed? framed? boolean? #f]
          [#:row-sep? row-sep? boolean? #f]
          [#:align align
           (or/c (listof (or/c 'left 'center 'right))
                 (or/c 'left 'center 'right))
           'left])
         string?]{
  Like @racket[table->string], but prints the table instead of returning it
  and uses default arguments for a minimalistic table.
}
@examples[#:eval my-eval
          (displayln
           (simple-table->string
            #:align '(left right)
            '((a b c d e f gggg h)
              (123 456 77 54 1  5646547987 41 1)
              (111 22 3333 44 5 6 7 8888))))]

@defform[(print-table args ...)]{
Shorthand form for @racket[(displayln (table->string args ...))].
Takes the same arguments as @racket[table->string].
}

@defform[(print-simple-table args ...)]{
Shorthand form for @racket[(displayln (simple-table->string args ...))].
Takes the same arguments as @racket[simple-table->string].
}