#lang racket

(define empty-list 47)
(define fixnum-shift 2)
(define bool-shift 7)
(define bool-tag 31)
(define char-tag 15)
(define char-shift 8)

(define (format x)
  (cond
    [(integer? x) (arithmetic-shift x fixnum-shift)]
    [(char? x) (bitwise-xor char-tag (arithmetic-shift (char->integer x) char-shift))]
    [(boolean? x) (bitwise-xor bool-tag (arithmetic-shift (if x 1 0) bool-shift))]
    [(and (list? x) (empty? x)) empty-list]))

(define out (open-output-file "entry.s" #:exists 'replace))

(define (emit . args)
  (apply fprintf (cons out args))
  (fprintf out "~n"))

(define (compile-program x)
  (emit ".text")
  (emit ".globl _entry, @function")
  (emit "_entry:")
  (emit "mov $~a, %eax" (format #\C))
  (emit "ret"))

(compile-program true)

(close-output-port out)