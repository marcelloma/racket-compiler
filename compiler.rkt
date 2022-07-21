#lang racket

(define out (open-output-file "entry.s" #:exists 'replace))

(define (emit . args)
  (apply fprintf (cons out args))
  (fprintf out "~n"))

(define (compile-program x)
  (emit ".text")
  (emit ".globl _entry, @function")
  (emit "_entry:")
  (emit "mov $~a, %eax" x)
  (emit "ret"))

(compile-program 420)

(close-output-port out)