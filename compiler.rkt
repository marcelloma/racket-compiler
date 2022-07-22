#lang racket

(define empty-list 47)
(define fixnum-shift 2)
(define bool-shift 7)
(define bool-tag 31)
(define char-tag 15)
(define char-shift 8)

(define eax "%eax")

;;; TODO: replace with counter + word-size
(define registers '("-4(%rsp)" "-8(%rsp)" "-12(%rsp)" "-16(%rsp)" "-20(%rsp)"))

(define (push-register register)
  (set! registers (cons register registers)))

(define (pop-register)
  (let ((register (car registers)))
    (set! registers (cdr registers))
     register))

(define (entry body)
  (emit ".text")
  (emit ".globl _entry, @function")
  (emit "_entry:")
  (body)
  (emit "ret"))

(define (add lhs rhs)
  (emit "addl ~a, ~a" lhs rhs))

(define (sub lhs rhs)
  (emit "subl ~a, ~a" lhs rhs))

(define (mul lhs rhs)
  (emit "imull ~a, ~a" lhs rhs))

(define (mov lhs rhs)
  (emit "movl ~a, ~a" lhs rhs))

(define (sal lhs rhs)
  (emit "sall $~a, ~a" lhs rhs))

(define (sar lhs rhs)
  (emit "sarl $~a, ~a" lhs rhs))

(define (emit-expr ast)
  (cond
    [(bin-op? ast) (bin-op ast)]
    [(immediate? ast) (mov (format  "$~a" (immediate ast)) eax)]))

(define (immediate? val)
  (or (integer? val) (char? val) (boolean? val) (and (list? val) (empty? val))))

(define (immediate val)
  (cond
    [(integer? val) (arithmetic-shift val fixnum-shift)]
    [(char? val) (bitwise-xor char-tag (arithmetic-shift (char->integer val) char-shift))]
    [(boolean? val) (bitwise-xor bool-tag (arithmetic-shift (if val 1 0) bool-shift))]
    [(and (list? val) (empty? val)) empty-list]))

(define (bin-op? ast)
  (match ast
    [(list op _ _) #t]
    [_ #f]))

(define (bin-op ast)
  (match ast
    [(list "+" lhs rhs)
      (let ((register (pop-register)))
        (emit-expr lhs)
        (mov eax register)
        (emit-expr rhs)
        (add register eax)
        (push-register register))]
    [(list "-" lhs rhs)
      (let ((register (pop-register)))
        (emit-expr rhs)
        (mov eax register)
        (emit-expr lhs)
        (sub register eax)
        (push-register register))]
    [(list "*" lhs rhs)
      (let ((register (pop-register)))
        (emit-expr lhs)
        (sar fixnum-shift eax)
        (mov eax register)
        (emit-expr rhs)
        (sar fixnum-shift eax)
        (mul register eax)
        (sal fixnum-shift eax)
        (push-register register))]))

(define out (open-output-file "entry.s" #:exists 'replace))

(define (emit . args)
  (apply fprintf (cons out args))
  (fprintf out "~n"))

(define (compile-program x)
  (entry
    (lambda ()
      (emit-expr '("*" ("+" 5 ("-" 5 1)) 10)))))

(compile-program true)

(close-output-port out)