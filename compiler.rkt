#lang racket

(define null 47)
(define fixnum-shift 2)
(define bool-shift 7)
(define bool-tag 31)
(define char-shift 8)
(define char-tag 15)

(define eax "%eax")
(define al "%al")
(define rsp-index 1)
(define word-size -4)

(define (rsp)
  (format "~a(%rsp)" (* rsp-index word-size)))

(define (+rsp-index)
  (set! rsp-index (+ rsp-index 1)))

(define (-rsp-index)
  (set! rsp-index (- rsp-index 1)))

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

(define (movzbl lhs rhs)
  (emit "movzbl ~a, ~a" lhs rhs))

(define (cmp lhs rhs)
  (emit "cmpl ~a, ~a" lhs rhs))

(define (sete op)
  (emit "sete ~a" op))

(define (andb lhs rhs)
  (emit "andb ~a, ~a" lhs rhs))

(define (xor lhs rhs)
  (emit "xorl ~a, ~a" lhs rhs))

(define (sal lhs rhs)
  (emit "sall $~a, ~a" lhs rhs))

(define (sar lhs rhs)
  (emit "sarl $~a, ~a" lhs rhs))

(define (emit-expr ast)
  (cond
    [(un-op? ast) (un-op ast)]
    [(bin-op? ast) (bin-op ast)]
    [(immediate? ast) (mov (format  "$~a" (immediate ast)) eax)]))

(define (immediate? val)
  (or (integer? val) (char? val) (boolean? val) (and (list? val) (empty? val))))

(define (immediate val)
  (cond
    [(integer? val) (arithmetic-shift val fixnum-shift)]
    [(char? val) (bitwise-xor char-tag (arithmetic-shift (char->integer val) char-shift))]
    [(boolean? val) (bitwise-xor bool-tag (arithmetic-shift (if val 1 0) bool-shift))]
    [(and (list? val) (empty? val)) null]))

(define (un-op? ast)
  (match ast
    [(list op _) #t]
    [_ #f]))

(define (bin-op? ast)
  (match ast
    [(list op _ _) #t]
    [_ #f]))

(define (un-op ast)
  (match ast
    [(list '- ast)
      (+rsp-index)
      (emit-expr ast)
      (mov eax (rsp))
      (xor eax eax)
      (sub (rsp) eax)
      (-rsp-index)]
    [(list 'null? ast)
      (emit-expr ast)
      (cmp (format  "$~a" null) eax)
      (sete al)
      (andb (format "$~a" 1) al)
      (movzbl al eax)
      (sal bool-shift eax)
      (xor (format "$~a" bool-tag) eax)]
    [(list 'zero? ast)
      (emit-expr ast)
      (cmp (format "$~a" (immediate 0)) eax)
      (sete al)
      (andb (format "$~a" 1) al)
      (movzbl al eax)
      (sal bool-shift eax)
      (xor (format "$~a" bool-tag) eax)]))

(define (bin-op ast)
  (match ast
    [(list '+ lhs rhs)
      (+rsp-index)
      (emit-expr lhs)
      (mov eax (rsp))
      (emit-expr rhs)
      (add (rsp) eax)
      (-rsp-index)]
    [(list '- lhs rhs)
      (+rsp-index)
      (emit-expr rhs)
      (mov eax (rsp))
      (emit-expr lhs)
      (sub (rsp) eax)
      (-rsp-index)]
    [(list '* lhs rhs)
      (+rsp-index)
      (emit-expr lhs)
      (sar fixnum-shift eax)
      (mov eax (rsp))
      (emit-expr rhs)
      (sar fixnum-shift eax)
      (mul (rsp) eax)
      (sal fixnum-shift eax)
      (-rsp-index)]))

(define out (open-output-file "entry.s" #:exists 'replace))

(define (emit . args)
  (apply printf args)
  (printf "~n")
  (apply fprintf (cons out args))
  (fprintf out "~n"))

(define (compile-program)
  (entry
    (lambda ()
      (emit-expr '(zero? 0)))))

(compile-program)

(close-output-port out)