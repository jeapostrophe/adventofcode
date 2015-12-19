#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (-> stx)
  (raise-syntax-error '-> "illegal use" stx))

(define-syntax (day7-mb stx)
  (syntax-parse stx
    #:literals (->)
    [(_ (~seq (~seq (~and lhs (~not ->)) ...) -> w:id) ...)
     (syntax/loc stx
       (#%module-begin
        (circuit
         [w (infixate lhs ...)]
         ...)))]))

(define-syntax (circuit stx)
  (syntax-parse stx
    [(_ [x:id f:expr] ...)
     (syntax/loc stx
       (begin
         (define c
           (make-immutable-hasheq
            (list (cons 'x (λ () f)) ...)))
         (module+ test
           (simulate-circuit c))))]))

(define current-id->v (make-parameter #f))
(struct exn:empty-wire ())
(define (ref x)
  (define id->v (current-id->v))
  (unless (and id->v (hash-has-key? id->v x))
    (raise (exn:empty-wire)))
  (hash-ref id->v x))

(define (simulate-circuit id->f)
  (define id->v (make-hasheq))
  (parameterize ([current-id->v id->v])
    (define (loop)
      (define changed? #f)
      (for ([(id f) (in-hash id->f)])
        (with-handlers ([exn:empty-wire? void])
          (hash-ref! id->v id
                     (λ ()
                       (begin0 (f)
                         (set! changed? #t))))))
      (when changed?
        (loop)))
    (loop)
    (for ([id (in-list (sort (hash-keys id->v) string<=? #:key symbol->string))])
      (define v (hash-ref id->v id))
      (printf "~a: ~a\n" id v))))

(define (16bit x)
  (unless (and (<= 0 x)
               (<= x 65535))
    (error '16bit "~a is not 16-bits" x))
  x)

(begin-for-syntax
  (define-syntax-class arg
    #:attributes (p)
    (pattern x:id
             #:attr p #'(ref 'x))
    (pattern n:nat
             #:attr p #'(16bit n)))

  (define-syntax-class op
    #:literals (AND OR LSHIFT RSHIFT)
    (pattern AND)
    (pattern OR)
    (pattern LSHIFT)
    (pattern RSHIFT)))

(define-syntax (infixate stx)
  (syntax-parse stx
    #:literals (NOT)
    [(_ l:arg o:op r:arg)
     (syntax/loc stx
       (o l.p r.p))]
    [(_ NOT a:arg)
     (syntax/loc stx
       (NOT a.p))]
    [(_ a:arg)
     (syntax/loc stx
       a.p)]))

(define (AND x y)
  (16bitize (bitwise-and x y)))
(define (OR x y)
  (16bitize (bitwise-ior x y)))
(define (LSHIFT x y)
  (16bitize (arithmetic-shift x y)))
(define (RSHIFT x y)
  (16bitize (arithmetic-shift x (* -1 y))))
(define (NOT x)
  (16bitize (bitwise-not x)))

(define (16bitize x)
  (define r (modulo x 65536))
  (if (negative? r)
      (16bitize (+ 65536 r))
      r))

(provide
 ->
 AND OR LSHIFT RSHIFT NOT
 #%datum
 (rename-out
  [day7-mb #%module-begin]))
