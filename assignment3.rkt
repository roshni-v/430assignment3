#lang typed/racket
(require typed/rackunit)

;;tstruct defn
(define-syntax tstruct
  (syntax-rules ()
    [(_ name fields)
     (struct name fields #:transparent)]))

(define-type ExprC (U NumC AppC Leq0? IdC Binop))
(tstruct NumC ([n : Real]))
(tstruct AppC ([fun : Symbol] [arg : ExprC]))
(tstruct Leq0?  ([test : ExprC] [t : ExprC] [e : ExprC]))
(tstruct IdC   ([id : Symbol]))
(tstruct Binop   ([op : Symbol] [l : ExprC] [r : ExprC]))

;; Find the binop b in the list of binops
#;
(define (get-binop [b : Symbol] [binops : (Listof Binop)]) : Binop
  (match b
    []))
;; Keywords
(define keywords '(+ - * / def leq0? then else =))

;; Predicate to determine if s is a legal symbol
(: legal-sym? (Any -> Boolean : #:+ Symbol))
(define (legal-sym? s)
  (and (symbol? s)
       (not (member s keywords)))) 

; Parser converts Concrete Syntax to Abstract Syntax
(define (parse [sexp : Sexp]) : ExprC
   (match sexp
     [(? real?) (NumC sexp)]
     [(list 'leq0? test 'then t 'else e) (Leq0? (parse test) (parse t) (parse e))]
     [(list (? legal-sym? funName) b) (AppC funName (parse b))]
     [(? legal-sym? sym) (IdC sym)]
     [(list (? symbol? b) l r) (Binop b (parse l) (parse r))]
     [other (error 'parse "Syntax error, given invalid term ~e" other)]))

; parse tests:
(check-equal? (parse '{+ 5 4}) (Binop '+ (NumC 5) (NumC 4)))
(check-equal? (parse '{leq0? 1 then 1 else {- 1 1}}) (Leq0? (NumC 1) (NumC 1) (Binop '- (NumC 1) (NumC 1))))
(check-equal? (parse '{foo 1}) (AppC 'foo (NumC 1)))
(check-equal? (parse 'abc) (IdC 'abc))
;(check-equal? (parse '=) 1)
(check-exn #px"Syntax error, given invalid term '=" (λ () (parse '=)))
(check-exn #px"Syntax error, given invalid term '(= 1)" (λ () (parse '(= 1))))


