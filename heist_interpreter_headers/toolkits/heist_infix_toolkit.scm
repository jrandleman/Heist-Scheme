;; Author: Jordan Randleman -- jrandleman@scu.edu -- heist_infix_toolkit.scm
;; => Defines -infix cmd-line flag's operators for the Heist Scheme Interpreter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           INFIX OPERATORS BY DESCENDING PRECEDENCE                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; :                                | (r-assoc) functional composition                          ;
; **                               | (r-assoc) expt                                            ;
; * / % // mod                     | (l-assoc) multiply, division, remainder, quotient, modulo ;
; + -                              | (l-assoc) addition, subtraction                           ;
; :: @                             | (r-assoc) cons, append                                    ;
; > < >= <=                        | (l-assoc) gt, lt, gte, lte                                ;
; == !=                            | (l-assoc) eq, neq                                         ;
; &&                               | (l-assoc) and                                             ;
; ||                               | (l-assoc) or                                              ;
; ->                               | (l-assoc) lambda                                          ;
; = <- **= *= /= %= //= mod= += -= | (r-assoc) define, set!, set! ** * / % // mod + -          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define : compose)
(infixr! 10 :)

(define ** expt)
(define % remainder)
(define // quotient)
(define mod modulo)
(infixr! 9 **)
(infix!  8 * / % // mod)
(infix!  7 + -)

(define :: cons)
(define @ append)
(infixr! 6 :: @)

(define == =)
(define != (compose not ==))
(infix! 5 > < >= <=)
(infix! 4 == !=)

(define-reader-alias && and)
(define-reader-alias || or)
(infix! 3 &&)
(infix! 2 ||)

(define-reader-alias -> lambda)
(infix! 1 ->)

(core-syntax    = (syntax-rules () ((_ n v) (define n v))))
(core-syntax   <- (syntax-rules () ((_ n v) (set! n v))))
(core-syntax  **= (syntax-rules () ((_ n v) (set! n (** n v)))))
(core-syntax   *= (syntax-rules () ((_ n v) (set! n (* n v)))))
(core-syntax   /= (syntax-rules () ((_ n v) (set! n (/ n v)))))
(core-syntax   %= (syntax-rules () ((_ n v) (set! n (% n v)))))
(core-syntax  //= (syntax-rules () ((_ n v) (set! n (// n v)))))
(core-syntax mod= (syntax-rules () ((_ n v) (set! n (mod n v)))))
(core-syntax   += (syntax-rules () ((_ n v) (set! n (+ n v)))))
(core-syntax   -= (syntax-rules () ((_ n v) (set! n (- n v)))))
(infixr! 0 = <- **= *= /= %= //= mod= += -=)