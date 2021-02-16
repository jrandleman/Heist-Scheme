;; Author: Jordan Randleman -- cppscm-toy-compiler.scm
;; WARNING: Just a demo Heist program. Assumes given special forms are correct.

#| 
Demo Heist program: toy compiler for a mini language, "cppscm"
  - lightweight scheme-esque script that directly compiles to C++17
  - types prefixed by "$", & putting sCcvmr prior that "$" denotes qualifiers:
    s = static, C = constexpr, c = const, v = volatile, m = move, r = reference
    * Use "-" instead of spaces for types (IE $long-long)
  - pass array params to fcns using "*" notation ("[]" params mess w/ parser)


Interface: 
  - (compile-cppscm <filename>) ; reads & compiles file (returns C++17 string)


Special Forms:
  0. (if <condition> <consequence> <optional-alternative>)
  1. (ifc <condition> <consequence> <optional-alternative>) ; if constexpr
  2. (set! <name> <value>)
  3. (begin <expr> ...) ; equivalent to (let () <expr> ...)
  4. (include <filename>) ; #include
     (include <<filename>>)
  5. (namespace <name> <expr> ...)
  6. (using namespace <name>) 
     (using <$alias> <$type>) 
     (using <$type>)
  7. (c++ <string>) ; inlines string as if C++
     (c <string>) ; inlines string in <extern "C" { ... }> clause
  8. (return <expr>) ; propagates through "if" "ifc" & "begin"
  9. ($cast-<typename> <value>)
  10. (<$type> <name>) ; typed decl of a variable (must obey C++ naming rules)
      (<$type> <name> <value>)
      (<$type> (<fcn-name> <$type> <$arg> ...) <expr> ...) ; function
      (<$type><crm> (<fcn-name> <$type> <$arg> ...) ; fcn w/ deduced args
        <expr> ...) ; (add "c" "cr" "r" "m" in "<>" for arg type semantics)
  11. (define <name> <value>) ; alias for ($auto <name> <value>)
      (define (<fcn-name> <$type> <arg> ...) <expr> ...) ; auto return value
      (define* (<fcn-name> <arg> ...) <expr> ...)   ; auto value params
      (define*c (<fcn-name> <arg> ...) <expr> ...)  ; auto const value params
      (define*cr (<fcn-name> <arg> ...) <expr> ...) ; auto const ref params
      (define*r (<fcn-name> <arg> ...) <expr> ...)  ; auto ref params
      (define*m (<fcn-name> <arg> ...) <expr> ...)  ; auto move params
  12. (lambda (<$type> <arg> ...) <expr> ...) ; lambda (NOT a closure!)
      (lambda* (<arg> ...) <expr> ...)        ; deduce args by value
      (lambda*c (<arg> ...) <expr> ...)       ; deduced const args
      (lambda*cr (<arg> ...) <expr> ...)      ; deduced const & args
      (lambda*r (<arg> ...) <expr> ...)       ; deduced & args
      (lambda*m (<arg> ...) <expr> ...)       ; deduced && args
  13. (closure (<$type> <arg> ...) <expr> ...) ; closure by value (uses "[=]")
      (closure* (<arg> ...) <expr> ...)        ; deduce args by value
      (closure*c (<arg> ...) <expr> ...)       ; deduced const args
      (closure*cr (<arg> ...) <expr> ...)      ; deduced const & args
      (closure*r (<arg> ...) <expr> ...)       ; deduced & args
      (closure*m (<arg> ...) <expr> ...)       ; deduced && args
  14. Operators:
      ~      ; ~ 
      !      ; ! 
      ++     ; ++ (prefix)
      --     ; -- (prefix)
      ++*    ; ++ (postfix)
      --*    ; -- (postfix)
      +      ; +
      -      ; -
      *      ; *
      /      ; /
      %      ; %
      ^      ; ^
      &      ; &
      |      ; |
      =      ; ==
      <      ; <
      >      ; >
      **     ; std::pow (right-associative)
      +=     ; +=
      -=     ; -=
      *=     ; *=
      /=     ; /=
      %=     ; %=
      &=     ; &=
      |=     ; |=
      !=     ; !=
      <<     ; <<
      >>     ; >>
      <<=    ; <<=
      >>=    ; >>=
      <=     ; <=
      >=     ; >=
      new    ; new <$type>, new <$type> [<amount>], new (<place>) <$type>, new (<tag>) <$type> [<amount>]
      delete ; delete <$type>, delete [] <$type>
      and    ; &&
      or     ; ||
      []     ; [] (also triggered by calling nil as an operator, since "'[]" == "'()")
      ?:     ; ?: (ternary, only accepts 3 args)
 |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL HELPERS

(define (tagged-list? expr tag) (eq? (car expr) tag))

(define (type? param) 
  (or (and (pair? param) (c++? param)) 
      (and (symbol? param) (string-contains (symbol->string param) "$"))))

;; VALID PREFIX: sCcvrm$
;; s - static
;; C - constexpr
;; c - const
;; v - volatile
;; r - &
;; m - &&
(define (unhash-type type) ; rm "$" of type string & expand qualifiers
  (if (pair? type) ; c++
      (cadr type)
      (begin 
        (define typestr (symbol->string type))
        (define $idx (string-contains typestr "$"))
        (regex-replace-all 
          (if (not-zero? $idx)
              (begin 
                (define move? #f)
                (define reference? #f)
                (define qualifiers "")
                (define qualifier-signature (slice typestr 0 $idx))
                (if (string-contains qualifier-signature "s") 
                    (set! qualifiers (append qualifiers "static ")))
                (if (string-contains qualifier-signature "C") 
                    (set! qualifiers (append qualifiers "constexpr ")))
                (if (string-contains qualifier-signature "c") 
                    (set! qualifiers (append qualifiers "const ")))
                (if (string-contains qualifier-signature "v") 
                    (set! qualifiers (append qualifiers "volatile ")))
                (if (string-contains qualifier-signature "r") 
                    (set! reference? #t)
                    (if (string-contains qualifier-signature "m") 
                        (set! move? #t)))
                (append qualifiers (slice typestr (+ 1 $idx)) (if reference? "&" (if move? "&&" ""))))
              (slice typestr 1))
            "-" 
            " "))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING SEQUENCES (BEGIN, LAMBDA-BODY, NAMESPACE-BODY, ETC.)

; Makes sure "#include" & inlined C/C++ statements don't get ";" added
(define (statement-terminator expr)
  (if (and (pair? expr) (or (include? expr) (c? expr) (c++? expr))) "\n" ";\n"))

(define (sequence->c++ exprs)
  (fold (lambda (acc expr) (append acc (cppscm->c++ expr) (statement-terminator expr))) "" exprs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING BEGIN

;; (begin <expr> ...)
(define (begin? expr) (tagged-list? expr 'begin))

(define (begin->c++ expr) (append "{\n" (sequence->c++ (cdr expr)) "}"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING PROCEDURES (NO VARIADICS!)

(define (lambda-args params args-parser) (if (null? params) "" (args-parser params)))

(define (general-lambda->c++ scoping expr args-parser)
  (append "[" scoping "](" (lambda-args (cadr expr) args-parser) ")mutable{\n" 
            (sequence->c++ (cddr expr)) 
          "}"))

(define (deduced-general-lambda->c++ scoping type expr)
  (general-lambda->c++ scoping expr (convert-deduced-lambda-args type)))

(define (convert-lambda-args params)
  (define type? #t)
  (define params-str
    (fold
      (lambda (acc e)
        (cond (type? (set! type? #f) (append acc (unhash-type e) " ")) 
              (else (set! type? #t) (append acc (symbol->string e) ", "))))
      ""
      params))
  (slice params-str 0 (- (length params-str) 2)))

(define (convert-deduced-lambda-args type)
  (lambda (params)
    (define params-str (fold (lambda (acc e) (append acc type (symbol->string e) ", ")) "" params))
    (slice params-str 0 (- (length params-str) 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING LAMBDAS (NO VARIADICS!)

;; (lambda (<$type> <name> ...) <expr> ...)
(define (lambda? expr) (tagged-list? expr 'lambda)) ; MUST HAVE TYPED PARAMS!
(define (lambda*? expr) (tagged-list? expr 'lambda*))
(define (lambda*c? expr) (tagged-list? expr 'lambda*c))
(define (lambda*cr? expr) (tagged-list? expr 'lambda*cr))
(define (lambda*r? expr) (tagged-list? expr 'lambda*r))
(define (lambda*m? expr) (tagged-list? expr 'lambda*m))

(define (lambda->c++ expr) (general-lambda->c++ "" expr convert-lambda-args))
(define (lambda*->c++ expr) (deduced-general-lambda->c++ "" "auto " expr))
(define (lambda*c->c++ expr) (deduced-general-lambda->c++ "" "const auto " expr))
(define (lambda*cr->c++ expr) (deduced-general-lambda->c++ "" "const auto& " expr))
(define (lambda*r->c++ expr) (deduced-general-lambda->c++ "" "auto& " expr))
(define (lambda*m->c++ expr) (deduced-general-lambda->c++ "" "auto&& " expr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING CLOSURES (NO VARIADICS!)

;; (closure (<$type> <name> ...) <expr> ...)
(define (closure? expr) (tagged-list? expr 'closure)) ; MUST HAVE TYPED PARAMS!
(define (closure*? expr) (tagged-list? expr 'closure*))
(define (closure*c? expr) (tagged-list? expr 'closure*c))
(define (closure*cr? expr) (tagged-list? expr 'closure*cr))
(define (closure*r? expr) (tagged-list? expr 'closure*r))
(define (closure*m? expr) (tagged-list? expr 'closure*m))

(define (closure->c++ expr) (general-lambda->c++ "=" expr convert-lambda-args))
(define (closure*->c++ expr) (deduced-general-lambda->c++ "=" "auto " expr))
(define (closure*c->c++ expr) (deduced-general-lambda->c++ "=" "const auto " expr))
(define (closure*cr->c++ expr) (deduced-general-lambda->c++ "=" "const auto& " expr))
(define (closure*r->c++ expr) (deduced-general-lambda->c++ "=" "auto& " expr))
(define (closure*m->c++ expr) (deduced-general-lambda->c++ "=" "auto&& " expr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING IF

;; (if <cond> <conseq> <optional-alt>)
(define (if? expr) (tagged-list? expr 'if))

(define (if-body->c++ expr)
  (if (and (pair? expr) (begin? expr))
      (cppscm->c++ expr)
      (append "{\n" (cppscm->c++ expr) ";\n}")))

(define (if-contents->c++ prefix expr)
  (append prefix (cppscm->c++ (car expr)) ")" (if-body->c++ (cadr expr))
    (if (= (length expr) 3) (append " else " (if-body->c++ (caddr expr))) "")))

(define (if->c++ expr) (if-contents->c++ "if(" (cdr expr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING CONSTEXPR IF

;; (ifc <cond> <conseq> <optional-alt>)
(define (ifc? expr) (tagged-list? expr 'ifc))

(define (ifc->c++ expr) (if-contents->c++ "if constexpr (" (cdr expr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING SET!

;; (set! <name> <val>)
(define (set!? expr) (tagged-list? expr 'set!))

(define (set!->c++ expr) 
  (append (symbol->string (cadr expr)) " = " (cppscm->c++ (caddr expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING NAMESPACE

;; (namespace <name> <expr> ...)
(define (namespace? expr) (tagged-list? expr 'namespace))

(define (namespace->c++ expr) 
  (append "namespace " (symbol->string (cadr expr)) " {\n" (sequence->c++ (cddr expr)) "}"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING INCLUDE

;; (include <<filename>>)
;; (include <filename>)
(define (include? expr) (tagged-list? expr 'include))

(define (include->c++ expr) 
  (define filestring (symbol->string (cadr expr)))
  (if (char=? (head filestring) #\<)
      (append "\n#include " filestring "\n")
      (append "\n#include \"" filestring "\"\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING USING

;; (using namespace <name>) ::= using namespace name;
;; (using <$alias> <$type>) ::= using alias = type;
;; (using <$type>) ::= using <type>;
(define (using? expr) (tagged-list? expr 'using))

(define (using->c++ expr) 
  (cond ((equal? (cadr expr) "namespace")
          (append "using namespace " (symbol->string (caddr expr))))
        ((= (length expr) 3)
          (append "using " (unhash-type (cadr expr)) " = " (unhash-type (caddr expr))))
        (else
          (append "using " (unhash-type (cadr expr))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING DEFINE

;; (define <name> <val>) ; => alias for ($auto <name> <val>)
;; (define (<proc-name> <$type> <arg> ...) <expr> ...) ; => function definition
(define (define? expr) (tagged-list? expr 'define))

(define (define-var->c++ expr)
  (append "auto " (symbol->string (cadr expr)) " = " (cppscm->c++ (caddr expr))))

(define (define-function->c++ expr) ; (NO VARIADICS & MUST HAVE TYPED ARGS!)
  (append "auto " (symbol->string (caadr expr) )
          "(" (lambda-args (cdadr expr) convert-lambda-args) "){\n" 
          (sequence->c++ (cddr expr)) "}"))

(define (define->c++ expr) 
  (if (atom? (cadr expr))
      (define-var->c++ expr)
      (define-function->c++ expr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING DEFINE* ("define" THAT DEDUCES ARGUMENT TYPES FOR FUNCTIONS!)

;; (define* <name> <val>) ; => alias for ($auto <name> <val>)
;; (define* (<proc-name> <arg> ...) <expr> ...) ; => function definition
(define (define*? expr) (tagged-list? expr 'define*))
(define (define*c? expr) (tagged-list? expr 'define*c))
(define (define*cr? expr) (tagged-list? expr 'define*cr))
(define (define*r? expr) (tagged-list? expr 'define*r))
(define (define*m? expr) (tagged-list? expr 'define*m))

(define *generated-type-counter* 0)

(define (generate-type-names total-types)
  (define type-names (map (lambda (idx) (append "CPPSCM_t" (number->string idx)))
                     (iota total-types *generated-type-counter*)))
  (set! *generated-type-counter* (+ *generated-type-counter* total-types))
  type-names)

(define (generate-deduced-template-header type-names)
  (append "template<" ((convert-deduced-lambda-args "typename ") (map string->symbol type-names)) ">\n"))

(define (generate-deduced-function-args const-qualifier value-semantic type-names params)
  (if (null? params)
      ""
      (let ((params-str (fold (lambda (acc t p) 
                                (append acc const-qualifier t value-semantic (symbol->string p) ", ")) 
                                "" type-names params)))
        (slice params-str 0 (- (length params-str) 2)))))

(define (deduced-general-define->c++ const-qualifier value-semantic expr)
  (if (atom? (cadr expr))
      (define-var->c++ expr)
      (let ((type-names (generate-type-names (length (cdadr expr)))))
        (if (null? type-names)
            (append "auto " (symbol->string (caadr expr)) "(){\n" ; no template (no args)
                    (sequence->c++ (cddr expr)) 
                    "}")
            (append (generate-deduced-template-header type-names)
                    "auto " (symbol->string (caadr expr))
                    "(" (generate-deduced-function-args const-qualifier value-semantic type-names (cdadr expr)) "){\n" 
                    (sequence->c++ (cddr expr)) 
                    "}")))))

(define (define*->c++ expr) (deduced-general-define->c++ "" " " expr))
(define (define*c->c++ expr) (deduced-general-define->c++ "const " " " expr))
(define (define*cr->c++ expr) (deduced-general-define->c++ "const " "& " expr))
(define (define*r->c++ expr) (deduced-general-define->c++ "" "& " expr))
(define (define*m->c++ expr) (deduced-general-define->c++ "" "&& " expr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING INLINED C++ / C

;; (c++ <c++ code>)
(define (c++? expr) (tagged-list? expr 'c++))

;; (c <c code>)
(define (c? expr) (tagged-list? expr 'c))

(define (c->c++ expr) (append "extern \"C\" {\n" (cadr expr) "\n}"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING INLINED C++

;; (return <expr>)
(define (return? expr) (tagged-list? expr 'return))

(define (if-return->c++ returned)
  (cppscm->c++ 
    (if (= (length returned) 3)
        (list (car returned) (cadr returned)
              (list 'return (caddr returned)))
        (list (car returned) (cadr returned)
              (list 'return (caddr returned))
              (list 'return (cadddr returned))))))

(define (begin-return->c++ returned)
  (if (null? (cdr returned))
      "return"
      (cppscm->c++ 
        (append (init returned) (list (list 'return (last returned)))))))

(define (return->c++ expr) 
  (define returned (cadr expr))
  (cond ((atom? returned) (append "return " (cppscm->c++ returned)))
        ((or (ifc? returned) (if? returned)) (if-return->c++ returned))
        ((begin? returned) (begin-return->c++ returned))
        (else (append "return " (cppscm->c++ returned)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING TYPED VARIABLE DECLARATIONS

;; (<$type> <name> <optional-value>)
(define (typed-variable-declaration? expr) (type? (car expr)))

(define (typed-deduced-function->c++ typename qualifiers expr)
  (define fcn-string (cppscm->c++ (cons (string->symbol (append "define*" qualifiers)) expr)))
  (define idx (string-contains fcn-string ">"))
  (if idx
      (append (slice fcn-string 0 (+ idx 2)) typename (slice fcn-string (+ idx 6)))
      (append typename (slice fcn-string 4)))) ; argless hence no template

(define (typed-explicit-function->c++ typename expr)
  (append typename (slice (cppscm->c++ (cons 'define expr)) 4)))

(define (typed-function-declaration->c++ expr)
  (cond ((pair? (car expr)) ; inlined c++ (assumed to be a type)
          (typed-explicit-function->c++ (cadar expr) (cdr expr)))
        (else ; symbolic typename
          (define typestr (symbol->string (car expr)))
          (define deduced-args? (string-contains typestr "<"))
          (define typename 
            (if deduced-args?
                (unhash-type (string->symbol (slice typestr 0 deduced-args?)))
                (unhash-type (car expr))))
          (if deduced-args?
              (typed-deduced-function->c++ typename (slice typestr (+ 1 deduced-args?) (- (length typestr) deduced-args? 2)) (cdr expr))
              (typed-explicit-function->c++ typename (cdr expr))))))

(define (typed-variable-declaration->c++ expr)
  (cond ((pair? (cadr expr))
          (typed-function-declaration->c++ expr))
        ((= (length expr) 3)
          (append (unhash-type (car expr)) " " (symbol->string (cadr expr)) " = " (cppscm->c++ (caddr expr))))
        (else
          (append (unhash-type (car expr)) " " (symbol->string (cadr expr))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING TYPE CASTING

;; ($cast-<typename> <value>)
(define (type-cast? expr) 
  (and (symbol? (car expr)) (string=? "$cast-" (slice (symbol->string (car expr)) 0 6))))

(define (type-cast->c++ expr)
  (append "(" (slice (symbol->string (car expr)) 6) ")" (cppscm->c++ (cadr expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING OPERATORS (EXTENDS C++ OPERATORS TO ALLOW FOR ** TO BE std::pow)

;; (<operator> <value> ...)
(define (operator? expr) (and (hmap-hashable? (car expr)) (hmap-key? *c++-operators* (car expr))))

(define (comparison-operator? operator)
  (not-zero? (count \(eq? operator %1) '#(= < > <= >=))))

(define (unary/? expr) (and (= (length expr) 2) (eq? (car expr) '/)))

(define *c++-operators*
  '$(~ ~ 
     ! ! 
     ++ ++ ; prefix
     -- -- ; prefix
     ++* ++ ; postfix
     --* -- ; postfix
     + +
     - -
     * *
     / /
     % %
     ^ ^
     & &
     | |
     = ==
     < <
     > >
     ** ** ; std::pow
     += +=
     -= -=
     *= *=
     /= /=
     %= %=
     &= &=
     |= |=
     != !=
     << <<
     >> >>
     <<= <<=
     >>= >>=
     <= <=
     >= >=
     new new
     delete delete
     and &&
     or ||
     [] [] ; [] (also triggered by calling nil as an operator, since "'[]" == "'()")
     ?: ?:))

(define (operator-sequence->c++ operator expr) 
  (string-join (map cppscm->c++ expr) (append " " (symbol->string operator) " ")))

(define (**->c++ expr) ; right-associative!
  (define operands (reverse (map cppscm->c++ (cdr expr))))
  (fold (lambda (acc e) (append "std::pow(" e "," acc ")"))
    (car operands)
    (cdr operands)))

(define (ref->c++ expr) 
  (append (cppscm->c++ (cadr expr)) "[" (cppscm->c++ (caddr expr)) "]"))

(define (?:->c++ expr) 
  (append (cppscm->c++ (cadr expr)) " ? " (cppscm->c++ (caddr expr)) " : " (cppscm->c++ (cadddr expr))))

;; new <$type> 
;; new <$type> [<amount>] 
;; new (<place>) <$type> 
;; new (<tag>) <$type> [<amount>]
(define (new->c++ expr) 
  (if (atom? (cadr expr))
      (if (= (length expr) 2)
          (append "new " (unhash-type (cadr expr)))
          (append "new " (unhash-type (cadr expr)) " [" (cppscm->c++ (caddr expr)) "]"))
      (if (= (length expr) 3)
          (append "new (" (symbol->string (cadr expr)) ") " (unhash-type (caddr expr)))
          (append "new (" (symbol->string (cadr expr)) ") " (unhash-type (caddr expr)) " [" (cppscm->c++ (cadddr expr)) "]"))))

;; delete <$type>
;; delete [] <$type>
(define (delete->c++ expr) 
  (if (= (length expr) 2)
      (append "delete " (unhash-type (cadr expr)))
      (append "delete [] " (unhash-type (cadr expr)))))

(define (unary-operator->c++ expr)
  (cond ((eq? (car expr) '++*) ; postfix ++
          (append (cppscm->c++ (cadr expr)) "++"))
        ((eq? (car expr) '--*) ; postfix --
          (append (cppscm->c++ (cadr expr)) "--"))
        (else 
          (append (symbol->string (car expr)) (cppscm->c++ (cadr expr))))))

;; whereas (+ 1 2 3) -> "1 + 2 + 3", (< 1 2 3) becomes "1 < 2 && 2 < 3"
(define (comparison-operator->c++ expr)
  (define operator (symbol->string (car expr)))
  (define (iter expr)
    (append (car expr) " " operator " " (cadr expr)
            (if (null? (cddr expr)) "" (append " && " (iter (cdr expr))))))
  (iter (map cppscm->c++ (cdr expr))))


(define (operator->c++ expr)
  (define hd (car expr))
  (cond ((eq? hd '**) (**->c++ expr))
        ((eq? hd 'new) (new->c++ expr))
        ((eq? hd 'delete) (delete->c++ expr))
        ((eq? hd '?:) (?:->c++ expr))
        ((eq? hd '()) (ref->c++ expr))
        ((unary/? expr) (append "(" (operator-sequence->c++ '/ (cons 1 (cdr expr))) ")")) ; inversion
        ((= (length expr) 2) (unary-operator->c++ expr))
        ((comparison-operator? hd) (append "(" (comparison-operator->c++ expr) ")"))
        (else (append "(" (operator-sequence->c++ (hmap-ref *c++-operators* hd) (cdr expr)) ")"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING FUNCTION CALLS

;; (<function> <arg> ...)
(define (function-call->c++ expr)
  (append (cppscm->c++ (car expr)) "(" (string-join (map cppscm->c++ (cdr expr)) ", ") ")"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPRESENTING C++ ATOMS

(define (atom->c++ atom)
  (cond ((char? atom) (regex-replace-all (write (string atom) "") "\"" "'"))
        ((string? atom) (write atom ""))
        ((boolean? atom) (if atom "true" "false"))
        (else (display atom ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RETURNS A STRING OF THE C++ REPRESENTING THE COMPILED CPPSCM

(define (cppscm->c++ datum)
  (cond ((atom? datum) (atom->c++ datum)) ; stringify atom
        ((if? datum) (if->c++ datum))
        ((ifc? datum) (ifc->c++ datum)) ; constexpr if
        ((set!? datum) (set!->c++ datum)) ; assignment
        ((begin? datum) (begin->c++ datum))
        ((include? datum) (include->c++ datum))
        ((namespace? datum) (namespace->c++ datum))
        ((using? datum) (using->c++ datum))
        ((define? datum) (define->c++ datum))
        ((c++? datum) (cadr datum)) ; inlined C++ code string
        ((c? datum) (c->c++ datum)) ; inlined C code string
        ((return? datum) (return->c++ datum))
        ((type-cast? datum) (type-cast->c++ datum))
        ;; TYPED VARIABLE DECL
        ((typed-variable-declaration? datum) (typed-variable-declaration->c++ datum))
        ;; LAMBDAS
        ((lambda? datum) (lambda->c++ datum))
        ((lambda*? datum) (lambda*->c++ datum))     ; deduce args by value
        ((lambda*c? datum) (lambda*c->c++ datum))   ; const
        ((lambda*cr? datum) (lambda*cr->c++ datum)) ; const &
        ((lambda*r? datum) (lambda*r->c++ datum))   ; &
        ((lambda*m? datum) (lambda*m->c++ datum))   ; &&
        ;; CLOSURES (BY VALUE, __NOT__ REFERENCE)
        ((closure? datum) (closure->c++ datum))
        ((closure*? datum) (closure*->c++ datum))     ; deduce args by value
        ((closure*c? datum) (closure*c->c++ datum))   ; const
        ((closure*cr? datum) (closure*cr->c++ datum)) ; const &
        ((closure*r? datum) (closure*r->c++ datum))   ; &
        ((closure*m? datum) (closure*m->c++ datum))   ; &&
        ;; FUNCTIONS WITH DEDUCED PARAM TYPES (SIMULATES C++20'S "auto-parameters" IN C++17!)
        ((define*? datum) (define*->c++ datum))
        ((define*c? datum) (define*c->c++ datum))
        ((define*cr? datum) (define*cr->c++ datum))
        ((define*r? datum) (define*r->c++ datum))
        ((define*m? datum) (define*m->c++ datum))
        ;; OPERATORS
        ((operator? datum) (operator->c++ datum))
        ;; FUNCTION CALL
        (else (function-call->c++ datum))))

; Unwraps topmost <begin> expr to not be in "{}"
(define (compile-global-cppscm datum)
  (if (and (pair? datum) (begin? datum))
      (sequence->c++ (cdr datum))
      (append (cppscm->c++ datum) (statement-terminator datum))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPPSCM FILE COMPILATION INTERFACE

(define (compile-cppscm filename) ; returns string of compiled-to C++17
  (compile-global-cppscm (read-file filename)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLE USE

;(display (compile-global-cppscm 
;  '(begin 
;    (include <iostream>)
;    ($int<> (fac n p)
;      (return (if (< n 2)
;                  p
;                  (fac (- n 1) (* n p)))))
;    ($int (main) (<< std::cout (fac 5 1) "\n")))))