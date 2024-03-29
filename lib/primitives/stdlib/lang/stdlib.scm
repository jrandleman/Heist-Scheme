;; Author: Jordan Randleman -- jordanran199@gmail.com -- stdlib.scm
;; => Defines helper functions for the Heist Scheme Interpreter's Scheme primitives

;; ====================================================
;; =========== SELECTED SPECIAL FORM MACROS ===========
;; ====================================================

; -:- TABLE OF CONTENTS -:-
; and
; or
; cond
; case
; let
; let*
; letrec
; letrec*
; scons
; stream
; let-syntax
; letrec-syntax
; def
; defn
; while
; do
; for
; -<>

(core-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ a) a)
    ((_ a b ...) 
      (if a (and b ...) #f))))


(core-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ a) a)
    ((_ a b ...)
      ((lambda (heist:core:or-val)
        (if heist:core:or-val
            heist:core:or-val
            (or b ...))) 
       a))))


(core-syntax cond 
  (syntax-rules (else =>)
    ((_ (e => f) c ...)    ; clause w/ application (cache result)
      ((lambda (heist:core:cond-result) 
        (if heist:core:cond-result 
            (f heist:core:cond-result) 
            (cond c ...))) e))
    ((_ (e0 e1 ...) c ...) ; clause
      (if e0 (begin e1 ...) (cond c ...)))
    ((_ (else e ...))      ; last clause w/ 'else
      (begin e ...))
    ((_ (e => f))          ; last clause w/ application (cache result)
      ((lambda (heist:core:cond-result) 
        (if heist:core:cond-result 
            (f heist:core:cond-result))) e))
    ((_ (e0 e1 ...))       ; last clause
      (if e0 (begin e1 ...)))))


(core-syntax case 
  (syntax-rules (else =>)
    ((_ val ((l ...) => f) c ...)  ; clause w/ application (cache result)
      ((lambda (heist:core:case-result)
        (if heist:core:case-result 
            (f heist:core:case-result)
            (case val c ...))) (memv val (list l ...))))
    ((_ val ((l ...) e ...) c ...) ; clause
      (if (memv val (list l ...)) 
          (begin e ...) 
          (case val c ...)))
    ((_ val (else e ...))          ; last clause w/ 'else
      (begin e ...))
    ((_ val ((l ...) => f))        ; last clause w/ application (cache result)
      ((lambda (heist:core:case-result) 
        (if heist:core:case-result 
            (f heist:core:case-result))) (memv val (list l ...))))
    ((_ val ((l ...) e ...))       ; last clause
      (if (memv val (list l ...)) 
          (begin e ...)))))


(core-syntax let 
  (syntax-rules ()
    ((_ () b ...) 
      ((lambda () b ...)))
    ((_ ((a v) ...) b ...)
      ((lambda (a ...) b ...) v ...))
    ((_ name () b ...) 
      ((lambda ()
        (define name (lambda () b ...)) 
        (name))))
    ((_ name ((a v) ...) b ...)
      ((lambda ()
        (define name (lambda (a ...) b ...)) 
        (name v ...))))))


(core-syntax let* 
  (syntax-rules ()
    ((_ () b ...) 
      ((lambda () b ...)))
    ((_ ((a v)) b ...)
      ((lambda (a) b ...) v))
    ((_ ((a v) other-bindings ...) b ...)
      ((lambda (a) (let* (other-bindings ...) b ...)) v))))


(core-syntax letrec 
  (syntax-rules ()
    ((_ () b ...) 
      ((lambda () b ...)))
    ((_ ((a v) ...) b ...)
      (let ((a (undefined)) ...)
        (set! a v) ...
        b ...))))


(core-syntax letrec* 
  (syntax-rules ()
    ((_ () b ...) 
      (let () b ...))
    ((_ ((a v)) b ...)
      (letrec ((a v)) b ...))
    ((_ ((a v) other-bindings ...) b ...)
      (letrec ((a v)) (letrec* (other-bindings ...) b ...)))))


(core-syntax scons 
  (syntax-rules ()
    ((_ a b) (cons (delay a) (delay b)))))


(core-syntax stream 
  (syntax-rules ()
    ((_) '())
    ((_ a) (cons (delay a) (delay '())))
    ((_ a b ...) (cons (delay a) (delay (stream b ...))))))


; NOTE: Our interpreter evals all macros in lazily, hence same rules
(define heist:core:scoped-syntax-rules
  (syntax-rules ()
    ((_ () b ...) 
      ((lambda () b ...)))
    ((_ ((a v) ...) b ...)
      ((lambda ()
        (define-syntax a v) ...
        b ...)))))
(core-syntax let-syntax heist:core:scoped-syntax-rules)
(core-syntax letrec-syntax heist:core:scoped-syntax-rules)


(core-syntax def
  (syntax-rules ()
    ((_ n v ...) (define n v ...))))


(core-syntax defn
  (syntax-rules ()
    ((_ (name) b ...)
     (define (name) b ...))
    ((_ (name a ...) b ...)
     (define name (fn ((a ...) b ...))))
    ((_ name instance ...) 
     (define name (fn instance ...)))))


(core-syntax while ; wrap the looping instrinsic to prevent leaking inner defs
  (syntax-rules ()
    ((_ (c) b ...)       ; condition & body
      ((lambda (*condition*) 
        (heist:core:while ((and *condition* c)) b ...)) #t))
    ((_ (c r ...) b ...) ; condition, returns, & body
      ((lambda (*condition*) 
        (heist:core:while ((and *condition* c) r ...) b ...)) #t))
    ((_ (c))             ; only condition
      ((lambda (*condition*) 
        (heist:core:while ((and *condition* c)))) #t))
    ((_ (c r ...))       ; condition & returns
      ((lambda (*condition*) 
        (heist:core:while ((and *condition* c) r ...))) #t))))


(core-syntax do
  (syntax-rules ()
    ((_ ((var val update) ...)
        (break-test returns ...)
        body ...)
      (letrec ((heist:core:do-call 
                (lambda (var ...)
                  (if break-test
                      (begin returns ...)
                      (begin body ... (set! var update) ... (heist:core:do-call var ...))))))
              (heist:core:do-call val ...)))
    ; no body
    ((_ ((var val update) ...)
        (break-test returns ...))
      (letrec ((heist:core:do-call 
                (lambda (var ...)
                  (if break-test
                      (begin returns ...)
                      (begin (set! var update) ... (heist:core:do-call var ...))))))
              (heist:core:do-call val ...)))
    ; no returns
    ((_ ((var val update) ...)
        (break-test)
        body ...)
      (letrec ((heist:core:do-call 
                (lambda (var ...)
                  (if (not break-test)
                      (begin body ... (set! var update) ... (heist:core:do-call var ...))))))
              (heist:core:do-call val ...)))
    ; no returns nor body
    ((_ ((var val update) ...) (break-test))
      (letrec ((heist:core:do-call 
                (lambda (var ...)
                  (if (not break-test)
                      (begin (set! var update) ... (heist:core:do-call var ...))))))
              (heist:core:do-call val ...)))
    ; no params (implies a body)
    ((_ () (break-test returns ...) body ...)
      (letrec ((heist:core:do-call 
                (lambda ()
                  (if break-test 
                      (begin returns ...) 
                      (begin body ... (heist:core:do-call))))))
              (heist:core:do-call)))
    ; no params nor returns (implies a body)
    ((_ () (break-test) body ...)
      (letrec ((heist:core:do-call 
                (lambda () 
                  (if (not break-test) (begin body ... (heist:core:do-call))))))
              (heist:core:do-call)))))


(core-syntax for
  (syntax-rules ()
    ((_ ((var val update) ...)
        (break-test returns ...)
        body ...)
      (let ()
        (define var val) ...
        (while ((not break-test) returns ...)
          body ...
          (set! var update) ...)))
    ; no body
    ((_ ((var val update) ...)
        (break-test returns ...))
      (let ()
        (define var val) ...
        (while ((not break-test) returns ...)
          (set! var update) ...)))
    ; no returns
    ((_ ((var val update) ...)
        (break-test)
        body ...)
      (let ()
        (define var val) ...
        (while ((not break-test))
          body ...
          (set! var update) ...)))
    ; no returns nor body
    ((_ ((var val update) ...) (break-test))
      (let ()
        (define var val) ...
        (while ((not break-test))
          (set! var update) ...)))
    ; no params (implies a body)
    ((_ () (break-test returns ...) body ...)
      (while ((not break-test) returns ...) body ...))
    ; no params nor returns (implies a body)
    ((_ () (break-test) body ...)
      (while ((not break-test)) body ...))))


(core-syntax -<> ; Note: the "<>" value is cached!
  (syntax-rules ()
    ((_ a) a)
    ((_ a op) ((lambda (<>) op) a))
    ((_ a op1 op2 ...) 
      (-<> ((lambda (<>) op1) a) op2 ...))))

;; ====================================================================================
;; =========== "QUASIQUOTE" MACRO FOR SELECTIVE EVALUATION DURING QUOTATION ===========
;; ====================================================================================

(define-reader-syntax "`" "quasiquote")
(define-reader-syntax "," "unquote")
(define-reader-syntax ",@" "unquote-splicing")

(define (heist:core:quasiquote:tagged-list? obj tag)
  (and (eq? (car obj) tag) (not (null? (cdr obj)))))

; Recursively perform an optimization pass
(define (heist:core:quasiquote:optimization-pass lst optimizable? optimize)
  (define (iter lst)
    (if (and (list? lst) (pair? lst))
      (if (optimizable? lst)
          (iter (optimize lst))
          (map iter lst))
      lst))
  (iter lst))

; (append <item>) => <item>
(define (heist:core:quasiquote:unwrap-unary-appends lst)
  (heist:core:quasiquote:optimization-pass 
    lst 
    (lambda (lst) (and (eq? (car lst) 'append) (= (length lst) 2))) 
    cadr))

; (append (list item ...) ...) => (list item ... ...)
(define (heist:core:quasiquote:redundant-append? append-exprs)
  (null? (filter \(not (and (pair? %1) (eq? (car %1) 'list))) append-exprs)))

(define (heist:core:quasiquote:unwrap-redundant-appends lst)
  (heist:core:quasiquote:optimization-pass 
    lst 
    (lambda (lst) (and (eq? (car lst) 'append) (heist:core:quasiquote:redundant-append? (cdr lst)))) 
    (lambda (lst) (cons 'list (apply append (map cdr (cdr lst)))))))

; Optimize generated code
(define (heist:core:quasiquote:optimize lst)
  (heist:core:quasiquote:unwrap-redundant-appends
    (heist:core:quasiquote:unwrap-unary-appends lst)))

; Convert a list to an hmap
(define (heist:core:quasiquote:list->hmap lst)
  (define (list->alist lst)
    (if (null? lst)
        '()
        (cons (list (car lst) (cadr lst)) 
              (list->alist (cddr lst)))))
  (alist->hmap (list->alist lst)))

; Convert an hmap to a list
(define (heist:core:quasiquote:hmap->list hmp)
  (apply append (hmap->alist hmp)))

; Generate Code
(define (heist:core:quasiquote->quote lst level)
  (define (iter lst)
    (define hd (if (not (atom? lst)) (car lst)))
          ; finished parsing expression (proper list)
    (cond ((null? lst) '())
          ; quasiquote vector literal at end of the expression
          ((vector? lst)
            (list (list 'list->vector (heist:core:quasiquote->quote (vector->list lst) level))))
          ; quasiquote hmap literal at end of the expression
          ((hmap? lst)
            (list (list 'heist:core:quasiquote:list->hmap (heist:core:quasiquote->quote (heist:core:quasiquote:hmap->list lst) level))))
          ; finished parsing expression (dotted list)
          ((atom? lst)
            (list (list 'quote lst)))
          ; unquote rest of list
          ((heist:core:quasiquote:tagged-list? lst 'unquote)
            (if (= level 0)
                (list (cadr lst)) 
                (list (list 'list ''unquote (heist:core:quasiquote->quote (cadr lst) (- level 1)))))) ; *there*: recursively parse, in nested quasiquote))
          ; quasiquote vector literal
          ((vector? hd)
            (cons (list 'list (list 'list->vector (heist:core:quasiquote->quote (vector->list hd) level)))
                  (iter (cdr lst))))
          ; quasiquote hmap literal
          ((hmap? hd)
            (cons (list 'list (list 'heist:core:quasiquote:list->hmap (heist:core:quasiquote->quote (heist:core:quasiquote:hmap->list hd) level)))
                  (iter (cdr lst))))
          ; quote atom
          ((atom? hd)
            (cons (list 'list (list 'quote hd))
                  (iter (cdr lst))))
          ; unquote datum
          ((heist:core:quasiquote:tagged-list? hd 'unquote)
            (if (= level 0)
                (cons (list 'list (cadr hd)) 
                      (iter (cdr lst)))
                (cons (list 'list (heist:core:quasiquote->quote hd level)) ; recursively parse, in nested quasiquote (level will be decremented *there*)
                      (iter (cdr lst)))))
          ; unquote & signal should splice element
          ((heist:core:quasiquote:tagged-list? hd 'unquote-splicing)
            (if (= level 0)
                (cons (cadr hd) ; evaluate datum & append to the expression
                      (iter (cdr lst)))
                (cons (list 'list (heist:core:quasiquote->quote hd (- level 1))) ; recursively parse, in nested quasiquote
                      (iter (cdr lst)))))
          ; nested quasiquote
          ((heist:core:quasiquote:tagged-list? hd 'quasiquote)
            (cons (list 'list (heist:core:quasiquote->quote hd (+ level 1))) ; recursively parse, in nested quasiquote
                  (iter (cdr lst))))
          ; quasiquote expression
          (else
            (cons (list 'list (heist:core:quasiquote->quote hd level))
                  (iter (cdr lst))))))
  (cons 'append (iter lst)))


(core-syntax quasiquote
  (lambda (heist:core:quasiquote-expr)
    (if (= (length heist:core:quasiquote-expr) 2)
        (heist:core:quasiquote:optimize (heist:core:quasiquote->quote (cadr heist:core:quasiquote-expr) 0))
        (syntax-error 'quasiquote "Quasiquote wasn't given exactly 1 arg," heist:core:quasiquote-expr))))

;; =========================================================
;; =========== "NEW" MACRO FOR ANONYMOUS OBJECTS ===========
;; =========================================================

(define (heist:oo:anon:members=? lhs rhs pred?) 
  (define lhs-members (object-members lhs)) 
  (define rhs-members (object-members rhs))
  (hmap-delete! lhs-members 'prototype) 
  (hmap-delete! rhs-members 'prototype)
  (pred? lhs-members rhs-members))

(define (heist:oo:anon:methods=? lhs rhs pred?) ; checks NAME equality
  (pred? (hmap-keys (object-methods lhs)) (hmap-keys (object-methods rhs))))

(define (heist:oo:anon:equal? self obj pred?)
  (and (object? obj) 
       (eq? 'heist:oo:anon:prototype (proto-name obj.prototype))
       (heist:oo:anon:methods=? self obj pred?)
       (heist:oo:anon:members=? self obj pred?)))

; Macro "new" used to create anonymous objects!
(core-syntax new
  (syntax-rules ()
    ((_ (name value) ...)
     (let ()
        (defclass heist:oo:anon:prototype ()
          (name value) ...
          ((equal? obj) ; structural equality (w/o prototype)
            (heist:oo:anon:equal? self obj equal?))
          ((eqv? obj) ; structural equality (w/o prototype)
            (heist:oo:anon:equal? self obj eqv?)))
        (new-heist:oo:anon:prototype)))))

;; ===============================================================================
;; =========== "SUPER" MACRO TO INVOKE THE INHERITED CLASS CONSTRUCTOR ===========
;; ===============================================================================

(core-syntax super
  (syntax-rules ()
    ((_) (set! self.super (self.super.prototype)))
    ((_ p ...) (set! self.super (self.super.prototype p ...)))))

;; ==================================================================
;; =========== "DEFINE-MODULE" MACRO FOR PROCEDURE HIDING ===========
;; ==================================================================

(defclass module ()
  (heist:core:module:name #f)
  ((module name)
    (set! self.heist:core:module:name 
          (append "#<module[" (symbol->string name) "]>")))
  ((self->type) 'module)
  ((self->string) self.heist:core:module:name))

(core-syntax define-module
  (syntax-rules ()
    ; anonymous module
    ((_ (exposure ...) expression ...)
      (begin 
        (define `@module-name ; hash anon module's name at expansion time
          (let ()
            expression ...
            (fn (((quote exposure) heist:core:module:arguments)
                  (apply exposure heist:core:module:arguments)) ...)))
        (define (exposure *dot* heist:core:module:arguments)
          (module-name (quote exposure) heist:core:module:arguments)) ...))
    ; named module
    ((_ module-name (exposure ...) expression ...)
      (begin 
        (define module-name (module (quote module-name)))
        (let ()
          expression ...
          ((.. module-name 'add-property!) (quote exposure) exposure) ...
          module-name)))))

;; ===================================
;; =========== CURRY MACRO ===========
;; ===================================

; CURRIED LAMBDAS via Macro
; => NOTE: It is UNDEFINED BEHAVIOR to have a VARIADIC CURRIED lambda
;          IE: (define f (curry (x . xs) x)) ;; INVALID!
; => 'lambda alternative that will curry its arguments
;    => IE (define K (curry (a b) a)) => (define K (lambda (a) (lambda (b) a)))
;          ((K 1) 2) = (K 1 2) ;; BOTH OF THESE MEANS OF APPLICATION WORK IDENTICALLY!
(core-syntax curry 
  (syntax-rules ()
    ((_ () body ...)
      (lambda () body ...))
    ((_ (arg) body ...) 
      (lambda (x *dot* xs) ; *dot* ensures variadic procedure after expansion even if user redefines '.
        (fold (lambda (f a) (f a)) 
              (lambda (arg) body ...)
              (cons x xs))))
    ((_ (arg rest-args ...) body ...)
      (let ((heist:curry:curried-lambdas 
              (lambda (arg) (curry (rest-args ...) body ...))))
        (lambda (x *dot* xs)
          (fold (lambda (f a) (f a)) 
                heist:curry:curried-lambdas
                (cons x xs)))))))

;; =========================================================
;; =========== OVERLOAD EXISTING FUNCTIONS MACRO ===========
;; =========================================================

; OVERLOAD EXISTING PROCEDURES
; => Use "*original*" to refer to <overloaded>
; => Use <else> to catch all other args that don't satisfy any <pred?>
(core-syntax define-overload
  (syntax-rules (else)
    ((_ overloaded (pred? function) ... (else else-function))
      (define overloaded
        (let ((*original* overloaded))
          (lambda (`@x *dot* `@xs) ; *dot* ensures variadic procedure after expansion even if user redefines '.
            (cond ((pred? x) (apply function (cons x xs))) ...
                  (else (apply else-function (cons x xs))))))))
    ((_ overloaded (pred? function) ...)
      (define overloaded
        (let ((*original* overloaded))
          (lambda (`@x *dot* `@xs)
            (cond ((pred? x) (apply function (cons x xs))) ...
                  (else (error 'overloaded "Unsupported Arg Type" x)))))))))

;; ==============================================
;; =========== LAZY STREAM ALGORITHMS ===========
;; ==============================================

; -:- TABLE OF CONTENTS -:-
; (stream-map        callable s . streams)
; (stream-filter     pred? s)
; (stream-take-while pred? s)
; (stream-from       first . optional-step)
; (stream-unfold     break-cond map-callable suc-callable seed)
; (stream-iterate    suc-callable seed)
; (stream-zip        s . streams)
; (stream-constant   . objs)
; (stream-append     s . streams)
; (stream-interleave stream1 stream2)
; (stream->generator s)

(define (heist:stream:error name message format variable)
  (syntax-error name (append message "\n" format) variable))


(defn stream-map
  ((callable) 
    (lambda (s . streams) 
      (apply stream-map (cons callable (cons s streams)))))
  ((callable s . streams)
    (if (not (callable? callable))
        (heist:stream:error 'stream-map "1st arg isn't a callable!" 
          "(stream-map <callable> <stream1> <stream2> ...)" callable))
    (for-each (lambda (s) 
                (if (not (stream? s)) 
                    (heist:stream:error 'stream-map "received a non stream!" 
                      "(stream-map <callable> <stream1> <stream2> ...)" s)))
              (cons s streams))
    (define (stream-map streams)
      (if (stream-null? (car streams))
          stream-null
          (scons
            (apply callable (map scar streams))
            (stream-map (map scdr streams)))))
    (stream-map (cons s streams))))


(defn stream-filter
  ((pred?)
    (lambda (s) (stream-filter pred? s)))
  ((pred? s)
    (if (not (callable? pred?))
        (heist:stream:error 'stream-filter "1st arg isn't a callable!" 
          "(stream-filter <predicate> <stream>)" pred?))
    (if (not (stream? s)) 
        (heist:stream:error 'stream-filter "2nd arg isn't a stream!" 
          "(stream-filter <predicate> <stream>)" s))
    (define (stream-filter s)
      (if (stream-null? s) 
          stream-null
          (if (pred? (scar s))
              (scons (scar s) (stream-filter (scdr s)))
              (stream-filter (scdr s)))))
    (stream-filter s)))


(defn stream-take-while
  ((pred?)
    (lambda (s) (stream-take-while pred? s)))
  ((pred? s)
    (if (not (callable? pred?))
        (heist:stream:error 'stream-take-while "1st arg isn't a callable!" 
          "(stream-take-while <predicate> <stream>)" pred?))
    (if (not (stream? s)) 
        (heist:stream:error 'stream-take-while "2nd arg isn't a stream!" 
          "(stream-take-while <predicate> <stream>)" s))
    (define (stream-take-while pred? s)
      (if (stream-null? s)
          '()
        (if (pred? (scar s))
            (scons (scar s) (stream-take-while pred? (scdr s)))
            '())))
    (stream-take-while pred? s)))


(defn stream-from
  ((first)
    (stream-from first 1))
  ((first step)
    (define (stream-from-iter n suc-proc)
      (scons n (stream-from-iter (suc-proc n) suc-proc)))
    (if (number? first)
        (if (number? step)
            (stream-from-iter first (+ step))
            (heist:stream:error 'stream-from "2nd arg isn't a number!" 
              "(stream-from <seed-number> <optional-step>)" step))
        (heist:stream:error 'stream-from "1st arg isn't a number!" 
          "(stream-from <seed-number> <optional-step>)" first))))


(defn stream-unfold
  ((break-cond)
    (fn 
      ((map-callable) (stream-unfold break-cond map-callable))
      ((map-callable suc-callable) (lambda (seed) (stream-unfold break-cond map-callable suc-callable seed)))
      ((map-callable suc-callable seed) (stream-unfold break-cond map-callable suc-callable seed))))
  ((break-cond map-callable)
    (fn
      ((suc-callable) (lambda (seed) (stream-unfold break-cond map-callable suc-callable seed)))
      ((suc-callable seed) (stream-unfold break-cond map-callable suc-callable seed))))
  ((break-cond map-callable suc-callable)
    (lambda (seed) (stream-unfold break-cond map-callable suc-callable seed)))
  ((break-cond map-callable suc-callable seed)
    (if (not (callable? break-cond))
        (heist:stream:error 'stream-unfold "1st arg isn't a callable!" 
          "(stream-unfold <break-condition> <map-callable> <successor-callable> <seed>)" break-cond))
    (if (not (callable? map-callable))
        (heist:stream:error 'stream-unfold "2nd arg isn't a callable!" 
          "(stream-unfold <break-condition> <map-callable> <successor-callable> <seed>)" map-callable))
    (if (not (callable? suc-callable))
        (heist:stream:error 'stream-unfold "3rd arg isn't a callable!" 
          "(stream-unfold <break-condition> <map-callable> <successor-callable> <seed>)" suc-callable))
    (define (stream-unfold seed)
      (if (break-cond seed)
          stream-null
          (scons (map-callable seed) (stream-unfold (suc-callable seed)))))
    (stream-unfold seed)))


(defn stream-iterate
  ((suc-callable)
    (lambda (seed) (stream-iterate suc-callable seed)))
  ((suc-callable seed)
    (if (not (callable? suc-callable))
        (heist:stream:error 'stream-iterate "1st arg isn't a callable!" 
          "(stream-iterate <successor-callable> <seed>)" suc-callable))
    (define (stream-iterate seed)
      (scons seed (stream-iterate (suc-callable seed))))
    (stream-iterate seed)))


(define (stream-zip s . streams)
  (for-each (lambda (s) 
              (if (not (stream? s)) 
                  (heist:stream:error 'stream-zip "received a non stream!" 
                    "(stream-zip <stream1> <stream2> ...)" s)))
            (cons s streams))
  (apply stream-map (cons list (cons s streams))))


(define (stream-constant . objs)
  (define (stream-constant obj-list)
    (if (null? obj-list)
        (stream-constant objs)
        (scons (car obj-list) (stream-constant (cdr obj-list)))))
  (if (null? objs)
      stream-null
      (stream-constant objs)))


(define (stream-append s . streams)
  (for-each (lambda (s) 
              (if (not (stream? s)) 
                  (heist:stream:error 'stream-append "received a non stream!" 
                    "(stream-append <stream1> <stream2> ...)" s)))
            (cons s streams))
  (define (stream-append s streams)
    (if (null? s)
        (if (null? streams)
            stream-null
            (stream-append (car streams) (cdr streams)))
        (scons (scar s) (stream-append (scdr s) streams))))
  (if (null? streams) s (stream-append s streams)))


(defn stream-interleave
  ((stream1) (lambda (stream2) (stream-interleave stream1 stream2)))
  ((stream1 stream2)
    (if (not (stream? stream1))
        (heist:stream:error 'stream-interleave "1st arg isn't a stream!" 
          "(stream-interleave <stream1> <stream2>)" stream1))
    (if (not (stream? stream2))
        (heist:stream:error 'stream-interleave "2nd arg isn't a stream!" 
          "(stream-interleave <stream2> <stream2>)" stream2))
    (define (stream-interleave stream1 stream2)
      (if (stream-null? stream1)
          stream2
          (scons (scar stream1) (stream-interleave stream2 (scdr stream1)))))
    (stream-interleave stream1 stream2)))


(define (stream->generator s)
  (if (not (stream? s))
      (heist:stream:error 'stream->generator "arg isn't a stream!" 
        "(stream->generator <stream>)" s))
  (if (null? s)
      (lambda () 'generator-complete)
      (let ((new-stream (scons '() s)))
        (lambda ()
          (if (null? new-stream)
              'generator-complete
              (begin 
                (set! new-stream (scdr new-stream))
                (scar new-stream)))))))

;; ===================================================
;; =========== MATHEMATIC FLONUM CONSTANTS ===========
;; ===================================================

(define fl-e (exp 1)) ; Bound to the mathematical constant e
(define fl-1/e (/ fl-e)) ; Bound to 1/e
(define fl-e-2 (exp 2)) ; Bound to e^2
(define fl-pi (acos -1)) ; Bound to the mathematical constant π
(define fl-1/pi (/ fl-pi)) ; Bound to 1/π
(define fl-2pi (* fl-pi 2)) ; Bound to 2π
(define fl-pi/2 (/ fl-pi 2)) ; Bound to π/2
(define fl-pi/4 (/ fl-pi 4)) ; Bound to π/4
(define fl-pi-squared (* fl-pi fl-pi)) ; Bound to π^2
(define fl-rad/deg (/ fl-pi 180)) ; Bound to π/180, the number of radians in a degree.
(define fl-deg/rad (/ fl-rad/deg)) ; Bound to 180/π, the number of degrees in a radian.
(define fl-2/pi (/ 2 fl-pi)) ; Bound to 2/π
(define fl-2/sqrt-pi (/ 2 (sqrt fl-pi))) ; Bound to 2/√π
(define fl-e-pi/4 (exp fl-pi/4)) ; Bound to e^(π/4)
(define fl-log2-e (/ (log 2))) ; Bound to log2(e)
(define fl-log10-e (/ (log 10))) ; Bound to log10(e)
(define fl-log-2 (log 2)) ; Bound to loge(2)
(define fl-1/log-2 (/ fl-log-2)) ; Bound to 1/loge(2)
(define fl-log-3 (log 3)) ; Bound to loge(3)
(define fl-log-pi (log fl-pi)) ; Bound to loge(π)
(define fl-log-10 (log 10)) ; Bound to loge(10)
(define fl-1/log-10 (/ fl-log-10)) ; Bound to 1/loge(10)
(define fl-sqrt-2 (sqrt 2)) ; Bound to √2
(define fl-sqrt-3 (sqrt 3)) ; Bound to √3
(define fl-sqrt-5 (sqrt 5)) ; Bound to √5
(define fl-sqrt-10 (sqrt 10)) ; Bound to √10
(define fl-1/sqrt-2 (/ fl-sqrt-2)) ; Bound to 1/√2
(define fl-cbrt-2 (expt 2 (/ 3))) ; Bound to 2^(1/3)
(define fl-cbrt-3 (expt 3 (/ 3))) ; Bound to 3^(1/3)
(define fl-4thrt-2 (expt 2 (/ 4))) ; Bound to 2^(1/4)
(define fl-phi 1.618033988749895) ; Bound to the mathematical constant φ
(define fl-log-phi (log fl-phi)) ; Bound to loge(φ)
(define fl-1/log-phi (/ fl-log-phi)) ; Bound to 1/loge(φ)
(define fl-euler 0.5772156649015329) ; Bound to the mathematical constant γ (Euler's constant)
(define fl-e-euler (exp fl-euler)) ; Bound to e^γ
(define fl-sin-1 (sin 1)) ; Bound to sin(1)
(define fl-cos-1 (cos 1)) ; Bound to cos(1)
(define fl-gamma-1/2 (sqrt fl-pi)) ; Bound to Γ(1/2) = √π
(define fl-gamma-1/3 2.678938534707748) ; Bound to Γ(1/3)
(define fl-gamma-2/3 1.3541179394264) ; Bound to Γ(2/3)

;; =========================================
;; =========== COMPOSE BIND & ID ===========
;; =========================================

; COMPOSE PROCS (RETURNS A PROC OF N ARGS APPLYING THEM TO THE COMPOSITION)
(define (compose proc . procs)
  (define rev-procs (reverse (cons proc procs)))
  (define fst-proc (car rev-procs))
  (define rest-procs (cdr rev-procs))
  (lambda (. args)
    (fold (lambda (result fcn) (fcn result)) 
          (apply fst-proc args) 
          rest-procs)))

(define o compose)


; BIND ARGUMENTS TO A PROC PRODUCING A NEW PROC
(define (bind proc . bound-vals)
  (lambda (. new-vals)
    (apply proc (append bound-vals new-vals))))


; IDENTITY PRIMITIVE PROCEDURE
(define (id a) a)

;; ==================================================
;; =========== SCM->CPS HELPER PROCEDURES ===========
;; ==================================================

; CALL/CC (ONLY WORKS IN scm->cps BLOCKS!)
(define (heist:core:pass-continuation-call/cc f k)
  (define (heist:core:pass-continuation-k-kestrel a b) (k a)) ; ignore 2nd cont. passed by CPS-ification
  (heist:core:apply-with-continuation f heist:core:pass-continuation-k-kestrel k))
(define call/cc heist:core:pass-continuation-call/cc)
(define call-with-current-continuation heist:core:pass-continuation-call/cc)


; CPS-EVAL & CPS-LOAD
(define cps-eval heist:core:pass-continuation-cps-eval)
(define cps-load heist:core:pass-continuation-cps-load)

;; ===============================
;; =========== CALL/CE ===========
;; ===============================

(define (call/ce heist:core:call/ce:callable . heist:core:call/ce:args)
  (apply (lexical-scope->dynamic-scope heist:core:call/ce:callable) heist:core:call/ce:args))
(set! call/ce (lexical-scope->dynamic-scope call/ce))
(define call-with-current-environment call/ce)

;; ==================================
;; =========== COROUTINES ===========
;; ==================================

;; Define a call/cc equivalent for coroutines
(define (heist:core:pass-continuation-co-call/cc f k) (heist:core:apply-with-continuation f k k))


(defclass coroutine ()
  (value #f) ; access yielded value
  ((next)    ; continue/start coroutine
    (if self.coroutine:private:cont                                          ; if started coroutine
        (heist:core:apply-with-continuation self.coroutine:private:cont id)  ;   continue execution
        (self.coroutine:private:launch)))                                    ; else launch coroutine
  ((self->type) 'coroutine)
  ((self->string) (append "#<coroutine[" (pointer-address self) "]>"))
  (coroutine:private:cont #f)    ; IGNORE: USED INTERNALLY
  (coroutine:private:launch #f)) ; IGNORE: USED INTERNALLY


(core-syntax yield 
  (syntax-rules () ; ONLY DESIGNED TO BE USED IN define-coroutine BODIES
    ((_ val)
      (heist:core:pass-continuation-co-call/cc
        (lambda (k)
          (jump! (new-coroutine (vector val (lambda () (catch-jump k id))))))))
    ((_)
      (heist:core:pass-continuation-co-call/cc
        (lambda (k)
          (jump! (new-coroutine (vector #f (lambda () (catch-jump k id))))))))))


;; NESTING define-coroutine CAUSES UNDEFINED BEHAVIOR
;; LAST RETURNED VALUE IS "#<procedure id>" IF THE LAST EXPRESSION YIELDS
;; -> IE if last expr yeilds, calling .next that result will return #<procedure id>
(core-syntax define-coroutine 
  (syntax-rules () 
    ((_ (co-name) body ...)
      (define (co-name)
        (new-coroutine (vector #f #f (lambda () (catch-jump (scm->cps body ...) id))))))
    ((_ (co-name arg ...) body ...)
      (define (co-name arg ...)
        (new-coroutine (vector #f #f (lambda () (catch-jump (scm->cps body ...) id))))))))


;; Convert a coroutine object into a generator thunk!
;; Returns the final value & stops iterating once coroutine finished
(define (coroutine->generator coroutine-object)
  (lambda ()
    (if (coroutine? coroutine-object)
        (begin 
          (set! coroutine-object (coroutine-object.next)) 
          (if (coroutine? coroutine-object)
              coroutine-object.value
              coroutine-object))
        'generator-complete))) ; finished iterating!

;; =================================
;; =========== UNIVERSES ===========
;; =================================

(defclass universe ()
  (universe:private:env '())
  (universe:private:buffer '(begin))
  ((push! datum)
    (set! self.universe:private:buffer
          (cons datum self.universe:private:buffer)))
  ((pop!)
    (if (> (length self.universe:private:buffer) 1)
        (let ((item (car self.universe:private:buffer)))
          (set! self.universe:private:buffer (cdr self.universe:private:buffer))
          item)))
  ((clear!)
    (set! self.universe:private:buffer '(begin)))
  ((run!)
    (define result (self.eval (reverse self.universe:private:buffer)))
    (set! self.universe:private:buffer '(begin))
    result)
  ((eval datum)
    (heist:core:universe:eval datum self))
  ((self->type) 'universe)
  ((self->string) (append "#<universe[" (pointer-address self) "]>")))

;; =============================================
;; =========== TAU BETA PI ASCII ART ===========
;; =============================================

(define *tbp-string*
"  ._____________________.
 / .___________________. \\
| /          _          \\ |
||         // \\\\         ||
||     2 0 \\\\ // 2 2     ||
||    ,_____| |_____,    ||
||    |_ 1 8 _ 8 5 _|    ||
||      /  /   \\  \\      ||
||     /  / CAZ \\  \\     ||
|| ,__/  /_______\\  \\__, ||
|| |___T____,B,____P___| ||
||          | |          ||
||   C   S  | |  E   N   ||
||          |_|          ||
| \\_____________________/ |
 \\_______________________/")


;; display <*tbp-string*>
(defn tbp 
  (() (display *tbp-string*))
  ((string-or-output-port) (display *tbp-string* string-or-output-port)))

;; =============================================================
;; =========== LICENSE ST-BUILD-SYSTEM & SHELL-ALIAS ===========
;; =============================================================

(define license
  (let ((license-text (slurp-file (append *heist-dirname* "/LICENSE"))))
    (fn (() (display license-text))
        ((string-or-output-port) (display license-text string-or-output-port)))))

(define sublime-text-build-system
  (let ((st (append "{\n  \"cmd\": [\"" *heist-dirname* "/heist\", \"-nansi\", \"$file\"],\n  \"file_regex\": \"^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$\",\n}")))
    (fn (() (display st))
        ((string-or-output-port) (display st string-or-output-port)))))

(define shell-alias
  (let ((shell-alias-string (append "alias heist='" *heist-dirname* "/heist'")))
    (fn (() (display shell-alias-string))
        ((string-or-output-port) (display shell-alias-string string-or-output-port)))))

;; ===================================================================================================
;; =========== FILE & PORT READING (SLURP ALTERNATIVE FOR DATA STRUCTS INSTEAD OF STRINGS) ===========
;; ===================================================================================================

(defn read-port
  (() (read-port (current-input-port))) ; read rest of exprs in input-port
  ((input-port)
    (define (parse-expression datum)
      (if (eof-object? datum)
          '()
          (cons datum (parse-expression (read input-port)))))
    (cons 'begin (parse-expression (read input-port)))))

(define (read-file filename) ; read all exprs in filename
  (define port (open-input-file filename))
  (define port-contents (read-port port))
  (close-port port)
  port-contents)

;; ==========================================================================
;; =========== OPTIMIZED MACRO EXPANSION (UNWRAPPED UNARY BEGINS) ===========
;; ==========================================================================

(define (heist:core:expand*:unexpandable? d)
  (or (eq? d 'quote)         (eq? d 'cps-quote)    (eq? d 'core-syntax)
      (eq? d 'define-syntax) (eq? d 'syntax-rules)))

(define (heist:core:expand*:unwrap-begins datum)
  (if (and (list? datum) (pair? datum) (not (heist:core:expand*:unexpandable? (car datum))))
      (if (and (= (length datum) 2) (eq? (car datum) 'begin))
          (heist:core:expand*:unwrap-begins (cadr datum))
          (map heist:core:expand*:unwrap-begins datum))
      datum))

(define (expand* heist:core:expand*:datum)
  (heist:core:expand*:unwrap-begins (expand heist:core:expand*:datum)))
(set! expand* (lexical-scope->dynamic-scope expand*)) ; enable "expand" to capture localized "define-syntax" bindings

(define (core-expand* heist:core:core-expand*:datum)
  (heist:core:expand*:unwrap-begins (core-expand heist:core:core-expand*:datum)))
