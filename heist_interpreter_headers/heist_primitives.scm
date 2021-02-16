;; Author: Jordan Randleman -- jrandleman@scu.edu -- heist_primitives.scm
;; => Defines helper functions for the Heist Scheme Interpreter's Scheme primitives

;; ====================================================
;; =========== SELECTED SPECIAL FORM MACROS ===========
;; ====================================================

; -:- TABLE OF CONTENTS -:-
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
; defn
; while
; do

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
    ((_ ((var val update) ...)
        (break-test))
      (letrec ((heist:core:do-call 
                (lambda (var ...)
                  (if (not break-test)
                      (begin (set! var update) ... (heist:core:do-call var ...))))))
              (heist:core:do-call val ...)))
    ; no params (implies a body)
    ((_ ()
        (break-test returns ...)
        body ...)
      (letrec ((heist:core:do-call 
                (lambda ()
                  (if break-test 
                      (begin returns ...) 
                      (begin body ... (heist:core:do-call))))))
              (heist:core:do-call)))
    ; no params nor returns (implies a body)
    ((_ ()
        (break-test)
        body ...)
      (letrec ((heist:core:do-call 
                (lambda () 
                  (if (not break-test) (begin body ... (heist:core:do-call))))))
              (heist:core:do-call)))))

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

(define (heist:oo:anon:equality self obj pred?)
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
            (heist:oo:anon:equality self obj equal?))
          ((eqv? obj) ; structural equality (w/o prototype)
            (heist:oo:anon:equality self obj eqv?)))
        (new-heist:oo:anon:prototype)))))

;; ==================================================================
;; =========== "DEFINE-MODULE" MACRO FOR PROCEDURE HIDING ===========
;; ==================================================================

(core-syntax define-module
  (syntax-rules ()
    ((_ (exposure ...) expression ...)
      (begin 
        (define `@module-name ; hash module's name at expansion time
          (let ()
            expression ...
            (fn (((quote exposure) heist:core:module:arguments)
                  (apply exposure heist:core:module:arguments)) ...)))
        (define (exposure *dot* heist:core:module:arguments)
          (module-name (quote exposure) heist:core:module:arguments)) ...))))

;; ==============================================
;; =========== LAZY STREAM ALGORITHMS ===========
;; ==============================================

; -:- TABLE OF CONTENTS -:-
; (stream-map        callable s . streams)
; (stream-filter     pred? s)
; (stream-from       first . optional-step)
; (stream-unfold     break-cond map-callable suc-callable seed)
; (stream-iterate    suc-callable seed)
; (stream-zip        s . streams)
; (stream-constant   . objs)
; (stream-append     s . streams)
; (stream-interleave stream1 stream2)

(define (heist:stream:error name message format variable)
  (syntax-error name (append message "\n               " format "\n               ") variable))


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
  (f heist:core:pass-continuation-k-kestrel k))
(define call/cc heist:core:pass-continuation-call/cc)
(define call-with-current-continuation heist:core:pass-continuation-call/cc)


; CPS-EVAL & CPS-LOAD
(define cps-eval heist:core:pass-continuation-cps-eval)
(define cps-load heist:core:pass-continuation-cps-load)


; CPS->SCM (BINDS 'id TO THE GIVEN PROC'S CONTINUATION)[FOR scm->cps & REGULAR CODE HIGHER-ORDER FCN INTEROP]
(define (heist:core:pass-continuation-cps->scm proc k)
  (k (lambda (. args) (apply proc (append args (cons id '()))))))
(define cps->scm heist:core:pass-continuation-cps->scm)

;; ==================================
;; =========== COROUTINES ===========
;; ==================================

;; Define a call/cc equivalent for coroutines
(define (heist:core:pass-continuation-co-call/cc f k) (f k k))
(define co-eval cps-eval)
(define co-load cps-load)
(define co-fn   cps->scm)


(defclass coroutine ()
  (value #f) ; access yielded value
  ((next)    ; continue/start coroutine
    (if self.coroutine:private:cont         ; if started coroutine
        (self.coroutine:private:cont id)    ;   continue execution
        (self.coroutine:private:launch)))   ; else launch coroutine
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
        'coroutine-complete))) ; finished iterating!

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
    (heist:core:universe:eval datum self)))

;; =============================================
;; =========== TAU BETA PI ASCII ART ===========
;; =============================================

(define *tbp-string*
"          _
        // \\\\
    2 0 \\\\ // 2 2
   ,_____| |_____,
   |_ 1 8 _ 8 5 _| 
     /  /   \\  \\ 
    /  / CAZ \\  \\ 
,__/  /_______\\  \\__,
|___T____,B,____P___|
         | |
  C   S  | |  E   N
         |_|")


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
  (let ((st (append "{\n  \"cmd\": [\"" *heist-dirname* "/heist\", \"-nansi\", \"-script\", \"$file\"],\n  \"file_regex\": \"^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$\",\n}")))
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
