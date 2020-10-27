;; Author: Jordan Randleman -- jrandleman@scu.edu -- heist_primitives.scm
;; => Defines helper functions for the Heist Scheme Interpreter's Scheme primitives

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


(define (stream-map callable s . streams)
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
  (stream-map (cons s streams)))


(define (stream-filter pred? s)
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
  (stream-filter s))


(define (stream-from first . optional-step)
  (define (stream-from-iter n suc-proc)
    (scons n (stream-from-iter (suc-proc n) suc-proc)))
  (define step 
    (if (null? optional-step)
        1
        (if (null? (cdr optional-step))
            (if (number? (car optional-step))
                (car optional-step)
                (heist:stream:error 'stream-from "2nd arg isn't a number!" 
                  "(stream-from <seed-number> <optional-step>)" (car optional-step)))
            (heist:stream:error 'stream-from "received more than 1 step!" 
              "(stream-from <seed-number> <optional-step>)" step))))
  (if (number? first)
      (stream-from-iter first (lambda (num) (+ num step)))
      (heist:stream:error 'stream-from "1st arg isn't a number!" 
        "(stream-from <seed-number> <optional-step>)" first)))


(define (stream-unfold break-cond map-callable suc-callable seed)
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
  (stream-unfold seed))


(define (stream-iterate suc-callable seed)
  (if (not (callable? suc-callable))
      (heist:stream:error 'stream-iterate "1st arg isn't a callable!" 
        "(stream-iterate <successor-callable> <seed>)" suc-callable))
  (define (stream-iterate seed)
    (scons seed (stream-iterate (suc-callable seed))))
  (stream-iterate seed))


(define (stream-zip s . streams)
  (for-each (lambda (s) 
              (if (not (stream? s)) 
                  (heist:stream:error 'stream-zip "received a non stream!" 
                    "(stream-zip <stream1> <stream2> ...)" s)))
            (cons s streams))
  (apply stream-map (cons (lambda (. l) l) (cons s streams))))


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


(define (stream-interleave stream1 stream2)
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
  (stream-interleave stream1 stream2))

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
      (lambda (x . xs)
        (fold (lambda (f a) (f a)) 
              (lambda (a) a)
              (cons (lambda (arg) body ...)
                    (cons x xs)))))
    ((_ (arg rest-args ...) body ...)
      (lambda (x . xs)
        (define curried-lambdas
          (lambda (arg) (curry (rest-args ...) body ...)))
        (fold (lambda (f a) (f a)) 
              (lambda (a) a)
              (cons curried-lambdas
                    (cons x xs)))))))

;; =========================================================
;; =========== OVERLOAD EXISTING FUNCTIONS MACRO ===========
;; =========================================================

; OVERLOAD EXISTING PROCEDURES
; => Use "overload:original" to refer to <overloaded>
; => Use <else> to catch all other args that don't satisfy any <pred?>
(core-syntax define-overload
  (syntax-rules (else)
    ((_ overloaded (pred? function) ... (else else-function))
      (define overloaded
        (let ((overload:original overloaded))
          (lambda (`@x . `@xs)
            (cond ((pred? x) (apply function (cons x xs))) ...
                  (else (apply else-function (cons x xs))))))))
    ((_ overloaded (pred? function) ...)
      (define overloaded
        (let ((overload:original overloaded))
          (lambda (`@x . `@xs)
            (cond ((pred? x) (apply function (cons x xs))) ...
                  (else 'overloaded "Unsupported Arg Type" x))))))))

;; =========================================
;; =========== COMPOSE BIND & ID ===========
;; =========================================

; COMPOSE PROCS (RETURNS A PROC OF N ARGS APPLYING THEM TO THE COMPOSITION)
(define (compose proc . procs)
  (define fst-proc (last (cons proc procs)))
  (define rest-procs (init (cons proc procs)))
  (lambda (. args)
    (define fst-result (apply fst-proc args))
    (fold (lambda (result fcn) (fcn result)) fst-result rest-procs)))


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
        (self.coroutine:private:cont #f id) ;   ignore #f value, artifact of cps-transform
        (self.coroutine:private:launch)))   ;   launch coroutine
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


;; Convert a coroutine iterator into a generator thunk!
;; Returns the final value & stops iterating once coroutine finished
(define (coroutine->generator coroutine-instance)
  (let ((co-iter coroutine-instance))
    (lambda ()
      (if (coroutine? co-iter)
          (begin 
            (set! co-iter (co-iter.next)) 
            (if (coroutine? co-iter)
                co-iter.value
                co-iter))
          'coroutine-complete)))) ; finished iterating!