;; Author: Jordan Randleman -- bonus_features.scm
;; Nice Procedures to have in addition to Heist-Scheme's default facilities

;; NOTE: 'curry, 'coroutine, & 'new got integrated from here originally!

; -:- TABLE OF CONTENTS -:-
; prn, pr          ; write arbitrary # of args
; println, print   ; display arbitrary # of args
; pprintln, pprint ; pretty-print arbitrary # of args
; function         ; procedure definition that can use <return>!
; time-operation   ; time an operation!
; swap!            ; swap 2 variables
; tlambda          ; use predicates (including Type checks) on lambda args!
; defstruct        ; simple basic vector-based OOP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVENIENCE PRINTING PROCEDURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prn . d) (for-each write d) (newline))
(define (pr . d) (for-each write d))
(define (println . d) (for-each display d) (newline))
(define (print . d) (for-each display d))
(define (pprintln . d) (for-each pretty-print d) (newline))
(define (pprint . d) (for-each pretty-print d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURES THAT CAN USE IMMEDIATE RETURNS (SIMILAR TO C++ FUNCTIONS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(core-syntax function 
  (syntax-rules () ;; function procedures can use <return>
    ((_ (name) b ...)
      (define name (lambda () (catch-jump (lambda () b ...)))))
    ((_ (name a ...) b ...)
      (define name (lambda (a ...) (catch-jump (lambda () b ...)))))))

(define return jump!)


;;; Demo a immediately returning procedure
;(function (f a b) (return a) b)
;(prn (f 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TIME AN OPERATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(core-syntax time-operation 
  (syntax-rules ()
    ((_ op)
      (let ()
        (define time-operation:start (seconds-since-epoch))
        op
        (define time-operation:end (seconds-since-epoch))
        (display "\n===================================\n> Operation ")
        (display 'op)
        (display " took ")
        (display (- time-operation:end time-operation:start))
        (display "s!\n===================================\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SWAP 2 VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(core-syntax swap!
  (syntax-rules ()
    ((_ a b) (let ((`@tmp a)) (set! a b) (set! b tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLAMBDA MACRO FOR AUTOMATED PREDICATED LAMBDA ARGUMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <tlambda-exp>'s args may be of form <symbol> or <(<pred?> <symbol>)>
;; where args are default checked against 'pred? (if such is present)
(define (heist:core:tlambda->lambda tlambda-exp . err-message)
  (define err-prefix 
    (if (null? err-message) 
        "\"" 
        (append (car err-message) " \"")))
  (define pred?-errors '())
  (define (pred?->error pred?)
    (list 'if (list 'not pred?) 
              (list 'error ''tlambda (append (write pred? err-prefix) "\" Failed!") (cadr pred?))))
  (define lambda-args 
    (map (lambda (arg) 
            (if (pair? arg)
                (begin (set! pred?-errors (cons (pred?->error arg) pred?-errors))
                       (cadr arg))
                arg))
         (cadr tlambda-exp)))
  (if (null? pred?-errors)
      tlambda-exp
      (cons 'lambda (cons lambda-args (cons (cons 'begin pred?-errors) (cddr tlambda-exp))))))


;; Typed-Lambda Macro to automate predicates on arguments
(core-syntax tlambda 
  (syntax-rules ()
    ((_ () b ...) (lambda () b ...)) ; 0 args
    ((_ (a ...) b ...)               ; N args
      (eval (heist:core:tlambda->lambda (cons 'lambda (cons (list 'a ...) '(b ...))))))
    ((_ err-message (a ...) b ...)  ; optional-descriptor & N args
      (eval (heist:core:tlambda->lambda (cons 'lambda (cons (list 'a ...) '(b ...))) err-message)))))


;; Ex1: ; (tlambda ((string? s) any-arg (number? n)) <body>) ; predicated & arbitrary args
;; Ex2: ; (tlambda "optional-description" ((string? s) any-arg) <body>) ; optional descriptor
;; Ex3: ; (tlambda ((string? s) . ((lambda (ns) (every even? ns)) numbers)) <body>) ; predicated variadic 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFSTRUCT MACRO FOR SIMPLE VECTOR-BASED OO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert each <member-name> instance from <member-list> in <method-body-list>
;;   to be (<struct-name>-<member-name> this) instead
(define (heist:core:ctor-defmethod-body struct-name member-list method-body-list)
  (define member-setters (map (lambda (s) (cons (symbol-append 'set- s '!) s)) member-list))
  (define (member-name? d) (and (symbol? d) (memq d member-list)))
  (define (member-setter? d) (and (symbol? d) (assq d member-setters)))
  (define (generate-setter exp)
    (set-car! exp (symbol-append 'set- struct-name '- (cdr (assq (car exp) member-setters)) '!))
    (set-cdr! exp (cons 'this (cdr exp))))
  (define (expand-method-exp exp)
    (if (not (null? exp))
        (begin (if (member-name? (car exp))
                   (set-car! exp (list (symbol-append struct-name '- (car exp)) 'this))
                   (if (member-setter? (car exp))
                       (generate-setter exp)
                       (if (pair? (car exp)) (expand-method-exp (car exp)))))
               (expand-method-exp (cdr exp)))))
  (expand-method-exp method-body-list)
  method-body-list)


;; Return a quoted syntax-rules list for defining a <struct-name> method macro
(define (heist:core:ctor-defmethod-syntax-rules struct-name)
  `(syntax-rules ()
    ((_ (method-name arg ...) body ...) ; METHOD W/ ARGS
      (eval `(define (,(symbol-append ',(symbol-append struct-name '>) 'method-name) this arg ...)
                ,@(heist:core:ctor-defmethod-body ',struct-name (,(symbol-append struct-name '>slots)) '(body ...)))))
    ((_ (method-name) body ...) ; METHOD W/O ARGS
      (eval `(define (,(symbol-append ',(symbol-append struct-name '>) 'method-name) this)
                ,@(heist:core:ctor-defmethod-body ',struct-name (,(symbol-append struct-name '>slots)) '(body ...)))))))


;; DEFINES A "STRUCTURE" OBJECT BASED ON VECTOR ACCESS
;; => CREATES A CTOR, GETTER, SETTER, PREDICATE, ANALYSIS, AND METHOD-GENERATOR FOR THE STRUCTURE
;;    => WARNING: IT IS UNDEFINED BEHAVIOR TO HAVE "this" BE A MEMBER NAME OF ANY STRUCT
;; => (defstruct <name> <member-name-1> ... <member-name-N>)
;;    -> CTOR: Returns a <name> object w/ member values of <member-val-1> ... <member-val-N>
;;             (make-<name> <member-val-1> ... <member-val-N>) 
;;    -> GETTER: Returns <member-name> value (or #f if DNE) of <name> structure object <struct-object>
;;             (<name>-<member-name> <struct-object>) 
;;    -> SETTER: Sets <member-name> value to <new-val> of <name> structure object <struct-object> (returns #f if dne)
;;             (set-<name>-<member-name>! <struct-object> <new-val>) 
;;    -> PREDICATE: Returns whether <struct-object> is a <name> struct
;;             (<name>? <struct-object>) 
;;    -> ANALYSIS: Returns a quoted list of <name> struct's member names
;;             (<name>>slots) 
;;    -> METHOD-GENERATOR: Defines an interface to create struct methods
;;             (defmethod-<name> (<method-name> <arg-1> ... <arg-N>) <body>)
;;             => INVOKING METHODS: (<name>><method-name> <struct-object> <arg-1> ... <arg-N>)
;;             => ADVANTAGES OF METHODS:
;;                1) <struct-object> ARG IS AUTOMATICALLY ADDED AS this
;;                2) MEMBER SETTERS DON'T NEED THE OBJECT OR STRUCT NAME IN THE INVOCATION
;;                3) MEMBER GETTERS MAY BE INVOKED JUST BY USING THE MEMBER NAME
;;                >>> Suppose: (defstruct student name id) 
;;                             (define (printf . d) (for-each display d))
;;                   ; Writing:
;;                   (defmethod-student (greet your-name)
;;                      (set-id! (+ id 1))
;;                      (printf "Hello " your-name 
;;                              ", my name is " name " and my id is "
;;                              id ", great to meet you!\n"))
;;                   ; Gets expanded into:
;;                   (define (student>greet this your-name)
;;                      (set-student-id! this (+ (student-id this) 1))
;;                      (printf "Hello " your-name 
;;                              ", my name is " (student-name this) " and my id is "
;;                              (student-id this) ", great to meet you!\n"))
(core-syntax defstruct 
  (syntax-rules ()
    ((_ name field ...)
      (eval (list 'define (cons (symbol-append 'make- 'name) '(field ...))
                '(vector 'name field ...)))
      (eval (list 'define (list (symbol-append 'name '- 'field) 'obj)
                '(define res (assq 'field (map cons '(field ...) (iota (length '#(field ...)) 1))))
                '(if res
                     (ref obj (cdr res))
                     #f))) ...
      (eval (list 'define (list (symbol-append 'set- 'name '- 'field '!) 'obj 'new-val)
                '(define res (assq 'field (map cons '(field ...) (iota (length '#(field ...)) 1))))
                '(if res
                     (set-index! obj (cdr res) new-val)
                     #f))) ...
      (eval (list 'define (list (symbol-append 'name '?) 'obj)
                '(and (vector? obj)
                      (= (length obj) (+ 1 (length '#(field ...))))
                      (eq? (head obj) 'name))))
      (eval (list 'define (list (symbol-append 'name '>slots)) 
                ''(field ...)))
      (eval (list 'define-syntax (symbol-append 'defmethod- 'name)
                '(eval (heist:core:ctor-defmethod-syntax-rules 'name)))))))