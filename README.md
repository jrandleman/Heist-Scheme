<!-- Author: Jordan Randleman -:- C++ Heist Scheme Interpreter's README -->

# Heist-Scheme
## Souped-Up Scheme Interpreter Written in C++!
### Written in as much C++ and as little Scheme as possible for runtime speed!


------------------------
# Using Heist Scheme:
=> _See [`INSTALL.md`](https://github.com/jrandleman/Heist-Scheme/blob/master/INSTALL.md) for step-by-step initial installation instructions!_<br>

0. Compiling the Interpreter: `$ clang++ -std=c++17 -O3 -o heist_main heist_main.cpp`
1. REPL: `$ ./heist_main` (exit REPL via [`(exit)`](#Control-Flow-Procedures) command)
2. Interpret Script: `$ ./heist_main -script <your-scripts-filename-here>`
3. Compile Script to C++: `$ ./heist_main -compile <your-scripts-filename-here> <optional-target-name>`
4. Embed Heist in C++: `#include` the [`heist_cpp_interop.hpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/heist_cpp_interop.hpp) header into your code (read it for more details)!
   * See [`embedded_heist_demo.cpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/heist_examples/embedded_heist_demo.cpp) for an example of embedding Heist in action!

------------------------
# Features:
0. Hygienic & Reader Macros
1. Tail-Call Optimization
2. Opt-In Dynamic Scoping (see the [`call/ce`](#control-flow-procedures) & [`inline`](#control-flow-procedures) application primitives)
3. Opt-In Continuations & [`call/cc`](#Scm-Cps-Procedures) (see [`scm->cps`](#Scm-Cps))
4. Native Even Streams (Lists w/ Delayed Car & Cdr)
5. Generic Algorithms (Polymorphic Algorithm Primitives)
6. SRFI Primitives (List, Vector, String, etc.)
7. Eval (Evaluate Symbolic Data as Code)
8. String I/O (Read/Write Compatibility w/ Strings as Ports)
9. Recursive Depth Control
10. And More!

------------------------ 
# Table of Contents
0. [Heist Properties](#Heist-Properties)
   - [Quick Overview](#Quick-Overview)
   - [Conventions](#Conventions)
   - [Metaprogramming Advantages](#Metaprogramming-Advantages)
   - [Notation](#Notation)
   - [Namespacing](#Namespacing)
1. [Heist Command-Line Flags](#Heist-Command-Line-Flags)
2. [Heist Primitive Data Types](#Heist-Primitive-Data-Types)
3. [Heist Numerics](#Heist-Numerics)
   - [3 Number Types](#3-Number-Types)
   - [2 Prefix Types](#2-Prefix-Types)
4. [Heist Hygienic Macro System, Procedures vs. Macros](#Heist-Hygienic-Macro-System-Procedures-vs-Macros)
5. [Heist Commenting](#Heist-Commenting)
6. [CPS: Continuation Passing Style](#CPS-Continuation-Passing-Style)
7. [Heist Special Forms](#Heist-Special-Forms)
   - [Quotation](#Quotation)
   - [Quasiquotation, Unquote, & Unquote-Splicing](#Quasiquotation-Unquote--Unquote-Splicing)
   - [Lambda](#Lambda)
   - [Define](#Define)
   - [Set!](#Set)
   - [Begin](#Begin)
   - [If](#If)
   - [And](#And)
   - [Or](#Or)
   - [Cond](#Cond)
   - [Case](#Case)
   - [Let](#Let)
   - [Let\*](#Let-1)
   - [Letrec](#Letrec)
   - [Do](#Do)
   - [Delay](#Delay)
   - [Scons](#Scons)
   - [Stream](#Stream)
   - [Vector-Literal](#Vector-Literal)
   - [Define-Syntax, Let-Syntax, Letrec-Syntax](#Define-Syntax-Let-Syntax-Letrec-Syntax)
   - [Syntax-Rules](#Syntax-Rules)
   - [Core-Syntax](#Core-Syntax)
   - [Cps-Quote](#Cps-Quote)
   - [Scm->Cps](#Scm-Cps)
   - [Defstruct](#Defstruct)
   - [Curry](#Curry)
   - [Tlambda](#Tlambda)
8. [Heist Primitive Variables](#Heist-Primitive-Variables)
9. [Heist Primitive Procedures](#Heist-Primitive-Procedures)
   - [Stream Primitives](#Stream-Primitives)
   - [Numeric Primitives](#Numeric-Primitives)
     * [General](#General)
     * [Numeric Predicates](#Numeric-Predicates)
     * [Numeric Rounding](#Numeric-Rounding)
     * [Trigonometry Procedures](#Trigonometry-Procedures)
     * [Logical Bitwise Operations](#Logical-Bitwise-Operations)
   - [Equality Predicates](#Equality-Predicates)
   - [Character Procedures](#Character-Procedures)
     * [General](#General-1)
     * [Character Predicates](#Character-Predicates)
   - [String Procedures](#String-Procedures)
     * [General](#General-2)
     * [String Predicates](#String-Predicates)
   - [List/Pair Procedures](#ListPair-Procedures)
     * [Accessors](#Accessors)
     * [List Constructors](#List-Constructors)
     * [List Predicates](#List-Predicates)
     * [List Seeking Procedures](#List-Seeking-Procedures)
   - [Vector Procedures](#Vector-Procedures)
   - [Generic Sequence, List|Vector|String, Algorithmic Procedures](#Generic-Sequence-ListVectorString-Algorithmic-Procedures)
     * [General](#General-3)
     * [Sorting Procedures](#Sorting-Procedures)
   - [Type Predicates](#Type-Predicates)
   - [Eval/Apply, Symbol-Append, & Typeof](#evalapply-symbol-append--typeof)
   - [Compose & Bind](#compose--bind)
   - [Delay Predicate & Force](#Delay-Predicate--Force)
   - [Type Coercion](#Type-Coercion)
   - [Output Procedures](#Output-Procedures)
   - [Input Procedures](#Input-Procedures)
   - [File & Port Procedures](#File--Port-Procedures)
   - [System Interface Procedures](#System-Interface-Procedures)
   - [Interpreter Invariants Manipulation](#Interpreter-Invariants-Manipulation)
   - [Control Flow Procedures](#Control-Flow-Procedures)
   - [Gensym](#Gensym)
   - [Scm->Cps Procedures](#Scm-Cps-Procedures)
   - [Syntax Procedures](#Syntax-Procedures)
10. [Heist Mathematical Flonum Constants](#Heist-Mathematical-Flonum-Constants)
11. [Heist Minimalist REPL Example](#Heist-Minimalist-REPL-Example)






------------------------ 
# Heist Properties

### _File Extension: `.scm` (scheme)_

## Quick Overview
* Weak & Dynamically Typed
* Properly Tail-Recursive
* Limits non-tail recursion to depth of 1000 by default
  - _See [`set-max-recursion-depth!`](#Interpreter-Invariants-Manipulation) primitive to change this_
* Embeddable in >= C++17
* `"heist:core:"` symbol prefix is reserved for internal use!

## Conventions:
* `?` suffix denotes a predicate procedure
* `!` suffix denotes a mutative (non-purely-functional) procedure
* `(`, `[`, & `{` are interchangeable (as are `)`, `]`, & `}`)
* `procedure` is said instead of `function`

## Metaprogramming Advantages:
* Code is data (parentheses construct an Abstract Syntax Tree)
  - Hence Hygienic Macro System enables direct manipulation of the AST
  - Quotation ([`quote`](#Quotation)) Converts Code to Data, Eval ([`eval`](#evalapply-symbol-append--typeof)) Converts Data to Code
  - Reader ([`read`](#Input-Procedures)) takes input and parses it into a quoted list of symbolic data
    * Hence [`read`](#Input-Procedures) and [`eval`](#evalapply-symbol-append--typeof) may be combined for a custom repl!

## Notation:
* Function (or "procedure") calls are denoted by parens:
  - in C++: `myFunc(0,'a',"hello")`
  - in Heist Scheme: `(myFunc 0 #\a "hello")`
* _Nearly_ every character can be used in a variable name!
  - Unless, of course, the combination could be interpreted as a<br>
    primitive data type (ie `1000` is an invalid variable name)
  - Hence can do things like name a factorial function `!` as if
    it were a primitive!

## Namespacing:
* Lisp 1: variables & procedures share a single namespace
* [`core-syntax`](#Core-Syntax) is evaluated first & ___MUST___ be matched (unlike runtime macros from [`define-syntax`](#Define-Syntax-Let-Syntax-Letrec-Syntax))
* Runtime macros & variables are in different namespaces
  - Hence if a [runtime macro's](#Define-Syntax-Let-Syntax-Letrec-Syntax) pattern doesn't match, it gets treated as an attempted procedure call




------------------------
# Heist Command-Line Flags
0. Interpret Script: `-script <script-filename>`
1. Compile Script: `-compile <script-filename> <optional-compiled-filename>`
   * _Only_ available if the `HEIST_DIRECTORY_FILE_PATH` macro is filled in by the user in [`heist_main.cpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/heist_main.cpp)
2. With CPS Evaluation: `-cps`
3. Disable ANSI Colors: `-nansi`
4. Case Insensitivity:  `-ci`






------------------------
# Heist Primitive Data Types
0. Symbol (quoted syntax label, `'hello`)
1. Number ([see numerics section](#Heist-Numerics))
2. Pair ([quoted](#Quotation) expression `'(1 2 3)`, [list](#ListPair-Procedures) `(list 1 2 3)`, or [cons](#ListPair-Procedures) `(cons 1 (cons 2 (cons 3 '())))`)
3. String (wrapped by `""`, `"hello"`)
4. Char (have the `#\` prefix, `#\h #\e #\l #\l #\o`) (uses `ascii` encoding!)
5. Boolean (true or false, `#t` or`#f`)
6. Vector (quoted literal `'#(1 2 3)`, or primitive `(vector 1 2 3)`)
7. Input Port, Output Port ([see port primitives](#File--Port-Procedures))
8. Syntax-Rules Object (see [`syntax-rules`](#Define-Syntax-Let-Syntax-Letrec-Syntax) special form)
9. Delayed Data (see [`delay`](#Delay) special form)
10. Procedure (via primitives or the [`lambda`](#Lambda) special form)
11. Void Data [`(void)`](#Type-Predicates)
12. Undefined Data [`(undefined)`](#Type-Predicates)






------------------------
# Heist Numerics
### 3 Number Types:
0. Exact/Ratnum (rational number)
   * Has a numerator and a denominator (automatically reduced to simplest form!)
   * _Special Case_: denominator of `1` creates a ___BigInt___ of arbitrary size
     ```scheme
     -1/2 ; stays as a fraction!
     3    ; ratnum w/ denom of 1 = bigint
     4/2  ; gets simplified to bigint 2
     ```
1. Inexact/Flonum (floating-point number)
   * Base-10 may use scientific notation!
   * Precision is bound by [`fl-precision`](#Heist-Primitive-Variables)
   * _Special Case_: `0.0` gets simplified to `0` (Zero is Exact)
     ```scheme
     1.0
     3.5e10 ; scientific notation
     -4E12  ; also scientific notation
     ```
2. Special Constants:
   * Positive Infinity: `+inf.0`
   * Negative Infinity: `-inf.0`
   * NaN: `+nan.0`, `-nan.0`
     - _Both `+nan.0` & `-nan.0` resolve to the same NaN object!_

### 2 Prefix Types:
0. Radix:
   - Binary: `#b`
   - Octal: `#o`
   - Hexadecimal: `#x`
   - Decimal: `#d` (enabled by default)
     ```scheme
     #b-101    ; -5
     #b10/11   ; 2/3
     #b1010.11 ; 10.75
   
     #o77 ; 63
   
     #xC0DE ; 49374
     #xc0de ; 49374
     ```
1. Exactness:
   - Inexact: `#i`
   - Exact: `#e`
     ```scheme
     #i3   ; 3.0
     #i1/2 ; 0.5

     #e3.5 ; 7/2
     #e1.0 ; 1

     #e#b101.1 ; Exact & Binary! => 11/2
     ```






------------------------
# Heist Hygienic Macro System, Procedures vs. Macros
One of Scheme's most powerful features is its flexible run-time hygienic macro system.<br>
Macros are identical to procedures, except for 3 key features:<br>

0. They ___expand___ into new code that will be run in the current scope, rather than<br> 
   processing a computation in a new scope (that of their definition, as with procedures)
   - ___Hygienic___ expansion also assures that no accidental expansions take place (unlike C++)
   - Macro argument names are automatically hashed to become unique symbols!
     ```scheme
     ;; The below macro (see [*]) clearly intends to expand into '2'.
     ;; However, unhygienic macro systems (like C++) would expand 'a' into 'b' (the arg),
     ;;   then that 'b' into 1. Since Scheme is hygienic though, the 'a' & 'b'
     ;;   in the macro's definition will be hashed & hence _not_ confused with the 'b' 
     ;;   passed to <my-macro>
     (define-syntax my-macro
        (syntax-rules ()
          ((_ a b) a)))
     (define b 2)
     (my-macro b 1) ; [*]
     ```
1. They do ___not___ evaluate their arguments (unlike procedures)
   - Hence macros can accept, and expand into, arbitrary code and data patterns!
2. They do _NOT_ have a recursive expansion limit (as does procedural non-tail-recursion)
   - Hence recursive expansions _MAY_ cause a segmentation fault if they infinitely expand
     - ___NOTE__: Such is an indication of a **USER** error however, and **NOT** an interpreter error!_






------------------------
# Heist Commenting
* Single-line comment: `;`
* Multi-line comment: Open: `#|` , Close: `|#`






------------------------
# CPS: Continuation Passing Style
A style of programming which explicitly handles control flow via "continuations",<br>
where a "continuation" represents the rest of the work to be done in a program.<br><br>

Programming with and manipulating continuations can yield certain advantages,<br>
most notably the ability to implement ___many___ control flow operations in terms<br>
of continuations (including threads, coroutines, try-catch, arbitrary returns, goto, etc.)<br><br>

Unfortunately, explicitly programming with continuations is rarely desirable and hardly enjoyable.<br>
Fortunately, there are ways to convert any program into CPS, and Scheme as a language has this<br>
transformation baked in by default.<br><br>

The power of continuations in Scheme may be leveraged through the primitive [`call/cc`](#Scm-Cps-Procedures) procedure:<br>
taking an unary procedure as its argument, [`call/cc`](#Scm-Cps-Procedures) (or [`call-with-current-continuation`](#Scm-Cps-Procedures)) passes<br>
the current continuation as an argument to the function it received.<br>
[Check out this blog post on implementing Coroutines, Exceptions, Generators, and more using `call/cc`](http://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/)!<br><br>

And yet, continuations pose certain penalties incurred by the transformation process, and as such<br>
some believe they should be removed from the Scheme standard altogether.<br>
Heist Scheme, in an effort to reconcile these perspectives, offers "opt-in" CPS tranformations by<br>
using the intrinsic [`scm->cps`](#scm-cps) macro to transform code blocks into CPS & the [`-cps`](#Heist-Command-Line-Flags) cmd-line flag to<br>
transform entire programs at the user's behest.<br><br>

As such, Heist programs may get the efficiency of not using continuations by default, then activate CPS<br>
transformations for their benefits as needed. However, this means that primitives such as [`call/cc`](#Scm-Cps-Procedures)<br>
may ___only___ be validly used in the scope of a [`scm->cps`](#scm-cps) block ___or___ when using the [`-cps`](#Heist-Command-Line-Flags) cmd-line flag.<br>
Other primitives of this nature include:<br>

0. [`load`](#system-interface-procedures) alternative in [`scm->cps`](#scm-cps) blocks: [`cps-load`](#system-interface-procedures)
1. [`eval`](#evalapply-symbol-append--typeof) alternative in [`scm->cps`](#scm-cps) blocks: [`cps-eval`](#evalapply-symbol-append--typeof)
2. [`compile`](#system-interface-procedures) alternative in [`scm->cps`](#scm-cps) blocks: [`cps-compile`](#system-interface-procedures)
3. Bind `id` as the continuation of a procedure: [`cps->scm`](#scm-cps-procedures)
   * for passing a procedure defined in a [`scm->cps`](#scm-cps) block as an argument<br>
     to a procedure __not__ defined in a [`scm->cps`](#scm-cps) block
   * Example:
     ```scheme
     ;; <sort> primitive is NOT defined in a <scm->cps> block, so <cps-lt>
     ;; _MUST_ be wrapped in <cps->scm> to bind <id> as its continuation
     ;; (since it was defined _IN_ a <scm->cps> block, it _EXPECTS_ a continuation,
     ;; but it won't get one if used in a non <scm->cps> block, such as in <sort>)
     ((scm->cps
      (define (cps-lt a b) (< a b))
      (sort (cps->scm cps-lt) '(1 3 5 7 2 4 6 8))
      ) id)
     ```






------------------------
# Heist Special Forms
### _Extensible via the Macro System!_


## Quotation:

#### Shorthand: `'<obj>` => `(quote <obj>)`

#### Use: ___Convert Code to Data!___

#### Quoting a Datum:
* Proper List: `(quote (<obj1> <obj2> ...))` => `(list '<obj1> '<obj2> (quote ...))`
* Dotted List: `(quote (<obj1> ... <objN> . <objN+1>))` =><br>
  `(append (list '<obj1> (quote ...) '<objN>) <objN+1>)`
* Empty List: `(quote ())` => `'()` _(unique value, ONLY one returning `#t` for `null?` primitive!)_
* Vector: `(quote #(<obj1> <obj2> ...))` => `(vector '<obj1> '<obj2> (quote ...))`
* Syntax: `(quote <syntax>)` => `<syntax-as-symbol>`
* Else: `(quote <any-other-obj>)` => `<any-other-obj>`

#### Examples:
```scheme
'12             ; => 12
'hello          ; => hello
'(1 2 3)        ; => (list 1 2 3)
'#(hello there) ; => (vector 'hello 'there)
''double        ; => (quote (quote double)) => (list 'quote 'double)
'(define a 12)  ; => (list 'define 'a '12) ; quoted code becomes a list of data!
```


------------------------
## Quasiquotation, Unquote, & Unquote-Splicing:

#### Shorthands: 
0. ``` `<obj> => (quasiquote <obj>)```
1. ``` ,<obj> => (unquote <obj>)```
2. ``` ,@<obj> => (unquote-splicing <obj>)```

#### Use: ___Selectivly Eval & Convert Code to Data!___

#### Quoting a Datum (exactly like `quote`, with 2 key exceptions):
0. `unquote`ing data undoes the quotation done by `quasiquote`
1. `unquote-splice` = `unquote` _and_ "unwraps" parenthesis
   * Hence result of `unquote-splice` **must** eval to acyclic list

#### Examples:
```scheme
(define a 12)
`(a a a)  ; => (list 'a 'a 'a)
`(a ,a a) ; => (list 'a 12 'a)

(define b '(1 2 3))
`(b b b)       ; => (list 'b 'b 'b)
`(,b ,b ,b)    ; => (list (list 1 2 3) (list 1 2 3) (list 1 2 3))
`(,@b ,@b ,@b) ; => (list 1 2 3 1 2 3 1 2 3)

(define c (cons 3 4))
`(1 2 ,c)  ; => `(1 2 (3 . 4))
`(1 ,c 2)  ; => `(1 (3 . 4) 2)
`(1 2 ,@c) ; => `(1 2 3 . 4)
`(1 ,@c 2) ; => ERROR! CANT APPEND 2 TO DOTTED LIST `(1 3 . 4)
```


------------------------
## Lambda:

#### Use: ___Generates Anonymous Procedure!___

#### Form: `(lambda (<arg1> <arg2> ...) <body> ...)`
* _Note: Pass a variadic number of args (0+) by using `.` as such:_
  - _Note: Variadic arg-list name must **always** be the last arg!_
    ```scheme
    (lambda (. va-args-list) <body> ...)       ; OK
    (lambda (a b . va-args-list) <body> ...)   ; OK
    (lambda (a b . va-args-list c) <body> ...) ; ERROR: Variadic Arg Name Isn't Last!
    ```


------------------------
## Define:

#### Use: ___Bind a Syntactic Label to a Value!___

#### Forms:
```scheme
;; Define a Variable
(define <name> <value>)

;; Define a Procedure
(define (<procedure-name> <arg1> <arg2> ...) <body> ...)
```

#### Procedure Definition Derivation:
```scheme
(define (<procedure-name> <arg> ...) <body> ...)
;; Becomes =>
(define <procedure-name> (lambda (<arg> ...) <body> ...))
```


------------------------
## Set!:

#### Use: ___Set a Syntactic Label to a New Value (must have already been defined)!___

#### Form: `(set! <name> <new-value>)`


------------------------
## Begin:

#### Use: ___Sequentially Evaluate Expressions (in the Current Environment Frame)!___
* Helps fit multiple expressions somewhere only expecting 1 (see [`if`](#if))

#### Form: `(begin <exp1> <exp2> ...)`


------------------------
## If:

#### Use: ___Conditional Branching!___

#### Form: `(if <condition> <consequent> <alternative>)`
* _Note: Use [`begin`](#begin) for multiple `<consequent>` and/or `<alternative>` expressions_


------------------------
## And:

#### Use: ___Confirm All Expressions Aren't `#f`!___

#### Form: `(and <exp1> <exp2> ...)`

#### Derivation Using [`if`](#if):
```scheme
(and <exp1> <exp2> <exp3> <exp4>)
;; Becomes =>
(if <exp1> (if <exp2> (if <exp3> <exp4> #f) #f) #f)
```


------------------------
## Or:

#### Use: ___Confirm 1 Expression Isn't `#f`!___

#### Form: `(or <exp1> <exp2> ...)`

#### Derivation Using [`if`](#if):
```scheme
(or <exp1> <exp2> <exp3> <exp4>)

;; Becomes =>

((lambda (or-result) ; Bind result to prevent 2x eval from condition & result
  (if or-result
      or-result
      ((lambda (or-result)
        (if or-result
            or-result
            ((lambda (or-result)
              (if or-result
                  or-result
                  <exp4>))
             <exp3>)))
       <exp2>)))
 <exp1>)
```


------------------------
## Cond:

#### Use: ___Concise If-Else Chains!___

#### Form: `(cond <clause1> <clause2> ...)`, `<clause>` = `(<condition> <exp1> <exp2> ...)`
* _Using `else` as the condition of the last clause is equivalent to using `#t` as the condition_
* _Use `=>` to apply the result of the condition to a procedure_

#### Derivation Using [`if`](#if):
```scheme
(cond (<condition1> <exp1> ...)
      (<condition2> <exp2> ...)
      (<condition3> => <procedure>)
      (else <exp4> ...))

;; Becomes =>

(if <condition1>
    (begin <exp1> ...)
    (if <condition2> 
        (begin <exp2> ...)
        (if <condition3>
            (<procedure> <condition3>)
            (begin <exp4> ...))))
```


------------------------
## Case:

#### Use: ___Switch-Statement Equivalent!___

#### Form: 
```scheme
(case <key> <clause1> ... <clauseN>)
; <clause> = ((<match1> ... <matchN>) <exp1> ... <expN>)
```
* _Using `else` as the condition of the last clause is equivalent to using `#t` as the condition_

#### Derivation Using [`cond`](#cond):
```scheme
(case <key> 
  ((<val1> ...) <exp1> ...)
  ((<val2> <key> <val3> ...) <exp2> ...)
  (else <exp3> ...))

;; Becomes =>

(cond ((memv <key> (list <val1> ...)) <exp1> ...) ; See the <memv> primitive!
      ((memv <key> (list <val2> <key> <val3> ...)) <exp2> ...)
      (else <exp3> ...))

```


------------------------
## Let:

#### Use: ___Temporary Bindings in a New Scope!___

#### Forms, `<arg-binding>` = `(<name> <value>)`:
0. Nameless: `(let (<arg-binding1> ... <arg-bindingN>) <body> ...)`
1. Named: `(let <name> (<arg-binding1> ... <arg-bindingN>) <body> ...)`

#### Derivations Using [`lambda`](#lambda):
```scheme
;; -:- NAMELESS -:-
(let ((<name> <value>) ...)
  <body> ...)
;; Becomes =>
((lambda (<name> ...) <body> ...)
 <value> ...)


;; -:- NAMED -:-
(let <procedure-name> ((<name> <value>) ...)
  <body> ...)
;; Becomes =>
(let () 
  (define <procedure-name>
    (lambda (<name> ...) <body> ...))
  (<procedure-name> <value> ...))
```


------------------------
## Let\*:

#### Use: ___Let with Bindings in Terms of One Another!___

#### Form: `(let* (<arg-binding1> ... <arg-bindingN>) <body> ...)`
* `<arg-binding>` = `(<name> <value>)`

#### Derivation Using [`let`](#let):
```scheme
(let* ((<name1> <value1>) (<name2> <value2>) (<name3> <value3>))
  <body> ...)

;; Becomes =>

(let ((<name1> <value1>))
  (let ((<name2> <value2>))
    (let ((<name3> <value3>))
      <body> ...)))
```


------------------------
## Letrec:

#### Use: ___Let with Recursive Bindings!___

#### Form: `(letrec (<arg-binding1> ... <arg-bindingN>) <body> ...)`
* `<arg-binding>` = `(<name> <value>)`

#### Derivation Using [`let`](#let):
```scheme
(letrec ((<name> <value>) ...)
  <body> ...)

;; Becomes =>

(let ((<name> #f) ...)
  (set! <name> <value>) ...
  <body> ...)
```


------------------------
## Do:

#### Use: ___Iteration Construct!___

#### Form:
```scheme
(do ((<var> <initial-val> <update>) ...)         ; <update> is optional (defaults to <var>)!
  (<break-test> <return-exp1> <return-exp2> ...) ; return expressions are optional (defaults to <void>)!
  <body> ...)
```

#### Derivation Using [`letrec`](#letrec):
```scheme
(do ((<var> <initial-val> <update>) ...)
    (<break-test> <return-exp1> <return-exp2> ...)
    <body> ...)

;; Becomes =>

(letrec ((<INTERNAL-RESERVED-NAME>
          (lambda (<var> ...)
            (if <break-test>
                (begin <return-exp1> <return-exp2> ...)
                (begin 
                  <body> ...
                  (set! <var> <update>) ...
                  (<INTERNAL-RESERVED-NAME> <var> ...))))))
        (<INTERNAL-RESERVED-NAME> <initial-val> ...))
```


------------------------
## Delay:

#### Use: ___Delay an Expression's Evaluation by Creating a Promise!___
* _Force the Promise to Run its Expression via the `force` primitive!_

#### Form: `(delay <exp>)`

#### Derivation Using [`lambda`](#lambda):
```scheme
(delay <exp>)

(force <promise>)

;; Becomes =>

(lambda () ; Memoized promises!
  (define already-run? #f)
  (define result #f)
  (lambda ()
    (if already-run?
        result
        (begin
          (set! already-run? #t)
          (set! result <exp>)
          result))))

(<promise>)
```


------------------------
## Scons:

#### Use: ___Create a Stream Pair!___
* _Stream pairs are regular pairs with delayed `car` and `cdr`!_
* _Allows for infinite lists (see `scar` & `scdr` primitives for manipulation)!_

#### Form: `(scons <obj1> <obj2>)`

#### Derivation Using [`delay`](#delay):
```scheme
(scons <obj1> <obj2>)
;; Becomes =>
(cons (delay <obj1>) (delay <obj2>))
```


------------------------
## Stream:

#### Use: ___Create a Stream!___
* _`stream` is to [`scons`](#scons) as [`list`](#list-constructors) is to [`cons`](#listpair-procedures)!_

#### Form: `(stream <obj1> <obj2> <obj3> ...)`

#### Derivation Using [`scons`](#scons):
```scheme
(stream <obj1> <obj2> <obj3>)
;; Becomes =>
(scons <obj1> (scons <obj2> (scons <obj3> '())))
```


------------------------
## Vector-Literal:

#### Use: ___Longhand Variant of the `#` Vector-Literal Shorthand!___
* _Hence, like `#`, `vector-literal` **must** be quoted to form a vector object!_

#### Form: `'(vector-literal <obj1> <obj2> <obj3> ...)`

#### Transformation:
```scheme
'#(<obj1> <obj2> <obj3> ...)
;; Becomes =>
'(vector-literal <obj1> <obj2> <obj3> ...)
;; Becomes =>
(vector '<obj1> '<obj2> '<obj3> '...)
```


------------------------
## Define-Syntax, Let-Syntax, Letrec-Syntax:

#### Use: ___Create a Run-Time Macro (Bind a Label to a Syntax Object)!___
* _Note: create a `<syntax-object>` via the [`syntax-rules`](#syntax-rules) special form below!_
* _Note: Run-Time macros are expanded **at run-time**, ie each time they're invoked!_
  - _See [`core-syntax`](#core-syntax) for an **analysis-time** macro alternative!_

#### Forms:
0. `(define-syntax <label> <syntax-object>)`
1. `(let-syntax ((<label> <syntax-object>) ...) <body> ...)`
2. `(letrec-syntax ((<label> <syntax-object>) ...) <body> ...)`

#### Derivation Using [`let`](#let):
```scheme
(let-syntax ((<label> <syntax-object>) ...) <body> ...)

(letrec-syntax ((<label> <syntax-object>) ...) <body> ...)

;; Both Become (letrec style macro evaluation is default) =>

(let ()
  (define-syntax <label> <syntax-object>) ...
  <body> ...)
```


------------------------
## Syntax-Rules:

#### Use: ___Construct a Syntax Object!___

#### Form: 
* `(syntax-rules (<key> ...) <syntax-clause1> <syntax-clause2> ...))`
  - `<syntax-clause>` = `(<pattern> <template>)`
    - `<pattern>` = `(<any-symbol> <expression-to-match-against>)`
    - `<template>` = `<expression-to-expand-into>`
  - _Note: Literals & `<key>`s in patterns must be matched exactly to expand!_
  - _Note: `...` is **always** a reserved `<key>` name!_

#### Variadic Matching & Hygienic Expansion:
##### Heist Scheme's Powerful Macro System Enables Matching & Constructing Arbitrarily Complex Expressions!
* _For Patterns:_
  - `<obj> ...` Matches 1 or more entities
  - `(<contents>) ...` Matches 1 or more expressions that match `<contents>`
    - _Note: Variadic Matches **must** accompany variadic expansions in the `<template>`!_
  - Examples:
    ```scheme
    a ...             ; Matches 1+ arbitrary objects
    (a b) ...         ; Matches 1+ pairs
    ((a b) (c d)) ... ; Matches 1+ pairs of pairs
    ((a ...) ...)     ; Matches 1+ expressions of 1+ arbitrary objects
    ```
* _For Templates:_
  - `<obj> ...` Expands 1 or more entities
  - `(<contents>) ...` Constructs 1 or more expressions with `<contents>`
  - _Note: Variadic Expansions **must** accompany variadic matches in the `<pattern>`!_
  - Examples:
    ```scheme
    a ...             ; Expands 1+ arbitrary objects
    (a b) ...         ; Constructs 1+ pairs of variadic matches <a> & <b>
    ((a b) (c d)) ... ; Constructs 1+ pairs of pairs of variadic matches <a>, <b>, <c>, & <d>
    ((a ...) ...)     ; Constructs 1+ expressions of 1+ variadic matches <a>
    ```

------------------------
## Core-Syntax:

#### Use: ___Construct an Analysis-Time Macro in the GLOBAL Scope!___

#### Form: 
* `(core-syntax <label> (<key> ...) <syntax-clause1> <syntax-clause2> ...))`
  - `<syntax-clause>` = `(<pattern> <template>)`
    - `<pattern>` = `(<any-symbol> <expression-to-match-against>)`
    - `<template>` = `<expression-to-expand-into>`
  - _Note: Form is as if [`define-syntax`](#Define-Syntax-Let-Syntax-Letrec-Syntax) and [`syntax-rules`](#syntax-rules) were merged!_

#### Analysis-Time Advantanges:
* Interpreter's [`eval`](#evalapply-symbol-append--typeof) seperates expression analysis (declaration) & execution (invocation):
  - [`define-syntax`](#Define-Syntax-Let-Syntax-Letrec-Syntax) macros, bound to an environment, dynamically expand at **run-time**
    * _Hence **run-time** macros in a [`lambda`](#lambda) body are re-expanded **upon every invocation!**_
  - [`core-syntax`](#core-syntax) macros, only bound to the **global environment**, expand at **analysis-time**
    * _Hence **analysis-time** macros in a [`lambda`](#lambda) body expand **in the [`lambda`](#lambda) declaration only once!**_


------------------------
## Cps-Quote:

#### Use: ___Convert Code to Data in CPS!___
* _Identical to [`quote`](#quotation) after transforming given code into CPS!_

#### Form: `(cps-quote <exp>)`


------------------------
## Scm->Cps:

#### Use: ___Convert Code to CPS & Evaluate the Result!___
* _Hence returns an unary procedure, accepting the "topmost" continuation!_
* _Enables use of [`call/cc`](#scm-cps-procedures), [`cps-eval`](#evalapply-symbol-append--typeof), [`cps-load`](#system-interface-procedures), & [`cps->scm`](#scm-cps-procedures) primitives!_
* _Automatically wraps entire program (& passed [`id`](#general-3)) if [`-cps`](#Heist-Command-Line-Flags) cmd-line flag used!_
* _Enables opt-in continuations for their benefits w/o their overhead when unused!_
  - _Optimizes the cps transformation as well for reasonable speed!_
  - _In general, `scm->cps` code at `-O3` optimization runs as fast as its non-cps version would at `-O0`_

#### Form: `(scm->cps <exp1> <exp2> ...)`

#### Coroutine Example Using [`call/cc`](#scm-cps-procedures):
```scheme
((scm->cps
  (define (make-queue) (cons '() '()))

  (define (enqueue! queue obj)
    (let ((lobj (list obj)))
      (if (null? (car queue))
          (begin
            (set-car! queue lobj)
            (set-cdr! queue lobj))
          (begin
            (set-cdr! (cdr queue) lobj)
            (set-cdr! queue lobj)))
            (car queue)))
   
  (define (dequeue! queue)
    (let ((obj (caar queue)))
      (set-car! queue (cdar queue))
      obj))

  ;;;; coroutine   
  (define process-queue (make-queue))

  (define (coroutine thunk)
    (enqueue! process-queue thunk))

  (define (start)
     ((dequeue! process-queue)))
     
  (define (pause)
    (call/cc
     (lambda (k)
       (coroutine (lambda () (k #f)))
       (start))))

  ;;;; example prints alternating ints & chars
  (coroutine (lambda ()
    (let loop ((i 0)) 
      (if (< i 26)
          (begin
            (display (+ 1 i)) ; print #
            (display " ") 
            (pause) ; pause coroutine to print a char
            (loop (+ 1 i)))))))
       
  (coroutine (lambda ()
    (let loop ((i 0)) 
      (if (< i 26)
          (begin
            (display (int->char (+ i 65))) ; print char
            (display " ")
            (pause) ; pause coroutine to print a #
            (loop (+ 1 i)))))))

  (newline)
  (start)) id)
```
##### Outputs:
```
1 A 2 B 3 C 4 D 5 E 6 F 7 G 8 H 9 I 10 J 11 K 12 L 13 M 14 N 15 O 16 P 17 Q 18 R 19 S 20 T 21 U 22 V 23 W 24 X 25 Y 26 Z
```


------------------------
## Defstruct:

#### Use: ___Define Objects for Object-Oriented Programming!___
* _Note: `defstruct` is actually a macro directly defined **in** Heist Scheme!_

#### Form: `(defstruct <name> <member-name1> <member-name2> ...)`

#### Procedures Generated:
0. Constructor: `(make-<name> <member-value1> <member-value2> ...)`
1. Getter: `(<name>-<member-name> <object>)`
2. Setter: `(set-<name>-<member-name>! <object> <new-val>)`
3. Predicate: `(<name>? <object>)`
4. Analysis (get quoted list of `<member>` names): `(<name>>slots)`
5. Method-Generator: `(defmethod-<name> (<method-name> <arg1> <arg2>) <body> ...)`
   - Advantages of Methods Over Procedures:
     1. `<object>` argument is automatically defined as `this`
     2. Member setters don't need the object or struct name in their invocation
     3. Member setters may be invoked just by using the member name
   - Example:
   ```scheme
   (defstruct student name id) 
   (define (printf . d) (for-each display d))
   ; Writing:
   (defmethod-student (greet your-name)
      (set-id! (+ id 1))
      (printf "Hello " your-name 
              ", my name is " name " and my id is "
              id ", great to meet you!\n"))
   ; Gets expanded into:
   (define (student>greet this your-name)
      (set-student-id! this (+ (student-id this) 1))
      (printf "Hello " your-name 
              ", my name is " (student-name this) " and my id is "
              (student-id this) ", great to meet you!\n"))
   ```


------------------------
## Curry:

#### Use: ___Define Curriable Lambdas with a Nicer Interface!___
* _Note: `curry` is actually a macro directly defined **in** Heist Scheme!_
* _Enables trivial means to bind arguments to values (especially helps w/ lambda calculus)_

#### Form: `(curry (<arg1> <arg2> ...) <body> ...)`
* _Note: it is undefined behavior to have a variadic `curry` lambda using `.`!_

#### Example:
```scheme
(define K (curry (a b) a))
; The following invocations are identical!
((K 1) 2) ; Traditional LISP curried call works!   ; => 1
(K 1 2)   ; Nicer invocation interface also works! ; => 1

(define Id (curry (a) a))
(define KI (K Id)) ; Binds "Id" as the first arg to "K"!
((KI 1) 2) ; "Id" is selected, then 2 is passed to "Id"! ; => 2
(KI 1 2)   ; => 2
```


------------------------
## Tlambda:

#### Use: ___Automate Predicating Lambda Arguments!___
* _Note: `tlambda` is actually a macro directly defined **in** Heist Scheme!_
* _Note: Predicates are optional (also works on variadic args)!_
* _Note: Add an optional descriptor string to be shown on predicate failures!_

#### Form: 
`(tlambda "optional-descriptor" 
          ((<pred?> <arg1>) <arg2> ... . (<pred?> <variadic-arg>)) 
          <body> ...)`

#### Examples:
```scheme
(tlambda ((string? s) any-arg (number? n)) <body>) ; predicated & arbitrary args
(tlambda "optional-description" ((string? s) any-arg) <body>) ; optional descriptor
(tlambda ((string? s) . ((lambda (ns) (every even? ns)) numbers)) <body>) ; predicated variadic 
```






------------------------
# Heist Primitive Variables

0. __True:__ `#t`

1. __False:__ `#f`

2. __Flonum Precision:__ `fl-precision`
   * Bound to `LDBL_DIG` from  `#include <cfloat>`

3. __Max Flonum Value:__ `fl-max`
   * Bound to `LDBL_MAX` from `#include <cfloat>`

4. __Min Flonum Value:__ `fl-min`
   * Bound to `LDBL_TRUE_MIN` if exists, else `LDBL_MIN`
     - _Either option from `#include <cfloat>`_

5. __Flonum Epsilon Value:__ `fl-epsilon`
   * Bound to `LDBL_EPSILON` from `#include <cfloat>`
   * Represents the smallest `x` so `1.0 + x != 1.0`

6. __The Empty Stream:__ `stream-null` (equivalent to `'()`)

7. __Optional Environment Arg Flags for [`Eval`](#evalapply-symbol-append--typeof), [`Load`](#system-interface-procedures), [`Cps-Eval`](#evalapply-symbol-append--typeof), [`Cps-Load`](#system-interface-procedures):__
   * Null Environment, all effects are sandboxed: `null-environment`
   * Local Environment, using local bindings: `local-environment`
   * Global Environment, using global bindings: `global-environment`






------------------------
# Heist Primitive Procedures
### _As if `define`d via `lambda`_


## Stream Primitives:
0. __Get Length of Stream__: `(stream-length <stream>)`

1. __Get Reverse of Stream__: `(stream-reverse <stream>)`

2. __Stream Access__: `scar` = first of stream, `scdr` = rest of stream, & composed `scar` & `scdr`
   * `(scar <stream>)`, `(scdr <stream>)`
   * `(scaar <stream>)`, `(scadr <stream>)`, `(scdar <stream>)`, `(scddr <stream>)`
   * `(scaaar <stream>)` ... `(scdddr <stream>)`
   * `(scaaaar <stream>)` ... `(scddddr <stream>)`

3. __Reference__: Get elt at `<index>` in `<stream-pair>`
   * `(stream-ref <stream-pair> <index>)`

4. __Append__: Join `<streams>` into a new stream
   * `(stream-append <stream1> <stream2> ...)`

5. __Drop__: Drop `<n>` elts from `<stream>`
   * `(stream-drop <stream> <n>)`

6. __Drop While__: Drop elts from `<stream>` while `<predicate?>` is true
   * `(stream-drop-while <predicate?> <stream>)`

7. __Take__: Take `<n>` elts from `<stream>`
   * `(stream-take <stream> <n>)`

8. __Take While__: Take elts from `<stream>` while `<predicate?>` is true
   * `(stream-take-while <predicate?> <stream>)`

9. __Map__: Apply `<procedure>` to each elt in each stream, forming a stream of results
   * `(stream-map <procedure> <stream1> <stream2> ...)`

10. __Filter__: Form a stream of elts from `<stream>` satisfying `<predicate?>`
    * `(stream-filter <predicate?> <stream>)`

11. __For Each__: Apply `<procedure>` to each elt of each `<stream>`
    * `(stream-for-each <procedure> <stream1> <stream2> ...)`

12. __Unfold__: Form a stream by mapping & incrementing seed, until `<break-cond-proc>` is true
    * _Note: **map** via `<map-proc>`, **increment** via `<suc-proc>`_
    * `(stream-unfold <break-cond-proc> <map-proc> <suc-proc> <seed>)`

13. __Fold__: Accumulate stream from left to right, starting with `<seed>` using `<procedure>`
    * `(stream-fold <procedure> <seed> <stream>)`

14. __Fold Right__: Accumulate stream from right to left, starting with `<seed>` using `<procedure>`
    * `(stream-fold-right <procedure> <seed> <stream>)`

15. __Numeric Stream__: Form a stream starting from `<first>` incrementing by `<optional-step>`
    * _Note: `<optional-step>` step is `1` by default_
    * `(stream-from <first> <optional-step>)`

16. __Stream Generation__: Form a stream starting from `<seed>` using `<suc-proc>`
    * `(stream-iterate <suc-proc> <seed>)`

17. __Zip__: Form a stream of lists containing the nth elt of each `<stream>`
    * `(stream-zip <stream1> <stream2> ...)`

18. __Infinite Cycle__: Forms an infinite stream of repeating `<objs>`
    * `(stream-constant <obj1> <obj2> ...)`
  
19. __Interleave__: Form a stream by interleaving elts of either `<stream>`
    * `(stream-interleave <stream1> <stream2>)`

20. __Stream->List Conversion__: Convert the 1st `<size>` elts of `<stream>` into a list
    * `(stream->list <stream> <size>)`

21. __List->Stream Conversion__: Convert `<list>` into a stream
    * `(list->stream <list>)`



------------------------
## Numeric Primitives:
### General:
0. __Addition__: Add n numbers
   * `(+ <number1> <number2> ...)`

1. __Subtraction__: Subtract n numbers, *or* negate 1 number
   * `(- <number1> <number2> ...)`
   * `(- <number>)`

2. __Multiplication__: Multiply n numbers
   * `(* <number1> <number2> ...)`

3. __Division__: Divide n numbers, *or* invert 1 number
   * `(/ <number1> <number2> ...)`
   * `(/ <number>)`

4. __Equality Comparisons__:
   * `(= <number1> <number2> ...)`
   * `(< <number1> <number2> ...)`
   * `(> <number1> <number2> ...)`
   * `(<= <number1> <number2> ...)`
   * `(>= <number1> <number2> ...)`

5. __Absolute Value__: `(abs <number>)`

6. __Exponentiation__: Raise `<number1>` to the power of `<number2>`
   * `(expt <number1> <number2>)`

7. __Exponentiation Modulo__: Raise `<number1>` to the power of `<number2>` modulo `<number3>`
   * `(expt-mod <number1> <number2> <number3>)`

8. __Maximum__: Get the maximum value
   * `(max <number1> <number2> ...)`

9. __Minimum__: Get the minimum value
   * `(min <number1> <number2> ...)`

10. __Quotient__: Get the quotient of `(/ <number1> <number2>)`
    * `(quotient <number1> <number2>)`

11. __Remainder__: Get the remainder of `(/ <number1> <number2>)`
    * `(remainder <number1> <number2>)`

12. __Modulo__: `(modulo <number1> <number2>)`

13. __Modulo Flonum__: Get the fractional portion of `<flonum>`
    * `(modf <flonum>)`

14. __Exponent__: Get e raised to the power of `<number>`
    * `(exp <number>)`

15. __Natural Logarithm__: `(log <number>)`

16. __Square Root__: `(sqrt <number>)`

17. __Greatest Common Denominator__: `(gcd <number1> <number2>)`

18. __Least Common Multiple__: `(lcm <number1> <number2>)`

19. __Extract Number's Numerator__: `(numerator <number>)`

20. __Extract Number's Denominator__: `(denominator <number>)`

21. __Generate a Log Procedure of a Certain Base__: `(make-log-base <number>)`

22. __Psuedo-Random Number Generator__: Seeded *or* unseeded
    * `(random)`, `(random <numeric-seed>)`

23. __Coerce Inexact to Exact__: `(inexact->exact <number>)`

24. __Coerce Exact to Inexact__: `(exact->inexact <number>)`


### Numeric Predicates:
0. __Odd Predicate__: `(odd? <number>)`

1. __Even Predicate__: `(even? <number>)`

2. __Positive Predicate__: `(positive? <number>)`

3. __Negative Predicate__: `(negative? <number>)`

4. __Zero Predicate__: `(zero? <number>)`

5. __Infinite Predicate__: `(infinite? <number>)`

6. __Finite Predicate__: `(finite? <number>)`

7. __NaN Predicate__: `(nan? <number>)`

8. __Exact Number Predicate__: `(exact? <number>)`

9. __Inexact Number Predicate__: `(inexact? <number>)`

10. __Integer Predicate__: `(integer? <number>)`


### Numeric Rounding:
0. __Round Number Up to Nearest Integer__: `(ceiling <number>)`

1. __Round Number Down to Nearest Integer__: `(floor <number>)`

2. __Round Number Towards Zero__: `(truncate <number>)`

3. __Round Number__: `(round <number>)`


### Trigonometry Procedures:
0. __Regular__: `(sin <number>)`, `(cos <number>)`, `(tan <number>)`

1. __Inverse__: `(asin <number>)`, `(acos <number>)`, `(atan <number>)`

2. __Hyperbolic__: `(sinh <number>)`, `(cosh <number>)`, `(tanh <number>)`

3. __Inverse Hyperbolic__: `(asinh <number>)`, `(acosh <number>)`, `(atanh <number>)`


### Logical Bitwise Operations:
0. __And__: `(logand <number1> <number2>)`

1. __Or__: `(logor <number1> <number2>)`

2. __Xor__: `(logxor <number1> <number2>)`

3. __Not__: `(lognot <number>)`

4. __Logical Shift Left__: `(loglsl <number> <shift-amount>)`

5. __Logical Shift Right__: `(loglsr <number> <shift-amount>)`

6. __Arithmetic Shift Right__: `(logasr <number> <shift-amount>)`

7. __Confirm Nth Bit is 1__: `(logbit? <number> <n>)`

8. __Set Nth Bit to 1__: `(logbit1 <number> <n>)`

9. __Set Nth Bit to 0__: `(logbit0 <number> <n>)`

10. __Complement Nth Bit__: `(logbit~ <number> <n>)`



------------------------
## Equality Predicates:
0. __Shallow Equality (pointer comparisons)__: `(eq? <obj1> <obj2> ...)`

1. __Equivalency Comparison (non-container value comparisons)__: `(eqv? <obj1> <obj2> ...)`

2. __Deep Equality (traverse continainers)__: `(equal? <obj1> <obj2> ...)`

3. __Boolean Not__: `(not <obj>)`



------------------------
## Character Procedures:
### General:
0. __Alphabetic Predicate__: `(char-alphabetic? <char>)`

1. __Numeric Predicate__: `(char-numeric? <char>)`

2. __Whitespace Predicate__: `(char-whitespace? <char>)`

3. __Uppercase Predicate__: `(char-upper-case? <char>)`

4. __Lowercase Predicate__: `(char-lower-case? <char>)`

5. __Convert to Uppercase__: `(char-upcase <char>)`

6. __Convert to Lowercase__: `(char-downcase <char>)`


### Character Predicates:
0. __Character Equality__: 
   * `(char=? <char1> <char2> ...)`
   * `(char<? <char1> <char2> ...)`
   * `(char>? <char1> <char2> ...)`
   * `(char<=? <char1> <char2> ...)`
   * `(char>=? <char1> <char2> ...)`

1. __Case-Insensitive Character Equality__: 
   * `(char-ci=? <char1> <char2> ...)`
   * `(char-ci<? <char1> <char2> ...)`
   * `(char-ci>? <char1> <char2> ...)`
   * `(char-ci<=? <char1> <char2> ...)`
   * `(char-ci>=? <char1> <char2> ...)`



------------------------
## String Procedures:
### General:
0. __Construction__: Creates a string of length `<size>`
   * defaults to a `<fill-char>` of `?`
   * `(make-string <size> <optional-fill-char>)`

1. __Construction Given Characters__: `(string <char1> <char2> ...)`

2. __Unfold__: Form a string by mapping & incrementing seed, until `<break-condition>` is true
   * _Note: **map** via `<map-procedure>`, **increment** via `<successor-procedure>`_
   * `(string-unfold <break-condition> <map-procedure> <successor-procedure> <seed>)`

3. __Unfold Right__: Form a string by mapping right & incrementing seed, until `<break-condition>` is true
   * _Note: **map** via `<map-procedure>`, **increment** via `<successor-procedure>`_
   * `(string-unfold-right <break-condition> <map-procedure> <successor-procedure> <seed>)`

4. __Character Padding Left of String__: pads `<length>` characters, `<character>` defaults to `<space>`
   * `(string-pad <string> <length> <optional-character>)`

5. __Character Padding Right of String__: pads `<length>` characters, `<character>` defaults to `<space>`
   * `(string-pad-right <string> <length> <optional-character>)`

6. __Character Trimming Left of String__: trims characters while `<predicate?>` is true
   * _Note: `<predicate?>` defaults to `char-whitespace?`_
   * `(string-trim <string> <optional-predicate?>)`

7. __Character Trimming Right of String__: trims characters while `<predicate?>` is true
   * _Note: `<predicate?>` defaults to `char-whitespace?`_
   * `(string-trim-right <string> <optional-predicate?>)`

8. __Character Trimming Left & Right of String__: trims characters while `<predicate?>` is true
   * _Note: `<predicate?>` defaults to `char-whitespace?`_
   * `(string-trim-both <string> <optional-predicate?>)`

9. __Replacement__: Replace `<string1>` between indices `<start1>` & `<end1>` with `<string2>`
   * `(string-replace <string1> <string2> <start1> <end1>)`

10. __String Contains Substring__: Get index of 1st instance
    * `(string-contains <string> <sub-string>)`

11. __String Contains Substring__: Get index of last instance
    * `(string-contains-right <string> <sub-string>)`

12. __Join a List of Strings Into 1 String__:
    * `(string-join <string-list> <optional-string-delimiter> <optional-grammar>)`
    * `<optional-grammar> = 'infix | 'suffix | 'prefix`

13. __Split String Into a List of Substrings__:
    * `<string-delimiter>` defaults to `""`
    * `(string-split <string-list> <optional-string-delimiter>)`

14. __Swap String Pointers__: `(string-swap! <string1> <string2>)`

15. __Mutating Push Character to String__: `(string-push! <string> <char>)`

16. __Confirm String is Empty__: `(string-empty? <string>)`

17. __Copy String__: Copy `<source-string>` to `<target-string>` from `<target-start-idx>`
  * `(string-copy! <target-string> <target-start-idx> <source-string>)`


### String Predicates:
0. __String Equality__: 
   * `(string=? <string1> <string2> ...)`
   * `(string<? <string1> <string2> ...)`
   * `(string>? <string1> <string2> ...)`
   * `(string<=? <string1> <string2> ...)`
   * `(string>=? <string1> <string2> ...)`

1. __Case-Insensitive String Equality__: 
   * `(string-ci=? <string1> <string2> ...)`
   * `(string-ci<? <string1> <string2> ...)`
   * `(string-ci>? <string1> <string2> ...)`
   * `(string-ci<=? <string1> <string2> ...)`
   * `(string-ci>=? <string1> <string2> ...)`



------------------------
## List/Pair Procedures:
### Accessors:
0. __Construct Pair__: `(cons <obj1> <obj2>)`

1. __List Access__: `car` = first of pair, `cdr` = second of pair, & composed `car` & `cdr`
   * `(car <pair>)`, `(cdr <pair>)`
   * `(caar <pair>)`, `(cadr <pair>)`, `(cdar <pair>)`, `(cddr <pair>)`
   * `(caaar <pair>)` ... `(cdddr <pair>)`
   * `(caaaar <pair>)` ... `(cddddr <pair>)`

2. __First/Second Setters__: `(set-car! <pair> <obj>)`, `(set-cdr! <pair> <obj>)`

3. __Last Pair In List__: `(last-pair <non-empty-list>)`

4. __Swap 2 Pairs__: `(pair-swap! <pair1> <pair2>)`


### List Constructors:
0. __Construct List (1)__: `(make-list <size> <fill-value>)`

1. __Construct List (2)__: `(list <obj1> <obj2> ...)`

2. __Construct Dotted List__: `(list* <obj1> <obj2> ...)`

3. __Construct Circular List__: `(circular-list <obj1> <obj2> ...)`

4. __Generate Numeric List__: Generate `<count>` objects, from `<start>` & incrementing w/ `<step>`
   * `<optional-start-number>` defaults to `0`
   * `<optional-step-number>` defaults to `1`
   * `(iota <count> <optional-start-number> <optional-step-number>)`

5. __Unfold__: Form a list by mapping & incrementing seed, until `<break-condition>` is true
   * _Note: **map** via `<map-procedure>`, **increment** via `<successor-procedure>`_
   * `(unfold <break-condition> <map-procedure> <successor-procedure> <seed>)`

6. __Unfold Right__: Form a list by mapping right & incrementing seed, until `<break-condition>` is true
   * _Note: **map** via `<map-procedure>`, **increment** via `<successor-procedure>`_
   * `(unfold-right <break-condition> <map-procedure> <successor-procedure> <seed>)`


### List Predicates:
0. __Empty List Predicate__: `(null? <obj>)`

1. __List Predicate__: `(list? <obj>)`

2. __Dotted List Predicate__: `(list*? <obj>)`

3. __Circular List Predicate__: `(circular-list? <obj>)`

4. __Associative List Predicate__: `(alist? <obj>)`


### List Seeking Procedures:
0. __(Lists) Get Sublist Beginning w/ an Object If Present (`#f` Otherwise)__:
   * _Seek using `eq?`_: `(memq <obj> <list>)`
   * _Seek using `eqv?`_: `(memv <obj> <list>)`
   * _Seek using `equal?`_: `(member <obj> <list>)`

1. __(Associative Lists) Get Pair Beginning w/ a Key If Present (`#f` Otherwise)__:
   * _Seek using `eq?`_: `(assq <obj> <list>)`
   * _Seek using `eqv?`_: `(assv <obj> <list>)`
   * _Seek using `equal?`_: `(assoc <obj> <list>)`



------------------------
## Vector Procedures:
0. __Construct Vector (1)__: `(make-vector <size> <fill-value>)`

1. __Construct Vector (2)__: `(vector <obj1> <obj2> ...)`

2. __Mutating Push Object to Vector__: `(vector-push! <vector> <obj>)`

3. __Generate Numeric Vector__: Generate `<count>` objects, from `<start>` & incrementing w/ `<step>`
   * `<optional-start-number>` defaults to `0`
   * `<optional-step-number>` defaults to `1`
   * `(vector-iota <count> <optional-start-number> <optional-step-number>)`

4. __Unfold__: Form a vector by mapping & incrementing seed, until `<break-condition>` is true
   * _Note: **map** via `<map-procedure>`, **increment** via `<successor-procedure>`_
   * `(vector-unfold <break-condition> <map-procedure> <successor-procedure> <seed>)`

5. __Unfold Right__: Form a vector by mapping right & incrementing seed, until `<break-condition>` is true
   * _Note: **map** via `<map-procedure>`, **increment** via `<successor-procedure>`_
   * `(vector-unfold-right <break-condition> <map-procedure> <successor-procedure> <seed>)`

6. __Grow a Vector__: Generate a new vector w/ same elts and new size
   * `(vector-grow <vector> <size>)`

7. __Empty Vector Predicate__: `(vector-empty? <vector>)`

8. __Copy Vector__: Copy `<source-vector>` to `<target-vector>` from `<target-start-idx>`
   * `(vector-copy! <target-vector> <target-start-idx> <source-vector>)`

9. __Swap Vector Pointers__: `(vector-swap! <vector1> <vector2>)`

10. __Vector Binary Search__: `(vector-binary-search <vector> <value> <3-way-comparison>)`
    * _Suppose values a & b:_
      - a < b: `(<3-way-comparison> a b)` < 0
      - a = b: `(<3-way-comparison> a b)` = 0
      - a > b: `(<3-way-comparison> a b)` > 0



------------------------
## Generic Sequence, List|Vector|String, Algorithmic Procedures:
### General:
0. __Generate Empty Variant of Sequence__: `(empty <sequence>)`

1. __Get Sequence Length__: `(length <sequence>)`

2. __Get Sequence Length (`#f` If a Circular List)__: `(length+ <sequence>)`

3. __Get Reverse of Sequence__: `(reverse <sequence>)`

4. __Mutating Reverse Sequence__: `(reverse! <sequence>)`

5. __Fold__: Accumulate sequence from left to right, starting with `<seed>` using `<procedure>`
   * `(fold <procedure> <seed> <sequence1> <sequence2> ...)`

6. __Fold Right__: Accumulate sequence from right to left, starting with `<seed>` using `<procedure>`
   * `(fold-right <procedure> <seed> <sequence1> <sequence2> ...)`

7. __Map__: Apply `<procedure>` to each elt in each sequence, forming a sequence of results
   * `(map <procedure> <sequence1> <sequence2> ...)`

8. __Mutating Map__: Apply `<procedure>` to each elt in each sequence, mapping on the 1st sequence
   * `(map! <procedure> <sequence1> <sequence2> ...)`

9. __Filter__: Form a sequence of elts from `<sequence>` satisfying `<predicate?>`
   * `(filter <predicate?> <sequence>)`

10. __For Each__: Apply `<procedure>` to each elt of each `<sequence>`
    * `(for-each <procedure> <sequence1> <sequence2> ...)`

11. __Copy__: Generate a freshly allocated copy of `<sequence>`
    * `(copy <sequence>)`

12. __Mutating Copy__: Copy `<source-sequence>` to `<dest-sequence>`
    * `(copy! <dest-sequence> <source-sequence>)`

13. __Count Elts With a Property__: `(count <predicate?> <sequence>)`

14. __Get Elt at an Index__: `(ref <sequence> <index>)`

15. __Get Subsequence__: `(slice <sequence> <start-index> <optional-length>)`
    * `<optional-length>` defaults to the end of `<sequence>` if not included!

16. __Set Elt at an Index__: `(set-index! <sequence> <index> <obj>)`

17. __Swap Elts at 2 Indices__: `(swap-indices! <sequence> <index> <index>)`

18. __Fill Sequence__: `(fill! <sequence> <fill-value>)`

19. __Append__: `(append <sequence1> ... <sequenceN> <obj>)`

20. __Remove__: `(remove <predicate?> <sequence>)`

21. __Remove First__: `(remove-first <predicate?> <sequence>)`

22. __Remove Last__: `(remove-last <predicate?> <sequence>)`

23. __Delete an Elt__: `(delete <sequence> <index>)`

24. __Get Last Elt__: `(last <sequence> <index>)`

25. __Get All Except Head__: `(tail <sequence> <index>)`

26. __Get First Elt__: `(head <sequence> <index>)`

27. __Get All Except Last__: `(init <sequence> <index>)`

28. __Compare Elts of Sequences__: `(seq= <predicate?> <sequence1> <sequence2> ...)`

29. __Get 1st Elt After `<predicate?>` is True__: `(skip <predicate?> <sequence>)`

30. __Get Last Elt After `<predicate?>` is True__: `(skip-right <predicate?> <sequence>)`

31. __Get Index of 1st Elt Satisfying `<predicate?>`__: `(index <predicate?> <sequence>)`

32. __Get Index of Last Elt Satisfying `<predicate?>`__: `(index-right <predicate?> <sequence>)`

33. __Drop `<length>` Elts From Left__: `(drop <sequence> <length>)`

34. __Drop `<length>` Elts From Right__: `(drop-right <sequence> <length>)`

35. __Take `<length>` Elts From Left__: `(take <sequence> <length>)`

36. __Take `<length>` Elts From Right__: `(take-right <sequence> <length>)`

37. __Drop Elts While `<predicate?>` From Left__: `(drop-while <predicate?> <sequence>)`

38. __Drop Elts While `<predicate?>` From Right__: `(drop-right-while <predicate?> <sequence>)`

39. __Take Elts While `<predicate?>` From Left__: `(take-while <predicate?> <sequence>)`

40. __Take Elts While `<predicate?>` From Right__: `(take-right-while <predicate?> <sequence>)`

41. __Confirm Any Sequence Satisfies `<predicate?>`__: `(any <predicate?> <sequence1> <sequence2> ...)`

42. __Confirm All Sequences Satisfy `<predicate?>`__: `(every <predicate?> <sequence1> <sequence2> ...)`

43. __Generic `cons`__: `cons` for lists, a copying `push-back` for strings & vectors
    * `(conj <obj> <sequence>)`

44. __Identity__: `(id <obj>)`


### Sorting Procedures:
0. __Sort__: `(sort <predicate?> <sequence>)`

1. __Mutating Sort__: `(sort! <predicate?> <sequence>)`

2. __Confirm Sequence is Sorted__: `(sorted? <predicate?> <sequence>)`

3. __Merge 2 Sequences Sorted With `<predicate?>`__: `(merge <predicate?> <sequence1> <sequence2>)`

4. __Delete Neighboring Duplicates__: `(delete-neighbor-dups <equality-predicate?> <sequence>)`

5. __Mutating Delete Neighboring Duplicates__: `(delete-neighbor-dups! <equality-predicate?> <sequence>)`



------------------------
## Type Predicates:
0. __Generate a Void Object__: `(void)`

1. __Void Predicate__: `(void? <obj>)`

2. __Generate an Undefined Object__: `(undefined)`

3. __Undefined Predicate__: `(undefined? <obj>)`

4. __Empty Sequence Predicate__: `(empty? <obj>)`

5. __Pair Predicate__: `(pair? <obj>)`

6. __Vector Predicate__: `(vector? <obj>)`

7. __Character Predicate__: `(char? <obj>)`

8. __Number Predicate__: `(number? <obj>)`

9. __Real Predicate__: `(real? <obj>)`

10. __Rational Number Predicate__: `(rational? <obj>)`

11. __String Predicate__: `(string? <obj>)`

12. __Symbol Predicate__: `(symbol? <obj>)`

13. __Boolean Predicate__: `(boolean? <obj>)`

14. __Atom Predicate__: `(atom? <obj>)`

15. __Procedure Predicate__: `(procedure? <obj>)`

16. __Cps-Procedure Predicate__: `(cps-procedure? <obj>)`

17. __Input-Port Predicate__: `(input-port? <obj>)`

18. __Output-Port Predicate__: `(output-port? <obj>)`

19. __Eof-Object Predicate__: `(eof-object? <obj>)`

20. __Stream-Pair Predicate__: `(stream-pair? <obj>)`

21. __Empty-Stream Predicate__: `(stream-null? <obj>)`

22. __Stream Predicate__: `(stream? <obj>)`

23. __Syntax-Rules Object Predicate__: `(syntax-rules-object? <obj>)`

24. __Sequence Predicate__: `(seq? <obj>)`



------------------------
## Eval/Apply, Symbol-Append, & Typeof:
0. __Eval__: Run quoted data as code
   * `(eval <data> <optional-environment>)`
   * _Pass `'null-environment` to `eval` in the empty environment!_
   * _Pass `'local-environment` to `eval` in the local environment!_
   * _Pass `'global-environment` to `eval` in the global environment (default)!_

1. __Cps-Eval__: Alternative to `eval` for [`scm->cps`](#Scm-Cps) blocks (evals in CPS)!
   * `(cps-eval <data> <optional-environment> <continuation>)`
   * _Pass `'null-environment` to `cps-eval` in the empty environment!_
   * _Pass `'local-environment` to `cps-eval` in the local environment (default)!_
   * _Pass `'global-environment` to `cps-eval` in the global environment!_

2. __Apply `<procedure>` to List of Args__: `(apply <procedure> <argument-list>)`

3. __Append Symbols__: `(symbol-append <symbol-1> ... <symbol-N>)`

4. __Get Typename Symbol__: `(typeof <obj>)`



------------------------
## Compose & Bind:
0. __Compose N `<procedure>`s__: `(compose <procedure-1> ... <procedure-N>)`
   * _Generates a procedure of N args that applies them to the procedure composition!_

1. __Bind N args to `<procedure>`: `(bind <procedure> <val-1> ... <val-N>)`__
   * _Generates a procedure that when invoked calls the arg-bound `<procedure>`!_
   * _Example: `((bind map even?) '(1 2 3))` is equivalent to `(map even? '(1 2 3))`_


------------------------
## Delay Predicate & Force:
0. __Delay Predicate__: `(delay? <obj>)`

1. __Force a Delayed Expression__: `(force <delayed-expression>)`



------------------------
## Type Coercion:
0. __Char to Integer__: `(char->int <char>)`

1. __Integer to Char__: `<int>` must be in range of [0,255]
   * `(int->char <int>)`

2. __Number to String__: `(number->string <number> <optional-radix> <optional-precision>)`

3. __String to Number__: `(string->number <string> <optional-radix>)`

4. __String to Symbol__: `(string->symbol <string>)`

5. __Symbol to String__: `(symbol->string <symbol>)`

6. __Vector to List__: `(vector->list <vector>)`

7. __List to Vector__: `(list->vector <list>)`

8. __String to Vector__: `(string->vector <string>)`

9. __Vector to String__: `(vector->string <vector>)`

10. __String to List__: `(string->list <string>)`

11. __List to String__: `(list->string <list>)`



------------------------
## Output Procedures:
0. __Pretty-Print (Indents Quoted Data)__: 
   * `(pretty-print <obj> <optional-open-output-port-or-string>)`
   * `(pprint <obj> <optional-open-output-port-or-string>)`

1. __Write (Machine-Readable)__: `(write <obj> <optional-open-output-port-or-string>)`

2. __Display (Human-Readable)__: `(display <obj> <optional-open-output-port-or-string>)`

3. __Newline__: `(newline <optional-open-output-port-or-string>)`

4. __Write-Char__: `(write-char <char> <optional-open-output-port-or-string>)`



------------------------
## Input Procedures:
0. __Read__: Get input as a quoted Datum
   * `(read <optional-open-input-port-or-string>)`

1. __Read Next Expression Into a String__: `(read-string <optional-open-input-port-or-string>)`

2. __Read Next Line of Input Into a String__: `(read-line <optional-open-input-port-or-string>)`

3. __Read Next Character of Input__: `(read-char <optional-open-input-port-or-string>)`

4. __Peek Char__: `(peek-char <optional-open-input-port-or-string>)`

5. __Whether a Character is Ready to be Read__: `(char-ready? <optional-open-input-port-or-string>)`

6. __Slurp Entire Port Contents Into a String__: `(slurp-port <optional-open-input-port-or-string>)`

7. __Slurp Entire File Contents Into a String__: `(slurp-file <filename-string>)`



------------------------
## File & Port Procedures:
0. __File Predicate__: `(file? <filename-string>)`

1. __Delete File__: `(delete-file! <filename-string>)`

2. __Rename File__: `(rename-file! <old-name-string> <new-name-string>)`

3. __Open-Port Predicate__: `(open-port? <port>)`

4. __Closed-Port Predicate__: `(closed-port? <port>)`

5. __Current Input Port__: `(current-input-port)`

6. __Current Output Port__: `(current-output-port)`

7. __Call With Input File__: `(call-with-input-file <filename-string> <unary-port-procedure>)`

8. __Call With Output File__: `(call-with-output-file <filename-string> <unary-port-procedure>)`

9. __With Input From File__: `(with-input-from-file <filename-string> <argless-procedure>)`

10. __With Output From File__: `(with-output-from-file <filename-string> <argless-procedure>)`

11. __Generate Input Port__: `(open-input-file <filename-string>)`

12. __Generate Output Port__: `(open-output-file <filename-string>)`

13. __Close Port__: `(close-port <input-or-output-port>)`



------------------------
## System Interface Procedures:
0. __Load__: `(load <filename-string> <optional-environment>)`
   * _Pass `'null-environment` to `load` in the empty environment!_
   * _Pass `'local-environment` to `load` in the local environment!_
   * _Pass `'global-environment` to `load` in the global environment (default)!_

1. __Cps-Load__: `(cps-load <filename-string> <optional-environment> <continuation-procedure>)`
   * _Alternative to `load` for [`scm->cps`](#Scm-Cps) blocks (converts file to CPS prior loading)!_
   * _Pass `'null-environment` to `cps-load` in the empty environment!_
   * _Pass `'local-environment` to `cps-load` in the local environment (default)!_
   * _Pass `'global-environment` to `cps-load` in the global environment!_

2. __System Interface Via Command-Line__: Returns `#f` if feature not offered by OS
   * `(system <optional-system-call-string>)`

3. __Get-Environment__: Get variable's value as a string
   * `(getenv <variable-name-string>)`

4. __Command-Line Args__: Get a string with command-line arg descriptions
   * `(command-line)`

5. __Current Working Directory__: Get a string of the current working directory
   * `(getcwd)`

6. __Get Parent Directory__: Given a filepath string, get a string of its parent directory
   * `(dirname <filepath-string>)`

7. __Get Heist Interpreter Directory__: Get a string to the Heist-Scheme interpreter's directory
   * `(heist-dirname)`

8. __Compile a File__: `(compile <filename-string> <optional-compiled-filename>)`

9. __Cps-Compile a File__: `(cps-compile <filename-string> <optional-compiled-filename>)`

10. __Get Seconds Since Epoch__: `(seconds-since-epoch)`



------------------------
## Interpreter Invariants Manipulation:
0. __Disable ANSI Escape Codes__: `(set-nansi! <boolean>)`

1. __Enable Case-Insensitivity__: `(set-ci! <boolean>)`

2. __Set Pretty-Print Column Length__: `(set-pprint-column-width! <positive-integer>)`

3. __Set Recursion Depth Limit__: `(set-max-recursion-depth! <positive-integer>)`

4. __Set REPL Prompt__: `(set-repl-prompt! <string>)`

5. __Trace All Calls (Debugging Help)__: `(set-trace-calls! <boolean>)`



------------------------
## Control Flow Procedures:
0. __Exit__: `(exit)`

1. __Trigger Error__: `(error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)`

2. __Trigger Syntax Error__: `(syntax-error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)`

3. __Call With Current Environment__: 
   * `(call/ce <procedure> <arg1> ... <argN>)`
   * `(call-with-current-environment <procedure> <arg1> ... <argN>)`

4. __Inline Call__: "deep" call/ce
   * `(inline <procedure> <arg1> ... <argN>)`

5. __Jump/Throw Value__: `(jump! <optional-arg>)`

6. __Catch Jumped/Thrown Value__: `(catch-jump <proc> <arg1> ... <argN>)`

7. __Expand Macro__: `(expand <quoted-macro-exp>)`

8. __Trace Procedure Call__: `(trace <proc> <arg1> ... <argN>)`



------------------------
## Gensym:
0. __Generate a Unique Symbol__: `(gensym <optional-instance-#-to-reference>)`

1. __Generate a Seeded Symbol__: `(sown-gensym <seed>)`
   * `<seed>` = number | char | symbol | boolean



------------------------
## Scm->Cps Procedures:
0. __Call With Current Continuation__: 
   * `(call/cc <unary-continuation-procedure>)`
   * `(call-with-current-continuation <unary-continuation-procedure>`

1. __Cps->Scm__: Bind `id` as procedure's "topmost" continuation
   * _Note: To pass procs defined **in** a [`scm->cps`](#Scm-Cps) block as an arg to a proc defined **out** of [`scm->cps`](#Scm-Cps)_
   * `(cps->scm <procedure>)`
     - _Hence programs written in and out of [`scm->cps`](#Scm-Cps) blocks may interop!_
     - _BEWARE: primitives are defined **OUT** of a [`scm->cps`](#Scm-Cps) block!_
       - _Hence wrap `cps->scm` around procs being passed to them as args when in a [`scm->cps`](#Scm-Cps) block!_



------------------------
## Syntax Procedures:
0. __Core-Syntax?__: Determine if a symbol was defined by [`core-syntax`](#Core-Syntax)
   * `(core-syntax? <symbol>)`

1. __Runtime-Syntax?__: Determine if a symbol was defined by [`define-syntax`](#Define-Syntax-Let-Syntax-Letrec-Syntax)
   * `(runtime-syntax? <symbol>)`

2. __Reader-Syntax?__: Determine if a symbol was defined by `define-reader-syntax`
   * `(reader-syntax? <string>)`
   * Must be a string to avoid expansion by the reader if **IS** syntax!

3. __Define Reader Shorthand Syntax__: 
   * `(define-reader-syntax <shorthand-string> <optional-longhand-string>)`
   * Have the reader expand `<shorthand-string>` around objects into `<longhand-string>`
     - _Internally, `'` works as if interpreted `(define-reader-syntax "'" "quote")`_
     - _Leaving out `<optional-longhand-string>` rms `<shorthand-string>` reader macro & returns if found_
   ```scheme
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; NOTE: Defn's _MUST_ be evaluated before being usable by the reader!

   ;; Ex 1:
   (define-reader-syntax "%" "display")
   %1 ; OK! Above definition was evaluated prior reading this expression!

   ;; Ex 2:
   ((lambda () (define-reader-syntax "%" "display")))
   %1 ; Also OK! Same reason as Ex 1.

   ;; Ex 3:
   ((lambda () 
      (define-reader-syntax "%" "display")
      %1)) ; ERROR (%1 not defined!): `%1` got read in the same expression as
           ; `(define-reader-syntax "%" "display")` before the definition was evaluated!
           ; >>> Hence `%1` didn't get expanded to `(display 1)` by the reader!
   ```

4. __Mutate Core Syntax__: `(set-core-syntax! <old-name-symbol> <optional-new-name-symbol>)`
   * Only old name: ___DELETES___ `<old-name-symbol>` as core-syntax
   * Both old & new name: ___RENAMES___ syntax's old name to new name
     - _NOTE: also recursively renames all recursive calls to the macro in its templates!_

5. __Mutate Runtime Syntax__: `(set-runtime-syntax! <old-name-symbol> <optional-new-name-symbol>)`
   * Only old name: ___DELETES___ `<old-name-symbol>` as runtime-syntax
   * Both old & new name: ___RENAMES___ syntax's old name to new name
     - _NOTE: also recursively renames all recursive calls to the macro in its templates!_






------------------------ 
# Heist Mathematical Flonum Constants
0. __e__: `fl-e`

1. __1/e__: `fl-1/e`

2. __e^2__: `fl-e-2`

3. __π__: `fl-pi`

4. __1/π__: `fl-1/pi`

5. __2π__: `fl-2pi`

6. __π/2__: `fl-pi/2`

7. __π/4__: `fl-pi/4`

8. __π^2__: `fl-pi-squared`

9. __π/180, Radians Per Degree__: `fl-rad/deg`

10. __180/π, Degrees Per Radian__: `fl-deg/rad`

11. __2/π__: `fl-2/pi`

12. __2/√π__: `fl-2/sqrt-pi`

13. __e^(π/4)__: `fl-e-pi/4`

14. __log2(e)__: `fl-log2-e`

15. __log10(e)__: `fl-log10-e`

16. __loge(2)__: `fl-log-2`

17. __1/loge(2)__: `fl-1/log-2`

18. __loge(3)__: `fl-log-3`

19. __loge(π)__: `fl-log-pi`

20. __loge(10)__: `fl-log-10`

21. __1/loge(10)__: `fl-1/log-10`

22. __√2__: `fl-sqrt-2`

23. __√3__: `fl-sqrt-3`

24. __√5__: `fl-sqrt-5`

25. __√10__: `fl-sqrt-10`

26. __1/√2__: `fl-1/sqrt-2`

27. __2^(1/3)__: `fl-cbrt-2`

28. __3^(1/3)__: `fl-cbrt-3`

29. __2^(1/4)__: `fl-4thrt-2`

30. __φ__: `fl-phi`

31. __loge(φ)__: `fl-log-phi`

32. __1/loge(φ)__: `fl-1/log-phi`

33. __γ (Euler's Constant)__: `fl-euler`

34. __e^γ__: `fl-e-euler`

35. __sin(1)__: `fl-sin-1`

36. __cos(1)__: `fl-cos-1`

37. __Γ(1/2) = √π__: `fl-gamma-1/2`

38. __Γ(1/3)__: `fl-gamma-1/3`

39. __Γ(2/3)__: `fl-gamma-2/3`






------------------------
# Heist Minimalist REPL Example
```scheme
(define (print data) (pretty-print data) (if (not (void? data)) (newline)))
(let loop ((ignore #f)) (loop (print (eval (read)))))
```
