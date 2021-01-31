<!-- Author: Jordan Randleman -:- C++ Heist Scheme Interpreter's README -->

# Heist-Scheme
## Souped-Up R4RS Scheme Interpreter Written in C++!
### Written in as much C++ and as little Scheme as possible for runtime speed!


------------------------
# Using Heist Scheme:
=> _See [`INSTALL.md`](https://github.com/jrandleman/Heist-Scheme/blob/master/INSTALL.md) for step-by-step initial/new-directory installation instructions!_<br>
=> _Tested on OSX & Linux with Clang++ & G++, but **should** work on Windows (adheres C++17 standard)_<br>

0. Compiling the Interpreter: `$ clang++ -std=c++17 -O3 -o heist heist.cpp`
1. REPL: `$ ./heist` (exit REPL via [`(exit)`](#Control-Flow-Procedures) command)
2. Interpret Script: `$ ./heist -script <script-filename> <argv1> <argv2> ...`
3. Compile Script to C++: `$ ./heist -compile <script-filename> <optional-target-name>`
4. Embed Heist in C++: `#include` the [`heist_cpp_interop.hpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/heist_cpp_interop.hpp) header into your code (read it for more details)!
   * See [`embedded_heist_demo.cpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/heist_examples/embedded_heist_demo.cpp) for an example of embedding Heist in action!

------------------------
# Notable Features:
0. [Tail-Call Optimization](#quick-overview)
1. [Unhygienic & Reader Macros](#Heist-Macro-System-Procedures-vs-Macros)
2. [OOP Support](#Defclass)
3. [Multi-Arity Pattern-Matching](#Fn)
4. [Infix-Operator Support](#Infix--Infixr)
5. [First-Class Hash-Maps](#Hash-Map-Procedures)
6. [Opt-In Dynamic Scoping](#control-flow-procedures)
7. [Opt-In Continuations](#Scm-Cps)
8. [Native Even Streams](#Stream-Primitives)
9. [Generic Algorithms](#Generic-Sequence-ListVectorString-Algorithmic-Procedures)
10. [Expanded String Library](#String-Procedures)
11. [String I/O](#Output-Procedures)
12. [Recursive Depth Control](#Interpreter-Invariants-Manipulation)
13. [A](#Curry)[n](#Heist-Mathematical-Flonum-Constants)[d](#Control-Flow-Procedures) [M](#Gensym)[o](#JSON-Interop)[r](#Define-Coroutine)[e](#System-Interface-Procedures)[!](#Syntax-Procedures)

------------------------ 
# Table of Contents
0. [Heist Properties](#Heist-Properties)
   - [Quick Overview](#Quick-Overview)
   - [Conventions](#Conventions)
   - [Metaprogramming Advantages](#Metaprogramming-Advantages)
   - [Notation](#Notation)
   - [Namespacing](#Namespacing)
1. [Heist Command-Line Flags](#Heist-Command-Line-Flags)
   - [`-infix` Operators](#-infix-operators)
2. [Heist Primitive Data Types](#Heist-Primitive-Data-Types)
3. [Heist Numerics](#Heist-Numerics)
   - [4 Number Types](#4-Number-Types)
   - [2 Prefix Types](#2-Prefix-Types)
4. [Heist Macro System, Procedures vs. Macros](#Heist-Macro-System-Procedures-vs-Macros)
5. [Heist Commenting](#Heist-Commenting)
6. [CPS: Continuation Passing Style](#CPS-Continuation-Passing-Style)
7. [Heist Special Forms](#Heist-Special-Forms)
   - [Quote](#Quote), [Quasiquote](#Quasiquote-Unquote--Unquote-Splicing)
   - [Lambda](#Lambda), [Fn](#Fn)
   - [Define](#Define), [Set!](#Set), [Defined?](#Defined), [Defn](#Defn)
   - [Begin](#Begin)
   - [If](#If), [And](#And), [Or](#Or)
   - [Cond](#Cond), [Case](#Case)
   - [Let](#Let), [Let\*](#Let-1), [Letrec](#Letrec)
   - [Do](#Do), [While](#While)
   - [Delay](#Delay), [Scons](#Scons), [Stream](#Stream)
   - [Vector-Literal](#Vector-Literal), [Hmap-Literal](#Hmap-Literal)
   - [Define-Syntax](#Define-Syntax-Let-Syntax-Letrec-Syntax), [Core-Syntax](#Core-Syntax), [Define-Reader-Alias](#Define-Reader-Alias)
   - [Let-Syntax](#Define-Syntax-Let-Syntax-Letrec-Syntax), [Letrec-Syntax](#Define-Syntax-Let-Syntax-Letrec-Syntax)
   - [Syntax-Rules](#Syntax-Rules), [Syntax-Hash](#Syntax-Hash)
   - [Scm->Cps](#Scm-Cps), [Cps-Quote](#Cps-Quote), [Using-Cps?](#Using-Cps)
   - [Curry](#Curry)
   - [Defclass](#Defclass), [New](#New)
   - [Define-Coroutine](#Define-Coroutine)
   - [Define-Overload](#Define-Overload)
   - [Infix!](#Infix--Infixr), [Infixr!](#Infix--Infixr), [Unfix!](#Unfix)
8. [Heist Primitive Variables](#Heist-Primitive-Variables)
9. [Heist Primitive Procedures](#Heist-Primitive-Procedures)
   - [Build System Information](#Build-System-Information)
   - [OOP Reflection Primitives](#OOP-Reflection-Primitives)
     * [Object Primitives](#Object-Primitives)
     * [Prototype Primitives](#Prototype-Primitives)
   - [Coroutine Handling Primitives](#Coroutine-Handling-Primitives)
   - [Stream Primitives](#Stream-Primitives)
   - [Numeric Primitives](#Numeric-Primitives)
     * [General](#General)
     * [Numeric Predicates](#Numeric-Predicates)
     * [Numeric Rounding](#Numeric-Rounding)
     * [Trigonometry Procedures](#Trigonometry-Procedures)
     * [Logical Bitwise Operations](#Logical-Bitwise-Operations)
     * [Complex Number Operations](#Complex-Number-Operations)
   - [Equality Predicates](#Equality-Predicates)
   - [Character Procedures](#Character-Procedures)
     * [General](#General-1)
     * [Eof Character](#Eof-Character)
     * [Character Predicates](#Character-Predicates)
   - [String Procedures](#String-Procedures)
     * [General](#General-2)
     * [String Predicates](#String-Predicates)
     * [Regex](#regex-uses-ecmascript-syntax)
   - [List/Pair Procedures](#ListPair-Procedures)
     * [Accessors](#Accessors)
     * [List Constructors](#List-Constructors)
     * [List Predicates](#List-Predicates)
     * [List Seeking Procedures](#List-Seeking-Procedures)
   - [Vector Procedures](#Vector-Procedures)
   - [Hash-Map Procedures](#Hash-Map-Procedures)
   - [Generic Sequence, List|Vector|String, Algorithmic Procedures](#Generic-Sequence-ListVectorString-Algorithmic-Procedures)
     * [General](#General-3)
     * [Set Procedures](#Set-Procedures)
     * [Sorting Procedures](#Sorting-Procedures)
   - [Type Predicates, Undefined, & Void](#Type-Predicates-Undefined--Void)
   - [Eval/Apply & Symbol-Append](#evalapply--symbol-append)
   - [Typeof & Copying](#typeof--copying)
   - [Compose, Bind, & Id](#compose-bind--id)
   - [Delay Predicate & Force](#Delay-Predicate--Force)
   - [Type Coercion](#Type-Coercion)
   - [Output Procedures](#Output-Procedures)
   - [Formatted Output Procedures](#Formatted-Output-Procedures)
     * [Formatting Stringification & Output](#Formatting-Stringification--Output)
     * [Formatting Guidelines](#Formatting-Guidelines)
     * [Convert Strings to ASCII/Whitespace Art](#Convert-Strings-to-ASCIIWhitespace-Art)
     * [Get ANSI Escape Code String (or `""` if `nansi` is active!)](#get-ansi-escape-code-string-or--if-nansi-is-active)
   - [Input Procedures](#Input-Procedures)
   - [File & Port Procedures](#File--Port-Procedures)
   - [System Interface Procedures](#System-Interface-Procedures)
   - [Interpreter Invariants Manipulation](#Interpreter-Invariants-Manipulation)
   - [Control Flow Procedures](#Control-Flow-Procedures)
   - [Gensym](#Gensym)
   - [Scm->Cps Procedures](#Scm-Cps-Procedures)
   - [Syntax Procedures](#Syntax-Procedures)
   - [Infix Analysis](#Infix-Analysis)
   - [JSON Interop](#JSON-Interop)
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
* `"heist:"` symbol prefix is reserved for internal use!

## Conventions:
* `?` suffix denotes a predicate procedure
* `!` suffix denotes a mutative (non-purely-functional) procedure
* `(`, `[`, & `{` are interchangeable (as are `)`, `]`, & `}`)
  - _Note: `{}` can also force precedence with [infix operators](#Infix--Infixr)!_
* `procedure` is said instead of `function`
* `#it` refers to the REPL's last evaluated expression

## Metaprogramming Advantages:
* Code is data (parentheses construct an Abstract Syntax Tree)
  - Hence Macro System enables direct manipulation of the AST
  - Quotation ([`quote`](#Quote)) Converts Code to Data, Eval ([`eval`](#evalapply--symbol-append)) Converts Data to Code
  - Reader ([`read`](#Input-Procedures)) takes input and parses it into a quoted list of symbolic data
    * Hence [`read`](#Input-Procedures) and [`eval`](#evalapply--symbol-append) may be combined for a custom repl!

## Notation:
* Function (or "procedure") calls are denoted by parens:
  - in C++: `myFunc(0,'a',"hello")`
  - in Heist Scheme: `(myFunc 0 #\a "hello")`
* _Nearly_ every character (except `.`) can be used in a variable name!
  - Unless, of course, the combination could be interpreted as a<br>
    primitive data type (ie `1000` is an invalid variable name)
  - Hence can do things like name a factorial function `!` as if it were a primitive!
  - This excludes `.` though, given it denotes property access for [objects](#Defclass)

## Namespacing:
* Lisp 1: variables & procedures share a single namespace
* [`core-syntax`](#Core-Syntax) is evaluated first & ___MUST___ be matched (unlike runtime macros from [`define-syntax`](#Define-Syntax-Let-Syntax-Letrec-Syntax))
* Runtime macros & variables are in different namespaces
  - Hence if a [runtime macro's](#Define-Syntax-Let-Syntax-Letrec-Syntax) pattern doesn't match, it gets treated as an attempted procedure call




------------------------
# Heist Command-Line Flags
0. Interpret Script: `-script <script-filename> <argv1> <argv2> ...`
1. Compile Script: `-compile <script-filename> <optional-compiled-filename>`
2. Load Script: `-l <script-filename>`
3. Infix Operators: `-infix`
4. With CPS Evaluation: `-cps`
5. Disable ANSI Colors: `-nansi`
6. Case Insensitivity: `-ci`
7. Dynamic Call Trace: `-dynamic-call-trace`
8. Trace Call Args: `-trace-args`
9. Stack Trace Size: `-trace-limit <non-negative-integer>`
10. Interpreter Version: `--version`
11. Show These Options: `--help`

### `-infix` Operators
| Order |                       Operators                      | Assoc |                Effects                 |
| :---: | :--------------------------------------------------- | :---: | :------------------------------------- |
|   10  | `:`                                                  | Right | functional composition                 |
|    9  | `**`                                                 | Right | expt                                   |
|    8  | `*` `/` `%` `//` `mod`                               | Left  | *, /, remainder, quotient, modulo      |
|    7  | `+` `-`                                              | Left  | addition, subtraction                  |
|    6  | `::` `@`                                             | Right | cons, append                           |
|    5  | `>` `<` `>=` `<=`                                    | Left  | gt, lt, gte, lte                       |
|    4  | `==` `!=`                                            | Left  | eq, neq                                |
|    3  | `&&`                                                 | Left  | and                                    |
|    2  | `\|\|`                                               | Left  | or                                     |
|    1  | `->`                                                 | Left  | lambda                                 |
|    0  | `=` `<-` `**=` `*=` `/=` `%=` `//=` `mod=` `+=` `-=` | Right | define, set!, set! ** * / % // mod + - |




------------------------
# Heist Primitive Data Types
0. Symbol (quoted syntax label, `'hello`)
1. Number ([see numerics section](#Heist-Numerics))
2. Pair ([quoted](#Quote) expression `'(1 2 3)`, [list](#ListPair-Procedures) `(list 1 2 3)`, or [cons](#ListPair-Procedures) `(cons 1 (cons 2 (cons 3 '())))`)
3. String (wrapped by `""`, `"hello"`)
4. Char (have the `#\` prefix, `#\h #\e #\l #\l #\o`) (uses `ascii` encoding!)
   * Also Supports Named Chars and Hex Chars:
     - `#\space`, `#\tab`, `#\newline`, `#\vtab`, `#\page`, `#\return`
     - `#\alarm`, `#\backspace`, `#\nul`, `#\esc`, `#\delete`
     - `#\x0` -> `#\xff`
5. Boolean (true or false, `#t` or`#f`)
6. Vector (quoted literal `'#(1 2 3)`, or primitive `(vector 1 2 3)`)
7. Hash-Map (quoted literal `'$(a 1 b 2)`, or primitive `(hmap 'a 1 'b 2)`)
8. Input Port, Output Port ([see port primitives](#File--Port-Procedures))
9. Syntax-Rules Object (see [`syntax-rules`](#Define-Syntax-Let-Syntax-Letrec-Syntax) special form)
10. Delayed Data (see [`delay`](#Delay) special form)
11. Procedure (via primitives or the [`lambda`](#Lambda)/[`fn`](#Fn) special forms)
12. Object (see [`defclass`](#Defclass))
13. Class-Prototype (see [`defclass`](#Defclass))
14. Void Datum [`(void)`](#Type-Predicates-Undefined--Void)
15. Undefined Datum [`(undefined)`](#Type-Predicates-Undefined--Void)






------------------------
# Heist Numerics
### 4 Number Types:
0. Exact/Ratnum (rational number)
   * Arbitrary precision numerator & denominator (automatically reduced to simplest form!)
   * _Special Case_: denominator of `1` creates a BigInt
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
3. Complex Numbers:
   * Both the real and imaginary components will match in exactness
   * Supports `+inf.0` or `-inf.0` components (_`+nan.0` is unique & never complex!_)
   * _Special Case_: imaginary value of `0` becomes a real (non-complex) number!
     ```scheme
     3/4+1/2i
     3/4+0.5i ; becomes 0.75+0.5i to match exactness
     -i       ; valid complex number!
     -44+0i   ; becomes -44
     ```


### 2 Prefix Types:
0. Radix:
   - Binary: `#b`, Octal: `#o`, Hexadecimal: `#x`, Decimal: `#d` (enabled by default)
   - ***N***ary 2-36: `#2r`-`#36r`
     ```scheme
     #b-101    ; -5
     #b10/11   ; 2/3
     #b1010.11 ; 10.75
     #o77      ; 63
     #xC0DE    ; 49374
     #xc0de    ; 49374

     #30rHeistScheme ; 10326335991592274
     #2r-101/10      ; -5/2
     ```
1. Exactness:
   - Inexact: `#i`, Exact: `#e`
     ```scheme
     #i3   ; 3.0
     #i1/2 ; 0.5
     #e3.5 ; 7/2
     #e1.0 ; 1

     #e#b101.1 ; Exact & Binary! => 11/2
     #i#2r101  ; Inexact & Binary! => 5.0
     ```






------------------------
# Heist Macro System, Procedures vs. Macros
One of Scheme's most powerful features is its flexible run-time macro system!<br>

For those in the know: 
* While R5RS+ Scheme supports hygienic macros, R4RS (Heist Scheme's base) makes this _optional_.<br>
* Unhygienic macros were selected after experimenting with CL, Clojure, & Scheme, finding:<br>
  1. Hygiene's pros are easier to emulate w/o it than non-hygiene's pros are to emulate with hygiene
  2. Forsaking hygiene enables more extensive control when meta-programming

Macros are identical to procedures, except for 3 key features:<br>

0. They ___expand___ into new code that will be run in the current scope, rather than<br> 
   processing a computation in a new scope (that of their definition, as with procedures)
   - Built-in [`syntax-hash`](#Syntax-Hash) mechanism makes avoiding namespace conflicts trivial!
   - Macro argument names are automatically hashed to become unique symbols!
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
1. [`eval`](#evalapply--symbol-append) alternative in [`scm->cps`](#scm-cps) blocks: [`cps-eval`](#evalapply--symbol-append)
2. [`compile`](#system-interface-procedures) alternative in [`scm->cps`](#scm-cps) blocks: [`cps-compile`](#system-interface-procedures)
3. Bind [`id`](#compose-bind--id) as the continuation of a procedure: [`cps->scm`](#scm-cps-procedures)
   * For passing a procedure defined in a [`scm->cps`](#scm-cps) block as an argument to a procedure<br>
     __not__ defined in a [`scm->cps`](#scm-cps) block (determine definition context via [`cps-procedure?`](#Type-Predicates-Undefined--Void))
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


## Quote:

#### Shorthand: `'<obj>` => `(quote <obj>)`

#### Use: ___Convert Code to Data!___

#### Quoting a Datum:
* Proper List: `(quote (<obj1> <obj2> ...))` => `(list '<obj1> '<obj2> (quote ...))`
* [Dotted](#Interpreter-Invariants-Manipulation) List: `(quote (<obj1> ... <objN> . <objN+1>))` => `(append '(<obj1> ... <objN>) <objN+1>)`
* Empty List: `(quote ())` => `'()` _(unique value, ONLY one returning `#t` for [`null?`](#list-predicates) primitive!)_
* Vector: `(quote #(<obj1> <obj2> ...))` => `(vector '<obj1> '<obj2> (quote ...))`
* Hash-Map: `(quote $(<key> <val> ...))` => `(hmap '<key> '<val> (quote ...))`
* Syntax: `(quote <syntax>)` => `<syntax-as-symbol>`
* Else: `(quote <any-other-obj>)` => `<any-other-obj>`

#### Examples:
```scheme
'12             ; => 12
'hello          ; => hello
'(1 2 3)        ; => (list 1 2 3)
'#(hello there) ; => (vector 'hello 'there)
'$(a 1 b 2)     ; => (hmap 'a 1 'b 2)
''double        ; => (quote (quote double)) => (list 'quote 'double)
'(define a 12)  ; => (list 'define 'a '12) ; quoted code becomes a list of data!
```


------------------------
## Quasiquote, Unquote, & Unquote-Splicing:

#### Shorthands: 
0. ``` `<obj> => (quasiquote <obj>)```
1. ``` ,<obj> => (unquote <obj>)```
2. ``` ,@<obj> => (unquote-splicing <obj>)```

#### Use: ___Selectivly Eval & Convert Code to Data!___

#### Quoting a Datum (exactly like [`quote`](#quote), with 2 key exceptions):
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
* _Note: Pass a variadic number of args (0+) by using [`.`](#Interpreter-Invariants-Manipulation) as such:_
  - _Note: Variadic arg-list name must **always** be the last arg!_
    ```scheme
    (lambda (. va-args-list) <body> ...)       ; OK
    (lambda (a b . va-args-list) <body> ...)   ; OK
    (lambda (a b . va-args-list c) <body> ...) ; ERROR: Variadic Arg Name Isn't Last!
    ```
* _Note: Assign default values to arguments by using `()`:_
  - _Note: Mandatory parameters must precede optional ones!_
    ```scheme
    (lambda (a (b 1) (c 2)) <body> ...)          ; OK, b & c have default values!
    (lambda (a (b 1) . va-args-list) <body> ...) ; OK, has both optionals & variadics!
    (lambda ((b 1) a . va-args-list) <body> ...) ; ERROR: a MUST precede optional b!
    (lambda (a b . (va-args-list 1)) <body> ...) ; ERROR: variadics CAN'T have defaults!
    ```

#### Reader Shorthand: `\<expr>`
* Use `%n` to refer to the `n`th argument (1-indexed so `%1` is the 1st arg)
* Use `%%` to refer to a variadic arg (hence [`list`](#List-Constructors) is equivalent to `\%%`)


------------------------
## Fn:

#### Use: ___Generates Anonymous Multi-Arity Pattern-Matching Procedure!___

#### Form: `(fn ((<arg> ...) <body> ...) ...)`
* _Note: Pass a variadic number of args (0+) by using [`.`](#Interpreter-Invariants-Manipulation) (like [`lambda`](#Lambda)!)_
* _Note: Pattern-match against containers by using literal syntax!_
  * _Like [`syntax-rules`](#syntax-rules), write more restrictive patterns first!_
  * _Match against symbol literals by using [`quote`](#quote)!_

#### Examples:
```scheme
(define list-map
  (fn ((f ()) '()) ; match against nil
      ((f (x . xs)) (cons (f x) (list-map f xs))))) ; match & unpack pair

(define factorial
  (fn ((n) (factorial n 1))
      ((0 p) p) ; 0 is more restrictive than 'n', so place 1st!
      ((n p) (factorial (- n 1) (* n p)))))
```


------------------------
## Define:

#### Use: ___Bind a Syntactic Label to a Value!___

#### Forms:
```scheme
;; Define a Variable
(define <var-name> <value>)

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

#### Form: `(set! <var-name> <new-value>)`

#### Special Case: becomes `(obj.set-name! <new-value>)` if `<var-name>` = `<obj.name>`


------------------------
## Defined?:

#### Use: ___Determine if a Symbol is [`define`](#define)d!___
* Given an [object](#defclass) property-access symbol, returns whether the property exists!
* Use [`runtime-syntax?`](#Syntax-Procedures), [`core-syntax?`](#Syntax-Procedures), & [`reader-syntax?`](#Syntax-Procedures) to check for macros!
* WARNING: This is _NOT_ the inverse of the [`undefined?`](#Type-Predicates-Undefined--Void) primitive!
  - [`undefined?`](#Type-Predicates-Undefined--Void) checks values, `defined?` checks the environment!

#### Form: `(defined? <symbol>)`

#### Example:
```scheme

(defined? a)   ; #f ; `a` was never registered in the environment!
(undefined? a) ; ERROR: `undefined?` operates on values, and a has none in the environment!

(define a 12)
(defined? a) ; #t
(set! a (undefined))

(defined? a)   ; #t ; "(undefined)" is a valid value type assigned to `a` in the environment!
(undefined? a) ; #t ; `undefined?` checks values!

```


------------------------
## Defn:

#### Use: ___Macro Combining [`define`](#define) & [`fn`](#fn)!___

#### Form: `(defn <procedure-name> ((<arg> ...) <body> ...) ...)`


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

(let ((or-result <exp1>)) ; Bind result to prevent 2x eval from condition & result
  (if or-result
      or-result
      (let ((or-result <exp2>))
        (if or-result
            or-result
            (let ((or-result <exp3>))
              (if or-result
                  or-result
                  <exp4>))))))
```


------------------------
## Cond:

#### Use: ___Concise If-Else Chains!___
* _Note: `cond` is actually a macro directly defined **in** Heist Scheme!_

#### Form: `(cond <clause1> <clause2> ...)`, `<clause>` = `(<condition> <exp1> <exp2> ...)`
* _Using `else` as the condition of the last clause is equivalent to using `#t` as the condition_
* _Use `=>` to apply the result of the condition to a callable_

#### Derivation Using [`if`](#if):
```scheme
(cond (<condition1> <exp1> ...)
      (<condition2> <exp2> ...)
      (<condition3> => <callable>)
      (else <exp4> ...))

;; Becomes =>

(if <condition1>
    (begin <exp1> ...)
    (if <condition2> 
        (begin <exp2> ...)
        (let ((cond-result <condition3>))
          (if cond-result
              (<callable> cond-result)
              (begin <exp4> ...)))))
```


------------------------
## Case:

#### Use: ___Switch-Statement Equivalent!___
* _Note: `case` is actually a macro directly defined **in** Heist Scheme!_

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
  ((<val4> ...) => <callable>)
  (else <exp3> ...))

;; Becomes =>

(cond ((memv <key> (list <val1> ...)) <exp1> ...) ; See the <memv> primitive!
      ((memv <key> (list <val2> <key> <val3> ...)) <exp2> ...)
      ((memv <key> (list <val4> ...)) => <callable>)
      (else <exp3> ...))
```


------------------------
## Let:

#### Use: ___Temporary Bindings in a New Scope!___
* _Note: `let` is actually a macro directly defined **in** Heist Scheme!_

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
* _Note: `let*` is actually a macro directly defined **in** Heist Scheme!_

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
* _Note: `letrec` is actually a macro directly defined **in** Heist Scheme!_

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

#### Use: ___Recursive Iteration Construct!___
* _Note: `do` is actually a macro directly defined **in** Heist Scheme!_

#### Form:
```scheme
(do ((<var> <initial-val> <update>) ...)
  (<break-test> <return-exp1> <return-exp2> ...) ; returns are optional (<void> by default)!
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
## While:

#### Use: ___True Iteration Construct!___
* _Warning: degrades to [`do`](#Do) in [cps contexts](#Scm-Cps)!_
* _Use `*condition*` as an alias for the current condition!_
* _Uses a true C++ `while` under the hood (no recursion overhead)!_

#### Form:
```scheme
(while (<test> <return-exp1> <return-exp2> ...) ; returns are optional (<void> by default)!
  <body> ...)
```

#### Examples:
```scheme
(define x 0)
(while ((< x 10))
  (set! x (+ x 1))
  (if (= x 5)
      (set! *condition* #f))) ; set condition to false w/o modifying "x"!
(display x) ; 5

(define x 0)
(while ((< x 10)) (set! x (+ x 1)))
(display x) ; 10 (as expected)
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

(let ((already-run? #f) (result #f)) ; Memoized promises!
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
* _Note: `scons` is actually a macro directly defined **in** Heist Scheme!_

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
* _Note: `stream` is actually a macro directly defined **in** Heist Scheme!_

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
## Hmap-Literal:

#### Use: ___Longhand Variant of the `$` Hashmap-Literal Shorthand!___
* _Hence, like `$`, `hmap-literal` **must** be quoted to form a hash-map object!_
* _Keys ::= `symbol` | `string` | `number` | `character` | `boolean`_

#### Form: `'(hmap-literal <key1> <val1> <key2> <val2> ...)`

#### Transformation:
```scheme
'$(<key1> <val1> <key2> <val2> ...)
;; Becomes =>
'(hmap-literal <key1> <val1> <key2> <val2> ...)
;; Becomes =>
(hmap '<key1> '<val1> '<key2> '<val2> '...)
```


------------------------
## Define-Syntax, Let-Syntax, Letrec-Syntax:

#### Use: ___Create a Run-Time Macro (Bind a Label to a Syntax Object)!___
* _Note: create a `<syntax-object>` via the [`syntax-rules`](#syntax-rules) special form below!_
* _Note: Run-Time macros are expanded **at run-time**, ie each time they're invoked!_
  - _See [`core-syntax`](#core-syntax) for an **analysis-time** macro alternative!_
* _Note: `let-syntax` & `letrec-syntax` are actually macros directly defined **in** Heist Scheme!_

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
  - _Note: `...` and `syntax-hash` are **always** reserved `<key>` names!_

#### Variadic Matching & Expansion:
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

#### Higher-Order Macro Template Expansion Support:
* _Writing macros that expand to other macro definitions using `...` can cause_<br>
  _issues, **however** this can be mediated by escaping nested `...` via `\...`_
* Example:
  ```scheme 
  (core-syntax define-inlined
    (syntax-rules ()
      ((_ (name) b ...)
        (core-syntax name
          (syntax-rules ()
            ((_)
              ((lambda ()
                b ...))))))
      ((_ (name a ...) b ...)
        (core-syntax name
          (syntax-rules ()
            ((_ arg \...) ; escape the ... to be un-escaped upon expansion
              ((lambda (a ...)
                b ...) arg \...)))))))
  ```


------------------------
## Syntax-Hash:

#### Use: ___Hash Local Macro Template Identifiers to Avoid Expansion Name Conflicts!___

#### Form: `(syntax-hash <symbol>)`
* _**ONLY** valid in `syntax-rules` templates!_
* _Expander replaces `syntax-hash` expression, and every instance of `<symbol>`,<br>
  with a hashed version of `<symbol>` unique to the expansion instance!_
  - _Similar to [`gensym`](#Gensym) but specialized for macro expansions!_

#### Shorthand: ``` `@<symbol> => (syntax-hash <symbol>)```

#### Example:
```scheme

  ;; Note the name conflict in the following:
  ;;   The <a> gets expanded to <b>, but the expansion then reads that <b> as 10
  ;;     due to the rebinding of <b> by <let>, thus the result is 20 and not 15

  (define-syntax my-macro
    (syntax-rules ()
      ((_ a) 
        (let ((b 10))
          (+ a b))))) ; expands to (+ b b) => (+ 10 10)

  (define b 5)
  (write (my-macro b)) ; 20



  ;; We can resolve this by binding our <b> in the macro to a UNIQUE identifier.
  ;;   We COULD solve this using <gensym>:

  (define-syntax my-macro
   (syntax-rules ()
     ((_ a) 
       (eval 
         `(let ((,(gensym) 10))     ; form the expression by splicing in a unique symbol,
             (+ a ,(gensym 1))))))) ; then evaluate the expression in the local environment

  (define b 5)
  (write (my-macro b)) ; 15



  ;; HOWEVER, this is a tad verbose for our purposes. Enter <syntax-hash>:
  ;;   a FAST alternative to <gensym> specialized ONLY for macro expansions!
  ;; => NOTE: we can use the "`@" reader macro to be even more concise!

  (define-syntax my-macro
    (syntax-rules ()
      ((_ a) 
        (let ((`@b 10)) ; `@b => (syntax-hash b) & binds <hashed-b> to 10
          (+ a b)))))   ; expands to (+ b <hashed-b>) => (+ 5 10)

  (define b 5)
  (write (my-macro b)) ; 15
```


------------------------
## Core-Syntax:

#### Use: ___Construct an Analysis-Time Macro in the GLOBAL Scope!___

#### Form: `(core-syntax <label> <syntax-object>)`

#### Analysis-Time Advantanges:
* Interpreter's [`eval`](#evalapply--symbol-append) seperates expression analysis (declaration) & execution (invocation):
  - [`define-syntax`](#Define-Syntax-Let-Syntax-Letrec-Syntax) macros, bound to an environment, dynamically expand at **run-time**
    * _Hence **run-time** macros in a [`lambda`](#lambda) body are re-expanded **upon every invocation!**_
  - [`core-syntax`](#core-syntax) macros, only bound to the **global environment**, expand at **analysis-time**
    * _Hence **analysis-time** macros in a [`lambda`](#lambda) body expand **in the [`lambda`](#lambda) declaration only once!**_

#### Example Runtime Expansion Degradation Risk:
* ***BEST PRACTICE***: _use `core-syntax` in the **GLOBAL SCOPE** to avoid the below!_
* Heist reads, analyzes, and runs each expression individually
* Hence reading `(define (f) ...)` below means the entire expr is analyzed at once,<br>
  **but** the `my-macro` core-syntax defn is only registered at run-time!
  - Hence `(my-macro 12)` is analyzed **before** `my-macro` is defn'd as core-syntax!
    * Thus `(my-macro 12)` _must_ be expanded at ***run-time*** instead of ***analysis-time***!
  - However `(my-macro 13)` **does** expand at analysis-time, since `(f)` triggered<br>
    `my-macro` to be bound as core-syntax prior analyzing the `(define (g) ...)` expr!
```scheme
  (define (f)
    (core-syntax my-macro    ; BINDS my-macro TO THE GLOBAL ENV AS CORE-SYNTAX AT RUNTIME
      (syntax-rules ()
        ((_ a) (* a 2))))
    (my-macro 12))           ; EXPANDS AT RUNTIME SINCE my-macro ISN'T CORE-SYNTAX YET!
  (f)                        ; RUN f TO REGISTER my-macro AS core-syntax IN THE GLOBAL ENV
  (define (g) (my-macro 13)) ; EXPANDS AT ANALYSIS TIME
```


------------------------
## Define-Reader-Alias:

#### Use: ___Define a Symbolic Alias to be Replaced by the Reader!___
* _Check for aliases via the [`reader-alias?`](#Syntax-Procedures) primitive!_
* _Get all current aliases via the [`reader-alias-list`](#Syntax-Procedures) primitive!_

#### Forms:
* `(define-reader-alias <alias-symbol> <name-symbol>)`
* `(define-reader-alias <alias-symbol-to-delete>)`

#### Warning: ___Reader Aliases do NOT Recursively Expand!___
```scheme
(define-reader-alias a b)
(define-reader-alias b +)
(b 1 2 3) ; 6
(a 1 2 3) ; ERROR: VARIABLE b IS UNBOUND !!!
```


------------------------
## Scm->Cps:

#### Use: ___Convert Code to CPS & Evaluate the Result!___
* _Hence returns an unary procedure, accepting the "topmost" continuation!_
* _Enables use of [`call/cc`](#scm-cps-procedures), [`cps-eval`](#evalapply--symbol-append), [`cps-load`](#system-interface-procedures), & [`cps->scm`](#scm-cps-procedures) primitives!_
* _Automatically wraps entire program (& passed [`id`](#compose-bind--id)) if [`-cps`](#Heist-Command-Line-Flags) cmd-line flag used!_
* _Enables opt-in continuations for their benefits w/o their overhead when unused!_
  - _Optimizes the cps transformation as well for reasonable speed!_
  - _In general, `scm->cps` code at `-O3` optimization runs as fast as its non-cps version would at `-O0`_

#### Form: `(scm->cps <exp1> <exp2> ...)`

#### Danger Zone:
* With CPS, avoid [macros](#define-syntax-let-syntax-letrec-syntax)/[eval](#evalapply--symbol-append) expanding to a [`define`](#define) in the current envrionment!
  - Lazy expansion breaks this functionality (may expand to localized bindings though!)
  - Includes [`defn`](#defn) & [`define-overload`](#Define-Overload) (manually write expansion)

#### Author's Advice:
* Experimentally, go wild! 
* For practical code, leave `scm->cps` to be used by libraries, & prefer specialized solutions<br>
  rather than homebrewed alternatives. 
  - _I.E. use [`define-coroutine`](#Define-Coroutine) and the [`jump!`](#Control-Flow-Procedures)/[`catch-jump`](#Control-Flow-Procedures) idiom rather than spinning_<br>
    _up your own versions via continuations._

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
            (display (integer->char (+ i 65))) ; print char
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
## Cps-Quote:

#### Use: ___Convert Code to Data in CPS!___
* _Identical to [`quote`](#quote) after transforming given code into CPS!_

#### Form: `(cps-quote <exp>)`

#### Note on Application Transformations:
* Applications may have a peculiar tag in front of them, with ___non-cps___ arguments<br>
  - _This is b/c Heist only knows if an application is a macro or callable at run-time!_
  - _For callables, cps-transformation precedes execution!_
  - _For macros, execution precedes cps-transformation (& re-execution)!_


------------------------
## Using-Cps?:

#### Use: ___Determine Whether in a [`scm->cps`](#Scm-Cps) Block or [`-cps`](#Heist-Command-Line-Flags) is Active!___

#### Form: `(using-cps?)`


------------------------
## Curry:

#### Use: ___Define Curriable Lambdas with a Nicer Interface!___
* _Note: `curry` is actually a macro directly defined **in** Heist Scheme!_
* _Enables trivial means to bind arguments to values (especially helps w/ lambda calculus)_

#### Form: `(curry (<arg1> <arg2> ...) <body> ...)`
* _Note: it is undefined behavior to have a variadic `curry` lambda using [`.`](#Interpreter-Invariants-Manipulation)!_

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
## Defclass:

#### Use: ___Define Class Prototypes for Object-Oriented Programming!___
* _`Defclass` creates a class prototype (think JavaScript) from which Objects are made!_

#### Form:
```
(defclass <class-name> (<optional-inherited-prototype>) <member-or-method-instances>)
=> <member-or-method-instance> ::= (<member-name> <default-value>)
                                 | ((<method-name> <arg1> <arg2> ...) <body> ...)
                                 | (defmethod <method-name> <procedure-value>)
                                 |
                                 | ((make-<class-name> <arg> ...) <body> ...) ; constructor
                                 | (make-<class-name> ((<arg> ...) <body> ...) ...) ; fn ctor
                                 |
                                 | ((eq? <obj>) <body> ...)    ; overload eq?
                                 | ((eqv? <obj>) <body> ...)   ; overload eqv?
                                 | ((equal? <obj>) <body> ...) ; overload equal?
                                 | ((self= <obj>) <body> ...)  ; overload all the above
                                 |
                                 | ((write) <body> ...)        ; overload write
                                 | ((display) <body> ...)      ; overload display
                                 | ((pprint) <body> ...)       ; overload pretty-print
                                 | ((self->string) <body> ...) ; overload all the above
                                 |
                                 | ((self->copy) <body> ...)   ; overload copy
                                 |
                                 | ((self->procedure <arg> ...) <body> ...) ; overload application
```

#### Constructor:
0. User-defined `make-<class-name>` ctor is optional, if undefined will be generated
   - Generated ctor is either nullary, or accepts a container to initialize member values:
     * container = name-value [`hash-map`](#Hash-Map-Procedures), or value [`list`](#ListPair-Procedures)/[`vector`](#Vector-Procedures)!
   - Default ctor is always available via `new-<class-name>`
1. Default values from class-prototypes are [`deep-copied`](#typeof--copying) to objects upon construction
2. [Dynamically add properties to prototypes](#Prototype-Primitives), which existing objects get access to!

#### Generated Predicate, Setters, & Property Registration:
0. Class Object Predicate `(<class-name>? <obj>)` is generated by default
1. Object member-setter methods are generated by default:
   - IE if there's a member named `value`, a method called `set-value!` is generated
   - This method is automatically invoked when using `set!` on an object member!
     * `(set! obj.name val)` => `(obj.set-name! val)`
2. Object dynamic member/method registration methods are generated by default:
   - `(<object>.add-member! <member-name-symbol> <default-value>)`
   - `(<object>.add-method! <method-name-symbol> <procedure-value>)`
   - If member/method exists: sets value, else: adds it as a new property

#### Self, Prototype, & Inherited Object Access:
0. `self` refers to the current invoking object (designed for use in methods)
1. `.prototype` member returns the class prototype of the object
2. `.super` member returns object's underlying inherited object (returns `#f` if dne)

#### Overload Equality, Printing, & Copying:
0. Equality: `self=` method will attempt to be invoked on objects for `eq?`, `eqv?`, `equal?`
   - Method should accept 1 argument to compare equality against!
   - May also have specific equality polymorphism by naming methods `eq?`, `eqv?`, `equal?` directly
1. Printing: `self->string` method will attempt to be invoked on objects for `display`, `write`, `pprint`
   - Method should accept 0 arguments, and return a string to be "displayed"!
   - May also have specific printing polymorphism by naming methods `display`, `write`, `pprint` directly
2. Copying: `self->copy` method will attempt to be invoked on objects for `copy`
   - Method should accept 0 arguments, and _by convention_ return a new object!
   - Unlike the above methods, `self->copy` is _NOT_ inherited by default!

#### Overload Application via Functors:
0. The `self->procedure` method will automatically be called on any object applied as a procedure!
   - Think `operator()()` in C++!
   - Check out the primitive [`functor?`](#Type-Predicates-Undefined--Void) predicate!
   - Convert functors to procedures via the [`functor->procedure`](#Type-Coercion) primitive!

#### Method Access to Object Members:
0. Similar to C++'s `this`, `self` is implicitly passed as a method argument upon invocation
1. Unlike C++, object members ___must___ be referenced via `self.<member>` in methods
   - Enables methods to also reference external variables with members' names

#### Value Semantics & Property Access:
0. Passed by reference (as are [strings](#String-Procedures), [pairs](#ListPair-Procedures), [vectors](#Vector-Procedures), and [hash-maps](#Hash-Map-Procedures))
   - May be deep-copied via [`copy`](#typeof--copying) & shallow-copied via [`shallow-copy`](#typeof--copying)
1. Traditional OOP Access, Member: `person.name`, Method: `(person.greet <friend's name>)`
   - Functional [`..`](#Object-Primitives) Access: `(.. person 'name)` & `((.. person 'greet) <friend's name>)`
   - [Reader](#Input-Procedures) evals property chains as 1 symbol, which are parsed by the core evaluator!

#### Example:
```scheme
(defclass node ()
  (left '())
  (right '())
  (val 0)
  ((leaf?)
    (and (null? self.left) (null? self.right))))

(define root (make-node))
(set! root.left (make-node))
(set! root.left.val 42)

(display root.val) ; 0
(newline)
(display root.left.val) ; 42
(newline)
(display (root.leaf?)) ; #f
(newline)
(display (root.left.leaf?)) ; #t
```


------------------------
## New:

#### Use: ___Create Anonymous Objects!___
* _Overloads [`equal?`](#Equality-Predicates) for structural equality against other anonymous objects!_
* _Note: `new` is actually a macro directly defined **in** Heist Scheme!_

#### Form: `(new (<property-name> <property-value>) ...)`


------------------------
## Define-Coroutine:

#### Use: ___Define Coroutine-Object Generators!___
* _Note: `define-coroutine` is actually a macro directly defined **in** Heist Scheme!_

#### Form: `(define-coroutine (<co-name> <arg> ...) <body> ...)`

#### Use: Initial invocation `(<co-name>)` will yield a `coroutine` object!
* Re-invoking `(<co-name>)` will return a new `coroutine` object instance!
* Hence `<co-name>` should ___not___ be called recursively internally, rather use<br>
  the [named-let](#let) construct in order to perform recursive operations!

#### Coroutine Objects:
* Creation: Either from invoking `(<co-name>)` or `yield` in a coroutine
* 2 Properties, `.value` __member__ & `.next` __method__:
  - `.value`: yielded value (`#f` if object isn't from a `yield`)
  - `.next`: either starts or continues the coroutine's execution

#### Associated Special Form:
* `(yield <value>)`: yield a value from the coroutine via a new coroutine object!
  - `(yield)` is equivalent to `(yield #f)`, designed for use with [`cycle-coroutines!`](#Coroutine-Handling-Primitives)

#### Special Conditions:
0. Use [`co-eval`](#Coroutine-Handling-Primitives) instead of [`eval`](#evalapply--symbol-append) in coroutines
1. Use [`co-load`](#Coroutine-Handling-Primitives) instead of [`load`](#system-interface-procedures) in coroutines
2. Use [`co-fn`](#Coroutine-Handling-Primitives) to pass local procedures defined in a coroutine to an external procedure

#### Danger Zone:
0. Nesting `define-coroutine` instances (or use in [`scm->cps`](#Scm-Cps)) is undefined behavior!
1. Using [`jump!`](#Control-Flow-Procedures) or [`catch-jump`](#Control-Flow-Procedures) in `define-coroutine` is undefined behavior (used by `yield`)!
2. The [`id`](#compose-bind--id) procedure is returned if no expressions exist after the last `yield`!
3. Like [`scm->cps`](#Scm-Cps), avoid [macros](#define-syntax-let-syntax-letrec-syntax)/[eval](#evalapply--symbol-append) expanding to a [`define`](#define) in the current environment!

#### Examples:
```scheme

;; Having 2 coroutines alternate until one completes (similar to the scm->cps example)!

(define-coroutine (print-ints)
  (let loop ((count 0))
    (display count)
    (display #\space)
    (yield)
    (if (< count 25)
        (loop (+ count 1)))))

(define-coroutine (print-chars)
  (let loop ((count 0))
    (display (integer->char (+ 65 count)))
    (display #\space)
    (yield)
    (if (< count 25)
        (loop (+ count 1)))))

(cycle-coroutines! (print-ints) (print-chars)) ; 0 A 1 B 2 C ... 25 Z




;; Create a generator thunk to iterate over all powers of 2!

(define-coroutine (all-pows-of-2)
  (let loop ((count 0))
    (yield (expt 2 count))
    (loop (+ count 1))))

(define 2-pow (coroutine->generator (all-pows-of-2)))
(display (2-pow)) ; 1
(display (2-pow)) ; 2
(display (2-pow)) ; 4
(display (2-pow)) ; 8




;; Step through a coroutine using coroutine objects!

(define-coroutine (example)
  (yield 1)
  (yield 2)
  (yield 3)
  4)

(define cobj (example))
(set! cobj (cobj.next)) ; launch coroutine
(display cobj.value)    ; 1
(set! cobj (cobj.next))
(display cobj.value)    ; 2
(set! cobj (cobj.next))
(display cobj.value)    ; 3
(set! cobj (cobj.next)) ; last iteration returns the final value!
(display cobj)          ; 4
```


------------------------
## Define-Overload:

#### Use: ___Define an Overload for an Existing Procedure!___
* _Note: `define-overload` is actually a macro directly defined **in** Heist Scheme!_

#### Form: `(define-overload <procedure-name> (<predicate?> <procedure>) ...)`
* _Access the original overloaded procedure version via `*original*`!_
* _Use `else` as a catch-all predicate!_

#### Examples:
```scheme
(define-overload < 
  (string? string<?) 
  (char? char<?)
  (else *original*)) ; use <else> to catch all cases!

(define-overload > 
  (number? *original*) ; reference original > via *original*
  (string? string>?) 
  (char? char>?))

(define-overload =
  (number? *original*)
  (else equal?))

(define-overload +
  (number? *original*)
  (char? string)
  (seq? append)
  (hmap? hmap-merge))
```


------------------------
## Infix! & Infixr!:

#### Use: ___Define Infix Operators with Precedence!___
* _Note: use `infix!` for left-associativity & `infixr!` for right-associativity!_
* _Note: converted to prefix notation by the reader!_
* _Inspired by Standard ML!_

#### Forms: 
* `(infix! <integer-literal> <symbol1> ...)`, `(infixr! <integer-literal> <symbol1> ...)`
  - _Define operators `<symbol1> ...` with `<integer-literal>` precedence!_
* `(infix! <symbol1> ...)`, `(infixr! <symbol1> ...)`
  - _Returns precedence level if `<symbol1> ...` are operators, else returns `#f`_

#### Forcing Precedence & Preventing Infix->Prefix Reader Conversion:
* _Force precedence via `{}` (like `()`'s use in most programming languages)!_
* _Escape infix operators from prefix conversion via `#!` prefix (rm'd by reader)!_
* _Prefix/postfix operators are ignored (presumed intentionally placed)!_


#### Examples:
```scheme
(define :: cons)
(define @ append)
(infixr! 5 :: @)

(defn qsort
  ((()) '())
  (((x . xs))
    (qsort (filter (>= x) xs)) @
    x :: (qsort (filter (< x) xs))))

(display (qsort '(1 3 5 7 2 4 6 8))) ; (1 2 3 4 5 6 7 8)



(define ** expt)
(define %% modulo)
(define % remainder)
(define // quotient)
(define != (compose not =))
(infixr! 8 **)
(infix!  7 * / // %% %)
(infix!  6 + -)
(infix!  4 > < >= <= = !=)

(display 10 + 2 ** 5)   ; 42
(display {10 + 2} ** 5) ; 248832 ; PRECEDENCE FORCED VIA "{}"



; (display (map + '(1 2) '(3 4)))) ; ERROR, READS: ((+ map '(1 2)) '(4 5))
(display (map #!+ '(1 2) '(3 4)))  ; OK: ESCAPED "+" AVOIDS INFIX CONVERSION
```


------------------------
## Unfix!:

#### Use: ___Deregister Existing Infix Operators!___

#### Form: `(unfix! <symbol1> ...)`

#### Examples:
```scheme
(infixr! 5 compose)

; (#<procedure>)
(display (list even? compose length)) 

(unfix! compose)

; (#<procedure even?> #<procedure compose> #<procedure length>)
(display (list even? compose length))
```






------------------------
# Heist Primitive Variables

0. __True & False:__ `#t`, `#f`

1. __Flonum Precision:__ `fl-precision`
   * Bound to `LDBL_DIG` from  `#include <cfloat>`

2. __Min & Max Flonum Values:__ `fl-min`, `fl-max`
   * `fl-max` bound to `LDBL_MAX` from `#include <cfloat>`
   * `fl-min` bound to `LDBL_TRUE_MIN` if exists, else `LDBL_MIN`
     - _Either option from `#include <cfloat>`_   

3. __Flonum Epsilon Value:__ `fl-epsilon`
   * Bound to `LDBL_EPSILON` from `#include <cfloat>`
   * Represents the smallest `x` so `1.0 + x != 1.0`

4. __The Empty Stream:__ `stream-null` (equivalent to `'()`)

5. __Min & Max Infix Operator Precedences:__ `*min-infix-precedence*`, `*max-infix-precedence*`
   * Bound to `LLONG_MIN` & `LLONG_MAX` from `#include <climits>`

6. __Optional Environment Arg Flags for [`Eval`](#evalapply--symbol-append), [`Load`](#system-interface-procedures), [`Cps-Eval`](#evalapply--symbol-append), [`Cps-Load`](#system-interface-procedures):__
   * Null Environment, all effects are sandboxed: `*null-environment*`
   * Local Environment, using local bindings: `*local-environment*`
   * Global Environment, using global bindings: `*global-environment*`

7. __Argc & Argv__: `*argc*`, `*argv*`
   * Interpreted Scripts: passed at the cmd-line after the script name
   * Compiled Script: passed to the executable of the compiled C++ file

8. __EXIT_SUCCESS & EXIT_FAILURE__: `*exit-success*`, `*exit-failure*`
   * Designed to be used in conjunction with [`(exit)`](#Control-Flow-Procedures)

9. __General Current Platform Name__: `*heist-platform*`
   * Possible results: `'windows` | `'apple` | `'linux` | `'unix` | `'posix` | `'unknown`

10. __Specific Current Platform Name__: `*heist-exact-platform*`
    * Possible results: 
      - `'windows-64` | `'windows-32`
      - `'apple-ios-simulator` | `'apple-ios` | `'apple-osx` | `'apple`
      - `'linux` | `'unix` | `'posix`
      - `'unknown`

11. __Get Heist Interpreter Directory__: `*heist-dirname*`
    * String to the Heist-Scheme interpreter's directory




------------------------
# Heist Primitive Procedures
## Prolific Partials:
#### All of the Below Support Partial Application!
* IE `(map even?)` is equivalent to `(lambda (x . xs) (apply map (cons even? (cons x xs))))`


## Build System Information:
0. __License__: `(license)`

1. __Sublime Text Build System__: `(sublime-text-build-system)`

2. __Shell Alias__: `(shell-alias)`


## OOP Reflection Primitives:
### Object Primitives:
0. __Functional Property Access__: `(.. <object> <property-symbol-1> ...)`
   * IE `person.sibling.age` = `(.. person 'sibling 'age)`

1. __Object Members Hash-Map__: `(object-members <object>)`
   * Returns a [`hash-map`](#Hash-Map-Procedures) of member names & values

2. __Object Methods Hash-Map__: `(object-methods <object>)`
   * Returns a [`hash-map`](#Hash-Map-Procedures) of method names & values
   * Method values already have `<object>` bound as `self`!


### Prototype Primitives:
0. __Class Name__: `(proto-name <class-prototype>)`

1. __Prototype Member Names List__: `(proto-members <class-prototype>)`

2. __Prototype Method Names List__: `(proto-methods <class-prototype>)`

3. __Inherited Prototype__: `(proto-super <class-prototype>)`

4. __Dynamically Add New Member__: 
   * `(proto-add-member! <class-prototype> <member-name-symbol> <default-value>)`

5. __Dynamically Add New Method__: 
   * `(proto-add-method! <class-prototype> <method-name-symbol> <procedure-value>)`



------------------------
## Coroutine Handling Primitives:
0. __Coroutine Object Predicate__: `(coroutine? <obj>)`
   * Coroutine objects can __only__ be made by [coroutine instantiations](#Define-Coroutine) or [`yield`](#Define-Coroutine)

1. __Convert Coroutine Object to a Generator Thunk__: `(coroutine->generator <coroutine-object>)`
   * Invoking the generator will continuously yield the next [`yield`](#Define-Coroutine)ed value
   * Yields the `'coroutine-complete` symbol once finished iterating the coroutine!

2. __Cyclical Coroutine Invocation__: `(cycle-coroutines! <coroutine-object-1> ...)`
   * ___TAKE HEED___: if none of the coroutines ever finish, neither will this procedure!
   * Invokes first coroutine until yields, then invokes next, and so on until wraps around
   * Returns the first non-coroutine-object received from a [`.next`](#Define-Coroutine) invocation
   * See the example from the [`define-coroutine`](#Define-Coroutine) section!

3. __Eval in Coroutines__: `(co-eval <datum>)`
   * Alias for [`cps-eval`](#evalapply--symbol-append) (cps-transform occurs when generating coroutines)

4. __Load in Coroutines__: `(co-load <filename-string>)`
   * Alias for [`cps-load`](#system-interface-procedures) (cps-transform occurs when generating coroutines)

5. __Pass Local Fcns to External Fcns__: `(co-fn <local-callable>)`
   * Alias for [`cps->scm`](#scm-cps-procedures) (cps-transform occurs when generating coroutines)



------------------------
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

9. __Map__: Apply `<callable>` to each elt in each stream, forming a stream of results
   * `(stream-map <callable> <stream1> <stream2> ...)`

10. __Filter__: Form a stream of elts from `<stream>` satisfying `<predicate?>`
    * `(stream-filter <predicate?> <stream>)`

11. __For Each__: Apply `<callable>` to each elt of each `<stream>`
    * `(stream-for-each <callable> <stream1> <stream2> ...)`

12. __Unfold__: Form a stream by mapping & incrementing seed, until `<break-cond-callable>` is true
    * _Note: **map** via `<map-callable>`, **increment** via `<suc-callable>`_
    * `(stream-unfold <break-cond-callable> <map-callable> <suc-callable> <seed>)`

13. __Fold__: Accumulate stream from left to right, starting with `<seed>` using `<callable>`
    * `(stream-fold <callable> <seed> <stream>)`

14. __Fold Right__: Accumulate stream from right to left, starting with `<seed>` using `<callable>`
    * `(stream-fold-right <callable> <seed> <stream>)`

15. __Numeric Stream__: Form a stream starting from `<first>` incrementing by `<optional-step>`
    * _Note: `<optional-step>` step is `1` by default_
    * `(stream-from <first> <optional-step>)`

16. __Stream Generation__: Form a stream starting from `<seed>` using `<suc-callable>`
    * `(stream-iterate <suc-callable> <seed>)`

17. __Zip__: Form a stream of lists containing the nth elt of each `<stream>`
    * `(stream-zip <stream1> <stream2> ...)`

18. __Infinite Cycle__: Forms an infinite stream of repeating `<objs>`
    * `(stream-constant <obj1> <obj2> ...)`
  
19. __Interleave__: Form a stream by interleaving elts of either `<stream>`
    * `(stream-interleave <stream1> <stream2>)`



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
   * `(< <real1> <real2> ...)`
   * `(> <real1> <real2> ...)`
   * `(<= <real1> <real2> ...)`
   * `(>= <real1> <real2> ...)`

5. __Absolute Value__: `(abs <real>)`

6. __Exponentiation__: Exponentiate n numbers
   * `(expt <number1> <number2> ...)`
   * As in math, exponentiation is _right_ associative!

7. __Exponentiation Modulo__: Raise `<real1>` to the power of `<real2>` modulo `<real3>`
   * `(expt-mod <real1> <real2> <real3>)`

8. __Maximum__: Get the maximum value
   * `(max <real1> <real2> ...)`

9. __Minimum__: Get the minimum value
   * `(min <real1> <real2> ...)`

10. __Quotient__: Get the quotient of `(/ <real1> <real2>)`
    * `(quotient <real1> <real2>)`

11. __Remainder__: Get the remainder of `(/ <real1> <real2>)`
    * `(remainder <real1> <real2>)`

12. __Divmod__: Get a pair with the quotient and remainder of `<real1>` & `<real2>`
    * `(divmod <real1> <real2>)`

13. __Modulo__: `(modulo <real1> <real2>)`

14. __Modulo Flonum__: Get a pair with the integral & fractional portions of `<flonum>`
    * `(modf <flonum>)`

15. __Exponent__: Get e raised to the power of `<number>`
    * `(exp <number>)`

16. __Logarithm__: `(log <number> <optional-base>)`
    * Defaults to the natural logarithm!

17. __Square Root__: `(sqrt <number>)`

18. __Greatest Common Denominator__: `(gcd <real1> <real2>)`

19. __Least Common Multiple__: `(lcm <real1> <real2>)`

20. __nPr__: `(npr <real1> <real2>)`

21. __nCr__: `(ncr <real1> <real2>)`

22. __Extract Number's Numerator__: `(numerator <real>)`

23. __Extract Number's Denominator__: `(denominator <real>)`

24. __Generate a Log Procedure of a Certain Base__: `(make-log-base <real>)`

25. __Psuedo-Random Number Generator__: Seeded *or* unseeded
    * `(random)`, `(random <real-seed>)`

26. __Coerce Inexact to Exact__: `(inexact->exact <number>)`

27. __Coerce Exact to Inexact__: `(exact->inexact <number>)`


### Numeric Predicates:
0. __Odd Predicate__: `(odd? <real>)`

1. __Even Predicate__: `(even? <real>)`

2. __Positive Predicate__: `(positive? <real>)`, `(not-positive? <real>)`

3. __Negative Predicate__: `(negative? <real>)`, `(not-negative? <real>)`

4. __Zero Predicate__: `(zero? <number>)`, `(not-zero? <number>)`

5. __Infinite Predicate__: `(infinite? <real>)`

6. __Finite Predicate__: `(finite? <real>)`

7. __NaN Predicate__: `(nan? <real>)`

8. __Exact Number Predicate__: `(exact? <number>)`

9. __Inexact Number Predicate__: `(inexact? <number>)`

10. __Integer Predicate__: `(integer? <number>)`

11. __Big-Integer Predicate__: `(bigint? <number>)`
    * Equivalent to `(and (exact? <number>) (integer? <number>))`


### Numeric Rounding:
0. __Round Number Up to Nearest Integer__: `(ceiling <real>)`

1. __Round Number Down to Nearest Integer__: `(floor <real>)`

2. __Round Number Towards Zero__: `(truncate <real>)`

3. __Round Number__: `(round <real>)`


### Trigonometry Procedures:
0. __Regular__: `(sin <number>)`, `(cos <number>)`, `(tan <number>)`

1. __Inverse__: `(asin <number>)`, `(acos <number>)`, `(atan <number>)`, `(atan <real1> <real2>)`

2. __Hyperbolic__: `(sinh <number>)`, `(cosh <number>)`, `(tanh <number>)`

3. __Inverse Hyperbolic__: `(asinh <number>)`, `(acosh <number>)`, `(atanh <number>)`


### Logical Bitwise Operations:
0. __And__: `(logand <real1> <real2>)`

1. __Or__: `(logor <real1> <real2>)`

2. __Xor__: `(logxor <real1> <real2>)`

3. __Not__: `(lognot <real>)`

4. __Logical Shift Left__: `(loglsl <real> <shift-amount>)`

5. __Logical Shift Right__: `(loglsr <real> <shift-amount>)`

6. __Arithmetic Shift Right__: `(logasr <real> <shift-amount>)`

7. __Confirm Nth Bit is 1__: `(logbit? <real> <n>)`

8. __Set Nth Bit to 1__: `(logbit1 <real> <n>)`

9. __Set Nth Bit to 0__: `(logbit0 <real> <n>)`

10. __Complement Nth Bit__: `(logbit~ <real> <n>)`


### Complex Number Operations:
0. Generate: `(make-rectangular <real-real> <real-imag>)`
1. Generate from polar values: `(make-polar <real-magnitude> <real-angle>)`
2. Get real part: `(real-part <number>)`
3. Get imaginary part: `(imag-part <number>)`
4. Get polar magnitude: `(magnitude <number>)`
5. Get polar angle: `(angle <number>)`
6. Get conjugate: `(conjugate <number>)`



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

5. __Alphanumeric Predicate__: `(char-alphanumeric? <char>)`

6. __Control Predicate__: `(char-control? <char>)`

7. __Printable Predicate__: `(char-print? <char>)`

8. __Graphical Predicate__: `(char-graph? <char>)`

9. __Punctuation Predicate__: `(char-punctuation? <char>)`

10. __Hexadecimal Digit Predicate__: `(char-xdigit? <char>)`

11. __Convert to Uppercase__: `(char-upcase <char>)`

12. __Convert to Lowercase__: `(char-downcase <char>)`


### Eof Character:
0. __Get EOF Character__: `(eof)`


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
   * `(make-string <size> <optional-fill-char>)`
   * _Note: `<optional-fill-char>` defaults to `#\?`_

1. __Construction__: `(string <char-or-string1> <char-or-string2> ...)`

2. __Unfold__: Form a string by mapping & incrementing seed, until `<break-condition>` is true
   * `(string-unfold <break-condition> <map-callable> <successor-callable> <seed>)`
   * _Note: **map** via `<map-callable>`, **increment** via `<successor-callable>`_

3. __Unfold Right__: Form a string by mapping right & incrementing seed, until `<break-condition>` is true
   * `(string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)`
   * _Note: **map** via `<map-callable>`, **increment** via `<successor-callable>`_

4. __Character Padding Left of String__: pads `<length>` characters, `<character>` defaults to `#\space`
   * `(string-pad <string> <length> <optional-character>)`

5. __Character Padding Right of String__: pads `<length>` characters, `<character>` defaults to `#\space`
   * `(string-pad-right <string> <length> <optional-character>)`

6. __Character Trimming Left of String__: trims characters while `<predicate?>` is true
   * `(string-trim <string> <optional-predicate?>)`
   * _Note: `<predicate?>` defaults to `char-whitespace?`_

7. __Character Trimming Right of String__: trims characters while `<predicate?>` is true
   * `(string-trim-right <string> <optional-predicate?>)`
   * _Note: `<predicate?>` defaults to `char-whitespace?`_

8. __Character Trimming Left & Right of String__: trims characters while `<predicate?>` is true
   * `(string-trim-both <string> <optional-predicate?>)`
   * _Note: `<predicate?>` defaults to `char-whitespace?`_

9. __Replacement__: Replace `<string1>` between indices `<start>` & `<end>` with `<string2>`
   * `(string-replace <string1> <string2> <start> <end>)`
   * _See [`regex-replace`](#regex-uses-ecmascript-syntax) & [`regex-replace-all`](#regex-uses-ecmascript-syntax) for a regex-based alternative!_

10. __String Contains Substring (From Left)__: Get index of 1st instance
    * `(string-contains <string> <sub-string>)`
    * _Returns `#f` is `<sub-string>` isn't in `<string>`!_

11. __String Contains Substring (From Right)__: Get index of last instance
    * `(string-contains-right <string> <sub-string>)`
    * _Returns `#f` is `<sub-string>` isn't in `<string>`!_

12. __Join a List of Strings Into 1 String__:
    * `(string-join <string-list> <optional-string-delimiter> <optional-grammar>)`
    * `<optional-grammar> = 'infix | 'suffix | 'prefix`
    * _Note: `<optional-string-delimiter>` defaults to `""`_
    * _Note: `<optional-grammar>` defaults to `'infix`_

13. __Split String Into a List of Substrings__:
    * `(string-split <target-string> <optional-string-delimiter> <optional-start-index>)`
    * _Note: `<string-delimiter>` defaults to `""`_
    * _Note: `<optional-start-index>` defaults to `0`_
    * _Enables splitting with delimiters using regex-significant chars more easily!_
      - _See [`regex-split`](#regex-uses-ecmascript-syntax) for a regex-based alternative!_

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


### Regex: (uses [ECMAScript Syntax](https://www.cplusplus.com/reference/regex/ECMAScript/))
0. __Replace 1st Regex Instance__: 
   * `(regex-replace <target-string> <regex-string> <replacement-string>)`
   * `(regex-replace <target-string> <regex-string> <callable>)`
     - `<callable> ::= (lambda (<prefix>, <suffix>, <match1>, ...) <body>)`
     - `<callable>` _must_ return a string to replace the match!

1. __Replace All Regex Instances__: 
   * `(regex-replace-all <target-string> <regex-string> <replacement-string>)`
   * `(regex-replace-all <target-string> <regex-string> <callable>)`
     - `<callable> ::= (lambda (<prefix>, <suffix>, <match1>, ...) <body>)`
     - `<callable>` _must_ return a string to replace the match!

2. __Get Alist of All Regex Matches__: `(regex-match <target-string> <regex-string>)`
   * Returned alist's sublists have the position & match substring instance!
   * If `<regex-string>` has multiple substrings per match, becomes a 2nd order alist!

3. __Regex Split String Into a List of Substrings__:
   * `(regex-split <target-string> <optional-regex-string>  <optional-start-index>)`
   * `<optional-regex-string>` defaults to `""` to split into char-strings
   * `<optional-start-index>` defaults to `0`



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
   * _Note: **map** via `<map-callable>`, **increment** via `<successor-callable>`_
   * `(unfold <break-condition> <map-callable> <successor-callable> <seed>)`

6. __Unfold Right__: Form a list by mapping right & incrementing seed, until `<break-condition>` is true
   * _Note: **map** via `<map-callable>`, **increment** via `<successor-callable>`_
   * `(unfold-right <break-condition> <map-callable> <successor-callable> <seed>)`

7. __Get All Combinations__: `(get-all-combinations <list>)`


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
   * _Seek using `eq?`_: `(assq <obj> <alist>)`
   * _Seek using `eqv?`_: `(assv <obj> <alist>)`
   * _Seek using `equal?`_: `(assoc <obj> <alist>)`



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
   * _Note: **map** via `<map-callable>`, **increment** via `<successor-callable>`_
   * `(vector-unfold <break-condition> <map-callable> <successor-callable> <seed>)`

5. __Unfold Right__: Form a vector by mapping right & incrementing seed, until `<break-condition>` is true
   * _Note: **map** via `<map-callable>`, **increment** via `<successor-callable>`_
   * `(vector-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)`

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

11. __Get All Combinations__: `(vector-get-all-combinations <vector>)`



------------------------
## Hash-Map Procedures:
#### Keys ::= `symbol` | `string` | `number` | `character` | `boolean`
0. __Constructor__: `(hmap <key1> <value1> <key2> <value2> ...)`

1. __Extract Key List__: `(hmap-keys <hash-map>)`

2. __Extract Value List__: `(hmap-vals <hash-map>)`

3. __Determine if Key in Hash-Map__: `(hmap-key? <hash-map> <key>)`

4. __Determine if Viable Key Type__: `(hmap-hashable? <obj>)`

5. __Access Value__: `(hmap-ref <hash-map> <key>)`

6. __Set/Create Association__: `(hmap-set! <hash-map> <key> <value>)`

7. __Delete Association__: `(hmap-delete! <hash-map> <key>)`

8. __Total Entries__: `(hmap-length <hash-map>)`

9. __Empty? Predicate__: `(hmap-empty? <hash-map>)`

10. __Merge Hash-Maps into a New Copy__: `(hmap-merge <hash-map-1> <hash-map-2> ...)`
    * _Note: keys of hmaps on the left take precedence over those on the right!_

11. __Merge `<hash-map-2> ...` into `<hash-map-1>`__: `(hmap-merge! <hash-map-1> <hash-map-2> ...)`
    * _Note: keys of hmaps on the left take precedence over those on the right!_

12. __Iterate Over Key-Value Pairs__: `(hmap-for-each <callable> <hash-map>)` 

13. __Iterate Over Keys__: `(hmap-for-each-key <callable> <hash-map>)` 

14. __Iterate Over Values__: `(hmap-for-each-val <callable> <hash-map>)` 

15. __Map Callable Over Values Making a New Hash-Map__: `(hmap-map <callable> <hash-map>)`

16. __Mutative Map Callable Over Values__: `(hmap-map! <callable> <hash-map>)`



------------------------
## Generic Sequence, List|Vector|String, Algorithmic Procedures:
### General:
0. __Generate Empty Variant of Sequence__: `(empty <sequence>)`

1. __Get Sequence Length__: `(length <sequence>)`

2. __Get Sequence Length (`#f` If a Circular List)__: `(length+ <sequence>)`

3. __Get Reverse of Sequence__: `(reverse <sequence>)`

4. __Mutating Reverse Sequence__: `(reverse! <sequence>)`

5. __Fold__: Accumulate sequence from left to right, starting with `<seed>` using `<callable>`
   * `(fold <callable> <seed> <sequence1> <sequence2> ...)`

6. __Fold Right__: Accumulate sequence from right to left, starting with `<seed>` using `<callable>`
   * `(fold-right <callable> <seed> <sequence1> <sequence2> ...)`

7. __Map__: Apply `<callable>` to each elt in each sequence, forming a sequence of results
   * `(map <callable> <sequence1> <sequence2> ...)`

8. __Mutating Map__: Apply `<callable>` to each elt in each sequence, mapping on the 1st sequence
   * `(map! <callable> <sequence1> <sequence2> ...)`

9. __Filter__: Form a sequence of elts from `<sequence>` satisfying `<predicate?>`
   * `(filter <predicate?> <sequence>)`

10. __For Each__: Apply `<callable>` to each elt of each `<sequence>`
    * `(for-each <callable> <sequence1> <sequence2> ...)`

11. __Mutating Copy__: Copy `<source-sequence>` to `<dest-sequence>`
    * `(seq-copy! <dest-sequence> <source-sequence>)`

12. __Count Elts With a Property__: `(count <predicate?> <sequence>)`

13. __Get Elt at an Index__: `(ref <sequence> <index>)`

14. __Get Subsequence__: `(slice <sequence> <start-index> <optional-length>)`
    * `<optional-length>` defaults to the end of `<sequence>` if not included!
    * Negative `<optional-length>` denotes offset from the end of the sequence!

15. __Set Elt at an Index__: `(set-index! <sequence> <index> <obj>)`

16. __Swap Elts at 2 Indices__: `(swap-indices! <sequence> <index> <index>)`

17. __Fill Sequence__: `(fill! <sequence> <fill-value>)`

18. __Append__: `(append <sequence1> ... <sequenceN> <obj>)`

19. __Remove__: `(remove <predicate?> <sequence>)`

20. __Remove First__: `(remove-first <predicate?> <sequence>)`

21. __Remove Last__: `(remove-last <predicate?> <sequence>)`

22. __Delete an Elt__: `(delete <sequence> <index>)`

23. __Get Last Elt__: `(last <sequence> <index>)`

24. __Get All Except Head__: `(tail <sequence> <index>)`

25. __Get First Elt__: `(head <sequence> <index>)`

26. __Get All Except Last__: `(init <sequence> <index>)`

27. __Compare Elts of Sequences__: `(seq= <predicate?> <sequence1> <sequence2> ...)`

28. __Get 1st Elt After `<predicate?>` is True__: `(skip <predicate?> <sequence>)`

29. __Get Last Elt After `<predicate?>` is True__: `(skip-right <predicate?> <sequence>)`

30. __Get Index of 1st Elt Satisfying `<predicate?>`__: `(index <predicate?> <sequence>)`

31. __Get Index of Last Elt Satisfying `<predicate?>`__: `(index-right <predicate?> <sequence>)`

32. __Drop `<length>` Elts From Left__: `(drop <sequence> <length>)`

33. __Drop `<length>` Elts From Right__: `(drop-right <sequence> <length>)`

34. __Take `<length>` Elts From Left__: `(take <sequence> <length>)`

35. __Take `<length>` Elts From Right__: `(take-right <sequence> <length>)`

36. __Drop Elts While `<predicate?>` From Left__: `(drop-while <predicate?> <sequence>)`

37. __Drop Elts While `<predicate?>` From Right__: `(drop-right-while <predicate?> <sequence>)`

38. __Take Elts While `<predicate?>` From Left__: `(take-while <predicate?> <sequence>)`

39. __Take Elts While `<predicate?>` From Right__: `(take-right-while <predicate?> <sequence>)`

40. __Confirm Any Sequence Satisfies `<predicate?>`__: `(any <predicate?> <sequence1> <sequence2> ...)`

41. __Confirm All Sequences Satisfy `<predicate?>`__: `(every <predicate?> <sequence1> <sequence2> ...)`

42. __Generic `cons`__: `cons` for lists, a copying `push-back` for strings & vectors
    * `(conj <obj> <sequence>)`


### Set Procedures:
0. __Union__: `(union <predicate?> <sequence1> <sequence2> ...)`

1. __Intersection__: `(intersection <predicate?> <sequence1> <sequence2> ...)`

2. __Difference__: `(difference <predicate?> <sequence1> <sequence2> ...)`

3. __Symmetric Difference__: `(symmetric-difference <predicate?> <sequence1> <sequence2> ...)`


### Sorting Procedures:
0. __Sort__: `(sort <predicate?> <sequence>)`

1. __Mutating Sort__: `(sort! <predicate?> <sequence>)`

2. __Confirm Sequence is Sorted__: `(sorted? <predicate?> <sequence>)`

3. __Merge 2 Sequences Sorted With `<predicate?>`__: `(merge <predicate?> <sequence1> <sequence2>)`

4. __Delete Neighboring Duplicates__: `(delete-neighbor-dups <equality-predicate?> <sequence>)`

5. __Mutating Delete Neighboring Duplicates__: `(delete-neighbor-dups! <equality-predicate?> <sequence>)`



------------------------
## Type Predicates, Undefined, & Void:
0. __Generate an Undefined Object__: `(undefined)`

1. __Undefined Predicate__: `(undefined? <obj>)`

2. __Generate a Void Object__: `(void)`

3. __Void Predicate__: `(void? <obj>)`

4. __Empty Sequence Predicate__: `(empty? <obj>)`

5. __Pair Predicate__: `(pair? <obj>)`

6. __Vector Predicate__: `(vector? <obj>)`

7. __Hash-Map Predicate__: `(hmap? <obj>)`

8. __Character Predicate__: `(char? <obj>)`

9. __Number Predicate__: `(number? <obj>)`

10. __Real Predicate__: `(real? <obj>)`

11. __Complex Predicate__: `(complex? <obj>)`

12. __Rational Number Predicate__: `(rational? <obj>)`

13. __String Predicate__: `(string? <obj>)`

14. __Symbol Predicate__: `(symbol? <obj>)`

15. __Boolean Predicate__: `(boolean? <obj>)`

16. __Atom Predicate__: `(atom? <obj>)`

17. __Procedure Predicate__: `(procedure? <obj>)`

18. __Functor Predicate__: `(functor? <obj>)`
    * _Functor = [object](#Defclass) with a `self->procedure` method defined!_
    * _Functors may be called as if a function!_

17. __Callable Predicate__: `(callable? <obj>)`
    * _Equivalent to: `(or (procedure? <obj>) (functor? <obj>))`_

19. __Cps-Procedure Predicate__: `(cps-procedure? <obj>)`

20. __Input-Port Predicate__: `(input-port? <obj>)`

21. __Output-Port Predicate__: `(output-port? <obj>)`

22. __Eof-Object Predicate__: `(eof-object? <obj>)`

23. __Stream-Pair Predicate__: `(stream-pair? <obj>)`

24. __Empty-Stream Predicate__: `(stream-null? <obj>)`

25. __Stream Predicate__: `(stream? <obj>)`

26. __Syntax-Rules Object Predicate__: `(syntax-rules-object? <obj>)`

27. __Sequence Predicate__: `(seq? <obj>)`

28. __Object Predicate__: `(object? <obj>)`

29. __Class Prototype Predicate__: `(class-prototype? <obj>)`



------------------------
## Eval/Apply & Symbol-Append:
0. __Eval__: Run quoted data as code
   * `(eval <data> <optional-environment>)`
   * _Pass `*null-environment*` to `eval` in an empty environment!_
   * _Pass `*local-environment*` to `eval` in the local environment (default)!_
   * _Pass `*global-environment*` to `eval` in the global environment!_

1. __Cps-Eval__: Alternative to `eval` for [`scm->cps`](#Scm-Cps) blocks (evals in CPS)!
   * `(cps-eval <data> <optional-environment> <continuation>)`
   * _Pass `*null-environment*` to `cps-eval` in an empty environment!_
   * _Pass `*local-environment*` to `cps-eval` in the local environment (default)!_
   * _Pass `*global-environment*` to `cps-eval` in the global environment!_

2. __Apply `<callable>` to List of Args__: `(apply <callable> <argument-list>)`

3. __Append Symbols__: `(symbol-append <symbol-1> ... <symbol-N>)`



------------------------
## Typeof & Copying:
0. __Get Typename Symbol__: `(typeof <obj>)`

1. __Deep-Copy Datum__: `(copy <obj>)`
   * Deep-copy vectors, strings, proper/dotted/circular lists, hmaps, & objects!

2. __Shallow-Copy Datum__: `(shallow-copy <obj>)`
   * Shallow-copy vectors, strings, proper/dotted/circular lists, hmaps, & objects!
   * Note that this performs _structural_ allocation w/ shallow content copying
     - Hence `copy` and `shallow-copy` are effectively identical for strings!



------------------------
## Compose, Bind, & Id:
0. __Compose N `<callable>`s__: `(compose <callable-1> ... <callable-N>)`
   * _Aliased as `o` for composition shorthand!_
   * _Generates a procedure of N args that applies them to the callable composition!_

1. __Bind N args to `<callable>`: `(bind <callable> <val-1> ... <val-N>)`__
   * _Generates a procedure that when invoked calls the arg-bound `<callable>`!_
   * _Example: `((bind map even?) '(1 2 3))` is equivalent to `(map even? '(1 2 3))`_

2. __Identity__: `(id <obj>)`



------------------------
## Delay Predicate & Force:
0. __Delay Predicate__: `(delay? <obj>)`

1. __Force a Delayed Expression__: `(force <delayed-expression>)`



------------------------
## Type Coercion:
0. __Char to Integer__: `(char->integer <char>)`

1. __Integer to Char__: `(integer->char <int>)`
   * `<int>` must be in range of [0,255]!

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

12. __Hash-Map to Alist__: `(hmap->alist <hash-map>)`

13. __Alist to Hash-Map__: `(alist->hmap <alist>)`

14. __Stream to List__: `(stream->list <stream> <size>)`
    * Convert the 1st `<size>` elts of `<stream>` into a list!

15. __List to Stream__: `(list->stream <list>)`

16. __Object Members to Hmap__: `(object->hmap <object>)`

17. __Object Members to Alist__: `(object->alist <object>)`

18. __Functor to Procedure__: `(functor->procedure <functor>)`



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
## Formatted Output Procedures:
### Formatting Stringification & Output:
0. __Sprintf__: Returns a new, formatted string!
   * `(sprintf <formatted-string> <optional-arg1> <optional-arg2> ...)`

1. __Formatted-Display__: 
   * `(displayf <optional-output-port> <formatted-string> <optional-arg1> ...)`

2. __Formatted-Write__: 
   * `(writef <optional-output-port> <formatted-string> <optional-arg1> ...)`

3. __Formatted-Pretty-Print__: 
   * `(pprintf <optional-output-port> <formatted-string> <optional-arg1> ...)`
   * `(pretty-printf <optional-output-port> <formatted-string> <optional-arg1> ...)`


### Formatting Guidelines:
```
=> <formatted-string> is like C's printf with unique formatting patterns:
   ----------------------------------------------------------------------
   %a = display anything
   %wa = write anything
   %pa = pretty-print anything
   ----------------------------------------------------------------------
   %n = number
   %+n = number (show sign if positive too)
   %,n = number with commas (only for bigints)
   %En = %en = number (coerced to exact)
   %In = %in = number (coerced to inexact)
   %#n = number (in base <#>)
   %.#n = number (with <#> digits of precision)
   -> IE "%+e2.5n": 5 digits of precision & mk exact in binary w/ sign
   -> NOTE: case of 'n' in "%n" denotes case of base >= 11 letters
   ----------------------------------------------------------------------
   %$ = display real finite as a dollar value
   ----------------------------------------------------------------------
   %s = display string
   %ws = write string
   ----------------------------------------------------------------------
   %c = display char
   %wc = write char
   ----------------------------------------------------------------------
   %b  = bool
   %wb = write "true" or "false" instead of "#t" or "#f"
   ----------------------------------------------------------------------
   %%  = "%" (escapes a "%")
   ----------------------------------------------------------------------
```


### Convert Strings to ASCII/Whitespace Art:
#### Supports Non-Whitespace ASCII, Space, Newline, Tab, Backspace, Esc!
0. __Convert String to ASCII Art__: `(string->ascii-art <string>)`

1. __Convert String to Whitespace Art__: `(string->space-art <string>)`


### Get ANSI Escape Code String (or `""` if `nansi` is active!):
#### Screen Fromatting & Text Decoration:
0. __Reset ANSI Formats__: `(fmt:reset)`

1. __Clear Screen__: `(fmt:clear)`

2. __Bold Text__: `(fmt:bold)`

3. __Underlined Text__: `(fmt:line)`

4. __Reverse Background & Foreground Colors__: `(fmt:rev)`

#### Text Colors (8 Basic Colors & Dark->Light Gradients):
0. __Black Text__: `(fmt:black)`, `(fmt:black1)` ... `(fmt:black8)`

1. __Red Text__: `(fmt:red)`, `(fmt:red1)` ... `(fmt:red8)`

2. __Green Text__: `(fmt:green)`, `(fmt:green1)` ... `(fmt:green8)`

3. __Yellow Text__: `(fmt:yellow)`, `(fmt:yellow1)` ... `(fmt:yellow8)`

4. __Blue Text__: `(fmt:blue)`, `(fmt:blue1)` ... `(fmt:blue8)`

5. __Magenta Text__: `(fmt:magenta)`, `(fmt:magenta1)` ... `(fmt:magenta8)`

6. __Cyan Text__: `(fmt:cyan)`, `(fmt:cyan1)` ... `(fmt:cyan8)`

7. __White Text__: `(fmt:white)`, `(fmt:white1)` ... `(fmt:white8)`

#### Background Colors (8 Basic Colors & Dark->Light Gradients):
0. __Black Background__: `(fmt:bblack)`, `(fmt:bblack1)` ... `(fmt:bblack8)`

1. __Red Background__: `(fmt:bred)`, `(fmt:bred1)` ... `(fmt:bred8)`

2. __Green Background__: `(fmt:bgreen)`, `(fmt:bgreen1)` ... `(fmt:bgreen8)`

3. __Yellow Background__: `(fmt:byellow)`, `(fmt:byellow1)` ... `(fmt:byellow8)`

4. __Blue Background__: `(fmt:bblue)`, `(fmt:bblue1)` ... `(fmt:bblue8)`

5. __Magenta Background__: `(fmt:bmagenta)`, `(fmt:bmagenta1)` ... `(fmt:bmagenta8)`

6. __Cyan Background__: `(fmt:bcyan)`, `(fmt:bcyan1)` ... `(fmt:bcyan8)`

7. __White Background__: `(fmt:bwhite)`, `(fmt:bwhite1)` ... `(fmt:bwhite8)`



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

8. __Read Entire Port Contents as a Data Struct__: `(read-port <optional-open-input-port-or-string>)`

9. __Read Entire File Contents as a Data Struct__: `(read-file <filename-string>)`



------------------------
## File & Port Procedures:
0. __File Predicate__: `(file? <filename-string>)`

1. __Delete File__: `(delete-file! <filename-string>)`

2. __Rename File__: `(rename-file! <old-name-string> <new-name-string>)`

3. __Open-Port Predicate__: `(open-port? <port>)`

4. __Closed-Port Predicate__: `(closed-port? <port>)`

5. __Current Input Port__: `(current-input-port)`

6. __Current Output Port__: `(current-output-port)`

7. __Call With Input File__: `(call-with-input-file <filename-string> <unary-port-callable>)`

8. __Call With Output File__: `(call-with-output-file <filename-string> <unary-port-callable>)`

9. __With Input From File__: `(with-input-from-file <filename-string> <nullary-callable>)`

10. __With Output From File__: `(with-output-from-file <filename-string> <nullary-callable>)`

11. __Generate Input Port__: `(open-input-file <filename-string>)`

12. __Generate Output Port__: `(open-output-file <filename-string>)`
    * _Only works to create files that don't already exist!_

13. __Generate Output Append Port__: `(open-output-file+ <filename-string>)`
    * _Both creates new files & appends to existing files!_

14. __Destructively Generate Output Port__: `(open-output-file! <filename-string>)`
    * _Equivalent to `(begin (delete-file! <filename-string>) (open-output-file <filename-string>))`_

15. __Close Port__: `(close-port <input-or-output-port>)`



------------------------
## System Interface Procedures:
0. __Load__: `(load <filename-string> <optional-environment>)`
   * _Pass `*null-environment*` to `load` in an empty environment!_
   * _Pass `*local-environment*` to `load` in the local environment (default)!_
   * _Pass `*global-environment*` to `load` in the global environment!_

1. __Cps-Load__: `(cps-load <filename-string> <optional-environment> <continuation-callable>)`
   * _Alternative to `load` for [`scm->cps`](#Scm-Cps) blocks (converts file to CPS prior loading)!_
   * _Pass `*null-environment*` to `cps-load` in an empty environment!_
   * _Pass `*local-environment*` to `cps-load` in the local environment (default)!_
   * _Pass `*global-environment*` to `cps-load` in the global environment!_

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

7. __Compile a File__: `(compile <filename-string> <optional-compiled-filename>)`

8. __Cps-Compile a File__: `(cps-compile <filename-string> <optional-compiled-filename>)`

9. __Get Seconds Since Epoch__: `(seconds-since-epoch)`

10. __Time Callable Execution__: `(time <callable> <arg1> ... <argN>)`
    * _Returns a pair: `(cons <time-in-seconds> <callable's-result>)`_

11. __Get Current Date as String__: `(current-date <optional-offset> ...)`
    * `<optional-offset>` = `(<symbolic-unit> <integer-amount>)`
    * `<symbolic-unit>` = `sec` | `min` | `hour` | `day` | `year`



------------------------
## Interpreter Invariants Manipulation:
0. __Disable ANSI Escape Codes__: `(set-nansi! <boolean>)`
   * Check status via `(nansi?)`!

1. __Enable Case-Insensitivity__: `(set-ci! <boolean>)`
   * Works by having the reader convert all input to lower-case!
   * Check status via `(ci?)`!

2. __Set Pretty-Print Column Length__: `(set-pprint-column-width! <positive-integer>)`
   * Get current width via `(pprint-column-width)`!

3. __Set Recursion Depth Limit__: `(set-max-recursion-depth! <positive-integer>)`
   * Get current max depth via `(max-recursion-depth)`!

4. __Set REPL Prompt__: `(set-repl-prompt! <string>)`
   * Get current repl prompt string via `(repl-prompt)`!

5. __Dynamic Procedure Trace (Last Resort Debugging)__: `(set-dynamic-call-trace! <boolean>)`
   * Check status via `(dynamic-call-trace?)`!

6. __Trace Procedure Call Arguments Too__: `(set-trace-args! <boolean>)`
   * Check status via `(trace-args?)`!

7. __Set Dot Character For Pair Literals & Variadics__: `(set-dot! <char>)`
   * Defaults to `.`!
   * Returns the last character that served this role!
   * Get current dot via `(dot)`!



------------------------
## Control Flow Procedures:
0. __Exit__: `(exit <optional-integer-exit-code>)`
   * Note: `<optional-integer-exit-code>` defaults to `*exit-success*`
   * If triggered while embedded in C++ ([`heist_cpp_interop.hpp`](https://github.com/jrandleman/Heist-Scheme/blob/master/heist_cpp_interop.hpp)), eval'd code<br>
     returns either `*exit-success*` or `*exit-failure*` as a SYMBOL!

1. __Trigger Error__: `(error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)`

2. __Trigger Syntax Error__: `(syntax-error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)`

3. __Call With Current Environment__: 
   * `(call/ce <callable> <arg1> ... <argN>)`
   * `(call-with-current-environment <callable> <arg1> ... <argN>)`

4. __Inline Call__: "deep" call/ce
   * `(inline <callable> <arg1> ... <argN>)`

5. __Jump/Throw Value__: `(jump! <optional-arg>)`
   * `<optional-arg>` defaults to [`(void)`](#Type-Predicates-Undefined--Void)

6. __Catch Jumped/Thrown Value__: `(catch-jump <callable> <arg1> ... <argN>)`

7. __Trace Procedure Call__: `(trace <procedure> <arg1> ... <argN>)`



------------------------
## Gensym:
0. __Generate a Unique Symbol__: `(gensym <optional-instance-#-to-reference>)`
   * `(gensym 1)` refers to the symbol generated by the last `(gensym)` invocation
   * `(gensym 2)` refers to the symbol generated by the 2nd to last `(gensym)` invocation
   * etc.

1. __Generate a Seeded Symbol__: `(sown-gensym <seed>)`
   * `<seed>` = number | symbol | boolean



------------------------
## Scm->Cps Procedures:
0. __Call With Current Continuation__: 
   * `(call/cc <unary-continuation-callable>)`
   * `(call-with-current-continuation <unary-continuation-callable>)`

1. __Cps->Scm__: Bind [`id`](#compose-bind--id) as callable's "topmost" continuation
   * _Note: To pass procs defined **in** a [`scm->cps`](#Scm-Cps) block as an arg to a proc defined **out** of [`scm->cps`](#Scm-Cps)_
   * `(cps->scm <callable>)`
     - _Hence programs written in and out of [`scm->cps`](#Scm-Cps) blocks may interop!_
     - _BEWARE: primitives are defined **OUT** of a [`scm->cps`](#Scm-Cps) block!_
       - _Hence wrap `cps->scm` around procs being passed to them as args when in a [`scm->cps`](#Scm-Cps) block!_



------------------------
## Syntax Procedures:
0. __Expand Macro__: `(expand <quoted-macro-exp>)`

1. __Core-Syntax?__: Determine if a symbol was defined by [`core-syntax`](#Core-Syntax)
   * `(core-syntax? <symbol>)`

2. __Runtime-Syntax?__: Determine if a symbol was defined by [`define-syntax`](#Define-Syntax-Let-Syntax-Letrec-Syntax)
   * `(runtime-syntax? <symbol>)`

3. __Reader-Alias?__: Determine if a symbol was defined by `define-reader-alias`
   * `(reader-alias? <string>)`
   * Must be a string to avoid conversion by the reader if **IS** an alias!

4. __Reader-Syntax?__: Determine if a symbol was defined by `define-reader-syntax`
   * `(reader-syntax? <string>)`
   * Must be a string to avoid expansion by the reader if **IS** syntax!

5. __Define Reader Shorthand Syntax__: 
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

6. __Get Alist of Reader Syntax Shorthands & Longhands__: `(reader-syntax-list)`

7. __Get Alist of Reader Aliases & Names__: `(reader-alias-list)`

8. __Mutate Core Syntax__: `(set-core-syntax! <old-name-symbol> <optional-new-name-symbol>)`
   * Only old name: ___DELETES___ `<old-name-symbol>` as core-syntax
   * Both old & new name: ___RENAMES___ syntax's old name to new name
     - _NOTE: also recursively renames all recursive calls to the macro in its templates!_

9. __Mutate Runtime Syntax__: `(set-runtime-syntax! <old-name-symbol> <optional-new-name-symbol>)`
   * Only old name: ___DELETES___ `<old-name-symbol>` as runtime-syntax
   * Both old & new name: ___RENAMES___ syntax's old name to new name
     - _NOTE: also recursively renames all recursive calls to the macro in its templates!_



------------------------
## Infix Analysis:

0. __Get Alist of Infix Symbols, Associativity, & Precedence__: `(infix-list)`



------------------------
## JSON Interop:
0. __Convert JSON String to a Scheme Datum__: `(json->scm <string>)`
   * Note: arrays -> vectors, null -> `'()`, & maps -> alists (of key-value lists)

1. __Convert Scheme Datum to a JSON String__: `(scm->json <obj> <optional-indent-width>)`
   ```
   <obj> ::= <string>
           | <number>
           | <'()>    ; -> <null>
           | <alist>  ; -> <map> (keys must be string | number | null | bool!)
           | <vector> ; -> <array>
           | <boolean>
   ```

2. __Convert Object Members into JSON String__: `(object->json <object> <optional-indent-width>)`

3. __JSON Datum Predicate__: `(json-datum? <obj>)`
   * Effectively returns whether `(scm->json <obj>)` would throw an error or not






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
