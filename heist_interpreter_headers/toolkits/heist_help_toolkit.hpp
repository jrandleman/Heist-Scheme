// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_help_toolkit.hpp
// => Defines logic for the Heist Scheme Interpreter's <help> special form

#ifndef HEIST_HELP_TOOLKIT_HPP_
#define HEIST_HELP_TOOLKIT_HPP_

/******************************************************************************
* HELP DB ENTRIES NAME/SIGNATURE LIST
******************************************************************************/

// ================ LITERAL TYPES & THEIR DECRIPTIONS ================
// nil
// expression
// symbol
// string
// number
// char
// boolean
// list
// pair
// vector
// hmap
// alist

// ================= OTHER TYPES & THEIR DECRIPTIONS =================
// input-port
// output-port
// object
// class-prototype
// procedure
// void
// undefined

// ================ COMMAND-LINE PRIMITIVE DESCRIPTION ================
// command-line

// ====================== COMMENTING DESCRIPTION ======================
// comments

// ========================= CPS DESCRIPTION =========================
// cps

// ===================== SPECIAL FORM DESCRIPTIONS =====================
// quote
// quasiquote
// lambda
// fn
// define
// set!
// defined?
// defn
// begin
// if
// and
// or
// cond
// case
// let
// let*
// letrec
// do
// while
// delay
// scons
// stream
// vector-literal
// hmap-literal
// define-syntax
// syntax-rules
// syntax-hash
// core-syntax
// define-reader-alias
// scm->cps
// cps-quote
// using-cps?
// curry
// defclass
// new
// define-coroutine
// define-overload
// infix
// unfix!

// ===================== PRIMITIVE VARIABLE DESCRIPTIONS =====================
// #t
// #f
// fl-precision
// fl-min
// fl-max
// fl-epsilon
// *min-infix-precedence*
// *max-infix-precedence*
// stream-null
// *null-environment*
// *local-environment*
// *global-environment*
// *argc*
// *argv*
// *heist-platform*
// *heist-exact-platform*
// *heist-dirname*
// *exit-success*
// *exit-failure*

// ===================== PRIMITIVE FUNCTION DESCRIPTIONS =====================

// (help <query-symbol-or-string>) (help)

// (license)
// (sublime-text-build-system)
// (shell-alias)

// (.. <object> <property-symbol-1> ...)
// (object-members <object>)
// (object-methods <object>)

// (proto-name <class-prototype>)
// (proto-members <class-prototype>)
// (proto-methods <class-prototype>)
// (proto-super <class-prototype>)
// (proto-add-member! <class-prototype> <member-name-symbol> <default-value>)
// (proto-add-method! <class-prototype> <method-name-symbol> <procedure-value>)

// (coroutine? <obj>)
// (coroutine->generator <coroutine-object>)
// (cycle-coroutines! <coroutine-object-1> ...)
// (co-eval <datum>)
// (co-load <filename-string>)
// (co-fn <local-callable>)

// (stream-length <stream>)
// (stream-reverse <stream>)
// (scar <stream>)
// (scdr <stream>)
// scaar ... scddddr
// (stream-ref <stream-pair> <index>)
// (stream-append <stream1> <stream2> ...)
// (stream-drop <stream> <n>)
// (stream-drop-while <predicate?> <stream>)
// (stream-take <stream> <n>)
// (stream-take-while <predicate?> <stream>)
// (stream-map <callable> <stream1> <stream2> ...)
// (stream-filter <predicate?> <stream>)
// (stream-for-each <callable> <stream1> <stream2> ...)
// (stream-unfold <break-cond-callable> <map-callable> <suc-callable> <seed>)
// (stream-fold <callable> <seed> <stream>)
// (stream-fold-right <callable> <seed> <stream>)
// (stream-from <first> <optional-step>)
// (stream-iterate <suc-callable> <seed>)
// (stream-zip <stream1> <stream2> ...)
// (stream-constant <obj1> <obj2> ...)
// (stream-interleave <stream1> <stream2>)

// (+ <number1> <number2> ...)
// (- <number1> <number2> ...) (- <number>)
// (* <number1> <number2> ...)
// (/ <number1> <number2> ...) (/ <number>)
// (= <number1> <number2> ...)
// (< <real1> <real2> ...)
// (> <real1> <real2> ...)
// (<= <real1> <real2> ...)
// (>= <real1> <real2> ...)
// (abs <real>)
// (expt <number1> <number2> ...)
// (expt-mod <real1> <real2> <real3>)
// (max <real1> <real2> ...)
// (min <real1> <real2> ...)
// (quotient <real1> <real2>)
// (remainder <real1> <real2>)
// (divmod <real1> <real2>)
// (modulo <real1> <real2>)
// (modf <flonum>)
// (exp <number>)
// (log <number> <optional-base>)
// (sqrt <number>)
// (gcd <real1> <real2>)
// (lcm <real1> <real2>)
// (npr <real1> <real2>)
// (ncr <real1> <real2>)
// (numerator <real>)
// (denominator <real>)
// (make-log-base <real>)
// (random <real-seed>) (random)
// (inexact->exact <number>)
// (exact->inexact <number>)
// (odd? <real>)
// (even? <real>)
// (positive? <real>)
// (not-positive? <real>)
// (negative? <real>)
// (not-negative? <real>)
// (zero? <number>)
// (not-zero? <number>)
// (infinite? <real>)
// (finite? <real>)
// (nan? <real>)
// (exact? <number>)
// (inexact? <number>)
// (integer? <number>)
// (bigint? <number>)
// (ceiling <real>)
// (floor <real>)
// (truncate <real>)
// (round <real>)
// (sin <number>)
// (cos <number>)
// (tan <number>)
// (asin <number>)
// (acos <number>)
// (atan <number>) (atan <real1> <real2>)
// (sinh <number>)
// (cosh <number>)
// (tanh <number>)
// (asinh <number>)
// (acosh <number>)
// (atanh <number>)
// (logand <real1> <real2>)
// (logor <real1> <real2>)
// (logxor <real1> <real2>)
// (lognot <real>)
// (loglsl <real> <shift-amount>)
// (loglsr <real> <shift-amount>)
// (logasr <real> <shift-amount>)
// (logbit? <real> <n>)
// (logbit1 <real> <n>)
// (logbit0 <real> <n>)
// (logbit~ <real> <n>)
// (make-rectangular <real-real> <real-imag>)
// (make-polar <real-magnitude> <real-angle>)
// (real-part <number>)
// (imag-part <number>)
// (magnitude <number>)
// (angle <number>)
// (conjugate <number>)

// (eq? <obj1> <obj2> ...)
// (eqv? <obj1> <obj2> ...)
// (equal? <obj1> <obj2> ...)
// (not <obj>)

// (char-alphabetic? <char>)
// (char-numeric? <char>)
// (char-whitespace? <char>)
// (char-upper-case? <char>)
// (char-lower-case? <char>)
// (char-alphanumeric? <char>)
// (char-control? <char>)
// (char-print? <char>)
// (char-graph? <char>)
// (char-punctuation? <char>)
// (char-xdigit? <char>)
// (char-upcase <char>)
// (char-downcase <char>)
// (eof)
// (char=? <char1> <char2> ...)
// (char<? <char1> <char2> ...)
// (char>? <char1> <char2> ...)
// (char<=? <char1> <char2> ...)
// (char>=? <char1> <char2> ...)
// (char-ci=? <char1> <char2> ...)
// (char-ci<? <char1> <char2> ...)
// (char-ci>? <char1> <char2> ...)
// (char-ci<=? <char1> <char2> ...)
// (char-ci>=? <char1> <char2> ...)

// (make-string <size> <optional-fill-char>)
// (string-unfold <break-condition> <map-callable> <successor-callable> <seed>)
// (string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)
// (string-pad <string> <length> <optional-character>)
// (string-pad-right <string> <length> <optional-character>)
// (string-trim <string> <optional-predicate?>)
// (string-trim-right <string> <optional-predicate?>)
// (string-trim-both <string> <optional-predicate?>)
// (string-replace <string1> <string2> <start> <end>)
// (string-contains <string> <sub-string>)
// (string-contains-right <string> <sub-string>)
// (string-join <string-list> <optional-string-delimiter> <optional-grammar>)
// (string-split <target-string> <optional-string-delimiter> <optional-start-index>)
// (string-swap! <string1> <string2>)
// (string-push! <string> <char>)
// (string-empty? <string>)
// (string-copy! <target-string> <target-start-idx> <source-string>)
// (string=? <string1> <string2> ...)
// (string<? <string1> <string2> ...)
// (string>? <string1> <string2> ...)
// (string<=? <string1> <string2> ...)
// (string>=? <string1> <string2> ...)
// (string-ci=? <string1> <string2> ...)
// (string-ci<? <string1> <string2> ...)
// (string-ci>? <string1> <string2> ...)
// (string-ci<=? <string1> <string2> ...)
// (string-ci>=? <string1> <string2> ...)
// (regex-replace <target-string> <regex-string> <replacement-string>) (regex-replace <target-string> <regex-string> <callable>)
// (regex-replace-all <target-string> <regex-string> <replacement-string>) (regex-replace-all <target-string> <regex-string> <callable>)
// (regex-match <target-string> <regex-string>)
// (regex-split <target-string> <optional-regex-string> <optional-start-index>)

// (cons <obj1> <obj2>)
// (car <pair>) 
// (cdr <pair>) 
// caar ... cddddr
// (set-car! <pair> <obj>)
// (set-cdr! <pair> <obj>)
// (last-pair <non-empty-list>)
// (pair-swap! <pair1> <pair2>)
// (make-list <size> <fill-value>)
// (list* <obj1> <obj2> ...)
// (circular-list <obj1> <obj2> ...)
// (iota <count> <optional-start-number> <optional-step-number>)
// (unfold <break-condition> <map-callable> <successor-callable> <seed>)
// (unfold-right <break-condition> <map-callable> <successor-callable> <seed>)
// (get-all-combinations <list>)
// (null? <obj>)
// (list? <obj>)
// (list*? <obj>)
// (circular-list? <obj>)
// (alist? <obj>)
// (memq <obj> <list>)
// (memv <obj> <list>)
// (member <obj> <list>)
// (assq <obj> <alist>)
// (assv <obj> <alist>)
// (assoc <obj> <alist>)

// (make-vector <size> <fill-value>)
// (vector-push! <vector> <obj>)
// (vector-iota <count> <optional-start-number> <optional-step-number>)
// (vector-unfold <break-condition> <map-callable> <successor-callable> <seed>)
// (vector-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)
// (vector-grow <vector> <size>)
// (vector-empty? <vector>)
// (vector-copy! <target-vector> <target-start-idx> <source-vector>)
// (vector-swap! <vector1> <vector2>)
// (vector-binary-search <vector> <value> <3-way-comparison>)
// (vector-get-all-combinations <vector>)

// (hmap-keys <hash-map>)
// (hmap-vals <hash-map>)
// (hmap-key? <hash-map> <key>)
// (hmap-hashable? <obj>)
// (hmap-ref <hash-map> <key>)
// (hmap-set! <hash-map> <key> <value>)
// (hmap-delete! <hash-map> <key>)
// (hmap-length <hash-map>)
// (hmap-empty? <hash-map>)
// (hmap-merge <hash-map-1> <hash-map-2> ...)
// (hmap-merge! <hash-map-1> <hash-map-2> ...)
// (hmap-for-each <callable> <hash-map>)
// (hmap-for-each-key <callable> <hash-map>)
// (hmap-for-each-val <callable> <hash-map>)
// (hmap-map <callable> <hash-map>)
// (hmap-map! <callable> <hash-map>)

// (empty <sequence>) 
// (length <sequence>)
// (length+ <sequence>)
// (reverse <sequence>)
// (reverse! <sequence>)
// (fold <callable> <seed> <sequence1> <sequence2> ...)
// (fold-right <callable> <seed> <sequence1> <sequence2> ...)
// (map <callable> <sequence1> <sequence2> ...)
// (map! <callable> <sequence1> <sequence2> ...)
// (filter <predicate?> <sequence>)
// (for-each <callable> <sequence1> <sequence2> ...)
// (seq-copy! <dest-sequence> <source-sequence>)
// (count <predicate?> <sequence>)
// (ref <sequence> <index>)
// (slice <sequence> <start-index> <optional-length>)
// (set-index! <sequence> <index> <obj>)
// (swap-indices! <sequence> <index> <index>)
// (fill! <sequence> <fill-value>)
// (append <sequence1> ... <sequenceN> <obj>)
// (remove <predicate?> <sequence>)
// (remove-first <predicate?> <sequence>)
// (remove-last <predicate?> <sequence>)
// (delete <sequence> <index>)
// (last <sequence> <index>)
// (tail <sequence> <index>)
// (head <sequence> <index>)
// (init <sequence> <index>)
// (seq= <predicate?> <sequence1> <sequence2> ...)
// (skip <predicate?> <sequence>)
// (skip-right <predicate?> <sequence>)
// (index <predicate?> <sequence>)
// (index-right <predicate?> <sequence>)
// (drop <sequence> <length>)
// (drop-right <sequence> <length>)
// (drop-while <predicate?> <sequence>)
// (drop-right-while <predicate?> <sequence>)
// (take <sequence> <length>)
// (take-right <sequence> <length>)
// (take-while <predicate?> <sequence>)
// (take-right-while <predicate?> <sequence>)
// (any <predicate?> <sequence1> <sequence2> ...)
// (every <predicate?> <sequence1> <sequence2> ...)
// (conj <obj> <sequence>)
// (union <predicate?> <sequence1> <sequence2> ...)
// (intersection <predicate?> <sequence1> <sequence2> ...)
// (difference <predicate?> <sequence1> <sequence2> ...)
// (symmetric-difference <predicate?> <sequence1> <sequence2> ...)
// (sort <predicate?> <sequence>)
// (sort! <predicate?> <sequence>)
// (sorted? <predicate?> <sequence>)
// (merge <predicate?> <sequence1> <sequence2>)
// (delete-neighbor-dups <equality-predicate?> <sequence>)
// (delete-neighbor-dups! <equality-predicate?> <sequence>)

// (undefined)
// (undefined? <obj>)
// (void)
// (void? <obj>)
// (empty? <obj>)
// (pair? <obj>)
// (vector? <obj>)
// (hmap? <obj>)
// (char? <obj>)
// (number? <obj>)
// (real? <obj>)
// (complex? <obj>)
// (rational? <obj>)
// (string? <obj>)
// (symbol? <obj>)
// (boolean? <obj>)
// (atom? <obj>)
// (procedure? <obj>)
// (functor? <obj>)
// (callable? <obj>)
// (cps-procedure? <obj>)
// (input-port? <obj>)
// (output-port? <obj>)
// (eof-object? <obj>)
// (stream-pair? <obj>)
// (stream-null? <obj>)
// (stream? <obj>)
// (syntax-rules-object? <obj>)
// (seq? <obj>)
// (object? <obj>)
// (class-prototype? <obj>)

// (eval <data> <optional-environment>)
// (cps-eval <data> <optional-environment> <continuation>)
// (apply <callable> <argument-list>)
// (symbol-append <symbol-1> ... <symbol-N>)

// (typeof <obj>)
// (copy <obj>)
// (shallow-copy <obj>)

// (compose <callable-1> ... <callable-N
// (bind <callable> <val-1> ... <val-N>)
// (id <obj>)

// (delay? <obj>)
// (force <delayed-expression>)

// (char->integer <char>)
// (integer->char <int>)
// (number->string <number> <optional-radix> <optional-precision>)
// (string->number <string> <optional-radix>)
// (string->symbol <string>)
// (symbol->string <symbol>)
// (vector->list <vector>)
// (list->vector <list>)
// (string->vector <string>)
// (vector->string <vector>)
// (string->list <string>)
// (list->string <list>)
// (hmap->alist <hash-map>)
// (alist->hmap <alist>)
// (stream->list <stream> <size>)
// (list->stream <list>)
// (object->hmap <object>)
// (object->alist <object>)
// (functor->procedure <functor>)

// (pretty-print <obj> <optional-open-output-port-or-string>) (pprint <obj> <optional-open-output-port-or-string>)
// (write <obj> <optional-open-output-port-or-string>)
// (display <obj> <optional-open-output-port-or-string>)
// (newline <optional-open-output-port-or-string>)
// (write-char <char> <optional-open-output-port-or-string>)

// (sprintf <formatted-string> <optional-arg1> <optional-arg2> ...)
// (displayf <optional-output-port> <formatted-string> <optional-arg1> ...)
// (writef <optional-output-port> <formatted-string> <optional-arg1> ...)
// (pretty-printf <optional-output-port> <formatted-string> <optional-arg1> ...) (pprintf <optional-output-port> <formatted-string> <optional-arg1> ...)
// (string->ascii-art <string>)
// (string->space-art <string>)
// (fmt:reset)
// (fmt:clear)
// (fmt:bold)
// (fmt:line)
// (fmt:rev)
// (fmt:black), (fmt:black1) ... (fmt:black8)
// (fmt:red), (fmt:red1) ... (fmt:red8)
// (fmt:green), (fmt:green1) ... (fmt:green8)
// (fmt:yellow), (fmt:yellow1) ... (fmt:yellow8)
// (fmt:blue), (fmt:blue1) ... (fmt:blue8)
// (fmt:magenta), (fmt:magenta1) ... (fmt:magenta8)
// (fmt:cyan), (fmt:cyan1) ... (fmt:cyan8)
// (fmt:white), (fmt:white1) ... (fmt:white8)
// (fmt:bblack), (fmt:bblack1) ... (fmt:bblack8)
// (fmt:bred), (fmt:bred1) ... (fmt:bred8)
// (fmt:bgreen), (fmt:bgreen1) ... (fmt:bgreen8)
// (fmt:byellow), (fmt:byellow1) ... (fmt:byellow8)
// (fmt:bblue), (fmt:bblue1) ... (fmt:bblue8)
// (fmt:bmagenta), (fmt:bmagenta1) ... (fmt:bmagenta8)
// (fmt:bcyan), (fmt:bcyan1) ... (fmt:bcyan8)
// (fmt:bwhite), (fmt:bwhite1) ... (fmt:bwhite8)

// (read <optional-open-input-port-or-string>)
// (read-string <optional-open-input-port-or-string>)
// (read-line <optional-open-input-port-or-string>)
// (read-char <optional-open-input-port-or-string>)
// (peek-char <optional-open-input-port-or-string>)
// (char-ready? <optional-open-input-port-or-string>)
// (slurp-port <optional-open-input-port-or-string>)
// (slurp-file <filename-string>)
// (read-port <optional-open-input-port-or-string>)
// (read-file <filename-string>)

// (file? <filename-string>)
// (delete-file! <filename-string>)
// (rename-file! <old-name-string> <new-name-string>)
// (open-port? <port>)
// (closed-port? <port>)
// (current-input-port)
// (current-output-port)
// (call-with-input-file <filename-string> <unary-port-callable>)
// (call-with-output-file <filename-string> <unary-port-callable>)
// (with-input-from-file <filename-string> <nullary-callable>)
// (with-output-from-file <filename-string> <nullary-callable>)
// (open-input-file <filename-string>)
// (open-output-file <filename-string>)
// (open-output-file+ <filename-string>)
// (open-output-file! <filename-string>)
// (close-port <input-or-output-port>)

// (load <filename-string> <optional-environment>)
// (cps-load <filename-string> <optional-environment> <continuation-callable>)
// (system <optional-system-call-string>)
// (getenv <variable-name-string>)
// (command-line)
// (getcwd)
// (dirname <filepath-string>)
// (compile <filename-string> <optional-compiled-filename>)
// (cps-compile <filename-string> <optional-compiled-filename>)
// (seconds-since-epoch)
// (time <callable> <arg1> ... <argN>)
// (current-date <optional-offset> ...)

// (set-nansi! <boolean>)
// (nansi?)
// (set-ci! <boolean>)
// (ci?)
// (set-pprint-column-width! <positive-integer>)
// (pprint-column-width)
// (set-max-recursion-depth! <positive-integer>)
// (max-recursion-depth)
// (set-repl-prompt! <string>)
// (repl-prompt)
// (set-dynamic-call-trace! <boolean>)
// (dynamic-call-trace?)
// (set-trace-args! <boolean>)
// (trace-args?)
// (set-dot! <char>)
// (dot)

// (exit <optional-integer-exit-code>)
// (error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)
// (syntax-error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)
// (call/ce <callable> <arg1> ... <argN>) (call-with-current-environment <callable> <arg1> ... <argN>)
// (inline <callable> <arg1> ... <argN>)
// (jump! <optional-arg>)
// (catch-jump <callable> <arg1> ... <argN>)
// (trace <procedure> <arg1> ... <argN>)

// (gensym <optional-instance-#-to-reference>)
// (sown-gensym <seed>)

// (call/cc <unary-continuation-callable>) (call-with-current-continuation <unary-continuation-callable>)
// (cps->scm <callable

// (expand <quoted-macro-exp>)
// (core-syntax? <symbol>)
// (runtime-syntax? <symbol>)
// (reader-alias? <string>)
// (reader-syntax? <string>)
// (define-reader-syntax <shorthand-string> <optional-longhand-string>)
// (reader-syntax-list)
// (reader-alias-list)
// (set-core-syntax! <old-name-symbol> <optional-new-name-symbol>)
// (set-runtime-syntax! <old-name-symbol> <optional-new-name-symbol>)

// (infix-list)

// (json->scm <string>)
// (scm->json <obj> <optional-indent-width>)
// (object->json <object> <optional-indent-width>)
// (json-datum? <obj>)

// ===================== MATHEMATICAL FLONUM CONSTANT DESCRIPTIONS =====================
// fl-e
// fl-1/e
// fl-e-2
// fl-pi
// fl-1/pi
// fl-2pi
// fl-pi/2
// fl-pi/4
// fl-pi-squared
// fl-rad/deg
// fl-deg/rad
// fl-2/pi
// fl-2/sqrt-pi
// fl-e-pi/4
// fl-log2-e
// fl-log10-e
// fl-log-2
// fl-1/log-2
// fl-log-3
// fl-log-pi
// fl-log-10
// fl-1/log-10
// fl-sqrt-2
// fl-sqrt-3
// fl-sqrt-5
// fl-sqrt-10
// fl-1/sqrt-2
// fl-cbrt-2
// fl-cbrt-3
// fl-4thrt-2
// fl-phi
// fl-log-phi
// fl-1/log-phi
// fl-euler
// fl-e-euler
// fl-sin-1
// fl-cos-1
// fl-gamma-1/2
// fl-gamma-1/3
// fl-gamma-2/3

// ===================== SPECIAL NUMERIC CONSTANT DESCRIPTIONS =====================

// +inf.0
// -inf.0
// +nan.0

// ===================== SEQUENCE TOPIC =====================

// sequence

// ===================== COROUTINE TOPIC =====================

// coroutine

// ===================== HEIST-INTEROP DOCUMENTATION =====================

// heist_cpp_interop.hpp

// ===================== SUPPLEMENT DOCUMENTATION =====================

// readme.md
// install.md

/******************************************************************************
* HELP DB DATA MATRIX
******************************************************************************/

namespace G {
  // define <HELP_ENTRIES> matrix of entries:
  //    entry ::= {name, classification, signatures, description}
  #include "heist_help_toolkit_db.hpp" 
}

/******************************************************************************
* HELP DB QUERYING HELPER FUNCTIONS
******************************************************************************/

namespace help::logic {
  scm_string indent_phrasing(const char* s)noexcept{
    scm_string str;
    while(*s) {
      str += *s;
      if(*s == '\n' && *(s+1)) str += ' ', str += ' ';
      ++s;
    }
    return str;
  }


  bool is_scar_scdr_composition(const sym_type& query)noexcept{
    if(query.size() <= 4 || query[0] != 's' || query[1] != 'c') return false;
    for(unsigned i = 2; i < 6; ++i) {
      if(query[i] != 'a' && query[i] != 'd') return false;
      if(query[i+1] == 'r' && !query[i+2]) return true;
    }
    return false;
  }


  bool is_car_cdr_composition(const sym_type& query)noexcept{
    if(query.size() <= 3 || query[0] != 'c') return false;
    for(unsigned i = 1; i < 5; ++i) {
      if(query[i] != 'a' && query[i] != 'd') return false;
      if(query[i+1] == 'r' && !query[i+2]) return true;
    }
    return false;
  }


  void strip_whitespace(sym_type& query)noexcept{
    if(query.empty() || (!isspace(*query.begin()) && !isspace(*query.rbegin()))) return;
    const auto n = query.size();
    size_type begin = 0, end = n-1;
    while(begin < n && isspace(query[begin])) ++begin;
    while(end && isspace(query[end])) --end;
    if(!end && isspace(query[end])) 
      query = "";
    else
      query = query.substr(begin,end-begin+1);
  }


  void prepare_query(sym_type& query)noexcept{
    // mk query lower-case
    for(auto& ch : query) ch = scm_numeric::mklower(ch);
    // strip whitespace at either end
    strip_whitespace(query);
    // replace aliases with their official entry
    if(query.empty() || query == "null" || query == "empty-list" || query == "()" || query == "'()" || query == "nihil") 
      query = "nil";
    else if(query == "numeric" || query == "num") 
      query = "number";
    else if(query == "character") 
      query = "char";
    else if(query == "bool") 
      query = "boolean";
    else if(query == "&&") 
      query = "and";
    else if(query == "||") 
      query = "or";
    else if(query == "class" || query == "prototype" || query == "class-proto" || query == "proto") 
      query = "class-prototype";
    else if(query == "function" || query == "fcn")
      query = "procedure";
    else if(query == "flags" || query == "command-line-flags" || query == "cmd-line" || query == "cmd-line-flags" || query == "cmd") 
      query = "command-line";
    else if(query == "-cps" || query == "continuation-passing-style" || query == "continuation" || query == "continuations") 
      query = "cps";
    else if(query == "comment" || query == "commenting" || query == ";" || query == "#||#" || query == "#|" || query == "|#") 
      query = "comments";
    else if(query == "unquote" || query == "unquote-splicing")
      query = "quasiquote";
    else if(query == "named-let" || query == "nameless-let") 
      query = "let";
    else if(query == "macro" || query == "define-macro" || query == "let-syntax" || query == "letrec-syntax") 
      query = "define-syntax";
    else if(query == "syntax") 
      query = "syntax-rules";
    else if(query == "reader-alias" || query == "alias")
      query = "define-reader-alias";
    else if(query == "define-class" || query == "define-prototype" || query == "define-class-prototype" || query == "self" || query == "super") 
      query = "defclass";
    else if(query == "anonymous-object" || query == "anon-object" || query == "anonymous-obj" || query == "anon-obj") 
      query = "new";
    else if(query == "make-coroutine")
      query = "define-coroutine";
    else if(query == "overload" || query == "polymorphism" || query == "polymorphic") 
      query = "define-overload";
    else if(query == "infixr!" || query == "infix" || query == "infixr" || query == "operator" || query == "infix-operator" || query == "infixr-operator")
      query = "infix!";
    else if(query == "true" || query == "t") 
      query = "#t";
    else if(query == "false") 
      query = "#f";
    else if(query == "min-infix-precedence" || query == "min-precedence") 
      query = "*min-infix-precedence*";
    else if(query == "max-infix-precedence" || query == "max-precedence") 
      query = "*max-infix-precedence*";
    else if(query == "null-environment" || query == "null-env") 
      query = "*null-environment*";
    else if(query == "local-environment" || query == "local-env") 
      query = "*local-environment*";
    else if(query == "global-environment" || query == "global-env") 
      query = "*global-environment*";
    else if(query == "argc") 
      query = "*argc*";
    else if(query == "argv") 
      query = "*argv*";
    else if(query == "heist-platform" || query == "platform") 
      query = "*heist-platform*";
    else if(query == "heist-exact-platform" || query == "exact-platform") 
      query = "*heist-exact-platform*";
    else if(query == "heist-dirname") 
      query = "*heist-dirname*";
    else if(query == "exit-success") 
      query = "*exit-success*";
    else if(query == "exit-failure") 
      query = "*exit-failure*";
    else if(query == "o") 
      query = "compose";
    else if(query == "e") 
      query = "fl-e";
    else if(query == "pi") 
      query = "fl-pi";
    else if(query == "phi") 
      query = "fl-phi";
    else if(query == "euler") 
      query = "fl-euler";
    else if(query == "+inf" || query == "inf" || query == "inf.0") 
      query = "+inf.0";
    else if(query == "-inf") 
      query = "-inf.0";
    else if(query == "-nan.0" || query == "nan.0" || query == "nan" || query == "+nan" || query == "-nan") 
      query = "+nan.0";
    else if(query == "seq" || query == "seqs" || query == "sequences") 
      query = "sequence";
    else if(query == "coro") 
      query = "coroutine";
    else if(query == "expr") 
      query = "expression";
    else if(query == "sym") 
      query = "symbol";
    else if(query == "str") 
      query = "string";
    else if(query == "hash-map" || query == "hashmap") 
      query = "hmap";
    else if(query == "heist-interop" || query == "heist-cpp-interop" || query == "heist-c++-interop" || query == "cpp-interop" || 
            query == "c++-interop" || query == "heist-cpp" || query == "heist-c++" || query == "heist_cpp_interop" || query == "interop")
      query = "heist_cpp_interop.hpp";
    else if(query == "readme") 
      query = "readme.md";
    else if(query == "install") 
      query = "install.md";
    else if(query == "license.md") 
      query = "license";
    else if(query == "associative-list" || query == "association-list") 
      query = "alist";
    else if(is_scar_scdr_composition(query))
      query = "scaar ... scddddr";
    else if(is_car_cdr_composition(query))
      query = "caar ... cddddr";
  }


  size_type longestCommonSubstring(const scm_string& s1, const char* s2, const size_type& n)noexcept{
    const auto m = s1.size(); // Traditional DP Soln: solve the "longest common suffix" prob instead
    std::vector<std::vector<size_type>> longestCommonSuffix(m+1,std::vector<size_type>(n+1,0));
    size_type longest = 0;
    for(size_type i = 0; i <= m; i++) {
      for(size_type j = 0; j <= n; j++) {
        if(i && j && s1[i-1] == s2[j-1]) {
            longestCommonSuffix[i][j] = longestCommonSuffix[i-1][j-1] + 1;
            if(longestCommonSuffix[i][j] > longest)
              longest = longestCommonSuffix[i][j];
        }
      }
    }
    return longest;
  }


  void print_possibly_intended_queries(const sym_type& query, std::vector<scm_string>& possible_matches)noexcept{
    size_type minimum_substring_length_match = query.size() < 4 ? query.size() : 4;
    static constexpr const char* ALTERNATIVE_HELP_QUERY_NAMES[] = {
      "","null","empty-list","()","'()","nihil","numeric","num","character","bool","&&","||","class","prototype","class-proto","proto",
      "function","fcn","flags","command-line-flags","cmd-line","cmd-line-flags","cmd","-cps","continuation-passing-style","continuation",
      "continuations","comment","commenting",";","#||#","#|","|#","unquote","unquote-splicing","named-let","nameless-let","macro","define-macro",
      "let-syntax","letrec-syntax","syntax","reader-alias","alias","define-class","define-prototype","define-class-prototype","self","super",
      "anonymous-object","anon-object","anonymous-obj","anon-obj","make-coroutine","overload","polymorphism","polymorphic","infixr!","infix",
      "infixr","operator","infix-operator","infixr-operator","true","t","false","min-infix-precedence","min-precedence","max-infix-precedence",
      "max-precedence","null-environment","null-env","local-environment","local-env","global-environment","global-env","argc","argv","heist-platform",
      "platform","heist-exact-platform","exact-platform","heist-dirname","exit-success","exit-failure","o","e","pi","phi","euler","+inf","inf","inf.0",
      "-inf","-nan.0","nan.0","nan","+nan","-nan","seq","seqs","sequences","coro","expr","sym","str","hash-map","hashmap","heist-interop",
      "heist-cpp-interop","heist-c++-interop","cpp-interop","c++-interop","heist-cpp","heist-c++","heist_cpp_interop","interop","readme","install",
      "license.md","associative-list","association-list","scaar","scadr","scdar","scddr","scaaar","scaadr","scadar","scaddr","scdaar","scdadr","scddar",
      "scdddr","scaaaar","scaaadr","scaadar","scaaddr","scadaar","scadadr","scaddar","scadddr","scdaaar","scdaadr","scdadar","scdaddr","scddaar","scddadr",
      "scdddar","scddddr","caar","cadr","cdar","cddr","caaar","caadr","cadar","caddr","cdaar","cdadr","cddar","cdddr","caaaar","caaadr","caadar","caaddr",
      "cadaar","cadadr","caddar","cadddr","cdaaar","cdaadr","cdadar","cdaddr","cddaar","cddadr","cdddar","cddddr",
    };
    // Continuously try finding possible mismatches of decreasing minimum substring match lengths (until a match is found)
    for(; minimum_substring_length_match > 0; --minimum_substring_length_match) {
      // Match against official entry names
      for(const auto& entry : G::HELP_ENTRIES)
        if(longestCommonSubstring(query, entry[0], strlen(entry[0])) >= minimum_substring_length_match)
          possible_matches.push_back(entry[0]);
      // Match against entry name aliases from <prepare_entry>
      for(const auto& alias : ALTERNATIVE_HELP_QUERY_NAMES) {
        if(longestCommonSubstring(query, alias, strlen(alias)) >= minimum_substring_length_match) {
          // get alias's offical entry name
          scm_string s(alias);
          prepare_query(s);
          // only add alias if not already in the list of possible mismatches
          bool found = false;
          for(const auto& pm : possible_matches)
            if(pm == s) { found = true; break; }
          if(!found) possible_matches.push_back(std::move(s));
        }
      }
      // break loop if found possible mismatches
      if(!possible_matches.empty()) break;
    }
    // print results & output prompt to get a new entry
    if(possible_matches.empty()) {
      printf("\nNo matches found, enter a new query!\n  => NOTE: Type \"q\" to quit!\n\n");
    } else {
      printf("\nNo matches found! Did you mean:\n");
      for(size_type i = 0, n = possible_matches.size(); i < n; ++i) {
        if(i < 10) printf("   %zu) %s\n", i, possible_matches[i].c_str());
        else        printf("  %zu) %s\n", i, possible_matches[i].c_str());
      }
      printf("\nEnter the number of the desired entry above, OR enter a new query!"
             "\n  => NOTE: Type \"q\" to quit!\n\n");
    }
  }


  scm_string get_new_help_query(const std::vector<scm_string>& possible_matches)noexcept{
    printf("help> ");
    fflush(stdout);
    // get new query
    scm_string new_query;
    int ch;
    while((ch = fgetc(stdin)) != '\n') new_query += ch;
    // clean new query & determine if given a number
    strip_whitespace(new_query);
    if(new_query.empty()) return get_new_help_query(possible_matches);
    bool only_digits = true;
    for(auto& ch : new_query)
      if(!isdigit(ch))
        only_digits = false, ch = scm_numeric::mklower(ch);
    // check if new query is an index access for 1 of the suggested matches
    int suggested_match_idx = -1;
    if(only_digits) {
      try {
        suggested_match_idx = std::stoi(new_query);
      } catch(...) {
        suggested_match_idx = -1;
      }
    }
    // return new query value
    if(suggested_match_idx >= 0 && (size_type)suggested_match_idx < possible_matches.size())
      return possible_matches[suggested_match_idx];
    return new_query;
  }


  // Returns G::EMPTY_ENTRY if no match
  auto get_entry(sym_type query)noexcept{
    prepare_query(query);
    for(const auto& entry : G::HELP_ENTRIES)
      if(entry[0] == query) 
        return entry;
    return G::EMPTY_ENTRY;
  }


  // Print the single entry instance
  // PRECONDITION: entry != G::EMPTY_ENTRY
  void print_entry(const char *const *const entry)noexcept{
    puts("\n==================================================================================");
    printf("Name: %s\n", entry[0]);
    for(size_type i = 0, n = strlen(entry[0]) + 6; i < n; ++i) putchar('*');
    printf("\nClassification: %s\n", entry[1]);
    if(entry[2][0])
      printf("\nSignatures:%s", indent_phrasing(entry[2]).c_str());
    printf("\nDescription:%s", indent_phrasing(entry[3]).c_str());
    puts("==================================================================================\n");
  }


  // Prints potential intended matches & returns new help query
  scm_string retry_help_query(const sym_type& query)noexcept{
    std::vector<scm_string> possible_matches;
    print_possibly_intended_queries(query,possible_matches);
    return get_new_help_query(possible_matches);
  }
} // End of namespace help::logic

/******************************************************************************
* HELP DB QUERYING MAIN FUNCTIONS
******************************************************************************/

namespace help {
  void query_datum(const sym_type& query)noexcept{
    const auto entry = logic::get_entry(query);
    if(entry != G::EMPTY_ENTRY) {
      logic::print_entry(entry);
    } else {
      auto new_query = logic::retry_help_query(query);
      if(new_query != "\"q\"" && new_query != "q" && 
         new_query != "quit" && new_query != ":q") {
        query_datum(new_query);
      }
    }
  }
} // End of namespace help
#endif