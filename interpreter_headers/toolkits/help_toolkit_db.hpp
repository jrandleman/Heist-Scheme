// Author: Jordan Randleman -- jrandleman@scu.edu -- help_toolkit_db.hpp
// => Defines db of help entries & descriptions

// VARIABLES DEFINED:
//   0. EMPTY_ENTRY
//   1. HELP_ENTRIES
//   2. HELP_MENU_VARIABLES
//   3. HELP_MENU_CONSTANTS
//   4. HELP_MENU_TOPICS
//   5. HELP_MENU_DOCUMENTATION
//   6. HELP_MENU_TYPES
//   7. HELP_MENU_SPECIALS
//   8. HELP_MENU_SUBMENUS, HELP_MENU_SUBMENUS_LENGTHS
//   9. HELP_MENU_PROCEDURES (& 31 SUBVARIANTS)
//   A. HELP_MENU_PROCEDURES_SUBMENU, HELP_MENU_PROCEDURES_SUBMENU_LENGTHS

#ifndef HELP_TOOLKIT_DB_HPP_
#define HELP_TOOLKIT_DB_HPP_

/******************************************************************************
* HELP MENU SUBSECTIONS
******************************************************************************/

static constexpr const char* HELP_MENU[] = {
  "topics",    "documentation", "types",      "variables", 
  "constants", "specials",      "procedures", 
};

static constexpr const char* HELP_MENU_VARIABLES[] = {
  "#t",              "#f",                 "fl-precision",           "fl-min", 
  "fl-max",          "fl-epsilon",         "*min-infix-precedence*", "*max-infix-precedence*", 
  "stream-null",     "*null-environment*", "*local-environment*",    "*global-environment*",
  "*argc*",          "*argv*",             "*heist-platform*",       "*heist-exact-platform*",
  "*heist-dirname*", "*exit-success*",     "*exit-failure*",
};

static constexpr const char* HELP_MENU_CONSTANTS[] = {
  "+inf.0",       "-inf.0",     "+nan.0",      "fl-e", "fl-1/e", "fl-e-2",       "fl-pi",        "fl-1/pi",
  "fl-2pi",       "fl-pi/2",    "fl-pi/4",     "fl-pi-squared",  "fl-rad/deg",   "fl-deg/rad",   "fl-2/pi",
  "fl-2/sqrt-pi", "fl-e-pi/4",  "fl-log2-e",   "fl-log10-e",     "fl-log-2",     "fl-1/log-2",   "fl-log-3",
  "fl-log-pi",    "fl-log-10",  "fl-1/log-10", "fl-sqrt-2",      "fl-sqrt-3",    "fl-sqrt-5",    "fl-sqrt-10",
  "fl-1/sqrt-2",  "fl-cbrt-2",  "fl-cbrt-3",   "fl-4thrt-2",     "fl-phi",       "fl-log-phi",   "fl-1/log-phi",
  "fl-euler",     "fl-e-euler", "fl-sin-1",    "fl-cos-1",       "fl-gamma-1/2", "fl-gamma-1/3", "fl-gamma-2/3",
};

static constexpr const char* HELP_MENU_TOPICS[] = { 
  "summary",     "macro-system", "conventions", "notation", 
  "namespacing", "comments",     "cps",         "*dot*",
};

static constexpr const char* HELP_MENU_DOCUMENTATION[] = { 
  "cpp_interop.hpp", "readme.md", "install.md", 
};

static constexpr const char* HELP_MENU_TYPES[] = { 
  "nil",             "expression", "symbol",     "string",             "number", 
  "char",            "boolean",    "list",       "pair",               "vector", 
  "hmap",            "alist",      "input-port", "output-port",        "object", 
  "class-prototype", "procedure",  "void",       "undefined",          "sequence", 
  "coroutine",       "universe",   "module",     "syntax-transformer", 
};

static constexpr const char* HELP_MENU_SPECIALS[] = { 
  "quote",           "quasiquote",          "lambda",        "fn",               "define",
  "set!",            "defn",                "defined?",      "delete!",          "begin",
  "if",              "and",                 "or",            "cond",             "case",
  "let",             "let*",                "letrec",        "letrec*",          "do",
  "while",           "for",                 "delay",         "scons",            "stream",
  "vector-literal",  "hmap-literal",        "define-syntax", "syntax-rules",     "syntax-hash",
  "core-syntax",     "define-reader-alias", "scm->cps",      "cps-quote",        "using-cps?",
  "curry",           "defclass",            "new",           "define-coroutine", "define-module", 
  "define-overload", "infix!",              "unfix!",
};

static constexpr const char* HELP_MENU_PROCEDURES[] = { 
  "help",         "build",       "objects",         "prototypes", "coroutines",
  "streams",      "numbers",     "equality",        "chars",      "strings",
  "pairs",        "vectors",     "hmaps",           "sequences",  "predicates",
  "evalapply",    "copying",     "delayforce",      "coercion",   "output",
  "formatoutput", "input",       "files",           "ports",      "sysinterface", 
  "invariants",   "controlflow", "call/cc",         "syntax",     "json",
  "csv",          "gensyms",     "compose-bind-id", "falsiness", 
};

static constexpr char** HELP_MENU_SUBMENUS[] = {
  (char**)HELP_MENU_TOPICS,    (char**)HELP_MENU_DOCUMENTATION, (char**)HELP_MENU_TYPES,      (char**)HELP_MENU_VARIABLES, 
  (char**)HELP_MENU_CONSTANTS, (char**)HELP_MENU_SPECIALS,      (char**)HELP_MENU_PROCEDURES, 
};

static constexpr size_type HELP_MENU_SUBMENUS_LENGTH[] = {
  sizeof(HELP_MENU_TOPICS)/sizeof(HELP_MENU_TOPICS[0]),         sizeof(HELP_MENU_DOCUMENTATION)/sizeof(HELP_MENU_DOCUMENTATION[0]), 
  sizeof(HELP_MENU_TYPES)/sizeof(HELP_MENU_TYPES[0]),           sizeof(HELP_MENU_VARIABLES)/sizeof(HELP_MENU_VARIABLES[0]), 
  sizeof(HELP_MENU_CONSTANTS)/sizeof(HELP_MENU_CONSTANTS[0]),   sizeof(HELP_MENU_SPECIALS)/sizeof(HELP_MENU_SPECIALS[0]),
  sizeof(HELP_MENU_PROCEDURES)/sizeof(HELP_MENU_PROCEDURES[0]), 
};

static constexpr const char* HELP_MENU_PROCEDURES_HELP[] = {}; // direct link

static constexpr const char* HELP_MENU_PROCEDURES_BUILD[] = {
  "license", "sublime-text-build-system", "shell-alias",
};

static constexpr const char* HELP_MENU_PROCEDURES_OBJECTS[] = {
 "..", "object-members", "object-methods", 
};

static constexpr const char* HELP_MENU_PROCEDURES_PROTOTYPES[] = {
  "proto-name",          "proto-members",     "proto-methods", "proto-super", 
  "proto-add-property!", 
};

static constexpr const char* HELP_MENU_PROCEDURES_COROUTINES[] = {
  "coroutine?", "coroutine->generator", "cycle-coroutines!",
};

static constexpr const char* HELP_MENU_PROCEDURES_STREAMS[] = {
  "stream-length",     "stream-reverse",  "scar",              "scdr", 
  "scaar...scddddr",   "stream-ref",      "stream-append",     "stream-drop", 
  "stream-drop-while", "stream-take",     "stream-take-while", "stream-map", 
  "stream-filter",     "stream-for-each", "stream-unfold",     "stream-fold", 
  "stream-fold-right", "stream-from",     "stream-iterate",    "stream-zip", 
  "stream-constant",   "stream-interleave", 
};

static constexpr const char* HELP_MENU_PROCEDURES_NUMBERS[] = {
  "+",         "-",        "*",                "/",             "=",             "<",             ">",              "<=", 
  ">=",        "abs",      "expt",             "expt-mod",      "max",           "min",           "quotient",       "remainder", 
  "divmod",    "modulo",   "modf",             "exp",           "log",           "sqrt",          "gcd",            "lcm", 
  "npr",       "ncr",      "numerator",        "denominator",   "make-log-base", "random",        "inexact->exact", "exact->inexact", 
  "odd?",      "even?",    "positive?",        "not-positive?", "negative?",     "not-negative?", "zero?",          "not-zero?", 
  "infinite?", "finite?",  "nan?",             "exact?",        "inexact?",      "integer?",      "bigint?",        "ceiling", 
  "floor",     "truncate", "round",            "sin",           "cos",           "tan",           "asin",           "acos", 
  "atan",      "sinh",     "cosh",             "tanh",          "asinh",         "acosh",         "atanh",          "logand", 
  "logor",     "logxor",   "lognot",           "loglsl",        "loglsr",        "logasr",        "logbit?",        "logbit1", 
  "logbit0",   "logbit~",  "make-rectangular", "make-polar",    "real-part",     "imag-part",     "magnitude",      "angle", 
  "conjugate", 
};

static constexpr const char* HELP_MENU_PROCEDURES_EQUALITY[] = {
  "eq?", "eqv?", "equal?", "not", 
};

static constexpr const char* HELP_MENU_PROCEDURES_CHARS[] = {
  "char-alphabetic?", "char-numeric?",      "char-whitespace?", "char-upper-case?", 
  "char-lower-case?", "char-alphanumeric?", "char-control?",    "char-print?", 
  "char-graph?",      "char-punctuation?",  "char-xdigit?",     "char-upcase", 
  "char-downcase",    "eof",                "char=?",           "char<?", 
  "char>?",           "char<=?",            "char>=?",          "char-ci=?", 
  "char-ci<?",        "char-ci>?",          "char-ci<=?",       "char-ci>=?", 
};

static constexpr const char* HELP_MENU_PROCEDURES_STRINGS[] = {
  "string",           "make-string",       "string-unfold",     "string-unfold-right", 
  "string-pad",       "string-pad-right",  "string-trim",       "string-trim-right", 
  "string-trim-both", "string-replace",    "string-contains",   "string-contains-right", 
  "string-join",      "string-split",      "string-swap!",      "string-push!", 
  "string-pop!",      "string-empty?",     "string-copy!",      "string=?",
  "string<?",         "string>?",          "string<=?",         "string>=?",
  "string-ci=?",      "string-ci<?",       "string-ci>?",       "string-ci<=?",
  "string-ci>=?",     "regex-replace",     "regex-replace-all", "regex-match",
  "regex-split", 
};

static constexpr const char* HELP_MENU_PROCEDURES_PAIRS[] = {
  "cons",      "car",      "cdr",          "caar...cddddr", 
  "set-car!",  "set-cdr!", "last-pair",    "pair-swap!", 
  "make-list", "list",     "list*",        "circular-list", 
  "iota",      "unfold",   "unfold-right", "get-all-combinations", 
  "null?",     "list?",    "list*?",       "circular-list?", 
  "alist?",    "memq",     "memv",         "member", 
  "assq",      "assv",     "assoc", 
};

static constexpr const char* HELP_MENU_PROCEDURES_VECTORS[] = {
  "vector",                      "make-vector",   "vector-push!",        "vector-pop!", 
  "vector-iota",                 "vector-unfold", "vector-unfold-right", "vector-grow",
  "vector-empty?",               "vector-copy!",  "vector-swap!",        "vector-binary-search", 
  "vector-get-all-combinations", 
};

static constexpr const char* HELP_MENU_PROCEDURES_HMAPS[] = {
  "hmap",           "hmap-keys",         "hmap-vals",         "hmap-key?", 
  "hmap-hashable?", "hmap-ref",          "hmap-set!",         "hmap-delete!", 
  "hmap-length",    "hmap-empty?",       "hmap-merge",        "hmap-merge!", 
  "hmap-for-each",  "hmap-for-each-key", "hmap-for-each-val", "hmap-map", 
  "hmap-map!", 
};

static constexpr const char* HELP_MENU_PROCEDURES_SEQUENCES[] = {
  "empty",                 "length",      "length+",              "reverse", 
  "reverse!",              "fold",        "fold-right",           "map", 
  "map!",                  "filter",      "for-each",             "seq-copy!", 
  "count",                 "ref",         "slice",                "set-index!", 
  "swap-indices!",         "fill!",       "append",               "remove", 
  "remove-first",          "remove-last", "delete",               "last", 
  "tail",                  "head",        "init",                 "seq=", 
  "skip",                  "skip-right",  "index",                "index-right", 
  "drop",                  "drop-right",  "drop-while",           "drop-right-while", 
  "take",                  "take-right",  "take-while",           "take-right-while", 
  "any",                   "every",       "conj",                 "union", 
  "intersection",          "difference",  "symmetric-difference", "sort", 
  "sort!",                 "sorted?",     "merge",                "delete-neighbor-dups", 
  "delete-neighbor-dups!", 
};

static constexpr const char* HELP_MENU_PROCEDURES_PREDICATES[] = {
  "typeof",       "undefined",    "undefined?",       "void", 
  "void?",        "empty?",       "pair?",            "vector?", 
  "hmap?",        "char?",        "number?",          "real?", 
  "complex?",     "rational?",    "string?",          "symbol?", 
  "boolean?",     "atom?",        "procedure?",       "functor?", 
  "callable?",    "input-port?",  "output-port?",     "eof-object?",
  "stream-pair?", "stream-null?", "stream?",          "syntax-rules-object?", 
  "seq?",         "object?",      "class-prototype?", 
};

static constexpr const char* HELP_MENU_PROCEDURES_EVALAPPLY[] = {
  "eval", "cps-eval", "apply", 
};

static constexpr const char* HELP_MENU_PROCEDURES_COPY[] = {
  "copy", "shallow-copy", 
};

static constexpr const char* HELP_MENU_PROCEDURES_DELAY[] = {
  "delay?", "force", 
};

static constexpr const char* HELP_MENU_PROCEDURES_COERCION[] = {
  "char->integer",  "integer->char",  "number->string",     "string->number", 
  "string->symbol", "symbol->string", "vector->list",       "list->vector", 
  "string->vector", "vector->string", "string->list",       "list->string", 
  "hmap->alist",    "alist->hmap",    "stream->list",       "list->stream", 
  "object->hmap",   "object->alist",  "functor->procedure", 
};

static constexpr const char* HELP_MENU_PROCEDURES_OUTPUT[] = {
  "pretty-print", "write", "display", "newline", "write-char", 
};

static constexpr const char* HELP_MENU_PROCEDURES_FORMATOUTPUT[] = {
  "sprintf",           "displayf",          "writef",      "pretty-printf", 
  "string->ascii-art", "string->space-art", "fmt:reset",   "fmt:clear", 
  "fmt:bold",          "fmt:line",          "fmt:rev",     "fmt:black", 
  "fmt:red",           "fmt:green",         "fmt:yellow",  "fmt:blue", 
  "fmt:magenta",       "fmt:cyan",          "fmt:white",   "fmt:bblack", 
  "fmt:bred",          "fmt:bgreen",        "fmt:byellow", "fmt:bblue", 
  "fmt:bmagenta",      "fmt:bcyan",         "fmt:bwhite", 
};

static constexpr const char* HELP_MENU_PROCEDURES_INPUT[] = {
  "read",      "read-string", "read-line",  "read-char", 
  "peek-char", "char-ready?", "slurp-port", "slurp-file",
  "read-port", "read-file", 
};

static constexpr const char* HELP_MENU_PROCEDURES_FILES[] = {
  "getcwd",                "dirname",              "mkdir",                 "chdir", 
  "file?",                 "delete-file!",         "rename-file!",          "copy-file",
  "file-size",
};

static constexpr const char* HELP_MENU_PROCEDURES_PORTS[] = {
  "open-port?",           "closed-port?",          "current-input-port",   "current-output-port",
  "call-with-input-file", "call-with-output-file", "with-input-from-file", "with-output-from-file", 
  "open-input-file",      "open-output-file",      "open-output-file+",    "open-output-file!",
  "rewind-port!",         "port-seek!",            "port-seek-front!",     "close-port", 
};

static constexpr const char* HELP_MENU_PROCEDURES_SYSINTERFACE[] = {
  "load",   "cps-load",     "compile",      "cps-compile",
  "system", "getenv",       "command-line", "seconds-since-epoch", 
  "time",   "current-date", 
};

static constexpr const char* HELP_MENU_PROCEDURES_INVARIANTS[] = {
  "set-nansi!",          "nansi?",                   "ci?",                 "set-pprint-column-width!", 
  "pprint-column-width", "set-max-recursion-depth!", "max-recursion-depth", "set-repl-prompt!",
  "repl-prompt",         "set-dynamic-call-trace!",  "dynamic-call-trace?", "set-trace-args!",
  "trace-args?",         "set-dot!",                 "dot", 
};

static constexpr const char* HELP_MENU_PROCEDURES_CONTROLFLOW[] = {
  "exit",                         "error",                        "syntax-error",   "call/ce", 
  "lexical-scope->dynamic-scope", "dynamic-scope->lexical-scope", "dynamic-scope?", "lexical-scope?", 
  "jump!",                        "catch-jump",                   "trace", 
};

static constexpr const char* HELP_MENU_PROCEDURES_CALLCC[] = {}; // direct link

static constexpr const char* HELP_MENU_PROCEDURES_SYNTAX[] = {
  "expand",                 "expand*",            "core-expand",       "core-expand*",
  "core-syntax?",           "runtime-syntax?",    "reader-alias?",     "reader-syntax?",
  "define-reader-syntax",   "reader-syntax-list", "reader-alias-list", "delete-core-syntax!", 
  "delete-runtime-syntax!", "infix-list", 
};

static constexpr const char* HELP_MENU_PROCEDURES_JSON[] = {
  "json->scm", "scm->json", "object->json", "json-datum?", 
};

static constexpr const char* HELP_MENU_PROCEDURES_CSV[] = {
  "csv->list", "csv->vector", "list->csv", "vector->csv", "csv-datum?", 
};

static constexpr const char* HELP_MENU_PROCEDURES_GENSYM[] = {
  "gensym", "sown-gensym", "symbol-append", 
};

static constexpr const char* HELP_MENU_PROCEDURES_COMPOSEBINDID[] = {
  "compose", "bind", "id", 
};

static constexpr const char* HELP_MENU_PROCEDURES_FALSINESS[] = {
  "set-falsey!", "set-truthy!", "falsey-values", 
};

static constexpr char** HELP_MENU_PROCEDURES_SUBMENU[] = {
  (char**)HELP_MENU_PROCEDURES_HELP, // help => direct link
  (char**)HELP_MENU_PROCEDURES_BUILD,       (char**)HELP_MENU_PROCEDURES_OBJECTS,       (char**)HELP_MENU_PROCEDURES_PROTOTYPES,   (char**)HELP_MENU_PROCEDURES_COROUTINES, 
  (char**)HELP_MENU_PROCEDURES_STREAMS,     (char**)HELP_MENU_PROCEDURES_NUMBERS,       (char**)HELP_MENU_PROCEDURES_EQUALITY,     (char**)HELP_MENU_PROCEDURES_CHARS, 
  (char**)HELP_MENU_PROCEDURES_STRINGS,     (char**)HELP_MENU_PROCEDURES_PAIRS,         (char**)HELP_MENU_PROCEDURES_VECTORS,      (char**)HELP_MENU_PROCEDURES_HMAPS, 
  (char**)HELP_MENU_PROCEDURES_SEQUENCES,   (char**)HELP_MENU_PROCEDURES_PREDICATES,    (char**)HELP_MENU_PROCEDURES_EVALAPPLY,    (char**)HELP_MENU_PROCEDURES_COPY, 
  (char**)HELP_MENU_PROCEDURES_DELAY,       (char**)HELP_MENU_PROCEDURES_COERCION,      (char**)HELP_MENU_PROCEDURES_OUTPUT,       (char**)HELP_MENU_PROCEDURES_FORMATOUTPUT, 
  (char**)HELP_MENU_PROCEDURES_INPUT,       (char**)HELP_MENU_PROCEDURES_FILES,         (char**)HELP_MENU_PROCEDURES_PORTS,        (char**)HELP_MENU_PROCEDURES_SYSINTERFACE, 
  (char**)HELP_MENU_PROCEDURES_INVARIANTS,  (char**)HELP_MENU_PROCEDURES_CONTROLFLOW,   (char**)HELP_MENU_PROCEDURES_CALLCC,       (char**)HELP_MENU_PROCEDURES_SYNTAX,
  (char**)HELP_MENU_PROCEDURES_JSON,        (char**)HELP_MENU_PROCEDURES_CSV,           (char**)HELP_MENU_PROCEDURES_GENSYM,       (char**)HELP_MENU_PROCEDURES_COMPOSEBINDID, 
  (char**)HELP_MENU_PROCEDURES_FALSINESS, 
};

static constexpr size_type HELP_MENU_PROCEDURES_SUBMENU_LENGTH[] = {
  0, /* help => direct link */
  sizeof(HELP_MENU_PROCEDURES_BUILD)/sizeof(HELP_MENU_PROCEDURES_BUILD[0]),               sizeof(HELP_MENU_PROCEDURES_OBJECTS)/sizeof(HELP_MENU_PROCEDURES_OBJECTS[0]),
  sizeof(HELP_MENU_PROCEDURES_PROTOTYPES)/sizeof(HELP_MENU_PROCEDURES_PROTOTYPES[0]),     sizeof(HELP_MENU_PROCEDURES_COROUTINES)/sizeof(HELP_MENU_PROCEDURES_COROUTINES[0]), 
  sizeof(HELP_MENU_PROCEDURES_STREAMS)/sizeof(HELP_MENU_PROCEDURES_STREAMS[0]),           sizeof(HELP_MENU_PROCEDURES_NUMBERS)/sizeof(HELP_MENU_PROCEDURES_NUMBERS[0]),
  sizeof(HELP_MENU_PROCEDURES_EQUALITY)/sizeof(HELP_MENU_PROCEDURES_EQUALITY[0]),         sizeof(HELP_MENU_PROCEDURES_CHARS)/sizeof(HELP_MENU_PROCEDURES_CHARS[0]), 
  sizeof(HELP_MENU_PROCEDURES_STRINGS)/sizeof(HELP_MENU_PROCEDURES_STRINGS[0]),           sizeof(HELP_MENU_PROCEDURES_PAIRS)/sizeof(HELP_MENU_PROCEDURES_PAIRS[0]),
  sizeof(HELP_MENU_PROCEDURES_VECTORS)/sizeof(HELP_MENU_PROCEDURES_VECTORS[0]),           sizeof(HELP_MENU_PROCEDURES_HMAPS)/sizeof(HELP_MENU_PROCEDURES_HMAPS[0]), 
  sizeof(HELP_MENU_PROCEDURES_SEQUENCES)/sizeof(HELP_MENU_PROCEDURES_SEQUENCES[0]),       sizeof(HELP_MENU_PROCEDURES_PREDICATES)/sizeof(HELP_MENU_PROCEDURES_PREDICATES[0]),
  sizeof(HELP_MENU_PROCEDURES_EVALAPPLY)/sizeof(HELP_MENU_PROCEDURES_EVALAPPLY[0]),       sizeof(HELP_MENU_PROCEDURES_COPY)/sizeof(HELP_MENU_PROCEDURES_COPY[0]), 
  sizeof(HELP_MENU_PROCEDURES_DELAY)/sizeof(HELP_MENU_PROCEDURES_DELAY[0]),               sizeof(HELP_MENU_PROCEDURES_COERCION)/sizeof(HELP_MENU_PROCEDURES_COERCION[0]),
  sizeof(HELP_MENU_PROCEDURES_OUTPUT)/sizeof(HELP_MENU_PROCEDURES_OUTPUT[0]),             sizeof(HELP_MENU_PROCEDURES_FORMATOUTPUT)/sizeof(HELP_MENU_PROCEDURES_FORMATOUTPUT[0]), 
  sizeof(HELP_MENU_PROCEDURES_INPUT)/sizeof(HELP_MENU_PROCEDURES_INPUT[0]),               sizeof(HELP_MENU_PROCEDURES_FILES)/sizeof(HELP_MENU_PROCEDURES_FILES[0]),
  sizeof(HELP_MENU_PROCEDURES_PORTS)/sizeof(HELP_MENU_PROCEDURES_PORTS[0]),               sizeof(HELP_MENU_PROCEDURES_SYSINTERFACE)/sizeof(HELP_MENU_PROCEDURES_SYSINTERFACE[0]), 
  sizeof(HELP_MENU_PROCEDURES_INVARIANTS)/sizeof(HELP_MENU_PROCEDURES_INVARIANTS[0]),     sizeof(HELP_MENU_PROCEDURES_CONTROLFLOW)/sizeof(HELP_MENU_PROCEDURES_CONTROLFLOW[0]),
  0, /* call/cc => direct link */                                                         sizeof(HELP_MENU_PROCEDURES_SYNTAX)/sizeof(HELP_MENU_PROCEDURES_SYNTAX[0]),
  sizeof(HELP_MENU_PROCEDURES_JSON)/sizeof(HELP_MENU_PROCEDURES_JSON[0]),                 sizeof(HELP_MENU_PROCEDURES_CSV)/sizeof(HELP_MENU_PROCEDURES_CSV[0]),
  sizeof(HELP_MENU_PROCEDURES_GENSYM)/sizeof(HELP_MENU_PROCEDURES_GENSYM[0]),             sizeof(HELP_MENU_PROCEDURES_COMPOSEBINDID)/sizeof(HELP_MENU_PROCEDURES_COMPOSEBINDID[0]), 
  sizeof(HELP_MENU_PROCEDURES_FALSINESS)/sizeof(HELP_MENU_PROCEDURES_FALSINESS[0]), 
};

static constexpr char** HELP_MENU_PROCEDURES_DIRECT_LINKS[] = {
  (char**)HELP_MENU_PROCEDURES_HELP, (char**)HELP_MENU_PROCEDURES_CALLCC,
};

/******************************************************************************
* HELP DB ENTRIES
******************************************************************************/

static constexpr const char* EMPTY_ENTRY[4] = {"","","",""};


static constexpr const char* HELP_ENTRIES[][4] = { // {{name, classification, signatures, description}, ...}

/******************************************************************************
* LITERAL OBJECT DESCRIPTIONS @NEW-SECTION
******************************************************************************/

{
"nil",
"Object (Value Semantics)",
R"()",
R"(
The empty list '(), terminating all 'proper lists', & the only value for which 
"null?" returns true. Unlike CL or Clojure, '() is 'truthy' in Heist Scheme by default.
  *) Only #f (boolean false) is falsey by default in Heist Scheme.
  *) See "set-falsey!" and "set-truthy!" to change this!
)",





}, {
"symbol",
"Object (Value Semantics)",
R"()",
R"(
Literal: Primitive type read in by reader, & evaluates to a value (if defined).
Value: Created by quoting a primitive symbolic literal or via "string->symbol".
)",





}, {
"expression",
"Object (Value Semantics)",
R"()",
R"(
Literal type read in by reader, & manipulated as syntax via macros.
  *) See "core-syntax" & "define-syntax" in order to create macros!
  *) Also handled by C++ Special Forms, & evaluated by core interpreter.
)",





}, {
"string",
"Object (Reference Semantics) | Procedure",
R"(
(string <char-or-string> ...)
(string)
)",
R"(
Object:
  Denoted by double-quotes: "<string-contents>". Print strings to be 
  human-readable with "display", and machine-readable with "write".
  Use "pretty-print" for "write" with indentation for lists.

Procedure:
  Appends all given chars & strings into a freshly-allocated string.
  W/o args returns "".
)",





}, {
"number",
"Object (Value Semantics)",
R"()",
R"(
4 Number Types:
  0. Exact/Ratnum (rational number)
     *) Arbitrary precision numerator & denominator (automatically reduced to simplest form!)
     *) Special Case: denominator of 1 creates a BigInt
        
        -1/2 ; stays as a fraction!
        3    ; ratnum w/ denom of 1 = bigint
        4/2  ; gets simplified to bigint 2

  1. Inexact/Flonum (floating-point number)
     *) Base-10 may use scientific notation!
     *) Precision is bound by fl-precision
     *) Special Case: 0.0 gets simplified to 0 (Zero is Exact)

        1.0
        3.5e10 ; scientific notation
        -4E12  ; also scientific notation

  2. Special Constants:
     *) Positive Infinity: +inf.0
     *) Negative Infinity: -inf.0
     *) NaN: +nan.0, -nan.0
        => Both +nan.0 & -nan.0 resolve to the same NaN object!
  
  3. Complex Numbers:
     *) Both the real and imaginary components will match in exactness
     *) Supports +inf.0 or -inf.0 components (+nan.0 is unique & never complex!)
     *) Special Case: imaginary value of 0 becomes a real (non-complex) number!

        3/4+1/2i
        3/4+0.5i ; BECOMES 0.75+0.5i to match exactness
        -i       ; valid complex number!
        -44+0i   ; BECOMES -44

2 Prefix Types:
  0. Radix:
     *) Binary: #b, Octal: #o, Hexadecimal: #x, Decimal: #d (enabled by default)
     *) Nary 2-36: #2r-#36r

        #b-101    ; -5
        #b10/11   ; 2/3
        #b1010.11 ; 10.75
        #o77      ; 63
        #xC0DE    ; 49374
        #xc0de    ; 49374
        #30rHeistScheme ; 10326335991592274
        #2r-101/10      ; -5/2

  1. Exactness:
     *) Inexact: #i, Exact: #e

        #i3   ; 3.0
        #i1/2 ; 0.5
        #e3.5 ; 7/2
        #e1.0 ; 1
        #e#b101.1 ; Exact & Binary! => 11/2
        #i#2r101  ; Inexact & Binary! => 5.0
)",





}, {
"char",
"Object (Value Semantics)",
R"()",
R"(
Have the '#\' prefix: #\h #\e #\l #\l #\o (uses ascii encoding!)
Also Supports Named Chars and Hex Chars:
  #\space, #\tab, #\newline, #\vtab, #\page, #\return,
  #\alarm, #\backspace, #\nul, #\esc, #\delete,
  #\x0 -> #\xff
)",





}, {
"boolean",
"Object (Value Semantics)",
R"()",
R"(
True:  #t
False: #f

By default, only #f is falsey and all other values are truthy in Heist Scheme.
  *) See "set-falsey!" and "set-truthy!" to change this!
)",





}, {
"list",
"Object (Reference Semantics), Procedure",
R"(
(list <object> ...)
(list)
)",
R"(
Object:
  Heterogeneous list: forms Heist Scheme's AST. Linked list under the hood. 
  Formed by nested pairs terminating with '(). Construct pairs with "cons",
  access the head with "car" & tail with "cdr". Set the head with "set-car!" 
  and set the tail with "set-cdr!"

    ; All of the following create the same list:
    (list 1 2 3)                   ; creation via primitive
    '(1 2 3)                       ; literal syntax
    (cons 1 (cons 2 (cons 3 '())))
    '(1 (2 (3 . ())))

  Historically, CAR stands for "Contents of the Address of the Register" and 
  CDR for "Contents of the Decrement of the Register".

Procedure:
  Returns a new list with all the given args. Returns '() w/o args.
)",





}, {
"vector",
"Object (Reference Semantics) | Procedure",
R"(
(vector <object> ...)
(vector)
)",
R"(
Object:
  Heterogeneous vector. std::vector under the hood.

    ; Both of the following create the same vector:
    (vector 1 2 3)
    '#(1 2 3)

Procedure:
  Returns a new vector with all the given args. Returns '#() w/o args.
)",






}, {
"hmap",
"Object (Reference Semantics) | Procedure",
R"(
(hmap <key1> <val1> ...)
(hmap)
)",
R"(
Object:
  Heterogeneous hash-map. std::unordered_map under the hood.
  Keys must be a <symbol>, <string>, <number>, <character>, or <boolean>.
  Values can be of any type.
    
    ; Both of the following create the same hmap:
    (hmap 'a 1 'b 2 'c 3)
    '$(a 1 b 2 c 3)

Procedure:
  Returns a new hmap with all the given args. Returns '$() w/o args.
)",





}, {
"pair",
"Object (Reference Semantics)",
R"()",
R"(
Heterogeneous pair literal. std::pair under the hood. Nesting these
with right-associativity and a terminating '() forms a 'proper list'.
Created via 'dotted' literals or the "cons" primitive.

  ; Both of the following create the same pair:
  (cons 1 2)
  '(1 . 2) ; NOTE: the "dot" character here can be changed via "set-dot!"
           ;       and checked via "dot" (both are primitive functions).

  ; The below forms a so-called 'dotted list' since it doesn't end in '():
  '(1 . (2 . (3 . 4)))
  (cons 1 (cons 2 (cons 3 4)))
)",





}, {
"alist",
"Object (Reference Semantics)",
R"()",
R"(
Associative list (list of lists of length 2). 

Most Schemes use such as a replacement for hash-maps, though alists inherently
have O(n) access. Heist Scheme has true hash-maps (denoted as "hmap") with O(1)
access, so the need for alists is reduced. 

However, alists are still nice to have when one desires to treat them as sequences 
(which hmaps are NOT) as alists can use all of the generic algorithmic primitives.
)",


/******************************************************************************
* NON-LITERAL OBJECT DESCRIPTIONS @NEW-SECTION
******************************************************************************/


}, {
"input-port",
"Object (Reference Semantics)",
R"()",
R"(
Input file handle. Created by "open-input-file" or "call-with-input-file". 
Close via "close-port". FILE* in "r" mode under the hood. Used with:

  read
  read-string
  read-line
  read-char
  peek-char
  char-ready?
  slurp-port
  read-port

Get the current implicit input-port w/ "current-input-port" (stdin by default).
  *) Sessions automatically close remaining open ports upon a successful exit.
)",





}, {
"output-port",
"Object (Reference Semantics)",
R"()",
R"(
Output file handle. Created by "open-output-file" or "call-with-output-file". 
Close via "close-port". FILE* under the hood. Set mode via: 
  
  open-output-file  ; only works if given file doesn't exist
  open-output-file! ; erase file if exists then call "open-output-file"
  open-output-file+ ; open file in "append" mode

Used with:

  write
  display
  pretty-print
  newline
  write-char
  writef
  displayf
  pretty-printf

Get the current implicit output-port w/ "current-output-port" (stdout by default).
  *) Sessions automatically close remaining open ports upon a successful exit.
)",





}, {
"object",
"Object (Reference Semantics)",
R"()",
R"(
Compound object derived from a class prototype. Created via a constructor, 
access its properties via "." notation or the ".." primitive.

Analyze objects via the "object-members" & "object-methods" primitives, &
dynamically add properties to objects via ".add-property!"
methods available to all objects by default.

  => NOTE: Dynamically added properties to an object ONLY CHANGE THAT 1 OBJECT

To access the "name" member of a "person" object, we write: person.name
To invoke the "greet" method of a "person" object, we write: (person.greet)
We can mutate object members via "set!": (set! person.name "Jordan")
)",





}, {
"class-prototype",
"Object (Reference Semantics)",
R"()",
R"(
Meta-objects outlining structure of objects. Enables single-inheritance,
members, methods, a ctor, and limited polymorphism. Created via "defclass".

Analyze prototypes via the "proto-name" "proto-members" "proto-methods" & 
"proto-super" primitives. Dynamically add properties to prototypes via the 
"proto-add-property!" primitive.

  => NOTE: Dynamically added properties to a prototype will give all new 
           AND ALL EXISTING objects of said prototype access to said property 
           too!

NOTE: "coroutine" is a class prototype under the hood, and "define-coroutine"
      simply serves to manipute the coro body & use it generate a coroutine
      object, which is then passed back to the user to control coro execution!

NOTE: "universe" is also a class prototype under the hood, though it leverages
      a reserved primitive in order to achieve its sandboxing of environments.
)",





}, {
"procedure",
"Object (Reference Semantics)",
R"()",
R"(
All procedures are closures in Heist Scheme. Created via "lambda" or "fn", with 
"lambda" being more efficient than "fn", but "fn" being more dynamic than 
"lambda".

"lambda" accepts 0+ args denoted by symbolic parameters. Accept n args via 
"dot-notation" (change the "dot" character via "set-dot!"). Accept optional 
values for args via '()' notation, though these become "fn"s under the hood:
  
  (lambda (a b c) <body>)   ; accepts exactly 3 args
  (lambda (x . xs) <body>)  ; all args after "x" become a list called "xs"
  (lambda (a (b 1)) <body>) ; "b" defaults to 1 if only "a" given

"fn"s are essentially multi-arity pattern-matching lambdas:

  (define factorial
    (fn
      ((n) (factorial n 1))                 ; called only if given 1 arg 
      ((0 p) p)                             ; called if given 0 and any arg
      ((n p) (factorial (- n 1) (* n p))))) ; called if given any 2 args

CPS Transformation:
  *) If you use "cps-quote" you won't see this expansion, rather there will be a 
     "heist:core:app-cps" tag prefixing the application in order to denote where 
     to cps-transform in the future should the application be of a macro.

  (<procedure> <arg1> <arg2> ... <argN>)
  ; BECOMES
  (lambda (k)
    ((cps-transform <procedure>)
      (lambda (procedure-value)
        ((cps-transform <arg1>)
          (lambda (arg1-value)
            ((cps-transform <arg2>)
              (lambda (arg2-value)
                ... ; continue transform-lambda pattern here for each <arg> value
                  ((cps-transform <argN>)
                    (lambda (argN-value)
                      (procedure-value arg1-value arg2-value ... argN-value k))))))))))
)",





}, {
"void",
"Object (Value Semantics) | Procedure",
R"(
(void)
)",
R"(
Object:
  Represents an empty value -- often returned by mutative actions like "set!",
  "set-car!", "set-cdr!", etc.

Procedure:
  Generates a "void" object.
)",





}, {
"undefined",
"Object (Value Semantics) | Procedure",
R"(
(undefined)
)",
R"(
Object:
  Represents an undefined value: allows error signal w/o having to "jump!" anything.

Procedure:
  Generates an "undefined" object.
)",


/******************************************************************************
* COMMAND-LINE DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"command-line",
"Procedure",
R"(
(command-line)
)",
R"(
Returns Heist Scheme's cmd-line options as a string:

  Interpret Script: -script <script-filename> <argv1> <argv2> ...
  Compile Script: -compile <script-filename> <optional-compiled-filename>
  Load Script: -l <script-filename>
  Infix Operators: -infix
  With CPS Evaluation: -cps
  Disable ANSI Colors: -nansi
  Case Insensitivity: -ci
  Dynamic Call Trace: -dynamic-call-trace
  Trace Call Args: -trace-args
  Stack Trace Size: -trace-limit <non-negative-integer>
  Interpreter Version: --version
  Show These Options: --help

Reader changes with "-compile":
  *) Compilation replaces interpreter's reader, hence:
     0. Reader-modifying operations must be done in a seperate file and linked with "-l"!
        => These include "infix!", "infixr!", "unfix!", "define-reader-syntax", & "define-reader-alias"!
        => IE: "$ heist -l reader_modifications.scm -compile file_to_compile.scm"

Quirks to account for with "-cps":
  *) Wraps scheme code in a "scm->cps" block automatically, hence:
     0. Reader-modifying operations must be done in a seperate file and linked with "-l"!
        => These include "infix!", "infixr!", "unfix!", "define-reader-syntax", & "define-reader-alias"!
        => IE: "$ heist -cps -l reader_modifications.scm -script file_to_interpret.scm"
     1. Affects the REPL, "-script", and "-compile"!
        => Use with the REPL wraps every expression in a unique "scm->cps" block!

Note that the "-infix" cmd-line flag defines the following:

  .-------.----------------------------------.-------.----------------------------------------.
  | Order |            Operators             | Assoc |                Effects                 |
  | ----- | -------------------------------- | ----- | -------------------------------------- |
  |   10  | :                                | Right | functional composition                 |
  |    9  | **                               | Right | expt                                   |
  |    8  | * / % // mod                     | Left  | *, /, remainder, quotient, modulo      |
  |    7  | + -                              | Left  | addition, subtraction                  |
  |    6  | :: @                             | Right | cons, append                           |
  |    5  | > < >= <=                        | Left  | gt, lt, gte, lte                       |
  |    4  | == !=                            | Left  | eq, neq                                |
  |    3  | &&                               | Left  | and                                    |
  |    2  | ||                               | Left  | or                                     |
  |    1  | ->                               | Left  | lambda                                 |
  |    0  | = <- **= *= /= %= //= mod= += -= | Right | define, set!, set! ** * / % // mod + - |
  ^-------^----------------------------------^-------^----------------------------------------^
)",

/******************************************************************************
* INTRO TOPICS DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"macro-system",
"Topic",
R"()",
R"(
One of Scheme's most powerful features is its flexible run-time macro system!

0. Metaprogramming advantages:
   *) Code is data (parentheses construct an Abstract Syntax Tree)
      => Hence Macro System enables direct manipulation of the AST
      => Quotation (quote) Converts Code to Data, Eval (eval) Converts Data to Code
      => Reader (read) takes input and parses it into a quoted list of symbolic data
         -> Hence read and eval may be combined for a custom repl!

1. For those in the know:
   *) While R5RS+ Scheme supports hygienic macros, R4RS (Heist Scheme's base) makes this optional.
   *) Unhygienic macros were selected after experimenting with CL, Clojure, & Scheme, finding:
      => Hygiene's pros are easier to emulate w/o it than non-hygiene's pros are to emulate with hygiene
      => Forsaking hygiene enables more extensive control when meta-programming

2. Macros are identical to procedures, except for 3 key features:
   *) They expand into new code that will be run in the current scope, rather than
      processing a computation in a new scope (that of their definition, as with procedures)
      => Built-in syntax-hash mechanism makes avoiding namespace conflicts trivial!
      => Macro argument names are automatically hashed to become unique symbols!
   *) They do not evaluate their arguments (unlike procedures)
      => Hence macros can accept, and expand into, arbitrary code and data patterns!
   *) They do NOT have a recursive expansion limit (as does procedural non-tail-recursion)
      => Hence recursive expansions MAY cause a segmentation fault if they infinitely expand
         -> NOTE: Such is an indication of a USER error however, and NOT an interpreter error!
)",





}, {
"summary",
"Topic",
R"()",
R"(
Heist is loosely/dynamically typed, properly tail-recursive, 
and may be embedded in C++17!

Heist reserves the "heist:" symbol prefix for internal use!

Heist limits non-tail recursion to depth of 1000 by default, but 
this may be altered via the "set-max-recursion-depth!" primitive.

See more in the other "topics" entries of the '(help)' menu!
)",





}, {
"conventions",
"Topic",
R"()",
R"(
Common Conventions:
  0. '?' suffix denotes a predicate procedure
  1. '!' suffix denotes a mutative (non-purely-functional) procedure
  2. '(', '[', & '{' are interchangeable (as are ')', ']', & '}')
     *) Note: {} can also force precedence with infix operators!
  3. 'procedure' is said instead of 'function'
  4. '#it' refers to the REPL's last evaluated expression
)",





}, {
"notation",
"Topic",
R"()",
R"(
Function (or "procedure") calls are denoted by parens:
  *) in C++:          myFunc(0,'a',"hello")
  *) in Heist Scheme: (myFunc 0 #\a "hello")

Nearly every character (except '.') can be used in a variable name!
  *) Unless, of course, the combination could be interpreted as a
     primitive data type (ie '1000' is an invalid variable name)
  *) Hence can do things like name a factorial function '!' as if it were a primitive!
  *) This excludes '.' though, given it denotes property access for objects
)",





}, {
"namespacing",
"Topic",
R"()",
R"(
Key Notes on Namespacing in Heist:
  0. Heist is a Lisp 1: variables & procedures share a single namespace.
  1. "core-syntax" is evaluated first & MUST be matched (unlike runtime macros from "define-syntax").
  3. Runtime macros & variables are in different namespaces.
     *) Hence if a runtime macro's pattern doesn't match, it gets treated as an attempted procedure call!
)",


/******************************************************************************
* COMMENT DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"comments",
"Topic",
R"()",
R"(
Single-line comments: denoted by ";"
Multi-line comments: opened by "#|" & closed by "|#"
)",


/******************************************************************************
* CPS DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"cps",
"Topic",
R"()",
R"(
A style of programming which explicitly handles control flow via 'continuations',
where a 'continuation' represents the rest of the work to be done in a program.

Programming with and manipulating continuations can yield certain advantages,
most notably the ability to implement many control flow operations in terms
of continuations (including threads, coroutines, try-catch, arbitrary returns, 
goto, etc.)

Unfortunately, explicitly programming with continuations is rarely desirable 
and hardly enjoyable. Fortunately, there are ways to convert any program into 
CPS, and Scheme as a language has this transformation baked in by default.

The power of continuations in Scheme may be leveraged through the primitive 
"call/cc" procedure: taking an unary procedure as its argument, "call/cc" 
(or "call-with-current-continuation") passes the current continuation as an 
argument to the function it received.

And yet, continuations pose certain penalties incurred by the transformation 
process, and as such some believe they should be removed from the Scheme standard 
altogether. Heist Scheme, in an effort to reconcile these perspectives, offers 
'opt-in' CPS tranformations by using the intrinsic "scm->cps" macro to transform 
code blocks into CPS & the "-cps" cmd-line flag to transform entire programs at 
the user's behest.

As such, Heist programs may get the efficiency of not using continuations by 
default, then activate CPS transformations for their benefits as needed. However, 
this means that primitives such as "call/cc" may only be validly used in the 
scope of a "scm->cps" block or when using the "-cps" cmd-line flag. Other 
primitives of this nature include:
  
  0. "load" alternative in "scm->cps" blocks: "cps-load"
  1. "eval" alternative in "scm->cps" blocks: "cps-eval"
  2. "compile" alternative in "scm->cps" blocks: "cps-compile"
)",


/******************************************************************************
* *DOT* SYNTAX DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"*dot*",
"Topic | Syntax",
R"()",
R"(
Syntax alias for the current value of '(dot)'. Designed for use with macros!
Enables guarenteed effect of expansion regardless of the current "dot" symbol.

Example:
  
  (core-syntax Make-Fcn
    (syntax-rules ()
      ((_) 
        (lambda (. args) args)))) ; "." is the default dot to denote variadics
  (set-dot! ':)
  ((Make-Fcn) 1 2 3 4 5) ; ERROR TOO MANY ARGS ("." is now a parameter name!)

  (core-syntax Make-Fcn
    (syntax-rules ()
      ((_) 
        (lambda (*dot* args) args)))) ; *dot* always denotes a variadic/pair-literal!
  (set-dot! ':)
  ((Make-Fcn) 1 2 3 4 5) ; (1 2 3 4 5)
)",


/******************************************************************************
* SPECIAL FORM DESCRIPTIONS @NEW-SECTION
******************************************************************************/


}, {
"quote",
"Special Form",
R"(
(quote <datum>)
'<datum>
)",
R"(
Why LISP code IS data! Quote literals to convert them to data equivalents:
  
  Proper List: '(<obj> ...) => (list '<obj> '...)
  Dotted List: '(<obj1> ... <objN> . <objN+1>) => (append '(<obj1> ... <objN>) <objN+1>)
  Empty List:  '()
  Vector:      '#(<obj1> ...) => (vector '<obj1> '...)
  Hash-Map:    (quote $(<key> <val> ...)) => (hmap '<key> '<val> '...)
  Syntax:      (quote <syntax>) => <syntax-as-symbol>
  Else:        (quote <any-other-obj>) => <any-other-obj>

Quote becomes VERY powerful when combined with "eval" (converts data to code),
especially in macros. IE: convert code to data via QUOTE, manipulate the data,
then convert the data back to code via EVAL. Allows a program to edit itself by
dynamically adding to the interpreted AST at runtime!

CPS Transformation:
  (quote <datum>)
  ; BECOMES
  (lambda (k) (k (quote <datum>)))
)",





}, {
"quasiquote",
"Macro",
R"(
(quasiquote <datum>)
`<datum>
)",
R"(
Includes 2 sub-special forms: (unquote <datum>) (unquote-splicing <datum>)
  
  0. (unquote <datum>)          ; ,<datum>
  1. (unquote-splicing <datum>) ; ,@<datum>

Works exactly like quote, except DOESN'T quote any datums wrapped in "unquote",
and 'unwraps parenthesis' while not quoting any datums wrapped in "unquote-splicing".

  (define l (list 1 2 3))
  `(l l l)       ; equivalent to: (list 'l 'l 'l)
  `(,l ,l ,l)    ; equivalent to: (list (list 1 2 3) (list 1 2 3) (list 1 2 3))
  `(,@l ,@l ,@l) ; equivalent to: (list 1 2 3 1 2 3 1 2 3)
)",





}, {
"lambda",
"Special Form",
R"(
(lambda (<arg> ...) <body> ...)
)",
R"(
Create an anonymous procedure!
  *) See "fn" for a multi-arity pattern-matching alternative!

Pass a variadic number of args (0+) by using ".":
  *) The variadic args-list name must always be the last arg!
  *) The "." symbol to denote variadics can be changed via "set-dot!"!
  *) The current "." symbol denoting variadics can always be aliased by "*dot*"!

  (lambda (. va-args-list) <body> ...)       ; OK
  (lambda (a b . va-args-list) <body> ...)   ; OK
  (lambda (a b . va-args-list c) <body> ...) ; ERROR: Variadic Arg Name Isn't Last!

Assign default values to arguments by using ():
  *) Mandatory parameters must precede optional ones!

  (lambda (a (b 1) (c 2)) <body> ...)          ; OK, b & c have default values!
  (lambda (a (b 1) . va-args-list) <body> ...) ; OK, has both optionals & variadics!
  (lambda ((b 1) a . va-args-list) <body> ...) ; ERROR: a MUST precede optional b!
  (lambda (a b . (va-args-list 1)) <body> ...) ; ERROR: variadics CAN'T have defaults!

Reader Shorthand: "\<expr>"
  *) Use "%n" to refer to the nth argument (1-indexed so "%1" is the 1st arg)
  *) Use "%%" to refer to a variadic arg (hence "list" is equivalent to "\%%")

CPS Transformation:
  *) If multiple <body> expressions, wrap them in a "begin" block!
  *) Continuation *here* at the end of the parameter list necessitates interpreter 
     be designed to handle variadic params w/ an extra continuation param at the end!

  (lambda (<arg> ...) <body>) 
  ; BECOMES
  (lambda (k)
    (k (lambda (<arg> ... k2) ; *here*
         ((cps-transform <body>)
           k2))))
)",





}, {
"fn",
"Special Form",
R"(
(fn ((<arg> ...) <body> ...) ...)
)",
R"(
Create an anonymous multi-arity pattern-matching procedure!
  *) See "lambda" for a light-weight single-arity alternative!
  *) See "defn" for a macro combining "define" & "fn"!

Pass a variadic number of args (0+) by using "." (like "lambda"!)
  *) The "." symbol to denote variadics can be changed via "set-dot!"!
  *) The current "." symbol denoting variadics can always be aliased by "*dot*"!

Pattern-match against containers by using literal syntax!
  *) Like "syntax-rules", write more restrictive patterns first!
  *) Boolean literals match based on TRUTHINESS rather than type!
  *) Match against symbol literals by using "quote"!

Examples:

  (define list-map
    (fn ((f ()) '()) ; match against nil
        ((f (x . xs)) (cons (f x) (list-map f xs))))) ; match & unpack pair


  (define factorial
    (fn ((n) (factorial n 1))
        ((0 p) p) ; 0 is more restrictive than 'n', so place 1st!
        ((n p) (factorial (- n 1) (* n p)))))


  (define bad-bool-match
    (fn ((#t) "true")
        ((1)  "one"))) ; NEVER TRIGGERED: 1 is "truthy" & hence matches #t!

  (define gud-bool-match
    (fn ((1)  "one") ; place more restrictive 1 literal first!
        ((#t) "true")))

  (bad-bool-match 1)  ; "true"
  (bad-bool-match #t) ; "true"
  (gud-bool-match 1)  ; "one"
  (gud-bool-match #t) ; "true"

CPS Transformation:
  *) If multiple <body> expressions, wrap them in a "begin" block!
  *) Continuation *here* at the end of the parameter list necessitates interpreter 
     be designed to handle variadic params w/ an extra continuation param at the end!

  (fn ((<arg> ...) <body>) ...)
  ; BECOMES
  (lambda (k)
    (k (fn ((<arg> ... k2) ; *here*
            ((cps-transform <body>) k2)) ...)))
)",





}, {
"define",
"Special Form",
R"(
(define <var-name> <value>)
(define (<procedure-name> <arg1> <arg2> ...) <body> ...)
)",
R"(
Bind a syntactic label to a value! 
Special case for object properties: 

  (define <object.name> <value>)
  ; BECOMES
  (object.add-property! (quote <name>) <value>)

Procedure definition expands to a lambda binding:
  
  (define (<procedure-name> <arg> ...) <body> ...)
  ; BECOMES
  (define <procedure-name> (lambda (<arg> ...) <body> ...))

CPS Transformation:
  *) If multiple <rest-of-code> expressions, wrap them in a "begin" block!
  *) The need to know symbol-name *here* is why macro-expansion/eval/load 
     can't leak definitions to the current environment in CPS!
  
  (define <var> <val>) <rest-of-code> 
  ; BECOMES
  (lambda (k)
    ((lambda (<var>) ; *here*
      ((cps-transform <val>)
        (lambda (val-value)
          (set! <var> val-value)
          ((cps-transform <rest-of-code>) k))))
    #f))
)",





}, {
"set!",
"Special Form",
R"(
(set! <var-name> <new-value>)
)",
R"(
Set a syntactic label to a new value (must have been "define"d)!
Special case for object properties: 

  (set! <object.name> <value>)
  ; BECOMES
  (object.set-property! (quote <name>) <value>)

CPS Transformation:
  (set! <var> <val>)
  ; BECOMES
  (lambda (k)
    ((cps-transform <val>)
      (lambda (val-value)
        (k (set! <var> val-value)))))
)",





}, {
"defined?",
"Special Form",
R"(
(defined? <symbol>)
)",
R"(
Determine if a symbol is "define"d!

Given an "object" property-access symbol, returns whether the property exists!
Use "runtime-syntax?", "core-syntax?", & "reader-syntax?" to check for macros!

WARNING: This is NOT the inverse of the "undefined?" primitive!
         "undefined?" checks values, "defined?" checks the environment!

Example:

  (defined? a)   ; #f ; 'a' was never registered in the environment!
  (undefined? a) ; ERROR: 'undefined?' operates on values, and 'a' has none in the environment!

  (define a 12)
  (defined? a) ; #t
  (set! a (undefined))

  (defined? a)   ; #t ; '(undefined)' is a valid value type assigned to 'a' in the environment!
  (undefined? a) ; #t ; 'undefined?' checks values!

CPS Transformation:
  (defined? <symbol>)
  ; BECOMES
  (lambda (k) (k (defined? <symbol>)))
)",





}, {
"delete!",
"Special Form",
R"(
(delete! <symbol>)
)",
R"(
Unbind a Symbol if "define"d!

Given an object property-access symbol, removes the property from the object!
  *) If property is in object's prototype, next access re-caches a copy in the object!

Examples:

  (define a 12)
  (display a) ; 12
  (delete! a) ; unbind <a>
  (display a) ; ERROR => UNBOUND a

  (defclass C () (val 12))
  (define c (new-C))
  (define c.val2 13) ; dynamically add <val2> member to <c>
  (set! c.val 14)    ; update <c.val> value
  (display c.val)    ; 14
  (display c.val2)   ; 13
  (delete! c.val)
  (delete! c.val2)
  (display c.val)    ; 12 [re-cached in <c> from prototype <C>]
  (display c.val2)   ; ERROR => <.val2> NOT A PROPERTY OF <c>

CPS Transformation:
  (delete! <symbol>)
  ; BECOMES
  (lambda (k) (k (delete! <symbol>)))
)",





}, {
"defn",
"Macro",
R"(
(defn <procedure-name> ((<arg> ...) <body> ...) ...)
)",
R"(
Combines "define" and "fn"!
)",





}, {
"begin",
"Special Form",
R"(
(begin <expr> ...)
)",
R"(
Sequentially evaluate expressions IN THE CURRENT ENVIRONMENT!
  *) Helps fit multiple expressions somewhere only expecting 1 (see "if")
  *) Unlike '(let () <expr> ...)', DOESN'T create a new environment frame!

CPS Transformation:
  (begin <expr1> <expr2> <expr3> ... <exprN>)
  ; BECOMES
  (lambda (k)
    ((cps-transform <expr1>)
      (lambda (ignore)
        ((cps-transform <expr2>)
          (lambda (ignore) 
            ((cps-transform <expr3>) 
              (lambda (ignore) 
                ... ; continue transform-ignore pattern here for each begin <expr>
                  ((cps-transform <exprN>) k))))))))
)",





}, {
"if",
"Special Form",
R"(
(if <condition> <consequent> <alternative>)
(if <condition> <consequent>)
)",
R"(
Conditional branching! 
  *) Use "begin" for multiple <consequent> and/or <alternative> expressions!
  *) Only #f is falsey (by default) in Heist Scheme:
     => See "set-falsey!" and "set-truthy!" to change this!

     (if  0  "true!" "false!") ; "true!"
     (if '() "true!" "false!") ; "true!"
     (if  #f "true!" "false!") ; "false!"

CPS Transformations:
  (if <condition> <consequent> <alternative>)
  ; BECOMES
  (lambda (k) 
    ((cps-transform <condition>)
      (lambda (condition-value)
        (if condition-value
            ((cps-transform <consequent>) k)
            ((cps-transform <alternative>) k)))))

  (if <condition> <consequent>)
  ; BECOMES
  (lambda (k) 
    ((cps-transform <condition>)
      (lambda (condition-value)
        (if condition-value
            ((cps-transform <consequent>) k)
            (k (void))))))
)",





}, {
"and",
"Macro",
R"(
(and <exp> ...)
)",
R"(
Confirm all expressions aren't "#f"!
Derivation Using if:

  (and <exp1> <exp2> <exp3> <exp4>)
  ; BECOMES
  (if <exp1> (if <exp2> (if <exp3> <exp4> #f) #f) #f)
)",





}, {
"or",
"Macro",
R"(
(or <exp> ...)
)",
R"(
Confirm 1 expression isn't "#f"!
Derivation Using if:
  
  (or <exp1> <exp2> <exp3> <exp4>)
  ; BECOMES
  (let ((or-val <exp1>)) ; bind value to prevent 2x eval from test & branch
    (if or-val
        or-val
        (let ((or-val <exp2>))
          (if or-val
              or-val
              (let ((or-val <exp3>))
                (if or-val
                    or-val
                    <exp4>))))))
)",





}, {
"cond",
"Macro",
R"(
(cond <clause1> <clause2> ...)
<clause> ::= (<condition> <exp1> <exp2> ...)
)",
R"(
Concise if-else chains!
Using "else" as the last clause's condition is equivalent to using "#t".
Use "=>" to apply the result of the condition to a callable.
Derivation using "if":

  (cond (<condition1> <exp1> ...)
        (<condition2> <exp2> ...)
        (<condition3> => <callable>)
        (else <exp3> ...))

  ; BECOMES

  (if <condition1>
      (begin <exp1> ...)
      (if <condition2> 
          (begin <exp2> ...)
          (let ((cond-result <condition3>))
            (if cond-result
                (<callable> cond-result)
                (begin <exp3> ...)))))
)",





}, {
"case",
"Macro",
R"(
(case <key> <clause1> ... <clauseN>)
<clause> ::= ((<match1> ... <matchN>) <exp1> ... <expN>)
)",
R"(
Switch-statement equivalent!
Using "else" as the last clause's condition is equivalent to using "#t".
Derivation using "cond":

  (case <key> 
    ((<val1> ...) <exp1> ...)
    ((<val2> <key> <val3> ...) <exp2> ...)
    ((<val4> ...) => <callable>)
    (else <exp3> ...))

  ; BECOMES

  (cond ((memv <key> (list <val1> ...)) <exp1> ...) ; See the "memv" primitive!
        ((memv <key> (list <val2> <key> <val3> ...)) <exp2> ...)
        ((memv <key> (list <val4> ...)) => <callable>)
        (else <exp3> ...))
)",





}, {
"let",
"Macro",
R"(
(let (<arg-binding1> ... <arg-bindingN>) <body> ...)        ; nameless
(let <name> (<arg-binding1> ... <arg-bindingN>) <body> ...) ; named
<arg-binding> ::= (<name> <value>)
)",
R"(
Temporary bindings in a new scope!
Derivations using "lambda":

  ; NAMELESS
  (let ((<name> <value>) ...) <body> ...)
  ; BECOMES
  ((lambda (<name> ...) <body> ...) <value> ...)

  ; NAMED
  (let <procedure-name> ((<name> <value>) ...) <body> ...)
  ; BECOMES
  (let () 
    (define <procedure-name>
      (lambda (<name> ...) <body> ...))
    (<procedure-name> <value> ...))
)",





}, {
"let*",
"Macro",
R"(
(let* (<arg-binding1> ... <arg-bindingN>) <body> ...)
<arg-binding> ::= (<name> <value>)
)",
R"(
"let" with bindings in terms of one another!
Derivation using "let":

  (let* ((<name1> <value1>) 
         (<name2> <value2>) 
         (<name3> <value3>))
    <body> ...)

  ; BECOMES

  (let ((<name1> <value1>))
    (let ((<name2> <value2>))
      (let ((<name3> <value3>))
        <body> ...)))
)",





}, {
"letrec",
"Macro",
R"(
(letrec (<arg-binding1> ... <arg-bindingN>) <body> ...)
<arg-binding> ::= (<name> <value>)
)",
R"(
"let" with recursive bindings! Derivation using "let":

  (letrec ((<name> <value>) ...) <body> ...)
  ; BECOMES
  (let ((<name> #f) ...)
    (set! <name> <value>) ...
    <body> ...)
)",





}, {
"letrec*",
"Macro",
R"(
(letrec* (<arg-binding1> ... <arg-bindingN>) <body> ...)
<arg-binding> ::= (<name> <value>)
)",
R"(
"letrec" with bindings in terms of one another!
Derivation using "letrec":

  (letrec* ((<name1> <value1>) 
            (<name2> <value2>) 
            (<name3> <value3>))
    <body> ...)

  ; BECOMES

  (letrec ((<name1> <value1>))
    (letrec ((<name2> <value2>))
      (letrec ((<name3> <value3>))
        <body> ...)))
)",





}, {
"do",
"Macro",
R"(
(do ((<var> <initial-val> <update>) ...)
  (<break-test> <return-exp1> <return-exp2> ...) ; optional returns (<void> by default)
  <body> ...)
)",
R"(
Recursive iteration construct!
Derivation using "letrec":

  (do ((<var> <initial-val> <update>) ...)
      (<break-test> <return-exp1> <return-exp2> ...)
      <body> ...)

  ; BECOMES

  (letrec ((<INTERNAL-RESERVED-NAME>
            (lambda (<var> ...)
              (if <break-test>
                  (begin <return-exp1> <return-exp2> ...)
                  (begin 
                    <body> ...
                    (set! <var> <update>) ...
                    (<INTERNAL-RESERVED-NAME> <var> ...))))))
          (<INTERNAL-RESERVED-NAME> <initial-val> ...))
)",





}, {
"while",
"Special Form",
R"(
(while (<test> <return-exp> ...) <body> ...) ; optional returns (<void> by default)
)",
R"(
True iteration construct!
  *) WARNING: degrades to "do" in cps contexts!
  *) Use "*condition*" as an alias for the current condition!
  *) Uses a true C++ "while" under the hood (no recursion overhead)!

Examples:

  (define x 0)
  (while ((< x 10))
    (set! x (+ x 1))
    (if (= x 5)
        (set! *condition* #f))) ; set condition to false w/o modifying "x"!
  (display x) ; 5

  (define x 0)
  (while ((< x 10)) (set! x (+ x 1)))
  (display x) ; 10 (as expected)
)",





}, {
"for",
"Macro",
R"(
(for ((<var> <initial-val> <update>) ...)
  (<break-test> <return-exp1> <return-exp2> ...) ; optional returns (<void> by default)
  <body> ...)
)",
R"(
True iteration construct!
Identical interface as "do", but expands to "while"!
Derivation using "while":

  (for ((<var> <initial-val> <update>) ...)
       (<break-test> <return-exp1> <return-exp2> ...)
       <body> ...)

  ; BECOMES

  (let ()
    (define <var> <initial-val>) ...
    (while ((not <break-test>) <return-exp1> <return-exp2> ...)
      <body> ...
      (set! <var> <update>) ...))
)",





}, {
"delay",
"Special Form",
R"(
(delay <expr>)
)",
R"(
Delay an expression's evaluation by creating a promise!
  *) Force the promise to run its expression via the "force" primitive!
  *) Delayed expressions have "id" bound as their topmost continuation in CPS!
  *) NOTE: unlike most Scheme implementations, Heist's delayed expressions are
           distinct from thunks/procedures (first-class in their own right)!
)",





}, {
"scons",
"Macro",
R"(
(scons <scar-obj> <scdr-obj>)
)",
R"(
Create a "stream" pair!
  *) Stream pairs are regular pairs with delayed "car" and "cdr"!
  *) Allows for infinite lists (see "scar" & "scdr" primitives for access)!

Derivation using "delay" & "cons":

  (scons <scar-obj> <scdr-obj>)
  ; BECOMES
  (cons (delay <scar-obj>) (delay <scdr-obj>))
)",





}, {
"stream",
"Macro",
R"(
(stream <obj> ...)
)",
R"(
Create a "stream" ("stream" is to "scons" as "list" is to "cons")!
Derivation using "scons":

  (stream <obj1> <obj2> <obj3>)
  ; BECOMES
  (scons <obj1> (scons <obj2> (scons <obj3> '())))

Stream Examples:
  
  ;; Generate all powers of 2!
  (define (pows-of n (count 0))
    (scons (expt n count)
           (pows-of n (+ count 1))))

  (define 2-pow
    (let ((s (pows-of 2)))
      (lambda ()
        (define hd (scar s))
        (set! s (scdr s))
        hd)))

  (do ((x 0 (+ x 1))) 
      ((> x 24))
    (display (2-pow)) ; 1 2 4 8 16 32 64 128 256 512 1024 ... 16777216
    (newline))



  ;; Generate all Fibonacci numbers!
  (define fibs (scons 0 (scons 1 (stream-map + fibs (scdr fibs)))))
  (display (stream->list fibs 25)) ; (0 1 1 2 3 5 8 13 21 34 55 89 ... 46368)



  ;; Generate all primes!
  (define (ints-from n)
    (scons n (ints-from (+ n 1))))

  (define (sieve ints)
    (scons (scar ints)
           (sieve (stream-filter \(not-zero? (remainder %1 (scar ints))) (scdr ints)))))

  (define primes (sieve (ints-from 2)))
  (display (stream->list primes 25)) ; (2 3 5 7 11 13 17 19 23 29 ... 97)
)",





}, {
"vector-literal",
"Special Form",
R"(
(vector-literal <obj1> <obj2> <obj3> ...)
)",
R"(
Longhand variant of the "#" vector-literal shorthand!
  *) Hence, like "#", "vector-literal" must be quoted to form a vector object!

Transformation:
  
  '#(<obj1> <obj2> <obj3> ...)
  ; BECOMES
  '(vector-literal <obj1> <obj2> <obj3> ...)
  ; BECOMES
  (vector '<obj1> '<obj2> '<obj3> '...)
)",





}, {
"hmap-literal",
"Special Form",
R"(
(hmap-literal <key1> <val1> ...)
)",
R"(
Longhand variant of the "$" hmap-literal shorthand!
  *) Hence, like "$", "hmap-literal" must be quoted to form a hash-map object!
  *) Keys ::= symbol | string | number | character | boolean

Transformation:
  
  '$(<key1> <val1> <key2> <val2> ...)
  ; BECOMES
  '(hmap-literal <key1> <val1> <key2> <val2> ...)
  ; BECOMES
  (hmap '<key1> '<val1> '<key2> '<val2> '...)
)",





}, {
"define-syntax",
"Special Form",
R"(
(define-syntax <label> <syntax-transformer>)
(let-syntax ((<label> <syntax-transformer>) ...) <body> ...)
(letrec-syntax ((<label> <syntax-transformer>) ...) <body> ...)
)",
R"(
Create a run-time macro (bind a label to a syntax object)!
  *) Run-Time macros are expanded at run-time, ie each time they're invoked!
  *) See "core-syntax" for an analysis-time macro alternative!

Deriving "let-syntax" & "letrec-syntax" via "let":
  
  (let-syntax ((<label> <syntax-transformer>) ...) <body> ...)
  (letrec-syntax ((<label> <syntax-transformer>) ...) <body> ...)

  ; BECOMES (letrec style macro evaluation is default) =>

  (let ()
    (define-syntax <label> <syntax-transformer>) ...
    <body> ...)

CPS Transformation:
  *) If multiple <rest-of-code> expressions, wrap them in a "begin" block!

  (define-syntax <label> <syntax-transformer>) <rest-of-code> 
  ; BECOMES
  (lambda (k)
    (define-syntax <label> <syntax-transformer>)
    ((cps-transform <rest-of-code>) k))
)",





}, {
"syntax-rules",
"Special Form",
R"(
(syntax-rules (<key> ...) <syntax-clause1> <syntax-clause2> ...))
<syntax-clause> ::= (<pattern> <template>)
<pattern> ::= (<any-symbol> <expression-to-match-against>)
<template> ::= <expression-to-expand-into>
<key> ::= <symbolic-literal>
)",
R"(
Create syntax-rules object to be assigned via "core-syntax", "define-syntax", 
"let-syntax", or "letrec-syntax"!
  *) Literals & <key>s in patterns must be matched exactly to expand!
  *) "..." and "syntax-hash" are always reserved <key> names!
  *) Use "*dot*" to alias the current '(dot)' in expansions!

Variadic Matching & Expansion:
  0. For Patterns:
     *) <obj> ... Matches 1 or more entities
     *) (<contents>) ... Matches 1 or more expressions that match <contents>
        => NOTE: Variadic Matches must accompany variadic expansions in the <template>!
     *) Examples:

        a ...             ; Matches 1+ arbitrary objects
        (a b) ...         ; Matches 1+ pairs
        ((a b) (c d)) ... ; Matches 1+ pairs of pairs
        ((a ...) ...)     ; Matches 1+ expressions of 1+ arbitrary objects

  1. For Templates:
     *) <obj> ... Expands 1 or more entities
     *) (<contents>) ... Constructs 1 or more expressions with <contents>
        => NOTE: Variadic Expansions must accompany variadic matches in the <pattern>!
     *) Examples:

        a ...             ; Expands 1+ arbitrary objects
        (a b) ...         ; Constructs 1+ pairs of variadic matches <a> & <b>
        ((a b) (c d)) ... ; Constructs 1+ pairs of pairs of variadic matches <a>, <b>, <c>, & <d>
        ((a ...) ...)     ; Constructs 1+ expressions of 1+ variadic matches <a>

Higher-Order Macro Template Expansion Support:
  *) Writing macros that expand to other macro definitions using ... can cause
     issues, however this can be mediated by escaping nested ... via \...
  *) Example:

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

CPS Transformation:
  (syntax-rules () (<pattern> <template>) ...)
  ; BECOMES
  (lambda (k)
    (k (syntax-rules () (<pattern> <template>) ...)))
)",





}, {
"syntax-hash",
"Special Form",
R"(
(syntax-hash <symbol>)
`@<symbol>
)",
R"(
Hash local macro template identifiers to avoid expansion name conflicts!
  *) ONLY valid in "syntax-rules" templates!
  *) Expander replaces "syntax-hash" expression, & every instance of <symbol>,
     with a hashed version of <symbol> unique to the expansion instance!
     => Similar to "gensym" but specialized for macro expansions!

Example:

  ; Note the name conflict in the following:
  ;   The <a> gets expanded to <b>, but the expansion then reads that <b> as 10
  ;     due to the rebinding of <b> by <let>, thus the result is 20 and not 15

  (define-syntax my-macro
    (syntax-rules ()
      ((_ a) 
        (let ((b 10))
          (+ a b))))) ; expands to (+ b b) => (+ 10 10)

  (define b 5)
  (write (my-macro b)) ; 20


  ; We can resolve this by binding our <b> in the macro to a UNIQUE identifier.
  ;   We COULD solve this using <gensym>:

  (define-syntax my-macro
   (syntax-rules ()
     ((_ a) 
       (eval 
         `(let ((,(gensym) 10)) ; form the expression by splicing in a unique symbol,
             (+ a ,(gensym 1))) ; then evaluate the expression in the local environment
         '*local-environment*))))

  (define b 5)
  (write (my-macro b)) ; 15


  ; HOWEVER, this is a tad verbose for our purposes. Enter <syntax-hash>:
  ;   a FAST alternative to <gensym> specialized ONLY for macro expansions!
  ; => NOTE: we can use the "`@" reader macro to be even more concise!

  (define-syntax my-macro
    (syntax-rules ()
      ((_ a) 
        (let ((`@b 10)) ; `@b => (syntax-hash b) & binds <hashed-b> to 10
          (+ a b)))))   ; expands to (+ b <hashed-b>) => (+ 5 10)

  (define b 5)
  (write (my-macro b)) ; 15
)",





}, {
"core-syntax",
"Special Form",
R"(
(core-syntax <label> <syntax-transformer>)
)",
R"(
Create an analysis-time macro in the GLOBAL scope!

Analysis-time advantanges:
  *) Interpreter's eval seperates expression analysis (declaration) & execution (invocation):
     => "define-syntax" macros, bound to an environment, dynamically expand at run-time
        - Hence run-time macros in a "lambda" body are re-expanded upon every invocation!
     => "core-syntax" macros, only bound to the global environment, expand at analysis-time
        - Hence analysis-time macros in a "lambda" body expand IN THE LAMBDA DECLARATION ONLY ONCE!

Example Runtime Expansion Degradation Risk:
  *) BEST PRACTICE: use "core-syntax" in the GLOBAL SCOPE to avoid the below!
  *) Heist reads, analyzes, and runs each expression individually
  *) Hence reading (define (f) ...) below means the entire expr is analyzed at once,
     but the my-macro core-syntax defn is only registered at run-time!
     => Hence (my-macro 12) is analyzed before "my-macro" is defn'd as core-syntax!
        - Thus (my-macro 12) must be expanded at run-time instead of analysis-time!
     => However (my-macro 13) does expand at analysis-time, since (f) triggered
        "my-macro" to be bound as core-syntax prior analyzing the (define (g) ...) expr!

    (define (f)
      (core-syntax my-macro    ; BINDS my-macro TO THE GLOBAL ENV AS CORE-SYNTAX AT RUNTIME
        (syntax-rules ()
          ((_ a) (* a 2))))
      (my-macro 12))           ; EXPANDS AT RUNTIME SINCE my-macro ISN'T CORE-SYNTAX YET!
    (f)                        ; RUN f TO REGISTER my-macro AS core-syntax IN THE GLOBAL ENV
    (define (g) (my-macro 13)) ; EXPANDS AT ANALYSIS TIME

CPS Transformation:
  *) If multiple <rest-of-code> expressions, wrap them in a "begin" block!

  (core-syntax <label> <syntax-transformer>) <rest-of-code> 
  ; BECOMES
  (lambda (k)
    (core-syntax <label> <syntax-transformer>)
    ((cps-transform <rest-of-code>) k))
)",





}, {
"define-reader-alias",
"Special Form",
R"(
(define-reader-alias <alias-symbol> <name-symbol>)
(define-reader-alias <alias-symbol-to-delete>)
)",
R"(
Define a symbolic alias to be replaced by the reader!
  *) Check for aliases via the "reader-alias?" primitive!
  *) Get all current aliases via the "reader-alias-list" primitive!

WARNING: Reader aliases do NOT recursively expand:

  (define-reader-alias a b)
  (define-reader-alias b +)
  (b 1 2 3) ; 6
  (a 1 2 3) ; ERROR: VARIABLE b IS UNBOUND !!!

CPS Transformations:
  (define-reader-alias <alias-symbol> <name-symbol>)
  ; BECOMES
  (lambda (k) 
    (k (define-reader-alias <alias-symbol> <name-symbol>)))

  (define-reader-alias <alias-symbol-to-delete>)
  ; BECOMES
  (lambda (k) 
    (k (define-reader-alias <alias-symbol-to-delete>)))
)",





}, {
"scm->cps",
"Special Form",
R"(
(scm->cps <exp1> <exp2> ...)
)",
R"(
Convert code to CPS & evaluate the result!
  *) Hence returns an unary procedure, accepting the 'topmost' continuation!
  *) Enables use of "call/cc", "cps-eval" & "cps-load" primitives!
  *) Automatically wraps entire program (& passed id) if -cps cmd-line flag used!
  *) Enables opt-in continuations for their benefits w/o their overhead when unused!
     => Optimizes the cps transformation as well for reasonable speed!
     => In general, scm->cps code at -O3 optimization runs as fast as its non-cps 
        version would at -O0

Danger zone:
  *) With CPS, avoid runtime-macros/eval/load expanding to a "define" in the current envrionment!
     => Lazy expansion breaks this functionality (may expand to localized bindings though!)
     => May use analysis-time macros expanding to "define"s though (hence "defn" etc. are fine)!
  *) CPS procedures applied in non-CPS contexts have <id> bound as their continuation!

Author's Advice:
  *) Experimentally, go wild! 
  *) For practical code, leave "scm->cps" to be used by libraries, & prefer 
     specialized solutions rather than homebrewed alternatives. 
     => IE use "define-coroutine" and the "jump!"/"catch-jump" idiom rather 
        than spinning up your own versions via continuations.
)",





}, {
"cps-quote",
"Special Form",
R"(
(cps-quote <exp>)
)",
R"(
Convert code to data in CPS!
Identical to "quote" after transforming given code into CPS!

NOTE: Applications may have a peculiar tag in front of them, with their
      arguments NOT having been turned into CPS. This is because Heist
      doesn't know whether an application is a macro or a callable at
      'cps-transformation-time', and hence does so at run-time.
      => For callables, cps-transformation precedes execution!
      => For macros, execution precedes cps-transformation (& re-execution)!
)",





}, {
"using-cps?",
"Special Form",
R"(
(using-cps?)
)",
R"(
Determine whether in a "scm->cps" block or "-cps" is active!

CPS Transformation:
  (using-cps?)
  ; BECOMES
  (lambda (k) (k (using-cps?)))
)",





}, {
"curry",
"Macro",
R"(
(curry (<arg1> <arg2> ...) <body> ...)
)",
R"(
Define curriable lambdas with a nicer interface!
  *) Enables trivial means to bind args to values (esp. helps w/ lambda calculus)
  *) NOTE: "curry"s ALWAYS only accept finite numbers of arguments (no variadics)!

Example:

  (define K (curry (a b) a))
  ; the following invocations are identical!
  ((K 1) 2) ; traditional LISP curried call works!   ; => 1
  (K 1 2)   ; Nncer invocation interface also works! ; => 1

  (define Id (curry (a) a))
  (define KI (K Id)) ; binds "Id" as the first arg to "K"!
  ((KI 1) 2)         ; "id" is selected, then 2 is passed to "Id"! ; => 2
  (KI 1 2)           ; => 2
)",





}, {
"defclass",
"Special Form",
R"(
(defclass <class-name> (<optional-inherited-prototype>) <member-or-method-instances>)
=> <member-or-method-instance> ::= (<member-name> <default-value>)
                                 | (<method-name> <procedure-value>)
                                 | ((<method-name> <arg1> <arg2> ...) <body> ...)
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
)",
R"(
Define class prototypes for object-oriented programming!
  *) "defclass" creates a class prototype (think JavaScript) from which objects are made!

Constructor:
  0. User-defined "make-<class-name>" ctor is optional, if undefined will be generated
     *) Generated ctor is either nullary, or accepts a container to initialize member values:
        => container = name-value "hash-map", or value "list"/"vector"!
     *) Default ctor is always available via "new-<class-name>"
  1. Default values from class-prototypes are "deep-copied" to objects upon construction
  2. Can dynamically add properties to prototypes (via "proto-add-method" "proto-add-member")
     which all existing objects also get access to!

Generated Predicate, Setter, & Property Registration:
  0. Class Object Predicate (<class-name>? <obj>) is generated by default
  1. Object member/method property setter method is generated by default:
     *) (<object>.set-property! <property-name-symbol> <new-value>)
     *) This method is automatically invoked when using "set!" on an object property!
        (set! obj.name val) => (obj.set-property! (quote name) val)
2. Object dynamic member/method property registration method is generated by default:
     *) (<object>.add-property! <property-name-symbol> <default-value>)
     *) If member/method exists: sets value, else: adds it as a new property
     *) This method is automatically invoked when using "define" on an object property!
        (define obj.name val) => (obj.add-property! (quote name) val)

Self, Prototype, & Inherited Object Access:
  0. "self" refers to the current invoking object (designed for use in methods)
  1. ".prototype" member returns the class prototype of the object
  2. ".super" member returns object's underlying inherited object (returns "#f" if dne)

Overload Equality, Printing, & Copying:
  0. Equality: "self=" method will attempt to be invoked on objects for "eq?", "eqv?", "equal?"
     *) Method should accept 1 argument to compare equality against!
     *) May also have specific equality polymorphism by naming methods "eq?", "eqv?", "equal?" directly
  1. Printing: "self->string" method will attempt to be invoked on objects for "display", "write", "pprint"
     *) Method should accept 0 arguments, and return a string to be 'displayed'!
     *) May also have specific printing polymorphism by naming methods "display", "write", "pprint" directly
  2. Copying: "self->copy" method will attempt to be invoked on objects for "copy"
    *) Method should accept 0 arguments, and by convention return a new object!
    *) Unlike the above methods, "self->copy" is NOT inherited by default!

Overload Application via Functors:
  0. The "self->procedure" method will automatically be called on any object applied as a procedure!
     *) Think 'operator()()' in C++!
     *) Check out the primitive "functor?" predicate!
     *) Convert functors to procedures via the "functor->procedure" primitive!

Method Access to Object Members:
  0. Similar to C++'s "this", "self" is implicitly passed as a method argument upon invocation
  1. Unlike C++, object members must be referenced via "self.<member>" in methods
     *) Enables methods to also reference external variables with members' names

Value Semantics & Property Access:
  0. Passed by reference (as are strings, pairs, vectors, and hash-maps)
     *) May be deep-copied via "copy" & shallow-copied via "shallow-copy"
  1. Traditional OOP Access, Member: "person.name", Method: (person.greet <friend's name>)
     *) Functional ".." Access: (.. person 'name) & ((.. person 'greet) <friend's name>)
     *) Reader evals property chains as 1 symbol, which are parsed by the core evaluator!

Example:
  
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

CPS Transformation:
  *) If multiple <rest-of-code> expressions, wrap them in a "begin" block!
  *) If multiple <body> expressions, wrap them in a "begin" block!
  *) Dynamically add all non-atomic default-value members *here*!
  *) Dynamically add all non-atomic/non-inlined methods *there*!

  (defclass <prototype-name> (<optional-super-prototype>)
    (<mem1> <atomic-val>)
    (<mem2> <val-expr>)
    ((<method1> <arg> ...) <body>)
    (<method2> <procedure-val-expr>))
  <rest-of-code> 

  ; BECOMES

  (lambda (k)
    (defclass <prototype-name> (<optional-super-prototype>) 
      (<mem1> <atomic-val>) 
      ((<method1> <arg> ... k2) 
        ((cps-transform <body>) k2)))
      ((cps-transform 
        (begin (proto-add-property! <prototype-name> (quote <mem2>) <val-expr>)              ; *here*
               (proto-add-property! <prototype-name> (quote <method2>) <procedure-val-expr>) ; *there*
               <rest-of-code>))
        k))
)",





}, {
"new",
"Macro",
R"(
(new (<property-name> <property-value>) ...)
)",
R"(
Create anonymous objects! 
Overloads "equal?" for structural equality against other anonymous objects!

  (define person1 (new (name "Jordan") (age 21)))
  (define person2 (new (name "Jordan") (age 21)))

  (equal? person1.prototype person2.prototype) ; #f -- anonymous objects have unique proto's
  (equal? person1 person2) ; #t -- "equal?" overloaded to check STRUCTURE & ignore proto's
)",





}, {
"define-coroutine",
"Macro",
R"(
(define-coroutine (<co-name> <arg> ...) <body> ...)
)",
R"(
Initial invocation '(<co-name>)' will yield a "coroutine" object!
  *) Re-invoking '(<co-name>)' will return a new "coroutine" object instance!
  *) Hence "<co-name>" should not be called recursively internally, rather use
     the 'named-let' construct in order to perform recursive operations!

Coroutine Objects:
  *) Creation: Either from invoking '(<co-name>)' or "yield" in a coroutine
  *) 2 Properties, ".value" member & ".next" method:
     => ".value": yielded value ("#f" if object isn't from a "yield")
     => ".next": either starts or continues the coroutine's execution

Associated Special Form:
  *) (yield <value>): yield a value from the coroutine via a new coroutine object!
     => '(yield)' is equivalent to '(yield #f)', designed for use with "cycle-coroutines"!

Danger Zone:
  0. Nesting "define-coroutine" instances (or use in "scm->cps") is undefined behavior!
  1. Using "jump!" or "catch-jump" in "define-coroutine" is undefined behavior (used by "yield")!
  2. The "id" procedure is returned if no expressions exist after the last "yield"!
  3. Like "scm->cps", avoid runtime-macros/eval/load expanding to a "define" in the current environment!

Examples:

  ;; Having 2 coroutines alternate until one completes!

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
)",





}, {
"define-module",
"Macro",
R"(
(define-module <optional-name> (<exposed-procedure-name> ...) <expression> ...)
)",
R"(
Define a module to hide/expose select procedures!
  *) Expose variables/macros by defining them outside of the module body!

Examples:
  
  ; ANONYMOUS MODULE: procedures are directly exposed
  (define-module (greet set-age! set-name!)
    (define name "")                                ; hidden (variable's in the module)!
    (define age 0)                                  ; hidden (variable's in the module)!
    (define (increment-value is-name? val)          ; hidden (procedure not exposed)!
      (if is-name? (set! name val) (set! age val)))
    (define (set-age! a) (increment-value #f a))    ; exposed procedure!
    (define (set-name! n) (increment-value #t n))   ; exposed procedure!
    (define (greet)                                 ; exposed procedure!
      (displayf "Hello! My name is %s and I'm %n years old!" name age)))

  (set-name! "Jordan")
  (set-age! 21)
  (greet) ; Hello! My name is Jordan and I'm 21 years old!


  ; NAMED MODULE: exposed procedures are members of a "module" object!
  (define-module Person (greet set-age! set-name!)
    (define name "")                                ; hidden (variable's in the module)!
    (define age 0)                                  ; hidden (variable's in the module)!
    (define (increment-value is-name? val)          ; hidden (procedure not exposed)!
      (if is-name? (set! name val) (set! age val)))
    (define (set-age! a) (increment-value #f a))    ; exposed procedure!
    (define (set-name! n) (increment-value #t n))   ; exposed procedure!
    (define (greet)                                 ; exposed procedure!
      (displayf "Hello! My name is %s and I'm %n years old!" name age)))

  (Person.set-name! "Jordan")
  (Person.set-age! 21)
  (Person.greet) ; Hello! My name is Jordan and I'm 21 years old!
)",





}, {
"define-overload",
"Macro",
R"(
(define-overload <procedure-name> (<predicate?> <procedure>) ...)
)",
R"(
Access the original overloaded procedure version via "*original*"!
  *) Use "else" as a catch-all predicate!

Examples:
  
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
)",





}, {
"infix!",
"Special Form",
R"(
(infix! <integer-literal> <symbol1> ...)  ; 'define form'
(infixr! <integer-literal> <symbol1> ...) ; 'define form'
(infix! <symbol1> ...)  ; 'query form'
(infixr! <symbol1> ...) ; 'query form'
)",
R"(
Forms:
  0. 'define form' defines <symbol1> ... as infix operators w/ <int-literal> precedence
     => Use "infix!" for left-associativity & "infixr!" for right-associativity!
  1. 'query form' returns precedence level if <symbol1> ... are operators, else returns #f

Forcing Precedence & Preventing Infix->Prefix Reader Conversion:
  *) Force precedence via {} (like ()'s use in most programming languages)!
  *) Escape infix operators from prefix conversion via "#!" prefix (rm'd by reader)!
  *) Prefix/postfix operators are ignored (presumed intentionally placed)!

Examples:
  
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

CPS Transformations:
  (infix! <integer-literal> <symbol1> ...)
  ; BECOMES
  (lambda (k) 
    (k (infix! <integer-literal> <symbol1> ...)))

  (infixr! <integer-literal> <symbol1> ...)
  ; BECOMES
  (lambda (k) 
    (k (infixr! <integer-literal> <symbol1> ...)))
)",





}, {
"unfix!",
"Special Form",
R"(
(unfix! <symbol1> ...)
)",
R"(
Deregister existing infix operators!
Examples:

  (infixr! 5 compose)

  ; (#<procedure>)
  (display (list even? compose length)) 

  (unfix! compose)

  ; (#<procedure even?> #<procedure compose> #<procedure length>)
  (display (list even? compose length))

CPS Transformation:
  (unfix! <symbol1> ...)
  ; BECOMES
  (lambda (k) (k (unfix! <symbol1> ...)))
)",


/******************************************************************************
* PRIMITIVE VARIABLE DESCRIPTIONS @NEW-SECTION
******************************************************************************/


}, {
"#t",
"Object (Value Semantics)",
R"()",
R"(
The "true" boolean literal.
)",





}, {
"#f",
"Object (Value Semantics)",
R"()",
R"(
The "false" boolean literal. Distinct from '() (which is "truthy" by default).
  *) See "set-falsey!" and "set-truthy!" to change this!
)",





}, {
"fl-precision",
"Variable (Number)",
R"()",
R"(
The number of digits of precision used for flonums (inexact numbers).
Bound to "LDBL_DIG" from "#include <cfloat>".
)",





}, {
"fl-min",
"Variable (Number)",
R"()",
R"(
The smallest possible flonum (inexact number) value.
Bound to "LDBL_TRUE_MIN" if exists, else "LDBL_MIN" (both from "#include <cfloat>").
  *) Use "fl-max" for the largest possible flonum!
)",





}, {
"fl-max",
"Variable (Number)",
R"()",
R"(
The largest possible flonum (inexact number) value.
Bound to "LDBL_MAX" from "#include <cfloat>".
  *) Use "fl-min" for the smallest possible flonum!
)",





}, {
"fl-epsilon",
"Variable (Number)",
R"()",
R"(
Represents the smallest "x" so 1.0 + "x" != 1.0
Bound to "LDBL_EPSILON" from "#include <cfloat>".
)",





}, {
"*min-infix-precedence*",
"Variable (Number)",
R"()",
R"(
Minimum valid infix operator precedence. 
Bound to "LLONG_MIN" from "#include <climits>".
  *) Use *max-infix-precedence* for the max valid precedence!
)",





}, {
"*max-infix-precedence*",
"Variable (Number)",
R"()",
R"(
Maximum valid infix operator precedence. 
Bound to "LLONG_MAX" from "#include <climits>".
  *) Use *min-infix-precedence* for the min valid precedence!
)",





}, {
"stream-null",
"Variable (Nil)",
R"()",
R"(
Represents the empty stream (simply an alias for '()).
)",





}, {
"*null-environment*",
"Variable (Symbol)",
R"()",
R"(
Optional environment flag for "eval", "load", "cps-eval", & "cps-load"!
Resolves to itself as a symbol.

"*null-environment*" says to eval the argument in a sandboxed environment,
using only the default Heist bindings without affecting the memory of the 
current process. 

Sandboxes effect of "exit" to just exit the current evaluation process without 
also exiting the calling environment (which would terminate the entire program).

Other possible flags include "*local-environment*" & "*global-environment*"!
)",





}, {
"*local-environment*",
"Variable (Symbol)",
R"()",
R"(
Default environment flag for "eval", "load", "cps-eval", & "cps-load"!
Resolves to itself as a symbol.

"*local-environment*" says to eval the argument in the current environment!

Other possible flags include "*null-environment*" & "*global-environment*"!
)",





}, {
"*global-environment*",
"Variable (Symbol)",
R"()",
R"(
Optional environment flag for "eval", "load", "cps-eval", & "cps-load"!
Resolves to itself as a symbol.

"*global-environment*" says to eval the argument in the global environment!

Other possible flags include "*null-environment*" & "*local-environment*"!
)",





}, {
"*argc*",
"Variable (Number)",
R"()",
R"(
Number of cmd-line arguments passed to the current script.
  *) Use "*argv*" for a list of the args as strings!
)",





}, {
"*argv*",
"Variable (String List)",
R"()",
R"(
List of cmd-line arguments (as strings) passed to the current script.
  *) Use *argc* for the number of arguments given!
)",





}, {
"*heist-platform*",
"Variable (Symbol)",
R"()",
R"(
General current platform name. Possible results: 
  'windows 'apple 'linux 'unix 'posix 'unknown

Use "*heist-exact-platform*" for specified insight about your platform!
)",





}, {
"*heist-exact-platform*",
"Variable (Symbol)",
R"()",
R"(
Exact current platform name. Possible results: 
  'windows-64 'windows-32
  'apple-ios-simulator 'apple-ios 'apple-osx 'apple
  'linux 'unix 'posix
  'unknown

Use "*heist-platform*" for more general insight about your platform!
)",





}, {
"*heist-dirname*",
"Variable (String)",
R"()",
R"(
Filepath string to the Heist Scheme interpreter's directory.
)",





}, {
"*exit-success*",
"Variable (Number)",
R"()",
R"(
Equivalent to 0. Designed to be passed as an argument to "exit".

Possible return value as a SYMBOL by Heist Scheme embedded in C++ when
"exit" is triggered in an "eval" or "apply" code instance.
  *) See "cpp_interop.hpp" for more on embedding Heist in C++!
  *) Other posisble return value is "*exit-failure*".
)",





}, {
"*exit-failure*",
"Variable (Number)",
R"()",
R"(
Equivalent to 1. Designed to be passed as an argument to "exit".

Possible return value as a SYMBOL by Heist Scheme embedded in C++ when
"exit" is triggered in an "eval" or "apply" code instance.
  *) See "cpp_interop.hpp" for more on embedding Heist in C++!
  *) Other posisble return value is "*exit-success*".
)",


/******************************************************************************
* PRIMITIVE FUNCTION DESCRIPTIONS @NEW-SECTION
******************************************************************************/


}, {
"help",
"Procedure",
R"(
(help <query-symbol-or-string>)
(help)
)",
R"(
The obligatory meta entry! Launches this querying mechanism to either:
  0. Search for the description of a specific Heist Scheme feature
  1. Launch the interactive "help" menu (call "help" w/o args)
)",





}, {
"license",
"Procedure | Document",
R"(
(license)
)",
R"(
Displays the terms of use for Heist Scheme to (current-output-port).

Contents:
  MIT License

  Copyright (c) 2020, 2021 Jordan Randleman

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
)",





}, {
"sublime-text-build-system",
"Procedure",
R"(
(sublime-text-build-system)
)",
R"(
Displays the build system for Heist Scheme on Sublime Text to (current-output-port).
)",





}, {
"shell-alias",
"Procedure",
R"(
(shell-alias)
)",
R"(
Displays the "~/.zshrc" or "~/.bashrc" interpreter alias for Heist Scheme to 
(current-output-port).
)",





}, {
"..",
"Procedure",
R"(
(.. <object> <property-symbol-1> ...)
)",
R"(
Functional object property access:

  (eq? person.sibling.age (.. person 'sibling 'age)) ; #t
)",





}, {
"object-members",
"Procedure",
R"(
(object-members <object>)
)",
R"(
Hash-map of <object>'s member names & values.
)",





}, {
"object-methods",
"Procedure",
R"(
(object-methods <object>)
)",
R"(
Hash-map of <object>'s method names & values (with "self" bound!).
)",





}, {
"proto-name",
"Procedure",
R"(
(proto-name <class-prototype>)
)",
R"(
Class name (as a symbol!) of the prototype.
)",





}, {
"proto-members",
"Procedure",
R"(
(proto-members <class-prototype>)
)",
R"(
List of prototype's member names.
)",





}, {
"proto-methods",
"Procedure",
R"(
(proto-methods <class-prototype>)
)",
R"(
List of prototype's method names.
)",





}, {
"proto-super",
"Procedure",
R"(
(proto-super <class-prototype>)
)",
R"(
Pointer to the prototype's inherited prototype (or "#f" if DNE).
)",





}, {
"proto-add-property!",
"Procedure",
R"(
(proto-add-property! <class-prototype> <property-name-symbol> <value>)
)",
R"(
Registers <property-name-symbol> as a property with <value> in <class-prototype>, 
which is then progated to all existing & new objects of the given prototype!
)",





}, {
"coroutine?",
"Procedure",
R"(
(coroutine? <obj>)
)",
R"(
Coroutine predicate. Note that coroutines can ONLY be made by 
"define-coroutine" or as the result of a "yield" expression!
)",





}, {
"coroutine->generator",
"Procedure",
R"(
(coroutine->generator <coroutine-object>)
)",
R"(
Converts the given coroutine object into a "generator" thunk! 
Invoking the generator will continuously "yield" the next yielded value! 
Yields the 'generator-complete symbol once finished iterating the coroutine!

  (define-coroutine (pows-of-2-coro)
    (let loop ((count 0))
      (yield (expt 2 count))
      (loop (+ count 1))))

  (define 2-pows (coroutine->generator (pows-of-2-coro)))

  (2-pows) ; 1
  (2-pows) ; 2
  (2-pows) ; 4
  (2-pows) ; 8
  (2-pows) ; 16
  (2-pows) ; 32
)",





}, {
"cycle-coroutines!",
"Procedure",
R"(
(cycle-coroutines! <coroutine-object-1> ...)
)",
R"(
Cyclically invoke each of the given coroutine objects!
Invokes first coroutine until yields, then invokes next, and so on until wraps 
around. Returns the first non-coroutine-object received from a ".next" invocation.

TAKE HEED: if none of the coroutines ever finish, neither will this procedure!

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
)",





}, {
"stream-length",
"Procedure",
R"(
(stream-length <stream>)
)",
R"(
Length of given stream.
WARNING: runs FOREVER if given an infinite stream!
)",





}, {
"stream-reverse",
"Procedure",
R"(
(stream-reverse <stream>)
)",
R"(
Reverse of given stream.
WARNING: runs FOREVER if given an infinite stream!
)",





}, {
"scar",
"Procedure",
R"(
(scar <stream>)
)",
R"(
Head of the given stream.
)",





}, {
"scdr",
"Procedure",
R"(
(scdr <stream>)
)",
R"(
Tail of the given stream.
)",





}, {
"scaar ... scddddr",
"Procedure",
R"(
(scaar <stream>) ... (scddddr <stream>)
)",
R"(
All possible combos of scar & scdr (up to 4 compositions) are already defined.
Hence: (scadaar <stream>) ; Equivalent to: (scar (scdr (scar (scar <stream>))))
)",





}, {
"stream-ref",
"Procedure",
R"(
(stream-ref <stream-pair> <index>)
)",
R"(
Get <index>th item in <stream-pair>.
)",





}, {
"stream-append",
"Procedure",
R"(
(stream-append <stream1> <stream2> ...)
)",
R"(
Create a new stream by appending <stream1> <stream2> ...
WARNING: runs FOREVER if given an infinite stream!
)",





}, {
"stream-drop",
"Procedure",
R"(
(stream-drop <stream> <n>)
)",
R"(
Copy of <stream> w/o the 1st <n> items.
)",





}, {
"stream-drop-while",
"Procedure",
R"(
(stream-drop-while <predicate?> <stream>)
)",
R"(
Copy of <stream> w/ all inital items satisfying <predicate?> dropped.
)",





}, {
"stream-take",
"Procedure",
R"(
(stream-take <stream> <n>)
)",
R"(
Stream copy of <stream>'s 1st <n> items.
)",





}, {
"stream-take-while",
"Procedure",
R"(
(stream-take-while <predicate?> <stream>)
)",
R"(
Stream copy of all <stream>'s initial items satisfying <predicate?>.
)",





}, {
"stream-map",
"Procedure",
R"(
(stream-map <callable> <stream1> <stream2> ...)
)",
R"(
New stream from mapping <callable> over the given streams.
)",





}, {
"stream-filter",
"Procedure",
R"(
(stream-filter <predicate?> <stream>)
)",
R"(
New stream with all items in <stream> satisfying <predicate?>.
)",





}, {
"stream-for-each",
"Procedure",
R"(
(stream-for-each <callable> <stream1> <stream2> ...)
)",
R"(
Apply <callable> on each item in the given streams.
WARNING: runs FOREVER if given infinite streams!
)",





}, {
"stream-unfold",
"Procedure",
R"(
(stream-unfold <break-cond-callable> <map-callable> <suc-callable> <seed>)
)",
R"(
Form a stream by mapping (via <map-callable>) & incrementing 
(via <suc-callable>) seed, until <break-cond-callable> is true.
)",





}, {
"stream-fold",
"Procedure",
R"(
(stream-fold <callable> <seed> <stream>)
)",
R"(
Accumulate stream from left to right, starting with <seed> using <callable>.
  *) See "stream-fold-right" to perform this from right to left instead!
)",





}, {
"stream-fold-right",
"Procedure",
R"(
(stream-fold-right <callable> <seed> <stream>)
)",
R"(
Accumulate stream from right to left, starting with <seed> using <callable>.
WARNING: runs FOREVER if given an infinite stream!
  *) See "stream-fold" to perform this from left to right instead!
)",





}, {
"stream-from",
"Procedure",
R"(
(stream-from <first> <optional-step>)
)",
R"(
Form a numeric stream starting from <first> incrementing by <optional-step>.
)",





}, {
"stream-iterate",
"Procedure",
R"(
(stream-iterate <suc-callable> <seed>)
)",
R"(
Form a stream starting from <seed> using <suc-callable>.
)",





}, {
"stream-zip",
"Procedure",
R"(
(stream-zip <stream1> <stream2> ...)
)",
R"(
Form a stream of lists containing the nth elt of each <stream>.
)",





}, {
"stream-constant",
"Procedure",
R"(
(stream-constant <obj1> <obj2> ...)
)",
R"(
Forms an infinite stream of repeating <objs>.
)",





}, {
"stream-interleave",
"Procedure",
R"(
(stream-interleave <stream1> <stream2>)
)",
R"(
Form a stream by interleaving elts of either <stream>.
)",





}, {
"+",
"Procedure",
R"(
(+ <number1> <number2> ...)
)",
R"(
Add the given numbers.
)",





}, {
"-",
"Procedure",
R"(
(- <number1> <number2> ...) 
(- <number>)
)",
R"(
1 Argument: Negate <number>.
2+ Arguments: Subtract <number2> ... from <number1>.
)",





}, {
"*",
"Procedure",
R"(
(* <number1> <number2> ...)
)",
R"(
Multiply the given numbers.
)",





}, {
"/",
"Procedure",
R"(
(/ <number1> <number2> ...) 
(/ <number>)
)",
R"(
1 Argument: Invert <number>.
2+ Arguments: Divide <number1> by <number2> ...
)",





}, {
"=",
"Procedure",
R"(
(= <number1> <number2> ...)
)",
R"(
Confirm numeric equality.
  *) Unlike "eq?", "eqv?", & "equal?", "=" does NOT care about exactness!
     => Hence: (= 1 1.0) ; #t
)",





}, {
"<",
"Procedure",
R"(
(< <real1> <real2> ...)
)",
R"(
Confirm all left <real>s are less than right <real>s.
)",





}, {
">",
"Procedure",
R"(
(> <real1> <real2> ...)
)",
R"(
Confirm all left <real>s are greater than right <real>s.
)",





}, {
"<=",
"Procedure",
R"(
(<= <real1> <real2> ...)
)",
R"(
Confirm all left <real>s are less than or equal to right <real>s.
)",





}, {
">=",
"Procedure",
R"(
(>= <real1> <real2> ...)
)",
R"(
Confirm all left <real>s are greater than or equal to right <real>s.
)",





}, {
"abs",
"Procedure",
R"(
(abs <real>)
)",
R"(
Absolute value of <real>.
)",





}, {
"expt",
"Procedure",
R"(
(expt <number1> <number2> ...)
)",
R"(
Exponentiate n numbers. 
  *) Keeping with convention, exponentiation is RIGHT ASSOCIATIVE!
  *) See "expt-mod" for a more efficient alternative to (modulo (expt a b) c)!
)",





}, {
"expt-mod",
"Procedure",
R"(
(expt-mod <real1> <real2> <real3>)
)",
R"(
Raise <real1> to the power of <real2> modulo <real3>.
  *) Under the hood this uses repeated squaring to perform the operation
     more efficiently than (modulo (expt <real1> <real2>) <real3>) would,
     making it ideal for handling bigints (ie for cryptographic applications).
)",





}, {
"max",
"Procedure",
R"(
(max <real1> <real2> ...)
)",
R"(
Get the maximum real value (opposite of "min").
)",





}, {
"min",
"Procedure",
R"(
(min <real1> <real2> ...)
)",
R"(
Get the minimum real value (opposite of "max").
)",





}, {
"quotient",
"Procedure",
R"(
(quotient <real1> <real2>)
)",
R"(
Get the <quotient> of (/ <real1> <real2>).
  *) See "divmod" for an efficient way of getting both 
     the quotient & remainder of a division operation!
)",





}, {
"remainder",
"Procedure",
R"(
(remainder <real1> <real2>)
)",
R"(
Get the <remainder> of (/ <real1> <real2>).
  *) See "divmod" for an efficient way of getting both 
     the quotient & remainder of a division operation!
)",





}, {
"divmod",
"Procedure",
R"(
(divmod <real1> <real2>)
)",
R"(
Get a pair with the quotient and remainder of <real1> & <real2>.
)",





}, {
"modulo",
"Procedure",
R"(
(modulo <real1> <real2>)
)",
R"(
Get <real1> modulo <real2>.
  *) See "expt-mod" for a more efficient alternative to (modulo (expt a b) c)!
)",





}, {
"modf",
"Procedure",
R"(
(modf <flonum>)
)",
R"(
Modulo Flonum: Get a pair with the integral & fractional portions of <flonum>.
)",





}, {
"exp",
"Procedure",
R"(
(exp <number>)
)",
R"(
Get e raised to the power of <number>.
)",





}, {
"log",
"Procedure",
R"(
(log <number> <optional-base>)
)",
R"(
Get the log of <number> using base <optional-base> (natural log by default).
)",





}, {
"sqrt",
"Procedure",
R"(
(sqrt <number>)
)",
R"(
Square root of <number>.
)",





}, {
"gcd",
"Procedure",
R"(
(gcd <real1> <real2>)
)",
R"(
Greatest common denominator of <real1> & <real2>.
)",





}, {
"lcm",
"Procedure",
R"(
(lcm <real1> <real2>)
)",
R"(
Least common multiple of <real1> & <real2>.
)",





}, {
"npr",
"Procedure",
R"(
(npr <real1> <real2>)
)",
R"(
The number of permutations from choosing an ordered set of r objects from a 
total of n objects.
)",





}, {
"ncr",
"Procedure",
R"(
(ncr <real1> <real2>)
)",
R"(
The number of different, unordered combinations of r objects from a 
set of n objects.
)",





}, {
"numerator",
"Procedure",
R"(
(numerator <real>)
)",
R"(
Extract the numerator of <real>.
)",





}, {
"denominator",
"Procedure",
R"(
(denominator <real>)
)",
R"(
Extract the denominator of <real>.
)",





}, {
"make-log-base",
"Procedure",
R"(
(make-log-base <real>)
)",
R"(
Generate a log procedure of base <real>.
)",





}, {
"random",
"Procedure",
R"(
(random <real-seed>) 
(random)
)",
R"(
Psuedo-random number generator: seeded or unseeded!
  *) Unseeded defaults to a seed using the current time!
)",





}, {
"inexact->exact",
"Procedure",
R"(
(inexact->exact <number>)
)",
R"(
Convert inexact <number> to its nearest exact representation.
)",





}, {
"exact->inexact",
"Procedure",
R"(
(exact->inexact <number>)
)",
R"(
Convert exact <number> to its nearest inexact representation.
)",





}, {
"odd?",
"Procedure",
R"(
(odd? <integer>)
)",
R"(
Confirm <integer> is odd.
)",





}, {
"even?",
"Procedure",
R"(
(even? <integer>)
)",
R"(
Confirm <integer> is even.
)",





}, {
"positive?",
"Procedure",
R"(
(positive? <real>)
)",
R"(
Confirm <real> is positive. Opposite of "not-positive?".
)",





}, {
"not-positive?",
"Procedure",
R"(
(not-positive? <real>)
)",
R"(
Confirm <real> is not positive. Opposite of "positive?".
)",





}, {
"negative?",
"Procedure",
R"(
(negative? <real>)
)",
R"(
Confirm <real> is negative. Opposite of "not-negative?".
)",





}, {
"not-negative?",
"Procedure",
R"(
(not-negative? <real>)
)",
R"(
Confirm <real> is not negative. Opposite of "negative?".
)",





}, {
"zero?",
"Procedure",
R"(
(zero? <number>)
)",
R"(
Confirm <number> is zero. Opposite of "not-zero?".
)",





}, {
"not-zero?",
"Procedure",
R"(
(not-zero? <number>)
)",
R"(
Confirm <number> is not zero. Opposite of "zero?".
)",





}, {
"infinite?",
"Procedure",
R"(
(infinite? <real>)
)",
R"(
Confirm <real> is either +inf.0 or -inf.0
)",





}, {
"finite?",
"Procedure",
R"(
(finite? <real>)
)",
R"(
Confirm <real> is neither +inf.0 nor -inf.0 nor +nan.0
)",





}, {
"nan?",
"Procedure",
R"(
(nan? <real>)
)",
R"(
Confirm <real> is +nan.0
)",





}, {
"exact?",
"Procedure",
R"(
(exact? <number>)
)",
R"(
Confirm <number> is exact (ratnum/bigint).
)",





}, {
"inexact?",
"Procedure",
R"(
(inexact? <number>)
)",
R"(
Confirm <number> is inexact (flonum/infinite).
)",





}, {
"integer?",
"Procedure",
R"(
(integer? <number>)
)",
R"(
Confirm <number> is an integer.
  *) This does NOT check exactness, hence (integer? 1.0) is true!
  *) Use "bigint?" to confirm <number> is an integer AND exact!
)",





}, {
"bigint?",
"Procedure",
R"(
(bigint? <number>)
)",
R"(
Confirm <number> is an integer AND exact!
)",





}, {
"ceiling",
"Procedure",
R"(
(ceiling <real>)
)",
R"(
Round <real> up to the nearest integer:

  (ceiling 0.5)  ; 1.0
  (ceiling -0.5) ; 0
)",





}, {
"floor",
"Procedure",
R"(
(floor <real>)
)",
R"(
Round <real> down to the nearest integer:

  (floor 0.5)  ; 0
  (floor -0.5) ; -1.0
)",





}, {
"truncate",
"Procedure",
R"(
(truncate <real>)
)",
R"(
Round <real> towards 0:

  (truncate 1.5)  ; 1.0
  (truncate -1.5) ; -1.0
)",





}, {
"round",
"Procedure",
R"(
(round <real>)
)",
R"(
Round <real> to the nearest integer:

  (round 0.5)  ; 1.0
  (round -0.5) ; -1.0
)",





}, {
"sin",
"Procedure",
R"(
(sin <number>)
)",
R"(
Get sin of <number>.
)",





}, {
"cos",
"Procedure",
R"(
(cos <number>)
)",
R"(
Get cos of <number>.
)",





}, {
"tan",
"Procedure",
R"(
(tan <number>)
)",
R"(
Get tan of <number>.
)",





}, {
"asin",
"Procedure",
R"(
(asin <number>)
)",
R"(
Get arcsin of <number>.
)",





}, {
"acos",
"Procedure",
R"(
(acos <number>)
)",
R"(
Get arccos of <number>.
)",





}, {
"atan",
"Procedure",
R"(
(atan <number>)
(atan <real1> <real2>)
)",
R"(
1 Argument: Get arctan of <number>.
2 Arguments: The principal value of the arctan of <real2>/<real1> (in radians).
)",





}, {
"sinh",
"Procedure",
R"(
(sinh <number>)
)",
R"(
Get the hyperbolic sin of <number>.
)",





}, {
"cosh",
"Procedure",
R"(
(cosh <number>)
)",
R"(
Get the hyperbolic cos of <number>.
)",





}, {
"tanh",
"Procedure",
R"(
(tanh <number>)
)",
R"(
Get the hyperbolic tan of <number>.
)",





}, {
"asinh",
"Procedure",
R"(
(asinh <number>)
)",
R"(
Get the hyperbolic arcsin of <number>.
)",





}, {
"acosh",
"Procedure",
R"(
(acosh <number>)
)",
R"(
Get the hyperbolic arccos of <number>.
)",





}, {
"atanh",
"Procedure",
R"(
(atanh <number>)
)",
R"(
Get the hyperbolic arctan of <number>.
)",





}, {
"logand",
"Procedure",
R"(
(logand <real1> <real2>)
)",
R"(
Logical bitwise AND of <real1> and <real2>.
)",





}, {
"logor",
"Procedure",
R"(
(logor <real1> <real2>)
)",
R"(
Logical bitwise OR of <real1> and <real2>.
)",





}, {
"logxor",
"Procedure",
R"(
(logxor <real1> <real2>)
)",
R"(
Logical bitwise EXCLUSIVE-OR of <real1> and <real2>.
)",





}, {
"lognot",
"Procedure",
R"(
(lognot <real>)
)",
R"(
Logical bitwise complement of <real>.
)",





}, {
"loglsl",
"Procedure",
R"(
(loglsl <real> <shift-amount>)
)",
R"(
Logical shift left of <real> by <shift-amount>.
Floods values shifted past with 0s.
)",





}, {
"loglsr",
"Procedure",
R"(
(loglsr <real> <shift-amount>)
)",
R"(
Logical shift right of <real> by <shift-amount>.
Floods values shifted past with 0s.
)",





}, {
"logasr",
"Procedure",
R"(
(logasr <real> <shift-amount>)
)",
R"(
Arithmetic shift right of <real> by <shift-amount>.
Floods values shifted past with the sign bit.
)",





}, {
"logbit?",
"Procedure",
R"(
(logbit? <real> <n>)
)",
R"(
Confirm <n>th bit of <real> is 1.
)",





}, {
"logbit1",
"Procedure",
R"(
(logbit1 <real> <n>)
)",
R"(
Set <n>th bit of <real> to 1.
)",





}, {
"logbit0",
"Procedure",
R"(
(logbit0 <real> <n>)
)",
R"(
Set <n>th bit of <real> to 0.
)",





}, {
"logbit~",
"Procedure",
R"(
(logbit~ <real> <n>)
)",
R"(
Complement <n>th bit of <real>.
)",





}, {
"make-rectangular",
"Procedure",
R"(
(make-rectangular <real-real> <real-imag>)
)",
R"(
Generate a complex number given its real & imaginary components.
)",





}, {
"make-polar",
"Procedure",
R"(
(make-polar <real-magnitude> <real-angle>)
)",
R"(
Generate a complex number given its polar magnitude & angle.
)",





}, {
"real-part",
"Procedure",
R"(
(real-part <number>)
)",
R"(
Get the real part of <number>.
)",





}, {
"imag-part",
"Procedure",
R"(
(imag-part <number>)
)",
R"(
Get the imaginary part of <number>.
)",





}, {
"magnitude",
"Procedure",
R"(
(magnitude <number>)
)",
R"(
Get the polar magnitude of <number>.
)",





}, {
"angle",
"Procedure",
R"(
(angle <number>)
)",
R"(
Get the polar angle of <number>.
)",





}, {
"conjugate",
"Procedure",
R"(
(conjugate <number>)
)",
R"(
Get the conjugate of <number>.
)",





}, {
"eq?",
"Procedure",
R"(
(eq? <obj1> <obj2> ...)
)",
R"(
Shallow equality (pointer comparisons)!
  *) Unlike "=", ALSO accounts for exactness in numbers!
  *) I.E.: (eq? 1.0 1)                   ; #f ; mismatched exactness
           (eq? 1.0 1.0)                 ; #t ; numbers use value semantics
           (eq? "hello" "hello")         ; #f ; different object pointers
           (let ((s "hello")) (eq? s s)) ; #t ; same object pointer
           (eq? '(1) '(1))               ; #f ; different object pointers
           (let ((s '(1))) (eq? s s))    ; #t ; same object pointer

Generally: 
  (eq? <item> <item>)                ; #t
  (eq? <item> (shallow-copy <item>)) ; #f
  (eq? <item> (copy <item>))         ; #f
)",





}, {
"eqv?",
"Procedure",
R"(
(eqv? <obj1> <obj2> ...)
)",
R"(
Equivalency (structural comparisons, uses "eq?" for structure components)!
  *) Unlike "=", ALSO accounts for exactness in numbers!
  *) I.E.: (eqv? 1.0 1)                   ; #f ; mismatched exactness
           (eqv? 1.0 1.0)                 ; #t ; numbers use value semantics
           (eqv? "hello" "hello")         ; #t ; string value comparison
           (let ((s "hello")) (eqv? s s)) ; #t ; same object pointer
           (eqv? '(1) '(1))               ; #f ; different object pointers
           (let ((s '(1))) (eqv? s s))    ; #t ; same object pointer

Generally: 
  (eqv? <item> <item>)                ; #t
  (eqv? <item> (shallow-copy <item>)) ; #t
  (eqv? <item> (copy <item>))         ; #f
)",





}, {
"equal?",
"Procedure",
R"(
(equal? <obj1> <obj2> ...)
)",
R"(
Recursive "deep" equality: recursively compares container components!
  *) Unlike "=", ALSO accounts for exactness in numbers!
  *) I.E.: (equal? 1.0 1)                   ; #f ; mismatched exactness
           (equal? 1.0 1.0)                 ; #t ; numbers use value semantics
           (equal? "hello" "hello")         ; #t ; string value comparison
           (let ((s "hello")) (equal? s s)) ; #t ; same object pointer
           (equal? '(1) '(1))               ; #t ; container recursive value comparison
           (let ((s '(1))) (equal? s s))    ; #t ; same object pointer

Generally: 
  (equal? <item> <item>)                ; #t
  (equal? <item> (shallow-copy <item>)) ; #t
  (equal? <item> (copy <item>))         ; #t
)",





}, {
"not",
"Procedure",
R"(
(not <obj>)
)",
R"(
Boolean not: (not #f) -> #t
             (not _ ) -> #f
)",





}, {
"char-alphabetic?",
"Procedure",
R"(
(char-alphabetic? <char>)
)",
R"(
Confirm <char> is an alphabetic character.
)",





}, {
"char-numeric?",
"Procedure",
R"(
(char-numeric? <char>)
)",
R"(
Confirm <char> is a numeric character.
)",





}, {
"char-whitespace?",
"Procedure",
R"(
(char-whitespace? <char>)
)",
R"(
Confirm <char> is a whitespace character.
)",





}, {
"char-upper-case?",
"Procedure",
R"(
(char-upper-case? <char>)
)",
R"(
Confirm <char> is an upper-case character.
)",





}, {
"char-lower-case?",
"Procedure",
R"(
(char-lower-case? <char>)
)",
R"(
Confirm <char> is a lower-case character.
)",





}, {
"char-alphanumeric?",
"Procedure",
R"(
(char-alphanumeric? <char>)
)",
R"(
Confirm <char> is an alphanumeric character.
Equivalent to: (or (char-alphabetic? <char>) (char-numeric? <char>))
)",





}, {
"char-control?",
"Procedure",
R"(
(char-control? <char>)
)",
R"(
Confirm <char> is a control character.
)",





}, {
"char-print?",
"Procedure",
R"(
(char-print? <char>)
)",
R"(
Confirm <char> is printable.
Equivalent to: (or (char-graph? <char>) (eq? #\space <char>))
)",





}, {
"char-graph?",
"Procedure",
R"(
(char-graph? <char>)
)",
R"(
Confirm <char> is graph character.
Equivalent to: (or (char-alphanumeric? <char>) (char-punctuation? <char>))
)",





}, {
"char-punctuation?",
"Procedure",
R"(
(char-punctuation? <char>)
)",
R"(
Confirm <char> is a punctuation character.
)",





}, {
"char-xdigit?",
"Procedure",
R"(
(char-xdigit? <char>)
)",
R"(
Confirm <char> is a hexadecimal digit character.
)",





}, {
"char-upcase",
"Procedure",
R"(
(char-upcase <char>)
)",
R"(
Upper-case <char>.
)",





}, {
"char-downcase",
"Procedure",
R"(
(char-downcase <char>)
)",
R"(
Lower-case <char>.
)",





}, {
"eof",
"Procedure",
R"(
(eof)
)",
R"(
Create an EOF object.
  *) Check for such via the "eof-object?" predicate.
)",





}, {
"char=?",
"Procedure",
R"(
(char=? <char1> <char2> ...)
)",
R"(
Character equality.
)",





}, {
"char<?",
"Procedure",
R"(
(char<? <char1> <char2> ...)
)",
R"(
Confirm all left <char>s are less than right <char>s.
)",





}, {
"char>?",
"Procedure",
R"(
(char>? <char1> <char2> ...)
)",
R"(
Confirm all left <char>s are greater than right <char>s.
)",





}, {
"char<=?",
"Procedure",
R"(
(char<=? <char1> <char2> ...)
)",
R"(
Confirm all left <char>s are less than or equal to right <char>s.
)",





}, {
"char>=?",
"Procedure",
R"(
(char>=? <char1> <char2> ...)
)",
R"(
Confirm all left <char>s are greater than or equal to right <char>s.
)",





}, {
"char-ci=?",
"Procedure",
R"(
(char-ci=? <char1> <char2> ...)
)",
R"(
Character equality (case-insensitive!).
)",





}, {
"char-ci<?",
"Procedure",
R"(
(char-ci<? <char1> <char2> ...)
)",
R"(
Confirm all left <char>s are less than right <char>s (case-insensitive!).
)",





}, {
"char-ci>?",
"Procedure",
R"(
(char-ci>? <char1> <char2> ...)
)",
R"(
Confirm all left <char>s are greater than right <char>s (case-insensitive!).
)",





}, {
"char-ci<=?",
"Procedure",
R"(
(char-ci<=? <char1> <char2> ...)
)",
R"(
Confirm all left <char>s are less than or equal to right <char>s (case-insensitive!).
)",





}, {
"char-ci>=?",
"Procedure",
R"(
(char-ci>=? <char1> <char2> ...)
)",
R"(
Confirm all left <char>s are greater than or equal to right <char>s (case-insensitive!).
)",





}, {
"make-string",
"Procedure",
R"(
(make-string <size> <optional-fill-char>)
)",
R"(
Construct of string of length <size> with <optional-fill-char>.
  *) <optional-fill-char> defaults to #\?
)",





}, {
"string-unfold",
"Procedure",
R"(
(string-unfold <break-condition> <map-callable> <successor-callable> <seed>)
)",
R"(
Form a string by mapping (via <map-callable>) & incrementing (via 
<successor-callable>) seed, until <break-condition> is true.
  *) See "string-unfold-right" to perform this from right to left instead!
)",





}, {
"string-unfold-right",
"Procedure",
R"(
(string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)
)",
R"(
Form a string by mapping right (via <map-callable>) & incrementing
(via <successor-callable>) seed, until <break-condition> is true.
  *) See "string-unfold" to perform this from left to right instead!
)",





}, {
"string-pad",
"Procedure",
R"(
(string-pad <string> <length> <optional-character>)
)",
R"(
Pads left of <string> with <length> <optional-character>s.
  *) <optional-character> defaults to #\space
  *) See "string-pad-right" to perform this from the right instead!
)",





}, {
"string-pad-right",
"Procedure",
R"(
(string-pad-right <string> <length> <optional-character>)
)",
R"(
Pads right of <string> with <length> <optional-character>s.
  *) <optional-character> defaults to #\space
  *) See "string-pad" to perform this from the left instead!
)",





}, {
"string-trim",
"Procedure",
R"(
(string-trim <string> <optional-predicate?>)
)",
R"(
Trim characters from the left of <string> while <predicate?> is true.
  *) <predicate?> defaults to "char-whitespace?".
  *) See "string-trim-right" to perform this from the right instead!
)",





}, {
"string-trim-right",
"Procedure",
R"(
(string-trim-right <string> <optional-predicate?>)
)",
R"(
Trim characters from the right of <string> while <predicate?> is true.
  *) <predicate?> defaults to "char-whitespace?".
  *) See "string-trim" to perform this from the left instead!
)",





}, {
"string-trim-both",
"Procedure",
R"(
(string-trim-both <string> <optional-predicate?>)
)",
R"(
Trim characters from the left & right of <string> while <predicate?> is true.
  *) <predicate?> defaults to "char-whitespace?".
  *) See "string-trim" & "string-trim-right" for side-specific alternatives!
)",





}, {
"string-replace",
"Procedure",
R"(
(string-replace <string1> <string2> <start> <end>)
)",
R"(
Replace <string1> between indices <start> & <end> with <string2>.
  *) See "regex-replace" & "regex-replace-all" for a regex-based alternative!
)",





}, {
"string-contains",
"Procedure",
R"(
(string-contains <string> <sub-string>)
)",
R"(
Confirms <string> contains <sub-string> (from left). 
Returns index of 1st instance, or #f if DNE!
  *) See "string-contains-right" to perform this from the right instead!
)",





}, {
"string-contains-right",
"Procedure",
R"(
(string-contains-right <string> <sub-string>)
)",
R"(
Confirms <string> contains <sub-string> (from right). 
Returns index of 1st instance, or #f if DNE!
  *) See "string-contains" to perform this from the left instead!
)",





}, {
"string-join",
"Procedure",
R"(
(string-join <string-list> <optional-string-delimiter> <optional-grammar>)
)",
R"(
Join a list of strings into 1 string using <string-delimiter> & following <grammar>.
<optional-grammar> ::= 'infix | 'suffix | 'prefix
  *) <optional-string-delimiter> defaults to ""
  *) <optional-grammar> defaults to 'infix
)",





}, {
"string-split",
"Procedure",
R"(
(string-split <target-string> <optional-string-delimiter> <optional-start-index>)
)",
R"(
Split <target-string> into a list of strings at each <optional-string-delimiter>
starting at <optional-start-index>.
  *) <string-delimiter> defaults to ""
  *) <optional-string-delimiter> defaults to 0

Enables splitting with delimiters using regex-significant chars more easily!
  *) See "regex-split" for a regex-based alternative!
)",





}, {
"string-swap!",
"Procedure",
R"(
(string-swap! <string1> <string2>)
)",
R"(
Swap string pointers.
)",





}, {
"string-push!",
"Procedure",
R"(
(string-push! <string> <char>)
)",
R"(
Mutate <string> by pushing <char> to its end.
)",





}, {
"string-pop!",
"Procedure",
R"(
(string-pop! <string>)
)",
R"(
Mutate <string> by popping its last character.
)",





}, {
"string-empty?",
"Procedure",
R"(
(string-empty? <string>)
)",
R"(
Confirm <string> is empty.
Equivalent to: (zero? (length <string>))
)",





}, {
"string-copy!",
"Procedure",
R"(
(string-copy! <target-string> <target-start-idx> <source-string>)
)",
R"(
Copy <source-string> to <target-string> from <target-start-idx>.
)",





}, {
"string=?",
"Procedure",
R"(
(string=? <string1> <string2> ...)
)",
R"(
String equality.
)",





}, {
"string<?",
"Procedure",
R"(
(string<? <string1> <string2> ...)
)",
R"(
Confirm all left <string>s are less than right <string>s.
)",





}, {
"string>?",
"Procedure",
R"(
(string>? <string1> <string2> ...)
)",
R"(
Confirm all left <string>s are greater than right <string>s.
)",





}, {
"string<=?",
"Procedure",
R"(
(string<=? <string1> <string2> ...)
)",
R"(
Confirm all left <string>s are less than or equal to right <string>s.
)",





}, {
"string>=?",
"Procedure",
R"(
(string>=? <string1> <string2> ...)
)",
R"(
Confirm all left <string>s are greater than or equal to right <string>s.
)",





}, {
"string-ci=?",
"Procedure",
R"(
(string-ci=? <string1> <string2> ...)
)",
R"(
String equality (case-insensitive!).
)",





}, {
"string-ci<?",
"Procedure",
R"(
(string-ci<? <string1> <string2> ...)
)",
R"(
Confirm all left <string>s are less than right <string>s (case-insensitive!).
)",





}, {
"string-ci>?",
"Procedure",
R"(
(string-ci>? <string1> <string2> ...)
)",
R"(
Confirm all left <string>s are greater than right <string>s (case-insensitive!).
)",





}, {
"string-ci<=?",
"Procedure",
R"(
(string-ci<=? <string1> <string2> ...)
)",
R"(
Confirm all left <string>s are less than or equal to right <string>s (case-insensitive!).
)",





}, {
"string-ci>=?",
"Procedure",
R"(
(string-ci>=? <string1> <string2> ...)
)",
R"(
Confirm all left <string>s are greater than or equal to right <string>s (case-insensitive!).
)",





}, {
"regex-replace",
"Procedure",
R"(
(regex-replace <target-string> <regex-string> <replacement-string>) 
(regex-replace <target-string> <regex-string> <callable>)
)",
R"(
Replace 1st regex match instance!
  *) <callable> ::= (lambda (<prefix>, <suffix>, <match1>, ...) <body>)
  *) <callable> MUST return a string to replace the match!

See "regex-replace-all" to replace all instances rather than just the first!

Regex's Use ECMAScript Syntax: 
  *) https://www.cplusplus.com/reference/regex/ECMAScript/
)",





}, {
"regex-replace-all",
"Procedure",
R"(
(regex-replace-all <target-string> <regex-string> <replacement-string>) 
(regex-replace-all <target-string> <regex-string> <callable>)
)",
R"(
Replace all regex match instances!
  *) <callable> ::= (lambda (<prefix>, <suffix>, <match1>, ...) <body>)
  *) <callable> MUST return a string to replace the match!

See "regex-replace" to replace the 1st instance rather than all of them!

Regex's Use ECMAScript Syntax: 
  *) https://www.cplusplus.com/reference/regex/ECMAScript/
)",





}, {
"regex-match",
"Procedure",
R"(
(regex-match <target-string> <regex-string>)
)",
R"(
Alist of all regex matches!
  *) Returned alist's sublists have the position & match substring instance!
  *) If <regex-string> has multiple substrings per match, becomes a 2nd order alist!

Regex's Use ECMAScript Syntax: 
  *) https://www.cplusplus.com/reference/regex/ECMAScript/
)",





}, {
"regex-split",
"Procedure",
R"(
(regex-split <target-string> <optional-regex-string> <optional-start-index>)
)",
R"(
Regex split string into a list of substrings!
  *) <optional-regex-string> defaults to "" to split into char-strings
  *) <optional-start-index> defaults to 0

Regex's Use ECMAScript Syntax: 
  *) https://www.cplusplus.com/reference/regex/ECMAScript/
)",





}, {
"cons",
"Procedure",
R"(
(cons <obj1> <obj2>)
)",
R"(
Construct a pair.
)",





}, {
"car",
"Procedure",
R"(
(car <pair>) 
)",
R"(
Get 1st object in pair.
)",





}, {
"cdr",
"Procedure",
R"(
(cdr <pair>)
)",
R"(
Get 2nd object in pair.
)",





}, {
"caar ... cddddr",
"Procedure",
R"(
(caar <pair>) ... (cddddr <pair>)
)",
R"(
All possible combos of "car" & "cdr" (up to 4 compositions) are already defined.
Hence: (cadaar <pair>) ; Equivalent to: (car (cdr (car (car <pair>))))
)",





}, {
"set-car!",
"Procedure",
R"(
(set-car! <pair> <obj>)
)",
R"(
Set the 1st object in <pair> to be <obj>.
)",





}, {
"set-cdr!",
"Procedure",
R"(
(set-cdr! <pair> <obj>)
)",
R"(
Set the 2nd object in <pair> to be <obj>.
)",





}, {
"last-pair",
"Procedure",
R"(
(last-pair <non-empty-list>)
)",
R"(
Last pair in <non-empty-list>.
)",





}, {
"pair-swap!",
"Procedure",
R"(
(pair-swap! <pair1> <pair2>)
)",
R"(
Swap pair pointers.
)",





}, {
"make-list",
"Procedure",
R"(
(make-list <size> <fill-value>)
)",
R"(
Create a list of length <size> with values of <fill-value>.
)",





}, {
"list*",
"Procedure",
R"(
(list* <obj1> <obj2> ...)
)",
R"(
Construct a dotted list:
  
  (list* 1)     ; 1
  (list* 1 2)   ; '(1 . 2)
  (list* 1 2 3) ; '(1 2 . 3)
)",





}, {
"circular-list",
"Procedure",
R"(
(circular-list <obj1> <obj2> ...)
)",
R"(
Creates a circular list of <obj1> <obj2> ... where the "cdr" of the last pair
points to the 1st pair in the created list.
)",





}, {
"iota",
"Procedure",
R"(
(iota <count> <optional-start-number> <optional-step-number>)
)",
R"(
Generate list of <count> numbers, from <start> & incrementing w/ <step>.
  *) <optional-start-number> defaults to 0
  *) <optional-step-number> defaults to 1
)",





}, {
"unfold",
"Procedure",
R"(
(unfold <break-condition> <map-callable> <successor-callable> <seed>)
)",
R"(
Form a list by mapping (via <map-callable>) & incrementing (via 
<successor-callable>) seed, until <break-condition> is true.
  *) See "unfold-right" to perform this from the right instead!
)",





}, {
"unfold-right",
"Procedure",
R"(
(unfold-right <break-condition> <map-callable> <successor-callable> <seed>)
)",
R"(
Form a list by mapping right (via <map-callable>) & incrementing 
(via <successor-callable>) seed, until <break-condition> is true.
  *) See "unfold" to perform this from the left instead!
)",





}, {
"get-all-combinations",
"Procedure",
R"(
(get-all-combinations <list>)
)",
R"(
Generate list of all combinations of objects in <list>.
  *) See "vector-get-all-combinations" for a vector-based alternative!
)",





}, {
"null?",
"Procedure",
R"(
(null? <obj>)
)",
R"(
Confirm <obj> is '()
Equivalent to: (eq? <obj> '())
)",





}, {
"list?",
"Procedure",
R"(
(list? <obj>)
)",
R"(
Confirm <obj> is a proper list (non-circular & null-terminated).
)",





}, {
"list*?",
"Procedure",
R"(
(list*? <obj>)
)",
R"(
Confirm <obj> is a non-circular dotted list.
)",





}, {
"circular-list?",
"Procedure",
R"(
(circular-list? <obj>)
)",
R"(
Confirm <obj> is a circular list.
  *) Whether last pair's "cdr" points to the 1st pair in the list.
)",





}, {
"alist?",
"Procedure",
R"(
(alist? <obj>)
)",
R"(
Confirm <obj> is an associative list.
  *) Whether <obj> is a list of pairs.
)",





}, {
"memq",
"Procedure",
R"(
(memq <obj> <list>)
)",
R"(
Get sublist beginning w/ <obj> (checked via "eq?") if present (#f otherwise).
  *) See "memv" & "member" for alternatives using "eqv?" & "equal?" to check for equality!
)",





}, {
"memv",
"Procedure",
R"(
(memv <obj> <list>)
)",
R"(
Get sublist beginning w/ <obj> (checked via "eqv?") if present (#f otherwise).
  *) See "memq" & "member" for alternatives using "eq?" & "equal?" to check for equality!
)",





}, {
"member",
"Procedure",
R"(
(member <obj> <list>)
)",
R"(
Get sublist beginning w/ <obj> (checked via "equal?") if present (#f otherwise).
  *) See "memq" & "memv" for alternatives using "eq?" & "eqv?" to check for equality!
)",





}, {
"assq",
"Procedure",
R"(
(assq <obj> <alist>)
)",
R"(
Get pair in <alist> beginning w/ key <obj> (checked via "eq?") 
if present (#f otherwise).
  *) See "assv" & "assoc" for alternatives using "eqv?" & "equal?" to check for equality!
)",





}, {
"assv",
"Procedure",
R"(
(assv <obj> <alist>)
)",
R"(
Get pair in <alist> beginning w/ key <obj> (checked via "eqv?") 
if present (#f otherwise).
  *) See "assq" & "assoc" for alternatives using "eq?" & "equal?" to check for equality!
)",





}, {
"assoc",
"Procedure",
R"(
(assoc <obj> <alist>)
)",
R"(
Get pair in <alist> beginning w/ key <obj> (checked via "equal?") 
if present (#f otherwise).
  *) See "assq" & "assv" for alternatives using "eq?" & "eqv?" to check for equality!
)",





}, {
"make-vector",
"Procedure",
R"(
(make-vector <size> <fill-value>)
)",
R"(
Create a vector of length <size> with values initialized to <fill-value>.
)",





}, {
"vector-push!",
"Procedure",
R"(
(vector-push! <vector> <obj>)
)",
R"(
Mutatate <vector> by pushing <obj> to its end.
)",





}, {
"vector-pop!",
"Procedure",
R"(
(vector-pop! <vector>)
)",
R"(
Mutatate <vector> by popping its last value.
)",





}, {
"vector-iota",
"Procedure",
R"(
(vector-iota <count> <optional-start-number> <optional-step-number>)
)",
R"(
Generate a vector of <count> numbers, from <start> & incrementing w/ <step>.
  *) <optional-start-number> defaults to 0
  *) <optional-step-number> defaults to 1
)",





}, {
"vector-unfold",
"Procedure",
R"(
(vector-unfold <break-condition> <map-callable> <successor-callable> <seed>)
)",
R"(
Form a vector by mapping (via <map-callable>) & incrementing seed 
(via <successor-callable>), until <break-condition> is true.
)",





}, {
"vector-unfold-right",
"Procedure",
R"(
(vector-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)
)",
R"(
Form a vector by mapping right (via <map-callable>) & incrementing
seed (via <successor-callable>), until <break-condition> is true.
)",





}, {
"vector-grow",
"Procedure",
R"(
(vector-grow <vector> <size>)
)",
R"(
Generate a new vector with the same elts and a new size.
)",





}, {
"vector-empty?",
"Procedure",
R"(
(vector-empty? <vector>)
)",
R"(
Confirm vector is empty.
Equivalent to: (zero? (length <vector>))
)",





}, {
"vector-copy!",
"Procedure",
R"(
(vector-copy! <target-vector> <target-start-idx> <source-vector>)
)",
R"(
Copy <source-vector> to <target-vector> from <target-start-idx>.
)",





}, {
"vector-swap!",
"Procedure",
R"(
(vector-swap! <vector1> <vector2>)
)",
R"(
Swap vector pointers.
)",





}, {
"vector-binary-search",
"Procedure",
R"(
(vector-binary-search <vector> <value> <3-way-comparison>)
)",
R"(
Seek <value> in <vector> via binary search, comparing values using <3-way-comparison>.
Suppose values a & b:
  *) a < b: (<3-way-comparison> a b) < 0
  *) a = b: (<3-way-comparison> a b) = 0
  *) a > b: (<3-way-comparison> a b) > 0
)",





}, {
"vector-get-all-combinations",
"Procedure",
R"(
(vector-get-all-combinations <vector>)
)",
R"(
Generate vector of all combinations of objects in <vector>.
  *) See "get-all-combinations" for a list-based alternative!
)",





}, {
"hmap-keys",
"Procedure",
R"(
(hmap-keys <hash-map>)
)",
R"(
List of keys in <hash-map>.
)",





}, {
"hmap-vals",
"Procedure",
R"(
(hmap-vals <hash-map>)
)",
R"(
List of values in <hash-map>.
)",





}, {
"hmap-key?",
"Procedure",
R"(
(hmap-key? <hash-map> <key>)
  => <key> ::= symbol | string | number | character | boolean
)",
R"(
Confirm <key> is a key in <hash-map>.
)",





}, {
"hmap-hashable?",
"Procedure",
R"(
(hmap-hashable? <obj>)
)",
R"(
Confirm <obj> is a viable hmap key type.
  *) Keys ::= symbol | string | number | character | boolean
)",





}, {
"hmap-ref",
"Procedure",
R"(
(hmap-ref <hash-map> <key>)
  => <key> ::= symbol | string | number | character | boolean
)",
R"(
Access value in <hash-map> associated to <key>.
)",





}, {
"hmap-set!",
"Procedure",
R"(
(hmap-set! <hash-map> <key> <value>)
  => <key> ::= symbol | string | number | character | boolean
)",
R"(
Set value associated to <key> in <hash-map> to <value>.
  *) Use "hmap-hashable?" to verify <key> is a valid hmap key!
)",





}, {
"hmap-delete!",
"Procedure",
R"(
(hmap-delete! <hash-map> <key>)
  => <key> ::= symbol | string | number | character | boolean
)",
R"(
Delete association to <key> in <hash-map>.
)",





}, {
"hmap-length",
"Procedure",
R"(
(hmap-length <hash-map>)
)",
R"(
Number of entries in <hash-map>.
  *) Use "hmap-empty?" as the composition of "zero?" and "hmap-length".
)",





}, {
"hmap-empty?",
"Procedure",
R"(
(hmap-empty? <hash-map>)
)",
R"(
Confirm <hash-map> is empty.
Equivalent to: (zero? (hmap-length <hash-map>))
)",





}, {
"hmap-merge",
"Procedure",
R"(
(hmap-merge <hash-map-1> <hash-map-2> ...)
)",
R"(
Merge hash-maps into a new copy.
  *) Keys of hmaps on the left take precedence over those on the right!
  *) Use "hmap-merge!" for a mutative version affecting <hash-map-1>!
)",





}, {
"hmap-merge!",
"Procedure",
R"(
(hmap-merge! <hash-map-1> <hash-map-2> ...)
)",
R"(
Merge <hash-map-2> ... into <hash-map-1> (mutating <hash-map-1>).
  *) Keys of hmaps on the left take precedence over those on the right!
  *) Use "hmap-merge" for a copying version generating a new hmap!
)",





}, {
"hmap-for-each",
"Procedure",
R"(
(hmap-for-each <callable> <hash-map>)
)",
R"(
Iterate over the key-value pairs in <hash-map> using <callable>.
  *) See "hmap-for-each-key" for a key-based alternative!
  *) See "hmap-for-each-val" for a value-based alternative!
)",





}, {
"hmap-for-each-key",
"Procedure",
R"(
(hmap-for-each-key <callable> <hash-map>)
)",
R"(
Iterate over the keys in <hash-map> using <callable>.
  *) See "hmap-for-each" for a key-value pair based alternative!
  *) See "hmap-for-each-val" for a value-based alternative!
)",





}, {
"hmap-for-each-val",
"Procedure",
R"(
(hmap-for-each-val <callable> <hash-map>)
)",
R"(
Iterate over the values in <hash-map> using <callable>.
  *) See "hmap-for-each" for a key-value pair based alternative!
  *) See "hmap-for-each-key" for a key-based alternative!
)",





}, {
"hmap-map",
"Procedure",
R"(
(hmap-map <callable> <hash-map>)
)",
R"(
New hash-map of values in <hash-map> being mapped over via <callable>.
  *) See "hmap-map!" for a mutative alternative affecting <hash-map>.
)",





}, {
"hmap-map!",
"Procedure",
R"(
(hmap-map! <callable> <hash-map>)
)",
R"(
Map (using <callable>) over the values in <hash-map> (mutating it!).
  *) See "hmap-map" for a copying alternative generating a new hmap.
)",





}, {
"empty",
"Procedure",
R"(
(empty <sequence>) 
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Generate an empty version of the given sequence.
)",





}, {
"length",
"Procedure",
R"(
(length <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get sequence length.
  *) Use "length+" to return #f (instead of triggering an error) 
     when given a circular list!
)",





}, {
"length+",
"Procedure",
R"(
(length+ <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get sequence length. Returns #f if given a circular list. 
  *) This is as opposed to triggering an error, as "length" would.
)",





}, {
"reverse",
"Procedure",
R"(
(reverse <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get reverse of a sequence.
  *) See "reverse!" for a mutative version affecting <sequence>.
)",





}, {
"reverse!",
"Procedure",
R"(
(reverse! <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Mutate <sequence> by reversing it.
  *) See "reverse" for a copying version generating a new sequence.
)",





}, {
"fold",
"Procedure",
R"(
(fold <callable> <seed> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Accumulate sequence from left to right, starting with <seed> using <callable>.
  *) See "fold-right" to perform the same operation from the right!
)",





}, {
"fold-right",
"Procedure",
R"(
(fold-right <callable> <seed> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Accumulate sequence from right to left, starting with <seed> using <callable>.
  *) See "fold" to perform the same operation from the left!
)",





}, {
"map",
"Procedure",
R"(
(map <callable> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Apply <callable> to each elt in each sequence, forming a sequence of results.
  *) See "map!" for a mutative alternative affecting <sequence1>.
)",





}, {
"map!",
"Procedure",
R"(
(map! <callable> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Apply <callable> to each elt in each sequence, mapping the result onto the 1st sequence.
  *) See "map" for a copying alternative that generates a new sequence.
)",





}, {
"filter",
"Procedure",
R"(
(filter <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Form a sequence of elts from <sequence> satisfying <predicate?>.
)",





}, {
"for-each",
"Procedure",
R"(
(for-each <callable> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Apply <callable> to each elt of each <sequence>.
)",





}, {
"seq-copy!",
"Procedure",
R"(
(seq-copy! <dest-sequence> <source-sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Copy <source-sequence> to <dest-sequence>.
)",





}, {
"count",
"Procedure",
R"(
(count <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Count total elts satisfying <predicate?>.
)",





}, {
"ref",
"Procedure",
R"(
(ref <sequence> <index>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get elt in <sequence> at index <index>.
)",





}, {
"slice",
"Procedure",
R"(
(slice <sequence> <start-index> <optional-length>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get a subsequence in <sequence> from <start-index> with <optional-length> elts.
  *) <optional-length> defaults to the end of <sequence> if not included!
  *) if <optional-length> < 0, its absolute value is the offset till which to 
     slice from the end of the sequence.

     (slice "hello!" 2)      ; "llo!"
     (slice "hello!" 2 1)    ; "l"
     (slice "hello!" 2 100)  ; "llo!" ; extra length => go to end
     (slice "hello!" 2 -1)   ; "llo"  ; offset till end of sequence by "1"
     (slice "hello!" 2 -100) ; ""     ; extra negative offset => empty sequence
)",





}, {
"set-index!",
"Procedure",
R"(
(set-index! <sequence> <index> <obj>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Set elt in <sequence> at <index> to be <obj>.
)",





}, {
"swap-indices!",
"Procedure",
R"(
(swap-indices! <sequence> <index> <index>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Swap elts at either index in <sequence>.
)",





}, {
"fill!",
"Procedure",
R"(
(fill! <sequence> <fill-value>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Fill the sequence with <fill-value>.
)",





}, {
"append",
"Procedure",
R"(
(append <sequence1> ... <sequenceN> <obj>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Append the sequences (& optional last object) to form a new sequence.
  *) If <sequence> is a <vector> or <string>, <obj> added via '.push_back()'
  *) If <sequence> is a <list>, <obj> is added such that the result is a DOTTED-LIST!

     (append '#(1 2) '#(3 4) 5) ; '#(1 2 3 4 5)
     (append "he" "ll" #\o)     ; "hello"
     (append '(1 2) '(3 4) 5)   ; '(1 2 3 4 . 5) ; dotted-list!
)",





}, {
"remove",
"Procedure",
R"(
(remove <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get copy of <sequence> w/o all elts satisfying <predicate?>.
  *) See "remove-first" & "remove-last" for similar operations!
)",





}, {
"remove-first",
"Procedure",
R"(
(remove-first <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get copy of <sequence> w/o 1st elt satisfying <predicate?>.
  *) See "remove" & "remove-last" for similar operations!
)",





}, {
"remove-last",
"Procedure",
R"(
(remove-last <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get copy of <sequence> w/o last elt satisfying <predicate?>.
  *) See "remove" & "remove-first" for similar operations!
)",





}, {
"delete",
"Procedure",
R"(
(delete <sequence> <index>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get copy of <sequence> w/o elt positioned at <index>.
)",





}, {
"last",
"Procedure",
R"(
(last <sequence> <index>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get the last elt in <sequence>.
  *) See "tail", "head", & "init" for similar getters.
)",





}, {
"tail",
"Procedure",
R"(
(tail <sequence> <index>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get a copy of the sequence w/ every elt except the 1st one.
  *) See "last", "head", & "init" for similar getters.
)",





}, {
"head",
"Procedure",
R"(
(head <sequence> <index>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get the 1st elt in <sequence>.
  *) See "last", "tail", & "init" for similar getters.
)",





}, {
"init",
"Procedure",
R"(
(init <sequence> <index>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get a copy of the sequence w/ every elt except the last one.
  *) See "last", "tail", & "head" for similar getters.
)",





}, {
"seq=",
"Procedure",
R"(
(seq= <predicate?> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Confirm equality (via <predicate?>) among elts in each sequence.
)",





}, {
"skip",
"Procedure",
R"(
(skip <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get 1st elt after <predicate?> is true.
  *) See "skip-right" to perform this operation from the right!
)",





}, {
"skip-right",
"Procedure",
R"(
(skip-right <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get last elt after <predicate?> is true.
  *) See "skip" to perform this operation from the left!
)",





}, {
"index",
"Procedure",
R"(
(index <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get index of 1st elt satisfying <predicate?>.
  *) See "index-right" to perform this operation from the right!
)",





}, {
"index-right",
"Procedure",
R"(
(index-right <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Get index of last elt satisfying <predicate?>.
  *) See "index" to perform this operation from the left!
)",





}, {
"drop",
"Procedure",
R"(
(drop <sequence> <length>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Drop <length> elts from the left of <sequence>.
  *) See "drop-right", "drop-while", & "drop-right-while" for similar operations!
)",





}, {
"drop-right",
"Procedure",
R"(
(drop-right <sequence> <length>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Drop <length> elts from the right of <sequence>.
  *) See "drop", "drop-while", & "drop-right-while" for similar operations!
)",





}, {
"drop-while",
"Procedure",
R"(
(drop-while <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Drop elts while they satisfy <predicate?> from the left of <sequence>.
  *) See "drop", "drop-right", & "drop-right-while" for similar operations!
)",





}, {
"drop-right-while",
"Procedure",
R"(
(drop-right-while <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Drop elts while they satisfy <predicate?> from the right of <sequence>.
  *) See "drop", "drop-right", & "drop-while" for similar operations!
)",





}, {
"take",
"Procedure",
R"(
(take <sequence> <length>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Take <length> elts from the left of <sequence>.
  *) See "take-right", "take-while", & "take-right-while" for similar operations!
)",





}, {
"take-right",
"Procedure",
R"(
(take-right <sequence> <length>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Take <length> elts from the right of <sequence>.
  *) See "take", "take-while", & "take-right-while" for similar operations!
)",





}, {
"take-while",
"Procedure",
R"(
(take-while <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Take elts while they satisfy <predicate?> from the left of <sequence>.
  *) See "take", "take-right", & "take-right-while" for similar operations!
)",





}, {
"take-right-while",
"Procedure",
R"(
(take-right-while <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Take elts while they satisfy <predicate?> from the right of <sequence>.
  *) See "take", "take-right", & "take-while" for similar operations!
)",





}, {
"any",
"Procedure",
R"(
(any <predicate?> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Confirm any sequence satisfies <predicate?>.
  *) See "every" for this operation's dual!
)",





}, {
"every",
"Procedure",
R"(
(every <predicate?> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Confirm every sequence satisfies <predicate?>.
  *) See "every" for this operation's dual!
)",





}, {
"conj",
"Procedure",
R"(
(conj <obj> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Generic "cons": "cons" for lists, a copying 'push-back' for strings & vectors.
)",





}, {
"union",
"Procedure",
R"(
(union <predicate?> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Set Operation: union of <sequence1> <sequence2> ... satisfying <predicate?>.
  *) Other set operations include "intersection", "difference", & "symmetric-difference"!
)",





}, {
"intersection",
"Procedure",
R"(
(intersection <predicate?> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Set Operation: intersection of <sequence1> <sequence2> ... satisfying <predicate?>.
  *) Other set operations include "union", "difference", & "symmetric-difference"!
)",





}, {
"difference",
"Procedure",
R"(
(difference <predicate?> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Set Operation: difference of <sequence1> <sequence2> ... satisfying <predicate?>.
  *) Other set operations include "union", "intersection", & "symmetric-difference"!
)",





}, {
"symmetric-difference",
"Procedure",
R"(
(symmetric-difference <predicate?> <sequence1> <sequence2> ...)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Set Operation: symmetric-difference of <sequence1> <sequence2> ... satisfying <predicate?>.
  *) Other set operations include "union", "intersection", & "difference"!
)",





}, {
"sort",
"Procedure",
R"(
(sort <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Copy of <sequence> sorted based on <predicate?>.
  *) See "sort!" for a mutative equivalent affecting <sequence>.
)",





}, {
"sort!",
"Procedure",
R"(
(sort! <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
In-place sort (based on <predicate?>) mutating <sequence>.
  *) See "sort" for a non-mutating equivalent.
)",





}, {
"sorted?",
"Procedure",
R"(
(sorted? <predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Confirm <sequence> is sorted according to <predicate?>.
)",





}, {
"merge",
"Procedure",
R"(
(merge <predicate?> <sequence1> <sequence2>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Merge <sequence1> & <sequence2> ASSUMING they've been sorted using <predicate?>.
)",





}, {
"delete-neighbor-dups",
"Procedure",
R"(
(delete-neighbor-dups <equality-predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Delete neighboring duplicates in <sequence> based on <equality-predicate?>.
)",





}, {
"delete-neighbor-dups!",
"Procedure",
R"(
(delete-neighbor-dups! <equality-predicate?> <sequence>)
=> <sequence> ::= <list> | <vector> | <string>
)",
R"(
Mutative deletion of neighboring duplicates in <sequence> based on <equality-predicate?>.
)",





}, {
"undefined?",
"Procedure",
R"(
(undefined? <obj>)
)",
R"(
Undefined object predicate.
  *) Create undefined objects via the "undefined" procedure.
)",





}, {
"void?",
"Procedure",
R"(
(void? <obj>)
)",
R"(
Void object predicate.
  *) Create void objects via the "void" procedure.
)",





}, {
"empty?",
"Procedure",
R"(
(empty? <obj>)
)",
R"(
Empty sequence predicate.
  *) sequence ::= <list> | <vector> | <string>
)",





}, {
"pair?",
"Procedure",
R"(
(pair? <obj>)
)",
R"(
Pair predicate. Equivalent to (compose not atom?).
)",





}, {
"vector?",
"Procedure",
R"(
(vector? <obj>)
)",
R"(
Vector predicate.
)",





}, {
"hmap?",
"Procedure",
R"(
(hmap? <obj>)
)",
R"(
Hmap (hash-map) predicate.
)",





}, {
"char?",
"Procedure",
R"(
(char? <obj>)
)",
R"(
Character predicate.
)",





}, {
"number?",
"Procedure",
R"(
(number? <obj>)
)",
R"(
Number predicate.
)",





}, {
"real?",
"Procedure",
R"(
(real? <obj>)
)",
R"(
Real-number predicate.
)",





}, {
"complex?",
"Procedure",
R"(
(complex? <obj>)
)",
R"(
Complex-number predicate.
)",





}, {
"rational?",
"Procedure",
R"(
(rational? <obj>)
)",
R"(
Rational-number predicate.
)",





}, {
"string?",
"Procedure",
R"(
(string? <obj>)
)",
R"(
String predicate.
)",





}, {
"symbol?",
"Procedure",
R"(
(symbol? <obj>)
)",
R"(
Symbol predicate.
)",





}, {
"boolean?",
"Procedure",
R"(
(boolean? <obj>)
)",
R"(
Boolean predicate.
)",





}, {
"atom?",
"Procedure",
R"(
(atom? <obj>)
)",
R"(
Atom predicate. Equivalent to (compose not pair?).
)",





}, {
"procedure?",
"Procedure",
R"(
(procedure? <obj>)
)",
R"(
Procedure predicate.
  *) Note: prefer "callable?" to also accept functors!
)",





}, {
"functor?",
"Procedure",
R"(
(functor? <obj>)
)",
R"(
Functor predicate. Functors are any <object> constructed from a class prototype w/
a <self->procedure> method defined. Note that these are distinct from procedures!
  *) Use "callable?" to check for either functors or procedures at once!
)",





}, {
"callable?",
"Procedure",
R"(
(callable? <obj>)
)",
R"(
Callable predicate. Callables are defined as either procedures or functors.
  *) Use "procedure?" or "functor?" to check explicitly for either type, BUT
     prefer "callable?" for writing more generic code!
)",





}, {
"input-port?",
"Procedure",
R"(
(input-port? <obj>)
)",
R"(
Input-port predicate.
)",





}, {
"output-port?",
"Procedure",
R"(
(output-port? <obj>)
)",
R"(
Output-port predicate.
)",





}, {
"eof-object?",
"Procedure",
R"(
(eof-object? <obj>)
)",
R"(
EOF object predicate. Note that the EOF object can only be generated by:
  0. The "eof" primitive procedure
  1. Any procedure streaming file contents (last object streamed to denote end)
     *) Under the hood EOF is a character, BUT the reader explicitly doesn't 
        support reading it in as a literal (MUST be user-created via the "eof" 
        procedure. (integer->char (char->integer (eof))) is undefined behavior).
)",





}, {
"stream-pair?",
"Procedure",
R"(
(stream-pair? <obj>)
)",
R"(
Stream pair predicate.
Equivalent to: (and (pair? <obj>) (delay? (car <obj>)) (delay? (cdr <obj>)))
)",





}, {
"stream-null?",
"Procedure",
R"(
(stream-null? <obj>)
)",
R"(
Empty stream predicate. Alias for "null?".
)",





}, {
"stream?",
"Procedure",
R"(
(stream? <obj>)
)",
R"(
Stream predicate.
Equivalent to: (or (stream-pair? <obj>) (stream-null? <obj>))
)",





}, {
"syntax-rules-object?",
"Procedure",
R"(
(syntax-rules-object? <obj>)
)",
R"(
Syntax-rules object predicate. 
These can only be made by the "syntax-rules" special form.
)",





}, {
"seq?",
"Procedure",
R"(
(seq? <obj>)
)",
R"(
Sequence predicate. Sequences are either <list>s, <vector>s, or <string>s.
)",





}, {
"object?",
"Procedure",
R"(
(object? <obj>)
)",
R"(
Object predicate. 
Object in terms of one constructed from a class-prototype defined by <defclass>.
)",





}, {
"class-prototype?",
"Procedure",
R"(
(class-prototype? <obj>)
)",
R"(
Class prototype predicate. 
These can only be made by the "defclass" special form.
)",





}, {
"eval",
"Procedure",
R"(
(eval <data> <optional-environment>)
)",
R"(
Run "quote"d code as data!
  *) <optional-environment> variants:
     0. *null-environment*   => eval in an empty environment!
     1. *local-environment*  => eval in the local environment (default)!
     2. *global-environment* => eval in the global environment!
)",





}, {
"cps-eval",
"Procedure",
R"(
(cps-eval <data> <optional-environment> <continuation>)
)",
R"(
Alternative to "eval" in CPS contexts (<continuation> provided by default!). 
Run "quote"d code as data!
  *) <optional-environment> variants:
     0. *null-environment*   => cps-eval in an empty environment!
     1. *local-environment*  => cps-eval in the local environment (default)!
     2. *global-environment* => cps-eval in the global environment!
)",





}, {
"apply",
"Procedure",
R"(
(apply <callable> <argument-list>)
)",
R"(
Apply <callable> to <argument-list>.
)",





}, {
"symbol-append",
"Procedure",
R"(
(symbol-append <symbol-1> ... <symbol-N>)
)",
R"(
Append symbols to generate a new symbol.
)",





}, {
"typeof",
"Procedure",
R"(
(typeof <obj>)
)",
R"(
Get name of <obj>'s type as a symbol.
)",





}, {
"copy",
"Procedure",
R"(
(copy <obj>)
)",
R"(
Deep-copy <obj> recursively.
  *) Deep-copies vectors, strings, proper/dotted/circular lists, hmaps, & objects!
)",





}, {
"shallow-copy",
"Procedure",
R"(
(shallow-copy <obj>)
)",
R"(
Shallow-copy <obj>.
  *) Shallow-copies vectors, strings, proper/dotted/circular lists, hmaps, & objects!
  *) Note that this performs STRUCTURAL allocation w/ shallow content copying.
     => Hence "copy" and "shallow-copy" are effectively identical for strings!
)",





}, {
"compose",
"Procedure",
R"(
(compose <callable-1> ... <callable-N>)
)",
R"(
Compose N callables!
  *) Aliased as "o" for composition shorthand!
  *) Generates a procedure of N args that applies them to the callable composition!
)",





}, {
"bind",
"Procedure",
R"(
(bind <callable> <val-1> ... <val-N>)
)",
R"(
Bind N arguments to <callable>.
  *) Generates a procedure that when invoked calls the arg-bound <callable>!
  *) Example: ((bind map even?) '(1 2 3)) is equivalent to (map even? '(1 2 3))
     => IRL primitives support partials by default, so (bind map even?) == (map even?)
)",





}, {
"id",
"Procedure",
R"(
(id <obj>)
)",
R"(
The identity procedure. Returns <obj>.
  *) Can be useful when used as a higher-order callable.
)",





}, {
"delay?",
"Procedure",
R"(
(delay? <obj>)
)",
R"(
Delayed expression predicate. 
  *) Unlike most Schemes, Heist Scheme supports 1st-class delayed 
     expressions, and hence "delay?" is distinct from "procedure?".
)",





}, {
"force",
"Procedure",
R"(
(force <delayed-expression>)
)",
R"(
Force the given delayed expression to evaluate.
  *) Delay expressions via the "delay" special form!
)",





}, {
"char->integer",
"Procedure",
R"(
(char->integer <char>)
)",
R"(
Char to integer coercion.
)",





}, {
"integer->char",
"Procedure",
R"(
(integer->char <int>)
)",
R"(
Integer to char coercion.
  *) <int> MUST be in range of [0, 255]!
)",





}, {
"number->string",
"Procedure",
R"(
(number->string <number> <optional-radix> <optional-precision>)
)",
R"(
Number to string coercion.
  *) <optional-radix> ::= [2, 36]
  *) <optional-precision> ::= Integer
)",





}, {
"string->number",
"Procedure",
R"(
(string->number <string> <optional-radix>)
)",
R"(
String to number coercion.
  *) <optional-radix> ::= [2, 36]
)",





}, {
"string->symbol",
"Procedure",
R"(
(string->symbol <string>)
)",
R"(
String to symbol coercion.
)",





}, {
"symbol->string",
"Procedure",
R"(
(symbol->string <symbol>)
)",
R"(
Symbol to string coercion.
)",





}, {
"vector->list",
"Procedure",
R"(
(vector->list <vector>)
)",
R"(
Vector to list coercion.
)",





}, {
"list->vector",
"Procedure",
R"(
(list->vector <list>)
)",
R"(
List to vector coercion.
)",





}, {
"string->vector",
"Procedure",
R"(
(string->vector <string>)
)",
R"(
String to vector coercion.
)",





}, {
"vector->string",
"Procedure",
R"(
(vector->string <vector>)
)",
R"(
Vector to string coercion (vector MUST only contain characters!).
)",





}, {
"string->list",
"Procedure",
R"(
(string->list <string>)
)",
R"(
String to list coercion.
)",





}, {
"list->string",
"Procedure",
R"(
(list->string <list>)
)",
R"(
List to string coercion (list MUST only contain characters!).
)",





}, {
"hmap->alist",
"Procedure",
R"(
(hmap->alist <hash-map>)
)",
R"(
Hmap to alist coercion.
)",





}, {
"alist->hmap",
"Procedure",
R"(
(alist->hmap <alist>)
)",
R"(
Alist to hmap coercion.
)",





}, {
"stream->list",
"Procedure",
R"(
(stream->list <stream> <size>)
)",
R"(
Convert <size> elements from the left of <stream> to a <list>.
  *) <size> parameter enables converting portions of infinite streams to lists.
)",





}, {
"list->stream",
"Procedure",
R"(
(list->stream <list>)
)",
R"(
List to stream coercion.
)",





}, {
"object->hmap",
"Procedure",
R"(
(object->hmap <object>)
)",
R"(
Object to hmap coercion.
)",





}, {
"object->alist",
"Procedure",
R"(
(object->alist <object>)
)",
R"(
Object to alist coercion.
)",





}, {
"functor->procedure",
"Procedure",
R"(
(functor->procedure <functor>)
)",
R"(
Functor to procedure coercion.
)",





}, {
"pretty-print",
"Procedure",
R"(
(pretty-print <obj> <optional-open-output-port-or-string>)
(pprint <obj> <optional-open-output-port-or-string>)
)",
R"(
Pretty-print <obj> to <optional-open-output-port-or-string>.
Equivalent to "write", BUT auto-indents lists.
  *) Makes "pretty-print" ideal for printing out quoted code!
  *) See "pretty-printf"/"pprintf" for a formatted-string equivalent!
)",





}, {
"write",
"Procedure",
R"(
(write <obj> <optional-open-output-port-or-string>)
)",
R"(
Write <obj> to <optional-open-output-port-or-string>.
Outputs <obj> in a machine-readable way: ideally "write" is "read"'s dual.
  *) See "display" for a human-readable printing alternative!
  *) See "writef" for a formatted-string equivalent!
)",





}, {
"display",
"Procedure",
R"(
(display <obj> <optional-open-output-port-or-string>)
)",
R"(
Display <obj> to <optional-open-output-port-or-string>.
Outputs <obj> in a human-readable way: NOT meant to output code to a file!
  *) See "write" for a machine-readable printing alternative!
  *) See "displayf" for a formatted-string equivalent!
)",





}, {
"newline",
"Procedure",
R"(
(newline <optional-open-output-port-or-string>)
)",
R"(
Displays #\newline to <optional-open-output-port-or-string>.
)",





}, {
"write-char",
"Procedure",
R"(
(write-char <char> <optional-open-output-port-or-string>)
)",
R"(
Write <char> to <optional-open-output-port-or-string>.
)",





}, {
"sprintf",
"Procedure",
R"(
(sprintf <formatted-string> <optional-arg1> <optional-arg2> ...)
)",
R"(
Returns a new, formatted string!
  *) <formatted-string> is like C's printf with unique formatting patterns:
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
     %#rn = %#Rn = number (in base <#> from 2 to 36)
     %#n = number (left-padded with 0s to a width of <#> characters)
     %.#n = number (with <#> digits of precision)
     -> IE "%+e2r.5n": 5 digits of precision & mk exact in binary w/ sign
     -> NOTE: case of 'n' in "%n" denotes case of base >= 11 letters
     -> NOTE: 0-padding & precision MUST be of 2 digits or less!
     ----------------------------------------------------------------------
     %$ = display real finite as a dollar value
     ----------------------------------------------------------------------
     %s = display string
     %#s = display string & pad left with # spaces
     %-#s = display string & pad right with # spaces
     %ws = write string
     -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)
     ----------------------------------------------------------------------
     %c = display char
     %wc = write char
     ----------------------------------------------------------------------
     %b  = bool
     %wb = write "true" or "false" instead of "#t" or "#f"
     ----------------------------------------------------------------------
     %%  = "%" (escapes a "%")
     ----------------------------------------------------------------------
)",





}, {
"displayf",
"Procedure",
R"(
(displayf <optional-output-port> <formatted-string> <optional-arg1> ...)
)",
R"(
Display the formatted string!
  *) <formatted-string> is like C's printf with unique formatting patterns:
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
     %#rn = %#Rn = number (in base <#> from 2 to 36)
     %#n = number (left-padded with 0s to a width of <#> characters)
     %.#n = number (with <#> digits of precision)
     -> IE "%+e2r.5n": 5 digits of precision & mk exact in binary w/ sign
     -> NOTE: case of 'n' in "%n" denotes case of base >= 11 letters
     -> NOTE: 0-padding & precision MUST be of 2 digits or less!
     ----------------------------------------------------------------------
     %$ = display real finite as a dollar value
     ----------------------------------------------------------------------
     %s = display string
     %#s = display string & pad left with # spaces
     %-#s = display string & pad right with # spaces
     %ws = write string
     -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)
     ----------------------------------------------------------------------
     %c = display char
     %wc = write char
     ----------------------------------------------------------------------
     %b  = bool
     %wb = write "true" or "false" instead of "#t" or "#f"
     ----------------------------------------------------------------------
     %%  = "%" (escapes a "%")
     ----------------------------------------------------------------------
)",





}, {
"writef",
"Procedure",
R"(
(writef <optional-output-port> <formatted-string> <optional-arg1> ...)
)",
R"(
Write the formatted string!
  *) <formatted-string> is like C's printf with unique formatting patterns:
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
     %#rn = %#Rn = number (in base <#> from 2 to 36)
     %#n = number (left-padded with 0s to a width of <#> characters)
     %.#n = number (with <#> digits of precision)
     -> IE "%+e2r.5n": 5 digits of precision & mk exact in binary w/ sign
     -> NOTE: case of 'n' in "%n" denotes case of base >= 11 letters
     -> NOTE: 0-padding & precision MUST be of 2 digits or less!
     ----------------------------------------------------------------------
     %$ = display real finite as a dollar value
     ----------------------------------------------------------------------
     %s = display string
     %#s = display string & pad left with # spaces
     %-#s = display string & pad right with # spaces
     %ws = write string
     -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)
     ----------------------------------------------------------------------
     %c = display char
     %wc = write char
     ----------------------------------------------------------------------
     %b  = bool
     %wb = write "true" or "false" instead of "#t" or "#f"
     ----------------------------------------------------------------------
     %%  = "%" (escapes a "%")
     ----------------------------------------------------------------------
)",





}, {
"pretty-printf",
"Procedure",
R"(
(pretty-printf <optional-output-port> <formatted-string> <optional-arg1> ...) 
(pprintf <optional-output-port> <formatted-string> <optional-arg1> ...)
)",
R"(
Pretty-print the formatted string!
  *) <formatted-string> is like C's printf with unique formatting patterns:
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
     %#rn = %#Rn = number (in base <#> from 2 to 36)
     %#n = number (left-padded with 0s to a width of <#> characters)
     %.#n = number (with <#> digits of precision)
     -> IE "%+e2r.5n": 5 digits of precision & mk exact in binary w/ sign
     -> NOTE: case of 'n' in "%n" denotes case of base >= 11 letters
     -> NOTE: 0-padding & precision MUST be of 2 digits or less!
     ----------------------------------------------------------------------
     %$ = display real finite as a dollar value
     ----------------------------------------------------------------------
     %s = display string
     %#s = display string & pad left with # spaces
     %-#s = display string & pad right with # spaces
     %ws = write string
     -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)
     ----------------------------------------------------------------------
     %c = display char
     %wc = write char
     ----------------------------------------------------------------------
     %b  = bool
     %wb = write "true" or "false" instead of "#t" or "#f"
     ----------------------------------------------------------------------
     %%  = "%" (escapes a "%")
     ----------------------------------------------------------------------
)",





}, {
"string->ascii-art",
"Procedure",
R"(
(string->ascii-art <string>)
)",
R"(
Convert string to an ASCII art string!
  *) Supports Non-Whitespace ASCII, Space, Newline, Tab, Backspace, Esc!
)",





}, {
"string->space-art",
"Procedure",
R"(
(string->space-art <string>)
)",
R"(
Convert string to a Whitespace art string!
  *) Supports Non-Whitespace ASCII, Space, Newline, Tab, Backspace, Esc!
)",





}, {
"fmt:reset",
"Procedure",
R"(
(fmt:reset)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape code to reset current ANSI formats.
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:clear",
"Procedure",
R"(
(fmt:clear)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape code to clear the screen.
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:bold",
"Procedure",
R"(
(fmt:bold)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape code for bolded text.
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:line",
"Procedure",
R"(
(fmt:line)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape code for underlined text.
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:rev",
"Procedure",
R"(
(fmt:rev)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape code to reverse background & foreground colors.
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:black",
"Procedure",
R"(
(fmt:black)
(fmt:black1) ... (fmt:black8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "black" text color
  *) Use "fmt:black1" ... "fmt:black8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:red",
"Procedure",
R"(
(fmt:red)
(fmt:red1) ... (fmt:red8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "red" text color
  *) Use "fmt:red1" ... "fmt:red8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:green",
"Procedure",
R"(
(fmt:green)
(fmt:green1) ... (fmt:green8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "green" text color
  *) Use "fmt:green1" ... "fmt:green8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:yellow",
"Procedure",
R"(
(fmt:yellow)
(fmt:yellow1) ... (fmt:yellow8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "yellow" text color
  *) Use "fmt:yellow1" ... "fmt:yellow8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:blue",
"Procedure",
R"(
(fmt:blue)
(fmt:blue1) ... (fmt:blue8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "blue" text color
  *) Use "fmt:blue1" ... "fmt:blue8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:magenta",
"Procedure",
R"(
(fmt:magenta)
(fmt:magenta1) ... (fmt:magenta8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "magenta" text color
  *) Use "fmt:magenta1" ... "fmt:magenta8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:cyan",
"Procedure",
R"(
(fmt:cyan)
(fmt:cyan1) ... (fmt:cyan8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "cyan" text color
  *) Use "fmt:cyan1" ... "fmt:cyan8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:white",
"Procedure",
R"(
(fmt:white)
(fmt:white1) ... (fmt:white8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "white" text color
  *) Use "fmt:white1" ... "fmt:white8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:bblack",
"Procedure",
R"(
(fmt:bblack)
(fmt:bblack1) ... (fmt:bblack8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "black" background color!
  *) Use "fmt:bblack1" ... "fmt:bblack8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:bred",
"Procedure",
R"(
(fmt:bred)
(fmt:bred1) ... (fmt:bred8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "red" background color!
  *) Use "fmt:bred1" ... "fmt:bred8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:bgreen",
"Procedure",
R"(
(fmt:bgreen)
(fmt:bgreen1) ... (fmt:bgreen8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "green" background color!
  *) Use "fmt:bgreen1" ... "fmt:bgreen8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:byellow",
"Procedure",
R"(
(fmt:byellow)
(fmt:byellow1) ... (fmt:byellow8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "yellow" background color!
  *) Use "fmt:byellow1" ... "fmt:byellow8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:bblue",
"Procedure",
R"(
(fmt:bblue)
(fmt:bblue1) ... (fmt:bblue8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "blue" background color!
  *) Use "fmt:bblue1" ... "fmt:bblue8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:bmagenta",
"Procedure",
R"(
(fmt:bmagenta)
(fmt:bmagenta1) ... (fmt:bmagenta8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "magenta" background color!
  *) Use "fmt:bmagenta1" ... "fmt:bmagenta8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:bcyan",
"Procedure",
R"(
(fmt:bcyan)
(fmt:bcyan1) ... (fmt:bcyan8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "cyan" background color!
  *) Use "fmt:bcyan1" ... "fmt:bcyan8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"fmt:bwhite",
"Procedure",
R"(
(fmt:bwhite)
(fmt:bwhite1) ... (fmt:bwhite8)
=> returns "" if -nansi is active!
)",
R"(
ANSI escape codes for the "white" background color!
  *) Use "fmt:bwhite1" ... "fmt:bwhite8" for dark->light variants!
  *) Display to the console in order to activate such IFF supported by your system!
)",





}, {
"read",
"Procedure",
R"(
(read <optional-open-input-port-or-string>)
)",
R"(
Get input from <optional-open-input-port-or-string> as a quoted datum!
)",





}, {
"read-string",
"Procedure",
R"(
(read-string <optional-open-input-port-or-string>)
)",
R"(
Read the next expression into a string!
)",





}, {
"read-line",
"Procedure",
R"(
(read-line <optional-open-input-port-or-string>)
)",
R"(
Read the next line of input into a string.
)",





}, {
"read-char",
"Procedure",
R"(
(read-char <optional-open-input-port-or-string>)
)",
R"(
Read the next character of input.
)",





}, {
"peek-char",
"Procedure",
R"(
(peek-char <optional-open-input-port-or-string>)
)",
R"(
Peek the next character (doesn't take it out of the input buffer).
)",





}, {
"char-ready?",
"Procedure",
R"(
(char-ready? <optional-open-input-port-or-string>)
)",
R"(
Confirm character is ready to be read.
)",





}, {
"slurp-port",
"Procedure",
R"(
(slurp-port <optional-open-input-port-or-string>)
)",
R"(
Slurp entire port contents into a string.
)",





}, {
"slurp-file",
"Procedure",
R"(
(slurp-file <filename-string>)
)",
R"(
Slurp entire file contents into a string.
)",





}, {
"read-port",
"Procedure",
R"(
(read-port <optional-open-input-port-or-string>)
)",
R"(
Read entire port contents as a data structure.
)",





}, {
"read-file",
"Procedure",
R"(
(read-file <filename-string>)
)",
R"(
Read entire file contents as a data structure.
)",





}, {
"file?",
"Procedure",
R"(
(file? <filename-string>)
)",
R"(
File/directory predicate.
)",





}, {
"delete-file!",
"Procedure",
R"(
(delete-file! <filename-string>)
)",
R"(
Delete file/directory <filename-string>.
)",





}, {
"rename-file!",
"Procedure",
R"(
(rename-file! <old-name-string> <new-name-string>)
)",
R"(
Renames file/directory named <old-name-string> to <new-name-string>.
)",





}, {
"copy-file",
"Procedure",
R"(
(copy-file <source-path-string> <destination-path-string>)
)",
R"(
Copies file/directory named <source-path-string> to <destination-path-string>.
)",





}, {
"file-size",
"Procedure",
R"(
(file-size <filename-string>)
)",
R"(
Get size of the file designated by <filename-string>.
  *) NOTE: Behavior is platform-dependant when invoked on directories!
)",





}, {
"open-port?",
"Procedure",
R"(
(open-port? <port>)
)",
R"(
Open port predicate.
  *) Works for both input & output ports.
)",





}, {
"closed-port?",
"Procedure",
R"(
(closed-port? <port>)
)",
R"(
Closed port predicate.
  *) Works for both input & output ports.
)",





}, {
"current-input-port",
"Procedure",
R"(
(current-input-port)
)",
R"(
The current input port (C++'s stdin by default).
Default used by all read-related procedures unless given a specific input port.
)",





}, {
"current-output-port",
"Procedure",
R"(
(current-output-port)
)",
R"(
The current output port (C++'s stdout by default).
Default used by all write-related procedures unless given a specific output port.
)",





}, {
"call-with-input-file",
"Procedure",
R"(
(call-with-input-file <filename-string> <unary-port-callable>)
)",
R"(
Apply <unary-port-callable> to <filename-string> as an open input port.
)",





}, {
"call-with-output-file",
"Procedure",
R"(
(call-with-output-file <filename-string> <unary-port-callable>)
)",
R"(
Apply <unary-port-callable> to <filename-string> as an open output port.
)",





}, {
"with-input-from-file",
"Procedure",
R"(
(with-input-from-file <filename-string> <nullary-callable>)
)",
R"(
Invoke <nullary-callable> with <filename-string> set as the new "current-input-port".
  *) Resets "current-input-port" back to its original value upon callable's completion.
)",





}, {
"with-output-from-file",
"Procedure",
R"(
(with-output-from-file <filename-string> <nullary-callable>)
)",
R"(
Invoke <nullary-callable> with <filename-string> set as the new "current-output-port".
  *) Resets "current-output-port" back to its original value upon callable's completion.
)",





}, {
"open-input-file",
"Procedure",
R"(
(open-input-file <filename-string>)
)",
R"(
Generate an input port from <filename-string>.
)",





}, {
"open-output-file",
"Procedure",
R"(
(open-output-file <filename-string>)
)",
R"(
Generate an output port from <filename-string>.
  *) Only works if <filename-string> isn't already a file!
  *) See "open-output-file+" & "open-output-file!" for alternative output port generators!
)",





}, {
"open-output-file+",
"Procedure",
R"(
(open-output-file+ <filename-string>)
)",
R"(
Generate an output port from <filename-string>.
  *) Both creates new files & appends to existing files!
  *) See "open-output-file" & "open-output-file!" for alternative output port generators!
)",





}, {
"open-output-file!",
"Procedure",
R"(
(open-output-file! <filename-string>)
)",
R"(
Destructively generates an output port from <filename-string>.
  *) Equivalent to: 
     (begin (delete-file! <filename-string>) (open-output-file <filename-string>))
  *) See "open-output-file" & "open-output-file+" for alternative output port generators!
)",





}, {
"rewind-port!",
"Procedure",
R"(
(rewind-port! <input-or-output-port>)
)",
R"(
Rewind the given port back the beginning of its stream. 
Works on both input & output ports.
)",





}, {
"port-seek!",
"Procedure",
R"(
(port-seek! <open-port> <integer-offset>)
)",
R"(
Seek from the port's current position in the stream.
  *) Equivalent to C++'s "fseek" using "SEEK_CUR"
)",





}, {
"port-seek-front!",
"Procedure",
R"(
(port-seek-front! <open-port> <integer-offset>)
)",
R"(
Seek from the start of port's stream.
  *) Equivalent to C++'s "fseek" using "SEEK_SET"
)",





}, {
"close-port",
"Procedure",
R"(
(close-port <input-or-output-port>)
)",
R"(
Close the given port. Works on both input & output ports.
)",





}, {
"load",
"Procedure",
R"(
(load <filename-string> <optional-environment>)
)",
R"(
Load <filename-string> as scheme code in <optional-environment>.
  *) <optional-environment> variants:
     0. *null-environment*   => load in an empty environment!
     1. *local-environment*  => load in the local environment (default)!
     2. *global-environment* => load in the global environment!
)",





}, {
"cps-load",
"Procedure",
R"(
(cps-load <filename-string> <optional-environment> <continuation>)
)",
R"(
Alternative to "load" in CPS contexts (<continuation> provided by default!). 
Load <filename-string> as scheme code in <optional-environment>.
  *) <optional-environment> variants:
     0. *null-environment*   => cps-load in an empty environment!
     1. *local-environment*  => cps-load in the local environment (default)!
     2. *global-environment* => cps-load in the global environment!
)",





}, {
"system",
"Procedure",
R"(
(system <optional-system-call-string>)
)",
R"(
System interface procedure. Returns #f if feature not offered by OS.
Call w/o args to check if feature provided, call w/ args to run a command.
)",





}, {
"getenv",
"Procedure",
R"(
(getenv <variable-name-string>)
)",
R"(
Get variable's value as a string.
)",





}, {
"getcwd",
"Procedure",
R"(
(getcwd)
)",
R"(
Get the current working directory as a string.
)",





}, {
"dirname",
"Procedure",
R"(
(dirname <filepath-string>)
)",
R"(
Given a filepath string, get a string of its parent directory.
)",





}, {
"mkdir",
"Procedure",
R"(
(mkdir <new-directory-name-string>)
)",
R"(
Create a new directory.
)",





}, {
"chdir",
"Procedure",
R"(
(chdir <directory-path-string>)
)",
R"(
Change the current working directory.
)",





}, {
"compile",
"Procedure",
R"(
(compile <filename-string> <optional-compiled-filename>)
)",
R"(
Compile the given Heist Scheme file.
  *) This really just pre-parses the AST into C++ expressions.
  *) The interpreter is still linked to evaluate the C++ AST.
  *) Enables producing a binary of scheme code for slightly faster eval.
  *) See "cps-compile" for a CPS-compilation alternative!

Reader changes with compilation:
  *) Compilation replaces interpreter's reader, hence:
     0. Reader-modifying operations must be processed in the script doing the compiling!
        => These include "infix!", "infixr!", "unfix!", "define-reader-syntax", & "define-reader-alias"!
)",





}, {
"cps-compile",
"Procedure",
R"(
(cps-compile <filename-string> <optional-compiled-filename>)
)",
R"(
Compile the given Heist Scheme file, & wrap its contents in a single 
large "scm->cps" block with "id" as the topmost continuation.
  *) This really just pre-parses the AST into C++ expressions.
  *) The interpreter is still linked to evaluate the C++ AST.
  *) Enables producing a binary of scheme code for slightly faster eval.
  *) See "compile" for a non-CPS-compilation alternative!

Reader changes with cps-compilation:
  *) Compilation replaces interpreter's reader, hence:
     0. Reader-modifying operations must be processed in the script doing the compiling!
        => These include "infix!", "infixr!", "unfix!", "define-reader-syntax", & "define-reader-alias"!
)",





}, {
"seconds-since-epoch",
"Procedure",
R"(
(seconds-since-epoch)
)",
R"(
Get seconds since the unix timestamp (1 January 1970).
)",





}, {
"time",
"Procedure",
R"(
(time <callable> <arg1> ... <argN>)
)",
R"(
Time <callable>'s execution after being applied to <arg1> ... <argN>.
  *) Returns a pair: (cons <time-in-seconds> <callable's-result>)
)",





}, {
"current-date",
"Procedure",
R"(
(current-date <optional-offset> ...)
)",
R"(
Get current date as a string.
  *) <optional-offset> = (<symbolic-unit> <integer-amount>)
  *) <symbolic-unit> = 'sec | 'min | 'hour | 'day | 'year
)",





}, {
"set-nansi!",
"Procedure",
R"(
(set-nansi! <boolean>)
)",
R"(
Disable ANSI escape codes. 
  *) Check status via "nansi?".
  *) Returns the last value that served this role!
)",





}, {
"nansi?",
"Procedure",
R"(
(nansi?)
)",
R"(
Confirm ANSI codes are NOT in use. 
  *) Set status via "set-nansi!".
)",





}, {
"ci?",
"Procedure",
R"(
(ci?)
)",
R"(
Confirm case sensitivity is INACTIVE. 
  *) Enable case-insensitivity via the "-ci" command-line flag!
  *) NOTE: Case-insensitivity implemented by having reader lowercase all input.
)",





}, {
"set-pprint-column-width!",
"Procedure",
R"(
(set-pprint-column-width! <positive-integer>)
)",
R"(
Set pretty-print column length. 
  *) Check current width via "pprint-column-width".
  *) Returns the last value that served this role!
)",





}, {
"pprint-column-width",
"Procedure",
R"(
(pprint-column-width)
)",
R"(
Check pretty-print column length.
  *) Set current width via "set-pprint-column-width!".
)",





}, {
"set-max-recursion-depth!",
"Procedure",
R"(
(set-max-recursion-depth! <positive-integer>)
)",
R"(
Set max recursion depth. 
  *) Check current max depth via "max-recursion-depth".
  *) Returns the last value that served this role!
)",





}, {
"max-recursion-depth",
"Procedure",
R"(
(max-recursion-depth)
)",
R"(
Check max recursion depth. 
  *) Set current max depth via "set-max-recursion-depth!".
)",





}, {
"set-repl-prompt!",
"Procedure",
R"(
(set-repl-prompt! <string>)
)",
R"(
Set the REPL prompt string. 
  *) Check current REPL prompt string via "repl-prompt".
  *) Returns the last value that served this role!
)",





}, {
"repl-prompt",
"Procedure",
R"(
(repl-prompt)
)",
R"(
Get the REPL prompt string. 
  *) Set current REPL prompt string via "set-repl-prompt!".
)",





}, {
"set-dynamic-call-trace!",
"Procedure",
R"(
(set-dynamic-call-trace! <boolean>)
)",
R"(
Set status as to whether should dynamically trace all procedure calls. 
  *) Check current status via "dynamic-call-trace?".
  *) Returns the last value that served this role!
)",





}, {
"dynamic-call-trace?",
"Procedure",
R"(
(dynamic-call-trace?)
)",
R"(
Check status as to whether dynamically tracing all procedure calls. 
  *) Set current status via "set-dynamic-call-trace!".
)",





}, {
"set-trace-args!",
"Procedure",
R"(
(set-trace-args! <boolean>)
)",
R"(
Set status as to whether should trace procedure arguments in the call stack. 
  *) Check current status via "trace-args?".
  *) Returns the last value that served this role!
)",





}, {
"trace-args?",
"Procedure",
R"(
(trace-args?)
)",
R"(
Check status as to whether dynamically tracing procedure arguments in the call stack.
  *) Set current status via "set-trace-args!".
)",





}, {
"set-dot!",
"Procedure",
R"(
(set-dot! <symbol>)
)",
R"(
Set the dot denoting dotted lists & variadic arguments.
  *) Defaults to '.
  *) Get current dot via '(dot)'!
  *) Returns the last value that served this role!
  *) Alias the current dot in syntax via "*dot*"!
)",





}, {
"dot",
"Procedure",
R"(
(dot)
)",
R"(
Get the current dot symbol being used to denote dotted lists & variadic args! 
  *) Set the current dot via '(set-dot! <new-dot-symbol>)'
  *) Alias the current dot in syntax via "*dot*"!
)",





}, {
"exit",
"Procedure",
R"(
(exit <optional-integer-exit-code>)
)",
R"(
Exit the current Heist session with <optional-integer-exit-code> as its status.
  *) <optional-integer-exit-code> defaults to *exit-success*
  *) If triggered while embedded in C++ (cpp_interop.hpp), eval'd code
     returns either *exit-success* or *exit-failure* as a SYMBOL!
  *) If triggered in *null-environment*, evaluation sandboxes exit & returns 
     the given code immediately!
)",





}, {
"error",
"Procedure",
R"(
(error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)
)",
R"(
Triggers an error.
  *) See "syntax-error" for a different error prefix to be printed!
)",





}, {
"syntax-error",
"Procedure",
R"(
(syntax-error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)
)",
R"(
Triggers a syntax error.
  *) See "error" for a different error prefix to be printed!
)",





}, {
"call/ce",
"Procedure",
R"(
(call/ce <callable> <arg1> ... <argN>) 
(call-with-current-environment <callable> <arg1> ... <argN>)
)",
R"(
Apply <callable> to <arg1> ... <argN> using dynamic scoping.
)",





}, {
"lexical-scope->dynamic-scope",
"Procedure",
R"(
(lexical-scope->dynamic-scope <callable>)
)",
R"(
Get a deep-copy of <callable> that, when invoked, uses dynamic scope!
)",





}, {
"dynamic-scope->lexical-scope",
"Procedure",
R"(
(dynamic-scope->lexical-scope <callable>)
)",
R"(
Get a deep-copy of <callable> that, when invoked, uses lexical scope!
  *) NOTE: Callables use lexical scope by default!
)",





}, {
"dynamic-scope?",
"Procedure",
R"(
(dynamic-scope? <callable>)
)",
R"(
Check whether <callable> uses dynamic scope!
)",





}, {
"lexical-scope?",
"Procedure",
R"(
(lexical-scope? <callable>)
)",
R"(
Check whether <callable> uses lexical scope!
  *) NOTE: Callables use lexical scope by default!
)",





}, {
"jump!",
"Procedure",
R"(
(jump! <optional-arg>)
)",
R"(
Throw <optional-arg>.
  *) <optional-arg> defaults to '(void)'
  *) Catch such via the "catch-jump" primitive!
)",





}, {
"catch-jump",
"Procedure",
R"(
(catch-jump <callable> <arg1> ... <argN>)
)",
R"(
Apply <callable> on <arg1> ... <argN>, catching any "jump!"ed values
  *) "jump!"ed values & regular returns from <callable> are indistinguishable.
  *) Jump values via the "jump!" procedure.
)",





}, {
"trace",
"Procedure",
R"(
(trace <procedure> <arg1> ... <argN>)
)",
R"(
Trace the application of <procedure> on <arg1> ... <argN>.
  *) Prints the arguments, return value, and recursive depth of each stack frame.
  *) Avoid I/O in traced procedures for clearer results!
)",





}, {
"gensym",
"Procedure",
R"(
(gensym <optional-instance-#-to-reference>)
)",
R"(
Generate a unique symbol.
  *) (gensym 1) refers to the symbol generated by the last (gensym) invocation
  *) (gensym 2) refers to the symbol generated by the 2nd to last (gensym) invocation
  *) etc.
)",





}, {
"sown-gensym",
"Procedure",
R"(
(sown-gensym <seed>)
=> <seed> ::= number | symbol | boolean
)",
R"(
Generate a seeded unique symbol.
)",





}, {
"call/cc",
"Procedure",
R"(
(call/cc <unary-continuation-callable>) 
(call-with-current-continuation <unary-continuation-callable>)
)",
R"(
Call <unary-continuation-callable> by passing it the current continuation as a procedure.
  *) Only valid in CPS contexts (query "cps" with "help" for details)!
)",





}, {
"expand",
"Procedure",
R"(
(expand <quoted-macro-exp>)
)",
R"(
Expand <quoted-macro-exp> based on the current macro bindings.
  *) Expands both analysis-time (core) & run-time macros!
  *) Does NOT expand any new macros defined in <quoted-macro-exp>!
  *) See "expand*" for an unary-begin optimizing alternative!
  *) See "core-expand" for an analysis-time-macro-only alternative!
)",





}, {
"expand*",
"Procedure",
R"(
(expand* <quoted-macro-exp>)
)",
R"(
Equivalent to "expand" but ALSO automatically unwraps unary begins!
  *) Expands both analysis-time (core) & run-time macros!
  *) Does NOT expand any new macros defined in <quoted-macro-exp>!
  *) See "core-expand*" for an analysis-time-macro-only alternative!
)",





}, {
"core-expand",
"Procedure",
R"(
(core-expand <quoted-macro-exp>)
)",
R"(
Expand <quoted-macro-exp> based on the current analysis-time (core) macro bindings.
  *) Does NOT expand any new macros defined in <quoted-macro-exp>!
  *) See "core-expand*" for an unary-begin optimizing alternative!
  *) See "expand" for an alternative expanding both analysis-time AND run-time macros!
)",





}, {
"core-expand*",
"Procedure",
R"(
(core-expand* <quoted-macro-exp>)
)",
R"(
Equivalent to "core-expand" but ALSO automatically unwraps unary begins!
  *) Does NOT expand any new macros defined in <quoted-macro-exp>!
  *) See "expand*" for an alternative expanding both analysis-time AND run-time macros!
)",





}, {
"core-syntax?",
"Procedure",
R"(
(core-syntax? <symbol>)
)",
R"(
Confirm <symbol> is core-syntax (ie defined by "core-syntax").
)",





}, {
"runtime-syntax",
"Procedure",
R"(
(runtime-syntax? <symbol>)
)",
R"(
Confirm <symbol> is runtime-syntax (ie defined by "define-syntax").
)",





}, {
"reader-alias?",
"Procedure",
R"(
(reader-alias? <string>)
)",
R"(
Confirm <string> is a reader alias (ie defined by "define-reader-alias").
  *) Must be a string to avoid conversion by the reader if IS an alias!
)",





}, {
"reader-syntax?",
"Procedure",
R"(
(reader-syntax? <string>)
)",
R"(
Confirm <string> is reader syntax (ie defined by "define-reader-syntax").
  *) Must be a string to avoid expansion by the reader if IS syntax!
)",





}, {
"define-reader-syntax",
"Procedure",
R"(
(define-reader-syntax <shorthand-string> <optional-longhand-string>)
)",
R"(
Have the reader expand <shorthand-string> around objects into <longhand-string>.
  *) Internally, "'" works as if interpreted (define-reader-syntax "'" "quote")!
  *) Leaving out <optional-longhand> rms <shorthand> reader macro & returns if found!
  *) Defining ":" as a shorthand is invalid (messes with internal reserved symbols)!
  *) Examples:

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
)",





}, {
"reader-syntax-list",
"Procedure",
R"(
(reader-syntax-list)
)",
R"(
Get an alist of the current reader syntax shorthands & longhands!
  *) Define such syntax via "define-reader-syntax"!
)",





}, {
"reader-alias-list",
"Procedure",
R"(
(reader-alias-list)
)",
R"(
Get an alist of the current reader aliases & names!
  *) Define such aliases via "define-reader-alias"!
)",





}, {
"delete-core-syntax!",
"Procedure",
R"(
(delete-core-syntax! <macro-name-symbol> ...)
)",
R"(
Delete core syntax labels (that were defined by "core-syntax")!
)",





}, {
"delete-runtime-syntax!",
"Procedure",
R"(
(delete-runtime-syntax! <macro-name-symbol> ...)
)",
R"(
Delete runtime syntax labels (that were defined by "define-syntax")!
)",





}, {
"infix-list",
"Procedure",
R"(
(infix-list)
)",
R"(
Get an alist of infix symbols, associativity, & precedence!
  *) Define infix operators via the "infix!" & "infixr!" special forms!
)",





}, {
"json->scm",
"Procedure",
R"(
(json->scm <string>)
)",
R"(
Convert JSON string to a Heist Scheme datum.
  *) arrays -> vectors
  *) null -> '()
  *) maps -> alists (of key-value lists)
)",





}, {
"scm->json",
"Procedure",
R"(
(scm->json <obj> <optional-indent-width>)
)",
R"(
Convert Scheme datum to a JSON string.
  *) <obj> ::= <string>
             | <number>
             | <'()>    ; -> <null>
             | <alist>  ; -> <map> (keys must be string | number | null | bool!)
             | <vector> ; -> <array>
             | <boolean>
)",





}, {
"object->json",
"Procedure",
R"(
(object->json <object> <optional-indent-width>)
)",
R"(
Recursively convert <object> members to a JSON string.
)",





}, {
"json-datum?",
"Procedure",
R"(
(json-datum? <obj>)
)",
R"(
Return whether <obj> is a valid JSON datum candidate.
  *) Effectively returns whether (scm->json <obj>) would trigger an error or not.
)",





}, {
"csv->list",
"Procedure",
R"(
(csv->list <string> <optional-delimiter-char>)
)",
R"(
Convert CSV string to a list of lists of numbers/strings.
  *) <optional-delimiter-char> defaults to #\,

Other CSV procedures include: 
  "csv->vector", "list->csv", "vector->csv", & "csv-datum?"
)",





}, {
"csv->vector",
"Procedure",
R"(
(csv->vector <string> <optional-delimiter-char>)
)",
R"(
Convert CSV string to a vector of vectors of numbers/strings.
  *) <optional-delimiter-char> defaults to #\,

Other CSV procedures include: 
  "csv->list", "list->csv", "vector->csv", & "csv-datum?"
)",





}, {
"list->csv",
"Procedure",
R"(
(list->csv <list-of-lists-of-csv-data> <optional-delimiter-char>)
)",
R"(
Convert list of lists of <csv-data> to a CSV string.
  *) <csv-data> ::= <string> | <number>
  *) <optional-delimiter-char> defaults to #\,

Other CSV procedures include: 
  "csv->list", "csv->vector", "vector->csv", & "csv-datum?"
)",





}, {
"vector->csv",
"Procedure",
R"(
(vector->csv <vector-of-vectors-of-csv-data> <optional-delimiter-char>)
)",
R"(
Convert vector of vectors of <csv-data> to a CSV string.
  *) <csv-data> ::= <string> | <number>
  *) <optional-delimiter-char> defaults to #\,

Other CSV procedures include: 
  "csv->list", "csv->vector", "list->csv", & "csv-datum?"
)",





}, {
"csv-datum?",
"Procedure",
R"(
(csv-datum? <obj>)
)",
R"(
Return whether <obj> is a valid CSV datum candidate.
  *) Effectively returns whether "list->csv" or "vector->csv" would trigger an error or not.

Other CSV procedures include: 
  "csv->vector", "csv->list", "list->csv", & "vector->csv"
)",





}, {
"set-falsey!",
"Procedure",
R"(
(set-falsey! <datum> ...)
)",
R"(
Register <datum> ... as falsey values.
  *) Note that "#t" can NEVER be set as falsey!
  *) By default, only "#f" is falsey in Heist Scheme.
  *) Falsey values are identified internally via "equal?".
  *) Falsey values are DEEP-COPIED via "copy" internally to the set of falsey values.
     Thus:

     (define l '(1 2 3))
     (set-falsey! l)
     (if l 1 0) ; 0
     (set-car! l 0)
     (if l 1 0) ; 1

Other falsiness/truthiness procedures include: 
  "set-truthy!" & "falsey-values"
)",





}, {
"set-truthy!",
"Procedure",
R"(
(set-truthy! <datum> ...)
)",
R"(
Register <datum> ... as truthy values.
  *) Note that "#f" can NEVER be set as truthy!
  *) Effectively removes <datum> ... from the set of falsey values.
  *) By default, everything EXCEPT "#f" is truthy in Heist Scheme.

Other falsiness/truthiness procedures include: 
  "set-falsey!" & "falsey-values"
)",





}, {
"falsey-values",
"Procedure",
R"(
(falsey-values)
)",
R"(
Get a list of the current values registered as "falsey" by Heist Scheme.
  *) Returned falsey values are NOT deep-copied to the generated list!

Other falsiness/truthiness procedures include: 
  "set-falsey!" & "set-truthy!"
)",


/******************************************************************************
* FLONUM SPECIAL CONSTANT DESCRIPTIONS @NEW-SECTION
******************************************************************************/


}, {
"fl-e",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant e.
)",





}, {
"fl-1/e",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 1/e.
)",





}, {
"fl-e-2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant e^2.
)",





}, {
"fl-pi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant pi.
)",





}, {
"fl-1/pi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 1/pi.
)",





}, {
"fl-2pi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 2pi.
)",





}, {
"fl-pi/2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant pi/2.
)",





}, {
"fl-pi/4",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant pi/4.
)",





}, {
"fl-pi-squared",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant pi^2.
)",





}, {
"fl-rad/deg",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant rad/deg (pi/180).
)",





}, {
"fl-deg/rad",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant deg/rad (180/pi).
)",





}, {
"fl-2/pi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 2/pi.
)",





}, {
"fl-2/sqrt-pi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 2/sqrt(pi).
)",





}, {
"fl-e-pi/4",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant e^(pi/4).
)",





}, {
"fl-log2-e",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant log2(e).
)",





}, {
"fl-log10-e",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant log10(e).
)",





}, {
"fl-log-2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant ln(2).
)",





}, {
"fl-1/log-2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 1/ln(2).
)",





}, {
"fl-log-3",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant ln(3).
)",





}, {
"fl-log-pi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant ln(pi).
)",





}, {
"fl-log-10",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant ln(10).
)",





}, {
"fl-1/log-10",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 1/ln(10).
)",





}, {
"fl-sqrt-2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant sqrt(2).
)",





}, {
"fl-sqrt-3",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant sqrt(3).
)",





}, {
"fl-sqrt-5",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant sqrt(5).
)",





}, {
"fl-sqrt-10",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant sqrt(10).
)",





}, {
"fl-1/sqrt-2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 1/sqrt(2).
)",





}, {
"fl-cbrt-2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant cbrt(2).
)",





}, {
"fl-cbrt-3",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant cbrt(3).
)",





}, {
"fl-4thrt-2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 4thrt(2).
)",





}, {
"fl-phi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant phi.
)",





}, {
"fl-log-phi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant ln(phi).
)",





}, {
"fl-1/log-phi",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant 1/ln(phi).
)",





}, {
"fl-euler",
"Variable (Number)",
R"()",
R"(
Bound to euler's constant.
)",





}, {
"fl-e-euler",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant e^(euler_constant).
)",





}, {
"fl-sin-1",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant sin(1).
)",





}, {
"fl-cos-1",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant cos(1).
)",





}, {
"fl-gamma-1/2",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant Gamma(1/2).
)",





}, {
"fl-gamma-1/3",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant Gamma(1/3).
)",





}, {
"fl-gamma-2/3",
"Variable (Number)",
R"()",
R"(
Bound to the mathematical constant Gamma(2/3).
)",


/******************************************************************************
* SPECIAL NUMERIC CONSTANT DESCRIPTIONS @NEW-SECTION
******************************************************************************/


}, {
"+inf.0",
"Object (Value Semantics), Numeric Constant",
R"()",
R"(
Numeric object representing positive infinity.
)",





}, {
"-inf.0",
"Object (Value Semantics), Numeric Constant",
R"()",
R"(
Numeric object representing negative infinity.
)",





}, {
"+nan.0",
"Object (Value Semantics), Numeric Constant",
R"()",
R"(
Numeric object designated to represent Not-A-Number.
)",


/******************************************************************************
* SEQUENCE TOPIC DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"sequence",
"Object (Reference Semantics)",
R"()",
R"(
Term for family of objects: <list> | <vector> | <string>
  *) Object family for which all generic algorithmic primitives operate on:
     
     (seq? <obj>)
     (empty? <obj>)
     (empty <sequence>) 
     (length <sequence>)
     (length+ <sequence>)
     (reverse <sequence>)
     (reverse! <sequence>)
     (fold <callable> <seed> <sequence1> <sequence2> ...)
     (fold-right <callable> <seed> <sequence1> <sequence2> ...)
     (map <callable> <sequence1> <sequence2> ...)
     (map! <callable> <sequence1> <sequence2> ...)
     (filter <predicate?> <sequence>)
     (for-each <callable> <sequence1> <sequence2> ...)
     (seq-copy! <dest-sequence> <source-sequence>)
     (count <predicate?> <sequence>)
     (ref <sequence> <index>)
     (slice <sequence> <start-index> <optional-length>)
     (set-index! <sequence> <index> <obj>)
     (swap-indices! <sequence> <index> <index>)
     (fill! <sequence> <fill-value>)
     (append <sequence1> ... <sequenceN> <obj>)
     (remove <predicate?> <sequence>)
     (remove-first <predicate?> <sequence>)
     (remove-last <predicate?> <sequence>)
     (delete <sequence> <index>)
     (last <sequence> <index>)
     (tail <sequence> <index>)
     (head <sequence> <index>)
     (init <sequence> <index>)
     (seq= <predicate?> <sequence1> <sequence2> ...)
     (skip <predicate?> <sequence>)
     (skip-right <predicate?> <sequence>)
     (index <predicate?> <sequence>)
     (index-right <predicate?> <sequence>)
     (drop <sequence> <length>)
     (drop-right <sequence> <length>)
     (drop-while <predicate?> <sequence>)
     (drop-right-while <predicate?> <sequence>)
     (take <sequence> <length>)
     (take-right <sequence> <length>)
     (take-while <predicate?> <sequence>)
     (take-right-while <predicate?> <sequence>)
     (any <predicate?> <sequence1> <sequence2> ...)
     (every <predicate?> <sequence1> <sequence2> ...)
     (conj <obj> <sequence>)
     (union <predicate?> <sequence1> <sequence2> ...)
     (intersection <predicate?> <sequence1> <sequence2> ...)
     (difference <predicate?> <sequence1> <sequence2> ...)
     (symmetric-difference <predicate?> <sequence1> <sequence2> ...)
     (sort <predicate?> <sequence>)
     (sort! <predicate?> <sequence>)
     (sorted? <predicate?> <sequence>)
     (merge <predicate?> <sequence1> <sequence2>)
     (delete-neighbor-dups <equality-predicate?> <sequence>)
     (delete-neighbor-dups! <equality-predicate?> <sequence>)
)",


/******************************************************************************
* COROUTINE TOPIC DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"coroutine",
"Class Prototype",
R"()",
R"(
Class-prototype for objects generated by "define-coroutine". 
Has 1 member & 1 method:
  
  <coro-obj>.value  ; current yielded value (#f by default)
  (<coro-obj>.next) ; reinvoke the next iteration of the coroutine:
                    ;   returns a new coroutine object if "yield" is
                    ;   invoked again, OR the final return value of the coro

Used in conjunction with the following procedures:
  
  (coroutine? <obj>)
  (coroutine->generator <coroutine-object>)
  (cycle-coroutines! <coroutine-object-1> ...)

Examples:

  ;; Having 2 coroutines alternate until one completes!

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
)",


/******************************************************************************
* UNIVERSE TOPIC DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"universe",
"Class Prototype",
R"()",
R"(
Class-prototype for objects generated by "new-universe". 

2 associated procedures:
  0. "new-universe": 
     Returns a "universe" object, initialized with Heist's default bindings,
     which enables evaluation in a persistent sandboxed environment.
  1. "universe?":
     Universe object predicate.

5 methods:
  0. (<uni-obj>.eval <datum>)  ; Evals "<datum>" in <uni-obj>'s sandboxed environment!
  1. (<uni-obj>.push! <datum>) ; Store Data in <uni-obj>'s buffer!
  2. (<uni-obj>.pop!)          ; Remove Data from <uni-obj>'s buffer!
  3. (<uni-obj>.clear!)        ; Clear <uni-obj>'s buffer!
  4. (<uni-obj>.run!)          ; Execute (then clear!) <uni-obj>'s buffer!

Example:
  (define world (new-universe))
  (world.eval '(set-dot! ':))
  (write (dot))               ; #\. (Still has default dot!)
  (world.eval '(write (dot))) ; #\: (Sandboxed env maintains unique dot!)
)",


/******************************************************************************
* MODULE TOPIC DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"module",
"Class Prototype",
R"()",
R"(
Class-prototype for objects generated by "define-module". 

Modules can be either anonymous or named:
  *) Anonymous modules directly expose procedures to the enclosing environment.
  *) Named modules package exposed procedures as methods in a module object,
     which is itself in turn exposed to the enclosing environment.
)",


/******************************************************************************
* SYNTAX-TRANSFORMER TOPIC DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"syntax-transformer",
"Object",
R"()",
R"(
Valid values for "define-syntax" & "core-syntax". These come in two flavors:
  0. Syntax-rules objects (created by "syntax-rules"). Designed for high-level macros,
     syntax-rules objects match the macro expression against a pattern, then expands it
     into a template. Can leverage "syntax-hash".

  1. Unary callables. Designed for low-level macros, unary callable transformers are 
     passed the macro expression as a quoted datum value, and must return an evaluable datum.
     *) NOTE: transformers defined in cps contexts (see "scm->cps" and "cps") have "id" bound
              as their topmost continuation, hence "call/cc" shouldn't be used in transformers
)",


/******************************************************************************
* HEIST-CPP-INTEROP DOCUMENTATION DESCRIPTION @NEW-SECTION
******************************************************************************/


}, {
"cpp_interop.hpp",
"Document",
R"()",
R"(
Header file enabling C++17 interop with Heist Scheme!
  *) Interop is designed for SINGLE-THREADED environments!
  *) Defines 4 C++17 Functions for Interop w/ Heist:
     0. eval   // evaluate heist code string, same as _heist literal (check out the header!)
     1. apply  // apply args to Heist callable (procedure or functor)
     2. define // define C++17 Heist primitive _OR_ a global Heist variable
  *) Check out https://github.com/jrandleman/Caper-Scheme for a more lightweight
     version of Heist (minimized Scheme implementation) optimized for C++17 embedding!
)",


/******************************************************************************
* SUPPLEMENTAL DOCUMENTATION DESCRIPTIONS @NEW-SECTION
******************************************************************************/


}, {
"readme.md",
"Document",
R"()",
R"(
Static equivalent of this "help" procedure, with some additional semantic notes.
Link: https://github.com/jrandleman/Heist-Scheme/blob/master/README.md
)",





}, {
"install.md",
"Document",
R"()",
R"(
Instructions as to how you might install Heist Scheme on your machine.
Given you're reading this right now, you're presumably familiar with it lol

Contents:
  =================================
    ________  ______________
   /__  __/ \/ / __\ __/\ \ \
   __/ /_/ // /_ \/ /  - \ \_\___
  /_____/_/\_/\__/_/__/^\_\___\__\
  ===============================================
  INSTALLING THE INTERPRETER IN A NEW DIRECTORY:
  ===============================================

  0. With "python3", AND "clang++" OR "g++":
     ----------------------------------------
     A) OPEN THE "/installers/" FOLDER
     B) RUN "$ python3 installer.py"
        *) This generates a file named "HEIST_FILEPATH.hpp" in "/interpreter_headers/"
           => Any instance of "<HEIST_DIRECTORY_FILE_PATH>" below refers to the string in this file
        *) NOTE: Use "$ python3 installer.py -debug" to compile with "-Wall -Wextra"!


  1. Otherwise:
     -----------
     A) OPEN THE "/installers/" FOLDER
     B) COMPILE AND RUN "installer.cpp" USING THE "-std=c++17" FLAG
        *) This generates a file named "HEIST_FILEPATH.hpp" in "/interpreter_headers/"
           => Any instance of "<HEIST_DIRECTORY_FILE_PATH>" below refers to the string in this file
     C) COMPILE THE INTERPRETER: "$ clang++ -std=c++17 -O3 -o heist heist.cpp"
        *) FLAG DESCRIPTIONS:
           => "-std=c++17": [REQUIRED] compile using the C++17 standard
           => "-O3": [RECOMMENDED FOR FASTEST EXECUTION] maximum optimization
              -> longest compile time, but fastest runtime
           => "-Os": [RECOMMENDED FOR MOST BUILDS] optimizes for binary's size
              -> faster compile-time than "-O3", smaller binary, & close runtime
        *) ON COMPILE TIME:
           => Full "-O3" compilation takes about 70s. Be patient.
              -> Compilation time has been traded for FAST runtime.
           => "-Os" compilation takes about 40s. Generated binary is smaller than
              "-O3"'s (as expected) & its runtime is nearly as fast


  ============================================================
  SETTING UP BETTER COMMAND-LINE INTERFACE -- FOR BASH & ZSH:
  ============================================================

  0. OPEN THE FILE:
     * FOR BASH: "~/.bashrc"
     * FOR ZSH: "~/.zshrc"
  1. WRITE: "alias heist='<HEIST_DIRECTORY_FILE_PATH>/heist'"
  2. SAVE THE FILE, CLOSE THE CURRENT TERMINAL WINDOW, & RELAUNCH THE TERMINAL
  3. NOW, WRITING "heist" FROM ANYWHERE IN THE TERMINAL LAUNCHES THE INTERPRETER!


  ===============================================
  SETTING UP A BUILD SYSTEM -- FOR SUBLIME TEXT:
  ===============================================

  0. GOTO "Tools > Build System > New Build System"
  1. WRITE:
      {
        "cmd": ["<HEIST_DIRECTORY_FILE_PATH>/heist", "-nansi", "-script", "$file"],
        "file_regex": "^(..[^:]*):([0-9]+):?([0-9]+)?:? (.*)$",
      }
  2. SAVE THE FILE AS "Heist-Scheme.sublime-build"
  3. NOW THE HEIST SCHEME INTERPRETER OUGHT TO APPEAR IN "Tools > Build System"
)",

}
};

#endif