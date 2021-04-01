// Author: Jordan Randleman -- jrandleman@scu.edu -- symbolic_constants.hpp
// => Contains symbolic constants for the C++ Heist Scheme Interpreter
// => NOTE: SYMBOL NAMES BEGINNING WITH "heist:" ARE RESERVED!

#ifndef HEIST_SCHEME_CORE_SYMBOLIC_CONSTANTS_HPP_
#define HEIST_SCHEME_CORE_SYMBOLIC_CONSTANTS_HPP_

namespace heist::symconst {
  constexpr const char * const dflt_compile_name = "a.cpp";
  constexpr const char * const emptylist         = "";
  constexpr const char * const dot               = "*dot*";
  constexpr const char * const tail_call         = "heist:core:tail-call";
  constexpr const char * const continuation      = "heist:core:cps-";              // hashed continuation arg name prefix
  constexpr const char * const pass_continuation = "heist:core:pass-continuation"; // denotes to treat proc as if defn'd in a scm->cps block
  constexpr const char * const cps_generated_val = "heist:core:value-of-cps-";
  constexpr const char * const cps_app_tag       = "heist:core:app-cps";
  constexpr const char * const cps_ignore_arg    = "heist:core:ignore";
  constexpr const char * const scm_cps           = "scm->cps";
  constexpr const char * const cps_quote         = "cps-quote";
  constexpr const char * const using_cpsp        = "using-cps?";
  constexpr const char * const null_env          = "*null-environment*";
  constexpr const char * const local_env         = "*local-environment*";
  constexpr const char * const global_env        = "*global-environment*";
  constexpr const char * const exit_success      = "*exit-success*";
  constexpr const char * const exit_failure      = "*exit-failure*";
  constexpr const char * const core_syn          = "core-syntax";
  constexpr const char * const defn_syn          = "define-syntax";
  constexpr const char * const syn_rules         = "syntax-rules";
  constexpr const char * const syn_hash          = "syntax-hash";
  constexpr const char * const defn_reader_alias = "define-reader-alias";
  constexpr const char * const inf_precedence    = "heist:core:inf-precedence";
  constexpr const char * const infix             = "infix!";
  constexpr const char * const infixr            = "infixr!";
  constexpr const char * const unfix             = "unfix!";
  constexpr const char * const gensym_prefix     = "heist:core:gensym-";
  constexpr const char * const defclass          = "defclass";
  constexpr const char * const lambda            = "lambda";
  constexpr const char * const reader_lambda     = "heist:core:reader-lambda";
  constexpr const char * const fn                = "fn";
  constexpr const char * const vec_literal       = "vector-literal";
  constexpr const char * const map_literal       = "hmap-literal";
  constexpr const char * const delay             = "delay";
  constexpr const char * const quote             = "quote";
  constexpr const char * const set               = "set!";
  constexpr const char * const define            = "define";
  constexpr const char * const definedp          = "defined?";
  constexpr const char * const delete_bang       = "delete!";
  constexpr const char * const begin             = "begin";
  constexpr const char * const while_t           = "heist:core:while";
  constexpr const char * const if_t              = "if";
  constexpr const char * const ellipsis          = "...";
  constexpr const char * const ellipsis_hash     = "heist:core:ellipsis-hash:";
  constexpr const char * const true_t            = "#t";
  constexpr const char * const false_t           = "#f";
  constexpr const char * const append            = "append";
  constexpr const char * const list              = "list";
  constexpr const char * const vector            = "vector";
  constexpr const char * const hmap              = "hmap";
} // End namespace heist::symconst

#endif
