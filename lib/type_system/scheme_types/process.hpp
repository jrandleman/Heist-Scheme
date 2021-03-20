// Author: Jordan Randleman -- jrandleman@scu.edu -- process.hpp
// => Contains "process_invariants_t" data structure, "G" current-process global object,
//    and "symbol_is_dot_operator"/"data_is_dot_operator" helper functions for the C++ 
//    Heist Scheme Interpreter

#ifndef HEIST_PROCESS_HPP_
#define HEIST_PROCESS_HPP_

namespace heist {

  /******************************************************************************
  * INFIX OPERATOR TABLE REGISTRY TYPE ALIASES
  ******************************************************************************/

  using infix_op_t    = std::pair<bool,std::string>; // {left-assoc?, symbol}
  using infix_level_t = std::vector<infix_op_t>;

  /******************************************************************************
  * GLOBAL PROCESS-DEPENDANT MUTABLE GLOBAL INVARIANTS
  ******************************************************************************/

  struct process_invariants_t {

    /* MAX NUMBER OF RECURSIVE CALLS */
    size_type MAX_RECURSION_DEPTH = 1000; // see set-max-recursion-depth! primitive

    /* PRETTY-PRINTER'S MAX COLUMN WIDTH */
    size_type PPRINT_MAX_COLUMN_WIDTH = 80; // see set-pprint-column-width! prim

    /* WHETHER TO USE ANSI ESCAPE SEQUENCES TO FORMAT OUTPUT */
    bool USING_ANSI_ESCAPE_SEQUENCES = true; // see set-nansi! primitive

    /* WHETHER "-cps" COMMAND LINE FLAG WAS PASSED */
    bool USING_CPS_CMD_LINE_FLAG = false;

    /* WHETHER TRACING ALL FUNCTION CALLS (DEBUGGING HELPER) */
    bool TRACING_ALL_FUNCTION_CALLS = false; // see set-dynamic-call-trace!

    /* NAME OF CURRENT TRACED FUNCTION (EMPTY = NO TRACE) */
    string TRACED_FUNCTION_NAME = ""; // see trace primitive

    /* REPL PROMPT VARIABLES */
    string REPL_PROMPT = "> "; // see set-repl-prompt! primitive
    string REPL_TAB    = "  ";

    /* REPL FORMATTING TRACKER VARIABLES */
    bool LAST_PRINTED_NEWLINE_TO_STDOUT = false;
    bool LAST_PRINTED_TO_STDOUT         = false;

    /* CURRENT DEFAULT INPUT & OUTPUT PORTS */
    FILE* CURRENT_INPUT_PORT  = stdin;
    FILE* CURRENT_OUTPUT_PORT = stdout;

    /* GENSYM UNIQUE HASHING KEYS */
    size_type GENSYM_HASH_IDX_1 = 0, GENSYM_HASH_IDX_2 = 0;

    /* THE GLOBAL MACRO LABEL REGISTRY & MACRO/CPS HASH INDICES */
    str_vector MACRO_LABEL_REGISTRY; // optimizes procedure analysis
    size_type MACRO_HASH_IDX_1 = 0, MACRO_HASH_IDX_2 = 0;
    size_type CPS_HASH_IDX_1 = 0, CPS_HASH_IDX_2 = 0;
    size_type CPS_VALUE_HASH_IDX_1 = 0, CPS_VALUE_HASH_IDX_2 = 0;

    /* THE GLOBAL REGISTRY OF ANALYSIS-TIME GLOBAL MACRO LABELS */
    str_vector ANALYSIS_TIME_MACRO_LABEL_REGISTRY;

    /* THE GLOBAL REGISTRY OF READER MACROS */
    str_vector SHORTHAND_READER_MACRO_REGISTRY = str_vector({"`@","\\","'"});
    str_vector LONGHAND_READER_MACRO_REGISTRY = str_vector({
      "syntax-hash",symconst::reader_lambda,"quote"
    });

    /* THE GLOBAL REGISTRY OF READER ALIASES */
    str_vector SHORTHAND_READER_ALIAS_REGISTRY, LONGHAND_READER_ALIAS_REGISTRY;

    /* STACK TRACE MODIFIERS */
    bool TRACE_ARGS = false;
    size_type TRACE_LIMIT = 16;

    /* INFIX SYMBOL READER TABLE */
    std::map<long long,infix_level_t> INFIX_TABLE; // {precedence, {left-assoc?, symbol}}

    /* GLOBAL ENVIRONMENT POINTER */
    env_type GLOBAL_ENVIRONMENT_POINTER = nullptr;

    /* DOT CHARACTER FOR VARIADIC & PAIR-LITERAL DENOTATION */
    string dot = "."; // see the "set-dot!" primitive

    /* FALSINESS VECTOR */
    data_vector FALSEY_VALUES = data_vector(1,bol_type(false));

  }; // End of struct process_invariants_t

  /******************************************************************************
  * CURRENT GLOBAL PROCESS INVARIANT SET
  ******************************************************************************/

  process_invariants_t G;

  /******************************************************************************
  * DOT ANALYSIS HELPER FUNCTIONS
  ******************************************************************************/

  bool symbol_is_dot_operator(const string& sym)noexcept{
    return sym == symconst::dot || sym == G.dot;
  }

  bool data_is_dot_operator(const data& d)noexcept{
    return d.is_type(types::sym) && symbol_is_dot_operator(d.sym);
  }
}

#endif