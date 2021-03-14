// Author: Jordan Randleman -- jrandleman@scu.edu -- heist.cpp
// => Main execution and AST evaluation for the C++ Heist Scheme Interpreter
// => GitHub: https://github.com/jrandleman/Heist-Scheme

#ifndef HEIST_CPP_
#define HEIST_CPP_

/***
 * COMPILE: 
 *   $ clang++ -std=c++17 -O3 -o heist heist.cpp
 *
 * FLAG DESCRIPTIONS:
 *   0. "-std=c++17": [REQUIRED] compile using the C++17 standard
 *   1. "-O3": [RECOMMENDED FOR FASTEST EXECUTION] maximum optimization
 *             -> longest compile time, but fastest runtime
 *   2. "-Os": [RECOMMENDED FOR MOST BUILDS] optimizes for binary's size
 *             -> faster compile-time than -O3, smaller binary, & close runtime
 *
 * ON COMPILE TIME:
 *   0. Full -O3 compilation takes about 70s. Be patient. Compilation
 *      time has been traded for FAST runtime.
 *   1. -Os compilation takes about 40s. Generated binary is smaller than
 *      -O3's (as expected) & its runtime is nearly as fast
 */

/******************************************************************************
* ABSOLUTE FILE PATH TO HEIST INTERPRETERS DIRECTORY
******************************************************************************/

#if __has_include("interpreter_headers/HEIST_FILEPATH.hpp")
  #include "interpreter_headers/HEIST_FILEPATH.hpp"
#else
  #error "installer.cpp" MUST BE COMPILED (USING "-std=c++17") AND RUN PRIOR THE INTERPRETER!
#endif

/******************************************************************************
* THE CIRCULAR EVALUATOR
******************************************************************************/

// +-----------------------------------------------------------------------+
// | It is no exaggeration to regard this as the most fundamental idea in  |
// | programming:                                                          |
// |    The evaluator, which determines the meaning of expressions in a    |
// |    programming language, is just another program. - SICP vol2, p.360  |
// +-----------------------------------------------------------------------+

/***
 * NUMBER SYSTEM:
 *   - EXACT FRACTIONS/INTERGERS (UNBOUND)
 *   - INEXACT FLOATS  (LONG DOUBLE)
 *   - COMPLEX NUMBERS (PAIRS OF EITHER ABOVE EXACTNESS)
 *
 * CHARS: USE ASCII ENCODING
 *
 * R4RS EXTENSIONS:
 *   - OBJECT SYSTEM           ; defclass
 *   - COROUTINES              ; define-coroutine
 *   - OPT-IN DYNAMIC SCOPING  ; call/ce & inline FUNCTION APPLICATIONS
 *   - OPT-IN CONTINUATIONS    ; "scm->cps" SPECIAL FORM W/ call/cc
 *   - FIRST-CLASS HASH-MAPS   ; "hmap" PRIMITIVE & "$(" LITERAL PREFIX
 *   - NATIVE EVEN STREAMS     ; LISTS WITH DELAYED CAR & CDR
 *   - GENERIC ALGORITHMS      ; POLYMORPHIC ALGORITHM PRIMITIVES
 *   - SRFI PRIMITIVES         ; LIST, VECTOR, STRING, ETC.
 *   - EVAL                    ; EVALUATE SYMBOLIC DATA AS CODE
 *   - UNHYGIENIC MACROS       ; MATCH SYMBOL-LIST PATTERNS TO TEMPLATES
 *     > SYNTAX-RULES
 *     > DEFINE-SYNTAX
 *     > LET-SYNTAX
 *     > LETREC-SYNTAX
 *     > CORE-SYNTAX
 *   - READER MACROS           ; EXPAND NON-S-EXPRESSIONS (like "'"->"quote")
 *   - STRING I/O              ; READ/WRITE COMPATIBILITY W/ STRINGS AS PORTS
 *   - RECURSIVE DEPTH CONTROL ; SET THE INTERPRETER'S MAX RECURSION DEPTH
 *   - VECTOR-LITERAL          ; LONG-HAND VARIANT OF THE #( PREFIX
 *   - HMAP-LITERAL            ; LONG-HAND VARIANT OF THE $( PREFIX
 *   - AND MORE!
 */

/***
 * RESERVED WORDS -VS- SPECIAL FORMS -VS- PRIMITIVES:
 *   (A) RESERVED WORDS:
 *       - DEFINITION: IMPLEMENTATION-SPECIFIC RESERVED INTERNAL OBJECT NAMES
 *       - PROPERTIES: 1) ***RESERVED SYMBOLS BEGIN WITH "heist:"***
 *                     2) ***USE BY USER IS UNDEFINED BEHAVIOR***
 *
 *   (B) SPECIAL FORMS:
 *       - DEFINITION: EXPLICIT KEYWORDS PARSED FOR BY THE INTERPRETER
 *       - PROPERTIES: 1) ***REDEFINITION BY USER IS UNDEFINED BEHAVIOR***
 *                     2) SPECIAL FORM ARGS ARE NOT EVALUATED PRIOR APPLICATION
 *                     3) USERS CAN DEFINE THEIR OWN SPECIAL FORMS VIA MACROS
 *       - EXAMPLES:
 *         * quote               ; SYMBOLIZE ARGS (CONVERT CODE TO DATA)
 *         * quasiquote          ; SELECTIVELY eval AND SYMBOLIZE CODE
 *         * define-syntax       ; RUN-TIME MACRO DEFINITION
 *         * core-syntax         ; ANALYSIS-TIME GLOBAL MACRO DEFINITION
 *         * syntax-rules        ; SYNTAX OBJECT
 *         * lambda              ; ANONYMOUS PROCEDURE
 *         * fn                  ; ANONYMOUS MULTI-ARITY PATTERN-MATCHING PROCEDURE
 *         * define              ; BIND VARIABLE TO VALUE
 *         * set!                ; ASSIGN VARIABLE A NEW VALUE (MUTATATION)
 *         * begin               ; SEQUENTIALLY eval ARGS
 *         * delay               ; DELAY ARG eval (RETURNS PROMISE)
 *         * if                  ; ARG1 ? ARG2 : ARG3
 *         * and                 ; ALL ARGS ARE TRUE
 *         * or                  ; NOT ALL ARGS ARE FALSE
 *         * defclass            ; CLASS PROTOTYPE DEFINITION
 *         * defined?            ; DETERMINE IF A VARIABLE/OBJECT-PROPERTY-ACCESS EXISTS
 *         * delete!             ; DELETE A VARIABLE/OBJECT-PROPERTY-ACCESS IF EXISTS
 *         * define-reader-alias ; DEFINE A SYMBOLIC ALIAS REPLACED VIA READER
 *         * infix!              ; DEFINE LEFT-ASSOCIATIVE INFIX OPERATORS
 *         * infixr!             ; DEFINE RIGHT-ASSOCIATIVE INFIX OPERATORS
 *         * unfix!              ; DEREGISTER EXISTING INFIX OPERATORS
 *         * cps-quote           ; RETURNS DATA AS CPS-EXPANDED QUOTED LIST
 *         * using-cps?          ; RETURNS WHETHER IN A scm->cps BLOCK OR THE -cps FLAG IS ACTIVE
 *         * scm->cps            ; SCOPED CPS TRANSFORMATION
 *
 *    (C) PRIMITIVES:
 *       - DEFINITION: C++ FUNCTIONS DEFINED IN THE HEIST GLOBAL ENVIRONMENT
 *       - PROPERTIES: 1) ***MAY BE REDEFINED BY USER AT RUN TIME***
 *                     2) MAY BE TREATED AS IF ANY OTHER HEIST PROCEDURE
 *       - EXAMPLES: 
 *         * (exit)               ; TERMINATE THE INTERPRETER
 *         * (help)               ; INTERACTIVE ALTERNATIVE TO README.MD
 *         * #t                   ; TRUE BOOLEAN VALUE
 *         * #f                   ; FALSE BOOLEAN VALUE
 *         * stream-null          ; EMPTY STREAM OBJECT
 *         * *null-environment*   ; eval IN DISJOINT GLOBAL ENVIRONMENT (FOR eval/load)
 *         * *local-environment*  ; eval IN LOCAL SCOPE (FOR eval/load)
 *         * *global-environment* ; eval IN GLOBAL SCOPE (FOR eval/load)
 *         * SEE "primitives.hpp" FOR THE ALL PRIMITIVE IMPLEMENTATIONS
 */

#include "interpreter_headers/types.hpp"
#include "interpreter_headers/primitives.hpp"
#include "interpreter_headers/input_parser.hpp"

namespace heist {

  data scm_eval(data&& datum, env_type& env);
  exe_fcn_t scm_analyze(data&& datum,const bool tail_call,const bool cps_block);
  scm_string generate_unique_cps_hash()noexcept;

  /******************************************************************************
  * AST-ANALYSIS HELPER FUNCTIONS
  ******************************************************************************/

  // Confirm whether list begins w/ designated symbol
  bool is_tagged_list(const scm_list& exp, const char* const tag)noexcept{
    return !exp.empty() && exp[0].is_type(types::sym) && exp[0].sym == tag;
  }


  // Generate a call signature from a procedure name & its given values
  sym_type procedure_call_signature(const sym_type& name,const frame_vals& vals)noexcept{
    if(vals.empty()) return '(' + name + ')';
    return '(' + name + ' ' + cio_expr_str<&data::noexcept_write>(vals).substr(1);
  }


  // Generate an improper procedure call error message
  sym_type improper_call_alert(sym_type name, const frame_vals& vals,
                                              const frame_vars& vars)noexcept{
    if(name.empty()) name = "#<procedure>"; // anonymous lambda
    // Generate the call signature
    auto call_signature = procedure_call_signature(name,vals);
    // Generate the definition signature
    sym_type defn_signature('(' + name);
    for(const auto& var : vars) defn_signature += ' ' + var;
    // Return the comparison between the called & defined procedure signatures
    return '\n' + scm_string(afmt(AFMT_35)) + "  >> Invalid Syntax:" + 
                  scm_string(afmt(AFMT_01)) + ' ' + call_signature + 
           '\n' + scm_string(afmt(AFMT_35)) + "  >> Defined Syntax:" + 
                  scm_string(afmt(AFMT_01)) + ' ' + defn_signature + ')';
  }

  bool symbol_is_property_chain_access(const sym_type& sym)noexcept{
    return sym.find('.') != scm_string::npos && sym != "." && sym != "..";
  }

  /******************************************************************************
  * AST DATA VALIDATION HELPER FUNCTIONS
  ******************************************************************************/

  // Throw an error if 'vars' contains duplicate or non-symbol arg names, 
  //   or improper (.) use
  void confirm_valid_procedure_parameters(const scm_list& vars,const scm_list& exp){
    const size_type n = vars.size();
    // variadic (.) arg must have a label afterwards
    if(n != 0 && symbol_is_dot_operator(vars[n-1].sym))
      THROW_ERR("Expected one item after variadic dot ("<<G.dot<<")! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
    // Search the vars list of the fcn's args for improper (.) use & duplicate arg names
    for(size_type i = 0; i < n; ++i) {
      if(!vars[i].is_type(types::sym)) // args must be symbols
        THROW_ERR("Non-Symbolic parameter [ "<<vars[i]<<" ] is an invalid arg name! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
      if(i+2 != n && symbol_is_dot_operator(vars[i].sym)) { // variadic (.) must come just prior the last arg
        if(i+3 == n && string_begins_with(vars[i+2].sym, symconst::continuation))
          continue; // allow continuations after variadic
        THROW_ERR("More than one item found after variadic dot ("<<G.dot<<")! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
      }
      for(size_type j = i+1; j < n; ++j)
        if(vars[i].sym == vars[j].sym) // duplicate arg name detected
          THROW_ERR("Duplicate arg name \""<<vars[i]<<"\" supplied! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
    }
  }


  // Confirm valid argument layout for variable assignment
  void confirm_valid_assignment(const scm_list& exp) {
    if(exp.size() != 3)
      THROW_ERR("'set! didn't receive 2 arguments: (set! <var> <val>)" << EXP_ERR(exp));
    if(!exp[1].is_type(types::sym) || exp[1].sym.empty())
      THROW_ERR("'set! 1st arg " << PROFILE(exp[1]) << " can't be reassigned"
        " (only symbols)!\n     (set! <var> <val>)" << EXP_ERR(exp));
    if(*exp[1].sym.rbegin() == '.')
      THROW_ERR("'set! 1st arg is invalid object property-chain-access [ " << exp[1] 
          << " ] ends with a '.':\n     (set! <var> <val>)" << EXP_ERR(exp));
  }


  // Confirm valid argument layout for variable & procedure definitions
  void confirm_valid_definition(const scm_list& exp) {
    if(exp.size() < 3)
      THROW_ERR("'define didn't receive enough arguments!\n     (define <var> <val>)"
        "\n     (define (<procedure-name> <args>) <body>)" << EXP_ERR(exp));
    if(!exp[1].is_type(types::sym) && !exp[1].is_type(types::exp))
      THROW_ERR("'define 1st arg [ " << exp[1] << " ] of type \"" 
        << exp[1].type_name() << "\" can't be defined (only symbols):"
        "\n     (define <var> <val>)"
        "\n     (define (<procedure-name> <args>) <body>)" << EXP_ERR(exp));
    if(exp[1].is_type(types::exp) && 
        (exp[1].exp.empty() || !exp[1].exp[0].is_type(types::sym)))
      THROW_ERR("'define procedure name [ " 
        << (exp[1].exp.empty() ? data("undefined") : exp[1].exp[0]) << " ] of type \"" 
        << (exp[1].exp.empty() ? "undefined" : exp[1].exp[0].type_name())
        << "\" is invalid (must be a symbol)!"
        "\n     (define <var> <val>)"
        "\n     (define (<procedure-name> <args>) <body>)" << EXP_ERR(exp));
    if(exp[1].is_type(types::sym) && exp.size() > 3)
      THROW_ERR("'define can only define 1 value to a variable (received " << exp.size()-2 << " vals)!"
        "\n     (define <var> <val>)"
        "\n     (define (<procedure-name> <args>) <body>)" << EXP_ERR(exp));
  }


  // Confirm valid argument layout for a lambda
  void confirm_valid_lambda(const scm_list& exp) {
    if(exp.size() < 3)
      THROW_ERR("'lambda special form didn't receive enough args: (lambda (<args>) <body>)"
        << EXP_ERR(exp));
    if(!exp[1].is_type(types::exp) && !data_is_the_empty_list(exp[1]))
      THROW_ERR("'lambda 1st arg [ " << exp[1] << " ] of type \"" 
        << exp[1].type_name() << "\" wasn't a proper parameter list!"
           "\n     (lambda (<args>) <body>)"
        << EXP_ERR(exp));
  }

  /******************************************************************************
  * ENVIRONMENTAL EXTENSION -- FRAME VARS/VALS EVALUATION HELPER FUNCTIONS
  ******************************************************************************/

  // Transforms the appropriate 'vals' into a list (for the given variadic arg)
  //   => ((lambda (. l) l) <arg1> <arg2> ... <argN>)      [ BECOMES -> ]
  //      ((lambda (l) l) (list <arg1> <arg2> ... <argN>))
  void transform_variadic_vals_into_a_list(frame_vars& vars,frame_vals& vals,const size_type continuation_offset)noexcept{
    const size_type va_arg_idx = vars.size()-2-continuation_offset;
    // Transform the arg names & vals as needed
    vars[va_arg_idx] = vars[va_arg_idx+1]; // shift up variadic arg name (erasing '.')
    vars.erase(vars.begin()+va_arg_idx+1); // erase the now-duplicate var-arg name
    data list_of_vals;
    if(vals.empty())
      list_of_vals = symconst::emptylist;
    else
      list_of_vals = primitive_LIST_to_CONS_constructor(vals.begin()+va_arg_idx, vals.end()-continuation_offset);
    vals.erase(vals.begin()+va_arg_idx, vals.end()-continuation_offset); // erase individual arg instances
    vals.insert(vals.end()-continuation_offset, list_of_vals); // reinsert args as a list
  }


  // Confirm given no args & NOT applying a void-arg fcn & NOT a variadic-arg fcn
  bool nullary_invocation_of_non_nullary_procedure(const frame_vars& vars,const frame_vals& vals)noexcept{
    return vals.empty() && !vars.empty() && !(vars.size()==2 && symbol_is_dot_operator(vars[0]));
  }


  // Confirm passing an arg to an argless procedure
  bool non_nullary_invocation_of_nullary_procedure(const frame_vars& vars,const frame_vals& vals)noexcept{
    return !vals.empty() && vars.empty();
  }


  // Confirm <vars> is a cps-variadic procedure
  bool is_cps_variadic_arg_declaration(const frame_vars& vars)noexcept{
    return vars.size() > 2 && string_begins_with(vars[vars.size()-1],symconst::continuation) 
                           && symbol_is_dot_operator(vars[vars.size()-3]);
  }


  // Determine whether proc takes variadic args
  bool variadic_arg_declaration(const frame_vars& vars, const bool is_cps_variadic)noexcept{
    return (vars.size() > 1 && symbol_is_dot_operator(vars[vars.size()-2])) || is_cps_variadic;
  }


  // Determine whether enough vals for the variadic arg decl
  bool invalid_variadic_arg_declaration(const frame_vars& vars, const frame_vals& vals, const bool is_cps_variadic)noexcept{
    return vals.size() < vars.size() - 2 - is_cps_variadic; // - again if at a continuation
  }


  // Wrapper composing the above helpers
  bool confirm_valid_environment_extension(frame_vars& vars, frame_vals& vals, 
                                                       const sym_type& name){
    if(nullary_invocation_of_non_nullary_procedure(vars,vals))
      THROW_ERR("Too few arguments supplied! -- EXTEND_ENVIRONMENT" 
        << improper_call_alert(name,vals,vars));
    if(non_nullary_invocation_of_nullary_procedure(vars,vals))
      THROW_ERR("Too many arguments supplied! -- EXTEND_ENVIRONMENT" 
        << improper_call_alert(name,vals,vars));
    // Transform variadic arg's corresponding values into a list (if present)
    const bool is_cps_variadic = is_cps_variadic_arg_declaration(vars);
    if(variadic_arg_declaration(vars,is_cps_variadic)) {
      if(invalid_variadic_arg_declaration(vars,vals,is_cps_variadic))
        THROW_ERR("Too few arguments supplied! -- EXTEND_ENVIRONMENT" 
        << improper_call_alert(name,vals,vars));
      transform_variadic_vals_into_a_list(vars,vals,is_cps_variadic);
    }
    return vars.size() == vals.size();
  }

  /******************************************************************************
  * ENVIRONMENT DATA STRUCTURE IMPLEMENTATION
  ******************************************************************************/

  // -- ENVIRONMENTAL EXTENSION
  env_type extend_environment(frame_vars&& vars, frame_vals& vals, env_type& base_env, 
                                                             const sym_type& name = ""){
    // If valid extension, return environment w/ a new frame prepended
    if(confirm_valid_environment_extension(vars,vals,name)) {
      env_type extended_env(make_env());
      extended_env->frame = frame_type(vars,vals,frame_macs());
      extended_env->parent = base_env;
      return extended_env;
    // Invalid extension
    } else if(vars.size() < vals.size()) {
      THROW_ERR("Too many arguments supplied! -- EXTEND_ENVIRONMENT" 
        << improper_call_alert(name,vals,vars));
    } else {
      THROW_ERR("Too few arguments supplied! -- EXTEND_ENVIRONMENT" 
        << improper_call_alert(name,vals,vars));
    }
  }

  // R-value overload is _ONLY_ to launch the global environment
  env_type extend_environment(frame_vars&& vars, frame_vals&& vals, env_type& base_env){
    return extend_environment(std::move(vars),vals,base_env,"");
  }


  // -- VARIABLE LOOKUP
  frame_val lookup_variable_value(const frame_var& var, env_type& env) {
    bool found = false;
    auto val = env->lookup_variable_value(var, found);
    if(found) return val;
    THROW_ERR("Variable " << var << " is not bound!");
  }


  // -- VARIABLE SETTING: (set! <var> <val>)
  void set_variable_value(const frame_var& var, frame_val&& val, env_type& env) {
    if(!env->set_variable_value(var, std::move(val))) {
      scm_list invalid_set_call(3);
      invalid_set_call[0] = symconst::set;
      invalid_set_call[1] = var;
      invalid_set_call[2] = val;
      THROW_ERR("Variable "<<var<<" is not bound!"<<EXP_ERR(invalid_set_call));
    }
  }


  // -- VARIABLE DEFINITION: (define <var> <val>)
  void define_variable(const frame_var& var, frame_val val, env_type& env)noexcept{
    env->define_variable(var,std::move(val));
  }


  // -- MACRO DEFINITION: (define-syntax <label> <syntax-transformer>)
  void define_syntax_extension(const frame_mac& mac, env_type& env)noexcept{
    env->define_macro(mac);
  }

  /******************************************************************************
  * REPRESENTING READER ALIASES: (define-reader-alias <alias> <name>)
  ******************************************************************************/

  bool is_defn_reader_alias(const scm_list& exp) noexcept{
    return is_tagged_list(exp,symconst::defn_reader_alias);
  }

  exe_fcn_t analyze_defn_reader_alias(scm_list& exp) { 
    static constexpr const char * const format = 
      "\n     (define-reader-alias <alias-symbol> <name-symbol>)"
      "\n     (define-reader-alias <alias-symbol-to-delete>)";
    if(exp.size() == 1 || exp.size() > 3)
      THROW_ERR("'define-reader-alias receive incorrect # of symbols!" << format << EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'define-reader-alias 1st arg isn't a symbol!" << format << EXP_ERR(exp));
    data val;
    if(exp.size() == 2) {
      val = delete_reader_alias(exp[1].sym);
    } else {
      if(!exp[2].is_type(types::sym))
        THROW_ERR("'define-reader-alias 2nd arg isn't a symbol!" << format << EXP_ERR(exp));
      if(exp[1].sym != exp[2].sym) // no-op if defining a symbol to itself
        register_reader_alias(exp[1].sym,exp[2].sym);
      val = data(types::dne);
    }
    return [val=std::move(val)](env_type&){return val;};
  }

  /******************************************************************************
  * REPRESENTING CONDITIONALS: (if <predicate> <consequent> <alternative>)
  ******************************************************************************/

  // -- IDENTIFICATION, GETTERS, & CONSTRUCTION
  bool is_if(const scm_list& exp)  noexcept{return is_tagged_list(exp,symconst::if_t);}
  data if_predicate(scm_list& exp) noexcept{return exp[1];}
  data if_consequent(scm_list& exp)noexcept{return exp[2];}

  data if_alternative(scm_list& exp)noexcept{
    if(exp.size() == 4) return exp[3]; // if has an <alternative>
    return GLOBALS::VOID_DATA_OBJECT;  // w/o <alternative> return VOID
  }


  // -- ANALYSIS
  void confirm_valid_if(const scm_list& exp) {
    if(exp.size() < 3) 
      THROW_ERR("'if didn't receive enough args:"
        "\n     (if <predicate> <consequent> <optional-alternative>)"
        << EXP_ERR(exp));
    if(exp.size() > 4) 
      THROW_ERR("'if received too many args:"
        "\n     (if <predicate> <consequent> <optional-alternative>)"
        << EXP_ERR(exp));
  }

  // Returns lambda so that if true, only eval consequent: 
  //   else, only eval alternative
  exe_fcn_t analyze_if(scm_list& exp,const bool tail_call=false,const bool cps_block=false) { 
    confirm_valid_if(exp);
    auto pproc = scm_analyze(if_predicate(exp),false,cps_block);
    auto cproc = scm_analyze(if_consequent(exp),tail_call,cps_block);
    auto aproc = scm_analyze(if_alternative(exp),tail_call,cps_block);
    return [pproc=std::move(pproc),cproc=std::move(cproc),
            aproc=std::move(aproc)](env_type& env){
      if(pproc(env).is_truthy()) 
        return cproc(env);
      return aproc(env);
    };
  }

  /******************************************************************************
  * REPRESENTING SEQUENCES: (begin <body>)
  ******************************************************************************/

  bool is_begin(const scm_list& exp)   noexcept{return is_tagged_list(exp,symconst::begin);}
  scm_list begin_actions(scm_list& exp)noexcept{return scm_list(exp.begin()+1, exp.end());}

  // Analyzes each expression, then returns an exec proc which 
  //   sequentially invokes each expression's exec proc
  exe_fcn_t analyze_sequence(scm_list&& exps,const bool tail_call=false,const bool cps_block=false){ // used for 'begin' & lambda bodies
    if(exps.empty())
      return [](env_type&){return GLOBALS::VOID_DATA_OBJECT;}; // void data
    const size_type n = exps.size();
    // If begin only has 1 expression, return exec proc of expression
    if(exps.size() == 1) return scm_analyze(std::move(exps[0]),tail_call,cps_block);
    // Analyze each expression
    std::vector<exe_fcn_t> sequence_exe_procs(exps.size());
    for(size_type i = 0, n = exps.size(); i < n; ++i)
      sequence_exe_procs[i] = scm_analyze(std::move(exps[i]),(i+1==n)&&tail_call,cps_block);
    // Return a lambda sequentially invoking each exec procedure
    return [n=std::move(n),sequence_exe_procs=std::move(sequence_exe_procs)]
    (env_type& env){
      for(size_type i = 0; i+1 < n; ++i)
        sequence_exe_procs[i](env);
      return sequence_exe_procs[n-1](env);
    };
  }

  /******************************************************************************
  * REPRESENTING ASSIGNMENT: (set! <var> <val>)
  ******************************************************************************/

  bool is_assignment(const scm_list& exp)     noexcept{return is_tagged_list(exp,symconst::set);}
  sym_type assignment_variable(scm_list& exp) noexcept{return exp[1].sym;}
  data assignment_value(scm_list& exp)        noexcept{return exp[2];}

  // Analyzes value being assigned, & returns an execution procedure 
  //   to install it as the variable in the designated env
  exe_fcn_t analyze_assignment(scm_list& exp,const bool cps_block=false) { 
    confirm_valid_assignment(exp);
    if(!symbol_is_property_chain_access(exp[1].sym)) {
      auto var = assignment_variable(exp);
      auto value_proc = scm_analyze(assignment_value(exp),false,cps_block);
      return [var=std::move(var),value_proc=std::move(value_proc)](env_type& env){
        set_variable_value(var,value_proc(env),env);
        return GLOBALS::VOID_DATA_OBJECT; // return is void
      };
    }
    scm_list set_call(4);
    set_call[0] = "heist:core:oo:set-property!";
    set_call[1] = exp[1].sym.substr(0, exp[1].sym.rfind('.'));
    set_call[2] = scm_list(2);
    set_call[2].exp[0] = symconst::quote;
    set_call[2].exp[1] = exp[1].sym.substr(exp[1].sym.rfind('.')+1);
    set_call[3] = std::move(exp[2]);
    return scm_analyze(std::move(set_call),false,cps_block);
  }

  /******************************************************************************
  * REPRESENTING DEFINITION: (define <var> <val>)
  ******************************************************************************/

  bool is_definition(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::define);}
  data make_lambda(scm_list parameters, scm_list body)noexcept; // Lambda ctor

  frame_var& definition_variable(scm_list& exp)noexcept{
    // if defining a variable, else defining a procedure
    if(exp[1].is_type(types::sym)) return exp[1].sym; 
    return exp[1].exp[0].sym;
  }

  data definition_value(scm_list& exp)noexcept{
    // if defining a variable, else defining a procedure
    if(exp[1].is_type(types::sym)) return exp[2]; 
    scm_list args(exp[1].exp.begin()+1,exp[1].exp.end());
    scm_list body(exp.begin()+2,exp.end());
    return make_lambda(args,body);
  }

  bool is_obj_property_definition(const scm_list& exp)noexcept{
    return exp[1].is_type(types::sym) && symbol_is_property_chain_access(exp[1].sym);
  }

  // Generate an 'add-property! call from the <define> expression
  data convert_obj_property_defintion_to_method_call(const scm_list& exp)noexcept{
    scm_list def_call(4);
    def_call[0] = "heist:core:oo:add-property!";
    def_call[1] = exp[1].sym.substr(0, exp[1].sym.rfind('.'));
    def_call[2] = scm_list(2);
    def_call[2].exp[0] = symconst::quote;
    def_call[2].exp[1] = exp[1].sym.substr(exp[1].sym.rfind('.')+1);
    def_call[3] = exp[2];
    return def_call;
  }

  // Analyzes value being defined, & returns an execution procedure 
  //   to install it as the variable in the designated env
  exe_fcn_t analyze_definition(scm_list& exp,const bool cps_block=false) { 
    confirm_valid_definition(exp);
    if(!is_obj_property_definition(exp)) {
      auto& var       = definition_variable(exp);
      auto value_proc = scm_analyze(definition_value(exp),false,cps_block);
      return [var=std::move(var),value_proc=std::move(value_proc)](env_type& env){
        define_variable(var,value_proc(env),env);
        return GLOBALS::VOID_DATA_OBJECT; // return is void
      };
    }
    return scm_analyze(convert_obj_property_defintion_to_method_call(exp),false,cps_block);
  }

  /******************************************************************************
  * REPRESENTING PROMISES: (delay <expression>)
  ******************************************************************************/

  bool is_delay(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::delay);}

  // Extracts the delayed expression and returns an exec proc ctor'ing a promise
  exe_fcn_t analyze_delay(scm_list& exp,const bool cps_block=false) {
    if(exp.size() != 2) 
      THROW_ERR("'delay expects 1 argument: (delay <delay-expression>)" << EXP_ERR(exp));
    if(!cps_block)
      return [delayed_datum=std::move(exp[1])](env_type& env){
        return make_del(delayed_datum,env,false);
      };
    // Bind delayed CPS expressions to always have 'id as the topmost continuation,
    //   since FORCE is defined outside a scm->cps block, would have its arg bound 
    //   to such regardless if implemented directly in heist scheme as well.
    data delay_list = scm_list(2);
    delay_list.exp[0] = generate_fundamental_form_cps(exp[1]);
    delay_list.exp[1] = "id";
    return [delay_list=std::move(delay_list)](env_type& env){
      return make_del(delay_list,env,true);
    };
  }

  /******************************************************************************
  * REPRESENTING QUOTATION: (quote <expression>)
  ******************************************************************************/

  bool is_quoted(const scm_list& exp)noexcept{return is_tagged_list(exp, symconst::quote);}

  // Quoting a vector literal is a special case of quotation
  bool is_vector_literal(const scm_list& exp)noexcept{
    return is_tagged_list(exp,symconst::vec_literal);
  }

  // Confirm whether quoting a vector literal
  bool quoting_a_vector_literal(const scm_list& exp)noexcept{
    return exp[1].is_type(types::exp) && !exp[1].exp.empty() && 
           is_vector_literal(exp[1].exp);
  }

  // Quoting a hash-map literal is a special case of quotation
  bool is_hmap_literal(const scm_list& exp)noexcept{
    return is_tagged_list(exp,symconst::map_literal);
  }

  // Confirm whether quoting a vector literal
  bool quoting_an_hmap_literal(const scm_list& exp)noexcept{
    return exp[1].is_type(types::exp) && !exp[1].exp.empty() && 
           is_hmap_literal(exp[1].exp);
  }

  bool is_variadic_cps_procedure_signature(const size_type i, const size_type n, scm_list& exp)noexcept{
    return i+3 == n && data_is_continuation_parameter(exp[n-1]) && !data_is_dot_operator(exp[i+1]);
  }

  // Returns quoted data's contents
  data text_of_quotation(scm_list& exp)noexcept{
    if(!exp[1].is_type(types::sym)) return exp[1];
    if(exp[1].sym==symconst::false_t || exp[1].sym==symconst::true_t)
      return boolean(exp[1].sym==symconst::true_t);
    return convert_string_to_symbol(exp[1].sym);
  }


  // Returns whether the quoted exp is a 'cons' via the (.) operator.
  //   If not, exp is a list & this returns false.
  //   => Throws error if more than 1 (.) found (improper use)
  //   => If is a valid 'cons' quote, also rm's the (.)
  bool is_quoted_cons(scm_list& exp, const sym_type& quote_name) {
    // Confirm (.) does not terminate the list
    if(!exp.empty() && data_is_dot_operator(*exp.rbegin()))
      THROW_ERR("Unexpected dot ("<<G.dot<<") terminated the quoted list! -- ANALYZE_QUOTED"
        << EXP_ERR('(' << quote_name << ' ' << data(exp).write() << ')'));
    // Iff exp begins w/ (.) & has a length of 2, it may be a valid instance of 
    //   quoting a variadic lambda [ ie '(lambda (. l) l) ] -- thus such is valid
    if(exp.size()==2 && data_is_dot_operator(exp[0]) && !data_is_dot_operator(exp[1]))
      return false;
    // Confirm no (.) prior the penultimate item in the list
    for(size_type i = 0, n = exp.size(); i+2 < n; ++i) {
      if(data_is_dot_operator(exp[i])) {
        if(is_variadic_cps_procedure_signature(i,n,exp)) return false;
        THROW_ERR("Unexpected dot ("<<G.dot<<") at position #"<<i+1<<" in quotation! -- ANALYZE_QUOTED"
          << EXP_ERR('(' << quote_name << ' ' << data(exp).write() << ')'));
      }
    }
    // Determine whether at a cons or a list
    if(exp.size() > 2 && data_is_dot_operator(*(exp.end()-2))) {
      exp.erase(exp.end()-2); // rm (.)
      return true; // cons
    }
    return false;  // list
  }


  // Analyzes the quote's vector/hmap literal & returns its execution procedure
  template <bool IS_VECTOR_LITERAL>
  exe_fcn_t analyze_quoted_vh_literal(scm_list& exp, const char* name) {
    scm_list args(exp.begin()+1,exp.end());
    if(is_quoted_cons(args, symconst::quote))
      THROW_ERR('\''<<name<<" had an unexpected dot ("<<G.dot<<")!"<<EXP_ERR(exp));
    // return an empty vector if given no args
    if(args.empty()) {
      if constexpr (IS_VECTOR_LITERAL)
        return [](env_type&){return make_vec(scm_list());};
      else
        return [](env_type&){return make_map(scm_map());};
    }
    // quote each item in the vector
    scm_list literal(args.size()+1);
    if constexpr (IS_VECTOR_LITERAL) 
      literal[0] = symconst::vector;
    else
      literal[0] = symconst::hmap;
    for(size_type i = 0, n = args.size(); i < n; ++i) {
      literal[i+1] = scm_list(2);
      literal[i+1].exp[0] = symconst::quote;
      literal[i+1].exp[1] = args[i];
    }
    // return analyzed vector
    return scm_analyze(std::move(literal));
  }


  exe_fcn_t analyze_quoted_vector_literal(scm_list& exp) {
    return analyze_quoted_vh_literal<true>(exp,"vector-literal");
  }


  exe_fcn_t analyze_quoted_hmap_literal(scm_list& exp) {
    return analyze_quoted_vh_literal<false>(exp,"hmap-literal");
  }


  // Analyzes the quote's text & returns an execution procedure for such
  exe_fcn_t analyze_quoted(scm_list& exp) {
    if(exp.size() != 2) 
      THROW_ERR("'quote form expects one argument: (quote <quoted-data>)!"<<EXP_ERR(exp));
    
    // Quote vector literals as needed
    if(quoting_a_vector_literal(exp)) return analyze_quoted_vector_literal(exp[1].exp);

    // Quote hmap literals as needed
    if(quoting_an_hmap_literal(exp)) return analyze_quoted_hmap_literal(exp[1].exp);
    
    // Get quoted data
    auto quoted_data = text_of_quotation(exp);
    
    // If quoted data is atomic, return as-is
    if(!quoted_data.is_type(types::exp))
      return [quoted_data=std::move(quoted_data)](env_type&){return quoted_data;};
    
    // If quoting an empty expression, return the empty list
    if(quoted_data.exp.empty())
      return [](env_type&){return symconst::emptylist;};
    
    // Confirm whether appending last item. 
    //   => NOTE: also rm's (.) if so, hence this must be done 
    //            PRIOR quoting each item
    bool append_last_item = is_quoted_cons(quoted_data.exp,symconst::quote);

    // Since quoting an expression, expand such into a list of quoted data
    scm_list quote_val(quoted_data.exp.size()+1);
    quote_val[0] = symconst::list;
    
    // Wrap 'quote' around each item in the list
    for(size_type i = 0, n = quoted_data.exp.size(); i < n; ++i){
      quote_val[i+1] = scm_list(2);
      quote_val[i+1].exp[0] = symconst::quote;
      quote_val[i+1].exp[1] = quoted_data.exp[i];
    }
    
    // Unpack the last item in the list and append it if quoting 
    //   a non-null-terminated list
    if(append_last_item) {
      auto last_item = *quote_val.rbegin();
      quote_val.pop_back();
      scm_list append_exp(3); 
      append_exp[0] = symconst::append;
      append_exp[1] = quote_val, append_exp[2] = last_item;
      quote_val = std::move(append_exp);
    }
    
    // Return the analyzed quoted list expression
    return scm_analyze(std::move(quote_val));
  }

  /******************************************************************************
  * REPRESENTING SELF-EVALUATING EXPRESSIONS & VARIABLES
  ******************************************************************************/

  bool is_self_evaluating(const data& datum)noexcept{
    return datum.is_self_evaluating();
  }

  bool is_variable(const data& datum)noexcept{return datum.is_type(types::sym);}

  /******************************************************************************
  * REPRESENTING LAMBDA PROCEDURES (FASTER THAN FN)
  ******************************************************************************/

  // -- LAMBDAS: (lambda (<parameters>) <body>)
  bool     is_lambda(const scm_list& exp)  noexcept{return is_tagged_list(exp,symconst::lambda);}
  scm_list lambda_parameters(scm_list& exp)noexcept{return exp[1].exp;}
  scm_list lambda_body(scm_list& exp)      noexcept{return scm_list(exp.begin()+2,exp.end());}

  // Recursivly (for fn) replace instances of G.dot w/ symconst::dot
  void replace_param_temporary_dot_with_internal_dot(scm_list& params)noexcept{
    for(auto& d : params) {
      if(d.is_type(types::sym) && d.sym == G.dot) 
        d.sym = symconst::dot;
      else if(d.is_type(types::exp))
        replace_param_temporary_dot_with_internal_dot(d.exp);
    }
  }

  // Is a lambda using optional args (gets converted to a <fn>)
  // WARNING: DOES __NOT__ VALIDATE SUCH IS IN PROPER FORM
  bool is_opt_arg_lambda(const scm_list& exp)noexcept{
    if(exp.size() >= 3 && exp[0].is_type(types::sym) && exp[0].sym == symconst::lambda && exp[1].is_type(types::exp))
      for(const auto& arg : exp[1].exp)
        if(arg.is_type(types::exp))
          return true;
    return false;
  }

  // Paramaters end with a continuation
  bool params_end_with_a_continuation(const scm_list& params)noexcept{
    return !params.empty() && data_is_continuation_parameter(*params.rbegin());
  }

  // Validate lambda using optional args prior fn transformation
  void validate_lambda_opt_args(const scm_list& exp) {
    const auto& vars = exp[1].exp;
    const size_type n = vars.size();
    // variadic (.) arg must have a label afterwards
    if(n != 0 && data_is_dot_operator(vars[n-1]))
      THROW_ERR("Expected one item after variadic dot ("<<G.dot<<")! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
    // Search the vars list of the fcn's args for improper (.) use & duplicate arg names
    bool found_opt_arg = false;
    for(size_type i = 0; i < n; ++i) {
      if(vars[i].is_type(types::sym)) {
        // Variadic (.) must come just prior the last arg
        if(symbol_is_dot_operator(vars[i].sym)) {
          if(i+3 == n && vars[i+2].is_type(types::sym) && string_begins_with(vars[i+2].sym, symconst::continuation)) 
            return; // allow continuations after variadic
          if(i+2 != n) 
            THROW_ERR("More than one item found after variadic dot ("<<G.dot<<")! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
          if(!vars[i+1].is_type(types::sym))
            THROW_ERR("Variadic arg can't accept optional values! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
          return;
        // Confirm haven't already discovered an arg w/ an optional value
        } else if(found_opt_arg) {
          THROW_ERR("All mandatory args must precede all optional args! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
        }
      // Confirm valid optional arg format
      } else if(vars[i].is_type(types::exp)) {
        found_opt_arg = true;
        if(vars[i].exp.size() != 2 || !vars[i].exp[0].is_type(types::sym))
          THROW_ERR("Improper optional arg format: (<optional-arg-name> <value>) -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
      // ERROR: Neither symbolic arg name, nor optional arg expression
      } else {
        THROW_ERR("Arg wasn't an optional-arg expression or symbolic name! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
      }
    }
  }

  // Lambda->fn when given a lambda with optional args
  // PRECONDITION: is_opt_arg_lambda(exp)
  scm_list convert_lambda_opt_args_to_fn(const scm_list& exp) {
    validate_lambda_opt_args(exp);
    auto& params = exp[1].exp;
    const size_type n = params.size(), variadic_offset = 2;
    bool is_variadic = n > 1 && data_is_dot_operator(params[n-variadic_offset]);
    bool found_dflt = false;
    // Get vectors of the mandatory args, & form "define" exprs for the default args
    scm_list mandatory_args, default_value_defns;
    for(size_type i = 0, j = 0, m = n - (variadic_offset * is_variadic); i < m; ++i) {
      if(params[i].is_type(types::exp)) {
        found_dflt = true;
        default_value_defns.push_back(scm_list(3));
        default_value_defns[j].exp[0] = symconst::define;
        default_value_defns[j].exp[1] = params[i].exp[0];
        default_value_defns[j++].exp[2] = params[i].exp[1];
      } else if(!found_dflt) {
        mandatory_args.push_back(params[i]);
      }
    }
    if(is_variadic) {
      size_type i = default_value_defns.size();
      default_value_defns.push_back(scm_list(3));
      default_value_defns[i].exp[0] = symconst::define;
      default_value_defns[i].exp[1] = params[n-(variadic_offset-1)].sym;
      default_value_defns[i].exp[2] = scm_list(2);
      default_value_defns[i].exp[2].exp[0] = symconst::quote;
      default_value_defns[i].exp[2].exp[1] = scm_list();
    }
    // Generate <fn>
    const size_type fn_size = 2+default_value_defns.size()-is_variadic;
    scm_list fn_expr(fn_size);
    fn_expr[0] = symconst::fn;
    // Generate <fn> bodies
    for(size_type i = 1; i < fn_size; ++i) {
      bool last_instance = is_variadic && i+1 == fn_size;
      // Generate <fn> parameter list instance
      fn_expr[i] = scm_list(1,mandatory_args); // param_list
      for(size_type j = 0; j < i-1; ++j)  // mandatory args that could've been defaults
        fn_expr[i].exp[0].exp.push_back(default_value_defns[j].exp[1]);
      if(last_instance) {                 // add in variadic arg as needed
        fn_expr[i].exp[0].exp.push_back(symconst::dot);
        fn_expr[i].exp[0].exp.push_back(default_value_defns.rbegin()->exp[1]);
      }
      // Generate <fn> body instance
      for(size_type j = i-1, n = default_value_defns.size()-last_instance; j < n; ++j)
        fn_expr[i].exp.push_back(default_value_defns[j]); // insert dflt value defns
      for(size_type j = 2, n = exp.size(); j < n; ++j)
        fn_expr[i].exp.push_back(exp[j]);                 // insert body
    }
    return fn_expr;
  }

  // Ctor for lambdas
  data make_lambda(scm_list parameters,scm_list body)noexcept{
    scm_list new_lambda(body.size()+2); 
    new_lambda[0] = symconst::lambda, new_lambda[1] = std::move(parameters);
    std::move(body.begin(), body.end(), new_lambda.begin()+2);
    return new_lambda;
  }

  // Returns an exec proc to mk a lambda w/ the analyzed parameter list & body
  exe_fcn_t analyze_lambda(scm_list& exp,const bool cps_block=false) {
    // convert lambdas w/ optional args to fns
    exe_fcn_t analyze_fn(scm_list&,const bool cps_block);
    if(is_opt_arg_lambda(exp)) 
      return scm_analyze(convert_lambda_opt_args_to_fn(exp),false,cps_block);
    // handle regular lambdas
    confirm_valid_lambda(exp);
    auto vars = lambda_parameters(exp);
    // create the lambda
    confirm_valid_procedure_parameters(vars,exp); // validate parameters
    replace_param_temporary_dot_with_internal_dot(vars);
    auto body_proc = analyze_sequence(lambda_body(exp),true,cps_block); // analyze body syntax
    // set CPS value if needed
    if(params_end_with_a_continuation(vars)) {
      return [vars=std::move(vars),body_proc=std::move(body_proc)](env_type& env){
        auto proc = scm_fcn(vars, body_proc, env, ""); // empty "" name by default (anon proc)
        proc.set_cps_procedure(true);
        return proc;
      };
    }
    return [vars=std::move(vars),body_proc=std::move(body_proc)](env_type& env){
      return scm_fcn(vars, body_proc, env, ""); // empty "" name by default (anon proc)
    };
  }


  // -- PROCEDURAL APPLICATION
  data operator_of(const scm_list& exp)  noexcept{return exp[0];}
  scm_list operands(const scm_list& exp) noexcept{return scm_list(exp.begin()+1, exp.end());}

  /******************************************************************************
  * REPRESENTING FN PROCEDURES (MORE DYNAMIC THAN LAMBDA)
  ******************************************************************************/

  bool is_fn(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::fn);}


  #define FN_LAYOUT "\n     (fn ((<arg> ...) <body> ...) ...)"
  void validate_fn_arg_quote_or_container_literal(const scm_list&,const scm_list&);
  void validate_fn_vect_arg_literal(const scm_list& exp, const scm_list& vect_arg) {
    for(size_type i = 1, n = vect_arg.size(); i < n; ++i)
      if(vect_arg[i].is_type(types::exp)) 
        validate_fn_arg_quote_or_container_literal(exp,vect_arg[i].exp);
  }

  void validate_fn_hmap_arg_literal(const scm_list& exp, const scm_list& hmap_arg) {
    if(!(hmap_arg.size() & 1)) // with literal tag, odd # of items = even # of args
      THROW_ERR("'fn invalid hmap literal in arg paramters,"
        "\n     uneven # of elts: " << data(hmap_arg) << FN_LAYOUT << EXP_ERR(exp));
    for(size_type i = 1, n = hmap_arg.size(); i < n; i += 2) {
      if(hmap_arg[i].is_type(types::exp))
        THROW_ERR("'fn invalid hmap literal in arg paramters,"
          "\n     found unhashable container key: " << data(hmap_arg) << FN_LAYOUT << EXP_ERR(exp));
      if(hmap_arg[i+1].is_type(types::exp)) 
        validate_fn_arg_quote_or_container_literal(exp,hmap_arg[i+1].exp);
    }
  }

  void validate_fn_list_arg_literal(const scm_list& exp, const scm_list& list_arg) {
    for(size_type i = 0, n = list_arg.size(); i < n; ++i) {
      if(data_is_dot_operator(list_arg[i]) && i+2 != n) {
        THROW_ERR("'fn invalid variadic list literal in arg (\".\" must be 2nd to last arg): "
          << data(list_arg) << FN_LAYOUT << EXP_ERR(exp));
      } else if(list_arg[i].is_type(types::exp)) {
        validate_fn_arg_quote_or_container_literal(exp,list_arg[i].exp);
      }
    }
  }

  void validate_fn_arg_quote_or_container_literal(const scm_list& exp, const scm_list& container) {
    if(container.empty()) return;
    // Guarenteed to be a list literal
    if(!container[0].is_type(types::sym)) {
      validate_fn_list_arg_literal(exp, container);
    } else if(container[0].sym == symconst::quote) {
      if(container.size() != 2 || !container[1].is_type(types::sym))
        THROW_ERR("'fn invalid quote in arg, did NOT quote a single symbol: " 
          << data(container) << FN_LAYOUT << EXP_ERR(exp));
    } else if(container[0].sym == symconst::vec_literal) {
      validate_fn_vect_arg_literal(exp, container);
    } else if(container[0].sym == symconst::map_literal) {
      validate_fn_hmap_arg_literal(exp, container);
    } else {
      validate_fn_list_arg_literal(exp, container);
    }
  }

  bool fn_invalid_variadic_arg(const size_type& i, const size_type& n, const scm_list& args)noexcept{
    return data_is_dot_operator(args[i]) &&
      !((i+2 == n && args[i+1].is_type(types::sym)) || 
        (i+3 == n && args[i+1].is_type(types::sym) && data_is_continuation_parameter(args[i+2])));
  }

  // Verify:
  // 0. variadics (must have a symbol token after ".") (only checked for in topmost call)
  // 1. hmaps (must have non-container keys & an even # of elts)
  // 2. validate list literals have "." as 2nd to last elt
  // 3. validate only quoting symbols
  void validate_fn_arg_signature(const scm_list& exp, const scm_list& args) {
    for(size_type i = 0, n = args.size(); i < n; ++i) {
      // validate symbol's proper variadic arg use
      if(fn_invalid_variadic_arg(i,n,args)) {
        THROW_ERR("'fn invalid variadic arg use (\".\" must be 2nd to last arg,"
          "\n     & last arg must be symbolic): " << data(args) << FN_LAYOUT << EXP_ERR(exp));
      // validate quote/hmap/vector/list literal
      } else if(args[i].is_type(types::exp)) {
        validate_fn_arg_quote_or_container_literal(exp,args[i].exp);
      }
    }
  }

  void validate_fn(const scm_list& exp) {
    if(exp.size() == 1)
      THROW_ERR("'fn didn't receive any match expressions!" FN_LAYOUT << EXP_ERR(exp));
    for(size_type i = 1, n = exp.size(); i < n; ++i) {
      if(!exp[i].is_type(types::exp) || exp[i].exp.size() < 2)
        THROW_ERR("'fn invalid non-exp match expression: " << PROFILE(exp[i]) << "!" FN_LAYOUT << EXP_ERR(exp));
      if(!exp[i].exp[0].is_type(types::exp))
        THROW_ERR("'fn invalid args-list in match expression: " << PROFILE(exp[i]) << "!" FN_LAYOUT << EXP_ERR(exp));
      validate_fn_arg_signature(exp,exp[i].exp[0].exp);
    }
  }
  #undef FN_LAYOUT


  exe_fcn_t analyze_fn(scm_list& exp,const bool cps_block=false) {
    validate_fn(exp);
    const size_type total_matches = exp.size()-1;
    std::vector<exp_type> param_insts(total_matches);
    std::vector<exe_fcn_t> bodies(total_matches);
    for(size_type i = 0; i < total_matches; ++i) {
      param_insts[i] = exp[i+1].exp[0].exp;
      bodies[i] = analyze_sequence(scm_list(exp[i+1].exp.begin()+1,exp[i+1].exp.end()),true,cps_block);
    }
    for(auto& params : param_insts)
      replace_param_temporary_dot_with_internal_dot(params);
    // set CPS value if needed
    if(!param_insts.empty() && params_end_with_a_continuation(param_insts[0])) {
      return [param_insts=std::move(param_insts),bodies=std::move(bodies)](env_type& env){
        auto proc = scm_fcn(param_insts, bodies, env, ""); // empty "" name by default (anon proc)
        proc.set_cps_procedure(true);
        return proc;
      };
    }
    return [param_insts=std::move(param_insts),bodies=std::move(bodies)](env_type& env){
      return scm_fcn(param_insts, bodies, env, ""); // empty "" name by default (anon proc)
    };
  }

  /******************************************************************************
  * DEFCLASS: (defclass <name> (<inheritance-list>) ...)
  ******************************************************************************/

  bool is_defclass(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::defclass);}


  #define DEFCLASS_LAYOUT\
    "\n     (defclass <class-name> (<optional-inherited-prototype>) <member-or-method-instances>)"\
    "\n     => <member-or-method> ::= (<member-name> <default-value>)"\
    "\n                             | (<method-name> <procedure-value>)"\
    "\n                             | ((<method-name> <arg1> <arg2> ...) <body> ...)"\
    "\n                             | ((make-<class-name> <arg> ...) <body> ...) ; constructor"\
    "\n                             | (make-<class-name> ((<arg> ...) <body> ...) ...) ; fn ctor"\
    "\n                             | ((eq? <obj>) <body> ...)    ; overload eq?"\
    "\n                             | ((eqv? <obj>) <body> ...)   ; overload eqv?"\
    "\n                             | ((equal? <obj>) <body> ...) ; overload equal?"\
    "\n                             | ((self= <obj>) <body> ...)  ; overload all the above"\
    "\n                             | ((write) <body> ...)        ; overload write"\
    "\n                             | ((display) <body> ...)      ; overload display"\
    "\n                             | ((pprint) <body> ...)       ; overload pretty-print"\
    "\n                             | ((self->string) <body> ...) ; overload all the above"\
    "\n                             | ((self->copy) <body> ...)   ; overload copy"\
    "\n                             | ((self->procedure <arg> ...) <body> ...) ; overload application"


  // -- OPTIONAL ARG CONVERSION
  // converts inline methods using opt args to a <fn> method
  // converts inline ctor using opt args to use its <fn> syntax
  void convert_opt_args_method_or_ctor_to_fn_expr(const sym_type& ctor_name, scm_list& exp) {
    sym_type name = exp[0].exp[0].sym;
    scm_list inline_method_as_lambda(exp.size()+1);
    inline_method_as_lambda[0] = symconst::lambda;
    inline_method_as_lambda[1] = scm_list(exp[0].exp.begin()+1,exp[0].exp.end());
    std::copy(exp.begin()+1,exp.end(),inline_method_as_lambda.begin()+2);
    if(ctor_name == name) { // ctor
      exp = convert_lambda_opt_args_to_fn(inline_method_as_lambda);
      exp[0] = ctor_name;
    } else { // method
      exp = scm_list(2);
      exp[0] = name, exp[1] = inline_method_as_lambda;
    }
  }

  // -- ERROR HANDLING
  void validate_defclass(scm_list& exp) {
    if(exp.size() < 3)
      THROW_ERR("'defclass not enough arguments given!" DEFCLASS_LAYOUT << EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'defclass 1st arg "<<PROFILE(exp[1])<<" isn't a symbolic class name!" DEFCLASS_LAYOUT << EXP_ERR(exp));
    if(!exp[2].is_type(types::exp))
      THROW_ERR("'defclass 2nd arg "<<PROFILE(exp[2])<<" isn't an inherited prototype definition!" DEFCLASS_LAYOUT << EXP_ERR(exp));
    if(exp[2].exp.size() > 1)
      THROW_ERR("'defclass 2nd arg "<<PROFILE(exp[2])<<" has more than 1 inherited prototype (no multi inheritance)!" DEFCLASS_LAYOUT << EXP_ERR(exp));
    if(exp[2].exp.size() == 1 && !exp[2].exp[0].is_type(types::sym))
      THROW_ERR("'defclass 2nd arg (inherited entity) "<<PROFILE(exp[2])<<" isn't a symbolic class prototype name!" DEFCLASS_LAYOUT << EXP_ERR(exp));
    sym_type ctor_name = "make-"+exp[1].sym;
    for(size_type i = 3, n = exp.size(); i < n; ++i) {
      if(!exp[i].is_type(types::exp) || exp[i].exp.size() < 2)
        THROW_ERR("'defclass invalid <member-or-method-instance> => " << PROFILE(exp[i]) << DEFCLASS_LAYOUT << EXP_ERR(exp));
      if(exp[i].exp[0].is_type(types::sym)) { // member | fn ctor
        if(exp[i].exp[0].sym == ctor_name) { // fn ctor
          if(exp[i].exp.size() == 1)
            THROW_ERR("'defclass <constructor> missing fn arg-body instances => " << PROFILE(exp[i]) << DEFCLASS_LAYOUT << EXP_ERR(exp));
          for(size_type j = 1, n = exp[i].exp.size(); j < n; ++j)
            if(!exp[i].exp[j].is_type(types::exp) || exp[i].exp[j].exp.size() < 2 || !exp[i].exp[j].exp[0].is_type(types::exp))
              THROW_ERR("'defclass <constructor> invalid fn arg-body instances => " << PROFILE(exp[i]) << DEFCLASS_LAYOUT << EXP_ERR(exp));
          continue;
        }
        if(exp[i].exp.size() != 2)
          THROW_ERR("'defclass invalid <member-or-method-instance> => " << PROFILE(exp[i]) << DEFCLASS_LAYOUT << EXP_ERR(exp));
        if(exp[i].exp[0].sym == "super")
          THROW_ERR("'defclass invalid member name, <super> already defined => " << PROFILE(exp[i]) << DEFCLASS_LAYOUT << EXP_ERR(exp));
        if(exp[i].exp[0].sym == "prototype")
          THROW_ERR("'defclass invalid member name, <prototype> already defined => " << PROFILE(exp[i]) << DEFCLASS_LAYOUT << EXP_ERR(exp));
        continue;
      }
      if(exp[i].exp[0].is_type(types::exp)) { // method
        for(size_type j = 0, m = exp[i].exp[0].exp.size(); j < m; ++j) { // verify proper name/parameters
          if(!exp[i].exp[0].exp[j].is_type(types::sym)) {
            if(j && exp[i].exp[0].exp[j].is_type(types::exp)) { // detected optional arg use
              convert_opt_args_method_or_ctor_to_fn_expr(ctor_name,exp[i].exp);
              break;
            }
            THROW_ERR("'defclass invalid <member-or-method-instance> => " << PROFILE(exp[i]) << DEFCLASS_LAYOUT << EXP_ERR(exp));
          }
        }
        continue;
      }
      THROW_ERR("'defclass invalid <member-or-method-instance> => " << PROFILE(exp[i]) << DEFCLASS_LAYOUT << EXP_ERR(exp));
    }
  }

  void validate_inherited_entity(class_prototype& proto, scm_list& exp, env_type& env){
    if(exp[2].exp.empty()) return;
    auto result = lookup_variable_value(exp[2].exp[0].sym,env);
    if(!result.is_type(types::cls))
      THROW_ERR("'defclass inheritance entity " << PROFILE(exp[2].exp[0]) << " isn't a class prototype!" 
        << DEFCLASS_LAYOUT << EXP_ERR(exp));
    proto.super = result.cls;
  }

  void validate_unique_property_name(scm_list& exp,const scm_string& name,const std::vector<scm_string>& seen_names,const char* message){
    for(const auto& n : seen_names)
      if(name == n)
        THROW_ERR("'defclass " << exp[1].sym << " => \"" << name << "\" " << message << '!'
          << DEFCLASS_LAYOUT << EXP_ERR(exp));
  }


  // -- CLASS PROTOTYPE GENERATION HELPERS
  exe_fcn_t convert_method_to_lambda(scm_list& method_exp,const bool cps_block) {
    scm_list method_lambda(1+method_exp.size());
    method_lambda[0] = symconst::lambda;
    method_lambda[1] = scm_list(method_exp[0].exp.begin()+1,method_exp[0].exp.end());
    std::copy(method_exp.begin()+1,method_exp.end(),method_lambda.begin()+2);
    if(cps_block) {
      // cps transform will wrap what we need in an extra lambda: (lambda (c) (c <procedure-we-desire>))
      auto cps_lambda = generate_fundamental_form_cps(method_lambda);
      return scm_analyze(std::move(cps_lambda[2].exp[1]),false,cps_block);
    } else {
      return scm_analyze(std::move(method_lambda));
    }
  }

  void evaluate_method_and_member_exec_procs(class_prototype& proto, std::vector<scm_string>& property_names, 
                                             std::vector<exe_fcn_t>& property_exec_procs, env_type& env) {
    for(size_type i = 0, n = property_exec_procs.size(); i < n; ++i) {
      auto value = property_exec_procs[i](env);
      if(value.is_type(types::fcn)) {
        proto.method_names.push_back(property_names[i]);
        proto.method_values.push_back(value);
      } else {
        proto.member_names.push_back(property_names[i]);
        proto.member_values.push_back(value);
      }
    }
  }

  // ((set-property! <name> <value>)
  //   (heist:core:oo:set-property! self <name> <value>))
  void define_property_setter(class_prototype& proto, env_type& env) {
    proto.method_names.push_back("set-property!");
    scm_list setter_lambda(3);
    setter_lambda[0] = symconst::lambda;
    setter_lambda[1] = scm_list(2);
    setter_lambda[1].exp[0] = "heist:core:oo:property-name";
    setter_lambda[1].exp[1] = "heist:core:oo:new-value";
    setter_lambda[2] = scm_list(4);
    setter_lambda[2].exp[0] = "heist:core:oo:set-property!";
    setter_lambda[2].exp[1] = "self";
    setter_lambda[2].exp[2] = "heist:core:oo:property-name";
    setter_lambda[2].exp[3] = "heist:core:oo:new-value";
    proto.method_values.push_back(scm_eval(std::move(setter_lambda),env));
  }

  // ((add-property! name value)
  //   (heist:core:oo:add-property! self name value))
  void define_dynamic_property_generator(class_prototype& proto, env_type& env) {
    proto.method_names.push_back("add-property!");
    scm_list property_generator(3);
    property_generator[0] = symconst::lambda;
    property_generator[1] = scm_list(2);
    property_generator[1].exp[0] = "heist:core:property-name";
    property_generator[1].exp[1] = "heist:core:property-value";
    property_generator[2] = scm_list(4);
    property_generator[2].exp[0] = "heist:core:oo:add-property!";
    property_generator[2].exp[1] = "self";
    property_generator[2].exp[2] = "heist:core:property-name";
    property_generator[2].exp[3] = "heist:core:property-value";
    proto.method_values.push_back(scm_eval(std::move(property_generator),env));
  }

  // (define (<class-name>? <obj>)
  //   (if (object? <obj>) 
  //       (eq? <class-name> <obj>.prototype) 
  //       #f))
  void define_class_prototype_predicate(scm_string& class_name, env_type& env) {
    scm_list predicate(3);
    predicate[0] = symconst::define;
    predicate[1] = scm_list(2);
    predicate[1].exp[0] = class_name + '?';
    predicate[1].exp[1] = "heist:core:oo:obj";
    predicate[2] = scm_list(4);
    predicate[2].exp[0] = symconst::if_t;
    predicate[2].exp[1] = scm_list(2);
    predicate[2].exp[1].exp[0] = "object?";
    predicate[2].exp[1].exp[1] = "heist:core:oo:obj";
    predicate[2].exp[2] = scm_list(3);
    predicate[2].exp[2].exp[0] = "eq?";
    predicate[2].exp[2].exp[1] = class_name;
    predicate[2].exp[2].exp[2] = "heist:core:oo:obj.prototype";
    predicate[2].exp[3] = "#f";
    scm_eval(std::move(predicate),env);
  }

  // (define (make-<class-name> . <optional-member-value-container>)
  //   (if (null? <optional-member-value-container>)
  //       (heist:core:oo:make-object <class-name>)
  //       (heist:core:oo:make-object <class-name> (car <optional-member-value-container>))))
  void define_default_prototype_constructor(const scm_string& class_name, env_type& env, const char* name_prefix) {
    scm_list dflt_ctor(3);
    dflt_ctor[0] = symconst::define;
    dflt_ctor[1] = scm_list(3);
    dflt_ctor[1].exp[0] = name_prefix+class_name;
    dflt_ctor[1].exp[1] = symconst::dot;
    dflt_ctor[1].exp[2] = "heist:core:optional-member-value-container";
    dflt_ctor[2] = scm_list(4);
    dflt_ctor[2].exp[0] = symconst::if_t;
    dflt_ctor[2].exp[1] = scm_list(2);
    dflt_ctor[2].exp[1].exp[0] = "null?";
    dflt_ctor[2].exp[1].exp[1] = "heist:core:optional-member-value-container";
    dflt_ctor[2].exp[2] = scm_list(2);
    dflt_ctor[2].exp[2].exp[0] = "heist:core:oo:make-object";
    dflt_ctor[2].exp[2].exp[1] = class_name;
    dflt_ctor[2].exp[3] = scm_list(3);
    dflt_ctor[2].exp[3].exp[0] = "heist:core:oo:make-object";
    dflt_ctor[2].exp[3].exp[1] = class_name;
    dflt_ctor[2].exp[3].exp[2] = scm_list(2);
    dflt_ctor[2].exp[3].exp[2].exp[0] = "car";
    dflt_ctor[2].exp[3].exp[2].exp[1] = "heist:core:optional-member-value-container";
    scm_eval(std::move(dflt_ctor),env);
  }

  // Convert user's custom ctor to be in CPS form as needed
  data generate_CPS_custom_prototype_constructor(scm_list& custom_ctor) {
    // convert the body to CPS notation, then unwrap it from the cps-transform generated lambda 
    // & pop the lambda's continuation to be passed as one of the custom ctor's params instead
    scm_list custom_body(custom_ctor.size()-1);
    custom_body[0] = symconst::begin;
    std::move(custom_ctor.begin()+2,custom_ctor.end(),custom_body.begin()+1);
    auto custom_body_cps = generate_fundamental_form_cps(custom_body); // (lambda (c) <sought-body>)
    auto continuation_param = custom_body_cps[1].exp[0];
    scm_list custom_ctor_cps(3);
    custom_ctor_cps[0] = std::move(custom_ctor[0]);
    custom_ctor_cps[1] = std::move(custom_ctor[1]);
    custom_ctor_cps[1].exp.push_back(std::move(continuation_param));
    custom_ctor_cps[2] = std::move(custom_body_cps[2]);
    return custom_ctor_cps;
  }

  // (define (make-<class-name> <... CUSTOM CTOR ARGS HERE ...>)
  //   (define self (heist:core:oo:make-object <class-name>))
  //   <... CUSTOM CTOR BODY HERE ...>
  //   self)
  void define_custom_prototype_constructor(const scm_string& class_name, env_type& env, scm_list& ctor_proc,const bool cps_block) {
    scm_list custom_ctor(3+ctor_proc.size());
    custom_ctor[0] = symconst::define;
    custom_ctor[1] = ctor_proc[0].exp;
    custom_ctor[2] = scm_list(3);
    custom_ctor[2].exp[0] = symconst::define;
    custom_ctor[2].exp[1] = "self";
    custom_ctor[2].exp[2] = scm_list(2);
    custom_ctor[2].exp[2].exp[0] = "heist:core:oo:make-object";
    custom_ctor[2].exp[2].exp[1] = class_name;
    std::move(ctor_proc.begin()+1,ctor_proc.end(),custom_ctor.begin()+3);
    *custom_ctor.rbegin() = "self";
    if(cps_block) {
      scm_analyze(generate_CPS_custom_prototype_constructor(custom_ctor),false,cps_block)(env);
    } else {
      scm_eval(std::move(custom_ctor),env);
    }
  }

  // (define make-<class-name>
  //   (fn ((<... CUSTOM CTOR ARGS HERE ...>) 
  //        (define self (heist:core:oo:make-object <class-name>))
  //        <... CUSTOM CTOR BODY HERE ...>
  //        self) ...))
  void define_custom_prototype_fn_constructor(const scm_string& class_name, env_type& env, scm_list& ctor_proc,const bool cps_block) {
    scm_list custom_ctor(3);
    custom_ctor[0] = symconst::define;
    custom_ctor[1] = ctor_proc[0];
    custom_ctor[2] = scm_list(ctor_proc.size());
    custom_ctor[2].exp[0] = symconst::fn;
    for(size_type i = 1, n = ctor_proc.size(); i < n; ++i) {
      custom_ctor[2].exp[i] = scm_list(ctor_proc[i].exp.size()+2);
      custom_ctor[2].exp[i].exp[0] = ctor_proc[i].exp[0];
      custom_ctor[2].exp[i].exp[1] = scm_list(3);
      custom_ctor[2].exp[i].exp[1].exp[0] = symconst::define;
      custom_ctor[2].exp[i].exp[1].exp[1] = "self";
      custom_ctor[2].exp[i].exp[1].exp[2] = scm_list(2);
      custom_ctor[2].exp[i].exp[1].exp[2].exp[0] = "heist:core:oo:make-object";
      custom_ctor[2].exp[i].exp[1].exp[2].exp[1] = class_name;
      std::copy(ctor_proc[i].exp.begin()+1,ctor_proc[i].exp.end(),custom_ctor[2].exp[i].exp.begin()+2);
      custom_ctor[2].exp[i].exp[ctor_proc[i].exp.size()+1] = "self";
    }
    if(cps_block) {
      // convert fn defn to CPS notation as needed
      auto cps_fn_expr = generate_fundamental_form_cps(custom_ctor[2].exp); // (lambda (c) (c <fn-we-want>))
      custom_ctor[2] = std::move(cps_fn_expr[2].exp[1]);
      scm_analyze(std::move(custom_ctor),false,cps_block)(env);
    } else {
      scm_eval(std::move(custom_ctor),env);
    }
  }

  void parse_defclass_expression(scm_list& exp, std::vector<scm_string>& property_names, 
                                                std::vector<exe_fcn_t>& property_exec_procs, 
                                                scm_list& ctor_proc,const bool cps_block) {
    const scm_string ctor_name("make-"+exp[1].sym);
    for(size_type i = 3, n = exp.size(); i < n; ++i) {
      // parse member
      if(exp[i].exp[0].is_type(types::sym)) {
        // parse ctor inline fn (if relevant)
        if(exp[i].exp[0].sym == ctor_name) {
          ctor_proc = exp[i].exp;
        } else {
          validate_unique_property_name(exp,exp[i].exp[0].sym,property_names,"property is already defined");
          property_names.push_back(exp[i].exp[0].sym);
          property_exec_procs.push_back(scm_analyze(data(exp[i].exp[1]),false,cps_block)); // cps transforms already handled!
        }
      // parse method
      } else {
        // extract ctor
        if(exp[i].exp[0].exp[0].sym == ctor_name) {
          ctor_proc = exp[i].exp;
        // regular method
        } else {
          validate_unique_property_name(exp,exp[i].exp[0].exp[0].sym,property_names,"property is already defined");
          property_names.push_back(exp[i].exp[0].exp[0].sym);
          property_exec_procs.push_back(convert_method_to_lambda(exp[i].exp,cps_block));
        }
      }
    }
  }


  // -- CLASS PROTOTYPE GENERATION MAIN
  exe_fcn_t analyze_defclass(scm_list& exp,const bool cps_block=false) {
    validate_defclass(exp);
    class_prototype proto;
    proto.class_name = exp[1].sym;
    // get exec procs for member values & method procedures
    std::vector<scm_string> property_names;
    std::vector<exe_fcn_t> property_exec_procs;
    scm_list ctor_proc;
    parse_defclass_expression(exp,property_names,property_exec_procs,ctor_proc,cps_block);
    return [proto=std::move(proto),property_names=std::move(property_names),
            property_exec_procs=std::move(property_exec_procs),exp=std::move(exp),
            ctor_proc=std::move(ctor_proc),cps_block](env_type& env)mutable{
      proto.defn_env = env;
      // confirm inheriting from class objects & add inherited prototype (if present)
      // NOTE: THIS ORDERING IS IN PART RESPONSIBLE (ALONG WITH SEARCH) FOR LEFT-PREDECENT INHERITANCE
      validate_inherited_entity(proto,exp,env);
      // evaluate member and method values
      evaluate_method_and_member_exec_procs(proto,property_names,property_exec_procs,env);
      // define property setter method
      define_property_setter(proto,env);
      // define dynamic property generator method
      define_dynamic_property_generator(proto,env);
      // define the class prototype
      define_variable(proto.class_name,make_cls(proto),env);
      // generate the ctor
      define_default_prototype_constructor(exp[1].sym,env,"new-"); // default ctor always available
      if(ctor_proc.empty()) define_default_prototype_constructor(exp[1].sym,env,"make-");
      else if(ctor_proc[0].is_type(types::exp)) define_custom_prototype_constructor(exp[1].sym,env,ctor_proc,cps_block);
      else define_custom_prototype_fn_constructor(exp[1].sym,env,ctor_proc,cps_block);
      // define the class predicate
      define_class_prototype_predicate(exp[1].sym,env); // exp[1].sym == class_name
      return GLOBALS::VOID_DATA_OBJECT;
    };
  }

  #undef DEFCLASS_LAYOUT

  /******************************************************************************
  * REPRESENTING TRUE ITERATION: WHILE (DEGRADES TO "DO" IN CPS BLOCKS)
  ******************************************************************************/

  bool is_while(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::while_t);}

  exe_fcn_t analyze_while(scm_list& exp,const bool cps_block=false) {
    // "while"s in cps contexts degrade to "do"s
    if(cps_block) {
      exp[0] = symconst::cps_app_tag;
      exp.insert(exp.begin()+1,"do");
      exp.insert(exp.begin()+2,data(scm_list()));
      if(exp.size() > 3 && exp[3].is_type(types::exp) && !exp[3].exp.empty()) {
        scm_list negated_cond(2);
        negated_cond[0] = "not", negated_cond[1] = exp[3].exp[0];
        exp[3].exp[0] = negated_cond;
      }
      return scm_analyze(std::move(exp),false,true);
    }
    // validate has a condition
    if(exp.size() < 2 || !exp[1].is_type(types::exp) || exp[1].exp.empty())
      THROW_ERR("'while 1st argument isn't a condition/return list!"
        "\n     (while (<condition> <optional-return-expr> ...) <body> ...)" << EXP_ERR(exp));
    // analyze condition & return exprs (if exists, else returns <void>)
    exe_fcn_t return_exe, condition_exe = scm_analyze(data(exp[1].exp[0]));
    if(exp[1].exp.size() > 1) {
      scm_list return_exps(exp[1].exp.size());
      return_exps[0] = symconst::begin;
      std::copy(exp[1].exp.begin()+1,exp[1].exp.end(),return_exps.begin()+1);
      return_exe = scm_analyze(std::move(return_exps));
    } else {
      return_exe = [](env_type&){return GLOBALS::VOID_DATA_OBJECT;};
    }
    // has no body
    if(exp.size() == 2) {
      return [return_exe=std::move(return_exe),condition_exe=std::move(condition_exe)](env_type& env){
        while(condition_exe(env).is_truthy());
        return return_exe(env);
      };
    }
    // has body
    scm_list body_exps(exp.size()-1);
    body_exps[0] = symconst::begin;
    std::copy(exp.begin()+2,exp.end(),body_exps.begin()+1);
    exe_fcn_t body_exe = scm_analyze(std::move(body_exps));
    return [return_exe=std::move(return_exe),condition_exe=std::move(condition_exe),
            body_exe=std::move(body_exe)](env_type& env){
      while(condition_exe(env).is_truthy()) body_exe(env);
      return return_exe(env);
    };
  }

  /******************************************************************************
  * REPRESENTING QUASIQUOTE, UNQUOTE, UNQUOTE-SPLICING
  ******************************************************************************/

  bool is_quasiquote(const scm_list& exp)      noexcept{return is_tagged_list(exp,symconst::quasiquote);}
  bool is_unquote(const scm_list& exp)         noexcept{return is_tagged_list(exp,symconst::unquote);}
  bool is_unquote_splicing(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::unquo_splice);}

  // Returns a data object containing the unquoted data. If the data is an 
  //   expression, it ought to be analyzed by scm_analyze in order to convert 
  //   its token strings into scheme code objects
  data process_unquoted(const scm_list& exp) {
    if(exp.size() != 2)
      THROW_ERR("'unquote expects one argument: (unquote <expression>)"
        << EXP_ERR(exp));
    return exp[1];
  }


  // Requotes data if symbolic
  data unquote_splicing_atom(const data& d)noexcept{
    if(d.is_type(types::sym)) {
      scm_list quoted_sym(2);
      quoted_sym[0] = symconst::quote, quoted_sym[1] = d;
      return data(quoted_sym);
    }
    return d;
  }


  // Confirm whether 'd' is the AST's empty-list representation
  bool data_is_the_AST_empty_list(const data& d)noexcept{
    return d.is_type(types::exp) && d.exp.size() == 2 && 
           d.exp[0].is_type(types::sym) && 
           d.exp[0].sym == symconst::quote && 
           d.exp[1].is_type(types::sym) && 
           d.exp[1].sym == symconst::emptylist;
  }


  // Expands the given pair into a scm expression. 
  //   => Helper fcn for processing ,@
  //   => Returns whether pair sequence was NOT null-terminated.
  // PRECONDITION: 'pair_object' MUST BE ACYCLIC
  bool expand_list_into_exp(par_type& pair_object,scm_list& exp)noexcept{
    // unpack car
    exp.push_back(unquote_splicing_atom(pair_object->first));
    // unpack cdr
    if(pair_object->second.is_type(types::par))
      return expand_list_into_exp(pair_object->second.par, exp);
    // don't push the terminating '() if at the end of the list/pair-sequence
    else if(is_not_THE_EMPTY_LIST(pair_object->second)) {
      exp.push_back(unquote_splicing_atom(pair_object->second));
      return true;
    }
    return false;
  }


  // Returns an expression object containing the unquoted list/vector's elements.
  //   => Returns whether the expanded sequence was NOT null-terminated.
  enum class unsplice_status {list_star, list, atom};
  unsplice_status process_unquote_splicing(const scm_list& exp, env_type& env, scm_list& spliceable_data) {
    if(exp.size() != 2)
      THROW_ERR("'unquote-splicing didn't receive 1 arg!"
        "\n     (unquote-splicing <spliceable-expression>)"
        << EXP_ERR(exp));
    data eval_result = scm_eval(data(exp[1]),env);
    // if splicing a non-pair
    if(!eval_result.is_type(types::par)) {
      spliceable_data.push_back(unquote_splicing_atom(eval_result)); // add as-is
      return unsplice_status::atom;                                  // trigger 'append'
    }
    // confirm an acyclic pair sequence otherwise
    if(primitive_list_is_acyclic_and_null_terminated(eval_result) == list_status::cyclic)
      THROW_ERR("'unquote-splicing "<<PROFILE(eval_result)<<" can't be spliced in with a cycle!"<<EXP_ERR(exp));
    return expand_list_into_exp(eval_result.par,spliceable_data) ? unsplice_status::list_star : unsplice_status::list;
  }


  // Handle appending 'atomic' or 'list_star' data to a quasiquote expression
  scm_list quasiquote_append_non_list(scm_list& spliceable_data, scm_list& quote_val,
                                      const scm_list& exp,        const bool& is_dotted_list, 
                                      const bool& quoting_vector, const bool& quoting_hmap, 
                                      const bool& not_last_elt) {
    static constexpr const char * const bad_vector = 
      "'quasiquote can't append [via ,@] an improper list to a vector!\n     Tried to splice in: ";
    static constexpr const char * const bad_hmap = 
      "'quasiquote can't append [via ,@] an improper list to an hmap!\n     Tried to splice in: ";
    static constexpr const char * const mid_splice = 
      "'quasiquote can't splice [via ,@] an improper list into the middle of a list!\n     Tried to splice in: ";
    // confirm not splicing a list_star/atomic into a vector nor mid-list
    if(quoting_vector) {
      if(is_dotted_list) THROW_ERR(bad_vector<<"(list_star "<<spliceable_data[0]<<' '<<spliceable_data[1]<<')'<<EXP_ERR(exp));
      THROW_ERR(bad_vector<<spliceable_data[0]<<" of type \""<<spliceable_data[0].type_name()<<'"'<<EXP_ERR(exp));
    }
    if(quoting_hmap) {
      if(is_dotted_list) THROW_ERR(bad_hmap<<"(list_star "<<spliceable_data[0]<<' '<<spliceable_data[1]<<')'<<EXP_ERR(exp));
      THROW_ERR(bad_hmap<<spliceable_data[0]<<" of type \""<<spliceable_data[0].type_name()<<'"'<<EXP_ERR(exp));
    }
    if(not_last_elt && is_dotted_list)
      THROW_ERR(mid_splice<<"(list_star "<<spliceable_data[0]<<' '<<spliceable_data[1]<<')'<<EXP_ERR(exp));
    if(not_last_elt)
      THROW_ERR(mid_splice<<spliceable_data[0]<<" of type \""<<spliceable_data[0].type_name()<<'"'<<EXP_ERR(exp));
    // return the current expression as an 'append' to the quasiquote expression
    scm_list appended_exp(3);
    appended_exp[0] = symconst::append, appended_exp[1] = quote_val;
    if(is_dotted_list) {
      spliceable_data.insert(spliceable_data.begin(), symconst::list_star);
      appended_exp[2] = spliceable_data;
      return appended_exp;
    }
    appended_exp[2] = spliceable_data[0];
    return appended_exp;
  }


  // Tag <quote_val> w/ 'vector 'list* or 'list as per <quoted_exp>
  void tag_quote_val(scm_list& quoted_exp, scm_list& quote_val, const scm_list& exp) {
    const bool is_vector      = is_vector_literal(quoted_exp);
    const bool is_hmap        = is_hmap_literal(quoted_exp);
    const bool is_dotted_list = is_quoted_cons(quoted_exp,symconst::quasiquote);
    if(is_dotted_list && (is_hmap || is_vector)) {
      if(is_vector) THROW_ERR("'quasiquote found unexpected dot ("<<G.dot<<") in 'vector-literal! -- ANALYZE_QUOTE_VECTOR"<<EXP_ERR(exp));
      THROW_ERR("'quasiquote found unexpected dot ("<<G.dot<<") in 'hmap-literal! -- ANALYZE_QUOTE_HMAP"<<EXP_ERR(exp));
    }
    if(is_vector) {
      quoted_exp.erase(quoted_exp.begin(),quoted_exp.begin()+1); // erase 'vector-literal tag
      quote_val.push_back(symconst::vector);
    } else if(is_hmap) {
      quoted_exp.erase(quoted_exp.begin(),quoted_exp.begin()+1); // erase 'hmap-literal tag
      quote_val.push_back(symconst::hmap);
    } else if(is_dotted_list) {
      quote_val.push_back(symconst::list_star);
    } else {
      quote_val.push_back(symconst::list);
    }
  }


  // Recursively unquotes/unquote-splices data as needed w/in quasiquote templates.
  // Also quotes each piece of data as needed.
  void unquote_quasiquote_template(scm_list& quote_val, scm_list& quoted_exp, env_type& env, 
                                   const scm_list& exp, const size_type& nested_level){
    // Account for whether splicing into a vector/hmap
    bool quoting_a_vector = (quote_val[0].sym == symconst::vector);
    bool quoting_an_hmap  = (quote_val[0].sym == symconst::hmap);
    
    // Wrap 'quote' around each obj in the expression, and expand unquote/unquote-splicing instances
    for(size_type i = 0, n = quoted_exp.size(); i < n; ++i) {
      // if quoting an empty expression, push the empty list 
      if(quoted_exp[i].is_type(types::exp) && quoted_exp[i].exp.empty()) {
        quote_val.push_back(scm_list(2));
        quote_val.rbegin()->exp[0] = symconst::quote;
        quote_val.rbegin()->exp[1] = symconst::emptylist;


      // UNQUOTE
      } else if(quoted_exp[i].is_type(types::exp) && is_unquote(quoted_exp[i].exp)) {
        if(!nested_level) { // if , add the data as-is
          quote_val.push_back(quoted_exp[i].exp[1]);
        } else { // in a nested quasiquote, recursively parse the unquote expression 1 level removed
          scm_list inner_exp;
          tag_quote_val(quoted_exp[i].exp, inner_exp, exp);
          unquote_quasiquote_template(inner_exp, quoted_exp[i].exp, env, exp, nested_level - 1);
          quote_val.push_back(inner_exp);
        }
      

      // UNQUOTE-SPLICING      
      } else if(quoted_exp[i].is_type(types::exp) && is_unquote_splicing(quoted_exp[i].exp)) {
        if(!nested_level) { // if ,@ eval the expression & splice in the resulting list's elts
          scm_list spliceable_data;
          auto unsplice_stat = process_unquote_splicing(quoted_exp[i].exp, env, spliceable_data);
          // If splicing the empty list, continue (does nothing)
          if(unsplice_stat == unsplice_status::atom && data_is_the_AST_empty_list(spliceable_data[0]))
            continue;
          // If splicing in a list_star or atom
          if(unsplice_stat == unsplice_status::list_star || unsplice_stat == unsplice_status::atom) {
            quote_val = quasiquote_append_non_list(spliceable_data, quote_val, exp,
                        (unsplice_stat == unsplice_status::list_star), quoting_a_vector, quoting_an_hmap, (i != n-1));
            return;
          // Otherwise (splicing a list), splice in data as-is
          } else {
            quote_val.insert(quote_val.end(), spliceable_data.begin(), spliceable_data.end());
          }
        } else { // in a nested quasiquote, recursively parse the unquote-splicing expression 1 level removed
          scm_list inner_exp;
          tag_quote_val(quoted_exp[i].exp, inner_exp, exp);
          unquote_quasiquote_template(inner_exp, quoted_exp[i].exp, env, exp, nested_level - 1);
          quote_val.push_back(inner_exp);
        }
        
        
      // else, quote the nested data
      } else {
        // if other expression, also recursively analyze for unquotes
        if(quoted_exp[i].is_type(types::exp)) {
          scm_list inner_exp;
          // tag <inner_exp> as needed w/ either 'vector, 'list_star, or 'list
          auto nested_expression_level = is_quasiquote(quoted_exp[i].exp) + nested_level;
          tag_quote_val(quoted_exp[i].exp, inner_exp, exp);
          // propagate quotations throughout the sub-expression
          unquote_quasiquote_template(inner_exp, quoted_exp[i].exp, env, exp, nested_expression_level);
          quote_val.push_back(inner_exp);
        

        // if atomic, simply quote the atom
        } else {
          quote_val.push_back(scm_list(2));
          quote_val.rbegin()->exp[0] = symconst::quote;
          quote_val.rbegin()->exp[1] = quoted_exp[i];
        }
      }
    }
  }


  // Works similarly to 'analyze_quoted', except that quoted 'unquote's are
  //   (wait for it) unquoted (BAYUM), and quoted 'unquote-splicing's are both
  //   unquoted & expanded.
  exe_fcn_t analyze_quasiquote(scm_list& exp,const bool cps_block=false) {
    if(exp.size() != 2)
      THROW_ERR("'quasiquote expects one argument: (quasiquote <expression>)"<<EXP_ERR(exp));
    // Get quasiquoted data
    auto quoted_data = text_of_quotation(exp);
    
    // If quasiquoted data is atomic, return as-is
    if(!quoted_data.is_type(types::exp)) {
      if(cps_block) return scm_analyze(generate_fundamental_form_cps(quoted_data),false,cps_block);
      return [unquoted_exp=std::move(quoted_data)](env_type&){return unquoted_exp;};
    }

    // If quoting an empty expression, return the empty list
    if(quoted_data.exp.empty()) {
      if(cps_block) {
        data quote_expr(scm_list(2));
        quote_expr.exp[0] = symconst::quote, quote_expr.exp[1] = scm_list();
        return scm_analyze(generate_fundamental_form_cps(quote_expr),false,cps_block);
      }
      return [](env_type&){return symconst::emptylist;};
    }

    // If quasiquoted an unquote, unpack its data
    if(is_unquote(quoted_data.exp)) {
      data unquoted_data = process_unquoted(quoted_data.exp);
      // return the exec proc of its evaluation
      if(cps_block) return scm_analyze(generate_fundamental_form_cps(unquoted_data),false,cps_block);
      return scm_analyze(std::move(unquoted_data));
    }

    // If quasiquoted an expression, expand such into a list/list_star of quoted data
    return [quoted_exp=std::move(quoted_data.exp),exp=std::move(exp),cps_block=std::move(cps_block)]
      (env_type& env) {
        // Unquote/Splice-In data as needed throughout the quasiquote template
        scm_list quote_val, mutable_quoted_exp = quoted_exp;
        // tag <quote_val> as needed w/ either 'vector, 'list_star, or 'list
        tag_quote_val(mutable_quoted_exp, quote_val, exp);
        // propagate quotations throughout the sub-expression
        unquote_quasiquote_template(quote_val,mutable_quoted_exp,env,exp,0);
        if(cps_block) return scm_analyze(generate_fundamental_form_cps(quote_val),false,true)(env);
        return scm_eval(std::move(quote_val),env);
      };
  }

  /******************************************************************************
  * REPRESENTING infix! infixr! unfix! SPECIAL FORMS: READER MANIPULATION
  ******************************************************************************/

  bool is_infix(const scm_list& exp)noexcept {return is_tagged_list(exp,symconst::infix);}
  bool is_infixr(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::infixr);}
  bool is_unfix(const scm_list& exp)noexcept {return is_tagged_list(exp,symconst::unfix);}

  void confirm_valid_infix_infixr_unfix_syntax(scm_list& exp, const char* name) {
    if(exp.size() < 2)
      THROW_ERR('\''<<name<<" didn't receive enough arguments!"
        "\n     ("<<name<<" <precedence-level-integer-literal> <symbol> ...)"
        "\n     ("<<name<<" <symbol> ...)" 
        "\n     <precedence-level> range: ["<<LLONG_MIN<<','<<LLONG_MAX<<']'
        << EXP_ERR(exp));
    size_type symbols_start_idx = 1 + exp[1].is_type(types::num);
    if(symbols_start_idx == 2 && !exp[1].num.is_integer())
      THROW_ERR('\''<<name<<" precedence level isn't an integer!"
        "\n     ("<<name<<" <precedence-level-integer-literal> <symbol> ...)"
        "\n     ("<<name<<" <symbol> ...)" 
        "\n     <precedence-level> range: ["<<LLONG_MIN<<','<<LLONG_MAX<<']'
        << EXP_ERR(exp));
    for(size_type n = exp.size(); symbols_start_idx < n; ++symbols_start_idx)
      if(!exp[symbols_start_idx].is_type(types::sym))
        THROW_ERR('\''<<name<<" argument #"<<symbols_start_idx+1<<", "<<PROFILE(exp[symbols_start_idx])
          << ", isn't a symbol!\n     ("<<name<<" <precedence-level-integer-literal> <symbol> ...)"
             "\n     ("<<name<<" <symbol> ...)" 
             "\n     <precedence-level> range: ["<<LLONG_MIN<<','<<LLONG_MAX<<']'
             << EXP_ERR(exp));
  }

  void remove_preexisting_operators_from_table(const scm_list& exp, const int symbol_offset)noexcept{
    const size_type n = exp.size();
    for(auto& prec_level : G.INFIX_TABLE)
      for(size_type i = 0; i < prec_level.second.size(); ++i)
        for(size_type j = symbol_offset; j < n; ++j)
          if(prec_level.second[i].second == exp[j].sym) {
            prec_level.second.erase(prec_level.second.begin()+i--);
            break;
          }
  }

  // redefines operators iff already defined
  exe_fcn_t register_infix_operators(const scm_list& exp,const char* name,bool is_left_assoc) {
    if(exp.size() < 3)
      THROW_ERR('\''<<name<<" didn't receive enough arguments!"
        "\n     ("<<name<<" <precedence-level-integer-literal> <symbol> ...)"
        "\n     ("<<name<<" <symbol> ...)" 
        "\n     <precedence-level> range: ["<<LLONG_MIN<<','<<LLONG_MAX<<']'
        << EXP_ERR(exp));
    remove_preexisting_operators_from_table(exp,2);
    long long level = exp[1].num.extract_inexact();
    for(size_type i = 2, n = exp.size(); i < n; ++i)
      G.INFIX_TABLE[level].push_back(std::make_pair(is_left_assoc,exp[i].sym));
    return [](env_type&){return GLOBALS::VOID_DATA_OBJECT;};
  }

  // returns either #f or the precedence level of the symbols
  exe_fcn_t seek_infix_operators(scm_list& exp,bool is_left_assoc)noexcept{
    const size_type n = exp.size();
    for(const auto& prec_level : G.INFIX_TABLE) {
      bool found = false;
      for(size_type j = 1; j < n; ++j) {
        if(std::find(prec_level.second.begin(),prec_level.second.end(),std::make_pair(is_left_assoc,exp[j].sym)) != prec_level.second.end()) {
          if(j == 1) {
            found = true;
          } else if(!found) {
            return [](env_type&){return GLOBALS::FALSE_DATA_BOOLEAN;};
          }
        } else if(found) {
          return [](env_type&){return GLOBALS::FALSE_DATA_BOOLEAN;};
        }
      }
      if(found) return [idx=prec_level.first](env_type&){return num_type(idx);};
    }
    return [](env_type&){return GLOBALS::FALSE_DATA_BOOLEAN;};
  }

  exe_fcn_t analyze_infix(scm_list& exp){
    confirm_valid_infix_infixr_unfix_syntax(exp,symconst::infix);
    if(exp[1].is_type(types::num))
      return register_infix_operators(exp,symconst::infix,true);
    return seek_infix_operators(exp,true);
  }

  exe_fcn_t analyze_infixr(scm_list& exp){
    confirm_valid_infix_infixr_unfix_syntax(exp,symconst::infixr);
    if(exp[1].is_type(types::num))
      return register_infix_operators(exp,symconst::infixr,false);
    return seek_infix_operators(exp,false);
  }

  exe_fcn_t analyze_unfix(scm_list& exp){
    if(exp.size() < 2)
      THROW_ERR("'unfix! didn't receive enough arguments!\n     (unfix! <symbol> ...)"<<EXP_ERR(exp));
    for(size_type i = 1, n = exp.size(); i < n; ++i)
      if(!exp[i].is_type(types::sym))
        THROW_ERR("'unfix! argument #"<<i+1<<", "<<PROFILE(exp[i])<< ", isn't a symbol!"
          "\n     (unfix! <symbol> ...)"<<EXP_ERR(exp));
    remove_preexisting_operators_from_table(exp,1);
    return [](env_type&){return GLOBALS::VOID_DATA_OBJECT;};
  }

  /******************************************************************************
  * REPRESENTING defined? SPECIAL FORM: VARS & OBJECT-PROPERTY-ACCESS
  ******************************************************************************/

  bool is_definedp(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::definedp);}


  namespace undefined_determination_helpers {
    // (string-split <call> ".")
    bool parse_object_property_chain_sequence(const scm_string& call, std::vector<scm_string>& chain)noexcept{
      chain.push_back("");
      for(const auto& ch : call) {
        if(ch == '.') chain.push_back("");
        else          *chain.rbegin() += ch;
      }
      // verify no ".." found or ".<call>" or "<call>."
      for(const auto& link : chain) if(link.empty()) return false;
      return true;
    }

    // Returns whether found <sought_property> in <proto> or its inherited prototype
    bool verify_in_prototype_and_inherited_properties(cls_type& proto, const scm_string& sought_property, bool& is_member, obj_type& obj)noexcept{
      bool verify_value_in_local_object(obj_type&,const scm_string&,bool&)noexcept;
      // Search the prototype
      for(size_type i = 0, n = proto->member_names.size(); i < n; ++i)
        if(proto->member_names[i] == sought_property) {
          // cache accessed inherited member
          obj->member_names.push_back(sought_property), obj->member_values.push_back(proto->member_values[i]);
          is_member = true;
          return true;
        }
      for(size_type i = 0, n = proto->method_names.size(); i < n; ++i)
        if(proto->method_names[i] == sought_property) {
          is_member = false;
          return true;
        }
      // Search the inherited prototypes (& in turn their inherited prototypes as well)
      return proto->super && obj->super && verify_value_in_local_object(obj->super,sought_property,is_member);
    }

    // Returns whether found <property> as a member/method in <obj> 
    // If returns true, <property> value is in <obj> & <is_member> denotes whether a member or method
    bool verify_value_in_local_object(obj_type& obj, const scm_string& property, bool& is_member)noexcept{
      auto& members = obj->member_names;
      // Seek members
      for(size_type i = 0, n = members.size(); i < n; ++i)
        if(members[i] == property) {
          is_member = true;
          return true;
        }
      // Seek methods
      auto& methods = obj->method_names;
      for(size_type i = 0, n = methods.size(); i < n; ++i)
        if(methods[i] == property) {
          is_member = false;
          return true;
        }
      // Seek proto & its inherited prototype
      // => IF FOUND, ADD IT TO THE LOCAL OBJECT INSTANCE
      return verify_in_prototype_and_inherited_properties(obj->proto,property,is_member,obj);
    }

    // Returns the ultimate value of the call-chain
    bool property_chain_is_undefined(scm_string&& call, env_type& env)noexcept{
      // split the call chain into object series
      std::vector<scm_string> chain;
      if(!parse_object_property_chain_sequence(call,chain)) return true;
      // get the first object instance
      if(!env->has_variable(chain[0])) return true;
      data value = lookup_variable_value(chain[0],env);
      // get the call value
      for(size_type i = 1, n = chain.size(); i < n; ++i) {
        if(!value.is_type(types::obj)) return true;
        // Search local members & methods, the proto, and the proto inheritances
        bool is_member = false;
        if(!verify_value_in_local_object(value.obj,chain[i],is_member)) return true;
        // if found a method, confirm at the last item in the call chain
        if(!is_member && i+1 < n) return true;
      }
      return false;
    }
  }; // End of namespace undefined_determination_helpers


  // NOTE: USE runtime-syntax? core-syntax? reader-syntax? TO CHECK MACROS !!!
  exe_fcn_t analyze_definedp(scm_list& exp) {
    if(exp.size() != 2)
      THROW_ERR("'defined? didn't receive 1 argument!\n     (defined? <symbol>)"<<EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'defined? arg "<<PROFILE(exp[1])<<" isn't a symbol!\n     (defined? <symbol>)"<<EXP_ERR(exp));
    // Check if non-member-access symbol is defined in the environment
    if(!symbol_is_property_chain_access(exp[1].sym))
      return [variable=std::move(exp[1].sym)](env_type& env){
        return boolean(env->has_variable(variable));
      };
    // Check if member-access chain is defined in the environment
    return [variable=std::move(exp[1].sym)](env_type& env)mutable{
      return boolean(!undefined_determination_helpers::property_chain_is_undefined(std::move(variable),env));
    };
  }

  /******************************************************************************
  * REPRESENTING delete! SPECIAL FORM: DELETE VARS & OBJECT-PROPERTY-ACCESS
  ******************************************************************************/

  bool is_delete(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::delete_bang);}

  namespace delete_variable_helpers {
    void delete_property_in_local_object(obj_type& obj, const scm_string& property)noexcept{
      auto& members = obj->member_names;
      // Seek members
      for(size_type i = 0, n = members.size(); i < n; ++i)
        if(members[i] == property) {
          obj->member_names.erase(obj->member_names.begin()+i);
          obj->member_values.erase(obj->member_values.begin()+i);
          return;
        }
      // Seek methods
      auto& methods = obj->method_names;
      for(size_type i = 0, n = methods.size(); i < n; ++i)
        if(methods[i] == property) {
          obj->method_names.erase(obj->method_names.begin()+i);
          obj->method_values.erase(obj->method_values.begin()+i);
          return;
        }
      // Seek in inherited
      if(obj->super) delete_property_in_local_object(obj->super, property);
    }

    // Returns the ultimate value of the call-chain
    void delete_property_chain_if_exists(scm_string&& call, env_type& env)noexcept{
      // split the call chain into object series
      std::vector<scm_string> chain;
      if(!undefined_determination_helpers::parse_object_property_chain_sequence(call,chain)) return;
      // get the first object instance
      if(!env->has_variable(chain[0])) return;
      data value = lookup_variable_value(chain[0],env);
      // get the call value
      for(size_type i = 1, n = chain.size(); i < n; ++i) {
        if(!value.is_type(types::obj)) return;
        // Delete value if at last point in property chain
        if(i+1 == n) {
          delete_property_in_local_object(value.obj,chain[i]);
          return;
        }
        // Search local members & methods, the proto, and the proto inheritances
        bool is_member = false;
        if(!undefined_determination_helpers::verify_value_in_local_object(value.obj,chain[i],is_member)) return;
        // if found a method, confirm at the last item in the call chain
        if(!is_member && i+1 < n) return;
      }
    }
  } // End of namesapce delete_variable_helpers


  exe_fcn_t analyze_delete(scm_list& exp) {
    if(exp.size() != 2)
      THROW_ERR("'delete! didn't receive 1 argument!\n     (delete! <symbol>)"<<EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'delete! arg "<<PROFILE(exp[1])<<" isn't a symbol!\n     (delete! <symbol>)"<<EXP_ERR(exp));
    // Check if non-member-access symbol is defined in the environment
    if(!symbol_is_property_chain_access(exp[1].sym))
      return [variable=std::move(exp[1].sym)](env_type& env){
        env->erase_variable(variable);
        return GLOBALS::VOID_DATA_OBJECT;
      };
    // Check if member-access chain is defined in the environment
    return [variable=std::move(exp[1].sym)](env_type& env)mutable{
      delete_variable_helpers::delete_property_chain_if_exists(std::move(variable),env);
      return GLOBALS::VOID_DATA_OBJECT;
    };
  }

  /******************************************************************************
  * CONTINUATION PASSING STYLE CORE SYNTAX EXPANSION HELPERS
  ******************************************************************************/

  // Expand data's core syntax macros
  data cps_recursively_deep_expand_core_macros(const data& d) {
    return recursively_deep_expand_syntax_macros(d,G.GLOBAL_ENVIRONMENT_POINTER,true);
  }

  /******************************************************************************
  * CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 1
  ******************************************************************************/

  // Whether <cps_exp> contains <sym>
  bool CPS_exp_contains_symbol(const scm_list& cps_exp,const sym_type& sym)noexcept{
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
      if((cps_exp[i].is_type(types::exp) && CPS_exp_contains_symbol(cps_exp[i].exp,sym)) ||
         (cps_exp[i].is_type(types::sym) && cps_exp[i].sym == sym))
        return true;
    return false;
  }


  // Is a lambda of 1 arg
  bool is_unary_arg_lambda_cps_exp(const scm_list& cps_exp)noexcept{
    return cps_exp.size() > 2 && is_tagged_list(cps_exp,symconst::lambda) && 
           cps_exp[1].is_type(types::exp) && cps_exp[1].exp.size() == 1 && 
           cps_exp[1].exp[0].is_type(types::sym);
  }


  // Optimizable (pass 1) CPS lambda
  bool is_optimizable_CPS_pass_1_exp(const scm_list& cps_exp)noexcept{
    return cps_exp.size() == 3 && is_unary_arg_lambda_cps_exp(cps_exp) && 
           cps_exp[2].is_type(types::exp) && cps_exp[2].exp.size() == 2 &&
           cps_exp[2].exp[0].is_type(types::exp) && cps_exp[2].exp[1].is_type(types::sym) &&
           cps_exp[2].exp[1].sym == cps_exp[1].exp[0].sym &&
           !CPS_exp_contains_symbol(cps_exp[2].exp[0].exp,cps_exp[2].exp[1].sym);
  }


  // (lambda (a) (<expression-w/o-a> a)) => <expression-w/o-a>
  void CPS_lambda_unwrapping_optimization_pass_1(scm_list& cps_exp)noexcept{
    if(is_optimizable_CPS_pass_1_exp(cps_exp)) {
      auto temp = cps_exp[2].exp[0].exp;
      cps_exp = temp;
      CPS_lambda_unwrapping_optimization_pass_1(cps_exp);
    } else {
      for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
        if(cps_exp[i].is_type(types::exp))
          CPS_lambda_unwrapping_optimization_pass_1(cps_exp[i].exp);
    }
  }

  /******************************************************************************
  * CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 2
  ******************************************************************************/

  bool data_is_continuation_parameter(const data&)noexcept;

  void replace_all_instances_of_symB_with_symA(scm_list& cps_exp,const sym_type& symA,
                                                                 const sym_type& symB)noexcept{
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i) {
      if(cps_exp[i].is_type(types::exp))
        replace_all_instances_of_symB_with_symA(cps_exp[i].exp,symA,symB);
      else if(cps_exp[i].is_type(types::sym) && cps_exp[i].sym == symB)
        cps_exp[i].sym = symA;
    }
  }


  // Optimizable (pass 2) CPS lambda
  bool is_optimizable_CPS_pass_2_exp(const scm_list& cps_exp)noexcept{
    return cps_exp.size() == 2 && cps_exp[0].is_type(types::exp) && 
           data_is_continuation_parameter(cps_exp[1]) && 
           is_unary_arg_lambda_cps_exp(cps_exp[0].exp) && cps_exp[0].exp.size() == 3 &&
           cps_exp[0].exp[2].is_type(types::exp) &&
           data_is_continuation_parameter(cps_exp[0].exp[1].exp[0]) &&
           !CPS_exp_contains_symbol(cps_exp[0].exp,cps_exp[1].sym);
  }


  // ((lambda (b) <exp-w/o-a>) a) => <exp-w/o-a>,
  // 1. With each reference to <b> replaced by <a>
  // 2. Iff both <a> & <b> are continuations
  void CPS_lambda_unwrapping_optimization_pass_2(scm_list& cps_exp)noexcept{
    if(is_optimizable_CPS_pass_2_exp(cps_exp)) {
      const auto a_param = cps_exp[1].sym, b_param = cps_exp[0].exp[1].exp[0].sym;
      auto temp = cps_exp[0].exp[2].exp;
      cps_exp = temp;
      replace_all_instances_of_symB_with_symA(cps_exp,a_param,b_param);
      CPS_lambda_unwrapping_optimization_pass_2(cps_exp);
    } else {
      for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
        if(cps_exp[i].is_type(types::exp))
          CPS_lambda_unwrapping_optimization_pass_2(cps_exp[i].exp);
    }
  }

  /******************************************************************************
  * CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 3
  ******************************************************************************/

  // Replaces the 1 instance of <symB> w/ <objA>
  bool replace_instance_of_symB_with_objA(scm_list& cps_exp,const data& objA,
                                                            const sym_type& symB)noexcept{
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
      if(cps_exp[i].is_type(types::exp) && replace_instance_of_symB_with_objA(cps_exp[i].exp,objA,symB)) {
        return true;
      } else if(cps_exp[i].is_type(types::sym) && cps_exp[i].sym == symB) {
        cps_exp[i] = objA;
        return true;
      }
    return false;
  }


  // counts instances of <sym> w/in <cps_exp>
  void CPS_exp_count_instances_of_symbol(const scm_list& cps_exp,const sym_type& sym,size_type& count)noexcept{
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
      if(cps_exp[i].is_type(types::exp)) {
        CPS_exp_count_instances_of_symbol(cps_exp[i].exp,sym,count);
        if(count > 2) return;
      } else if(cps_exp[i].is_type(types::sym) && cps_exp[i].sym == sym) {
        if(++count > 2) return;
      }
  }


  // Optimizable (pass 3) CPS lambda
  bool is_optimizable_CPS_pass_3_exp(const scm_list& cps_exp)noexcept{
    if(cps_exp.size() == 2 && cps_exp[0].is_type(types::exp) && 
      is_unary_arg_lambda_cps_exp(cps_exp[0].exp) && cps_exp[0].exp.size() == 3 &&
      cps_exp[0].exp[2].is_type(types::exp) && data_is_continuation_parameter(cps_exp[0].exp[1].exp[0])) {
        size_type count = 0;
        CPS_exp_count_instances_of_symbol(cps_exp[0].exp[2].exp,cps_exp[0].exp[1].exp[0].sym,count);
        return count < 2;
      }
    return false;
  }


  // ((lambda (k) <exp-w/-only-1-instance-of-k>) <obj>) => <exp-w/-only-1-instance-of-k> 
  // 1. <k> is a continuation
  // 2. That 1 instance of <k> is replaced w/ <obj>
  void CPS_lambda_unwrapping_optimization_pass_3(scm_list& cps_exp)noexcept{
    if(is_optimizable_CPS_pass_3_exp(cps_exp)) {
      const data a_obj = cps_exp[1];
      const auto b_param = cps_exp[0].exp[1].exp[0].sym;
      auto temp = cps_exp[0].exp[2].exp;
      cps_exp = temp;
      replace_instance_of_symB_with_objA(cps_exp,a_obj,b_param);
      CPS_lambda_unwrapping_optimization_pass_3(cps_exp);
    } else {
      for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
        if(cps_exp[i].is_type(types::exp))
          CPS_lambda_unwrapping_optimization_pass_3(cps_exp[i].exp);
    }
  }

  /******************************************************************************
  * CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 4
  ******************************************************************************/

  // Expand the symconst::cps_ignore_arg lambda application
  void expand_CPS_lambda_pass_4_application(scm_list& cps_exp,const size_type& i)noexcept{
    scm_list unwrapped_exp(cps_exp[i].exp[0].exp.size()-1);
    unwrapped_exp[0] = cps_exp[i].exp[1];
    std::move(cps_exp[i].exp[0].exp.begin()+2,cps_exp[i].exp[0].exp.end(),unwrapped_exp.begin()+1);
    cps_exp.erase(cps_exp.begin()+i); // erase optimized lambda application
    cps_exp.insert(cps_exp.begin()+i, // insert expanded application
                   std::make_move_iterator(unwrapped_exp.begin()),
                   std::make_move_iterator(unwrapped_exp.end())); 
  }


  // Optimizable (pass 4) CPS lambda
  bool is_optimizable_CPS_pass_4_exp(const scm_list& cps_exp)noexcept{
    return cps_exp.size() == 2 && cps_exp[0].is_type(types::exp) &&
           is_unary_arg_lambda_cps_exp(cps_exp[0].exp) &&
           cps_exp[0].exp[1].exp[0].is_type(types::sym) &&
           cps_exp[0].exp[1].exp[0].sym == symconst::cps_ignore_arg;
  }


  // ((lambda (ignore) <exp> ...) <obj>) => <obj> <exp> ...
  void CPS_lambda_unwrapping_optimization_pass_4(scm_list& cps_exp)noexcept{
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i) {
      if(cps_exp[i].is_type(types::exp)) {
        if(is_optimizable_CPS_pass_4_exp(cps_exp[i].exp)) {
          expand_CPS_lambda_pass_4_application(cps_exp,i);
          CPS_lambda_unwrapping_optimization_pass_4(cps_exp);
          return;
        }
        CPS_lambda_unwrapping_optimization_pass_4(cps_exp[i].exp);
      }
    }
  }

  /******************************************************************************
  * CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 5
  ******************************************************************************/

  // Revert the "lambda-set!" definition transformation
  void expand_CPS_lambda_pass_5_definition(scm_list& cps_exp,const size_type& i)noexcept{
    scm_list unwrapped_exp(cps_exp[i].exp[0].exp.size()-2);
    std::move(cps_exp[i].exp[0].exp.begin()+2,cps_exp[i].exp[0].exp.end(),unwrapped_exp.begin());
    unwrapped_exp[0].exp[0].sym = symconst::define;
    cps_exp.erase(cps_exp.begin()+i); // erase optimized lambda application
    cps_exp.insert(cps_exp.begin()+i, // insert expanded application
                   std::make_move_iterator(unwrapped_exp.begin()),
                   std::make_move_iterator(unwrapped_exp.end()));
  }


  // Optimizable (pass 5) CPS lambda
  bool is_optimizable_CPS_pass_5_exp(const scm_list& cps_exp)noexcept{
    return cps_exp.size() == 2 && cps_exp[0].is_type(types::exp) &&
           is_unary_arg_lambda_cps_exp(cps_exp[0].exp) &&
           ((cps_exp[1].is_type(types::bol) && !cps_exp[1].bol.val) || 
            (cps_exp[1].is_type(types::sym) && cps_exp[1].sym == symconst::false_t)) && 
           cps_exp[0].exp[2].is_type(types::exp) && cps_exp[0].exp[2].exp.size() == 3 && 
           cps_exp[0].exp[2].exp[0].is_type(types::sym) && cps_exp[0].exp[2].exp[0].sym == symconst::set;
  }


  // Pass 5: Reifying Definitions
  // ((lambda (<name>) (set! <name> <val>) <exp> ...) #f) => (define <name> <val>) <exp> ...
  void CPS_lambda_unwrapping_optimization_pass_5(scm_list& cps_exp)noexcept{
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i) {
      if(cps_exp[i].is_type(types::exp)) {
        if(is_optimizable_CPS_pass_5_exp(cps_exp[i].exp)) {
          expand_CPS_lambda_pass_5_definition(cps_exp,i);
          CPS_lambda_unwrapping_optimization_pass_5(cps_exp);
          return;
        }
        CPS_lambda_unwrapping_optimization_pass_5(cps_exp[i].exp);
      }
    }
  }

  /******************************************************************************
  * CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- DISPATCH
  ******************************************************************************/

  // Perform several optimization passes on the generated CPS to reduce lambda count 
  // NOTE: CPS atomics are already optimized @expansion-time
  void optimize_CPS_code_generation(scm_list& cps_exp)noexcept{
    CPS_lambda_unwrapping_optimization_pass_1(cps_exp);
    CPS_lambda_unwrapping_optimization_pass_2(cps_exp);
    CPS_lambda_unwrapping_optimization_pass_3(cps_exp);
    CPS_lambda_unwrapping_optimization_pass_4(cps_exp);
    CPS_lambda_unwrapping_optimization_pass_5(cps_exp);
  }

  /******************************************************************************
  * CONTINUATION-PASSING-STYLE EXPANSION
  ******************************************************************************/

  void confirm_valid_define_syntax(const scm_list&);

  bool is_scm_cps(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::scm_cps);}
  bool is_cps_quote(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::cps_quote);}
  bool is_cps_application(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::cps_app_tag);}
  bool is_using_cpsp(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::using_cpsp);}

  // Heist-specific checker to not prefix C++ derived special forms w/ application tag
  bool is_HEIST_cpp_derived_special_form(const sym_type& app)noexcept{
    return app == symconst::cps_quote || app == symconst::scm_cps    || app == symconst::map_literal ||
           app == symconst::while_t   || app == symconst::quasiquote || app == symconst::vec_literal || 
           app == symconst::unquote   || app == symconst::unquo_splice;
  }


  // Generate a unique hashed variant of a cps identifier name
  // NOTE: Max Unique Hashes = (expt 18446744073709551615 18446744073709551615)
  scm_string generate_unique_cps_hash()noexcept{
    if(G.CPS_HASH_IDX_1 != GLOBALS::MAX_SIZE_TYPE)
      return symconst::continuation
        + std::to_string(G.CPS_HASH_IDX_2) + '_' + std::to_string(G.CPS_HASH_IDX_1++);
    return symconst::continuation
      + std::to_string(++G.CPS_HASH_IDX_2) + '_' + std::to_string(G.CPS_HASH_IDX_1++);
  }


  // Generate a unique hashed variant of a cps value name
  // NOTE: Max Unique Hashes = (expt 18446744073709551615 18446744073709551615)
  scm_string generate_unique_cps_value_hash()noexcept{
    if(G.CPS_VALUE_HASH_IDX_1 != GLOBALS::MAX_SIZE_TYPE)
      return symconst::cps_generated_val
        + std::to_string(G.CPS_VALUE_HASH_IDX_2) + '_' + std::to_string(G.CPS_VALUE_HASH_IDX_1++);
    return symconst::cps_generated_val
      + std::to_string(++G.CPS_VALUE_HASH_IDX_2) + '_' + std::to_string(G.CPS_VALUE_HASH_IDX_1++);
  }


  // CPS atomics may be returned as is: (cps <cps-atomic>) -> <cps-atomic>
  bool data_is_cps_atomic(const data& d)noexcept{
    return !d.is_type(types::exp)                      || 
           is_tagged_list(d.exp,symconst::syn_rules)   || 
           is_tagged_list(d.exp,symconst::quote)       ||
           is_tagged_list(d.exp,symconst::delay)       ||
           is_tagged_list(d.exp,symconst::using_cpsp)  || 
           is_tagged_list(d.exp,symconst::definedp)    ||
           is_tagged_list(d.exp,symconst::delete_bang) ||
           is_tagged_list(d.exp,symconst::infix)       ||
           is_tagged_list(d.exp,symconst::infixr)      ||
           is_tagged_list(d.exp,symconst::unfix)       ||
           is_tagged_list(d.exp,symconst::defn_reader_alias);
  }


  data cps_expand_application(const scm_list& application) {
    const auto app_len = application.size();
    scm_list cps_app(app_len+2);
    scm_list cps_exp(3);
    cps_exp[0] = symconst::lambda;
    cps_exp[1] = scm_list(1,generate_unique_cps_hash()); // "k"
    cps_exp[2] = scm_list(2);
    auto iter = cps_exp.begin()+2;
    for(size_type i = 0; i < app_len; ++i) {
      if(data_is_cps_atomic(application[i])) {
        cps_app[i+1] = application[i];
      } else {
        iter->exp[0] = generate_fundamental_form_cps(application[i]);
        iter->exp[1] = scm_list(3);
        iter->exp[1].exp[0] = symconst::lambda;
        iter->exp[1].exp[1] = scm_list(1,generate_unique_cps_value_hash()); // "arg"
        cps_app[i+1] = iter->exp[1].exp[1].exp[0];
        iter->exp[1].exp[2] = scm_list(2);
        iter = iter->exp[1].exp.begin()+2;
      }
    }
    cps_app[0] = symconst::cps_app_tag; // Add the cps-application prefix
    cps_app[app_len+1] = cps_exp[1].exp[0];
    iter->exp = std::move(cps_app);
    return cps_exp;
  }


  // Generates the procedure to set <var> to <val> after binding <var> as a lambda arg
  scm_list get_cps_defn_set_procedure(const data& continuation,const data& var,const data& val){
    scm_list set_exp(2);
    // Set atomic values and pass to the continuation
    if(data_is_cps_atomic(val)) {
      set_exp[0] = continuation;
      set_exp[1] = scm_list(3);
      set_exp[1].exp[0] = symconst::set;
      set_exp[1].exp[1] = var;
      set_exp[1].exp[2] = val;
    // Cps-ify non-atomic values, passing a lambda as a continuation that in 
    //   turn sets the received value & passes such to the continuation given here as an arg
    } else {
      set_exp[0] = generate_fundamental_form_cps(val,false,false);
      set_exp[1] = scm_list(3);
      set_exp[1].exp[0] = symconst::lambda;
      set_exp[1].exp[1] = scm_list(1,generate_unique_cps_value_hash()); // "defn-val"
      set_exp[1].exp[2] = scm_list(2);
      set_exp[1].exp[2].exp[0] = continuation;
      set_exp[1].exp[2].exp[1] = scm_list(3);
      set_exp[1].exp[2].exp[1].exp[0] = symconst::set;
      set_exp[1].exp[2].exp[1].exp[1] = var;
      set_exp[1].exp[2].exp[1].exp[2] = set_exp[1].exp[1].exp[0]; // defn-val
    }
    return set_exp;
  }


  // Generates a CPS definition in the middle of a BEGIN or LAMBDA BODY sequence, w/ <rest_exp>
  //   being the remaining expressions AFTER this definition in the sequence
  // PRECONDITION: !rest_exp.empty()
  scm_list generate_mid_seq_cps_var_defn(const scm_list& defn_exp, const data& rest_exp){
    scm_list cps_defn(3);
    cps_defn[0] = symconst::lambda;
    cps_defn[1] = scm_list(1,generate_unique_cps_hash()); // topmost continuation "k"
    cps_defn[2] = scm_list(2);

    cps_defn[2].exp[1] = GLOBALS::FALSE_DATA_BOOLEAN; // initially bind defined symbol to #f
    cps_defn[2].exp[0] = scm_list(3);
    cps_defn[2].exp[0].exp[0] = symconst::lambda;
    cps_defn[2].exp[0].exp[1] = scm_list(1,defn_exp[1]); // defined symbol as an argument
    cps_defn[2].exp[0].exp[2] = scm_list(2);

    // Bind Var to Value
    cps_defn[2].exp[0].exp[2].exp[0] = scm_list(3);
    cps_defn[2].exp[0].exp[2].exp[0].exp[0] = symconst::lambda;
    cps_defn[2].exp[0].exp[2].exp[0].exp[1] = scm_list(1,generate_unique_cps_hash()); // continuation "k1" of set!
    cps_defn[2].exp[0].exp[2].exp[0].exp[2] = get_cps_defn_set_procedure(cps_defn[2].exp[0].exp[2].exp[0].exp[1].exp[0],
                                                                         defn_exp[1],defn_exp[2]);

    // Continue w/ expression after binding [SELF IS THE "k1" CONTINUATION OF THE EXPRESSION ABOVE]
    cps_defn[2].exp[0].exp[2].exp[1] = scm_list(3);
    cps_defn[2].exp[0].exp[2].exp[1].exp[0] = symconst::lambda;
    cps_defn[2].exp[0].exp[2].exp[1].exp[1] = scm_list(1,symconst::cps_ignore_arg); // result of set!
    cps_defn[2].exp[0].exp[2].exp[1].exp[2] = scm_list(2);

    // Pass <rest_exp> of expression to the topmost continuation if CPS-ATOMIC
    if(data_is_cps_atomic(rest_exp)) {
      cps_defn[2].exp[0].exp[2].exp[1].exp[2].exp[0] = cps_defn[1].exp[0];
      cps_defn[2].exp[0].exp[2].exp[1].exp[2].exp[1] = rest_exp;
    // Else CPS-ify <rest_exp> of expression & pass it the topmost continuation
    } else {
      cps_defn[2].exp[0].exp[2].exp[1].exp[2].exp[0] = generate_fundamental_form_cps(rest_exp,false,false);
      cps_defn[2].exp[0].exp[2].exp[1].exp[2].exp[1] = cps_defn[1].exp[0];
    }
    return cps_defn;
  }


  // Generates the CPS expression needed to evaluate <rest_exp> after defining new syntax
  scm_list generate_fundamental_form_cps_syn_defn_REST_EXP_continuation(const data& continuation,const data& rest_exp){
    scm_list rest_cont(2);
    // Pass <rest_exp> of expression to the topmost continuation if CPS-ATOMIC
    if(data_is_cps_atomic(rest_exp)) {
      rest_cont[0] = continuation;
      rest_cont[1] = rest_exp;
    // Else CPS-ify <rest_exp> of expression & pass it the topmost continuation
    } else {
      rest_cont[0] = generate_fundamental_form_cps(rest_exp,false,false);
      rest_cont[1] = continuation;
    }
    return rest_cont;
  }


  // Generates a CPS syntax definition in the middle of a BEGIN or LAMBDA BODY sequence, w/ <rest_exp>
  //   being the remaining expressions AFTER this syntax definition in the sequence
  // PRECONDITION: !rest_exp.empty()
  scm_list generate_mid_seq_cps_syn_defn(const scm_list& defn_exp, const data& rest_exp){
    const bool atomic_syntax_rules = data_is_cps_atomic(defn_exp[2]);
    scm_list cps_defn(3 + atomic_syntax_rules);
    cps_defn[0] = symconst::lambda;
    cps_defn[1] = scm_list(1,generate_unique_cps_hash()); // topmost continuation "k"
    // Atomic Syntax-Rules reduces # of lambdas needed by 1
    if(atomic_syntax_rules) {
      cps_defn[2] = scm_list(3);
      cps_defn[2].exp[0] = symconst::defn_syn;
      cps_defn[2].exp[1] = defn_exp[1];
      cps_defn[2].exp[2] = defn_exp[2];
      cps_defn[3] = generate_fundamental_form_cps_syn_defn_REST_EXP_continuation(cps_defn[1].exp[0],rest_exp);
      return cps_defn;
    }
    cps_defn[2] = scm_list(2);
    cps_defn[2].exp[0] = generate_fundamental_form_cps(defn_exp[2],false,false);
    cps_defn[2].exp[1] = scm_list(4);
    cps_defn[2].exp[1].exp[0] = symconst::lambda;
    cps_defn[2].exp[1].exp[1] = scm_list(1,generate_unique_cps_value_hash()); // "syntax-object"

    cps_defn[2].exp[1].exp[2] = scm_list(3);
    cps_defn[2].exp[1].exp[2].exp[0] = symconst::defn_syn;
    cps_defn[2].exp[1].exp[2].exp[1] = defn_exp[1];
    cps_defn[2].exp[1].exp[2].exp[2] = cps_defn[2].exp[1].exp[1].exp[0];
    cps_defn[2].exp[1].exp[3] = generate_fundamental_form_cps_syn_defn_REST_EXP_continuation(cps_defn[1].exp[0],rest_exp);
    return cps_defn;
  }


  template<scm_list(*generate_begin_defn)(const scm_list&,const data&)>
  scm_list generate_begin_mid_seq_defn(const scm_list& defn_exp,const data& begin){
    if(begin.exp.size() == 3 && data_is_cps_atomic(begin.exp[2])) {
      return generate_begin_defn(defn_exp,begin.exp[2]);
    } else {
      scm_list begin_tail(begin.exp.size()-1);
      begin_tail[0] = symconst::begin;
      std::copy(begin.exp.begin()+2,begin.exp.end(),begin_tail.begin()+1);
      return generate_begin_defn(defn_exp,begin_tail);
    }
  }


  scm_list convert_proc_defn_to_lambda_defn(const scm_list& defn_exp)noexcept{
    scm_list lambda_defn(3);
    lambda_defn[0] = symconst::define;
    lambda_defn[1] = defn_exp[1].exp[0]; // proc name
    lambda_defn[2] = scm_list(defn_exp.size());
    lambda_defn[2].exp[0] = symconst::lambda;
    lambda_defn[2].exp[1] = scm_list(defn_exp[1].exp.begin()+1,defn_exp[1].exp.end()); // args
    std::copy(defn_exp.begin()+2,defn_exp.end(),lambda_defn[2].exp.begin()+2); // append body to lambda
    return lambda_defn;
  }


  void get_cps_lambda_body(const scm_list& lambda_exp, scm_list& lambda_cps){
    lambda_cps[2] = scm_list(2); // lambda body
    // If single-expression body, NO NEED FOR "BEGIN"
    if(lambda_exp.size() == 3) { 
      if(data_is_cps_atomic(lambda_exp[2])) {
        lambda_cps[2].exp[0] = *lambda_cps[1].exp.rbegin(); // DYNAMIC CONTINUATION
        lambda_cps[2].exp[1] = lambda_exp[2];
      } else {
        lambda_cps[2].exp[0] = generate_fundamental_form_cps(lambda_exp[2],false,false);
        lambda_cps[2].exp[1] = *lambda_cps[1].exp.rbegin(); // DYNAMIC CONTINUATION
      }
    // If multi-expression body, WRAP W/ "BEGIN"
    } else {
      scm_list begin_recur(lambda_exp.size()-1);
      begin_recur[0] = symconst::begin;
      std::copy(lambda_exp.begin()+2,lambda_exp.end(),begin_recur.begin()+1);
      lambda_cps[2] = scm_list(2);
      lambda_cps[2].exp[0] = generate_fundamental_form_cps(begin_recur,false,false);
      lambda_cps[2].exp[1] = *lambda_cps[1].exp.rbegin();
    }
  }


  // PRECONDITION: lambda.capacity() == 3
  void generate_cps_lambda_form(const data& code, scm_list& lambda) {
    confirm_valid_lambda(code.exp);
    lambda[0] = symconst::lambda;
    lambda[1] = scm_list(1,generate_unique_cps_hash()); // "k"
    lambda[2] = scm_list(2);
    lambda[2].exp[0] = lambda[1].exp[0];
    lambda[2].exp[1] = scm_list(3);
    lambda[2].exp[1].exp[0] = symconst::lambda;
    if(code.exp[1].exp.empty()) { // ARGLESS
      lambda[2].exp[1].exp[1] = scm_list(1,generate_unique_cps_hash()); // "dyn-k"
    } else { // N ARGS
      const auto param_len = code.exp[1].exp.size();
      lambda[2].exp[1].exp[1] = scm_list(param_len+1);
      std::copy(code.exp[1].exp.begin(),code.exp[1].exp.end(),lambda[2].exp[1].exp[1].exp.begin());
      lambda[2].exp[1].exp[1].exp[param_len] = generate_unique_cps_hash(); // "dyn-k"
    }
    get_cps_lambda_body(code.exp,lambda[2].exp[1].exp);
  }


  scm_list fn_unwrap_inner_lambda(const scm_list& lambda_exp)noexcept{
    return scm_list(lambda_exp[2].exp[1].exp.begin()+1,lambda_exp[2].exp[1].exp.end());
  }


  scm_list get_cps_IF_consequent(const data& code, const data& continuation){
    scm_list consequent(2);
    if(data_is_cps_atomic(code.exp[2])) { // (k <atomic-consequent>)
      consequent[0] = continuation;
      consequent[1] = code.exp[2];
    } else { // ((cps <consequent>) k)
      consequent[0] = generate_fundamental_form_cps(code.exp[2],false,false);
      consequent[1] = continuation;
    }
    return consequent;
  }

  // PRECONDITION: Assumes IF alternative exists
  scm_list get_cps_IF_alternative(const data& code, const data& continuation){
    scm_list alternative(2);
    if(data_is_cps_atomic(code.exp[3])) { // (k <atomic-alternative>)
      alternative[0] = continuation;
      alternative[1] = code.exp[3];
    } else { // ((cps <alternative>) k)
      alternative[0] = generate_fundamental_form_cps(code.exp[3],false,false);
      alternative[1] = continuation;
    }
    return alternative;
  }

  scm_list get_cps_IF_VOID_alternative(const data& continuation){
    scm_list void_alternative(2);
    void_alternative[0] = continuation; // continuation
    void_alternative[1] = scm_list(1,"void");  // add (void)
    return void_alternative;
  }


  bool cps_is_non_atomc_defclass_property(const data& d)noexcept{
    return d.is_type(types::exp) && d.exp.size() == 2 && 
           d.exp[0].is_type(types::sym) && d.exp[1].is_type(types::exp);
  }

  bool cps_defclass_requires_outlined_properties(scm_list& defclass_expr, 
                                                 std::vector<scm_list>& stripped_property_values, 
                                                 std::vector<sym_type>& stripped_property_names) {
    validate_defclass(defclass_expr);
    bool requires_outlined_properties = false;
    for(size_type i = 2, n = defclass_expr.size(); i < n; ++i) {
      if(cps_is_non_atomc_defclass_property(defclass_expr[i])) {
        stripped_property_names.push_back(defclass_expr[i].exp[0].sym);
        stripped_property_values.push_back(defclass_expr[i].exp[1].exp);
        defclass_expr.erase(defclass_expr.begin()+i);
        --i; // account for loop's "++i" after having rm'd an item from <defclass_expr>
        requires_outlined_properties = true;
      }
    }
    return requires_outlined_properties;
  }


  scm_list cps_generate_macro_defn(const data& code,const bool topmost_call, const scm_string& mac_defn_statement){
    confirm_valid_define_syntax(code.exp);
    scm_list cps_defn_syn(3);
    cps_defn_syn[0] = symconst::lambda;
    cps_defn_syn[1] = scm_list(1,generate_unique_cps_hash()); // "k"
    cps_defn_syn[2] = scm_list(2);
    if(data_is_cps_atomic(code.exp[2])) {
      cps_defn_syn[2].exp[0] = cps_defn_syn[1].exp[0];
      cps_defn_syn[2].exp[1] = scm_list(3);
      cps_defn_syn[2].exp[1].exp[0] = mac_defn_statement;
      cps_defn_syn[2].exp[1].exp[1] = code.exp[1];
      cps_defn_syn[2].exp[1].exp[2] = code.exp[2];
      return cps_defn_syn;
    }
    cps_defn_syn[2].exp[0] = generate_fundamental_form_cps(code.exp[2],false,false);
    cps_defn_syn[2].exp[1] = scm_list(3);
    cps_defn_syn[2].exp[1].exp[0] = symconst::lambda;
    cps_defn_syn[2].exp[1].exp[1] = scm_list(1,generate_unique_cps_value_hash()); // "syntax-object"

    cps_defn_syn[2].exp[1].exp[2] = scm_list(2);
    cps_defn_syn[2].exp[1].exp[2].exp[0] = cps_defn_syn[1].exp[0];
    cps_defn_syn[2].exp[1].exp[2].exp[1] = scm_list(3);
    cps_defn_syn[2].exp[1].exp[2].exp[1].exp[0] = mac_defn_statement;
    cps_defn_syn[2].exp[1].exp[2].exp[1].exp[1] = code.exp[1];
    cps_defn_syn[2].exp[1].exp[2].exp[1].exp[2] = cps_defn_syn[2].exp[1].exp[1].exp[0];
    if(topmost_call) optimize_CPS_code_generation(cps_defn_syn);
    return cps_defn_syn;
  }


  // NOTE: <topmost_call> signals to optimize the result prior returning
  scm_list generate_fundamental_form_cps(const data& code,const bool topmost_call,const bool core_unexpanded){
    // EXPAND CORE SYNTAX 
    if(core_unexpanded)
      return generate_fundamental_form_cps(cps_recursively_deep_expand_core_macros(code),topmost_call,false);

    // ATOMIC DATUM OR EXPRESSION
    if(data_is_cps_atomic(code)) {
      scm_list lambda(3);
      lambda[0] = symconst::lambda;
      lambda[1] = scm_list(1,generate_unique_cps_hash()); // "k"
      lambda[2] = scm_list(2);
      lambda[2].exp[0] = lambda[1].exp[0];
      lambda[2].exp[1] = code;
      return lambda;

    // DEFCLASS
    } else if(is_tagged_list(code.exp,symconst::defclass)) {
      // Check if need to strip-out any property defns (if they have non-atomic values)
      std::vector<scm_list> stripped_property_values;
      std::vector<sym_type> stripped_property_names;
      // If must strip out defns
      auto defclass_expr = code.exp; // trasformation may mutate the <deflcass> expression
      if(cps_defclass_requires_outlined_properties(defclass_expr,stripped_property_values,stripped_property_names)){
        scm_list begin_expr(2+stripped_property_names.size());
        begin_expr[0] = symconst::begin;
        begin_expr[1] = defclass_expr;
        size_type j = 2;
        for(size_type i = 0, n = stripped_property_names.size(); i < n; ++i, ++j) {
          begin_expr[j] = scm_list(4);
          begin_expr[j].exp[0] = "proto-add-property!";
          begin_expr[j].exp[1] = begin_expr[1].exp[1]; // prototype name
          begin_expr[j].exp[2] = scm_list(2);
          begin_expr[j].exp[2].exp[0] = symconst::quote;
          begin_expr[j].exp[2].exp[1] = stripped_property_names[i];
          begin_expr[j].exp[3] = stripped_property_values[i];
        }
        return generate_fundamental_form_cps(begin_expr,topmost_call,false);
      // No external definitions needed! Treat as if cps-atomic.
      } else {
        scm_list lambda(3);
        lambda[0] = symconst::lambda;
        lambda[1] = scm_list(1,generate_unique_cps_hash()); // "k"
        lambda[2] = scm_list(2);
        lambda[2].exp[0] = lambda[1].exp[0];
        lambda[2].exp[1] = code;
        return lambda;
      }

    // DEFINE-SYNTAX
    } else if(is_tagged_list(code.exp,symconst::defn_syn)) {
      return cps_generate_macro_defn(code,topmost_call,symconst::defn_syn);

    // CORE-SYNTAX
    } else if(is_tagged_list(code.exp,symconst::core_syn)) {
      return cps_generate_macro_defn(code,topmost_call,symconst::core_syn);

    // SET!
    } else if(is_tagged_list(code.exp,symconst::set)) {
      confirm_valid_assignment(code.exp);
      scm_list lambda(3);
      lambda[0] = symconst::lambda;
      lambda[1] = scm_list(1,generate_unique_cps_hash()); // "k"
      lambda[2] = scm_list(2);
      if(data_is_cps_atomic(code.exp[2])) {
        lambda[2].exp[0] = lambda[1].exp[0];
        lambda[2].exp[1] = code.exp;
        return lambda;
      }
      lambda[2].exp[0] = generate_fundamental_form_cps(code.exp[2],false,false);
      lambda[2].exp[1] = scm_list(3);
      lambda[2].exp[1].exp[0] = symconst::lambda;
      lambda[2].exp[1].exp[1] = scm_list(1,generate_unique_cps_value_hash()); // "value"
      lambda[2].exp[1].exp[2] = scm_list(2);
      lambda[2].exp[1].exp[2].exp[0] = lambda[1].exp[0];
      lambda[2].exp[1].exp[2].exp[1] = scm_list(3);
      lambda[2].exp[1].exp[2].exp[1].exp[0] = symconst::set;
      lambda[2].exp[1].exp[2].exp[1].exp[1] = code.exp[1];
      lambda[2].exp[1].exp[2].exp[1].exp[2] = lambda[2].exp[1].exp[1].exp[0];
      if(topmost_call) optimize_CPS_code_generation(lambda);
      return lambda;

    // BEGIN
    } else if(is_tagged_list(code.exp,symconst::begin)) {
      scm_list lambda(3);
      lambda[0] = symconst::lambda;
      lambda[1] = scm_list(1,generate_unique_cps_hash()); // "k"
      lambda[2] = scm_list(2);
      // 0 Args
      if(code.exp.size() == 1) {
        lambda[2].exp[0] = lambda[1].exp[0];
        lambda[2].exp[1] = scm_list(1,"void");
      // 1 Arg
      } else if(code.exp.size() == 2) {
        if(data_is_cps_atomic(code.exp[1])) {
          lambda[2].exp[0] = lambda[1].exp[0];
          lambda[2].exp[1] = code.exp[1];
        } else {
          lambda[2].exp[0] = generate_fundamental_form_cps(code.exp[1],false,false);
          lambda[2].exp[1] = lambda[1].exp[0];
        }
      // N Args
      } else {
        bool rec_idx = !data_is_cps_atomic(code.exp[1]);
        if(rec_idx) {
          if(is_tagged_list(code.exp[1].exp,symconst::define)) {
            if(code.exp[1].exp[1].is_type(types::exp)) // Convert procedure definition to a lambda defn
              lambda[2].exp[0] = generate_begin_mid_seq_defn<generate_mid_seq_cps_var_defn>(
                                                             convert_proc_defn_to_lambda_defn(code.exp[1].exp),code.exp);
            else
              lambda[2].exp[0] = generate_begin_mid_seq_defn<generate_mid_seq_cps_var_defn>(code.exp[1].exp,code.exp);
            lambda[2].exp[1] = lambda[1].exp[0];
            if(topmost_call) optimize_CPS_code_generation(lambda);
            return lambda;
          } else if(is_tagged_list(code.exp[1].exp,symconst::defn_syn)) {
            lambda[2].exp[0] = generate_begin_mid_seq_defn<generate_mid_seq_cps_syn_defn>(code.exp[1].exp,code.exp);
            lambda[2].exp[1] = lambda[1].exp[0];
            if(topmost_call) optimize_CPS_code_generation(lambda);
            return lambda;
          } else {
            lambda[2].exp[0] = generate_fundamental_form_cps(code.exp[1],false,false);
          }
        } else {
          lambda[2].exp[1] = code.exp[1];
        }
        lambda[2].exp[rec_idx] = scm_list(3);
        lambda[2].exp[rec_idx].exp[0] = symconst::lambda;
        lambda[2].exp[rec_idx].exp[1] = scm_list(1,symconst::cps_ignore_arg);
        lambda[2].exp[rec_idx].exp[2] = scm_list(2);
        if(code.exp.size() == 3 && data_is_cps_atomic(code.exp[2])) { // 2 ARGS, THE LAST BEING CPS-ATOMIC
          lambda[2].exp[rec_idx].exp[2].exp[0] = lambda[1].exp[0];
          lambda[2].exp[rec_idx].exp[2].exp[1] = code.exp[2];
        } else { // 2+ ARGS, IF 2, 2ND != CPS-ATOMIC
          data begin_recur(scm_list(code.exp.size()-1));
          begin_recur.exp[0] = symconst::begin;
          std::copy(code.exp.begin()+2, code.exp.end(), begin_recur.exp.begin()+1);
          lambda[2].exp[rec_idx].exp[2].exp[0] = generate_fundamental_form_cps(begin_recur,false,false);
          lambda[2].exp[rec_idx].exp[2].exp[1] = lambda[1].exp[0];
        }
      }
      if(topmost_call) optimize_CPS_code_generation(lambda);
      return lambda;

    // LAMBDA
    } else if(is_tagged_list(code.exp,symconst::lambda)) {
      if(is_opt_arg_lambda(code.exp)) // convert optional-args <lambda> to a <fn>
        return generate_fundamental_form_cps(convert_lambda_opt_args_to_fn(code.exp),topmost_call,false);
      scm_list lambda(3);
      generate_cps_lambda_form(code,lambda);
      if(topmost_call) optimize_CPS_code_generation(lambda);
      return lambda;

    // FN
    } else if(is_tagged_list(code.exp,symconst::fn)) {
      validate_fn(code.exp);
      scm_list fn_exp(3);
      fn_exp[0] = symconst::lambda;
      fn_exp[1] = scm_list(1,generate_unique_cps_hash()); // "k"
      fn_exp[2] = scm_list(2);
      fn_exp[2].exp[0] = fn_exp[1].exp[0];
      fn_exp[2].exp[1] = scm_list(code.exp.size());
      fn_exp[2].exp[1].exp[0] = symconst::fn;
      for(size_type i = 1, n = code.exp.size(); i < n; ++i) {
        scm_list lambda(3);
        data lambda_exp(scm_list(1+code.exp[i].exp.size()));
        lambda_exp.exp[0] = symconst::lambda;
        std::copy(code.exp[i].exp.begin(),code.exp[i].exp.end(),lambda_exp.exp.begin()+1);
        generate_cps_lambda_form(lambda_exp,lambda);
        fn_exp[2].exp[1].exp[i] = fn_unwrap_inner_lambda(lambda);
      }
      if(topmost_call) optimize_CPS_code_generation(fn_exp);
      return fn_exp;

    // IF
    } else if(is_tagged_list(code.exp,symconst::if_t)) {
      confirm_valid_if(code.exp);
      scm_list lambda(3);
      lambda[0] = symconst::lambda;
      lambda[1] = scm_list(1,generate_unique_cps_hash()); // "k"
      // Atomic IF test
      if(data_is_cps_atomic(code.exp[1])) { 
        lambda[2] = scm_list(4);
        lambda[2].exp[0] = symconst::if_t;
        lambda[2].exp[1] = code.exp[1];
        lambda[2].exp[2] = get_cps_IF_consequent(code,lambda[1].exp[0]);
        if(code.exp.size() > 3) // Has IF alternative
          lambda[2].exp[3] = get_cps_IF_alternative(code,lambda[1].exp[0]);
        else // add pass (void) as alternative if none given
          lambda[2].exp[3] = get_cps_IF_VOID_alternative(lambda[1].exp[0]);
        if(topmost_call) optimize_CPS_code_generation(lambda);
        return lambda;
      }
      // Non-Atomic IF test
      lambda[2] = scm_list(2);
      lambda[2].exp[0] = generate_fundamental_form_cps(code.exp[1],false,false);
      lambda[2].exp[1] = scm_list(3);
      lambda[2].exp[1].exp[0] = symconst::lambda;
      lambda[2].exp[1].exp[1] = scm_list(1,generate_unique_cps_value_hash()); // "test-result"
      lambda[2].exp[1].exp[2] = scm_list(4);
      lambda[2].exp[1].exp[2].exp[0] = symconst::if_t;
      lambda[2].exp[1].exp[2].exp[1] = lambda[2].exp[1].exp[1].exp[0];
      lambda[2].exp[1].exp[2].exp[2] = get_cps_IF_consequent(code,lambda[1].exp[0]);
      if(code.exp.size() > 3) // Has IF alternative
        lambda[2].exp[1].exp[2].exp[3] = get_cps_IF_alternative(code,lambda[1].exp[0]);
      else // add pass (void) as alternative if none given
        lambda[2].exp[1].exp[2].exp[3] = get_cps_IF_VOID_alternative(lambda[1].exp[0]);
      if(topmost_call) optimize_CPS_code_generation(lambda);
      return lambda;

    // DEFINE
    } else if(is_tagged_list(code.exp,symconst::define)) {
      confirm_valid_definition(code.exp);
      if(is_obj_property_definition(code.exp)) { // DYNAMIC PROPERTY ADDITION
        return generate_fundamental_form_cps(convert_obj_property_defintion_to_method_call(code.exp),topmost_call,false);
      } else if(!code.exp[1].is_type(types::exp)) { // DEFINING VARIABLE
        scm_list cps_defn(3);
        cps_defn[0] = symconst::lambda;
        cps_defn[1] = scm_list(1,generate_unique_cps_hash()); // topmost continuation "k"
        cps_defn[2] = scm_list(2);
        cps_defn[2].exp[1] = GLOBALS::FALSE_DATA_BOOLEAN; // initially bind defined symbol to #f
        cps_defn[2].exp[0] = scm_list(3);
        cps_defn[2].exp[0].exp[0] = symconst::lambda;
        cps_defn[2].exp[0].exp[1] = scm_list(1,code.exp[1]); // defined symbol as an argument, and bind via set! (below)
        cps_defn[2].exp[0].exp[2] = get_cps_defn_set_procedure(cps_defn[1].exp[0],code.exp[1],code.exp[2]);
        if(topmost_call) optimize_CPS_code_generation(cps_defn);
        return cps_defn;
      } else { // DEFINING PROCEDURE
        auto cps_defn = generate_fundamental_form_cps(convert_proc_defn_to_lambda_defn(code.exp),false,false); // cps lambda defn
        if(topmost_call) optimize_CPS_code_generation(cps_defn);
        return cps_defn;
      }

    // APPLICATION
    } else {
      if(code.exp.empty())
        THROW_ERR("'scm->cps CAN'T EVAL AN EMPTY EXPRESSION!" << EXP_ERR(code.exp));
      // Applications are expanded @eval-time (accounts for expanding macros)
      if(code.exp[0].is_type(types::sym) && is_HEIST_cpp_derived_special_form(code.exp[0].sym))
        return code.exp; // Don't tag applications of HEIST's C++ derived forms
      // Tag any other application
      bool no_tag = !is_cps_application(code.exp);
      scm_list app(code.exp.size()+no_tag);
      if(no_tag) app[0] = symconst::cps_app_tag;
      std::copy(code.exp.begin(),code.exp.end(),app.begin()+no_tag);
      return app;
    }
  }


  // Extra <ignore> arg used to account for the <id> procedure that will be 
  // passed automatically by <scm_fcn::get_extended_environment>
  void account_for_automatically_passed_ID_continuation(scm_list& exp)noexcept{
    // Add an extra continuation param to account for the auto-added <id> procedure
    if(exp.size() >= 2 && exp[0].is_type(types::sym) && exp[0].sym == symconst::lambda && exp[1].is_type(types::exp) && exp[1].exp.size() == 1) {
      exp[1].exp.push_back(generate_unique_cps_hash()); // account for <id>
    // Wrap application in a lambda accepting an extra continuation param to 
    // account for the auto-added <id> procedure
    } else if(!exp.empty() && exp[0].is_type(types::sym) && exp[0].sym == symconst::cps_app_tag) {
      scm_list lambda(3);
      lambda[0] = symconst::lambda;
      lambda[1] = scm_list(2);
      lambda[1].exp[0] = generate_unique_cps_hash(); // k
      lambda[1].exp[1] = generate_unique_cps_hash(); // account for <id>
      lambda[2] = scm_list(2);
      lambda[2].exp[0] = std::move(exp);
      lambda[2].exp[1] = lambda[1].exp[0];
      exp = std::move(lambda);
    }
  }


  // Process convert & eval exp in CPS (Continuation Passing Style)
  exe_fcn_t analyze_scm_cps(scm_list& exp) {
    if(exp.size() == 1)
      THROW_ERR("'scm->cps expects at least 1 expression: (scm->cps <exp-1> ... <exp-N>)"<<EXP_ERR(exp));
    scm_list cps_exp;
    if(exp.size() == 2) {
      cps_exp = generate_fundamental_form_cps(exp[1]);
    } else { // Wrap multi-statement transforms in a BEGIN
      scm_list begin(exp.size());
      begin[0] = symconst::begin;
      std::copy(exp.begin()+1,exp.end(),begin.begin()+1);
      cps_exp = generate_fundamental_form_cps(begin);
    }
    account_for_automatically_passed_ID_continuation(cps_exp);
    return scm_analyze(std::move(cps_exp),false,true);
  }


  // Returns the generated CPS form of exp as a quoted list of data
  exe_fcn_t analyze_cps_quote(scm_list& exp,const bool cps_block) {
    if(exp.size() == 1)
      THROW_ERR("'cps-quote expects at least 1 expression: (cps-quote <exp-1> ... <exp-N>)"<<EXP_ERR(exp));
    scm_list quoted_cps(2);
    quoted_cps[0] = symconst::quote;
    if(exp.size() == 2) {
      quoted_cps[1] = generate_fundamental_form_cps(exp[1]);
    } else { // Wrap multi-statement transforms in a BEGIN
      scm_list begin(exp.size());
      begin[0] = symconst::begin;
      std::copy(exp.begin()+1,exp.end(),begin.begin()+1);
      quoted_cps[1] = generate_fundamental_form_cps(begin);
    }
    if(cps_block) return scm_analyze(generate_fundamental_form_cps(quoted_cps),false,true);
    return scm_analyze(std::move(quoted_cps));
  }


  // Return whether in a <scm->cps> block or the <-cps> flag is active
  exe_fcn_t analyze_using_cpsp(scm_list& exp,const bool cps_block) {
    if(exp.size() != 1)
      THROW_ERR("'using-cps? expects 0 args: (using-cps?)"<<EXP_ERR(exp));
    return [cps_block](env_type&){
      return boolean(cps_block||G.USING_CPS_CMD_LINE_FLAG);
    };
  }

  /******************************************************************************
  * REPRESENTING MACRO SYNTACTIC EXTENSIONS -- GENERAL PARSING HELPER FUNCTIONS
  ******************************************************************************/

  // (define-syntax <label>
  //   (syntax-rules (<keywords>)
  //     ((<pattern>) <template>)
  //     ((<pattern>) <template>)
  //     ((<pattern>) <template>))
  //
  // => token strings in the <keywords> list allow those symbols, and only 
  //    those symbols, in places where they are mentioned w/in <pattern>s

  // EXAMPLES:
  // ; Redefining λ to expand to 'lambda
  // (define-syntax λ
  //   (syntax-rules ()
  //     ((λ () body ...) (lambda () body ...))
  //     ((λ (arg ...) body ...) (lambda (arg ...) body ...))))
  //
  // ; Macro simulating variadic multiplication if '* were a binary operation
  // (define-syntax multiply-all 
  //   (syntax-rules ()
  //     ((multiply-all) 1)
  //     ((multiply-all a) a)
  //     ((multiply-all a b ...) (* a (multiply-all b ...)))))


  // 0. Each macro Id entry in <MACRO_ID_TABLE> (ie <MACRO_ID_VAR_TABLE.first>) represents 
  //      an instance of a macro identifier [in the pattern] to be expanded [in the template]
  //      into a value(s) [from the matched expression].
  // 1. <macId_name> gets the symbolic name of the identifier in question.
  // 2. <macId_val_pos_map> gets a vector of value-position pairs
  //    - "values" are those which the "name" should expand into
  //      * >1 "value" indicates a variadic expansion of the "name"
  //    - a "position" is a vector of indices to locate the "value" in the 
  //      pattern-matched expression (multiple idxs to traverse the nested vectors)
  // 3. <macId_values> returns a flatmap of the values in <macId_val_pos_map>
  //    - this is maintained alongside <macId_val_pos_map> in a seperate structure 
  //      internally in order to have fast reads (rather than generating a new instance
  //      from <macId_val_pos_map> each time)

  // 0. <VARARG_POSITIONS> (ie <MACRO_ID_VAR_TABLE.second>) tracks all of the position idx 
  //      vectors of '...' symbols found in the macro pattern (1st idx of each row in the matrix
  //      is detracted by 1 to disregard the intitial '_' & line up w/ the values of <MACRO_ID_TABLE>'s
  //      idxs of values [from <macId_val_pos_map>] in the matched expression)

  // 0. <MacroId_varArg_posPair> Holds 2 vectors, each holding nested vectors of position idxs 
  //    (of an elt w/in nested vectors) for:
  //    - .first: the current macro Id being parsed
  //    - .second: the current variadic '...' symbol being detected

  using macId_position_t = std::vector<size_type>;
  using MacroId_varArg_posPair = std::pair<macId_position_t,macId_position_t>;
  using MACRO_ID_VAL_POS_PAIR = std::pair<scm_list,macId_position_t>;
  using MACRO_ID_VAL_POS_PAIRS = std::vector<MACRO_ID_VAL_POS_PAIR>;
  using MACRO_ID = std::tuple<sym_type,MACRO_ID_VAL_POS_PAIRS,scm_list>;
  using MACRO_ID_TABLE = std::vector<MACRO_ID>;
  using VARARG_POSITIONS = std::vector<macId_position_t>;
  using MACRO_ID_VAR_TABLE = std::pair<MACRO_ID_TABLE,VARARG_POSITIONS>;

  // Node elt in the variadic expansion process
  struct macro_expansion_node {
    sym_type id_name;                             // Identifier name being expanded
    std::vector<macro_expansion_node> children;   // Variadic subgroup children
    std::vector<macId_position_t> positions;      // Position vector(s) of leaf node value(s)
    scm_list values;                              // Leaf node value(s)
    bool is_variadic = false;                     // Determines whether value corresponds to ...
    bool is_leaf()const{return children.empty();} // Determines valid elt: true ? value : children
    macro_expansion_node(const sym_type& name, const bool& variadic_node = false) : id_name(name), 
                                                                                    is_variadic(variadic_node) {}
  };

  // - Topmost node (ie node of in <MACRO_EXPANSION_TREE> is a symbol w/ children)
  //   * NON-VARIADIC identifiers are repn'd by nodes that are both ROOTS & a LEAF
  // - Leaves are ultimate values to be expanded into
  // - Intermediate nodes repn any multi-layered variadic expansion [ie (a ...) ...]
  using MACRO_EXPANSION_TREES_t = std::vector<macro_expansion_node>;


  // Accessors
  sym_type& macId_name(MACRO_ID& macId_instance)                      noexcept{return std::get<0>(macId_instance);}
  MACRO_ID_VAL_POS_PAIRS& macId_val_pos_map(MACRO_ID& macId_instance) noexcept{return std::get<1>(macId_instance);}
  scm_list& macId_values(MACRO_ID& macId_instance)                    noexcept{return std::get<2>(macId_instance);}


  // Confirm whether the given word is a keyword
  bool is_keyword(const sym_type& word, const frame_vars& keywords)noexcept{
    return std::find(keywords.begin(), keywords.end(), word) != keywords.end();
  }


  bool data_is_ellipsis(const data& d)noexcept{
    return d.is_type(types::sym) && d.sym == symconst::ellipsis;
  }


  // Primitive symbolic literals: #t #f '()
  bool is_primitive_symbolic_literal(const data& obj)noexcept{
    return obj.is_type(types::sym) && 
      (obj.sym == symconst::true_t || obj.sym == symconst::false_t || obj.sym == symconst::emptylist);
  }


  // Confirm <pat_entity> is a potential macro identifier
  // => WARNING: Doesn't check for whether is a keyword (used _only_ in expansion)!
  bool is_symbolic_macro_identifier(const data& pat_entity)noexcept{
    return pat_entity.is_type(types::sym) && !is_primitive_symbolic_literal(pat_entity) &&
           pat_entity.sym != symconst::ellipsis;
  }


  // Confirm <pat_entity> is a macro argument (non-keyword) name
  bool is_macro_argument_label(const data& pat_entity, const frame_vars& keywords)noexcept{
    return is_symbolic_macro_identifier(pat_entity) && !is_keyword(pat_entity.sym, keywords);
  }


  // Confirm whether 'pattern' is argless but was given 'args' (or vise versa)
  bool incompatible_void_arg_use(const scm_list& pattern, const scm_list& args)noexcept{
    return (pattern.size() == 1) ^ args.empty(); // pattern_is_argless ^ args_is_argless;
  }


  // Associate a pattern's macro identifier to the objects it will expand into
  void register_macro_identifier_expansion_values(MACRO_ID_TABLE& ID_TO_VAL_MAP,const sym_type& id_name, 
                                                  scm_list&& expansion_values,  const macId_position_t& macId_pos_vector)noexcept{
    for(auto& id : ID_TO_VAL_MAP) {
      if(macId_name(id) == id_name) {
        // Add to the flatmap of values
        auto& id_values = macId_values(id);
        id_values.insert(id_values.end(), expansion_values.begin(), expansion_values.end());
        // Add to the map of values-to-positions
        auto& val_pos_map = macId_val_pos_map(id);
        val_pos_map.push_back(std::make_pair(expansion_values,macId_pos_vector));
        return;
      }
    }
    MACRO_ID_VAL_POS_PAIRS val_pos_pairs(1,std::make_pair(expansion_values,macId_pos_vector));
    ID_TO_VAL_MAP.push_back(std::make_tuple(id_name,val_pos_pairs,expansion_values));
  }

  /******************************************************************************
  * REPRESENTING MACRO SYNTACTIC EXTENSIONS -- PATTERN MATCHING HELPER FUNCTIONS
  ******************************************************************************/

  bool compare_pattern_args_exp_match(const scm_list&,const scm_list&,const frame_vars&,MACRO_ID_VAR_TABLE&,
                                                      const size_type&,MacroId_varArg_posPair)noexcept;
  

  // Verify if pat_elt is a keyword that arg_elt is the same keyword
  bool mismatched_keywords(const data& pat_elt, const data& arg_elt, const frame_vars& keywords)noexcept{
    if(pat_elt.is_type(types::sym) && is_keyword(pat_elt.sym,keywords))
      return !arg_elt.is_type(types::sym) || arg_elt.sym != pat_elt.sym;
    return false;
  }


  // Confirm given 2 incompatible atomics
  bool mismatched_atomics(const data& pat_entity, const data& arg_entity)noexcept{
    if(is_primitive_symbolic_literal(pat_entity))
       return !is_primitive_symbolic_literal(arg_entity) || pat_entity.sym != arg_entity.sym;
    if(pat_entity.is_type(types::sym) || pat_entity.is_type(types::exp)) return false;
    return !pat_entity.noexcept_equal(arg_entity);
  }


  // Confirm the 2 given pattern/arg elts are mismatched subexpressions
  bool mismatched_subexpressions(const data& pat_elt, const data& arg_elt, const frame_vars& keywords, 
                                 MACRO_ID_VAR_TABLE& MID_VARG_PAIR, MacroId_varArg_posPair macId_varArg_vecs, 
                                 const size_type& args_idx, const size_type& pat_idx)noexcept{
    if(!pat_elt.is_type(types::exp) || !arg_elt.is_type(types::exp)) return true;
    if(pat_elt.exp.empty()) return !arg_elt.exp.empty();
    macId_varArg_vecs.first.push_back(args_idx), macId_varArg_vecs.second.push_back(pat_idx);
    return !compare_pattern_args_exp_match(pat_elt.exp,arg_elt.exp,keywords,MID_VARG_PAIR,0,macId_varArg_vecs);
  }


  // Handle '...' pattern analysis
  bool account_for_pattern_ellipsis_and_return_whether_no_match(const scm_list& args_exp,    size_type& args_idx, const data& pat_obj_prior_ellipsis,
                                                                const size_type& number_args_left_after_variadic, const frame_vars& keywords,
                                                                MACRO_ID_VAR_TABLE& MID_VARG_PAIR,MacroId_varArg_posPair macId_varArg_vecs,
                                                                const size_type& pat_idx)noexcept{
    // Start associating objs based on the first obj prior "..."'s position
    --args_idx;
    // Confirm enough room in <args_exp> for the variadic
    const auto& args_size = args_exp.size();
    if(number_args_left_after_variadic + args_idx >= args_size) return true;
    const auto va_objs_end = args_size - number_args_left_after_variadic;
    // Confirm each variadic obj in <args_exp> matches the layout of <pat_obj_prior_ellipsis>
    // Symbol Identifiers may expand to _any_ form
    if(pat_obj_prior_ellipsis.is_type(types::sym)) {
      macId_varArg_vecs.first.push_back(args_idx);
      register_macro_identifier_expansion_values(MID_VARG_PAIR.first,pat_obj_prior_ellipsis.sym,
                                                                     scm_list(args_exp.begin() + args_idx, 
                                                                              args_exp.begin() + va_objs_end),
                                                                     macId_varArg_vecs.first);
      const auto number_of_va_objs_in_args = va_objs_end - args_idx;
      args_idx += number_of_va_objs_in_args - 1; // advance <args_idx> to the last va obj associated
    // Expression Identifiers _may only_ expand into expressions of the same form
    } else {
      for(; args_idx < va_objs_end; ++args_idx)
        if(mismatched_subexpressions(pat_obj_prior_ellipsis,args_exp[args_idx],keywords,MID_VARG_PAIR,macId_varArg_vecs,args_idx,pat_idx))
          return true;
      --args_idx; // move back to the last associated obj (accounts for '++' in loop returning to)
    }
    // Save current position vector for ... identifier
    macId_varArg_vecs.second.push_back(pat_idx+1);
    MID_VARG_PAIR.second.push_back(macId_varArg_vecs.second);
    macId_varArg_vecs.second.pop_back();
    return false;
  }


  // Confirm whether the pattern sub-expression matches the 'args' sub-expression
  bool compare_pattern_args_exp_match(const scm_list& pat_exp, const scm_list& args_exp, const frame_vars& keywords,
                                      MACRO_ID_VAR_TABLE& MID_VARG_PAIR,                 const size_type& pat_idx_start, 
                                                                      MacroId_varArg_posPair macId_varArg_vecs)noexcept{
    // Confirm whether <pat_exp> & <args_exp> match one another
    size_type pat_idx = pat_idx_start, args_idx = 0, args_size = args_exp.size(), pat_size = pat_exp.size();
    for(; pat_idx < pat_size && args_idx < args_size; ++pat_idx, ++args_idx){
      // Check for proper "..." use in the pat_exp definition
      if(data_is_ellipsis(pat_exp[pat_idx])) { // Guarenteed pat_idx > 0 by syntax-rules analysis
        macId_varArg_vecs.second.push_back(pat_idx);
        MID_VARG_PAIR.second.push_back(macId_varArg_vecs.second);
        macId_varArg_vecs.second.pop_back();
        if(account_for_pattern_ellipsis_and_return_whether_no_match(args_exp, args_idx,pat_exp[pat_idx-1], pat_size-pat_idx-1,
                                                                        keywords, MID_VARG_PAIR,macId_varArg_vecs,pat_idx-1)){
          return false;
        }
      // Register the pat_exp's identifier & associated expansion value
      } else if(is_macro_argument_label(pat_exp[pat_idx],keywords)) {
        if(pat_idx+1 == pat_size || !data_is_ellipsis(pat_exp[pat_idx+1])) {
          macId_varArg_vecs.first.push_back(args_idx);
          register_macro_identifier_expansion_values(MID_VARG_PAIR.first,pat_exp[pat_idx].sym,scm_list(1,args_exp[args_idx]),macId_varArg_vecs.first);
          macId_varArg_vecs.first.pop_back();
        }
      // Verify matching subexpressions
      } else if(pat_exp[pat_idx].is_type(types::exp)) {
        if(!args_exp[args_idx].is_type(types::exp) || 
          ((pat_idx+1 == pat_size || !data_is_ellipsis(pat_exp[pat_idx+1])) &&
            mismatched_subexpressions(pat_exp[pat_idx],args_exp[args_idx],keywords,MID_VARG_PAIR,macId_varArg_vecs,args_idx,pat_idx))) {
          return false;
        }
      // Verify literal & keyword use are aligned
      } else if(mismatched_atomics(pat_exp[pat_idx],args_exp[args_idx]) || 
                mismatched_keywords(pat_exp[pat_idx],args_exp[args_idx],keywords)){
        return false;
      }
    }
    // Register the last identifier if variadic portion of expansion @ the end of the pattern & empty in args
    if(pat_idx+1 == pat_size && data_is_ellipsis(pat_exp[pat_idx]) && args_idx == args_size) {
      if(is_macro_argument_label(pat_exp[pat_idx-1],keywords)) {
        macId_varArg_vecs.second.push_back(pat_idx);
        MID_VARG_PAIR.second.push_back(macId_varArg_vecs.second);
        macId_varArg_vecs.second.pop_back();
        macId_varArg_vecs.first.push_back(args_idx-1);
        register_macro_identifier_expansion_values(MID_VARG_PAIR.first,pat_exp[pat_idx-1].sym,scm_list(1,args_exp[args_idx-1]),macId_varArg_vecs.first);
        macId_varArg_vecs.first.pop_back();
        return true;
      }
      return !account_for_pattern_ellipsis_and_return_whether_no_match(args_exp, args_idx, pat_exp[pat_idx-1], pat_size-pat_idx-1, 
                                                                           keywords, MID_VARG_PAIR, macId_varArg_vecs, pat_idx-1);
    }
    // Verify both <pat_exp> & <arg_exp> have been fully iterated
    return pat_idx == pat_size && args_idx == args_size;
  }


  // Confirm the given arg combo matches the given pattern (in terms of layout)
  bool is_pattern_match(const scm_list& args,const frame_vars& keywords,const scm_list& pattern,
                                                   MACRO_ID_VAR_TABLE& MID_VARG_PAIR)noexcept{
    if(incompatible_void_arg_use(pattern,args)) return false;
    MacroId_varArg_posPair macId_varArg_vecs;
    if(!compare_pattern_args_exp_match(pattern,args,keywords,MID_VARG_PAIR,1,macId_varArg_vecs)){
      MID_VARG_PAIR.first.clear();
      MID_VARG_PAIR.second.clear();
      return false;
    }
    return true;
  }


  // Returns whether the given args correspond to the given macro
  bool is_macro_match(const scm_list& args, const syn_type& mac, size_type& match_idx, 
                                            MACRO_ID_VAR_TABLE& MID_VARG_PAIR)noexcept{
    for(size_type i = 0, n = mac.patterns.size(); i < n; ++i) {
      if(is_pattern_match(args, mac.keywords, mac.patterns[i], MID_VARG_PAIR)) {
        match_idx = i;
        return true;
      }
    }
    return false;
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- EXPANSION HELPER FUNCTIONS
  ******************************************************************************/

  // Recursively prints <MACRO_EXPANSION_TREES> children subgroups
  void recur_stringify_MACRO_EXPANSION_TREES(const MACRO_EXPANSION_TREES_t& children,scm_string& buffer)noexcept{
    for(const auto& child : children) {
      if(!child.is_variadic) {
        buffer += "NON-VARIADIC = " + data(child.values).noexcept_write() + ',';
      } else if(child.is_leaf()) {
        buffer += data(child.values).noexcept_write() + ',';
      } else {
        buffer += '[';
        recur_stringify_MACRO_EXPANSION_TREES(child.children,buffer);
        buffer += ']';
      }
    }
  }

  // Stringifies <MACRO_EXPANSION_TREES> contents
  scm_string stringify_MACRO_EXPANSION_TREES(const MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES)noexcept{
    scm_string buffer("     ======================\n     MACRO_EXPANSION_TREES:\n     ");
    // for each tree
    for(const auto& tree : MACRO_EXPANSION_TREES) {
      buffer += "  " + tree.id_name + ": ";
      // print leaves immediately
      if(!tree.is_variadic) {
        buffer += "NON-VARIADIC = " + data(tree.values).noexcept_write() + "\n     ";
      // recursively print subgroups
      } else if(tree.is_leaf()) {
        buffer += "LEAF: " + data(tree.values).noexcept_write() + "\n     ";
      } else {
        buffer += '[';
        recur_stringify_MACRO_EXPANSION_TREES(tree.children,buffer);
        buffer += "]\n     ";
      }
    }
    return buffer + "======================";
  }


  // Generate a unique hashed id_name for the expanded symbol
  scm_string hash_macro_expansion_identifier(const scm_string& id_name,const bool& finished_expanding = false)noexcept{
    static size_type IDX_1 = 0, IDX_2 = 0;
    if(finished_expanding) {
      IDX_1 = IDX_2 = 0;
      return "";
    } else {
      if(IDX_1 != GLOBALS::MAX_SIZE_TYPE)
        return id_name + '-' + std::to_string(IDX_2) + '-' + std::to_string(IDX_1++);
      return id_name + '-' + std::to_string(++IDX_2) + '-' + std::to_string(IDX_1++);
    }
  }


  // Changes all <id_name> in <id_node> & below to be <tagged_symbol>
  void propagate_new_tagged_identifier_name(const scm_string& tagged_symbol,macro_expansion_node& id_node)noexcept{
    id_node.id_name = tagged_symbol;
    if(id_node.is_leaf()) return;
    for(auto& child : id_node.children)
      propagate_new_tagged_identifier_name(tagged_symbol,child);
  }


  // Get idx of <id_name> in <MACRO_EXPANSION_TREES>. Returns <GLOBALS::MAX_SIZE_TYPE> if not found.
  size_type find_macro_identifier_leaf_index(const MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES,const sym_type& id_name)noexcept{
    for(size_type i = 0, n = MACRO_EXPANSION_TREES.size(); i < n; ++i)
      if(MACRO_EXPANSION_TREES[i].id_name == id_name) return i;
    return GLOBALS::MAX_SIZE_TYPE;
  }


  // Expand level-1 ids, tag all nested variadic ids, wrench up tagged children of nested variadic ids
  void tag_and_expand_identifiers_while_wrenching_up_children(const size_type expansion_No, scm_list& expansions,
                                                                    MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES)noexcept{
    for(size_type i = 0, n = expansions.size(); i < n; ++i) {
      if(expansions[i].is_type(types::exp)) {
        tag_and_expand_identifiers_while_wrenching_up_children(expansion_No,expansions[i].exp,MACRO_EXPANSION_TREES);
      } else if(is_symbolic_macro_identifier(expansions[i])) {
        auto val_idx = find_macro_identifier_leaf_index(MACRO_EXPANSION_TREES,expansions[i].sym);
        if(val_idx == GLOBALS::MAX_SIZE_TYPE) continue; // symbol != variadic macro identifier
        // Splice in level-1 (non-nested) ... value
        if(MACRO_EXPANSION_TREES[val_idx].is_leaf()) {
          expansions[i] = MACRO_EXPANSION_TREES[val_idx].values[expansion_No];
        // Tag nested ... identifier to be expanded further, & wrench up the associated child node as a new (tagged) root
        } else {
          auto tagged_symbol = hash_macro_expansion_identifier(expansions[i].sym);
          expansions[i].sym = tagged_symbol; // tag symbol
          MACRO_EXPANSION_TREES.push_back(MACRO_EXPANSION_TREES[val_idx].children[expansion_No]); // wrench up child
          propagate_new_tagged_identifier_name(tagged_symbol,*MACRO_EXPANSION_TREES.rbegin()); // tag up wrenched child
        }
      }
    }
  }


  // <verify_all_identifiers_have_same_variadic_length> helper
  void confirm_identifier_variadic_length_is_consistent(size_type& total_expansions,  const scm_list& exp,
                                                        const scm_list& expanded_exp, const scm_list& args,
                                                        const sym_type& name,         const size_type& result, 
                                                        const MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES){
    if(total_expansions == GLOBALS::MAX_SIZE_TYPE) {
      total_expansions = result;
    } else if(total_expansions != result && result != GLOBALS::MAX_SIZE_TYPE) {
      THROW_ERR("'syntax-rules Different variadic identifiers can't expand in the same template expression!"
        "\n     Length 1 = " << total_expansions << ", Length 2 = " << result << 
        "\n     In subexpression: [ " << exp << " ]"
        "\n     Of template expansion: [ " << expanded_exp << " ]\n" 
        << stringify_MACRO_EXPANSION_TREES(MACRO_EXPANSION_TREES) << FCN_ERR(name,args));
    }
  }


  // Returns the length that the identifiers match (throw error if any are off)
  size_type verify_all_identifiers_have_same_variadic_length(const scm_list& args, const sym_type& name, const scm_list& exp, 
                                                             const scm_list& expanded_exp, const MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES){
    size_type total_expansions = GLOBALS::MAX_SIZE_TYPE;
    for(auto& elt : exp) {
      if(elt.is_type(types::exp)) {
        confirm_identifier_variadic_length_is_consistent(total_expansions,exp,expanded_exp,args,name,
          verify_all_identifiers_have_same_variadic_length(args,name,elt.exp,expanded_exp,MACRO_EXPANSION_TREES),
          MACRO_EXPANSION_TREES);
      } else if(is_symbolic_macro_identifier(elt)) {
        auto val_idx = find_macro_identifier_leaf_index(MACRO_EXPANSION_TREES,elt.sym);
        if(val_idx == GLOBALS::MAX_SIZE_TYPE) continue; // symbol != variadic macro identifier
        confirm_identifier_variadic_length_is_consistent(total_expansions,exp,expanded_exp,args,name,
          MACRO_EXPANSION_TREES[val_idx].is_leaf() ? MACRO_EXPANSION_TREES[val_idx].values.size() : 
                                                     MACRO_EXPANSION_TREES[val_idx].children.size(),
          MACRO_EXPANSION_TREES);
      }
    }
    return total_expansions;
  }


  // Non-Variadics have been expanded, expand all (possibly nested) variadics identifiers
  // NOTE: Traverses in POST-ORDER!
  void expand_macro_variadic_identifiers(const scm_list& args, const sym_type& name, scm_list& expanded_exp,
                                                               MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES){
    for(size_type i = 0; i < expanded_exp.size(); ++i) {
      if(i+1 < expanded_exp.size() && data_is_ellipsis(expanded_exp[i+1])) {
        // Expand variadic symbolic identifer immediately (no ctoring of any expression)
        if(is_symbolic_macro_identifier(expanded_exp[i])) {
          auto val_idx = find_macro_identifier_leaf_index(MACRO_EXPANSION_TREES,expanded_exp[i].sym);
          if(val_idx == GLOBALS::MAX_SIZE_TYPE) continue; // symbol != variadic macro identifier
          // confirm expanding into a non-nested variadic identifier
          if(!MACRO_EXPANSION_TREES[val_idx].is_leaf())
            THROW_ERR("'syntax-rules Misplaced \"...\" after improper non-nested variadic identifier [ " 
              << expanded_exp[i].sym << " ]\n     in [ " << expanded_exp << " ]\n"
              << stringify_MACRO_EXPANSION_TREES(MACRO_EXPANSION_TREES) << FCN_ERR(name,args));
          // erase the identifier & "...", then insert the variadic expansion values
          auto& expansions = MACRO_EXPANSION_TREES[val_idx].values;
          expanded_exp.erase(expanded_exp.begin()+i,expanded_exp.begin()+i+2);
          expanded_exp.insert(expanded_exp.begin()+i,expansions.begin(),expansions.end());
          i += expansions.size() - 1; // -1 accounts for loop's "++i"
        // Expand variadic expressions by constructing N expressions filled in w/ N values
        } else if(expanded_exp[i].is_type(types::exp)) {
          // verify ... follows an expression using the same # of expansion values per identifier
          size_type total_expansions = verify_all_identifiers_have_same_variadic_length(args,name,expanded_exp[i].exp,
                                                                                        expanded_exp,MACRO_EXPANSION_TREES);
          if(total_expansions == GLOBALS::MAX_SIZE_TYPE)
            THROW_ERR("'syntax-rules Misplaced \"...\" after non-variadic subexpression [ " 
              << expanded_exp[i].exp << " ]\n     in [ " << expanded_exp << " ]" << FCN_ERR(name,args));
          scm_list expansions(total_expansions,expanded_exp[i].exp);
          // tag <expansions> nested identifiers, tag associated tree groups & 
          //   wrench them up to be a root (WHILE KEEPING THE NEW ROOTS IN PLACE)
          for(size_type i = 0, n = expansions.size(); i < n; ++i)
            tag_and_expand_identifiers_while_wrenching_up_children(i,expansions[i].exp,MACRO_EXPANSION_TREES);
          // expand the ctord exps & re-traverse to expand any nested ...
          expanded_exp.erase(expanded_exp.begin()+i,expanded_exp.begin()+i+2);
          expanded_exp.insert(expanded_exp.begin()+i,expansions.begin(),expansions.end());
          --i; // mv back to account for loop's "++" & completely re-traverse expanded exp
        // NOTE: SHOULD NEVER BE INVOKED, BUT KEPT HERE AS A SAFETY GUARD
        } else {
          THROW_ERR("'syntax-rules Misplaced \"...\" after non-variadic identifier [ " 
            << expanded_exp[i].sym << " ]\n     in [ " << expanded_exp << " ]\n"
            << stringify_MACRO_EXPANSION_TREES(MACRO_EXPANSION_TREES) << FCN_ERR(name,args));
        }
      // Parse the nested non-variadic expression
      } else if(expanded_exp[i].is_type(types::exp)) {
        expand_macro_variadic_identifiers(args,name,expanded_exp[i].exp,MACRO_EXPANSION_TREES);
      }
    } // End of for
  }
  

  // Expands non-variadics, and guarentees:
  //   0. No expressions begin w/ ...
  //   2. Any SYMBOLIC identifier followed by ... is variadic
  //      => NOTE: EXPRESSIONS FOLLOWED BY ... HAVE __NOT__ BEEN VERIFIED THO !!!
  void expand_non_variadic_macro_symbols(const scm_list& args, const sym_type& name, scm_list& expanded_exp, 
                                                               MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES){
    for(size_type i = 0; i < expanded_exp.size(); ++i) {
      if(is_symbolic_macro_identifier(expanded_exp[i])) {
        // Expand non-variadic identifiers
        auto val_idx = find_macro_identifier_leaf_index(MACRO_EXPANSION_TREES,expanded_exp[i].sym);
        if(i+1 == expanded_exp.size() || !data_is_ellipsis(expanded_exp[i+1])) {
          if(val_idx != GLOBALS::MAX_SIZE_TYPE && !MACRO_EXPANSION_TREES[val_idx].is_variadic)
            expanded_exp[i] = MACRO_EXPANSION_TREES[val_idx].values[0];
        // Skip past ... if at a variadic identifier (handled in <expand_macro_variadic_identifiers>)
        } else if(val_idx != GLOBALS::MAX_SIZE_TYPE && MACRO_EXPANSION_TREES[val_idx].is_variadic) {
          ++i;
        // Catch non-macro syntax identifiers OR non-variadic syntax identifier followed by ...
        } else {
          THROW_ERR("'syntax-rules Misplaced \"...\" after non-variadic identifier [ " 
            << expanded_exp[i].sym << " ]\n     in [ " << expanded_exp << " ]" << FCN_ERR(name,args));
        }
      } else if(expanded_exp[i].is_type(types::exp)) {
        // Recursively expand symbolic identifiers
        expand_non_variadic_macro_symbols(args,name,expanded_exp[i].exp,MACRO_EXPANSION_TREES);
        // Skip past variadics after expressions (handled in <expand_macro_variadic_identifiers>)
        if(i+1 < expanded_exp.size() && data_is_ellipsis(expanded_exp[i+1])) ++i;
      } else if(data_is_ellipsis(expanded_exp[i])) {
        if(i) {
          THROW_ERR("'syntax-rules Misplaced \"...\" after non-variadic identifier [ " 
            << expanded_exp[i-1].sym << " ]\n     in [ " << expanded_exp << " ]" << FCN_ERR(name,args));
        } else {
          THROW_ERR("'syntax-rules Misplaced \"...\" at front of a template expression!" 
            << expanded_exp << FCN_ERR(name,args));
        }
      }
    }
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- UNWRAP ESCAPED VARIADIC TOKENS
  ******************************************************************************/

  bool string_is_an_escaped_variadic_token(const scm_string& str)noexcept{
    if(str.size() < 4 || str.compare(str.size()-3,3,"...")) 
      return false;
    for(size_type i = 0, n = str.size()-3; i < n; ++i)
      if(str[i] != '\\') return false;
    return true;
  }


  bool datum_is_an_escaped_variadic_token(const data& d)noexcept{
    return d.is_type(types::sym) && string_is_an_escaped_variadic_token(d.sym);
  }


  void unwrap_macro_escaped_variadic_tokens(scm_list& expanded)noexcept{
    for(size_type i = 0, n = expanded.size(); i < n; ++i) {
      if(expanded[i].is_type(types::exp))
        unwrap_macro_escaped_variadic_tokens(expanded[i].exp);
      else if(datum_is_an_escaped_variadic_token(expanded[i]))
        expanded[i].sym = expanded[i].sym.substr(1);
    }
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- DYNAMICALLY HASH syntax-hash IDENTIFIERS
  ******************************************************************************/

  // Generate a unique hashed variant of the given macro arg name for safe expansion
  // NOTE: Max Unique Hashes = (expt 18446744073709551615 18446744073709551615)
  scm_string safe_expansion_hashed_macro_arg(const scm_string& label)noexcept{
    if(G.MACRO_HASH_IDX_1 != GLOBALS::MAX_SIZE_TYPE)
      return "heist:core:sh:" + label + '-' 
        + std::to_string(G.MACRO_HASH_IDX_2) + '-' 
        + std::to_string(G.MACRO_HASH_IDX_1++);
    return "heist:core:sh:" + label + '-' 
      + std::to_string(++G.MACRO_HASH_IDX_2) + '-' 
      + std::to_string(G.MACRO_HASH_IDX_1++);
  }


  void recursively_apply_syntax_hash_to_identifiers(scm_list& expanded_exp, const frame_vars& hashed_ids, 
                                                                            const frame_vars& to_hash_ids)noexcept{
    const auto n = to_hash_ids.size();
    for(auto& datum : expanded_exp) {
      if(datum.is_type(types::sym)) {
        for(size_type i = 0; i < n; ++i) {
          if(to_hash_ids[i] == datum.sym) {
            datum.sym = hashed_ids[i];
            break;
          }
        }
      } else if(datum.is_type(types::exp)) {
        recursively_apply_syntax_hash_to_identifiers(datum.exp,hashed_ids,to_hash_ids);
      }
    }
  }


  void apply_syntax_hash_to_identifiers(scm_list& expanded_exp, const frame_vars& to_hash_ids)noexcept{
    // Get the dynamic (runtime) hash of each <syntax-hash> identifier in the macro
    frame_vars hashed_ids(to_hash_ids.size());
    for(size_type i = 0, n = to_hash_ids.size(); i < n; ++i)
      hashed_ids[i] = safe_expansion_hashed_macro_arg(to_hash_ids[i]);
    recursively_apply_syntax_hash_to_identifiers(expanded_exp,hashed_ids,to_hash_ids);
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- MATRIX DATA CLEANING HELPER FCNS PRIOR TREE
  ******************************************************************************/

  void remove_VA_POS_MATRIX_duplicate_instances(VARARG_POSITIONS& VA_POS_MATRIX)noexcept{
    const auto sameRows = [](auto& row1, auto& row2) {
      if(row1.size() != row2.size()) return false;
      for(size_type i = 0, n = row1.size(); i < n; ++i)
        if(row1[i] != row2[i]) return false;
      return true;
    };
    for(size_type i = 0; i < VA_POS_MATRIX.size(); ++i)
      for(size_type j = i+1; j < VA_POS_MATRIX.size();) {
        if(sameRows(VA_POS_MATRIX[i],VA_POS_MATRIX[j]))
          VA_POS_MATRIX.erase(VA_POS_MATRIX.begin()+j);
        else
           ++j;
      }
  }


  // Correlate positions in pattern moreso to those in args by negating result of skipping initial '_'
  void decrement_first_elt_of_each_VA_POS_VECTOR(VARARG_POSITIONS& VA_POS_MATRIX)noexcept{
    for(size_type i = 0, n = VA_POS_MATRIX.size(); i < n; ++i) --VA_POS_MATRIX[i][0];
  }


  // Compose the above 2 helper fcns functions to clean the position data
  void clean_VA_POS_MATRIX_for_MACRO_ID_comparision(VARARG_POSITIONS& VA_POS_MATRIX)noexcept{
    remove_VA_POS_MATRIX_duplicate_instances(VA_POS_MATRIX);
    decrement_first_elt_of_each_VA_POS_VECTOR(VA_POS_MATRIX);
    // Sort variadics based on descending # of idxs, 
    //   to process subgroups of nested ... prior outer ...
    std::sort(VA_POS_MATRIX.begin(),VA_POS_MATRIX.end(),
      [](macId_position_t& e1,macId_position_t& e2){return e1.size()>e2.size();});
  }


  // Break down sequential grouped instances of variadic identifier matches into individual instances
  void split_ID_TO_VAL_MAP_children_into_unique_entries(MACRO_ID_TABLE& ID_TO_VAL_MAP)noexcept{
    for(auto& id_val_pos_tuple : ID_TO_VAL_MAP) {
      auto& val_pos_pairs = macId_val_pos_map(id_val_pos_tuple);
      for(size_type i = 0; i < val_pos_pairs.size(); ++i) {
        // Expand the set of values into single value instances
        if(val_pos_pairs[i].first.size() > 1) {
          MACRO_ID_VAL_POS_PAIRS indiv_val_pos_instances;
          auto posv = val_pos_pairs[i].second;
          for(size_type j = 1, n = val_pos_pairs[i].first.size(); j < n; ++j) {
            ++(*posv.rbegin());
            indiv_val_pos_instances.push_back(std::make_pair(scm_list(1,val_pos_pairs[i].first[j]),posv));
          }
          // Erase the excess values in the original value set
          val_pos_pairs[i].first.erase(val_pos_pairs[i].first.begin()+1,val_pos_pairs[i].first.end());
          // Add the values/positions as individual instances
          val_pos_pairs.insert(val_pos_pairs.begin()+i+1,std::make_move_iterator(indiv_val_pos_instances.begin()),
                                                         std::make_move_iterator(indiv_val_pos_instances.end()));
        }
      }
    }
  }


  // Determine if <id_posv> begins w/ the elts in <prefix>
  bool id_posv_begins_with_prefix(const macId_position_t& id_posv, const macId_position_t& prefix)noexcept{
    if(id_posv.size() < prefix.size()) return false;
    for(size_type i = 0, n = prefix.size(); i < n; ++i)
      if(id_posv[i] != prefix[i]) return false;
    return true;
  }


  // init <va_prefix> w/ the prefix values of <id_posv>
  void get_new_VA_POSV_prefix(macId_position_t& va_prefix, const macId_position_t& id_posv)noexcept{
    std::copy(id_posv.begin(),id_posv.begin()+va_prefix.size(),va_prefix.begin());
  }


  // Cleans & reorganizes the <MACRO_ID_VAR_TABLE> table for easier analysis
  void clean_MID_VARG_PAIR_for_macro_expansion_analysis(MACRO_ID_VAR_TABLE& MID_VARG_PAIR) {
    split_ID_TO_VAL_MAP_children_into_unique_entries(MID_VARG_PAIR.first);
    clean_VA_POS_MATRIX_for_MACRO_ID_comparision(MID_VARG_PAIR.second);
  }


  // Adds values & positions to a <macro_expansion_node>
  // => NOTE: LAST UNUSED ARG IS JUST TO MATCH THE SAME FCN PTR TYPE AS <extract_id_children_subgroup>
  void accumulate_id_leaf_values_and_positions(macro_expansion_node& id_node, MACRO_ID_VAL_POS_PAIR& val_pos_pair,
                                                                              MACRO_EXPANSION_TREES_t&)noexcept{
    id_node.values.insert(id_node.values.end(),val_pos_pair.first.begin(),val_pos_pair.first.end());
    id_node.positions.push_back(val_pos_pair.second);
  }


  // Confirm <posv_matrix> contains <sought_posv>
  bool matrix_contains_vector(const macId_position_t& sought_posv, const std::vector<macId_position_t>& posv_matrix)noexcept{
    for(auto& posv : posv_matrix) {
      if(posv.size() != sought_posv.size()) continue;
      bool same_posv = true;
      for(size_type i = 0, n = posv.size(); i < n; ++i)
        if(posv[i] != sought_posv[i]) {
          same_posv = false;
          break;
        }
      if(same_posv) return true;
    }
    return false;
  }


  // Recursive search for <extract_id_children_subgroup>, returns whether found position in subgroup
  bool extract_id_children_subgroup_recur(macro_expansion_node& child, MACRO_ID_VAL_POS_PAIR& val_pos_pair)noexcept{
    if(child.is_leaf()) return matrix_contains_vector(val_pos_pair.second,child.positions);
    for(auto& grand_child : child.children)
      if(extract_id_children_subgroup_recur(grand_child,val_pos_pair))
        return true;
    return false;
  }


  // Extracts the subgroup from <generated_subgroups> containing <val_pos_pair> & puts it into <id_node>
  // => NOTE: if <generated_subgroups> DOESN'T have <val_pos_pair>, it is assumed to be already in <id_node>
  void extract_id_children_subgroup(macro_expansion_node& id_node, MACRO_ID_VAL_POS_PAIR& val_pos_pair,
                                                          MACRO_EXPANSION_TREES_t& generated_subgroups)noexcept{
    for(size_type i = 0, n = generated_subgroups.size(); i < n; ++i)
      if(extract_id_children_subgroup_recur(generated_subgroups[i],val_pos_pair)) {
        id_node.children.push_back(generated_subgroups[i]);
        generated_subgroups.erase(generated_subgroups.begin()+i);
        return; // found the subgroup, no more search needed!
      }
  }


  // Constructs <MACRO_EXPANSION_TREES> based on <ID_TO_VAL_MAP> & <VA_POS_MATRIX>
  // (transformation yields an easier means to expand nested variadic expressions by)
  void derive_and_construct_macro_expansion_tree(MACRO_ID_TABLE& ID_TO_VAL_MAP, VARARG_POSITIONS& VA_POS_MATRIX, 
                                                        MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES)noexcept{
    // For each identifier instance
    for(auto& macId_instance : ID_TO_VAL_MAP) {
      // Create the macro identifier expansion tree's root
      macro_expansion_node macId_node(macId_name(macId_instance));
      auto& val_pos_map = macId_val_pos_map(macId_instance);

      // For each ... instance
      for(auto& va_posv : VA_POS_MATRIX) {
        auto sought_id_posv_prefix = va_posv;
        --(*sought_id_posv_prefix.rbegin()); // 1st identifier associated w/ ... appears 1 idx prior ...
        
        // If identifier does match against ... instance
        if(id_posv_begins_with_prefix(val_pos_map[0].second,sought_id_posv_prefix)) {
          macId_node.is_variadic = true;
          sought_id_posv_prefix.pop_back(); // rm last idx to match against (no longer relevant to match)

          // Mk a subgroup node for the variadic expansion
          macro_expansion_node subgroup_node(macId_node.id_name,true);

          // If !macId_node.is_leaf(), keep a buffer of the current children subgroups
          //   from which to derive a higher level of subgroups (from nested ...)
          MACRO_EXPANSION_TREES_t generated_subgroups;

          // Fcn to build up the tree, based on whether currently aggregating leaves or combining subgroups
          void(*build_macro_expansion_tree)(macro_expansion_node&,MACRO_ID_VAL_POS_PAIR&,MACRO_EXPANSION_TREES_t&)noexcept = nullptr;
          // Add the leaf values as needed
          if(macId_node.is_leaf()) {
            accumulate_id_leaf_values_and_positions(subgroup_node,val_pos_map[0],generated_subgroups);
            build_macro_expansion_tree = accumulate_id_leaf_values_and_positions;
          // Get the current subgroup set as needed
          } else {
            generated_subgroups = std::move(macId_node.children);
            macId_node.children.clear();
            extract_id_children_subgroup(subgroup_node,val_pos_map[0],generated_subgroups);
            build_macro_expansion_tree = extract_id_children_subgroup;
          }

          // For each value instance of the identifier 
          for(size_type i = 1, n = val_pos_map.size(); i < n; ++i) {
            // if value posv matches the current ... subgroup instance
            if(id_posv_begins_with_prefix(val_pos_map[i].second,sought_id_posv_prefix)) {
              build_macro_expansion_tree(subgroup_node,val_pos_map[i],generated_subgroups);
            // if value posv matches a new ... subgroup instance
            } else {
              // AT A NEW SUBGROUP!
              get_new_VA_POSV_prefix(sought_id_posv_prefix,val_pos_map[i].second);
              // Add the current subgroup as a child, & reset the current subgroup node
              macId_node.children.push_back(subgroup_node);
              subgroup_node = macro_expansion_node(macId_node.id_name,true);
              build_macro_expansion_tree(subgroup_node,val_pos_map[i],generated_subgroups);
            }
          } // End of for

          // Add the current subgroup as a child
          macId_node.children.push_back(subgroup_node);

        } // End of if
      } // End of for
      if(!macId_node.is_variadic) {
        // Save the leaf non-variadic value (in this instance, root = leaf)
        macId_node.values = val_pos_map[0].first;
        macId_node.positions.push_back(val_pos_map[0].second);
      }

      // Register the generated macro id expansion tree
      if(macId_node.is_variadic && macId_node.children.size() == 1) 
        MACRO_EXPANSION_TREES.push_back(std::move(macId_node.children[0]));
      else
        MACRO_EXPANSION_TREES.push_back(std::move(macId_node));
    } // End of for
  }


  void expand_macro(const scm_list& args, const sym_type& name, scm_list& expanded_exp, 
                                                    MACRO_ID_VAR_TABLE& MID_VARG_PAIR){
    MACRO_EXPANSION_TREES_t MACRO_EXPANSION_TREES;
    clean_MID_VARG_PAIR_for_macro_expansion_analysis(MID_VARG_PAIR);
    derive_and_construct_macro_expansion_tree(MID_VARG_PAIR.first,MID_VARG_PAIR.second,MACRO_EXPANSION_TREES);
    expand_non_variadic_macro_symbols(args,name,expanded_exp,MACRO_EXPANSION_TREES);
    expand_macro_variadic_identifiers(args,name,expanded_exp,MACRO_EXPANSION_TREES);
    unwrap_macro_escaped_variadic_tokens(expanded_exp);
    hash_macro_expansion_identifier("",true); // reset hash idxs
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- ELLIPSIS HASHING & UNHASHING
  ******************************************************************************/

  bool data_is_hashable_ellipsis(const data& d)noexcept{
    return data_is_ellipsis(d) || datum_is_an_escaped_variadic_token(d);
  }

  bool data_is_hashed_ellipsis(const data& d)noexcept{
    return d.is_type(types::sym) && string_begins_with(d.sym,symconst::ellipsis_hash);
  }

  void hash_all_ellipsis_in_macro_args(exp_type& args)noexcept{
    for(size_type i = 0, n = args.size(); i < n; ++i) {
      if(args[i].is_type(types::exp)) {
        hash_all_ellipsis_in_macro_args(args[i].exp);
      } else if(data_is_hashable_ellipsis(args[i])) {
        args[i].sym = symconst::ellipsis_hash + args[i].sym;
      }
    }
  }

  void unhash_all_ellipsis_in_macro_args(exp_type& args)noexcept{
    static const size_type ellipsis_hash_prefix_length = strlen(symconst::ellipsis_hash);
    for(size_type i = 0, n = args.size(); i < n; ++i) {
      if(args[i].is_type(types::exp)) {
        unhash_all_ellipsis_in_macro_args(args[i].exp);
      } else if(data_is_hashed_ellipsis(args[i])) {
        args[i].sym = args[i].sym.substr(ellipsis_hash_prefix_length);
      }
    }
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- SYNTAX TRANSFORMER APPLICATION/EXECUTION
  ******************************************************************************/

  exp_type convert_transformer_data_result_to_syntax(data&& result, const scm_list& macro_expr) {
    data result_as_syntax;
    if(!convert_data_to_evaluable_syntax(result,result_as_syntax))
      THROW_ERR("Syntax Transformer Callable for macro \"" << macro_expr 
        << "\" didn't result in an evaluable datum:\n     " 
        << PROFILE(result) << EXP_ERR(macro_expr));
    if(!result_as_syntax.is_type(types::exp)) {
      exp_type begin_expr(2);
      begin_expr[0] = symconst::begin;
      begin_expr[1] = std::move(result_as_syntax);
      return begin_expr;
    }
    return result_as_syntax.exp;
  }


  data derive_quoted_macro_exression(const scm_list& macro_expr,env_type& env) {
    exp_type quoted_macro_expr(2);
    quoted_macro_expr[0] = symconst::quote;
    quoted_macro_expr[1] = macro_expr;
    return scm_eval(std::move(quoted_macro_expr),env);
  }


  // Expands the given syntax-transformer procedure & returns success status (ie whether matched)
  void apply_syntax_transformer_callable(const scm_list& args,const fcn_type& mac,
                                         scm_list& expanded_exp,env_type& env) {
    exp_type macro_expr(args.size()+1);
    macro_expr[0] = mac.name;
    std::copy(args.begin(), args.end(), macro_expr.begin() + 1);
    exp_type transformer_args(1,derive_quoted_macro_exression(macro_expr,env));
    expanded_exp = convert_transformer_data_result_to_syntax(execute_application(mac,transformer_args,env),macro_expr);
  }


  // Expands the given syntax-rules object & returns success status (ie whether matched)
  bool execute_syntax_rules_transform(const scm_list& args,const syn_type& mac,
                                      scm_list& expanded_exp,MACRO_ID_VAR_TABLE& MID_VARG_PAIR) {
    size_type match_idx = 0; // idx of the pattern & template w/in 'mac' that the label & args match
    if(is_macro_match(args, mac, match_idx, MID_VARG_PAIR)) {
      expanded_exp = mac.templates[match_idx]; // prefilled, then has contents expanded into it
      if(!mac.hashed_template_ids[match_idx].empty()) // dynamic <syntax-hash> application
        apply_syntax_hash_to_identifiers(expanded_exp,mac.hashed_template_ids[match_idx]);
      expand_macro(args, mac.label, expanded_exp, MID_VARG_PAIR);
      return true;
    }
    return false;
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- EXPANSION MAIN FUNCTIONS
  ******************************************************************************/

  // Confirm whether 'application_label' is a potential macro label
  bool application_is_a_potential_macro(const scm_string& application_label, 
                                        const std::vector<scm_string>& label_registry)noexcept{
    if(application_label.empty()) return false;
    for(const auto& label : label_registry)
      if(label == application_label)
        return true;
    return false;
  }


  // Returns whether the given label & args form a macro found in 'macs'.
  // If true, it also transforms the macro by expanding it into 'expanded_exp'
  bool handle_macro_transformation(const sym_type& label,const scm_list& args, 
                                   const frame_macs& macs,scm_list& expanded_exp,
                                   env_type& env){
    //  Map of pattern identifier & expansion value pairs
    MACRO_ID_VAR_TABLE MID_VARG_PAIR;
    // Search for macro matches
    for(const auto& mac : macs) {
      // Syntax-rules object
      if(mac.is_type(types::syn)) {
        if(label == mac.syn.label && execute_syntax_rules_transform(args,mac.syn,expanded_exp,MID_VARG_PAIR)) return true;
      // Syntax-transformer procedure (extracted from a callable)
      } else if(mac.is_type(types::fcn)) {
        if(label == mac.fcn.name) {
          apply_syntax_transformer_callable(args,mac.fcn,expanded_exp,env);
          return true;
        }
      // Unkown macro value: ERROR!
      } else {
        THROW_ERR("HEIST MACRO EXPANDER: UNKNOWN MACRO VALUE " << PROFILE(mac)
        << "\n     CURRENTLY ONLY SUPPORTING SYNTAX-RULES OBJECTS & PROCEDURES!" 
        << FCN_ERR(label,args));
      }
    }
    return false;
  }


  // Returns whether the given label & args form a macro found in 'env'.
  // If true, it also transforms the macro by expanding it into 'expanded_exp'
  bool expand_macro_if_in_env(const sym_type& label, scm_list args, 
                              env_type& env,scm_list& expanded_exp){
    env_type env_iterator = env;
    hash_all_ellipsis_in_macro_args(args);
    while(env_iterator != nullptr) {
      if(handle_macro_transformation(label,args,env_iterator->macros(),expanded_exp,env)) {
        unhash_all_ellipsis_in_macro_args(expanded_exp);
        return true;
      }
      env_iterator = env_iterator->parent;
    }
    return false;
  }

  /******************************************************************************
  * MACRO SYNTAX-RULES ANALYSIS VALIDATION HELPER FUNCTIONS:
  ******************************************************************************/

  // Confirm data is an unexpandable syntax-rules macro token
  bool data_is_literal_or_keyword(const data& pat_entity, const frame_vars& keywords)noexcept{
    return !pat_entity.is_type(types::exp) && !is_macro_argument_label(pat_entity,keywords);
  }


  // PRECONDITION: is_macro_argument_label(pattern[i],keywords) = true
  void confirm_unique_syntax_rules_pattern_identifier(const scm_list& pattern,const size_type& i,
                                                      const scm_list& exp, frame_vars& identifiers) {
    static constexpr const char * const format = 
      "\n     (syntax-rules (<keyword-list>) <pattern-template-clauses>)"
      "\n     <pattern-template-clause> = ((<pattern>) <template>)";
    // Confirm each pattern identifier only appears once
    if(std::find(identifiers.begin(),identifiers.end(),pattern[i].sym) != identifiers.end())
      THROW_ERR("'syntax-rules " << pattern[i].sym << " identifier found twice!"
        "\n     Each syntax-rules pattern identifier MUST be unique!\n     " 
        << cio_expr_str<&data::noexcept_write>(pattern) << format << EXP_ERR(exp));
    identifiers.push_back(pattern[i].sym);
  }


  void confirm_proper_syntax_rules_pattern_layout(const scm_list& pattern,const scm_list& exp,
                                                  const frame_vars& keywords, frame_vars& identifiers){
    static constexpr const char * const format = 
      "\n     (syntax-rules (<keyword-list>) <pattern-template-clauses>)"
      "\n     <pattern-template-clause> = ((<pattern>) <template>)";
    // Confirm pattern subexpression doesn't begin w/ '...'
    if(pattern.empty()) return; // guarenteed to be part of a recursive call (topmost pattern asserted non-empty)
    if(!pattern.empty() && data_is_ellipsis(pattern[0]))
      THROW_ERR("'syntax-rules \"...\" may NEVER begin a pattern subexpression!"
        "\n     " << cio_expr_str<&data::noexcept_write>(pattern) << format <<EXP_ERR(exp));
    if(is_macro_argument_label(pattern[0],keywords))
      confirm_unique_syntax_rules_pattern_identifier(pattern,0,exp,identifiers);
    bool seen_ellipses = false;
    for(size_type i = 1, n = pattern.size(); i < n; ++i) {
      if(data_is_ellipsis(pattern[i])) {
        // Confirm each subexpression has at most 1 '...'
        if(seen_ellipses){
          THROW_ERR("'syntax-rules \"...\" may only appear ONCE per pattern subexpression!"
            "\n     " << cio_expr_str<&data::noexcept_write>(pattern) <<
            "\n     -> IE: (a ... (b ...)) => VALID: 1 '...' PER EXPRESSION!"
            "\n            (a ... b ...)   => INVALID: 2 '...' IN A SINGLE EXPRESSION!"
            << format <<EXP_ERR(exp));
        } 
        // Confirm '...' doesn't follow a literal or keyword in the pattern
        if(data_is_literal_or_keyword(pattern[i-1],keywords)){
          THROW_ERR("'syntax-rules \"...\" may only be preceded by a non-literal-non-keyword"
            "\n     symbol or expression!\n     " << cio_expr_str<&data::noexcept_write>(pattern) 
            << format <<EXP_ERR(exp));
        }
        seen_ellipses = true;
      } else if(pattern[i].is_type(types::exp)) {
        confirm_proper_syntax_rules_pattern_layout(pattern[i].exp,exp,keywords,identifiers);
      } else if(is_macro_argument_label(pattern[i],keywords)) {
        confirm_unique_syntax_rules_pattern_identifier(pattern,i,exp,identifiers);
      }
    }
  }


  // Confirm proper '...' use & that each pattern identifier only appears once
  void confirm_proper_syntax_rules_pattern_template_clauses(const scm_list& exp,const syn_type& mac,const char* format){
    //   IE: ((a) ... (b ...)) is fine,     1 '...' per subexpression "depth"
    //       ((a) ... b ...)   is NOT fine, 2 '...' at a single "depth"
    //       (... a)         is NOT fine, '...' begins the subexpression/depth
    for(size_type i = 2, n = exp.size(); i < n; ++i) {
      // Confirm pattern-template clause begins w/ a non-empty pattern expression
      if(!exp[i].is_type(types::exp) || exp[i].exp.size() < 2 || 
         !exp[i].exp[0].is_type(types::exp) || exp[i].exp[0].exp.empty())
        THROW_ERR("'syntax-rules " << PROFILE(exp[i])
          << " is an invalid ((<pattern>) <template>) clause!" << format << EXP_ERR(exp));
      // Confirm pattern begins w/ a symbol
      if(!exp[i].exp[0].exp[0].is_type(types::sym))
        THROW_ERR("'syntax-rules patterns MUST begin with a symbol!"
          "\n     " << cio_expr_str<&data::noexcept_write>(exp[i].exp[0].exp) << format << EXP_ERR(exp));
      // Confirm pattern's topmost subexpression depth doesn't start w/ '...'
      if(exp[i].exp[0].exp.size() > 1 && data_is_ellipsis(exp[i].exp[0].exp[1]))
        THROW_ERR("'syntax-rules pattern '...' identifier must be preceded"
          " by a symbol or expression identifier!\n     " 
          << cio_expr_str<&data::noexcept_write>(exp[i].exp[0].exp) << format << EXP_ERR(exp));
      // Confirm each pattern identifier only appears once
      frame_vars identifiers;
      confirm_proper_syntax_rules_pattern_layout(exp[i].exp[0].exp,exp,mac.keywords,identifiers);
    }
  }


  // Confirm syntax-rules keywords list is valid, and extract it if so
  void extract_syntax_rules_keywords(const scm_list& exp,syn_type& mac,const char* format) {
    if(!exp[1].is_type(types::exp))
      THROW_ERR("'syntax-rules 1st arg "<<PROFILE(exp[1])<<" isn't a list of keyword symbols:"
        <<format<<EXP_ERR(exp));
    for(const auto& e : exp[1].exp) {
      if(!e.is_type(types::sym))
        THROW_ERR("'syntax-rules keyword "<<PROFILE(e)<<" must be a symbol!"<<format<<EXP_ERR(exp));
      if(e.sym == symconst::ellipsis)
        THROW_ERR("'syntax-rules \"...\" identifier may never be a keyword!"<<format<<EXP_ERR(exp));
      mac.keywords.push_back(e.sym); // Extract keywords
    }
  }


  void confirm_valid_syntax_rules_and_extract_keywords(const scm_list& exp, syn_type& mac) {
    static constexpr const char * const format = 
      "\n     (syntax-rules (<keyword-list>) <pattern-template-clauses>)"
      "\n     <pattern-template-clause> = ((<pattern>) <template>)";
    if(exp.size() < 3) // Confirm correct # of arguments
      THROW_ERR("'syntax-rules received incorrect # of arguments!"<<format<<EXP_ERR(exp));
    extract_syntax_rules_keywords(exp,mac,format);
    confirm_proper_syntax_rules_pattern_template_clauses(exp,mac,format);
  }

  /******************************************************************************
  * REPRESENTING SYNTAX: (syntax-rules <keyword-list> <pattern-template-clauses>)
  ******************************************************************************/

  bool is_syntax_rules(const scm_list& exp)noexcept{
    return is_tagged_list(exp,symconst::syn_rules);
  }


  void recursively_safe_expansion_hash_macro_template(const scm_string& value, const scm_string& hash_value, 
                                                                             scm_list& mac_template)noexcept{
    for(size_type i = 0, n = mac_template.size(); i < n; ++i) {
      if(mac_template[i].is_type(types::exp)) {
        recursively_safe_expansion_hash_macro_template(value, hash_value, mac_template[i].exp);
      } else if(mac_template[i].is_type(types::sym) && mac_template[i].sym == value) {
        mac_template[i].sym = hash_value;
      }
    }
  }


  void recursively_safe_expansion_hash_macro_pattern(scm_list& pattern, scm_list& mac_template, 
                                                     const frame_vars& keywords, const size_type& start=0)noexcept{
    for(size_type i = start, n = pattern.size(); i < n; ++i) {
      if(pattern[i].is_type(types::exp)) {
        recursively_safe_expansion_hash_macro_pattern(pattern[i].exp, mac_template, keywords);
      } else if(is_macro_argument_label(pattern[i], keywords)) {
        scm_string original_label = pattern[i].sym;
        pattern[i].sym = safe_expansion_hashed_macro_arg(pattern[i].sym);
        recursively_safe_expansion_hash_macro_template(original_label, pattern[i].sym, mac_template);
      }
    }
  }


  bool datum_is_syntax_hashed_symbol(const data& datum, const scm_list& exp){
    if(datum.is_type(types::exp) && is_tagged_list(datum.exp,symconst::syn_hash)) {
      if(datum.exp.size() != 2 || !datum.exp[1].is_type(types::sym))
        THROW_ERR("'syntax-hash didn't receive 1 symbol arg: (syntax-hash <symbol>)" << EXP_ERR(exp));
      return true;
    }
    return false;
  }


  void parse_macro_template_for_syntax_hashed_identifiers(frame_vars& hashed_id_registry, 
                                                          scm_list& mac_template, 
                                                          const scm_list& exp){
    for(size_type i = 0, n = mac_template.size(); i < n; ++i) {
      if(datum_is_syntax_hashed_symbol(mac_template[i],exp)) {
        hashed_id_registry.push_back(mac_template[i].exp[1].sym);
        mac_template[i] = *hashed_id_registry.rbegin(); // replace syntax-hash expression with the var once recorded
      } else if(mac_template[i].is_type(types::exp)) {
        parse_macro_template_for_syntax_hashed_identifiers(hashed_id_registry,mac_template[i].exp,exp);
      }
    }
  }


  exe_fcn_t analyze_syntax_rules(scm_list& exp) {
    syn_type mac("");
    confirm_valid_syntax_rules_and_extract_keywords(exp, mac);
    // Extract pattern-template clauses
    for(size_type i = 2, n = exp.size(); i < n; ++i) {
      mac.patterns.push_back(exp[i].exp[0].exp);
      // Wrap 'begin' around templates prior evaluation (for multi-exp bodies)
      mac.templates.push_back(scm_list(exp[i].exp.size()));
      mac.hashed_template_ids.push_back(frame_vars());
      mac.templates.rbegin()->operator[](0) = symconst::begin;
      std::copy(exp[i].exp.begin()+1, 
                exp[i].exp.end(), 
                mac.templates.rbegin()->begin()+1);
      // Parse identifiers to hash at runtime to avoid expansion collisions
      parse_macro_template_for_syntax_hashed_identifiers(*mac.hashed_template_ids.rbegin(),*mac.templates.rbegin(),exp);
    }
    // Hash macro args to prevent unintentional argument expansions
    for(size_type i = 0, n = mac.patterns.size(); i < n; ++i) {
      // pass <1> to not hash 1st pattern symbol (not an arg)
      recursively_safe_expansion_hash_macro_pattern(mac.patterns[i],mac.templates[i],mac.keywords,1);
    }
    return [syntax_rule=std::move(mac)](env_type&){return syntax_rule;};
  }

  /******************************************************************************
  * REPRESENTING SYNTAX EXTENSIONS: (define-syntax <label> <syntax-transformer>)
  ******************************************************************************/

  bool is_define_syntax(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::defn_syn);}


  void register_symbol_iff_new(std::vector<sym_type>& registry, const sym_type& label) {
    for(const auto& macro_label : registry)
      if(macro_label == label)
        return;
    registry.push_back(label);
  }


  void confirm_is_not_core_syntax_label(scm_list& exp) {
    if(std::find(G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.begin(),
                 G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.end(),exp[1].sym) != 
       G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.end()) {
      THROW_ERR("'define-syntax label \""<<exp[1].sym<<"\" is already 'core-syntax!"
        "\n     (define-syntax <label> <syntax-transformer>)"<<EXP_ERR(exp));
    }
  }


  void confirm_valid_define_syntax(const scm_list& exp) {
    if(exp.size() != 3)
      THROW_ERR("'define-syntax expects 2 arguments:"
        "\n     (define-syntax <label> <syntax-transformer>)"<<EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'define-syntax 1st arg "<<PROFILE(exp[1])
        <<" isn't a symbolic label!" << EXP_ERR(exp));
  }


  data extract_syntax_transformer(data&& mac, const scm_list& exp) {
    if(mac.is_type(types::syn)) return std::move(mac);
    if(primitive_data_is_a_callable(mac))
      return primitive_extract_callable_procedure(mac);
    THROW_ERR("'define-syntax syntax-transformer 2nd arg "<<PROFILE(exp[2])
      <<" isn't a syntax-rules object or callable:\n     (define-syntax "
        "<label> <syntax-transformer>)"<<EXP_ERR(exp));
  }


  void assign_macro_label(data& mac, const scm_string& label)noexcept{
    if(mac.is_type(types::syn)) {
      mac.syn.label = label;
    } else if(mac.is_type(types::fcn)) {
      mac.fcn.name = label;
    }
  }


  exe_fcn_t analyze_define_syntax(scm_list& exp,const bool cps_block=false,const bool core_syntax=false) {
    confirm_valid_define_syntax(exp);
    if(!core_syntax) confirm_is_not_core_syntax_label(exp);
    auto syntax_transformer_exe_proc = scm_analyze(data(exp[2]),false,cps_block);
    return [syntax_transformer_exe_proc=std::move(syntax_transformer_exe_proc),
      exp=std::move(exp),core_syntax](env_type& env)mutable{
      data mac = extract_syntax_transformer(syntax_transformer_exe_proc(env),exp);
      assign_macro_label(mac,exp[1].sym);
      register_symbol_iff_new(G.MACRO_LABEL_REGISTRY,exp[1].sym);
      if(core_syntax) {
        define_syntax_extension(mac,G.GLOBAL_ENVIRONMENT_POINTER); // bind in global environment
      } else {
        define_syntax_extension(mac,env); // bind in local environment
      }
      return GLOBALS::VOID_DATA_OBJECT;
    };
  }

  /******************************************************************************
  * ANALYSIS-TIME & ALWAYS-GLOBAL-SCOPE MACRO
  ******************************************************************************/

  bool is_core_syntax(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::core_syn);}

  exe_fcn_t analyze_core_syntax(scm_list& exp,const bool cps_block=false) {
    static constexpr const char * const format = 
      "\n     (core-syntax <label> <syntax-transformer>)";
    if(exp.size() < 3)
      THROW_ERR("'core-syntax didn't receive enough args!"<<format<<EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'core-syntax 1st arg "<<PROFILE(exp[1])<<" isn't a symbolic label!"<<format<<EXP_ERR(exp));
    // Eval the syntax defn in the global env at runtime
    exp[0] = symconst::defn_syn;
    auto core_syntax_name = exp[1].sym;
    auto core_proc = analyze_define_syntax(exp,cps_block,true);
    return [core_proc=std::move(core_proc),core_syntax_name=std::move(core_syntax_name)](env_type& env){
      // Register the core-syntax label
      register_symbol_iff_new(G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY,core_syntax_name);
      // Trigger the definition at runtime
      core_proc(env);
      return GLOBALS::VOID_DATA_OBJECT;
    };
  }

  /******************************************************************************
  * TRACING PROCEDURE CALLS
  ******************************************************************************/

  // Prints Debugging Call Trace (see <set-dynamic-call-trace!> primitive)
  void output_debug_call_trace(const scm_fcn& procedure,const scm_list& arguments,
                               const bool tail_call,    const bool callceing)noexcept{
    // Generate the call signature & Application-Affecting Call States
    auto call_signature = procedure_call_signature(procedure.printable_procedure_name(),arguments);
    const char* in_tail_call = tail_call ? "#t" : "#f";
    const char* using_callce = callceing ? "#t" : "#f";
    // Generate colors for truth-values (Green=#t, Red=#f) of Call States
    const auto tail_c_color = tail_call ? AFMT_32 : AFMT_31;
    const auto callce_color = callceing ? AFMT_32 : AFMT_31;
    // Output Trace Data
    fprintf(G.CURRENT_OUTPUT_PORT,
            "%s%s#<CALL-TRACE>%s Tail-Call: %s%s%s, Call/ce: %s%s%s %s]=>%s %s%s\n",
            afmt(AFMT_01), afmt(AFMT_35), afmt(AFMT_01), 
            afmt(tail_c_color), in_tail_call, afmt(AFMT_01), 
            afmt(callce_color), using_callce, afmt(AFMT_01), 
            afmt(AFMT_35), afmt(AFMT_01), call_signature.c_str(), afmt(AFMT_0));
    fflush(G.CURRENT_OUTPUT_PORT);
  }


  // Prints Call Trace (see <trace> primitive)
  void print_call_trace_depth_indentation(const scm_fcn& procedure,const bool tail_call=false)noexcept{
    size_type recursive_depth = 0; // default to 0 (level for all prims)
    if(procedure.is_compound()) {
      recursive_depth = procedure.recursive_depth();
      if(recursive_depth && tail_call) --recursive_depth;
    }
    for(size_type i = 0; i <= recursive_depth; ++i) {
      if(i & 1) fputc(' ',G.CURRENT_OUTPUT_PORT);
      else      fputc('|',G.CURRENT_OUTPUT_PORT);
    }
    fflush(G.CURRENT_OUTPUT_PORT);
  }


  // Print the current recursive depth as indentation, and the current invocation signature
  void output_call_trace_invocation(const scm_fcn& procedure, const scm_list& arguments,const bool tail_call=false)noexcept{
    auto call_signature = procedure_call_signature(procedure.printable_procedure_name(),arguments);
    print_call_trace_depth_indentation(procedure,tail_call);
    fprintf(G.CURRENT_OUTPUT_PORT, "%s\n", call_signature.c_str());
    fflush(G.CURRENT_OUTPUT_PORT);
  }


  // Print the current recursive depth as indentation, and the result string
  void output_call_trace_result(const scm_fcn& procedure, const data& result)noexcept{
    print_call_trace_depth_indentation(procedure);
    fprintf(G.CURRENT_OUTPUT_PORT, "%s\n", result.noexcept_write().c_str());
    fflush(G.CURRENT_OUTPUT_PORT);
  }


  bool tracing_procedure(const scm_string& name)noexcept{
    return !G.TRACED_FUNCTION_NAME.empty() && G.TRACED_FUNCTION_NAME == name;
  }

  /******************************************************************************
  * ANALYZING VARIABLES (& POTENTIAL OBJECT PROPERTY CHAINS!)
  ******************************************************************************/

  // (string-split <call> ".")
  void get_object_property_chain_sequence(const scm_string& call, std::vector<scm_string>& chain){
    chain.push_back("");
    for(const auto& ch : call) {
      if(ch == '.')
        chain.push_back("");
      else
        *chain.rbegin() += ch;
    }
    // verify no ".." found or ".<call>" or "<call>."
    for(const auto& link : chain)
      if(link.empty())
        THROW_ERR('\''<<call<<" invalid property access (missing an object and/or property)!"
          << EXP_ERR(call));
  }


  // extend <procedure>'s env with <calling_obj> as "self"
  data extend_method_env_with_SELF_object(obj_type& calling_obj, scm_fcn& procedure)noexcept{
    procedure.self = calling_obj;
    return procedure;
  }


  // Returns whether found <sought_property> in <proto> or its inherited prototype
  bool search_prototype_and_inherited_properties(cls_type& proto, const scm_string& sought_property, bool& is_member, data& value) {
    bool seek_call_value_in_local_object(data& value, const scm_string& property, bool& is_member);
    // Search the prototype
    for(size_type i = 0, n = proto->member_names.size(); i < n; ++i)
      if(proto->member_names[i] == sought_property) {
        // cache accessed inherited member
        value.obj->member_names.push_back(sought_property);
        auto deep_copied_value = proto->member_values[i].copy();
        value.obj->member_values.push_back(deep_copied_value);
        value = deep_copied_value;
        is_member = true;
        return true;
      }
    for(size_type i = 0, n = proto->method_names.size(); i < n; ++i)
      if(proto->method_names[i] == sought_property) {
        // cache accessed inherited method
        value.obj->method_names.push_back(sought_property);
        value.obj->method_values.push_back(proto->method_values[i]);
        value = extend_method_env_with_SELF_object(value.obj, proto->method_values[i].fcn);
        is_member = false;
        return true;
      }

    if(!value.obj->super) return false;
    value = value.obj->super;
    return proto->super && seek_call_value_in_local_object(value,sought_property,is_member);
  }


  // Returns whether found <property> as a member/method in <value.obj> 
  // If returns true, <property> value is in <value> & <is_member> denotes whether a member or method
  bool seek_call_value_in_local_object(data& value, const scm_string& property, bool& is_member) {
    auto& members = value.obj->member_names;
    // Seek members
    for(size_type i = 0, n = members.size(); i < n; ++i)
      if(members[i] == property) {
        value = value.obj->member_values[i];
        is_member = true;
        return true;
      }
    // Seek methods
    auto& methods = value.obj->method_names;
    for(size_type i = 0, n = methods.size(); i < n; ++i)
      if(methods[i] == property) {
        value = extend_method_env_with_SELF_object(value.obj, value.obj->method_values[i].fcn);
        is_member = false;
        return true;
      }
    // Seek proto & its inherited prototype
    // => IF FOUND, ADD IT TO THE LOCAL OBJECT INSTANCE
    return search_prototype_and_inherited_properties(value.obj->proto,property,is_member,value);
  }


  // Returns the ultimate value of the call-chain
  data get_object_property_chain_value(scm_string&& call, env_type& env) {
    // split the call chain into object series
    std::vector<scm_string> chain;
    get_object_property_chain_sequence(call,chain);
    // get the first object instance
    data value = lookup_variable_value(chain[0],env);
    // get the call value
    for(size_type i = 1, n = chain.size(); i < n; ++i) {
      if(!value.is_type(types::obj))
        THROW_ERR('\''<<call<<" can't access property "<<chain[i]<<" in non-object "
          << PROFILE(value) << '!' << EXP_ERR(call));
      // Search local members & methods, the proto, and the proto inheritances
      bool is_member = false;
      if(!seek_call_value_in_local_object(value,chain[i],is_member))
        THROW_ERR('\''<<call<<' '<<chain[i]<<" isn't a property in object"
          "\n     " << value << " of class name [ " << value.obj->proto->class_name << " ]!" << EXP_ERR(call));
      // if found a method, confirm at the last item in the call chain
      if(!is_member && i+1 < n)
        THROW_ERR('\''<<call<<" can't access property "<<chain[i]
          <<" of method "<<PROFILE(value)<<'!'<< EXP_ERR(call));
    }
    return value;
  }


  exe_fcn_t analyze_variable(scm_string variable) {
    // If a regular variable (no object property chain)
    if(!symbol_is_property_chain_access(variable))
      return [variable=std::move(variable)](env_type& env){
        return lookup_variable_value(variable,env);
      };
    // Object accessing members/methods!
    return [variable=std::move(variable)](env_type& env)mutable{
      return get_object_property_chain_value(std::move(variable),env);
    };
  }

  /******************************************************************************
  * APPLICATION
  ******************************************************************************/

  // -- STACK TRACE REGISTRATION
  void register_call_in_stack_trace(scm_fcn& procedure,scm_list& arguments)noexcept{
    if(!G.TRACE_LIMIT) return;
    if(!G.TRACE_ARGS) 
      GLOBALS::STACK_TRACE.push_back(procedure.printable_procedure_name());
    else
      GLOBALS::STACK_TRACE.push_back(procedure_call_signature(procedure.printable_procedure_name(),arguments));
  }

  // -- OPERATOR EVALUATION
  // generates <data proc.is_type(types::fcn)>: macro avoids extra copies
  #define evaluate_operator(OPERATOR_PROC,OPERATOR_ENV)\
    auto proc = OPERATOR_PROC(OPERATOR_ENV);\
    if(proc.is_type(types::obj) && primitive_data_is_a_functor(proc))\
      proc = primitive_extract_callable_procedure(proc);


  // -- APPLYING PRIMITIVE PROCEDURES
  data apply_primitive_procedure(data& proc,scm_list& args,env_type& env,const bool tail_call){
    // Output tracing information as needed
    auto tracing_proc = tracing_procedure(proc.fcn.name);
    if(tracing_proc) output_call_trace_invocation(proc.fcn,args);
    // Provide the environment to primitives applying user-defined procedures
    if(primitive_requires_environment(proc.fcn.prm)) args.push_back(env);
    if(proc.fcn.prm == primitive_APPLY) args.push_back(boolean(tail_call));
    // Extend partially applied args as needed
    if(!proc.fcn.param_instances.empty()) {
      if(args.empty())
        THROW_ERR('\''<<proc.fcn.printable_procedure_name()<<" partial procedure didn't receive any arguments!"
          << "\n     Partial Bindings: " << procedure_call_signature(proc.fcn.printable_procedure_name(),proc.fcn.param_instances[0]));
      args.insert(args.begin(),proc.fcn.param_instances[0].begin(),proc.fcn.param_instances[0].end());
    }
    auto result = proc.fcn.prm(args);
    // clear call from stack
    if(!GLOBALS::STACK_TRACE.empty()) GLOBALS::STACK_TRACE.pop_back();
    if(!tracing_proc) return result;
    // Output result's trace as needed
    output_call_trace_result(proc.fcn,result);
    return result;
  }


  // -- APPLY
  // Applies the given procedure, & then reapplies iteratively if at a tail call
  data apply_compound_procedure(exe_fcn_t& proc, env_type& extended_env) {
    auto result = proc(extended_env);
    size_type count = 1;
  tail_call_recur:
    if(result.is_type(types::exp) && is_tagged_list(result.exp,symconst::tail_call)) { // if tail call
      result = result.exp[1].fcn.bodies[0](result.exp[1].fcn.env);
      ++count;
      goto tail_call_recur;
    }
    // clear calls from stack trace (kept tail calls in trace for debuggability)
    if(count >= GLOBALS::STACK_TRACE.size())
      GLOBALS::STACK_TRACE.clear();
    else
      GLOBALS::STACK_TRACE.erase(GLOBALS::STACK_TRACE.end()-count,GLOBALS::STACK_TRACE.end());
    return result;
  }

  // Analogue to "apply", except no need to analyze the body of compound 
  //   procedures (already done). Hence only calls the execution procedure 
  //   for the proc's body w/ the extended environment
  data execute_application(data& procedure,scm_list& arguments,env_type& env,const bool tail_call,const bool applying_in_cps){
    if(!procedure.is_type(types::fcn))
      THROW_ERR("Invalid application of non-procedure "<<PROFILE(procedure)<<'!'
        <<FCN_ERR(procedure.noexcept_write(),arguments));
    // save call to stack trace output
    register_call_in_stack_trace(procedure.fcn,arguments);
    // output debugger call trace as needed
    if(G.TRACING_ALL_FUNCTION_CALLS)
      output_debug_call_trace(procedure.fcn,arguments,tail_call,procedure.fcn.is_using_dynamic_scope());
    // execute primitive procedure directly
    if(procedure.fcn.is_primitive())
      return apply_primitive_procedure(procedure,arguments,env,tail_call);
    // compound proc -- create the procedure body's extended environment frame
    exe_fcn_t fcn_body;
    auto extended_env = procedure.fcn.get_extended_environment(arguments,fcn_body,applying_in_cps);
    // splice in current env for dynamic scope as needed
    if(procedure.fcn.is_using_dynamic_scope()) {
      extended_env->parent = env;
    }
    // add the 'self' object iff applying a method
    if(procedure.fcn.self) {
      extended_env->variables().push_back("self");
      extended_env->values().push_back(procedure.fcn.self);
    }
    // confirm max recursive depth hasn't been exceeded
    auto& recursive_depth = procedure.fcn.recursive_depth();
    if(recursive_depth > G.MAX_RECURSION_DEPTH) {
      recursive_depth = 0;
      THROW_ERR("Maximum recursion depth of "<<G.MAX_RECURSION_DEPTH<<" exceeded!"
        << FCN_ERR(procedure.fcn.printable_procedure_name(), arguments));
    }
    // output tracing information as needed
    auto tracing_proc = tracing_procedure(procedure.fcn.name);
    if(tracing_proc) output_call_trace_invocation(procedure.fcn,arguments,tail_call);
    // store application data & return such back up to the last call if in a tail call
    if(tail_call) {
      scm_list tail_call_signature(2); // {tail-call-tag, proc-body, extended-env}
      tail_call_signature[0] = symconst::tail_call;
      tail_call_signature[1] = scm_fcn(extended_env,fcn_body);
      return tail_call_signature;
    }
    ++recursive_depth;
    auto result = apply_compound_procedure(fcn_body,extended_env);
    --recursive_depth;
    // output result's trace as needed
    if(tracing_proc) output_call_trace_result(procedure.fcn,result);
    return result;
  }

  // R-value overload
  data execute_application(data&& procedure,scm_list& arguments,env_type& env,const bool tail_call,const bool applying_in_cps){
    return execute_application(procedure,arguments,env,tail_call,applying_in_cps);
  }

  /******************************************************************************
  * CPS-BLOCK APPLICATION HELPER FUNCTIONS
  ******************************************************************************/

  bool data_is_continuation_parameter(const data& d)noexcept{
    return d.is_type(types::sym) && string_begins_with(d.sym,symconst::continuation);
  }


  bool procedure_defined_outside_of_CPS_block(const data& p)noexcept{
    return p.is_type(types::fcn) && 
            (p.fcn.is_primitive() || 
              (p.fcn.is_compound() && 
                (p.fcn.param_instances[0].empty() || 
                  !data_is_continuation_parameter(*p.fcn.param_instances[0].rbegin()))));
  }

  bool procedure_requires_continuation(const scm_fcn& p)noexcept{
    return (p.is_compound() && string_begins_with(p.name,symconst::pass_continuation)) ||
           (p.is_primitive() && string_begins_with(p.name,symconst::pass_continuation));
  }

  /******************************************************************************
  * CPS-BLOCK APPLICATION
  ******************************************************************************/

  void eval_application_arg_procs(const std::vector<exe_fcn_t>& arg_procs,scm_list& arg_vals,env_type& env){
    for(size_type i = 0, n = arg_procs.size(); i < n; ++i)
      arg_vals[i] = arg_procs[i](env);
  }


  // Application of CPS-block defined procedure in a CPS block
  exe_fcn_t analyze_CPS_block_application_of_CPS_proc(scm_list& exp,const bool tail_call){
    auto op_proc  = scm_analyze(operator_of(exp),false,true);
    auto arg_exps = operands(exp);
    std::vector<exe_fcn_t> arg_procs(arg_exps.size());
    for(size_type i = 0, n = arg_exps.size(); i < n; ++i)
      arg_procs[i] = scm_analyze(std::move(arg_exps[i]),false,true);
    return [op_proc=std::move(op_proc),arg_procs=std::move(arg_procs),
            tail_call=std::move(tail_call)](env_type& env)mutable{
      evaluate_operator(op_proc,env); // generates <data proc.is_type(types::fcn)>
      // Pass the result of the proc to the current continuation IFF
      //   proc was defined OUTSIDE of a scm->cps block
      if(procedure_defined_outside_of_CPS_block(proc)) {
        // Extract the continuation from the parameter list as needed
        auto continuation = (*arg_procs.rbegin())(env);
        bool passing_continuation = procedure_requires_continuation(proc.fcn);
        scm_list arg_vals(arg_procs.size() - !passing_continuation);
        // Eval each arg's exec proc to obtain the actual arg values
        if(!passing_continuation) {
          arg_procs.pop_back();
          eval_application_arg_procs(arg_procs,arg_vals,env);
          // Pass the result of the proc to the continuation
          auto result_arg = scm_list(1,execute_application(proc,arg_vals,env,false,true));
          // Pass the result of the proc to the continuation
          return execute_application(continuation,result_arg,env,tail_call,true);
        }
        // Apply the proc w/ the continuation
        size_type i = 0, n = arg_procs.size()-1;
        for(; i < n; ++i) arg_vals[i] = arg_procs[i](env);
        arg_vals[n] = continuation; // don't re-eval the continuation
        return execute_application(proc,arg_vals,env,false,true);
      // Else, apply the proc defined IN the CPS block as-is
      } else {
        scm_list arg_vals(arg_procs.size());
        eval_application_arg_procs(arg_procs,arg_vals,env);
        return execute_application(proc,arg_vals,env,tail_call,true);
      }
    };
  }


  // Application of a macro OR non-CPS-block defined procedure entity in a CPS block
  exe_fcn_t analyze_CPS_block_application_of_non_CPS_proc(scm_list& exp,const bool tail_call){
    auto arg_exps = operands(exp);
    // Save name of invoking entity (iff a symbol) to check for a possible macro
    sym_type op_name = exp[0].is_type(types::sym) ? exp[0].sym : "";
    // If possible analysis-time macro, expand and return analysis of the expansion
    if(application_is_a_potential_macro(op_name,G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY)) {
      if(scm_list expanded; expand_macro_if_in_env(op_name, arg_exps, G.GLOBAL_ENVIRONMENT_POINTER, expanded)) {
        return scm_analyze(generate_fundamental_form_cps(expanded),tail_call,true);
      } else {
        THROW_ERR("'core-syntax expression (label \"" << op_name 
          << "\") didn't match any patterns!" << EXP_ERR(exp));
      }
    }
    // If possible macro, expand the application if so, else analyze args at eval
    return [arg_exps=std::move(arg_exps),op_name=std::move(op_name),exp=std::move(exp),
            tail_call=std::move(tail_call)](env_type& env)mutable{
      // check for a possible macro instance, & expand/cps/eval it if so
      if(scm_list expanded; expand_macro_if_in_env(op_name, arg_exps, env, expanded)) {
        return scm_analyze(generate_fundamental_form_cps(expanded),tail_call,true)(env);
      }
      // else convert the function application to CPS & eval it
      return scm_analyze(cps_expand_application(exp),tail_call,true)(env);
    };
  }


  // Macro/procedure application in a CPS block
  exe_fcn_t analyze_CPS_block_application(scm_list& exp,const bool tail_call=false){
    // If application is already expanded (given a continuation as last arg),
    //   GUARANTEED such is applying to a function and NOT a macro
    if(data_is_continuation_parameter(*exp.rbegin()))
      return analyze_CPS_block_application_of_CPS_proc(exp,tail_call);
    // If application is NOT already expanded
    return analyze_CPS_block_application_of_non_CPS_proc(exp,tail_call);
  }

  /******************************************************************************
  * ANALYSIS & EVALUATION
  ******************************************************************************/

  // -- EVAL 
  data scm_eval(data&& datum, env_type& env) { // evaluate expression environment
    return scm_analyze(std::move(datum))(env);
  }


  // Analyzes the operator & operands, then returns an exec proc passing 
  //   both the operator/operand proc exec's to 'execute-application'
  //   (after having checked for macro use as well)
  exe_fcn_t analyze_application(scm_list& exp,const bool tail_call=false,const bool cps_block=false){
    // If in a scm->cps block
    if(cps_block && is_cps_application(exp)) {
      exp.erase(exp.begin()); // Rm the cps-application prefix
      return analyze_CPS_block_application(exp,tail_call);
    }
    auto arg_exps = operands(exp);
    // Save name of invoking entity (iff a symbol) to check for a possible macro
    sym_type op_name = exp[0].is_type(types::sym) ? exp[0].sym : "";
    // If possible analysis-time macro, expand and return analysis of the expansion
    if(application_is_a_potential_macro(op_name,G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY)) {
      if(scm_list expanded; expand_macro_if_in_env(op_name, arg_exps, G.GLOBAL_ENVIRONMENT_POINTER, expanded)) {
        return scm_analyze(std::move(expanded),tail_call,cps_block);
      } else {
        THROW_ERR("'core-syntax expression (label \"" << op_name 
          << "\") didn't match any patterns!" << EXP_ERR(exp));
      }
    }
    auto op_proc = scm_analyze(operator_of(exp),false,cps_block);
    // If _NOT_ a possible macro, analyze the applicator's args ahead of time
    if(!application_is_a_potential_macro(op_name,G.MACRO_LABEL_REGISTRY)) {
      std::vector<exe_fcn_t> arg_procs(arg_exps.size());
      for(size_type i = 0, n = arg_exps.size(); i < n; ++i)
        arg_procs[i] = scm_analyze(std::move(arg_exps[i]),false,cps_block);
      return [op_proc=std::move(op_proc),arg_procs=std::move(arg_procs),
              tail_call=std::move(tail_call),cps_block=cps_block](env_type& env){
        scm_list arg_vals(arg_procs.size());
        eval_application_arg_procs(arg_procs,arg_vals,env);
        evaluate_operator(op_proc,env); // generates <data proc.is_type(types::fcn)>
        return execute_application(proc,arg_vals,env,tail_call,cps_block);
      };
    }
    // If possible macro, expand the application if so, else analyze args at eval
    return [op_proc=std::move(op_proc),arg_exps=std::move(arg_exps),
            op_name=std::move(op_name),tail_call=std::move(tail_call),
            cps_block=std::move(cps_block)](env_type& env){
      // check for a possible macro instance, & expand/eval it if so
      if(scm_list expanded; expand_macro_if_in_env(op_name, arg_exps, env, expanded))
        return scm_analyze(std::move(expanded),tail_call,cps_block)(env);
      // eval each arg's exec proc to obtain the actual arg values
      scm_list arg_vals(arg_exps.size());
      for(size_type i = 0, n = arg_exps.size(); i < n; ++i)
        arg_vals[i] = scm_analyze(data(arg_exps[i]),false,cps_block)(env);
      evaluate_operator(op_proc,env); // generates <data proc.is_type(types::fcn)>
      return execute_application(proc,arg_vals,env,tail_call,cps_block);
    };
  }


  // -- ANALYZE (SYNTAX)
  exe_fcn_t scm_analyze(data&& datum,const bool tail_call,const bool cps_block) { // analyze expression
    if(is_self_evaluating(datum))            return [d=std::move(datum)](env_type&){return d;};
    else if(is_variable(datum))              return analyze_variable(datum.sym);
    else if(datum.exp.empty())                      THROW_ERR("Can't eval an empty expression!"<<EXP_ERR("()"));
    else if(is_quoted(datum.exp))            return analyze_quoted(datum.exp);
    else if(is_assignment(datum.exp))        return analyze_assignment(datum.exp,cps_block);
    else if(is_definition(datum.exp))        return analyze_definition(datum.exp,cps_block);
    else if(is_if(datum.exp))                return analyze_if(datum.exp,tail_call,cps_block);
    else if(is_lambda(datum.exp))            return analyze_lambda(datum.exp,cps_block);
    else if(is_begin(datum.exp))             return analyze_sequence(begin_actions(datum.exp),tail_call,cps_block);
    else if(is_delay(datum.exp))             return analyze_delay(datum.exp,cps_block);
    else if(is_defclass(datum.exp))          return analyze_defclass(datum.exp,cps_block);
    else if(is_fn(datum.exp))                return analyze_fn(datum.exp,cps_block);
    else if(is_scm_cps(datum.exp))           return analyze_scm_cps(datum.exp);
    else if(is_cps_quote(datum.exp))         return analyze_cps_quote(datum.exp,cps_block);
    else if(is_using_cpsp(datum.exp))        return analyze_using_cpsp(datum.exp,cps_block);
    else if(is_quasiquote(datum.exp))        return analyze_quasiquote(datum.exp,cps_block);
    else if(is_core_syntax(datum.exp))       return analyze_core_syntax(datum.exp,cps_block);
    else if(is_define_syntax(datum.exp))     return analyze_define_syntax(datum.exp,cps_block);
    else if(is_syntax_rules(datum.exp))      return analyze_syntax_rules(datum.exp);
    else if(is_definedp(datum.exp))          return analyze_definedp(datum.exp);
    else if(is_delete(datum.exp))            return analyze_delete(datum.exp);
    else if(is_infix(datum.exp))             return analyze_infix(datum.exp);
    else if(is_infixr(datum.exp))            return analyze_infixr(datum.exp);
    else if(is_unfix(datum.exp))             return analyze_unfix(datum.exp);
    else if(is_defn_reader_alias(datum.exp)) return analyze_defn_reader_alias(datum.exp);
    else if(is_while(datum.exp))             return analyze_while(datum.exp,cps_block);
    else if(is_vector_literal(datum.exp))           THROW_ERR("Misplaced keyword 'vector-literal outside of a quotation! -- ANALYZE"   <<EXP_ERR(datum.exp));
    else if(is_hmap_literal(datum.exp))             THROW_ERR("Misplaced keyword 'hmap-literal outside of a quotation! -- ANALYZE"     <<EXP_ERR(datum.exp));
    return analyze_application(datum.exp,tail_call,cps_block);
  }

  #undef evaluate_operator

  /******************************************************************************
  * GLOBAL ENVIRONMENT SETUP
  ******************************************************************************/

  void set_default_global_environment() {
    G.GLOBAL_ENVIRONMENT_POINTER = make_env();
    G.GLOBAL_ENVIRONMENT_POINTER = extend_environment(
      primitive_procedure_names(),
      primitive_procedure_objects(),
      G.GLOBAL_ENVIRONMENT_POINTER
    );
    define_variable(symconst::true_t,        GLOBALS::TRUE_DATA_BOOLEAN,           G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::false_t,       GLOBALS::FALSE_DATA_BOOLEAN,          G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("*heist-dirname*",       make_str(HEIST_DIRECTORY_FILE_PATH),  G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("fl-precision",          num_type(num_type::INEXACT_PRECISION),G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("fl-max",                num_type(num_type::INEXACT_MAX),      G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("fl-min",                num_type(num_type::INEXACT_MIN),      G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("fl-epsilon",            num_type(num_type::INEXACT_EPSILON),  G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("*max-infix-precedence*",num_type(LLONG_MAX),                  G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("*min-infix-precedence*",num_type(LLONG_MIN),                  G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("*heist-platform*",      HEIST_PLATFORM,                       G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("*heist-exact-platform*",HEIST_EXACT_PLATFORM,                 G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("stream-null",           symconst::emptylist,                  G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::exit_success,  num_type(0),                          G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::exit_failure,  num_type(1),                          G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::null_env,      symconst::null_env,                   G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::local_env,     symconst::local_env,                  G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::global_env,    symconst::global_env,                 G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("*argv*", primitive_LIST_to_CONS_constructor(GLOBALS::ARGV.begin(),GLOBALS::ARGV.end()), G.GLOBAL_ENVIRONMENT_POINTER);
    define_variable("*argc*", num_type(GLOBALS::ARGV.size()),                                                G.GLOBAL_ENVIRONMENT_POINTER);
    evaluate_primitives_written_in_heist_scheme();
  }

  /******************************************************************************
  * GLOBAL PORT REGISTRY CLEANUP
  ******************************************************************************/

  void close_port_registry()noexcept{
    for(size_type i = 2, n = GLOBALS::PORT_REGISTRY.size(); i < n; ++i)
      if(GLOBALS::PORT_REGISTRY[i] && 
         GLOBALS::PORT_REGISTRY[i]!=stdout && GLOBALS::PORT_REGISTRY[i]!=stderr &&
         GLOBALS::PORT_REGISTRY[i]!=stdin){
        fclose(GLOBALS::PORT_REGISTRY[i]);
        GLOBALS::PORT_REGISTRY[i] = nullptr;
      }
  }

  /******************************************************************************
  * REPL DRIVER LOOP
  ******************************************************************************/

  void announce_input(FILE* outs)noexcept{fputs(G.REPL_PROMPT.c_str(), outs);}
  void indent_input(FILE* outs)  noexcept{fputs(G.REPL_TAB.c_str(), outs);}


  // Read & parse user expressions
  scm_list read_user_input(FILE* outs, FILE* ins, const bool& in_repl){
    scm_string input, tmp_buffer;
    scm_list abstract_syntax_tree;
    int ch;
    for(;;) {
      // Read input
      fflush(outs);
      tmp_buffer.clear();
      while((ch = fgetc(ins)) != '\n' && ch != EOF) tmp_buffer += ch;
      // Handle EOF Signal
      if(ch == EOF && ins == stdin) {
        clearerr(stdin);
        if(in_repl) return scm_list(1,chr_type(EOF)); // called by REPL
        return scm_list();                            // called by <read>
      }
      // Try parsing the expression, & read more input if unsuccessful 
      try {
        // Return AST if successfully parsed an expression
        parse_input_exp(input+'\n'+tmp_buffer,abstract_syntax_tree);
        return abstract_syntax_tree;
      } catch(const READER_ERROR& read_error) {
        // Continue parsing the current expression/string if incomplete
        if(is_non_repl_reader_error(read_error)) {
          input += '\n'+tmp_buffer;
          if(read_error == READER_ERROR::incomplete_expression) 
            indent_input(outs);
        // Alert user if read an invalid expression, then reprompt for input
        } else {
          if(!input.empty()) alert_reader_error(outs,read_error,input+'\n'+tmp_buffer);
          else               alert_reader_error(outs,read_error,tmp_buffer);
          fputc('\n', outs);
          if(in_repl) announce_input(outs);
          abstract_syntax_tree.clear(), input.clear();
        }
      // Alert user if detected unparsable input (-:- ANOMALY -:-)
      } catch(const size_type& read_error_index) {
        if(!input.empty()) alert_reader_error(outs,read_error_index,input+'\n'+tmp_buffer);
        else               alert_reader_error(outs,read_error_index,tmp_buffer);
        fputc('\n', outs);
        if(in_repl) announce_input(outs);
        abstract_syntax_tree.clear(), input.clear();
      }
    }
    return abstract_syntax_tree;
  }
} // End of namespace heist


// Account for whether REPL should print a newline
#ifndef CPP_INTEROP_HPP_ // @NOT-EMBEDDED-IN-C++
#ifndef HEIST_INTERPRETING_COMPILED_AST // @ONLY-INTERPRETER
void print_repl_newline(const bool& printed_data)noexcept{ // after printing data
  if(printed_data || (!heist::G.LAST_PRINTED_NEWLINE_TO_STDOUT && heist::G.LAST_PRINTED_TO_STDOUT))
    putchar('\n');
  heist::G.LAST_PRINTED_NEWLINE_TO_STDOUT = heist::G.LAST_PRINTED_TO_STDOUT = false;
}

void print_repl_newline()noexcept{ // after printing an error
  putchar('\n'), heist::G.LAST_PRINTED_NEWLINE_TO_STDOUT=heist::G.LAST_PRINTED_TO_STDOUT=false;
}

void account_for_whether_printed_data(const heist::data& val,bool& printed_data)noexcept{
  printed_data = !val.is_type(heist::types::dne);
}


// Print output object
void user_print(FILE* outs, heist::data& object) {
  fputs(object.pprint().c_str(), outs);
  fflush(outs);
}


// Determine if REPL received the EOF signal to terminate interpretation
bool repl_detected_EOF_signal(const heist::scm_list& AST)noexcept{
  return AST.size() == 1 && AST[0].is_type(heist::types::chr) && AST[0].chr == EOF;
}


// Wrap each entry in "scm->cps" (w/ "id" bound as the topmost cont.) 
//   if "-cps" cmd-line flag passed
void cpsify_inputs(heist::scm_list& AST) {
  const heist::size_type n = AST.size();
  heist::scm_list CPS_AST(n);
  for(heist::size_type i = 0; i < n; ++i) {
    CPS_AST[i] = heist::scm_list(2);
    CPS_AST[i].exp[0] = heist::scm_list(2);
    CPS_AST[i].exp[0].exp[0] = heist::symconst::scm_cps;
    CPS_AST[i].exp[0].exp[1] = AST[i];
    CPS_AST[i].exp[1] = "id";
  }
  AST = CPS_AST;
}


// Returns (begin (define #it <d>) #it)
heist::data repl_tag_expression(const heist::data& d)noexcept{
  heist::data tag_exp = heist::exp_type(3);
  tag_exp.exp[0] = heist::symconst::begin;
  tag_exp.exp[1] = heist::exp_type(3);
  tag_exp.exp[1].exp[0] = heist::symconst::define;
  tag_exp.exp[1].exp[1] = "#it";
  tag_exp.exp[1].exp[2] = d;
  tag_exp.exp[2] = "#it";
  return tag_exp;
}


int driver_loop() {
  bool printed_data = true;
  print_repl_newline(printed_data);
  for(;;) {
    heist::announce_input(stdout);
    auto AST = heist::read_user_input(stdout,stdin); // AST = Abstract Syntax Tree
    // Handle EOF Signal
    if(repl_detected_EOF_signal(AST)) {
      puts("\nAdios!"); 
      return heist::GLOBALS::HEIST_EXIT_CODE;
    }
    // Convert input to CPS as needed
    if(heist::G.USING_CPS_CMD_LINE_FLAG) cpsify_inputs(AST);
    // Eval each datum given
    for(const auto& input : AST) {
      try {
        auto value = heist::scm_eval(repl_tag_expression(input),heist::G.GLOBAL_ENVIRONMENT_POINTER);
        account_for_whether_printed_data(value,printed_data);
        user_print(stdout, value);
        print_repl_newline(printed_data);
      } catch(const heist::SCM_EXCEPT& eval_throw) {
        if(eval_throw == heist::SCM_EXCEPT::EXIT) { 
          if(!heist::GLOBALS::HEIST_EXIT_CODE) {
            puts("Adios!"); 
          } else {
            PRINT_ERR("HEIST SCHEME REPL TERMINATION: EXIT FAILURE (" << heist::GLOBALS::HEIST_EXIT_CODE << ")!");
            puts("");
          }
          return heist::GLOBALS::HEIST_EXIT_CODE; 
        }
        if(eval_throw == heist::SCM_EXCEPT::JUMP)
          PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
            << PROFILE(heist::GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
        print_repl_newline();
      } catch(...) {
        PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
             "\n     Triggered By: " << input << 
             "\n  => Please send your code to jrandleman@scu.edu to fix"
             "\n     the interpreter's bug!"
             "\n  => Terminating Heist Scheme Interpretation.");
        return 1;
      }
    }
  }
}

/******************************************************************************
* COMMAND LINE ARGUMENT VALIDATION
******************************************************************************/

void POPULATE_ARGV_REGISTRY(int argc,int& i,char* argv[])noexcept{
  while(i < argc) heist::GLOBALS::ARGV.push_back(heist::str_type(argv[i++]));
}


bool confirm_valid_non_negative_integer(const char* name, int& i, int argc, char* argv[], heist::size_type& result)noexcept{
  if(i == argc-1) {
    fprintf(stderr,"\n> \"%s\" wasn't followed by a non-negative integer!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n",name);
    return false;
  }
  auto num = heist::num_type(argv[++i]);
  if(!num.is_integer() || num.is_neg()) {
    fprintf(stderr,"\n> \"%s\" wasn't followed by a non-negative integer!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n",name);
    return false;
  }
  auto float_num = num.to_inexact();
  if(float_num < 0 || float_num > heist::GLOBALS::MAX_SIZE_TYPE) {
    fprintf(stderr,"\n> \"%s\" integer was out of range: [0, %zu]!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n",name,heist::GLOBALS::MAX_SIZE_TYPE);
    return false;
  }
  result = heist::size_type(float_num.extract_inexact());
  return true;
}


bool not_heist_cmd_line_flag(const std::string& next_cmd)noexcept{
return next_cmd != "-script" && next_cmd != "-compile" && next_cmd != "-l" && next_cmd != "-infix" && 
       next_cmd != "-cps" && next_cmd != "-nansi" && next_cmd != "-ci" && next_cmd != "-dynamic-call-trace" && 
       next_cmd != "-trace-args" && next_cmd != "-trace-limit" && next_cmd != "--version" && next_cmd != "--help";
}


bool confirm_valid_command_line_args(int argc,char* argv[],int& script_pos,
                                     int& compile_pos,std::string& compile_as,
                                     bool& immediate_exit, bool& trace_calls,
                                     std::vector<const char*>& LOADED_FILES)noexcept{
  if(argc == 1) return true;

  // Validate argument layout
  if(argc > 7) {
    fprintf(stderr, "\n> Invalid # of command-line args (given %d)!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n", argc-1);
    return false;
  }

  // Parse input arguments
  for(int i = 1; i < argc; ++i) {
    if(std::string cmd_flag(argv[i]); cmd_flag == "-ci") {
      heist::GLOBALS::USING_CASE_SENSITIVE_SYMBOLS = false;
    } else if(cmd_flag == "--version") {
      immediate_exit = true;
      puts("Heist Scheme Version 7.0\nTarget: " HEIST_EXACT_PLATFORM "\nInstalledDir: " HEIST_DIRECTORY_FILE_PATH);
      return true;
    } else if(cmd_flag == "--help") {
      immediate_exit = true;
      puts(HEIST_COMMAND_LINE_ARGS);
      return true;
    } else if(cmd_flag == "-infix") {
      LOADED_FILES.push_back(HEIST_DIRECTORY_FILE_PATH "/interpreter_headers/toolkits/infix_toolkit.scm");
    } else if(cmd_flag == "-dynamic-call-trace") {
      trace_calls = true;
    } else if(cmd_flag == "-trace-args") {
      heist::G.TRACE_ARGS = true;
    } else if(cmd_flag == "-nansi") {
      heist::G.USING_ANSI_ESCAPE_SEQUENCES = false;
    } else if(cmd_flag == "-cps") {
      heist::G.USING_CPS_CMD_LINE_FLAG = true;
    } else if(cmd_flag == "-trace-limit") {
      if(!confirm_valid_non_negative_integer("-trace-limit",i,argc,argv,heist::G.TRACE_LIMIT))
        return false;
    } else if(cmd_flag == "-l") {
      if(i == argc-1) {
        fprintf(stderr,"\n> \"-l\" wasn't followed by a file!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n");
        return false;
      }
      LOADED_FILES.push_back(argv[++i]);
    } else if(cmd_flag == "-script") {
      if(i == argc-1) {
        fprintf(stderr,"\n> \"-script\" wasn't followed by a file!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n");
        return false;
      } else if(compile_pos != -1) {
        fprintf(stderr,"\n> Can't interpret & compile files simultaneously!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n");
        return false;
      }
      script_pos = ++i;
      POPULATE_ARGV_REGISTRY(argc,i,argv);
    } else if(cmd_flag == "-compile") {
      if(i == argc-1) {
        fprintf(stderr,"\n> \"-compile\" wasn't followed by a file!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n");
        return false;
      }
      compile_pos = ++i;
      if(i < argc-1 && not_heist_cmd_line_flag(argv[i+1])) compile_as = argv[++i];
    } else {
      fprintf(stderr,"\n> Invalid command-line flag \"%s\"!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n",argv[i]);
      return false;
    }
  }
  return true;
}

/******************************************************************************
* INTERPRET SCRIPT HELPER FUNCTION
******************************************************************************/

int load_script(const char* filename){
  // Load the script & immediately exit
  heist::scm_list load_args(2 + heist::G.USING_CPS_CMD_LINE_FLAG);
  load_args[0] = heist::make_str(filename);
  load_args[1 + heist::G.USING_CPS_CMD_LINE_FLAG] = heist::G.GLOBAL_ENVIRONMENT_POINTER;
  // Bind "id" as the topmost continuation if "-cps" was passed
  if(heist::G.USING_CPS_CMD_LINE_FLAG)
    load_args[1] = heist::scm_fcn("id",(heist::prm_ptr_t)[](heist::scm_list& args){return args[0];});
  try {
    if(heist::G.USING_CPS_CMD_LINE_FLAG)
      heist::primitive_CPS_LOAD(load_args);
    else
      heist::primitive_LOAD(load_args);
  } catch(const heist::SCM_EXCEPT& eval_throw) {
    /* warn about uncaught <jump!> */
    if(eval_throw == heist::SCM_EXCEPT::JUMP)
      PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
        << PROFILE(heist::GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
    /* catch <exit> signal */
    if(eval_throw == heist::SCM_EXCEPT::EXIT) 
      return heist::GLOBALS::HEIST_EXIT_CODE;
    /* catch errors already output to stdout */ 
    putchar('\n');
    return 1;
  } catch(...) {
    /* catch uncaught C++ exceptions -:- ANOMALY -:- */
    PRINT_ERR(afmt(heist::AFMT_1) << 
      "\nUncaught C++ Exception Detected! -:- BUG ALERT -:-"
      "\n  => While interpreting script \"" << filename << "\""
      "\n  => Please send your code to jrandleman@scu.edu to fix"
      "\n     the interpreter's bug!"
      "\n  => Terminating Heist Scheme Interpretation.\n\n" << afmt(heist::AFMT_0));
    return 1;
  }
  return 0;
}

/******************************************************************************
* COMPILE SCRIPT HELPER FUNCTION
******************************************************************************/

int compile_script(char* argv[], const int& compile_pos, std::string& compile_as){
  // Compile the script & immediately exit
  heist::scm_list compile_args(2);
  compile_args[0] = heist::make_str(argv[compile_pos]);
  compile_args[1] = heist::make_str(compile_as);
  try {
    if(heist::G.USING_CPS_CMD_LINE_FLAG)
      heist::primitive_CPS_COMPILE(compile_args);
    else
      heist::primitive_COMPILE(compile_args);
  } catch(const heist::SCM_EXCEPT& eval_throw) {
    /* warn about uncaught <jump!> */
    if(eval_throw == heist::SCM_EXCEPT::JUMP)
      PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
        << PROFILE(heist::GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
    /* catch <exit> signal */
    if(eval_throw == heist::SCM_EXCEPT::EXIT) 
      return heist::GLOBALS::HEIST_EXIT_CODE;
    /* catch errors already output to stdout */ 
    putchar('\n');
    return 1;
  } catch(...) {
    /* catch uncaught C++ exceptions -:- ANOMALY -:- */
    PRINT_ERR(afmt(heist::AFMT_1) << 
      "\nUncaught C++ Exception Detected! -:- BUG ALERT -:-"
      "\n  => While compiling script \"" << argv[compile_pos] << "\""
      "\n  => Please send your code to jrandleman@scu.edu to fix"
      "\n     the interpreter's bug!"
      "\n  => Terminating Heist Scheme Interpretation.\n\n" << afmt(heist::AFMT_0));
    heist::close_port_registry();
    return 1;
  }
  heist::close_port_registry();
  return 0;
}

/******************************************************************************
* LOAD SCRIPT HELPER FUNCTION
******************************************************************************/

int load_scripts(const std::vector<const char*>& LOADED_FILES) {
  bool old_cps_val = heist::G.USING_CPS_CMD_LINE_FLAG;
  heist::G.USING_CPS_CMD_LINE_FLAG = false;
  for(auto filename : LOADED_FILES)
    if(int result = load_script(filename); result) {
      heist::close_port_registry();
      return result;
    }
  heist::G.USING_CPS_CMD_LINE_FLAG = old_cps_val;
  return 0;
}

/******************************************************************************
* MAIN INTERPRETER EXECUTION
******************************************************************************/

int main(int argc, char* argv[]) {
  // Validate arguments
  int script_pos = -1, compile_pos = -1;
  bool immediate_exit = false, trace_calls = false;
  std::string compile_as = heist::symconst::dflt_compile_name;
  std::vector<const char*> LOADED_FILES;
  if(!confirm_valid_command_line_args(argc,argv,script_pos,compile_pos,compile_as,
    immediate_exit,trace_calls,LOADED_FILES)) return 1;
  // "--version" & "--help" trigger an immediate exit
  if(immediate_exit) return 0;
  // Set up the environment (allocates & fills G.GLOBAL_ENVIRONMENT_POINTER)
  heist::set_default_global_environment();
  // Reset Stack Trace to ONLY Show User Invocations
  heist::GLOBALS::STACK_TRACE.clear();
  // Trace All Subsequent Calls (as needed)
  if(trace_calls) heist::G.TRACING_ALL_FUNCTION_CALLS = true;
  // Load Files (as needed)
  if(!LOADED_FILES.empty()) 
    if(int result = load_scripts(LOADED_FILES); result) return result;
  // Interpret a Script (as needed)
  if(script_pos != -1) {
    int result = load_script(argv[script_pos]);
    heist::close_port_registry();
    return result;
  }
  // Compile a Script (as needed)
  if(compile_pos != -1) 
    return compile_script(argv, compile_pos, compile_as);
  // Run the REPL
  puts("Heist Scheme Version 7.0\nEnter '(help)' for Help, '(exit)' to Exit");
  int result = driver_loop();
  heist::close_port_registry();
  return result;
}

/******************************************************************************
* MAIN COMPILED-AST EXECUTION
******************************************************************************/

#else // @ONLY-COMPILER
void POPULATE_ARGV_REGISTRY(int argc,int& i,char* argv[])noexcept{
  while(i < argc) heist::GLOBALS::ARGV.push_back(heist::str_type(argv[i++]));
}

int interpret_premade_AST_code(){
  heist::set_default_global_environment();
  heist::GLOBALS::STACK_TRACE.clear();
  POPULATE_HEIST_PRECOMPILED_READ_AST_EXPS();
  // => We wrap evaluated datums in "data" (*)HERE(*) to NOT mv the evaluated datum, 
  //    in case such needs to be shown in the error message upon an evaluation error.
  for(const auto& input : HEIST_PRECOMPILED_READ_AST_EXPS) {
    try {
      heist::scm_eval(heist::data(input),heist::G.GLOBAL_ENVIRONMENT_POINTER); // (*)HERE(*)
    } catch(const heist::SCM_EXCEPT& eval_throw) {
      if(eval_throw == heist::SCM_EXCEPT::EXIT) return heist::GLOBALS::HEIST_EXIT_CODE;
      if(eval_throw == heist::SCM_EXCEPT::JUMP)
        PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
          << PROFILE(heist::GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
    } catch(...) {
      PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
           "\n     Triggered By: " << input << 
           "\n  => Please send your source & compiled code to jrandleman@scu.edu to fix"
           "\n     the interpreter's bug!"
           "\n  => Terminating Heist Scheme Interpretation.");
      return 1;
    }
  }
  return 0;
}

int main(int argc, char* argv[]) {
  int i = 0;
  POPULATE_ARGV_REGISTRY(argc,i,argv);
  int result = interpret_premade_AST_code(); 
  heist::close_port_registry();
  return result;
}
#endif // @ONLY-COMPILER
#undef afmt
#undef HEIST_DIRECTORY_FILE_PATH
#undef HEIST_PLATFORM
#undef HEIST_EXACT_PLATFORM
#undef ERR_HEADER
#undef BAD_SYNTAX
#undef EXP_ERR
#undef FCN_ERR
#undef PROFILE
#undef PRINT_ERR
#undef THROW_ERR
#endif // @NOT-EMBEDDED-IN-C++
#undef HEIST_COMMAND_LINE_ARGS
#endif