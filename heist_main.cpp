// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_main.cpp
// => Main execution and AST evaluation for the C++ Heist Scheme Interpreter

#ifndef HEIST_MAIN_CPP_
#define HEIST_MAIN_CPP_

/***
 * COMPILE: 
 *   $ clang++ -std=c++17 -O3 -o heist_main heist_main.cpp
 *
 * FLAG DESCRIPTIONS:
 *   0. "-std=c++17": [REQUIRED] compile using the C++17 standard
 *   1. "-O3": [RECOMMENDED FOR FASTEST EXECUTION] maximum optimization
 *             -> longest compile time, but fastest runtime
 *   2. "-Os": [RECOMMENDED FOR MOST BUILDS] optimizes for binary's size
 *             -> faster compile-time than -O3, smaller binary, & close runtime
 *
 * ON COMPILE TIME:
 *   0. Full -O3 compilation takes about 30s. Be patient. Compilation
 *      time has been traded for FAST runtime.
 *   1. -Os compilation takes about 20s. Generated binary is smaller than
 *      -O3's (as expected) & its runtime is nearly as fast
 */

/******************************************************************************
* ABSOLUTE FILE PATH TO HEIST INTERPRETERS DIRECTORY: FOR THE compile PRIMITIVE
* => HINT: JUST USE WHAT "$ pwd" (OR "$ cd" ON WINDOWS) PRINTS!
******************************************************************************/

// #define HEIST_DIRECTORY_FILE_PATH "/Users/jordanrandleman/desktop/Heist-Scheme"

/******************************************************************************
* WARN USER IF COMPILING W/O AN ABSOLUTE FILE PATH
******************************************************************************/

#ifndef HEIST_DIRECTORY_FILE_PATH
  #warning NO FILE PATH IN "HEIST_DIRECTORY_FILE_PATH" MACRO OF "heist_main.cpp" DEFINED!
  #warning PRIMITIVE compile IS DISABLED (ENABLE BY PROVIDING "HEIST_DIRECTORY_FILE_PATH")!
#else
  #include <filesystem> // only used by "compile" primitive
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
 * WARNING: NO CALL/CC & TRANSCRIPTS
 *
 * NUMBER SYSTEM:
 *   - EXACT INTERGERS (UNBOUND)
 *   - EXACT FRACTIONS (> LDBL_DIG <cfloat> DIGITS = COLLAPSE TO INEXACT FLOAT)
 *   - INEXACT FLOATS  (LONG DOUBLE)
 *   - UNSUPPORTED NUMERICS:
 *     > NUMBER::RECTANGULAR
 *     > NUMBER::POLAR
 *     > NUMBER::COMPLEX
 *     > S F D L INEXACT PRECISIONS (ALWAYS LDBL_DIG DIGITS)
 *
 * CHARS: USE ASCII ENCODING
 *
 * R4RS EXTENSIONS:
 *   - OPT-IN DYNAMIC SCOPING  ; "INLINE" FUNCTION APPLICATIONS
 *   - NATIVE EVEN STREAMS     ; LISTS WITH DELAYED CAR & CDR
 *   - GENERIC ALGORITHMS      ; POLYMORPHIC ALGORITHM PRIMITIVES
 *   - SFRI PRIMITIVES         ; LIST, VECTOR, STRING, ETC.
 *   - EVAL                    ; EVALUATE SYMBOLIC DATA AS CODE
 *   - HYGIENIC MACROS         ; MATCH SYMBOL-LIST PATTERNS TO TEMPLATES
 *     > SYNTAX-RULES
 *     > DEFINE-SYNTAX
 *     > LET-SYNTAX
 *     > LETREC-SYNTAX
 *   - STRING I/O              ; READ/WRITE COMPATIBILITY W/ STRINGS AS PORTS
 *   - RECURSIVE DEPTH CONTROL ; SET THE INTERPRETER'S MAX RECURSION DEPTH
 *   - VECTOR-LITERAL          ; LONG-HAND VARIANT OF THE #( PREFIX
 *   - AND MORE!
 */

/***
 * RESERVED WORDS -VS- SPECIAL FORMS -VS- PRIMITIVES: 
 *   (A) RESERVED WORDS:
 *       - DEFINITION: IMPLEMENTATION-SPECIFIC RESERVED INTERNAL OBJECT NAMES
 *       - PROPERTIES: 1) ***RESERVED SYMBOLS BEGIN WITH "__HEIST-"***
 *                     2) ***USE BY USER IS UNDEFINED BEHAVIOR***
 *
 *   (B) SPECIAL FORMS:
 *       - DEFINITION: EXPLICIT KEYWORDS PARSED FOR BY THE INTERPRETER
 *       - PROPERTIES: 1) ***REDEFINITION BY USER IS UNDEFINED BEHAVIOR***
 *                     2) SPECIAL FORM ARGS ARE NOT EVALUATED PRIOR APPLICATION
 *                     3) USERS CAN DEFINE THEIR OWN SPECIAL FORMS VIA MACROS
 *       - EXAMPLES:
 *         * '                ; quote
 *         * `                ; quasiquote
 *         * ,                ; unquote
 *         * ,@               ; unquote-splicing
 *         * .                ; VARIADIC ARGS, cons LITERAL
 *         * #\               ; CHARACTER PREFIX
 *         * =>               ; APPLY CONDITION RESULT (FOR cond)
 *         * quote            ; SYMBOLIZE ARGS (CONVERT CODE TO DATA)
 *         * quasiquote       ; SELECTIVELY eval AND SYMBOLIZE CODE
 *         * unquote          ; eval quasiquote CODE
 *         * unquote-splicing ; eval AND SPLICE IN quasiquote CODE'S RESULT
 *         * define-syntax    ; MACRO DEFINITION
 *         * let-syntax       ; LOCALLY-SCOPED MACRO DEFINITION
 *         * letrec-syntax    ; LOCALLY-SCOPED RECURSIVE MACRO DEFINITION
 *         * syntax-rules     ; SYNTAX OBJECT
 *         * lambda           ; ANONYMOUS PROCEDURE
 *         * define           ; BIND VARIABLE TO VALUE
 *         * set!             ; ASSIGN VARIABLE A NEW VALUE (MUTATATION)
 *         * begin            ; SEQUENTIALLY eval ARGS
 *         * delay            ; DELAY ARG eval (RETURNS PROMISE)
 *         * if               ; ARG1 ? ARG2 : ARG3
 *         * and              ; ALL ARGS ARE TRUE
 *         * or               ; NOT ALL ARGS ARE FALSE
 *         * cond             ; ALTERNATIVE TO NESTED IF-ELSE CHAINS
 *         * case             ; SWITCH-STATEMENT EQUIVALENT IN HEIST SCHEME
 *         * let              ; LOCALLY-SCOPED DEFINITIONS
 *         * let*             ; let WITH BINDINGS IN TERMS OF ONE ANOTHER
 *         * letrec           ; let WITH RECURSIVE BINDINGS
 *         * do               ; ITERATION CONSTRUCT ('LOOP' MECHANISM)
 *         * scons            ; STREAM-PAIR CONSTRUCTION
 *         * stream           ; STREAM CONSTRUCTION
 *         * vector-literal   ; LONGHAND OF #( PREFIX
 *
 *    (C) PRIMITIVES:
 *       - DEFINITION: C++ FUNCTIONS DEFINED IN THE HEIST GLOBAL ENVIRONMENT
 *       - PROPERTIES: 1) ***MAY BE REDEFINED BY USER AT RUN TIME***
 *                     2) MAY BE TREATED AS IF ANY OTHER HEIST PROCEDURE
 *       - EXAMPLES: 
 *         * (exit)            ; TERMINATE THE INTERPRETER
 *         * #t                ; TRUE BOOLEAN VALUE
 *         * #f                ; FALSE BOOLEAN VALUE
 *         * stream-null       ; EMPTY STREAM OBJECT
 *         * null-environment  ; eval IN DISJOINT GLOBAL ENVIRONMENT (FOR eval)
 *         * local-environment ; eval IN LOCAL SCOPE (FOR eval)
 *         * SEE "heist_primitives.hpp" FOR THE ALL PRIMITIVE IMPLEMENTATIONS
 */

#include "heist_interpreter_headers/heist_types.hpp"
#include "heist_interpreter_headers/heist_primitives.hpp"
#include "heist_interpreter_headers/heist_input_parser.hpp"

namespace heist {

  scm_list scm_eval(scm_list&& exp, env_type& env);
  exe_type scm_analyze(scm_list&& exp,const bool tail_call=false);

  /******************************************************************************
  * AST-ANALYSIS HELPER FUNCTIONS
  ******************************************************************************/

  // Confirm whether list begins w/ designated symbol
  bool is_tagged_list(const scm_list& exp, const char* const tag)noexcept{
    return !exp.empty() && exp[0].is_type(types::sym) && exp[0].sym == tag;
  }


  // 'Casts' a data object to a scm_list object
  //   => If data = argless application, adds sentinel-arg
  //   => If data contains a scm_list, degrades data to scm_list
  //   => Else, wraps data in a scm_list
  scm_list scm_list_cast(const data& d)noexcept{
    // if at an argless application (ie a single entity in an expression)
    if(d.is_type(types::exp) && d.exp.size()==1) {
      scm_list argless_app(2); // add sentinel-arg to argless application
      argless_app[0] = d.exp[0], argless_app[1] = scm_list(2);
      argless_app[1].exp[0] = symconst::quote;
      argless_app[1].exp[1] = symconst::sentinel_arg;
      return argless_app;
    }
    if(d.is_type(types::exp)) return d.exp;
    return scm_list(1,d);
  }


  // 'Casts' a scm_list object to a data object
  //   => If scm_list only contains 1 data object, degrades scm_list to the data
  //   => Else, wraps scm_list in a datum
  data data_cast(const scm_list& l)noexcept{
    if(l.size() == 1) return l[0]; 
    return data(l);
  }


  // Generate a call signature from a procedure name & its given values
  sym_type procedure_call_signature(const sym_type& name,const frame_vals& vals)noexcept{
    if(no_args_given(vals) || data_is_the_SENTINEL_VAL(data(vals)))
      return '(' + name + ')';
    return '(' + name + ' ' + cio_expr_str<&data::write>(vals).substr(1);
  }


  // Generate an improper procedure call error message
  sym_type improper_call_alert(sym_type name, const frame_vals& vals,
                                              const frame_vars& vars)noexcept{
    if(name.empty()) name = " #<procedure>"; // anonymous lambda
    name.erase(0,1);                         // rm initial ' '
    // Generate the call signature
    auto call_signature = procedure_call_signature(name,vals);
    // Generate the definition signature (w/o sentinel arg)
    sym_type defn_signature('(' + name);
    if(vars.size() != 1 || vars[0] != symconst::sentinel_arg)
      for(const auto& var : vars) defn_signature += ' ' + var;
    // Return the comparison between the called & defined procedure signatures
    return '\n' + scm_string(afmt(AFMT_35)) + "  >> Invalid Syntax:" + 
                  scm_string(afmt(AFMT_01)) + ' ' + call_signature + 
           '\n' + scm_string(afmt(AFMT_35)) + "  >> Defined Syntax:" + 
                  scm_string(afmt(AFMT_01)) + ' ' + defn_signature + ')';
  }

  /******************************************************************************
  * AST DATA VALIDATION HELPER FUNCTIONS
  ******************************************************************************/

  // Throw an error if 'vars' contains duplicate or non-symbol arg names, 
  //   or improper (.) use
  void confirm_valid_procedure_parameters(const scm_list& vars,const scm_list& exp){
    const size_type n = vars.size();
    // variadic (.) arg must have a label afterwards
    if(n != 0 && vars[n-1].sym == ".")
      THROW_ERR("Expected one item after variadic dot (.)! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
    // Search the vars list of the fcn's args for improper (.) use & duplicate arg names
    for(size_type i = 0; i < n; ++i) {
      if(!vars[i].is_type(types::sym)) // args must be symbols
        THROW_ERR("Non-Symbolic parameter [ "<<vars[i]<<" ] is an invalid arg name! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
      if(i+2 != n && vars[i].sym == ".") // variadic (.) must come just prior the last arg
        THROW_ERR("More than one item found after variadic dot (.)! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
      for(size_type j = i+1; j < n; ++j)
        if(vars[i].sym == vars[j].sym) // duplicate arg name detected
          THROW_ERR("Duplicate arg name \""<<vars[i]<<"\" supplied! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
    }
  }


  // Confirm valid argument layout for variable assignment
  void confirm_valid_assignment(const scm_list& exp) {
    if(exp.size() != 3)
      THROW_ERR("'set! didn't receive 2 arguments: (set! <var> <val>)" << EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'set! 1st arg " << PROFILE(exp[1]) << " can't be reassigned"
        " (only symbols)!\n     (set! <var> <val>)" << EXP_ERR(exp));
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
    if(!exp[1].is_type(types::exp) && !primitive_IS_THE_EMPTY_LIST(exp[1]))
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
  void transform_variadic_vals_into_a_list(frame_vars& vars,frame_vals& vals)noexcept{
    const size_type va_arg_idx = vars.size()-2;
    // Transform the arg names & vals as needed
    vars[va_arg_idx] = vars[va_arg_idx+1]; // shift up variadic arg name (erasing '.')
    vars.erase(vars.end()-1, vars.end());  // erase the now-duplicate var-arg name
    data list_of_vals;
    if(no_args_given(vals))
      list_of_vals = symconst::emptylist;
    else
      list_of_vals = primitive_LIST_to_CONS_constructor(vals.begin()+va_arg_idx, vals.end());
    vals.erase(vals.begin()+va_arg_idx, vals.end()); // erase individual arg instances
    vals.push_back(list_of_vals);                    // reinsert args as a list
  }


  // Confirm given no args & NOT applying a void-arg fcn & NOT a variadic-arg fcn
  bool invalid_sentinel_arg_use(const frame_vars& vars,const frame_vals& vals)noexcept{
    return no_args_given(vals) &&
            !(vars.size()==1 && vars[0]==symconst::sentinel_arg) && 
            !(vars.size()==2 && vars[0][0] == '.' && !vars[0][1]);
  }


  // Confirm passing an arg to an argless procedure
  bool given_arg_for_argless_fcn(const frame_vars& vars,const frame_vals& vals)noexcept{
    return !no_args_given(vals) && vars.size()==1 && vars[0]==symconst::sentinel_arg;
  }


  // Wrapper composing the above helpers
  bool confirm_valid_environment_extension(frame_vars& vars, frame_vals& vals, 
                                                       const sym_type& name){
    // Confirm proper use of the SENTINEL_ARG
    if(invalid_sentinel_arg_use(vars,vals))
      THROW_ERR("Too few arguments supplied! -- EXTEND_ENVIRONMENT" 
        << improper_call_alert(name,vals,vars));
    if(given_arg_for_argless_fcn(vars,vals))
      THROW_ERR("Too many arguments supplied! -- EXTEND_ENVIRONMENT" 
        << improper_call_alert(name,vals,vars));
    // Transform variadic arg's corresponding values into a list (if present)
    if(vars.size() > 1 && vars[vars.size()-2] == ".") 
      transform_variadic_vals_into_a_list(vars,vals);
    return vars.size() == vals.size();
  }

  /******************************************************************************
  * ENVIRONMENT DATA STRUCTURE IMPLEMENTATION
  ******************************************************************************/

  // -- ENVIRONMENTAL GETTERS
  frame_vars& frame_variables(frame_t& f)noexcept{return std::get<0>(f);}
  frame_vals& frame_values(frame_t& f)   noexcept{return std::get<1>(f);}
  frame_macs& frame_macros(frame_t& f)   noexcept{return std::get<2>(f);}


  // -- ENVIRONMENTAL EXTENSION
  env_type extend_environment(frame_vars&& vars, frame_vals& vals, env_type& base_env, 
                                                             const sym_type& name = ""){
    // If valid extension, return environment w/ a new frame prepended
    if(confirm_valid_environment_extension(vars,vals,name)) {
      env_type extended_env(make_env());
      extended_env->push_back(make_frame(frame_t(vars,vals,frame_macs())));
      extended_env->insert(extended_env->end(), base_env->begin(), base_env->end());
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
    // Search Each Environment Frame
    for(size_type i = 0, total_frames = env->size(); i < total_frames; ++i) {
      // Get Variables & Values Lists of the current frame
      auto& [var_list, val_list, mac_list] = *env->operator[](i);
      // Search Variable Bindings List of the Frame
      for(size_type j = 0, total_vars = var_list.size(); j < total_vars; ++j)
        if(var == var_list[j]) {
          if(val_list[j].is_type(types::undefined))
            THROW_ERR("Unassigned Variable -- " << var);
          return val_list[j];
        }
    }
    THROW_ERR("Variable " << var << " is not bound!");
  }


  // -- VARIABLE SETTING: (set! <var> <val>)
  bool is_single_self_evaluating_object_in_list(frame_val& val)noexcept{
    return val.is_type(types::exp) && val.exp.size()==1;
  }
  void set_variable_value(const frame_var& var, frame_val&& val, env_type& env) {
    if(is_single_self_evaluating_object_in_list(val)) val = val.exp[0];
    // Search Each Environment Frame
    for(size_type i = 0, total_frames = env->size(); i < total_frames; ++i) {
      // Get Variables & Values Lists of the current frame
      auto& [var_list, val_list, mac_list] = *env->operator[](i);
      // Search Variable Bindings List of the Frame, Assign New Val if Found
      for(size_type j = 0, total_vars = var_list.size(); j < total_vars; ++j)
        if(var == var_list[j]) {
          val_list[j] = val; // var found, change val
          return;
        }
    }
    scm_list invalid_set_call(3);
    invalid_set_call[0] = symconst::set;
    invalid_set_call[1] = var, invalid_set_call[2] = val;
    THROW_ERR("Variable "<<var<<" is not bound!"<<EXP_ERR(invalid_set_call));
  }


  // -- VARIABLE DEFINITION: (define <var> <val>)
  void define_variable(const frame_var& var, frame_val val, env_type& env)noexcept{
    if(env->empty()) // add an empty frame to if given an empty environment
      env->push_back(make_frame(frame_t())); 
    if(is_single_self_evaluating_object_in_list(val)) val = val.exp[0];
    // Get Variables & Values Lists of the foremost frame
    auto& [var_list, val_list, mac_list] = *env->operator[](0);
    // Search Variable Bindings List of the Frame, Assign New Val if Found
    for(size_type j = 0, total_vars = var_list.size(); j < total_vars; ++j)
      if(var == var_list[j]) {
        val_list[j] = val; // var found, change val
        return;
      }
    // var not found, bind it to the foremost frame
    frame_variables(*env->operator[](0)).push_back(var);
    frame_values(*env->operator[](0)).push_back(val);
  }


  // -- MACRO DEFINITION: (define-syntax <label> <syntax-rules>)
  void define_syntax_extension(const frame_mac& mac, env_type& env)noexcept{
    if(env->empty()) // add an empty frame to if given an empty environment
      env->push_back(make_frame(frame_t())); 
    // macro list of the first frame
    auto& macs = frame_macros(*env->operator[](0)); 
    // if syntax is already defined, redefine it in the foremost frame
    for(auto& m : macs)
      if(m.label == mac.label) {
        m = mac;
        return;
      }
    // define the new syntax in the foremost frame & register its label
    macs.push_back(mac);
  }

  /******************************************************************************
  * REPRESENTING CONDITIONALS: (if <predicate> <consequent> <alternative>)
  ******************************************************************************/

  // -- IDENTIFICATION, GETTERS, & CONSTRUCTION
  bool     is_if(const scm_list& exp)  noexcept{return is_tagged_list(exp,symconst::if_t);}
  scm_list if_predicate(scm_list& exp) noexcept{return scm_list_cast(exp[1]);}
  scm_list if_consequent(scm_list& exp)noexcept{return scm_list_cast(exp[2]);}

  scm_list if_alternative(scm_list& exp)noexcept{
    if(exp.size() == 4) return scm_list_cast(exp[3]); // if has an <alternative>
    return VOID_DATA_EXPRESSION; // w/o <alternative> return VOID
  }

  scm_list make_if(const scm_list& predicate, const scm_list& consequent, 
                                              const scm_list& alternative)noexcept{
    scm_list if_exp(4);
    if_exp[0] = symconst::if_t, if_exp[1] = predicate;
    if_exp[2] = consequent, if_exp[3] = alternative;
    return if_exp;
  }


  // -- PREDICATE TESTING
  bool is_true(const scm_list& exp)noexcept{ // true is not false
    return !exp[0].is_type(types::bol) || exp[0].bol.val;
  }
  bool is_false(const scm_list& exp)noexcept{ // false is false
    return exp[0].is_type(types::bol) && !exp[0].bol.val;
  }


  // -- ANALYSIS
  // Returns lambda so that if true, only eval consequent: 
  //   else, only eval alternative
  exe_type analyze_if(scm_list& exp,const bool tail_call=false) { 
    if(exp.size() < 3) 
      THROW_ERR("IF didn't receive enough args:"
        "\n     (if <predicate> <consequent> <optional-alternative>)"
        << EXP_ERR(exp));
    if(exp.size() > 4) 
      THROW_ERR("IF received too many args:"
        "\n     (if <predicate> <consequent> <optional-alternative>)"
        << EXP_ERR(exp));
    auto pproc = scm_analyze(if_predicate(exp));
    auto cproc = scm_analyze(if_consequent(exp),tail_call);
    auto aproc = scm_analyze(if_alternative(exp),tail_call);
    return [pproc=std::move(pproc),cproc=std::move(cproc),
            aproc=std::move(aproc)](env_type& env){
      if(is_true(pproc(env))) 
        return cproc(env);
      return aproc(env);
    };
  }


  // -- AND: (and <condition1> <condition2> ...)
  bool is_and(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::and_t);}
  // Returns an exec proc to confirm whether all exp's are true
  exe_type analyze_and(scm_list& and_exp) {
    const size_type n = and_exp.size();
    // (and) = #t
    if(n == 1 || data_is_the_SENTINEL_VAL(and_exp[1])) 
      return [](env_type&){return scm_list(1,TRUE_DATA_BOOLEAN);};
    // Confirm all true
    return [n,and_exp=std::move(and_exp)](env_type& env){
      scm_list res;
      for(size_type i = 1; i < n; ++i) // args start at idx=1
        if(res = scm_eval(scm_list_cast(and_exp[i]),env); is_false(res))
          return scm_list(1,FALSE_DATA_BOOLEAN);
      return res;
    };
  }


  // -- OR: (or <condition1> <condition2> ...)
  bool is_or(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::or_t);}
  // Returns an exec proc to confirm whether >= 1 exp is true
  exe_type analyze_or(scm_list& or_exp) {
    const size_type n = or_exp.size();
    // (or) = #f
    if(n == 1 || data_is_the_SENTINEL_VAL(or_exp[1])) 
      return [](env_type&){return scm_list(1,FALSE_DATA_BOOLEAN);};
    // Confirm >= 1 true
    return [n,or_exp=std::move(or_exp)](env_type& env){
      for(size_type i = 1; i < n; ++i) // args start at idx=1
        if(auto res = scm_eval(scm_list_cast(or_exp[i]),env); is_true(res))
          return res;
      return scm_list(1,FALSE_DATA_BOOLEAN);
    };
  }

  /******************************************************************************
  * REPRESENTING SEQUENCES: (begin <body>)
  ******************************************************************************/

  bool is_begin(const scm_list& exp)   noexcept{return is_tagged_list(exp,symconst::begin);}
  scm_list begin_actions(scm_list& exp)noexcept{return scm_list(exp.begin()+1, exp.end());}

  // Analyzes each expression, then returns an exec proc which 
  //   sequentially invokes each expression's exec proc
  exe_type analyze_sequence(scm_list&& exps,const bool tail_call=false){ // used for 'begin' & lambda bodies
    if(exps.empty() || (exps.size()==1 && data_is_the_SENTINEL_VAL(exps[0])))
      return [](env_type&){return VOID_DATA_EXPRESSION;}; // void data
    const size_type n = exps.size();
    std::vector<exe_type> sequence_exe_procs(exps.size());
    // Analyze each expression
    for(size_type i = 0, n = exps.size(); i < n; ++i)
      sequence_exe_procs[i] = scm_analyze(scm_list_cast(exps[i]),(i+1==n)&&tail_call);
    // Return a lambda sequentially invoking each exec procedure
    return [n=std::move(n),sequence_exe_procs=std::move(sequence_exe_procs)]
    (env_type& env){
      for(size_type i = 0; i+1 < n; ++i)
        sequence_exe_procs[i](env);
      return sequence_exe_procs[n-1](env);
    };
  }

  // Convert a sequence into a single expression via 'begin
  scm_list convert_sequence_exp(scm_list seq)noexcept{ 
    scm_list begin_sequence(seq.size()+1); 
    begin_sequence[0] = symconst::begin;
    std::move(seq.begin(),seq.end(),begin_sequence.begin()+1);
    return begin_sequence;
  }

  /******************************************************************************
  * REPRESENTING ASSIGNMENT: (set! <var> <val>)
  ******************************************************************************/

  bool is_assignment(const scm_list& exp)       noexcept{return is_tagged_list(exp,symconst::set);}
  frame_var& assignment_variable(scm_list& exp) noexcept{return exp[1].sym;}
  scm_list assignment_value(const scm_list& exp)noexcept{return scm_list_cast(exp[2]);}

  // Analyzes value being assigned, & returns an execution procedure 
  //   to install it as the variable in the designated env
  exe_type analyze_assignment(scm_list& exp) { 
    confirm_valid_assignment(exp);
    auto& var       = assignment_variable(exp);
    auto value_proc = scm_analyze(assignment_value(exp));
    return [var=std::move(var),value_proc=std::move(value_proc)](env_type& env){
      set_variable_value(var,data_cast(value_proc(env)),env);
      return VOID_DATA_EXPRESSION; // return is undefined
    };
  }

  /******************************************************************************
  * MANGLING PROCEDURE NAMES: EMBEDDING A PROCEDURE'S NAME WITHIN ITS LAMBDA TAG
  ******************************************************************************/

  frame_var mangled_lambda_name_tag(const frame_var& proc_name)noexcept{
    return symconst::mangle_prefix + proc_name;
  }

  bool is_mangled_lambda_tag(const data& d)noexcept{
    return d.is_type(types::sym) && d.sym.find(symconst::mangle_prefix)==0;
  }

  frame_var demangled_lambda_tag(const data& tag)noexcept{
    static constexpr const size_type mangled_length = 
      sizeof(symconst::mangle_prefix)/sizeof(char)-1;
    if(is_mangled_lambda_tag(tag))
      return ' ' + tag.sym.substr(mangled_length);
    return ""; // anonymous procedure
  }

  /******************************************************************************
  * REPRESENTING DEFINITION: (define <var> <val>)
  ******************************************************************************/

  bool is_definition(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::define);}
  scm_list make_lambda(scm_list parameters, // Lambda ctor
                       scm_list body, 
                       const frame_var& lambda_tag = symconst::lambda)noexcept; 

  frame_var& definition_variable(scm_list& exp)noexcept{
    // if defining a variable, else defining a procedure
    if(exp[1].is_type(types::sym)) return exp[1].sym; 
    return exp[1].exp[0].sym;
  }

  scm_list definition_value(scm_list& exp,const frame_var& var_name)noexcept{
    // if defining a variable, else defining a procedure
    if(exp[1].is_type(types::sym)) return scm_list_cast(exp[2]); 
    scm_list args(exp[1].exp.begin()+1,exp[1].exp.end());
    scm_list body(exp.begin()+2,exp.end());
    return make_lambda(args,body,mangled_lambda_name_tag(var_name));
  }

  // Analyzes value being defined, & returns an execution procedure 
  //   to install it as the variable in the designated env
  exe_type analyze_definition(scm_list& exp) { 
    confirm_valid_definition(exp);
    auto& var       = definition_variable(exp);
    auto value_proc = scm_analyze(definition_value(exp,var));
    return [var=std::move(var),value_proc=std::move(value_proc)](env_type& env){
      define_variable(var,data_cast(value_proc(env)),env);
      return VOID_DATA_EXPRESSION; // return is undefined
    };
  }

  /******************************************************************************
  * REPRESENTING PROMISES: (delay <expression>)
  ******************************************************************************/

  bool is_delay(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::delay);}

  // 'Delay' data struct consists of the delayed expression, its environment, 
  //   whether its already been forced, and the result of forcing it (if forced)
  scm_list make_delay(const scm_list& exp, env_type& env)noexcept{
    scm_list del(2); del[0] = symconst::delay, del[1] = make_del(exp,env);
    return del;
  }

  // Extracts the delayed expression and returns an exec proc ctor'ing a promise
  exe_type analyze_delay(scm_list& exp) {
    if(exp.size() != 2 || data_is_the_SENTINEL_VAL(exp[1])) {
      exp[0].sym += ' '; // add space to tag, avoiding #<delay> degradation
      auto delay_call = cio_expr_str<&data::write>(exp);
      delay_call.erase(6,1);   // erase appended space
      THROW_ERR("'delay expects 1 argument: (delay <delay-expression>)" 
        << EXP_ERR(delay_call));
    }
    auto delay_list = scm_list_cast(exp[1]);
    return [delay_list=std::move(delay_list)](env_type& env){
      return make_delay(delay_list,env);
    };
  }

  /******************************************************************************
  * REPRESENTING STREAMS: (scons <stream-car> <stream-cdr>)
  ******************************************************************************/

  // -- scons: (scons <arg1> <arg2>) = (cons (delay <arg1>) (delay <arg2>))
  bool is_scons(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::scons);}

  exe_type analyze_scons(scm_list& exp) {
    if(exp.size() != 3)
      THROW_ERR("'scons expects 2 arguments: (scons <car> <cdr>)"<<EXP_ERR(exp));
    scm_list scons_exp(3); 
    scons_exp[0] = symconst::cons;
    scons_exp[1] = scm_list(2);
    scons_exp[1].exp[0] = symconst::delay;
    scons_exp[1].exp[1] = exp[1];
    scons_exp[2] = scm_list(2);
    scons_exp[2].exp[0] = symconst::delay;
    scons_exp[2].exp[1] = exp[2];
    return scm_analyze(std::move(scons_exp));
  }


  // -- stream: (stream <a> <b> ...) = (scons <a> (scons <b> ...))
  bool is_stream(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::stream);}

  exe_type analyze_stream(scm_list& exp)noexcept{
    if(exp.size() == 1 || data_is_the_SENTINEL_VAL(exp[1]))
      return [](env_type&){return EMPTY_LIST_EXPRESSION;};
    return [exp=std::move(exp)](env_type& env) mutable {
      return scm_list(1, primitive_STREAM_to_SCONS_constructor(exp.begin()+1,exp.end(),env));
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

  // Returns whether data is the (.) symbol
  bool data_is_dot_operator(const data& d)noexcept{
    return d.is_type(types::sym) && d.sym == ".";
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
      THROW_ERR("Unexpected dot (.) terminated the quoted list! -- ANALYZE_QUOTED"
        << FCN_ERR(quote_name,exp));
    // Iff exp begins w/ (.) & has a length of 2, it may be a valid instance of 
    //   quoting a variadic lambda [ ie '(lambda (. l) l) ] -- thus such is valid
    if(exp.size()==2 && data_is_dot_operator(exp[0]) && !data_is_dot_operator(exp[1]))
      return false;
    // Confirm no (.) prior the penultimate item in the list
    for(size_type i = 0, n = exp.size(); i+2 < n; ++i)
      if(data_is_dot_operator(exp[i]))
        THROW_ERR("Unexpected dot (.) at position #"<<i+1<<" in quotation! -- ANALYZE_QUOTED"
          << FCN_ERR(quote_name,exp));
    // Determine whether at a cons or a list
    if(exp.size() > 2 && data_is_dot_operator(*(exp.end()-2))) {
      exp.erase(exp.end()-2); // rm (.)
      return true; // cons
    }
    return false;  // list
  }


  // Analyzes the quote's vector literal & returns its execution procedure
  exe_type analyze_quoted_vector_literal(scm_list& exp) {
    scm_list args(exp.begin()+1,exp.end());
    if(is_quoted_cons(args, symconst::quote))
      THROW_ERR("'vector-literal had an unexpected dot (.)! -- ANALYZE_QUOTED_VECTOR_LITERAL"
        << EXP_ERR(exp));
    // return an empty vector if given no args
    if(no_args_given(args)) 
      return [](env_type&){return scm_list(1,make_vec(scm_list()));};
    // quote each item in the vector
    scm_list vector_literal(args.size()+1);
    vector_literal[0] = symconst::vector;
    for(size_type i = 0, n = args.size(); i < n; ++i) {
      vector_literal[i+1] = scm_list(2);
      vector_literal[i+1].exp[0] = symconst::quote;
      vector_literal[i+1].exp[1] = args[i];
    }
    // return analyzed vector
    return scm_analyze(std::move(vector_literal));
  }


  // Analyzes the quote's text & returns an execution procedure for such
  exe_type analyze_quoted(scm_list& exp) {
    if(exp.size() != 2 || data_is_the_SENTINEL_VAL(exp[1])) 
      THROW_ERR("'quote form expects one argument: (quote <quoted-data>)!"<<EXP_ERR(exp));
    
    // Quote vector literals as needed
    if(quoting_a_vector_literal(exp))
      return analyze_quoted_vector_literal(exp[1].exp);
    
    // Get quoted data
    auto quoted_data = text_of_quotation(exp);
    
    // If quoted data is atomic, return as-is
    if(!quoted_data.is_type(types::exp))
      return [quoted_data=std::move(quoted_data)](env_type&){
        return scm_list(1, quoted_data);
      };
    
    // If quoting an empty expression, return the empty list
    if(quoted_data.exp.empty())
      return [](env_type&){return EMPTY_LIST_EXPRESSION;};
    
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

  bool is_self_evaluating(const scm_list& exp)noexcept{
    return exp.size() == 1 && 
           (exp[0].is_type(types::num) || exp[0].is_type(types::str) || 
            exp[0].is_type(types::chr) || exp[0].is_type(types::par) || 
            exp[0].is_type(types::vec) || exp[0].is_type(types::bol) ||
            exp[0].is_type(types::syn) || exp[0].is_type(types::dne) || 
            exp[0].is_type(types::undefined));
  }

  bool is_variable(const scm_list& exp)noexcept{return exp[0].is_type(types::sym);}

  /******************************************************************************
  * REPRESENTING PROCEDURES
  ******************************************************************************/

  // PROCEDURE CONSTRUCTION
  scm_list make_procedure(const scm_list& parameters, const exe_type& body_proc,
                          env_type& env, const frame_var& name)noexcept{
    scm_list procedure_exp(6);
    procedure_exp[0] = symconst::procedure, procedure_exp[1] = parameters;
    procedure_exp[2] = body_proc,           procedure_exp[3] = env;
    procedure_exp[4] = make_cal(0),         procedure_exp[5] = name;
    return procedure_exp;
  }

  bool is_compound_procedure(const scm_list& p)    noexcept{return is_tagged_list(p,symconst::procedure);}
  exe_type& procedure_body(scm_list& p)            noexcept{return p[2].exe;}
  env_type& procedure_environment(scm_list& p)     noexcept{return p[3].env;}
  size_type& procedure_recursive_depth(scm_list& p)noexcept{return *p[4].cal;}

  frame_var procedure_name(const scm_list& p)noexcept{
    if(is_compound_procedure(p)) // compound procedure name
      return p[5].sym;
    return ' ' + p[2].sym; // primitive procedure name
  }

  frame_vars procedure_parameters(scm_list& p)noexcept{
    frame_vars var_names(p[1].exp.size());
    for(size_type i = 0, n = p[1].exp.size(); i < n; ++i)
      var_names[i] = p[1].exp[i].sym;
    return var_names;
  }


  // -- LAMBDAS: (lambda (<parameters>) <body>)
  bool is_lambda(const scm_list& exp)       noexcept{return is_tagged_list(exp,symconst::lambda) || 
                                                            is_mangled_lambda_tag(exp[0]);}
  scm_list  lambda_parameters(scm_list& exp)noexcept{return exp[1].exp;}
  scm_list  lambda_body(scm_list& exp)      noexcept{return scm_list(exp.begin()+2,exp.end());}
  frame_var lambda_name(scm_list& exp)      noexcept{return demangled_lambda_tag(exp[0]);}

  // Ctor for lambdas
  //   => NOTE: "lambda_tag" != "lambda" iff ctoring the value of a procedural
  //            definition, wherein the tag is also mangled w/ the procedure name
  scm_list make_lambda(scm_list parameters,scm_list body,const frame_var& lambda_tag)noexcept{
    scm_list new_lambda(body.size()+2); 
    new_lambda[0] = lambda_tag, new_lambda[1] = std::move(parameters);
    if(new_lambda[1].exp.empty()) // add the sentinel arg as needed
      new_lambda[1].exp.push_back(symconst::sentinel_arg);
    std::move(body.begin(), body.end(), new_lambda.begin()+2);
    return new_lambda;
  }

  // Returns an exec proc to mk a lambda w/ the analyzed parameter list & body
  exe_type analyze_lambda(scm_list& exp) {
    confirm_valid_lambda(exp);
    auto vars = lambda_parameters(exp);
    confirm_valid_procedure_parameters(vars,exp);            // validate parameters
    if(vars.empty()) vars.push_back(symconst::sentinel_arg); // add sentinel-arg
    auto body_proc = analyze_sequence(lambda_body(exp),true);// analyze body syntax
    auto name      = lambda_name(exp);
    return [vars=std::move(vars),body_proc=std::move(body_proc),name=std::move(name)]
      (env_type& env){
        return make_procedure(vars, body_proc, env, name);
      };
  }


  // -- PROCEDURAL APPLICATION
  bool is_application(const scm_list& exp)noexcept{return exp.size()>1;}
  scm_list operator_of(scm_list& exp)     noexcept{return scm_list_cast(exp[0]);}
  scm_list operands(scm_list& exp)        noexcept{return scm_list(exp.begin()+1, exp.end());}

  /******************************************************************************
  * DERIVING COND: (cond <clause1> ... <clauseN>)
  ******************************************************************************/

  bool is_cond(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::cond);}


  // -- CLAUSE VALIDATION
  void confirm_valid_clause(const scm_node& current_clause, const scm_list& exp){
    if(!current_clause->is_type(types::exp) || 
      current_clause->exp.empty()     || 
      data_is_the_SENTINEL_VAL(current_clause->exp[0]))
      THROW_ERR("Invalid COND clause [ " << *current_clause << " ], expects:"
        "\n     (cond <clause1> <clause2> ...)"
        "\n     <clause> = (<condition> <consequent>)  -- CONVERT_COND_IF"
        << EXP_ERR(exp));
  }


  // -- CLAUSE GETTERS
  scm_list cond_predicate(scm_node& clause)noexcept{
    if(clause->exp[0].is_type(types::exp))
      return clause->exp[0].exp; // expression
    scm_list cond_pred(2); 
    cond_pred[0] = symconst::and_t, cond_pred[1] = clause->exp[0];
    return cond_pred; // variable as an expression
  }

  scm_list cond_actions(scm_node& clause)noexcept{
    return convert_sequence_exp(scm_list(
            clause->exp.begin()+1,clause->exp.end()));
  }


  // -- CLAUSE ANALYSIS
  bool is_cond_else_clause(const scm_node& clause)noexcept{
    return is_tagged_list(clause->exp,symconst::else_t);
  }

  bool is_cond_arrow_clause(const scm_node& clause, const scm_list& exp) {
    const bool has_cond_arrow_notation = clause->exp.size() > 1 && 
                                         clause->exp[1].is_type(types::sym) && 
                                         clause->exp[1].sym == "=>";
    if(has_cond_arrow_notation && clause->exp.size() != 3)
      THROW_ERR("COND Clause '=> Notation didn't receive 3 args:"
        "\n     (cond <clause1> <clause2> ...)"
        "\n     (<condition> => <procedure>)  -- CONVERT_COND_IF"
        << EXP_ERR(exp));
    return has_cond_arrow_notation;
  }

  data cond_arrow_procedure(scm_node& clause)noexcept{
    return clause->exp[2];
  }


  // -- CLAUSE EXPANSION
  // Each clause consists of a <condition> & a <body>
  scm_list expand_clauses(scm_node& current_clause, const scm_node& end_clauses, const scm_list& exp) {
    // no else clause -- return is now implementation-dependant, I've chosen VOID
    if(current_clause == end_clauses) {
      scm_list void_exp(2); 
      void_exp[0] = symconst::and_t, void_exp[1] = VOID_DATA_OBJECT;
      return void_exp;
    }
    confirm_valid_clause(current_clause,exp);

    // Set <test> as <consequent> if only given a <test> in the clause
    if(current_clause->exp.size() == 1)
      current_clause->exp.push_back(current_clause->exp[0]);

    // Convert COND into a series of cascading 'if's
    scm_node rest_clauses = current_clause+1;
    if(is_cond_else_clause(current_clause)) {
      if(rest_clauses == end_clauses)
        return cond_actions(current_clause);
      else
        THROW_ERR("ELSE clause isn't last -- CONVERT_COND_IF\n     Total clauses after ELSE: " 
          << std::distance(current_clause,end_clauses)-1
          << "\n     (cond <clause1> <clause2> ...)"
          << EXP_ERR(exp));
    } else if(is_cond_arrow_clause(current_clause,exp)) {
      scm_list cond_arrow_application(2);
      cond_arrow_application[0] = cond_arrow_procedure(current_clause);
      cond_arrow_application[1] = cond_predicate(current_clause);
      return make_if(cond_predicate(current_clause), 
                     cond_arrow_application,
                     expand_clauses(rest_clauses,end_clauses,exp));
    } else {
      return make_if(cond_predicate(current_clause),
                     cond_actions(current_clause),
                     expand_clauses(rest_clauses,end_clauses,exp));
    }
  }


  // Expands 'cond into a series of nested 'if
  scm_list convert_cond_if(scm_list& exp) {
    if(exp.size() < 2 || data_is_the_SENTINEL_VAL(exp[1]))
      THROW_ERR("Invalid COND expression, NO CLAUSES GIVEN:"
        "\n     (cond <clause1> <clause2> ...)"
        "\n     <clause> = (<condition> <consequent>)"
        << EXP_ERR(exp));
    scm_node start_of_clauses = exp.begin()+1;
    const scm_node end_of_clauses = exp.end();
    return expand_clauses(start_of_clauses,end_of_clauses,exp);
  }

  /******************************************************************************
  * DERIVING LET: (let ((<var1> <val1>) ... (<varN> <valN>)) <body>)
  ******************************************************************************/

  // (let ((<var1> <val1>) ... (<varN> <valN>)) <body>)

  // ((lambda (<var1> ... <varN>) <body>) <val1> ... <valN>)

  bool     is_let(const scm_list& exp)        noexcept{return is_tagged_list(exp,symconst::let);}
  bool     is_named_let(const scm_list& exp)  noexcept{return exp[1].is_type(types::sym);}
  sym_type named_let_name(const scm_list& exp)noexcept{return exp[1].sym;}
  scm_list let_parameters(scm_list& exp)      noexcept{return exp[1].exp;}
  scm_list named_let_parameters(scm_list& exp)noexcept{return exp[2].exp;}
  scm_list let_body(scm_list& exp)            noexcept{return scm_list(exp.begin()+2,exp.end());}
  scm_list named_let_body(scm_list& exp)      noexcept{return scm_list(exp.begin()+3,exp.end());}


  // PARAMETERS = (<varN> <valN>) OF 'let
  // Return list of parameter <var> names
  scm_list let_variables(const scm_list& parameters)noexcept{
    scm_list vars(parameters.size()); // return var names
    for(size_type i = 0, n = parameters.size(); i < n; ++i)
      vars[i] = parameters[i].exp[0];
    return vars;
  }

  // Return list of parameter <val> values
  scm_list let_expressions(const scm_list& parameters)noexcept{
    // returns variable values (ie their assinged expressions) of parameters
    scm_list exps(parameters.size());
    for(size_type i = 0, n = parameters.size(); i < n; ++i)
      exps[i] = parameters[i].exp[1];
    return exps;
  }


  // Convert 'let to a 'lambda
  scm_list convert_let_combination(scm_list exp) {
    if(exp.size() < 3)
      THROW_ERR("Invalid LET expression, didn't received 2 args:"
        "\n     (let (<var-bindings>) <body>)"
        "\n     <var-binding> = (<name> <value>)" << EXP_ERR(exp));
    const bool named = is_named_let(exp);
    if(named && exp.size() < 4)
      THROW_ERR("Invalid NAMED LET expression, didn't received 3 args:"
        "\n     (let <name> (<var-bindings>) <body>)"
        "\n     <var-binding> = (<name> <value>)" << EXP_ERR(exp));

    // list of var-val pairs
    scm_list params = (named ? named_let_parameters : let_parameters)(exp);

    // add the sentinel arg as a var name, if 'let' was given no parameters
    if(params.empty()) {
      params.push_back(scm_list(2)); // () => ((SENTINTEL_ARG, SENTINTEL_VAL))
      params[0].exp[0] = symconst::sentinel_arg;
      params[0].exp[1] = scm_list(2);
      params[0].exp[1].exp[0] = symconst::quote;
      params[0].exp[1].exp[1] = symconst::sentinel_arg;
    }

    // retreive let expressions & body
    auto exprs             = let_expressions(params);
    auto let_lambda_object = data(make_lambda(let_variables(params),(named ? named_let_body : let_body)(exp)));

    // convert let into a lambda (both named & unnamed)
    if(named) {
      auto let_name_symbol = named_let_name(exp);
      scm_list lambda_defn(3);
      lambda_defn[0] = symconst::define;
      lambda_defn[1] = let_name_symbol;
      lambda_defn[2] = std::move(let_lambda_object);
      scm_list self_invocation(exprs.size()+1);
      self_invocation[0] = std::move(let_name_symbol);
      // push back each expression as an arg for the self-invoked-lambda call
      std::move(exprs.begin(), exprs.end(), self_invocation.begin()+1);
      // bind lambda to name & immediately invoke
      scm_list let_exp(2); 
      let_exp[0] = std::move(lambda_defn), let_exp[1] = std::move(self_invocation);
      return convert_sequence_exp(let_exp); 
    } else {
      scm_list nameless_self_invoke(exprs.size()+1);
      nameless_self_invoke[0] = std::move(let_lambda_object);
      // push back each expression as an arg for the self-invoked-lambda call
      std::move(exprs.begin(), exprs.end(), nameless_self_invoke.begin()+1);
      // immediately invoke lambda w/ exps for vars
      return nameless_self_invoke; 
    }
  }

  /******************************************************************************
  * DERIVING LET*: (let* ((<var1> <val1>) ... (<varN> <valN>)) <body>)
  ******************************************************************************/

  // -- LET*: "LET", BUT VARIABLES CAN INVOKE ONE ANOTHER IN BINDINGS
  bool is_let_star(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::let_star);}

  scm_list make_let(const scm_list& variable_bindings,scm_list&& body)noexcept{
    scm_list let_exp(3);
    let_exp[0] = symconst::let, let_exp[1] = variable_bindings, let_exp[2] = body;
    return let_exp;
  }

  // Recursively nest lets, each containing one of let*'s parameters
  scm_list nest_lets(scm_node param, const scm_node& empty_param, scm_list& exp) {
    if(param == empty_param)
      return convert_sequence_exp(let_body(exp));
    else {
      return convert_let_combination( // transform into combination immediately
              make_let(scm_list(1,*param), nest_lets(param+1,empty_param,exp))
            );
    }
  }

  // Convert let* into a series of nested let's
  scm_list convert_let_star_nested_lets(scm_list& exp) {
    if(exp.size() < 3)
      THROW_ERR("Invalid LET* expression, didn't received 2 args:"
        "\n     (let* (<var-bindings>) <body>)"
        "\n     <var-binding> = (<name> <value>)" << EXP_ERR(exp));
    scm_list params = let_parameters(exp);
    if(params.empty()) 
      return convert_let_combination(exp);
    return nest_lets(params.begin(),params.end(),exp);
  }

  /******************************************************************************
  * DERIVING LETREC: (letrec ((<var1> <val1>) ... (<varN> <valN>)) <body>)
  ******************************************************************************/

  // -- LETREC: "LET" ENABLING RECURSIVE BINDINGS
  //            => IE "LET" W/ ALL VARS GUARENTEED TO BE EVAL'D SIMULTANEOUSLY
  //            => TRANSFORM INTO A LET ASSIGNING EACH VAL 2B <undefined>, THEN
  //               SETTING EACH VAL TO ITS VALUE 1ST THING W/IN THE LET'S BODY
  //               -> LOOKING UP AN <undefined> VARIABLE THROWS AN ERROR!
  bool is_letrec(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::letrec);}

  scm_list convert_letrec_let(scm_list& exp) {
    if(exp.size() < 3)
      THROW_ERR("Invalid LETREC expression, didn't received 2 args:"
        "\n     (letrec (<var-bindings>) <body>)"
        "\n     <var-binding> = (<name> <value>)" << EXP_ERR(exp));
    scm_list params = let_parameters(exp);
    // We convert a paramterless letrec directly into a let
    if(params.empty()) return convert_let_combination(exp);
    scm_list body = let_body(exp);
    scm_list vars = let_variables(params);
    scm_list vals = let_expressions(params);
    // Assign var default values to be undefined
    scm_list dflt_value_params(vars.size()), set_var_undef(2);
    set_var_undef[1] = data();
    for(size_type i = 0, n = vars.size(); i < n; ++i) {
      set_var_undef[0] = vars[i]; // var name bound to undefined data type
      dflt_value_params[i] = set_var_undef; 
    }
    // Define a new let, un-recursed
    scm_list unrec_let(vars.size()+body.size()+2);
    unrec_let[0] = symconst::let, unrec_let[1] = std::move(dflt_value_params);
    // Set the each var's value w/in the 'letrec's body
    for(size_type i = 0, n = vars.size(); i < n; ++i) {
      unrec_let[i+2] = scm_list(3);
      unrec_let[i+2].exp[0] = symconst::set;
      unrec_let[i+2].exp[1] = vars[i];
      unrec_let[i+2].exp[2] = vals[i];
    }
    // Append elts in the body, now post-assignment
    std::move(body.begin(), body.end(), unrec_let.begin()+vars.size()+2);
    return convert_let_combination(unrec_let);
  }

  /******************************************************************************
  * DERIVING CASE
  ******************************************************************************/

  // CASE => (case <val> ((<keys1>) <exp1>) ... (else <expN>))
  //      => (cond ((memv <val> <keys1>) <exp1>) ... (else <expN>))
  bool is_case(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::case_t);}

  // Confirm 'case clause is correctly formatted
  void confirm_valid_case_clause(const scm_node& clause,        const char* format, 
                                 const size_type& clause_count, const scm_list& exp) {
    // Confirm clause is an expression
    if(!clause->is_type(types::exp))
      THROW_ERR("'case clause #" << clause_count << " [ " << *clause << " ] isn't an expression!" 
        << format << EXP_ERR(exp));
    // Confirm clause contains 2 elts
    if(clause->exp.size() != 2)
      THROW_ERR("'case clause #" << clause_count << " isn't an expression of length 2 (length = "
        << (!no_args_given(clause->exp) * clause->exp.size()) << ")!" 
        << format << EXP_ERR(exp));
    // Confirm clause was given a list of keys
    if(!is_cond_else_clause(clause) && !clause->exp[0].is_type(types::exp))
      THROW_ERR("'case clause #" << clause_count << " [ " << clause->exp[0]
        << " ] doesn't have a list of keys as its 1st arg!" << format << EXP_ERR(exp));
  }

  // Construct a 'cond equality clause from the 'case clause
  scm_list case_equality_clause(scm_node& clause, data& sought_val)noexcept{
    data keys_list(scm_list(clause->exp[0].exp.size()+1));
    keys_list.exp[0] = symconst::list;
    std::copy(clause->exp[0].exp.begin(), 
              clause->exp[0].exp.end(), 
              keys_list.exp.begin()+1);
    scm_list memv_exp(3);
    memv_exp[0] = symconst::memv, memv_exp[1] = sought_val;
    memv_exp[2] = std::move(keys_list);
    return memv_exp;
  }

  // Extract a normal & <else> 'case clause
  scm_list case_clause(scm_node& clause, data& sought_val)noexcept{
    scm_list clause_exp(2);
    clause_exp[0] = case_equality_clause(clause,sought_val);
    clause_exp[1] = cond_actions(clause);
    return clause_exp;
  }
  scm_list case_else_clause(scm_node& clause)noexcept{
    scm_list else_exp(2);
    else_exp[0] = symconst::else_t;
    else_exp[1] = data_cast(cond_actions(clause));
    return else_exp;
  }

  // Transform 'case into 'cond
  scm_list convert_case_cond(scm_list& exp) {
    static constexpr const char * const format = "\n     (case <val> <clause1> ... <clauseN>)"
                                                 "\n     <clause> = (<keys-list> <consequent>)";
    if(exp.size() < 3)
      THROW_ERR("CASE expression didn't receive enough args:" << format << EXP_ERR(exp));
    data& sought_val = exp[1];
    size_type clause_count = 1;
    scm_list converted_case(1,symconst::cond);
    for(auto clause=exp.begin()+2, null_clause=exp.end(); clause!=null_clause; ++clause, ++clause_count) {
      confirm_valid_case_clause(clause, format, clause_count, exp);
      if(is_cond_else_clause(clause)) {
        if(clause+1 == null_clause)
          converted_case.push_back(case_else_clause(clause));
        else
          THROW_ERR("CASE ELSE clause isn't last!\n     Total clauses after ELSE: " 
            << std::distance(clause,null_clause)-1 << format << EXP_ERR(exp));
      } else
        converted_case.push_back(case_clause(clause,sought_val));
    }
    return converted_case;
  }

  /******************************************************************************
  * DERIVING DO
  ******************************************************************************/

  // (do ((<var-name1> <init-val1> <val-manip1>) 
  //      ... 
  //      (<var-nameN> <init-valN> <val-manipN>))
  //     (<test> <expression1> ... <expressionN>)
  //     <body>)

  // (letrec ((<HEIST-DO-LETREC> 
  //   (lambda (<var-name1> ... <var-nameN>)
  //     (if <test>)
  //         (begin <expression1> ... <expressionN>) ; <void> w/o "<expression>"
  //         (begin 
  //           <body>
  //           (set! <var1> <val-manip1>)
  //           ...
  //           (set! <varN> <val-manipN>)
  //           (<HEIST-DO-LETREC> <var-name1> ... <var-nameN>)))))
  //   (<HEIST-DO-LETREC> <init-val1> ... <init-valN>))
  bool is_do(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::do_t);}

  // Confirms do-expression's var_defn_list is valid in structure
  void confirm_valid_var_defn_list(const scm_list& var_defn_list, const scm_list& exp) {
    for(const auto& var_defn : var_defn_list) {
      if(!var_defn.is_type(types::exp) || (var_defn.is_type(types::exp) && 
          var_defn.exp.size() != 2 && var_defn.exp.size() != 3))
        THROW_ERR("DO expression has non-var-defn-expression in <var-defn> list! -- CONVERT_DO_LETREC\n     -> [ "
                    << PROFILE(var_defn) << " ]\n     <var-defn> = (<var> <init-val> <optional-iteration-mutation>)"
                    << EXP_ERR(exp));
      else if(!var_defn.exp[0].is_type(types::sym))
        THROW_ERR("DO expression has an invalid <var> name in its <var-defn> list! -- CONVERT_DO_LETREC\n     -> [ "
                    << PROFILE(var_defn.exp[0]) 
                    << " ]\n     <var-defn> = (<var> <init-val> <optional-iteration-mutation>)" << EXP_ERR(exp));
    }
  }

  // Returns list of var-init-mod inner lists
  scm_list do_var_defn_list(const data& defn_exp, const scm_list& exp) {
    if(defn_exp.is_type(types::exp) && defn_exp.exp.empty())
      return scm_list(); // empty list, no vars to speak of
    if(!defn_exp.is_type(types::exp))
      THROW_ERR("DO expression expects <var-defn>s at position 1 -- CONVERT_DO_LETREC!\n     -> [ "
        << PROFILE(defn_exp) << " ]\n     <var-defn> = (<var> <init-val> <optional-iteration-mutation>)"
        << EXP_ERR(exp));
    confirm_valid_var_defn_list(defn_exp.exp, exp);
    return defn_exp.exp;
  }

  // Returns a list of variable names being defined
  // PRECONDITION: 'var_defn_list' MUST BE VALIDATED
  frame_vars do_var_names(const scm_list& var_defn_list, const scm_list& exp) {
    if(var_defn_list.empty()) {
      frame_vars no_vars(1,symconst::sentinel_arg); 
      return no_vars;
    }
    frame_vars names(var_defn_list.size());
    // parse variable names
    for(size_type i = 0, n = var_defn_list.size(); i < n; ++i)
      names[i] = var_defn_list[i].exp[0].sym;
    // confirm no duplicate variables names
    for(size_type i = 0, n = names.size(); i+1 < n; ++i)
      for(size_type j = i+1; j < n; ++j) 
        if(names[i] == names[j])
          THROW_ERR("DO expression has a duplicate <var> name in its <var-defn> list -- CONVERT_DO_LETREC!\n     -> [ " 
            << names[i] << " ]\n     <var-defn> = (<var> <init-val> <optional-iteration-mutation>)"
            << EXP_ERR(exp));
    return names;
  }

  // Returns a list of variabl initial values
  // PRECONDITION: 'var_defn_list' MUST BE VALIDATED
  scm_list do_var_init_values(const scm_list& var_defn_list)noexcept{
    if(var_defn_list.empty()) {
      scm_list empty_inits(1,scm_list(2));
      empty_inits[0].exp[0] = symconst::quote;
      empty_inits[0].exp[1] = symconst::sentinel_arg;
      return empty_inits;
    }
    scm_list init_values(var_defn_list.size());
    for(size_type i = 0, n = var_defn_list.size(); i < n; ++i)
      init_values[i] = var_defn_list[i].exp[1];
    return init_values;
  }

  // Returns a list of variable per-iteration value modifications
  // PRECONDITION: 'var_defn_list' MUST BE VALIDATED
  scm_list do_var_iteration_updates(const scm_list& var_defn_list, 
                                    const frame_vars& names)noexcept{
    if(var_defn_list.empty()) return scm_list();
    scm_list modifications(var_defn_list.size());
    // parse variable modifications
    for(size_type i = 0, n = var_defn_list.size(); i < n; ++i){
      if(var_defn_list[i].exp.size() != 3)
        modifications[i] = names[i];
      else
        modifications[i] = var_defn_list[i].exp[2];
    }
    return modifications;
  }


  // Returns a list breaking conditions for the do-expression
  scm_list do_break_test_exps(const data& break_exp, const scm_list& exp) {
    if(!break_exp.is_type(types::exp) || break_exp.exp.empty())
      THROW_ERR("DO expression expects <break-condition-list> at position 2 -- CONVERT_DO_LETREC! -> [ " 
        << PROFILE(break_exp) << " ]\n     <break-condition-list> = (<break-condition> <optional-returned-expression>)"
        << EXP_ERR(exp));
    return break_exp.exp;
  }


  // Returns the do-exp's body + variable modifications (per-iteration) wrapped in a 'begin' clause
  scm_list do_modified_body(const scm_list& exp, const frame_vars& names, 
                                                 const scm_list&& mod_vals)noexcept{
    // Add the do-expression's body
    scm_list body(exp.begin()+3, exp.end());
    // Add the modifications of its values (per-iteration)
    if(names.size() > 1 || names[0] != symconst::sentinel_arg) {
      scm_list set_exp(3); set_exp[0] = symconst::set;
      for(size_type i = 0, n = names.size(); i < n; ++i) {
        set_exp[1] = names[i], set_exp[2] = mod_vals[i];
        body.push_back(set_exp);
      }
    }
    // Add the recursive call to reiterate the do-expression
    body.push_back(scm_list(1,symconst::do_label));
    auto& recursive_call = body[body.size()-1].exp;
    for(const auto& name : names) 
      recursive_call.push_back(name);
    return convert_sequence_exp(body); // Return as a 'begin' clause
  }


  // Returns a lambda to run for each iteration over the loop, until >= 1 break 
  //   condition is met
  scm_list do_iteration_lambda(const scm_list& body, const scm_list&& break_test_exps, 
                                                     const frame_vars& names)noexcept{
    // test the break condition
    scm_list conditioned_body(2);
    conditioned_body[0] = symconst::if_t;
    conditioned_body[1] = break_test_exps[0];
    // eval each expression & return the last one (iff provided optional exps)
    if(break_test_exps.size() > 1) {
      conditioned_body.push_back(scm_list(1, symconst::begin));
      auto& break_exps = conditioned_body.rbegin()->exp;
      break_exps.insert(break_exps.end(), break_test_exps.begin()+1, break_test_exps.end());
    // if NOT given the optional exps, return <void>
    } else {
      conditioned_body.push_back(VOID_DATA_OBJECT); 
    }
    // if break !condition, evaluate the body & reset values
    conditioned_body.push_back(body);
    // convert list of names to a scm_list of name symbols
    scm_list names_scm_list(names.size());
    std::copy(names.begin(), names.end(), names_scm_list.begin());
    scm_list lambda_exp(3);
    lambda_exp[0] = symconst::lambda;
    lambda_exp[1] = std::move(names_scm_list);
    lambda_exp[2] = std::move(conditioned_body);
    return lambda_exp;
  }


  // Converts the 'do expression into a 'letrec
  scm_list convert_do_letrec(scm_list& exp) {
    if(exp.size() < 3)
      THROW_ERR("DO expression didn't receive enough args!"
        "\n     (do ((<var> <init-val> <optional-iteration-mutation>) ...)"
        "\n         (<test> <optional-return-expressions>)"
        "\n         <optional-body>)"
        << EXP_ERR(exp));

    auto var_defns = do_var_defn_list(exp[1],exp);
    auto var_names = do_var_names(var_defns,exp);
    auto var_inits = do_var_init_values(var_defns);
    auto iteration_lambda = do_iteration_lambda(do_modified_body(exp, var_names, 
                                                  do_var_iteration_updates(var_defns,var_names)), 
                                                do_break_test_exps(exp[2],exp), var_names);
    // do expression's intial call to be invoked
    scm_list initial_do_call(var_inits.size()+1);
    initial_do_call[0] = symconst::do_label;
    std::move(var_inits.begin(), var_inits.end(), initial_do_call.begin()+1);
    // ctor the do expression: (letrec ((do-label iteration-lambda)) initial-do-call)
    scm_list letrec_conversion(3);
    letrec_conversion[0] = symconst::letrec;
    letrec_conversion[1] = scm_list(1,scm_list(2));
    letrec_conversion[1].exp[0].exp[0] = symconst::do_label;
    letrec_conversion[1].exp[0].exp[1] = std::move(iteration_lambda);
    letrec_conversion[2] = std::move(initial_do_call);
    return letrec_conversion;
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
    data eval_result = data_cast(scm_eval(scm_list_cast(exp[1]),env));
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
                                      const bool& quoting_vector, const bool& not_last_elt) {
    static constexpr const char * const bad_vector = 
      "'quasiquote can't append [via ,@] an improper list to a vector!\n     Tried to splice in: ";
    static constexpr const char * const mid_splice = 
      "'quasiquote can't splice [via ,@] an improper list into the middle of a list!\n     Tried to splice in: ";
    // confirm not splicing a list_star/atomic into a vector nor mid-list
    if(quoting_vector && is_dotted_list)
      THROW_ERR(bad_vector<<"(list_star "<<spliceable_data[0]<<' '<<spliceable_data[1]<<')'<<EXP_ERR(exp));
    if(quoting_vector)
      THROW_ERR(bad_vector<<spliceable_data[0]<<" of type \""<<spliceable_data[0].type_name()<<'"'<<EXP_ERR(exp));
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
    const bool is_dotted_list = is_quoted_cons(quoted_exp,symconst::quasiquote);
    if(is_vector && is_dotted_list) 
      THROW_ERR("'quasiquote found unexpected dot (.) in 'vector-literal! -- ANALYZE_QUOTE_VECTOR"<<EXP_ERR(exp));
    if(is_vector) {
      quoted_exp.erase(quoted_exp.begin(),quoted_exp.begin()+1); // erase 'vector-literal tag
      quote_val.push_back(symconst::vector);
    } else if(is_dotted_list)
      quote_val.push_back(symconst::list_star);
    else
      quote_val.push_back(symconst::list);
  }


  // Recursively unquotes/unquote-splices data as needed w/in quasiquote templates.
  // Also quotes each piece of data as needed.
  void unquote_quasiquote_template(scm_list& quote_val, scm_list& quoted_exp, env_type& env, 
                                   const scm_list& exp, const size_type& nested_level){
    // Account for whether splicing into a vector
    bool quoting_a_vector = (quote_val[0].sym == symconst::vector);
    
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
                        (unsplice_stat == unsplice_status::list_star), quoting_a_vector, (i != n-1));
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
  exe_type analyze_quasiquote(scm_list& exp) {
    if(exp.size() != 2 || data_is_the_SENTINEL_VAL(exp[1]))
      THROW_ERR("'quasiquote expects one argument: (quasiquote <expression>)"<<EXP_ERR(exp));
    // Get quasiquoted data
    auto quoted_data = text_of_quotation(exp);
    
    // If quasiquoted data is atomic, return as-is
    if(!quoted_data.is_type(types::exp)) {
      return [unquoted_exp=scm_list(1,quoted_data)](env_type&){return unquoted_exp;};
    }

    // If quoting an empty expression, return the empty list
    if(quoted_data.exp.empty())
      return [](env_type&){return EMPTY_LIST_EXPRESSION;};

    // If quasiquoted an unquote, unpack its data
    if(is_unquote(quoted_data.exp)) {
      data unquoted_data = process_unquoted(quoted_data.exp);
      // If an expression, return the exec proc of the analyzed expression
      if(unquoted_data.is_type(types::exp))
        return scm_analyze(std::move(unquoted_data.exp));
      // If an atomic, return the exec proc of its evaluation (in case a variable or some such)
      return [unquoted_exp=scm_list(1, unquoted_data)](env_type& env)mutable{
        return scm_eval(std::move(unquoted_exp),env);
      };
    }

    // If quasiquoted an expression, expand such into a list/list_star of quoted data
    return [quoted_exp=std::move(quoted_data.exp),exp=std::move(exp)]
      (env_type& env) {
        // Unquote/Splice-In data as needed throughout the quasiquote template
        scm_list quote_val, mutable_quoted_exp = quoted_exp;
        // tag <quote_val> as needed w/ either 'vector, 'list_star, or 'list
        tag_quote_val(mutable_quoted_exp, quote_val, exp);
        // propagate quotations throughout the sub-expression
        unquote_quasiquote_template(quote_val,mutable_quoted_exp,env,exp,0);
        return scm_eval(std::move(quote_val),env);
      };
  }

  /******************************************************************************
  * REPRESENTING MACRO SYNTACTIC EXTENSIONS -- PATTERN MATCHING HELPER FUNCTIONS
  ******************************************************************************/

  // (define-syntax <label>
  //   (syntax-rules (<keywords>)
  //     ((<pattern>) <template>)
  //     ((<pattern>) <template>)
  //     ((<pattern>) <template>))
  //
  // (let-syntax ((<label-1> <syntax-rules-1>)
  //              ...
  //              (<label-N> <syntax-rules-N>))
  //             <body>)
  //
  // => token strings in the <keywords> list allow those symbols, and only 
  //    those symbols, in places where they are mentioned w/in <pattern>s

  // EXAMPLES:
  // ; Redefining λ to expand to 'lambda
  // (define-syntax λ
  //       (syntax-rules ()
  //         ((λ args body ...) (lambda args body ...))))
  //
  // ; Macro simulating variadic multiplication if '* were a binary operation
  // (define-syntax multiply-all 
  //     (syntax-rules ()
  //       ((multiply-all) 1)
  //       ((multiply-all a) a)
  //       ((multiply-all a b ...) (* a (multiply-all b ...)))))

  // Fcn prototypes for pattern-subexpression comparisons
  bool mismatched_subexpressions(const data& pat_elt,    const data& arg_elt,
                                 const sym_type& label,  const frame_vars& keywords,
                                 const size_type& pat_No,const scm_list& exp);
  bool duplicate_pattern_object_name_instance(const sym_type& obj1_name,const data& obj2)noexcept;

  // Confirm whether the given word is a keyword
  bool is_keyword(const sym_type& word, const frame_vars& keywords)noexcept{
    return std::find(keywords.begin(), keywords.end(), word) != keywords.end();
  }


  // Determine whether only the par's elt OR only the arg's elt is a keyword
  bool mismatched_keywords(const data& pat_elt, const data& arg_elt, const frame_vars& keywords)noexcept{
    const bool pat_is_key = pat_elt.is_type(types::sym) && is_keyword(pat_elt.sym,keywords);
    const bool arg_is_key = arg_elt.is_type(types::sym) && is_keyword(arg_elt.sym,keywords);
    return pat_is_key ^ arg_is_key;
  }


  // Primitive symbolic literals: #t #f '()
  bool is_primitive_symbolic_literal(const data& obj)noexcept{
    return obj.is_type(types::sym) && (obj.sym == symconst::true_t || 
                                       obj.sym == symconst::false_t || 
                                       obj.sym == symconst::emptylist);
  }


  // Confirm given 2 incompatible atomics
  bool mismatched_atomics(const data& pat_entity, const data& arg_entity)noexcept{
    if(is_primitive_symbolic_literal(pat_entity))
       return !is_primitive_symbolic_literal(arg_entity) || 
              pat_entity.sym != arg_entity.sym;
    if(pat_entity.is_type(types::sym) || pat_entity.is_type(types::exp)) return false;
    if(pat_entity.type != arg_entity.type)                               return true;
    if(pat_entity.is_type(types::par))
      return !prm_compare_PAIRs(pat_entity.par, arg_entity.par);
    if(pat_entity.is_type(types::vec))
      return !prm_compare_VECTs(pat_entity.vec, arg_entity.vec);
    return !prm_compare_atomic_values(pat_entity, arg_entity, pat_entity.type);
  }


  // Confirm whether 'pattern' is argless but was given 'args' (or vise versa)
  bool incompatible_void_arg_use(const scm_list& pattern, const scm_list& args)noexcept{
    const bool pattern_is_argless = pattern.size() == 2 && 
                                    pattern[1].is_type(types::sym) && 
                                    pattern[1].sym==symconst::sentinel_arg;
    const bool args_is_argless    = args.size()==1 && data_is_the_SENTINEL_VAL(args[0]);
    return pattern_is_argless ^ args_is_argless;
  }


  // Check whether enough args to match against the current pattern
  bool incompatible_sizes(const scm_list& pattern, const scm_list& args)noexcept{
    const bool ends_in_ellipsis = !pattern.empty() && 
                                  pattern.rbegin()->is_type(types::sym) && 
                                  pattern.rbegin()->sym=="...";
    return pattern.empty() ||                                       // no pattern
            args.size() < (pattern.size()-1-ends_in_ellipsis) ||    // not enough args (for possible variadic)
            (!ends_in_ellipsis && args.size() != pattern.size()-1); // not exact args (for non-variadic)
  }


  // Confirms pattern sub-expression does not contain an object with the same name as 'existing_pat_obj'
  bool pattern_sub_expression_has_duplicate(const sym_type& existing_pat_obj,const scm_list& pattern)noexcept{
    for(const auto& pat : pattern) {
      if(!pat.is_type(types::sym) && !pat.is_type(types::exp)) continue; // naught to compare
      if(duplicate_pattern_object_name_instance(existing_pat_obj, pat)) return true;
    }
    return false;
  }


  // Confirms whether the obj1 & obj2 share a duplicate name instance
  bool duplicate_pattern_object_name_instance(const sym_type& obj1_name, const data& obj2)noexcept{
    return (!is_primitive_symbolic_literal(obj2) && obj2.is_type(types::sym) && obj2.sym == obj1_name) ||
           (obj2.is_type(types::exp) && pattern_sub_expression_has_duplicate(obj1_name,obj2.exp));
  }


  // Confirm each pattern object only appears once in the pattern
  void confirm_no_duplicate_pattern_objects(const sym_type& label,  const scm_list& pattern,
                                            const size_type& pat_No,const scm_list& exp) {
    for(size_type i=0, n=pattern.size(); i < n; ++i) {
      if(!pattern[i].is_type(types::sym)) continue; // nothing to compare against
      for(size_type j = i+1; j < n; ++j)
        if(duplicate_pattern_object_name_instance(pattern[i].sym, pattern[j]))
          THROW_ERR("Invalid syntax \""<<label<<"\", pattern #"<<pat_No<<", identifier \"" 
            << pattern[i].sym << "\" appeared more than once!\n     ((<pattern>) <template>)"
            << EXP_ERR(exp));
    }
  }


  // Check whether '...' was used prior the 3rd pattern object
  bool uses_Ellipsis_prematurely(const scm_list& pattern)noexcept{
    const size_type n = pattern.size();
    return (pattern[0].is_type(types::sym) && pattern[0].sym == "...") ||
           (n > 1 && pattern[1].is_type(types::sym) && pattern[1].sym == "...") || 
           (n > 2 && pattern[2].is_type(types::sym) && pattern[2].sym == "...");
  }


  // Confirm whether the pattern sub-expression matches the 'args' sub-expression
  bool compare_pattern_args_exp_match(const scm_list& pat_exp,const scm_list& args_exp,
                                      const sym_type& label,  const frame_vars& keywords,
                                      const size_type& pat_No,const scm_list& exp){
    if(pat_exp.empty() && args_exp.empty()) return true;
    // Check whether enough args to match against the current pattern sub-expression
    if(pat_exp.empty() || args_exp.size() < pat_exp.size()) return false;
    // Confirm whether pattern & label-args combo match one another
    for(size_type i=0, j=0, n=pat_exp.size(); i < n; ++i, ++j){
      // Check for proper "..." use in the pattern definition
      if(pat_exp[i].is_type(types::sym) && pat_exp[i].sym == "...")
        THROW_ERR("Invalid syntax \"" << label << "\", '...' wasn't the last identifier in pattern #" 
          << pat_No << "!\n     ((<pattern>) <template>)" << EXP_ERR(exp));
      if(mismatched_atomics(pat_exp[i], args_exp[j])) return false;
      // Check for a missing keyword
      if(mismatched_keywords(pat_exp[i], args_exp[j], keywords)) return false;
      // Check for a missing expr
      if(mismatched_subexpressions(pat_exp[i],args_exp[j],label,keywords,pat_No,exp)) return false;
    }
    return true; // not improper use is proper use
  }


  // Confirm the 2 given pattern/arg elts are mismatched subexpressions
  bool mismatched_subexpressions(const data& pat_elt,    const data& arg_elt, 
                                 const sym_type& label,  const frame_vars& keywords, 
                                 const size_type& pat_No,const scm_list& exp){
    return pat_elt.is_type(types::exp) && 
        (!arg_elt.is_type(types::exp) || 
         !compare_pattern_args_exp_match(pat_elt.exp,arg_elt.exp,
                                         label,keywords,pat_No,exp));
  }


  // Wraps various helper fcns above, confirming whether or not a pattern was incompatible
  bool incompatible_pattern(const sym_type& label,     const scm_list& args,
                            const frame_vars& keywords,const scm_list& pattern,
                            const size_type& pat_No,   const scm_list& exp) {
    // Check whether enough args to match against the current pattern
    if(incompatible_void_arg_use(pattern,args) || incompatible_sizes(pattern,args)) return true;
    // Confirm valid first symbol in pattern is a non-keyword symbol
    if(!pattern[0].is_type(types::sym) || is_keyword(pattern[0].sym,keywords))
      THROW_ERR("Invalid syntax \""<< label <<"\", pattern #" << pat_No <<", first identifier in pattern "
        "wasn't a non-keyword symbol\n     ((<pattern>) <template>) ; <pattern> = (<non-keyword-symbol> <body>)"
        << EXP_ERR(exp));
    // Confirm valid use of '...'
    if(is_keyword("...", keywords))
      THROW_ERR("Invalid syntax \"" << label <<"\", identifier '...' may never be a keyword!" << EXP_ERR(exp));
    if(uses_Ellipsis_prematurely(pattern))
      THROW_ERR("Invalid syntax \"" << label <<"\", pattern #" 
        << pat_No <<", identifier '...' must follow at least 2 other identifiers!" << EXP_ERR(exp));
    // Confirm unique pattern names were used
    confirm_no_duplicate_pattern_objects(label, pattern, pat_No, exp);
    return false;
  }


  // Confirm the given label-arg combo matches the given pattern (in terms of layout)
  bool is_pattern_match(const sym_type& label,     const scm_list& args, 
                        const frame_vars& keywords,const scm_list& pattern,
                        const size_type& pat_No,   const scm_list& exp) {
    // Confirm pattern is a viable candidate for matching
    if(incompatible_pattern(label,args,keywords,pattern,pat_No,exp)) return false;
    // Confirm whether pattern & label-args combo match one another
    for(size_type i=1, j=0, n=pattern.size(); i < n; ++i, ++j) {
      // Check for proper "..." use in the pattern definition
      if(pattern[i].is_type(types::sym) && pattern[i].sym == "...") {
        if(i != n-1) THROW_ERR("Invalid syntax \"" << label 
          << "\", '...' wasn't the last identifier in pattern #" << pat_No << "!"
          << EXP_ERR(exp));
        return true;
      }
      if(mismatched_atomics(pattern[i], args[j])) return false;
      // Check for a missing keyword
      if(mismatched_keywords(pattern[i], args[j], keywords)) return false;
      // Check for a missing expr
      if(mismatched_subexpressions(pattern[i],args[j],label,keywords,pat_No,exp)) return false;
    }
    return true; // not improper use is proper use
  }


  // Returns whether the given label & args correspond to the given macro
  bool is_macro_match(const sym_type& label, const scm_list& args, 
                      const frame_mac& mac,  size_type& match_idx,
                      const scm_list& exp) {
    for(size_type i = 0, n = mac.patterns.size(); i < n; ++i)
      if(is_pattern_match(label, args, mac.keywords, mac.patterns[i], i+1, exp)) {
        match_idx = i;
        return true;
      }
    return false;
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- EXPANSION HELPER FUNCTIONS
  ******************************************************************************/

  // Splice in 'obj' for every instance of 'macro_arg_name' in 'expanded_exp'
  void splice_object_throughout_macro(const data& obj, const sym_type& macro_arg_name, 
                                                       scm_list& expanded_exp)noexcept{
    for(auto& e : expanded_exp) {
      if(e.is_type(types::sym) && e.sym == macro_arg_name)
        e = obj;
      else if(e.is_type(types::exp))
        splice_object_throughout_macro(obj, macro_arg_name, e.exp);
    }
  }


  // Splice in the 'remaining_body' for every instance of '...' in 'expanded_exp'
  void splice_remaining_body_throughout_macro(const scm_list& remaining_body, 
                                                    scm_list& expanded_exp)noexcept{
    for(size_type i = 0; i < expanded_exp.size(); ++i) {
      if(expanded_exp[i].is_type(types::sym) && expanded_exp[i].sym == "...") {
        // erase the "..." arg & splicing in the 'remaining body'
        expanded_exp.erase(expanded_exp.begin()+i); 
        expanded_exp.insert(expanded_exp.begin()+i, remaining_body.begin(), remaining_body.end());
      }
      else if(expanded_exp[i].is_type(types::exp))
        splice_remaining_body_throughout_macro(remaining_body, expanded_exp[i].exp);
    }
  }


  // Expands the args (as per the pattern & template) into expanded_exp
  // NOTE: "skip_pattern_name" = true iff at the 1st (non-recursive) call 
  void expand_macro(const scm_list& args, const scm_list& pattern, 
                    const frame_vars& keywords, scm_list& expanded_exp, 
                    const bool& skip_pattern_name = false)noexcept{
    for(size_type i = size_type(skip_pattern_name), j = 0, n = pattern.size(); i < n; ++i, ++j){
      if(pattern[i].is_type(types::sym) && is_keyword(pattern[i].sym, keywords)) continue;
      // Expand args nested w/in expressions
      if(pattern[i].is_type(types::exp))
        expand_macro(args[j].exp, pattern[i].exp, keywords, expanded_exp);
      // Expand symbols
      else if(pattern[i].is_type(types::sym) && pattern[i].sym != "...")
        splice_object_throughout_macro(args[j], pattern[i].sym, expanded_exp);
      // Expand "..."
      else if(pattern[i].is_type(types::sym) && pattern[i].sym == "...")
        splice_remaining_body_throughout_macro(scm_list(args.begin()+j,args.end()), expanded_exp);
    }
  }

  /******************************************************************************
  * MACRO SYNTACTIC EXTENSIONS -- EXPANSION MAIN FUNCTIONS
  ******************************************************************************/

  // Confirm whether 'application_label' is a potential macro label
  bool application_is_a_potential_macro(const scm_string& application_label)noexcept{
    if(application_label.empty()) return false;
    for(const auto& label : MACRO_LABEL_REGISTRY)
      if(label == application_label)
        return true;
    return false;
  }


  // Returns whether the given label & args form a macro found in 'macs'.
  // If true, it also transforms the macro by expanding it into 'expanded_exp'
  bool handle_macro_transformation(const sym_type& label,const scm_list& args, 
                                   const frame_macs& macs,scm_list& expanded_exp,
                                   const scm_list& exp) {
    // search for macro matches
    for(const auto& mac : macs) {
      size_type match_idx = 0; // idx of the pattern & template w/in 'mac' that the label & args match
      if(label == mac.label && is_macro_match(label, args, mac, match_idx, exp)) {
        expanded_exp = mac.templates[match_idx];    // prefilled, then has contents expanded into it
        expand_macro(args, mac.patterns[match_idx], mac.keywords, expanded_exp, true);
        expanded_exp = scm_list_cast(expanded_exp); // if only 1 sub-expression, degrade to the sub-expression
        return true;
      }
    }
    return false;
  }


  // Returns whether the given label & args form a macro found in 'env'.
  // If true, it also transforms the macro by expanding it into 'expanded_exp'
  bool expand_macro_if_in_env(const sym_type& label,const scm_list& args, 
                              env_type& env,scm_list& expanded_exp,
                              const scm_list& exp) {
    // Search Each Environment Frame
    for(auto& frame : *env) {
      // Get Macro List of the current frame & search 
      //   for a match with the current label & args
      if(handle_macro_transformation(label,args,frame_macros(*frame),expanded_exp,exp)) 
        return true;
    }
    return false;
  }

  /******************************************************************************
  * REPRESENTING SYNTAX: (syntax-rules <keyword-list> <pattern-template-clauses>)
  ******************************************************************************/

  bool is_syntax_rules(const scm_list& exp)noexcept{
    return is_tagged_list(exp,symconst::syn_rules);
  }


  void confirm_valid_syntax_rules(const scm_list& exp) {
    static constexpr const char * const format = 
      "\n     (syntax-rules (<keyword-list>) <pattern-template-clauses>)";
    if(exp.size() < 3)
      THROW_ERR("'syntax-rules received incorrect # of arguments!"<<format<<EXP_ERR(exp));
    if(!exp[1].is_type(types::exp))
      THROW_ERR("'syntax-rules 1st arg "<<PROFILE(exp[1])<<" isn't a list of keyword symbols:"<<format<<EXP_ERR(exp));
    for(const auto& e : exp[1].exp)
      if(!e.is_type(types::sym))
        THROW_ERR("'syntax-rules keyword "<<PROFILE(e)<<" must be a symbol!"<<format<<EXP_ERR(exp));
    for(size_type i = 2, n = exp.size(); i < n; ++i)
      if(!exp[i].is_type(types::exp) || exp[i].exp.size() < 2 || 
         !exp[i].exp[0].is_type(types::exp))
        THROW_ERR("'syntax-rules "<<PROFILE(exp[i])<<" is an invalid ((<pattern>) <template>) clause!"
          <<format<< "\n     <pattern-template-clause> = ((<pattern>) <template>)" << EXP_ERR(exp));
  }


  // Generate a unique hashed (hygienic!) variant of the given macro arg name
  // NOTE: Max Unique Hashes = (expt 18446744073709551615 18446744073709551615)
  scm_string hygienically_hashed_macro_arg(const scm_string& label)noexcept{
    if(MACRO_HASH_IDX_1 != MAX_SIZE_TYPE)
      return "__HEIST-" + label + "-" 
        + std::to_string(MACRO_HASH_IDX_2) + "-" 
        + std::to_string(MACRO_HASH_IDX_1++);
    return "__HEIST-" + label + "-" 
      + std::to_string(++MACRO_HASH_IDX_2) + "-" 
      + std::to_string(MACRO_HASH_IDX_1++);
  }


  // Confirm <pat_entity> is a macro argument name
  bool is_macro_argument_label(const data& pat_entity, const frame_vars& keywords)noexcept{
    return pat_entity.is_type(types::sym) && !is_primitive_symbolic_literal(pat_entity) &&
      pat_entity.sym != symconst::ellipsis && pat_entity.sym != symconst::sentinel_arg 
      && !is_keyword(pat_entity.sym, keywords);
  }


  void recursively_hygienically_hash_macro_template(const scm_string& value, const scm_string& hash_value, 
                                                                             scm_list& mac_template)noexcept{
    for(size_type i = 0, n = mac_template.size(); i < n; ++i) {
      if(mac_template[i].is_type(types::exp)) {
        recursively_hygienically_hash_macro_template(value, hash_value, mac_template[i].exp);
      } else if(mac_template[i].is_type(types::sym) && mac_template[i].sym == value) {
        mac_template[i].sym = hash_value;
      }
    }
  }


  void recursively_hygienically_hash_macro_pattern(scm_list& pattern, scm_list& mac_template, 
                                                   const frame_vars& keywords, const size_type& start=0)noexcept{
    for(size_type i = start, n = pattern.size(); i < n; ++i) {
      if(pattern[i].is_type(types::exp)) {
        recursively_hygienically_hash_macro_pattern(pattern[i].exp, mac_template, keywords);
      } else if(is_macro_argument_label(pattern[i], keywords)) {
        scm_string original_label = pattern[i].sym;
        pattern[i].sym = hygienically_hashed_macro_arg(pattern[i].sym);
        recursively_hygienically_hash_macro_template(original_label, pattern[i].sym, mac_template);
      }
    }
  }


  exe_type analyze_syntax_rules(scm_list& exp) {
    confirm_valid_syntax_rules(exp);
    syn_type mac("");
    // Extract keywords
    for(const auto& keyword : exp[1].exp)
      mac.keywords.push_back(keyword.sym);
    // Extract pattern-template clauses
    for(size_type i = 2, n = exp.size(); i < n; ++i) {
      mac.patterns.push_back(exp[i].exp[0].exp);
      // Add sentinel arg if pattern is argless
      if(exp[i].exp[0].exp.size() == 1)
        mac.patterns.rbegin()->push_back(symconst::sentinel_arg); 
      // Wrap 'begin' around templates prior evaluation (for multi-exp bodies)
      mac.templates.push_back(scm_list(exp[i].exp.size()));
      mac.templates.rbegin()->operator[](0) = symconst::begin;
      std::copy(exp[i].exp.begin()+1, 
                exp[i].exp.end(), 
                mac.templates.rbegin()->begin()+1);
    }
    // Hash macro args to prevent unintentional expansions => "hygienic" macros
    for(size_type i = 0, n = mac.patterns.size(); i < n; ++i) {
      // pass <1> to not hash 1st pattern symbol (not an arg)
      recursively_hygienically_hash_macro_pattern(mac.patterns[i],mac.templates[i],mac.keywords,1);
    }
    return [syntax_rule=scm_list(1,std::move(mac))](env_type&){return syntax_rule;};
  }

  /******************************************************************************
  * REPRESENTING SYNTAX EXTENSIONS: (define-syntax <label> <syntax-rules-object>)
  ******************************************************************************/

  bool is_define_syntax(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::defn_syn);}
  bool is_let_syntax   (const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::let_syn);}
  bool is_letrec_syntax(const scm_list& exp)noexcept{return is_tagged_list(exp,symconst::letrec_syn);}


  bool must_evaluate_2nd_arg_for_syntax_rules_object(scm_list& exp) {
    if(exp.size() != 3)
      THROW_ERR("'define-syntax expects 2 arguments:"
        "\n     (define-syntax <label> <syntax-rules-object>)"<<EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'define-syntax 1st arg "<<PROFILE(exp[1])
        <<" isn't a symbolic label:" << EXP_ERR(exp));
    // If given a syntax-rules expression, immediately evaluate (no need for environment)
    if(exp[2].is_type(types::exp) && !exp[2].exp.empty() && 
      is_tagged_list(exp[2].exp,symconst::syn_rules)) {
      env_type nil_env = nullptr;
      exp[2] = scm_analyze(std::move(exp[2].exp))(nil_env)[0];
      return false;
    }
    return !exp[2].is_type(types::syn);
  }


  void confirm_valid_let_macro(const scm_list& exp, const char* name) {
    static constexpr const char * const format = 
      " (<syntactic-bindings-list>) <body>)"
      "\n     <syntactic-binding> = (<label> <syntax-rules>)";
    if(exp.size() < 2 || !exp[1].is_type(types::exp))
      THROW_ERR('\''<<name<<" 1st argument must be a syntactic bindings list:"
        "\n     (" << name << format << EXP_ERR(exp));
    if(exp.size() < 3)
      THROW_ERR('\''<<name<<" 2nd argument must be a body:"
        "\n     (" << name << format << EXP_ERR(exp));
    // Confirm bindings list is either EMPTY or a LIST OF LISTS, EACH OF LENGTH 2
    for(const auto& binding : exp[1].exp)
      if(!binding.is_type(types::exp) || binding.exp.size() != 2)
        THROW_ERR('\''<<name<<" binding "<<binding<<" isn't a proper syntactic binding:"
          "\n     (" << name << format << EXP_ERR(exp));
  }


  // Template for 'let-syntax' & 'letrec-syntax' special forms (letrec style is default)
  exe_type let_syntactic_extension_binding_template(scm_list& exp, const char* name){
    // Convert let-syntax/letrec-syntax to an argless let defining syntax in its body
    confirm_valid_let_macro(exp,name);
    scm_list let_exp(exp[1].exp.size()+exp.size());
    let_exp[0] = symconst::let, let_exp[1] = scm_list();
    // Splice in syntax defns
    for(size_type i = 0, n = exp[1].exp.size(); i < n; ++i){ 
      let_exp[i+2] = scm_list(3);
      let_exp[i+2].exp[0] = symconst::defn_syn;
      let_exp[i+2].exp[1] = exp[1].exp[i].exp[0];
      let_exp[i+2].exp[2] = exp[1].exp[i].exp[1];
    }
    // Add let body
    std::copy(exp.begin()+2,exp.end(),let_exp.begin()+exp[1].exp.size()+2);
    // Analyze let of syntax defns
    return scm_analyze(std::move(let_exp)); 
  }


  exe_type analyze_define_syntax(scm_list& exp) {
    if(must_evaluate_2nd_arg_for_syntax_rules_object(exp)) { 
      auto syntax_rules_obj_proc = scm_analyze(scm_list_cast(exp[2]));
      return [syntax_rules_obj_proc=std::move(syntax_rules_obj_proc),
        exp=std::move(exp)](env_type& env)mutable{
        data mac = data_cast(syntax_rules_obj_proc(env));
        if(!mac.is_type(types::syn)) 
          THROW_ERR("'define-syntax 2nd arg "<<PROFILE(exp[2])
            <<" isn't a syntax-rules object:\n     (define-syntax "
              "<label> <syntax-rules-object>)"<<EXP_ERR(exp));
        mac.syn.label = exp[1].sym;     // assign macro label
        MACRO_LABEL_REGISTRY.push_back(exp[1].sym);
        define_syntax_extension(mac.syn,env); // establish in environment
        return VOID_DATA_EXPRESSION;
      };
    }
    exp[2].syn.label = exp[1].sym; // assign macro label
    MACRO_LABEL_REGISTRY.push_back(exp[1].sym);
    return [mac = std::move(exp[2].syn)](env_type& env){
      define_syntax_extension(mac,env); // establish in environment
      return VOID_DATA_EXPRESSION;
    };
  }


  exe_type analyze_let_syntax(scm_list& exp) {
    return let_syntactic_extension_binding_template(exp,symconst::let_syn);
  }


  exe_type analyze_letrec_syntax(scm_list& exp) {
    return let_syntactic_extension_binding_template(exp,symconst::letrec_syn);
  }

  /******************************************************************************
  * ANALYSIS, EVALUATION, & APPLICATION
  ******************************************************************************/

  // -- PRIMITIVE PROCEDURES: identification & application
  bool is_primitive_procedure(const scm_list& p)noexcept{
    return is_tagged_list(p,symconst::primitive);
  }


  scm_list apply_primitive_procedure(scm_list& proc,scm_list& args,env_type& env){
    // Rm "sentinel-arg" value from args (if present from an argless application)
    if(args.size()==1 && args[0].is_type(types::sym) && args[0].sym == symconst::sentinel_arg)
      args.pop_back();
    // Provide the environment to primitives applying user-defined procedures
    if(primitive_requires_environment(proc[1].prm)) args.push_back(env);
    return scm_list_cast(proc[1].prm(args));
  }


  // -- EVAL 
  scm_list scm_eval(scm_list&& exp, env_type& env) { // evaluate expression environment
    return scm_analyze(std::move(exp))(env);
  }


  // -- APPLY
  // Applies the given procedure, & then reapplies iteratively if at a tail call
  scm_list apply_compound_procedure(exe_type& proc, env_type& extended_env) {
    auto result = proc(extended_env);
  tail_call_recur:
    if(is_tagged_list(result,symconst::tail_call)) { // if tail call
      result = result[1].exe(result[2].env);
      goto tail_call_recur;
    }
    return result;
  }


  // Analogue to "apply", except no need to analyze the body of compound 
  //   procedures (already done). Hence only calls the execution procedure 
  //   for the proc's body w/ the extended environment
  scm_list execute_application(scm_list& procedure,scm_list& arguments,env_type& env,
                                            const bool tail_call,const bool inlined){
    if(is_primitive_procedure(procedure)) // execute primitive procedure directly
      return apply_primitive_procedure(procedure,arguments,env);
    else if(is_compound_procedure(procedure)) {
      // create the procedure body's extended environment frame
      auto extended_env = extend_environment(procedure_parameters(procedure),
                                             arguments,
                                             procedure_environment(procedure),
                                             procedure_name(procedure));
      if(inlined) extended_env->insert(extended_env->begin()+1, env->begin(), env->end());
      // confirm max recursive depth hasn't been exceeded
      auto& recursive_depth = procedure_recursive_depth(procedure);
      if(recursive_depth > MAX_RECURSION_DEPTH) {
        auto name = procedure_name(procedure);
        if(name.empty()) name = "#<procedure>"; // lambda
        else             name.erase(0,1);       // rm prefixing ' '
        recursive_depth = 0;
        THROW_ERR("Maximum recursion depth of "<<MAX_RECURSION_DEPTH<<" exceeded!"
          << FCN_ERR(name, arguments));
      }
      // store application data & return such back up to the last call if in a tail call
      if(tail_call) {
        scm_list tail_call_signature(3); // {tail-call-tag, proc-body, extended-env}
        tail_call_signature[0] = symconst::tail_call;
        tail_call_signature[1] = procedure_body(procedure);
        tail_call_signature[2] = extended_env;
        return tail_call_signature;
      }
      ++recursive_depth;
      auto result = apply_compound_procedure(procedure_body(procedure),extended_env);
      --recursive_depth;
      return result;
    }
    if(procedure.empty())
      THROW_ERR("Invalid application of unknown procedure type NULL (received an empty expression)!");
    THROW_ERR("Invalid application of unknown procedure "<<procedure[0]<<"! "<<EXP_ERR(procedure));
  }

  // R-value overload
  scm_list execute_application(scm_list&& procedure,scm_list& arguments,env_type& env,
                               const bool tail_call=false,const bool inlined=false){
    return execute_application(procedure,arguments,env,tail_call,inlined);
  }


  // Analyzes the operator & operands, then returns an exec proc passing 
  //   both the operator/operand proc exec's to 'execute-application'
  //   (after having checked for macro use as well)
  exe_type analyze_application(scm_list& exp,const bool tail_call=false) {
    auto op_proc  = scm_analyze(operator_of(exp));
    auto arg_exps = operands(exp);
    // Save name of invoking entity (iff a symbol) to check for a possible macro
    sym_type op_name = exp[0].is_type(types::sym) ? exp[0].sym : "";
    // If _NOT_ a possible macro, analyze the applicator's args ahead of time
    if(!application_is_a_potential_macro(op_name)) {
      std::vector<exe_type> arg_procs(arg_exps.size());
      for(size_type i = 0, n = arg_exps.size(); i < n; ++i)
        arg_procs[i] = scm_analyze(scm_list_cast(arg_exps[i]));
      return [op_proc=std::move(op_proc),arg_procs=std::move(arg_procs),
              tail_call=std::move(tail_call)](env_type& env){
        scm_list arg_vals(arg_procs.size());
        for(size_type i = 0, n = arg_procs.size(); i < n; ++i)
          arg_vals[i] = data_cast(arg_procs[i](env));
        return execute_application(op_proc(env),arg_vals,env,tail_call);
      };
    }
    // If possible macro, expand the application if so, else analyze args at eval
    return [op_proc=std::move(op_proc),arg_exps=std::move(arg_exps),
            op_name=std::move(op_name),exp=std::move(exp),
            tail_call=std::move(tail_call)](env_type& env){
      // check for a possible macro instance, & expand/eval it if so
      scm_list expanded;
      if(expand_macro_if_in_env(op_name, arg_exps, env, expanded, exp))
        return scm_eval(std::move(expanded),env);
      // eval each arg's exec proc to obtain the actual arg values
      scm_list arg_vals(arg_exps.size());
      for(size_type i = 0, n = arg_exps.size(); i < n; ++i)
        arg_vals[i] = data_cast(scm_eval(scm_list_cast(arg_exps[i]),env));
      return execute_application(op_proc(env),arg_vals,env,tail_call);
    };
  }


  // -- ANALYZE (SYNTAX)
  void throw_unknown_analysis_anomalous_error(const scm_list& exp) {
    scm_string err_str("ANALYZE: received unknown expression! -:- BUG ALERT -:-"
                       "\n         Triggered By: " + cio_expr_str<&data::write>(exp) + 
                       "\n         Profile of data in expression:");
    for(size_type i = 0, n = exp.size(); i < n; ++i)
      err_str += (("\n         " + std::to_string(i+1) + ". " + 
                    exp[i].write() + " of type \"") + exp[i].type_name()) + '"';
    THROW_ERR(err_str << "\n         => Please send your code to jrandleman@scu.edu to fix"
                         "\n            the interpreter's bug!");
  }


  exe_type scm_analyze(scm_list&& exp,const bool tail_call) { // analyze expression
    if(exp.empty())                         THROW_ERR("Can't eval an empty expression!"<<EXP_ERR("()"));
    else if(is_self_evaluating(exp)) return [exp=std::move(exp)](env_type&){return exp;};
    else if(is_quoted(exp))          return analyze_quoted(exp);
    else if(is_assignment(exp))      return analyze_assignment(exp);
    else if(is_definition(exp))      return analyze_definition(exp);
    else if(is_if(exp))              return analyze_if(exp,tail_call);
    else if(is_and(exp))             return analyze_and(exp);
    else if(is_or(exp))              return analyze_or(exp);
    else if(is_lambda(exp))          return analyze_lambda(exp);
    else if(is_begin(exp))           return analyze_sequence(begin_actions(exp),tail_call);
    else if(is_scons(exp))           return analyze_scons(exp);
    else if(is_stream(exp))          return analyze_stream(exp);
    else if(is_delay(exp))           return analyze_delay(exp);
    else if(is_cond(exp))            return scm_analyze(convert_cond_if(exp));
    else if(is_case(exp))            return scm_analyze(convert_case_cond(exp));
    else if(is_let(exp))             return scm_analyze(convert_let_combination(exp));
    else if(is_let_star(exp))        return scm_analyze(convert_let_star_nested_lets(exp));
    else if(is_letrec(exp))          return scm_analyze(convert_letrec_let(exp));
    else if(is_do(exp))              return scm_analyze(convert_do_letrec(exp));
    else if(is_quasiquote(exp))      return analyze_quasiquote(exp);
    else if(is_define_syntax(exp))   return analyze_define_syntax(exp);
    else if(is_let_syntax(exp))      return analyze_let_syntax(exp);
    else if(is_letrec_syntax(exp))   return analyze_letrec_syntax(exp);
    else if(is_syntax_rules(exp))    return analyze_syntax_rules(exp);
    else if(is_vector_literal(exp))         THROW_ERR("Misplaced keyword 'vector-literal outside of a quotation! -- ANALYZE"   <<EXP_ERR(exp));
    else if(is_unquote(exp))                THROW_ERR("Misplaced keyword 'unquote outside of 'quasiquote ! -- ANALYZE"         <<EXP_ERR(exp));
    else if(is_unquote_splicing(exp))       THROW_ERR("Misplaced keyword 'unquote-splicing outside of 'quasiquote ! -- ANALYZE"<<EXP_ERR(exp));
    else if(is_application(exp))     return analyze_application(exp,tail_call);
    else if(is_variable(exp)){
      return [exp=std::move(exp)](env_type& env){
        return scm_list_cast(lookup_variable_value(exp[0].sym,env));
      };
    }
    throw_unknown_analysis_anomalous_error(exp);
    return exe_type();
  }

  /******************************************************************************
  * GLOBAL ENVIRONMENT SETUP
  ******************************************************************************/

  void set_default_global_environment() {
    GLOBAL_ENVIRONMENT_POINTER = make_env();
    GLOBAL_ENVIRONMENT_POINTER = extend_environment(
      primitive_procedure_names(),
      primitive_procedure_objects(),
      GLOBAL_ENVIRONMENT_POINTER
    );
    define_variable(symconst::true_t,  TRUE_DATA_BOOLEAN,  GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::false_t, FALSE_DATA_BOOLEAN, GLOBAL_ENVIRONMENT_POINTER);
    define_variable("fl-precision", num_type(num_type::INEXACT_PRECISION), GLOBAL_ENVIRONMENT_POINTER);
    define_variable("fl-max",       num_type(num_type::INEXACT_MAX),       GLOBAL_ENVIRONMENT_POINTER);
    define_variable("fl-min",       num_type(num_type::INEXACT_MIN),       GLOBAL_ENVIRONMENT_POINTER);
    define_variable("fl-epsilon",   num_type(num_type::INEXACT_EPSILON),   GLOBAL_ENVIRONMENT_POINTER);
    define_variable("stream-null",          symconst::emptylist,    GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::null_env,     symconst::null_env,     GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::locl_env,     symconst::locl_env,     GLOBAL_ENVIRONMENT_POINTER);
    define_variable(symconst::sentinel_arg, symconst::sentinel_arg, GLOBAL_ENVIRONMENT_POINTER);
    evaluate_primitives_written_in_heist_scheme(GLOBAL_ENVIRONMENT_POINTER);
  }

  /******************************************************************************
  * GLOBAL PORT REGISTRY CLEANUP
  ******************************************************************************/

  void close_port_registry()noexcept{
    for(size_type i = 2, n = PORT_REGISTRY.size(); i < n; ++i)
      if(PORT_REGISTRY[i] && 
         PORT_REGISTRY[i]!=stdout && PORT_REGISTRY[i]!=stderr &&
         PORT_REGISTRY[i]!=stdin){
        fclose(PORT_REGISTRY[i]);
        PORT_REGISTRY[i] = nullptr;
      }
  }

  /******************************************************************************
  * REPL DRIVER LOOP
  ******************************************************************************/

  void announce_input(FILE* outs)noexcept{fputs(REPL_PROMPT.c_str(), outs);}
  void indent_input(FILE* outs)  noexcept{fputs(REPL_TAB.c_str(), outs);}


  // Read & parse user expressions
  scm_list read_user_input(FILE* outs, FILE* ins, const bool& in_repl){
    scm_string input, tmp_buffer;
    scm_list abstract_syntax_tree;
    int ch;
    for(;;) {
      // Read input
      fflush(outs);
      tmp_buffer.clear();
      while((ch = fgetc(ins)) != '\n') tmp_buffer += ch;
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
#ifndef HEIST_CPP_INTEROP_HPP_ // @NOT-EMBEDDED-IN-C++
#ifndef HEIST_INTERPRETING_COMPILED_AST // @ONLY-INTERPRETER
void print_repl_newline(const bool& printed_data)noexcept{ // after printing data
  if(!heist::LAST_PRINTED_NEWLINE_TO_STDOUT&&(printed_data||heist::LAST_PRINTED_TO_STDOUT))
    putchar('\n');
  heist::LAST_PRINTED_NEWLINE_TO_STDOUT = heist::LAST_PRINTED_TO_STDOUT = false;
}

void print_repl_newline()noexcept{ // after printing an error
  putchar('\n'), heist::LAST_PRINTED_NEWLINE_TO_STDOUT=heist::LAST_PRINTED_TO_STDOUT=false;
}

void account_for_whether_printed_data(const heist::scm_list& val,bool& printed_data)noexcept{
  printed_data = !val.empty() && !val[0].is_type(heist::types::dne);
}


// Print output object
void user_print(FILE* outs, heist::scm_list& object)noexcept{
  if(heist::is_compound_procedure(object) || heist::is_primitive_procedure(object))
    fprintf(outs, "#<procedure%s>", heist::procedure_name(object).c_str());
  else if(heist::is_delay(object) && object.size() > 1)
    fputs("#<delay>", outs);
  else
    fputs(object[0].write().c_str(), outs);
  fflush(outs);
}


void driver_loop() {
  bool printed_data = true;
  print_repl_newline(printed_data);
  for(;;) {
    heist::announce_input(stdout);
    auto AST = heist::read_user_input(stdout,stdin); // AST = Abstract Syntax Tree
    // Eval each expression given
    for(const auto& input : AST) {
      try {
        auto value = heist::scm_eval(heist::scm_list_cast(input),heist::GLOBAL_ENVIRONMENT_POINTER);
        account_for_whether_printed_data(value,printed_data);
        user_print(stdout, value);
        print_repl_newline(printed_data);
      } catch(const heist::SCM_EXCEPT& eval_throw) {
        if(eval_throw == heist::SCM_EXCEPT::EXIT) { puts("Adios!"); return; }
        if(eval_throw == heist::SCM_EXCEPT::JUMP)
          PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
            << PROFILE(heist::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
        print_repl_newline();
      } catch(...) {
        PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
             "\n     Triggered By: " << input << 
             "\n  => Please send your code to jrandleman@scu.edu to fix"
             "\n     the interpreter's bug!"
             "\n  => Terminating Heist Scheme Interpretation.");
        return;
      }
    }
  }
}

/******************************************************************************
* COMMAND LINE ARGUMENT VALIDATION
******************************************************************************/

bool confirm_valid_command_line_args(int argc,char* argv[],int& script_pos,
                                     int& compile_pos,std::string& compile_as)noexcept{
  if(argc == 1) return true;
  
  constexpr const char * const cmd_line_options = 
    "\n> Interpret Script:    -script <script-filename>"
    #ifdef HEIST_DIRECTORY_FILE_PATH // @GIVEN-COMPILE-PATH
    "\n> Compile Script:      -compile <script-filename> <optional-compiled-filename>"
    #endif
    "\n> Disable ANSI Colors: -nansi"
    "\n> Case Insensitivity:  -ci"
    "\n> Terminating Heist Scheme Interpretation.\n\n";

  // Validate argument layout
  if(argc > 7) {
    fprintf(stderr, "\n> Invalid # of command-line args (given %d)!%s", argc-1, cmd_line_options);
    return false;
  }

  // Parse input arguments
  for(int i = 1; i < argc; ++i) {
    if(std::string cmd_flag(argv[i]); cmd_flag == "-ci") {
      heist::USING_CASE_SENSITIVE_SYMBOLS = false;
    } else if(cmd_flag == "-nansi") {
      heist::USING_ANSI_ESCAPE_SEQUENCES = false;
    } else if(cmd_flag == "-script") {
      if(i == argc-1) {
        fprintf(stderr,"\n> \"-script\" wasn't followed by a file!%s",cmd_line_options);
        return false;
      }
      script_pos = ++i;
    } 
    #ifdef HEIST_DIRECTORY_FILE_PATH // @GIVEN-COMPILE-PATH
    else if(cmd_flag == "-compile") {
      if(i == argc-1) {
        fprintf(stderr,"\n> \"-compile\" wasn't followed by a file!%s",cmd_line_options);
        return false;
      }
      compile_pos = ++i;
      if(std::string next_cmd(argv[i]); i < argc-1 && next_cmd != "-nansi" && 
         next_cmd != "-script" && next_cmd != "-compile" && next_cmd != "-ci")
        compile_as = argv[++i];
    } 
    #endif
    else {
      fprintf(stderr,"\n> Invalid command-line flag \"%s\"!%s",argv[i],cmd_line_options);
      return false;
    }
  }
  return true;
}

/******************************************************************************
* INTERPRET SCRIPT HELPER FUNCTION
******************************************************************************/

int load_script(char *argv[], const int& script_pos){
  // Load the script & immediately exit
  heist::scm_list load_args(2); 
  load_args[0] = heist::make_str(argv[script_pos]);
  load_args[1] = heist::GLOBAL_ENVIRONMENT_POINTER;
  try {
    heist::primitive_LOAD(load_args);
    heist::close_port_registry();
  } catch(const heist::SCM_EXCEPT& eval_throw) {
    if(eval_throw == heist::SCM_EXCEPT::JUMP)
      PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
        << PROFILE(heist::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
    /* catch errors already output to stdout */ 
    putchar('\n');
  } catch(...) {
    /* catch uncaught C++ exceptions -:- ANOMALY -:- */
    PRINT_ERR(afmt(heist::AFMT_1) << 
      "\nUncaught C++ Exception Detected! -:- BUG ALERT -:-"
      "\n  => While interpreting script \"" << argv[script_pos] << "\""
      "\n  => Please send your code to jrandleman@scu.edu to fix"
      "\n     the interpreter's bug!"
      "\n  => Terminating Heist Scheme Interpretation.\n\n" << afmt(heist::AFMT_0));
    heist::close_port_registry();
    return 1;
  }
  return 0;
}

/******************************************************************************
* COMPILE SCRIPT HELPER FUNCTION
******************************************************************************/

#ifdef HEIST_DIRECTORY_FILE_PATH // @GIVEN-COMPILE-PATH
int compile_script(char *argv[], const int& compile_pos, std::string& compile_as){
  // Compile the script & immediately exit
  heist::scm_list compile_args(2);
  compile_args[0] = heist::make_str(argv[compile_pos]);
  compile_args[1] = heist::make_str(compile_as);
  try {
    heist::primitive_COMPILE(compile_args);
    heist::close_port_registry();
  } catch(const heist::SCM_EXCEPT& eval_throw) {
    if(eval_throw == heist::SCM_EXCEPT::JUMP)
      PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
        << PROFILE(heist::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
    /* catch errors already output to stdout */ 
    putchar('\n');
  } catch(...) {
    /* catch uncaught C++ exceptions -:- ANOMALY -:- */
    PRINT_ERR(afmt(heist::AFMT_1) << 
      "\nUncaught C++ Exception Detected! -:- BUG ALERT -:-"
      "\n  => While Compiling script \"" << argv[compile_pos] << "\""
      "\n  => Please send your code to jrandleman@scu.edu to fix"
      "\n     the interpreter's bug!"
      "\n  => Terminating Heist Scheme Interpretation.\n\n" << afmt(heist::AFMT_0));
    heist::close_port_registry();
    return 1;
  }
  return 0;
}
#endif

/******************************************************************************
* MAIN INTERPRETER EXECUTION
******************************************************************************/

int main(int argc, char *argv[]) {
  // Validate arguments
  int script_pos = -1, compile_pos = -1;
  std::string compile_as = "HEIST_COMPILER_OUTPUT.cpp";
  if(!confirm_valid_command_line_args(argc,argv,script_pos,compile_pos,compile_as)) 
    return 1;
  // Set up the environment (allocates & fills GLOBAL_ENVIRONMENT_POINTER)
  heist::set_default_global_environment();
  // Interpret a Script (as needed)
  if(script_pos != -1) 
    return load_script(argv, script_pos);
  // Compile a Script (as needed)
  #ifdef HEIST_DIRECTORY_FILE_PATH // @GIVEN-COMPILE-PATH
  if(compile_pos != -1) 
    return compile_script(argv, compile_pos, compile_as);
  #endif
  // Run the REPL
  puts("Heist Scheme Version 5.0");
  driver_loop();
  heist::close_port_registry();
  return 0;
}

/******************************************************************************
* MAIN COMPILED-AST EXECUTION
******************************************************************************/

#else // @ONLY-COMPILER
void interpret_premade_AST_code(){
  heist::set_default_global_environment();
  POPULATE_HEIST_PRECOMPILED_READ_AST_EXPS();
  for(const auto& input : HEIST_PRECOMPILED_READ_AST_EXPS) {
    try {
      heist::scm_eval(heist::scm_list_cast(input),heist::GLOBAL_ENVIRONMENT_POINTER);
    } catch(const heist::SCM_EXCEPT& eval_throw) {
      if(eval_throw == heist::SCM_EXCEPT::EXIT) return;
      if(eval_throw == heist::SCM_EXCEPT::JUMP)
        PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
          << PROFILE(heist::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
    } catch(...) {
      PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
           "\n     Triggered By: " << input << 
           "\n  => Please send your source & compiled code to jrandleman@scu.edu to fix"
           "\n     the interpreter's bug!"
           "\n  => Terminating Heist Scheme Interpretation.");
      return;
    }
  }
}

int main() {
  interpret_premade_AST_code(); 
  heist::close_port_registry();
  return 0;
}
#endif // @ONLY-COMPILER
#undef HEIST_DIRECTORY_FILE_PATH
#undef ERR_HEADER
#undef BAD_SYNTAX
#undef EXP_ERR
#undef FCN_ERR
#undef PROFILE
#undef PRINT_ERR
#undef THROW_ERR
#endif // @NOT-EMBEDDED-IN-C++
#endif