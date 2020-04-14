// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_scm.cpp
// => Main execution and AST evaluation for the C++ Heist Scheme Interpreter

/***
 * COMPILE: 
 *   $ g++ -std=c++17 -O3 -o heist_scm heist_scm.cpp
 *
 * FLAG DESCRIPTIONS:
 *   0. "-std=c++17": [REQUIRED] compile using the C++17 standard
 *   1. "-O3": [HIGHLY RECOMMENDED TO EXECUTE] maximum optimization
 *             -> longer compile time, BUT smaller binary & faster execution
 *
 * ON COMPILE TIME:
 *   0. Full -O3 compilation averages at 45s. Be patient. Compilation time
 *      has been traded for fast runtime, and boyo is it wicked fast. Plus
 *      you're only compiling once, then never again (only interpreting).
 */

// +-----------------------------------------------------------------------+
// | It is no exaggeration to regard this as the most fundamental idea in  |
// | programming:                                                          |
// |    The evaluator, which determines the meaning of expressions in a    |
// |    programming language, is just another program. - SICP vol2, p.360  |
// +-----------------------------------------------------------------------+

/***
 * WARNING: NO CALL/CC / TRANSCRIPTS
 *
 * NUMBER SYSTEM:
 *   - EXACT INTERGERS (UNBOUND) 
 *   - EXACT FRACTIONS (18 DIGITS OF PRECISION, >18 COLLAPSES TO INEXACT FLOAT)
 *   - INEXACT FLOATS  (LONG DOUBLE)
 *   - UNSUPPORTED NUMERICS:
 *     > NUMBER::RECTANGULAR
 *     > NUMBER::POLAR
 *     > NUMBER::COMPLEX
 *     > S F D L INEXACT PRECISIONS (ALWAYS 18 DIGITS)
 *
 * CHARS: USE ASCII ENCODING
 *
 * R4RS EXTENSIONS:
 *   - NATIVE EVEN STREAMS     ; LISTS WITH DELAYED CAR & CDR
 *   - EVAL                    ; EVALUATE SYMBOLIC DATA AS CODE
 *   - READ-STRING             ; READ DATA FROM A STRING LITERAL AS IF A PORT
 *   - RECURSIVE DEPTH CONTROL ; SET THE INTERPRETER'S MAX RECURSION DEPTH
 *   - VECTOR-LITERAL          ; LONG-HAND VARIANT OF THE #( PREFIX
 *   - VARIOUS PRIMITIVES      ; delay?, file?, vector-map, ETC.
 *   - MACROS                  ; MATCH SYMBOL-LIST PATTERNS TO TEMPLATES
 *     > DEFINE-SYNTAX
 *     > LET-SYNTAX
 *     > LETREC-SYNTAX
 *     > SYNTAX-RULES
 */

/***
 * RESERVED WORDS -VS- SPECIAL FORMS -VS- PRIMITIVES: 
 *   (A) RESERVED WORDS:
 *       - DEFN: IMPLEMENTATION-SPECIFIC RESERVED PHRASES, INDEPENDANT OF STANDARD SCHEME
 *       - PROPERTIES: _**USE BY USER IS UNDEFINED BEHAVIOR**, HARD-CODED INTO INTERPRETER'S PARSER_
 *       - EXAMPLES:
 *         * HEIST-NIL-ARG   -> VOID ARGUMENT NAME
 *         * HEIST-DO-LETREC -> DO-ITERATION RECURSIVE CALLBACK NAME
 *         * HEIST-PROCEDURE -> PROCEDURE TAG
 *         * HEIST-PRIMITIVE -> PRIMITIVE PROCEDURE TAG
 *
 *   (B) SPECIAL FORMS:
 *       - DEFN: EXPLICIT TOKEN STRINGS PARSED FOR BY THE INTERPRETER
 *       - PROPERTIES: _**MAY NOT BE REDEFINED** BY USER AT RUN-TIME, HARD-CODED_
 *       - EXAMPLES:
 *         * '
 *         * `
 *         * ,
 *         * ,@
 *         * .
 *         * '()
 *         * #()
 *         * #\
 *         * =>
 *         * quote
 *         * quasiquote
 *         * unquote
 *         * unquote-splicing
 *         * define-syntax
 *         * let-syntax
 *         * letrec-syntax
 *         * syntax-rules
 *         * lambda
 *         * define
 *         * set!
 *         * begin
 *         * delay
 *         * if
 *         * and
 *         * or
 *         * cond
 *         * case
 *         * let
 *         * let*
 *         * letrec
 *         * do
 *         * scons
 *         * stream
 *         * vector-literal
 *         * exit
 *
 *    (C) PRIMITIVES:
 *       - DEFN: PREMADE C++ FUNCTIONS DEFN'D IN THE GLOBAL ENVIRONMENT
 *       - PROPERTIES: _**MAY BE REDEFINED** BY USER AT RUN TIME_
 *       - EXAMPLES: 
 *         * #t -- 'true'
 *         * #f -- 'false'
 *         * stream-null
 *         * null-environment
 *         * local-environment
 *         * set-max-recursion-depth!
 *         * SEE "heist_primitives.hpp" FOR THE ALL PRIMITIVE IMPLEMENTATIONS
 */

#include <iterator>
#include "heist_types.hpp"
#include "heist_primitives.hpp"
#include "heist_input_parser.hpp"

/******************************************************************************
* THE CIRCULAR EVALUATOR
******************************************************************************/

scm_list scm_eval(scm_list&& exp, env_type& env);
exe_type scm_analyze(scm_list&& exp);

/******************************************************************************
* AST-ANALYSIS HELPER FUNCTIONS
******************************************************************************/

// Confirm whether list begins w/ designated symbol
bool is_tagged_list(const scm_list& exp, const char * const tag) {
  return (exp[0].is_type(types::sym) && exp[0].value.sym == tag);
}


// 'Casts' a data object to a scm_list object
//   => If data = argless application, adds sentinel-arg
//   => If data contains a scm_list, degrades data to scm_list
//   => Else, wraps data in a scm_list
scm_list scm_list_cast(const data& d) {
  // if at an argless application (ie a single entity in an expression)
  if(d.is_type(types::exp) && d.value.exp.size()==1) {
    scm_list tmp({d.value.exp[0]}); // add sentinel-arg to argless application
    tmp.push_back(scm_list({symconst::quote, symconst::sentinel}));
    return tmp;
  }
  return d.is_type(types::exp) ? d.value.exp : scm_list({d});
}


// 'Casts' a scm_list object to a data object
//   => If scm_list only contains 1 data object, degrades scm_list to the data
//   => Else, wraps scm_list in a datum
data data_cast(const scm_list& l) {return l.size()==1 ? l[0] : data({l});}


// Generate a call signature from a procedure name & its given values
sym_type procedure_call_signature(const sym_type& name, const frame_vals& vals) {
  if(no_args_given(vals) || data_is_the_SENTINEL_VAL(data(vals)))
    return '(' + name + ')';
  return '(' + name + ' ' + cio_expr_str(vals).substr(1);
}


// Generate an improper procedure call error message
sym_type improper_call_alert(sym_type name, const frame_vals& vals,
                                            const frame_vars& vars){
  if(name.empty()) name = " #<procedure>"; // anonymous lambda
  name.erase(0,1);                         // rm initial ' '
  // Generate the call signature
  auto call_signature = procedure_call_signature(name, vals);
  // Generate the definition signature (w/o sentinel arg)
  sym_type defn_signature('(' + name);
  if(vars.size() != 1 || vars[0] != symconst::sentinel)
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
  if(n != 0 && vars[n-1].value.sym == ".")
    THROW_ERR("Expected one item after variadic dot (.)! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
  // Search the vars list of the fcn's args for improper (.) use & duplicate arg names
  for(size_type i = 0; i < n; ++i) {
    if(!vars[i].is_type(types::sym)) // args must be symbols
      THROW_ERR("Non-Symbolic parameter [ "<<vars[i]<<" ] is an invalid arg name! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
    if(i+2 != n && vars[i].value.sym == ".") // variadic (.) must come just prior the last arg
      THROW_ERR("More than one item found after variadic dot (.)! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
    for(size_type j = i+1; j < n; ++j)
      if(vars[i].value.sym == vars[j].value.sym) // duplicate arg name detected
        THROW_ERR("Duplicate arg name \""<<vars[i]<<"\" supplied! -- ANALYZE_LAMBDA"<<EXP_ERR(exp));
  }
}


// Confirm valid argument layout for variable assignment
void confirm_valid_assignment(const scm_list& exp) {
  if(exp.size() != 3)
    THROW_ERR("'set! didn't recieve 2 arguments: (set! <var> <val>)" << EXP_ERR(exp));
  if(!exp[1].is_type(types::sym))
    THROW_ERR("'set! 1st arg " << PROFILE(exp[1]) << " can't be reassigned"
      " (only symbols)!\n     (set! <var> <val>)" << EXP_ERR(exp));
}


// Confirm valid argument layout for variable & procedure definitions
void confirm_valid_definition(const scm_list& exp) {
  if(exp.size() < 3)
    THROW_ERR("'define didn't recieve enough arguments!\n     (define <var> <val>)"
      "\n     (define (<procedure-name> <args>) <body>)" << EXP_ERR(exp));
  if(!exp[1].is_type(types::sym) && !exp[1].is_type(types::exp))
    THROW_ERR("'define 1st arg [ " << exp[1] << " ] of type \"" 
      << exp[1].type_name() << "\" can't be defined (only symbols):"
      "\n     (define <var> <val>)"
      "\n     (define (<procedure-name> <args>) <body>)" << EXP_ERR(exp));
  if(exp[1].is_type(types::exp) && 
      (exp[1].value.exp.empty() || !exp[1].value.exp[0].is_type(types::sym)))
    THROW_ERR("'define procedure name [ " 
      << (exp[1].value.exp.empty() ? data("undefined") : exp[1].value.exp[0]) << " ] of type \"" 
      << (exp[1].value.exp.empty() ? "undefined" : exp[1].value.exp[0].type_name())
      << "\" is invalid (must be a symbol)!"
      "\n     (define <var> <val>)"
      "\n     (define (<procedure-name> <args>) <body>)" << EXP_ERR(exp));
  if(exp[1].is_type(types::sym) && exp.size() > 3)
    THROW_ERR("'define can only define 1 value to a variable (recieved " << exp.size()-2 << " vals)!"
      "\n     (define <var> <val>)"
      "\n     (define (<procedure-name> <args>) <body>)" << EXP_ERR(exp));
}


// Confirm valid argument layout for a lambda
void confirm_valid_lambda(const scm_list& exp) {
  if(exp.size() < 3)
    THROW_ERR("'lambda didn't receive enough args: (lambda (<args>) <body>)"
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
void transform_variadic_vals_into_a_list(frame_vars& vars, frame_vals& vals) {
  const size_type va_arg_idx = vars.size()-2;
  // Transform the arg names & vals as needed
  vars[va_arg_idx] = vars[va_arg_idx+1]; // shift up variadic arg name (erasing '.')
  vars.erase(vars.end()-1, vars.end());  // erase the now-duplicate var-arg name
  data list_of_vals;
  if(no_args_given(vals))
    list_of_vals = symconst::emptylist;
  else {
    scm_list values(vals.begin()+va_arg_idx, vals.end());
    list_of_vals = primitive_LIST(values);
  }
  vals.erase(vals.begin()+va_arg_idx, vals.end()); // erase individual arg instances
  vals.push_back(list_of_vals);                    // reinsert args as a list
}


// Confirm given no args & NOT applying a void-arg fcn & NOT a variadic-arg fcn
bool invalid_sentinel_arg_use(const frame_vars& vars, const frame_vals& vals) {
  return no_args_given(vals) &&
          !(vars.size()==1 && vars[0]==SENTINEL_ARG) && 
          !(vars.size()==2 && vars[0]==".");
}


// Confirm passing an arg to an argless procedure
bool given_arg_for_argless_fcn(const frame_vars& vars,const frame_vals& vals){
  return !no_args_given(vals) && vars.size()==1 && vars[0]==SENTINEL_ARG;
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
frame_vars& frame_variables(frame_t& f) {return std::get<0>(f);}
frame_vals& frame_values(frame_t& f)    {return std::get<1>(f);}
frame_macs& frame_macros(frame_t& f)    {return std::get<2>(f);}


// -- ENVIRONMENTAL EXTENSION
env_type extend_environment(frame_vars vars, frame_vals& vals, env_type& base_env, 
                                                         const sym_type& name = ""){
  bool valid_env_extension = confirm_valid_environment_extension(vars,vals,name);
  // If valid extension, return environment w/ a new frame prepended
  if(valid_env_extension) {
    env_type extended_env(make_env());
    extended_env->push_back(make_frame(frame_t(vars,vals,frame_macs{})));
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
bool is_single_self_evaluating_object_in_list(frame_val& val) {
  return val.is_type(types::exp) && val.value.exp.size()==1;
}
void set_variable_value(const frame_var& var, frame_val&& val, env_type& env) {
  if(is_single_self_evaluating_object_in_list(val)) val = val.value.exp[0];
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
  THROW_ERR("Variable " << var << " is not bound!" 
    << EXP_ERR(scm_list({symconst::set, var, val})));
}


// -- VARIABLE DEFINITION: (define <var> <val>)
void define_variable(const frame_var& var, frame_val val, env_type& env) {
  if(env->empty()) // add an empty frame to if given an empty environment
    env->push_back(make_frame(frame_t{})); 
  if(is_single_self_evaluating_object_in_list(val)) val = val.value.exp[0];
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
//    => NOTE: EXTENDS R4RS SCHEME
void define_syntax_extension(const frame_mac& mac, env_type& env) {
  if(env->empty()) // add an empty frame to if given an empty environment
    env->push_back(make_frame(frame_t{})); 
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
  MACRO_LABEL_REGISTRY.push_back(mac.label);
}

/******************************************************************************
* REPRESENTING CONDITIONALS: (if <predicate> <consequent> <alternative>)
******************************************************************************/

// -- IDENTIFICATION, GETTERS, & CONSTRUCTION
bool is_if(const scm_list& exp)       {return is_tagged_list(exp, "if");}
scm_list if_predicate(scm_list& exp)  {return scm_list_cast(exp[1]);}
scm_list if_consequent(scm_list& exp) {return scm_list_cast(exp[2]);}

scm_list if_alternative(scm_list& exp){
  if(exp.size() == 4)                   // if has an <alternative>
    return scm_list_cast(exp[3]);
  return scm_list({types::dne}); // w/o <alternative> return VOID
}

scm_list make_if(scm_list predicate, scm_list consequent, scm_list alternative) {
  return scm_list({symconst::if_t, predicate, consequent, alternative});
}


// -- PREDICATE TESTING
bool is_true(const scm_list& exp) { // true is not false
  return !exp[0].is_type(types::bol) || exp[0].value.bol.val;
}
bool is_false(const scm_list& exp) { // false is false
  return exp[0].is_type(types::bol) && !exp[0].value.bol.val;
}


// -- ANALYSIS
// Returns lambda so that if true, only eval consequent: 
//   else, only eval alternative
exe_type analyze_if(scm_list& exp) { 
  if(exp.size() < 3) 
    THROW_ERR("IF didn't recieve enough args:"
      "\n     (if <predicate> <consequent> <optional-alternative>)"
      << EXP_ERR(exp));
  if(exp.size() > 4) 
    THROW_ERR("IF recieved too many args:"
      "\n     (if <predicate> <consequent> <optional-alternative>)"
      << EXP_ERR(exp));
  auto pproc = scm_analyze(if_predicate(exp));
  auto cproc = scm_analyze(if_consequent(exp));
  auto aproc = scm_analyze(if_alternative(exp));
  return [pproc=std::move(pproc),cproc=std::move(cproc),
          aproc=std::move(aproc)](env_type& env){
    return is_true(pproc(env)) ? cproc(env) : aproc(env);
  };
}


// -- AND: (and <condition1> <condition2> ...)
bool is_and(const scm_list& exp) {return is_tagged_list(exp,"and");}
// Returns an exec proc to confirm whether all exp's are true
exe_type analyze_and(scm_list& and_exp) {
  const size_type n = and_exp.size();
  auto TRUE_EXP     = scm_list({boolean(true)});
  auto FALSE_EXP    = scm_list({boolean(false)});
  // (and) = #t
  if(n == 1 || data_is_the_SENTINEL_VAL(and_exp[1])) 
    return [TRUE_EXP=std::move(TRUE_EXP)](env_type& env){return TRUE_EXP;};
  // Confirm all true
  return [n,and_exp=std::move(and_exp),FALSE_EXP=std::move(FALSE_EXP)](env_type& env){
    scm_list res;
    for(size_type i = 1; i < n; ++i) // args start at idx=1
      if(res = scm_eval(scm_list_cast(and_exp[i]),env); is_false(res))
        return FALSE_EXP;
    return res;
  };
}


// -- OR: (or <condition1> <condition2> ...)
bool is_or(const scm_list& exp) {return is_tagged_list(exp,"or");}
// Returns an exec proc to confirm whether >= 1 exp is true
exe_type analyze_or(scm_list& or_exp) {
  const size_type n = or_exp.size();
  auto FALSE_EXP    = scm_list({boolean(false)});
  // (or) = #f
  if(n == 1 || data_is_the_SENTINEL_VAL(or_exp[1])) 
    return [FALSE_EXP=std::move(FALSE_EXP)](env_type& env){return FALSE_EXP;};
  // Confirm >= 1 true
  return [n,or_exp=std::move(or_exp),FALSE_EXP=std::move(FALSE_EXP)](env_type& env){
    for(size_type i = 1; i < n; ++i) // args start at idx=1
      if(auto res = scm_eval(scm_list_cast(or_exp[i]),env); is_true(res))
        return res;
    return FALSE_EXP;
  };
}

/******************************************************************************
* REPRESENTING SEQUENCES: (begin <body>)
******************************************************************************/

bool is_begin(const scm_list& exp)    {return is_tagged_list(exp, "begin");}
scm_list begin_actions(scm_list& exp) {return scm_list(exp.begin()+1, exp.end());}

// Analyzes each expression, then returns an exec proc which 
//   sequentially invokes each expression's exec proc
exe_type analyze_sequence(scm_list&& exps){ // used for 'begin' & lambda bodies
  if(exps.empty() || (exps.size()==1 && data_is_the_SENTINEL_VAL(exps[0])))
    return [](env_type& env){return scm_list({data(types::dne)});}; // void data
  const size_type n = exps.size();
  std::vector<exe_type> sequence_exe_procs;
  // Analyze each expression
  for(auto& e : exps)
    sequence_exe_procs.push_back(scm_analyze(scm_list_cast(e)));
  // Return a lambda sequentially invoking each exec procedure
  return [n,sequence_exe_procs=std::move(sequence_exe_procs)](env_type& env) {
    for(size_type i = 0; i+1 < n; ++i)
      sequence_exe_procs[i](env);
    return sequence_exe_procs[n-1](env);
  };
}

// Convert a sequence into a single expression via 'begin
scm_list convert_sequence_exp(scm_list seq) { 
  scm_list begin_sequence({symconst::begin});
  begin_sequence.insert(begin_sequence.end(), seq.begin(), seq.end());
  return begin_sequence;
}

/******************************************************************************
* REPRESENTING ASSIGNMENT: (set! <var> <val>)
******************************************************************************/

bool is_assignment(const scm_list& exp)        {return is_tagged_list(exp, "set!");}
frame_var& assignment_variable(scm_list& exp)  {return exp[1].value.sym;}
scm_list assignment_value(const scm_list& exp) {return scm_list_cast(exp[2]);}

// Analyzes value being assigned, & returns an execution procedure 
//   to install it as the variable in the designated env
exe_type analyze_assignment(scm_list& exp) { 
  confirm_valid_assignment(exp);
  auto& var       = assignment_variable(exp);
  auto value_proc = scm_analyze(assignment_value(exp));
  auto empty_val  = scm_list({data(types::dne)});
  return [var=std::move(var),value_proc=std::move(value_proc),
          empty_val=std::move(empty_val)](env_type& env){
    set_variable_value(var,data_cast(value_proc(env)),env);
    return empty_val; // return is undefined
  };
}

/******************************************************************************
* MANGLING PROCEDURE NAMES: EMBEDDING A PROCEDURE'S NAME WITHIN ITS LAMBDA TAG
******************************************************************************/

frame_var mangled_lambda_name_tag(const frame_var& proc_name) {
  return symconst::mangle_prefix + proc_name;
}

bool is_mangled_lambda_tag(const data& d) {
  return d.is_type(types::sym) && d.value.sym.find(symconst::mangle_prefix)==0;
}

frame_var demangled_lambda_tag(const data& tag) {
  if(is_mangled_lambda_tag(tag))
    return ' ' + tag.value.sym.substr(symconst::mangle_prefix.size());
  return ""; // anonymous procedure
}

/******************************************************************************
* REPRESENTING DEFINITION: (define <var> <val>)
******************************************************************************/

bool is_definition(const scm_list& exp) {return is_tagged_list(exp, "define");}
scm_list make_lambda(scm_list parameters, // Lambda ctor
                     scm_list body, 
                     const frame_var& lambda_tag = symconst::lambda); 

frame_var& definition_variable(scm_list& exp) {
  // if defining a variable, else defining a procedure
  if(exp[1].is_type(types::sym)) return exp[1].value.sym; 
  return exp[1].value.exp[0].value.sym;
}

scm_list definition_value(scm_list& exp, const frame_var& var_name) {
  // if defining a variable, else defining a procedure
  if(exp[1].is_type(types::sym)) return scm_list_cast(exp[2]); 
  scm_list args(exp[1].value.exp.begin()+1,exp[1].value.exp.end());
  scm_list body(exp.begin()+2,exp.end());
  return make_lambda(args,body,mangled_lambda_name_tag(var_name));
}

// Analyzes value being defined, & returns an execution procedure 
//   to install it as the variable in the designated env
exe_type analyze_definition(scm_list& exp) { 
  confirm_valid_definition(exp);
  auto& var       = definition_variable(exp);
  auto value_proc = scm_analyze(definition_value(exp,var));
  auto empty_val  = scm_list({data(types::dne)});
  return [var=std::move(var),value_proc=std::move(value_proc),
          empty_val=std::move(empty_val)](env_type& env){
    define_variable(var,data_cast(value_proc(env)),env);
    return empty_val; // return is undefined
  };
}

/******************************************************************************
* REPRESENTING PROMISES: (delay <expression>)
******************************************************************************/

bool is_delay(const scm_list& exp) {return is_tagged_list(exp,"delay");}

// 'Delay' data struct consists of the delayed expression, its environment, 
//   whether its already been forced, and the result of forcing it (if forced)
scm_list make_delay(const scm_list& exp, env_type& env) {
  return scm_list({ symconst::delay, make_del(exp,env) });
}

// Extracts the delayed expression and returns an exec proc ctor'ing a promise
exe_type analyze_delay(scm_list& exp) {
  if(exp.size() != 2 || data_is_the_SENTINEL_VAL(exp[1])) {
    exp[0].value.sym += ' '; // add space to tag, avoiding #<delay> degradation
    auto delay_call = cio_expr_str(exp);
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
bool is_scons(const scm_list& exp) {return is_tagged_list(exp,"scons");}

exe_type analyze_scons(scm_list& exp) {
  if(exp.size() != 3)
    THROW_ERR("'scons expects 2 arguments: (scons <car> <cdr>)"<<EXP_ERR(exp));
  return scm_analyze(scm_list({
    symconst::cons, scm_list({symconst::delay, exp[1]}), 
                    scm_list({symconst::delay, exp[2]})
  }));
}


// -- stream: (stream <a> <b> ...) = (scons <a> (scons <b> ...))
bool is_stream(const scm_list& exp) {return is_tagged_list(exp,"stream");}

exe_type analyze_stream(scm_list& exp) {
  if(exp.size() == 1 || data_is_the_SENTINEL_VAL(exp[1]))
    return [](env_type& env){return scm_list({symconst::emptylist});};
  return [exp=std::move(exp)](env_type& env) mutable {
    return scm_list({
      primitive_STREAM_to_SCONS_constructor(exp.begin()+1,exp.end(),env)});
  };
}

/******************************************************************************
* REPRESENTING QUOTATION: (quote <expression>)
******************************************************************************/

bool is_quoted(const scm_list& exp) {return is_tagged_list(exp, "quote");}

// Quoting a vector literal is a special case of quotation
bool is_vector_literal(const scm_list& exp) {
  return is_tagged_list(exp, "vector-literal");
}

// Confirm whether quoting a vector literal
bool quoting_a_vector_literal(const scm_list& exp) {
  return exp[1].is_type(types::exp) && !exp[1].value.exp.empty() && 
         is_vector_literal(exp[1].value.exp);
}

// Returns whether data is the (.) symbol
bool data_is_dot_operator(const data& d) {
  return d.is_type(types::sym) && d.value.sym == ".";
}

// Returns quoted data's contents
data text_of_quotation(scm_list& exp) {
  if(!exp[1].is_type(types::sym)) return exp[1];
  if(exp[1].value.sym=="#f" || exp[1].value.sym=="#t")
    return boolean(exp[1].value.sym=="#t");
  return convert_string_to_symbol(exp[1].value.sym);
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
  if(is_quoted_cons(args, "quote"))
    THROW_ERR("'vector-literal had an unexpected dot (.)! -- ANALYZE_QUOTED_VECTOR_LITERAL"
      << EXP_ERR(exp));
  // return an empty vector if given no args
  if(no_args_given(args))
    return [](env_type& env){return scm_list({data(make_vec(scm_list{}))});};
  // quote each item in the vector
  scm_list vector_literal({symconst::vector});
  for(const auto& arg : args)
    vector_literal.push_back(data(scm_list({symconst::quote,arg})));
  // return analyzed vector
  return scm_analyze(std::move(vector_literal));
}


// Analyzes the quote's text & returns an execution procedure for such
exe_type analyze_quoted(scm_list& exp) {
  if(exp.size() != 2 || data_is_the_SENTINEL_VAL(exp[1])) 
    THROW_ERR("'quote form expects one argument: (quote <quoted-data>)!"<<EXP_ERR(exp));
  
  // Quote vector literals as needed
  if(quoting_a_vector_literal(exp))
    return analyze_quoted_vector_literal(exp[1].value.exp);
  
  // Get quoted data
  auto quoted_data = text_of_quotation(exp);
  
  // If quoted data is atomic, return as-is
  if(!quoted_data.is_type(types::exp)) 
    return [quoted_data=scm_list({quoted_data})](env_type& env){
      return quoted_data;
    };
  
  // If quoting an empty expression, return the empty list
  if(quoted_data.value.exp.empty())
    return [](env_type& env){return scm_list({symconst::emptylist});};
  
  // If quoted an expression, expand such into a list of quoted data
  scm_list quote_val;
  quote_val.push_back(symconst::list);
  
  // Confirm whether appending last item. 
  //   => NOTE: also rm's (.) if so, hence this must be done 
  //            PRIOR quoting each item
  bool append_last_item = is_quoted_cons(quoted_data.value.exp,"quote");
  
  // Wrap 'quote' around each item in the list
  for(const auto& d : quoted_data.value.exp) 
    quote_val.push_back(data(scm_list({symconst::quote, d})));
  
  // Unpack the last item in the list and append it if quoting 
  //   a non-null-terminated list
  if(append_last_item) {
    auto last_item = *quote_val.rbegin();
    quote_val.pop_back();
    quote_val = scm_list({symconst::append, quote_val, last_item});
  }
  
  // Return the analyzed quoted list expression
  return scm_analyze(std::move(quote_val));
}

/******************************************************************************
* REPRESENTING SELF-EVALUATING EXPRESSIONS & VARIABLES
******************************************************************************/

bool is_self_evaluating(const scm_list& exp) {
  return exp.size() == 1 && 
         (exp[0].is_type(types::num) || exp[0].is_type(types::str) || 
          exp[0].is_type(types::chr) || exp[0].is_type(types::par) || 
          exp[0].is_type(types::vec) || exp[0].is_type(types::bol) ||
          exp[0].is_type(types::undefined));
}

bool is_variable(const scm_list& exp) {return (exp[0].is_type(types::sym));}

/******************************************************************************
* REPRESENTING PROCEDURES
******************************************************************************/

// PROCEDURE CONSTRUCTION
scm_list make_procedure(const scm_list& parameters,
                        const exe_type& body_proc,
                        env_type& env,
                        const frame_var& name) {
  return scm_list({ symconst::procedure, parameters, 
                    body_proc, env, make_cal(0), name });
}

bool is_compound_procedure(const scm_list& p)     {return is_tagged_list(p, PROCEDURE_TAG);}
exe_type procedure_body(scm_list& p)              {return p[2].value.exe;}
env_type& procedure_environment(scm_list& p)      {return p[3].value.env;}
size_type& procedure_recursive_depth(scm_list& p) {return *p[4].value.cal;}

frame_var procedure_name(const scm_list& p) {
  if(is_compound_procedure(p)) // compound procedure name
    return p[5].value.sym;
  return ' ' + p[2].value.sym; // primitive procedure name
}

frame_vars procedure_parameters(scm_list& p)  {
  frame_vars var_names;
  for(const auto& param : p[1].value.exp)
    var_names.push_back(param.value.sym);
  return var_names;
}


// -- LAMBDAS: (lambda (<parameters>) <body>)
bool is_lambda(const scm_list& exp)        {return is_tagged_list(exp,"lambda") || 
                                                   is_mangled_lambda_tag(exp[0]);}
scm_list  lambda_parameters(scm_list& exp) {return exp[1].value.exp;}
scm_list  lambda_body(scm_list& exp)       {return scm_list(exp.begin()+2,exp.end());}
frame_var lambda_name(scm_list& exp)       {return demangled_lambda_tag(exp[0]);}

// Ctor for lambdas
//   => NOTE: "lambda_tag" != "lambda" iff ctoring the value of a procedural
//            definition, wherein the tag is also mangled w/ the procedure name
scm_list make_lambda(scm_list parameters,scm_list body,const frame_var& lambda_tag){
  scm_list new_lambda({lambda_tag, parameters});
  if(new_lambda[1].value.exp.empty()) // add the sentinel arg as needed
    new_lambda[1].value.exp.push_back(symconst::sentinel);
  new_lambda.insert(new_lambda.end(), body.begin(), body.end());
  return new_lambda;
}

// Returns an exec proc to mk a lambda w/ the analyzed parameter list & body
exe_type analyze_lambda(scm_list& exp) {
  confirm_valid_lambda(exp);
  auto vars = lambda_parameters(exp);
  confirm_valid_procedure_parameters(vars,exp);        // validate parameters
  if(vars.empty()) vars.push_back(symconst::sentinel); // add sentinel-arg
  auto body_proc = analyze_sequence(lambda_body(exp)); // analyze body syntax
  auto name      = lambda_name(exp);
  return [vars=std::move(vars),body_proc=std::move(body_proc),name=std::move(name)]
    (env_type& env){
      return make_procedure(vars, body_proc, env, name);
    };
}


// -- PROCEDURAL APPLICATION
bool is_application(const scm_list& exp) {return exp.size()>1;}
scm_list operator_of(scm_list& exp)      {return scm_list_cast(exp[0]);}
scm_list operands(scm_list& exp)         {return scm_list(exp.begin()+1, exp.end());}

/******************************************************************************
* DERIVING COND: (cond <clause1> ... <clauseN>)
******************************************************************************/

bool is_cond(const scm_list& exp) {return is_tagged_list(exp, "cond");}


// -- CLAUSE VALIDATION
void confirm_valid_clause(const scm_node& current_clause, const scm_list& exp){
  if(!current_clause->is_type(types::exp) || 
    current_clause->value.exp.empty()     || 
    data_is_the_SENTINEL_VAL(current_clause->value.exp[0]))
    THROW_ERR("Invalid COND clause [ " << *current_clause << " ], expects:"
      "\n     (cond <clause1> <clause2> ...)"
      "\n     <clause> = (<condition> <consequent>)  -- CONVERT_COND_IF"
      << EXP_ERR(exp));
}


// -- CLAUSE GETTERS
scm_list cond_predicate(scm_node& clause) {
  if(clause->value.exp[0].is_type(types::exp))
    return clause->value.exp[0].value.exp;                  // expression
  return scm_list({symconst::and_t, clause->value.exp[0]}); // variable as an expression
}

scm_list cond_actions(scm_node& clause) {
  return convert_sequence_exp(scm_list(
          clause->value.exp.begin()+1,clause->value.exp.end()));
}


// -- CLAUSE ANALYSIS
bool is_cond_else_clause(const scm_node& clause){
  return !clause->value.exp.empty() && is_tagged_list(clause->value.exp,"else");
}

bool is_cond_arrow_clause(const scm_node& clause, const scm_list& exp) {
  const bool has_cond_arrow_notation = clause->value.exp.size() > 1 && 
                                       clause->value.exp[1].is_type(types::sym) && 
                                       clause->value.exp[1].value.sym == "=>";
  if(has_cond_arrow_notation && clause->value.exp.size() != 3)
    THROW_ERR("COND Clause '=> Notation didn't receive 3 args:"
      "\n     (cond <clause1> <clause2> ...)"
      "\n     (<condition> => <procedure>)  -- CONVERT_COND_IF"
      << EXP_ERR(exp));
  return has_cond_arrow_notation;
}

data cond_arrow_procedure(scm_node& clause) {
  return clause->value.exp[2];
}


// -- CLAUSE EXPANSION
// Each clause consists of a <condition> & a <body>
scm_list expand_clauses(scm_node& current_clause, const scm_node& end_clauses, const scm_list& exp) {
  // no else clause -- return is now implementation-dependant, I've chosen VOID
  if(current_clause == end_clauses) return scm_list({symconst::and_t, data(types::dne)});
  confirm_valid_clause(current_clause,exp);

  // Set <test> as <consequent> if only given a <test> in the clause
  if(current_clause->value.exp.size() == 1)
    current_clause->value.exp.push_back(current_clause->value.exp[0]);

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
    return make_if(cond_predicate(current_clause), 
                   scm_list({cond_arrow_procedure(current_clause),
                             cond_predicate(current_clause)}),
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

bool is_let(const scm_list& exp)              {return is_tagged_list(exp, "let");}
bool is_named_let(const scm_list& exp)        {return exp[1].is_type(types::sym);}
sym_type named_let_name(const scm_list& exp)  {return exp[1].value.sym;}
scm_list& let_parameters(scm_list& exp)       {return exp[1].value.exp;}
scm_list& named_let_parameters(scm_list& exp) {return exp[2].value.exp;}
scm_list  let_body(scm_list& exp)             {return scm_list(exp.begin()+2,exp.end());}
scm_list  named_let_body(scm_list& exp)       {return scm_list(exp.begin()+3,exp.end());}


// PARAMETERS = (<varN> <valN>) OF 'let
// Return list of parameter <var> names
scm_list let_variables(const scm_list& parameters) {
  scm_list vars; // return var names
  for(const auto& param : parameters)
    vars.push_back(param.value.exp[0]);
  return vars;
}

// Return list of parameter <val> values
scm_list let_expressions(const scm_list& parameters) {
  // returns variable values (ie their assinged expressions) of parameters
  scm_list exps;
  for(const auto& param : parameters)
    exps.push_back(param.value.exp[1]);
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
  auto params = named ? named_let_parameters(exp) : let_parameters(exp);

  // add the sentinel arg as a var name, if 'let' was given no parameters
  if(params.empty()) {
    params.push_back(scm_list{}); // () => ((SENTINTEL_ARG, SENTINTEL_VAL))
    params[0].value.exp.push_back(symconst::sentinel);
    params[0].value.exp.push_back(scm_list({symconst::quote, symconst::sentinel}));
  }

  // retreive let expressions & body
  const auto exprs             = let_expressions(params);
  const auto let_body_getter   = named ? named_let_body : let_body;
  data       let_lambda_object = data(make_lambda(let_variables(params),let_body_getter(exp)));

  // convert let into a lambda (both named & unnamed)
  if(named) {
    const auto let_name_symbol = named_let_name(exp);
    scm_list lambda_defn({
      symconst::define, 
      let_name_symbol,
      let_lambda_object
    });
    scm_list self_invocation({let_name_symbol});
    // push back each expression as an arg for the self-invoked-lambda call
    self_invocation.insert(self_invocation.end(), exprs.begin(), exprs.end());
    // bind lambda to name & immediately invoke
    return convert_sequence_exp(scm_list({ lambda_defn, self_invocation })); 
  
  } else {
    scm_list nameless_self_invoke({let_lambda_object});
    // push back each expression as an arg for the self-invoked-lambda call
    nameless_self_invoke.insert(nameless_self_invoke.end(), exprs.begin(), exprs.end());
    // immediately invoke lambda w/ exps for vars
    return nameless_self_invoke; 
  }
}

/******************************************************************************
* DERIVING LET*: (let* ((<var1> <val1>) ... (<varN> <valN>)) <body>)
******************************************************************************/

// -- LET*: "LET", BUT VARIABLES CAN INVOKE ONE ANOTHER IN BINDINGS
bool is_let_star(const scm_list& exp) {return is_tagged_list(exp, "let*");}

scm_list make_let(scm_list&& variable_bindings, scm_list&& body) {
  return scm_list({symconst::let, variable_bindings, body});
}

// Recursively nest lets, each containing one of let*'s parameters
scm_list nest_lets(scm_node param, const scm_node& empty_param, scm_list& exp) {
  if(param == empty_param)
    return convert_sequence_exp(let_body(exp));
  else
    return convert_let_combination( // transform into combination immediately
            make_let(scm_list({*param}), nest_lets(param+1,empty_param,exp))
          );
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
bool is_letrec(const scm_list& exp) {return is_tagged_list(exp, "letrec");}

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
  scm_list dflt_value_params;
  for(const auto& var : vars) // var name bound to undefined data type
    dflt_value_params.push_back(scm_list({var, data{}})); 
  // Set the each var's value w/in the 'letrec's body
  scm_list body_with_assignment;
  for(size_type i = 0, n = vars.size(); i < n; ++i)
    body_with_assignment.push_back(
      scm_list({ symconst::set, vars[i], vals[i] })
    );
  // Append elts in the body, now post-assignment
  body_with_assignment.insert(body_with_assignment.end(), body.begin(), body.end());

  // Define a new let, un-recursed
  scm_list unrec_let({ symconst::let,dflt_value_params });
  // push each new exp w/in the body that has assignments
  unrec_let.insert(unrec_let.end(), body_with_assignment.begin(), body_with_assignment.end());
  return convert_let_combination(unrec_let);
}

/******************************************************************************
* DERIVING CASE
******************************************************************************/

// CASE => (case <val> ((<keys1>) <exp1>) ... (else <expN>))
//      => (cond ((memv <val> <keys1>) <exp1>) ... (else <expN>))
bool is_case(const scm_list& exp) {return is_tagged_list(exp, "case");}

// Confirm 'case clause is correctly formatted
void confirm_valid_case_clause(const scm_node& clause,        const char* format, 
                               const size_type& clause_count, const scm_list& exp) {
  // Confirm clause is an expression
  if(!clause->is_type(types::exp))
    THROW_ERR("'case clause #" << clause_count << " [ " << *clause << " ] isn't an expression!" 
      << format << EXP_ERR(exp));
  // Confirm clause contains 2 elts
  if(clause->value.exp.size() != 2)
    THROW_ERR("'case clause #" << clause_count << " isn't an expression of length 2 (length = "
      << (no_args_given(clause->value.exp) ? 0 : clause->value.exp.size()) << ")!" 
      << format << EXP_ERR(exp));
  // Confirm clause was given a list of keys
  if(!is_cond_else_clause(clause) && !clause->value.exp[0].is_type(types::exp))
    THROW_ERR("'case clause #" << clause_count << " [ " << clause->value.exp[0]
      << " ] doesn't have a list of keys as its 1st arg!" << format << EXP_ERR(exp));
}

// Construct a 'cond equality clause from the 'case clause
scm_list case_equality_clause(scm_node& clause, data& sought_val) {
  data keys_list(scm_list({symconst::list}));
  keys_list.value.exp.insert(keys_list.value.exp.end(), 
    clause->value.exp[0].value.exp.begin(), 
    clause->value.exp[0].value.exp.end());
  return scm_list({symconst::memv, sought_val, keys_list});
}

// Extract a normal & <else> 'case clause
scm_list case_clause(scm_node& clause, data& sought_val) {
  return scm_list({
    case_equality_clause(clause,sought_val),
    cond_actions(clause)
  });
}
scm_list case_else_clause(scm_node& clause) {
  return scm_list({ symconst::else_t, data_cast(cond_actions(clause)) });
}

// Transform 'case into 'cond
scm_list convert_case_cond(scm_list& exp) {
  static constexpr const char * const format = "\n     (case <val> <clause1> ... <clauseN>)"
                                               "\n     <clause> = (<keys-list> <consequent>)";
  if(exp.size() < 3)
    THROW_ERR("CASE expression didn't receive enough args:" << format << EXP_ERR(exp));
  data& sought_val = exp[1];
  size_type clause_count = 1;
  scm_list converted_case({symconst::cond});
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

// (do ((<var-name init-val val-manip>) ...)
//     ((<test>) <expression1> ...)
//     <body>)

// (letrec ((<HEIST-DO-LETREC> 
//   (lambda (<var-names>...)
//     (if <test>)
//         (begin <expression1> ...) ; returns <void> if "<expression>" undefined
//         (begin 
//           <body>
//           (set! <val> <val-manip>)...
//           (<HEIST-DO-LETREC> <var-names>...))))))
//   (<HEIST-DO-LETREC> <init-vals>...))

bool is_do(const scm_list& exp) {return is_tagged_list(exp, "do");}

// Confirms do-expression's var_defn_list is valid in structure
void confirm_valid_var_defn_list(const scm_list& var_defn_list, const scm_list& exp) {
  for(const auto& var_defn : var_defn_list) {
    if(!var_defn.is_type(types::exp) || (var_defn.is_type(types::exp) && 
        var_defn.value.exp.size() != 2 && var_defn.value.exp.size() != 3))
      THROW_ERR("DO expression has non-var-defn-expression in var defn list! -- CONVERT_DO_LETREC\n     -> [ "
                  << var_defn << " ]\n     <var-defn> = (<var> <init-val> <optional-iteration-mutation>)"
                  << EXP_ERR(exp));
    else if(!var_defn.value.exp[0].is_type(types::sym))
      THROW_ERR("DO expression has an invalid var name in its var defn list! -- CONVERT_DO_LETREC\n     -> [ "
                  << var_defn.value.exp[0] << " ]\n     <var-defn> = (<var> <init-val> <optional-iteration-mutation>)"
                  << EXP_ERR(exp));
  }
}

// Returns list of var-init-mod inner lists
scm_list do_var_defn_list(const data& defn_exp, const scm_list& exp) {
  if(defn_exp.is_type(types::exp) && defn_exp.value.exp.empty())
    return scm_list(); // empty list, no vars to speak of
  if(!defn_exp.is_type(types::exp))
    THROW_ERR("DO expression expects var defns at position 1 -- CONVERT_DO_LETREC!\n     -> [ "
      << defn_exp << " ]\n     <var-defn> = (<var> <init-val> <optional-iteration-mutation>)"
      << EXP_ERR(exp));
  confirm_valid_var_defn_list(defn_exp.value.exp, exp);
  return defn_exp.value.exp;
}

// Returns a list of variable names being defined
// PRECONDITION: 'var_defn_list' MUST BE VALIDATED
frame_vars do_var_names(const scm_list& var_defn_list, const scm_list& exp) {
  if(var_defn_list.empty()) return frame_vars({SENTINEL_ARG});
  frame_vars names;
  // parse variable names
  for(const auto& var_defn : var_defn_list)
    names.push_back(var_defn.value.exp[0].value.sym);
  // confirm no duplicate variables names
  for(size_type i = 0, n = names.size(); i+1 < n; ++i)
    for(size_type j = i+1; j < n; ++j) 
      if(names[i] == names[j])
        THROW_ERR("DO expression has a duplicate var name in its var defn list -- CONVERT_DO_LETREC!\n     -> [ " 
          << names[i] << " ]\n     <var-defn> = (<var> <init-val> <optional-iteration-mutation>)"
          << EXP_ERR(exp));
  return names;
}

// Returns a list of variabl initial values
// PRECONDITION: 'var_defn_list' MUST BE VALIDATED
scm_list do_var_init_values(const scm_list& var_defn_list) {
  if(var_defn_list.empty()) return scm_list({
    data(scm_list({symconst::quote,symconst::sentinel}))
  });
  scm_list init_values;
  for(const auto& var_defn : var_defn_list)
    init_values.push_back(var_defn.value.exp[1]);
  return init_values;
}

// Returns a list of variable per-iteration value modifications
// PRECONDITION: 'var_defn_list' MUST BE VALIDATED
scm_list do_var_iteration_updates(const scm_list& var_defn_list, 
                                  const frame_vars& names) {
  if(var_defn_list.empty()) return scm_list();
  scm_list modifications;
  // parse variable modifications
  size_type count=0;
  for(const auto& var_defn : var_defn_list) {
    if(var_defn.value.exp.size() != 3)
      modifications.push_back(names[count]);
    else
      modifications.push_back(var_defn.value.exp[2]);
    ++count;
  }
  return modifications;
}


// Returns a list breaking conditions for the do-expression
scm_list do_break_test_exps(const data& break_exp, const scm_list& exp) {
  if(!break_exp.is_type(types::exp) || break_exp.value.exp.empty())
    THROW_ERR("DO expression expects break-condition list at position 2 -- CONVERT_DO_LETREC! -> [ " 
      << break_exp << " ]\n     <break-condition-list> = (<break-condition> <optional-returned-expression>)"
      << EXP_ERR(exp));
  return break_exp.value.exp;
}


// Returns the do-exp's body + variable modifications (per-iteration) wrapped in a 'begin' clause
scm_list do_modified_body(const scm_list& exp, 
                          const frame_vars& names, 
                          const scm_list&& mod_vals) {
  // Add the do-expression's body
  scm_list body(exp.begin()+3, exp.end());
  // Add the modifications of its values (per-iteration)
  if(names.size() > 1 || names[0] != SENTINEL_ARG)
    for(size_type i = 0, n = names.size(); i < n; ++i)
      body.push_back(scm_list({symconst::set, names[i], mod_vals[i]}));
  // Add the recursive call to reiterate the do-expression
  body.push_back(scm_list({symconst::do_label}));
  auto& recursive_call = body[body.size()-1].value.exp;
  for(const auto& name : names) 
    recursive_call.push_back(name);
  return convert_sequence_exp(body); // Return as a 'begin' clause
}


// Returns a lambda to run for each iteration over the loop, until >= 1 break 
//   condition is met
scm_list do_iteration_lambda(const scm_list& body, 
                             const scm_list&& break_test_exps, 
                             const frame_vars& names) {
  // test the break condition
  scm_list conditioned_body({symconst::if_t, break_test_exps[0]});
  // eval each expression & return the last one (iff provided optional exps)
  if(break_test_exps.size() > 1) {
    conditioned_body.push_back(scm_list({symconst::begin}));
    auto& break_exps = conditioned_body.rbegin()->value.exp;
    break_exps.insert(break_exps.end(), break_test_exps.begin()+1, break_test_exps.end());
  // if NOT given the optional exps, return <void>
  } else {
    conditioned_body.push_back(data(types::dne)); 
  }
  // if break !condition, evaluate the body & reset values
  conditioned_body.push_back(body);
  // convert list of names to a scm_list of name symbols
  scm_list names_scm_list;
  for(const auto& name : names)
    names_scm_list.push_back(name);
  return scm_list({symconst::lambda, names_scm_list, conditioned_body});
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
  auto iteration_lambda_body = do_modified_body(exp, var_names, 
                                                do_var_iteration_updates(var_defns,var_names));
  auto iteration_lambda = do_iteration_lambda(iteration_lambda_body, 
                                              do_break_test_exps(exp[2],exp), var_names);
  
  scm_list initial_do_call({symconst::do_label});
  initial_do_call.insert(initial_do_call.end(), var_inits.begin(), var_inits.end());

  scm_list letrec_conversion({symconst::letrec});
  letrec_conversion.push_back(scm_list());
  letrec_conversion[1].value.exp.push_back(scm_list({ 
    symconst::do_label, iteration_lambda 
  }));
  letrec_conversion.push_back(initial_do_call);
  return letrec_conversion;
}

/******************************************************************************
* REPRESENTING QUASIQUOTE, UNQUOTE, UNQUOTE-SPLICING
******************************************************************************/

bool is_quasiquote(const scm_list& exp)       {return is_tagged_list(exp, "quasiquote");}
bool is_unquote(const scm_list& exp)          {return is_tagged_list(exp, "unquote");}
bool is_unquote_splicing(const scm_list& exp) {return is_tagged_list(exp, "unquote-splicing");}

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
data unquote_splicing_atom(const data& d) {
  if(d.is_type(types::sym))
    return data(scm_list({symconst::quote, d}));
  return d;
}


// Confirm whether 'd' is the AST's empty-list representation
bool data_is_the_AST_empty_list(const data& d){
  return d.is_type(types::exp) && d.value.exp.size() == 2 && 
         d.value.exp[0].is_type(types::sym) && 
         d.value.exp[0].value.sym == "quote" && 
         d.value.exp[1].is_type(types::sym) && 
         d.value.exp[1].value.sym == THE_EMPTY_LIST;
}


// Expands the given pair into a scm expression. 
//   => Helper fcn for processing ,@
//   => Returns whether pair sequence was NOT null-terminated.
// PRECONDITION: 'pair_object' MUST BE ACYCLIC
bool expand_list_into_exp(par_type& pair_object, scm_list& exp) {
  // unpack car
  exp.push_back(unquote_splicing_atom(pair_object->first));
  // unpack cdr
  if(pair_object->second.is_type(types::par))
    return expand_list_into_exp(pair_object->second.value.par, exp);
  // don't push the terminating '() if at the end of the list/pair-sequence
  else if(is_not_THE_EMPTY_LIST(pair_object->second)) {
    exp.push_back(unquote_splicing_atom(pair_object->second));
    return true;
  }
  return false;
}


// Returns an expression object containing the unquoted list/vector's elements.
//   => Returns whether the expanded sequence was NOT null-terminated.
enum class unsplice_status {cons, list, atom};
unsplice_status process_unquote_splicing(const scm_list& exp, env_type& env, scm_list& spliceable_data) {
  if(exp.size() != 2)
    THROW_ERR("'unquote-splicing didn't recieve 1 arg!"
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
  return expand_list_into_exp(eval_result.value.par,spliceable_data) ? unsplice_status::cons : unsplice_status::list;
}


// Handle appending 'atomic' or 'cons' data to a quasiquote expression
scm_list quasiquote_append_non_list(scm_list& spliceable_data, scm_list& quote_val,
                                    const scm_list& exp,        const bool& is_cons, 
                                    const bool& quoting_vector, const bool& not_last_elt) {
  static constexpr const char * const bad_vector = 
    "'quasiquote can't append [via ,@] an improper list to a vector!\n     Tried to splice in: ";
  static constexpr const char * const mid_splice = 
    "'quasiquote can't splice [via ,@] an improper list into the middle of a list!\n     Tried to splice in: ";
  // confirm not splicing a cons/atomic into a vector nor mid-list
  if(quoting_vector && is_cons)
    THROW_ERR(bad_vector<<"(cons "<<spliceable_data[0]<<' '<<spliceable_data[1]<<')'<<EXP_ERR(exp));
  if(quoting_vector)
    THROW_ERR(bad_vector<<spliceable_data[0]<<" of type \""<<spliceable_data[0].type_name()<<'"'<<EXP_ERR(exp));
  if(not_last_elt && is_cons)
    THROW_ERR(mid_splice<<"(cons "<<spliceable_data[0]<<' '<<spliceable_data[1]<<')'<<EXP_ERR(exp));
  if(not_last_elt)
    THROW_ERR(mid_splice<<spliceable_data[0]<<" of type \""<<spliceable_data[0].type_name()<<'"'<<EXP_ERR(exp));
  // return the current expression as an 'append' to the quasiquote expression
  if(is_cons)
    return scm_list({symconst::append, quote_val, 
      scm_list({symconst::cons, spliceable_data[0], spliceable_data[1]})
    });
  return scm_list({symconst::append, quote_val, spliceable_data[0]});
}


// tags <quote_val> w/ 'vector 'cons or 'list as per <quoted_exp>
void tag_quote_val(scm_list& quoted_exp, scm_list& quote_val, const scm_list& exp) {
  const bool is_vector = is_vector_literal(quoted_exp);
  const bool is_cons   = is_quoted_cons(quoted_exp, "quasiquote");
  if(is_vector && is_cons) 
    THROW_ERR("'quasiquote found unexpected dot (.) in 'vector-literal! -- ANALYZE_QUOTE_VECTOR"<<EXP_ERR(exp));
  if(is_vector) {
    quoted_exp.erase(quoted_exp.begin(),quoted_exp.begin()+1); // erase 'vector-literal tag
    quote_val.push_back(symconst::vector);
  } else if(is_cons)
    quote_val.push_back(symconst::cons);
  else
    quote_val.push_back(symconst::list);
}


// Recursively unquotes/unquote-splices data as needed w/in quasiquote templates.
// Also quotes each piece of data as needed.
void unquote_quasiquote_template(scm_list& quote_val, scm_list& quoted_exp, env_type& env, 
                                 const scm_list& exp, bool in_nested_template = false){
  // Account for whether splicing into a vector
  bool quoting_a_vector = (quote_val[0].value.sym == symconst::vector);
  
  // May only unquote data of non-nested-template expressions
  const bool NESTED_TEMPLATE = is_quasiquote(quoted_exp);

  // Wrap 'quote' around each obj in the expression, and expand unquote/unquote-splicing instances
  for(size_type i = 0, n = quoted_exp.size(); i < n; ++i) {
    // if quoting an empty expression, push the empty list 
    if(quoted_exp[i].is_type(types::exp) && quoted_exp[i].value.exp.empty()) {
      quote_val.push_back(scm_list({symconst::quote, symconst::emptylist}));

    // if , add the data as-is
    } else if(!in_nested_template && quoted_exp[i].is_type(types::exp) && is_unquote(quoted_exp[i].value.exp)) {
      quote_val.push_back(quoted_exp[i].value.exp[1]);
    
    // if ,@ eval the expression & splice in the resulting list's elts
    } else if(!in_nested_template && quoted_exp[i].is_type(types::exp) && is_unquote_splicing(quoted_exp[i].value.exp)) {
      scm_list spliceable_data;
      auto unsplice_stat = process_unquote_splicing(quoted_exp[i].value.exp, env, spliceable_data);
      // If splicing the empty list, continue (does nothing)
      if(unsplice_stat == unsplice_status::atom && data_is_the_AST_empty_list(spliceable_data[0]))
        continue;

      // If splicing in a cons or atom
      if(unsplice_stat == unsplice_status::cons || unsplice_stat == unsplice_status::atom) {
        quote_val = quasiquote_append_non_list(spliceable_data, quote_val, exp,
                    (unsplice_stat == unsplice_status::cons), quoting_a_vector, (i != n-1));
        return;

      // Otherwise (splicing a list), splice in data as-is
      } else {
        quote_val.insert(quote_val.end(), spliceable_data.begin(), spliceable_data.end());
      }
      
    // else, quote the nested data
    } else {
      // if other expression, also recursively analyze for unquotes
      if(quoted_exp[i].is_type(types::exp)) {
        scm_list inner_exp;
        
        // tag <inner_exp> as needed w/ either 'vector, 'cons, or 'list
        tag_quote_val(quoted_exp[i].value.exp, inner_exp, exp);
        
        // propagate quotations throughout the sub-expression
        unquote_quasiquote_template(inner_exp, quoted_exp[i].value.exp, env, exp, NESTED_TEMPLATE);
        quote_val.push_back(inner_exp);
      
      // if atomic, simply quote the atom
      } else {
        quote_val.push_back(scm_list({symconst::quote, quoted_exp[i]}));
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
  if(!quoted_data.is_type(types::exp)) 
    return [quoted_data=scm_list({quoted_data})](env_type& env){return quoted_data;};

  // If quoting an empty expression, return the empty list
  if(quoted_data.value.exp.empty())
    return [](env_type& env){return scm_list({symconst::emptylist});};

  // If quasiquoted an unquote, unpack its data
  if(is_unquote(quoted_data.value.exp)) {
    data unquoted_data = process_unquoted(quoted_data.value.exp);
    // If an expression, return the exec proc of the analyzed expression
    if(unquoted_data.is_type(types::exp))
      return scm_analyze(std::move(unquoted_data.value.exp));
    // If an atomic, return the exec proc of its evaluation (in case a variable or some such)
    return [unquoted_data=std::move(unquoted_data)](env_type& env){
      return scm_eval(scm_list({unquoted_data}),env);
    };
  }

  // If quasiquoted an expression, expand such into a list/cons of quoted data
  return [quoted_exp=std::move(quoted_data.value.exp),exp=std::move(exp)]
    (env_type& env) {
      // Unquote/Splice-In data as needed throughout the quasiquote template
      scm_list quote_val, mutable_quoted_exp = quoted_exp;
      // tag <quote_val> as needed w/ either 'vector, 'cons, or 'list
      tag_quote_val(mutable_quoted_exp, quote_val, exp);
      // propagate quotations throughout the sub-expression
      unquote_quasiquote_template(quote_val,mutable_quoted_exp,env,exp);
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
bool duplicate_pattern_object_name_instance(const sym_type& obj1_name,const data& obj2);

// Confirm whether the given word is a keyword
bool is_keyword(const sym_type& word, const frame_vars& keywords) {
  return std::find(keywords.begin(), keywords.end(), word) != keywords.end();
}


// Determine whether only the par's elt OR only the arg's elt is a keyword
bool mismatched_keywords(const data& pat_elt, const data& arg_elt, const frame_vars& keywords) {
  const bool pat_is_key = pat_elt.is_type(types::sym) && is_keyword(pat_elt.value.sym,keywords);
  const bool arg_is_key = arg_elt.is_type(types::sym) && is_keyword(arg_elt.value.sym,keywords);
  return (pat_is_key && !arg_is_key) || (!pat_is_key && arg_is_key);
}


// Confirm whether 'pattern' is argless but was given 'args' (or vise versa)
bool incompatible_void_arg_use(const scm_list& pattern, const scm_list& args) {
  const bool pattern_is_argless = pattern.size() == 2 && 
                                  pattern[1].is_type(types::sym) && 
                                  pattern[1].value.sym==SENTINEL_ARG;
  const bool args_is_argless    = args.size()==1 && 
                                  data_is_the_SENTINEL_VAL(args[0]);
  return (pattern_is_argless && !args_is_argless) || (!pattern_is_argless && args_is_argless);
}


// Check whether enough args to match against the current pattern
bool incompatible_sizes(const scm_list& pattern, const scm_list& args) {
  const bool ends_in_ellipsis = !pattern.empty() && 
                                pattern.rbegin()->is_type(types::sym) && 
                                pattern.rbegin()->value.sym=="...";
  return pattern.empty() ||                                              // no pattern
          args.size() < (pattern.size()-1-(ends_in_ellipsis ? 1 : 0)) || // not enough args (for possible variadic)
          (!ends_in_ellipsis && args.size() != pattern.size()-1);        // not exact args (for non-variadic)
}


// Confirms pattern sub-expression does not contain an object with the same name as 'existing_pat_obj'
bool pattern_sub_expression_has_duplicate(const sym_type& existing_pat_obj,const scm_list& pattern) {
  for(const auto& pat : pattern) {
    if(!pat.is_type(types::sym) && !pat.is_type(types::exp)) continue; // naught to compare
    if(duplicate_pattern_object_name_instance(existing_pat_obj, pat)) return true;
  }
  return false;
}


// Confirms whether the obj1 & obj2 share a duplicate name instance
bool duplicate_pattern_object_name_instance(const sym_type& obj1_name, const data& obj2) {
  return (obj2.is_type(types::sym) && obj2.value.sym == obj1_name) ||
         (obj2.is_type(types::exp) && pattern_sub_expression_has_duplicate(obj1_name,obj2.value.exp));
}


// Confirm each pattern object only appears once in the pattern
void confirm_no_duplicate_pattern_objects(const sym_type& label,  const scm_list& pattern,
                                          const size_type& pat_No,const scm_list& exp) {
  for(size_type i = 0, n = pattern.size(); i < n; ++i) {
    if(!pattern[i].is_type(types::sym) && !pattern[i].is_type(types::exp)) // invalid pattern
      THROW_ERR("Invalid syntax \""<<label<<"\", pattern #"<<pat_No<<", identifier \"" 
        << pattern[i] << "\" was neither a symbol nor a sub-expression!\n     ((<pattern>) <template>)"
        << EXP_ERR(exp));
    if(!pattern[i].is_type(types::sym)) continue; // nothing to compare against
    for(size_type j = i+1; j < n; ++j)
      if(duplicate_pattern_object_name_instance(pattern[i].value.sym, pattern[j]))
        THROW_ERR("Invalid syntax \""<<label<<"\", pattern #"<<pat_No<<", identifier \"" 
          << pattern[i].value.sym << "\" appeared more than once!\n     ((<pattern>) <template>)"
          << EXP_ERR(exp));
  }
}


// Check whether '...' was used prior the 3rd pattern object
bool uses_Ellipsis_prematurely(const scm_list& pattern) {
  const size_type n = pattern.size();
  return (pattern[0].is_type(types::sym) && pattern[0].value.sym == "...") ||
         (n > 1 && pattern[1].is_type(types::sym) && pattern[1].value.sym == "...") || 
         (n > 2 && pattern[2].is_type(types::sym) && pattern[2].value.sym == "...");
}


// Confirm whether the pattern sub-expression matches the 'args' sub-expression
bool compare_pattern_args_exp_match(const scm_list& pat_exp,const scm_list& args_exp,
                                    const sym_type& label,  const frame_vars& keywords,
                                    const size_type& pat_No,const scm_list& exp){
  // Check whether enough args to match against the current pattern sub-expression
  if(pat_exp.empty() || args_exp.size() < pat_exp.size()-1) return false;
  // Confirm whether pattern & label-args combo match one another
  for(size_type i = 1, j = 0, n = pat_exp.size(); i < n; ++i, ++j) {
    // Check for proper "..." use in the pattern definition
    if(pat_exp[i].is_type(types::sym) && pat_exp[i].value.sym == "...")
      THROW_ERR("Invalid syntax \"" << label << "\", '...' wasn't the last identifier in pattern #" 
        << pat_No << "!\n     ((<pattern>) <template>)" << EXP_ERR(exp));
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
                               const size_type& pat_No,const scm_list& exp) {
  return pat_elt.is_type(types::exp) && 
      (!arg_elt.is_type(types::exp) || 
       !compare_pattern_args_exp_match(pat_elt.value.exp,arg_elt.value.exp,
                                       label,keywords,pat_No,exp));
}


// Wraps various helper fcns above, confirming whether or not a pattern was incompatible
bool incompatible_pattern(const sym_type& label,     const scm_list& args,
                          const frame_vars& keywords,const scm_list& pattern,
                          const size_type& pat_No,   const scm_list& exp) {
  // Check whether enough args to match against the current pattern
  if(incompatible_void_arg_use(pattern,args) || incompatible_sizes(pattern,args)) return true;
  // Confirm valid first symbol in pattern is a non-keyword symbol
  if(!pattern[0].is_type(types::sym) || is_keyword(pattern[0].value.sym,keywords))
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
  for(size_type i = 1, j = 0, n = pattern.size(); i < n; ++i, ++j) {
    // Check for proper "..." use in the pattern definition
    if(pattern[i].is_type(types::sym) && pattern[i].value.sym == "...") {
      if(i != n-1) THROW_ERR("Invalid syntax \"" << label 
        << "\", '...' wasn't the last identifier in pattern #" << pat_No << "!"
        << EXP_ERR(exp));
      return true;
    }
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
                                    scm_list& expanded_exp) {
  for(auto& e : expanded_exp) {
    if(e.is_type(types::sym) && e.value.sym == macro_arg_name)
      e = obj;
    else if(e.is_type(types::exp))
      splice_object_throughout_macro(obj, macro_arg_name, e.value.exp);
  }
}


// Splice in the 'remaining_body' for every instance of '...' in 'expanded_exp'
void splice_remaining_body_throughout_macro(const scm_list& remaining_body, 
                                            scm_list& expanded_exp) {
  for(size_type i = 0; i < expanded_exp.size(); ++i) {
    if(expanded_exp[i].is_type(types::sym) && expanded_exp[i].value.sym == "...") {
      // erase the "..." arg & splicing in the 'remaining body'
      expanded_exp.erase(expanded_exp.begin()+i); 
      expanded_exp.insert(expanded_exp.begin()+i, remaining_body.begin(), remaining_body.end());
    }
    else if(expanded_exp[i].is_type(types::exp))
      splice_remaining_body_throughout_macro(remaining_body, expanded_exp[i].value.exp);
  }
}


// Expands the args (as per the pattern & template) into expanded_exp
void expand_macro(const scm_list& args, const scm_list& pattern, 
                  const frame_vars& keywords, scm_list& expanded_exp) {
  for(size_type i = 1, j = 0, n = pattern.size(); i < n; ++i, ++j) {
    if(pattern[i].is_type(types::sym) && is_keyword(pattern[i].value.sym, keywords)) continue;
    // Expand args nested w/in expressions
    if(pattern[i].is_type(types::exp))
      expand_macro(args[j].value.exp, pattern[i].value.exp, keywords, expanded_exp);
    // Expand symbols
    else if(pattern[i].is_type(types::sym) && pattern[i].value.sym != "...")
      splice_object_throughout_macro(args[j], pattern[i].value.sym, expanded_exp);
    // Expand "..."
    else if(pattern[i].is_type(types::sym) && pattern[i].value.sym == "...")
      splice_remaining_body_throughout_macro(scm_list(args.begin()+j,args.end()), expanded_exp);
  }
}

/******************************************************************************
* MACRO SYNTACTIC EXTENSIONS -- MAIN FUNCTIONS
******************************************************************************/

// Confirm whether 'application_label' is a potential macro label
bool application_is_a_potential_macro(const scm_string& application_label) {
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
      expand_macro(args, mac.patterns[match_idx], mac.keywords, expanded_exp);
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


// Confirm whether at a syntactic extension
bool is_define_syntax(const scm_list& exp){return is_tagged_list(exp, "define-syntax");}
bool is_let_syntax(const scm_list& exp)   {return is_tagged_list(exp, "let-syntax");}
bool is_letrec_syntax(const scm_list& exp){return is_tagged_list(exp, "letrec-syntax");}
bool is_syntax_rules(const scm_list& exp) {return is_tagged_list(exp, "syntax-rules");}


// Confirm macro defn fulfills all needed parameters
void confirm_valid_syntactic_extension(const scm_list& exp) {
  constexpr const char * const syntax_rules_obj = "\n     <syntax-rules> = (syntax-rules (<keyword-list>) <pattern-template-patterns>)";
  if(exp.size() != 3)
    THROW_ERR("'define-syntax expects 2 arguments: (define-syntax <label> <syntax-rules>)"<<syntax_rules_obj<<EXP_ERR(exp));
  if(!exp[1].is_type(types::sym))
    THROW_ERR("'define-syntax expects a symbol as its 1st argument: (define-syntax <label> <syntax-rules>)" << EXP_ERR(exp));
  if(!exp[2].is_type(types::exp) || !is_tagged_list(exp[2].value.exp, "syntax-rules"))
    THROW_ERR("'define-syntax expects a 'syntax-rules object as its 2nd argument:"
      "\n     (define-syntax <label> <syntax-rules>)" << syntax_rules_obj << EXP_ERR(exp));

  const auto& syntax_rules = exp[2].value.exp;
  constexpr const char * const syntax_rules_format = "\n     (syntax-rules (<keyword-list>) <pattern-template-patterns>)";
  if(syntax_rules.size() < 3)
    THROW_ERR("'syntax-rules expects at least 2 arguments!"<<syntax_rules_format<<EXP_ERR(exp));
  if(!syntax_rules[1].is_type(types::exp))
    THROW_ERR("'syntax-rules expects a list of keyword symbols as its 1st argument!"<<syntax_rules_format<<EXP_ERR(exp));
  for(const auto& e : syntax_rules[1].value.exp)
    if(!e.is_type(types::sym))
      THROW_ERR("'syntax-rules keyword \""<<e<<"\" wasn't a symbol!"<<syntax_rules_format<<EXP_ERR(exp));

  for(size_type i = 2, n = syntax_rules.size(); i < n; ++i)
    if(!syntax_rules[i].is_type(types::exp) || syntax_rules[i].value.exp.size() < 2 || 
       !syntax_rules[i].value.exp[0].is_type(types::exp))
      THROW_ERR("'syntax-rules received an invalid ((<pattern>) <template>) definition!"<<syntax_rules_format
        << "\n     <pattern-template-pattern> = ((<pattern>) <template>)" << EXP_ERR(exp));
}


// Confirm valid let-macro syntax
void confirm_valid_let_macro(const scm_list& exp) {
  // Confirm enough args given to be a valid 'let-syntax
  constexpr const char * const format = "\n     (let-syntax (<syntactic-bindings-list>) <body>)"
                                        "\n     <syntactic-binding> = (<label> <syntax-rules>)";
  if(exp.size() < 2 || !exp[1].is_type(types::exp))
    THROW_ERR("'let-syntax & 'letrec-syntax both expect syntactic bindings list as their 1st argument!"<<format<<EXP_ERR(exp));
  if(exp.size() < 3)
    THROW_ERR("'let-syntax & 'letrec-syntax both expect a body as their 2nd argument!"<<format<<EXP_ERR(exp));
  // Confirm syntax bindings are lists of length 3
  const auto& syntax_bindings = exp[1].value.exp;
  for(size_type i = 0, n = syntax_bindings.size(); i < n; ++i) {
    if(!syntax_bindings[i].is_type(types::exp))
      THROW_ERR("'let-syntax & 'letrec-syntax -- invalid syntactic binding " << syntax_bindings[i] << " wasn't an expression!"<<format<<EXP_ERR(exp));
    if(syntax_bindings[i].value.exp.size() != 2)
      THROW_ERR("'let-syntax & 'letrec-syntax -- invalid syntactic binding #" << i+1 << ", expects 2 args!"<<format<<EXP_ERR(exp));
  }
}

/******************************************************************************
* REPRESENTING MACROS (1/2): (define-syntax <name> <syntax-rules>)
******************************************************************************/

// Returns an exec proc defining the syntactic extension
exe_type analyze_define_syntax(scm_list& exp) {
  confirm_valid_syntactic_extension(exp);
  const auto& syntax_rules = exp[2].value.exp;
  frame_mac mac(exp[1].value.sym);                            // assign macro name
  for(const auto& keyword : syntax_rules[1].value.exp)        // assign macro keywords
    mac.keywords.push_back(keyword.value.sym);
  for(size_type i = 2, n = syntax_rules.size(); i < n; ++i) { // assign macro patterns & templates
    mac.patterns.push_back(syntax_rules[i].value.exp[0].value.exp);
    if(syntax_rules[i].value.exp[0].value.exp.size() == 1)    // add the sentinel arg if pattern is argless
      mac.patterns.rbegin()->push_back(symconst::sentinel); 
    mac.templates.push_back(scm_list({symconst::begin}));     // wrap the macro body template in a sequence prior evaluation
    mac.templates.rbegin()->insert(mac.templates.rbegin()->end(),
                                   syntax_rules[i].value.exp.begin()+1, 
                                   syntax_rules[i].value.exp.end());
  }
  // return exec proc defining the macro in the environment
  return [mac = std::move(mac), 
          empty_val = scm_list({data(types::dne)})](env_type& env){
    define_syntax_extension(mac,env);
    return empty_val;
  };
}

/******************************************************************************
* REPRESENTING MACROS (2/2): (let-syntax ((<name> <syntax-rules>) ...) <body>)
******************************************************************************/

// (let-syntax ((<label-1> <syntax-rules-1>)
//              ...
//              (<label-N> <syntax-rules-N>))
//             <body>)

// (let () 
//   (define-syntax <label-1> <syntax-rules-1>)
//   ...
//   (define-syntax <label-N> <syntax-rules-N>)
//   <body>)

exe_type analyze_let_syntax(scm_list& exp) {
  // Convert let-syntax to an argless let defining syntax in its body
  confirm_valid_let_macro(exp);
  scm_list let_exp({symconst::let, scm_list{}});
  for(auto& binding : exp[1].value.exp)     // splice in syntax defns
    let_exp.push_back(data(scm_list({symconst::defn_syn, 
                                     binding.value.exp[0], 
                                     binding.value.exp[1]})));
  let_exp.insert(let_exp.end(), exp.begin()+2, exp.end()); // add let body
  return scm_analyze(std::move(let_exp));   // analyze let of syntax defns
}

auto analyze_letrec_syntax=analyze_let_syntax; // default letrec style support 

/******************************************************************************
* EXIT PROCEDURE
******************************************************************************/

bool is_exit_procedure(const scm_list& proc) {
  return is_tagged_list(proc, "exit"); // regardless of args, 'exit' exits
}

exe_type analyze_exit() {
  return [](env_type& env){throw SCM_EXCEPT::EXIT;return scm_list{};};
}

/******************************************************************************
* VOID EXPRESSION
******************************************************************************/

bool is_void_expression(const scm_list& exp) {
  return exp.size() == 1 && exp[0].is_type(types::dne);
}

// NOTE: for analyzing a VOID if/cond alternative if none given by the user
exe_type analyze_void() {
  return [](env_type& env){return scm_list({data(types::dne)});};
}

/******************************************************************************
* ANALYSIS, EVALUATION, & APPLICATION
******************************************************************************/

// -- PRIMITIVE PROCEDURES: identification & application
bool is_primitive_procedure(const scm_list& p){
  return is_tagged_list(p, PRIMITIVE_TAG);
}


scm_list apply_primitive_procedure(scm_list& proc,scm_list& args,env_type& env){
  // Provide the environment to primitives applying user-defined procedures
  if(primitive_requires_environment(proc[1].value.prm)) args.push_back(env);
  return scm_list_cast(proc[1].value.prm(args));
}


// -- EVAL 
scm_list scm_eval(scm_list&& exp, env_type& env) { // evaluate expression environment
  return scm_analyze(std::move(exp))(env);
}


// -- APPLY
// Analogue to "apply", except no need to analyze the body of compound 
//   procedures (already done). Hence only calls the execution procedure 
//   for the proc's body w/ the extended environment
scm_list execute_application(scm_list&& procedure, scm_list& arguments, env_type& env) {
  if(is_primitive_procedure(procedure)) // execute primitive procedure directly
    return apply_primitive_procedure(procedure,arguments,env);
  else if(is_compound_procedure(procedure)) {
    // create the procedure body's extended environment frame
    auto extended_env = extend_environment(procedure_parameters(procedure),
                                           arguments,
                                           procedure_environment(procedure),
                                           procedure_name(procedure));
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
    ++recursive_depth;
    auto result = procedure_body(procedure)(extended_env);
    --recursive_depth;
    return result;
  }
  if(procedure.empty())
    THROW_ERR("Invalid application of unknown procedure type NULL (received an empty expression)!");
  THROW_ERR("Invalid application of unknown procedure "<<procedure[0]<<"! "<<EXP_ERR(procedure));
}


// Analyzes the operator & operands, then returns an exec proc passing 
//   both the operator/operand proc exec's to 'execute-application'
//   (after having checked for macro use as well)
exe_type analyze_application(scm_list& exp) {
  auto op_proc  = scm_analyze(operator_of(exp));
  auto arg_exps = operands(exp);
  // Save name of invoking entity (iff a symbol) to check for a possible macro
  sym_type op_name = exp[0].is_type(types::sym) ? exp[0].value.sym : "";
  // If _NOT_ a possible macro, analyze the applicator's args ahead of time
  if(!application_is_a_potential_macro(op_name)) {
    std::vector<exe_type> arg_procs;
    for(const auto& arg : arg_exps)
      arg_procs.push_back(scm_analyze(scm_list_cast(arg)));
    return [op_proc=std::move(op_proc),arg_procs=std::move(arg_procs)](env_type& env){
      scm_list arg_vals;
      for(const auto& arg_proc : arg_procs)
        arg_vals.push_back(data_cast(arg_proc(env)));
      return execute_application(op_proc(env),arg_vals,env);
    };
  }
  // If possible macro, expand the application if so, else analyze args at eval
  return [op_proc=std::move(op_proc),arg_exps=std::move(arg_exps),
          op_name=std::move(op_name),exp=std::move(exp)](env_type& env){
    // check for a possible macro instance, & expand/eval it if so
    scm_list expanded;
    if(expand_macro_if_in_env(op_name, arg_exps, env, expanded, exp))
      return scm_eval(std::move(expanded),env);
    // eval each arg's exec proc to obtain the actual arg values
    scm_list arg_vals;
    for(const auto& arg : arg_exps)
      arg_vals.push_back(data_cast(scm_eval(scm_list_cast(arg),env)));
    return execute_application(op_proc(env),arg_vals,env);
  };
}


// -- ANALYZE (SYNTAX)
exe_type scm_analyze(scm_list&& exp) { // analyze expression
  if(exp.empty())                         THROW_ERR("Can't eval an empty expression!"<<EXP_ERR("()"));
  else if(is_exit_procedure(exp))  return analyze_exit();
  else if(is_void_expression(exp)) return analyze_void();
  else if(is_self_evaluating(exp)) return [exp=std::move(exp)](env_type& env){return exp;};
  else if(is_quoted(exp))          return analyze_quoted(exp);
  else if(is_assignment(exp))      return analyze_assignment(exp);
  else if(is_definition(exp))      return analyze_definition(exp);
  else if(is_if(exp))              return analyze_if(exp);
  else if(is_and(exp))             return analyze_and(exp);
  else if(is_or(exp))              return analyze_or(exp);
  else if(is_lambda(exp))          return analyze_lambda(exp);
  else if(is_begin(exp))           return analyze_sequence(begin_actions(exp));
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
  else if(is_vector_literal(exp))         THROW_ERR("Misplaced keyword 'vector-literal outside of a quotation! -- ANALYZE"   <<EXP_ERR(exp));
  else if(is_syntax_rules(exp))           THROW_ERR("Misplaced keyword 'syntax-rules outside of a macro! -- ANALYZE"         <<EXP_ERR(exp));
  else if(is_unquote(exp))                THROW_ERR("Misplaced keyword 'unquote outside of 'quasiquote ! -- ANALYZE"         <<EXP_ERR(exp));
  else if(is_unquote_splicing(exp))       THROW_ERR("Misplaced keyword 'unquote-splicing outside of 'quasiquote ! -- ANALYZE"<<EXP_ERR(exp));
  else if(is_application(exp))     return analyze_application(exp);
  else if(is_variable(exp)){
    return [exp=std::move(exp)](env_type& env){
      return scm_list_cast(lookup_variable_value(exp[0].value.sym,env));
    };
  }
  THROW_ERR("ANALYZE: Recieved unknown expression!"<<EXP_ERR(exp));
  return exe_type{};
}

/******************************************************************************
* GLOBAL ENVIRONMENT SETUP
******************************************************************************/

env_type setup_environment() {
  env_type initial_env(make_env());
  initial_env = extend_environment(
    primitive_procedure_names(),
    primitive_procedure_objects,
    initial_env
  );
  define_variable("#t", TRUE_DATA_BOOLEAN,  initial_env);
  define_variable("#f", FALSE_DATA_BOOLEAN, initial_env);
  define_variable("stream-null",       symconst::emptylist, initial_env);
  define_variable("null-environment",  symconst::null_env,  initial_env);
  define_variable("local-environment", symconst::locl_env,  initial_env);
  define_variable(SENTINEL_ARG,        symconst::sentinel,  initial_env);
  evaluate_primtives_written_in_heist_scheme(initial_env);
  return initial_env;
}

/******************************************************************************
* GLOBAL PORT REGISTRY CLEANUP
******************************************************************************/

void close_port_registry() {
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

void announce_input(FILE* outs){fputs("> ", outs);}
void indent_input(FILE* outs)  {fputs("  ", outs);}

// Account for whether REPL should print a newline
void print_repl_newline(const bool& printed_data) { // after printing data
  if(!LAST_PRINTED_NEWLINE_TO_STDOUT&&(printed_data||LAST_PRINTED_TO_STDOUT))
    putchar('\n');
  LAST_PRINTED_NEWLINE_TO_STDOUT = LAST_PRINTED_TO_STDOUT = false;
}

void print_repl_newline() { // after printing an error
  putchar('\n'), LAST_PRINTED_NEWLINE_TO_STDOUT=LAST_PRINTED_TO_STDOUT=false;
}

void account_for_whether_printed_data(const scm_list& val, bool& printed_data){
  printed_data = !val.empty() && !val[0].is_type(types::dne);
}


// Print output object
void user_print(FILE* outs, scm_list& object) {
  if(is_compound_procedure(object) || is_primitive_procedure(object))
    fprintf(outs, "#<procedure%s>", procedure_name(object).c_str());
  else if(is_delay(object))
    fputs("#<delay>", outs);
  else
    fputs(object[0].cio_str().c_str(), outs);
  fflush(outs);
}


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
        auto faulty_input = !input.empty() ? input+'\n'+tmp_buffer : tmp_buffer;
        alert_reader_error(outs,read_error,faulty_input);
        fputc('\n', outs);
        if(in_repl) announce_input(outs);
        abstract_syntax_tree.clear(), input.clear();
      }
    // Alert user if detected unparsable input (-:- ANOMALY -:-)
    } catch(const size_type& read_error_index) {
      auto faulty_input = !input.empty() ? input+'\n'+tmp_buffer : tmp_buffer;
      alert_reader_error(outs,read_error_index,faulty_input);
      fputc('\n', outs);
      if(in_repl) announce_input(outs);
      abstract_syntax_tree.clear(), input.clear();
    }
  }
  return abstract_syntax_tree;
}


void driver_loop(env_type& the_global_environment) {
  bool printed_data = true;
  GLOBAL_ENVIRONMENT_POINTER = the_global_environment;
  print_repl_newline(printed_data);
  for(;;) {
    announce_input(stdout);
    scm_list AST = read_user_input(stdout,stdin); // AST = Abstract Syntax Tree
    // Eval each expression given
    for(const auto& input : AST) {
      try {
        auto value = scm_eval(scm_list_cast(input),the_global_environment);
        account_for_whether_printed_data(value,printed_data);
        user_print(stdout, value);
        print_repl_newline(printed_data);
      } catch(const SCM_EXCEPT& eval_throw) {
        if(eval_throw == SCM_EXCEPT::EXIT) { puts("Adios!"); return; }
        print_repl_newline();
      } catch(...) {
        PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
             "\n     Triggered By: " << input << 
             "\n  => Please send your code to jrandleman@scu.edu to fix"
             "\n     the interpreter's bug!"
             "\n  => Terminating Scheme Interpretation.");
        return;
      }
    }
  }
}

/******************************************************************************
* COMMAND LINE ARGUMENT VALIDATION
******************************************************************************/

bool confirm_valid_command_line_args(int argc, char *argv[], int& script_name_pos){
  if(argc == 1) return true;

  const scm_string cmd_line_options = 
    "\n> To run a Script:     --script <script-filename>"
    "\n> To run the REPL:     don't pass any args!"
    "\n> Disable ANSI Colors: --nansi"
    "\n> Terminating the Scheme interpreter.\n\n" + scm_string(afmt(AFMT_0));

  // Validate argument layout
  if(argc > 4) {
    fprintf(stderr, 
      "%s\n> Invalid # of command-line args (given %d)!%s", 
      afmt(AFMT_1), argc, cmd_line_options.c_str());
    return false;
  }
  if((argc == 2 && scm_string(argv[1]) != "--nansi") || 
     (argc == 3 && scm_string(argv[1]) != "--script")) {
    fprintf(stderr, 
      "%s\n> Invalid command-line flag \"%s\"!%s", 
      afmt(AFMT_1), argv[1], cmd_line_options.c_str());
    return false;
  }
  if(argc == 4 && 
    (scm_string(argv[1]) != "--script" || scm_string(argv[3]) != "--nansi") && 
    (scm_string(argv[1]) != "--nansi"  || scm_string(argv[2]) != "--script")){
    fprintf(stderr, 
      "%s\n> Invalid command-line flag format: $ %s %s %s%s", 
      afmt(AFMT_1), argv[1], argv[2], argv[3], cmd_line_options.c_str());
    return false;
  }

  // Assign flag statuses
  if(argc == 3) {
    script_name_pos = 2;
  } else if(argc == 2) {
    USING_ANSI_ESCAPE_SEQUENCES = false;
  } else if(argc == 4) {
    USING_ANSI_ESCAPE_SEQUENCES = false;
    script_name_pos = (scm_string(argv[1]) == "--script") ? 2 : 3;
  }
  return true;
}

/******************************************************************************
* RUN SCRIPT HELPER FUNCTION
******************************************************************************/

int load_script(char *argv[], env_type& env, const int& script_name_pos){
  // Load the script & immediately exit
  scm_list load_args({data(make_str(argv[script_name_pos]))});
  GLOBAL_ENVIRONMENT_POINTER = env;
  try {
    primitive_LOAD(load_args);
    close_port_registry();
  } catch(const SCM_EXCEPT& eval_throw) {
    /* catch errors already output to stdout */ 
    putchar('\n');
  } catch(...) {
    /* catch uncaught C++ exceptions -:- ANOMALY -:- */
    PRINT_ERR(afmt(AFMT_1) << 
      "\nUncaught C++ Exception Detected! -:- BUG ALERT -:-"
      "\n  => While interpreting script \"" << argv[script_name_pos] << "\""
      "\n  => Please send your code to jrandleman@scu.edu to fix"
      "\n     the interpreter's bug!"
      "\n  => Terminating Scheme Interpretation.\n\n" << afmt(AFMT_0));
    return 1;
  }
  return 0;
}

/******************************************************************************
* MAIN EXECUTION
******************************************************************************/

int main(int argc, char *argv[]) {
  int script_name_pos = -1;

  // Validate arguments
  if(!confirm_valid_command_line_args(argc, argv, script_name_pos)) return 1;

  // Set up the environment
  auto global_env = setup_environment();

  // Run a Script
  if(script_name_pos != -1) 
    return load_script(argv, global_env, script_name_pos);

  // Run the REPL
  if(!global_env->empty()) 
    driver_loop(global_env);
  close_port_registry();

  return 0;
}
