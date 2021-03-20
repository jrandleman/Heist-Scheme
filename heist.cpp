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

#if __has_include("lib/HEIST_FILEPATH.hpp")
  #include "lib/HEIST_FILEPATH.hpp"
#else
  #error "installers/installer.cpp" MUST BE COMPILED (USING "-std=c++17") AND RUN PRIOR THE INTERPRETER!
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

#include "lib/type_system/types.hpp"
#include "lib/primitives/primitives.hpp"
#include "lib/core_evaluator/input_parser.hpp"

namespace heist {

  data scm_eval(data&& datum, env_type& env);
  exe_fcn_t scm_analyze(data&& datum,const bool tail_call,const bool cps_block);

  /******************************************************************************
  * AST-ANALYSIS HELPER FUNCTIONS
  ******************************************************************************/

  // Confirm whether list begins w/ designated symbol
  bool is_tagged_list(const data_vector& exp, const char* const tag)noexcept{
    return !exp.empty() && exp[0].is_type(types::sym) && exp[0].sym == tag;
  }

  bool symbol_is_property_chain_access(const string& sym)noexcept{
    return sym.find('.') != string::npos && sym != "." && sym != "..";
  }

  data lookup_variable_value(const string& var, env_type& env) {
    bool found = false;
    auto val = env->lookup_variable_value(var, found);
    if(found) return val;
    THROW_ERR("Variable " << var << " is not bound!");
  }

  /******************************************************************************
  * REPRESENTING READER ALIASES: (define-reader-alias <alias> <name>)
  ******************************************************************************/

  bool is_defn_reader_alias(const data_vector& exp)noexcept{
    return is_tagged_list(exp,symconst::defn_reader_alias);
  }

  exe_fcn_t analyze_defn_reader_alias(data_vector& exp) { 
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
  bool is_if(const data_vector& exp) noexcept{return is_tagged_list(exp,symconst::if_t);}

  void confirm_valid_if(const data_vector& exp) {
    if(exp.size() < 3) 
      THROW_ERR("'if didn't receive enough args:"
        "\n     (if <predicate> <consequent> <optional-alternative>)" << EXP_ERR(exp));
    if(exp.size() > 4) 
      THROW_ERR("'if received too many args:"
        "\n     (if <predicate> <consequent> <optional-alternative>)" << EXP_ERR(exp));
  }

  // Returns lambda so that if true, only eval consequent: 
  //   else, only eval alternative
  exe_fcn_t analyze_if(data_vector& exp,const bool tail_call=false,const bool cps_block=false) { 
    confirm_valid_if(exp);
    auto pproc = scm_analyze(std::move(exp[1]),false,cps_block);
    auto cproc = scm_analyze(std::move(exp[2]),tail_call,cps_block);
    exe_fcn_t aproc;
    if(exp.size() == 4) {
      aproc = scm_analyze(std::move(exp[3]),tail_call,cps_block);
    } else {
      aproc = [](env_type&){return GLOBALS::VOID_DATA_OBJECT;};
    }
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

  bool is_begin(const data_vector& exp) noexcept{return is_tagged_list(exp,symconst::begin);}
  data_vector begin_actions(data_vector& exp) noexcept{return data_vector(exp.begin()+1,exp.end());}

  // Analyzes each expression, then returns an exec proc which 
  //   sequentially invokes each expression's exec proc
  exe_fcn_t analyze_sequence(data_vector&& exps,const bool tail_call=false,const bool cps_block=false){ // used for 'begin' & lambda bodies
    // Nullary begin => <void>
    if(exps.empty()) return [](env_type&){return GLOBALS::VOID_DATA_OBJECT;};
    // If begin only has 1 expression, return exec proc of expression
    const size_type n = exps.size();
    if(n == 1) return scm_analyze(std::move(exps[0]),tail_call,cps_block);
    // Analyze each expression
    std::vector<exe_fcn_t> sequence_exe_procs(n);
    for(size_type i = 0; i < n; ++i)
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

  bool is_assignment(const data_vector& exp) noexcept{return is_tagged_list(exp,symconst::set);}

  // Confirm valid argument layout for variable assignment
  void confirm_valid_assignment(const data_vector& exp) {
    if(exp.size() != 3)
      THROW_ERR("'set! didn't receive 2 arguments: (set! <var> <val>)" << EXP_ERR(exp));
    if(!exp[1].is_type(types::sym) || exp[1].sym.empty())
      THROW_ERR("'set! 1st arg " << PROFILE(exp[1]) << " can't be set"
        " (only symbols)!\n     (set! <var> <val>)" << EXP_ERR(exp));
    if(*exp[1].sym.rbegin() == '.')
      THROW_ERR("'set! 1st arg is invalid object property-chain-access [ " << exp[1] 
          << " ] ends with a '.':\n     (set! <var> <val>)" << EXP_ERR(exp));
  }

  // Analyzes value being assigned, & returns an execution procedure 
  //   to install it as the variable in the designated env
  exe_fcn_t analyze_assignment(data_vector& exp,const bool cps_block=false) { 
    confirm_valid_assignment(exp);
    // Set variable
    if(!symbol_is_property_chain_access(exp[1].sym)) {
      auto var = exp[1].sym;
      auto value_proc = scm_analyze(data(exp[2]),false,cps_block); // cpy to avoid mving lest we want to show the expr in an error message
      return [var=std::move(var),value_proc=std::move(value_proc),exp=std::move(exp)](env_type& env){
        if(!env->set_variable_value(var, value_proc(env)))
          THROW_ERR("Variable "<<var<<" is not bound!"<<EXP_ERR(exp));
        return GLOBALS::VOID_DATA_OBJECT; // return is void
      };
    }
    // Set object property
    data_vector set_call(4);
    set_call[0] = "heist:core:oo:set-property!";
    set_call[1] = exp[1].sym.substr(0, exp[1].sym.rfind('.'));
    set_call[2] = data_vector(2);
    set_call[2].exp[0] = symconst::quote;
    set_call[2].exp[1] = exp[1].sym.substr(exp[1].sym.rfind('.')+1);
    set_call[3] = std::move(exp[2]);
    return scm_analyze(std::move(set_call),false,cps_block);
  }

  /******************************************************************************
  * REPRESENTING DEFINITION: (define <var> <val>)
  ******************************************************************************/

  bool is_definition(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::define);}
  data make_lambda(data_vector parameters, data_vector body)noexcept; // Lambda ctor

  // Confirm valid argument layout for variable & procedure definitions
  void confirm_valid_definition(const data_vector& exp) {
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

  string& definition_variable(data_vector& exp)noexcept{
    if(exp[1].is_type(types::sym)) return exp[1].sym; // if defining a variable
    return exp[1].exp[0].sym; // if defining a procedure
  }

  data definition_value(data_vector& exp)noexcept{
    // if defining a variable, else defining a procedure
    if(exp[1].is_type(types::sym)) return exp[2]; 
    data_vector args(exp[1].exp.begin()+1,exp[1].exp.end());
    data_vector body(exp.begin()+2,exp.end());
    return make_lambda(args,body);
  }

  bool is_obj_property_definition(const data_vector& exp)noexcept{
    return exp[1].is_type(types::sym) && symbol_is_property_chain_access(exp[1].sym);
  }

  // Generate an 'add-property! call from the <define> expression
  data convert_obj_property_defintion_to_method_call(const data_vector& exp)noexcept{
    data_vector def_call(4);
    def_call[0] = "heist:core:oo:add-property!";
    def_call[1] = exp[1].sym.substr(0, exp[1].sym.rfind('.'));
    def_call[2] = data_vector(2);
    def_call[2].exp[0] = symconst::quote;
    def_call[2].exp[1] = exp[1].sym.substr(exp[1].sym.rfind('.')+1);
    def_call[3] = exp[2];
    return def_call;
  }

  // Analyzes value being defined, & returns an execution procedure 
  //   to install it as the variable in the designated env
  exe_fcn_t analyze_definition(data_vector& exp,const bool cps_block=false) { 
    confirm_valid_definition(exp);
    // Define variable
    if(!is_obj_property_definition(exp)) {
      auto& var       = definition_variable(exp);
      auto value_proc = scm_analyze(definition_value(exp),false,cps_block);
      return [var=std::move(var),value_proc=std::move(value_proc)](env_type& env){
        env->define_variable(var,value_proc(env));
        return GLOBALS::VOID_DATA_OBJECT; // return is <void>
      };
    }
    // Define object property
    return scm_analyze(convert_obj_property_defintion_to_method_call(exp),false,cps_block);
  }

  /******************************************************************************
  * REPRESENTING PROMISES: (delay <expression>)
  ******************************************************************************/

  bool is_delay(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::delay);}

  // Extracts the delayed expression and returns an exec proc ctor'ing a promise
  exe_fcn_t analyze_delay(data_vector& exp,const bool cps_block=false) {
    if(exp.size() != 2) 
      THROW_ERR("'delay expects 1 argument: (delay <delay-expression>)" << EXP_ERR(exp));
    if(!cps_block)
      return [delayed_datum=std::move(exp[1])](env_type& env){
        return make_del(delayed_datum,env,false);
      };
    // Bind delayed CPS expressions to always have 'id as the topmost continuation,
    //   since FORCE is defined outside a scm->cps block, would have its arg bound 
    //   to such regardless if implemented directly in heist scheme as well.
    data delay_list = data_vector(2);
    delay_list.exp[0] = generate_fundamental_form_cps(exp[1]);
    delay_list.exp[1] = "id";
    return [delay_list=std::move(delay_list)](env_type& env){
      return make_del(delay_list,env,true);
    };
  }

  /******************************************************************************
  * REPRESENTING QUOTATION: (quote <expression>)
  ******************************************************************************/

  bool is_quoted(const data_vector& exp)noexcept{return is_tagged_list(exp, symconst::quote);}

  // Quoting a vector literal is a special case of quotation
  bool is_vector_literal(const data_vector& exp)noexcept{
    return is_tagged_list(exp,symconst::vec_literal);
  }

  // Quoting a hash-map literal is a special case of quotation
  bool is_hmap_literal(const data_vector& exp)noexcept{
    return is_tagged_list(exp,symconst::map_literal);
  }

  // Confirm whether quoting a vector/hmap literal
  bool quoting_a_container_literal(const data_vector& exp, bool is_container_literal(const data_vector& exp)noexcept)noexcept{
    return exp[1].is_type(types::exp) && !exp[1].exp.empty() && is_container_literal(exp[1].exp);
  }

  bool is_variadic_cps_procedure_signature(const size_type i, const size_type n, data_vector& exp)noexcept{
    return i+3 == n && data_is_continuation_parameter(exp[n-1]) && !data_is_dot_operator(exp[i+1]);
  }

  // Returns quoted data's contents
  data get_quoted_data(data_vector& exp)noexcept{
    if(!exp[1].is_type(types::sym)) return exp[1];
    if(exp[1].sym==symconst::false_t || exp[1].sym==symconst::true_t)
      return boolean(exp[1].sym==symconst::true_t);
    return convert_string_to_symbol(exp[1].sym);
  }


  // Returns whether the quoted exp is a 'cons' via the (.) operator.
  //   If not, exp is a list & this returns false.
  //   => Throws error if more than 1 (.) found (improper use)
  //   => If is a valid 'cons' quote, also rm's the (.)
  bool is_quoted_cons(data_vector& exp, const string& quote_name) {
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
  exe_fcn_t analyze_quoted_vh_literal(data_vector& exp, const char* name) {
    data_vector args(exp.begin()+1,exp.end());
    if(is_quoted_cons(args, symconst::quote))
      THROW_ERR('\''<<name<<" had an unexpected dot ("<<G.dot<<")!"<<EXP_ERR(exp));
    // return an empty vector if given no args
    if(args.empty()) {
      if constexpr (IS_VECTOR_LITERAL)
        return [](env_type&){return make_vec(data_vector());};
      else
        return [](env_type&){return make_map(map_object());};
    }
    // quote each item in the vector
    data_vector literal(args.size()+1);
    if constexpr (IS_VECTOR_LITERAL) 
      literal[0] = symconst::vector;
    else
      literal[0] = symconst::hmap;
    for(size_type i = 0, n = args.size(); i < n; ++i) {
      literal[i+1] = data_vector(2);
      literal[i+1].exp[0] = symconst::quote;
      literal[i+1].exp[1] = args[i];
    }
    // return analyzed vector
    return scm_analyze(std::move(literal));
  }


  exe_fcn_t analyze_quoted_vector_literal(data_vector& exp) {
    return analyze_quoted_vh_literal<true>(exp,"vector-literal");
  }


  exe_fcn_t analyze_quoted_hmap_literal(data_vector& exp) {
    return analyze_quoted_vh_literal<false>(exp,"hmap-literal");
  }
  

  // Analyzes the quote's text & returns an execution procedure for such
  exe_fcn_t analyze_quoted(data_vector& exp) {
    if(exp.size() != 2) 
      THROW_ERR("'quote form expects one argument: (quote <quoted-data>)!"<<EXP_ERR(exp));
    
    // Quote vector literals as needed
    if(quoting_a_container_literal(exp,is_vector_literal)) return analyze_quoted_vector_literal(exp[1].exp);

    // Quote hmap literals as needed
    if(quoting_a_container_literal(exp,is_hmap_literal)) return analyze_quoted_hmap_literal(exp[1].exp);
    
    // Get quoted data
    auto quoted_data = get_quoted_data(exp);
    
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
    data_vector quote_val(quoted_data.exp.size()+1);
    quote_val[0] = symconst::list;
    
    // Wrap 'quote' around each item in the list
    for(size_type i = 0, n = quoted_data.exp.size(); i < n; ++i){
      quote_val[i+1] = data_vector(2);
      quote_val[i+1].exp[0] = symconst::quote;
      quote_val[i+1].exp[1] = quoted_data.exp[i];
    }
    
    // Unpack the last item in the list and append it if quoting 
    //   a non-null-terminated list
    if(append_last_item) {
      auto last_item = *quote_val.rbegin();
      quote_val.pop_back();
      data_vector append_exp(3); 
      append_exp[0] = symconst::append;
      append_exp[1] = quote_val, append_exp[2] = last_item;
      quote_val = std::move(append_exp);
    }
    
    // Return the analyzed quoted list expression
    return scm_analyze(std::move(quote_val));
  }

  /******************************************************************************
  * REPRESENTING LAMBDA PROCEDURES (FASTER THAN FN)
  ******************************************************************************/

  // -- LAMBDAS: (lambda (<parameters>) <body>)
  bool        is_lambda(const data_vector& exp)  noexcept{return is_tagged_list(exp,symconst::lambda);}
  data_vector lambda_parameters(data_vector& exp)noexcept{return exp[1].exp;}
  data_vector lambda_body(data_vector& exp)      noexcept{return data_vector(exp.begin()+2,exp.end());}

  // Confirm valid argument layout for a lambda
  void confirm_valid_lambda(const data_vector& exp) {
    if(exp.size() < 3)
      THROW_ERR("'lambda special form didn't receive enough args: (lambda (<args>) <body>)" << EXP_ERR(exp));
    if(!exp[1].is_type(types::exp) && !data_is_the_empty_list(exp[1]))
      THROW_ERR("'lambda 1st arg [ " << exp[1] << " ] of type \"" 
        << exp[1].type_name() << "\" wasn't a proper parameter list!"
           "\n     (lambda (<args>) <body>)" << EXP_ERR(exp));
  }

  // Throw an error if 'vars' contains duplicate or non-symbol arg names, 
  //   or improper (.) use
  void confirm_valid_procedure_parameters(const data_vector& vars,const data_vector& exp){
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

  // Recursivly (for fn) replace instances of G.dot w/ symconst::dot
  void replace_param_temporary_dot_with_internal_dot(data_vector& params)noexcept{
    for(auto& d : params) {
      if(d.is_type(types::sym) && d.sym == G.dot) 
        d.sym = symconst::dot;
      else if(d.is_type(types::exp))
        replace_param_temporary_dot_with_internal_dot(d.exp);
    }
  }

  // Is a lambda using optional args (gets converted to a <fn>)
  // WARNING: DOES __NOT__ VALIDATE SUCH IS IN PROPER FORM
  bool is_opt_arg_lambda(const data_vector& exp)noexcept{
    if(exp.size() >= 3 && exp[0].is_type(types::sym) && exp[0].sym == symconst::lambda && exp[1].is_type(types::exp))
      for(const auto& arg : exp[1].exp)
        if(arg.is_type(types::exp))
          return true;
    return false;
  }

  // Paramaters end with a continuation
  bool params_end_with_a_continuation(const data_vector& params)noexcept{
    return !params.empty() && data_is_continuation_parameter(*params.rbegin());
  }

  // Validate lambda using optional args prior fn transformation
  void validate_lambda_opt_args(const data_vector& exp) {
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
  data_vector convert_lambda_opt_args_to_fn(const data_vector& exp) {
    validate_lambda_opt_args(exp);
    auto& params = exp[1].exp;
    const size_type n = params.size(), variadic_offset = 2;
    bool is_variadic = n > 1 && data_is_dot_operator(params[n-variadic_offset]);
    bool found_dflt = false;
    // Get vectors of the mandatory args, & form "define" exprs for the default args
    data_vector mandatory_args, default_value_defns;
    for(size_type i = 0, j = 0, m = n - (variadic_offset * is_variadic); i < m; ++i) {
      if(params[i].is_type(types::exp)) {
        found_dflt = true;
        default_value_defns.push_back(data_vector(3));
        default_value_defns[j].exp[0] = symconst::define;
        default_value_defns[j].exp[1] = params[i].exp[0];
        default_value_defns[j++].exp[2] = params[i].exp[1];
      } else if(!found_dflt) {
        mandatory_args.push_back(params[i]);
      }
    }
    if(is_variadic) {
      size_type i = default_value_defns.size();
      default_value_defns.push_back(data_vector(3));
      default_value_defns[i].exp[0] = symconst::define;
      default_value_defns[i].exp[1] = params[n-(variadic_offset-1)].sym;
      default_value_defns[i].exp[2] = data_vector(2);
      default_value_defns[i].exp[2].exp[0] = symconst::quote;
      default_value_defns[i].exp[2].exp[1] = data_vector();
    }
    // Generate <fn>
    const size_type fn_size = 2+default_value_defns.size()-is_variadic;
    data_vector fn_expr(fn_size);
    fn_expr[0] = symconst::fn;
    // Generate <fn> bodies
    for(size_type i = 1; i < fn_size; ++i) {
      bool last_instance = is_variadic && i+1 == fn_size;
      // Generate <fn> parameter list instance
      fn_expr[i] = data_vector(1,mandatory_args); // param_list
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
  data make_lambda(data_vector parameters,data_vector body)noexcept{
    data_vector new_lambda(body.size()+2); 
    new_lambda[0] = symconst::lambda, new_lambda[1] = std::move(parameters);
    std::move(body.begin(), body.end(), new_lambda.begin()+2);
    return new_lambda;
  }

  // Returns an exec proc to mk a lambda w/ the analyzed parameter list & body
  exe_fcn_t analyze_lambda(data_vector& exp,const bool cps_block=false) {
    // convert lambdas w/ optional args to fns
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
        auto proc = fcn_type(vars, body_proc, env, ""); // empty "" name by default (anon proc)
        proc.set_cps_procedure(true);
        return proc;
      };
    }
    return [vars=std::move(vars),body_proc=std::move(body_proc)](env_type& env){
      return fcn_type(vars, body_proc, env, ""); // empty "" name by default (anon proc)
    };
  }

  /******************************************************************************
  * REPRESENTING FN PROCEDURES (MORE DYNAMIC THAN LAMBDA)
  ******************************************************************************/

  bool is_fn(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::fn);}


  #define FN_LAYOUT "\n     (fn ((<arg> ...) <body> ...) ...)"
  void validate_fn_arg_quote_or_container_literal(const data_vector&,const data_vector&);
  void validate_fn_vect_arg_literal(const data_vector& exp, const data_vector& vect_arg) {
    for(size_type i = 1, n = vect_arg.size(); i < n; ++i)
      if(vect_arg[i].is_type(types::exp)) 
        validate_fn_arg_quote_or_container_literal(exp,vect_arg[i].exp);
  }

  void validate_fn_hmap_arg_literal(const data_vector& exp, const data_vector& hmap_arg) {
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

  void validate_fn_list_arg_literal(const data_vector& exp, const data_vector& list_arg) {
    for(size_type i = 0, n = list_arg.size(); i < n; ++i) {
      if(data_is_dot_operator(list_arg[i]) && i+2 != n) {
        THROW_ERR("'fn invalid variadic list literal in arg (\".\" must be 2nd to last arg): "
          << data(list_arg) << FN_LAYOUT << EXP_ERR(exp));
      } else if(list_arg[i].is_type(types::exp)) {
        validate_fn_arg_quote_or_container_literal(exp,list_arg[i].exp);
      }
    }
  }

  void validate_fn_arg_quote_or_container_literal(const data_vector& exp, const data_vector& container) {
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

  bool fn_invalid_variadic_arg(const size_type& i, const size_type& n, const data_vector& args)noexcept{
    return data_is_dot_operator(args[i]) &&
      !((i+2 == n && args[i+1].is_type(types::sym)) || 
        (i+3 == n && args[i+1].is_type(types::sym) && data_is_continuation_parameter(args[i+2])));
  }

  // Verify:
  // 0. variadics (must have a symbol token after ".") (only checked for in topmost call)
  // 1. hmaps (must have non-container keys & an even # of elts)
  // 2. validate list literals have "." as 2nd to last elt
  // 3. validate only quoting symbols
  void validate_fn_arg_signature(const data_vector& exp, const data_vector& args) {
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

  void validate_fn(const data_vector& exp) {
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


  exe_fcn_t analyze_fn(data_vector& exp,const bool cps_block=false) {
    validate_fn(exp);
    const size_type total_matches = exp.size()-1;
    std::vector<data_vector> param_insts(total_matches);
    std::vector<exe_fcn_t> bodies(total_matches);
    for(size_type i = 0; i < total_matches; ++i) {
      param_insts[i] = exp[i+1].exp[0].exp;
      bodies[i] = analyze_sequence(data_vector(exp[i+1].exp.begin()+1,exp[i+1].exp.end()),true,cps_block);
    }
    for(auto& params : param_insts)
      replace_param_temporary_dot_with_internal_dot(params);
    // set CPS value if needed
    if(!param_insts.empty() && params_end_with_a_continuation(param_insts[0])) {
      return [param_insts=std::move(param_insts),bodies=std::move(bodies)](env_type& env){
        auto proc = fcn_type(param_insts, bodies, env, ""); // empty "" name by default (anon proc)
        proc.set_cps_procedure(true);
        return proc;
      };
    }
    return [param_insts=std::move(param_insts),bodies=std::move(bodies)](env_type& env){
      return fcn_type(param_insts, bodies, env, ""); // empty "" name by default (anon proc)
    };
  }

  /******************************************************************************
  * DEFCLASS: (defclass <name> (<inheritance-list>) ...)
  ******************************************************************************/

  bool is_defclass(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::defclass);}


  #define DEFCLASS_LAYOUT\
    "\n     (defclass <class-name> (<optional-inherited-prototype>) <member-or-method-instances>)"\
    "\n     => <member-or-method> ::= (<member-name> <default-value>)"\
    "\n                             | (<method-name> <procedure-value>)"\
    "\n                             | ((<method-name> <arg1> <arg2> ...) <body> ...)"\
    "\n                             | ((<class-name> <arg> ...) <body> ...) ; constructor"\
    "\n                             | (<class-name> ((<arg> ...) <body> ...) ...) ; fn ctor"\
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
  void convert_opt_args_method_or_ctor_to_fn_expr(const string& ctor_name, data_vector& exp) {
    string name = exp[0].exp[0].sym;
    data_vector inline_method_as_lambda(exp.size()+1);
    inline_method_as_lambda[0] = symconst::lambda;
    inline_method_as_lambda[1] = data_vector(exp[0].exp.begin()+1,exp[0].exp.end());
    std::copy(exp.begin()+1,exp.end(),inline_method_as_lambda.begin()+2);
    if(ctor_name == name) { // ctor
      exp = convert_lambda_opt_args_to_fn(inline_method_as_lambda);
      exp[0] = ctor_name;
    } else { // method
      exp = data_vector(2);
      exp[0] = name, exp[1] = inline_method_as_lambda;
    }
  }

  // -- ERROR HANDLING
  void validate_defclass(data_vector& exp) {
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
    string ctor_name = exp[1].sym;
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

  void validate_inherited_entity(class_prototype& proto, data_vector& exp, env_type& env){
    if(exp[2].exp.empty()) return;
    auto result = lookup_variable_value(exp[2].exp[0].sym,env);
    if(!result.is_type(types::cls))
      THROW_ERR("'defclass inheritance entity " << PROFILE(exp[2].exp[0]) << " isn't a class prototype!" 
        << DEFCLASS_LAYOUT << EXP_ERR(exp));
    proto.super = result.cls;
  }

  void validate_unique_property_name(data_vector& exp,const string& name,const str_vector& seen_names,const char* message){
    for(const auto& n : seen_names)
      if(name == n)
        THROW_ERR("'defclass " << exp[1].sym << " => \"" << name << "\" " << message << '!'
          << DEFCLASS_LAYOUT << EXP_ERR(exp));
  }


  // -- CLASS PROTOTYPE GENERATION HELPERS
  exe_fcn_t convert_method_to_lambda(data_vector& method_exp,const bool cps_block) {
    data_vector method_lambda(1+method_exp.size());
    method_lambda[0] = symconst::lambda;
    method_lambda[1] = data_vector(method_exp[0].exp.begin()+1,method_exp[0].exp.end());
    std::copy(method_exp.begin()+1,method_exp.end(),method_lambda.begin()+2);
    if(cps_block) {
      // cps transform will wrap what we need in an extra lambda: (lambda (c) (c <procedure-we-desire>))
      auto cps_lambda = generate_fundamental_form_cps(method_lambda);
      return scm_analyze(std::move(cps_lambda[2].exp[1]),false,cps_block);
    } else {
      return scm_analyze(std::move(method_lambda));
    }
  }

  void evaluate_method_and_member_exec_procs(class_prototype& proto, str_vector& property_names, 
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
    data_vector setter_lambda(3);
    setter_lambda[0] = symconst::lambda;
    setter_lambda[1] = data_vector(2);
    setter_lambda[1].exp[0] = "heist:core:oo:property-name";
    setter_lambda[1].exp[1] = "heist:core:oo:new-value";
    setter_lambda[2] = data_vector(4);
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
    data_vector property_generator(3);
    property_generator[0] = symconst::lambda;
    property_generator[1] = data_vector(2);
    property_generator[1].exp[0] = "heist:core:property-name";
    property_generator[1].exp[1] = "heist:core:property-value";
    property_generator[2] = data_vector(4);
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
  void define_class_prototype_predicate(string& class_name, env_type& env) {
    data_vector predicate(3);
    predicate[0] = symconst::define;
    predicate[1] = data_vector(2);
    predicate[1].exp[0] = class_name + '?';
    predicate[1].exp[1] = "heist:core:oo:obj";
    predicate[2] = data_vector(4);
    predicate[2].exp[0] = symconst::if_t;
    predicate[2].exp[1] = data_vector(2);
    predicate[2].exp[1].exp[0] = "object?";
    predicate[2].exp[1].exp[1] = "heist:core:oo:obj";
    predicate[2].exp[2] = data_vector(3);
    predicate[2].exp[2].exp[0] = "eq?";
    predicate[2].exp[2].exp[1] = class_name;
    predicate[2].exp[2].exp[2] = "heist:core:oo:obj.prototype";
    predicate[2].exp[3] = "#f";
    scm_eval(std::move(predicate),env);
  }

  // (fn (() (heist:core:oo:make-object <class-name>))
  //     ((<member-value-container>) (heist:core:oo:make-object <class-name> <member-value-container>)))
  data_vector generate_default_prototype_constructor_fn(const string& class_name)noexcept{
    data_vector dflt_ctor(3);
    dflt_ctor[0] = symconst::fn;
    dflt_ctor[1] = data_vector(2); // clause 1: nullary
    dflt_ctor[1].exp[0] = data_vector();
    dflt_ctor[1].exp[1] = data_vector(2);
    dflt_ctor[1].exp[1].exp[0] = "heist:core:oo:make-object";
    dflt_ctor[1].exp[1].exp[1] = class_name;
    dflt_ctor[2] = data_vector(2); // clause 2: member-value-container
    dflt_ctor[2].exp[0] = data_vector(1,"heist:core:oo:member-value-container");
    dflt_ctor[2].exp[1] = data_vector(3);
    dflt_ctor[2].exp[1].exp[0] = "heist:core:oo:make-object";
    dflt_ctor[2].exp[1].exp[1] = class_name;
    dflt_ctor[2].exp[1].exp[2] = "heist:core:oo:member-value-container";
    return dflt_ctor;
  }

  void bind_default_prototype_constructor(class_prototype& proto, env_type& env) {
    proto.bind_user_ctor(scm_eval(generate_default_prototype_constructor_fn(proto.class_name),env).fcn);
  }

  // (define new-<class-name>
  //   (fn (() (heist:core:oo:make-object <class-name>))
  //       ((<member-value-container>) (heist:core:oo:make-object <class-name> <member-value-container>)))
  void define_default_prototype_constructor(const string& class_name, env_type& env) {
    data_vector dflt_ctor_defn(3);
    dflt_ctor_defn[0] = symconst::define;
    dflt_ctor_defn[1] = "new-"+class_name;
    dflt_ctor_defn[2] = generate_default_prototype_constructor_fn(class_name);
    scm_eval(std::move(dflt_ctor_defn),env);
  }

  // Convert user's custom ctor to be in CPS form as needed
  data generate_CPS_custom_prototype_constructor(data_vector& custom_ctor) {
    // convert the body to CPS notation, then unwrap it from the cps-transform generated lambda 
    // & pop the lambda's continuation to be passed as one of the custom ctor's params instead
    data_vector custom_body(custom_ctor.size()-1);
    custom_body[0] = symconst::begin;
    std::move(custom_ctor.begin()+2,custom_ctor.end(),custom_body.begin()+1);
    auto custom_body_cps = generate_fundamental_form_cps(custom_body); // (lambda (c) <sought-body>)
    auto continuation_param = custom_body_cps[1].exp[0];
    data_vector custom_ctor_cps(3);
    custom_ctor_cps[0] = std::move(custom_ctor[0]);
    custom_ctor_cps[1] = std::move(custom_ctor[1]);
    custom_ctor_cps[1].exp.push_back(std::move(continuation_param));
    custom_ctor_cps[2] = std::move(custom_body_cps[2]);
    return custom_ctor_cps;
  }

  // (lambda (<... CUSTOM CTOR ARGS HERE ...>)
  //   (define self (heist:core:oo:make-object <class-name>))
  //   <... CUSTOM CTOR BODY HERE ...>
  //   self)
  void bind_custom_prototype_constructor(class_prototype& proto, env_type& env, data_vector& ctor_proc, const bool cps_block) {
    data_vector custom_ctor(3+ctor_proc.size());
    custom_ctor[0] = symconst::lambda;
    custom_ctor[1] = data_vector(ctor_proc[0].exp.begin()+1,ctor_proc[0].exp.end());
    custom_ctor[2] = data_vector(3);
    custom_ctor[2].exp[0] = symconst::define;
    custom_ctor[2].exp[1] = "self";
    custom_ctor[2].exp[2] = data_vector(2);
    custom_ctor[2].exp[2].exp[0] = "heist:core:oo:make-object";
    custom_ctor[2].exp[2].exp[1] = proto.class_name;
    std::move(ctor_proc.begin()+1,ctor_proc.end(),custom_ctor.begin()+3);
    *custom_ctor.rbegin() = "self";
    if(cps_block) {
      proto.bind_user_ctor(scm_analyze(generate_CPS_custom_prototype_constructor(custom_ctor),false,cps_block)(env).fcn);
    } else {
      proto.bind_user_ctor(scm_eval(std::move(custom_ctor),env).fcn);
    }
  }

  // (fn ((<... CUSTOM CTOR ARGS HERE ...>) 
  //        (define self (heist:core:oo:make-object <class-name>))
  //        <... CUSTOM CTOR BODY HERE ...>
  //        self) ...)
  void bind_custom_prototype_fn_constructor(class_prototype& proto, env_type& env, data_vector& ctor_proc,const bool cps_block) {
    data_vector custom_ctor(ctor_proc.size());
    custom_ctor[0] = symconst::fn;
    for(size_type i = 1, n = ctor_proc.size(); i < n; ++i) {
      const size_type clause_length = ctor_proc[i].exp.size()+2;
      custom_ctor[i] = data_vector(clause_length);
      custom_ctor[i].exp[0] = ctor_proc[i].exp[0];
      custom_ctor[i].exp[1] = data_vector(3);
      custom_ctor[i].exp[1].exp[0] = symconst::define;
      custom_ctor[i].exp[1].exp[1] = "self";
      custom_ctor[i].exp[1].exp[2] = data_vector(2);
      custom_ctor[i].exp[1].exp[2].exp[0] = "heist:core:oo:make-object";
      custom_ctor[i].exp[1].exp[2].exp[1] = proto.class_name;
      std::copy(ctor_proc[i].exp.begin()+1,ctor_proc[i].exp.end(),custom_ctor[i].exp.begin()+2);
      custom_ctor[i].exp[clause_length-1] = "self";
    }
    if(cps_block) {
      // convert fn defn to CPS notation as needed
      auto cps_fn_expr = generate_fundamental_form_cps(custom_ctor); // (lambda (c) (c <fn-we-want>))
      proto.bind_user_ctor(scm_analyze(std::move(cps_fn_expr[2].exp[1]),false,cps_block)(env).fcn);
    } else {
      proto.bind_user_ctor(scm_eval(std::move(custom_ctor),env).fcn);
    }
  }

  void parse_defclass_expression(data_vector& exp, str_vector& property_names, 
                                 std::vector<exe_fcn_t>& property_exec_procs, 
                                 data_vector& ctor_proc,const bool cps_block) {
    const string ctor_name(exp[1].sym);
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
  exe_fcn_t analyze_defclass(data_vector& exp,const bool cps_block=false) {
    validate_defclass(exp);
    class_prototype proto;
    proto.class_name = exp[1].sym;
    // get exec procs for member values & method procedures
    str_vector property_names;
    std::vector<exe_fcn_t> property_exec_procs;
    data_vector ctor_proc;
    parse_defclass_expression(exp,property_names,property_exec_procs,ctor_proc,cps_block);
    return [proto=std::move(proto),property_names=std::move(property_names),
            property_exec_procs=std::move(property_exec_procs),exp=std::move(exp),
            ctor_proc=std::move(ctor_proc),cps_block](env_type& env)mutable{
      proto.defn_env = env;
      // confirm inheriting from class objects & add inherited prototype (if present)
      validate_inherited_entity(proto,exp,env);
      // evaluate member and method values
      evaluate_method_and_member_exec_procs(proto,property_names,property_exec_procs,env);
      // define property setter method
      define_property_setter(proto,env);
      // define dynamic property generator method
      define_dynamic_property_generator(proto,env);
      // generate the underlying default object ctor
      define_default_prototype_constructor(proto.class_name,env); // default ctor always available
      // bind the user-defined ctor (which the prototype is polymorphic for in application)
      if(ctor_proc.empty()) bind_default_prototype_constructor(proto,env);
      else if(ctor_proc[0].is_type(types::exp)) bind_custom_prototype_constructor(proto,env,ctor_proc,cps_block);
      else bind_custom_prototype_fn_constructor(proto,env,ctor_proc,cps_block);
      // define the class predicate
      define_class_prototype_predicate(proto.class_name,env);
      // define the class prototype
      env->define_variable(proto.class_name,make_cls(proto));
      return GLOBALS::VOID_DATA_OBJECT;
    };
  }

  #undef DEFCLASS_LAYOUT

  /******************************************************************************
  * REPRESENTING TRUE ITERATION: WHILE (DEGRADES TO "DO" IN CPS BLOCKS)
  ******************************************************************************/

  bool is_while(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::while_t);}

  exe_fcn_t analyze_while(data_vector& exp,const bool cps_block=false) {
    // "while"s in cps contexts degrade to "do"s
    if(cps_block) {
      exp[0] = symconst::cps_app_tag;
      exp.insert(exp.begin()+1,"do");
      exp.insert(exp.begin()+2,data(data_vector()));
      if(exp.size() > 3 && exp[3].is_type(types::exp) && !exp[3].exp.empty()) {
        data_vector negated_cond(2);
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
      data_vector return_exps(exp[1].exp.size());
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
    data_vector body_exps(exp.size()-1);
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
  * REPRESENTING infix! infixr! unfix! SPECIAL FORMS: READER MANIPULATION
  ******************************************************************************/

  bool is_infix(const data_vector& exp)noexcept {return is_tagged_list(exp,symconst::infix);}
  bool is_infixr(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::infixr);}
  bool is_unfix(const data_vector& exp)noexcept {return is_tagged_list(exp,symconst::unfix);}

  void confirm_valid_infix_infixr_unfix_syntax(data_vector& exp, const char* name) {
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

  void remove_preexisting_operators_from_table(const data_vector& exp, const int symbol_offset)noexcept{
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
  exe_fcn_t register_infix_operators(const data_vector& exp,const char* name,bool is_left_assoc) {
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
  exe_fcn_t seek_infix_operators(data_vector& exp,bool is_left_assoc)noexcept{
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

  exe_fcn_t analyze_infix(data_vector& exp){
    confirm_valid_infix_infixr_unfix_syntax(exp,symconst::infix);
    if(exp[1].is_type(types::num))
      return register_infix_operators(exp,symconst::infix,true);
    return seek_infix_operators(exp,true);
  }

  exe_fcn_t analyze_infixr(data_vector& exp){
    confirm_valid_infix_infixr_unfix_syntax(exp,symconst::infixr);
    if(exp[1].is_type(types::num))
      return register_infix_operators(exp,symconst::infixr,false);
    return seek_infix_operators(exp,false);
  }

  exe_fcn_t analyze_unfix(data_vector& exp){
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

  bool is_definedp(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::definedp);}


  // (string-split <call> ".")
  bool parse_object_property_chain_sequence(const string& call, str_vector& chain)noexcept{
    chain.push_back("");
    for(const auto& ch : call) {
      if(ch == '.') chain.push_back("");
      else          *chain.rbegin() += ch;
    }
    // verify no ".." found or ".<call>" or "<call>."
    for(const auto& link : chain) if(link.empty()) return false;
    return true;
  }


  bool property_chain_is_defined(str_vector&& chain, env_type& env)noexcept{
    // get the first object instance
    if(!env->has_variable(chain[0])) return false;
    data value = lookup_variable_value(chain[0],env);
    // get the call value
    for(size_type i = 1, n = chain.size(); i < n; ++i) {
      if(!value.is_type(types::obj)) return false;
      bool found = false;
      value = value.obj->get_property(chain[i], found);
      if(!found) return false;
    }
    return true;
  }


  // NOTE: USE runtime-syntax? core-syntax? reader-syntax? TO CHECK MACROS !!!
  exe_fcn_t analyze_definedp(data_vector& exp) {
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
    str_vector chain; // split the call chain into object series
    if(!parse_object_property_chain_sequence(exp[1].sym,chain)) 
      return [](env_type&){return GLOBALS::FALSE_DATA_BOOLEAN;};
    return [chain=std::move(chain)](env_type& env)mutable{
      return boolean(property_chain_is_defined(std::move(chain),env));
    };
  }

  /******************************************************************************
  * REPRESENTING delete! SPECIAL FORM: DELETE VARS & OBJECT-PROPERTY-ACCESS
  ******************************************************************************/

  bool is_delete(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::delete_bang);}


  void delete_property_chain_if_exists(str_vector&& chain, env_type& env)noexcept{
    // get the first object instance
    if(!env->has_variable(chain[0])) return;
    data value = lookup_variable_value(chain[0],env);
    // get the call value
    for(size_type i = 1, n = chain.size(); i < n; ++i) {
      if(!value.is_type(types::obj)) return;
      // Delete value if at last point in property chain
      if(i+1 == n) {
        value.obj->delete_property(chain[i]);
      // Search local members & methods, the proto, and the proto inheritances
      } else {
        bool found = false;
        value = value.obj->get_property(chain[i], found);
        if(!found) return;  
      }      
    }
  }


  exe_fcn_t analyze_delete(data_vector& exp) {
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
    str_vector chain; // split the call chain into object series
    if(!parse_object_property_chain_sequence(exp[1].sym,chain)) 
      return [](env_type&){return GLOBALS::VOID_DATA_OBJECT;};
    return [chain=std::move(chain)](env_type& env)mutable{
      delete_property_chain_if_exists(std::move(chain),env);
      return GLOBALS::VOID_DATA_OBJECT;
    };
  }

  /******************************************************************************
  * CONTINUATION-PASSING-STYLE EXPANSION
  ******************************************************************************/

  // Get:
  //   0. data_vector generate_fundamental_form_cps(const data&) // convert a datum to be a CPS expression
  //   1. bool        data_is_cps_atomic(const data&)            // if datum evaluates to itself in a CPS context
  //   2. bool        is_cps_application(const data_vector&)     // if expression is an application tagged by <generate_fundamental_form_cps>
  //   3. string      generate_unique_cps_hash()                 // generate a unique/hashed continuation name
  //   4. string      generate_unique_cps_value_hash()           // generate a unique/hashed non-continuation value name
  #include "lib/core_evaluator/cps/expander.hpp"

  /******************************************************************************
  * REPRESENTING CPS-TRANSFORMATION (EVALUATION & QUOTATION)
  ******************************************************************************/

  bool is_scm_cps(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::scm_cps);}
  bool is_cps_quote(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::cps_quote);}


  // Extra <ignore> arg used to account for the <id> procedure that will be 
  // passed automatically by <fcn_type::get_extended_environment>
  void account_for_automatically_passed_ID_continuation(data_vector& exp)noexcept{
    // Add an extra continuation param to account for the auto-added <id> procedure
    if(exp.size() >= 2 && exp[0].is_type(types::sym) && exp[0].sym == symconst::lambda && exp[1].is_type(types::exp) && exp[1].exp.size() == 1) {
      exp[1].exp.push_back(generate_unique_cps_hash()); // account for <id>
    // Wrap application in a lambda accepting an extra continuation param to 
    // account for the auto-added <id> procedure
    } else if(!exp.empty() && exp[0].is_type(types::sym) && exp[0].sym == symconst::cps_app_tag) {
      data_vector lambda(3);
      lambda[0] = symconst::lambda;
      lambda[1] = data_vector(2);
      lambda[1].exp[0] = generate_unique_cps_hash(); // k
      lambda[1].exp[1] = generate_unique_cps_hash(); // account for <id>
      lambda[2] = data_vector(2);
      lambda[2].exp[0] = std::move(exp);
      lambda[2].exp[1] = lambda[1].exp[0];
      exp = std::move(lambda);
    }
  }


  // Process convert & eval exp in CPS (Continuation Passing Style)
  exe_fcn_t analyze_scm_cps(data_vector& exp) {
    if(exp.size() == 1)
      THROW_ERR("'scm->cps expects at least 1 expression: (scm->cps <exp-1> ... <exp-N>)"<<EXP_ERR(exp));
    data_vector cps_exp;
    if(exp.size() == 2) {
      cps_exp = generate_fundamental_form_cps(exp[1]);
    } else { // Wrap multi-statement transforms in a BEGIN
      data_vector begin(exp.size());
      begin[0] = symconst::begin;
      std::copy(exp.begin()+1,exp.end(),begin.begin()+1);
      cps_exp = generate_fundamental_form_cps(begin);
    }
    account_for_automatically_passed_ID_continuation(cps_exp);
    return scm_analyze(std::move(cps_exp),false,true);
  }


  // Returns the generated CPS form of exp as a quoted list of data
  exe_fcn_t analyze_cps_quote(data_vector& exp,const bool cps_block) {
    if(exp.size() == 1)
      THROW_ERR("'cps-quote expects at least 1 expression: (cps-quote <exp-1> ... <exp-N>)"<<EXP_ERR(exp));
    data_vector quoted_cps(2);
    quoted_cps[0] = symconst::quote;
    if(exp.size() == 2) {
      quoted_cps[1] = generate_fundamental_form_cps(exp[1]);
    } else { // Wrap multi-statement transforms in a BEGIN
      data_vector begin(exp.size());
      begin[0] = symconst::begin;
      std::copy(exp.begin()+1,exp.end(),begin.begin()+1);
      quoted_cps[1] = generate_fundamental_form_cps(begin);
    }
    if(cps_block) return scm_analyze(generate_fundamental_form_cps(quoted_cps),false,true);
    return scm_analyze(std::move(quoted_cps));
  }

  /******************************************************************************
  * REPRESENTING THE EXECUTION-CONTEXT CPS-PREDICATE
  ******************************************************************************/

  bool is_using_cpsp(const data_vector& exp)noexcept{
    return is_tagged_list(exp,symconst::using_cpsp);
  }

  // Return whether in a <scm->cps> block or the <-cps> flag is active
  exe_fcn_t analyze_using_cpsp(data_vector& exp,const bool cps_block) {
    if(exp.size() != 1)
      THROW_ERR("'using-cps? expects 0 args: (using-cps?)"<<EXP_ERR(exp));
    return [cps_block](env_type&){
      return boolean(cps_block||G.USING_CPS_CMD_LINE_FLAG);
    };
  }

  /******************************************************************************
  * EXECUTING MACRO EXPANSIONS
  ******************************************************************************/

  // Get:
  //   0. bool expand_macro_if_in_env(const string& label,data_vector args,env_type& env,data_vector& expanded)  // expand into <expanded> & return success
  //   1. bool is_macro_argument_label(const data& d, const str_vector& keywords)                                // <d> is a syntax-rules identifier
  //   2. bool data_is_ellipsis(const data& d)                                                                   // <d> is the "..." symbol
  #include "lib/core_evaluator/macro_expander.hpp"

  /******************************************************************************
  * MACRO SYNTAX-RULES ANALYSIS VALIDATION HELPER FUNCTIONS:
  ******************************************************************************/

  // Confirm data is an unexpandable syntax-rules macro token
  bool data_is_literal_or_keyword(const data& pat_entity, const str_vector& keywords)noexcept{
    return !pat_entity.is_type(types::exp) && !is_macro_argument_label(pat_entity,keywords);
  }


  // PRECONDITION: is_macro_argument_label(pattern[i],keywords) = true
  void confirm_unique_syntax_rules_pattern_identifier(const data_vector& pattern,const size_type& i,
                                                      const data_vector& exp, str_vector& identifiers) {
    static constexpr const char * const format = 
      "\n     (syntax-rules (<keyword-list>) <pattern-template-clauses>)"
      "\n     <pattern-template-clause> = ((<pattern>) <template>)";
    // Confirm each pattern identifier only appears once
    if(std::find(identifiers.begin(),identifiers.end(),pattern[i].sym) != identifiers.end())
      THROW_ERR("'syntax-rules " << pattern[i].sym << " identifier found twice!"
        "\n     Each syntax-rules pattern identifier MUST be unique!\n     " 
        << stringify_expr<&data::noexcept_write>(pattern) << format << EXP_ERR(exp));
    identifiers.push_back(pattern[i].sym);
  }


  void confirm_proper_syntax_rules_pattern_layout(const data_vector& pattern,const data_vector& exp,
                                                  const str_vector& keywords, str_vector& identifiers){
    static constexpr const char * const format = 
      "\n     (syntax-rules (<keyword-list>) <pattern-template-clauses>)"
      "\n     <pattern-template-clause> = ((<pattern>) <template>)";
    // Confirm pattern subexpression doesn't begin w/ '...'
    if(pattern.empty()) return; // guarenteed to be part of a recursive call (topmost pattern asserted non-empty)
    if(!pattern.empty() && data_is_ellipsis(pattern[0]))
      THROW_ERR("'syntax-rules \"...\" may NEVER begin a pattern subexpression!"
        "\n     " << stringify_expr<&data::noexcept_write>(pattern) << format <<EXP_ERR(exp));
    if(is_macro_argument_label(pattern[0],keywords))
      confirm_unique_syntax_rules_pattern_identifier(pattern,0,exp,identifiers);
    bool seen_ellipses = false;
    for(size_type i = 1, n = pattern.size(); i < n; ++i) {
      if(data_is_ellipsis(pattern[i])) {
        // Confirm each subexpression has at most 1 '...'
        if(seen_ellipses){
          THROW_ERR("'syntax-rules \"...\" may only appear ONCE per pattern subexpression!"
            "\n     " << stringify_expr<&data::noexcept_write>(pattern) <<
            "\n     -> IE: (a ... (b ...)) => VALID: 1 '...' PER EXPRESSION!"
            "\n            (a ... b ...)   => INVALID: 2 '...' IN A SINGLE EXPRESSION!"
            << format <<EXP_ERR(exp));
        } 
        // Confirm '...' doesn't follow a literal or keyword in the pattern
        if(data_is_literal_or_keyword(pattern[i-1],keywords)){
          THROW_ERR("'syntax-rules \"...\" may only be preceded by a non-literal-non-keyword"
            "\n     symbol or expression!\n     " << stringify_expr<&data::noexcept_write>(pattern) 
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
  void confirm_proper_syntax_rules_pattern_template_clauses(const data_vector& exp,const syn_type& mac,const char* format){
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
          "\n     " << stringify_expr<&data::noexcept_write>(exp[i].exp[0].exp) << format << EXP_ERR(exp));
      // Confirm pattern's topmost subexpression depth doesn't start w/ '...'
      if(exp[i].exp[0].exp.size() > 1 && data_is_ellipsis(exp[i].exp[0].exp[1]))
        THROW_ERR("'syntax-rules pattern '...' identifier must be preceded"
          " by a symbol or expression identifier!\n     " 
          << stringify_expr<&data::noexcept_write>(exp[i].exp[0].exp) << format << EXP_ERR(exp));
      // Confirm each pattern identifier only appears once
      str_vector identifiers;
      confirm_proper_syntax_rules_pattern_layout(exp[i].exp[0].exp,exp,mac.keywords,identifiers);
    }
  }


  // Confirm syntax-rules keywords list is valid, and extract it if so
  void extract_syntax_rules_keywords(const data_vector& exp,syn_type& mac,const char* format) {
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


  void confirm_valid_syntax_rules_and_extract_keywords(const data_vector& exp, syn_type& mac) {
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

  bool is_syntax_rules(const data_vector& exp)noexcept{
    return is_tagged_list(exp,symconst::syn_rules);
  }


  void recursively_safe_expansion_hash_macro_template(const string& value, const string& hash_value, 
                                                                           data_vector& mac_template)noexcept{
    for(size_type i = 0, n = mac_template.size(); i < n; ++i) {
      if(mac_template[i].is_type(types::exp)) {
        recursively_safe_expansion_hash_macro_template(value, hash_value, mac_template[i].exp);
      } else if(mac_template[i].is_type(types::sym) && mac_template[i].sym == value) {
        mac_template[i].sym = hash_value;
      }
    }
  }


  void recursively_safe_expansion_hash_macro_pattern(data_vector& pattern, data_vector& mac_template, 
                                                     const str_vector& keywords, const size_type& start=0)noexcept{
    for(size_type i = start, n = pattern.size(); i < n; ++i) {
      if(pattern[i].is_type(types::exp)) {
        recursively_safe_expansion_hash_macro_pattern(pattern[i].exp, mac_template, keywords);
      } else if(is_macro_argument_label(pattern[i], keywords)) {
        string original_label = pattern[i].sym;
        pattern[i].sym = safe_expansion_hashed_macro_arg(pattern[i].sym);
        recursively_safe_expansion_hash_macro_template(original_label, pattern[i].sym, mac_template);
      }
    }
  }


  bool datum_is_syntax_hashed_symbol(const data& datum, const data_vector& exp){
    if(datum.is_type(types::exp) && is_tagged_list(datum.exp,symconst::syn_hash)) {
      if(datum.exp.size() != 2 || !datum.exp[1].is_type(types::sym))
        THROW_ERR("'syntax-hash didn't receive 1 symbol arg: (syntax-hash <symbol>)" << EXP_ERR(exp));
      return true;
    }
    return false;
  }


  void parse_macro_template_for_syntax_hashed_identifiers(str_vector& hashed_id_registry, data_vector& mac_template, const data_vector& exp){
    for(size_type i = 0, n = mac_template.size(); i < n; ++i) {
      if(datum_is_syntax_hashed_symbol(mac_template[i],exp)) {
        hashed_id_registry.push_back(mac_template[i].exp[1].sym);
        mac_template[i] = *hashed_id_registry.rbegin(); // replace syntax-hash expression with the var once recorded
      } else if(mac_template[i].is_type(types::exp)) {
        parse_macro_template_for_syntax_hashed_identifiers(hashed_id_registry,mac_template[i].exp,exp);
      }
    }
  }


  exe_fcn_t analyze_syntax_rules(data_vector& exp) {
    syn_type mac("");
    confirm_valid_syntax_rules_and_extract_keywords(exp, mac);
    // Extract pattern-template clauses
    for(size_type i = 2, n = exp.size(); i < n; ++i) {
      mac.patterns.push_back(exp[i].exp[0].exp);
      // Wrap 'begin' around templates prior evaluation (for multi-exp bodies)
      mac.templates.push_back(data_vector(exp[i].exp.size()));
      mac.hashed_template_ids.push_back(str_vector());
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

  bool is_define_syntax(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::defn_syn);}


  void register_symbol_iff_new(str_vector& registry, const string& label)noexcept{
    for(const auto& macro_label : registry)
      if(macro_label == label)
        return;
    registry.push_back(label);
  }


  void confirm_is_not_core_syntax_label(data_vector& exp) {
    if(std::find(G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.begin(),
                 G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.end(),exp[1].sym) != 
       G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.end()) {
      THROW_ERR("'define-syntax label \""<<exp[1].sym<<"\" is already 'core-syntax!"
        "\n     (define-syntax <label> <syntax-transformer>)"<<EXP_ERR(exp));
    }
  }


  void confirm_valid_define_syntax(const data_vector& exp) {
    if(exp.size() != 3)
      THROW_ERR("'define-syntax expects 2 arguments:"
        "\n     (define-syntax <label> <syntax-transformer>)"<<EXP_ERR(exp));
    if(!exp[1].is_type(types::sym))
      THROW_ERR("'define-syntax 1st arg "<<PROFILE(exp[1])
        <<" isn't a symbolic label!" << EXP_ERR(exp));
  }


  data extract_syntax_transformer(data&& mac, const data_vector& exp) {
    if(mac.is_type(types::syn)) return std::move(mac);
    if(primitive_data_is_a_callable(mac))
      return primitive_extract_callable_procedure(mac);
    THROW_ERR("'define-syntax syntax-transformer 2nd arg "<<PROFILE(exp[2])
      <<" isn't a syntax-rules object or callable:\n     (define-syntax "
        "<label> <syntax-transformer>)"<<EXP_ERR(exp));
  }


  void assign_macro_label(data& mac, const string& label)noexcept{
    if(mac.is_type(types::syn)) {
      mac.syn.label = label;
    } else if(mac.is_type(types::fcn)) {
      mac.fcn.name = label;
    }
  }


  exe_fcn_t analyze_define_syntax(data_vector& exp,const bool cps_block=false,const bool core_syntax=false) {
    confirm_valid_define_syntax(exp);
    if(!core_syntax) confirm_is_not_core_syntax_label(exp);
    auto syntax_transformer_exe_proc = scm_analyze(data(exp[2]),false,cps_block);
    return [syntax_transformer_exe_proc=std::move(syntax_transformer_exe_proc),
      exp=std::move(exp),core_syntax](env_type& env)mutable{
      data mac = extract_syntax_transformer(syntax_transformer_exe_proc(env),exp);
      assign_macro_label(mac,exp[1].sym);
      register_symbol_iff_new(G.MACRO_LABEL_REGISTRY,exp[1].sym);
      if(core_syntax) {
        G.GLOBAL_ENVIRONMENT_POINTER->define_macro(mac); // bind in global environment
      } else {
        env->define_macro(mac); // bind in local environment
      }
      return GLOBALS::VOID_DATA_OBJECT;
    };
  }

  /******************************************************************************
  * REPRESENTING ANALYSIS-TIME & ALWAYS-GLOBAL-SCOPE SYNTACTIC EXTENSIONS
  ******************************************************************************/

  bool is_core_syntax(const data_vector& exp)noexcept{return is_tagged_list(exp,symconst::core_syn);}

  exe_fcn_t analyze_core_syntax(data_vector& exp,const bool cps_block=false) {
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
  * REPRESENTING VARIABLES (& POTENTIAL OBJECT PROPERTY CHAINS!)
  ******************************************************************************/

  // (string-split <call> ".")
  void get_object_property_chain_sequence(const string& call, str_vector& chain){
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


  // Returns the ultimate value of the call-chain
  data get_object_property_chain_value(string&& call, str_vector&& chain, env_type& env) {
    // get the first object instance
    data value = lookup_variable_value(chain[0],env);
    // get the call value
    for(size_type i = 1, n = chain.size(); i < n; ++i) {
      if(!value.is_type(types::obj))
        THROW_ERR('\''<<call<<" can't access property "<<chain[i]<<" in non-object "
          << PROFILE(value) << '!' << EXP_ERR(call));
      bool found = false;
      // if NOT at the end of a call chain, call MUST refer to an object
      if(i+1 < n) {
        value = value.obj->get_property(chain[i], found);
      // if at the end of a call chain, could be referencing a method, so save "self" for extension
      } else {
        obj_type self = value.obj;
        value = value.obj->get_property(chain[i], found);
        if(value.is_type(types::fcn)) value.fcn.bind_self(self);
      }
      if(!found)
        THROW_ERR('\''<<call<<' '<<chain[i]<<" isn't a property in object\n     " 
          << value << " of class name [ " << value.obj->proto->class_name << " ]!" << EXP_ERR(call));
    }
    return value;
  }


  exe_fcn_t analyze_variable(string variable) {
    // If a regular variable (no object property chain)
    if(!symbol_is_property_chain_access(variable))
      return [variable=std::move(variable)](env_type& env){
        return lookup_variable_value(variable,env);
      };
    // Object accessing members/methods!
    str_vector chain; // split the call chain into object series
    get_object_property_chain_sequence(variable,chain);
    return [variable=std::move(variable),chain=std::move(chain)](env_type& env)mutable{
      return get_object_property_chain_value(std::move(variable),std::move(chain),env);
    };
  }

  /******************************************************************************
  * TRACING PROCEDURE CALLS
  ******************************************************************************/

  // Prints Debugging Call Trace (see <set-dynamic-call-trace!> primitive)
  void output_debug_call_trace(const fcn_type& procedure,const data_vector& arguments,
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
  void print_call_trace_depth_indentation(const fcn_type& procedure,const bool tail_call=false)noexcept{
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
  void output_call_trace_invocation(const fcn_type& procedure, const data_vector& arguments,const bool tail_call=false)noexcept{
    auto call_signature = procedure_call_signature(procedure.printable_procedure_name(),arguments);
    print_call_trace_depth_indentation(procedure,tail_call);
    fprintf(G.CURRENT_OUTPUT_PORT, "%s\n", call_signature.c_str());
    fflush(G.CURRENT_OUTPUT_PORT);
  }


  // Print the current recursive depth as indentation, and the result string
  void output_call_trace_result(const fcn_type& procedure, const data& result)noexcept{
    print_call_trace_depth_indentation(procedure);
    fprintf(G.CURRENT_OUTPUT_PORT, "%s\n", result.noexcept_write().c_str());
    fflush(G.CURRENT_OUTPUT_PORT);
  }


  bool tracing_procedure(const string& name)noexcept{
    return !G.TRACED_FUNCTION_NAME.empty() && G.TRACED_FUNCTION_NAME == name;
  }

  /******************************************************************************
  * APPLICATION EXECUTION
  ******************************************************************************/

  // -- STACK TRACE REGISTRATION
  void register_call_in_stack_trace(fcn_type& procedure,data_vector& arguments)noexcept{
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
      proc = primitive_extract_callable_procedure(proc);\
    else if(proc.is_type(types::cls))\
      proc = proc.cls->user_ctor;


  // -- APPLYING PRIMITIVE PROCEDURES
  data apply_primitive_procedure(data& proc,data_vector& args,env_type& env,const bool tail_call){
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
  data execute_application(data& procedure,data_vector& arguments,env_type& env,const bool tail_call,const bool applying_in_cps){
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
      extended_env->define_variable("self",procedure.fcn.self);
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
      data_vector tail_call_signature(2); // {tail-call-tag, proc-body, extended-env}
      tail_call_signature[0] = symconst::tail_call;
      tail_call_signature[1] = fcn_type(extended_env,fcn_body);
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
  data execute_application(data&& procedure,data_vector& arguments,env_type& env,const bool tail_call,const bool applying_in_cps){
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

  bool procedure_requires_continuation(const fcn_type& p)noexcept{
    return (p.is_compound() && string_begins_with(p.name,symconst::pass_continuation)) ||
           (p.is_primitive() && string_begins_with(p.name,symconst::pass_continuation));
  }

  /******************************************************************************
  * OPERATOR APPLICATION EXTRACTION
  ******************************************************************************/

  data get_operator(const data_vector& exp)noexcept{
    return exp[0];
  }

  data_vector get_operands(const data_vector& exp)noexcept{
    return data_vector(exp.begin()+1, exp.end());
  }

  /******************************************************************************
  * CPS-BLOCK APPLICATION
  ******************************************************************************/

  // Convert the <application> expression to be in CPS
  data cps_expand_application(const data_vector& application) {
    const auto app_len = application.size();
    data_vector cps_app(app_len+2);
    data_vector cps_exp(3);
    cps_exp[0] = symconst::lambda;
    cps_exp[1] = data_vector(1,generate_unique_cps_hash()); // "k"
    cps_exp[2] = data_vector(2);
    auto iter = cps_exp.begin()+2;
    for(size_type i = 0; i < app_len; ++i) {
      if(data_is_cps_atomic(application[i])) {
        cps_app[i+1] = application[i];
      } else {
        iter->exp[0] = generate_fundamental_form_cps(application[i]);
        iter->exp[1] = data_vector(3);
        iter->exp[1].exp[0] = symconst::lambda;
        iter->exp[1].exp[1] = data_vector(1,generate_unique_cps_value_hash()); // "arg"
        cps_app[i+1] = iter->exp[1].exp[1].exp[0];
        iter->exp[1].exp[2] = data_vector(2);
        iter = iter->exp[1].exp.begin()+2;
      }
    }
    cps_app[0] = symconst::cps_app_tag; // Add the cps-application prefix
    cps_app[app_len+1] = cps_exp[1].exp[0];
    iter->exp = std::move(cps_app);
    return cps_exp;
  }


  // Evaluate the argument execution procedures
  void eval_application_arg_procs(const std::vector<exe_fcn_t>& arg_procs,data_vector& arg_vals,env_type& env){
    for(size_type i = 0, n = arg_procs.size(); i < n; ++i)
      arg_vals[i] = arg_procs[i](env);
  }


  // Application of CPS-block defined procedure in a CPS block
  exe_fcn_t analyze_CPS_block_application_of_CPS_proc(data_vector& exp,const bool tail_call){
    auto op_proc  = scm_analyze(get_operator(exp),false,true);
    auto arg_exps = get_operands(exp);
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
        data_vector arg_vals(arg_procs.size() - !passing_continuation);
        // Eval each arg's exec proc to obtain the actual arg values
        if(!passing_continuation) {
          arg_procs.pop_back();
          eval_application_arg_procs(arg_procs,arg_vals,env);
          // Pass the result of the proc to the continuation
          auto result_arg = data_vector(1,execute_application(proc,arg_vals,env,false,true));
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
        data_vector arg_vals(arg_procs.size());
        eval_application_arg_procs(arg_procs,arg_vals,env);
        return execute_application(proc,arg_vals,env,tail_call,true);
      }
    };
  }


  // Application of a macro OR non-CPS-block defined procedure entity in a CPS block
  exe_fcn_t analyze_CPS_block_application_of_non_CPS_proc(data_vector& exp,const bool tail_call){
    auto arg_exps = get_operands(exp);
    // Save name of invoking entity (iff a symbol) to check for a possible macro
    string op_name = exp[0].is_type(types::sym) ? exp[0].sym : "";
    // If possible analysis-time macro, expand and return analysis of the expansion
    if(application_is_a_potential_macro(op_name,G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY)) {
      if(data_vector expanded; expand_macro_if_in_env(op_name, arg_exps, G.GLOBAL_ENVIRONMENT_POINTER, expanded)) {
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
      if(data_vector expanded; expand_macro_if_in_env(op_name, arg_exps, env, expanded)) {
        return scm_analyze(generate_fundamental_form_cps(expanded),tail_call,true)(env);
      }
      // else convert the function application to CPS & eval it
      return scm_analyze(cps_expand_application(exp),tail_call,true)(env);
    };
  }


  // Macro/procedure application in a CPS block
  exe_fcn_t analyze_CPS_block_application(data_vector& exp,const bool tail_call=false){
    // If application is already expanded (given a continuation as last arg),
    //   GUARANTEED such is applying to a function and NOT a macro
    if(data_is_continuation_parameter(*exp.rbegin()))
      return analyze_CPS_block_application_of_CPS_proc(exp,tail_call);
    // If application is NOT already expanded
    return analyze_CPS_block_application_of_non_CPS_proc(exp,tail_call);
  }

  /******************************************************************************
  * APPLICATION
  ******************************************************************************/

  // Analyzes the operator & operands, then returns an exec proc passing 
  //   both the operator/operand proc exec's to 'execute-application'
  //   (after having checked for macro use as well)
  exe_fcn_t analyze_application(data_vector& exp,const bool tail_call=false,const bool cps_block=false){
    // If in a scm->cps block
    if(cps_block && is_cps_application(exp)) {
      exp.erase(exp.begin()); // Rm the cps-application prefix
      return analyze_CPS_block_application(exp,tail_call);
    }
    auto arg_exps = get_operands(exp);
    // Save name of invoking entity (iff a symbol) to check for a possible macro
    string op_name = exp[0].is_type(types::sym) ? exp[0].sym : "";
    // If possible analysis-time macro, expand and return analysis of the expansion
    if(application_is_a_potential_macro(op_name,G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY)) {
      if(data_vector expanded; expand_macro_if_in_env(op_name, arg_exps, G.GLOBAL_ENVIRONMENT_POINTER, expanded)) {
        return scm_analyze(std::move(expanded),tail_call,cps_block);
      } else {
        THROW_ERR("'core-syntax expression (label \"" << op_name 
          << "\") didn't match any patterns!" << EXP_ERR(exp));
      }
    }
    auto op_proc = scm_analyze(get_operator(exp),false,cps_block);
    // If _NOT_ a possible macro, analyze the applicator's args ahead of time
    if(!application_is_a_potential_macro(op_name,G.MACRO_LABEL_REGISTRY)) {
      std::vector<exe_fcn_t> arg_procs(arg_exps.size());
      for(size_type i = 0, n = arg_exps.size(); i < n; ++i)
        arg_procs[i] = scm_analyze(std::move(arg_exps[i]),false,cps_block);
      return [op_proc=std::move(op_proc),arg_procs=std::move(arg_procs),
              tail_call=std::move(tail_call),cps_block=cps_block](env_type& env){
        data_vector arg_vals(arg_procs.size());
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
      if(data_vector expanded; expand_macro_if_in_env(op_name, arg_exps, env, expanded))
        return scm_analyze(std::move(expanded),tail_call,cps_block)(env);
      // eval each arg's exec proc to obtain the actual arg values
      data_vector arg_vals(arg_exps.size());
      for(size_type i = 0, n = arg_exps.size(); i < n; ++i)
        arg_vals[i] = scm_analyze(data(arg_exps[i]),false,cps_block)(env);
      evaluate_operator(op_proc,env); // generates <data proc.is_type(types::fcn)>
      return execute_application(proc,arg_vals,env,tail_call,cps_block);
    };
  }

  /******************************************************************************
  * ANALYSIS & EVALUATION
  ******************************************************************************/

  // -- EVAL 
  data scm_eval(data&& datum, env_type& env) { // evaluate expression environment
    return scm_analyze(std::move(datum))(env);
  }

  // -- ANALYZE (SYNTAX)
  exe_fcn_t scm_analyze(data&& datum,const bool tail_call,const bool cps_block) { // analyze expression
    if(datum.is_self_evaluating())           return [d=std::move(datum)](env_type&){return d;};
    else if(datum.is_type(types::sym))       return analyze_variable(datum.sym);
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
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(symconst::true_t,        GLOBALS::TRUE_DATA_BOOLEAN);
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(symconst::false_t,       GLOBALS::FALSE_DATA_BOOLEAN);
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*heist-dirname*",       make_str(HEIST_DIRECTORY_FILE_PATH));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("fl-precision",          num_type(num_type::INEXACT_PRECISION));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("fl-max",                num_type(num_type::INEXACT_MAX));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("fl-min",                num_type(num_type::INEXACT_MIN));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("fl-epsilon",            num_type(num_type::INEXACT_EPSILON));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*max-infix-precedence*",num_type(LLONG_MAX));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*min-infix-precedence*",num_type(LLONG_MIN));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*heist-platform*",      HEIST_PLATFORM);
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*heist-exact-platform*",HEIST_EXACT_PLATFORM);
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("stream-null",           symconst::emptylist);
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(symconst::exit_success,  num_type(0));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(symconst::exit_failure,  num_type(1));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(symconst::null_env,      symconst::null_env);
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(symconst::local_env,     symconst::local_env);
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(symconst::global_env,    symconst::global_env);
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*argv*", primitive_LIST_to_CONS_constructor(GLOBALS::ARGV.begin(),GLOBALS::ARGV.end()));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*argc*", num_type(GLOBALS::ARGV.size()));
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
  data_vector read_user_input(FILE* outs, FILE* ins, const bool& in_repl){
    string input, tmp_buffer;
    data_vector abstract_syntax_tree;
    int ch;
    for(;;) {
      // Read input
      fflush(outs);
      tmp_buffer.clear();
      while((ch = fgetc(ins)) != '\n' && ch != EOF) tmp_buffer += ch;
      // Handle EOF Signal
      if(ch == EOF && ins == stdin) {
        clearerr(stdin);
        if(in_repl) return data_vector(1,chr_type(EOF)); // called by REPL
        return data_vector();                            // called by <read>
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
#ifndef HEIST_CPP_INTEROP_HPP_ // @NOT-EMBEDDED-IN-C++
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
bool repl_detected_EOF_signal(const heist::data_vector& AST)noexcept{
  return AST.size() == 1 && AST[0].is_type(heist::types::chr) && AST[0].chr == EOF;
}


// Wrap each entry in "scm->cps" (w/ "id" bound as the topmost cont.) 
//   if "-cps" cmd-line flag passed
void cpsify_inputs(heist::data_vector& AST) {
  const heist::size_type n = AST.size();
  heist::data_vector CPS_AST(n);
  for(heist::size_type i = 0; i < n; ++i) {
    CPS_AST[i] = heist::data_vector(2);
    CPS_AST[i].exp[0] = heist::data_vector(2);
    CPS_AST[i].exp[0].exp[0] = heist::symconst::scm_cps;
    CPS_AST[i].exp[0].exp[1] = AST[i];
    CPS_AST[i].exp[1] = "id";
  }
  AST = CPS_AST;
}


// Returns (begin (define #it <d>) #it)
heist::data repl_tag_expression(const heist::data& d)noexcept{
  heist::data tag_exp = heist::data_vector(3);
  tag_exp.exp[0] = heist::symconst::begin;
  tag_exp.exp[1] = heist::data_vector(3);
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
      LOADED_FILES.push_back(HEIST_DIRECTORY_FILE_PATH "/lib/core_evaluator/infix.scm");
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
  heist::data_vector load_args(2 + heist::G.USING_CPS_CMD_LINE_FLAG);
  load_args[0] = heist::make_str(filename);
  load_args[1 + heist::G.USING_CPS_CMD_LINE_FLAG] = heist::G.GLOBAL_ENVIRONMENT_POINTER;
  // Bind "id" as the topmost continuation if "-cps" was passed
  if(heist::G.USING_CPS_CMD_LINE_FLAG)
    load_args[1] = heist::fcn_type("id",(heist::prm_ptr_t)[](heist::data_vector& args){return args[0];});
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
  heist::data_vector compile_args(2);
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