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
#include "lib/core/input_parser.hpp"
#include "lib/core/evaluation/evaluator.hpp"

namespace heist {

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
  * REPL READER
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

/******************************************************************************
* REPL DRIVER LOOP HELPER FUNCTIONS
******************************************************************************/

#ifndef HEIST_CPP_INTEROP_HPP_ // @NOT-EMBEDDED-IN-C++
#ifndef HEIST_INTERPRETING_COMPILED_AST // @ONLY-INTERPRETER

// Account for whether REPL should print a newline
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

/******************************************************************************
* REPL DRIVER LOOP
******************************************************************************/

int launch_repl() {
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
      LOADED_FILES.push_back(HEIST_DIRECTORY_FILE_PATH "/lib/core/infix.scm");
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
  int result = launch_repl();
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