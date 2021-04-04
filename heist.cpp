// Author: Jordan Randleman -- jrandleman@scu.edu -- heist.cpp
// => Main execution and AST evaluation for the C++ Heist Scheme Interpreter
// => GitHub: https://github.com/jrandleman/Heist-Scheme

#ifndef HEIST_SCHEME_CORE_CPP_
#define HEIST_SCHEME_CORE_CPP_

/***
 * INSTALL:
 *   SEE "docs/INSTALL.md" FOR REQUIRED INSTALL INSTRUCTIONS PRIOR COMPILATION!
 *
 * COMPILE: 
 *   $ clang++ -std=c++17 -O3 -o heist heist.cpp
 *
 * FLAG DESCRIPTIONS:
 *   0. "-std=c++17": [REQUIRED] compile using the C++17 standard
 *   1. "-O3": [RECOMMENDED] maximum runtime optimization
 *             -> Full -O3 compilation takes about 70s. Be patient.
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
 *         * SEE "primitives.json" FOR THE ALL C++ PRIMITIVE BINDINGS
 */

#include "lib/core/type_system/types.hpp"
#include "lib/core/reader/parser.hpp"
#include "lib/core/reader/repl.hpp"
#include "lib/core/primitives.hpp"
#include "lib/core/evaluator/evaluator.hpp"

/******************************************************************************
* GLOBAL ENVIRONMENT SETUP
******************************************************************************/

namespace heist {
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
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*argv*", primitive_toolkit::convert_data_vector_to_proper_list(GLOBALS::ARGV.begin(),GLOBALS::ARGV.end()));
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable("*argc*", num_type(GLOBALS::ARGV.size()));
    evaluate_primitives_written_in_heist_scheme();
  }
}

/******************************************************************************
* INTERPRET SCRIPT HELPER FUNCTION
******************************************************************************/

namespace heist {
  int load_script(const char* filename){
    // Load the script & immediately exit
    data_vector load_args(2 + G.USING_CPS_CMD_LINE_FLAG);
    load_args[0] = make_str(filename);
    load_args[1 + G.USING_CPS_CMD_LINE_FLAG] = G.GLOBAL_ENVIRONMENT_POINTER;
    // Bind "id" as the topmost continuation if "-cps" was passed
    if(G.USING_CPS_CMD_LINE_FLAG)
      load_args[1] = fcn_type("id",DEFAULT_TOPMOST_CONTINUATION::id);
    try {
      if(G.USING_CPS_CMD_LINE_FLAG)
        primitive_CPS_LOAD(std::move(load_args));
      else
        primitive_LOAD(std::move(load_args));
    } catch(const SCM_EXCEPT& eval_throw) {
      /* warn about uncaught <jump!> */
      if(eval_throw == SCM_EXCEPT::JUMP)
        HEIST_PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
          << HEIST_PROFILE(GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
      /* catch <exit> signal */
      if(eval_throw == SCM_EXCEPT::EXIT) 
        return GLOBALS::HEIST_EXIT_CODE;
      /* catch errors already output to stdout */ 
      putchar('\n');
      return 1;
    } catch(...) {
      /* catch uncaught C++ exceptions -:- ANOMALY -:- */
      HEIST_PRINT_ERR(HEIST_AFMT(AFMT_1) << 
        "\nUncaught C++ Exception Detected! -:- BUG ALERT -:-"
        "\n  => While interpreting script \"" << filename << "\""
        "\n  => Please send your code to jrandleman@scu.edu to fix"
        "\n     the interpreter's bug!"
        "\n  => Terminating Heist Scheme Interpretation.\n\n" << HEIST_AFMT(AFMT_0));
      return 1;
    }
    return 0;
  }
}

/******************************************************************************
* COMPILE SCRIPT HELPER FUNCTION
******************************************************************************/

namespace heist {
  int compile_script(char* argv[], const int& compile_pos, std::string& compile_as){
    // Compile the script & immediately exit
    data_vector compile_args(2);
    compile_args[0] = make_str(argv[compile_pos]);
    compile_args[1] = make_str(compile_as);
    try {
      if(G.USING_CPS_CMD_LINE_FLAG)
        primitive_CPS_COMPILE(std::move(compile_args));
      else
        primitive_COMPILE(std::move(compile_args));
    } catch(const SCM_EXCEPT& eval_throw) {
      /* warn about uncaught <jump!> */
      if(eval_throw == SCM_EXCEPT::JUMP)
        HEIST_PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
          << HEIST_PROFILE(GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
      /* catch <exit> signal */
      if(eval_throw == SCM_EXCEPT::EXIT) 
        return GLOBALS::HEIST_EXIT_CODE;
      /* catch errors already output to stdout */ 
      putchar('\n');
      return 1;
    } catch(...) {
      /* catch uncaught C++ exceptions -:- ANOMALY -:- */
      HEIST_PRINT_ERR(HEIST_AFMT(AFMT_1) << 
        "\nUncaught C++ Exception Detected! -:- BUG ALERT -:-"
        "\n  => While compiling script \"" << argv[compile_pos] << "\""
        "\n  => Please send your code to jrandleman@scu.edu to fix"
        "\n     the interpreter's bug!"
        "\n  => Terminating Heist Scheme Interpretation.\n\n" << HEIST_AFMT(AFMT_0));
      return 1;
    }
    return 0;
  }
}

/******************************************************************************
* LOAD SCRIPT HELPER FUNCTION
******************************************************************************/

namespace heist {
  int load_scripts(const std::vector<const char*>& LOADED_FILES) {
    bool old_cps_val = G.USING_CPS_CMD_LINE_FLAG;
    G.USING_CPS_CMD_LINE_FLAG = false;
    for(auto filename : LOADED_FILES)
      if(int result = load_script(filename); result)
        return result;
    G.USING_CPS_CMD_LINE_FLAG = old_cps_val;
    return 0;
  }
}


#ifndef HEIST_SCHEME_CORE_CPP_INTEROP_HPP_ // @NOT-EMBEDDED-IN-C++
#ifndef HEIST_INTERPRETING_COMPILED_AST // @ONLY-INTERPRETER

/******************************************************************************
* COMMAND LINE ARGUMENT VALIDATION
******************************************************************************/

namespace heist {

  void POPULATE_ARGV_REGISTRY(int argc,int& i,char* argv[])noexcept{
    while(i < argc) GLOBALS::ARGV.push_back(str_type(argv[i++]));
  }


  bool confirm_valid_non_negative_integer(const char* name, int& i, int argc, char* argv[], size_type& result)noexcept{
    if(i == argc-1) {
      fprintf(stderr,"\n> \"%s\" wasn't followed by a non-negative integer!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n",name);
      return false;
    }
    auto num = num_type(argv[++i]);
    if(!num.is_integer() || num.is_neg()) {
      fprintf(stderr,"\n> \"%s\" wasn't followed by a non-negative integer!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n",name);
      return false;
    }
    auto float_num = num.to_inexact();
    if(float_num < 0 || float_num > GLOBALS::MAX_SIZE_TYPE) {
      fprintf(stderr,"\n> \"%s\" integer was out of range: [0, %zu]!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n",name,GLOBALS::MAX_SIZE_TYPE);
      return false;
    }
    result = size_type(float_num.extract_inexact());
    return true;
  }


  bool script_exists(const char* script)noexcept{
    FILE* read = fopen(script, "r");
    if(read) {
      fclose(read);
      return true;
    }
    return false;
  }


  bool not_heist_cmd_line_flag(const std::string& next_cmd)noexcept{
  return next_cmd != "-c" && next_cmd != "-l" && next_cmd != "-infix" && next_cmd != "-cps" && 
         next_cmd != "-nansi" && next_cmd != "-ci" && next_cmd != "-dynamic-call-trace" && next_cmd != "-trace-args" && 
         next_cmd != "-trace-limit" && next_cmd != "--version" && next_cmd != "--help";
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
        GLOBALS::USING_CASE_SENSITIVE_SYMBOLS = false;
      } else if(cmd_flag == "--version") {
        immediate_exit = true;
        puts("Heist Scheme Version 8.0\nTarget: " HEIST_EXACT_PLATFORM "\nInstalledDir: " HEIST_DIRECTORY_FILE_PATH);
        return true;
      } else if(cmd_flag == "--help") {
        immediate_exit = true;
        puts(HEIST_COMMAND_LINE_ARGS);
        return true;
      } else if(cmd_flag == "-infix") {
        LOADED_FILES.push_back(HEIST_DIRECTORY_FILE_PATH "/lib/core/reader/infix.scm");
      } else if(cmd_flag == "-dynamic-call-trace") {
        trace_calls = true;
      } else if(cmd_flag == "-trace-args") {
        G.TRACE_ARGS = true;
      } else if(cmd_flag == "-nansi") {
        G.USING_ANSI_ESCAPE_SEQUENCES = false;
      } else if(cmd_flag == "-cps") {
        G.USING_CPS_CMD_LINE_FLAG = true;
      } else if(cmd_flag == "-trace-limit") {
        if(!confirm_valid_non_negative_integer("-trace-limit",i,argc,argv,G.TRACE_LIMIT))
          return false;
      } else if(cmd_flag == "-l") {
        if(i == argc-1) {
          fprintf(stderr,"\n> \"-l\" wasn't followed by a file!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n");
          return false;
        }
        LOADED_FILES.push_back(argv[++i]);
      } else if(cmd_flag == "-c") {
        if(i == argc-1) {
          fprintf(stderr,"\n> \"-c\" wasn't followed by a file!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n");
          return false;
        }
        compile_pos = ++i;
        if(i < argc-1 && not_heist_cmd_line_flag(argv[i+1])) compile_as = argv[++i];
      } else { // found file to interpret
        if(compile_pos != -1) {
          fprintf(stderr,"\n> Can't interpret & compile files simultaneously!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n");
          return false;
        } else if(!script_exists(argv[i])) {
          fprintf(stderr,"\n> Invalid Script: can't open \"%s\" for reading!\n\n" HEIST_COMMAND_LINE_ARGS "\n\n", argv[i]);
          return false;
        }
        script_pos = i;
        POPULATE_ARGV_REGISTRY(argc,i,argv);
        return true;
      }
    }
    return true;
  }

} // End of namespace heist

/******************************************************************************
* MAIN INTERPRETER EXECUTION
******************************************************************************/

int main(int argc, char* argv[]) {
  // Validate arguments
  int script_pos = -1, compile_pos = -1;
  bool immediate_exit = false, trace_calls = false;
  std::string compile_as = heist::symconst::dflt_compile_name;
  std::vector<const char*> LOADED_FILES;
  if(!heist::confirm_valid_command_line_args(argc,argv,script_pos,compile_pos,compile_as,
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
    if(int result = heist::load_scripts(LOADED_FILES); result) return result;
  // Interpret a Script (as needed)
  if(script_pos != -1)
    return heist::load_script(argv[script_pos]);
  // Compile a Script (as needed)
  if(compile_pos != -1) 
    return heist::compile_script(argv, compile_pos, compile_as);
  // Run the REPL
  puts("Heist Scheme Version 8.0\nEnter '(help)' for Help, '(exit)' to Exit");
  return heist::launch_repl();
}

/******************************************************************************
* MAIN COMPILED-AST EXECUTION
******************************************************************************/

#else // @ONLY-COMPILER
void POPULATE_ARGV_REGISTRY(int argc,char* argv[])noexcept{
  int i = 0;
  while(i < argc) heist::GLOBALS::ARGV.push_back(heist::str_type(argv[i++]));
}

int interpret_premade_AST_code(){
  // Set up the environment (allocates & fills G.GLOBAL_ENVIRONMENT_POINTER)
  heist::set_default_global_environment();
  // Eval the pre-read & "compiled" scm AST code
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
        HEIST_PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
          << HEIST_PROFILE(heist::GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
    } catch(...) {
      HEIST_PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
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
  POPULATE_ARGV_REGISTRY(argc,argv);
  return interpret_premade_AST_code();
}
#endif // @ONLY-COMPILER
#undef HEIST_AFMT
#undef HEIST_DIRECTORY_FILE_PATH
#undef HEIST_PLATFORM
#undef HEIST_EXACT_PLATFORM
#undef HEIST_ERR_HEADER
#undef HEIST_BAD_SYNTAX
#undef HEIST_EXP_ERR
#undef HEIST_FCN_ERR
#undef HEIST_PROFILE
#undef HEIST_PRINT_ERR
#undef HEIST_THROW_ERR
#endif // @NOT-EMBEDDED-IN-C++
#undef HEIST_COMMAND_LINE_ARGS
#endif