// Author: Jordan Randleman -- jrandleman@scu.edu -- error_handling.hpp
// => Contains error-handling macros for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_ERROR_HANDLING_HPP_
#define HEIST_SCHEME_CORE_ERROR_HANDLING_HPP_

namespace heist {

  /******************************************************************************
  * ANSI ESCAPE SEQUENCE FORMATS & MACRO
  ******************************************************************************/

  enum afmts {
    AFMT_0 = 1, // clear
    AFMT_1,     // bold
    AFMT_31,    // red     (errors)
    AFMT_35,    // magenta (warnings)
    AFMT_131,   // bold red
    AFMT_135,   // bold magenta
    AFMT_01,    // clear bold
    AFMT_32,    // green
  };

  constexpr const char * const ansi_formats[] = {
    "",         "\x1b[0m",         "\x1b[1m",         "\x1b[31m",
    "\x1b[35m", "\x1b[1m\x1b[31m", "\x1b[1m\x1b[35m", "\x1b[0m\x1b[1m",
    "\x1b[32m", 
  };

  #define HEIST_AFMT(heist_ansi_esc) heist::ansi_formats[heist::G.USING_ANSI_ESCAPE_SEQUENCES*heist_ansi_esc]

  /******************************************************************************
  * CALL SIGNATURE GENERATION
  ******************************************************************************/

  // Generate a call signature from a procedure name & its given values
  string procedure_call_signature(const string& name,const data_vector& vals)noexcept{
    if(vals.empty()) return '(' + name + ')';
    return '(' + name + ' ' + data(vals).noexcept_write().substr(1);
  }

  /******************************************************************************
  * STACK TRACE STRINGIFICATION HELPER FUNCTION
  ******************************************************************************/

  string stack_trace_str(const string& tab = "  ") noexcept {
    if(GLOBALS::STACK_TRACE.empty() || !G.TRACE_LIMIT) return "";
    string trace(HEIST_AFMT(heist::AFMT_01));
    trace += HEIST_AFMT(heist::AFMT_35);
    trace += tab + ">> Stack Trace:";
    trace += HEIST_AFMT(heist::AFMT_01);
    auto end = GLOBALS::STACK_TRACE.size() < G.TRACE_LIMIT ? GLOBALS::STACK_TRACE.rend() 
                                                           : GLOBALS::STACK_TRACE.rbegin() + G.TRACE_LIMIT;
    for(auto iter = GLOBALS::STACK_TRACE.rbegin(); iter != end; ++iter)
      trace += "\n   " + tab + *iter;
    GLOBALS::STACK_TRACE.clear();
    return (trace + HEIST_AFMT(heist::AFMT_0)) + '\n';
  }

  /******************************************************************************
  * ERROR HANDLING CODE ENUMERATIONS
  ******************************************************************************/

  enum class SCM_EXCEPT {EXIT, EVAL, READ, JUMP};

  /******************************************************************************
  * THROW HACK: ENABLES throw IN THE "SAME EXPRESSION" AS A std::cerr INVOCATION
  ******************************************************************************/

  std::string throw_eval_error() {
    throw heist::SCM_EXCEPT::EVAL;
    return ""; // never triggered
  }

  /******************************************************************************
  * ERROR HANDLING PRINTER MACROS
  ******************************************************************************/

  #define HEIST_ERR_HEADER '\n' << HEIST_AFMT(heist::AFMT_1) << __FILE__ << ':' << __func__ << \
                           ':' << __LINE__ << ':' << HEIST_AFMT(heist::AFMT_31) << \
                           " ERROR: \n" << HEIST_AFMT(heist::AFMT_01) << "  => "

  #define HEIST_BAD_SYNTAX '\n'<<HEIST_AFMT(heist::AFMT_35)<<"  >> Invalid Syntax:"<<HEIST_AFMT(heist::AFMT_01)<<' '

  #define HEIST_EXP_ERR(heist_errExp) HEIST_BAD_SYNTAX << heist_errExp
  #define HEIST_FCN_ERR(heist_fName,heist_fVal) HEIST_BAD_SYNTAX << heist::procedure_call_signature(heist_fName,heist_fVal)

  #define HEIST_PROFILE(heist_dataObj) heist_dataObj<<" of type \""<<heist_dataObj.type_name()<<'"'

  #define HEIST_PRINT_ERR(...) std::cerr<<HEIST_ERR_HEADER<<__VA_ARGS__<<'\n'<<heist::stack_trace_str()<<HEIST_AFMT(heist::AFMT_0)
  #define HEIST_THROW_ERR(...) HEIST_PRINT_ERR(__VA_ARGS__)<<heist::throw_eval_error()
}

#endif