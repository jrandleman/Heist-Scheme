// Author: Jordan Randleman -- jrandleman@scu.edu -- error_handling.hpp
// => Contains error-handling macros for the C++ Heist Scheme Interpreter

#ifndef HEIST_ERROR_HANDLING_HPP_
#define HEIST_ERROR_HANDLING_HPP_

namespace heist {

  // Helper procedure provided by heist.cpp
  string procedure_call_signature(const string& name,const data_vector& vals)noexcept;

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

  #define afmt(ansi_esc) heist::ansi_formats[heist::G.USING_ANSI_ESCAPE_SEQUENCES*ansi_esc]

  /******************************************************************************
  * STACK TRACE STRINGIFICATION HELPER FUNCTION
  ******************************************************************************/

  string stack_trace_str(const string& tab = "  ") noexcept {
    if(GLOBALS::STACK_TRACE.empty() || !G.TRACE_LIMIT) return "";
    string trace(afmt(heist::AFMT_01));
    trace += afmt(heist::AFMT_35);
    trace += tab + ">> Stack Trace:";
    trace += afmt(heist::AFMT_01);
    auto end = GLOBALS::STACK_TRACE.size() < G.TRACE_LIMIT ? GLOBALS::STACK_TRACE.rend() 
                                                           : GLOBALS::STACK_TRACE.rbegin() + G.TRACE_LIMIT;
    for(auto iter = GLOBALS::STACK_TRACE.rbegin(); iter != end; ++iter)
      trace += "\n   " + tab + *iter;
    GLOBALS::STACK_TRACE.clear();
    return (trace + afmt(heist::AFMT_0)) + '\n';
  }

  /******************************************************************************
  * ERROR HANDLING CODE ENUMERATIONS
  ******************************************************************************/

  enum class SCM_EXCEPT {EXIT, EVAL, READ, JUMP};

  /******************************************************************************
  * ERROR HANDLING PRINTER MACROS
  ******************************************************************************/

  #define ERR_HEADER '\n' << afmt(heist::AFMT_1) << __FILE__ << ':' << __func__ << \
                     ':' << __LINE__ << ':' << afmt(heist::AFMT_31) << \
                     " ERROR: \n" << afmt(heist::AFMT_01) << "  => "

  #define BAD_SYNTAX '\n'<<afmt(heist::AFMT_35)<<"  >> Invalid Syntax:"<<afmt(heist::AFMT_01)<<' '

  #define EXP_ERR(errExp)     BAD_SYNTAX << errExp
  #define FCN_ERR(fName,fVal) BAD_SYNTAX << heist::procedure_call_signature(fName,fVal)

  #define PROFILE(dataObj) dataObj<<" of type \""<<dataObj.type_name()<<'"'

  #define PRINT_ERR(...) std::cerr<<ERR_HEADER<<__VA_ARGS__<<'\n'<<heist::stack_trace_str()<<afmt(heist::AFMT_0)
  #define THROW_ERR(...) ({PRINT_ERR(__VA_ARGS__); throw heist::SCM_EXCEPT::EVAL;})
}

#endif