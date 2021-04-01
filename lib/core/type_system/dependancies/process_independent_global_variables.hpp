// Author: Jordan Randleman -- jrandleman@scu.edu -- process_indpendent_global_variables.hpp
// => Contains process-independent global variables for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_PROCESS_INDEPENDENT_GLOBAL_VARIABLES_HPP_
#define HEIST_SCHEME_CORE_PROCESS_INDEPENDENT_GLOBAL_VARIABLES_HPP_

namespace heist::GLOBALS {

  /* EXIT SUCCESS CODE TO RETURN */
  int HEIST_EXIT_CODE = 0;


  /* WHETHER SYMBOLS ARE CASE-SENSITIVE */
  bool USING_CASE_SENSITIVE_SYMBOLS = true; // see (ci?) primitive


  /* ARGV REGISTRY OF STRINGS */
  std::vector<str_type> ARGV;


  /* STACK TRACE */
  str_vector STACK_TRACE;


  /* MAX VALUE FOR SIZE_TYPE */
  constexpr const auto MAX_SIZE_TYPE = std::numeric_limits<size_type>::max();


  /* INTERNAL COMMON CONSTANT VALUES */
  const auto FALSE_DATA_BOOLEAN = data(boolean(false));
  const auto TRUE_DATA_BOOLEAN  = data(boolean(true));
  const auto VOID_DATA_OBJECT   = data(types::dne);


  /* GLOBAL "JUMP!" PRIMITIVE ARGUMENT STORAGE */
  data JUMP_GLOBAL_PRIMITIVE_ARGUMENT; // see catch-jump & jump!


  /* REGISTRY OF PRIMITIVES ALSO REQUIRING AN ENVIRONMENT (TO APPLY A PROCEDURE) */
  #ifdef HEIST_SCHEME_CORE_CPP_INTEROP_HPP_ // @EMBEDDED-IN-C++
    std::vector<prm_ptr_t> USER_DEFINED_PRIMITIVES_REQUIRING_ENV;
  #endif

}

#endif