// Author: Jordan Randleman -- jordanran199@gmail.com -- primitives.hpp
// => Defines the registry of C++ primitive functions for the C++ Heist Scheme Interpreter

////////////////////////////////////////////////////////////////////////////////////////
// => NOTE: This file is modified by either installer to include the primitives from  //
//          "lib/primitives/primitives.json", & hence should NOT be modified by hand! //
////////////////////////////////////////////////////////////////////////////////////////

#ifndef HEIST_SCHEME_CORE_PRIMITIVES_HPP_
#define HEIST_SCHEME_CORE_PRIMITIVES_HPP_

/******************************************************************************
* HELPER FUNCTIONS FOR C++ PRIMITIVE EXTENSIONS
******************************************************************************/

#include "../primitives/primitive_toolkit.hpp"

/******************************************************************************
* HELPER FUNCTION FROM "heist.cpp" IN ORDER TO LOAD HEIST SCHEME SCRIPTS
******************************************************************************/

namespace heist { int load_script(const char* filename); } // from heist.cpp

/******************************************************************************
* SET OF C++ FILES TO INCLUDE AS EXTENSIONS TO THE INTERPRETER
******************************************************************************/

//@HEIST-INSTALLER-FILE-INCLUDE-REGISTRY-BEGIN
//@HEIST-INSTALLER-FILE-INCLUDE-REGISTRY-END

namespace heist {

  /******************************************************************************
  * REGISTRY OF PRIMITIVES USING DYNAMIC SCOPE (GET CURRENT ENV APPENDED TO ARGS)
  ******************************************************************************/

//@HEIST-INSTALLER-DYNAMIC-SCOPE-REGISTRY-BEGIN
//@HEIST-INSTALLER-DYNAMIC-SCOPE-REGISTRY-END

#ifndef HEIST_SCHEME_CORE_CPP_INTEROP_HPP_ // @NOT-EMBEDDED-IN-C++
  constexpr bool primitive_requires_environment(const prm_ptr_t& prm)noexcept{
    for(const auto& p : PRIMITIVES_REQUIRING_CURRENT_ENVIRONMENT)
      if(p == prm) return true;
    return false;
  }
#else // @EMBEDDED-IN-C++
  bool primitive_requires_environment(const prm_ptr_t& prm)noexcept{
    for(const auto& p : PRIMITIVES_REQUIRING_CURRENT_ENVIRONMENT)
      if(p == prm) return true;
    for(const auto& p : GLOBALS::USER_DEFINED_PRIMITIVES_REQUIRING_ENV)
      if(p == prm) return true;
    return false;
  }
#endif

  /******************************************************************************
  * REGISTRY OF C++ & SCM PRIMITIVE NAME ASSOCIATIONS
  ******************************************************************************/

//@HEIST-INSTALLER-SCHEME-PRIMITIVE-REGISTRY-BEGIN
//@HEIST-INSTALLER-SCHEME-PRIMITIVE-REGISTRY-END

  void evaluate_primitives_written_in_heist_scheme() {
    static constexpr const char* stdlib_path = HEIST_DIRECTORY_FILE_PATH "/lib/primitives/stdlib/lang/stdlib.scm";
    bool old_USING_CPS_value = G.USING_CPS_CMD_LINE_FLAG;
    G.USING_CPS_CMD_LINE_FLAG = false;
    for(auto filename : primitive_scm_source_files) {
      if(strcmp(filename,"stdlib.scm") == 0) filename = stdlib_path;
      if(load_script(filename)) {
        HEIST_PRINT_ERR("FATAL HEIST SCHEME ERROR:"
          "\n=> PRIMITIVE SCHEME SOURCE CODE FROM \"" << filename << "\" TRIGGERED AN"
          "\n   UNHANDLED C++ EXCEPTION WHILE INITIATING THE GLOBAL ENVIRONMENT!"
          "\n=> PRIMITIVE SCHEME SOURCE FILES MUST ___NEVER___ TRIGGER EXCEPTIONS!"
          "\n   -> THIS INCLUDES UNCAUGHT \"jump!\" CLAUSES!"
          "\n=> TERMINATING HEIST SCHEME INTERPRETATION.\n\n");
        std::exit(1);
      }
    }
    G.USING_CPS_CMD_LINE_FLAG = old_USING_CPS_value;
  }

  /******************************************************************************
  * REGISTRY OF C++ & SCM PRIMITIVE NAME ASSOCIATIONS
  ******************************************************************************/

//@HEIST-INSTALLER-PRIMITIVE-ASSOCIATION-REGISTRY-BEGIN
//@HEIST-INSTALLER-PRIMITIVE-ASSOCIATION-REGISTRY-END

  data_vector primitive_procedure_objects()noexcept{
    constexpr const auto n = sizeof(primitive_procedure_declarations) / sizeof(primitive_procedure_declarations[0]);
    data_vector primitive_procedures(n);
    for(size_type i = 0; i < n; ++i)
      primitive_procedures[i] = fcn_type(primitive_procedure_declarations[i].second,primitive_procedure_declarations[i].first);
    return primitive_procedures;
  }

  str_vector primitive_procedure_names()noexcept{
    constexpr const auto n = sizeof(primitive_procedure_declarations) / sizeof(primitive_procedure_declarations[0]);
    str_vector names(n);
    for(size_type i = 0; i < n; ++i)
      names[i] = primitive_procedure_declarations[i].second;
    return names;
  }
} // End of namespace heist
#endif