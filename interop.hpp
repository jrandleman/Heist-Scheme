// Author: Jordan Randleman -- jrandleman@scu.edu -- interop.hpp
// => Wrapper around the Heist Scheme Interpreter to set up C++17 interop

/**************************************************
 * DESIGNED FOR ___SINGLE-THREADED___ APPLICATIONS
 *************************************************/

#ifndef HEIST_SCHEME_CORE_CPP_INTEROP_HPP_
#define HEIST_SCHEME_CORE_CPP_INTEROP_HPP_

// Defines 3 Functions for C++ Interop w/ Heist:
//   0) eval   // evaluate heist code string, same as _heist literal (see below)
//   1) apply  // apply args to Heist callable (procedure or functor)
//   2) define // define C++ Heist primitive _OR_ a global Heist variable

#include "heist.cpp"

namespace heist {
  // Evaluate Heist Scheme Expression in String
  data eval(std::string exp) noexcept {
    if(!G.GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment(); 
    data_vector abstract_syntax_tree;
    try {
      // Evaluate AST if successfully parsed an expression
      // => We wrap evaluated datums in "data" (*)HERE(*) to NOT mv the evaluated datum, 
      //    in case such needs to be shown in the error message upon an evaluation error.
      parse_input_exp(std::move(exp),abstract_syntax_tree);
      for(size_type i = 0, n = abstract_syntax_tree.size(); i < n; ++i) {
        try {
          if(i+1 == n)
            return scm_eval(data(abstract_syntax_tree[i]),G.GLOBAL_ENVIRONMENT_POINTER); // (*)HERE(*)
          scm_eval(data(abstract_syntax_tree[i]),G.GLOBAL_ENVIRONMENT_POINTER);          // (*)HERE(*)
        } catch(const SCM_EXCEPT& eval_throw) {
          if(eval_throw == heist::SCM_EXCEPT::JUMP) {
            HEIST_PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
              << HEIST_PROFILE(heist::GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
          } else if(eval_throw == heist::SCM_EXCEPT::EXIT) {
            if(heist::GLOBALS::HEIST_EXIT_CODE)
              return data(heist::symconst::exit_failure);
            return data(heist::symconst::exit_success);
          }
          fputs("\n",stderr);
          return data();
        } catch(...) {
          HEIST_PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
               "\n     Triggered By: " << abstract_syntax_tree[i].noexcept_write() << 
               "\n  => Please send your code to jrandleman@scu.edu to fix"
               "\n     the interpreter's bug!"
               "\n  => Terminating Heist Scheme Interpretation.\n");
          return data();
        }
      }
    // Alert improper heist expression
    } catch(const READER_ERROR& read_error) {
      if(is_non_repl_reader_error(read_error)) 
        alert_non_repl_reader_error(stderr,read_error,exp);
      else 
        alert_reader_error(stderr,read_error,exp);
    // Alert user if detected unparsable input (-:- ANOMALY -:-)
    } catch(const size_type& read_error_index) {
      alert_reader_error(stderr,read_error_index,exp);
    }
    fputs("\n",stderr);
    return data();
  }


  // Define C++ Primitive for Heist Scheme
  //  => NOTE: "append_env_to_args" is used by higher-order procedures to apply
  //           heist procedures received as arguments
  void define(const std::string& heist_primitive_name, prm_ptr_t cpp_function, bool append_env_to_args=false) noexcept {
    if(!G.GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(heist_primitive_name, fcn_type(heist_primitive_name,cpp_function));
    if(append_env_to_args)
      GLOBALS::USER_DEFINED_PRIMITIVES_REQUIRING_ENV.push_back(cpp_function);
  }


  // Define Heist Scheme Variable
  void define(const std::string& heist_variable_name, const data& variable_value) noexcept {
    if(!G.GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    G.GLOBAL_ENVIRONMENT_POINTER->define_variable(heist_variable_name, variable_value);
  }


  // Apply Heist Scheme Callable by Value
  data apply(data& heist_procedure, data_vector args) noexcept {
    if(!G.GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    if(!primitive_toolkit::data_is_callable(heist_procedure)) {
      HEIST_PRINT_ERR("Invalid Heist Scheme Callable: " << HEIST_PROFILE(heist_procedure));
      return data();
    }
    try {
      return primitive_toolkit::apply_callable(heist_procedure, std::move(args));
    } catch(const SCM_EXCEPT& eval_throw) {
      if(eval_throw == heist::SCM_EXCEPT::JUMP) {
        HEIST_PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
          << HEIST_PROFILE(heist::GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
      } else if(eval_throw == heist::SCM_EXCEPT::EXIT) {
        if(heist::GLOBALS::HEIST_EXIT_CODE)
          return data(heist::symconst::exit_failure);
        return data(heist::symconst::exit_success);
      }
      return data();
    }
  }

  // Apply Heist Scheme Callable by Name
  data apply(const std::string& heist_procedure_name, data_vector args) noexcept {
    if(!G.GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    try {
      auto val = lookup_variable_value(heist_procedure_name,G.GLOBAL_ENVIRONMENT_POINTER);
      return primitive_toolkit::apply_callable(val, std::move(args));
    } catch(const SCM_EXCEPT& eval_throw) {
      if(eval_throw == heist::SCM_EXCEPT::JUMP) {
        HEIST_PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
          << HEIST_PROFILE(heist::GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
      } else if(eval_throw == heist::SCM_EXCEPT::EXIT) {
        if(heist::GLOBALS::HEIST_EXIT_CODE)
          return data(heist::symconst::exit_failure);
        return data(heist::symconst::exit_success);
      }
      return data();
    }
  }
} // End of namespace heist

// Heist Scheme expression literal
heist::data operator"" _heist(const char* exp, std::size_t){return heist::eval(exp);}

#undef afmt
#undef ERR_HEADER
#undef BAD_SYNTAX
#undef HEIST_EXP_ERR
#undef HEIST_FCN_ERR
#undef HEIST_PROFILE
#undef HEIST_PRINT_ERR
#undef HEIST_THROW_ERR
#endif