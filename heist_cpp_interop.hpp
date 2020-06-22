// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_cpp_interop.hpp
// => Wrapper around Heist Interpreter to set up C++ interop

#ifndef HEIST_CPP_INTEROP_HPP_
#define HEIST_CPP_INTEROP_HPP_

// Defines 4 Functions for C++ Interop w/ Heist:
//   0) eval   // evaluate heist code string, same as _heist literal (see below)
//   1) apply  // apply args to Heist procedure
//   2) defun  // define C++ Heist primitive
//   3) defvar // define a global Heist variable

#include "heist_main.cpp"

namespace heist {
  // Evaluate Heist Scheme Expression in String
  data eval(std::string exp) noexcept {
    if(!GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    scm_list abstract_syntax_tree;
    try {
      // Evaluate AST if successfully parsed an expression
      parse_input_exp(std::move(exp),abstract_syntax_tree);
      for(size_type i = 0, n = abstract_syntax_tree.size(); i < n; ++i) {
        try {
          if(i+1 == n) 
            return data_cast(scm_eval(scm_list_cast(abstract_syntax_tree[i]),GLOBAL_ENVIRONMENT_POINTER));
          scm_eval(scm_list_cast(abstract_syntax_tree[i]),GLOBAL_ENVIRONMENT_POINTER);
        } catch(const SCM_EXCEPT& eval_throw) {
          if(eval_throw == heist::SCM_EXCEPT::JUMP)
            PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
              << PROFILE(heist::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
          fputs("\n",stderr);
          return data();
        } catch(...) {
          PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
               "\n     Triggered By: " << abstract_syntax_tree[i].write() << 
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
  //           heist procedures recieved as arguments
  void defun(const std::string& heist_primitive_name, prm_type cpp_function, bool append_env_to_args=false) noexcept {
    if(!GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    scm_list primitive_procedure(3);
    primitive_procedure[0] = symconst::primitive;
    primitive_procedure[1] = cpp_function;
    primitive_procedure[2] = heist_primitive_name;
    define_variable(heist_primitive_name, primitive_procedure, GLOBAL_ENVIRONMENT_POINTER);
    if(append_env_to_args)
      USER_DEFINED_PRIMITIVES_REQUIRING_ENV.push_back(cpp_function);
  }


  // Define Heist Scheme Variable
  void defvar(const std::string& heist_variable_name, const data& variable_value) noexcept {
    if(!GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    define_variable(heist_variable_name, variable_value, GLOBAL_ENVIRONMENT_POINTER);
  }


  // Apply Heist Scheme Procedure
  data apply(const std::string& heist_procedure_name, scm_list args) noexcept {
    if(!GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    if(args.empty()) args.push_back(symconst::sentinel_arg);
    try {
      return data_cast(execute_application(
              scm_list_cast(lookup_variable_value(heist_procedure_name,GLOBAL_ENVIRONMENT_POINTER)),
              args, GLOBAL_ENVIRONMENT_POINTER));
    } catch(const SCM_EXCEPT& eval_throw) {
      if(eval_throw == heist::SCM_EXCEPT::JUMP)
        PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
          << PROFILE(heist::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
      return data();
    }
  }
} // End of namespace heist

// Heist Scheme expression literal
heist::data operator"" _heist(const char* exp, std::size_t){return heist::eval(exp);}

#undef ERR_HEADER
#undef BAD_SYNTAX
#undef EXP_ERR
#undef FCN_ERR
#undef PROFILE
#undef PRINT_ERR
#undef THROW_ERR
#endif