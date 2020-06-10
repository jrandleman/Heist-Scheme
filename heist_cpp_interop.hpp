// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_cpp_interop.hpp
// => Wrapper around Hiest Interpreter to set up C++ interop

#ifndef HEIST_CPP_INTEROP_HPP_
#define HEIST_CPP_INTEROP_HPP_

// Defines 4 Functions for C++ Interop w/ Heist:
//   0) eval   // evaluate heist code string, same as _lisp literal (see below)
//   1) apply  // apply args to Heist procedure
//   2) defun  // define C++ Heist primitive (use "no_args_given" to check for 0 args)
//   3) defvar // define a global Heist variable

#include "heist_main.cpp"

namespace heist_scm {
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
            return scm_eval(scm_list_cast(abstract_syntax_tree[i]),GLOBAL_ENVIRONMENT_POINTER);
          scm_eval(scm_list_cast(abstract_syntax_tree[i]),GLOBAL_ENVIRONMENT_POINTER);
        } catch(const SCM_EXCEPT& eval_throw) {
          fputs("\n",stderr);
          return data();
        } catch(...) {
          PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
               "\n     Triggered By: " << abstract_syntax_tree[i].cpp_str() << 
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


  // Define Heist Scheme Primitive
  void defun(const std::string& heist_primitive_name, prm_type cpp_function, bool append_env_to_args=false) noexcept {
    if(!GLOBAL_ENVIRONMENT_POINTER) set_default_global_environment();
    scm_list primitive_procedures(3);
    primitive_procedures[0] = symconst::primitive;
    primitive_procedures[1] = cpp_function;
    primitive_procedures[2] = heist_primitive_name;
    define_variable(heist_primitive_name, primitive_procedures, GLOBAL_ENVIRONMENT_POINTER);
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
    try {
      return data_cast(execute_application(
              scm_list_cast(lookup_variable_value(heist_procedure_name,GLOBAL_ENVIRONMENT_POINTER)),
              args, GLOBAL_ENVIRONMENT_POINTER));
    } catch(const SCM_EXCEPT& eval_throw) {
      return data();
    }
  }
} // End of namespace heist_scm

// Lisp expression literal
heist_scm::data operator"" _lisp(const char* exp, std::size_t s){return heist_scm::eval(exp);}

#undef ERR_HEADER
#undef BAD_SYNTAX
#undef EXP_ERR
#undef FCN_ERR
#undef PROFILE
#undef PRINT_ERR
#undef THROW_ERR
#endif