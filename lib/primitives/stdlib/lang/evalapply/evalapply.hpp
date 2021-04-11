// Author: Jordan Randleman -- jordanran199@gmail.com -- evalapply.hpp
// => Defines the primitive eval & apply functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_EVALAPPLY_HPP_
#define HEIST_SCHEME_CORE_STDLIB_EVALAPPLY_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * EVAL PRIMITIVE
  ******************************************************************************/

  // primitive "eval" procedure:
  data primitive_EVAL(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (eval <data> <optional-environment>)" 
      "\n     -> Pass *null-environment* to eval in the empty environment!"
      "\n     -> Pass *local-environment* to eval in the local environment (default)!"
      "\n     -> Pass *global-environment* to eval in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    // use the initial/local environment if passed *null-environment* or
    //   *local-environment* as a 2nd arg
    bool must_reset_global_env = false;
    auto env = local_env;
    stdlib_eval_apply::eval_confirm_correct_number_of_args(args,must_reset_global_env,env,"eval",format);

    // Convert data to evaluable syntax
    data data_as_syntax;
    if(!primitive_toolkit::convert_data_to_evaluable_syntax(args[0],data_as_syntax))
      HEIST_THROW_ERR("'eval didn't receive an evaluable expression:\n     "
        << HEIST_PROFILE(args[0]) << format << HEIST_FCN_ERR("eval", args));

    // If arg is self-evaluating, return arg
    if(!data_as_syntax.is_type(types::sym) && !data_as_syntax.is_type(types::exp)) 
      return data_as_syntax;

    // Eval the symbol or expression
    // if using *local-environment* or *global-environment*
    if(!must_reset_global_env) {
      return scm_eval(std::move(data_as_syntax),env);
    // if using *null-environment*
    } else {
      auto old_invariants = stdlib_sysinterface::reset_process_invariant_state();
      try {
        auto result = scm_eval(std::move(data_as_syntax),G.GLOBAL_ENVIRONMENT_POINTER);
        stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
        return result;
      } catch(const SCM_EXCEPT& eval_throw) {
        stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
        if(eval_throw == SCM_EXCEPT::EXIT) 
          return num_type(GLOBALS::HEIST_EXIT_CODE);
        throw eval_throw;
      }
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * CPS-EVAL PRIMITIVE
  ******************************************************************************/

  // primitive "cps-eval" procedure:
  data primitive_CPS_EVAL(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (cps-eval <data> <optional-environment> <continuation>)" 
      "\n     -> Pass *null-environment* to cps-eval in the empty environment!"
      "\n     -> Pass *local-environment* to cps-eval in the local environment (default)!"
      "\n     -> Pass *global-environment* to cps-eval in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      HEIST_THROW_ERR("'cps-eval received incorrect # of arguments:"
        << format << HEIST_FCN_ERR("cps-eval", args));
    // Extract the continuation & confirm its a procedure
    auto continuation = primitive_toolkit::validate_callable_and_convert_to_procedure(*args.rbegin(), args, "cps-eval", format);
    args.pop_back();
    // use the initial/global environment if passed *null-environment* or
    //   *global-environment* as a 2nd arg
    bool must_reset_global_env = false;
    auto env = local_env;
    stdlib_eval_apply::eval_confirm_correct_number_of_args(args,must_reset_global_env,env,"cps-eval",format);

    // Convert data to evaluable syntax
    data data_as_syntax;
    if(!primitive_toolkit::convert_data_to_evaluable_syntax(args[0],data_as_syntax))
      HEIST_THROW_ERR("'cps-eval didn't receive an evaluable expression:\n     "
        << HEIST_PROFILE(args[0]) << format << HEIST_FCN_ERR("cps-eval", args));

    // If arg is self-evaluating, return arg
    if(!data_as_syntax.is_type(types::sym) && !data_as_syntax.is_type(types::exp)) {
      return execute_application(scm_analyze(generate_fundamental_form_cps(data_as_syntax),false,true)(env),data_vector(1,continuation),env,false,true);
    }

    // CPS-eval the symbol or expression
    // if using *local-environment* or *global-environment*
    if(!must_reset_global_env) {
      return execute_application(scm_analyze(generate_fundamental_form_cps(data_as_syntax),false,true)(env),data_vector(1,continuation),env,false,true);
    // if using *null-environment*
    } else {
      auto old_invariants = stdlib_sysinterface::reset_process_invariant_state();
      try {
        auto result = execute_application(scm_analyze(generate_fundamental_form_cps(data_as_syntax),false,true)(G.GLOBAL_ENVIRONMENT_POINTER),
                                          data_vector(1,continuation),G.GLOBAL_ENVIRONMENT_POINTER,false,true);
        stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
        return result;
      } catch(const SCM_EXCEPT& eval_throw) {
        stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
        if(eval_throw == SCM_EXCEPT::EXIT) 
          return num_type(GLOBALS::HEIST_EXIT_CODE);
        throw eval_throw;
      }
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * APPLY PRIMITIVE
  ******************************************************************************/

  // primitive "apply" procedure:
  data primitive_APPLY(data_vector&& args) {
    // get whether in a tail call
    bool tail_call = args.rbegin()->bol.val;
    args.pop_back();
    // get current environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm the correct # of arguments were passed
    static constexpr const char * const format = "\n     (apply <callable> <argument-list>)";
    if(args.size() != 2)
      HEIST_THROW_ERR("'apply received incorrect # of arguments:" << format << HEIST_FCN_ERR("apply",args));
    // confirm 1st arg is a callable
    primitive_toolkit::confirm_data_is_callable(args[0], args, "apply", format);
    // confirm 2nd arg is a finite, nul-terminated list
    if(!primitive_toolkit::data_is_proper_list(args[1]))
      HEIST_THROW_ERR("'apply 2nd arg " << HEIST_PROFILE(args[1]) << " isn't a proper list!"
        << format << HEIST_FCN_ERR("apply",args));
    // apply arguments in list to the callable
    return primitive_toolkit::apply_callable(args[0],primitive_toolkit::convert_proper_list_to_data_vector(args[1]),env,tail_call);
  }

} // End of namespace heist

#endif