// Author: Jordan Randleman -- jordanran199@gmail.com -- universes.hpp
// => Defines an internally-used specialized "eval" procedure for "universe" 
//    objects of the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_UNIVERSES_HPP_
#define HEIST_SCHEME_CORE_STDLIB_UNIVERSES_HPP_

#include "implementation.hpp"

/***
 * NOTE:
 *   Similar to "coroutine"s, universes as constructs are actually entirely defined 
 *   directly in Heist Scheme, and as such, the interpreter only ever sees them as just 
 *   being "any other object".
 *
 *   Unlike coroutines, however, the isolation of run-time execution environments for
 *   universes requires internal support for the swapping of "process invariants". 
 *   => These process invariants include items like the global environment pointer,
 *      whether or not we should use ANSI escape-code colors in our error messages,
 *      the current "dot" value, which values are "falsey", etc.
 *      -> See "lib/core/type_system/scheme_types/process.hpp" for a list of these vars!
 *
 *   With this in mind, we define a C++ primitive that has access to said invariants in 
 *   order to expose a specialized version of "eval" that, in addition to evaluating data
 *   as code, also provides the infrastructure to switch between process invariants.
 *
 */

namespace heist {

  data primitive_HEIST_CORE_UNIVERSE_EVAL(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:universe:eval <datum> <universe-object>)";
    if(args.size() != 2)
      HEIST_THROW_ERR("'heist:core:universe:eval not given 2 args!"
        << format << HEIST_FCN_ERR("heist:core:universe:eval",args));
    if(!args[1].is_type(types::obj) || args[1].obj->proto != stdlib_universes::get_universe_class_prototype(args,format))
      HEIST_THROW_ERR("'heist:core:universe:eval 2nd arg " << HEIST_PROFILE(args[1])
        << " isn't a <universe> object!" << format << HEIST_FCN_ERR("heist:core:universe:eval",args));
    for(size_type i = 0, n = args[1].obj->member_names.size(); i < n; ++i) {
      if(args[1].obj->member_names[i] == "universe:private:env") {
        // Create new environment as needed
        if(!args[1].obj->member_values[i].is_type(types::prc)) {
          auto old_invariants = stdlib_sysinterface::reset_process_invariant_state();
          args[1].obj->member_values[i] = prc_type(G);
          stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
        }
        // Save current universe & set to the given universe
        auto old_invariants = std::move(G);
        G = std::move(*args[1].obj->member_values[i].prc);
        try {
          // eval in new universe
          data_vector eval_args(3);
          eval_args[0] = args[0];
          eval_args[1] = symconst::global_env;
          eval_args[2] = G.GLOBAL_ENVIRONMENT_POINTER; // ignored
          auto result = primitive_EVAL(std::move(eval_args));
          // reset to old universe
          *args[1].obj->member_values[i].prc = std::move(G);
          G = std::move(old_invariants);
          return result;
        } catch(const SCM_EXCEPT& eval_throw) {
          // reset to old universe
          *args[1].obj->member_values[i].prc = std::move(G);
          G = std::move(old_invariants);
          // sandboxed exit
          if(eval_throw == SCM_EXCEPT::EXIT) 
            return num_type(GLOBALS::HEIST_EXIT_CODE);
          // display where error occurred
          fprintf(stderr, "\n  %s>> Universe Exception:%s\n     Universe: \"%s\"\n     Expression: %s\n%s", 
            HEIST_AFMT(AFMT_135), HEIST_AFMT(AFMT_01), args[1].obj->member_values[i].noexcept_write().c_str(), 
            args[0].noexcept_write().c_str(), HEIST_AFMT(AFMT_0));
          throw eval_throw;
        }
      }
    }
    // The below is never triggered if user doesn't alter source-code/access private variables
    HEIST_THROW_ERR("'heist:core:universe:eval <universe> object " << HEIST_PROFILE(args[1]) 
      << " missing its \"universe:private:env\" member!"
      << format << HEIST_FCN_ERR("heist:core:universe:eval",args));
    return data(); // NEVER TRIGGERED
  }

} // End of namespace heist

#endif