// Author: Jordan Randleman -- jordanran199@gmail.com -- sysinterface.hpp
// => Defines the primitive system interface functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_SYSINTERFACE_HPP_
#define HEIST_SCHEME_CORE_STDLIB_SYSINTERFACE_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * LOAD
  ******************************************************************************/

  // Load a script into the global environment
  data primitive_LOAD(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (load <filename-string> <optional-environment>)"
      "\n     -> Pass *null-environment* to load in the empty environment!"
      "\n     -> Pass *local-environment* to load in the local environment (default)!"
      "\n     -> Pass *global-environment* to load in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    // Set *local-environment* evaluation as default
    if(args.size() == 1) args.push_back(symconst::local_env);
    // determine which environment to load <filename-string> wrt to
    auto env = G.GLOBAL_ENVIRONMENT_POINTER;
    if(args.size()==2 && args[1].is_type(types::sym)) {
      if(args[1].sym == symconst::null_env) {
        // Reset interpreter invariants to their default states
        auto old_invariants = stdlib_sysinterface::reset_process_invariant_state();
        args.pop_back();
        try {
          stdlib_sysinterface::interpret_file_contents(args,G.GLOBAL_ENVIRONMENT_POINTER,format);
        } catch(const SCM_EXCEPT& eval_throw) {
          stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
          if(eval_throw == SCM_EXCEPT::EXIT) 
            return num_type(GLOBALS::HEIST_EXIT_CODE);
          throw eval_throw;
        }
        // Reset interpreter invariants to their previous bindings
        stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
        return GLOBALS::VOID_DATA_OBJECT;
      } else if(args[1].sym == symconst::local_env) {
        env = local_env, args.pop_back();
      } else if(args[1].sym == symconst::global_env) {
        args.pop_back(); // *global-environment* is default
      } else {
        HEIST_THROW_ERR("'load \""<<args[1].sym<<"\" isn't an evaluation environment:"
          << format << HEIST_FCN_ERR("load", args));
      }
    }
    stdlib_sysinterface::interpret_file_contents(args,env,format);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * CPS-LOAD
  ******************************************************************************/

  // Load a script into the global environment, convert it to CPS, and pass it to the given continuation
  data primitive_CPS_LOAD(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (cps-load <filename-string> <optional-environment> <continuation>)"
      "\n     -> Pass *null-environment* to cps-load in the empty environment!"
      "\n     -> Pass *local-environment* to cps-load in the local environment (default)!"
      "\n     -> Pass *global-environment* to cps-load in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    if(args.size() < 2)
      HEIST_THROW_ERR("'cps-load received incorrect # of args!" << format << HEIST_FCN_ERR("cps-load",args));
    // extract the continuation
    auto continuation = primitive_toolkit::validate_callable_and_convert_to_procedure(*args.rbegin(), args, "cps-load", format);
    args.pop_back();
    // determine which environment to load <filename-string> wrt to
    auto env = local_env;
    if(args.size()==2 && args[1].is_type(types::sym)) {
      if(args[1].sym == symconst::null_env) {
        // Reset interpreter invariants to their default states
        auto old_invariants = stdlib_sysinterface::reset_process_invariant_state();
        args.pop_back();
        try {
          // pass the continuation to the loaded file
          auto result = execute_application(
            stdlib_sysinterface::cps_interpret_file_contents(args,G.GLOBAL_ENVIRONMENT_POINTER,format),
            stdlib_sysinterface::generate_CPS_LOAD_args(continuation),G.GLOBAL_ENVIRONMENT_POINTER,false,true);
          // Reset interpreter invariants to their previous states
          stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
          return result;
        } catch(const SCM_EXCEPT& eval_throw) {
          stdlib_sysinterface::set_process_invariant_state(std::move(old_invariants));
          if(eval_throw == SCM_EXCEPT::EXIT) 
            return num_type(GLOBALS::HEIST_EXIT_CODE);
          throw eval_throw;
        }
      } else if(args[1].sym == symconst::global_env) {
        env = G.GLOBAL_ENVIRONMENT_POINTER; args.pop_back();
      } else if(args[1].sym == symconst::local_env) {
        args.pop_back(); // *local-environment* is default
      } else {
        HEIST_THROW_ERR("'cps-load \""<<args[1].sym<<"\" isn't an evaluation environment:"
          << format << HEIST_FCN_ERR("cps-load", args));
      }
    }
    // pass the continuation to the loaded file
    return execute_application(stdlib_sysinterface::cps_interpret_file_contents(args,env,format),
                               stdlib_sysinterface::generate_CPS_LOAD_args(continuation),env,false,true);
  }

  /******************************************************************************
  * COMPILATION
  ******************************************************************************/

  // Compiles a given filename's file's Heist-Scheme code into a C++ File
  data primitive_COMPILE(data_vector&& args){
    return stdlib_sysinterface::generic_compilation(args,"compile","\n     (compile <filename-string> <optional-compiled-filename>)",false);
  }

  // Compiles a given file w/ a ((scm->cps <file-contents>) id) wrapper
  data primitive_CPS_COMPILE(data_vector&& args){
    return stdlib_sysinterface::generic_compilation(args,"cps-compile","\n     (cps-compile <filename-string> <optional-compiled-filename>)",true);
  }

  /******************************************************************************
  * SYSTEM
  ******************************************************************************/

  // Make a system call, returns #f if can't use 'system, 
  //   and the call's success status if can use the system
  data primitive_SYSTEM(data_vector&& args) {
    if(args.size() > 1)
      HEIST_THROW_ERR("'system received incorrect # of args!"
        "\n     (system <optional-system-call-string>)"<<HEIST_FCN_ERR("system",args));
    if(!args.empty() && !args[0].is_type(types::str))
      HEIST_THROW_ERR("'system "<<HEIST_PROFILE(args[0])<<" isn't a string!"
        "\n     (system <optional-system-call-string>)"<<HEIST_FCN_ERR("system",args));
    // return false if CAN'T use 'system'
    if(!std::system(nullptr)) return GLOBALS::FALSE_DATA_BOOLEAN;
    // return true if just checking whether may use the system (no args given)
    if(args.empty()) return GLOBALS::TRUE_DATA_BOOLEAN;
    return num_type(std::system(args[0].str->c_str()));
  }

  /******************************************************************************
  * GETENV
  ******************************************************************************/

  // Given a string of an environment var name, returns a string of that var's value
  data primitive_GETENV(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'getenv didn't receive exactly 1 arg!"
        "\n     (getenv <environment-variable-name-string>)" << HEIST_FCN_ERR("getenv",args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR("'getenv "<<HEIST_PROFILE(args[0])<<" isn't a string!"
        "\n     (getenv <environment-variable-name-string>)"<<HEIST_FCN_ERR("getenv",args));
    char* result = getenv(args[0].str->c_str());
    if(result) return make_str(result);
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * LIST OF POSSIBLE COMMAND-LINE ARGS
  ******************************************************************************/

  // Returns a string of Heist Scheme's command-line args & their descriptions
  data primitive_COMMAND_LINE(data_vector&& args) {
    if(!args.empty())
      HEIST_THROW_ERR("'command-line doesn't expect any args!"
        "\n     (command-line)" << HEIST_FCN_ERR("command-line",args));
    return make_str(HEIST_COMMAND_LINE_ARGS);
  }

  /******************************************************************************
  * CURRENT TIME/DATE PRIMITIVES
  ******************************************************************************/

  data primitive_MILLISECONDS_SINCE_EPOCH(data_vector&& args) {
    if(!args.empty())
      HEIST_THROW_ERR("'ms-since-epoch doesn't expect any args!"
        "\n     (ms-since-epoch)" << HEIST_FCN_ERR("ms-since-epoch",args));
    return num_type(std::chrono::duration_cast<std::chrono::milliseconds>(
                    std::chrono::system_clock::now().time_since_epoch()).count());
  }

  data primitive_TIME(data_vector&& args) {
    if(args.empty())
      HEIST_THROW_ERR("'time received incorrect # of args!"
        "\n     (time <callable> <arg1> ... <argN>)" << HEIST_FCN_ERR("time",args));
    primitive_toolkit::confirm_data_is_callable(args[0], args, "time", "\n     (time <callable> <arg1> ... <argN>)");
    auto start = std::chrono::high_resolution_clock::now();
    auto result = primitive_toolkit::apply_callable(args[0],data_vector(args.begin()+1,args.end()));
    auto end = std::chrono::high_resolution_clock::now();
    // return a pair: (cons <time> <result>)
    data p = make_par();
    p.par->first = num_type(stdlib_sysinterface::convert_us_to_s(std::chrono::duration_cast<std::chrono::microseconds>(end-start).count()));
    p.par->second = std::move(result);
    return p;
  }

  data primitive_CURRENT_DATE(data_vector&& args) {
    if(args.empty()) return make_str(stdlib_sysinterface::get_current_time_stamp());
    long long s=0, m=0, h=0, d=0, y=0;
    stdlib_sysinterface::parse_current_date_offsets(args,s,m,h,d,y);
    return make_str(stdlib_sysinterface::get_current_time_stamp(s,m,h,d,y));
  }

} // End of namespace heist

#endif