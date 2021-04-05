// Author: Jordan Randleman -- jrandleman@scu.edu -- controlflow.hpp
// => Defines the primitive control flow functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_CONTROLFLOW_HPP_
#define HEIST_SCHEME_CORE_STDLIB_CONTROLFLOW_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * EXIT
  ******************************************************************************/

  data primitive_EXIT(data_vector&& args) {
    static constexpr const auto MAX_INT = std::numeric_limits<int>::max();
    static constexpr const auto MIN_INT = std::numeric_limits<int>::min();
    if(args.size() > 1)
      HEIST_THROW_ERR("'exit received more than 1 argument!"
        "\n     (exit <optional-integer-exit-code>)"
        "\n     => <optional-integer-exit-code> bounds: [" 
        << MIN_INT << ", " << MAX_INT <<']' << HEIST_FCN_ERR("exit",args));
    if(args.empty()) throw SCM_EXCEPT::EXIT;
    if(!args[0].is_type(types::num) || !args[0].num.is_integer())
      HEIST_THROW_ERR("'exit didn't receive an integer argument!"
        "\n     (exit <optional-integer-exit-code>)"
        "\n     => <optional-integer-exit-code> bounds: [" 
        << MIN_INT << ", " << MAX_INT <<']' << HEIST_FCN_ERR("exit",args));
    if(args[0].num > MAX_INT || args[0].num < MIN_INT)
      HEIST_THROW_ERR("'exit integer argument doesn't fall w/in proper bounds!"
        "\n     (exit <optional-integer-exit-code>)"
        "\n     => <optional-integer-exit-code> bounds: [" 
        << MIN_INT << ", " << MAX_INT <<']' << HEIST_FCN_ERR("exit",args));
    GLOBALS::HEIST_EXIT_CODE = (int)args[0].num.extract_inexact();
    throw SCM_EXCEPT::EXIT;
    return data();
  }

  /******************************************************************************
  * ERRORS
  ******************************************************************************/

  data primitive_ERROR(data_vector&& args) {
    stdlib_controlflow::generic_error(args,"error","Exception",AFMT_131);
    return data();
  }

  data primitive_SYNTAX_ERROR(data_vector&& args) {
    stdlib_controlflow::generic_error(args,"syntax-error","Invalid Syntax",AFMT_135);
    return data();
  }

  /******************************************************************************
  * LEXICAL/DYNAMIC SCOPE CHECKING/TOGGLING
  ******************************************************************************/

  data primitive_COERCE_LEXICAL_SCOPE_TO_DYNAMIC_SCOPE(data_vector&& args) {
    return stdlib_controlflow::convert_callable_scope(args, true, "lexical-scope->dynamic-scope", "\n     (lexical-scope->dynamic-scope <callable>)");
  }

  data primitive_COERCE_DYNAMIC_SCOPE_TO_LEXICAL_SCOPE(data_vector&& args) {
    return stdlib_controlflow::convert_callable_scope(args, false, "dynamic-scope->lexical-scope", "\n     (dynamic-scope->lexical-scope <callable>)");
  }

  data primitive_DYNAMIC_SCOPEP(data_vector&& args) {
    return stdlib_controlflow::check_callable_scope(args,true,"dynamic-scope?","\n     (dynamic-scope? <callable>)");
  }

  data primitive_LEXICAL_SCOPEP(data_vector&& args) {
    return stdlib_controlflow::check_callable_scope(args,false,"lexical-scope?","\n     (lexical-scope? <callable>)");
  }

  /******************************************************************************
  * JUMPING
  ******************************************************************************/

  data primitive_JUMP_BANG(data_vector&& args) {
    if(args.size() > 1)
      HEIST_THROW_ERR("'jump! received incorrect # of args!"
        "\n     (jump! <optional-arg>)" << HEIST_FCN_ERR("jump!",args));
    if(args.size() == 1)
      GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT = args[0];
    else
      GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT = GLOBALS::VOID_DATA_OBJECT;
    throw SCM_EXCEPT::JUMP;
    return data();
  }

  data primitive_CATCH_JUMP(data_vector&& args) {
    if(args.empty())
      HEIST_THROW_ERR("'catch-jump received incorrect # of args!"
        "\n     (catch-jump <callable> <arg1> ... <argN>)" << HEIST_FCN_ERR("catch-jump",args));
    primitive_toolkit::confirm_data_is_callable(args[0], args, "catch-jump", "\n     (catch-jump <callable> <arg1> ... <argN>)");
    try {
      return primitive_toolkit::apply_callable(args[0],data_vector(args.begin()+1,args.end()));
    } catch(const SCM_EXCEPT& jump_error) {
      if(jump_error == SCM_EXCEPT::JUMP)
        return GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT;
      throw jump_error;
    }
    return data();
  }

  /******************************************************************************
  * MACRO EXPANSION
  ******************************************************************************/

  // Returns quoted list of data macro-expanded (returns data as-is if not a macro):
  data primitive_EXPAND(data_vector&& args) {
    // Extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 1)
      HEIST_THROW_ERR("'expand expects 1 arg: (expand <quoted-macro-exp>)"
        "\n     (expand <quoted-macro-exp>)" << HEIST_FCN_ERR("expand",args));
    // Atomics can't be macro applications
    if(!args[0].is_type(types::par)) return args[0];
    // Expand Macros as needed
    return stdlib_controlflow::recursively_deep_expand_datum_macros(args[0],env,false);
  }

  // Returns quoted list of data core-syntax-expanded (returns data as-is if not a macro):
  data primitive_CORE_EXPAND(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'core-expand expects 1 arg: (core-expand <quoted-macro-exp>)"
        "\n     (core-expand <quoted-macro-exp>)" << HEIST_FCN_ERR("core-expand",args));
    // Atomics can't be macro applications
    if(!args[0].is_type(types::par)) return args[0];
    // Expand Core Macros as needed
    return stdlib_controlflow::recursively_deep_expand_datum_macros(args[0],G.GLOBAL_ENVIRONMENT_POINTER,true);
  }

  /******************************************************************************
  * FUNCTION TRACING
  ******************************************************************************/

  // Invoke <proc> w/ args & trace the application (esp. helpful to trace recursion)
  // NOTE: The procedure MUST be a NAMED procedure (no anonymous lambda tracing support)!
  data primitive_TRACE(data_vector&& args) {
    if(args.empty())
      HEIST_THROW_ERR("'trace received incorrect # of args!"
        "\n     (trace <procedure> <arg1> ... <argN>)" << HEIST_FCN_ERR("trace",args));
    if(!args[0].is_type(types::fcn))
      HEIST_THROW_ERR("'trace 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a procedure!"
        "\n     (trace <procedure> <arg1> ... <argN>)" << HEIST_FCN_ERR("trace",args));
    // Set name of the function to trace
    G.TRACED_FUNCTION_NAME = args[0].fcn.name;
    auto result = execute_application(args[0],data_vector(args.begin()+1,args.end()));
    G.TRACED_FUNCTION_NAME = "";
    return result;
  }

  /******************************************************************************
  * INTERNAL CPS APPLICATOR
  ******************************************************************************/

  /***
   * NOTE:
   *   The below is designed to be used when applying a procedure that expects a continuation
   *   in a non-CPS context. By default, the Heist Scheme interpreter will automagically
   *   add in "id" as the continuation to CPS-procedures applied in non-CPS contexts. This is
   *   in order to have seamless flow between CPS & non-CPS Heist Scheme code.
   *
   *   However, sometimes we wish to explicitly pass in a continuation value to a CPS-procedure
   *   in a non-CPS context. Examples of this include the implementations of "call/cc" and 
   *   coroutines in Heist Scheme directly.
   *
   *   Enter "heist:core:apply-with-continuation": intended to be used by libraries (hence 
   *   its use of the reserved "heist:" symbolic prefix), such enables the application of a 
   *   CPS-procedure in a non-CPS context WITHOUT the automagic addition of "id" as its continuation. 
   *   Rather, the last argument in <data_vector& args> is expected to be the application's continuation.
   *
   */

  // Designed for use with "heist:core:pass-continuation-" procedures!
  data primitive_HEIST_CORE_APPLY_WITH_CONTINUATION(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:apply-with-continuation <callable> <optional-arg> ... <continuation-callable>)";
    if(args.size() < 2)
      HEIST_THROW_ERR("'heist:core:apply-with-continuation didn't enough args!"
        << format << HEIST_FCN_ERR("heist:core:apply-with-continuation", args));
    // confirm given callable to apply, & a callable continuation
    primitive_toolkit::confirm_data_is_callable(args[0], args, "heist:core:apply-with-continuation", format);
    primitive_toolkit::confirm_data_is_callable(*args.rbegin(), args,"heist:core:apply-with-continuation",format);
    return primitive_toolkit::apply_callable_with_continuation(args[0],data_vector(args.begin()+1,args.end()));
  }

} // End of namespace heist

#endif