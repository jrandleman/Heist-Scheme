// Author: Jordan Randleman -- jrandleman@scu.edu -- syntax.hpp
// => Defines the primitive syntax functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_SYNTAX_HPP_
#define HEIST_SCHEME_CORE_STDLIB_SYNTAX_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * SYNTAX PREDICATE PRIMITIVES
  ******************************************************************************/

  data primitive_CORE_SYNTAXP(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::sym))
      HEIST_THROW_ERR("'core-syntax? didn't receive 1 symbolic arg!"
        "\n     (core-syntax? <symbol>)" << HEIST_FCN_ERR("core-syntax?",args));
    for(const auto& core_syntax_label : G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY)
      if(core_syntax_label == args[0].sym)
        return GLOBALS::TRUE_DATA_BOOLEAN;
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }


  data primitive_RUNTIME_SYNTAXP(data_vector&& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 1 || !args[0].is_type(types::sym))
      HEIST_THROW_ERR("'runtime-syntax? didn't receive 1 symbolic arg!"
        "\n     (runtime-syntax? <symbol>)" << HEIST_FCN_ERR("runtime-syntax?",args));
    // Confirm not core-syntax
    for(const auto& core_syntax_label : G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY)
      if(core_syntax_label == args[0].sym)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    // Search for macro in the environment
    return boolean(env->has_macro(args[0].sym));
  }


  data primitive_READER_SYNTAXP(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'reader-syntax? didn't receive 1 string arg!"
        "\n     => Must be a string to avoid expansion by the reader if IS syntax!"
        "\n     (reader-syntax? <string>)" << HEIST_FCN_ERR("reader-syntax?",args));
    auto& sought_shorthand = *args[0].str;
    for(const auto& reader_syntax_label : G.SHORTHAND_READER_MACRO_REGISTRY)
      if(reader_syntax_label == sought_shorthand)
        return GLOBALS::TRUE_DATA_BOOLEAN;
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }


  data primitive_READER_ALIASP(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'reader-alias? didn't receive 1 string arg!"
        "\n     => Must be a string to avoid expansion by the reader if IS syntax!"
        "\n     (reader-alias? <string>)" << HEIST_FCN_ERR("reader-alias?",args));
    auto& sought_shorthand = *args[0].str;
    for(const auto& reader_syntax_label : G.SHORTHAND_READER_ALIAS_REGISTRY)
      if(reader_syntax_label == sought_shorthand)
        return GLOBALS::TRUE_DATA_BOOLEAN;
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * SYNTAX DELETION PRIMITIVES
  ******************************************************************************/

  data primitive_DELETE_CORE_SYNTAX_BANG(data_vector&& args) {
    if(args.empty()) return GLOBALS::VOID_DATA_OBJECT;
    stdlib_syntax::confirm_only_symbol_args(args,"delete-core-syntax!","\n     (delete-core-syntax! <macro-name-symbol> ...)");
    stdlib_syntax::erase_all_macros_from_core_registry(args);
    return boolean(stdlib_syntax::erase_all_macro_labels_from_env(args,G.GLOBAL_ENVIRONMENT_POINTER));
  }

  data primitive_DELETE_RUNTIME_SYNTAX_BANG(data_vector&& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    stdlib_syntax::confirm_only_symbol_args(args,"delete-runtime-syntax!","\n     (delete-runtime-syntax! <macro-name-symbol> ...)");
    stdlib_syntax::remove_args_referencing_core_syntax(args);
    if(args.empty()) return GLOBALS::VOID_DATA_OBJECT;
    return boolean(stdlib_syntax::erase_all_macro_labels_from_env(args,env));
  }

  /******************************************************************************
  * READER MACRO/ALIAS DEFINITION & ANALYSIS PRIMITIVE
  ******************************************************************************/

  data primitive_DEFINE_READER_SYNTAX(data_vector&& args) {
    if(args.empty() || args.size() > 2 || !args[0].is_type(types::str) || 
      (args.size() == 2 && !args[1].is_type(types::str)))
      HEIST_THROW_ERR("'define-reader-syntax improper arg signature!"
        "\n     (define-reader-syntax <shorthand-string> <optional-longhand-string>)"
        << HEIST_FCN_ERR("define-reader-syntax",args));
    // Delete Reader Macro
    if(args.size() == 1) return stdlib_syntax::delete_reader_macro(*args[0].str);
    // Confirm not shorthand isn't ":" (messes w/ internal reserved symbols)
    if(*args[0].str == ":")
      HEIST_THROW_ERR("'define-reader-syntax \":\" is an invalid reader-syntax shorthand!"
        "\n     (define-reader-syntax <shorthand-string> <optional-longhand-string>)"
        "\n     => Defining \":\" as reader-syntax messes with internal reserved symbols!"
        << HEIST_FCN_ERR("define-reader-syntax",args));
    // Define/Redefine Reader Macro
    stdlib_syntax::register_reader_macro(*args[0].str,*args[1].str);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  data primitive_READER_SYNTAX_LIST(data_vector&& args) {
    stdlib_syntax::confirm_no_args_given(args,"reader-syntax-list");
    data_vector pairs;
    for(size_type i = 0, n = G.SHORTHAND_READER_MACRO_REGISTRY.size(); i < n; ++i) {
      pairs.push_back(make_par());
      pairs[i].par->first = make_str(G.SHORTHAND_READER_MACRO_REGISTRY[i]);
      pairs[i].par->second = make_str(G.LONGHAND_READER_MACRO_REGISTRY[i]);
    }
    return primitive_toolkit::convert_data_vector_to_proper_list(pairs.begin(),pairs.end());
  }

  data primitive_READER_ALIAS_LIST(data_vector&& args) {
    stdlib_syntax::confirm_no_args_given(args,"reader-alias-list");
    data_vector pairs;
    for(size_type i = 0, n = G.SHORTHAND_READER_ALIAS_REGISTRY.size(); i < n; ++i) {
      pairs.push_back(make_par());
      pairs[i].par->first = make_str(G.SHORTHAND_READER_ALIAS_REGISTRY[i]);
      pairs[i].par->second = make_str(G.LONGHAND_READER_ALIAS_REGISTRY[i]);
    }
    return primitive_toolkit::convert_data_vector_to_proper_list(pairs.begin(),pairs.end());
  }

  /******************************************************************************
  * INFIX TABLE ALIST
  ******************************************************************************/

  data primitive_INFIX_LIST(data_vector&& args) {
    stdlib_syntax::confirm_no_args_given(args,"infix-list");
    return stdlib_syntax::get_infix_list();
  }

} // End of namespace heist

#endif