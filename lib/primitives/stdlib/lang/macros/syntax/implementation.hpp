// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for syntax.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_SYNTAX_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_SYNTAX_IMPLEMENTATION_HPP_

namespace heist::stdlib_syntax {

  /******************************************************************************
  * VALIDATION
  ******************************************************************************/

  void confirm_no_args_given(const data_vector& args, const char* name) {
    if(!args.empty())
      HEIST_THROW_ERR('\''<<name<<" doesn't accept any args!\n     ("<<name<<')'<<HEIST_FCN_ERR(name,args));
  }

  /******************************************************************************
  * SYNTAX DELETION
  ******************************************************************************/

  void confirm_only_symbol_args(const data_vector& args, const char* name, const char* format) {
    for(size_type i = 0, n = args.size(); i < n; ++i)
      if(!args[i].is_type(types::sym))
        HEIST_THROW_ERR('\'' << name << " arg #" << i+1 << ' ' << HEIST_PROFILE(args[i]) << " isn't a symbol!"
          << format << HEIST_FCN_ERR(name,args));
  }


  // PRECONDITION: !args.empty() && confirm_only_symbol_args(args,name,format)
  bool erase_all_macro_labels_from_env(const data_vector& args, env_type& env)noexcept{
    bool deleted_all_macros = true;
    for(const auto& arg : args)
      deleted_all_macros = deleted_all_macros && env->erase_macro(arg.sym);
    return deleted_all_macros;
  }


  // PRECONDITION: !args.empty() && confirm_only_symbol_args(args,name,format)
  void erase_all_macros_from_core_registry(const data_vector& args)noexcept{
    for(const auto& arg : args) {
      bool found_in_core_registry = false;
      for(size_type i = 0; i < G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.size(); ++i) {
        if(G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY[i] == arg.sym) {
          G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.erase(G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.begin()+i);
          found_in_core_registry = true;
          break;
        }
      }
      if(found_in_core_registry) {
        for(size_type i = 0; i < G.MACRO_LABEL_REGISTRY.size(); ++i) {
          if(G.MACRO_LABEL_REGISTRY[i] == arg.sym) {
            G.MACRO_LABEL_REGISTRY.erase(G.MACRO_LABEL_REGISTRY.begin()+i);
            break;
          }
        }
      }
    }
  }


  // PRECONDITION: !args.empty() && confirm_only_symbol_args(args,name,format)
  void remove_args_referencing_core_syntax(data_vector& args)noexcept{
    for(size_type i = 0; i < args.size();) {
      if(std::find(G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.begin(),
                   G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.end(),
                   args[i].sym) != G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.end()) {
        args.erase(args.begin()+i);
      } else {
        ++i;
      }
    }
  }

  /******************************************************************************
  * READER MACRO DEFINITION / DELETION
  ******************************************************************************/

  data delete_reader_macro_OR_alias(const string& shorthand, 
                                    str_vector& shorthand_registry, 
                                    str_vector& longhand_registry)noexcept{
    for(auto short_iter = shorthand_registry.begin(), long_iter = longhand_registry.begin();
      short_iter != shorthand_registry.end();
      ++short_iter, ++long_iter) {
      // Rm shorthand/longhand if found
      if(shorthand == *short_iter) {
        shorthand_registry.erase(short_iter);
        longhand_registry.erase(long_iter);
        return GLOBALS::TRUE_DATA_BOOLEAN;
      }
    }
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }


  void register_reader_macro_OR_alias(const string& shorthand, const string& longhand, 
                                      str_vector& shorthand_registry, 
                                      str_vector& longhand_registry)noexcept{
    const auto shorthand_len = shorthand.size();
    for(auto short_iter = shorthand_registry.begin(), long_iter = longhand_registry.begin();
      short_iter != shorthand_registry.end();
      ++short_iter, ++long_iter) {
      // Change longhand associated w/ existing shorthand
      if(shorthand == *short_iter) {
        *long_iter = longhand;
        return;
      // Register new reader macro/alias
      } else if(shorthand > *short_iter && shorthand_len >= short_iter->size()) {
        shorthand_registry.insert(short_iter,shorthand);
        longhand_registry.insert(long_iter,longhand);
        return;
      }
    }
    // Register new reader macro/alias
    shorthand_registry.push_back(shorthand);
    longhand_registry.push_back(longhand);
  }


  data delete_reader_macro(const string& shorthand)noexcept{
    return delete_reader_macro_OR_alias(shorthand,G.SHORTHAND_READER_MACRO_REGISTRY,G.LONGHAND_READER_MACRO_REGISTRY);
  }

  void register_reader_macro(const string& shorthand, const string& longhand)noexcept{
    return register_reader_macro_OR_alias(shorthand,longhand,G.SHORTHAND_READER_MACRO_REGISTRY,G.LONGHAND_READER_MACRO_REGISTRY);
  }


  data delete_reader_alias(const string& shorthand)noexcept{
    return delete_reader_macro_OR_alias(shorthand,G.SHORTHAND_READER_ALIAS_REGISTRY,G.LONGHAND_READER_ALIAS_REGISTRY);
  }

  void register_reader_alias(const string& shorthand, const string& longhand)noexcept{
    return register_reader_macro_OR_alias(shorthand,longhand,G.SHORTHAND_READER_ALIAS_REGISTRY,G.LONGHAND_READER_ALIAS_REGISTRY);
  }

  /******************************************************************************
  * INFIX & INFIXR LISTS
  ******************************************************************************/

  data get_infix_list() {
    data_vector infixes;
    for(const auto& prec_level : G.INFIX_TABLE)
      for(const auto& op : prec_level.second) {
        data inf = make_par();
        inf.par->first = num_type(prec_level.first);
        inf.par->second = make_par();
        inf.par->second.par->first = op.first ? "infix" : "infixr";
        inf.par->second.par->second = make_par();
        inf.par->second.par->second.par->first = op.second;
        inf.par->second.par->second.par->second = "";
        infixes.push_back(inf);
      }
    return primitive_toolkit::convert_data_vector_to_proper_list(infixes.begin(),infixes.end());
  }

} // End of namespace heist::stdlib_syntax

#endif