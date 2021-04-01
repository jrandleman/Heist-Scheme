// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for invariants.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_CONTROLFLOW_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_CONTROLFLOW_IMPLEMENTATION_HPP_

namespace heist {
  // Expand the macro named <label> with arguments <args> in environment <env> into <expanded_exp>.
  // Return success status.
  // From "lib/core/evaluator/dependancies/macro_expander.hpp"
  bool expand_macro_if_in_env(const string& label,data_vector args, env_type& env,data_vector& expanded_exp);
}

namespace heist::stdlib_controlflow {

  /******************************************************************************
  * ERROR HANDLING
  ******************************************************************************/

  void confirm_valid_error_primitive_layout(const data_vector& args, const char* name){
    if(args.size() < 2)
    HEIST_THROW_ERR('\''<<name<<" requires at least 2 args: a SYMBOL to represent the "
      "errorful entity & a STRING explaining the error!\n     ("<<name<<
      " <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << HEIST_FCN_ERR(name, args));
  if(!args[0].is_type(types::sym))
    HEIST_THROW_ERR('\''<<name<<" requires 1st arg "<<HEIST_PROFILE(args[0])<<" to be a "
      "SYMBOL to represent the errorful entity!\n     ("<<name<<
      " <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << HEIST_FCN_ERR(name, args));
  if(!args[1].is_type(types::str))
    HEIST_THROW_ERR('\''<<name<<" requires its 2nd arg "<<HEIST_PROFILE(args[1])<<
      " to be a STRING explaining the error!\n     ("<<name<<
      " <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << HEIST_FCN_ERR(name, args));
  }


  // Replace all "\n" with "\n  ", and add "\n" at the end IF one 
  // isn't there AND "\n" is in the string
  string prepare_error_string(string err, const bool given_item)noexcept{
    if(err.empty()) return "";
    bool found_newline = false;
    for(size_type i = 0; i < err.size(); ++i) {
      if(err[i] == '\n') {
        found_newline = true;
        // erase all spaces after "\n"
        size_type j = i+1;
        for(size_type m = err.size(); j < m && err[j] == ' '; ++j);
        err.erase(i+1, j-i);
        // add 2 spaces after "\n"
        err.insert(i+1,2,' ');
        i += 2; // skip past inserted spaces
      }
    }
    if(given_item && found_newline && *err.rbegin() != '\n') 
      return err + "\n ";
    return err;
  }


  void generic_error(const data_vector& args, const char* name, const char* err_type, const afmts& err_format){
    confirm_valid_error_primitive_layout(args, name);
    // Cook the raw error string
    string error_str = prepare_error_string(*args[1].str, args.size() == 3);
    // Alert error
    fprintf(stdout, "\n%s%s%s in %s: %s", HEIST_AFMT(err_format), err_type, HEIST_AFMT(AFMT_01), 
                    args[0].sym.c_str(), error_str.c_str());
    // Check for irritants (if provided, these are optional)
    if(args.size() == 3)
      fprintf(stdout, " with irritant %s", args[2].noexcept_write().c_str());
    else if(args.size() > 3) {
      data_vector irritant_list(args.begin()+2, args.end());
      fprintf(stdout, " with irritants %s", 
        primitive_toolkit::convert_data_vector_to_proper_list(irritant_list.begin(),irritant_list.end()).noexcept_write().c_str());
    }
    fprintf(stdout, "%s\n%s", HEIST_AFMT(AFMT_0), stack_trace_str("").c_str());
    fflush(stdout);
    throw SCM_EXCEPT::EVAL;
  }

  /******************************************************************************
  * CALLABLE SCOPE (LEXICAL VS DYNAMIC) SETTER/PREDICATE
  ******************************************************************************/

  // PRECONDITION: primitive_data_is_a_callable(d)
  void toggle_functor_scope_semantics(data& d, bool using_dynamic_scope)noexcept{
    obj_type obj = d.obj;
    while(obj) {
      // search object's local members
      for(size_type i = 0, n = obj->method_names.size(); i < n; ++i)
        if(obj->method_names[i] == "self->procedure") {
          obj->method_values[i].fcn.set_using_dynamic_scope(using_dynamic_scope);
          return;
        }
      // search object's prototype
      for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i)
        if(obj->proto->method_names[i] == "self->procedure") {
          // Cache the method dynamically added to the object's prototype IN the object
          obj->method_names.push_back("self->procedure"), obj->method_values.push_back(obj->proto->method_values[i]);
          obj->method_values.rbegin()->fcn.set_using_dynamic_scope(using_dynamic_scope);
          return;
        }
      // search inherited object prototype
      obj = obj->super;
    }
  }


  data convert_callable_scope(data_vector& args, bool using_dynamic_scope, const char* name, const char* format) {
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" didn't receive 1 arg!" << format << HEIST_FCN_ERR(name,args));
    primitive_toolkit::confirm_data_is_callable(args[0], args, name, format);
    if(args[0].is_type(types::obj)) {
      data obj = args[0].copy();
      toggle_functor_scope_semantics(obj, using_dynamic_scope);
      return obj;
    } else {
      data callable = args[0];
      callable.fcn.set_using_dynamic_scope(using_dynamic_scope);
      return callable;
    }
  }


  data check_callable_scope(data_vector& args, bool checking_dynamic_scope, const char* name, const char* format) {
    if(args.empty()) HEIST_THROW_ERR('\''<<name<<" didn't receive 1 arg!" << format << HEIST_FCN_ERR(name,args));
    return boolean(primitive_toolkit::validate_callable_and_convert_to_procedure(args[0],args,name,format).fcn.is_using_dynamic_scope() == checking_dynamic_scope);
  }

  /******************************************************************************
  * MACRO EXPANSION
  ******************************************************************************/

  bool symbol_is_a_core_syntax_label(const string& sym)noexcept{
    return std::find(G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.begin(),
                     G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.end(),
                     sym) != G.ANALYSIS_TIME_MACRO_LABEL_REGISTRY.end();
  }


  // Expand a confirmed expression & w/ symbol as the 1st arg
  data shallow_expand_syntax_macro_instance(const data_vector& exp, env_type& env, const bool core_only) {
    data_vector expanded;
    if(!exp.empty() && exp[0].is_type(types::sym) && 
       (!core_only || symbol_is_a_core_syntax_label(exp[0].sym)) &&
       expand_macro_if_in_env(exp[0].sym,data_vector(exp.begin()+1,exp.end()),env,expanded)){
      return expanded;
    }
    return data();
  }


  // Expand <d>'s macros
  data recursively_deep_expand_syntax_macros(const data& d, env_type& env, const bool core_only) {
    if(!d.is_type(types::exp)) return d;
    if(auto expanded = shallow_expand_syntax_macro_instance(d.exp,env,core_only); !expanded.is_type(types::undefined))
      return recursively_deep_expand_syntax_macros(expanded,env,core_only);
    const auto n = d.exp.size();
    data_vector expr(n);
    for(size_type i = 0; i < n; ++i)
      expr[i] = recursively_deep_expand_syntax_macros(d.exp[i],env,core_only);
    return expr;
  }


  data recursively_deep_expand_datum_macros(const data& d,env_type& env,const bool core_only) {
    data d_as_syntax;
    if(!primitive_toolkit::convert_data_to_evaluable_syntax(d,d_as_syntax)) return d;
    data quoted = data_vector(2);
    quoted.exp[0] = symconst::quote;
    quoted.exp[1] = recursively_deep_expand_syntax_macros(d_as_syntax,env,core_only);
    return scm_eval(std::move(quoted),env);
  }

} // End of namespace heist::stdlib_controlflow

#endif