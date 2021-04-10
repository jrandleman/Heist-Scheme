// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Contains methods implementations of "struct function_object" for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_FUNCTION_OBJECT_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_FUNCTION_OBJECT_IMPLEMENTATION_HPP_

namespace heist {

  /******************************************************************************
  * EQUALITY
  ******************************************************************************/

  bool function_object::operator==(const function_object& f)const noexcept{
    if(is_primitive() || f.is_primitive())
      return prm == f.prm && param_instances.empty() && f.param_instances.empty();
    if(env != f.env || self != f.self || rec_depth != f.rec_depth || name != f.name || 
       flags != f.flags || param_instances.size() != f.param_instances.size())
       return false;
    for(size_type i = 0, n = param_instances.size(); i < n; ++i) {
      if(param_instances[i].first.size() != f.param_instances[i].first.size()) return false;
      for(size_type j = 0, m = param_instances[i].first.size(); j < m; ++j)
        if(!param_instances[i].first[j].noexcept_equal(f.param_instances[i].first[j])) return false;
    }
    return true;
  }

  /******************************************************************************
  * LAMBDA PARAMETER LIST EXTRACTION
  ******************************************************************************/

  str_vector function_object::lambda_parameters()const{
    const auto n = param_instances[0].first.size();
    str_vector var_names(n);
    for(size_type i = 0; i < n; ++i) {
      if(param_instances[0].first[i].is_type(types::sym)) {
        var_names[i] = param_instances[0].first[i].sym;
      } else {
        HEIST_THROW_ERR("Non-symbolic lambda parameter detected! -:- BUG ALERT -:-"
           "\n     Triggered By " << HEIST_PROFILE(param_instances[0].first[i])
           << " IN " << param_instances[0].first << "!" 
           "\n  => Please send your code to jrandleman@scu.edu to fix"
           "\n     the interpreter's bug!"
           "\n  => Terminating Heist Scheme Interpretation.");
      }
    }
    return var_names;
  }

  /******************************************************************************
  * ASSIGNMENT
  ******************************************************************************/

  void function_object::operator=(const function_object& f)noexcept{
    if(this == &f) return;
    name = f.name, param_instances = f.param_instances;
    if(f.is_primitive()) {
      prm = f.prm;
    } else {
      bodies = f.bodies, self = f.self, prm = nullptr;
      env = f.env, rec_depth = f.rec_depth, flags = f.flags;
    }
  }


  void function_object::operator=(function_object&& f)noexcept{
    if(this == &f) return;
    name = std::move(f.name), param_instances = std::move(f.param_instances);
    if(f.is_primitive()) {
      prm = std::move(f.prm);
    } else {
      bodies = std::move(f.bodies), self = std::move(f.self), prm = nullptr;
      env = std::move(f.env), rec_depth = std::move(f.rec_depth), flags = std::move(f.flags);
    }
  }

  /******************************************************************************
  * FN PARAMETER MATCHING
  ******************************************************************************/

  namespace fn_param_matching {
    string get_possible_signature(const data_vector& params)noexcept{
      string buff;
      for(size_type i = 0, n = params.size(); i < n; ++i) {
        if(params[i].is_type(types::exp)) {
          if(!params[i].exp.empty() && params[i].exp[0].is_type(types::sym) && 
            (params[i].exp[0].sym == symconst::vec_literal || params[i].exp[0].sym == symconst::map_literal)){
            buff += params[i].exp[0].sym == symconst::vec_literal ? '#' : '$';
            buff += '(' + get_possible_signature(data_vector(params[i].exp.begin()+1,params[i].exp.end()));
          } else {
            buff += '(' + get_possible_signature(params[i].exp);
          }
        } else {
          buff += params[i].noexcept_write();
        }
        if(i+1 < n) buff += ' ';
      }
      return buff + ')';
    }


    string get_possible_signatures(const std::vector<function_object::params_type>& param_instances,const string& name)noexcept{
      string buff;
      for(size_type i = 0, n = param_instances.size(); i < n; ++i) {
        if(param_instances[i].first.empty())
          buff += "\n        (" + name + ')';
        else
          buff += "\n        (" + name + ' ' + get_possible_signature(param_instances[i].first);
      }
      return buff;
    }


    bool param_is_token(const data& d)noexcept{
      return d.is_type(types::sym);
    }


    // PRECONDITION: param_is_token(d)
    bool param_is_boolean_literal(const data& d)noexcept{ 
      return d.sym == "#f" || d.sym == "#t";
    }


    bool param_boolean_mismatch(const string& sym, const data& arg)noexcept{ 
      return (sym == "#f") ^ arg.is_falsey();
    }


    bool param_is_symbol_literal(const data& d)noexcept{
      return d.is_type(types::exp) && d.exp.size() == 2 && 
             d.exp[0].is_type(types::sym) && d.exp[1].is_type(types::sym) && 
             d.exp[0].sym == symconst::quote;
    }


    bool param_symbol_mismatch(const string& sym, const data& arg)noexcept{
      return !arg.is_type(types::sym) || sym != arg.sym;
    }


    bool param_is_non_container_literal(const data& d)noexcept{
      return !d.is_type(types::exp);
    }


    bool param_non_container_literal_mismatch(const data& lhs, const data& rhs)noexcept{
      if(lhs.type != rhs.type) return true;
      if(lhs.is_type(types::num)) return lhs.num != rhs.num; // use = for numbers (* COMMENT THIS LINE OUT FOR EXACTNESS SENSITIVITY *)
      return !lhs.noexcept_equal(rhs); // noexcept equal since only matching against literals (ie no objects)
    }


    bool param_is_vector_literal(const data& d)noexcept{ // PRECONDITION: d.is_type(types::exp)
      return !d.exp.empty() && d.exp[0].is_type(types::sym) && d.exp[0].sym == symconst::vec_literal;
    }


    bool param_is_hmap_literal(const data& d)noexcept{ // PRECONDITION: d.is_type(types::exp)
      return !d.exp.empty() && d.exp[0].is_type(types::sym) && d.exp[0].sym == symconst::map_literal;
    }


    bool param_parse_hmap_literal(const data&,data&,data_vector&,str_vector&)noexcept;
    bool param_parse_list_literal(const data&,data&,data_vector&,str_vector&)noexcept;


    bool param_parse_vector_literal(const data& vec, data& arg, data_vector& values, str_vector& unpacked_params)noexcept{
      if(!arg.is_type(types::vec)) return false;
      if(vec.exp.size() != arg.vec->size()+1) return false;
      for(size_type i = 1, j = 0, n = vec.exp.size(); i < n; ++i, ++j) {
        // tokens match anything
        if(param_is_token(vec.exp[i])) {
          // booleans are a specialized token instance
          if(param_is_boolean_literal(vec.exp[i])) {
            if(param_boolean_mismatch(vec.exp[i].sym,arg.vec->operator[](j))) return false;
          } else {
            unpacked_params.push_back(vec.exp[i].sym);
            values.push_back(arg.vec->operator[](j));
          }
        // match against quoted non-nil literal symbols
        } else if(param_is_symbol_literal(vec.exp[i])) {
          if(param_symbol_mismatch(vec.exp[i].exp[1].sym,arg.vec->operator[](j))) return false;
        // match against non-container literals
        } else if(param_is_non_container_literal(vec.exp[i])) {
          if(param_non_container_literal_mismatch(vec.exp[i],arg.vec->operator[](j))) return false;
        // match against vector literals
        } else if(param_is_vector_literal(vec.exp[i])) {
          if(!param_parse_vector_literal(vec.exp[i],arg.vec->operator[](j),values,unpacked_params)) return false;
        // match against hmap literals
        } else if(param_is_hmap_literal(vec.exp[i])) {
          if(!param_parse_hmap_literal(vec.exp[i],arg.vec->operator[](j),values,unpacked_params)) return false;
        // match against list literals
        } else {
          if(!param_parse_list_literal(vec.exp[i],arg.vec->operator[](j),values,unpacked_params)) return false;
        }
      }
      return true;
    }


    bool param_parse_hmap_literal(const data& map, data& arg, data_vector& values, str_vector& unpacked_params)noexcept{
      if(!arg.is_type(types::map)) return false;
      if((map.exp.size()-1)/2 != arg.map->val.size()) return false;
      auto iter = arg.map->val.begin();
      for(size_type i = 1, n = map.exp.size(); i < n; i += 2, ++iter) {
        data elt = map_object::unhash_key(iter->first);
        for(size_type offset = 0; offset < 2; ++offset) {
          // tokens match anything
          if(param_is_token(map.exp[i+offset])) {
            // booleans are a specialized token instance
            if(param_is_boolean_literal(map.exp[i+offset])) {
              if(param_boolean_mismatch(map.exp[i+offset].sym,elt)) return false;
            } else {
              unpacked_params.push_back(map.exp[i+offset].sym);
              values.push_back(elt);
            }
          // match against quoted non-nil literal symbols
          } else if(param_is_symbol_literal(map.exp[i+offset])) {
            if(param_symbol_mismatch(map.exp[i+offset].exp[1].sym,elt)) return false;
          // match against non-container literals
          } else if(param_is_non_container_literal(map.exp[i+offset])) {
            if(param_non_container_literal_mismatch(map.exp[i+offset],elt)) return false;
          // match against vector literals
          } else if(param_is_vector_literal(map.exp[i+offset])) {
            if(!param_parse_vector_literal(map.exp[i+offset],elt,values,unpacked_params)) return false;
          // match against hmap literals
          } else if(param_is_hmap_literal(map.exp[i+offset])) {
            if(!param_parse_hmap_literal(map.exp[i+offset],elt,values,unpacked_params)) return false;
          // match against list literals
          } else {
            if(!param_parse_list_literal(map.exp[i+offset],elt,values,unpacked_params)) return false;
          }
          if(!offset) elt = iter->second;
        }
      }
      return true;
    }


    bool param_parse_list_literal(const data& lst, data& arg, data_vector& values, str_vector& unpacked_params)noexcept{
      if(lst.exp.empty()) return arg.is_type(types::sym) && arg.sym == symconst::emptylist; // match NIL
      if(!arg.is_type(types::par)) return false;
      size_type i = 0, n = lst.exp.size();
      auto iter = arg;
      for(; i < n; ++i) {
        // dotted list may match all remaining values
        if(data_is_dot_operator(lst.exp[i])) {
          // pattern match dotted elt
          if(param_is_token(lst.exp[i+1])) {
            // booleans are a specialized token instance
            if(param_is_boolean_literal(lst.exp[i+1])) {
              return !param_boolean_mismatch(lst.exp[i+1].sym,iter);
            } else {
              unpacked_params.push_back(lst.exp[i+1].sym);
              values.push_back(iter);
              return true;
            }
          } 
          // match against quoted non-nil literal symbols
          if(param_is_symbol_literal(lst.exp[i+1]))
            return !param_symbol_mismatch(lst.exp[i+1].exp[1].sym,iter);
          // match against non-container literals
          if(param_is_non_container_literal(lst.exp[i+1]))
            return !param_non_container_literal_mismatch(lst.exp[i+1],iter);
          // match against vector literals
          if(param_is_vector_literal(lst.exp[i+1]))
            return param_parse_vector_literal(lst.exp[i+1],iter,values,unpacked_params);
          // match against hmap literals
          if(param_is_hmap_literal(lst.exp[i+1]))
            return param_parse_hmap_literal(lst.exp[i+1],iter,values,unpacked_params);
          // match against list literals
          return param_parse_list_literal(lst.exp[i+1],iter,values,unpacked_params);
        }
        // non-dotted list must currently match against a pair elt
        if(!iter.is_type(types::par)) return false;
        // tokens match anything
        if(param_is_token(lst.exp[i])) {
          // booleans are a specialized token instance
          if(param_is_boolean_literal(lst.exp[i])) {
            if(param_boolean_mismatch(lst.exp[i].sym,iter.par->first)) return false;
          } else {
            unpacked_params.push_back(lst.exp[i].sym);
            values.push_back(iter.par->first);
          }
        // match against quoted non-nil literal symbols
        } else if(param_is_symbol_literal(lst.exp[i])) {
          if(param_symbol_mismatch(lst.exp[i].exp[1].sym,iter.par->first)) return false;
        // match against non-container literals
        } else if(param_is_non_container_literal(lst.exp[i])) {
          if(param_non_container_literal_mismatch(lst.exp[i],iter.par->first)) return false;
        // match against vector literals
        } else if(param_is_vector_literal(lst.exp[i])) {
          if(!param_parse_vector_literal(lst.exp[i],iter.par->first,values,unpacked_params)) return false;
        // match against hmap literals
        } else if(param_is_hmap_literal(lst.exp[i])) {
          if(!param_parse_hmap_literal(lst.exp[i],iter.par->first,values,unpacked_params)) return false;
        // match against list literals
        } else {
          if(!param_parse_list_literal(lst.exp[i],iter.par->first,values,unpacked_params)) return false;
        }
        iter = iter.par->second;
      }
      return i == n && iter.is_type(types::sym) && iter.sym == symconst::emptylist;
    }


    // ACCOUNT FOR:
    // 0. VARIADICS VIA "."
    // 1. PARAMETER SYMBOLIC TOKENS (MATCH AGAINST ANYTHING)
    // 2. MATCHING SYMBOLS AGAINST QUOTED SYMBOL LITERALS (NOT NIL: FN PARAMETER SHOULD BE () NOT '())
    // 3. MATCHING NON-CONTAINER NON-QUOTED-SYMBOL LITERALS
    // 4. MATCHING & UPPACKING LIST/VECTOR/HMAP CONTAINER LITERALS
    bool is_fn_call_match(const data_vector& params, data_vector& arguments, data_vector& values, str_vector& unpacked_params)noexcept{
      // confirm emptiness match
      if((params.empty() || !data_is_dot_operator(params[0])) && (params.empty() ^ arguments.empty())) {
        return false;
      } else if(params.empty()) {
        return true;
      }
      size_type i = 0, j = 0, n = params.size(), m = arguments.size();
      for(; i < n && j < m+1; ++i, ++j) { // j < m+1 to match no args against varaidic args
        // variadics match all remaining values
        if(data_is_dot_operator(params[i])) {
          for(auto iter = params.begin()+i, end = params.end(); iter != end; ++iter)
            unpacked_params.push_back(iter->sym);
          values.insert(values.end(),arguments.begin()+j,arguments.end());
          return true;
        }
        if(j == m) return false;
        // tokens match anything
        if(param_is_token(params[i])) {
          // booleans are a specialized token instance
          if(param_is_boolean_literal(params[i])) {
            if(param_boolean_mismatch(params[i].sym,arguments[j])) return false;
          } else {
            unpacked_params.push_back(params[i].sym);
            values.push_back(arguments[j]);
          }
        // match against quoted non-nil literal symbols
        } else if(param_is_symbol_literal(params[i])) {
          if(param_symbol_mismatch(params[i].exp[1].sym,arguments[j])) return false;
        // match against non-container literals
        } else if(param_is_non_container_literal(params[i])) {
          if(param_non_container_literal_mismatch(params[i],arguments[j])) return false;
        // match against vector literals
        } else if(param_is_vector_literal(params[i])) {
          if(!param_parse_vector_literal(params[i],arguments[j],values,unpacked_params)) return false;
        // match against hmap literals
        } else if(param_is_hmap_literal(params[i])) {
          if(!param_parse_hmap_literal(params[i],arguments[j],values,unpacked_params)) return false;
        // match against list literals
        } else {
          if(!param_parse_list_literal(params[i],arguments[j],values,unpacked_params)) return false;
        }
      }
      return i == n && j == m;
    }


    size_type match_fn_call_signature(const std::vector<function_object::params_type>& param_instances, const string& name, 
                                      data_vector& arguments, data_vector& values, str_vector& unpacked_params){
      for(size_type i = 0, n = param_instances.size(); i < n; ++i) {
        if(is_fn_call_match(param_instances[i].first,arguments,values,unpacked_params)) return i;
        unpacked_params.clear(), values.clear();
      }
      HEIST_THROW_ERR("'fn arguments "<<data(arguments)<<" don't match any signatures!\n     -> Possible Signatures:" 
        << get_possible_signatures(param_instances,name) << HEIST_FCN_ERR(name,arguments));
      return 0; // never triggered
    }
  } // End of namespace fn_param_matching

  /******************************************************************************
  * COMPOUND PROCEDURE ENVIRONMENT EXTENSION
  ******************************************************************************/

  // Default continuation to provide to cps procs applied in a non-cps context
  namespace DEFAULT_TOPMOST_CONTINUATION {
    data id(data_vector&& args) {
      if(args.size() != 1) HEIST_THROW_ERR("'id not given 1 argument: (id <obj>)" << HEIST_FCN_ERR("id",args));
      return args[0];
    }
  }


  // Get the extended environment for the compound procedure given <arguments>
  env_type function_object::get_extended_environment(data_vector& arguments, exe_fcn_t& body, const bool applying_in_cps){
    // add <id> as the topmost continuation if applying a procedure accepting a continuation in a non-cps environment
    if(is_cps_procedure() && !applying_in_cps)
      arguments.push_back(function_object("id",DEFAULT_TOPMOST_CONTINUATION::id));
    // extend the lambda environment
    if(is_lambda()) {
      body = bodies[0];
      return extend_environment(lambda_parameters(), param_instances[0].second, arguments, env, name);
    }
    // extend the fn environment
    str_vector unpacked_params;
    data_vector values;
    auto param_idx = fn_param_matching::match_fn_call_signature(param_instances,printable_procedure_name(),arguments,values,unpacked_params);
    body = bodies[param_idx];
    return extend_environment(std::move(unpacked_params), param_instances[param_idx].second, values, env, name);
  }
}

#endif