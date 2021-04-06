// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for coroutines.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_COROUTINES_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_COROUTINES_IMPLEMENTATION_HPP_

namespace heist::stdlib_coroutines {
  
  cls_type get_coroutine_class_prototype(data_vector& args, const char* format) {
    bool found = false;
    auto val = G.GLOBAL_ENVIRONMENT_POINTER->lookup_variable_value("coroutine", found);
    if(!found || !val.is_type(types::cls)) 
      HEIST_THROW_ERR("'cycle-coroutines! 'coroutine symbol isn't bound to a class prototype!"
        << format << HEIST_FCN_ERR("cycle-coroutines!",args));
    return val.cls;
  }


  bool datum_is_a_coroutine(data& d, cls_type& coro_proto)noexcept{
    return d.is_type(types::obj) && d.obj->proto == coro_proto;
  }


  data invoke_coroutine_NEXT_method(data& d, const char* format) {
    auto& methods = d.obj->method_names;
    for(size_type i = 0, n = methods.size(); i < n; ++i) {
      if(methods[i] == "next") {
        auto& env = d.obj->proto->defn_env;
        d = d.obj->method_values[i].fcn.bind_self(d.obj);
        return execute_application(d,data_vector(),env);
      }
    }
    HEIST_THROW_ERR("'cycle-coroutines! 'coroutine object " << d
      << " is missing the \"next\" method!" << format); 
    return data(); // never triggered
  }

} // End of namespace heist::stdlib_coroutines

#endif