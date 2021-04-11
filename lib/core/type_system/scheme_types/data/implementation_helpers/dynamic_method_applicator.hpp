// Author: Jordan Randleman -- jordanran199@gmail.com -- dynamic_method_applicator.hpp
// => Contains "apply_dynamic_method" function to help with object printing & copying 
//    for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_DYNAMIC_METHOD_APPLICATOR_HPP_
#define HEIST_SCHEME_CORE_DYNAMIC_METHOD_APPLICATOR_HPP_

data execute_application(data&,data_vector&& args = data_vector(),env_type& env=G.GLOBAL_ENVIRONMENT_POINTER,const bool tail_call=false,const bool applying_in_cps=false);
data execute_application(data&&,data_vector&& args = data_vector(),env_type& env=G.GLOBAL_ENVIRONMENT_POINTER,const bool tail_call=false,const bool applying_in_cps=false);

data apply_dynamic_method(obj_type& obj, data_vector args, fcn_type procedure_cpy) {
  data procedure = procedure_cpy.bind_self(obj);
  env_type env = obj->proto->defn_env;
  return execute_application(procedure,std::move(args),env);
}

#endif