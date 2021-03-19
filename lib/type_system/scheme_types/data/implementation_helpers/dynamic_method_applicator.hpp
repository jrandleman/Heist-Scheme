// Author: Jordan Randleman -- jrandleman@scu.edu -- dynamic_method_applicator.hpp
// => Contains "apply_dynamic_method" function to help with object printing & copying 
//    for the C++ Heist Scheme Interpreter

#ifndef HEIST_DYNAMIC_METHOD_APPLICATOR_HPP_
#define HEIST_DYNAMIC_METHOD_APPLICATOR_HPP_

data execute_application(data&,data_vector&,env_type& env=G.GLOBAL_ENVIRONMENT_POINTER,const bool tail_call=false,const bool applying_in_cps=false);
data execute_application(data&&,data_vector&,env_type& env=G.GLOBAL_ENVIRONMENT_POINTER,const bool tail_call=false,const bool applying_in_cps=false);
data extend_method_env_with_SELF_object(obj_type& calling_obj, fcn_type& procedure)noexcept;

data apply_dynamic_method(obj_type& obj, data_vector args, fcn_type procedure_cpy) {
  data procedure = extend_method_env_with_SELF_object(obj,procedure_cpy);
  env_type env = obj->proto->defn_env;
  return execute_application(procedure,args,env);
}

#endif