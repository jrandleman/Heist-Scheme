// Author: Jordan Randleman -- jrandleman@scu.edu -- primitive_toolkit.hpp
// => Defines quality-of-life helper functions for extending the Heist Scheme Interpreter
//    with C++ primitives

// Recall: the primitive C++ function signature is "heist::data(std::vector<heist::data>&&)"

#ifndef HEIST_SCHEME_CORE_PRIMITIVE_TOOLKIT_HPP_
#define HEIST_SCHEME_CORE_PRIMITIVE_TOOLKIT_HPP_

#include "stdlib/lang/primitive_toolkit_helper.hpp"

namespace heist::primitive_toolkit {

  /******************************************************************************
  * GENERATE A PARTIALLY APPLIED PRIMITIVE PROCEDURE
  ******************************************************************************/

  data GENERATE_PRIMITIVE_PARTIAL(prm_ptr_t primitive_function_pointer, const data_vector& bound_args)noexcept{
    return fcn_type(bound_args,primitive_function_pointer);
  }

  /******************************************************************************
  * GET CURRENT INPUT/OUTPUT PORT AS FILE POINTERS (MUST __NOT__ GET CLOSED!)
  ******************************************************************************/

  FILE* get_current_output_port(const data_vector& args, const char* name, const char* format = "") {
    if(G.CURRENT_OUTPUT_PORT.is_open()) return *G.CURRENT_OUTPUT_PORT.fp;
    HEIST_THROW_ERR('\''<<name<<" current output port is closed!" << format << HEIST_FCN_ERR(name,args));
  }

  FILE* get_current_input_port(const data_vector& args, const char* name, const char* format = "") {
    if(G.CURRENT_INPUT_PORT.is_open()) return *G.CURRENT_INPUT_PORT.fp;
    HEIST_THROW_ERR('\''<<name<<" current input port is closed!" << format << HEIST_FCN_ERR(name,args));
  }

  /******************************************************************************
  * DATA LIST PREDICATES
  ******************************************************************************/

  bool data_is_nil(const data& d)noexcept{
    return d.is_type(types::sym) && d.sym == symconst::emptylist;
  }

  bool data_is_proper_list(const data& d)noexcept{
    return data_is_nil(d) || (d.is_type(types::par) && get_list_status(d) == list_status::proper);
  }

  bool data_is_dotted_list(const data& d)noexcept{
    return d.is_type(types::par) && get_list_status(d) == list_status::dotted;
  }

  bool data_is_circular_list(const data& d)noexcept{
    return d.is_type(types::par) && get_list_status(d) == list_status::circular;
  }

  /******************************************************************************
  * DATA-VECTOR -> PROPER-LIST COERCION
  ******************************************************************************/

  template<typename data_vector_iterator>
  data convert_data_vector_to_proper_list(const data_vector_iterator& obj, const data_vector_iterator& null_obj)noexcept{
    if(obj == null_obj) return symconst::emptylist;
    data new_pair = data(make_par());
    new_pair.par->first = *obj;
    new_pair.par->second = convert_data_vector_to_proper_list(obj+1,null_obj);
    return new_pair;
  }

  /******************************************************************************
  * PROPER-LIST -> DATA-VECTOR COERCION
  ******************************************************************************/

  data_vector convert_proper_list_to_data_vector(const data& d)noexcept{
    data_vector vec;
    data iter = d;
    while(iter.is_type(types::par)) {
      vec.push_back(iter.par->first);
      iter = iter.par->second;
    }
    return vec;
  }

  /******************************************************************************
  * DATA->SIZE_TYPE COERCION
  ******************************************************************************/

  // Returns a pair: {size_type_value, success_status}
  std::pair<size_type,bool> convert_data_to_size_type(const data& d)noexcept{
    if(d.is_type(types::num) && d.num.is_integer() && !d.num.is_neg() && d.num <= GLOBALS::MAX_SIZE_TYPE)
      return std::make_pair((size_type)d.num.extract_inexact(),true);
    return std::make_pair(0,false);
  }

  /******************************************************************************
  * GENERAL FUNCTOR / CALLABLE HANDLING PRIMITIVES
  ******************************************************************************/

  bool data_is_functor(const data& d)noexcept{
    if(!d.is_type(types::obj)) return false;
    obj_type obj = d.obj;
    while(obj) {
      // search object's local members
      for(size_type i = 0, n = obj->method_names.size(); i < n; ++i)
        if(obj->method_names[i] == "self->procedure") return true;
      // search object's prototype
      for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i)
        if(obj->proto->method_names[i] == "self->procedure") return true;
      // search inherited object prototype
      obj = obj->super;
    }
    return false;
  }


  bool data_is_callable(const data& d)noexcept{
    return d.is_type(types::fcn) || d.is_type(types::cls) || data_is_functor(d);
  }


  void confirm_data_is_callable(const data& d, const data_vector& args, const char* name, const char* format = ""){
    if(!data_is_callable(d))
      HEIST_THROW_ERR('\'' << name << " arg " << HEIST_PROFILE(d) 
        << " isn't a callable (procedure or functor)!" << format << HEIST_FCN_ERR(name,args));
  }


  // PRECONDITION: data_is_callable(d)
  // NOTE: Data <d> is mutable for functors to be able to cache "self->procedure" from proto if dynamically added
  data convert_callable_to_procedure(data& d)noexcept{
    // handle class prototypes (constructors)
    if(d.is_type(types::cls)) return d.cls->user_ctor;
    // handle primitive or compound procedure
    if(!d.is_type(types::obj)) return d;
    // handle functor
    obj_type obj = d.obj;
    while(obj) {
      // search object's local members
      for(size_type i = 0, n = obj->method_names.size(); i < n; ++i)
        if(obj->method_names[i] == "self->procedure")
          return obj->method_values[i].fcn.bind_self(obj);
      // search object's prototype
      for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i)
        if(obj->proto->method_names[i] == "self->procedure") {
          // Cache the method dynamically added to the object's prototype IN the object
          obj->method_names.push_back("self->procedure"), obj->method_values.push_back(obj->proto->method_values[i]);
          return obj->method_values.rbegin()->fcn.bind_self(obj);
        }
      // search inherited object prototype
      obj = obj->super;
    }
    return data(); // never triggered iff precondition met
  }


  // NOTE: Data <d> is mutable for functors to be able to cache "self->procedure" from proto if dynamically added
  data validate_callable_and_convert_to_procedure(data& d, const data_vector& args, const char* name, const char* format = "") {
    confirm_data_is_callable(d,args,name,format);
    return convert_callable_to_procedure(d);
  }


  // PRECONDITION: data_is_callable(d)
  // NOTE: <callable> is passed by reference ONLY for efficiency. 
  //       <callable> is guaranteed to be in a valid, "callable" state after application.
  data apply_callable(data& callable,data_vector&& args = data_vector(),env_type& env = G.GLOBAL_ENVIRONMENT_POINTER,const bool tail_call = false){
    return execute_application(convert_callable_to_procedure(callable),std::move(args),env,tail_call);
  }


  // PRECONDITION: data_is_callable(d)
  // NOTE: Assumes the continuation is *args.rbegin()
  // NOTE: <callable> is passed by reference ONLY for efficiency. 
  //       <callable> is guaranteed to be in a valid, "callable" state after application.
  data apply_callable_with_continuation(data& callable,data_vector&& args,env_type& env = G.GLOBAL_ENVIRONMENT_POINTER,const bool tail_call = false){
    return execute_application(convert_callable_to_procedure(callable),std::move(args),env,tail_call,true);
  }

  /******************************************************************************
  * DATA->SYNTAX TRANSFORMATION
  ******************************************************************************/

  bool convert_data_to_evaluable_syntax(const data& d, data& data_as_syntax)noexcept{
    // Primitive non-container atomics evaluate to themselves
    if(data_is_self_evaluating_for_EVAL(d)) {
      data_as_syntax = d;
      return true;
    }
    // Nil is syntax, & all non-nil symbols are evaluable
    if(d.is_type(types::sym)) {
      if(d.sym == symconst::emptylist) {
        data_as_syntax = data_vector();
      } else {
        data_as_syntax = d;
      }
      return true;
    }
    // Pairs become expressions
    if(d.is_type(types::par) && !data_is_circular_list(d)) {
      data_as_syntax = data_vector();
      return deep_unpack_data_list_into_syntax_expr(d,data_as_syntax.exp);
    }
    // Vectors revert to s-expressions using the "vector-literal" tag
    if(d.is_type(types::vec)) {
      data_as_syntax = data_vector();
      return deep_unpack_data_vector_into_syntax_expr(d,data_as_syntax.exp);
    }
    // Hmaps revert to s-expressions using the "hmap-literal" tag
    if(d.is_type(types::map)) {
      data_as_syntax = data_vector();
      return deep_unpack_data_hmap_into_syntax_expr(d,data_as_syntax.exp);
    }
    return false;
  }
}

#endif