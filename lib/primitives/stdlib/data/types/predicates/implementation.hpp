// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for predicates.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_TYPE_PREDICATES_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_TYPE_PREDICATES_IMPLEMENTATION_HPP_


// Dependancy to cache values from the object's prototypes that were dynamically 
// added to said prototype (& hence currently missing from <obj>)
// => from lib/primitives/stdlib/data/oo/objects/implementation.hpp
namespace heist::stdlib_objects {
  void populate_obj_with_new_dynamic_proto_properties(obj_type& obj)noexcept;
}


namespace heist::stdlib_type_predicates {

  /******************************************************************************
  * VALIDATION
  ******************************************************************************/

  void confirm_given_one_arg(const data_vector& args, const char* name){
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of arguments:"
        "\n     ("<<name<<" <obj>)"<<HEIST_FCN_ERR(name,args));
  }

  /******************************************************************************
  * TYPEOF OO-METHOD POLYMORPHISM HELPERS
  ******************************************************************************/

  bool object_has_overloaded_COERCE_SELF_TO_TYPE_method(obj_type& obj)noexcept{
    stdlib_objects::populate_obj_with_new_dynamic_proto_properties(obj);
    for(const auto& method_name : obj->method_names) 
      if(method_name == "self->type") return true;
    return obj->super && object_has_overloaded_COERCE_SELF_TO_TYPE_method(obj->super);
  }


  bool is_object_with_overloaded_COERCE_SELF_TO_TYPE_method(data& d)noexcept{
    if(!d.is_type(types::obj)) return false;
    return object_has_overloaded_COERCE_SELF_TO_TYPE_method(d.obj);
  }


  // PRECONDITION: <is_object_with_overloaded_COERCE_SELF_TO_TYPE_method>
  // <apply_dynamic_method> comes from: 
  //   lib/core/type_system/scheme_types/data/implementation_helpers/dynamic_method_applicator.hpp
  data apply_object_overloaded_COERCE_SELF_TO_TYPE_method(obj_type& obj) {
    for(size_type i = 0, n = obj->method_names.size(); i < n; ++i)
      if(obj->method_names[i] == "self->type")
        return apply_dynamic_method(obj,data_vector(),obj->method_values[i].fcn);
    if(obj->super) 
      return apply_object_overloaded_COERCE_SELF_TO_TYPE_method(obj->super);
    return data(); // never triggered IFF precondition met
  }

} // End of namespace heist::stdlib_type_predicates

#endif