// Author: Jordan Randleman -- jrandleman@scu.edu -- prototypes.hpp
// => Defines the primitive class-prototype analysis functions written in 
//    C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_OO_PROTOTYPES_HPP_
#define HEIST_SCHEME_CORE_STDLIB_OO_PROTOTYPES_HPP_

#include "implementation.hpp"

namespace heist {

  data primitive_PROTO_NAME(data_vector&& args) {
    stdlib_prototypes::confirm_given_unary_class_prototype_arg(args,"proto-name");
    return args[0].cls->class_name;
  }

  data primitive_PROTO_MEMBERS(data_vector&& args) {
    stdlib_prototypes::confirm_given_unary_class_prototype_arg(args,"proto-members");
    data_vector member_names;
    for(const auto& name : args[0].cls->member_names)
      member_names.push_back(name);
    return primitive_toolkit::convert_data_vector_to_proper_list(member_names.begin(),member_names.end());
  }

  data primitive_PROTO_METHODS(data_vector&& args) {
    stdlib_prototypes::confirm_given_unary_class_prototype_arg(args,"proto-methods");
    data_vector method_names;
    for(const auto& name : args[0].cls->method_names)
      method_names.push_back(name);
    return primitive_toolkit::convert_data_vector_to_proper_list(method_names.begin(),method_names.end());
  }

  data primitive_PROTO_SUPER(data_vector&& args) {
    stdlib_prototypes::confirm_given_unary_class_prototype_arg(args,"proto-super");
    if(!args[0].cls->super) return GLOBALS::FALSE_DATA_BOOLEAN;
    return args[0].cls->super;
  }

  data primitive_PROTO_ADD_PROPERTY_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (proto-add-property! <class-property> <property-name-symbol> <procedure-value>)";
    stdlib_prototypes::confirm_proper_new_property_args(args,"proto-add-property!",format);
    // Verify new property name isn't already the name of a member or method
    stdlib_prototypes::confirm_new_property_name_doesnt_already_exist(args,"proto-add-property!",format);
    // Define the new method
    if(args[2].is_type(types::fcn)) {
      args[0].cls->method_names.push_back(args[1].sym);
      if(args[2].fcn.name.empty()) args[2].fcn.name = object_type::hash_method_name(args[1].sym);
      args[0].cls->method_values.push_back(args[2]);
    // Define the new member
    } else {
      args[0].cls->member_names.push_back(args[1].sym);
      args[0].cls->member_values.push_back(args[2]);
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

} // End of namespace heist

#endif