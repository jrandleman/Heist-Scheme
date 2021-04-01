// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Contains method implementations of "object_type" for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_OBJECT_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_OBJECT_IMPLEMENTATION_HPP_

#include "implementation_helpers/object_property_manipulation_logic.hpp"

namespace heist {
  void object_type::define_property(const string& name, data value) noexcept {
    if(value.is_type(types::fcn)) {
      define_object_method_property(*this, name, value);
    } else {
      define_object_member_property(*this, name, value);
    }
  }


  // Returns if found
  bool object_type::delete_property(const string& name) noexcept {
    return delete_object_property(*this, name);
  }


  // Returns if found
  bool object_type::set_property(const string& name, data value) noexcept {
    return set_new_object_property_value(*this, name, value);
  }


  // NOTE: DOES _NOT_ EXTEND METHODS WITH "self" => USE "function_object::bind_self" FOR SUCH !!!
  data object_type::get_property(const string& name, bool& found) {
    return get_object_property(*this, name, found);
  }


  // Returns if found
  bool object_type::has_property(const string& name) {
    bool ignore_this_parameter = false;
    return object_has_property_name(*this, name, ignore_this_parameter);
  }


  // Returns if found
  bool object_type::has_member(const string& name) {
    bool is_member = false;
    bool found_property = object_has_property_name(*this, name, is_member);
    return found_property && is_member;
  }


  // Returns if found
  bool object_type::has_method(const string& name) {
    bool is_member = false;
    bool found_property = object_has_property_name(*this, name, is_member);
    return found_property && !is_member;
  }
}

#endif