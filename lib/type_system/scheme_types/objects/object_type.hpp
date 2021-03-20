// Author: Jordan Randleman -- jrandleman@scu.edu -- object_type.hpp
// => Contains "object_type" data structure for the C++ Heist Scheme Interpreter

#ifndef HEIST_OBJECT_TYPE_HPP_
#define HEIST_OBJECT_TYPE_HPP_

namespace heist {
  struct object_type {
    obj_type super = nullptr; // inherited proto subobject instance
    cls_type proto;           // ptr to the prototype object
    str_vector member_names, method_names;
    data_vector member_values, method_values;
    void define_property(const string& name, data value) noexcept;
    bool delete_property(const string& name) noexcept; // returns if found
    bool set_property(const string& name, data value) noexcept; // returns if found
    data get_property(const string& name, bool& found) noexcept; // NOTE: DOES _NOT_ EXTEND METHODS WITH "self"  => USE "function_object::bind_self" FOR SUCH !!!
    bool has_property(const string& name) noexcept; // returns if found
    bool has_member(const string& name) noexcept; // returns if found
    bool has_method(const string& name) noexcept; // returns if found
  };
}

#endif