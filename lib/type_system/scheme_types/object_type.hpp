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
  };
}

#endif