// Author: Jordan Randleman -- jrandleman@scu.edu -- class_prototype.hpp
// => Contains "class_prototype" data structure for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_CLASS_PROTOTYPE_HPP_
#define HEIST_SCHEME_CORE_CLASS_PROTOTYPE_HPP_

namespace heist {
  struct class_prototype {
    cls_type super = nullptr; // inherited proto
    env_type defn_env;        // environment of class prototype definition
    fcn_type user_ctor;       // user-defined ctor (generated if undefined by user)
    string class_name;
    str_vector member_names, method_names;
    data_vector member_values, method_values; // default member values of the proto
    void bind_user_ctor(const fcn_type& c)noexcept{
      user_ctor = c;
      user_ctor.name = class_name;
    }
  };
}

#endif