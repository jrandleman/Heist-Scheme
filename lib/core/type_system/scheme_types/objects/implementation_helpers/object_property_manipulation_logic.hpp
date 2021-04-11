// Author: Jordan Randleman -- jordanran199@gmail.com -- object_property_manipulation_logic.hpp
// => Contains helper functions of "../implementation.hpp" for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_OBJECT_PROPERTY_MANIPULATION_LOGIC_HPP_
#define HEIST_SCHEME_CORE_OBJECT_PROPERTY_MANIPULATION_LOGIC_HPP_

namespace heist {

  /******************************************************************************
  * OBJECTS PROPERTY DEFINITION
  ******************************************************************************/

  void define_object_member_property(object_type& obj, const string& property_name, data& value)noexcept{
    // Set local member if already exists
    for(size_type i = 0, n = obj.member_names.size(); i < n; ++i) {
      if(obj.member_names[i] == property_name) {
        obj.member_values[i] = value;
        return;
      }
    }
    // Rm if member already exists as a local method
    for(size_type i = 0, n = obj.method_names.size(); i < n; ++i) {
      if(obj.method_names[i] == property_name) {
        obj.method_names.erase(obj.method_names.begin()+i);
        obj.method_values.erase(obj.method_values.begin()+i);
        break;
      }
    }
    // add the new member name & assign it the given value
    obj.member_names.push_back(property_name);
    obj.member_values.push_back(value);
  }


  void define_object_method_property(object_type& obj, const string& property_name, data& value)noexcept{
    if(value.fcn.name.empty()) value.fcn.name = object_type::hash_method_name(property_name);
    // Set local method if already exists
    for(size_type i = 0, n = obj.method_names.size(); i < n; ++i) {
      if(obj.method_names[i] == property_name) {
        obj.method_values[i] = value;
        return;
      }
    }
    // Rm if method already exists as a local member
    for(size_type i = 0, n = obj.member_names.size(); i < n; ++i) {
      if(obj.member_names[i] == property_name) {
        obj.member_names.erase(obj.member_names.begin()+i);
        obj.member_values.erase(obj.member_values.begin()+i);
        break;
      }
    }
    // add the new method name & assign it the given value
    obj.method_names.push_back(property_name);
    obj.method_values.push_back(value);
  }

  /******************************************************************************
  * OBJECT PROPERTY MUTATION
  ******************************************************************************/

  bool set_new_property_value_SEEK_IN_PROTO(const str_vector& property_names, object_type& obj, 
                                            const string& property_name, data& value)noexcept{
    for(size_type i = 0, n = property_names.size(); i < n; ++i) {
      if(property_names[i] == property_name) {
        // setting property to be a member
        if(!value.is_type(types::fcn)) {
          obj.member_names.push_back(property_name);
          obj.member_values.push_back(value);
        // setting property to be a method
        } else {
          obj.method_names.push_back(property_name);
          if(value.fcn.name.empty()) value.fcn.name = object_type::hash_method_name(property_name);
          obj.method_values.push_back(value);
        }
        return true;
      }
    }
    return false;
  }


  bool set_new_property_value_SEEK_IN_OBJ(str_vector& seeking_names, data_vector& seeking_values, 
                                          str_vector& alt_names, data_vector& alt_values, const bool value_in_SEEKING_set, 
                                          const string& property_name, data& value)noexcept{
    for(size_type i = 0, n = seeking_names.size(); i < n; ++i) {
      if(seeking_names[i] == property_name) {
        if(value.is_type(types::fcn) && value.fcn.name.empty()) 
          value.fcn.name = object_type::hash_method_name(property_name);
        if(value_in_SEEKING_set) {
          seeking_values[i] = value;
        } else {
          seeking_names.erase(seeking_names.begin()+i);
          seeking_values.erase(seeking_values.begin()+i);
          alt_names.push_back(property_name);
          alt_values.push_back(value);
        }
        return true;
      }
    }
    return false;
  }


  // Returns whether found <property_name> in <obj>, <proto>, or its inheritance chain
  bool set_new_object_property_value(object_type& obj, const string& property_name, data& value)noexcept{
    return 
      // Search local members
      set_new_property_value_SEEK_IN_OBJ(obj.member_names,obj.member_values,obj.method_names,obj.method_values,
                                         !value.is_type(types::fcn),property_name,value) || 
      // Search local methods
      set_new_property_value_SEEK_IN_OBJ(obj.method_names,obj.method_values,obj.member_names,obj.member_values,
                                         value.is_type(types::fcn),property_name,value)  ||
      // Search the prototype & cache the new member/method if found
      set_new_property_value_SEEK_IN_PROTO(obj.proto->member_names, obj, property_name, value) ||
      set_new_property_value_SEEK_IN_PROTO(obj.proto->method_names, obj, property_name, value) ||
      // Search the inherited super & its prototype
      (obj.super && set_new_object_property_value(*obj.super,property_name,value));
  }

  /******************************************************************************
  * OBJECT PROPERTY ACCOUNTING
  ******************************************************************************/

  // Returns whether found <property_name> in <proto> or its inherited prototype
  // NOTE: THIS PROCEDURE MAY THROW DUE TO THE ".copy()" METHOD
  bool prototype_or_super_has_property_name(object_type& obj, const string& property_name, bool& is_member) {
    bool object_has_property_name(object_type&,const string&,bool&);
    // Search the prototype
    for(size_type i = 0, n = obj.proto->member_names.size(); i < n; ++i)
      if(obj.proto->member_names[i] == property_name) {
        // cache accessed prototype member
        obj.member_names.push_back(property_name), obj.member_values.push_back(obj.proto->member_values[i].copy());
        is_member = true;
        return true;
      }
    for(size_type i = 0, n = obj.proto->method_names.size(); i < n; ++i)
      if(obj.proto->method_names[i] == property_name) {
        // cache accessed prototype method
        obj.method_names.push_back(property_name), obj.method_values.push_back(obj.proto->method_values[i]);
        is_member = false;
        return true;
      }
    // Search the inherited prototypes (& in turn their inherited prototypes as well)
    return obj.super && object_has_property_name(*obj.super,property_name,is_member);
  }


  // Returns whether found <property_name> as a member/method in <obj> 
  // If returns true, <property_name> value is in <obj> & <is_member> denotes whether a member or method
  bool object_has_property_name(object_type& obj, const string& property_name, bool& is_member) {
    // Seek members
    for(size_type i = 0, n = obj.member_names.size(); i < n; ++i)
      if(obj.member_names[i] == property_name) {
        is_member = true;
        return true;
      }
    // Seek methods
    for(size_type i = 0, n = obj.method_names.size(); i < n; ++i)
      if(obj.method_names[i] == property_name) {
        is_member = false;
        return true;
      }
    // Seek proto & its inherited prototype
    // => IF FOUND, ADD IT TO THE LOCAL OBJECT INSTANCE
    return prototype_or_super_has_property_name(obj,property_name,is_member);
  }

  /******************************************************************************
  * OBJECT PROPERTY ACCESSING
  ******************************************************************************/

  // Returns <property_name>'s associated  value if found as a member/method in <obj.proto> 
  // NOTE: THIS PROCEDURE MAY THROW DUE TO THE ".copy()" METHOD
  data get_prototype_or_super_property(object_type& obj, const string& property_name, bool& found) {
    data get_object_property(object_type&,const string&,bool&);
    // Search the prototype
    for(size_type i = 0, n = obj.proto->member_names.size(); i < n; ++i)
      if(obj.proto->member_names[i] == property_name) {
        // cache accessed prototype member
        obj.member_names.push_back(property_name), obj.member_values.push_back(obj.proto->member_values[i].copy());
        found = true;
        return *obj.member_values.rbegin();
      }
    for(size_type i = 0, n = obj.proto->method_names.size(); i < n; ++i)
      if(obj.proto->method_names[i] == property_name) {
        // cache accessed prototype method
        obj.method_names.push_back(property_name), obj.method_values.push_back(obj.proto->method_values[i]);
        found = true;
        return *obj.method_values.rbegin();
      }
    // Search the inherited prototypes (& in turn their inherited prototypes as well)
    if(!obj.super) {
      found = false;
      return data();
    }
    return get_object_property(*obj.super,property_name,found);
  }


  // Returns <property_name>'s associated  value if found as a member/method in <obj> 
  data get_object_property(object_type& obj, const string& property_name, bool& found) {
    // Seek members
    for(size_type i = 0, n = obj.member_names.size(); i < n; ++i)
      if(obj.member_names[i] == property_name) {
        found = true;
        return obj.member_values[i];
      }
    // Seek methods
    for(size_type i = 0, n = obj.method_names.size(); i < n; ++i)
      if(obj.method_names[i] == property_name) {
        found = true;
        return obj.method_values[i];
      }
    // Seek proto & its inherited prototype
    // => IF FOUND, ADD IT TO THE LOCAL OBJECT INSTANCE
    return get_prototype_or_super_property(obj,property_name,found);
  }

  /******************************************************************************
  * OBJECT PROPERTY DELETION
  ******************************************************************************/

  // Delete's <property_name> name-value association in <obj> | <obj.super> if present 
  bool delete_object_property(object_type& obj, const string& property_name)noexcept{
    // Seek members
    for(size_type i = 0, n = obj.member_names.size(); i < n; ++i)
      if(obj.member_names[i] == property_name) {
        obj.member_names.erase(obj.member_names.begin()+i);
        obj.member_values.erase(obj.member_values.begin()+i);
        return true;
      }
    // Seek methods
    for(size_type i = 0, n = obj.method_names.size(); i < n; ++i)
      if(obj.method_names[i] == property_name) {
        obj.method_names.erase(obj.method_names.begin()+i);
        obj.method_values.erase(obj.method_values.begin()+i);
        return true;
      }
    // Seek super
    return obj.super && delete_object_property(*obj.super,property_name);
  }
}

#endif