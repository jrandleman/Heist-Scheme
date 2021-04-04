// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Contains methods implementations of "struct environment" for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_ENVIRONMENT_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_ENVIRONMENT_IMPLEMENTATION_HPP_

#include "implementation_helpers/environment_extension.hpp" // defines the "extend_environment" procedure

namespace heist {

  /******************************************************************************
  * ENVIRONMENT VARIABLE LOOKUP
  ******************************************************************************/

  frame_val environment::lookup_variable_value(const frame_var& var, bool& found)const noexcept{
    const auto& objs = objects();
    if(const auto pos = objs.find(var); pos != objs.end()) {
      found = true;
      return pos->second;
    }
    if(parent) return parent->lookup_variable_value(var,found);
    found = false;
    return frame_val();
  }

  /******************************************************************************
  * ENVIRONMENT VARIABLE MUTATION
  ******************************************************************************/

  bool environment::set_variable_value(const frame_var& var, frame_val&& val)noexcept{
    auto& objs = objects();
    if(auto pos = objs.find(var); pos != objs.end()) {
      if(val.is_type(types::fcn) && val.fcn.name.empty()) val.fcn.name = var;
      pos->second = std::move(val);
      return true;
    }
    return parent && parent->set_variable_value(var,std::move(val));
  }

  /******************************************************************************
  * ENVIRONMENT VARIABLE & MACRO DEFINITION
  ******************************************************************************/

  void environment::define_variable(const frame_var& var, frame_val val)noexcept{
    // binding anonymous procedures -> named procedure
    if(val.is_type(types::fcn) && val.fcn.name.empty()) val.fcn.name = var;
    objects()[var] = std::move(val);
  }


  void environment::define_macro(const frame_mac& mac_val)noexcept{
    // extract macro name (either a syntax-rules-object or a syntax-transformer-procedure)
    const auto& mac_name = mac_val.is_type(types::syn) ? mac_val.syn.label : mac_val.fcn.name;
    auto& macs = macros();
    for(auto& mac : macs) {
      if(macro_has_label(mac,mac_name)) {
        mac = mac_val;
        return;
      }
    }
    macs.push_back(mac_val);
  }

  /******************************************************************************
  * GETENV: GET VARIABLE VALUE AS A STRING
  ******************************************************************************/

  string environment::getenv(const frame_var& var, bool& found)const{
    const auto& objs = objects();
    if(const auto pos = objs.find(var); pos != objs.end()) {
      found = true;
      return pos->second.write();
    }
    if(parent) return parent->getenv(var,found);
    found = false;
    return "";
  }

  /******************************************************************************
  * ENVIRONMENT VARIABLE & MACRO QUERYING
  ******************************************************************************/

  bool environment::has_macro(const string& label)const noexcept{
    for(const auto& mac : macros())
      if(macro_has_label(mac,label)) return true;
    return parent && parent->has_macro(label);
  }


  bool environment::has_variable(const frame_var& var)const noexcept{
    return objects().count(var) || (parent && parent->has_variable(var));
  }

  /******************************************************************************
  * ENVIRONMENT VARIABLE & MACRO DELETION
  ******************************************************************************/

  // Returns whether found
  bool environment::erase_variable(const frame_var& var)noexcept{
    return objects().erase(var) || (parent && parent->erase_variable(var));
  }


  // Returns whether found
  bool environment::erase_macro(const string& label)noexcept{
    auto& macs = macros();
    for(size_type i = 0, n = macs.size(); i < n; ++i) {
      if(macro_has_label(macs[i],label)) {
        macs.erase(macs.begin()+i);
        return true;
      }
    }
    return parent && parent->erase_macro(label);
  }

  /******************************************************************************
  * ENVIRONMENT MACRO-LABEL COMPARISON
  ******************************************************************************/

  string environment::macro_label(const frame_mac& mac)noexcept{
    if(mac.is_type(types::syn)) return mac.syn.label;
    if(mac.is_type(types::fcn)) return mac.fcn.name;
    return "";
  }


  bool environment::macro_has_label(const frame_mac& mac, const string& label)noexcept{
    return (mac.is_type(types::syn) && mac.syn.label == label) ||
           (mac.is_type(types::fcn) && mac.fcn.name == label);
  }
}

#endif