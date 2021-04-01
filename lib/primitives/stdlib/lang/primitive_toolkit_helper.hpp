// Author: Jordan Randleman -- jrandleman@scu.edu -- primitive_toolkit_helper.hpp
// => Defines helper functions for "../primitive_toolkit.hpp" for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_PRIMITIVE_TOOLKIT_HELPER_HPP_
#define HEIST_SCHEME_CORE_PRIMITIVE_TOOLKIT_HELPER_HPP_

namespace heist {

  /******************************************************************************
  * HELPER PROCEDURE FROM THE FILE THIS FILE ULTIMATELY HELPS IMPLEMENT
  ******************************************************************************/

  namespace primitive_toolkit { bool data_is_nil(const data& d)noexcept; }

  /******************************************************************************
  * DATA LIST PREDICATES HELPER
  ******************************************************************************/

  // Note: the type declaration for "enum class list_status" can be found in 
  //       "lib/core/type_system/scheme_types/data/implementation_helpers/copying.hpp"

  // PRECONDITION: data_is_nil(curr_pair) || curr_pair.is_type(types::par)
  // Uses the 1st half of the Floyd Loop Detection Algorithm (doesn't need to find WHERE the cycle is).
  list_status get_list_status(const data& curr_pair)noexcept{
    data slow = curr_pair, fast = curr_pair;
    while(fast.is_type(types::par) && fast.par->second.is_type(types::par)) {
      slow = slow.par->second;             // move 1 node/iteration
      fast = fast.par->second.par->second; // move 2 nodes/iteration
      if(slow.par == fast.par) break;
    }
    // if found end of the list, return whether ends in '()
    if(!fast.is_type(types::par)) {
      if(primitive_toolkit::data_is_nil(fast)) return list_status::proper;
      return list_status::dotted;
    }
    if(!fast.par->second.is_type(types::par)) {
      if(primitive_toolkit::data_is_nil(fast.par->second)) return list_status::proper;
      return list_status::dotted;
    }
    // if didn't find end of the list, contains a cycle.
    return list_status::circular; 
  }

  /******************************************************************************
  * DATA->SYNTAX TRANSFORMATION HELPERS
  ******************************************************************************/

  namespace primitive_toolkit {
    // Helper prototypes in "primitive_toolkit.hpp"
    bool convert_data_to_evaluable_syntax(const data&,data&)noexcept;
    bool data_is_nil(const data&)noexcept;


    // Determine if data is self-evaluating for <eval>
    bool data_is_self_evaluating_for_EVAL(const data& d)noexcept{
      return d.is_type(types::num) || d.is_type(types::str) || 
             d.is_type(types::chr) || d.is_type(types::bol);
    }


    // Converts the given pair into an expression
    // Returns whether succeeded in transformation
    bool deep_unpack_data_list_into_syntax_expr(const data& p, data_vector& data_as_syntax)noexcept{
      if(p.is_type(types::par)) {
        data_as_syntax.push_back(data());
        return convert_data_to_evaluable_syntax(p.par->first,*data_as_syntax.rbegin()) && 
               deep_unpack_data_list_into_syntax_expr(p.par->second, data_as_syntax);
      } else if(!data_is_nil(p)) {
        data_as_syntax.push_back(symconst::dot);
        data_as_syntax.push_back(data());
        return convert_data_to_evaluable_syntax(p,*data_as_syntax.rbegin());
      }
      return true;
    }


    // Converts the given vector into an expression
    // Returns whether succeeded in transformation
    bool deep_unpack_data_vector_into_syntax_expr(const data& v, data_vector& data_as_syntax)noexcept{
      data_as_syntax.push_back(symconst::vec_literal);
      for(const auto& e : *v.vec) {
        data_as_syntax.push_back(data());
        if(!convert_data_to_evaluable_syntax(e,*data_as_syntax.rbegin()))
          return false;
      }
      return true;
    }


    // Converts the given hmap into an expression
    // Returns whether succeeded in transformation
    bool deep_unpack_data_hmap_into_syntax_expr(const data& m, data_vector& data_as_syntax)noexcept{
      data_as_syntax.push_back(symconst::map_literal);
      for(auto& keyvalue : m.map->val) {
        data_as_syntax.push_back(data());
        if(!convert_data_to_evaluable_syntax(map_object::unhash_key(keyvalue.first),*data_as_syntax.rbegin()))
          return false;
        data_as_syntax.push_back(data());
        if(!convert_data_to_evaluable_syntax(keyvalue.second,*data_as_syntax.rbegin()))
          return false;
      }
      return true;
    }
  } // End of namespace primitive_toolkit
} // End of namespace heist

#endif