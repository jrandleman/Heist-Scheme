// Author: Jordan Randleman -- jordanran199@gmail.com -- equality.hpp
// => Contains helper functions for "struct data" value equality for the C++ 
//    Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_DATA_EQUALITY_HELPERS_HPP_
#define HEIST_SCHEME_CORE_DATA_EQUALITY_HELPERS_HPP_

/******************************************************************************
* GENERIC DATA EQUALITY METHOD TYPE ALIAS
******************************************************************************/

using DATA_COMPARER = bool(data::*)(const data&)const;

/******************************************************************************
* PAIR EQUALITY HELPERS (ENABLES COMPARING CIRCULAR LISTS)
******************************************************************************/

bool new_cycle_detected(const data& slow, const data& fast, par_type cycle_start)noexcept{
  return !cycle_start && fast.is_type(types::par) && fast.par->second.is_type(types::par) && slow.par == fast.par;
}


void find_cycle_start(const data& slow, const data& fast, par_type& cycle_start)noexcept{
  auto fast_runner = fast;
  cycle_start = slow.par; // seek cycle's start
  while(cycle_start != fast_runner.par)
    cycle_start = cycle_start->second.par,
    fast_runner = fast_runner.par->second.par->second;
}


// Equality proper list recursive helper (p1 || p2 confirmed not to be a circular list)
template<DATA_COMPARER same_as>
bool proper_list_equality_recur(const data& p1, const data& p2){
  // Confirm working with 2 pairs
  if(!p1.is_type(types::par) || !p2.is_type(types::par)) 
    return (p1.*same_as)(p2);
  // Confirm car elts are equal
  if(!(p1.par->first.*same_as)(p2.par->first)) 
    return false;
  // Confirm next 2 elts are pairs
  if(!p1.par->second.is_type(types::par) || !p2.par->second.is_type(types::par)) 
    return (p1.par->second.*same_as)(p2.par->second);
  // Check the rest of the list
  return proper_list_equality_recur<same_as>(p1.par->second, p2.par->second);
}


// Equality list recursive helper
template<DATA_COMPARER same_as>
bool list_equality_recur(const data& slow1, const data& fast1, const data& slow2, const data& fast2, 
                                                      par_type cycle_start1, par_type cycle_start2){
  // Confirm working with 2 pairs
  if(!slow1.is_type(types::par) || !slow2.is_type(types::par)) 
    return (slow1.*same_as)(slow2);
  // Confirm car elts are equal
  if(!(slow1.par->first.*same_as)(slow2.par->first)) 
    return false;
  // Confirm next 2 elts are pairs
  if(!slow1.par->second.is_type(types::par) || !slow2.par->second.is_type(types::par)) 
    return (slow1.par->second.*same_as)(slow2.par->second);
  // If confirmed either list isn't circular, just check the rest of the lists w/o checking for cycles
  if(!fast1.is_type(types::par) || !fast1.par->second.is_type(types::par) || 
     !fast2.is_type(types::par) || !fast2.par->second.is_type(types::par))
    return proper_list_equality_recur<same_as>(slow1.par->second,slow2.par->second);
  // Check if detected a cycle (simultaneously performs Floyd's Loop Detection algorithm)
  if(new_cycle_detected(slow1,fast1,cycle_start1)) find_cycle_start(slow1,fast1,cycle_start1);
  if(new_cycle_detected(slow2,fast2,cycle_start2)) find_cycle_start(slow2,fast2,cycle_start2);
  // Check if at a cycle
  if(slow1.par->second.par == cycle_start1 || slow2.par->second.par == cycle_start2)
    return slow1.par->second.par == cycle_start1 && slow2.par->second.par == cycle_start2;
  // Check the rest of the list
  return list_equality_recur<same_as>(slow1.par->second, fast1.par->second.par->second, 
                                      slow2.par->second, fast2.par->second.par->second, 
                                      cycle_start1, cycle_start2);
}


template<DATA_COMPARER same_as>
bool prm_compare_PAIRs(const par_type& p1, const par_type& p2) {
  return list_equality_recur<same_as>(p1,p1,p2,p2,nullptr,nullptr);
}

/******************************************************************************
* EXPRESSION EQUALITY HELPERS
******************************************************************************/

template<DATA_COMPARER same_as>
bool prm_compare_EXPRs(const exp_type& l1, const exp_type& l2) {
  if(l1.size() != l2.size()) return false;
  for(size_type i = 0, n = l1.size(); i < n; ++i)
    if(!(l1[i].*same_as)(l2[i])) return false;
  return true;
}

/******************************************************************************
* VECTOR EQUALITY HELPERS
******************************************************************************/

template<DATA_COMPARER same_as>
bool prm_compare_VECTs(const vec_type& v1, const vec_type& v2) {
  return prm_compare_EXPRs<same_as>(*v1, *v2);
}

/******************************************************************************
* HASH-MAP EQUALITY HELPERS
******************************************************************************/

template<DATA_COMPARER same_as>
bool prm_compare_HMAPs(const map_type& m1, const map_type& m2) {
  if(m1->val.size() != m2->val.size()) return false;
  for(auto p1=m1->val.begin(), end=m1->val.end(), p2=m2->val.begin(); p1 != end; ++p1, ++p2)
    if(p1->first != p2->first || !(p1->second.*same_as)(p2->second)) return false;
  return true;
}

/******************************************************************************
* OBJECT EQUALITY HELPERS
******************************************************************************/

template<DATA_COMPARER same_as>
bool prm_compare_OBJs(const obj_type& o1, const obj_type& o2) {
  if(o1->proto != o2->proto                               ||
     o1->member_names.size()  != o2->member_names.size()  || 
     o1->member_values.size() != o2->member_values.size() || 
     o1->method_names.size()  != o2->method_names.size()  || 
     o1->method_values.size() != o2->method_values.size()) return false;
  for(size_type i = 0, n = o1->member_names.size(); i < n; ++i)
    if(o1->member_names[i] != o2->member_names[i]) return false;
  for(size_type i = 0, n = o1->method_names.size(); i < n; ++i) // methods ONLY compared by name
    if(o1->method_names[i] != o2->method_names[i]) return false;
  for(size_type i = 0, n = o1->member_values.size(); i < n; ++i)
    if(!(o1->member_values[i].*same_as)(o2->member_values[i])) return false;
  return true;
}

/******************************************************************************
* OBJECT POLYMORPHIC EQUALITY HELPERS
******************************************************************************/

// returns whether found a valid method
bool prm_DYNAMIC_OBJeq(const obj_type& object,const data& rhs,const char* eq_name,bool& result) {
  // Search methods for the "self=" printing polymorphic method
  obj_type obj = object;
  while(obj) {
    // search object's local members
    for(size_type i = 0, n = obj->method_names.size(); i < n; ++i) {
      if(obj->method_names[i] == "self=" || obj->method_names[i] == eq_name) {
        data eq_result = apply_dynamic_method(obj,data_vector(1,rhs),obj->method_values[i].fcn);
        result = eq_result.is_truthy();
        return true;
      }
    }
    // search object's prototype
    for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i) {
      if(obj->proto->method_names[i] == "self=" || obj->proto->method_names[i] == eq_name) {
        obj->method_names.push_back(obj->proto->method_names[i]), obj->method_values.push_back(obj->proto->method_values[i]);
        data eq_result = apply_dynamic_method(obj,data_vector(1,rhs),obj->method_values[i].fcn);
        result = eq_result.is_truthy();
        return true;
      }
    }
    // search inherited object prototype
    obj = obj->super;
  }
  return false;
}

#endif