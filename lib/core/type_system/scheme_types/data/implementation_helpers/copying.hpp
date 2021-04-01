// Author: Jordan Randleman -- jrandleman@scu.edu -- copying.hpp
// => Contains helper functions for "struct data" value copying for the C++ 
//    Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_DATA_COPYING_HPP_
#define HEIST_SCHEME_CORE_DATA_COPYING_HPP_

/******************************************************************************
* COPYING EXTERNAL HELPER TYPE & FUNCTION
******************************************************************************/

enum class list_status {proper, circular, dotted};
list_status get_list_status(const data& curr_pair)noexcept;

/******************************************************************************
* PAIR DEEP-COPYING HELPERS
******************************************************************************/

template<bool DEEP_COPY>
void copy_cycle_link(data& p, par_type& q, par_type& cycle_start) {
  if constexpr (DEEP_COPY) {
    q->first = p.par->first.copy();
  } else {
    q->first = p.par->first;
  }
  p = p.par->second;
  if(p.par != cycle_start) {
    q->second = make_par();
    q = q->second.par;
  }
}


template<bool DEEP_COPY>
void copy_list_until_cycle_start(data& p, par_type& q, par_type& cycle_start) {
  while(p.par != cycle_start)
    copy_cycle_link<DEEP_COPY>(p,q,cycle_start);
}


template<bool DEEP_COPY>
data copy_circular_list(const data& d) {
  // find the start of the cycle
  data slow = d, fast = d;
  while(fast.is_type(types::par) && fast.par->second.is_type(types::par)) {
    slow = slow.par->second;             // move 1 node/iteration
    fast = fast.par->second.par->second; // move 2 nodes/iteration
    if(slow.par == fast.par) break;
  }
  // By now fast.par is where the cycle starts
  // -> Deep-Copy up to the cycle start, copy the cycle link, then copy again till cycle start
  data root = make_par();
  auto q = root.par;
  auto p = d;
  copy_list_until_cycle_start<DEEP_COPY>(p,q,fast.par);
  auto cycle_start = q;
  copy_cycle_link<DEEP_COPY>(p,q,fast.par); // copy the cycle start
  copy_list_until_cycle_start<DEEP_COPY>(p,q,fast.par);
  q->second = cycle_start;
  return root;
}


template<bool DEEP_COPY>
data copy_non_circular_list(const data& d) {
  // note: guarenteed by data::deep_copy <d.type> ::= types::par
  auto p = d;
  data root = make_par();
  auto q = root.par;
  while(p.is_type(types::par)) {
    if constexpr (DEEP_COPY) {
      q->first = p.par->first.copy();
    } else {
      q->first = p.par->first;
    }
    p = p.par->second;
    if(p.is_type(types::par)) {
      q->second = make_par();
      q = q->second.par;
    }
  }
  if constexpr (DEEP_COPY) {
    q->second = p.copy();
  } else {
    q->second = p;
  }
  return root;
}


data deep_copy_pair(const data& d) {
  switch(get_list_status(d)) {
    case list_status::proper: 
    case list_status::dotted: 
      return copy_non_circular_list<true>(d);
    default: return copy_circular_list<true>(d);
  }
}

/******************************************************************************
* OBJECT DEEP-COPYING HELPERS
******************************************************************************/

// Search methods for the "self->copy" copying method
bool dynamic_object_copy(obj_type obj, data& result) {
  // search object's local members
  for(size_type i = 0, n = obj->method_names.size(); i < n; ++i)
    if(obj->method_names[i] == "self->copy") {
      result = apply_dynamic_method(obj,data_vector(),obj->method_values[i].fcn);
      return true;
    }
  // search object's prototype
  for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i) {
    if(obj->proto->method_names[i] == "self->copy") {
      obj->method_names.push_back(obj->proto->method_names[i]), obj->method_values.push_back(obj->proto->method_values[i]);
      result = apply_dynamic_method(obj,data_vector(),obj->method_values[i].fcn);
      return true;
    }
  }
  return false;
}


data deep_copy_obj(const data& d) {
  // deep copy inherited objects
  object_type o;
  o.proto = d.obj->proto; // shallow copy the prototype (these are never deep copied!)
  if(o.super) o.super = deep_copy_obj(make_obj(*o.super)).obj;
  // check for a custom self->copy
  data result;
  if(dynamic_object_copy(d.obj,result)) return result;
  // apply the default object copying mechanism
  o.member_names = d.obj->member_names;
  o.method_names = d.obj->method_names;
  for(const auto& member_val : d.obj->member_values)
    o.member_values.push_back(member_val.copy());
  for(const auto& method_val : d.obj->method_values)
    o.method_values.push_back(method_val.copy());
  return obj_type(std::move(o));
}

/******************************************************************************
* PAIR SHALLOW-COPYING HELPER
******************************************************************************/

data shallow_copy_pair(const data& d) {
  switch(get_list_status(d)) {
    case list_status::proper: 
    case list_status::dotted: 
      return copy_non_circular_list<false>(d);
    default: return copy_circular_list<false>(d);
  }
}

/******************************************************************************
* OBJECT SHALLOW-COPYING HELPER
******************************************************************************/

data shallow_copy_obj(const data& d) {
  // shallow copy inherited objects
  object_type o;
  o.proto = d.obj->proto; // shallow copy the prototype (these are never deep copied!)
  if(o.super) o.super = shallow_copy_obj(make_obj(*o.super)).obj;
  // apply the default object copying mechanism
  o.member_names = d.obj->member_names;
  o.method_names = d.obj->method_names;
  for(const auto& member_val : d.obj->member_values)
    o.member_values.push_back(member_val);
  for(const auto& method_val : d.obj->method_values)
    o.method_values.push_back(method_val);
  return obj_type(o);
}

#endif