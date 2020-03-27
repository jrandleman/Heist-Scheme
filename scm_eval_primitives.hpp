// Author: Jordan Randleman -- scm_eval_primitives.hpp
// => Defines primitive Scheme functions written in C++ for the C++ Scheme Interpreter

// PRIMITIVE FUNCTION MANDATORY TYPE SIGNATURE: 
// -> struct data(*)(scm_list&)

#ifndef SCM_EVAL_PRIMITIVES_HPP_
#define SCM_EVAL_PRIMITIVES_HPP_

#include <sstream>
#include "scm_eval_types.hpp"

/******************************************************************************
* PRIMITIVE HELPER FUNCTION PROTOTYPES
******************************************************************************/

data data_cast(scm_list l);
bool is_true(const scm_list& exp);
scm_list scm_eval(scm_list&& exp, env_type& env);
scm_list execute_application(scm_list&& procedure, scm_list& arguments, env_type& env);
env_type setup_environment();

/******************************************************************************
* PRIMITIVE HELPER FUNCTIONS / CONSTANTS
******************************************************************************/

// ----------------
// Constant Values:
// ----------------

const auto FALSE_DATA_BOOLEAN = data(boolean(false));
const auto TRUE_DATA_BOOLEAN  = data(boolean(true));

// --------------------------
// General Primitive Helpers:
// --------------------------

// Determine whether a primitive was given no args (ie only given the sentinel arg)
bool no_args_given(const scm_list& args) {
  return args.empty() || (args.size()==1 && 
         args[0].is_type(types::sym) && 
         args[0].value.sym == SENTINEL_ARG);
}

// Confirm args only consists of datum w/ type 't'
// POSTCONDITION: returns i >= 0 for i'th index of non-type-t data,
//                else returns -1 if all data is of type t
signed_num confirm_only_args_of_type(const scm_list& args, types t) {
  for(signed_num i = 0, n = args.size(); i < n; ++i)
    if(!args[i].is_type(t)) return i;
  return -1;
}

// Confirm data is a procedure
data primitive_PROCEDUREP(scm_list& args);
void primitive_confirm_data_is_a_procedure(const data& d, const char* name, 
                                           const char* format){
  if(scm_list proc({d}); !primitive_PROCEDUREP(proc).value.bol.val)
    FATAL_ERR("'"<<name<<" primitive 1st arg [ "<<d<<" ] of type \""
      << d.type_name() << "\" isn't a procedure!" << format);
}

// ----------------
// Numeric Helpers:
// ----------------

// Confirms numeric-arg-based primitive given >= 1 arg & ONLY numeric args
void confirm_no_numeric_primitive_errors(const scm_list& args, const char* primitive_name, const char* format) {
  if(no_args_given(args))
    FATAL_ERR("'"<<primitive_name<<" primitive recieved no arguments!\n     "<<format);
  if(auto idx = confirm_only_args_of_type(args,types::num); idx > -1)
    FATAL_ERR("'"<<primitive_name<<" primitive recieved non-numeric argument: "<<args[idx]<<"!\n     "<<format);
}

// Confirms given n arguments
void confirm_n_args(const size_type n, const scm_list& args, const char* primitive_name, const char* format) {
  if(args.size() != n) {
    if(n != 1) FATAL_ERR("'"<<primitive_name<<" primitive didn't receive "<<n<<" arguments!\n     "<<format);
    else       FATAL_ERR("'"<<primitive_name<<" primitive didn't receive 1 argument!\n     "<<format);
  }
}

// Confirms given 1 valid numeric argument
void confirm_unary_numeric(const scm_list& args, const char* primitive_name, const char* format) {
  confirm_no_numeric_primitive_errors(args, primitive_name, format);
  confirm_n_args(1, args, primitive_name, format);
}

// ---------------
// Equalp Helpers:
// ---------------

bool prm_compare_PAIRs(par_type& p1, par_type& p2);
bool prm_compare_VECTs(vec_type& v1, vec_type& v2);

// compares 2 given atomic value fields of types::t -- helper for "equal?"/"eqv?" primitives
bool prm_compare_atomic_values(data_value_field& v1, data_value_field& v2, types t) {
  if(t == types::undefined || t == types::exe || t == types::exp) return true;
  else if(t == types::num) return v1.num == v2.num && v1.num.is_exact() == v2.num.is_exact();
  else if(t == types::chr) return v1.chr == v2.chr;
  else if(t == types::str) return *v1.str == *v2.str;
  else if(t == types::sym) return v1.sym == v2.sym;
  else if(t == types::bol) return v1.bol.val == v2.bol.val;
  else if(t == types::par) return v1.par == v2.par;
  else if(t == types::vec) return v1.vec == v2.vec;
  else if(t == types::prm) return v1.prm == v2.prm;
  else if(t == types::del) return v1.del == v2.del;
  else if(t == types::env) return v1.env == v2.env;
  else if(t == types::cal) return v1.cal == v2.cal;
  return false;
}

// compares 2 given scm_list's -- helper for "equal?" primitive
bool prm_compare_SCM_LISTs(scm_list& l1, scm_list& l2) {
  if(l1.size() != l2.size()) return false;
  for(size_type i = 0, n = l1.size(); i < n; ++i) {
    if(l1[i].type != l2[i].type) return false; // compare types
    if(l1[i].is_type(types::exp)) {            // compare sub-lists
      if(!prm_compare_SCM_LISTs(l1[i].value.exp,l2[i].value.exp))
        return false;
    } else if(l1[i].is_type(types::par)) {
      if(!prm_compare_PAIRs(l1[i].value.par,l2[i].value.par))
        return false;
    } else if(l1[i].is_type(types::vec)) {
      if(!prm_compare_VECTs(l1[i].value.vec,l2[i].value.vec))
        return false;
    } else if(!prm_compare_atomic_values(l1[i].value,l2[i].value,l1[i].type))
      return false;                            // compare values
  }
  return true;
}

// compares 2 given lists (IE PAIRS) -- helper for "equal?" primitive
bool prm_compare_PAIRs(par_type& p1, par_type& p2) {
  if(p1->first.type != p2->first.type || p1->second.type != p2->second.type) 
    return false;
  auto& p1_car = p1->first, &p2_car = p2->first;
  auto& p1_cdr = p1->second, &p2_cdr = p2->second;
  // Compare cars
  if(p1_car.is_type(types::exp)) {
    if(!prm_compare_SCM_LISTs(p1_car.value.exp, p2_car.value.exp))               return false;
  } else if(p1_car.is_type(types::par)) { 
    if(!prm_compare_PAIRs(p1_car.value.par, p2_car.value.par))                   return false;
  } else if(p1_car.is_type(types::vec)) {
    if(!prm_compare_VECTs(p1_car.value.vec, p2_car.value.vec))                   return false;
  } else if(!prm_compare_atomic_values(p1_car.value, p2_car.value, p1_car.type)) return false;
  // Compare cdrs
  if(p1_cdr.is_type(types::exp)) {
    if(!prm_compare_SCM_LISTs(p1_cdr.value.exp, p2_cdr.value.exp))               return false;
  } else if(p1_cdr.is_type(types::par)) {
    if(!prm_compare_PAIRs(p1_cdr.value.par, p2_cdr.value.par))                   return false;
  } else if(p1_cdr.is_type(types::vec)) {
    if(!prm_compare_VECTs(p1_cdr.value.vec, p2_cdr.value.vec))                   return false;
  } else if(!prm_compare_atomic_values(p1_cdr.value, p2_cdr.value, p1_cdr.type)) return false;
  return true;
}

// compares 2 given vectors -- helper for "equal?" primitive
bool prm_compare_VECTs(vec_type& v1, vec_type& v2) {
  return prm_compare_SCM_LISTs(*v1, *v2);
}

// -------------
// Char Helpers:
// -------------

// Confirm given N character/string args
void confirm_given_char_string_args(const scm_list& args, const char* name, const scm_string& type) {
  if(no_args_given(args))
    FATAL_ERR("'"<<name<<" primitive didn't recieved any args: ("<<name<<" <"<<type<<"1> <"<<type<<"2>)");
  auto t = type=="char" ? types::chr : types::str;
  for(const auto& arg : args)
    if(!arg.is_type(t))
      FATAL_ERR("'"<<name<<" primitive recieved non-"<<type<<" arg [ "<<arg<<" ]:"
        << "\n     ("<<name<<" <"<<type<<"1> <"<<type<<"2>)");
}

// Confirm given 1 character arg
void confirm_given_one_char_arg(const scm_list& args, const char* name) {
  if(no_args_given(args) || args.size() != 1)
    FATAL_ERR("'"<<name<<" primitive didn't recieve 1 arg: ("<<name<<" <char>)");
  if(!args[0].is_type(types::chr))
    FATAL_ERR("'"<<name<<" primitive didn't recieve a character arg: ("<<name<<" <char>)"
      << "\n     Recieved [ " << args[0] << " ] of type \"" << args[0].type_name() << "\"!");
}

// -------------------------
// Static Container Helpers:
// -------------------------

enum class list_status {ok, cyclic, no_null};
list_status primitive_list_is_acyclic_and_null_terminated(data& curr_pair);
bool data_is_the_empty_expression(const data& d);

// Confirm given a single string or vector argument
void primitive_confirm_valid_str_or_vec_arg(const scm_list& args,const size_type& n,
                                            const char* name, const char* layout, 
                                            const scm_string& type) {
  // confirm given correct number of args
  if(no_args_given(args))
    FATAL_ERR("'"<<name<<" primitive didn't recieve any arguments!\n     "<<layout);
  if(args.size() != n)
    FATAL_ERR("'"<<name<<" primitive didn't recieve "<<n<<" argument" 
      <<(n>1?"s":"")<<" (given "<<args.size()<<")!\n     "<<layout);
  // confirm given the correct container
  if(!args[0].is_type(type=="string" ? types::str : types::vec))
    FATAL_ERR("'"<<name<<" primitive didn't recieve a "<<type<<" arg!"<<"\n     Variable [ " 
     <<args[0]<<" ] of type \""<<args[0].type_name()<<"\" isn't a "<<type<<"!\n     "<<layout);
}

// Confirm given a valid string/vector index. Returns idx as a 'size_type' if so
size_type primitive_confirm_valid_str_or_vec_idx(const scm_list& args,const char* name, 
                                                 const char* layout,const scm_string& type){
  // confirm given an in-'size_type'-range non-negative index
  if(!args[1].is_type(types::num) || !args[1].value.num.is_integer() || 
      args[1].value.num.is_neg()  || args[1].value.num > MAX_SIZE_TYPE)
    FATAL_ERR("'"<<name<<" primitive didn't recieve a proper non-negative integer index!"
      << "\n     "<<layout<< "\n     <non-neg-integer> range: [0," << MAX_SIZE_TYPE << "]");
  // confirm index falls w/in range of the container
  const size_type i = (size_type)args[1].value.num.extract_inexact();
  const size_type l = (type=="string") ? args[0].value.str->size() : args[0].value.vec->size();
  if(i >= l)
    FATAL_ERR("'"<<name<<" primitive recieved out of bounds index " << i 
      <<"\n     for "<<type<<' '<<args[0]<<" of size "<<l<<"!");
  return i;
}

// Confirm given a proper list to be coerced & return whether the list is empty
bool primitive_validate_list_and_return_if_empty(scm_list& args, const scm_string& name) {
  // confirm valid arg length  
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'"<<name<<" primitive recieved incorrect # of arguments: ("<<name<<" <proper-list>)");
  // return an empty string/vector if given an empty list
  if(data_is_the_empty_expression(args[0])) return true;
  // confirm given a proper list
  if(!args[0].is_type(types::par)) {
    FATAL_ERR("'"<<name<<" primitive isn't given a pair: ("<<name<<" <proper-list>)");
  } else if(auto list_stat = primitive_list_is_acyclic_and_null_terminated(args[0]); 
    list_stat == list_status::cyclic) {
    FATAL_ERR("'"<<name<<" primitive was given a cyclic list: ("<<name<<" <proper-list>)");
  } else if(list_stat == list_status::no_null) {
    FATAL_ERR("'"<<name<<" primitive isn't given a '() terminated list: ("<<name<<" <proper-list>)");
  }
  return false;
}

// ---------------
// String Helpers:
// ---------------

// Confirm given a single string argument
void primitive_confirm_valid_string_arg(const scm_list& args,const size_type& n,
                                        const char* name, const char* layout) {
  primitive_confirm_valid_str_or_vec_arg(args,n,name,layout,"string");
}

// Confirm given a valid string index. Returns idx as a 'size_type' if so
size_type primitive_confirm_valid_string_idx(const scm_list& args,const char* name, 
                                             const char* layout) {
  return primitive_confirm_valid_str_or_vec_idx(args,name,layout,"string");
}

// Returns the given string in lowercase letters
scm_string lowercase_str(const scm_string& s) {
  scm_string tmp;
  for(const auto& ch : s) tmp += mklower(ch);
  return tmp;
}


// -------------
// Pair Helpers:
// -------------

// confirms a list accessor ('car'/'cdr' etc.) is given appropriate args
void confirm_given_a_pair_arg(scm_list& args, const char* primitive_name) {
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'"<<primitive_name<<" primitive recieved incorrect # of arguments!");
  if(!args[0].is_type(types::par))
    FATAL_ERR("'"<<primitive_name<<" primitive isn't given a pair!");
}

// confirm nth car is a pair
void confirm_nth_car_is_pair(const data& args, const char* primitive_name, const char* nth_car) {
  if(!args.value.par->first.is_type(types::par))
    FATAL_ERR("'"<<primitive_name<<" primitive's "<<nth_car<<" 'car' isn't a pair!");
}

// confirm nth cdr is a pair
void confirm_nth_cdr_is_pair(const data& args, const char* primitive_name, const char* nth_cdr) {
  if(!args.value.par->second.is_type(types::par))
    FATAL_ERR("'"<<primitive_name<<" primitive's "<<nth_cdr<<" 'cdr' isn't a pair!");
}

// -------------
// List Helpers:
// -------------

// "list" primitive helper fcn: recursively constructs embedded conses
data primitive_LIST_to_CONS_constructor(const scm_node& obj, const scm_node& null_obj) {
  if(obj == null_obj) 
    return data(symconst::emptylist);
  data new_pair = data(make_par());
  new_pair.value.par->first = *obj;
  new_pair.value.par->second = primitive_LIST_to_CONS_constructor(obj+1,null_obj);
  return new_pair;
}

// "length" primitive helper fcn: recursively computes a list's length
void primitive_LENGTH_computation(const data& curr_pair, num_type& exact_count, size_type count = 1) {
  if(count == MAX_SIZE_TYPE) {
    exact_count += count;
    count = 0;
  }
  if(curr_pair.is_type(types::par)) 
    primitive_LENGTH_computation(curr_pair.value.par->second, exact_count, count+1);
  else exact_count += count;
}

// returns whether 'curr_pair' is the end of a valid 'list' sequence
bool primitive_IS_THE_EMPTY_LIST(const data& curr_pair) {
  return curr_pair.is_type(types::sym) && 
          (curr_pair.value.sym == THE_EMPTY_LIST || 
           curr_pair.value.sym == SENTINEL_ARG);
}

// helper function for 'primitive_LISTP'. Uses the 1st half of the
//   Floyd Loop Detection Algorithm (doesn't need to find WHERE the cycle is).
list_status primitive_list_is_acyclic_and_null_terminated(data& curr_pair) {
  data slow = curr_pair, fast = curr_pair;
  while(fast.is_type(types::par) && fast.value.par->second.is_type(types::par)) {
    slow = slow.value.par->second;                   // move 1 node/iteration
    fast = fast.value.par->second.value.par->second; // move 2 nodes/iteration
    if(slow.value.par == fast.value.par) break;
  }
  // if found end of the list, return whether ends in '()
  if(!fast.is_type(types::par))
    return primitive_IS_THE_EMPTY_LIST(fast) 
            ? list_status::ok 
            : list_status::no_null;
  if(!fast.value.par->second.is_type(types::par))
    return primitive_IS_THE_EMPTY_LIST(fast.value.par->second) 
            ? list_status::ok 
            : list_status::no_null;
  // if didn't find end of the list, contains a cycle.
  return list_status::cyclic; 
}

// "append" primitive helper fcn: recursively links 'curr_pair's last cdr to 'link_to'
void primitive_APPEND_list_linker(data& curr_pair, data& link_to) {
  if(curr_pair.is_type(types::par)) 
    primitive_APPEND_list_linker(curr_pair.value.par->second, link_to);
  else 
    curr_pair = link_to;
}

// "list-tail" primitive helper fcn: recursively seeks kth cdr
data primitive_LIST_TAIL_recur(data& curr_pair, const size_type& k, const size_type& count) {
  if(curr_pair.is_type(types::par) && count == k) return curr_pair;
  if(curr_pair.is_type(types::par) && count < k)
    return primitive_LIST_TAIL_recur(curr_pair.value.par->second, k, count+1);
  return FALSE_DATA_BOOLEAN;
}

// Confirm given list-index pair are compatible
data primitive_confirm_valid_list_index(scm_list& args, const scm_string& name) {
  scm_string format = "\n     ("+name+" <proper-list> <non-negative-integer>)" +
                      scm_string("\n     <non-negative-integer> range: [0,") +
                      std::to_string(MAX_SIZE_TYPE) + "]";
  // confirm valid arg length  
  if(args.size() != 2 || no_args_given(args))
    FATAL_ERR("'"<<name<<" primitive recieved incorrect # of arguments:" << format);
  // confirm given a proper list
  if(!args[0].is_type(types::par))
    FATAL_ERR("'"<<name<<" primitive isn't given a pair:" << format);
  else if(auto list_stat = primitive_list_is_acyclic_and_null_terminated(args[0]); 
    list_stat == list_status::cyclic) {
    FATAL_ERR("'"<<name<<" primitive was given a cyclic list:" << format);
  } else if(list_stat == list_status::no_null)
    FATAL_ERR("'"<<name<<" primitive isn't given a '() terminated list:" << format);
  // confirm given an in-'size_type'-range non-negative index
  if(!args[1].is_type(types::num) || !args[1].value.num.is_integer() || 
      args[1].value.num.is_neg()  || args[1].value.num > MAX_SIZE_TYPE)
    FATAL_ERR("'"<<name<<" primitive didn't recieve a proper non-negative integer index!" << format);
  // confirm index falls w/in range of the container
  const size_type k = (size_type)args[1].value.num.extract_inexact();
  auto tail = primitive_LIST_TAIL_recur(args[0],k,0);
  if(tail.is_type(types::bol))
    FATAL_ERR("'"<<name<<" primitive's given index " << k 
      << " is out of bounds for list " << args[0] << "!");
  return tail;
}

// ------------------------------
// List Control Features Helpers:
// ------------------------------

// Compute length of a guarenteed 'data' list 
//   (avoids redundant checks for 'data' not being a list)
num_type primitive_guarenteed_list_length(const data& d) {
  if(data_is_the_empty_expression(d)) return num_type();
  num_type count;
  primitive_LENGTH_computation(d.value.par->second,count);
  return count;
}

// Confirm lists are proper & have the same length
data primitive_LISTP(scm_list& args);
void primitive_confirm_proper_same_sized_lists(const scm_list& lists, 
                                               const char* name, const char* format,
                                               const int& first_arg_pos){
  num_type length0;
  for(size_type i = 0, n = lists.size(); i < n; ++i) {
    // confirm proper list
    if(scm_list arg_list({lists[i]}); !primitive_LISTP(arg_list).value.bol.val)
      FATAL_ERR("'"<<name<<" primitive arg #" << first_arg_pos+i+1 << " of type \""
        << lists[i].type_name() << "\" isn't a proper list!" << format);
    // confirm congruent length
    if(i == 0) 
      length0 = primitive_guarenteed_list_length(lists[i]);
    else if(length0 != primitive_guarenteed_list_length(lists[i]))
      FATAL_ERR("'"<<name<<" lists "<< lists[0] << " and " 
        << lists[i] << " differ in length!" << format);
  }
}

// "for-each" primitive helper fcn: recursively applies 'proc' to each 'curr_pair'
void primitive_FOR_EACH_applicator(scm_list& curr_pairs, scm_list& proc, env_type& env) {
  // Return if fully iterated through each list
  if(!curr_pairs[0].is_type(types::par)) return;
  scm_list args(curr_pairs.size());
  // Add each arg for 'proc' & advance each list's head ptr
  for(size_type i = 0, n = curr_pairs.size(); i < n; ++i) {
    args[i] = curr_pairs[i].value.par->first;
    curr_pairs[i] = curr_pairs[i].value.par->second;
  }
  // Execute proc & recurse down the rest of the lists
  execute_application(std::move(proc),args,env);
  primitive_FOR_EACH_applicator(curr_pairs, proc, env);
}

// "map" primitive helper fcn: recursively applies 'proc' to each 'curr_pair' 
//   & stores the result
void primitive_MAP_list_constructor(scm_list& curr_pairs, scm_list& proc, 
                                    scm_list& mapped_list, env_type& env) {
  // Return if fully iterated through each list
  if(!curr_pairs[0].is_type(types::par)) return;
  scm_list args(curr_pairs.size());
  // Add each arg for 'proc' & advance each list's head ptr
  for(size_type i = 0, n = curr_pairs.size(); i < n; ++i) {
    args[i] = curr_pairs[i].value.par->first;
    curr_pairs[i] = curr_pairs[i].value.par->second;
  }
  // Execute proc, store result, & recurse down the rest of the lists
  mapped_list.push_back(data_cast(execute_application(std::move(proc),args,env)));
  primitive_MAP_list_constructor(curr_pairs, proc, mapped_list, env);
}

// "filter" primitive helper fcn: recursively applies 'proc' to each 
//   'curr_pair', and if result is not false stores the 'curr_pair'
void primitive_FILTER_list_constructor(data& curr_pair, scm_list& proc, 
                                       scm_list& filtered_list, env_type& env) {
  // Return if fully iterated through the list
  if(!curr_pair.is_type(types::par)) return;
  // Execute proc, store result, & recurse down the rest of the lists
  scm_list arg({curr_pair.value.par->first});
  if(is_true(execute_application(std::move(proc),arg,env)))
    filtered_list.push_back(curr_pair.value.par->first);
  // Recurse through the rest of the list
  primitive_FILTER_list_constructor(curr_pair.value.par->second, proc, filtered_list, env);
}

// "fold-left" & "fold-right" primitives helper fcn: recursively applies 'proc' 
//   to each 'curr_pair', and accumulates their result in 'init_val'
void primitive_FOLD_accumulator(scm_list curr_pairs, scm_list& proc, 
                                 data& init_val, env_type& env, 
                                 const bool& folding_left) {
  // Return if fully iterated through each list
  if(!curr_pairs[0].is_type(types::par)) return;
  scm_list args;
  // Add each arg for 'proc' & advance each list's head ptr
  for(auto& list_head : curr_pairs) {
    args.push_back(list_head.value.par->first);
    list_head = list_head.value.par->second;
  }
  // Execute proc, accumulate result, & recurse down the rest of the lists
  if(folding_left) { // fold-left is preorder
    args.insert(args.begin(), init_val);
    init_val = data_cast(execute_application(std::move(proc),args,env));
  }
  primitive_FOLD_accumulator(curr_pairs,proc,init_val,env,folding_left);
  if(!folding_left) { // fold-right is postorder
    args.insert(args.end(), init_val);
    init_val = data_cast(execute_application(std::move(proc),args,env));
  }
}

// primitive "fold-left" & "fold-right" procedure helper template:
data primitive_FOLD_template(scm_list& args, const scm_string& name, const char* format) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given correct # of args needed
  if(args.size() < 3) 
    FATAL_ERR("'"<<name<<" primitive recieved insufficient args (only " 
      << (no_args_given(args) ? 0 : args.size()) << "):" << format);
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0],name.c_str(),format);
  // Confirm only given proper lists of the same length
  scm_list list_heads(args.begin()+2, args.end());
  primitive_confirm_proper_same_sized_lists(list_heads,name.c_str(),format,2);
  // Apply the procedure on each elt of each list, & accumulate the 
  data init_val = args[1];
  primitive_FOLD_accumulator(list_heads,args[0].value.exp,init_val,env,(name=="fold-left"));
  return init_val; // return the accumulated value
}

// "merge" primitive helper fcn: recursively applies 'proc' to each 'curr_pair' 
//   & stores the args as per the result
void primitive_MERGE_list_constructor(scm_list& curr_pairs, scm_list& proc, 
                                    scm_list& merged_list, env_type& env) {
  // If fully iterated both lists, return
  if(!curr_pairs[0].is_type(types::par) && !curr_pairs[1].is_type(types::par)) 
    return;
  // If fully iterated through 1 list, append all the elts of the non-empty 
  //   list & return
  if(!curr_pairs[0].is_type(types::par) || !curr_pairs[1].is_type(types::par)) {
    auto non_empty_list = !curr_pairs[0].is_type(types::par) ? curr_pairs[1] : curr_pairs[0];
    while(non_empty_list.is_type(types::par)) {
      merged_list.push_back(non_empty_list.value.par->first);
      non_empty_list = non_empty_list.value.par->second;
    }
    return;
  }
  // Test proc, merge appropriate arg, & recurse down the rest of the lists
  scm_list args({curr_pairs[0].value.par->first, curr_pairs[1].value.par->first});
  if(is_true(execute_application(std::move(proc),args,env))) {
    merged_list.push_back(args[0]);
    curr_pairs[0] = curr_pairs[0].value.par->second;
  } else {
    merged_list.push_back(args[1]);
    curr_pairs[1] = curr_pairs[1].value.par->second;
  }
  primitive_MERGE_list_constructor(curr_pairs, proc, merged_list, env);
}

// -------------------------------
// List Member Extraction Helpers:
// -------------------------------

// "member" "memv" "memq" primitive helper: recursively compares cars to 'obj'
//   & returns a sublist w/ 'obj' as its 'car' if found. Else returns #f
data primitive_MEM_car_comparison(data& curr_pair, const data& obj, const prm_type& equality_fcn) {
  if(!curr_pair.is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  if(scm_list args({curr_pair.value.par->first, obj}); equality_fcn(args).value.bol.val)
    return curr_pair;
  return primitive_MEM_car_comparison(curr_pair.value.par->second, obj, equality_fcn);
}

// Template helper fcn for the "member" "memv" "memq" primitives.
data primitive_MEM_template(scm_list& args, const char* name, const prm_type& equality_fcn) {
  // Confirm given the correct # of args
  if(args.size() != 2)
    FATAL_ERR("'"<<name<<" primitive recieved incorrect # of args:\n     ("<<name<<" <obj> <proper-list>)");
  // (<mem> <obj> '()) = #f
  if(primitive_IS_THE_EMPTY_LIST(args[1]))
    return FALSE_DATA_BOOLEAN;
  // Confirm given a proper list
  if(scm_list list_arg({args[1]}); !primitive_LISTP(list_arg).value.bol.val)
    FATAL_ERR("'"<<name<<" primitive arg #2 [ "<<args[1]<<" ] of type \""<<args[1].type_name()<<"\" isn't a proper list!"
      << "\n     ("<<name<<" <obj> <proper-list>)");
  // Get the sublist w/ 'obj' at its head (if exists)
  return primitive_MEM_car_comparison(args[1], args[0], equality_fcn);
}

// "assoc" "assv" "assq" primitive helper: recursively compares pairs' cars to 
//   'obj' & returns a pair w/ 'obj' as its 'car' if found. If finds a non-pair, 
//   throws an error. Else returns #f.
data primitive_ASSOCIATION_key_seeker(data& curr_pair, const data& obj, const data& head, const char* name, const prm_type& equality_fcn) {
  if(!curr_pair.is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  if(!curr_pair.value.par->first.is_type(types::par))
    FATAL_ERR("'"<<name<<" primitive arg #2 "<<head<<" isn't a proper association list (list of pairs)!"
      << "\n     ("<<name<<" <obj> <proper-association-list>)");
  if(scm_list args({curr_pair.value.par->first.value.par->first, obj}); equality_fcn(args).value.bol.val)
    return curr_pair.value.par->first;
  return primitive_ASSOCIATION_key_seeker(curr_pair.value.par->second, obj, head, name, equality_fcn);
}

// Template helper fcn for the "assoc" "assv" "assq" primitives.
data primitive_ASSOCIATION_template(scm_list& args, const char* name, const prm_type& equality_fcn) {
  // Confirm given the correct # of args
  if(args.size() != 2)
    FATAL_ERR("'"<<name<<" primitive recieved incorrect # of args:\n     ("<<name<<" <obj> <proper-association-list>)");
  // (<mem> <obj> '()) = #f
  if(primitive_IS_THE_EMPTY_LIST(args[1]))
    return FALSE_DATA_BOOLEAN;
  // Confirm given a proper list
  if(scm_list list_arg({args[1]}); !primitive_LISTP(list_arg).value.bol.val)
    FATAL_ERR("'"<<name<<" primitive arg #2 [ "<<args[1]<<" ] of type \""<<args[1].type_name()<<"\" isn't a proper list!"
      << "\n     ("<<name<<" <obj> <proper-association-list>)");
  // Get the sublist w/ 'obj' at its head (if exists)
  return primitive_ASSOCIATION_key_seeker(args[1], args[0], args[1], name, equality_fcn);
}

// ---------------
// Vector Helpers:
// ---------------

// Confirm given a single vector argument
void primitive_confirm_valid_vector_arg(const scm_list& args,const size_type& n,
                                        const char* name, const char* layout) {
  primitive_confirm_valid_str_or_vec_arg(args,n,name,layout,"vector");
}

// Confirm given a valid vector index. Returns idx as a 'size_type' if so
size_type primitive_confirm_valid_vector_idx(const scm_list& args,const char* name, 
                                             const char* layout) {
  return primitive_confirm_valid_str_or_vec_idx(args,name,layout,"vector");
}

// -------------------
// Eval/Apply Helpers:
// -------------------

// [ EVAL ] Confirms whether given data is the AST's repn of ()
bool data_is_the_empty_expression(const data& d) {
  return d.is_type(types::sym) && d.value.sym==THE_EMPTY_LIST;
}

// [ EVAL ] Converts the given pair into an expression ('deep' b/c it 
//   also recursively converts ALL nested pairs into expressions too)
void deep_unpack_list_into_exp(data& curr_pair, scm_list& args_list) {
  if(curr_pair.is_type(types::par)) {
    // Recursively unpack nested lists
    if(curr_pair.value.par->first.is_type(types::par)) {
      scm_list nested_list;
      deep_unpack_list_into_exp(curr_pair.value.par->first, nested_list);
      args_list.push_back(nested_list);
    // Convert the empty list to an empty expression
    } else if(data_is_the_empty_expression(curr_pair.value.par->first)) {
      args_list.push_back(scm_list({}));
    // Unpack atomic obj
    } else {
      args_list.push_back(curr_pair.value.par->first);
    }
    deep_unpack_list_into_exp(curr_pair.value.par->second, args_list); 
  } else if(!primitive_IS_THE_EMPTY_LIST(curr_pair)) {
    args_list.push_back(symconst::period);
    args_list.push_back(curr_pair);
  }
}

// [ APPLY ] Converts the given pair into an expression ('shallow' b/c it 
//   doesn't recursively convert ALL nested pairs into expressions too) 
void shallow_unpack_list_into_exp(data& curr_pair, scm_list& args_list) {
  if(curr_pair.is_type(types::par)) {
    args_list.push_back(curr_pair.value.par->first);
    shallow_unpack_list_into_exp(curr_pair.value.par->second, args_list); 
  }
}

// ---------------------
// Type-Checking Helper:
// ---------------------

void confirm_given_one_arg(const scm_list& args, const char* name) {
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'"<<name<<" primitive recieved incorrect # of arguments: ("<<name<<" <obj>)");
}

// ----------------------
// Type Coercion Helpers:
// ----------------------

// Determine whether input[i] is at a hex value.
// POSTCONDITION: i is returned if input[i] is _NOT_ at a hex value.
//                else, the index of the hex value's closing ';' is returned.
size_type is_symbol_hex_val(size_type i, const size_type& n, const scm_string& input) {
  if(i < n-2 && input[i] == '\\' && input[i+1] == 'x') {
    auto j = i+2; // mv past the '\x' prefix
    while(input[j] && isalnum(input[j])) ++j;
    return (input[j] == ';') ? j : i;
  }
  return i;
}

// primitive "symbol->string" conversion helper
scm_string convert_symbol_to_string(const scm_string& string_val) {
  if(string_val.size() <= 2) return string_val;
  scm_string symbol_str;
  for(size_type i = 0, n = string_val.size(); i < n; ++i) {
    if(auto end_hex_idx = is_symbol_hex_val(i,n,string_val); end_hex_idx!=i){
      // i+1 [below] skips past prefixing '\'
      scm_string hex_num(string_val.begin()+i+1, string_val.begin()+end_hex_idx);
      // convert hex# string -> int -> char
      symbol_str += char(std::stoi("0"+hex_num,nullptr,16)); 
      i = end_hex_idx;
    } else {
      symbol_str += string_val[i];
    }
  }
  return symbol_str;
}

// primitive "string->symbol" conversion helper
scm_string convert_string_to_symbol(const scm_string& symbol_val) {
  if(symbol_val.empty()) return symbol_val;
  std::ostringstream symbol_str;
  symbol_str << std::hex << std::uppercase;
  const scm_string hex_chars = R"(()[]{}`'",;\)";

  // Convert chars in the string to their symbolic versions
  for(const auto& ch : symbol_val) {
    if(hex_chars.find(ch) != scm_string::npos || isspace(ch)) 
      symbol_str << "\\x" << unsigned(ch) << ';';
    else 
      symbol_str << ch;
  }
  return symbol_str.str();
}

/******************************************************************************
* ARITHMETIC PRIMITIVES
******************************************************************************/

// primitive "+" procedure
data primitive_ADD(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "+", "(+ <num1> <num2> ...)");
  num_type sum = 0;
  for(size_type i = 0, n = args.size(); i < n; ++i)
    sum += args[i].value.num;
  return data(sum);
}

// primitive "-" procedure: BOTH NEGATION & SUBTRACTION
data primitive_SUB(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "-", "(- <num1> <num2> ...)");
  num_type difference = args[0].value.num;
  if(args.size()==1) return -1 * difference;        // negation
  for(size_type i = 1, n = args.size(); i < n; ++i) // subtraction
    difference -= args[i].value.num;
  return data(difference);
}

// primitive "*" procedure:
data primitive_MUL(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "*", "(* <num1> <num2> ...)");
  num_type product = 1;
  for(size_type i = 0, n = args.size(); i < n; ++i)
    product *= args[i].value.num;
  return data(product);
}

// primitive "/" procedure:
data primitive_DIV(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "/", "(/ <num1> <num2> ...)");
  if(args.size()==1) return data(1 / args[0].value.num);
  num_type dividend = args[0].value.num;
  for(size_type i = 1, n = args.size(); i < n; ++i)
    dividend /= args[i].value.num;
  return data(dividend);
}

/******************************************************************************
* MISCELLANEOUS NUMERIC PRIMITIVE OPERATIONS 
******************************************************************************/

// primitive "abs" procedure
data primitive_ABS(scm_list& args) {
  confirm_unary_numeric(args, "abs", "(abs <num>)");
  return data(args[0].value.num.abs());
}

// primitive "expt" procedure
data primitive_EXPT(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "expt", "(expt <num1> <num2>)");
  confirm_n_args(2, args, "expt", "(expt <num1> <num2>)");
  return data((args[0].value.num ^ args[1].value.num));
}

// primitive "max" procedure
data primitive_MAX(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "max", "(max <num1> <num2> ...)");
  num_type max = args[0].value.num;
  for(size_type i = 1, n = args.size(); i < n; ++i)
    if(args[i].value.num > max)
      max = args[i].value.num;
  return max;
}

// primitive "min" procedure
data primitive_MIN(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "min", "(min <num1> <num2> ...)");
  num_type min = args[0].value.num;
  for(size_type i = 1, n = args.size(); i < n; ++i)
    if(args[i].value.num < min)
      min = args[i].value.num;
  return min;
}

// primitive "quotient" procedure
data primitive_QUOTIENT(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "quotient", "(quotient <num1> <num2>)");
  confirm_n_args(2, args, "quotient", "(quotient <num1> <num2>)");
  return data(args[0].value.num.quotient(args[1].value.num));
}

// primitive "remainder" procedure
data primitive_REMAINDER(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "remainder", "(remainder <num1> <num2>)");
  confirm_n_args(2, args, "remainder", "(remainder <num1> <num2>)");
  return data((args[0].value.num % args[1].value.num));
}

// primitive "modulo" procedure
data primitive_MODULO(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "modulo", "(modulo <num1> <num2>)");
  confirm_n_args(2, args, "modulo", "(modulo <num1> <num2>)");
  return data(args[0].value.num.modulo(args[1].value.num));
}

// primitive "exp" procedure
data primitive_EXP(scm_list& args) {
  confirm_unary_numeric(args, "exp", "(exp <num>)");
  return data(args[0].value.num.exp());
}

// primitive "log" procedure -- NATURAL LOGARITHM
data primitive_LOG(scm_list& args) {
  confirm_unary_numeric(args, "log", "(log <num>)");
  return data(args[0].value.num.log());
}

// primitive "sqrt" procedure
data primitive_SQRT(scm_list& args) {
  confirm_unary_numeric(args, "sqrt", "(sqrt <num>)");
  return data(args[0].value.num.sqrt());
}

// primitive "gcd" procedure
data primitive_GCD(scm_list& args) {
  if(no_args_given(args)) return num_type(0);
  confirm_no_numeric_primitive_errors(args, "gcd", "(gcd <num1> <num2> ...)");
  if(args.size() == 1) return args[0];
  // GCD is associative
  num_type gcd_val(args[0].value.num);
  for(size_type i = 1, n = args.size(); i < n; ++i)
    gcd_val = gcd_val.gcd(args[i].value.num);
  return gcd_val;
}

// primitive "lcm" procedure
data primitive_LCM(scm_list& args) {
  if(no_args_given(args)) return num_type(1);
  confirm_no_numeric_primitive_errors(args, "lcm", "(lcm <num1> <num2> ...)");
  if(args.size() == 1) return args[0];
  // LCM is associative
  num_type lcm_val(args[0].value.num);
  for(size_type i = 1, n = args.size(); i < n; ++i)
    lcm_val = lcm_val.lcm(args[i].value.num);
  return lcm_val;
}

/******************************************************************************
* MISCELLANEOUS NUMERIC PREDICATE PRIMITIVES 
******************************************************************************/

// primitive "odd?" procedure
data primitive_ODDP(scm_list& args) {
  confirm_unary_numeric(args, "odd?", "(odd? <num>)");
  return data(boolean(args[0].value.num.is_odd()));
}

// primitive "even?" procedure
data primitive_EVENP(scm_list& args) {
  confirm_unary_numeric(args, "even?", "(even? <num>)");
  return data(boolean(args[0].value.num.is_even()));
}

// primitive "positive?" procedure
data primitive_POSITIVEP(scm_list& args) {
  confirm_unary_numeric(args, "positive?", "(positive? <num>)");
  return data(boolean(args[0].value.num.is_pos()));
}

// primitive "negative?" procedure
data primitive_NEGATIVEP(scm_list& args) {
  confirm_unary_numeric(args, "negative?", "(negative? <num>)");
  return data(boolean(args[0].value.num.is_neg()));
}

// primitive "zero?" procedure
data primitive_ZEROP(scm_list& args) {
  confirm_unary_numeric(args, "zero?", "(zero? <num>)");
  return data(boolean(args[0].value.num.is_zero()));
}

/******************************************************************************
* NUMERIC ROUNDING PRIMITIVES 
******************************************************************************/

// primitive "ceiling" procedure -- ROUNDS UP
data primitive_CEILING(scm_list& args) {
  confirm_unary_numeric(args, "ceiling", "(ceiling <num>)");
  return data(args[0].value.num.ceil());
}

// primitive "floor" procedure -- ROUNDS DOWN
data primitive_FLOOR(scm_list& args) {
  confirm_unary_numeric(args, "floor", "(floor <num>)");
  return data(args[0].value.num.floor());
}

// primitive "truncate" procedure -- ROUNDS TOWARDS ZERO
data primitive_TRUNCATE(scm_list& args) {
  confirm_unary_numeric(args, "truncate", "(truncate <num>)");
  return data(args[0].value.num.trunc());
}

// primitive "round" procedure -- ROUNDS TOWARDS THE NEAREST INT
data primitive_ROUND(scm_list& args) {
  confirm_unary_numeric(args, "round", "(round <num>)");
  return data(args[0].value.num.round());
}

/******************************************************************************
* NUMERIC PRECISION COERCION & INTEGER ANALYSIS PRIMITIVES 
******************************************************************************/

// primitive "inexact->exact" procedure
data primitive_COERCE_INEXACT_TO_EXACT(scm_list& args) {
  confirm_unary_numeric(args, "inexact->exact", "(inexact->exact <num>)");
  return data(args[0].value.num.to_exact());
}

// primitive "exact->inexact" procedure
data primitive_COERCE_EXACT_TO_INEXACT(scm_list& args) {
  confirm_unary_numeric(args, "exact->inexact", "(exact->inexact <num>)");
  return data(args[0].value.num.to_inexact());
}

// primitive "exact?" procedure
data primitive_EXACTP(scm_list& args) {
  confirm_unary_numeric(args, "exact?", "(exact? <num>)");
  return data(boolean(args[0].value.num.is_exact()));
}

// primitive "inexact?" procedure
data primitive_INEXACTP(scm_list& args) {
  confirm_unary_numeric(args, "inexact?", "(inexact? <num>)");
  return data(boolean(args[0].value.num.is_inexact()));
}

// primitive "integer?" procedure
data primitive_INTEGERP(scm_list& args) {
  confirm_unary_numeric(args, "integer?", "(integer? <num>)");
  return data(boolean(args[0].value.num.is_integer()));
}

// primitive "numerator" procedure
data primitive_NUMERATOR(scm_list& args) {
  confirm_unary_numeric(args, "numerator", "(numerator <num>)");
  return data(args[0].value.num.extract_numerator());
}

// primitive "denominator" procedure
data primitive_DENOMINATOR(scm_list& args) {
  confirm_unary_numeric(args, "denominator", "(denominator <num>)");
  return data(args[0].value.num.extract_denominator());
}

/******************************************************************************
* NUMERIC TRIGONOMETRIC PRIMITIVES -- IN RADIANS
******************************************************************************/

// primitive "sin" procedure
data primitive_SIN(scm_list& args) {
  confirm_unary_numeric(args, "sin", "(sin <num>)");
  return data(args[0].value.num.sin());
}

// primitive "cos" procedure
data primitive_COS(scm_list& args) {
  confirm_unary_numeric(args, "cos", "(cos <num>)");
  return data(args[0].value.num.cos());
}

// primitive "tan" procedure
data primitive_TAN(scm_list& args) {
  confirm_unary_numeric(args, "tan", "(tan <num>)");
  return data(args[0].value.num.tan());
}

// primitive "asin" procedure
data primitive_ASIN(scm_list& args) {
  confirm_unary_numeric(args, "asin", "(asin <num>)");
  return data(args[0].value.num.asin());
}

// primitive "acos" procedure
data primitive_ACOS(scm_list& args) {
  confirm_unary_numeric(args, "acos", "(acos <num>)");
  return data(args[0].value.num.acos());
}

// primitive "atan" procedure
data primitive_ATAN(scm_list& args) {
  confirm_unary_numeric(args, "atan", "(atan <num>)");
  return data(args[0].value.num.atan());
}

// primitive "sinh" procedure
data primitive_SINH(scm_list& args) {
  confirm_unary_numeric(args, "sinh", "(sinh <num>)");
  return data(args[0].value.num.sinh());
}

// primitive "cosh" procedure
data primitive_COSH(scm_list& args) {
  confirm_unary_numeric(args, "cosh", "(cosh <num>)");
  return data(args[0].value.num.cosh());
}

// primitive "tanh" procedure
data primitive_TANH(scm_list& args) {
  confirm_unary_numeric(args, "tanh", "(tanh <num>)");
  return data(args[0].value.num.tanh());
}

// primitive "asinh" procedure
data primitive_ASINH(scm_list& args) {
  confirm_unary_numeric(args, "asinh", "(asinh <num>)");
  return data(args[0].value.num.asinh());
}

// primitive "acosh" procedure
data primitive_ACOSH(scm_list& args) {
  confirm_unary_numeric(args, "acosh", "(acosh <num>)");
  return data(args[0].value.num.acosh());
}

// primitive "atanh" procedure
data primitive_ATANH(scm_list& args) {
  confirm_unary_numeric(args, "atanh", "(atanh <num>)");
  return data(args[0].value.num.atanh());
}

/******************************************************************************
* NUMERIC RANDOM NUMBER GENERATOR PRIMITIVES 
******************************************************************************/

// primitive "random" procedure -- SEED DEFAULTS TO CURRENT TIME SINCE EPOCH
data primitive_RANDOM(scm_list& args) {
  if(args.size() > 1)
    FATAL_ERR("'random primitive recieved more than 1 argument: (random <optional-numeric-seed>)");
  if(!no_args_given(args) && args.size()==1 && !args[0].is_type(types::num))
    FATAL_ERR("'random primitive recieved non-numeric argument: (random <optional-numeric-seed>)");
  if(no_args_given(args))
    return data(num_type::random());
  return data(num_type::random(args[0].value.num));
}

/******************************************************************************
* NUMERIC COMPARISON PRIMITIVES
******************************************************************************/

// primitive "=" procedure:
data primitive_EQ(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "=", "(= <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num != args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive ">" procedure:
data primitive_GT(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, ">", "(> <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num <= args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "<" procedure:
data primitive_LT(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "<", "(< <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num >= args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive ">=" procedure:
data primitive_GTE(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, ">=", "(>= <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num < args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "<=" procedure:
data primitive_LTE(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "<=", "(<= <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num > args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

/******************************************************************************
* GENERAL COMPARISON PRIMITIVES
******************************************************************************/

// primitive "eq?" procedure:
data primitive_EQP(scm_list& args) {
  if(no_args_given(args))
    FATAL_ERR("'eq? primitive recieved no arguments: (eq? <obj1> <obj2> ...)");
  // compare types & values
  for(size_type i = 0, n = args.size(); i+1 < n; ++i) {
    if(args[i].type != args[i+1].type) 
      return FALSE_DATA_BOOLEAN;
    if(args[i].type == types::str) { // compare strings via pointers
      if(args[i].value.str != args[i+1].value.str)
        return FALSE_DATA_BOOLEAN;
    } else if(!prm_compare_atomic_values(args[i].value,args[i+1].value,args[i].type)) {
      return FALSE_DATA_BOOLEAN;
    }
  }
  return TRUE_DATA_BOOLEAN;
}

// primitive "eqv?" procedure:
data primitive_EQVP(scm_list& args) {
  if(no_args_given(args))
    FATAL_ERR("'eqv? primitive recieved no arguments: (eqv? <obj1> <obj2> ...)");
  // compare types & values
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].type != args[i+1].type || !prm_compare_atomic_values(args[i].value,args[i+1].value,args[i].type))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "equal?" procedure:
data primitive_EQUALP(scm_list& args) {
  if(no_args_given(args))
    FATAL_ERR("'equal? primitive recieved no arguments: (equal? <obj1> <obj2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i) {
    if(args[i].type != args[i+1].type) // compare types
      return FALSE_DATA_BOOLEAN;
    if(args[i].is_type(types::exp)) {  // compare sub-lists
      if(!prm_compare_SCM_LISTs(args[i].value.exp,args[i+1].value.exp))
        return FALSE_DATA_BOOLEAN;
    } else if(args[i].is_type(types::par)) {
      if(!prm_compare_PAIRs(args[i].value.par,args[i+1].value.par))
        return FALSE_DATA_BOOLEAN;
    } else if(args[i].is_type(types::vec)) {
      if(!prm_compare_VECTs(args[i].value.vec,args[i+1].value.vec))
        return FALSE_DATA_BOOLEAN;
    } else if(!prm_compare_atomic_values(args[i].value,args[i+1].value,args[i].type))
        return FALSE_DATA_BOOLEAN;     // compare values
  }
  return TRUE_DATA_BOOLEAN;
}

// primitive "not" procedure:
data primitive_NOT(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'not primitive recieved incorrect # of arguments: (not <obj>)");
  bool data_is_false = args[0].is_type(types::bol) && !args[0].value.bol.val;
  return data(boolean(data_is_false));
}

/******************************************************************************
* CHARACTER PRIMITIVES
******************************************************************************/

// -----------------
// Char Comparators:
// -----------------

// primitive "char=?" procedure:
data primitive_CHAR_EQ(scm_list& args) {
  confirm_given_char_string_args(args, "char=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr != args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char<?" procedure:
data primitive_CHAR_LT(scm_list& args) {
  confirm_given_char_string_args(args, "char<?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr >= args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char>?" procedure:
data primitive_CHAR_GT(scm_list& args) {
  confirm_given_char_string_args(args, "char>?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr <= args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char<=?" procedure:
data primitive_CHAR_LTE(scm_list& args) {
  confirm_given_char_string_args(args, "char<=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr > args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char>=?" procedure:
data primitive_CHAR_GTE(scm_list& args) {
  confirm_given_char_string_args(args, "char>=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr < args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci=?" procedure:
data primitive_CHAR_CI_EQ(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) != mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci<?" procedure:
data primitive_CHAR_CI_LT(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci<?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) >= mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci>?" procedure:
data primitive_CHAR_CI_GT(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci>?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) <= mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci<=?" procedure:
data primitive_CHAR_CI_LTE(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci<=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) > mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci>=?" procedure:
data primitive_CHAR_CI_GTE(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci>=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) < mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// --------------
// Char Analysis:
// --------------

// primitive "char-alphabetic?" procedure:
data primitive_CHAR_ALPHABETICP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-alphabetic?");
  return boolean(isalpha(args[0].value.chr));
}

// primitive "char-numeric?" procedure:
data primitive_CHAR_NUMERICP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-numeric?");
  return boolean(isdigit(args[0].value.chr));
}

// primitive "char-whitespace?" procedure:
data primitive_CHAR_WHITESPACEP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-whitespace?");
  return boolean(isspace(args[0].value.chr));
}

// primitive "char-upper-case?" procedure:
data primitive_CHAR_UPPER_CASEP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-upper-case?");
  return boolean(isupper(args[0].value.chr));
}

// primitive "char-lower-case?" procedure:
data primitive_CHAR_LOWER_CASEP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-lower-case?");
  return boolean(islower(args[0].value.chr));
}

// ------------------
// Char Case Control:
// ------------------

// primitive "char-upcase" procedure:
data primitive_CHAR_UPCASE(scm_list& args) {
  confirm_given_one_char_arg(args, "char-upcase");
  return char(toupper(args[0].value.chr));
}

// primitive "char-downcase" procedure:
data primitive_CHAR_DOWNCASE(scm_list& args) {
  confirm_given_one_char_arg(args, "char-downcase");
  return char(tolower(args[0].value.chr));
}

/******************************************************************************
* STRING PRIMITIVES
******************************************************************************/

// primitive "make-string" procedure:
data primitive_MAKE_STRING(scm_list& args) {
  // confirm valid length given
  if(no_args_given(args) || args.size() > 2 || !args[0].is_type(types::num) 
    || !args[0].value.num.is_integer() || !args[0].value.num.is_pos()
    || args[0].value.num > MAX_SIZE_TYPE)
    FATAL_ERR("'make-string primitive didn't recieve a proper positive integer size!"
      << "\n     (make-string <positive-integer> <optional-fill-char>)"
      << "\n     <positive-integer> range: (0," << MAX_SIZE_TYPE << "]");
  if(args.size()==2 && !args[1].is_type(types::chr))
    FATAL_ERR("'make-string primitive recieved a non-character fill value:"
      << "\n     Received fill value [ "<<args[1]<<" ] of type \""<<args[1].type_name()<<"\"!"
      << "\n     (make-string <positive-integer> <optional-fill-char>)");
  // mk a string w/ the the given reserve size
  size_type n = (size_type)args[0].value.num.extract_inexact();
  return make_str(scm_string(n, (args.size()==2 ? args[1].value.chr : '?')));
}

// primitive "string" procedure:
data primitive_STRING(scm_list& args) {
  if(no_args_given(args)) return make_str("");
  if(auto i = confirm_only_args_of_type(args, types::chr); i != -1)
    FATAL_ERR("'string primitive arg #" << i+1 << ", [ " << args[i] << " ]"
      << " of type \"" << args[i].type_name()
      << "\",\n     isn't a character: (string <char1> <char2> ...)");
  scm_string str_val;
  for(const auto& ch : args) str_val += ch.value.chr;
  return make_str(str_val);
}

// primitive "string-length" procedure:
data primitive_STRING_LENGTH(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 1, "string-length", "(string-length <string>)");
  return num_type(args[0].value.str->size());
}

// primitive "string-ref" procedure:
data primitive_STRING_REF(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 2, "string-ref", "(string-ref <string> <non-neg-integer>)");
  auto i = primitive_confirm_valid_string_idx(args, "string-ref", "(string-ref <string> <non-neg-integer>)");
  return args[0].value.str->operator[](i);
}

// primitive "string-set!" procedure:
data primitive_STRING_SET(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 3, "string-set!", "(string-set! <string> <non-neg-integer> <char>)");
  auto i = primitive_confirm_valid_string_idx(args, "string-set!", "(string-set! <string> <non-neg-integer> <char>)");
  if(!args[2].is_type(types::chr))
    FATAL_ERR("'string-set! primitive recieved non-character set-value " 
      << "\n     [ " << args[2] << " ] of type \"" << args[2].type_name() 
      << "\"!\n     (string-set! <string> <non-neg-integer> <char>)");
  args[0].value.str->operator[](i) = args[2].value.chr;
  return symconst::ok;
}

// primitive "substring" procedure:
data primitive_SUBSTRING(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 3, "substring", "(substring <string> <start> <end>)");
  auto start = primitive_confirm_valid_string_idx(args, "string-ref", "(string-ref <string> <end>)");
  // confirm given an in-'size_type'-range non-negative end index
  if(!args[2].is_type(types::num) || !args[2].value.num.is_integer() || 
      args[2].value.num.is_neg()  || args[2].value.num > MAX_SIZE_TYPE)
    FATAL_ERR("'substring primitive didn't recieve a proper non-negative integer <end> index!"
      << "\n     (substring <string> <start> <end>)"
      << "\n     <start> & <end> range: [0," << MAX_SIZE_TYPE << "]");
  // confirm index falls w/in range of the container
  const size_type end = (size_type)args[2].value.num.extract_inexact();
  if(end > args[0].value.str->size())
    FATAL_ERR("'substring primitive recieved out of bounds <end> index " << end 
      <<"\n     for string "<<args[0]<<" of size "<< args[0].value.str->size()<<"!");
  return make_str(args[0].value.str->substr(start,end));
}

// primitive "string-append" procedure:
data primitive_STRING_APPEND(scm_list& args) {
  if(no_args_given(args)) return make_str("");
  if(auto i = confirm_only_args_of_type(args, types::str); i != -1)
    FATAL_ERR("'string-append primitive arg #" << i+1 << ", [ " << args[i] << " ]"
      << " of type \"" << args[i].type_name()
      << "\",\n     isn't a string: (string-append <string1> <string2> ...)");
  scm_string str_val;
  for(const auto& str : args) str_val += *str.value.str;
  return make_str(str_val);
}

// primitive "string-copy" procedure:
data primitive_STRING_COPY(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 1, "string-copy", "(string-copy <string>)");
  return make_str(*args[0].value.str);
}

// primitive "string-fill!" procedure:
data primitive_STRING_FILL(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 2, "string-fill!", "(string-fill! <string> <char>)");
  if(!args[1].is_type(types::chr))
    FATAL_ERR("'string-fill! primitive recieved non-character fill-value " 
      << "\n     [ " << args[1] << " ] of type \"" << args[1].type_name() 
      << "\"!\n     (string-fill! <string> <char>)");
  for(size_type i = 0, n = args[0].value.str->size(); i < n; ++i)
    args[0].value.str->operator[](i) = args[1].value.chr;
  return symconst::ok;
}

// -------------------
// String Comparators:
// -------------------

// primitive "string=?" procedure:
data primitive_STRING_EQ(scm_list& args) {
  confirm_given_char_string_args(args, "string=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str != *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string<?" procedure:
data primitive_STRING_LT(scm_list& args) {
  confirm_given_char_string_args(args, "string<?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str >= *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string>?" procedure:
data primitive_STRING_GT(scm_list& args) {
  confirm_given_char_string_args(args, "string>?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str <= *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string<=?" procedure:
data primitive_STRING_LTE(scm_list& args) {
  confirm_given_char_string_args(args, "string<=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str > *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string>=?" procedure:
data primitive_STRING_GTE(scm_list& args) {
  confirm_given_char_string_args(args, "string>=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str < *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci=?" procedure:
data primitive_STRING_CI_EQ(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) != lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci<?" procedure:
data primitive_STRING_CI_LT(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci<?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) >= lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci>?" procedure:
data primitive_STRING_CI_GT(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci>?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) <= lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci<=?" procedure:
data primitive_STRING_CI_LTE(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci<=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) > lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci>=?" procedure:
data primitive_STRING_CI_GTE(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci>=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) < lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

/******************************************************************************
* PAIR PRIMITIVES
******************************************************************************/

// --------------
// Pair Handlers:
// --------------

// primitive "cons" procedure:
data primitive_CONS(scm_list& args) {
  if(args.size() != 2 || no_args_given(args))
    FATAL_ERR("'cons primitive did not recieved 2 arguments: (cons <car-obj> <cdr-obj>)");
  data new_pair = data(make_par());
  new_pair.value.par->first = args[0];
  new_pair.value.par->second = args[1];
  return new_pair;
}

// primitive "car" procedure:
data primitive_CAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "car");
  return args[0].value.par->first;
}

// primitive "cdr" procedure:
data primitive_CDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdr");
  return args[0].value.par->second;
}

// primitive "null?" procedure:
data primitive_NULLP(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'null? primitive recieved incorrect # of arguments: (null? <obj>)");
  return data(boolean(data_is_the_empty_expression(args[0])));
}

// primitive "set-car!" procedure:
data primitive_SETCAR(scm_list& args) {
  if(args.size() != 2 || no_args_given(args))
    FATAL_ERR("'set-car! primitive recieved incorrect # of arguments: (set-car! <pair-obj>)");
  if(!args[0].is_type(types::par))
    FATAL_ERR("'set-car! primitive's 1st arg isn't a pair: (set-car! <pair-obj>)");
  args[0].value.par->first = args[1];
  return data(symconst::ok); // return is implementation dependant, We've chosen 'ok symbol
}

// primitive "set-cdr!" procedure:
data primitive_SETCDR(scm_list& args) {
  if(args.size() != 2 || no_args_given(args))
    FATAL_ERR("'set-cdr! primitive recieved incorrect # of arguments: (set-cdr! <pair-obj>)");
  if(!args[0].is_type(types::par))
    FATAL_ERR("'set-cdr! primitive's 1st arg isn't a pair: (set-cdr! <pair-obj>)");
  args[0].value.par->second = args[1];
  return data(symconst::ok); // return is implementation dependant, We've chosen 'ok symbol
}

// ---------------------
// Car/Cdr Combinations:
// ---------------------

// primitive "caar" procedure:
data primitive_CAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caar");
  confirm_nth_car_is_pair(args[0], "caar", "1st");
  return args[0].value.par->first.value.par->first;
}

// primitive "cadr" procedure:
data primitive_CADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadr");
  confirm_nth_cdr_is_pair(args[0], "cadr", "1st");
  return args[0].value.par->second.value.par->first;
}

// primitive "cdar" procedure:
data primitive_CDAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdar");
  confirm_nth_car_is_pair(args[0], "cdar", "1st");
  return args[0].value.par->first.value.par->second;
}

// primitive "cddr" procedure:
data primitive_CDDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddr");
  confirm_nth_cdr_is_pair(args[0], "cddr", "1st");
  return args[0].value.par->second.value.par->second;
}

// ----------

// primitive "caaar" procedure:
data primitive_CAAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caaar");
  confirm_nth_car_is_pair(args[0], "caaar", "1st");
  confirm_nth_car_is_pair(args[0].value.par->first, "caaar", "2nd");
  return args[0].value.par->first.value.par->first.value.par->first;
}

// primitive "caadr" procedure:
data primitive_CAADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caadr");
  confirm_nth_cdr_is_pair(args[0], "caadr", "1st");
  confirm_nth_car_is_pair(args[0].value.par->second, "caadr", "1st");
  return args[0].value.par->second.value.par->first.value.par->first;
}

// primitive "cadar" procedure:
data primitive_CADAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadar");
  confirm_nth_car_is_pair(args[0], "cadar", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->first, "cadar", "1st");
  return args[0].value.par->first.value.par->second.value.par->first;
}

// primitive "caddr" procedure:
data primitive_CADDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caddr");
  confirm_nth_cdr_is_pair(args[0], "caddr", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->second, "caddr", "2nd");
  return args[0].value.par->second.value.par->second.value.par->first;
}

// primitive "cdaar" procedure:
data primitive_CDAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdaar");
  confirm_nth_car_is_pair(args[0], "cdaar", "1st");
  confirm_nth_car_is_pair(args[0].value.par->first, "cdaar", "2nd");
  return args[0].value.par->first.value.par->first.value.par->second;
}

// primitive "cdadr" procedure:
data primitive_CDADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdadr");
  confirm_nth_cdr_is_pair(args[0], "cdadr", "1st");
  confirm_nth_car_is_pair(args[0].value.par->second, "cdadr", "1st");
  return args[0].value.par->second.value.par->first.value.par->second;
}

// primitive "cddar" procedure:
data primitive_CDDAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddar");
  confirm_nth_car_is_pair(args[0], "cddar", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->first, "cddar", "1st");
  return args[0].value.par->first.value.par->second.value.par->second;
}

// primitive "cdddr" procedure:
data primitive_CDDDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdddr");
  confirm_nth_cdr_is_pair(args[0], "cdddr", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->second, "cdddr", "2nd");
  return args[0].value.par->second.value.par->second.value.par->second;
}

// ----------

// primitive "caaaar" procedure:
data primitive_CAAAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caaaar");
  confirm_nth_car_is_pair(args[0], "caaaar", "1st");
  confirm_nth_car_is_pair(args[0].value.par->first, "caaaar", "2nd");
  confirm_nth_car_is_pair(args[0].value.par->first.value.par->first, "caaaar", "3rd");
  return args[0].value.par->first.value.par->first.value.par->first.value.par->first;
}

// primitive "caaadr" procedure:
data primitive_CAAADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caaadr");
  confirm_nth_cdr_is_pair(args[0], "caaadr", "1st");
  confirm_nth_car_is_pair(args[0].value.par->second, "caaadr", "1st");
  confirm_nth_car_is_pair(args[0].value.par->second.value.par->first, "caaadr", "2nd");
  return args[0].value.par->second.value.par->first.value.par->first.value.par->first;
}

// primitive "caadar" procedure:
data primitive_CAADAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caadar");
  confirm_nth_car_is_pair(args[0], "caadar", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->first, "caadar", "1st");
  confirm_nth_car_is_pair(args[0].value.par->first.value.par->second, "caadar", "2nd");
  return args[0].value.par->first.value.par->second.value.par->first.value.par->first;
}

// primitive "caaddr" procedure:
data primitive_CAADDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caaddr");
  confirm_nth_cdr_is_pair(args[0], "caaddr", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->second, "caaddr", "2nd");
  confirm_nth_car_is_pair(args[0].value.par->second.value.par->second, "caaddr", "1st");
  return args[0].value.par->second.value.par->second.value.par->first.value.par->first;
}

// primitive "cadaar" procedure:
data primitive_CADAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadaar");
  confirm_nth_car_is_pair(args[0], "cadaar", "1st");
  confirm_nth_car_is_pair(args[0].value.par->first, "cadaar", "2nd");
  confirm_nth_cdr_is_pair(args[0].value.par->first.value.par->first, "cadaar", "1st");
  return args[0].value.par->first.value.par->first.value.par->second.value.par->first;
}

// primitive "cadadr" procedure:
data primitive_CADADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadadr");
  confirm_nth_cdr_is_pair(args[0], "cadadr", "1st");
  confirm_nth_car_is_pair(args[0].value.par->second, "cadadr", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->second.value.par->first, "cadadr", "2nd");
  return args[0].value.par->second.value.par->first.value.par->second.value.par->first;
}

// primitive "caddar" procedure:
data primitive_CADDAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caddar");
  confirm_nth_car_is_pair(args[0], "caddar", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->first, "caddar", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->first.value.par->second, "caddar", "2nd");
  return args[0].value.par->first.value.par->second.value.par->second.value.par->first;
}

// primitive "cadddr" procedure:
data primitive_CADDDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadddr");
  confirm_nth_cdr_is_pair(args[0], "cadddr", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->second, "cadddr", "2nd");
  confirm_nth_cdr_is_pair(args[0].value.par->second.value.par->second, "cadddr", "3rd");
  return args[0].value.par->second.value.par->second.value.par->second.value.par->first;
}


// primitive "cdaaar" procedure:
data primitive_CDAAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdaaar");
  confirm_nth_car_is_pair(args[0], "cdaaar", "1st");
  confirm_nth_car_is_pair(args[0].value.par->first, "cdaaar", "2nd");
  confirm_nth_car_is_pair(args[0].value.par->first.value.par->first, "cdaaar", "3rd");
  return args[0].value.par->first.value.par->first.value.par->first.value.par->second;
}

// primitive "cdaadr" procedure:
data primitive_CDAADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdaadr");
  confirm_nth_cdr_is_pair(args[0], "cdaadr", "1st");
  confirm_nth_car_is_pair(args[0].value.par->second, "cdaadr", "1st");
  confirm_nth_car_is_pair(args[0].value.par->second.value.par->first, "cdaadr", "2nd");
  return args[0].value.par->second.value.par->first.value.par->first.value.par->second;
}

// primitive "cdadar" procedure:
data primitive_CDADAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdadar");
  confirm_nth_car_is_pair(args[0], "cdadar", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->first, "cdadar", "1st");
  confirm_nth_car_is_pair(args[0].value.par->first.value.par->second, "cdadar", "2nd");
  return args[0].value.par->first.value.par->second.value.par->first.value.par->second;
}

// primitive "cdaddr" procedure:
data primitive_CDADDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdaddr");
  confirm_nth_cdr_is_pair(args[0], "cdaddr", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->second, "cdaddr", "2nd");
  confirm_nth_car_is_pair(args[0].value.par->second.value.par->second, "cdaddr", "1st");
  return args[0].value.par->second.value.par->second.value.par->first.value.par->second;
}

// primitive "cddaar" procedure:
data primitive_CDDAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddaar");
  confirm_nth_car_is_pair(args[0], "cddaar", "1st");
  confirm_nth_car_is_pair(args[0].value.par->first, "cddaar", "2nd");
  confirm_nth_cdr_is_pair(args[0].value.par->first.value.par->first, "cddaar", "1st");
  return args[0].value.par->first.value.par->first.value.par->second.value.par->second;
}

// primitive "cddadr" procedure:
data primitive_CDDADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddadr");
  confirm_nth_cdr_is_pair(args[0], "cddadr", "1st");
  confirm_nth_car_is_pair(args[0].value.par->second, "cddadr", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->second.value.par->first, "cddadr", "2nd");
  return args[0].value.par->second.value.par->first.value.par->second.value.par->second;
}

// primitive "cdddar" procedure:
data primitive_CDDDAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdddar");
  confirm_nth_car_is_pair(args[0], "cdddar", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->first, "cdddar", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->first.value.par->second, "cdddar", "2nd");
  return args[0].value.par->first.value.par->second.value.par->second.value.par->second;
}

// primitive "cddddr" procedure:
data primitive_CDDDDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddddr");
  confirm_nth_cdr_is_pair(args[0], "cddddr", "1st");
  confirm_nth_cdr_is_pair(args[0].value.par->second, "cddddr", "2nd");
  confirm_nth_cdr_is_pair(args[0].value.par->second.value.par->second, "cddddr", "3rd");
  return args[0].value.par->second.value.par->second.value.par->second.value.par->second;
}

/******************************************************************************
* LIST PRIMITIVES
******************************************************************************/

// primitive "list" procedure:
data primitive_LIST(scm_list& args) {
  if(no_args_given(args)) return data(symconst::emptylist); // "(list)" = "'()"
  return primitive_LIST_to_CONS_constructor(args.begin(), args.end());
}

// primitive "length" procedure:
data primitive_LENGTH(scm_list& args) {
  if(primitive_validate_list_and_return_if_empty(args,"length")) 
    return num_type();
  num_type count;
  primitive_LENGTH_computation(args[0].value.par->second,count);
  return count;
}

// primitive "list?" procedure:
//   => where 'list' := finite & null-terminated pair sequence
data primitive_LISTP(scm_list& args) {
  // list? requires 1 arg
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'list? primitive recieved incorrect # of arguments: (list? <obj>)");
  // the empty list is a list
  if(data_is_the_empty_expression(args[0]))
    return TRUE_DATA_BOOLEAN;
  // if non-empty list & not pair, GUARENTEED not a list
  if(!args[0].is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  // valid lists are finite & terminate with '()
  if(primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::ok)
    return TRUE_DATA_BOOLEAN;
  return FALSE_DATA_BOOLEAN;
}

// primitive "append" procedure:
// PRECONDITIONS: 1) THE FIRST n-1 ARGS MUST HAVE "list?" = TRUE
//                2) THE LAST ARG MUST NOT BE A CYCLIC LIST
data primitive_APPEND(scm_list& args) {
  // (append) = '()
  if(no_args_given(args)) return data(symconst::emptylist);
  // (append <obj>) = <obj>
  const size_type n = args.size();
  if(n == 1)              return args[0];
  // (append '() <obj>) = <obj>
  if(data_is_the_empty_expression(args[0]))
    return args[1];
  // Confirm Precondition 2 
  if(args[n-1].is_type(types::par) && 
    primitive_list_is_acyclic_and_null_terminated(args[n-1]) == list_status::cyclic)
    FATAL_ERR("'append - last argument is a cyclic list (invalid): (append <proper-list> <obj>)");
  // Confirm Precondition 1
  for(size_type i = 0; i < n-1; ++i) {
    if(!args[i].is_type(types::par))
      FATAL_ERR("'append argument #" << i+1 << " [ " << args[i] << " ] expects a pair: (append <proper-list> <obj>)");
    else if(auto stat = primitive_list_is_acyclic_and_null_terminated(args[i]); 
      stat == list_status::cyclic) {
      FATAL_ERR("'append argument #" << i+1 << " expects an acyclic list: (append <proper-list> <obj>)");
    } else if(stat == list_status::no_null)
      FATAL_ERR("'append argument #" << i+1 << " expects a '() terminated list: (append <proper-list> <obj>)");
  }
  // Link the lists to one another
  for(size_type i = 0; i < n-1; ++i)
    primitive_APPEND_list_linker(args[i],args[i+1]);
  return args[0];
}

// primitive "reverse" procedure:
data primitive_REVERSE(scm_list& args) {
  if(primitive_validate_list_and_return_if_empty(args,"reverse")) 
    return make_par();
  scm_list par_as_exp;
  shallow_unpack_list_into_exp(args[0], par_as_exp);
  std::reverse(par_as_exp.begin(),par_as_exp.end());
  return primitive_LIST(par_as_exp);
}

// primitive "list-tail" procedure:
data primitive_LIST_TAIL(scm_list& args) {
  return primitive_confirm_valid_list_index(args, "list-tail");
}

// primitive "list-ref" procedure:
data primitive_LIST_REF(scm_list& args) {
  return primitive_confirm_valid_list_index(args, "list-ref").value.par->first;
}

// ----------------------
// List Control Features:
// ----------------------

// primitive "for-each" procedure:
data primitive_FOR_EACH(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given minimum # of args needed
  constexpr const char * const format = "\n     (for-each <procedure> <list1> <list2> ...)";
  if(args.size() < 2) 
    FATAL_ERR("'for-each primitive recieved insufficient args (only "
      << (no_args_given(args) ? 0 : args.size()) << "):" << format);
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], "for-each", format);
  scm_list list_heads(args.begin()+1, args.end());
  // Confirm only given proper lists of the same length
  primitive_confirm_proper_same_sized_lists(list_heads,"for-each",format,1);
  // Apply the procedure on each elt of each list
  primitive_FOR_EACH_applicator(list_heads, args[0].value.exp, env);
  return symconst::ok;
}

// primitive "map" procedure:
data primitive_MAP(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given minimum # of args needed
  constexpr const char * const format = "\n     (map <procedure> <list1> <list2> ...)";
  if(args.size() < 2) 
    FATAL_ERR("'map primitive recieved insufficient args (only " 
      << (no_args_given(args) ? 0 : args.size()) << "):" << format);
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], "map", format);
  // Confirm only given proper lists of the same length
  scm_list list_heads(args.begin()+1, args.end());
  primitive_confirm_proper_same_sized_lists(list_heads,"map",format,1);
  // Apply the procedure on each elt of each list & store the result
  scm_list mapped_list;
  primitive_MAP_list_constructor(list_heads, args[0].value.exp, mapped_list, env);
  return primitive_LIST(mapped_list); // return a list of the results
}

// primitive "filter" procedure:
data primitive_FILTER(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given correct # of args needed
  constexpr const char * const format = "\n     (filter <procedure> <list>)";
  if(args.size() != 2) 
    FATAL_ERR("'filter primitive recieved incorrect # of args (only " 
      << (no_args_given(args) ? 0 : args.size()) << "):" << format);
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], "filter", format);
  // Confirm only given proper lists of the same length
  scm_list list_head({args[1]});
  primitive_confirm_proper_same_sized_lists(list_head,"filter",format,1);
  // Apply the procedure on each elt of each list, & store args that eval'd to 'true'
  scm_list filtered_list;
  primitive_FILTER_list_constructor(list_head[0], args[0].value.exp, filtered_list, env);
  return primitive_LIST(filtered_list); // return a list of the valid values
}

// primitive "fold-left" procedure:
data primitive_FOLD_LEFT(scm_list& args) {
  return primitive_FOLD_template(args, "fold-left", "\n     (fold-left <procedure> <init> <list1> <list2> ...)");
}

// primitive "fold-right" procedure:
data primitive_FOLD_RIGHT(scm_list& args) {
  return primitive_FOLD_template(args, "fold-right", "\n     (fold-right <procedure> <init> <list1> <list2> ...)");
}

// primitive "merge" procedure:
data primitive_MERGE(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given minimum # of args needed
  constexpr const char * const format = "\n     (merge <procedure> <list1> <list2>)";
  if(args.size() != 3) 
    FATAL_ERR("'merge primitive recieved incorrect # of args (only " 
      << (no_args_given(args) ? 0 : args.size()) << "):" << format);
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], "merge", format);
  // Confirm only given proper lists
  for(size_type i = 1, n = args.size(); i < n; ++i)
    if(scm_list arg_list({args[i]}); !primitive_LISTP(arg_list).value.bol.val)
      FATAL_ERR("'merge primitive arg #" << i+1 << " of type \""
        << args[i].type_name() << "\" isn't a proper list!" << format);
  // Apply the procedure on each elt either list & merge args as per the result
  scm_list list_heads(args.begin()+1, args.end());
  scm_list merged_list;
  primitive_MERGE_list_constructor(list_heads, args[0].value.exp, merged_list, env);
  return primitive_LIST(merged_list); // return a list of the results
}

// -----------------------
// List Member Extraction:
// -----------------------

// primitive "memq" procedure:
data primitive_MEMQ(scm_list& args) {
  return primitive_MEM_template(args, "memq", primitive_EQP);
}

// primitive "memv" procedure:
data primitive_MEMV(scm_list& args) {
  return primitive_MEM_template(args, "memv", primitive_EQVP);
}

// primitive "member" procedure:
data primitive_MEMBER(scm_list& args) {
  return primitive_MEM_template(args, "member", primitive_EQUALP);
}

// primitive "assq" procedure:
data primitive_ASSQ(scm_list& args) {
  return primitive_ASSOCIATION_template(args, "assq", primitive_EQP);
}

// primitive "assv" procedure:
data primitive_ASSV(scm_list& args) {
  return primitive_ASSOCIATION_template(args, "assv", primitive_EQVP);
}

// primitive "assoc" procedure:
data primitive_ASSOC(scm_list& args) {
  return primitive_ASSOCIATION_template(args, "assoc", primitive_EQUALP);
}

/******************************************************************************
* VECTOR PRIMITIVES
******************************************************************************/

// primitive "vector" procedure:
data primitive_VECTOR(scm_list& args) {
  if(no_args_given(args)) return data(make_vec(scm_list{}));
  return data(make_vec(scm_list(args.begin(), args.end())));
}


// primitive "make-vector" procedure:
data primitive_MAKE_VECTOR(scm_list& args) {
  // confirm valid length given
  if(no_args_given(args) || args.size() > 2 || !args[0].is_type(types::num) 
    || !args[0].value.num.is_integer() || !args[0].value.num.is_pos()
    || args[0].value.num > MAX_SIZE_TYPE)
    FATAL_ERR("'make-vector primitive didn't recieve a proper positive integer size!"
      << "\n     (make-vector <positive-integer> <optional-fill-value>)"
      << "\n     <positive-integer> range: (0," << MAX_SIZE_TYPE << "]");
  // mk a vector w/ the the given reserve size
  size_type n = (size_type)args[0].value.num.extract_inexact();
  data vect(make_vec(scm_list(n)));
  // fill vector as needed
  if(args.size() == 2)
    for(size_type i = 0; i < n; ++i)
      vect.value.vec->operator[](i) = args[1];
  return vect;
}


// primitive "vector-length" procedure:
data primitive_VECTOR_LENGTH(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 1, "vector-length", "(vector-length <vector>)");
  return num_type(args[0].value.vec->size());
}


// primitive "vector-ref" procedure:
data primitive_VECTOR_REF(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 2, "vector-ref", "(vector-ref <vector> <non-neg-integer>)");
  auto i = primitive_confirm_valid_vector_idx(args, "vector-ref", "(vector-ref <vector> <non-neg-integer>)");
  return args[0].value.vec->operator[](i);
}


// primitive "vector-set!" procedure:
data primitive_VECTOR_SET(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 3, "vector-set!", "(vector-set! <vector> <non-neg-integer> <obj>)");
  auto i = primitive_confirm_valid_vector_idx(args, "vector-set!", "(vector-set! <vector> <non-neg-integer> <obj>)");
  args[0].value.vec->operator[](i) = args[2];
  return symconst::ok;
}


// primitive "vector-fill!" procedure:
data primitive_VECTOR_FILL(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 2, "vector-fill!", "(vector-fill! <vector> <obj>)");
  for(size_type i = 0, n = args[0].value.vec->size(); i < n; ++i)
    args[0].value.vec->operator[](i) = args[1];
  return symconst::ok;
}

/******************************************************************************
* TYPE-CHECKING PRIMITIVES
******************************************************************************/

// primitive "pair?" procedure:
data primitive_PAIRP(scm_list& args) {
  confirm_given_one_arg(args, "pair?");
  return data(boolean(args[0].is_type(types::par)));
}

// primitive "vector?" procedure:
data primitive_VECTORP(scm_list& args) {
  confirm_given_one_arg(args, "vector?");
  return data(boolean(args[0].is_type(types::vec)));
}

// primitive "char?" procedure:
data primitive_CHARP(scm_list& args) {
  confirm_given_one_arg(args, "char?");
  return data(boolean(args[0].is_type(types::chr)));
}

// primitive "number?" procedure:
data primitive_NUMBERP(scm_list& args) {
  confirm_given_one_arg(args, "number?");
  return data(boolean(args[0].is_type(types::num)));
}

// primitive "real?" procedure:
// => "real" denotes a # that's neither NaN nor Inf
data primitive_REALP(scm_list& args) {
  confirm_given_one_arg(args, "real?");
  return data(boolean(
    args[0].is_type(types::num) && args[0].value.num.is_real()));
}

// primitive "rational?" procedure:
// => "rational" denotes a # that inexact->exact won't approximate
data primitive_RATIONALP(scm_list& args) {
  confirm_given_one_arg(args, "rational?");
  return data(boolean(
    args[0].is_type(types::num) && args[0].value.num.is_rational()));
}

// primitive "string?" procedure:
data primitive_STRINGP(scm_list& args) {
  confirm_given_one_arg(args, "string?");
  return data(boolean(args[0].is_type(types::str)));
}

// primitive "symbol?" procedure:
data primitive_SYMBOLP(scm_list& args) {
  confirm_given_one_arg(args, "symbol?");
  return data(boolean(args[0].is_type(types::sym)));
}

// primitive "boolean?" procedure:
data primitive_BOOLEANP(scm_list& args) {
  confirm_given_one_arg(args, "boolean?");
  return data(boolean(args[0].is_type(types::bol)));
}

// primitive "atom?" procedure:
data primitive_ATOMP(scm_list& args) {
  confirm_given_one_arg(args, "atom?");
  const bool is_null = primitive_NULLP(args).value.bol.val;
  const bool is_pair = primitive_PAIRP(args).value.bol.val;
  return data(boolean(!is_null && !is_pair));
}

// primitive "procedure?" procedure:
data primitive_PROCEDUREP(scm_list& args) {
  confirm_given_one_arg(args, "procedure?");
  bool is_procedure = args[0].is_type(types::exp) &&
                      args[0].value.exp[0].is_type(types::sym) &&
                      (args[0].value.exp[0].value.sym == PROCEDURE_TAG || 
                       args[0].value.exp[0].value.sym == PRIMITIVE_TAG);
  return data(boolean(is_procedure));
}

/******************************************************************************
* EVAL PRIMITIVE
******************************************************************************/

// primitive "eval" procedure:
data primitive_EVAL(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // use the initial environment if passed 'null-environment as a 2nd arg
  if(args.size()==2 && args[1].is_type(types::sym) && 
     args[1].value.sym == "null-environment") {
    env = setup_environment();
    args.pop_back();
  }
  // confirm the correct # of arguments were passed
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'eval primitive recieved incorrect # of arguments: (eval <data>)");
  // if arg is self-evaluating, return arg
  if(args[0].is_type(types::num) || args[0].is_type(types::str) || 
     args[0].is_type(types::chr) || args[0].is_type(types::bol)) {
    return args[0];
  }
  // if arg is a symbol, eval the symbol
  if(args[0].is_type(types::sym)) {
    // confirm arg is not '()
    if(args[0].value.sym == THE_EMPTY_LIST) 
      FATAL_ERR("'eval primitive can't evaluate '() (nil to eval): (eval <data>)");
    return data_cast(scm_eval(scm_list({args[0]}),env));
  }
  // else confirm arg is a proper list
  if(!args[0].is_type(types::par))
    FATAL_ERR("'eval primitive didn't recieve an evaluable expression: [ "<<args[0]<<" ] (eval <data>)");
  if(scm_list first_arg({args[0]}); !primitive_LISTP(first_arg).value.bol.val)
    FATAL_ERR("'eval primitive recieved an improper list: [ "<<args[0]<<" ] (eval <data>)");
  // eval list contents
  scm_list par_as_exp;
  deep_unpack_list_into_exp(args[0], par_as_exp);
  return data_cast(scm_eval(std::move(par_as_exp),env));
}

/******************************************************************************
* APPLY PRIMITIVE
******************************************************************************/

// primitive "apply" procedure:
data primitive_APPLY(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm the correct # of arguments were passed
  constexpr const char * const format = "\n     (apply <procedure> <proper-list-of-arguments>)";
  if(args.size() != 2 || no_args_given(args))
    FATAL_ERR("'apply primitive recieved incorrect # of arguments:" << format);
  // confirm 1st arg is a procedure
  primitive_confirm_data_is_a_procedure(args[0], "apply", format);
  // confirm 2nd arg is a finite, nul-terminated list
  if(scm_list second_arg({args[1]}); !primitive_LISTP(second_arg).value.bol.val)
    FATAL_ERR("'apply primitive expects a proper list as its 2nd argument:"<<format);
  // apply arguments in list to the procedure
  scm_list args_list;
  shallow_unpack_list_into_exp(args[1], args_list);
  return data_cast(execute_application(std::move(args[0].value.exp),args_list,env));
}

/******************************************************************************
* FORCE-DELAY PRIMITIVES
******************************************************************************/

// primitive "delay?" predicate procedure:
data primitive_DELAYP(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'delay? primitive recieved incorrect # of arguments: (delay? <obj>)");
  return boolean(
    args[0].is_type(types::exp) && !args[0].value.exp.empty() &&
    args[0].value.exp[0].is_type(types::sym) && 
    args[0].value.exp[0].value.sym == "delay"
  );
}


// primitive "force" procedure:
data primitive_FORCE(scm_list& args) { // force primitive proc for delay special form
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'force primitive recieved incorrect # of arguments: (force <delayed-expression>)");
  if(!primitive_DELAYP(args).value.bol.val)
    FATAL_ERR("'force primitive didn't recieve a delayed expression!"
      << "\n     Received [ "<< args[0] << " ] of type \"" << args[0].type_name() << "\"!");
  auto delay = args[0].value.exp[1].value.del;
  if(delay->already_forced)
    return delay->result; // Memoize delays, "call by need" evaluation
  else {
    delay->already_forced = true;
    delay->result = data_cast(scm_eval(std::move(delay->exp),delay->env));
    return delay->result;
  }
}

/******************************************************************************
* CONS-STREAM PRIMITIVES
******************************************************************************/

// primitive "stream-car" procedure:
data primitive_STREAM_CAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "stream-car");
  return primitive_CAR(args);
}

// primitive "stream-cdr" procedure:
data primitive_STREAM_CDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "stream-cdr");
  scm_list cdr_promise({primitive_CDR(args)});
  return primitive_FORCE(cdr_promise);
}

// primitive "stream-null?" procedure (identical to 'null?):
data primitive_STREAM_NULLP(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'stream-null? primitive recieved incorrect # of arguments:"
      << "\n     (stream-null? <obj>)");
  return data(boolean(data_is_the_empty_expression(args[0])));
}

/******************************************************************************
* TYPE COERCION PRIMITIVES
******************************************************************************/

// primitive "char->integer" procedure:
data primitive_COERCE_CHAR_TO_INTEGER(scm_list& args) {
  confirm_given_one_char_arg(args, "char->integer");
  return num_type(int(args[0].value.chr));
}

// primitive "integer->char" procedure:
data primitive_COERCE_INTEGER_TO_CHAR(scm_list& args) {
  if(no_args_given(args) || args.size() != 1)
    FATAL_ERR("'integer->char primitive didn't recieve 1 integer arg:"
      << "\n     (integer->char <non_negative-integer>)"
      << "\n     <non_negative-integer> range: [0,255]");
  if(!args[0].is_type(types::num) || !args[0].value.num.is_integer())
    FATAL_ERR("'integer->char primitive didn't recieve an integer arg:"
      << "\n     Received arg [ " << args[0] << " ] of type \"" << args[0].type_name() << "\""
      << "\n     (integer->char <non_negative-integer>)"
      << "\n     <non_negative-integer> range: [0,255]");
  if(args[0].value.num.is_neg() || args[0].value.num > 255)
    FATAL_ERR("'integer->char primitive argument [ " << args[0] << " ] isn't a"
      << "\n     positive integer ranging from 0 to 255!");
  return char(args[0].value.num.extract_inexact());
}

// primitive "number->string" procedure:
data primitive_COERCE_NUMBER_TO_STRING(scm_list& args) {
  if(args.empty() || args.size() > 2 || no_args_given(args))
    FATAL_ERR("'number->string primitive recieved incorrect # of arguments:"
      << "\n     (number->string <number> <optional-numeric-radix>)");
  // no number given
  if(!args[0].is_type(types::num)) return FALSE_DATA_BOOLEAN;
  // invalid radix given
  if(args.size() == 2 && !args[1].is_type(types::num)) return FALSE_DATA_BOOLEAN;
  // given a radix
  if(args.size() == 2) {
    size_type radix = args[1].value.num.round().extract_inexact();
    if(radix < 2 || radix > 36)
      FATAL_ERR("'number->string primitive radix can only range from 2-36:"
        << "\n     (number->string <number> <optional-numeric-radix>)");
    if(radix != 10)
      return data(args[0].value.num.extract_exact(radix));
  }
  return data(args[0].value.num.extract_exact());
}

// primitive "string->number" procedure:
data primitive_COERCE_STRING_TO_NUMBER(scm_list& args) {
  if(args.empty() || args.size() > 2 || no_args_given(args))
    FATAL_ERR("'string->number primitive recieved incorrect # of arguments!"
      << "\n     (string->number <string> <optional-numeric-radix>)");
  // no string given
  if(!args[0].is_type(types::str)) return FALSE_DATA_BOOLEAN;
  // invalid radix given
  if(args.size() == 2 && !args[1].is_type(types::num)) return FALSE_DATA_BOOLEAN;
  // given a radix
  if(args.size() == 2) {
    size_type radix = args[1].value.num.round().extract_inexact();
    if(radix < 2 || radix > 36)
      FATAL_ERR("'number->string primitive radix can only range from 2-36!"
        << "\n     (string->number <string> <optional-numeric-radix>)");
    if(radix != 10) {
      try {
        return data(num_type(*args[0].value.str, radix));
      } catch(const num_type::error_t& err) {
        return FALSE_DATA_BOOLEAN;
      }
    }
  }
  try {
    return data(num_type(*args[0].value.str));
  } catch(const num_type::error_t& err) {
    return FALSE_DATA_BOOLEAN;
  }
}

// primitive "symbol->string" procedure:
data primitive_COERCE_SYMBOL_TO_STRING(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'symbol->string primitive recieved incorrect # of arguments: (symbol->string <symbol>)");
  if(!args[0].is_type(types::sym))
    return FALSE_DATA_BOOLEAN;
  return make_str(convert_symbol_to_string(args[0].value.sym));
}

// primitive "string->symbol" procedure:
data primitive_COERCE_STRING_TO_SYMBOL(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    FATAL_ERR("'string->symbol primitive recieved incorrect # of arguments: (string->symbol <string>)");
  if(!args[0].is_type(types::str))
    return FALSE_DATA_BOOLEAN;
  return data(convert_string_to_symbol(*args[0].value.str)); 
}

// primitive "vector->list" procedure:
data primitive_COERCE_VECTOR_TO_LIST(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 1, "vector->list", "(vector->list <vector>)");
  return primitive_LIST(*args[0].value.vec);
}

// primitive "list->vector" procedure:
data primitive_COERCE_LIST_TO_VECTOR(scm_list& args) {
  if(primitive_validate_list_and_return_if_empty(args, "list->vector"))
    return make_vec(scm_list{});
  data new_vec(make_vec(scm_list{}));
  shallow_unpack_list_into_exp(args[0], *new_vec.value.vec);
  return new_vec;
}

// primitive "string->list" procedure:
data primitive_STRING_TO_LIST(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 1, "string->list", "(string->list <string>)");
  scm_list char_list;
  for(const auto& ch : *args[0].value.str)
    char_list.push_back(ch);
  return primitive_LIST(char_list);
}

// primitive "list->string" procedure:
data primitive_LIST_TO_STRING(scm_list& args) {
  if(primitive_validate_list_and_return_if_empty(args, "list->string"))
    return make_vec(scm_list{});
  scm_list char_list;
  scm_string char_str;
  shallow_unpack_list_into_exp(args[0], char_list);
  for(const auto ch : char_list) {
    if(!ch.is_type(types::chr))
      FATAL_ERR("'list->string primitive list obj [ "<<ch<<" ] of type \""
        << ch.type_name() << "\" isn't a character!"
        << "\n     (list->string <proper-list-of-chars>)");
    else
      char_str += ch.value.chr;
  }
  return make_str(char_str);
}

/******************************************************************************
* RESET MAX RECURSION DEPTH PRIMITIVE
******************************************************************************/

data primitive_SET_MAX_RECURSION_DEPTH(scm_list& args) {
  // Confirm valid maximum recursion arg
  if(no_args_given(args) || args.size() != 1 || !args[0].is_type(types::num) || 
     !args[0].value.num.is_integer() || !args[0].value.num.is_pos())
    FATAL_ERR("'set-max-recursion-depth! didn't recieve a positive integer arg:"
      << "\n     (set-max-recursion-depth! <positive-integer>)"
      << "\n     <positive-integer>: (0, " << MAX_SIZE_TYPE << "]");
  auto float_num = args[0].value.num.to_inexact();
  if(float_num < 0 || float_num > MAX_SIZE_TYPE)
    FATAL_ERR("'set-max-recursion-depth! integer arg is out of bounds!"
      << "\n     (set-max-recursion-depth! <positive-integer>)"
      << "\n     <positive-integer>: (0, " << MAX_SIZE_TYPE << "]");
  // Set the cap on max recursive calls, & return the original
  auto original = MAX_RECURSIVE_DEPTH;
  MAX_RECURSIVE_DEPTH = (size_type)float_num.extract_inexact();
  return num_type(original);
}

/******************************************************************************
* ERROR HANDLING PRIMITIVE
******************************************************************************/

data primitive_ERROR(scm_list& args) {
  // Confirm valid error layout (a tad ironic)
  if(args.size() < 2)
    FATAL_ERR("'error requires at least 2 args: a SYMBOL to represent the errorful entity & a STRING explaining the error!"
      << "\n     (error <symbol-of-errorful-obj> <error-message-string> <optional-errorful-objs>)");
  if(!args[0].is_type(types::sym))
    FATAL_ERR("'error requires its 1st arg to be a SYMBOL to represent the errorful entity!"
      << "\n     (error <symbol-of-errorful-obj> <error-message-string> <optional-errorful-objs>)");
  if(!args[1].is_type(types::str))
    FATAL_ERR("'error requires its 2nd arg to be a STRING explaining the error!"
      << "\n     (error <symbol-of-errorful-obj> <error-message-string> <optional-errorful-objs>)");
  // Throw error
  std::cerr << "\nException in " << args[0].value.sym << ": " << *args[1].value.str;
  // Check for irritants (if provided, these are optional)
  if(args.size() == 3)
    std::cerr << " with irritant " << args[2] << '\n';
  else if(args.size() > 3) {
    scm_list irritant_list(args.begin()+2, args.end());
    std::cerr << " with irritants " << primitive_LIST(irritant_list) << '\n';
  }
  throw EXIT_SUCCESS;
  return data{};
}

/******************************************************************************
* REGISTRY OF PRIMITIVES ALSO REQUIRING AN ENVIRONMENT (TO APPLY A PROCEDURE)
******************************************************************************/

constexpr const auto ENV_REQUIRING_PRIMITIVES = {
  primitive_APPLY,  primitive_EVAL,      primitive_FOR_EACH,   primitive_MAP,
  primitive_FILTER, primitive_FOLD_LEFT, primitive_FOLD_RIGHT, primitive_MERGE,
};

constexpr bool primitive_requires_environment(const prm_type& prm) {
  for(const auto& p : ENV_REQUIRING_PRIMITIVES)
    if(p == prm) return true;
  return false;
}

/******************************************************************************
* PRIMITIVE NAMES & OBJECTS AS FRAME VARS & VALS FOR THE GLOBAL ENVIRONMENT
******************************************************************************/

auto primitive_procedure_objects = 
  frame_vals({
    data(scm_list({symconst::primitive, primitive_ADD, "+"})),
    data(scm_list({symconst::primitive, primitive_SUB, "-"})),
    data(scm_list({symconst::primitive, primitive_MUL, "*"})),
    data(scm_list({symconst::primitive, primitive_DIV, "/"})),

    data(scm_list({symconst::primitive, primitive_ABS,       "abs"})),
    data(scm_list({symconst::primitive, primitive_EXPT,      "expt"})),
    data(scm_list({symconst::primitive, primitive_MAX,       "max"})),
    data(scm_list({symconst::primitive, primitive_MIN,       "min"})),
    data(scm_list({symconst::primitive, primitive_QUOTIENT,  "quotient"})),
    data(scm_list({symconst::primitive, primitive_REMAINDER, "remainder"})),
    data(scm_list({symconst::primitive, primitive_MODULO,    "modulo"})),
    data(scm_list({symconst::primitive, primitive_EXP,       "exp"})),
    data(scm_list({symconst::primitive, primitive_LOG,       "log"})),
    data(scm_list({symconst::primitive, primitive_SQRT,      "sqrt"})),
    data(scm_list({symconst::primitive, primitive_GCD,       "gcd"})),
    data(scm_list({symconst::primitive, primitive_LCM,       "lcm"})),

    data(scm_list({symconst::primitive, primitive_ODDP,      "odd?"})),
    data(scm_list({symconst::primitive, primitive_EVENP,     "even?"})),
    data(scm_list({symconst::primitive, primitive_POSITIVEP, "positive?"})),
    data(scm_list({symconst::primitive, primitive_NEGATIVEP, "negative?"})),
    data(scm_list({symconst::primitive, primitive_ZEROP,     "zero?"})),

    data(scm_list({symconst::primitive, primitive_CEILING,  "ceiling"})),
    data(scm_list({symconst::primitive, primitive_FLOOR,    "floor"})),
    data(scm_list({symconst::primitive, primitive_TRUNCATE, "truncate"})),
    data(scm_list({symconst::primitive, primitive_ROUND,    "round"})),

    data(scm_list({symconst::primitive, primitive_COERCE_INEXACT_TO_EXACT, "inexact->exact"})),
    data(scm_list({symconst::primitive, primitive_COERCE_EXACT_TO_INEXACT, "exact->inexact"})),
    data(scm_list({symconst::primitive, primitive_EXACTP,                  "exact?"})),
    data(scm_list({symconst::primitive, primitive_INEXACTP,                "inexact?"})),
    data(scm_list({symconst::primitive, primitive_INTEGERP,                "integer?"})),
    data(scm_list({symconst::primitive, primitive_NUMERATOR,               "numerator"})),
    data(scm_list({symconst::primitive, primitive_DENOMINATOR,             "denominator"})),

    data(scm_list({symconst::primitive, primitive_SIN,   "sin"})),
    data(scm_list({symconst::primitive, primitive_COS,   "cos"})),
    data(scm_list({symconst::primitive, primitive_TAN,   "tan"})),
    data(scm_list({symconst::primitive, primitive_ASIN,  "asin"})),
    data(scm_list({symconst::primitive, primitive_ACOS,  "acos"})),
    data(scm_list({symconst::primitive, primitive_ATAN,  "atan"})),
    data(scm_list({symconst::primitive, primitive_SINH,  "sinh"})),
    data(scm_list({symconst::primitive, primitive_COSH,  "cosh"})),
    data(scm_list({symconst::primitive, primitive_TANH,  "tanh"})),
    data(scm_list({symconst::primitive, primitive_ASINH, "asinh"})),
    data(scm_list({symconst::primitive, primitive_ACOSH, "acosh"})),
    data(scm_list({symconst::primitive, primitive_ATANH, "atanh"})),

    data(scm_list({symconst::primitive, primitive_RANDOM, "random"})),

    data(scm_list({symconst::primitive, primitive_EQ,  "="})),
    data(scm_list({symconst::primitive, primitive_GT,  ">"})),
    data(scm_list({symconst::primitive, primitive_LT,  "<"})),
    data(scm_list({symconst::primitive, primitive_GTE, ">="})),
    data(scm_list({symconst::primitive, primitive_LTE, "<="})),

    data(scm_list({symconst::primitive, primitive_EQP,    "eq?"})),
    data(scm_list({symconst::primitive, primitive_EQVP,   "eqv?"})),
    data(scm_list({symconst::primitive, primitive_EQUALP, "equal?"})),
    data(scm_list({symconst::primitive, primitive_NOT,    "not"})),

    data(scm_list({symconst::primitive, primitive_CHAR_EQ,          "char=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_LT,          "char<?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_GT,          "char>?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_LTE,         "char<=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_GTE,         "char>=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_EQ,       "char-ci=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_LT,       "char-ci<?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_GT,       "char-ci>?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_LTE,      "char-ci<=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_GTE,      "char-ci>=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_ALPHABETICP, "char-alphabetic?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_NUMERICP,    "char-numeric?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_WHITESPACEP, "char-whitespace?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_UPPER_CASEP, "char-upper-case?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_LOWER_CASEP, "char-lower-case?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_UPCASE,      "char-upcase"})),
    data(scm_list({symconst::primitive, primitive_CHAR_DOWNCASE,    "char-downcase"})),

    data(scm_list({symconst::primitive, primitive_MAKE_STRING,   "make-string"})),
    data(scm_list({symconst::primitive, primitive_STRING,        "string"})),
    data(scm_list({symconst::primitive, primitive_STRING_LENGTH, "string-length"})),
    data(scm_list({symconst::primitive, primitive_STRING_REF,    "string-ref"})),
    data(scm_list({symconst::primitive, primitive_STRING_SET,    "string-set!"})),
    data(scm_list({symconst::primitive, primitive_SUBSTRING,     "substring"})),
    data(scm_list({symconst::primitive, primitive_STRING_APPEND, "string-append"})),
    data(scm_list({symconst::primitive, primitive_STRING_COPY,   "string-copy"})),
    data(scm_list({symconst::primitive, primitive_STRING_FILL,   "string-fill!"})),
    data(scm_list({symconst::primitive, primitive_STRING_EQ,     "string=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_LT,     "string<?"})),
    data(scm_list({symconst::primitive, primitive_STRING_GT,     "string>?"})),
    data(scm_list({symconst::primitive, primitive_STRING_LTE,    "string<=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_GTE,    "string>=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_EQ,  "string-ci=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_LT,  "string-ci<?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_GT,  "string-ci>?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_LTE, "string-ci<=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_GTE, "string-ci>=?"})),

    data(scm_list({symconst::primitive, primitive_CONS,   "cons"})),
    data(scm_list({symconst::primitive, primitive_CAR,    "car"})),
    data(scm_list({symconst::primitive, primitive_CDR,    "cdr"})),
    data(scm_list({symconst::primitive, primitive_NULLP,  "null?"})),
    data(scm_list({symconst::primitive, primitive_SETCAR, "set-car!"})),
    data(scm_list({symconst::primitive, primitive_SETCDR, "set-cdr!"})),

    data(scm_list({symconst::primitive, primitive_CAAR, "caar"})),
    data(scm_list({symconst::primitive, primitive_CADR, "cadr"})),
    data(scm_list({symconst::primitive, primitive_CDAR, "cdar"})),
    data(scm_list({symconst::primitive, primitive_CDDR, "cddr"})),
    data(scm_list({symconst::primitive, primitive_CAAAR, "caaar"})),
    data(scm_list({symconst::primitive, primitive_CAADR, "caadr"})),
    data(scm_list({symconst::primitive, primitive_CADAR, "cadar"})),
    data(scm_list({symconst::primitive, primitive_CADDR, "caddr"})),
    data(scm_list({symconst::primitive, primitive_CDAAR, "cdaar"})),
    data(scm_list({symconst::primitive, primitive_CDADR, "cdadr"})),
    data(scm_list({symconst::primitive, primitive_CDDAR, "cddar"})),
    data(scm_list({symconst::primitive, primitive_CDDDR, "cdddr"})),
    data(scm_list({symconst::primitive, primitive_CAAAAR, "caaaar"})),
    data(scm_list({symconst::primitive, primitive_CAAADR, "caaadr"})),
    data(scm_list({symconst::primitive, primitive_CAADAR, "caadar"})),
    data(scm_list({symconst::primitive, primitive_CAADDR, "caaddr"})),
    data(scm_list({symconst::primitive, primitive_CADAAR, "cadaar"})),
    data(scm_list({symconst::primitive, primitive_CADADR, "cadadr"})),
    data(scm_list({symconst::primitive, primitive_CADDAR, "caddar"})),
    data(scm_list({symconst::primitive, primitive_CADDDR, "cadddr"})),
    data(scm_list({symconst::primitive, primitive_CDAAAR, "cdaaar"})),
    data(scm_list({symconst::primitive, primitive_CDAADR, "cdaadr"})),
    data(scm_list({symconst::primitive, primitive_CDADAR, "cdadar"})),
    data(scm_list({symconst::primitive, primitive_CDADDR, "cdaddr"})),
    data(scm_list({symconst::primitive, primitive_CDDAAR, "cddaar"})),
    data(scm_list({symconst::primitive, primitive_CDDADR, "cddadr"})),
    data(scm_list({symconst::primitive, primitive_CDDDAR, "cdddar"})),
    data(scm_list({symconst::primitive, primitive_CDDDDR, "cddddr"})),

    data(scm_list({symconst::primitive, primitive_LIST,      "list"})),
    data(scm_list({symconst::primitive, primitive_LENGTH,    "length"})),
    data(scm_list({symconst::primitive, primitive_LISTP,     "list?"})),
    data(scm_list({symconst::primitive, primitive_APPEND,    "append"})),
    data(scm_list({symconst::primitive, primitive_REVERSE,   "reverse"})),
    data(scm_list({symconst::primitive, primitive_LIST_TAIL, "list-tail"})),
    data(scm_list({symconst::primitive, primitive_LIST_REF,  "list-ref"})),

    data(scm_list({symconst::primitive, primitive_FOR_EACH,   "for-each"})),
    data(scm_list({symconst::primitive, primitive_MAP,        "map"})),
    data(scm_list({symconst::primitive, primitive_FILTER,     "filter"})),
    data(scm_list({symconst::primitive, primitive_FOLD_LEFT,  "fold-left"})),
    data(scm_list({symconst::primitive, primitive_FOLD_RIGHT, "fold-right"})),
    data(scm_list({symconst::primitive, primitive_MERGE,      "merge"})),

    data(scm_list({symconst::primitive, primitive_MEMQ,   "memq"})),
    data(scm_list({symconst::primitive, primitive_MEMV,   "memv"})),
    data(scm_list({symconst::primitive, primitive_MEMBER, "member"})),
    data(scm_list({symconst::primitive, primitive_ASSQ,   "assq"})),
    data(scm_list({symconst::primitive, primitive_ASSV,   "assv"})),
    data(scm_list({symconst::primitive, primitive_ASSOC,  "assoc"})),

    data(scm_list({symconst::primitive, primitive_VECTOR,        "vector"})),
    data(scm_list({symconst::primitive, primitive_MAKE_VECTOR,   "make-vector"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_LENGTH, "vector-length"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_REF,    "vector-ref"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_SET,    "vector-set!"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_FILL,   "vector-fill!"})),

    data(scm_list({symconst::primitive, primitive_PAIRP,      "pair?"})),
    data(scm_list({symconst::primitive, primitive_VECTORP,    "vector?"})),
    data(scm_list({symconst::primitive, primitive_CHARP,      "char?"})),
    data(scm_list({symconst::primitive, primitive_NUMBERP,    "number?"})),
    data(scm_list({symconst::primitive, primitive_REALP,      "real?"})),
    data(scm_list({symconst::primitive, primitive_RATIONALP,  "rational?"})),
    data(scm_list({symconst::primitive, primitive_STRINGP,    "string?"})),
    data(scm_list({symconst::primitive, primitive_SYMBOLP,    "symbol?"})),
    data(scm_list({symconst::primitive, primitive_BOOLEANP,   "boolean?"})),
    data(scm_list({symconst::primitive, primitive_ATOMP,      "atom?"})),
    data(scm_list({symconst::primitive, primitive_PROCEDUREP, "procedure?"})),

    data(scm_list({symconst::primitive, primitive_EVAL,  "eval"})),
    data(scm_list({symconst::primitive, primitive_APPLY, "apply"})),

    data(scm_list({symconst::primitive, primitive_DELAYP, "delay?"})),
    data(scm_list({symconst::primitive, primitive_FORCE,  "force"})),

    data(scm_list({symconst::primitive, primitive_STREAM_CAR,   "stream-car"})),
    data(scm_list({symconst::primitive, primitive_STREAM_CDR,   "stream-cdr"})),
    data(scm_list({symconst::primitive, primitive_STREAM_NULLP, "stream-null?"})),

    data(scm_list({symconst::primitive, primitive_COERCE_CHAR_TO_INTEGER,  "char->integer"})),
    data(scm_list({symconst::primitive, primitive_COERCE_INTEGER_TO_CHAR,  "integer->char"})),
    data(scm_list({symconst::primitive, primitive_COERCE_NUMBER_TO_STRING, "number->string"})),
    data(scm_list({symconst::primitive, primitive_COERCE_STRING_TO_NUMBER, "string->number"})),
    data(scm_list({symconst::primitive, primitive_COERCE_STRING_TO_SYMBOL, "string->symbol"})),
    data(scm_list({symconst::primitive, primitive_COERCE_SYMBOL_TO_STRING, "symbol->string"})),
    data(scm_list({symconst::primitive, primitive_COERCE_VECTOR_TO_LIST,   "vector->list"})),
    data(scm_list({symconst::primitive, primitive_COERCE_LIST_TO_VECTOR,   "list->vector"})),
    data(scm_list({symconst::primitive, primitive_STRING_TO_LIST,          "string->list"})),
    data(scm_list({symconst::primitive, primitive_LIST_TO_STRING,          "list->string"})),

    data(scm_list({symconst::primitive, primitive_SET_MAX_RECURSION_DEPTH, "set-max-recursion-depth!"})),

    data(scm_list({symconst::primitive, primitive_ERROR, "error"})),
  });


frame_vars primitive_procedure_names() {
  const auto n = primitive_procedure_objects.size();
  frame_vars names(n);
  for(size_type i = 0; i < n; ++i)
    names[i] = primitive_procedure_objects[i].value.exp[2].value.sym;
  return names;
}
#endif