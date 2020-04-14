// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_primitives_toolkit.hpp
// => Defines helper functions for the Heist Scheme Interpreter's C++ primitives

#ifndef HEIST_PRIMITIVES_TOOLKIT_HPP_
#define HEIST_PRIMITIVES_TOOLKIT_HPP_

#include "heist_types.hpp"

/******************************************************************************
* PRIMITIVE HELPER FUNCTIONS ENUMERATION & PROTOTYPES
******************************************************************************/

//         -- HELPER STATUS CODE ENUMERATION
enum class list_status {ok, cyclic, no_null};


//       -- FROM THE EVALUATOR
bool     is_true(const scm_list& exp);
bool     is_non_escaped_double_quote(size_type i, const scm_string& input);
void     parse_input_exp(scm_string&& input, scm_list& abstract_syntax_tree);
data     data_cast(const scm_list& l);
scm_list scm_list_cast(const data& d);
scm_list make_delay(const scm_list& exp, env_type& env);
scm_list read_user_input(FILE* outs,FILE* ins,const bool& in_repl=true);
scm_list scm_eval(scm_list&& exp, env_type& env);
scm_list execute_application(scm_list&& procedure,scm_list& arguments,env_type& env);
env_type setup_environment();
constexpr bool IS_END_OF_WORD(const char& c, const char& c2);
std::pair<chr_type,scm_string> data_is_named_char(const size_type& i,
                                                  const scm_string& input);


//          -- FROM PRIMITIVES & ITS TOOLKIT
data        primitive_PROCEDUREP(scm_list& args);
data        primitive_LIST(scm_list& args);
data        primitive_LISTP(scm_list& args);
bool        prm_compare_PAIRs(par_type& p1, par_type& p2);
bool        prm_compare_VECTs(vec_type& v1, vec_type& v2);
bool        prm_compare_SCM_LISTs(scm_list& l1, scm_list& l2);
bool        data_is_the_empty_expression(const data& d);
void        shallow_unpack_list_into_exp(data& curr_pair, scm_list& args_list);
num_type    primitive_guarenteed_list_length(const data& d);
list_status primitive_list_is_acyclic_and_null_terminated(const data& curr_pair);

/******************************************************************************
* PRIMITIVE CONSTANT VALUES
******************************************************************************/

const auto FALSE_DATA_BOOLEAN = data(boolean(false));
const auto TRUE_DATA_BOOLEAN  = data(boolean(true));

/******************************************************************************
* GENERAL PRIMITIVE HELPERS
******************************************************************************/

// Determine whether a primitive was given no args (ie only the sentinel arg)
bool no_args_given(const scm_list& args) {
  return args.empty() || (args.size()==1 && 
         args[0].is_type(types::sym) && 
         args[0].value.sym == SENTINEL_ARG);
}


// Counts the number of arguments given
size_type count_args(const scm_list& args) {
  if(no_args_given(args)) return 0;
  return args.size();
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
void primitive_confirm_data_is_a_procedure(const data& d,      const char* name, 
                                           const char* format, const scm_list& args){
  if(scm_list proc({d}); !primitive_PROCEDUREP(proc).value.bol.val)
    THROW_ERR('\'' << name << " arg " << PROFILE(d) 
      << " isn't a procedure!" << format << FCN_ERR(name,args));
}


// Confirm data is a valid container index
bool primitive_is_valid_index(const data& d) {
  return d.is_type(types::num) && d.value.num.is_integer() &&
         !d.value.num.is_neg() && d.value.num <= MAX_SIZE_TYPE;
}


// Confirm data is a valid container size
bool primitive_is_valid_size(const data& d) {
  return d.is_type(types::num) && d.value.num.is_integer() &&
         d.value.num.is_pos() && d.value.num <= MAX_SIZE_TYPE;
}


// Confirm (apply procedure args) = true
bool is_true_scm_condition(scm_list& procedure,scm_list& args,env_type& env){
  return is_true(execute_application(std::move(procedure),args,env));
}

/******************************************************************************
* NUMERIC PRIMITIVE HELPERS
******************************************************************************/

// Confirms numeric-arg-based primitive given >= 1 arg & ONLY numeric args
void confirm_no_numeric_primitive_errors(const scm_list& args, 
                                         const char* primitive_name, 
                                         const char* format) {
  if(no_args_given(args))
    THROW_ERR('\''<<primitive_name<<" recieved no arguments!\n     "<<format
      <<FCN_ERR(primitive_name,args));
  if(auto idx = confirm_only_args_of_type(args,types::num); idx > -1)
    THROW_ERR('\''<<primitive_name<<" recieved non-numeric argument: "
      <<PROFILE(args[idx])<<"!\n     "<<format<<FCN_ERR(primitive_name,args));
}


// Confirms given n arguments
void confirm_n_args(const size_type n,          const scm_list& args, 
                    const char* primitive_name, const char* format) {
  if(args.size() != n && n != 1)
    THROW_ERR('\'' << primitive_name << " didn't receive "<< n
      << " arguments (given " << args.size() << ")!\n     " << format
      << FCN_ERR(primitive_name,args));
  if(args.size() != n) 
      THROW_ERR('\'' << primitive_name << " didn't receive 1 argument (given "
        <<args.size()<<")!\n     "<<format<<FCN_ERR(primitive_name,args));
}


// Confirms given 1 valid numeric argument
void confirm_unary_numeric(const scm_list& args, const char* primitive_name, 
                                                 const char* format){
  confirm_no_numeric_primitive_errors(args, primitive_name, format);
  confirm_n_args(1, args, primitive_name, format);
}

/******************************************************************************
* EQUALITY PRIMITIVE HELPERS
******************************************************************************/

// compares 2 given atomic value fields of types::t
bool prm_compare_atomic_values(data_value_field& v1, data_value_field& v2, types t){
  if(t == types::undefined || t == types::exe) return true;
  else if(t == types::num) return v1.num == v2.num && 
                                  v1.num.is_exact() == v2.num.is_exact();
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
  else if(t == types::fip) return v1.fip.port_idx == v2.fip.port_idx;
  else if(t == types::fop) return v1.fop.port_idx == v2.fop.port_idx;
  else if(t == types::exp) return prm_compare_SCM_LISTs(v1.exp, v2.exp);
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

/******************************************************************************
* CHARACTER PRIMITIVE HELPERS
******************************************************************************/

// Confirm given N character/string args
void confirm_given_char_string_args(const scm_list& args, const char* name, 
                                                          const scm_string& type){
  if(no_args_given(args))
    THROW_ERR('\''<<name<<" didn't recieved any args:\n     ("<<name
      <<" <"<<type<<"1> <"<<type<<"2>)"<<FCN_ERR(name,args));
  auto t = type=="char" ? types::chr : types::str;
  for(const auto& arg : args)
    if(!arg.is_type(t))
      THROW_ERR('\''<<name<<" recieved non-"<<type<<" arg "<<PROFILE(arg)
        <<":\n     ("<<name<<" <"<<type<<"1> <"<<type<<"2>)"<<FCN_ERR(name,args));
}


// Confirm given 1 character arg
void confirm_given_one_char_arg(const scm_list& args, const char* name) {
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR('\''<<name<<" didn't recieve 1 arg:\n     ("<<name<<" <char>)"
      << FCN_ERR(name,args));
  if(!args[0].is_type(types::chr))
    THROW_ERR('\''<<name<<" didn't recieve a character arg:\n     ("<<name<<" <char>)"
      "\n     Recieved " << PROFILE(args[0]) << '!' <<FCN_ERR(name,args));
}

/******************************************************************************
* STATIC CONTAINER (STRING & VECTOR) PRIMITIVE HELPERS
******************************************************************************/

// Confirm given a single string or vector argument
void primitive_confirm_valid_str_or_vec_arg(const scm_list& args,const size_type& n,
                                            const char* name,    const char* layout, 
                                                                 const scm_string& type){
  // confirm given correct number of args
  if(no_args_given(args))
    THROW_ERR('\''<<name<<" didn't recieve any arguments!\n     "<<layout<<FCN_ERR(name,args));
  if(args.size() != n)
    THROW_ERR('\''<<name<<" didn't recieve "<<n<<" argument" 
      <<(n>1?"s":"")<<" (given "<<args.size()<<")!\n     "<<layout<<FCN_ERR(name,args));
  // confirm given the correct container
  if(!args[0].is_type(type=="string" ? types::str : types::vec))
    THROW_ERR('\''<<name<<" didn't recieve a "<<type<<" arg!"<<"\n     Variable " 
      <<PROFILE(args[0])<<" isn't a "<<type<<"!\n     "<<layout<<FCN_ERR(name,args));
}


// Confirm given a valid string/vector index. Returns idx as a 'size_type' if so
size_type primitive_confirm_valid_str_or_vec_idx(const scm_list& args,const char* name, 
                                                 const char* layout,  const scm_string& type){
  // confirm given an in-'size_type'-range non-negative index
  if(!primitive_is_valid_index(args[1]))
    THROW_ERR('\''<<name<<" index "<<args[1]<<" isn't a proper non-negative integer!"
      "\n     "<<layout<< "\n     <index> range: [0," << MAX_SIZE_TYPE << ']'
      << FCN_ERR(name,args));
  // confirm index falls w/in range of the container
  const size_type i = (size_type)args[1].value.num.extract_inexact();
  const size_type l = (type=="string") ? args[0].value.str->size() : args[0].value.vec->size();
  if(i >= l)
    THROW_ERR('\''<<name<<" recieved out of range index " << i 
      <<"\n     for "<<type<<' '<<args[0]<<" of size "<<l<<'!'<<FCN_ERR(name,args));
  return i;
}


// Confirm given a proper list to be coerced & return whether the list is empty
bool primitive_validate_list_and_return_if_empty(scm_list& args, const scm_string& name){
  // confirm valid arg length  
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR('\''<<name<<" recieved incorrect # of arguments:"
      "\n     ("<<name<<" <list>)"<<FCN_ERR(name,args));
  // return an empty string/vector if given an empty list
  if(data_is_the_empty_expression(args[0])) return true;
  // confirm given a proper list
  if(!args[0].is_type(types::par)) {
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't a proper list:"
      "\n     ("<<name<<" <list>)"<<FCN_ERR(name,args));
  } else if(auto list_stat = primitive_list_is_acyclic_and_null_terminated(args[0]); 
    list_stat == list_status::cyclic) {
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't an acyclic list:"
      "\n     ("<<name<<" <list>)"<<FCN_ERR(name,args));
  } else if(list_stat == list_status::no_null) {
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't a '() terminated list:"
      "\n     ("<<name<<" <list>)"<<FCN_ERR(name,args));
  }
  return false;
}

/******************************************************************************
* STRING PRIMITIVE HELPERS
******************************************************************************/

// Confirm given a single string argument
void primitive_confirm_valid_string_arg(const scm_list& args,const size_type& n,
                                        const char* name,    const char* layout){
  primitive_confirm_valid_str_or_vec_arg(args,n,name,layout,"string");
}


// Confirm given a valid string index. Returns idx as a 'size_type' if so
size_type primitive_confirm_valid_string_idx(const scm_list& args,const char* name, 
                                                                  const char* layout){
  return primitive_confirm_valid_str_or_vec_idx(args,name,layout,"string");
}


// Returns the given string in lowercase letters
scm_string lowercase_str(const scm_string& s) {
  scm_string tmp;
  for(const auto& ch : s) tmp += mklower(ch);
  return tmp;
}

/******************************************************************************
* PAIR PRIMITIVE HELPERS
******************************************************************************/

// confirms a list accessor ('car'/'cdr' etc.) is given appropriate args
void confirm_given_a_pair_arg(scm_list& args, const char* name, const char* format){
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR('\''<<name<<" recieved incorrect # of arguments!"<<format<<FCN_ERR(name,args));
  if(!args[0].is_type(types::par))
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't a pair!"<<format<<FCN_ERR(name,args));
}


// confirm nth car is a pair
void confirm_nth_car_is_pair(const data& nth_arg,const char* name,
                             const char* nth,    const scm_list& args){
  if(!nth_arg.value.par->first.is_type(types::par))
    THROW_ERR('\''<<name<<' '<<nth<<" 'car' "<<PROFILE(nth_arg.value.par->first)
      <<" isn't a pair!"<<FCN_ERR(name,args));
}


// confirm nth cdr is a pair
void confirm_nth_cdr_is_pair(const data& nth_arg,const char* name,
                             const char* nth,    const scm_list& args){
  if(!nth_arg.value.par->second.is_type(types::par))
    THROW_ERR('\''<<name<<' '<<nth<<" 'cdr' "<<PROFILE(nth_arg.value.par->second)
      <<" isn't a pair!"<<FCN_ERR(name,args));
}

/******************************************************************************
* LIST PRIMITIVE HELPERS
******************************************************************************/

// "list" primitive helper fcn: recursively constructs embedded conses
data primitive_LIST_to_CONS_constructor(const scm_node& obj, const scm_node& null_obj){
  if(obj == null_obj) 
    return data(symconst::emptylist);
  data new_pair = data(make_par());
  new_pair.value.par->first = *obj;
  new_pair.value.par->second = primitive_LIST_to_CONS_constructor(obj+1,null_obj);
  return new_pair;
}


// "length" primitive helper fcn: recursively computes a list's length
void primitive_LENGTH_computation(const data& curr_pair, num_type& exact_count, 
                                                         size_type count = 1){
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


// helper function for 'list?' & 'alist?'. Uses the 1st half of the
//   Floyd Loop Detection Algorithm (doesn't need to find WHERE the cycle is).
list_status primitive_list_is_acyclic_and_null_terminated(const data& curr_pair) {
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


// helper function for 'alist?'. confirm the given list only contains pairs
bool primitive_list_only_contains_pairs(const data& curr_pair) {
  if(!curr_pair.value.par->first.is_type(types::par))
    return false;
  if(!curr_pair.value.par->second.is_type(types::par))
    return true;
  return primitive_list_only_contains_pairs(curr_pair.value.par->second);
}


// "append" primitive helper fcn: recursively links 'curr_pair's last cdr to 'link_to'
void primitive_APPEND_list_linker(data& curr_pair, data& link_to) {
  if(curr_pair.is_type(types::par)) 
    primitive_APPEND_list_linker(curr_pair.value.par->second, link_to);
  else 
    curr_pair = link_to;
}


// "sublist" primitive helper fcn: recursively mk sublist from 'curr_pair's [start,end)
void primitive_MK_SUBLIST_recur(data& curr_pair,      const size_type& start, 
                                const size_type& end, const size_type& count, 
                                scm_list& list_exp){
  if(count == end || !curr_pair.is_type(types::par)) return;
  if(count >= start) list_exp.push_back(curr_pair.value.par->first);
  primitive_MK_SUBLIST_recur(curr_pair.value.par->second, start, end, count+1, list_exp);
}


// Confirm given list is proper & index pair is valid. If so, returns sublist
data primitive_list_sublist_extraction(scm_list& args,     const scm_string& name, 
                                       const char* format, const bool& sublist_til_nil) {
  // confirm valid arg length  
  if(args.size() != 3 && !(args.size() == 2 && sublist_til_nil))
    THROW_ERR('\''<<name<<" recieved incorrect # of arguments:"<<format<<FCN_ERR(name,args));
  // confirm given a proper list
  if(!args[0].is_type(types::par) && !data_is_the_empty_expression(args[0]))
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't a proper list:"<<format<<FCN_ERR(name,args));
  else if(auto list_stat = primitive_list_is_acyclic_and_null_terminated(args[0]); 
    list_stat == list_status::cyclic) {
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't an acyclic list:"<<format<<FCN_ERR(name,args));
  } else if(list_stat == list_status::no_null)
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't a '() terminated list:"<<format<<FCN_ERR(name,args));
  
  // confirm given valid in-'size_type'-range non-negative indices
  if(!primitive_is_valid_index(args[1]))
    THROW_ERR('\''<<name<<" index "<<PROFILE(args[1])
      <<" isn't a proper non-negative integer!"<<format<<FCN_ERR(name,args));
  if(!sublist_til_nil && !primitive_is_valid_index(args[2]))
    THROW_ERR('\''<<name<<" index "<<PROFILE(args[2])
      <<" isn't a proper non-negative integer!"<<format<<FCN_ERR(name,args));
  
  // confirm index falls w/in range of the container
  const size_type start = (size_type)args[1].value.num.extract_inexact();
  const size_type length = (size_type)primitive_guarenteed_list_length(args[0]).extract_inexact();
  const size_type end = (sublist_til_nil) ? length : (size_type)args[2].value.num.extract_inexact();
  if(start > end && sublist_til_nil) 
    THROW_ERR('\''<<name<<" index "<<start<<" is out of range for list "
      << args[0] <<" of length "<< length <<'!'<<format<<FCN_ERR(name,args));
  if(start > end)
    THROW_ERR('\''<<name<<" <start> index ("<<start<<") > <end> index ("
      << end<<")!"<<format<<FCN_ERR(name,args));
  if(end > length)
    THROW_ERR('\''<<name<<" index "<<end<<" is out of range for list "
      << args[0]<<" of length "<<length<<'!'<<format<<FCN_ERR(name,args));
  
  // Extract the sublist
  if(start == end) return symconst::emptylist; // start = end -> '()
  scm_list sublist_exp;
  primitive_MK_SUBLIST_recur(args[0], start, end, 0, sublist_exp);
  return primitive_LIST(sublist_exp);
}

/******************************************************************************
* LIST CONTROL FEATURES PRIMITIVE HELPERS
******************************************************************************/

// Compute length of a guarenteed 'data' list 
//   (avoids redundant checks for 'data' not being a list)
num_type primitive_guarenteed_list_length(const data& d) {
  if(data_is_the_empty_expression(d)) return num_type();
  num_type count;
  primitive_LENGTH_computation(d.value.par->second,count);
  return count;
}


// Confirm lists are proper & have the same length
void primitive_confirm_proper_same_sized_lists(const scm_list& lists, 
                                               const char* name, const char* format,
                                               const int& first_arg_pos,
                                               const scm_list& args){
  num_type length0;
  for(size_type i = 0, n = lists.size(); i < n; ++i) {
    // confirm proper list
    if(scm_list arg_list({lists[i]}); !primitive_LISTP(arg_list).value.bol.val)
      THROW_ERR('\''<<name<<" arg #" << first_arg_pos+i+1 << ' ' << PROFILE(lists[i])
        << " isn't a proper list!" << format << FCN_ERR(name,args));
    // confirm congruent length
    if(i == 0) 
      length0 = primitive_guarenteed_list_length(lists[i]);
    else if(length0 != primitive_guarenteed_list_length(lists[i]))
      THROW_ERR('\''<<name<<" lists "<< lists[0] << " and " 
        << lists[i] << " differ in length!" << format << FCN_ERR(name,args));
  }
}


// Primitive helper function for "for-each" & "map": extract each list's head
void primitive_FOREACH_MAP_get_list_heads(scm_list& args, scm_list& list_heads,
                                          const char* name,const char* format){
  // Confirm given minimum # of args needed
  if(args.size() < 2)
    THROW_ERR('\''<<name<<" recieved insufficient args (only "
      << count_args(args) << "):" << format << FCN_ERR(name, args));
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], name, format, args);
  // Confirm only given proper lists of the same length
  primitive_confirm_proper_same_sized_lists(list_heads,name,format,1,args);
}


// Adds cars to 'args' & advances cdrs. Returns whether lists are empty
bool check_empty_list_else_acquire_cars_advance_cdrs(scm_list& curr_pairs, 
                                                     scm_list& args){
  // Return if fully iterated through each list
  if(!curr_pairs[0].is_type(types::par)) return true;
  // Add each arg for 'proc' & advance each list's head ptr
  for(size_type i = 0, n = curr_pairs.size(); i < n; ++i) {
    args[i] = curr_pairs[i].value.par->first;
    curr_pairs[i] = curr_pairs[i].value.par->second;
  }
  return false;
}


// "for-each" primitive helper fcn: recursively applies 'proc' to each 'curr_pair'
void primitive_FOR_EACH_applicator(scm_list& curr_pairs, scm_list& proc, env_type& env) {
  scm_list args(curr_pairs.size());
  if(check_empty_list_else_acquire_cars_advance_cdrs(curr_pairs,args)) return;
  // Execute proc & recurse down the rest of the lists
  execute_application(std::move(proc),args,env);
  primitive_FOR_EACH_applicator(curr_pairs, proc, env);
}


// "map" primitive helper fcn: recursively applies 'proc' to each 'curr_pair' 
//   & stores the result
void primitive_MAP_list_constructor(scm_list& curr_pairs, scm_list& proc, 
                                    scm_list& mapped_list, env_type& env) {
  scm_list args(curr_pairs.size());
  if(check_empty_list_else_acquire_cars_advance_cdrs(curr_pairs,args)) return;
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
  if(is_true_scm_condition(proc,arg,env))
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
data primitive_FOLD_template(scm_list& args,     const char* name, 
                             const char* format, const bool& folding_left){
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given correct # of args needed
  if(args.size() < 3) 
    THROW_ERR('\''<<name<<" recieved insufficient args (only " 
      << count_args(args) << "):" << format << FCN_ERR(name,args));
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0],name,format,args);
  // Confirm only given proper lists of the same length
  scm_list list_heads(args.begin()+2, args.end());
  primitive_confirm_proper_same_sized_lists(list_heads,name,format,2,args);
  // Apply the procedure on each elt of each list, & accumulate the result
  data init_val = args[1];
  primitive_FOLD_accumulator(list_heads,args[0].value.exp,init_val,env,folding_left);
  return init_val; // return the accumulated value
}


// primitive "unfold" & "vector-unfold" procedure helper template:
void primitive_UNFOLD_template(scm_list& args,scm_list& unfolded,
                               const char* name,const char* format){
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm 'unfold call has a proper argument signature
  if(args.size() != 4)
    THROW_ERR('\''<<name<<" recieved incorrect # of args:"<<format<<FCN_ERR(name,args));
  primitive_confirm_data_is_a_procedure(args[0], name, format, args);
  primitive_confirm_data_is_a_procedure(args[1], name, format, args);
  primitive_confirm_data_is_a_procedure(args[2], name, format, args);
  // unfold the seed into a list
  auto& break_condition = args[0].value.exp;
  auto& mapper          = args[1].value.exp;
  auto& incrementer     = args[2].value.exp;
  scm_list seed({args[3]});
  while(!is_true_scm_condition(break_condition,seed,env)) {
    unfolded.push_back(data_cast(execute_application(std::move(mapper),seed,env)));
    seed[0] = data_cast(execute_application(std::move(incrementer),seed,env));
  }
}

/******************************************************************************
* LIST MEMBER EXTRACTION PRIMITIVE HELPERS
******************************************************************************/

// "member" "memv" "memq" primitive helper: recursively compares cars to 'obj'
//   & returns a sublist w/ 'obj' as its 'car' if found. Else returns #f
data primitive_MEM_car_comparison(data& curr_pair, const data& obj, 
                                                   const prm_type& equality_fcn){
  if(!curr_pair.is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  if(scm_list args({curr_pair.value.par->first, obj}); equality_fcn(args).value.bol.val)
    return curr_pair;
  return primitive_MEM_car_comparison(curr_pair.value.par->second, obj, equality_fcn);
}


// Template helper fcn for the "member" "memv" "memq" primitives.
data primitive_MEM_template(scm_list& args, const char* name, 
                                            const prm_type& equality_fcn){
  // Confirm given the correct # of args
  if(args.size() != 2)
    THROW_ERR('\''<<name<<" recieved incorrect # of args:\n     ("
      <<name<<" <obj> <list>)"<<FCN_ERR(name,args));
  // (<mem> <obj> '()) = #f
  if(primitive_IS_THE_EMPTY_LIST(args[1]))
    return FALSE_DATA_BOOLEAN;
  // Confirm given a proper list
  if(scm_list list_arg({args[1]}); !primitive_LISTP(list_arg).value.bol.val)
    THROW_ERR('\''<<name<<" arg #2 "<<PROFILE(args[1])
      <<" isn't a proper list!\n     ("<<name<<" <obj> <list>)"<<FCN_ERR(name,args));
  // Get the sublist w/ 'obj' at its head (if exists)
  return primitive_MEM_car_comparison(args[1], args[0], equality_fcn);
}


// "assoc" "assv" "assq" primitive helper: recursively compares pairs' cars to 
//   'obj' & returns a pair w/ 'obj' as its 'car' if found. If finds a non-pair, 
//   throws an error. Else returns #f.
data primitive_ASSOCIATION_key_seeker(data& curr_pair,  const data& obj, 
                                      const data& head, const char* name, 
                                      const prm_type& equality_fcn,
                                      const scm_list& args) {
  if(!curr_pair.is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  if(!curr_pair.value.par->first.is_type(types::par))
    THROW_ERR('\''<<name<<" arg #2 "<<head
      <<" isn't a proper association list (list of pairs)!"
      "\n     ("<<name<<" <obj> <association-list>)"<<FCN_ERR(name,args));
  if(scm_list args({curr_pair.value.par->first.value.par->first, obj}); 
     equality_fcn(args).value.bol.val)
    return curr_pair.value.par->first;
  return primitive_ASSOCIATION_key_seeker(curr_pair.value.par->second, 
                                          obj,head,name,equality_fcn,args);
}


// Template helper fcn for the "assoc" "assv" "assq" primitives.
data primitive_ASSOCIATION_template(scm_list& args, const char* name, 
                                                    const prm_type& equality_fcn){
  // Confirm given the correct # of args
  if(args.size() != 2)
    THROW_ERR('\''<<name<<" recieved incorrect # of args:\n     ("
      <<name<<" <obj> <association-list>)"<<FCN_ERR(name,args));
  // (<mem> <obj> '()) = #f
  if(primitive_IS_THE_EMPTY_LIST(args[1]))
    return FALSE_DATA_BOOLEAN;
  // Confirm given a proper list
  if(scm_list list_arg({args[1]}); !primitive_LISTP(list_arg).value.bol.val)
    THROW_ERR('\''<<name<<" arg #2 "<<PROFILE(args[1])<<" isn't a proper list!"
      "\n     ("<<name<<" <obj> <association-list>)"<<FCN_ERR(name,args));
  // Get the sublist w/ 'obj' at its head (if exists)
  return primitive_ASSOCIATION_key_seeker(args[1],args[0],args[1],name,equality_fcn,args);
}

/******************************************************************************
* VECTOR PRIMITIVE HELPERS
******************************************************************************/

// Confirm given a single vector argument
void primitive_confirm_valid_vector_arg(const scm_list& args,const size_type& n,
                                        const char* name,    const char* layout) {
  primitive_confirm_valid_str_or_vec_arg(args,n,name,layout,"vector");
}


// Confirm given a valid vector index. Returns idx as a 'size_type' if so
size_type primitive_confirm_valid_vector_idx(const scm_list& args,const char* name, 
                                                                  const char* layout) {
  return primitive_confirm_valid_str_or_vec_idx(args,name,layout,"vector");
}


// Confirm given index pair is valid. If so, returns subvector
data primitive_subvector_extraction(scm_list& args,     const scm_string& name, 
                                   const char* format,  const bool& subvec_til_nil) {
  // confirm valid arg length  
  if(args.size() != 3 && !(args.size() == 2 && subvec_til_nil))
    THROW_ERR('\''<<name<<" recieved incorrect # of arguments:"<<format<<FCN_ERR(name,args));
  // confirm given a vector
  if(!args[0].is_type(types::vec))
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't a vector:"<<format<<FCN_ERR(name,args));
  
  // confirm given valid in-'size_type'-range non-negative indices
  if(!primitive_is_valid_index(args[1]))
    THROW_ERR('\''<<name<<" index "<<PROFILE(args[1])
      <<" isn't a proper non-negative integer!"<<format<<FCN_ERR(name,args));
  if(!subvec_til_nil && !primitive_is_valid_index(args[2]))
    THROW_ERR('\''<<name<<" index "<<PROFILE(args[2])
      <<" isn't a proper non-negative integer!"<<format<<FCN_ERR(name,args));
  
  // confirm index falls w/in range of the container
  const size_type start = (size_type)args[1].value.num.extract_inexact();
  const size_type length = args[0].value.vec->size();
  const size_type end = (subvec_til_nil) ? length : (size_type)args[2].value.num.extract_inexact();
  if(start > end && subvec_til_nil) 
    THROW_ERR('\''<<name<<" index "<<start<<" is out of range for vector "
      <<args[0]<<" of length "<<length<<'!'<<format<<FCN_ERR(name,args));
  if(start > end)
    THROW_ERR('\''<<name<<" <start> index ("<<start<<") > <end> index ("
      <<end<<")!"<<format<<FCN_ERR(name,args));
  if(end > length)
    THROW_ERR('\''<<name<<" index "<<end<<" is out of range for vector "
      <<args[0]<<" of length "<<length<< '!'<<format<<FCN_ERR(name,args));
  
  // Extract the subvector
  if(start == end) return make_vec(scm_list{}); // start = end -> '#()
  return make_vec(scm_list(args[0].value.vec->begin()+start, args[0].value.vec->begin()+end));
}


// Confirm all data in 'vectors' are of types::vec & have the same length
void primitive_confirm_same_sized_vector(scm_list& vectors,const char* format,
                                                           const scm_list& args){
  const size_type size = vectors[0].is_type(types::vec) ? 
                         vectors[0].value.vec->size()  : 0;
  for(const auto& v : vectors) {
    if(!v.is_type(types::vec))
      THROW_ERR("'vector-map "<<PROFILE(v)<<" isn't a vector!"<<format
        << FCN_ERR("vector-map",args));
    if(v.value.vec->size() != size)
      THROW_ERR("'vector-map vectors "<<v<<" and "<<vectors[0]
        << " differ in length!" << format << FCN_ERR("vector-map",args));
  }
}


// "vector-map" primitive helper fcn: maps 'proc' to each elt in each vector
void primitive_MAP_vector_constructor(scm_list& vectors, scm_list& proc, 
                                      scm_list& mapped_vector, env_type& env) {
  const size_type total_vecs = vectors.size();
  const size_type total_elts = vectors[0].value.vec->size();
  // Traverse each elt
  for(size_type i = 0, j; i < total_elts; ++i) {
    scm_list args(total_vecs);
    // Aquire ith elt from each vector
    for(j = 0; j < total_vecs; ++j)
      args[j] = vectors[j].value.vec->operator[](i);
    // Push the mapped result of each elt
    mapped_vector.push_back(data_cast(execute_application(std::move(proc),args,env)));
  }
}

/******************************************************************************
* SORTING PRIMITIVES HELPERS: FOR BOTH LISTS & VECTORS
******************************************************************************/

// Convert the given AST container to a pair or vector
data cast_ast_container_to_scheme(const bool& is_vector, scm_list& container){
  if(is_vector) 
    return data(make_vec(container));
  return primitive_LIST(container);
}


// Convert the given pair or vector to an AST container
void cast_scheme_container_to_ast(const bool& is_vector,data& scm_container,
                                                        scm_list& container){
  if(is_vector) container = *scm_container.value.vec;
  else          shallow_unpack_list_into_exp(scm_container,container);
}


void primitive_confirm_sortable_container(scm_list& args, const char* name, 
                                                          const char* format){
  // confirm given enough arguments & a valid procedure
  if(args.size() != 2)
    THROW_ERR('\''<<name<<" recieved incorrect # of args!"<<format<<FCN_ERR(name,args));
  primitive_confirm_data_is_a_procedure(args[0], name, format, args);
  // the empty list is a valid list
  if(primitive_IS_THE_EMPTY_LIST(args[1])) return;
  // confirm given either a list or vector
  if(!args[1].is_type(types::vec) && !args[1].is_type(types::par))
    THROW_ERR('\''<<name<<' '<<PROFILE(args[1])<<" is neither a list nor a vector!"
      << format << FCN_ERR(name,args));
  // return immediately if given a vector
  if(args[1].is_type(types::vec)) return;
  // confirm list is proper
  if(auto stat = primitive_list_is_acyclic_and_null_terminated(args[1]);
     stat == list_status::cyclic)
    THROW_ERR('\''<<name<<' '<<PROFILE(args[1])<<" isn't an acyclic list!"<<format
      << FCN_ERR(name,args));
  else if(stat == list_status::no_null)
    THROW_ERR('\''<<name<<' '<<PROFILE(args[1])<<" isn't a proper list!"<<format
      << FCN_ERR(name,args));
}


// Sort the args[1] vector or list container using the args[0] procedure
data primitive_sort_container(scm_list& args, env_type& env) {
  scm_list container;
  const bool sorting_a_vector = args[1].is_type(types::vec);
  cast_scheme_container_to_ast(sorting_a_vector,args[1],container);
  // sort unpacked container
  if(container.size() > 1) {
    std::sort(container.begin(), container.end(),
      [procedure=std::move(args[0].value.exp),env=std::move(env)]
      (data& lhs, data& rhs) mutable {
        scm_list args_list({lhs, rhs});
        return is_true_scm_condition(procedure,args_list,env);
      });
  }
  // return the sorted container
  return cast_ast_container_to_scheme(sorting_a_vector,container);
}


// "merge" primitive helper fcn: recursively applies 'proc' to each 'curr_pair' 
//   & stores the args as per the result
void primitive_MERGE_list_constructor(scm_list& curr_pairs, scm_list& proc, 
                                      scm_list& merged_list, env_type& env){
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
  if(is_true_scm_condition(proc,args,env)) {
    merged_list.push_back(args[0]);
    curr_pairs[0] = curr_pairs[0].value.par->second;
  } else {
    merged_list.push_back(args[1]);
    curr_pairs[1] = curr_pairs[1].value.par->second;
  }
  primitive_MERGE_list_constructor(curr_pairs, proc, merged_list, env);
}


// "merge" primitive helper fcn: applies 'proc' to each 'v1' & 'v2' element & 
//   stores the args as per the result
void primitive_MERGE_vector_constructor(scm_list& v1, scm_list& v2, scm_list& proc,
                                        scm_list& merged_vect, env_type& env){
  const size_type n1 = v1.size(), n2 = v2.size();
  size_type i = 0, j = 0;
  // Merge vectors
  for(; i < n1 && j < n2;) {
    scm_list args({v1[i], v2[j]});
    if(is_true_scm_condition(proc,args,env))
      merged_vect.push_back(v1[i]), ++i;
    else
      merged_vect.push_back(v2[j]), ++j;
  }
  // If fully iterated through 1 vector, append elts of the non-empty vector
  if(i != n1)
    merged_vect.insert(merged_vect.end(), v1.begin()+i, v1.end());
  else if(j != n2)
    merged_vect.insert(merged_vect.end(), v2.begin()+j, v2.end());
}

/******************************************************************************
* EVAL/APPLY PRIMITIVE HELPERS
******************************************************************************/

// [ EVAL ] Confirms whether given data is the AST's repn of ()
bool data_is_the_empty_expression(const data& d) {
  return d.is_type(types::sym) && d.value.sym==THE_EMPTY_LIST;
}


// [ EVAL ] Confirms pair is the invocation of an argless procedure
bool evaling_an_argless_procedure(const par_type& par) {
  return primitive_IS_THE_EMPTY_LIST(par->second);
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

/******************************************************************************
* FORCE-DELAY PRIMITIVE HELPERS
******************************************************************************/

bool data_is_a_delay(const data& d) {
  return d.is_type(types::exp) && !d.value.exp.empty() &&
    d.value.exp[0].is_type(types::sym) && d.value.exp[0].value.sym == "delay";
}


data force_data_delay(data& d) {
  if(!data_is_a_delay(d))
    THROW_ERR("'force "<<PROFILE(d)<<" isn't a delayed expression:\n     "
      "(force <delayed-expression>)"<<EXP_ERR("(force "<<d.cio_str()<<')'));
  auto delay = d.value.exp[1].value.del;
  if(!delay->already_forced) {
    delay->already_forced = true;
    delay->result = data_cast(scm_eval(std::move(delay->exp),delay->env));
  }
  return delay->result; // Memoize delays, "call by need" evaluation
}

/******************************************************************************
* STREAM PRIMITIVE HELPERS
******************************************************************************/

constexpr const char * const STREAM_SCXXXXR_ACCESSOR_NUMBER[] = {
  "", " 1st", " 2nd", " 3rd", " 4th", 
};


bool data_is_stream_pair(const data& d) {
  return d.is_type(types::par) && data_is_a_delay(d.value.par->first) && 
                                  data_is_a_delay(d.value.par->second);
}


bool data_is_stream(const data& d) {
  return data_is_the_empty_expression(d) || data_is_stream_pair(d);
}


void confirm_given_a_stream_pair_arg(scm_list& args, const char* name, 
                                                     const char* format){
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR('\''<<name<<" recieved incorrect # of args (given "
      << count_args(args) << "):" << format << FCN_ERR(name,args));
  if(!data_is_stream_pair(args[0]))
    THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't a stream-pair:" 
      << format << FCN_ERR(name,args));
}


// l-value scar & scdr
data get_stream_data_car(data& d) {
  if(!data_is_stream_pair(d))
    THROW_ERR("'scar "<<PROFILE(d)<<" isn't a stream-pair:" 
      "\n     (scar <stream-pair>)" << 
      EXP_ERR("(scar " << d.cio_str() << ')'));
  return force_data_delay(d.value.par->first);
}


data get_stream_data_cdr(data& d) {
  if(!data_is_stream_pair(d))
    THROW_ERR("'scdr "<<PROFILE(d)<<" isn't a stream-pair:\n     "
      "(scdr <stream-pair>)" << EXP_ERR("(scdr " << d.cio_str() << ')'));
  data cdr_promise = force_data_delay(d.value.par->second);
  if(!data_is_stream(cdr_promise))
    THROW_ERR("'scdr forced cdr " << PROFILE(cdr_promise)
      << " isn't a stream:\n     (scdr <stream-pair>)" 
      << EXP_ERR("(scdr " << d.cio_str() << ')'));
  return cdr_promise;
}


// r-value scar & scdr (for composition in the sc****r permutations)
data get_stream_data_car(data&& d, const char * const name, 
                                   const char * const format,
                                   const size_type& nth_scar){
  if(!data_is_stream_pair(d))
    THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scar]
      <<" 'scar' "<<PROFILE(d)<<" isn't a stream-pair:" 
      <<format<<EXP_ERR("(scar "<<d.cio_str()<<')'));
  return force_data_delay(d.value.par->first);
}


data get_stream_data_cdr(data&& d, const char * const name, 
                                   const char * const format,
                                   const size_type& nth_scdr){
  if(!data_is_stream_pair(d))
    THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scdr]
      <<" 'scdr' "<<PROFILE(d)<<" isn't a stream-pair:"
      <<format<<EXP_ERR("(scdr "<<d.cio_str()<<')'));
  data cdr_promise = force_data_delay(d.value.par->second);
  if(!data_is_stream(cdr_promise))
    THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scdr]
      <<" 'scdr's forced cdr "<<PROFILE(cdr_promise)
      <<" isn't a stream:"<<format<<EXP_ERR("(scdr "<<d.cio_str()<<')'));
  return cdr_promise;
}


// "stream" special form helper fcn: recursively constructs embedded sconses
data primitive_STREAM_to_SCONS_constructor(const scm_node& obj, 
                                           const scm_node& null_obj,
                                           env_type& env){
  if(obj == null_obj)
    return data(scm_list({symconst::list})); // becomes '() once forced
  data new_stream_pair = data(make_par());
  new_stream_pair.value.par->first  = make_delay(scm_list_cast(*obj),env);
  new_stream_pair.value.par->second = make_delay(
                                        scm_list_cast(
                                          primitive_STREAM_to_SCONS_constructor(obj+1,null_obj,env)),
                                        env);
  return new_stream_pair;
}


void unpack_stream_into_exp(data&& curr_pair, scm_list& stream_as_exp) {
  if(!data_is_stream_pair(curr_pair)) return;
  stream_as_exp.push_back(get_stream_data_car(curr_pair));
  unpack_stream_into_exp(get_stream_data_cdr(curr_pair), stream_as_exp);
}


// "stream-length" primitive helper fcn
void primitive_STREAM_LENGTH_computation(data&& curr_pair, 
                                         num_type& exact_count, 
                                         size_type count = 1){
  if(count == MAX_SIZE_TYPE) {
    exact_count += count;
    count = 0;
  }
  if(data_is_stream_pair(curr_pair)) 
    primitive_STREAM_LENGTH_computation(
      get_stream_data_cdr(curr_pair), exact_count, count+1);
  else exact_count += count;
}

/******************************************************************************
* STREAM CONTROL FEATURES PRIMITIVE HELPERS
******************************************************************************/

void primitive_confirm_only_given_streams(const scm_list& streams, 
                                          const char* name, const char* format,
                                          const int& first_arg_pos,
                                          const scm_list& args){
  bool found_null_stream = false, found_pair_stream = false;
  size_type null_stream_pos = 0, pair_stream_pos = 0;
  for(size_type i = 0, n = streams.size(); i < n; ++i) {
    // If '(), confirm no stream-pair's
    if(data_is_the_empty_expression(streams[i])) {
      if(found_pair_stream)
        THROW_ERR('\''<<name<<" arg #" << first_arg_pos+i+1 << ' ' << PROFILE(streams[i]) 
          << " and\n                      arg #" << first_arg_pos+pair_stream_pos+1 << ' ' 
          << PROFILE(streams[pair_stream_pos]) << "\n                      differ in length!");
      null_stream_pos = i, found_null_stream = true;
    // If stream-pair, confirm no '()'s
    } else if(data_is_stream_pair(streams[i])) {
      if(found_null_stream)
        THROW_ERR('\''<<name<<" arg #" << first_arg_pos+i+1 << ' ' << PROFILE(streams[i]) 
          << " and\n                      arg #" << first_arg_pos+null_stream_pos+1 << ' ' 
          << PROFILE(streams[null_stream_pos]) << "\n                      differ in length!");
      pair_stream_pos = i, found_pair_stream = true;
    // Not a stream -- ERROR
    } else {
      THROW_ERR('\''<<name<<" arg #" << first_arg_pos+i+1 << ' ' 
        << PROFILE(streams[i])<<" isn't a stream:"<<format<<FCN_ERR(name,args));
    }
  }
}


// Adds cars to 'args' & advances cdrs. Returns whether streams are empty
bool acquire_scars_advance_scdrs(scm_list& curr_streams, scm_list& args, 
                                                         const char* name, 
                                                         const char* format){
  // Confirm given streams of the same length
  primitive_confirm_only_given_streams(curr_streams,name,format,1,scm_list{});
  // Check if completed parsing every stream
  if(data_is_the_empty_expression(curr_streams[0])) return true;
  // Add each arg for 'proc' & advance each stream's head ptr
  for(size_type i = 0, n = curr_streams.size(); i < n; ++i) {
    args[i]         = get_stream_data_car(curr_streams[i]);
    curr_streams[i] = get_stream_data_cdr(curr_streams[i]);
  }
  return false;
}


void primitive_STREAM_FOR_EACH_applicator(scm_list& curr_streams, scm_list& proc,
                                                                  env_type& env){
  scm_list args(curr_streams.size());
  if(acquire_scars_advance_scdrs(curr_streams,args, "stream-for-each",
    "\n     (stream-for-each <procedure> <stream1> <stream2> ...)")) return;
  // Execute proc & recurse down the rest of the lists
  execute_application(std::move(proc),args,env);
  primitive_STREAM_FOR_EACH_applicator(curr_streams, proc, env);
}


data primitive_REF_DROP_SUBSTREAM_seeker(data&& curr_pair,  const size_type& n, 
                                         const char* name,  const char* format, 
                                         size_type count=1, const bool& must_exist=true){
  if(count == n) return std::move(curr_pair);
  if(data_is_stream_pair(curr_pair))
    return primitive_REF_DROP_SUBSTREAM_seeker(get_stream_data_cdr(curr_pair),
                                               n,name,format,count+1,must_exist);
  if(must_exist)
    THROW_ERR('\''<<name<<" index "<<n<<" is out of range!"<<format);
  return std::move(curr_pair);
}


void primitive_TAKE_SUBSTREAM_seeker(data&& curr_pair,    const size_type& n, 
                                     scm_list& substream, const char* name, 
                                     const char* format,  size_type count=0){
  if(count < n && data_is_stream_pair(curr_pair)) {
    substream.push_back(get_stream_data_car(curr_pair));
    primitive_TAKE_SUBSTREAM_seeker(get_stream_data_cdr(curr_pair),
                                    n,substream,name,format,count+1);
  }
}


data primitive_DROP_WHILE_ctor(data&& curr_pair, scm_list& proc, env_type& env,
                               const char* name, const char* format){
  if(!data_is_stream_pair(curr_pair))
    return std::move(curr_pair);
  if(scm_list args({get_stream_data_car(curr_pair)}); 
    !is_true(execute_application(std::move(proc),args,env)))
    return std::move(curr_pair);
  return primitive_DROP_WHILE_ctor(get_stream_data_cdr(curr_pair),
                                   proc,env,name,format);
}


void primitive_TAKE_WHILE_ctor(data&& curr_pair, scm_list& proc, scm_list& substream, 
                               env_type& env, const char* name, const char* format){
  if(!data_is_stream_pair(curr_pair)) return;
  if(scm_list args({get_stream_data_car(curr_pair)}); 
    !is_true(execute_application(std::move(proc),args,env))) return;
  substream.push_back(get_stream_data_car(curr_pair));
  primitive_TAKE_WHILE_ctor(get_stream_data_cdr(curr_pair),proc,
                            substream,env,name,format);
}


void primitive_STREAM_FOLD_accumulator(data&& curr_pair, scm_list& proc, 
                                       data& init_val,   env_type& env,
                                       const bool& folding_left){
  // Return if fully iterated through stream
  if(!data_is_stream_pair(curr_pair)) return;
  // Execute proc, accumulate result, & recurse down the rest of the lists
  if(folding_left) { // stream-fold-left is preorder
    scm_list args({init_val, get_stream_data_car(curr_pair)});
    init_val = data_cast(execute_application(std::move(proc),args,env));
  }
  primitive_STREAM_FOLD_accumulator(get_stream_data_cdr(curr_pair),
                                    proc,init_val,env,folding_left);
  if(!folding_left) { // stream-fold-right is postorder
    scm_list args({get_stream_data_car(curr_pair), init_val});
    init_val = data_cast(execute_application(std::move(proc),args,env));
  }
}


data primitive_STREAM_FOLD_template(scm_list& args,     const char* name, 
                                    const char* format, const bool& folding_left){
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Convert given proper arg signature
  if(args.size() != 3)
    THROW_ERR('\''<<name<<" recieved incorrect # of args (given " << count_args(args) 
      << "):" << format << FCN_ERR(name,args));
  primitive_confirm_data_is_a_procedure(args[0], name, format, args);
  if(data_is_the_empty_expression(args[2])) // folding '() returns seed
    return args[1];
  if(!data_is_stream_pair(args[2]))
    THROW_ERR('\''<<name<<' '<<PROFILE(args[2])<<" isn't a stream:" 
      << format << FCN_ERR(name,args));
  // Apply the procedure on each elt of each list, & accumulate the result
  data init_val = args[1];
  primitive_STREAM_FOLD_accumulator(std::move(args[2]),args[0].value.exp,
                                    init_val,env,folding_left);
  return init_val; // return the accumulated value
}

// ---------------------------------------------------
// Stream Take/Drop Primitive Data Validation Helpers:
// ---------------------------------------------------

void primitive_TEMPLATE_TAKE_DROP_VALIDATION(scm_list& args, const char* name, 
                                                             const char* format){
  if(args.size() != 2) 
    THROW_ERR('\''<<name<<" recieved incorrect # of args (given "
      << count_args(args) << "):" << format << FCN_ERR(name, args));
  // Confirm given a valid size
  if(!primitive_is_valid_index(args[0]))
    THROW_ERR('\''<<name<<' '<< PROFILE(args[0]) << " isn't a valid size!"
      << format << FCN_ERR(name, args));
  // Confirm given a stream
  if(!data_is_stream(args[1]))
    THROW_ERR('\''<<name<<' '<< PROFILE(args[1]) << " isn't a stream!"
      << format << FCN_ERR(name, args));
}


void primitive_TEMPLATE_TAKE_DROP_WHILE_VALIDATION(scm_list& args, 
                                                   const char* name, 
                                                   const char* format){
  if(args.size() != 2) 
    THROW_ERR('\''<<name<<" recieved incorrect # of args (given "
      << count_args(args) << "):" << format << FCN_ERR(name, args));
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], name, format, args);
  // Confirm given a stream
  if(!data_is_stream(args[1]))
    THROW_ERR('\''<<name<<' '<< PROFILE(args[1]) << " isn't a stream!"
      << format << FCN_ERR(name, args));
}

/******************************************************************************
* TYPE-CHECKING PRIMITIVE HELPER
******************************************************************************/

void confirm_given_one_arg(const scm_list& args, const char* name) {
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR('\''<<name<<" recieved incorrect # of arguments:"
      "\n     ("<<name<<" <obj>)" << FCN_ERR(name,args));
}

/******************************************************************************
* TYPE-COERCION PRIMITIVE HELPERS
******************************************************************************/

// Determine whether input[i] is at a hex value.
// POSTCONDITION: i is returned if input[i] is _NOT_ at a hex value.
//                else, the index of the hex value's closing ';' is returned.
size_type is_symbol_hex_val(size_type i, const size_type& n, const scm_string& input){
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
* READER ERROR INPUT PRIMITIVE HELPERS
******************************************************************************/

// Reader error code enumeration
enum class READER_ERROR {early_end_paren,       incomplete_string, 
                         incomplete_expression, quoted_end_of_buffer,
                         quoted_space,          quoted_end_of_expression};


// Confirm reader error is a non-repl-specific fatal error
constexpr bool is_non_repl_reader_error(const READER_ERROR& err) {
  return err == READER_ERROR::incomplete_string || 
         err == READER_ERROR::incomplete_expression;
}


// Alert error as per read's throw
void alert_reader_error(FILE* outs, const READER_ERROR& read_error, const scm_string& input) {
  if(read_error == READER_ERROR::early_end_paren) {
    if(USING_ANSI_ESCAPE_SEQUENCES) {
      fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------\x1b[0m\n", outs);
      fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m ')' Found Prior A Matching '('!\x1b[0m\n", outs);
      fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------\x1b[0m\n", outs);
      fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
      fputs("\x1b[1m-------------------------------------------\x1b[0m\n", outs);
    } else {
      fputs("\n-------------------------------------------\n", outs);
      fputs("READ ERROR: ')' Found Prior A Matching '('!\n", outs);
      fputs("-------------------------------------------\n", outs);
      fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
      fputs("-------------------------------------------\n", outs);
    }
  } else if(read_error == READER_ERROR::quoted_end_of_buffer) {
    if(USING_ANSI_ESCAPE_SEQUENCES) {
      fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------------\x1b[0m\n", outs);
      fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Quoted a Non-Expression (Unexpected End of Buffer)!\x1b[0m\n", outs);
      fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------------\x1b[0m\n", outs);
      fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
      fputs("\x1b[1m---------------------------------------------------------------\x1b[0m\n", outs);
    } else {
      fputs("\n---------------------------------------------------------------\n", outs);
      fputs("READ ERROR: Quoted a Non-Expression (Unexpected End of Buffer)!\n", outs);
      fputs("---------------------------------------------------------------\n", outs);
      fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
      fputs("---------------------------------------------------------------\n", outs);
    }
  } else if(read_error == READER_ERROR::quoted_end_of_expression) {
    if(USING_ANSI_ESCAPE_SEQUENCES) {
      fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------------\x1b[0m\n", outs);
      fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Quoted a Non-Expression (Unexpected Closing Paren)!\x1b[0m\n", outs);
      fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------------\x1b[0m\n", outs);
      fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
      fputs("\x1b[1m---------------------------------------------------------------\x1b[0m\n", outs);
    } else {
      fputs("\n---------------------------------------------------------------\n", outs);
      fputs("READ ERROR: Quoted a Non-Expression (Unexpected Closing Paren)!\n", outs);
      fputs("---------------------------------------------------------------\n", outs);
      fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
      fputs("---------------------------------------------------------------\n", outs);
    }
  } else if(read_error == READER_ERROR::quoted_space) {
    if(USING_ANSI_ESCAPE_SEQUENCES) {
      fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------------------\x1b[0m\n", outs);
      fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Quoted a Non-Expression (Unexpected Space)!\x1b[0m\n", outs);
      fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------------------\x1b[0m\n", outs);
      fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
      fputs("\x1b[1m-------------------------------------------------------\x1b[0m\n", outs);
    } else {
      fputs("\n-------------------------------------------------------\n", outs);
      fputs("READ ERROR: Quoted a Non-Expression (Unexpected Space)!\n", outs);
      fputs("-------------------------------------------------------\n", outs);
      fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
      fputs("-------------------------------------------------------\n", outs);
    }
  }
}

void alert_non_repl_reader_error(FILE* outs, const READER_ERROR& read_error, const scm_string& input) {
  if(read_error == READER_ERROR::incomplete_string) {
    if(USING_ANSI_ESCAPE_SEQUENCES) {
      fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------\x1b[0m\n",outs);
      fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Incomplete String, Missing a Closing '\"'!\x1b[0m\n",outs);
      fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------\x1b[0m\n",outs);
      fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
      fputs("\x1b[1m-----------------------------------------------------\x1b[0m\n",outs);
    } else {
      fputs("\n-----------------------------------------------------\n",outs);
      fputs("READ ERROR: Incomplete String, Missing a Closing '\"'!\n",outs);
      fputs("-----------------------------------------------------\n",outs);
      fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
      fputs("-----------------------------------------------------\n",outs);
    }
  }
  if(read_error == READER_ERROR::incomplete_expression) {
    if(USING_ANSI_ESCAPE_SEQUENCES) {
      fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------\x1b[0m\n",outs);
      fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Incomplete expression, Missing a Closing ')'!\x1b[0m\n",outs);
      fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------\x1b[0m\n",outs);
      fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
      fputs("\x1b[1m---------------------------------------------------------\x1b[0m\n",outs);
    } else {
      fputs("\n---------------------------------------------------------\n",outs);
      fputs("READ ERROR: Incomplete expression, Missing a Closing ')'!\n",outs);
      fputs("---------------------------------------------------------\n",outs);
      fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
      fputs("---------------------------------------------------------\n",outs);
    }
  }
}

void alert_reader_error(FILE* outs, const size_type& read_error_index, const scm_string& input) {
  if(USING_ANSI_ESCAPE_SEQUENCES) {
    fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------\x1b[0m\n", outs);
    fprintf(outs,"\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Unparsable Type In Expression At Index = %03lu\x1b[0m\n",read_error_index);
    fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------\x1b[0m\n", outs);
    fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
    fputs("\x1b[1m--------------------------------------------------------\x1b[0m\n", outs);
  } else {
    fputs("\n--------------------------------------------------------\n", outs);
    fprintf(outs,"READ ERROR: Unparsable Type In Expression At Index = %03lu\n",read_error_index);
    fputs("--------------------------------------------------------\n", outs);
    fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
    fputs("--------------------------------------------------------\n", outs);
  }
}

/******************************************************************************
* OUTPUT PRIMITIVE HELPERS
******************************************************************************/

// Confirm character c is an escaped character
constexpr bool is_escapable_char(const char& c) {
  return c=='\''||c=='"'||c=='?'||c=='\\'||c=='a'||
         c=='b' ||c=='f'||c=='n'||c=='r'||c=='t' ||
         c=='v';
}


// Constexpr isxdigit
constexpr bool ishexdigit(const char& c) {
  return (c>='0' && c<='9')||(c>='A' && c<='F')||(c>='a' && c<='f');
}


// Confirm char is an escape hexadecimal char
constexpr bool is_hex_escape(const char& c, const char& c2) {
  return c=='x' && ishexdigit(c2);
}


// Confirm char is an escape octal char
constexpr bool is_oct_escape(const char& c) {
  return c>='0' && c<='7';
}


// Retrieve the special character variant of the given escaped character
constexpr char special_char_variant(const char& c) {
  switch(c) {
    case 'a': return '\a'; case 'b': return '\b';
    case 'f': return '\f'; case 'n': return '\n';
    case 'r': return '\r'; case 't': return '\t';
    case 'v': return '\v'; default:  return c;
  }
}


// Inserts the unescaped hex/oct as a char & rm's its escaped representation
void insert_hex_oct_escaped_char(scm_string& str,size_type& i,const int& base){
  str[i] = char(std::stoi(str.substr(i), nullptr, base));
  ++i;
  auto is_parsed_digit = (base == 16) ? ishexdigit : is_oct_escape;
  while(str[i] && is_parsed_digit(str[i])) str.erase(i,1);
  --i; // account for 'unescape_chars's for-loop ++i
}


// Unescape escaped special characters in the given string
void unescape_chars(scm_string& str) {
  for(size_type i = 0; i < str.size(); ++i)
    if(str[i] == '\\') {
      if(is_escapable_char(str[i+1])) {
        str.erase(i,1);
        str[i] = special_char_variant(str[i]);
      } else if(is_hex_escape(str[i+1],str[i+2])) {
        str.erase(i,2);
        insert_hex_oct_escaped_char(str,i,16);
      } else if(is_oct_escape(str[i+1])) {
        str.erase(i,1);
        insert_hex_oct_escaped_char(str,i,8);
      }
    }
}


// Confirm given valid output args
//  => NOTE: 'total_non_port_args' also doubles as the index of 
//           where the port would be if it were given as an arg
void confirm_valid_output_args(const scm_list& args, FILE*& outs, 
                               const size_type& total_non_port_args,
                               const char* name, const char* format){
  // Confirm given 0 args if expected such
  if(!total_non_port_args && no_args_given(args)) return;
  // Confirm given enough non-port args if given no port
  if(!no_args_given(args) && args.size()==total_non_port_args) return;
  // Confirm given an open output port if recieved the optional port arg
  if(!no_args_given(args) && args.size()==total_non_port_args+1) {
    if(args[total_non_port_args].is_type(types::fop) && 
       args[total_non_port_args].value.fop.is_open()) {
      outs = args[total_non_port_args].value.fop.port(); 
      return;
    } else {
      THROW_ERR('\''<< name <<" arg "<< PROFILE(args[total_non_port_args])
        << " isn't an open output port: "<< format << FCN_ERR(name,args));
    }
  }
  THROW_ERR('\''<<name<<" recieved incorrect # of args: "<<format<<FCN_ERR(name,args));
}

/******************************************************************************
* INPUT PRIMITIVE HELPERS
******************************************************************************/

// Confirm given valid input args, & return whether at the input file's EOF
bool confirm_valid_input_args_and_non_EOF(const scm_list& args, FILE*& ins, 
                                          const char* name, bool& reading_stdin){
  if(!no_args_given(args)) {
    if(args.size() == 1) {
      if(args[0].is_type(types::fip) && args[0].value.fip.is_open()) {
        ins = args[0].value.fip.port();
        // NOT reading from stdin requires a specialized parsing algorithm
        if(ins != stdin) {
          if(feof(ins)) return false;
          reading_stdin = false; 
        }
      } else {
        THROW_ERR('\''<< name <<" arg " << PROFILE(args[0]) <<
          " isn't an open input port:"
          "\n     ("<< name <<" <optional-open-input-port>)" <<
          FCN_ERR(name,args));
      }
    } else {
      THROW_ERR('\''<< name <<" recieved incorrect # of args:" 
        "\n     ("<< name <<" <optional-open-input-port>)" <<
        FCN_ERR(name,args));
    }
  }
  return true;
}

/******************************************************************************
* READ PORT PRIMITIVE HELPERS
******************************************************************************/

namespace read_port {
  // Determine the read char's length (for reading from a port)
  // PRECONDITION: input[i] = the 1st char after the '#\' of the char literal
  size_type character_length(const size_type& i, const scm_string& input) {
    if(auto [ch, name] = data_is_named_char(i,input); !name.empty()) 
      return (input[i] == 'x' && isxdigit(input[i+1])) ? name.size()+1 : name.size();
    return 1;
  }


  // Parse a char literal from "ins", rm excess letters from "input", & 
  //   mv "ins" back to be 1 char past the end of the char literal
  void parse_char(FILE*& ins, scm_string& input) {
    const size_type char_start = input.find("#\\")+2;
    if(char_start+2 > input.size()) return; // if parsing a single/null character
    const size_type char_length = character_length(char_start,input);
    if(input.size()-(char_start+char_length) > 0) {
      const size_type excess_read_length = input.size()-(char_start+char_length);
      fseek(ins, -excess_read_length, SEEK_CUR);    // mv "ins" back
      input.erase(input.size()-excess_read_length); // erase the excess parsed
    }
  }


  // Confirm improper use for quote, quasiquote, & unquote
  constexpr bool improper_quotation_char(const chr_type& c,const chr_type& c2){
    return (c == '\'' || c == '`' || c == ',') && (c2 == ')' || isspace(c2));
  }


  // Confirm improper use for unquote-splicing
  constexpr bool improper_quotation_char(const chr_type& c,const chr_type& c2,
                                                           const chr_type& c3){
    return c == ',' && c2 == '@' && (c3 == ')' || isspace(c3));
  }


  // Confirm improper use for any quotation shorthand
  bool improper_quotation(const scm_string& input) {
    return (input.size() > 1 && 
            improper_quotation_char(*(input.end()-2),*input.rbegin())) 
           ||
           (input.size() > 2 && 
            improper_quotation_char(*(input.end()-3),*(input.end()-2),
                                                               *input.rbegin()));
  }


  // Confirm quotation shorthand found at the end of the file
  bool improper_EOF_quotation(const scm_string& input) {
    return ((!input.empty() && 
             (*input.rbegin()=='\''||*input.rbegin()=='`'||*input.rbegin()==','))
            || 
            (input.size()>1 && 
             *(input.rbegin()+1) == ',' && *input.rbegin() == '@'))
           &&
           !(input.size()>2 && 
             *(input.rbegin()+2)=='#' && *(input.rbegin()+1)=='\\');
  }


  // Confirm just appended a valid open/close paren to 'input'
  bool is_valid_open_exp(const scm_string &input,  const size_type& paren_count,
                         const bool& possible_vect,const bool& found_nonquote_data){
    return *input.rbegin() == '(' && (input.size() < 3 || 
                                      *(input.rbegin()+2) != '#' || 
                                      *(input.rbegin()+1) != '\\')
                                  && (paren_count || 
                                      !found_nonquote_data || 
                                      possible_vect);
  }


  bool is_valid_close_exp(const scm_string &input) {
    return *input.rbegin() == ')' && (input.size() < 3 || 
                                      *(input.rbegin()+2) != '#' || 
                                      *(input.rbegin()+1) != '\\');
  }
}


// Read from a non-stdin port
// PRECONDITION: feof(ins) MUST RETURN false
scm_list primitive_read_from_port(FILE* outs, FILE* ins) {
  // input parsing variables & status trackers
  scm_string input;
  int ch;
  bool in_a_string   = false, possible_vect=false,  found_nonquote_data=false;
  bool possible_char = false, confirmed_char=false, parsing_a_char=false;
  size_type paren_count = 0;

  fflush(outs);

  // parse the input port's scheme code
  while((ch = fgetc(ins)) != EOF) {

    // don't include prefixing whitespace
    if(input.empty() && isspace(ch)) continue;

    // append the character
    input += ch;

    // continue to get the quoted data
    if(!found_nonquote_data && (ch == '\'' || ch == '`' || ch == ',' || 
      (input.size()>1 && ch == '@' && *(input.rbegin()+1) == ','))) {
        continue;
    }

    // skip first char of a char literal
    if(confirmed_char) { confirmed_char=false, parsing_a_char=true; continue; }
    // if at the end of a possible char
    if(parsing_a_char && IS_END_OF_WORD(*(input.end()-2), ch)) {
      parsing_a_char = false;
      if(!paren_count) { read_port::parse_char(ins,input); break; }
    } else if(parsing_a_char) continue; // keep parsing the char until complete

    // skip if parsing a string literal
    if(in_a_string && ch != '"') continue;
    // check whether at a char
    if(possible_char && ch != '\\') possible_char = false;
    // check whether at a vector
    if(possible_vect && ch != '(')  possible_vect = false;

    // check if at a string
    if(ch == '"' && (in_a_string || paren_count || !found_nonquote_data)) {
      if(!in_a_string)
        in_a_string = true;
      else if(is_non_escaped_double_quote(input.size()-1,input)) {
        in_a_string = false;
        if(!paren_count) break;
      }
    }

    // check if at an expression or vector
    else if(read_port::is_valid_open_exp(input,paren_count,possible_vect,
                                                     found_nonquote_data))
     possible_vect = false, ++paren_count;
    // check if at a closing expression
    else if(read_port::is_valid_close_exp(input)) {
      if(!paren_count) {
        alert_reader_error(outs,READER_ERROR::early_end_paren,input);
        throw SCM_EXCEPT::READ;
      }
      --paren_count;
      if(!paren_count) break;
    }

    // check for improper quotation shorthand use
    else if(read_port::improper_quotation(input)){
      if(ch == ')') 
           alert_reader_error(outs,READER_ERROR::quoted_end_of_expression,input);
      else alert_reader_error(outs,READER_ERROR::quoted_space,input);
      throw SCM_EXCEPT::READ;
    }

    // continue parsing if in an expression
    else if(paren_count) continue;

    // check if at a char or vector
    else if(ch == '#' && !found_nonquote_data) 
      possible_vect = possible_char = true;
    else if(possible_char && ch == '\\') confirmed_char=true, possible_char=false;

    // check for the end of the current atomic. if so, mv "ins" back to the end
    else if(found_nonquote_data && !paren_count && input.size() > 2 && 
            IS_END_OF_WORD(*(input.end()-2), ch)) {
      fseek(ins, -2, SEEK_CUR);    // mv "ins" back
      input.erase(input.size()-2); // erase the excess from "input"
      break;
    }

    found_nonquote_data = true; // found non-quote data
  } // -- End of parsing loop


  // If read an empty file
  if(input.empty()) return scm_list({data(chr_type(EOF))});

  // Confirm file didn't end mid-string or mid-expression
  if(in_a_string || paren_count) {
    if(in_a_string)
         alert_non_repl_reader_error(outs,READER_ERROR::incomplete_string,input);
    else alert_non_repl_reader_error(outs,READER_ERROR::incomplete_expression,input);
    throw SCM_EXCEPT::READ;
  }

  // Parse the char literal if appeared at the end of the buffer
  //   NOTE: possible_char & confirmed_char are ok 2B true w/o further logic
  if(parsing_a_char) {
    read_port::parse_char(ins,input);

  // Confirm didn't quote the end of the buffer
  } else if(read_port::improper_EOF_quotation(input)) {
    alert_reader_error(outs, READER_ERROR::quoted_end_of_buffer,input);
    throw SCM_EXCEPT::READ;
  }

  // Try parsing the given input expression, & throw an error as needed
  try {
    scm_list abstract_syntax_tree;
    // Return AST if successfully parsed an expression
    parse_input_exp(std::move(input),abstract_syntax_tree);
    return abstract_syntax_tree;
  } catch(const READER_ERROR& read_error) {
    if(is_non_repl_reader_error(read_error))
         alert_non_repl_reader_error(outs,read_error,input);
    else alert_reader_error(outs,read_error,input);
    throw SCM_EXCEPT::READ;
  } catch(const size_type& read_error_index) {
    alert_reader_error(outs,read_error_index,input);
    throw SCM_EXCEPT::READ;
  }
}

/******************************************************************************
* PORT PRIMITIVE HELPERS
******************************************************************************/

// Confirm given a single string argument
void confirm_given_one_string_arg(const scm_list& args, const char* name, 
                                                        const char* format){
  if(no_args_given(args) || args.size() != 1) 
    THROW_ERR('\''<<name<<" didn't recieve any args: "<<format<<FCN_ERR(name,args));
  if(!args[0].is_type(types::str)) 
    THROW_ERR('\''<<name<<" arg "<<PROFILE(args[0])<<" isn't a string: "<<format
      << FCN_ERR(name,args));
}


// Confirm the given filename exists as a file
bool confirm_file_exists(const char* filename) {
  FILE* existential_check = fopen(filename, "r");
  const bool exists = existential_check != nullptr;
  if(existential_check) fclose(existential_check);
  return exists;
}


// Returns a file pointer if 'filename' is:
// => INPUT:  the string name of an existing file
// => OUTPUT: a file we have permission to write to
FILE* confirm_valid_io_file(const data& filename, const char* name, 
                            const char* format,   const char* file_open_type,
                            const scm_list& args){
  // confirm given a proper filename
  if(!filename.is_type(types::str))
    THROW_ERR('\'' << name << ' ' << PROFILE(filename)
      << " is not a filename string:" << format << FCN_ERR(name,args));
  // confirm given a valid io filename string
  bool exists = confirm_file_exists(filename.value.str->c_str());
  FILE* fp = fopen(filename.value.str->c_str(), file_open_type);
  // if INPUT, confirm file isnt null
  if(fp == nullptr && file_open_type[0] == 'r')
    THROW_ERR('\'' << name << " file \"" << *filename.value.str
      << "\" doesn't exist (invalid for input):"<<format<<FCN_ERR(name,args));
  // if OUTPUT, confirm file doesn't exist
  if((fp == nullptr || exists) && file_open_type[0] == 'w')
    THROW_ERR('\'' << name << " file \"" << *filename.value.str 
      << "\" already exists!" << format << FCN_ERR(name,args));
  return fp;
}


// Returns a file pointer if 'filename' is the string name of an existing file
FILE* confirm_valid_input_file(const data& filename, const char* name, 
                               const char* format,   const scm_list& args) {
  return confirm_valid_io_file(filename, name, format, "r", args);
}


// Returns a file pointer if 'filename' is a file we have permission to write
FILE* confirm_valid_output_file(const data& filename, const char* name, 
                                const char* format,   const scm_list& args) {
  return confirm_valid_io_file(filename, name, format, "w", args);
}


// Confirm the port predicate was given 1 port
void confirm_valid_port_predicate_arg(const scm_list& args,const char* name, 
                                                           const char* format){
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR('\'' << name << " recieved incorrect # of args: "<<format<<FCN_ERR(name,args));
  if(!args[0].is_type(types::fip) && !args[0].is_type(types::fop))
    THROW_ERR('\''<<name<<" arg "<<PROFILE(args[0])<<" isn't a port: "<<format<<FCN_ERR(name,args));
}


// Confirm close-port primitive was given 1 port of the correct type
void confirm_valid_close_port_arg(const scm_list& args, const char* io_type, 
                                                        const types& t) {
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR("'close-"<<io_type<<"-port recieved incorrect # of args:" 
      "\n     (close-"<<io_type<<"-port <"<<io_type<<"-port>)"
      << FCN_ERR("close-"+scm_string(io_type)+"-port",args));
  if(!args[0].is_type(t))
    THROW_ERR("'close-"<<io_type<<"-port arg " << PROFILE(args[0])
      << "\n     isn't an "<<io_type<<" port:\n     (close-"
      << io_type<<"-port <"<<io_type<<"-port>)"
      << FCN_ERR("close-"+scm_string(io_type)+"-port",args));
}


// Call an unary procedure with a file's port as its argument
template<typename port_ctor,typename PORT_GETTER>
data primitive_CALL_WITH_FILE(scm_list& args,     const char* name,
                              const char* format, PORT_GETTER get_port){
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm given a filename string & a procedure
  if(args.size() != 2)
    THROW_ERR('\'' << name << " recieved incorrect # of args:" 
      << format << FCN_ERR(name,args));
  primitive_confirm_data_is_a_procedure(args[1], name, format, args);
  // add file to the port registry
  PORT_REGISTRY.push_back(get_port(args[0],name,format,args));
  // apply the given procedure w/ a port to the file
  scm_list port_arg({port_ctor(PORT_REGISTRY.size()-1)});
  return data_cast(execute_application(std::move(args[1].value.exp),port_arg,env));
}


// Call an argless procedure with a file's port as the default port
template<typename PORT_GETTER>
data primitive_WITH_FILE(scm_list& args,     const char* name,
                         const char* format, FILE*& DEFAULT_PORT,
                         PORT_GETTER get_port){
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm given a filename string & a procedure
  if(args.size() != 2)
    THROW_ERR('\'' << name << " recieved incorrect # of args:" 
      << format << FCN_ERR(name,args));
  primitive_confirm_data_is_a_procedure(args[1], name, format, args);
  // save & set the current port
  FILE* original_port = DEFAULT_PORT;
  DEFAULT_PORT = get_port(args[0], name, format, args);
  // apply the given procedure
  auto null_arg = scm_eval(scm_list({symconst::quote, symconst::sentinel}),env);
  auto result   = data_cast(execute_application(std::move(args[1].value.exp),null_arg,env));
  // reset the current port
  if(DEFAULT_PORT && DEFAULT_PORT != stdin && 
     DEFAULT_PORT != stdout && DEFAULT_PORT != stderr) fclose(DEFAULT_PORT);
  DEFAULT_PORT = original_port;
  return result;
}

/******************************************************************************
* SYSTEM INTERFACE PRIMITIVE HELPER
******************************************************************************/

data primitive_LOAD_TEMPLATE(scm_list& args, env_type& env, const char* name){
  // Confirm given a valid input filename string
  const scm_string load_err = afmt(AFMT_135) + scm_string(">> Load Exception:") + 
                              afmt(AFMT_01)  + scm_string(1, ' ');
  const scm_string format   = "\n     ("+scm_string(name)+" <filename-string>)";
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR('\''<<name<<" recieved incorrect # of args:"<<format<<FCN_ERR(name,args));
  // Load file contents
  FILE* ins = confirm_valid_input_file(args[0],name,format.c_str(),args);
  size_type exp_count = 1;
  while(!feof(ins)) {
    // Try reading & evaluating an expression
    try {
      scm_eval(scm_list_cast(primitive_read_from_port(CURRENT_OUTPUT_PORT,ins)[0]),env);
      ++exp_count;
    // Catch, notify 'load' error occurred, & rethrow
    } catch(const SCM_EXCEPT& load_error) {
      if(load_error == SCM_EXCEPT::EVAL)
        std::cerr << "  " << load_err << "\n     File \"" << *args[0].value.str
                  << "\"\n     Expression #" << exp_count << '\n' << afmt(AFMT_0);
      else if(load_error == SCM_EXCEPT::READ)
        std::cerr << load_err << "\n   File \"" << *args[0].value.str 
                  << "\"\n   Expression #" << exp_count << '\n' << afmt(AFMT_0);
      if(ins) fclose(ins);
      throw load_error;
    }
  }
  if(ins) fclose(ins);
  return data(types::dne);
}
#endif
