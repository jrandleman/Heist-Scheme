// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for lists.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_LISTS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_LISTS_IMPLEMENTATION_HPP_

namespace heist::stdlib_lists {

  /******************************************************************************
  * LIST*
  ******************************************************************************/

  data convert_data_vector_to_dotted_list(const data_vector::iterator& obj, const data_vector::iterator& null_obj)noexcept{
    if(obj+1 == null_obj) return *obj;
    data new_pair = data(make_par());
    new_pair.par->first = *obj;
    new_pair.par->second = convert_data_vector_to_dotted_list(obj+1,null_obj);
    return new_pair;
  }

  /******************************************************************************
  * CIRCULAR-LIST
  ******************************************************************************/

  // "circular-list" primitive construction helper
  data convert_data_vector_to_circular_list(const data_vector::iterator& obj, const data_vector::iterator& null_obj, 
                                            const data& head = data(),        const bool& past_head=false)noexcept{
    data new_pair = data(make_par());
    new_pair.par->first = *obj;
    if(obj+1 == null_obj) {
      if(past_head) new_pair.par->second = head;
      else          new_pair.par->second = new_pair;
      return new_pair;
    }
    if(!past_head) {
      new_pair.par->second = convert_data_vector_to_circular_list(obj+1,null_obj,new_pair,true);
    } else {
      new_pair.par->second = convert_data_vector_to_circular_list(obj+1,null_obj,head,true);
    }
    return new_pair;
  }

  /******************************************************************************
  * LAST-PAIR
  ******************************************************************************/

  data get_last_pair(const data& d)noexcept{
    if(!d.par->second.is_type(types::par)) return d;
    return get_last_pair(d.par->second);
  }

  /******************************************************************************
  * UNFOLD
  ******************************************************************************/

  void primitive_UNFOLD_template_recur(data& break_condition, data& mapper, 
                                       data& successor, const data& seed, data_vector& unfolded){
    if(execute_application(break_condition,data_vector(1,seed)).is_truthy()) return;
    unfolded.push_back(execute_application(mapper,data_vector(1,seed)));
    primitive_UNFOLD_template_recur(break_condition,mapper,successor,execute_application(successor,data_vector(1,seed)),unfolded);
  }


  void primitive_UNFOLD_template(data_vector& args,data_vector& unfolded,
                                 const char* name,const char* format){
    // confirm 'unfold call has a proper argument signature
    if(args.size() != 4) HEIST_THROW_ERR('\''<<name<<" received incorrect # of args:"<<format<<HEIST_FCN_ERR(name,args));
    auto break_condition = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, name, format);
    auto mapper          = primitive_toolkit::validate_callable_and_convert_to_procedure(args[1], args, name, format);
    auto successor       = primitive_toolkit::validate_callable_and_convert_to_procedure(args[2], args, name, format);
    primitive_UNFOLD_template_recur(break_condition,mapper,successor,args[3],unfolded);
  }

  /******************************************************************************
  * ALIST?
  ******************************************************************************/

  // "alist?" primitive helper
  bool list_only_contains_pairs(const data& curr_pair)noexcept{
    if(!curr_pair.par->first.is_type(types::par))  return false;
    if(!curr_pair.par->second.is_type(types::par)) return true;
    return list_only_contains_pairs(curr_pair.par->second);
  }

  /******************************************************************************
  * COMBINATIONS GENERATION
  ******************************************************************************/

  std::vector<data_vector> get_all_combos_DAC(const typename data_vector::const_iterator& start, 
                                              const typename data_vector::const_iterator& end){
    if(start == end) return std::vector<data_vector>(1,data_vector());
    std::vector<data_vector> new_combos;
    auto result = get_all_combos_DAC(start+1,end);
    for(std::size_t i = 0, n = result.size(); i < n; ++i) {
      new_combos.push_back(result[i]);
      result[i].push_back(*start);
      new_combos.push_back(result[i]);
    }
    return new_combos;
  }


  std::vector<data_vector> get_all_combos(data_vector v) {
    auto result = get_all_combos_DAC(v.begin(),v.end());
    std::sort(result.begin(),result.end(),[](const data_vector& s1, const data_vector& s2){return s1.size() < s2.size();});
    return result;
  }

  /******************************************************************************
  * LIST MEMBER EXTRACTION PRIMITIVE HELPERS
  ******************************************************************************/

  // "member" "memv" "memq" primitive helper: recursively compares cars to 'obj'
  //   & returns a sublist w/ 'obj' as its 'car' if found. Else returns #f
  data primitive_MEM_car_comparison(data& curr_pair, const data& obj, bool(data::*equality_fcn)(const data&)const){
    if(!curr_pair.is_type(types::par)) return GLOBALS::FALSE_DATA_BOOLEAN;
    if((curr_pair.par->first.*equality_fcn)(obj)) return curr_pair;
    return primitive_MEM_car_comparison(curr_pair.par->second, obj, equality_fcn);
  }


  // Template helper fcn for the "member" "memv" "memq" primitives.
  data primitive_MEM_template(data_vector& args, const char* name, bool(data::*equality_fcn)(const data&)const){
    // Confirm given the correct # of args
    if(args.size() != 2)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args:\n     ("
        <<name<<" <obj> <list>)"<<HEIST_FCN_ERR(name,args));
    // (<mem> <obj> '()) = #f
    if(primitive_toolkit::data_is_nil(args[1])) return GLOBALS::FALSE_DATA_BOOLEAN;
    // Confirm given a proper list
    if(!primitive_toolkit::data_is_proper_list(args[1]))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])
        <<" isn't a proper list!\n     ("<<name<<" <obj> <list>)"<<HEIST_FCN_ERR(name,args));
    // Get the sublist w/ 'obj' at its head (if exists)
    return primitive_MEM_car_comparison(args[1], args[0], equality_fcn);
  }


  // "assoc" "assv" "assq" primitive helper: recursively compares pairs' cars to 
  //   'obj' & returns a pair w/ 'obj' as its 'car' if found. If finds a non-pair, 
  //   throws an error. Else returns #f.
  data primitive_ASSOCIATION_key_seeker(data& curr_pair, const data& obj, const data& head, const char* name, 
                                        bool(data::*equality_fcn)(const data&)const, const data_vector& args) {
    if(!curr_pair.is_type(types::par)) return GLOBALS::FALSE_DATA_BOOLEAN;
    if(!curr_pair.par->first.is_type(types::par))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<head
        <<" isn't a proper association list (list of pairs)!"
        "\n     ("<<name<<" <obj> <association-list>)"<<HEIST_FCN_ERR(name,args));
    if((curr_pair.par->first.par->first.*equality_fcn)(obj)) return curr_pair.par->first;
    return primitive_ASSOCIATION_key_seeker(curr_pair.par->second,obj,head,name,equality_fcn,args);
  }


  // Template helper fcn for the "assoc" "assv" "assq" primitives.
  data primitive_ASSOCIATION_template(data_vector& args, const char* name, bool(data::*equality_fcn)(const data&)const){
    // Confirm given the correct # of args
    if(args.size() != 2)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args:\n     ("
        <<name<<" <obj> <association-list>)"<<HEIST_FCN_ERR(name,args));
    // (<mem> <obj> '()) = #f
    if(primitive_toolkit::data_is_nil(args[1])) return GLOBALS::FALSE_DATA_BOOLEAN;
    // Confirm given a proper list
    if(!primitive_toolkit::data_is_proper_list(args[1]))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a proper list!"
        "\n     ("<<name<<" <obj> <association-list>)"<<HEIST_FCN_ERR(name,args));
    // Get the sublist w/ 'obj' at its head (if exists)
    return primitive_ASSOCIATION_key_seeker(args[1],args[0],args[1],name,equality_fcn,args);
  }

} // End of namespace heist::stdlib_lists

#endif