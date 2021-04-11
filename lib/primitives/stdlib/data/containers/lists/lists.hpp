// Author: Jordan Randleman -- jordanran199@gmail.com -- lists.hpp
// => Defines primitive list functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_LISTS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_LISTS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * LIST CONSTRUCTORS
  ******************************************************************************/

  // primitive "list" procedure:
  data primitive_LIST(data_vector&& args)noexcept{
    if(args.empty()) return data(symconst::emptylist); // (list) = '()
    return primitive_toolkit::convert_data_vector_to_proper_list(args.begin(), args.end());
  }

  // primitive "list*" procedure:
  data primitive_LIST_STAR(data_vector&& args)noexcept{
    if(args.empty()) return data(symconst::emptylist); // (list*) = '()
    return stdlib_lists::convert_data_vector_to_dotted_list(args.begin(), args.end());
  }

  // primitive "circular-list" procedure:
  data primitive_CIRCULAR_LIST(data_vector&& args)noexcept{
    if(args.empty()) return data(symconst::emptylist);
    return stdlib_lists::convert_data_vector_to_circular_list(args.begin(), args.end());
  }

  // primitive "make-list" procedure:
  data primitive_MAKE_LIST(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MAKE_LIST,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'make-list received incorrect # of args (only "
        << args.size() << "):\n     (make-list <size> <fill-value>)"
        << HEIST_FCN_ERR("make-list", args));
    auto [n, valid_size] = primitive_toolkit::convert_data_to_size_type(args[0]);
    if(!valid_size)
      HEIST_THROW_ERR("'make-list didn't receive a proper positive integer size!"
        "\n     (make-list <size> <fill-value>)"
        "\n     <size> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR("make-list", args));
    // mk a list w/ n copies of the given <fill-value>
    if(!n) return data(symconst::emptylist);
    data_vector mk_list_args(n, args[1]);
    return primitive_toolkit::convert_data_vector_to_proper_list(mk_list_args.begin(), mk_list_args.end());
  }

  // primitive "iota" procedure:
  data primitive_IOTA(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (iota <count> <optional-start-number> <optional-step-number>)";
    if(args.empty() || args.size() > 3)
      HEIST_THROW_ERR("'iota received incorrect # of args (only "
        << args.size() << "):" << format << HEIST_FCN_ERR("iota", args));
    auto [n, valid_index] = primitive_toolkit::convert_data_to_size_type(args[0]);
    if(!valid_index)
      HEIST_THROW_ERR("'iota 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a non-negative integer count!"
        << format << "\n     <count> range: (0," << GLOBALS::MAX_SIZE_TYPE << ']' 
        << HEIST_FCN_ERR("iota", args));
    auto start = 0_n, step = 1_n; // num_type literals
    if(args.size() > 1) {
      if(!args[1].is_type(types::num))
        HEIST_THROW_ERR("'iota 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't an number:"
          << format << HEIST_FCN_ERR("iota", args));
      start = args[1].num;
    }
    if(args.size() > 2) {
      if(!args[2].is_type(types::num))
        HEIST_THROW_ERR("'iota 3rd arg "<<HEIST_PROFILE(args[2])<<" isn't an number:"
          << format << HEIST_FCN_ERR("iota", args));
      step = args[2].num;
    }
    if(!n) return symconst::emptylist;
    data_vector iota_vals(n);
    for(size_type i = 0; i < n; ++i) iota_vals[i] = start + (i * step);
    return primitive_toolkit::convert_data_vector_to_proper_list(iota_vals.begin(), iota_vals.end());
  }

  // primitive "last-pair" procedure:
  data primitive_LAST_PAIR(data_vector&& args) {
    static constexpr const char * const format = "\n     (last-pair <non-empty-proper-list>)";
    if(args.size() != 1 || !primitive_toolkit::data_is_proper_list(args[0]))
      HEIST_THROW_ERR("'last-pair didn't receive exactly 1 non-nil proper list!"
        << format <<HEIST_FCN_ERR("last-pair",args));
    if(!args[0].is_type(types::par))
      HEIST_THROW_ERR("'last-pair can't get the last pair of the empty list!" 
        << format << HEIST_FCN_ERR("last-pair",args));
    return stdlib_lists::get_last_pair(args[0]);
  }

  // primitive "unfold" procedure:
  data primitive_UNFOLD(data_vector&& args) {
    if(!args.empty() && args.size() < 4) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_UNFOLD,args);
    data_vector unfolded;
    stdlib_lists::primitive_UNFOLD_template(args,unfolded,"unfold",
      "\n     (unfold <break-condition> <map-callable> <successor-callable> <seed>)");
    return primitive_toolkit::convert_data_vector_to_proper_list(unfolded.begin(),unfolded.end());
  }

  // primitive "unfold-right" procedure:
  data primitive_UNFOLD_RIGHT(data_vector&& args) {
    if(!args.empty() && args.size() < 4) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_UNFOLD_RIGHT,args);
    data_vector unfolded;
    stdlib_lists::primitive_UNFOLD_template(args,unfolded,"unfold-right",
      "\n     (unfold-right <break-condition> <map-callable> <successor-callable> <seed>)");
    return primitive_toolkit::convert_data_vector_to_proper_list(unfolded.rbegin(),unfolded.rend());
  }

  /******************************************************************************
  * LIST PREDICATES
  ******************************************************************************/

  // primitive "circular-list?" procedure:
  data primitive_CIRCULAR_LISTP(data_vector&& args) {
    if(args.size() != 1) 
      HEIST_THROW_ERR("'circular-list? didn't receive exactly 1 arg!\n     (circular-list? <obj>)" 
        << HEIST_FCN_ERR("circular-list?", args));
    return boolean(primitive_toolkit::data_is_circular_list(args[0]));
  }

  // primitive "list*?" procedure:
  data primitive_LIST_STARP(data_vector&& args) {
    if(args.size() != 1) 
      HEIST_THROW_ERR("'list*? didn't receive exactly 1 arg!\n     (list*? <obj>)" << HEIST_FCN_ERR("list*?", args));
    return boolean(primitive_toolkit::data_is_dotted_list(args[0]));
  }

  // primitive "list?" procedure:
  //   => where 'list' := finite & null-terminated pair sequence
  data primitive_LISTP(data_vector&& args) {
    if(args.size() != 1) 
      HEIST_THROW_ERR("'list? didn't receive exactly 1 arg!\n     (list? <obj>)" << HEIST_FCN_ERR("list?", args));
    return boolean(primitive_toolkit::data_is_proper_list(args[0]));
  }

  // primitive "alist?" procedure:
  data primitive_ALISTP(data_vector&& args) {
    if(args.size() != 1) 
      HEIST_THROW_ERR("'alist? didn't receive exactly 1 arg!\n     (alist? <obj>)" << HEIST_FCN_ERR("alist?", args));
    // valid association lists are finite, terminate with '(), and only contain other pairs
    return boolean(primitive_toolkit::data_is_nil(args[0]) || 
                   (args[0].is_type(types::par) && 
                    get_list_status(args[0]) == list_status::proper && 
                    stdlib_lists::list_only_contains_pairs(args[0])));
  }

  /******************************************************************************
  * LIST COMBINATORICS
  ******************************************************************************/

  // primitive "get-all-combinations" procedure:
  data primitive_GET_ALL_COMBINATIONS(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'get-all-combinations didn't receive 1 arg!"
        "\n     (get-all-combinations <proper-list>)" << HEIST_FCN_ERR("get-all-combinations",args));
    if(!primitive_toolkit::data_is_proper_list(args[0]))
      HEIST_THROW_ERR("'get-all-combinations arg " << HEIST_PROFILE(args[0]) << " isn't a proper list!"
          "\n     (get-all-combinations <proper-list>)" << HEIST_FCN_ERR("get-all-combinations",args));
    auto list_vector = primitive_toolkit::convert_proper_list_to_data_vector(args[0]);
    auto result = stdlib_lists::get_all_combos(list_vector);
    data_vector combinations_list;
    for(auto& lis : result)
      combinations_list.push_back(primitive_toolkit::convert_data_vector_to_proper_list(lis.begin(),lis.end()));
    return primitive_toolkit::convert_data_vector_to_proper_list(combinations_list.begin(),combinations_list.end());
  }

  /******************************************************************************
  * LIST MEMBER EXTRACTION
  ******************************************************************************/

  // primitive "memq" procedure:
  data primitive_MEMQ(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MEMQ,args);
    return stdlib_lists::primitive_MEM_template(args, "memq", &data::eq);
  }

  // primitive "memv" procedure:
  data primitive_MEMV(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MEMV,args);
    return stdlib_lists::primitive_MEM_template(args, "memv", &data::eqv);
  }

  // primitive "member" procedure:
  data primitive_MEMBER(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MEMBER,args);
    return stdlib_lists::primitive_MEM_template(args, "member", &data::equal);
  }

  /******************************************************************************
  * ASSOCIATIVE LIST MEMBER EXTRACTION
  ******************************************************************************/

  // primitive "assq" procedure:
  data primitive_ASSQ(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ASSQ,args);
    return stdlib_lists::primitive_ASSOCIATION_template(args, "assq", &data::eq);
  }

  // primitive "assv" procedure:
  data primitive_ASSV(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ASSV,args);
    return stdlib_lists::primitive_ASSOCIATION_template(args, "assv", &data::eqv);
  }

  // primitive "assoc" procedure:
  data primitive_ASSOC(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ASSOC,args);
    return stdlib_lists::primitive_ASSOCIATION_template(args, "assoc", &data::equal);
  }

} // End of namespace heist

#endif