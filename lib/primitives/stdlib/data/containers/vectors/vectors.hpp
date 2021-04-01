// Author: Jordan Randleman -- jrandleman@scu.edu -- vectors.hpp
// => Defines primitive vector functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_VECTORS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_VECTORS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * VECTOR CONSTRUCTORS
  ******************************************************************************/

  // primitive "vector" procedure:
  data primitive_VECTOR(data_vector&& args)noexcept{
    return make_vec(std::move(args));
  }

  // primitive "make-vector" procedure:
  data primitive_MAKE_VECTOR(data_vector&& args) {
    // confirm valid length given
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR("'make-vector didn't receive the proper number of args!"
        "\n     (make-vector <size> <optional-fill-value>)"
        "\n     <size> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR("make-vector", args));
    auto [n, valid_size] = primitive_toolkit::convert_data_to_size_type(args[0]);
    if(!valid_size)
      HEIST_THROW_ERR("'make-vector didn't receive a proper positive integer size!"
        "\n     (make-vector <size> <optional-fill-value>)"
        "\n     <size> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR("make-vector", args));
    // mk a vector w/ the the given reserve size
    data vect(make_vec(data_vector(n)));
    // fill vector as needed
    if(args.size() == 2)
      for(size_type i = 0; i < n; ++i)
        vect.vec->operator[](i) = args[1];
    return vect;
  }

  // primitive "vector-iota" procedure:
  data primitive_VECTOR_IOTA(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (vector-iota <count> <optional-start-number> <optional-step-number>)";
    if(args.empty() || args.size() > 3)
      HEIST_THROW_ERR("'vector-iota received incorrect # of args (only "
        << args.size() << "):" << format << HEIST_FCN_ERR("vector-iota", args));
    auto [n, valid_index] = primitive_toolkit::convert_data_to_size_type(args[0]);
    if(!valid_index)
      HEIST_THROW_ERR("'vector-iota 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a non-negative integer count!"
        << format << "\n     <count> range: (0," << GLOBALS::MAX_SIZE_TYPE << ']' 
        << HEIST_FCN_ERR("vector-iota", args));
    auto start = 0_n, step = 1_n; // num_type literals
    if(args.size() > 1) {
      if(!args[1].is_type(types::num))
        HEIST_THROW_ERR("'vector-iota 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't an number:"
          << format << HEIST_FCN_ERR("vector-iota", args));
      start = args[1].num;
    }
    if(args.size() > 2) {
      if(!args[2].is_type(types::num))
        HEIST_THROW_ERR("'vector-iota 3rd arg "<<HEIST_PROFILE(args[2])<<" isn't an number:"
          << format << HEIST_FCN_ERR("vector-iota", args));
      step = args[2].num;
    }
    if(!n) return make_vec(data_vector());
    data_vector iota_vals(n);
    for(size_type i = 0; i < n; ++i)
      iota_vals[i] = start + (i * step);
    return make_vec(std::move(iota_vals));
  }

  // primitive "vector-unfold" procedure:
  data primitive_VECTOR_UNFOLD(data_vector&& args) {
    if(!args.empty() && args.size() < 4) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_VECTOR_UNFOLD,args);
    data_vector unfolded;
    stdlib_vectors::primitive_UNFOLD_template(args,unfolded,"vector-unfold",
      "\n     (vector-unfold <break-condition> <map-callable> <successor-callable> <seed>)");
    return make_vec(std::move(unfolded));
  }

  // primitive "vector-unfold-right" procedure:
  data primitive_VECTOR_UNFOLD_RIGHT(data_vector&& args) {
    if(!args.empty() && args.size() < 4) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_VECTOR_UNFOLD_RIGHT,args);
    data_vector unfolded;
    stdlib_vectors::primitive_UNFOLD_template(args,unfolded,"vector-unfold-right",
      "\n     (vector-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)");
    return make_vec(data_vector(unfolded.rbegin(),unfolded.rend()));
  }

  /******************************************************************************
  * VECTOR MUTATION
  ******************************************************************************/

  // primitive "vector-push-back!" procedure:
  data primitive_VECTOR_PUSH_BACK_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_VECTOR_PUSH_BACK_BANG,args);
    stdlib_vectors::primitive_confirm_valid_vector_arg(args, 2, "vector-push-back!", "\n     (vector-push-back! <vector> <obj>)");
    args[0].vec->push_back(args[1]);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "vector-push-front!" procedure:
  data primitive_VECTOR_PUSH_FRONT_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_VECTOR_PUSH_FRONT_BANG,args);
    stdlib_vectors::primitive_confirm_valid_vector_arg(args, 2, "vector-push-front!", "\n     (vector-push-front! <vector> <obj>)");
    args[0].vec->insert(args[0].vec->begin(), args[1]);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "vector-pop-back!" procedure:
  data primitive_VECTOR_POP_BACK_BANG(data_vector&& args) {
    stdlib_vectors::primitive_confirm_valid_vector_arg(args, 1, "vector-pop-back!", "\n     (vector-pop-back! <vector>)");
    if(args[0].vec->empty())
      HEIST_THROW_ERR("'vector-pop-back! can't pop items from an empty vector!"
        "\n     (vector-pop-back! <vector>)" << HEIST_FCN_ERR("vector-pop-back!",args));
    data last_item = *args[0].vec->rbegin();
    args[0].vec->pop_back();
    return last_item;
  }

  // primitive "vector-pop-front!" procedure:
  data primitive_VECTOR_POP_FRONT_BANG(data_vector&& args) {
    stdlib_vectors::primitive_confirm_valid_vector_arg(args, 1, "vector-pop-front!", "\n     (vector-pop-front! <vector>)");
    if(args[0].vec->empty())
      HEIST_THROW_ERR("'vector-pop-front! can't pop items from an empty vector!"
        "\n     (vector-pop-front! <vector>)" << HEIST_FCN_ERR("vector-pop-front!",args));
    data first_item = *args[0].vec->begin();
    args[0].vec->erase(args[0].vec->begin());
    return first_item;
  }

  // primitive "vector-copy!" procedure:
  data primitive_VECTOR_COPY_BANG(data_vector&& args) {
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_VECTOR_COPY_BANG,args);
    static constexpr const char * const format = 
      "\n     (vector-copy! <target-vector> <target-start-idx> <source-vector>)";
    if(args.size() != 3)
      HEIST_THROW_ERR("'vector-copy! received incorrect # of arguments:"
        << format << HEIST_FCN_ERR("vector-copy!",args));
    if(!args[0].is_type(types::vec))
      HEIST_THROW_ERR("'vector-copy! 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a vector!"
        <<format<<HEIST_FCN_ERR("vector-copy!",args));
    size_type idx = stdlib_vectors::get_if_valid_vector_idx(args,format);
    if(!args[2].is_type(types::vec))
      HEIST_THROW_ERR("'vector-copy! 3rd arg "<<HEIST_PROFILE(args[2])<<" isn't a vector!"
        <<format<<HEIST_FCN_ERR("vector-copy!",args));
    // splice in the source sequence
    const auto source_sequence(*args[2].vec); // in case copying a seq to itself
    const size_type target_size = args[0].vec->size();
    const size_type source_size = source_sequence.size();
    if(target_size-idx-1 < source_size){
      args[0].vec->erase(args[0].vec->begin()+idx,args[0].vec->end());
      args[0].vec->insert(args[0].vec->end(),source_sequence.begin(), source_sequence.end());
    } else {
      args[0].vec->erase(args[0].vec->begin()+idx,args[0].vec->begin()+idx+source_size);
      args[0].vec->insert(args[0].vec->begin()+idx,source_sequence.begin(), source_sequence.end());
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "vector-swap!" procedure:
  data primitive_VECTOR_SWAP_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_VECTOR_SWAP_BANG,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'vector-swap! received incorrect # of args (only "
        << args.size() << "):\n     (vector-swap! <vector1> <vector2>)" 
        << HEIST_FCN_ERR("vector-swap!", args));
    if(!args[0].is_type(types::vec))
      HEIST_THROW_ERR("'vector-swap! 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a vector: "
        "\n     (vector-swap! <vector1> <vector2>)" << HEIST_FCN_ERR("vector-swap!",args));
    if(!args[1].is_type(types::vec))
      HEIST_THROW_ERR("'vector-swap! 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a vector: "
        "\n     (vector-swap! <vector1> <vector2>)" << HEIST_FCN_ERR("vector-swap!",args));
    data_vector tmp(*args[0].vec);
    *args[0].vec = *args[1].vec;
    *args[1].vec = tmp;
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * VECTOR GROWING
  ******************************************************************************/

  // primitive "vector-grow" procedure:
  data primitive_VECTOR_GROW(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_VECTOR_GROW,args);
    stdlib_vectors::primitive_confirm_valid_vector_arg(args, 2, "vector-grow", "\n     (vector-grow <vector> <size>)");
    if(!stdlib_vectors::data_is_valid_vector_size(args[1]))
      HEIST_THROW_ERR("'vector-grow didn't receive a proper positive integer size!"
        "\n     (vector-grow <vector> <size>)"
        "\n     <size> range: (0," << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR("vector-grow", args));
    if(args[1].num < args[0].vec->size())
      HEIST_THROW_ERR("'vector-grow "<<args[1].num.str()<<" is too small to expand "
        << args[0] << " of size " << args[0].vec->size() << " with!"
        "\n     (vector-grow <vector> <size>)"
        "\n     <size> range: (0," << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR("vector-grow", args));
    if(args[1].num == args[0].vec->size())
      return args[0]; // nothing to expand
    data_vector expanded_vec((size_type)args[1].num.extract_inexact());
    std::copy(args[0].vec->begin(), args[0].vec->end(), expanded_vec.begin());
    return make_vec(std::move(expanded_vec));
  }

  /******************************************************************************
  * VECTOR EMPTY PREDICATE
  ******************************************************************************/

  // primitive "vector-empty?" procedure:
  data primitive_VECTOR_EMPTYP(data_vector&& args) {
    stdlib_vectors::primitive_confirm_valid_vector_arg(args, 1, "vector-empty?", "\n     (vector-empty? <vector>)");
    return data(boolean(args[0].vec->empty()));
  }

  /******************************************************************************
  * VECTOR BINARY SEARCH
  ******************************************************************************/

  // primitive "vector-binary-search" procedure:
  data primitive_VECTOR_BINARY_SEARCH(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (vector-binary-search <vector> <value> <3-way-comparison>)"
      "\n     ; Suppose values a & b"
      "\n     ; a < b -> (<3-way-comparison> a b) < 0"
      "\n     ; a = b -> (<3-way-comparison> a b) = 0"
      "\n     ; a > b -> (<3-way-comparison> a b) > 0";
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_VECTOR_BINARY_SEARCH,args);
    // Confirm proper args
    if(args.size() != 3)
      HEIST_THROW_ERR("'vector-binary-search received incorrect # of args (given "
        << args.size() << "):" << format << HEIST_FCN_ERR("vector-binary-search", args));
    if(!args[0].is_type(types::vec))
      HEIST_THROW_ERR("'vector-binary-search 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a vector:"
        << format << HEIST_FCN_ERR("vector-binary-search", args));
    auto proc = primitive_toolkit::validate_callable_and_convert_to_procedure(args[2], args, "vector-binary-search", format);
    // Perform binary search
    if(args[0].vec->empty()) return GLOBALS::FALSE_DATA_BOOLEAN;
    const auto& vec = *args[0].vec;
    const auto& value = args[1];
    size_type high = vec.size()-1, low = 0;
    while (low <= high) {
      const auto mid = low + (high-low)/2; // no overflow on mid
      data_vector bsearch_args(2);
      bsearch_args[0] = vec[mid], bsearch_args[1] = value;
      auto cmp_result = execute_application(proc,std::move(bsearch_args));
      if(!cmp_result.is_type(types::num))
        HEIST_THROW_ERR("'vector-binary-search result "<<HEIST_PROFILE(cmp_result)<<
          " from callable "<<args[2]<<"\n     applied to args "<<vec[mid]
          <<" and "<<value<<" isn't a number:"<< format << HEIST_FCN_ERR("vector-binary-search", args));
      if(cmp_result.num.is_zero()) // found <value> in <vec>
        return num_type(mid);
      if(cmp_result.num.is_neg())
        low = mid + 1;
      else {
        if(!mid) return GLOBALS::FALSE_DATA_BOOLEAN;
        high = mid - 1;
      }
    }
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * VECTOR COMBINATORICS
  ******************************************************************************/

  // primitive "vector-get-all-combinations" procedure:
  data primitive_VECTOR_GET_ALL_COMBINATIONS(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'vector-get-all-combinations didn't receive 1 arg!"
        "\n     (vector-get-all-combinations <vector>)" << HEIST_FCN_ERR("vector-get-all-combinations",args));
    if(!args[0].is_type(types::vec))
      HEIST_THROW_ERR("'vector-get-all-combinations arg " << HEIST_PROFILE(args[0]) << " isn't a vector!"
          "\n     (vector-get-all-combinations <vector>)" << HEIST_FCN_ERR("vector-get-all-combinations",args));
    auto result = stdlib_vectors::get_all_combos(*args[0].vec);
    data_vector combinations_vect;
    for(auto& vect : result)
      combinations_vect.push_back(make_vec(vect));
    return make_vec(combinations_vect);
  }

} // End of namespace heist

#endif