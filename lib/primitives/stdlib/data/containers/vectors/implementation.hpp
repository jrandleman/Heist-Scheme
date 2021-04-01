// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for vectors.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_VECTORS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_VECTORS_IMPLEMENTATION_HPP_

namespace heist::stdlib_vectors {

  /******************************************************************************
  * VALIDATION
  ******************************************************************************/

  // PRECONDITION: total_args > 0
  void primitive_confirm_valid_vector_arg(const data_vector& args,const size_type& total_args,
                                          const char* name,       const char* format) {
    if(args.size() != total_args)
      HEIST_THROW_ERR('\''<<name<<" didn't receive the correct number of args!" 
        << format << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::vec))
      HEIST_THROW_ERR('\''<<name<<" 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a vector!" 
        << format << HEIST_FCN_ERR(name,args));
  }


  bool data_is_valid_vector_size(const data& d)noexcept{
    return d.is_type(types::num) && d.num.is_integer() &&
           !d.num.is_neg() && d.num <= GLOBALS::MAX_SIZE_TYPE;
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
  * VECTOR-COPY!
  ******************************************************************************/

  size_type get_if_valid_vector_idx(const data_vector& args, const char* format){
    // confirm given an in-'size_type'-range non-negative index
    if(!data_is_valid_vector_size(args[1]))
      HEIST_THROW_ERR("'vector-copy! index "<<args[1]<<" isn't a proper non-negative integer!"
        << format << "\n     <index> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' 
        << HEIST_FCN_ERR("vector-copy!",args));
    // confirm index falls w/in range of the sequence
    const size_type i = (size_type)args[1].num.extract_inexact();
    const size_type l = args[0].vec->size();
    if(i >= l)
      HEIST_THROW_ERR("'vector-copy! received out of range index " << i 
        <<"\n     for vector "<<args[0]<<" of size "
        <<l<<'!'<<format<<HEIST_FCN_ERR("vector-copy!",args));
    return i;
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

} // End of namespace heist::stdlib_vectors

#endif