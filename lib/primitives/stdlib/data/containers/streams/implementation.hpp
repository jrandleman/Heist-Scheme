// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for streams.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_STREAMS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_STREAMS_IMPLEMENTATION_HPP_

namespace heist::stdlib_streams {

  /******************************************************************************
  * GENERAL VALIDATION
  ******************************************************************************/

  void confirm_given_one_arg(const data_vector& args, const char* name, const char* arg_name = "<obj>"){
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of arguments:"
        "\n     ("<<name<<' '<<arg_name<<')'<<HEIST_FCN_ERR(name,args));
  }

  /******************************************************************************
  * FORCING A DATUM
  ******************************************************************************/

  data force_data_delay(data& d) {
    if(!d.is_type(types::del))
      HEIST_THROW_ERR("'force "<<HEIST_PROFILE(d)<<" isn't a delayed expression:\n     "
        "(force <delayed-expression>)"<<HEIST_EXP_ERR("(force "<<d.noexcept_write()<<')'));
    return d.del->force(); // Memoizes delays, "call by need" evaluation
  }

  /******************************************************************************
  * STREAM PRIMITIVE HELPERS
  ******************************************************************************/

  constexpr const char * const STREAM_SCXXXXR_ACCESSOR_NUMBER[] = {
    "", " 1st", " 2nd", " 3rd", " 4th", 
  };


  bool data_is_stream_pair(const data& d)noexcept{
    return d.is_type(types::par) && d.par->first.is_type(types::del) && 
                                    d.par->second.is_type(types::del);
  }


  bool data_is_stream(const data& d)noexcept{
    return primitive_toolkit::data_is_nil(d) || data_is_stream_pair(d);
  }


  void confirm_given_a_stream_pair_arg(data_vector& args, const char* name, const char* format){
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args (given "
        << args.size() << "):" << format << HEIST_FCN_ERR(name,args));
    if(!data_is_stream_pair(args[0]))
      HEIST_THROW_ERR('\''<<name<<' '<<HEIST_PROFILE(args[0])<<" isn't a stream-pair:" 
        << format << HEIST_FCN_ERR(name,args));
  }


  // l-value scar & scdr
  data get_stream_data_car(data& d) {
    if(!data_is_stream_pair(d))
      HEIST_THROW_ERR("'scar "<<HEIST_PROFILE(d)<<" isn't a stream-pair:" 
        "\n     (scar <stream-pair>)" << 
        HEIST_EXP_ERR("(scar " << d.noexcept_write() << ')'));
    return force_data_delay(d.par->first);
  }


  data get_stream_data_cdr(data& d) {
    if(!data_is_stream_pair(d))
      HEIST_THROW_ERR("'scdr "<<HEIST_PROFILE(d)<<" isn't a stream-pair:\n     "
        "(scdr <stream-pair>)" << HEIST_EXP_ERR("(scdr " << d.noexcept_write() << ')'));
    data cdr_promise = force_data_delay(d.par->second);
    if(!data_is_stream(cdr_promise))
      HEIST_THROW_ERR("'scdr forced cdr " << HEIST_PROFILE(cdr_promise)
        << " isn't a stream:\n     (scdr <stream-pair>)" 
        << HEIST_EXP_ERR("(scdr " << d.noexcept_write() << ')'));
    return cdr_promise;
  }


  // r-value scar & scdr (for composition in the sc****r permutations)
  data get_stream_data_car(data&& d, const char * const name, const char * const format, const size_type& nth_scar){
    if(!data_is_stream_pair(d))
      HEIST_THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scar]
        <<" 'scar' "<<HEIST_PROFILE(d)<<" isn't a stream-pair:" 
        <<format<<HEIST_EXP_ERR("(scar "<<d.noexcept_write()<<')'));
    return force_data_delay(d.par->first);
  }


  data get_stream_data_cdr(data&& d, const char * const name, const char * const format, const size_type& nth_scdr){
    if(!data_is_stream_pair(d))
      HEIST_THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scdr]
        <<" 'scdr' "<<HEIST_PROFILE(d)<<" isn't a stream-pair:"
        <<format<<HEIST_EXP_ERR("(scdr "<<d.noexcept_write()<<')'));
    data cdr_promise = force_data_delay(d.par->second);
    if(!data_is_stream(cdr_promise))
      HEIST_THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scdr]
        <<" 'scdr's forced cdr "<<HEIST_PROFILE(cdr_promise)
        <<" isn't a stream:"<<format<<HEIST_EXP_ERR("(scdr "<<d.noexcept_write()<<')'));
    return cdr_promise;
  }


  // "stream" special form helper fcn: recursively constructs embedded sconses
  data convert_data_vector_to_stream(const data_vector::iterator& obj, const data_vector::iterator& null_obj)noexcept{
    if(obj == null_obj) {
      data_vector empty_list(1,symconst::list);
      return empty_list; // becomes '() once forced
    }
    data new_stream_pair = data(make_par());
    new_stream_pair.par->first  = make_del(*obj,G.GLOBAL_ENVIRONMENT_POINTER,false);
    new_stream_pair.par->second = make_del(convert_data_vector_to_stream(obj+1,null_obj),G.GLOBAL_ENVIRONMENT_POINTER,false);
    return new_stream_pair;
  }


  void unpack_stream_into_exp(data&& curr_pair, data_vector& stream_as_exp) {
    if(!data_is_stream_pair(curr_pair)) return;
    stream_as_exp.push_back(get_stream_data_car(curr_pair));
    unpack_stream_into_exp(get_stream_data_cdr(curr_pair), stream_as_exp);
  }


  // "stream-length" primitive helper fcn
  size_type stream_length(data&& curr_pair, size_type count = 1){
    if(data_is_stream_pair(curr_pair)) 
      return stream_length(get_stream_data_cdr(curr_pair), count+1);
    return count;
  }

  /******************************************************************************
  * STREAM CONTROL FEATURES PRIMITIVE HELPERS
  ******************************************************************************/

  void confirm_only_given_streams(const data_vector& streams, const char* name, const char* format,
                                  const int first_arg_pos, const data_vector& args){
    bool found_null_stream = false, found_pair_stream = false;
    size_type null_stream_pos = 0, pair_stream_pos = 0;
    for(size_type i = 0, n = streams.size(); i < n; ++i) {
      // If '(), confirm no stream-pair's
      if(primitive_toolkit::data_is_nil(streams[i])) {
        if(found_pair_stream)
          HEIST_THROW_ERR('\''<<name<<" arg #" << first_arg_pos+i+1 << ' ' << HEIST_PROFILE(streams[i]) 
            << " and\n                      arg #" << first_arg_pos+pair_stream_pos+1 << ' ' 
            << HEIST_PROFILE(streams[pair_stream_pos]) << "\n                      differ in length!");
        null_stream_pos = i, found_null_stream = true;
      // If stream-pair, confirm no '()'s
      } else if(data_is_stream_pair(streams[i])) {
        if(found_null_stream)
          HEIST_THROW_ERR('\''<<name<<" arg #" << first_arg_pos+i+1 << ' ' << HEIST_PROFILE(streams[i]) 
            << " and\n                      arg #" << first_arg_pos+null_stream_pos+1 << ' ' 
            << HEIST_PROFILE(streams[null_stream_pos]) << "\n                      differ in length!");
        pair_stream_pos = i, found_pair_stream = true;
      // Not a stream -- ERROR
      } else {
        HEIST_THROW_ERR('\''<<name<<" arg #" << first_arg_pos+i+1 << ' ' 
          << HEIST_PROFILE(streams[i])<<" isn't a stream:"<<format<<HEIST_FCN_ERR(name,args));
      }
    }
  }


  // Adds cars to 'args' & advances cdrs. Returns whether streams are empty
  bool acquire_scars_advance_scdrs(data_vector& curr_streams, data_vector& args, const char* name, const char* format){
    // Confirm given streams of the same length
    confirm_only_given_streams(curr_streams,name,format,1,data_vector());
    // Check if completed parsing every stream
    if(primitive_toolkit::data_is_nil(curr_streams[0])) return true;
    // Add each arg for 'proc' & advance each stream's head ptr
    for(size_type i = 0, n = curr_streams.size(); i < n; ++i) {
      args[i]         = get_stream_data_car(curr_streams[i]);
      curr_streams[i] = get_stream_data_cdr(curr_streams[i]);
    }
    return false;
  }


  void stream_for_each(data_vector& curr_streams, data& proc){
    data_vector args(curr_streams.size());
    if(acquire_scars_advance_scdrs(curr_streams,args, "stream-for-each",
      "\n     (stream-for-each <procedure> <stream1> <stream2> ...)")) return;
    // Execute proc & recurse down the rest of the lists
    execute_application(proc,std::move(args));
    stream_for_each(curr_streams, proc);
  }


  data stream_ref_drop(data&& curr_pair, const size_type& n, const char* name,  const char* format, 
                                                             size_type count=1, const bool must_exist=true){
    if(count == n) return std::move(curr_pair);
    if(data_is_stream_pair(curr_pair))
      return stream_ref_drop(get_stream_data_cdr(curr_pair),n,name,format,count+1,must_exist);
    if(must_exist) HEIST_THROW_ERR('\''<<name<<" index "<<n<<" is out of range!"<<format);
    return std::move(curr_pair);
  }


  void stream_take(data&& curr_pair,    const size_type& n, 
                                       data_vector& substream, size_type count=0){
    if(count < n && data_is_stream_pair(curr_pair)) {
      substream.push_back(get_stream_data_car(curr_pair));
      stream_take(get_stream_data_cdr(curr_pair),n,substream,count+1);
    }
  }


  data stream_drop_while(data&& curr_pair, data& proc){
    if(!data_is_stream_pair(curr_pair)) return std::move(curr_pair);
    if(execute_application(proc,data_vector(1,get_stream_data_car(curr_pair))).is_falsey()) return std::move(curr_pair);
    return stream_drop_while(get_stream_data_cdr(curr_pair),proc);
  }


  template<bool FOLDING_LEFT>
  void stream_fold_accumulator(data&& curr_pair, data& proc, data& init_val){
    // Return if fully iterated through stream
    if(!data_is_stream_pair(curr_pair)) return;
    // Execute proc, accumulate result, & recurse down the rest of the lists
    if constexpr (FOLDING_LEFT) { // stream-fold is preorder
      data_vector args(2);
      args[0] = init_val, args[1] = get_stream_data_car(curr_pair);
      init_val = execute_application(proc,std::move(args));
    }
    stream_fold_accumulator<FOLDING_LEFT>(get_stream_data_cdr(curr_pair),proc,init_val);
    if constexpr (!FOLDING_LEFT) { // stream-fold-right is postorder
      data_vector args(2);
      args[0] = get_stream_data_car(curr_pair), args[1] = init_val;
      init_val = execute_application(proc,std::move(args));
    }
  }


  template<bool FOLDING_LEFT>
  data stream_fold(data_vector& args,  const char* name, const char* format){
    // Convert given proper arg signature
    if(args.size() != 3)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args (given " << args.size() 
        << "):" << format << HEIST_FCN_ERR(name,args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, name, format);
    if(primitive_toolkit::data_is_nil(args[2])) // folding '() returns seed
      return args[1];
    if(!data_is_stream_pair(args[2]))
      HEIST_THROW_ERR('\''<<name<<' '<<HEIST_PROFILE(args[2])<<" isn't a stream:" 
        << format << HEIST_FCN_ERR(name,args));
    // Apply the procedure on each elt of each list, & accumulate the result
    data init_val = args[1];
    stream_fold_accumulator<FOLDING_LEFT>(std::move(args[2]),procedure,init_val);
    return init_val; // return the accumulated value
  }

  /******************************************************************************
  * STREAM TAKE/DROP VALIDATION HELPERS
  ******************************************************************************/

  bool data_is_valid_index(const data& d)noexcept{
    return d.is_type(types::num) && d.num.is_integer() && 
           !d.num.is_neg() && d.num <= GLOBALS::MAX_SIZE_TYPE;
  }


  void validate_stream_take_drop_args(data_vector& args, const char* name, const char* format){
    if(args.size() != 2) 
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args (given "
        << args.size() << "):" << format << HEIST_FCN_ERR(name, args));
    // Confirm given a valid size
    if(!data_is_valid_index(args[1]))
      HEIST_THROW_ERR('\''<<name<<' '<< HEIST_PROFILE(args[1]) << " isn't a valid size!"
        << format << HEIST_FCN_ERR(name, args));
    // Confirm given a stream
    if(!data_is_stream(args[0]))
      HEIST_THROW_ERR('\''<<name<<' '<< HEIST_PROFILE(args[0]) << " isn't a stream!"
        << format << HEIST_FCN_ERR(name, args));
  }

} // End of namespace heist::stdlib_streams

#endif