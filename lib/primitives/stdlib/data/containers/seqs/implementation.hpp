// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for seqs.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_SEQS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_SEQS_IMPLEMENTATION_HPP_


// VALID SEQUENCE INDEX RANGE (undef in seqs.hpp)
#define VALID_SEQUENCE_INDEX_RANGE\
  "\n     <index> range: [0," << heist::GLOBALS::MAX_SIZE_TYPE << ']'


namespace heist::stdlib_seqs {

  /******************************************************************************
  * CONDITIONAL APPLICATION PROCEDURES
  ******************************************************************************/

  bool is_false_scm_condition(data& proc, data_vector&& args) {
    return execute_application(proc,std::move(args)).is_falsey();
  }


  bool is_true_scm_condition(data& proc, data_vector&& args) {
    return !is_false_scm_condition(proc,std::move(args));
  }

  /******************************************************************************
  * GENERAL SEQUENCE HELPER PREDICATES
  ******************************************************************************/

  // Sequence type enumeration
  enum class heist_sequence {lis, nul, vec, str};


  // Proper sequence =  string | vector | proper-list
  heist_sequence is_proper_sequence(const data& d,    const data_vector& args,
                                    const char* name, const char* format){
    if(d.is_type(types::vec))                     return heist_sequence::vec;
    if(d.is_type(types::str))                     return heist_sequence::str;
    if(primitive_toolkit::data_is_nil(d))         return heist_sequence::nul;
    if(primitive_toolkit::data_is_proper_list(d)) return heist_sequence::lis;
    HEIST_THROW_ERR('\''<<name<<" given arg "<<HEIST_PROFILE(d)<<" isn't a proper sequence!" 
      << format << HEIST_FCN_ERR(name,args)); // throws
    return heist_sequence::nul; // never triggered
  }


  // Confirm given an empty <sequence> (string, list, or vector)
  bool data_is_empty(const data& d)noexcept{
    return primitive_toolkit::data_is_nil(d) || 
           (d.is_type(types::str) && d.str->empty()) || 
           (d.is_type(types::vec) && d.vec->empty());
  }


  void confirm_given_one_sequence_arg(const data_vector& args, const char* name){
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of arguments:"
        "\n     ("<<name<<" <sequence>)" 
        "\n     <sequence> ::= <list> | <vector> | <string>" << HEIST_FCN_ERR(name,args));
  }


  bool data_is_valid_index(const data& d)noexcept{
    return d.is_type(types::num) && d.num.is_integer() &&
           !d.num.is_neg() && d.num <= GLOBALS::MAX_SIZE_TYPE;
  }

  /******************************************************************************
  * GENERAL RANDOM-ACCESS SEQUENCE (STRING & VECTOR) HELPER PROCEDURES
  ******************************************************************************/

  template<types SEQ_TYPE>
  constexpr const char* get_random_access_seq_typename()noexcept{
    if constexpr (SEQ_TYPE == types::str) { return "string"; } else { return "vector"; }
  }


  template<types SEQ_TYPE, typename SEQUENCE_PTR>
  void confirm_same_sized_random_access_seqs(const data_vector& sequences, const char* name, const char* format,
                                             const data_vector& args, SEQUENCE_PTR seq_ptr, const size_type first_seq_idx){
    static constexpr const char* seq_name = get_random_access_seq_typename<SEQ_TYPE>();
    size_type length = 0;
    for(size_type i = 0, n = sequences.size(); i < n; ++i) {
      if(!sequences[i].is_type(SEQ_TYPE))
        HEIST_THROW_ERR('\''<<name<<" arg #" << first_seq_idx+i+1 << ' ' << HEIST_PROFILE(sequences[i]) 
          << " isn't a "<< seq_name <<'!'<< format << HEIST_FCN_ERR(name,args));
      if(i == 0) {
        length = (sequences[i].*seq_ptr)->size();
      } else if(length != (sequences[i].*seq_ptr)->size()) {
        HEIST_THROW_ERR('\''<<name<<' '<< seq_name <<"s "<< sequences[0] << " and " 
          << sequences[i] << " differ in length!" << format << HEIST_FCN_ERR(name,args));
      }
    }
  }


  // Return 'size_type' index (if given string/vector index is valid)
  template<typename SEQUENCE_PTR>
  size_type get_if_valid_str_or_vec_idx(const data_vector& args,  const char* name, 
                                        const char* format,       const char* seq_name, 
                                        const size_type& idx_pos, const size_type& sequence_pos, 
                                        SEQUENCE_PTR seq_ptr){
    // confirm given an in-'size_type'-range non-negative index
    if(!data_is_valid_index(args[idx_pos]))
      HEIST_THROW_ERR('\''<<name<<" index "<<args[idx_pos]<<" isn't a proper non-negative integer!"
        << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR(name,args));
    // confirm index falls w/in range of the sequence
    const size_type i = (size_type)args[idx_pos].num.extract_inexact();
    const size_type l = (args[sequence_pos].*seq_ptr)->size();
    if(i >= l)
      HEIST_THROW_ERR('\''<<name<<" received out of range index " << i 
        <<" for "<<seq_name<<' '<<args[sequence_pos]<<" of size "
        <<l<<'!'<<format<<HEIST_FCN_ERR(name,args));
    return i;
  }


  // Confirm given a valid vector index. Returns idx as a 'size_type' if so
  size_type get_if_valid_vector_idx(const data_vector& args,const char* name, const char* format, const size_type& idx_pos=1){
    return get_if_valid_str_or_vec_idx(args,name,format,"vector",idx_pos,0,&data::vec);
  }


  // Confirm given a valid string index. Returns idx as a 'size_type' if so
  size_type get_if_valid_string_idx(const data_vector& args,const char* name, const char* format, const size_type& idx_pos=1){
    return get_if_valid_str_or_vec_idx(args,name,format,"string",idx_pos,0,&data::str);
  }

  /******************************************************************************
  * GENERAL LIST HELPER PROCEDURES
  ******************************************************************************/

  size_type get_list_length(const data& d, size_type count = 0)noexcept{
    if(!d.is_type(types::par)) return count;
    return get_list_length(d.par->second,count+1);
  }


  void confirm_proper_same_sized_lists(const data_vector& lists,const char* name, const char* format,
                                       const int first_list_idx, const data_vector& args){
    size_type length = 0;
    for(size_type i = 0, n = lists.size(); i < n; ++i) {
      // confirm proper list
      if(!primitive_toolkit::data_is_proper_list(lists[i]))
        HEIST_THROW_ERR('\''<<name<<" <list> arg #" << first_list_idx+i+1 << ' ' << HEIST_PROFILE(lists[i])
          << " isn't a proper list!" << format << HEIST_FCN_ERR(name,args));
      // confirm congruent length
      if(i == 0) length = get_list_length(lists[i]);
      else if(length != get_list_length(lists[i]))
        HEIST_THROW_ERR('\''<<name<<" <list> lists "<< lists[0] << " and " 
          << lists[i] << " differ in length!" << format << HEIST_FCN_ERR(name,args));
    }
  }


  // Adds cars to 'args' & advances cdrs. Returns whether lists are empty
  bool check_empty_list_else_acquire_cars_advance_cdrs(data_vector& curr_pairs, data_vector& args)noexcept{
    // Return if fully iterated through each list
    if(!curr_pairs[0].is_type(types::par)) return true;
    // Add each arg for 'proc' & advance each list's head ptr
    for(size_type i = 0, n = curr_pairs.size(); i < n; ++i) {
      args[i] = curr_pairs[i].par->first;
      curr_pairs[i] = curr_pairs[i].par->second;
    }
    return false;
  }

  /******************************************************************************
  * LENGTH & LENGTH+
  ******************************************************************************/

  data compute_length(data_vector& args, const char* name, const char* format){
    switch(is_proper_sequence(args[0],args,name,format)) {
      case heist_sequence::vec: return num_type(args[0].vec->size());
      case heist_sequence::str: return num_type(args[0].str->size());
      case heist_sequence::nul: return num_type();
      default: return num_type(get_list_length(args[0]));
    }
  }

  /******************************************************************************
  * REVERSE & REVERSE!
  ******************************************************************************/

  data reverse_list(data& d, const data& acc = symconst::emptylist)noexcept{
    if(!d.is_type(types::par)) return acc;
    data rev_list = make_par();
    rev_list.par->first = d.par->first;
    rev_list.par->second = acc;
    return reverse_list(d.par->second, rev_list);
  }


  data reverse_bang_list(data& d)noexcept{
    *d.par = *reverse_list(d).par;
    return GLOBALS::VOID_DATA_OBJECT;
  }


  template<typename SEQUENCE_PTR>
  data reverse_bang_random_access_seq(data& d, SEQUENCE_PTR seq_ptr)noexcept{
    std::reverse((d.*seq_ptr)->begin(), (d.*seq_ptr)->end());
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * FOLD
  ******************************************************************************/

  template<bool FOLDING_LEFT>
  constexpr const char* get_fold_function_name()noexcept{
    if constexpr (FOLDING_LEFT) { return "fold"; } else { return "fold-right"; }
  }


  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template<bool FOLDING_LEFT, typename SEQUENCE_PTR>
  void fold_random_access_seq_accumulator(data_vector& sequences, data& proc, data& init_val, SEQUENCE_PTR seq_ptr){
    if((sequences[0].*seq_ptr)->empty()) return;
    const size_type total_sequences = sequences.size();
    if constexpr (FOLDING_LEFT) {
      // for each ith element
      for(size_type i = 0, n = (sequences[0].*seq_ptr)->size(); i < n; ++i){
        // in each sequence
        data_vector args(total_sequences+1);
        args[0] = init_val;
        for(size_type j = 0; j < total_sequences; ++j)
          args[j+1] = (sequences[j].*seq_ptr)->operator[](i); // extract the ith elements
        init_val = execute_application(proc,std::move(args)); // and accumulate the elements
      }
    } else {
      // for each ith element (in reverse)
      for(size_type i = (sequences[0].*seq_ptr)->size(); i-- > 0;){
        // in each sequence
        data_vector args(total_sequences+1);
        args[total_sequences] = init_val;
        for(size_type j = 0; j < total_sequences; ++j)
          args[j] = (sequences[j].*seq_ptr)->operator[](i); // extract the ith elements
        init_val = execute_application(proc,std::move(args)); // and accumulate the elements
      }
    }
  }


  template<bool FOLDING_LEFT, types SEQ_TYPE, typename SEQUENCE_PTR>
  data fold_random_access_seq(data& procedure, data_vector& args, const char* format, SEQUENCE_PTR seq_ptr){
    static constexpr const char* name = get_fold_function_name<FOLDING_LEFT>();
    data_vector sequences(args.begin()+2, args.end());
    confirm_same_sized_random_access_seqs<SEQ_TYPE>(sequences,name,format,args,seq_ptr,2);
    // Apply the procedure on each elt of each sequence, & accumulate the result
    data init_val = args[1];
    fold_random_access_seq_accumulator<FOLDING_LEFT>(sequences,procedure,init_val,seq_ptr);
    return init_val; // return the accumulated value
  }


  // -- LISTS
  template<bool FOLDING_LEFT>
  void fold_list_accumulator(data_vector& curr_pairs, data& proc, data& init_val){
    // Return if fully iterated through each list
    if(!curr_pairs[0].is_type(types::par)) return;
    data_vector args;
    // Add each arg for 'proc' & advance each list's head ptr
    for(auto& list_head : curr_pairs) {
      args.push_back(list_head.par->first);
      list_head = list_head.par->second;
    }
    // Execute proc, accumulate result, & recurse down the rest of the lists
    if constexpr (FOLDING_LEFT) { // fold is preorder
      args.insert(args.begin(), init_val);
      init_val = execute_application(proc,std::move(args));
    }
    fold_list_accumulator<FOLDING_LEFT>(curr_pairs,proc,init_val);
    if constexpr (!FOLDING_LEFT) { // fold-right is postorder
      args.insert(args.end(), init_val);
      init_val = execute_application(proc,std::move(args));
    }
  }


  template<bool FOLDING_LEFT>
  data fold_list(data& procedure, data_vector& args, const char* format){
    static constexpr const char* name = get_fold_function_name<FOLDING_LEFT>();
    // Confirm only given proper lists of the same length
    data_vector list_heads(args.begin()+2, args.end());
    confirm_proper_same_sized_lists(list_heads,name,format,2,args);
    // Apply the procedure on each elt of each list, & accumulate the result
    data init_val = args[1];
    fold_list_accumulator<FOLDING_LEFT>(list_heads,procedure,init_val);
    return init_val; // return the accumulated value
  }

  /******************************************************************************
  * FILTER & REMOVE
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template <bool(*truth_proc)(data&,data_vector&&), typename SEQUENCE_PTR>
  data random_access_seq_selective_iteration(data& procedure, data_vector& args, const types seq_type, SEQUENCE_PTR seq_ptr){
    data_vector pruned_sequence;
    for(auto& d : *(args[1].*seq_ptr))
      if(truth_proc(procedure,data_vector(1,d))) // true = filter, false = rm
        pruned_sequence.push_back(data(d));
    if(seq_type == types::str) {
      string str;
      for(auto& letter : pruned_sequence)
        str += letter.chr;
      return make_str(str);
    }
    return make_vec(pruned_sequence);
  }


  // -- LISTS
  template <bool(*truth_proc)(data&,data_vector&&)>
  data list_selective_iteration(data& procedure, data& curr_pair){
    if(!curr_pair.is_type(types::par)) return symconst::emptylist;
    if(truth_proc(procedure,data_vector(1,curr_pair.par->first))) {
      data filtered = make_par();
      filtered.par->first = curr_pair.par->first;
      filtered.par->second = list_selective_iteration<truth_proc>(procedure,curr_pair.par->second);
      return filtered;
    }
    return list_selective_iteration<truth_proc>(procedure,curr_pair.par->second);
  }

  /******************************************************************************
  * MAP
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  data convert_data_vector_to_string(const data_vector& sequence, const data_vector& args, const char* name, const char* format){
    if(sequence.empty()) return make_str("");
    string generated_string;
    for(size_type i = 0, n = sequence.size(); i < n; ++i) {
      if(!sequence[i].is_type(types::chr))
        HEIST_THROW_ERR('\''<<name<<" generated value #" << i+1 << ' ' << HEIST_PROFILE(sequence[i]) 
          << " isn't\n     a character:" << format << "\n     Generated values: " 
          << sequence << HEIST_FCN_ERR(name,args));
      generated_string += sequence[i].chr;
    }
    return make_str(generated_string);
  }


  template<typename SEQUENCE_PTR>
  void random_access_seq_map_constructor(data_vector& sequences, data& proc, data_vector& mapped_sequence, SEQUENCE_PTR seq_ptr){
    const size_type total_sequences = sequences.size();
    const size_type total_elts = (sequences[0].*seq_ptr)->size();
    // Traverse each elt
    for(size_type i = 0; i < total_elts; ++i) {
      data_vector args(total_sequences);
      // Aquire ith elt from each sequence
      for(size_type j = 0; j < total_sequences; ++j)
        args[j] = (sequences[j].*seq_ptr)->operator[](i);
      // Push the mapped result of each elt
      mapped_sequence.push_back(execute_application(proc,std::move(args)));
    }
  }


  template<types SEQ_TYPE, typename SEQUENCE_PTR>
  data random_access_seq_map(data& procedure, data_vector& args, const char* name, const char* format, SEQUENCE_PTR seq_ptr){
    data_vector sequences(args.begin()+1, args.end());
    confirm_same_sized_random_access_seqs<SEQ_TYPE>(sequences,name,format,args,seq_ptr,1);
    // Apply the procedure on each elt of each sequence & store the result
    data_vector mapped_sequence;
    random_access_seq_map_constructor(sequences,procedure,mapped_sequence,seq_ptr);
    if constexpr (SEQ_TYPE == types::str) {
      return convert_data_vector_to_string(mapped_sequence,args,name,format);
    } else {
      return make_vec(mapped_sequence);
    }
  }


  // -- LISTS
  data list_map_constructor(data_vector& curr_pairs, data& proc){
    data_vector args(curr_pairs.size());
    if(check_empty_list_else_acquire_cars_advance_cdrs(curr_pairs,args)) return symconst::emptylist;
    // Execute proc, store result, & recurse down the rest of the lists
    data mapped = make_par();
    mapped.par->first = execute_application(proc, std::move(args));
    mapped.par->second = list_map_constructor(curr_pairs, proc);
    return mapped;
  }


  data list_map(data& procedure, data_vector& args, const char* format){
    // Mapping a list or '() -> get the head of each list
    data_vector list_heads(args.begin()+1, args.end());
    confirm_proper_same_sized_lists(list_heads,"map",format,1,args);
    // Apply the procedure on each elt of each list & store the result
    return list_map_constructor(list_heads, procedure);
  }

  /******************************************************************************
  * MAP!
  ******************************************************************************/

  // -- LISTS
  void list_map_bang_constructor(data_vector& curr_pairs, data& proc){
    data_vector args(curr_pairs.size());
    auto map_to = curr_pairs[0];
    if(check_empty_list_else_acquire_cars_advance_cdrs(curr_pairs,args)) return;
    // Execute proc, store result, & recurse down the rest of the lists
    map_to.par->first = execute_application(proc,std::move(args));
    list_map_bang_constructor(curr_pairs, proc);
  }


  data list_map_bang(data& procedure, data_vector& args, const char* format){
    data_vector list_heads(args.begin()+1, args.end());
    confirm_proper_same_sized_lists(list_heads,"map!",format,1,args);
    list_map_bang_constructor(list_heads, procedure);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * FOR-EACH
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template<typename SEQUENCE_PTR>
  void random_access_seq_for_each_applicator(data_vector& sequences, data& proc, SEQUENCE_PTR seq_ptr){
    const size_type total_sequences = sequences.size();
    const size_type total_elts = (sequences[0].*seq_ptr)->size();
    // Traverse each elt
    for(size_type i = 0; i < total_elts; ++i) {
      data_vector args(total_sequences);
      // Aquire ith elt from each sequence
      for(size_type j = 0; j < total_sequences; ++j)
        args[j] = (sequences[j].*seq_ptr)->operator[](i);
      // Apply the procedure to each elt
      execute_application(proc,std::move(args));
    }
  }


  template<types SEQ_TYPE, typename SEQUENCE_PTR>
  data random_access_seq_for_each(data& procedure, data_vector& args, const char* format, SEQUENCE_PTR seq_ptr){
    data_vector sequences(args.begin()+1, args.end());
    confirm_same_sized_random_access_seqs<SEQ_TYPE>(sequences,"for-each",format,args,seq_ptr,1);
    // Apply the procedure on each elt of each sequence & store the result
    random_access_seq_for_each_applicator(sequences, procedure, seq_ptr);
    return GLOBALS::VOID_DATA_OBJECT;
  }


  // -- LISTS
  void list_for_each_applicator(data_vector& curr_pairs, data& proc){
    data_vector args(curr_pairs.size());
    if(check_empty_list_else_acquire_cars_advance_cdrs(curr_pairs,args)) return;
    // Execute proc & recurse down the rest of the lists
    execute_application(proc,std::move(args));
    list_for_each_applicator(curr_pairs, proc);
  }


  data list_for_each(data& procedure, data_vector& args, const char* format){
    data_vector list_heads(args.begin()+1, args.end());
    confirm_proper_same_sized_lists(list_heads,"for-each",format,1,args);
    list_for_each_applicator(list_heads, procedure);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * SEQ-COPY!
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template<typename SEQ_PTR>
  data random_access_seq_copy_bang(SEQ_PTR& s1, SEQ_PTR& s2)noexcept{
    for(size_type i = 0, n = s1->size() < s2->size() ? s1->size() : s2->size(); i < n; ++i)
      s1->operator[](i) = s2->operator[](i);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  
  // -- LISTS
  data list_copy_bang(data& dest_pair,data& source_pair)noexcept{
    if(!dest_pair.is_type(types::par) || !source_pair.is_type(types::par)) 
      return GLOBALS::VOID_DATA_OBJECT;
    dest_pair.par->first = source_pair.par->first;
    return list_copy_bang(dest_pair.par->second, source_pair.par->second);
  }

  /******************************************************************************
  * COUNT
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template<typename SEQUENCE_PTR>
  data random_access_seq_count(data& procedure, data_vector& args, SEQUENCE_PTR seq_ptr){
    size_type count = 0;
    for(auto& d : *(args[1].*seq_ptr))
      count += size_type(is_true_scm_condition(procedure,data_vector(1,d)));
    return num_type(count);
  }


  // -- LISTS
  size_type list_count(const data& curr_pair, data& pred, size_type count = 0){
    if(!curr_pair.is_type(types::par)) return count;
    count += size_type(is_true_scm_condition(pred,data_vector(1,curr_pair.par->first))); // if(pred(elt)) ++count
    return list_count(curr_pair.par->second,pred,count);
  }

  /******************************************************************************
  * REF
  ******************************************************************************/

  // -- LISTS
  data list_ref_seeker(const data& curr_pair, const size_type& idx, const char* format,
                       const data_vector& args, const size_type& pos=0){
    if(!curr_pair.is_type(types::par))
      HEIST_THROW_ERR("'ref <list> received out of range index " << idx 
        <<"\n     for list "<<args[0]<<" of size "<<pos<<'!'<<format
        << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("ref",args));
    if(pos == idx) return curr_pair.par->first;
    return list_ref_seeker(curr_pair.par->second,idx,format,args,pos+1);
  }


  data list_ref(const data_vector& args, const char* format) {
    if(!data_is_valid_index(args[1])) 
      HEIST_THROW_ERR("'ref <list> 2nd arg " << HEIST_PROFILE(args[1]) << " is an invalid <index>:"
        << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("ref",args));
    return list_ref_seeker(args[0],(size_type)args[1].num.extract_inexact(),format,args);
  }

  /******************************************************************************
  * SLICE
  ******************************************************************************/

  size_type confirm_valid_slice_length(data_vector& args, const size_type& seq_len,  const size_type& start_idx, 
                                                          const char* type_instance, const char* format){
    if(args.size() == 2) return seq_len - start_idx; // to the end of the sequence
    if(!args[2].is_type(types::num) || !args[2].num.is_integer())
      HEIST_THROW_ERR("'slice " << type_instance << " received non-integer length " << HEIST_PROFILE(args[2])
         << '!' << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("slice", args));
    if(args[2].num.is_neg()) args[2].num = seq_len - start_idx + args[2].num; // negative length
    if(args[2].num.is_neg()) args[2].num = num_type(); // excessive negative offset -> length of 0
    auto length = (size_type)args[2].num.extract_inexact();
    if(start_idx + length > seq_len) length = seq_len - start_idx;
    return length;
  }


  // -- STRINGS
  data string_slice(data_vector& args, const char* format) {
    // confirm given valid in-'size_type'-range non-negative start index
    if(!data_is_valid_index(args[1]))
      HEIST_THROW_ERR("'slice <string> arg "<<HEIST_PROFILE(args[1])
        <<" isn't a proper non-negative integer!"<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<HEIST_FCN_ERR("slice",args));
    const size_type start      = (size_type)args[1].num.extract_inexact();
    const size_type str_length = args[0].str->size();
    if(start == str_length) return make_str(""); // idx = length -> ""
    if(start+1 > str_length)
      HEIST_THROW_ERR("'slice <string> <start-index> "<<start<<" is out of range for string "
        <<args[0]<<" of length "<<str_length<<'!'<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<HEIST_FCN_ERR("slice",args));
    const auto length = confirm_valid_slice_length(args,args[0].str->size(),start,"<string>",format);
    return make_str(args[0].str->substr(start,length));
  }


  // -- VECTORS
  data vector_slice(data_vector& args, const char* format) {
    // confirm given valid in-'size_type'-range non-negative start index
    if(!data_is_valid_index(args[1]))
      HEIST_THROW_ERR("'slice <vector> arg "<<HEIST_PROFILE(args[1])
        <<" isn't a proper non-negative integer!"<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<HEIST_FCN_ERR("slice",args));
    const size_type start      = (size_type)args[1].num.extract_inexact();
    const size_type vec_length = args[0].vec->size();
    if(start == vec_length) return make_vec(data_vector()); // idx = length -> '#()
    if(start+1 > vec_length)
      HEIST_THROW_ERR("'slice <vector> <start-index> "<<start<<" is out of range for vector "
        <<args[0]<<" of length "<<vec_length<<'!'<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<HEIST_FCN_ERR("slice",args));
    const auto length = confirm_valid_slice_length(args,vec_length,start,"<vector>",format);
    // Extract the subvector
    if(!length) return make_vec(data_vector()); // length = 0 -> '#()
    return make_vec(data_vector(args[0].vec->begin()+start, args[0].vec->begin()+start+length));
  }


  // -- LISTS
  // recursively mk sublist from 'curr_pair's [start,end)
  data list_slice_constructor(data& curr_pair,      const size_type& start, 
                              const size_type& end, const size_type& count = 0)noexcept{
    if(count == end || !curr_pair.is_type(types::par)) return symconst::emptylist;
    if(count >= start) {
      data sublist = make_par();
      sublist.par->first = curr_pair.par->first;
      sublist.par->second = list_slice_constructor(curr_pair.par->second, start, end, count+1);
      return sublist;
    }
    return list_slice_constructor(curr_pair.par->second, start, end, count+1);
  }


  data list_slice(data_vector& args, const char* format){  
    // confirm given valid in-'size_type'-range non-negative start index
    if(!data_is_valid_index(args[1]))
      HEIST_THROW_ERR("'slice <list> index "<<HEIST_PROFILE(args[1])
        <<" isn't a proper non-negative integer!"<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<HEIST_FCN_ERR("slice",args));
    const size_type start      = (size_type)args[1].num.extract_inexact();
    const size_type lis_length = get_list_length(args[0]);
    if(start == lis_length) return symconst::emptylist;  // idx = length -> '()
    if(start+1 > lis_length)
      HEIST_THROW_ERR("'slice <list> <start-index> "<<start<<" is out of range for list "
        <<args[0]<<" of length "<<lis_length<<'!'<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<HEIST_FCN_ERR("slice",args));
    const auto length = confirm_valid_slice_length(args,lis_length,start,"<list>",format);
    // Extract the sublist
    if(!length) return symconst::emptylist; // length = 0 -> '()
    return list_slice_constructor(args[0], start, start+length);
  }

  /******************************************************************************
  * SLICE
  ******************************************************************************/

  // -- LISTS
  void list_set_index(data& curr_pair, const size_type& idx, const char* format, 
                                       data_vector& args,    const size_type& pos=0){
    if(!curr_pair.is_type(types::par))
      HEIST_THROW_ERR("'set-index! <list> received out of range index " << idx 
        <<"\n     for list "<<args[0]<<" of size "<<pos<<'!'<<format
        << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("set-index!",args));
    if(pos == idx) {
      curr_pair.par->first = args[2];
    } else {
      list_set_index(curr_pair.par->second,idx,format,args,pos+1);
    }
  }

  /******************************************************************************
  * SWAP-INDICES!
  ******************************************************************************/

  void list_swap_indices_applicator(data& curr_pair,const size_type& front_idx, const size_type& back_idx, data& first_node,
                                                    const char* format, data_vector& args, const size_type& pos=0){
    if(!curr_pair.is_type(types::par))
      HEIST_THROW_ERR("'swap-indices! <list> received out of range index pair " << front_idx 
        << " & " << back_idx <<"\n     for list "<<args[0]<<" of size "<<pos<<'!'<<format
        << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("swap-indices!",args));
    if(pos == front_idx) {
      list_swap_indices_applicator(curr_pair.par->second,front_idx,back_idx,curr_pair.par->first,format,args,pos+1);
    } else if(pos == back_idx) {
      std::swap(first_node,curr_pair.par->first);
    } else {
      list_swap_indices_applicator(curr_pair.par->second,front_idx,back_idx,first_node,format,args,pos+1);
    }
  }


  void list_swap_indices(data_vector& args, const char* format){
    if(!data_is_valid_index(args[1])) 
      HEIST_THROW_ERR("'swap-indices! <list> 2nd arg " << HEIST_PROFILE(args[1]) << " is an invalid <index>:"
        << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("swap-indices!",args));
    if(!data_is_valid_index(args[2])) 
      HEIST_THROW_ERR("'swap-indices! <list> 3rd arg " << HEIST_PROFILE(args[2]) << " is an invalid <index>:"
        << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("swap-indices!",args));
    size_type front_idx = (size_type)args[1].num.extract_inexact();
    size_type back_idx = (size_type)args[2].num.extract_inexact();
    data tmp; // dummy first node
    if(front_idx < back_idx) {
      list_swap_indices_applicator(args[0],front_idx,back_idx,tmp,format,args);
    } else if(front_idx > back_idx) {
      list_swap_indices_applicator(args[0],back_idx,front_idx,tmp,format,args);
    }
  }

  /******************************************************************************
  * FILL!
  ******************************************************************************/

  // -- STRINGS
  data string_fill_bang(data_vector& args, const char* format){
    if(!args[1].is_type(types::chr))
      HEIST_THROW_ERR("'fill! <string> for "<<HEIST_PROFILE(args[0])<<" received non-character fill-value\n     " 
        << HEIST_PROFILE(args[1]) << '!'<< format << HEIST_FCN_ERR("fill!", args));
    for(size_type i = 0, n = args[0].str->size(); i < n; ++i)
      args[0].str->operator[](i) = args[1].chr;
    return GLOBALS::VOID_DATA_OBJECT;
  }


  // -- VECTORS
  data vector_fill_bang(data_vector& args)noexcept{
    for(size_type i = 0, n = args[0].vec->size(); i < n; ++i)
      args[0].vec->operator[](i) = args[1];
    return GLOBALS::VOID_DATA_OBJECT;
  }


  // -- LISTS
  data list_fill_bang(data& curr_pair, data& fill_value)noexcept{
    if(!curr_pair.is_type(types::par)) return GLOBALS::VOID_DATA_OBJECT;
    curr_pair.par->first = fill_value;
    return list_fill_bang(curr_pair.par->second,fill_value);
  }

  /******************************************************************************
  * APPEND
  ******************************************************************************/

  // -- STRINGS
  data string_append(data_vector& args, const char* format){
    string str_val;
    const auto n = args.size()-1;
    for(size_type i = 0; i < n; ++i) {
      if(args[i].is_type(types::chr)) {
        str_val += args[i].chr;
      } else if(args[i].is_type(types::str)) {
        str_val += *args[i].str;
      } else {
        HEIST_THROW_ERR("'append <string> arg #" << i+1 << ", " << HEIST_PROFILE(args[i]) 
          << ", isn't a string or character:" << format << HEIST_FCN_ERR("append", args));
      }
    }
    if(str_val.empty()) return *args.rbegin(); // (append <empty-sequence1> ... <empty-sequenceN> <obj>) = <obj>
    if(args[n].is_type(types::chr)) {
      str_val += args[n].chr;
    } else if(args[n].is_type(types::str)) {
      str_val += *args[n].str;
    } else {
      HEIST_THROW_ERR("'append <string> arg #" << n+1 << ", " << HEIST_PROFILE(args[n]) 
        << ", isn't a string or character:" << format << HEIST_FCN_ERR("append", args));
    }
    return make_str(str_val);
  }


  // -- VECTORS
  data vector_append(data_vector& args, const char* format){
    data_vector appended_vector;
    for(size_type i = 0, n = args.size()-1; i < n; ++i) {
      if(!args[i].is_type(types::vec))
        HEIST_THROW_ERR("'append <vector> arg #"<<i+1<<' '<<HEIST_PROFILE(args[i])
          << " isn't a vector:" << format << HEIST_FCN_ERR("append",args));
      appended_vector.insert(appended_vector.end(), args[i].vec->begin(), args[i].vec->end());
    }
    if(appended_vector.empty()) return *args.rbegin(); // (append <empty-sequence1> ... <empty-sequenceN> <obj>) = <obj>
    if(!args.rbegin()->is_type(types::vec))
      appended_vector.push_back(*args.rbegin());
    else
      appended_vector.insert(appended_vector.end(), args.rbegin()->vec->begin(), args.rbegin()->vec->end());
    return make_vec(appended_vector);
  }


  // -- LISTS
  void unpack_possibly_dotted_list_into_data_vector(data& curr_pair, data_vector& args_list)noexcept{
    if(curr_pair.is_type(types::par)) {
      args_list.push_back(curr_pair.par->first);
      unpack_possibly_dotted_list_into_data_vector(curr_pair.par->second, args_list); 
    } else if(!primitive_toolkit::data_is_nil(curr_pair)) {
      args_list.push_back(curr_pair);
    }
  }


  void list_append_last_item(data& LHS, data& RHS)noexcept{
    if(!LHS.par->second.is_type(types::par)) {
      if(!RHS.is_type(types::par)) {
        LHS.par->second = RHS;
      } else {
        LHS.par->second = RHS.shallow_copy();
      }
    } else {
      list_append_last_item(LHS.par->second,RHS);
    }
  }


  // PRECONDITIONS: 1) THE FIRST n-1 ARGS MUST HAVE "list?" = TRUE
  //                2) THE LAST ARG MUST NOT BE A CYCLIC LIST
  data list_append(data_vector& args, const char* format){
    // (append <obj>) = <obj>
    const auto n = args.size();
    // Confirm Precondition 2
    if(primitive_toolkit::data_is_circular_list(args[n-1]))
      HEIST_THROW_ERR("'append <list> last argument "<<HEIST_PROFILE(args[n-1])
        <<" isn't an acyclic list:"<< format << HEIST_FCN_ERR("append", args));
    // Confirm Precondition 1
    for(size_type i = 0; i < n-1; ++i) {
      if(primitive_toolkit::data_is_nil(args[i])) continue;
      if(!args[i].is_type(types::par))
        HEIST_THROW_ERR("'append <list> argument #" << i+1 << ' ' << HEIST_PROFILE(args[i]) 
          << " isn't a pair:" << format << HEIST_FCN_ERR("append", args));
      else if(auto stat = get_list_status(args[i]); 
        stat == list_status::circular) {
        HEIST_THROW_ERR("'append <list> argument #" << i+1 << ' ' << HEIST_PROFILE(args[i]) 
          << " isn't an acyclic list:" << format << HEIST_FCN_ERR("append", args));
      } else if(stat == list_status::dotted)
        HEIST_THROW_ERR("'append <list> argument #" << i+1 << ' ' << HEIST_PROFILE(args[i]) 
          << " isn't a '() terminated list:" << format << HEIST_FCN_ERR("append", args));
    }
    // Link the guarenteed proper lists to one another
    data_vector appended;
    for(size_type i = 0; i < n-1; ++i)
      if(!primitive_toolkit::data_is_nil(args[i])) 
        unpack_possibly_dotted_list_into_data_vector(args[i],appended);
    // Link last object (anything except a circular list)
    if(appended.empty()) return args[n-1];
    auto appended_lists = primitive_toolkit::convert_data_vector_to_proper_list(appended.begin(), appended.end());
    list_append_last_item(appended_lists,args[n-1]);
    return appended_lists;
  }

  /******************************************************************************
  * REMOVE-FIRST & REMOVE-LAST
  ******************************************************************************/

  template <bool REMOVING_FIRST, typename SEQUENCE_TYPE>
  auto remove_first_or_last(data& pred, SEQUENCE_TYPE sequence){
    // constexpr version of ?:
    auto start = [](SEQUENCE_TYPE& s){if constexpr (REMOVING_FIRST) return s.begin(); else return s.rbegin();}(sequence);
    auto end   = [](SEQUENCE_TYPE& s){if constexpr (REMOVING_FIRST) return s.end();   else return s.rend();}(sequence);
    for(; start != end; ++start) {
      if(is_true_scm_condition(pred,data_vector(1,*start))) {
        if constexpr (REMOVING_FIRST) {
          sequence.erase(start);
        } else {
          sequence.erase((start+1).base());
        }
        break;
      }
    }
    return sequence;
  }

  /******************************************************************************
  * DELETE
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template<size_type(*get_idx_if_valid)(const data_vector&,const char*,const char*,const size_type&),typename SEQUENCE_PTR>
  auto random_access_seq_delete(data_vector& args, const char* format, SEQUENCE_PTR seq_ptr){
    auto new_sequence(*(args[0].*seq_ptr));
    new_sequence.erase(new_sequence.begin()+get_idx_if_valid(args,"delete",format,1));
    return new_sequence;
  }


  // -- LISTS
  data list_delete_recur(data& p, const size_type& pos, const size_type& idx, data_vector& args, const char* format) {
    if(!p.is_type(types::par))
      HEIST_THROW_ERR("'delete <list> received out of range index " << idx <<" for list "
        << args[0] << '!' << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("delete",args));
    if(pos == idx) {
      return p.par->second.shallow_copy();
    } else {
      data del = make_par();
      del.par->first = p.par->first;
      del.par->second = list_delete_recur(p.par->second,pos+1,idx,args,format);
      return del;
    }
  }


  data list_delete(data_vector& args, const char* format){
    if(!data_is_valid_index(args[1])) 
      HEIST_THROW_ERR("'delete <list> 2nd arg " << HEIST_PROFILE(args[1]) << " is an invalid <index>:"
        << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("delete",args));
    return list_delete_recur(args[0],0,(size_type)args[1].num.extract_inexact(),args,format);
  }

  /******************************************************************************
  * LAST
  ******************************************************************************/

  data list_last(const data& curr_pair)noexcept{
    if(curr_pair.par->second.is_type(types::par)) return list_last(curr_pair.par->second);
    return curr_pair.par->first;
  }

  /******************************************************************************
  * INIT
  ******************************************************************************/

  data list_init(const data& curr_pair)noexcept{
    if(!curr_pair.par->second.is_type(types::par)) return symconst::emptylist;
    data new_pair = data(make_par());
    new_pair.par->first = curr_pair.par->first;
    new_pair.par->second = list_init(curr_pair.par->second);
    return new_pair;
  }

  /******************************************************************************
  * SEQ=
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template<types SEQ_TYPE, typename SEQUENCE_PTR>
  data random_access_seq_eq(data& procedure,data_vector& args,const char* format, SEQUENCE_PTR seq_ptr){
    // Confirm only given sequences of type t (besides the procedure)
    static constexpr const char* seq_name = get_random_access_seq_typename<SEQ_TYPE>();
    bool same_sizes = true;
    const size_type total_sequences = args.size();
    for(size_type i = 1, n = total_sequences; i < n; ++i) {
      if(!args[i].is_type(SEQ_TYPE))
        HEIST_THROW_ERR("'seq= <"<<seq_name<<"> arg #"<<i+1 <<' '<<HEIST_PROFILE(args[i])
          <<" isn't a "<<seq_name<<':' << format << HEIST_FCN_ERR("seq=",args));
      same_sizes = same_sizes && ((args[i].*seq_ptr)->size() == (args[1].*seq_ptr)->size());
    }
    if(total_sequences < 3) return GLOBALS::TRUE_DATA_BOOLEAN; // 0 or 1 sequence -> true
    if(!same_sizes) return GLOBALS::FALSE_DATA_BOOLEAN; // sequences of != sizes are !=
    // Confirm each sequence is elt=?
    const size_type total_elements = (args[1].*seq_ptr)->size();
    for(size_type i = 0; i < total_elements; ++i) { // for each element
      data_vector sequence_args(total_sequences-1);
      for(size_type j = 1; j < total_sequences; ++j) // in each sequence
        sequence_args[j-1] = (args[j].*seq_ptr)->operator[](i);
      if(is_false_scm_condition(procedure,std::move(sequence_args))) // if elts are !=
        return GLOBALS::FALSE_DATA_BOOLEAN; // sequences are !=
    }
    return GLOBALS::TRUE_DATA_BOOLEAN; // else sequences are ==
  }


  // -- LISTS
  data list_seq_eq(data& procedure, data_vector& args, const char* format){
    // Confirm only given lists (besides the procedure)
    bool same_sizes = true;
    const size_type total_lists = args.size();
    std::vector<data_vector> lists_as_exps(total_lists-1);
    for(size_type i = 1, n = total_lists; i < n; ++i) {
      if(!primitive_toolkit::data_is_nil(args[i])) {
        if(!args[i].is_type(types::par) || get_list_status(args[i]) != list_status::proper)
          HEIST_THROW_ERR("'seq= <list> arg #"<<i+1<<' '<<HEIST_PROFILE(args[i])
            <<" isn't a proper null-terminated list:"<< format << HEIST_FCN_ERR("seq=", args));
        lists_as_exps[i-1] = primitive_toolkit::convert_proper_list_to_data_vector(args[i]);
      }
      same_sizes = same_sizes && lists_as_exps[i-1].size() == lists_as_exps[0].size();
    }
    if(total_lists < 3) return GLOBALS::TRUE_DATA_BOOLEAN; // 0 or 1 list -> true
    if(!same_sizes)     return GLOBALS::FALSE_DATA_BOOLEAN; // lists of != sizes are !=
    // Confirm each list is elt=?
    const size_type total_elements = lists_as_exps[0].size();
    for(size_type i = 0; i < total_elements; ++i) { // for each element
      data_vector lis_args(total_lists-1);
      for(size_type j = 0; j+1 < total_lists; ++j) // in each list
        lis_args[j] = lists_as_exps[j][i];
      if(is_false_scm_condition(procedure,std::move(lis_args))) // if elts are !=
        return GLOBALS::FALSE_DATA_BOOLEAN; // lists are !=
    }
    return GLOBALS::TRUE_DATA_BOOLEAN; // else lists are ==
  }

  /******************************************************************************
  * SKIP & INDEX
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template <bool(*truth_proc)(data&,data_vector&&), typename SEQUENCE_PTR>
  data search_random_access_seq_from_left(data& procedure, data_vector& args, SEQUENCE_PTR seq_ptr){
    if((args[1].*seq_ptr)->empty()) return GLOBALS::FALSE_DATA_BOOLEAN;
    for(size_type i = 0, n = (args[1].*seq_ptr)->size(); i < n; ++i)
      if(truth_proc(procedure,data_vector(1,(args[1].*seq_ptr)->operator[](i)))) 
        return num_type(i);
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }


  // -- LISTS
  template <bool(*truth_proc)(data&,data_vector&&)>
  data search_list_from_left(data& procedure,data& p,const size_type& count=0){
    if(!p.is_type(types::par)) return GLOBALS::FALSE_DATA_BOOLEAN;
    if(truth_proc(procedure,data_vector(1,p.par->first))) return num_type(count);
    return search_list_from_left<truth_proc>(procedure,p.par->second,count+1);
  }

  /******************************************************************************
  * SKIP-RIGHT & INDEX-RIGHT
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template <bool(*truth_proc)(data&,data_vector&&), typename SEQUENCE_PTR>
  data search_random_access_seq_from_right(data& procedure, data_vector& args, SEQUENCE_PTR seq_ptr){
    if((args[1].*seq_ptr)->empty()) return GLOBALS::FALSE_DATA_BOOLEAN;
    for(size_type i = (args[1].*seq_ptr)->size(); i-- > 0;)
      if(truth_proc(procedure,data_vector(1,(args[1].*seq_ptr)->operator[](i)))) 
        return num_type(i);
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }


  // -- LISTS
  template <bool(*truth_proc)(data&,data_vector&&)>
  data search_list_from_right_recur(data& p, const size_type& pos, data& procedure){
    if(p.is_type(types::par)) {
      auto res = search_list_from_right_recur<truth_proc>(p.par->second,pos+1,procedure);
      if(res.is_type(types::num)) return res;
      if(truth_proc(procedure,data_vector(1,p.par->first))) return num_type(pos);
    }
    return GLOBALS::FALSE_DATA_BOOLEAN;
  }


  template <bool(*truth_proc)(data&,data_vector&&)>
  data search_list_from_right(data& procedure, data_vector& args){
    return search_list_from_right_recur<truth_proc>(args[1],0,procedure);
  }

  /******************************************************************************
  * TAKE, DROP, TAKE-RIGHT, DROP-RIGHT DISPATCH
  ******************************************************************************/

  size_type get_length_if_valid(data_vector& args,  const char* name, 
                                const char* format, const char* seq_name,
                                const size_type& sequence_length){
    if(!data_is_valid_index(args[1]))
      HEIST_THROW_ERR('\''<<name<<" <"<<seq_name<<"> 2nd arg " << HEIST_PROFILE(args[1]) << " isn't a"
        "\n     proper non-negative integer length!" << format
        << "\n     <length> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' 
        << HEIST_FCN_ERR(name, args));
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(n > sequence_length)
      HEIST_THROW_ERR('\''<<name<<" <"<<seq_name<<"> 2nd arg " << HEIST_PROFILE(args[1])
        << " exceeds the length " << sequence_length
        << " of the given " << seq_name << ' '
        << args[0] << HEIST_FCN_ERR(name, args));
    return n;
  }


  template<data(*vector_logic)(data_vector&,vec_type(*)(data_vector&&),data_vector&,const char*,const char*),
           data(*string_logic)(string&,str_type(*)(string&&),data_vector&,const char*,const char*),
           data(*list_logic)(data_vector&,decltype(primitive_toolkit::convert_data_vector_to_proper_list<data_vector::iterator>),data_vector&,const char*,const char*)>
  data take_drop_template(data_vector& args,const char* name,const char* format){
    if(args.size() != 2) 
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args (given " 
        << args.size() << "):" << format 
        << "\n     <length> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' 
        << HEIST_FCN_ERR(name,args));
    switch(is_proper_sequence(args[0],args,name,format)){
      case heist_sequence::vec: return vector_logic(*args[0].vec,make_vec,args,format,"vector");
      case heist_sequence::str: return string_logic(*args[0].str,make_str,args,format,"string");
      default:
        auto flattened_list = primitive_toolkit::convert_proper_list_to_data_vector(args[0]);
        return list_logic(flattened_list,primitive_toolkit::convert_data_vector_to_proper_list,args,format,"list");
    }
  }

  /******************************************************************************
  * DROP
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data drop_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, 
                          data_vector& args, const char* format, const char* seq_name){
    // Lists DON'T require a C++ ctor to mk a Heist sequence from iterators
    if constexpr (MAKING_A_LIST) {
      return make_heist_sequence(
        sequence.begin() + 
          get_length_if_valid(args,"drop",format,seq_name,sequence.size()), 
        sequence.end());
    } else { // Strings & Vectors DO require a C++ ctor prior becoming a heist sequence
      return make_heist_sequence(SEQUENCE_TYPE(
        sequence.begin() + 
          get_length_if_valid(args,"drop",format,seq_name,sequence.size()),
        sequence.end()));
    }
  }

  /******************************************************************************
  * DROP-RIGHT
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data drop_right_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, 
                                data_vector& args, const char* format, const char* seq_name){
    if constexpr (MAKING_A_LIST) { // See "drop_GENERIC_logic"
      return make_heist_sequence(
        sequence.begin(), 
        sequence.end() - 
          get_length_if_valid(args,"drop-right",format,seq_name,sequence.size()));
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(
        sequence.begin(), 
        sequence.end() - 
          get_length_if_valid(args,"drop-right",format,seq_name,sequence.size())));
    }
  }

  /******************************************************************************
  * TAKE
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data take_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, 
                          data_vector& args, const char* format, const char* seq_name){
    if constexpr (MAKING_A_LIST) { // See "drop_GENERIC_logic"
      return make_heist_sequence(
        sequence.begin(),
        sequence.begin() + 
          get_length_if_valid(args,"take",format,seq_name,sequence.size()));
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(
        sequence.begin(),
        sequence.begin() + 
          get_length_if_valid(args,"take",format,seq_name,sequence.size())));
    }
  }

  /******************************************************************************
  * TAKE-RIGHT
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data take_right_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, 
                                data_vector& args, const char* format, const char* seq_name){
    if constexpr (MAKING_A_LIST) { // See "drop_GENERIC_logic"
      return make_heist_sequence(
        sequence.end() - 
          get_length_if_valid(args,"take-right",format,seq_name,sequence.size()),
        sequence.end());
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(
        sequence.end() - 
          get_length_if_valid(args,"take-right",format,seq_name,sequence.size()),
        sequence.end()));
    }
  }

  /******************************************************************************
  * TAKE-WHILE, DROP-WHILE, TAKE-RIGHT-WHILE, DROP-RIGHT-WHILE DISPATCH
  ******************************************************************************/

  template<data(*vector_logic)(data_vector&,vec_type(*)(data_vector&&),data&),
           data(*string_logic)(string&,str_type(*)(string&&),data&),
           data(*list_logic)(data_vector&,decltype(primitive_toolkit::convert_data_vector_to_proper_list<data_vector::iterator>),data&)>
  data take_drop_while_template(data_vector& args,const char* name,const char* format){
    if(args.size() != 2) 
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR(name,args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, name, format);
    switch(is_proper_sequence(args[1],args,name,format)){
      case heist_sequence::vec: return vector_logic(*args[1].vec,make_vec,procedure);
      case heist_sequence::str: return string_logic(*args[1].str,make_str,procedure);
      default:
        auto flattened_list = primitive_toolkit::convert_proper_list_to_data_vector(args[1]);
        return list_logic(flattened_list,primitive_toolkit::convert_data_vector_to_proper_list,procedure);
    }
  }

  /******************************************************************************
  * DROP-WHILE
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data drop_while_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, data& proc){
    size_type i = 0;
    for(const size_type n = sequence.size(); i < n; ++i)
      if(is_false_scm_condition(proc,data_vector(1,sequence[i]))) break;
    // Lists DON'T require a C++ ctor to mk a Heist sequence from iterators
    if constexpr (MAKING_A_LIST) { 
      return make_heist_sequence(sequence.begin()+i,sequence.end());
    } else { // Strings & Vectors DO require a C++ ctor prior becoming a heist sequence
      return make_heist_sequence(SEQUENCE_TYPE(sequence.begin()+i,sequence.end()));
    }
  }

  /******************************************************************************
  * DROP-RIGHT-WHILE
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data drop_right_while_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, data& proc){
    size_type i = sequence.size();
    for(; i-- > 0;) if(is_false_scm_condition(proc,data_vector(1,sequence[i]))) break;
    if constexpr (MAKING_A_LIST) { // See "drop_while_GENERIC_logic"
      return make_heist_sequence(sequence.begin(),sequence.begin()+i+1);
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(sequence.begin(),sequence.begin()+i+1));
    }
  }

  /******************************************************************************
  * TAKE-WHILE
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data take_while_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, data& proc){
    size_type i = 0;
    for(const size_type n = sequence.size(); i < n; ++i) 
      if(is_false_scm_condition(proc,data_vector(1,sequence[i]))) break;
    if constexpr (MAKING_A_LIST) { // See "drop_while_GENERIC_logic"
      return make_heist_sequence(sequence.begin(),sequence.begin()+i);
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(sequence.begin(),sequence.begin()+i));
    }
  }

  /******************************************************************************
  * TAKE-RIGHT-WHILE
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data take_right_while_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, data& proc){
    size_type i = sequence.size();
    for(; i-- > 0;) if(is_false_scm_condition(proc,data_vector(1,sequence[i]))) break;
    if constexpr (MAKING_A_LIST) { // See "drop_while_GENERIC_logic"
      return make_heist_sequence(sequence.begin()+i+1,sequence.end());
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(sequence.begin()+i+1,sequence.end()));
    }
  }

  /******************************************************************************
  * ANY & EVERY GENERAL HELPERS
  ******************************************************************************/

  template<types SEQ_TYPE>
  constexpr const char* get_seq_typename()noexcept{
    if constexpr (SEQ_TYPE == types::str) { 
      return "string"; 
    } else if(SEQ_TYPE == types::vec) { 
      return "vector"; 
    } else {
      return "list";
    }
  }


  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template <types SEQUENCE_TYPE, typename SEQUENCE_PTR>
  size_type confirm_proper_random_access_seq_any_every_args(data_vector& args, SEQUENCE_PTR seq_ptr, const char* name, const char* format){
    static constexpr const char* seq_name = get_seq_typename<SEQUENCE_TYPE>();
    if(args.size() < 2)
      HEIST_THROW_ERR('\''<<name<<" <"<<seq_name<<"> received insufficient args (only "
        << args.size() << "):" << format << HEIST_FCN_ERR(name, args));
    primitive_toolkit::confirm_data_is_callable(args[0], args, name, format);
    size_type min_sequence_length = GLOBALS::MAX_SIZE_TYPE;
    for(size_type i = 1, n = args.size(); i < n; ++i) {
      // Validate sequence type if working w/ a vector or string (lists already validated)
      if constexpr (SEQUENCE_TYPE == types::vec || SEQUENCE_TYPE == types::str) { 
        if(!args[i].is_type(SEQUENCE_TYPE))
          HEIST_THROW_ERR('\''<<name<<" <"<<seq_name<<"> arg #"<<i+1<<' '<<HEIST_PROFILE(args[i])
            <<" isn't a "<<seq_name<<':'<< format << HEIST_FCN_ERR(name, args));
        if((args[i].*seq_ptr)->size() < min_sequence_length) 
          min_sequence_length = (args[i].*seq_ptr)->size();
      } else {
        if(args[i].exp.size() < min_sequence_length) 
          min_sequence_length = args[i].exp.size();
      }
    }
    return min_sequence_length;
  }


  // -- LISTS
  bool any_every_convert_lists_to_exp_matrix_and_return_if_empty(data_vector& args, data_vector& list_exps, const char* name,  const char* format){
    list_exps.push_back(args[0]);
    list_exps.push_back(primitive_toolkit::convert_proper_list_to_data_vector(args[1]));
    for(size_type i = 2, n = args.size(); i < n; ++i) {
      if(auto stat = is_proper_sequence(args[i],args,name,format); stat == heist_sequence::nul)
        return true;
      else if(stat != heist_sequence::lis)
        HEIST_THROW_ERR('\''<<name<<" <list> arg #"<<i+1<<' '<<HEIST_PROFILE(args[i])<<" isn't a proper list:"
          << format << HEIST_FCN_ERR(name,args));
      list_exps.push_back(primitive_toolkit::convert_proper_list_to_data_vector(args[i]));
    }
    return false;
  }

  /******************************************************************************
  * ANY
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template <types SEQUENCE_TYPE, typename SEQUENCE_PTR>
  data random_access_seq_any(data& procedure, data_vector& args, SEQUENCE_PTR seq_ptr, const char* format){
    size_type min_length = confirm_proper_random_access_seq_any_every_args<SEQUENCE_TYPE>(args,seq_ptr,"any",format);
    if(!min_length) return GLOBALS::FALSE_DATA_BOOLEAN;
    const size_type total_sequences = args.size();
    // For each element
    for(size_type i = 0; i < min_length; ++i){
      // In each sequence
      data_vector any_args(total_sequences-1);
      for(size_type j = 1; j < total_sequences; ++j) {
        if constexpr (SEQUENCE_TYPE == types::vec || SEQUENCE_TYPE == types::str) {
          any_args[j-1] = (args[j].*seq_ptr)->operator[](i);
        } else { // lists aren't passed via a smart pointer member
          any_args[j-1] = args[j].exp[i];
        }
      }
      // If set of elements is true
      auto result = execute_application(procedure,std::move(any_args));
      if(result.is_truthy()) return result; // return element set
    }
    return GLOBALS::FALSE_DATA_BOOLEAN; // else return false
  }


  // -- LISTS
  data list_any(data& procedure, data_vector& args, const char* format){
    data_vector list_exps;
    if(any_every_convert_lists_to_exp_matrix_and_return_if_empty(args,list_exps,"any",format))
      return GLOBALS::FALSE_DATA_BOOLEAN;
    return random_access_seq_any<types::exp>(procedure,list_exps,nullptr,format);
  }

  /******************************************************************************
  * EVERY
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  template <types SEQUENCE_TYPE, typename SEQUENCE_PTR>
  data random_access_seq_every(data& procedure, data_vector& args, SEQUENCE_PTR seq_ptr, const char* format){
    size_type min_length = confirm_proper_random_access_seq_any_every_args<SEQUENCE_TYPE>(args,seq_ptr,"every",format);
    if(!min_length) return GLOBALS::FALSE_DATA_BOOLEAN;
    const size_type total_sequences = args.size();
    // For each element
    for(size_type i = 0; i < min_length; ++i){
      // In each sequence
      data_vector every_args(total_sequences-1);
      for(size_type j = 1; j < total_sequences; ++j) {
        if constexpr (SEQUENCE_TYPE == types::vec || SEQUENCE_TYPE == types::str) {
          every_args[j-1] = (args[j].*seq_ptr)->operator[](i);
        } else { // lists aren't passed via a smart pointer member
          every_args[j-1] = args[j].exp[i];
        }
      }
      // If set of elements is false
      auto result = execute_application(procedure,std::move(every_args));
      if(result.is_falsey()) return GLOBALS::FALSE_DATA_BOOLEAN; // return false
      if(i+1 == min_length) return result; // else return last <predicate> result
    }
    return GLOBALS::FALSE_DATA_BOOLEAN; // else return false
  }


  // -- LISTS
  data list_every(data& procedure, data_vector& args, const char* format){
    data_vector list_exps;
    any_every_convert_lists_to_exp_matrix_and_return_if_empty(args,list_exps,"every",format);
    return random_access_seq_every<types::exp>(procedure,list_exps,nullptr,format);
  }

  /******************************************************************************
  * SEQUENCE COERCIONS
  ******************************************************************************/

  data convert_string_to_list(const string& str)noexcept{
    const auto n = str.size();
    data_vector char_list(n);
    for(size_type i = 0; i < n; ++i) char_list[i] = str[i];
    return primitive_toolkit::convert_data_vector_to_proper_list(char_list.begin(),char_list.end());
  }


  // Returns success status
  // PRECONDITION: primitive_toolkit::data_is_proper_list(list)
  bool convert_list_to_string(data list, data& str)noexcept{
    string char_str;
    while(list.is_type(types::par)) {
      if(!list.par->first.is_type(types::chr)) return false;
      char_str += char(list.par->first.chr);
      list = list.par->second;
    }
    str = make_str(char_str);
    return true;
  }


  data convert_string_to_vector(const string& str)noexcept{
    data_vector char_vect;
    for(const auto& ch : str) char_vect.push_back(ch);
    return make_vec(char_vect);
  }


  // Returns success status
  // PRECONDITION: vect.is_type(types::vec)
  bool convert_vector_to_string(const data& vect, data& str)noexcept{
    string char_str;
    for(const auto& e : *vect.vec) {
      if(!e.is_type(types::chr)) return false;
      char_str += char(e.chr);
    }
    str = make_str(char_str);
    return true;
  }


  // PRECONDITION: primitive_toolkit::data_is_proper_list(list)
  data convert_list_to_vector(data& list)noexcept{
    return make_vec(primitive_toolkit::convert_proper_list_to_data_vector(list));
  }


  // PRECONDITION: vect.is_type(types::vec)
  data convert_vector_to_list(data& vect)noexcept{
    return primitive_toolkit::convert_data_vector_to_proper_list(vect.vec->begin(),vect.vec->end());
  }

  /******************************************************************************
  * GENERAL SET OPERATION HELPERS
  ******************************************************************************/

  void convert_lists_to_exp_matrix(data_vector& args, data_vector& list_exps, const char* name, const char* format){
    for(size_type i = 0, n = args.size(); i < n; ++i) {
      if(auto stat = is_proper_sequence(args[i],args,name,format); stat != heist_sequence::nul && stat != heist_sequence::lis) {
        HEIST_THROW_ERR('\''<<name<<" <list> arg #"<<i+1<<' '<<HEIST_PROFILE(args[i])<<" isn't a proper list:"
          << format << HEIST_FCN_ERR(name,args));
      } else if(stat == heist_sequence::lis) {
        list_exps.push_back(primitive_toolkit::convert_proper_list_to_data_vector(args[i]));
      }
    }
  }


  void confirm_only_given_data_with_type_of_first_arg(data_vector& args, const char* name, const char* format){
    for(auto& d : args)
      if(d.type != args[0].type)
        HEIST_THROW_ERR('\''<<name<<" arg " << HEIST_PROFILE(d) << " isn't of type " << args[0].type_name() 
          << "!" << format << HEIST_FCN_ERR(name,args));
  }


  template<typename SEQ_TYPE>
  bool found_in_seq(data& callable_predicate, const data& elt, const SEQ_TYPE& seq) {
    for(const auto& item : seq) {
      data_vector args(2);
      args[0] = elt;
      args[1] = item;
      if(is_true_scm_condition(callable_predicate,std::move(args))) return true;
    }
    return false;
  }

  /******************************************************************************
  * UNION
  ******************************************************************************/

  template<bool IS_LIST, typename SEQUENCE_CTOR, typename SEQUENCE_PTR, typename SEQUENCE_ACCESSOR>
  data random_access_seq_union(data& callable, data_vector& args, SEQUENCE_ACCESSOR sequence_accessor, 
                                  SEQUENCE_PTR seq_ptr = nullptr, SEQUENCE_CTOR make_heist_sequence = nullptr) {
    auto seq = sequence_accessor(args[0]);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      for(const auto& elt : sequence_accessor(args[i]))
        if(!found_in_seq(callable,elt,seq)) seq.push_back(elt);
    if constexpr (IS_LIST) {
      seq_ptr = make_heist_sequence = nullptr; // silences unused variables warnings
      return primitive_toolkit::convert_data_vector_to_proper_list(seq.begin(),seq.end());
    } else {
      return make_heist_sequence(seq);
    }
  }


  data vector_union(data& callable, data_vector& args, const char* format) {
    confirm_only_given_data_with_type_of_first_arg(args,"union",format);
    return random_access_seq_union<false,vec_type(*)(const data_vector&)>(callable,args,[](auto& d){return *(d.vec);},&data::vec,make_vec);
  }


  data string_union(data& callable, data_vector& args, const char* format) {
    confirm_only_given_data_with_type_of_first_arg(args,"union",format);
    return random_access_seq_union<false,str_type(*)(const string&)>(callable,args,[](auto& d){return *(d.str);},&data::str,make_str);
  }


  data list_union(data& callable, data_vector& args, const char* format) {
    data_vector list_exps;
    convert_lists_to_exp_matrix(args,list_exps,"union",format);
    return random_access_seq_union<true,std::nullptr_t,std::nullptr_t>(callable,list_exps,[](auto& d){return d.exp;});
  }

  /******************************************************************************
  * INTERSECTION
  ******************************************************************************/

  template<bool IS_LIST, typename SEQUENCE_CTOR, typename SEQUENCE_PTR, typename SEQUENCE_ACCESSOR>
  data random_access_seq_intersection(data& callable, data_vector& args, SEQUENCE_ACCESSOR sequence_accessor, 
                                         SEQUENCE_PTR seq_ptr = nullptr, SEQUENCE_CTOR make_heist_sequence = nullptr) {
    auto first_seq = sequence_accessor(args[0]);
    decltype(first_seq) seq;
    for(auto& elt : first_seq) {
      for(size_type i = 1, n = args.size(); i < n; ++i)
        if(!found_in_seq(callable,elt,sequence_accessor(args[i]))) 
          goto next_element;
      seq.push_back(elt);
      next_element: continue;
    }
    if constexpr (IS_LIST) {
      seq_ptr = make_heist_sequence = nullptr; // silences unused variables warnings
      return primitive_toolkit::convert_data_vector_to_proper_list(seq.begin(),seq.end());
    } else {
      return make_heist_sequence(seq);
    }
  }
  

  data vector_intersection(data& callable, data_vector& args, const char* format) {
    confirm_only_given_data_with_type_of_first_arg(args,"intersection",format);
    return random_access_seq_intersection<false,vec_type(*)(const data_vector&)>(callable,args,[](auto& d){return *(d.vec);},&data::vec,make_vec);
  }


  data string_intersection(data& callable, data_vector& args, const char* format) {
    confirm_only_given_data_with_type_of_first_arg(args,"intersection",format);
    return random_access_seq_intersection<false,str_type(*)(const string&)>(callable,args,[](auto& d){return *(d.str);},&data::str,make_str);
  }


  data list_intersection(data& callable, data_vector& args, const char* format) {
    data_vector list_exps;
    convert_lists_to_exp_matrix(args,list_exps,"intersection",format);
    return random_access_seq_intersection<true,std::nullptr_t,std::nullptr_t>(callable,list_exps,[](auto& d){return d.exp;});
  }

  /******************************************************************************
  * SYMMETRIC DIFFERENCE
  ******************************************************************************/

  template<bool IS_LIST, typename SEQUENCE_CTOR, typename SEQUENCE_PTR, typename SEQUENCE_ACCESSOR>
  data random_access_seq_sym_diff(data& callable, data_vector& args, SEQUENCE_ACCESSOR sequence_accessor, 
                                     SEQUENCE_PTR seq_ptr = nullptr, SEQUENCE_CTOR make_heist_sequence = nullptr) {
    decltype(sequence_accessor(args[0])) seq;
    for(size_type i = 0, n = args.size(); i < n; ++i) {
      for(auto& elt : sequence_accessor(args[i])) {
        for(size_type j = 0; j < n; ++j) {
          if(i == j) continue;
          if(found_in_seq(callable,elt,sequence_accessor(args[j]))) 
            goto next_element;
        }
        seq.push_back(elt);
        next_element: continue;
      }
    }
    if constexpr (IS_LIST) {
      seq_ptr = make_heist_sequence = nullptr; // silences unused variables warnings
      return primitive_toolkit::convert_data_vector_to_proper_list(seq.begin(),seq.end());
    } else {
      return make_heist_sequence(seq);
    }
  }
  

  data vector_sym_diff(data& callable, data_vector& args, const char* format) {
    confirm_only_given_data_with_type_of_first_arg(args,"symmetric-difference",format);
    return random_access_seq_sym_diff<false,vec_type(*)(const data_vector&)>(callable,args,[](auto& d){return *(d.vec);},&data::vec,make_vec);
  }


  data string_sym_diff(data& callable, data_vector& args, const char* format) {
    confirm_only_given_data_with_type_of_first_arg(args,"symmetric-difference",format);
    return random_access_seq_sym_diff<false,str_type(*)(const string&)>(callable,args,[](auto& d){return *(d.str);},&data::str,make_str);
  }


  data list_sym_diff(data& callable, data_vector& args, const char* format) {
    data_vector list_exps;
    convert_lists_to_exp_matrix(args,list_exps,"symmetric-difference",format);
    return random_access_seq_sym_diff<true,std::nullptr_t,std::nullptr_t>(callable,list_exps,[](auto& d){return d.exp;});
  }

  /******************************************************************************
  * DIFFERENCE
  ******************************************************************************/

  template<bool IS_LIST, typename SEQUENCE_CTOR, typename SEQUENCE_PTR, typename SEQUENCE_ACCESSOR>
  data random_access_seq_diff(data& callable, data_vector& args, SEQUENCE_ACCESSOR sequence_accessor, 
                                 SEQUENCE_PTR seq_ptr = nullptr, SEQUENCE_CTOR make_heist_sequence = nullptr) {
    decltype(sequence_accessor(args[0])) seq;
    const auto n = args.size();
    for(auto& elt : sequence_accessor(args[0])) {
      for(size_type j = 1; j < n; ++j) {
        if(found_in_seq(callable,elt,sequence_accessor(args[j]))) 
          goto next_element;
      }
      seq.push_back(elt);
      next_element: continue;
    }
    if constexpr (IS_LIST) {
      seq_ptr = make_heist_sequence = nullptr; // silences unused variables warnings
      return primitive_toolkit::convert_data_vector_to_proper_list(seq.begin(),seq.end());
    } else {
      return make_heist_sequence(seq);
    }
  }
  

  data vector_diff(data& callable, data_vector& args, const char* format) {
    confirm_only_given_data_with_type_of_first_arg(args,"difference",format);
    return random_access_seq_diff<false,vec_type(*)(const data_vector&)>(callable,args,[](auto& d){return *(d.vec);},&data::vec,make_vec);
  }


  data string_diff(data& callable, data_vector& args, const char* format) {
    confirm_only_given_data_with_type_of_first_arg(args,"difference",format);
    return random_access_seq_diff<false,str_type(*)(const string&)>(callable,args,[](auto& d){return *(d.str);},&data::str,make_str);
  }


  data list_diff(data& callable, data_vector& args, const char* format) {
    data_vector list_exps;
    convert_lists_to_exp_matrix(args,list_exps,"difference",format);
    return random_access_seq_diff<true,std::nullptr_t,std::nullptr_t>(callable,list_exps,[](auto& d){return d.exp;});
  }

  /******************************************************************************
  * SORT
  ******************************************************************************/

  void confirm_sortable_sequence(data_vector& args, const char* name, const char* format){
    if(args.size() != 2)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args!"<<format<<HEIST_FCN_ERR(name,args));
    primitive_toolkit::confirm_data_is_callable(args[0], args, name, format);
    is_proper_sequence(args[1],args,name,format);
  }


  // Convert the given AST sequence to a pair or vector
  data cast_ast_sequence_to_scheme(const types seq_type, data_vector& sequence, 
                                   const char* name,      const char* format,
                                                          const data_vector& args){
    if(seq_type == types::vec) return data(make_vec(sequence));
    if(seq_type == types::par) return primitive_toolkit::convert_data_vector_to_proper_list(sequence.begin(),sequence.end());
    return convert_data_vector_to_string(sequence,args,name,format);
  }


  // Convert the given pair or vector to an AST sequence
  void cast_scheme_sequence_to_ast(data& scm_sequence,data_vector& sequence)noexcept{
    if(scm_sequence.type == types::vec) {
      sequence = *scm_sequence.vec;
    } else if(scm_sequence.type == types::par) {
      sequence = primitive_toolkit::convert_proper_list_to_data_vector(scm_sequence);
    } else {
      for(const auto& ch : *scm_sequence.str)
        sequence.push_back(chr_type(ch));
    }
  }


  // Sort the args[1] vector or list sequence using the args[0] procedure
  data sort_sequence(data_vector& args, const char* name, const char* format){
    data_vector sequence;
    const types seq_type = args[1].type;
    cast_scheme_sequence_to_ast(args[1],sequence);
    // sort unpacked sequence
    if(sequence.size() > 1) {
      std::sort(sequence.begin(), sequence.end(),
        [procedure=primitive_toolkit::convert_callable_to_procedure(args[0])]
        (data& lhs, data& rhs) mutable {
          data_vector args_list(2);
          args_list[0] = lhs, args_list[1] = rhs;
          return is_true_scm_condition(procedure,std::move(args_list));
        });
    }
    // return the sorted sequence
    return cast_ast_sequence_to_scheme(seq_type,sequence,name,format,args);
  }

  /******************************************************************************
  * MERGE
  ******************************************************************************/

  // -- RANDOM ACCESS SEQUENCES: STRINGS & VECTORS
  data random_access_seq_merge(data_vector& args, data_vector& merged, const char* format){
    auto procedure(primitive_toolkit::convert_callable_to_procedure(args[0]));
    data_vector sequence1, sequence2;
    cast_scheme_sequence_to_ast(args[1],sequence1);
    cast_scheme_sequence_to_ast(args[2],sequence2);
    const size_type n1 = sequence1.size(), n2 = sequence2.size();
    size_type i = 0, j = 0;
    // Merge sequences
    for(; i < n1 && j < n2;) {
      data_vector eq_args(2);
      eq_args[0] = sequence1[i], eq_args[1] = sequence2[j];
      if(is_true_scm_condition(procedure,std::move(eq_args)))
        merged.push_back(sequence1[i]), ++i;
      else
        merged.push_back(sequence2[j]), ++j;
    }
    // If fully iterated through 1 sequence, append elts of the non-empty sequence
    if(i != n1)
      merged.insert(merged.end(),sequence1.begin()+i,sequence1.end());
    else if(j != n2)
      merged.insert(merged.end(),sequence2.begin()+j,sequence2.end());
    return cast_ast_sequence_to_scheme(args[1].type,merged,"merge",format,args);
  }


  // -- LISTS
  void list_merge(data_vector& curr_pairs, data& proc, data_vector& merged_list){
    // If fully iterated both lists, return
    if(!curr_pairs[0].is_type(types::par) && !curr_pairs[1].is_type(types::par))return;
    // If fully iterated through 1 list, append all the elts of the non-empty list & return
    if(!curr_pairs[0].is_type(types::par) || !curr_pairs[1].is_type(types::par)) {
      auto non_empty_list = !curr_pairs[0].is_type(types::par) ? curr_pairs[1] : curr_pairs[0];
      while(non_empty_list.is_type(types::par)) {
        merged_list.push_back(non_empty_list.par->first);
        non_empty_list = non_empty_list.par->second;
      }
      return;
    }
    // Test proc, merge appropriate arg, & recurse down the rest of the lists
    data_vector args(2);
    args[0] = curr_pairs[0].par->first;
    args[1] = curr_pairs[1].par->first;
    if(is_true_scm_condition(proc,std::move(args))) {
      merged_list.push_back(curr_pairs[0].par->first);
      curr_pairs[0] = curr_pairs[0].par->second;
    } else {
      merged_list.push_back(curr_pairs[1].par->first);
      curr_pairs[1] = curr_pairs[1].par->second;
    }
    list_merge(curr_pairs, proc, merged_list);
  }

  /******************************************************************************
  * SORT! & DELETE-NEIGHBOR-DUPS!
  ******************************************************************************/

  // Mutates 'sequence_target' by assigning its value to 'sequence_source'
  data mutatable_assign_scm_sequence(data& sequence_target, data&& sequence_source)noexcept{
    if(sequence_target.is_type(types::vec))
      *sequence_target.vec = *sequence_source.vec;
    else if(sequence_target.is_type(types::par))
      *sequence_target.par = *sequence_source.par;
    else
      *sequence_target.str = *sequence_source.str;
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * DELETE-NEIGHBOR-DUPS & DELETE-NEIGHBOR-DUPS!
  ******************************************************************************/

  // primitive "delete-neighbor-dups" & "delete-neighbor-dups!" helper template
  template<bool MUTATING_DELETION>
  data generic_DELETE_NEIGHBOR_DUPS(data_vector& args, const char* name, const char* format){
    // confirm has a valid argument signature
    confirm_sortable_sequence(args,name,format);
    // return if deleting duplicates from the empty list
    if(args[1].is_type(types::sym)) {
      if constexpr (MUTATING_DELETION) {
        return GLOBALS::VOID_DATA_OBJECT;
      } else {
        return args[1];
      }
    }
    // unpack sequence
    data_vector sequence;
    const auto seq_type = args[1].type;
    cast_scheme_sequence_to_ast(args[1],sequence);
    // return if deleting duplicates from an empty sequence
    if(sequence.empty()) {
      if constexpr (MUTATING_DELETION) {
        return GLOBALS::VOID_DATA_OBJECT;
      } else {
        return cast_ast_sequence_to_scheme(seq_type,sequence,name,format,args);
      }
    }
    // rm duplicates from the sequence
    data_vector new_sequence(1,sequence[0]);
    auto procedure = primitive_toolkit::convert_callable_to_procedure(args[0]);
    for(size_type i=1, j=0, n = sequence.size(); i < n; ++i) {
      data_vector args_list(2);
      args_list[0] = new_sequence[j], args_list[1] = sequence[i];
      if(is_false_scm_condition(procedure,std::move(args_list)))
        new_sequence.push_back(sequence[i]), ++j;
    }
    if constexpr (MUTATING_DELETION) {
      return mutatable_assign_scm_sequence(args[1],cast_ast_sequence_to_scheme(seq_type,new_sequence,name,format,args));
    } else {
      return cast_ast_sequence_to_scheme(seq_type,new_sequence,name,format,args);
    }
  }

} // End of namespace heist::stdlib_seqs

#endif