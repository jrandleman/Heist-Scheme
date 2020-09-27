// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_primitives_toolkit.hpp
// => Defines helper functions for the Heist Scheme Interpreter's C++ primitives

#ifndef HEIST_PRIMITIVES_TOOLKIT_HPP_
#define HEIST_PRIMITIVES_TOOLKIT_HPP_

/******************************************************************************
* PRIMITIVE HELPER ENUMERATIONS & FUNCTION PROTOTYPES
******************************************************************************/

// VALID SEQUENCE INDEX RANGE (undef in heist_primitives.hpp)
#define VALID_SEQUENCE_INDEX_RANGE\
  "\n     <index> range: [0," << G::MAX_SIZE_TYPE << ']'

namespace heist {

  //         -- HELPER STATUS CODE ENUMERATIONS
  enum class list_status {ok, cyclic, no_null};
  enum class heist_sequence {lis, nul, vec, str};


  //       -- FROM THE EVALUATOR
  bool     is_true(const scm_list& exp)noexcept;
  bool     is_non_escaped_double_quote(size_type i, const scm_string& input)noexcept;
  bool     prepare_string_for_AST_generation(scm_string& input);
  bool     data_is_continuation_parameter(const data& d)noexcept;
  bool     expand_macro_if_in_env(const sym_type&,const scm_list&,env_type&,scm_list&);
  void     parse_input_exp(scm_string&& input, scm_list& abstract_syntax_tree);
  void     set_default_global_environment();
  data     data_cast(const scm_list& l)noexcept;
  scm_list scm_list_cast(const data& d)noexcept;
  scm_list generate_fundamental_form_cps(const data& code,const bool topmost_call=true);
  scm_list make_delay(const scm_list& exp, env_type& env)noexcept;
  scm_list read_user_input(FILE* outs,FILE* ins,const bool& in_repl=true);
  scm_list scm_eval(scm_list&& exp, env_type& env);
  exe_type scm_analyze(scm_list&& exp,const bool tail_call=false,const bool cps_block=false);
  scm_list execute_application(scm_list&,scm_list&,env_type&,const bool tail_call=false,const bool inlined=false);
  scm_list execute_application(scm_list&&,scm_list&,env_type&,const bool tail_call=false,const bool inlined=false);
  size_type is_expandable_reader_macro(const scm_string&, const size_type)noexcept;
  constexpr bool IS_END_OF_WORD(const char& c, const char& c2)noexcept;
  std::pair<chr_type,scm_string> data_is_named_char(const size_type&,const scm_string&)noexcept;


  //          -- FROM PRIMITIVES & ITS TOOLKIT
  bool        prm_compare_PAIRs(const par_type& p1, const par_type& p2)noexcept;
  bool        prm_compare_VECTs(const vec_type& v1, const vec_type& v2)noexcept;
  bool        prm_compare_EXPRs(const scm_list& l1, const scm_list& l2)noexcept;
  bool        prm_compare_SNTXs(const syn_type& s1, const syn_type& s2)noexcept;
  bool        data_is_the_empty_expression(const data& d)noexcept;
  void        shallow_unpack_list_into_exp(data& curr_pair, scm_list& args_list)noexcept;
  void        primitive_UNFOLD_template(scm_list&,scm_list&,const char*,const char* format);
  scm_string  escape_chars(const scm_string& str)noexcept;
  scm_string  unescape_chars(const scm_string& str)noexcept;
  num_type    primitive_guarenteed_list_length(const data& d)noexcept;
  list_status primitive_list_is_acyclic_and_null_terminated(const data& curr_pair)noexcept;
  scm_list    primitive_read_from_port(FILE* outs, FILE* ins);
  constexpr bool IS_OPEN_PAREN(const char& c) noexcept;
  constexpr bool IS_CLOSE_PAREN(const char& c) noexcept;

  /******************************************************************************
  * INTERNAL PRIMITIVE CONSTANT VALUES
  ******************************************************************************/

  namespace G {
    const auto FALSE_DATA_BOOLEAN    = data(boolean(false));
    const auto TRUE_DATA_BOOLEAN     = data(boolean(true));
    const auto VOID_DATA_OBJECT      = data(types::dne);
    const auto VOID_DATA_EXPRESSION  = scm_list(1,VOID_DATA_OBJECT);
    const auto EMPTY_LIST_EXPRESSION = scm_list(1,symconst::emptylist);
  } // End of namespace G

  /******************************************************************************
  * PRIMITIVE ARGUMENT ANALYSIS HELPERS
  ******************************************************************************/

  void confirm_given_one_arg(const scm_list& args, const char* name, 
                             const char* arg_name = "<obj>"){
    if(args.size() != 1)
      THROW_ERR('\''<<name<<" received incorrect # of arguments:"
        "\n     ("<<name<<' '<<arg_name<<')'<<FCN_ERR(name,args));
  }


  // Confirm args only consists of datum w/ type 't'
  // POSTCONDITION: returns i >= 0 for i'th index of non-type-t data,
  //                else returns G::MAX_SIZE_TYPE if all data is of type t
  size_type confirm_only_args_of_type(const scm_list& args, types t)noexcept{
    for(size_type i = 0, n = args.size(); i < n; ++i)
      if(!args[i].is_type(t)) return i;
    return G::MAX_SIZE_TYPE;
  }

  /******************************************************************************
  * GENERAL PRIMITIVE HELPERS
  ******************************************************************************/

  bool primitive_data_is_a_procedure(const data& d)noexcept{
    return d.is_type(types::exp) && 
      d.exp[0].is_type(types::sym) &&
      (d.exp[0].sym == symconst::procedure || 
       d.exp[0].sym == symconst::primitive);
  }


  void primitive_confirm_data_is_a_procedure(const data& d,      const char* name, 
                                             const char* format, const scm_list& args){
    if(!primitive_data_is_a_procedure(d))
      THROW_ERR('\'' << name << " arg " << PROFILE(d) 
        << " isn't a procedure!" << format << FCN_ERR(name,args));
  }


  bool data_is_proper_list(const data& d)noexcept{
    return data_is_the_empty_expression(d) || // data is the empty list
           (d.is_type(types::par) &&          // OR a finite & '()-terminated pair sequence
            (primitive_list_is_acyclic_and_null_terminated(d) == list_status::ok));
  }


  bool primitive_is_valid_index(const data& d)noexcept{
    return d.is_type(types::num) && d.num.is_integer() &&
           !d.num.is_neg() && d.num <= G::MAX_SIZE_TYPE;
  }


  bool primitive_is_valid_size(const data& d)noexcept{
    return d.is_type(types::num) && d.num.is_integer() &&
           d.num.is_pos() && d.num <= G::MAX_SIZE_TYPE;
  }


  // (apply procedure args) == true
  bool is_true_scm_condition(scm_list& procedure,scm_list& args,env_type& env){
    return is_true(execute_application(procedure,args,env));
  }


  // (apply procedure args) == false
  bool is_false_scm_condition(scm_list& procedure,scm_list& args,env_type& env){
    return !is_true(execute_application(procedure,args,env));
  }


  // Proper sequence =  string || vector || proper-list
  heist_sequence is_proper_sequence(const data& d,    const scm_list& args,
                                    const char* name, const char* format){
    if(d.is_type(types::vec))           return heist_sequence::vec;
    if(d.is_type(types::str))           return heist_sequence::str;
    if(data_is_the_empty_expression(d)) return heist_sequence::nul;
    if(d.is_type(types::par) && 
      primitive_list_is_acyclic_and_null_terminated(d) == list_status::ok)
      return heist_sequence::lis;
    THROW_ERR('\''<<name<<" given arg "<<PROFILE(d)<<" isn't a proper sequence!" 
        << format << FCN_ERR(name,args)); // throws
    return heist_sequence::nul; // never used, only to satisfy primitive type sig.
  }


  // Confirm given an empty <sequence> (string, list, or vector)
  bool data_is_empty(const data& d)noexcept{
    return data_is_the_empty_expression(d) || 
      (d.is_type(types::str) && d.str->empty()) || 
      (d.is_type(types::vec) && d.vec->empty());
  }

  /******************************************************************************
  * NUMERIC PRIMITIVE HELPERS
  ******************************************************************************/

  // Confirm given >= 1 arg & ONLY numeric args
  void confirm_no_numeric_primitive_errors(const scm_list& args, 
                                           const char* primitive_name, 
                                           const char* format) {
    if(args.empty())
      THROW_ERR('\''<<primitive_name<<" received no arguments!\n     "
        << format << FCN_ERR(primitive_name,args));
    if(auto idx = confirm_only_args_of_type(args,types::num); idx != G::MAX_SIZE_TYPE)
      THROW_ERR('\''<<primitive_name<<" received non-numeric argument: "
        <<PROFILE(args[idx])<<"!\n     "<<format<<FCN_ERR(primitive_name,args));
  }

  // Confirm given >= 1 arg & ONLY real numeric args
  void confirm_no_real_numeric_primitive_errors(const scm_list& args, 
                                                const char* primitive_name, 
                                                const char* format) {
    if(args.empty())
      THROW_ERR('\''<<primitive_name<<" received no arguments!\n     "
        << format << FCN_ERR(primitive_name,args));
    for(const auto& arg : args)
      if(!arg.is_type(types::num) || !arg.num.is_real())
        THROW_ERR('\''<<primitive_name<<" received non-real-numeric argument: "
          <<PROFILE(arg)<<"!\n     "<<format<<FCN_ERR(primitive_name,args));
  }


  void confirm_2_args(const scm_list& args, const char* primitive_name, const char* format){
    if(args.size() != 2)
      THROW_ERR('\'' << primitive_name << " didn't receive two arguments (given "
        << args.size() << ")!\n     " << format << FCN_ERR(primitive_name,args));
  }


  // Confirm given 1 numeric arg
  void confirm_unary_numeric(const scm_list& args, const char* primitive_name, 
                                                   const char* format){
    if(args.size() != 1) 
      THROW_ERR('\'' << primitive_name << " didn't receive 1 argument (given "
        <<args.size()<<")!\n     "<<format<<FCN_ERR(primitive_name,args));
    if(!args[0].is_type(types::num))
      THROW_ERR('\''<<primitive_name<<" received non-numeric argument: "
        <<PROFILE(args[0])<<"!\n     "<<format<<FCN_ERR(primitive_name,args));
  }

  // Confirm given 1 numeric arg
  void confirm_unary_real_numeric(const scm_list& args, const char* primitive_name, 
                                                   const char* format){
    if(args.size() != 1) 
      THROW_ERR('\'' << primitive_name << " didn't receive 1 argument (given "
        <<args.size()<<")!\n     "<<format<<FCN_ERR(primitive_name,args));
    if(!args[0].is_type(types::num) || !args[0].num.is_real())
      THROW_ERR('\''<<primitive_name<<" received non-real-numeric argument: "
        <<PROFILE(args[0])<<"!\n     "<<format<<FCN_ERR(primitive_name,args));
  }

  /******************************************************************************
  * EQUALITY PRIMITIVE HELPERS
  ******************************************************************************/

  bool prm_comparing_primitives(const scm_list& l1, const scm_list& l2)noexcept{
    return l1.size() == 3 && l1[0].is_type(types::sym) && l2[0].is_type(types::sym) &&
      l1[0].sym == l2[0].sym && l1[0].sym == symconst::primitive;
  }


  bool prm_compare_atomic_values(const data& v1,const data& v2,const types& t)noexcept{
    switch(t) {
      case types::undefined: case types::dne: case types::exe: return true;
      case types::num: return v1.num == v2.num && v1.num.is_exact() == v2.num.is_exact();
      case types::chr: return v1.chr == v2.chr;
      case types::str: return *v1.str == *v2.str;
      case types::sym: return v1.sym == v2.sym;
      case types::bol: return v1.bol.val == v2.bol.val;
      case types::par: return v1.par == v2.par;
      case types::vec: return v1.vec == v2.vec;
      case types::prm: return v1.prm == v2.prm;
      case types::del: return v1.del == v2.del;
      case types::env: return v1.env == v2.env;
      case types::cal: return v1.cal == v2.cal;
      case types::fip: return v1.fip.port_idx == v2.fip.port_idx;
      case types::fop: return v1.fop.port_idx == v2.fop.port_idx;
      case types::syn: return prm_compare_SNTXs(v1.syn, v2.syn);
      case types::exp: return prm_compare_EXPRs(v1.exp, v2.exp);
      default:         return false;
    }
  }


  bool prm_compare_EXPRs(const scm_list& l1, const scm_list& l2)noexcept{
    if(l1.size() != l2.size())          return false;
    if(prm_comparing_primitives(l1,l2)) return l1[1].prm == l2[1].prm;
    for(size_type i = 0, n = l1.size(); i < n; ++i) {
      if(l1[i].type != l2[i].type) return false; // compare types
      if(l1[i].is_type(types::exp)) {            // compare sub-lists
        if(!prm_compare_EXPRs(l1[i].exp,l2[i].exp))
          return false;
      } else if(l1[i].is_type(types::par)) {
        if(!prm_compare_PAIRs(l1[i].par,l2[i].par))
          return false;
      } else if(l1[i].is_type(types::vec)) {
        if(!prm_compare_VECTs(l1[i].vec,l2[i].vec))
          return false;
      } else if(!prm_compare_atomic_values(l1[i],l2[i],l1[i].type))
        return false;                            // compare values
    }
    return true;
  }


  bool prm_compare_PAIRs(const par_type& p1, const par_type& p2)noexcept{
    if(!p1 || !p2 || p1->first.type != p2->first.type || p1->second.type != p2->second.type)
      return false;
    auto& p1_car = p1->first, &p2_car = p2->first;
    auto& p1_cdr = p1->second, &p2_cdr = p2->second;
    // Compare cars
    if(p1_car.is_type(types::exp)) {
      if(!prm_compare_EXPRs(p1_car.exp, p2_car.exp)) return false;
    } else if(p1_car.is_type(types::par)) { 
      if(!prm_compare_PAIRs(p1_car.par, p2_car.par)) return false;
    } else if(p1_car.is_type(types::vec)) {
      if(!prm_compare_VECTs(p1_car.vec, p2_car.vec)) return false;
    } else if(!prm_compare_atomic_values(p1_car, p2_car, p1_car.type)) return false;
    // Compare cdrs
    if(p1_cdr.is_type(types::exp)) {
      if(!prm_compare_EXPRs(p1_cdr.exp, p2_cdr.exp)) return false;
    } else if(p1_cdr.is_type(types::par)) {
      if(!prm_compare_PAIRs(p1_cdr.par, p2_cdr.par)) return false;
    } else if(p1_cdr.is_type(types::vec)) {
      if(!prm_compare_VECTs(p1_cdr.vec, p2_cdr.vec)) return false;
    } else if(!prm_compare_atomic_values(p1_cdr, p2_cdr, p1_cdr.type)) return false;
    return true;
  }


  bool prm_compare_VECTs(const vec_type& v1, const vec_type& v2)noexcept{
    return prm_compare_EXPRs(*v1, *v2);
  }


  bool prm_compare_SNTXs(const syn_type& s1, const syn_type& s2)noexcept{
    frame_var label;
    frame_vars keywords;
    std::vector<scm_list> patterns;
    std::vector<scm_list> templates;
    if(s1.label != s2.label || s1.patterns.size()  != s2.patterns.size()
                            || s1.keywords.size()  != s2.keywords.size()
                            || s1.templates.size() != s2.templates.size()){
      return false;
    }
    for(size_type i = 0, n = s1.keywords.size(); i < n; ++i)
      if(s1.keywords[i] != s2.keywords[i]) return false;
    for(size_type i = 0, n = s1.patterns.size(); i < n; ++i)
      if(!prm_compare_EXPRs(s1.patterns[i], s2.patterns[i])) return false;
    for(size_type i = 0, n = s1.templates.size(); i < n; ++i)
      if(!prm_compare_EXPRs(s1.templates[i], s2.templates[i])) return false;
    return true;
  }

  /******************************************************************************
  * CHARACTER PRIMITIVE HELPERS
  ******************************************************************************/

  // Confirm given N character/string args
  void confirm_given_char_string_args(const scm_list& args, const char* name, 
                                                            const types& type){
    const char* type_name = (type == types::str ? "string" : "char");
    if(args.empty())
      THROW_ERR('\''<<name<<" didn't received any args:\n     ("<<name
        <<" <"<<type_name<<"1> <"<<type_name<<"2> ...)"<<FCN_ERR(name,args));
    for(const auto& arg : args)
      if(!arg.is_type(type))
        THROW_ERR('\''<<name<<" received non-"<<type_name<<" arg "<<PROFILE(arg)
          <<":\n     ("<<name<<" <"<<type_name<<"1> <"<<type_name<<"2> ...)"<<FCN_ERR(name,args));
  }


  // Confirm given 1 character arg
  void confirm_given_one_char_arg(const scm_list& args, const char* name) {
    confirm_given_one_arg(args,name,"<char>");
    if(!args[0].is_type(types::chr))
      THROW_ERR('\''<<name<<" didn't receive a character arg:\n     ("<<name<<" <char>)"
        "\n     received " << PROFILE(args[0]) << '!' <<FCN_ERR(name,args));
  }

  /******************************************************************************
  * STATIC SEQUENCE (STRING & VECTOR) PRIMITIVE HELPERS
  ******************************************************************************/

  void primitive_confirm_valid_str_or_vec_arg(const scm_list& args, const size_type& total_args,
                                              const char* name,     const char* format, 
                                              const char* seq_name, const types& seq_type){
    if(args.empty())
      THROW_ERR('\''<<name<<" didn't receive any arguments!"<<format<<FCN_ERR(name,args));
    if(args.size() != total_args)
      THROW_ERR('\''<<name<<" didn't receive "<<total_args<<" argument"<<(total_args>1?"s":"")
        <<" (given "<<args.size()<<")!"<<format<<FCN_ERR(name,args));
    if(!args[0].is_type(seq_type))
      THROW_ERR('\''<<name<<" didn't receive a "<<seq_name<<" arg!"
        <<"\n     Variable "<<PROFILE(args[0])<<" isn't a "<<seq_name
        <<'!'<<format<<FCN_ERR(name,args));
  }


  data mk_string_from_generated_chrs(const scm_list& sequence, const scm_list& args,
                                     const char* name,         const char* format){
    if(sequence.empty()) return make_str("");
    if(auto i = confirm_only_args_of_type(sequence, types::chr); i != G::MAX_SIZE_TYPE)
      THROW_ERR('\''<<name<<" generated value #" << i+1 << ' ' << PROFILE(sequence[i]) 
        << " isn't\n     a character:" << format << "\n     Generated values: " 
        << sequence << FCN_ERR(name,args));
    scm_string str_val;
    for(const auto& ch : sequence) str_val += ch.chr;
    return make_str(str_val);
  }


  // Return 'size_type' index (if given string/vector index is valid)
  template<typename SEQUENCE_PTR>
  size_type primitive_get_if_valid_str_or_vec_idx(const scm_list& args,     const char* name, 
                                                  const char* format,       const char* seq_name, 
                                                  const size_type& idx_pos, const size_type& sequence_pos, 
                                                  SEQUENCE_PTR seq_ptr){
    // confirm given an in-'size_type'-range non-negative index
    if(!primitive_is_valid_index(args[idx_pos]))
      THROW_ERR('\''<<name<<" index "<<args[idx_pos]<<" isn't a proper non-negative integer!"
        << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR(name,args));
    // confirm index falls w/in range of the sequence
    const size_type i = (size_type)args[idx_pos].num.extract_inexact();
    const size_type l = (args[sequence_pos].*seq_ptr)->size();
    if(i >= l)
      THROW_ERR('\''<<name<<" received out of range index " << i 
        <<"\n     for "<<seq_name<<' '<<args[sequence_pos]<<" of size "
        <<l<<'!'<<format<<FCN_ERR(name,args));
    return i;
  }


  // Confirm given a proper list to be coerced & return list.empty()
  bool primitive_validate_list_and_return_if_empty(scm_list& args, const char* name){
    confirm_given_one_arg(args,name,"<list>");
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


  template<typename SEQUENCE_PTR>
  void primitive_FOLD_sequence_accumulator(scm_list& sequences,     scm_list& proc, 
                                           data& init_val,          env_type& env, 
                                           const bool& folding_left,SEQUENCE_PTR seq_ptr) {
    if((sequences[0].*seq_ptr)->empty()) return;
    if(folding_left) {
      // for each ith element
      for(size_type i = 0, n = (sequences[0].*seq_ptr)->size(); i < n; ++i){
        // in each sequence
        scm_list args;
        for(auto& sequence : sequences)
          args.push_back((sequence.*seq_ptr)->operator[](i)); // extract the ith elements
        args.insert(args.begin(), init_val);                        // and accumulate the elements
        init_val = data_cast(execute_application(proc,args,env));
      }
    } else {
      // for each ith element (in reverse)
      for(size_type i = (sequences[0].*seq_ptr)->size(); i-- > 0;){
        // in each sequence
        scm_list args;
        for(auto& sequence : sequences)
          args.push_back((sequence.*seq_ptr)->operator[](i)); // extract the ith elements
        args.insert(args.end(), init_val);                          // and accumulate the elements
        init_val = data_cast(execute_application(proc,args,env));
      }
    }
  }


  template<typename SEQUENCE_PTR>
  void primitive_confirm_same_sized_sequences(const scm_list& sequences, 
                                              const char* name,     const char* format,
                                              const scm_list& args, const types& seq_type, 
                                              const char* seq_name, SEQUENCE_PTR seq_ptr){
    size_type length0;
    for(size_type i = 0, n = sequences.size(); i < n; ++i) {
      if(!sequences[i].is_type(seq_type))
        THROW_ERR('\''<<name<<" arg #" << i+3 << ' ' // sequences begin at idx 2
          << PROFILE(sequences[i]) << " isn't a "<<seq_name<<':'
          << format << FCN_ERR(name,args));
      if(i == 0) length0 = (sequences[0].*seq_ptr)->size();
      else if(length0 != (sequences[i].*seq_ptr)->size())
        THROW_ERR('\''<<name<<' '<<seq_name<<"s "<< sequences[0] << " and " 
          << sequences[i] << " differ in length!" << format << FCN_ERR(name,args));
    }
  }


  // primitive "fold" & "fold-right" procedure helper for strings & vectors
  template<typename SEQUENCE_PTR>
  data primitive_STATIC_SEQUENCE_FOLD_template(scm_list& args,       const char* name, 
                                               const char* format,   const bool& folding_left,
                                               const types& seq_type,const char* seq_name, 
                                               SEQUENCE_PTR seq_ptr, env_type& env){
    scm_list sequences(args.begin()+2, args.end());
    primitive_confirm_same_sized_sequences(sequences,name,format,args,seq_type,
                                                          seq_name,seq_ptr);
    // Apply the procedure on each elt of each sequence, & accumulate the result
    data init_val = args[1];
    primitive_FOLD_sequence_accumulator(sequences,args[0].exp,init_val,
                                                env,folding_left,seq_ptr);
    return init_val; // return the accumulated value
  }


  // "mpa" construction helper for strings/vectors
  template<typename SEQUENCE_PTR>
  void primitive_MAP_sequence_constructor(scm_list& sequences,       scm_list& proc, 
                                          scm_list& mapped_sequence, env_type& env,
                                          SEQUENCE_PTR seq_ptr){
    const size_type total_sequences = sequences.size();
    const size_type total_elts = (sequences[0].*seq_ptr)->size();
    // Traverse each elt
    for(size_type i = 0; i < total_elts; ++i) {
      scm_list args(total_sequences);
      // Aquire ith elt from each sequence
      for(size_type j = 0; j < total_sequences; ++j)
        args[j] = (sequences[j].*seq_ptr)->operator[](i);
      // Push the mapped result of each elt
      mapped_sequence.push_back(data_cast(execute_application(proc,args,env)));
    }
  }


  template<typename SEQUENCE_PTR>
  data primitive_STATIC_SEQUENCE_MAP_template(scm_list& args,      const char* name, 
                                              const char* format,  const types& seq_type, 
                                              const char* seq_name,SEQUENCE_PTR seq_ptr,
                                                                   env_type& env){
    scm_list sequences(args.begin()+1, args.end());
    primitive_confirm_same_sized_sequences(sequences,name,format,args,seq_type,
                                                          seq_name,seq_ptr);
    // Apply the procedure on each elt of each sequence & store the result
    scm_list mapped_sequence;
    primitive_MAP_sequence_constructor(sequences,args[0].exp,
                                       mapped_sequence,env,seq_ptr);
    if(seq_type == types::str)
      return mk_string_from_generated_chrs(mapped_sequence,args,name,format);
    return make_vec(mapped_sequence);
  }


  // "for-each" application helper for strings/vectors
  template<typename SEQUENCE_PTR>
  void primitive_FOR_EACH_sequence_applicator(scm_list& sequences, scm_list& proc, 
                                              env_type& env, SEQUENCE_PTR seq_ptr){
    const size_type total_sequences = sequences.size();
    const size_type total_elts = (sequences[0].*seq_ptr)->size();
    // Traverse each elt
    for(size_type i = 0; i < total_elts; ++i) {
      scm_list args(total_sequences);
      // Aquire ith elt from each sequence
      for(size_type j = 0; j < total_sequences; ++j)
        args[j] = (sequences[j].*seq_ptr)->operator[](i);
      // Apply the procedure to each elt
      execute_application(proc,args,env);
    }
  }


  template<typename SEQUENCE_PTR>
  data primitive_STATIC_SEQUENCE_FOR_EACH_template(scm_list& args,       const char* name, 
                                                   const char* format,   const types& seq_type, 
                                                   const char* seq_name, SEQUENCE_PTR seq_ptr,
                                                                         env_type& env){
    scm_list sequences(args.begin()+1, args.end());
    primitive_confirm_same_sized_sequences(sequences,name,format,args,seq_type,
                                                          seq_name,seq_ptr);
    // Apply the procedure on each elt of each sequence & store the result
    primitive_FOR_EACH_sequence_applicator(sequences, args[0].exp, env, seq_ptr);
    return G::VOID_DATA_OBJECT;
  }


  template<typename SEQUENCE_PTR>
  data primitive_STATIC_SEQUENCE_COUNT_template(scm_list& args, SEQUENCE_PTR seq_ptr, env_type& env){
    size_type count = 0;
    for(auto& d : *(args[1].*seq_ptr)){
      scm_list count_args(1,d);
      count += size_type(is_true_scm_condition(args[0].exp,count_args,env));
    }
    return num_type(count);
  }


  // Helper for string/vector filtration & removal
  template <bool(*truth_proc)(scm_list&,scm_list&,env_type&), typename SEQUENCE_PTR>
  data prm_sequence_selective_iteration_template(scm_list& args, const types& seq_type, 
                                                 SEQUENCE_PTR seq_ptr, env_type& env){
    scm_list pruned_sequence;
    for(auto& d : *(args[1].*seq_ptr)){
      scm_list pruning_args(1,d);
      if(truth_proc(args[0].exp,pruning_args,env)) // true = filter, false = rm
        pruned_sequence.push_back(data(d));
    }
    if(seq_type == types::str) {
      scm_string str;
      for(auto& letter : pruned_sequence)
        str += letter.chr;
      return make_str(str);
    }
    return make_vec(pruned_sequence);
  }


  // Helper for string-copy! & vector-copy!
  template <typename SEQUENCE_PTR>
  data primitive_STATIC_SEQUENCE_COPY_BANG_template(scm_list& args,       const char* name, 
                                                    const char* format,   const types& seq_type, 
                                                    const char* seq_name, SEQUENCE_PTR seq_ptr){
    if(args.size() != 3)
      THROW_ERR('\'' << name << " received incorrect # of arguments:"
        << format << FCN_ERR(name,args));
    if(!args[0].is_type(seq_type))
      THROW_ERR('\'' << name << " 1st arg "<<PROFILE(args[0])<<" isn't a "
        <<seq_name<<':'<<format<<FCN_ERR(name,args));
    size_type idx = primitive_get_if_valid_str_or_vec_idx(args,name,format,seq_name,1,0,seq_ptr);
    if(!args[2].is_type(seq_type))
      THROW_ERR('\'' << name << " 3rd arg "<<PROFILE(args[2])<<" isn't a "
        <<seq_name<<':'<<format<<FCN_ERR(name,args));
    // splice in the source sequence
    const auto source_sequence(*(args[2].*seq_ptr)); // in case copying a seq to itself
    const size_type target_size = (args[0].*seq_ptr)->size();
    const size_type source_size = source_sequence.size();
    if(target_size-idx-1 < source_size){
      (args[0].*seq_ptr)->erase((args[0].*seq_ptr)->begin()+idx, 
                                (args[0].*seq_ptr)->end());
      (args[0].*seq_ptr)->insert((args[0].*seq_ptr)->end(), 
                                 source_sequence.begin(), source_sequence.end());
    } else {
      (args[0].*seq_ptr)->erase((args[0].*seq_ptr)->begin()+idx, 
                                (args[0].*seq_ptr)->begin()+idx+source_size);
      (args[0].*seq_ptr)->insert((args[0].*seq_ptr)->begin()+idx, 
                                 source_sequence.begin(), source_sequence.end());
    }
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * STRING GRAMMARS FOR JOINING
  ******************************************************************************/

  enum class STRING_GRAMMARS {INFIX, SUFFIX, PREFIX};

  /******************************************************************************
  * STRING PRIMITIVE HELPERS
  ******************************************************************************/

  void primitive_confirm_valid_string_arg(const scm_list& args,const size_type& total_args,
                                          const char* name,    const char* layout){
    primitive_confirm_valid_str_or_vec_arg(args,total_args,name,layout,"string",types::str);
  }


  size_type primitive_get_if_valid_string_idx(const scm_list& args,const char* name, 
                                              const char* layout,const size_type& idx_pos=1){
    return primitive_get_if_valid_str_or_vec_idx(args,name,layout,"string",idx_pos,0,&data::str);
  }


  scm_string lowercase_str(const scm_string& s)noexcept{
    scm_string tmp;
    for(const auto& ch : s) tmp += scm_numeric::mklower(ch);
    return tmp;
  }


  void confirm_given_string_and_valid_length(scm_list& args, const char* name, 
                                                             const char* format){
    if(!args[0].is_type(types::str))
      THROW_ERR('\''<<name<<" 1st arg " << PROFILE(args[0]) << " isn't a string:"
        << format << FCN_ERR(name, args));
    if(!primitive_is_valid_index(args[1]))
      THROW_ERR('\''<<name<<" 2nd arg " << PROFILE(args[1]) << " isn't a"
        "\n     proper non-negative integer length!" << format
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR(name, args));
  }


  char confirm_valid_string_pad_args(scm_list& args, const char* name, 
                                                     const char* format){
    if(args.size() != 2 && args.size() != 3)
      THROW_ERR('\''<<name<<" recieved incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR(name, args));
    confirm_given_string_and_valid_length(args, name, format);
    if(args.size() == 3) {
      if(!args[2].is_type(types::chr))
        THROW_ERR('\''<<name<<" 3rd arg " << PROFILE(args[2]) 
          << " isn't a character:" << format << FCN_ERR(name, args));
      return char(args[2].chr);
    }
    return ' ';
  }


  void confirm_valid_string_trim_args(scm_list& args, const char* name, 
                                                      const char* format){
    if(args.empty() || args.size() > 2)
      THROW_ERR('\''<<name<<" recieved incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR(name, args));
    if(!args[0].is_type(types::str))
      THROW_ERR('\''<<name<<" 1st arg " << PROFILE(args[0]) << " isn't a string:"
        << format << FCN_ERR(name, args));
    if(args.size() == 2)
      primitive_confirm_data_is_a_procedure(args[1], name, format, args);  
  }


  data prm_trim_left_of_string(scm_list& args, env_type& env) {
    scm_string str(*args[0].str);
    const size_type n = str.size();
    size_type i = 0;
    if(args.size() == 1) { // no predicate given, trim whitespace
      for(; i < n && isspace(str[i]); ++i);
    } else {
      for(; i < n; ++i) { // while predicate is true, trim character
        scm_list proc_args(1,chr_type(str[i]));
        if(is_false_scm_condition(args[1].exp,proc_args,env))
          break;
      }
    }
    if(i == n) return make_str("");
    return make_str(str.substr(i));
  }


  data prm_trim_right_of_string(scm_list& args, env_type& env) {
    if(args[0].str->empty()) return make_str("");
    scm_string str(*args[0].str);
    const size_type n = str.size();
    size_type i = n-1;
    if(args.size() == 1) { // no predicate given, trim whitespace
      for(; i > 0 && isspace(str[i]); --i);
      if(i == 0 && isspace(str[i])) return make_str("");
    } else {
      scm_list proc_args(1); 
      for(; i > 0; --i) { // while predicate is true, trim character
        proc_args[0] = data(chr_type(str[i]));
        if(is_false_scm_condition(args[1].exp,proc_args,env))
          break;
      }
      proc_args[0] = data(chr_type(str[i]));
      if(i == 0 && is_true_scm_condition(args[1].exp,proc_args,env)){
        return make_str("");
      }
    }
    if(i == n) return make_str("");
    return make_str(str.substr(0,i+1));
  }


  data prm_string_contains_template(scm_list& args,     const char* name, 
                                    const char* format, const bool& from_left){
    if(args.size() != 2) 
      THROW_ERR('\''<<name<<" recieved incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR(name, args));
    if(!args[0].is_type(types::str))
      THROW_ERR('\''<<name<<" 1st arg "<<PROFILE(args[0])<<" isn't a string:" 
        << format << FCN_ERR(name, args));
    if(!args[1].is_type(types::str))
      THROW_ERR('\''<<name<<" 2nd arg "<<PROFILE(args[1])<<" isn't a string:" 
        << format << FCN_ERR(name, args));
    size_type pos;
    if(from_left)
      pos = args[0].str->find(*args[1].str);
    else
      pos = args[0].str->rfind(*args[1].str);
    if(pos == scm_string::npos)
      return G::FALSE_DATA_BOOLEAN;
    return num_type(pos);
  }


  void confirm_given_list_of_strings(scm_list& args, const char* format, 
                                                     scm_list& strings_list){
    // confirm proper list
    if(!args[0].is_type(types::par) && !data_is_the_empty_expression(args[0]))
      THROW_ERR("'string-join 1st arg " << PROFILE(args[0]) 
        << " isn't a proper list:" << format << FCN_ERR("string-join",args));
    else if(auto list_stat = primitive_list_is_acyclic_and_null_terminated(args[0]); 
      list_stat == list_status::cyclic)
      THROW_ERR("'string-join 1st arg " << PROFILE(args[0]) 
        << " isn't an acyclic list:" << format << FCN_ERR("string-join",args));
    else if(list_stat == list_status::no_null)
      THROW_ERR("'string-join 1st arg " << PROFILE(args[0]) 
        << " isn't a '() terminated list:" << format << FCN_ERR("string-join",args));
    // confirm proper list only has strings
    shallow_unpack_list_into_exp(args[0], strings_list);
    for(size_type i = 0, n = strings_list.size(); i < n; ++i)
      if(!strings_list[i].is_type(types::str))
        THROW_ERR("'string-join <string-list> item #"<<i+1<<' '
          << PROFILE(strings_list[i]) << " isn't string:" 
          << format << FCN_ERR("string-join",args));
  }


  void confirm_proper_string_join_args(scm_list& args, STRING_GRAMMARS& grammar, 
                                       scm_string& delimiter, scm_list& strings_list){
    static constexpr const char * const format = 
      "\n     (string-join <string-list> <optional-string-delimiter> <optional-grammar>)"
      "\n     <optional-grammar> = 'infix | 'suffix | 'prefix";
    if(args.empty() || args.size() > 3) // confirm enough args
      THROW_ERR("'string-join recieved incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("string-join", args));
    confirm_given_list_of_strings(args, format, strings_list); // confirm list of strings
    if(args.size() > 1) { // confirm proper delimiter
      if(!args[1].is_type(types::str))
        THROW_ERR("'string-join 2nd arg "<<PROFILE(args[1])<<" isn't a string:" 
          << format << FCN_ERR("string-join", args));
      delimiter = *args[1].str;
    }
    if(args.size() == 3) { // confirm proper grammar
      if(!args[2].is_type(types::sym))
        THROW_ERR("'string-join 3rd arg "<<PROFILE(args[2])<<" isn't a symbol:" 
          << format << FCN_ERR("string-join", args));
      if(args[2].sym == "suffix") grammar = STRING_GRAMMARS::SUFFIX;
      else if(args[2].sym == "prefix") grammar = STRING_GRAMMARS::PREFIX;
      else if(args[2].sym != "infix") {
        THROW_ERR("'string-join 3rd arg "<<PROFILE(args[2])
          <<" wasn't a proper grammar:" 
          << format << FCN_ERR("string-join", args));
      }
    }
  }


  void confirm_proper_string_split_args(scm_list& args, scm_string& delimiter){
    static constexpr const char * const format = 
      "\n     (string-split <string-list> <optional-string-delimiter>)";
    // confirm proper arg signature
    if(args.empty() || args.size() > 2) 
      THROW_ERR("'string-split recieved incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("string-split", args));
    if(!args[0].is_type(types::str))
      THROW_ERR("'string-split 1st arg "<<PROFILE(args[0])<<" isn't a string:"
        << format << FCN_ERR("string-split",args));
    if(args.size() > 1) { // confirm proper delimiter
      if(!args[1].is_type(types::str))
        THROW_ERR("'string-split 2nd arg "<<PROFILE(args[1])<<" isn't a string:" 
          << format << FCN_ERR("string-split", args));
      delimiter = *args[1].str;
    }
  }


  data primitive_STRING_UNFOLD_template(scm_list& args, const bool& unfolding_right, 
                                              const char* name, const char* format){
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,name,format);
    if(unfolded.empty()) return make_str("");
    scm_string str_val;
    for(size_type i = 0, n = unfolded.size(); i < n; ++i) {
      if(!unfolded[i].is_type(types::chr)) 
        THROW_ERR('\''<<name<<" generated value #" << i+1 << ", " << PROFILE(unfolded[i]) 
          << ",\n     isn't a character: " << format << FCN_ERR(name, args));
      str_val += unfolded[i].chr;
    }
    if(unfolding_right)
      return make_str(scm_string(str_val.rbegin(),str_val.rend()));
    return make_str(str_val);
  }

  /******************************************************************************
  * PAIR PRIMITIVE HELPERS
  ******************************************************************************/

  void confirm_given_a_pair_arg(scm_list& args, const char* name){
    confirm_given_one_arg(args,name,"<pair>");
    if(!args[0].is_type(types::par))
      THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<
        " isn't a pair!\n     ("<<name<<" <pair>)"<<FCN_ERR(name,args));
  }


  void confirm_nth_car_is_pair(const data& nth_arg,const char* name,
                               const char* nth,    const scm_list& args){
    if(!nth_arg.par->first.is_type(types::par))
      THROW_ERR('\''<<name<<' '<<nth<<" 'car "<<PROFILE(nth_arg.par->first)
        <<" isn't a pair!"<<FCN_ERR(name,args));
  }


  void confirm_nth_cdr_is_pair(const data& nth_arg,const char* name,
                               const char* nth,    const scm_list& args){
    if(!nth_arg.par->second.is_type(types::par))
      THROW_ERR('\''<<name<<' '<<nth<<" 'cdr "<<PROFILE(nth_arg.par->second)
        <<" isn't a pair!"<<FCN_ERR(name,args));
  }

  /******************************************************************************
  * LIST PRIMITIVE HELPERS
  ******************************************************************************/

  // "list" primitive proper-list construction helper
  template<typename SCM_LIST_ITERATOR>
  data primitive_LIST_to_CONS_constructor(const SCM_LIST_ITERATOR& obj, 
                                          const SCM_LIST_ITERATOR& null_obj)noexcept{
    if(obj == null_obj) return symconst::emptylist;
    data new_pair = data(make_par());
    new_pair.par->first = *obj;
    new_pair.par->second = primitive_LIST_to_CONS_constructor(obj+1,null_obj);
    return new_pair;
  }


  // "list*" primitive dotted-list construction helper
  data primitive_LIST_STAR_to_CONS_constructor(const scm_node& obj, const scm_node& null_obj)noexcept{
    if(obj+1 == null_obj) return *obj;
    data new_pair = data(make_par());
    new_pair.par->first = *obj;
    new_pair.par->second = primitive_LIST_STAR_to_CONS_constructor(obj+1,null_obj);
    return new_pair;
  }


  // "circular-list" primitive construction helper
  data primitive_CIRCULAR_LIST_to_CONS_constructor(const scm_node& obj,       const scm_node& null_obj, 
                                                   const data& head = data(), const bool& past_head=false)noexcept{
    data new_pair = data(make_par());
    new_pair.par->first = *obj;
    if(obj+1 == null_obj) {
      if(past_head) new_pair.par->second = head;
      else          new_pair.par->second = new_pair;
      return new_pair;
    }
    if(!past_head) {
      new_pair.par->second = primitive_CIRCULAR_LIST_to_CONS_constructor(obj+1,null_obj,new_pair,true);
    } else {
      new_pair.par->second = primitive_CIRCULAR_LIST_to_CONS_constructor(obj+1,null_obj,head,true);
    }
    return new_pair;
  }


  // "length" & "length+" primitive helper
  void primitive_list_LENGTH_computation(const data& curr_pair, num_type& exact_count, 
                                                                size_type count = 1)noexcept{
    if(count == G::MAX_SIZE_TYPE) {
      exact_count += count;
      count = 0;
    }
    if(curr_pair.is_type(types::par)) 
      primitive_list_LENGTH_computation(curr_pair.par->second, exact_count, count+1);
    else exact_count += count;
  }


  // "count" primitive helper
  void primitive_LIST_COUNT_computation(const data& curr_pair, num_type& exact_count, 
                                        scm_list& pred, env_type& env, size_type count = 1){
    if(count == G::MAX_SIZE_TYPE) {
      exact_count += count;
      count = 0;
    }
    if(curr_pair.is_type(types::par)) {
      scm_list args(1,curr_pair.par->first);
      count += size_type(is_true_scm_condition(pred,args,env)); // if(pred(elt)) ++count
      primitive_LIST_COUNT_computation(curr_pair.par->second,exact_count,pred,env,count+1);
    }
    else exact_count += count;
  }


  // returns whether 'curr_pair' is the end of a valid "list" sequence
  bool primitive_IS_THE_EMPTY_LIST(const data& curr_pair)noexcept{
    return curr_pair.is_type(types::sym) && 
            (curr_pair.sym == symconst::emptylist || 
             curr_pair.sym == symconst::sentinel_arg);
  }


  // "list?" & "alist?" helper. Uses the 1st half of the Floyd Loop Detection 
  //   Algorithm (doesn't need to find WHERE the cycle is).
  list_status primitive_list_is_acyclic_and_null_terminated(const data& curr_pair)noexcept{
    data slow = curr_pair, fast = curr_pair;
    while(fast.is_type(types::par) && fast.par->second.is_type(types::par)) {
      slow = slow.par->second;             // move 1 node/iteration
      fast = fast.par->second.par->second; // move 2 nodes/iteration
      if(slow.par == fast.par) break;
    }
    // if found end of the list, return whether ends in '()
    if(!fast.is_type(types::par))
      return primitive_IS_THE_EMPTY_LIST(fast) 
              ? list_status::ok 
              : list_status::no_null;
    if(!fast.par->second.is_type(types::par))
      return primitive_IS_THE_EMPTY_LIST(fast.par->second) 
              ? list_status::ok 
              : list_status::no_null;
    // if didn't find end of the list, contains a cycle.
    return list_status::cyclic; 
  }


  // "alist?" primitive helper
  bool primitive_list_only_contains_pairs(const data& curr_pair)noexcept{
    if(!curr_pair.par->first.is_type(types::par))  return false;
    if(!curr_pair.par->second.is_type(types::par)) return true;
    return primitive_list_only_contains_pairs(curr_pair.par->second);
  }


  // "last-pair" primitive helper
  data primitive_LAST_iteration(const data& curr_pair, const bool& last_pair)noexcept{
    if(curr_pair.par->second.is_type(types::par)) 
      return primitive_LAST_iteration(curr_pair.par->second, last_pair);
    if(last_pair) return curr_pair;
    return curr_pair.par->first;
  }

  /******************************************************************************
  * LIST ITERATION/CREATION ALGORITHM PRIMITIVE HELPERS
  ******************************************************************************/

  // Compute length of a guarenteed 'data' list (avoids redundant error handling)
  num_type primitive_guarenteed_list_length(const data& d)noexcept{
    if(data_is_the_empty_expression(d)) return num_type();
    num_type count;
    primitive_list_LENGTH_computation(d.par->second,count);
    return count;
  }


  void primitive_confirm_proper_same_sized_lists(const scm_list& lists,const char* name,
                                                 const char* format,   const int first_arg_pos,
                                                                       const scm_list& args){
    num_type length0;
    for(size_type i = 0, n = lists.size(); i < n; ++i) {
      // confirm proper list
      if(!data_is_proper_list(lists[i]))
        THROW_ERR('\''<<name<<" <list> arg #" << first_arg_pos+i+1 << ' ' << PROFILE(lists[i])
          << " isn't a proper list!" << format << FCN_ERR(name,args));
      // confirm congruent length
      if(i == 0) length0 = primitive_guarenteed_list_length(lists[i]);
      else if(length0 != primitive_guarenteed_list_length(lists[i]))
        THROW_ERR('\''<<name<<" <list> lists "<< lists[0] << " and " 
          << lists[i] << " differ in length!" << format << FCN_ERR(name,args));
    }
  }


  // Adds cars to 'args' & advances cdrs. Returns whether lists are empty
  bool check_empty_list_else_acquire_cars_advance_cdrs(scm_list& curr_pairs, 
                                                       scm_list& args)noexcept{
    // Return if fully iterated through each list
    if(!curr_pairs[0].is_type(types::par)) return true;
    // Add each arg for 'proc' & advance each list's head ptr
    for(size_type i = 0, n = curr_pairs.size(); i < n; ++i) {
      args[i] = curr_pairs[i].par->first;
      curr_pairs[i] = curr_pairs[i].par->second;
    }
    return false;
  }


  void primitive_FOR_EACH_applicator(scm_list& curr_pairs, scm_list& proc, env_type& env){
    scm_list args(curr_pairs.size());
    if(check_empty_list_else_acquire_cars_advance_cdrs(curr_pairs,args)) return;
    // Execute proc & recurse down the rest of the lists
    execute_application(proc,args,env);
    primitive_FOR_EACH_applicator(curr_pairs, proc, env);
  }


  void primitive_MAP_list_constructor(scm_list& curr_pairs, scm_list& proc, 
                                      scm_list& mapped_list, env_type& env){
    scm_list args(curr_pairs.size());
    if(check_empty_list_else_acquire_cars_advance_cdrs(curr_pairs,args)) return;
    // Execute proc, store result, & recurse down the rest of the lists
    mapped_list.push_back(data_cast(execute_application(proc,args,env)));
    primitive_MAP_list_constructor(curr_pairs, proc, mapped_list, env);
  }

  void primitive_MAP_BANG_list_constructor(scm_list& curr_pairs, scm_list& proc, 
                                                                 env_type& env){
    scm_list args(curr_pairs.size());
    auto map_to = curr_pairs[0];
    if(check_empty_list_else_acquire_cars_advance_cdrs(curr_pairs,args)) return;
    // Execute proc, store result, & recurse down the rest of the lists
    map_to.par->first = data_cast(execute_application(proc,args,env));
    primitive_MAP_BANG_list_constructor(curr_pairs, proc, env);
  }


  void primitive_FILTER_list_constructor(data& curr_pair, scm_list& proc, 
                                         scm_list& filtered_list, env_type& env){
    // Return if fully iterated through the list
    if(!curr_pair.is_type(types::par)) return;
    // Execute proc, store result, & recurse down the rest of the lists
    scm_list arg(1,curr_pair.par->first);
    if(is_true_scm_condition(proc,arg,env))
      filtered_list.push_back(curr_pair.par->first);
    // Recurse through the rest of the list
    primitive_FILTER_list_constructor(curr_pair.par->second, proc, filtered_list, env);
  }


  // "fold" & "fold-right" primitives helper: recursively applies 'proc' 
  //   to each 'curr_pair', and accumulates their result in 'init_val'
  void primitive_FOLD_accumulator(scm_list& curr_pairs, scm_list& proc, 
                                   data& init_val, env_type& env, 
                                   const bool& folding_left){
    // Return if fully iterated through each list
    if(!curr_pairs[0].is_type(types::par)) return;
    scm_list args;
    // Add each arg for 'proc' & advance each list's head ptr
    for(auto& list_head : curr_pairs) {
      args.push_back(list_head.par->first);
      list_head = list_head.par->second;
    }
    // Execute proc, accumulate result, & recurse down the rest of the lists
    if(folding_left) { // fold is preorder
      args.insert(args.begin(), init_val);
      init_val = data_cast(execute_application(proc,args,env));
    }
    primitive_FOLD_accumulator(curr_pairs,proc,init_val,env,folding_left);
    if(!folding_left) { // fold-right is postorder
      args.insert(args.end(), init_val);
      init_val = data_cast(execute_application(proc,args,env));
    }
  }


  // "fold" & "fold-right" primitive helper template:
  data primitive_FOLD_template(scm_list& args,     const char* name, 
                               const char* format, const bool& folding_left,
                                                             env_type& env){
    // Confirm only given proper lists of the same length
    scm_list list_heads(args.begin()+2, args.end());
    primitive_confirm_proper_same_sized_lists(list_heads,name,format,2,args);
    // Apply the procedure on each elt of each list, & accumulate the result
    data init_val = args[1];
    primitive_FOLD_accumulator(list_heads,args[0].exp,init_val,env,folding_left);
    return init_val; // return the accumulated value
  }


  // "unfold" & "vector-unfold" & "string-unfold" procedure helper template:
  void primitive_UNFOLD_template(scm_list& args,scm_list& unfolded,
                                 const char* name,const char* format){
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm 'unfold call has a proper argument signature
    if(args.size() != 4)
      THROW_ERR('\''<<name<<" received incorrect # of args:"<<format<<FCN_ERR(name,args));
    primitive_confirm_data_is_a_procedure(args[0], name, format, args);
    primitive_confirm_data_is_a_procedure(args[1], name, format, args);
    primitive_confirm_data_is_a_procedure(args[2], name, format, args);
    // unfold the seed into a list
    auto& break_condition = args[0].exp;
    auto& mapper          = args[1].exp;
    auto& successor       = args[2].exp;
    scm_list seed(1,args[3]);
    while(is_false_scm_condition(break_condition,seed,env)) {
      unfolded.push_back(data_cast(execute_application(mapper,seed,env)));
      seed[0] = data_cast(execute_application(successor,seed,env));
    }
  }

  /******************************************************************************
  * LIST MEMBER EXTRACTION PRIMITIVE HELPERS
  ******************************************************************************/

  // "member" "memv" "memq" primitive helper: recursively compares cars to 'obj'
  //   & returns a sublist w/ 'obj' as its 'car' if found. Else returns #f
  data primitive_MEM_car_comparison(data& curr_pair, const data& obj, 
                                                     const prm_type& equality_fcn){
    if(!curr_pair.is_type(types::par)) return G::FALSE_DATA_BOOLEAN;
    scm_list args(2);
    args[0] = curr_pair.par->first, args[1] = obj;
    if(equality_fcn(args).bol.val)
      return curr_pair;
    return primitive_MEM_car_comparison(curr_pair.par->second, obj, equality_fcn);
  }


  // Template helper fcn for the "member" "memv" "memq" primitives.
  data primitive_MEM_template(scm_list& args, const char* name, 
                                              const prm_type& equality_fcn){
    // Confirm given the correct # of args
    if(args.size() != 2)
      THROW_ERR('\''<<name<<" received incorrect # of args:\n     ("
        <<name<<" <obj> <list>)"<<FCN_ERR(name,args));
    // (<mem> <obj> '()) = #f
    if(data_is_the_empty_expression(args[1]))
      return G::FALSE_DATA_BOOLEAN;
    // Confirm given a proper list
    if(!data_is_proper_list(args[1]))
      THROW_ERR('\''<<name<<" 2nd arg "<<PROFILE(args[1])
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
      return G::FALSE_DATA_BOOLEAN;
    if(!curr_pair.par->first.is_type(types::par))
      THROW_ERR('\''<<name<<" 2nd arg "<<head
        <<" isn't a proper association list (list of pairs)!"
        "\n     ("<<name<<" <obj> <association-list>)"<<FCN_ERR(name,args));
    scm_list eq_args(2);
    eq_args[0] = curr_pair.par->first.par->first, eq_args[1] = obj;
    if(equality_fcn(eq_args).bol.val)
      return curr_pair.par->first;
    return primitive_ASSOCIATION_key_seeker(curr_pair.par->second, 
                                            obj,head,name,equality_fcn,args);
  }


  // Template helper fcn for the "assoc" "assv" "assq" primitives.
  data primitive_ASSOCIATION_template(scm_list& args, const char* name, 
                                                      const prm_type& equality_fcn){
    // Confirm given the correct # of args
    if(args.size() != 2)
      THROW_ERR('\''<<name<<" received incorrect # of args:\n     ("
        <<name<<" <obj> <association-list>)"<<FCN_ERR(name,args));
    // (<mem> <obj> '()) = #f
    if(data_is_the_empty_expression(args[1]))
      return G::FALSE_DATA_BOOLEAN;
    // Confirm given a proper list
    if(!data_is_proper_list(args[1]))
      THROW_ERR('\''<<name<<" 2nd arg "<<PROFILE(args[1])<<" isn't a proper list!"
        "\n     ("<<name<<" <obj> <association-list>)"<<FCN_ERR(name,args));
    // Get the sublist w/ 'obj' at its head (if exists)
    return primitive_ASSOCIATION_key_seeker(args[1],args[0],args[1],name,equality_fcn,args);
  }

  /******************************************************************************
  * VECTOR PRIMITIVE HELPERS
  ******************************************************************************/

  // Confirm given a single vector argument
  void primitive_confirm_valid_vector_arg(const scm_list& args,const size_type& total_args,
                                          const char* name,    const char* layout) {
    primitive_confirm_valid_str_or_vec_arg(args,total_args,name,layout,"vector",types::vec);
  }


  // Confirm given a valid vector index. Returns idx as a 'size_type' if so
  size_type primitive_get_if_valid_vector_idx(const scm_list& args,const char* name, 
                                              const char* layout,const size_type& idx_pos=1){
    return primitive_get_if_valid_str_or_vec_idx(args,name,layout,"vector",idx_pos,0,&data::vec);
  }

  /******************************************************************************
  * IOTA (FOR VECTORS & LISTS) PRIMITIVE HELPER
  ******************************************************************************/

  template<bool MAKING_A_LIST, typename SEQUENCE_CTOR>
  data primitive_IOTA_generic(scm_list& args, const char* name, const char* format, 
                                                      SEQUENCE_CTOR make_sequence){
    if(args.empty() || args.size() > 3)
      THROW_ERR('\''<<name<<" received incorrect # of args (only "
        << args.size() << "):" << format << FCN_ERR(name, args));
    if(!primitive_is_valid_index(args[0]))
      THROW_ERR('\''<<name<<" 1st arg "<<PROFILE(args[0])<<" isn't a non-negative integer count!"
        << format << "\n     <count> range: (0," << G::MAX_SIZE_TYPE << ']' 
        << FCN_ERR(name, args));
    auto start = 0_n, step = 1_n; // num_type literals
    if(args.size() > 1) {
      if(!args[1].is_type(types::num))
        THROW_ERR('\''<<name<<" 2nd arg "<<PROFILE(args[1])<<" isn't an number:"
          << format << FCN_ERR(name, args));
      start = args[1].num;
    }
    if(args.size() > 2) {
      if(!args[2].is_type(types::num))
        THROW_ERR('\''<<name<<" 3rd arg "<<PROFILE(args[2])<<" isn't an number:"
          << format << FCN_ERR(name, args));
      step = args[2].num;
    }
    size_type n = (size_type)args[0].num.extract_inexact();
    if(!n) return symconst::emptylist;
    scm_list iota_vals(n);
    for(size_type i = 0; i < n; ++i)
      iota_vals[i] = start + (i * step);
    if constexpr (MAKING_A_LIST) {
      return make_sequence(iota_vals.begin(), iota_vals.end());
    } else {
      return make_sequence(iota_vals);
    }
  }

  /******************************************************************************
  * ALGORITHMIC PRIMITIVE HELPERS: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  void confirm_given_one_sequence_arg(const scm_list& args, const char* name){
    if(args.size() != 1)
      THROW_ERR('\''<<name<<" received incorrect # of arguments:"
        "\n     ("<<name<<" <sequence>)" 
        "\n     <sequence> = <list> || <vector> || <string>" << FCN_ERR(name,args));
  }


  // ************************ "length" & "length+" helper ************************
  data primitive_compute_seq_length(scm_list& args, const char* name, const char* format){
    switch(is_proper_sequence(args[0],args,name,format)) {
      case heist_sequence::vec: return num_type(args[0].vec->size());
      case heist_sequence::str: return num_type(args[0].str->size());
      case heist_sequence::nul: return num_type();
      default:
        num_type count;
        primitive_list_LENGTH_computation(args[0].par->second,count);
        return count;
    }
  }


  // ************************ "reverse" helpers ************************
  data primitive_list_reverse_logic(data& d)noexcept{
    scm_list par_as_exp;
    shallow_unpack_list_into_exp(d,par_as_exp);
    return primitive_LIST_to_CONS_constructor(par_as_exp.rbegin(),par_as_exp.rend());
  }

  template<typename SEQ_CPP_CTOR, typename SEQUENCE_PTR, typename SEQUENCE_CTOR>
  data primitive_reverse_STATIC_SEQUENCE_logic(data& d, SEQUENCE_PTR seq_ptr, 
                                               SEQUENCE_CTOR make_sequence)noexcept{
    return make_sequence(SEQ_CPP_CTOR((d.*seq_ptr)->rbegin(),(d.*seq_ptr)->rend()));
  }


  // ************************ "reverse!" helpers ************************
  data primitive_list_reverse_bang_logic(data& d)noexcept{
    *d.par = *primitive_list_reverse_logic(d).par;
    return G::VOID_DATA_OBJECT;
  }

  template<typename SEQUENCE_PTR>
  data primitive_reverse_bang_STATIC_SEQUENCE_logic(data& d, SEQUENCE_PTR seq_ptr)noexcept{
    std::reverse((d.*seq_ptr)->begin(), (d.*seq_ptr)->end());
    return G::VOID_DATA_OBJECT;
  }


  // ************************ "filter" helper ************************
  data primitive_list_filter_logic(scm_list& args, env_type& env){
    scm_list filtered_list;
    primitive_FILTER_list_constructor(args[1],args[0].exp,filtered_list,env);
    return primitive_LIST_to_CONS_constructor(filtered_list.begin(),filtered_list.end());
  }


  // ************************ "map" helper ************************
  data primitive_list_map_logic(scm_list& args, env_type& env, const char* format){
    // Mapping a list or '() -> get the head of each list
    scm_list list_heads(args.begin()+1, args.end());
    primitive_confirm_proper_same_sized_lists(list_heads,"map",format,1,args);
    // Apply the procedure on each elt of each list & store the result
    scm_list mapped_list;
    primitive_MAP_list_constructor(list_heads,args[0].exp,mapped_list,env);
    return primitive_LIST_to_CONS_constructor(mapped_list.begin(),mapped_list.end());
  }


  // ************************ "for-each" helper ************************
  data primitive_list_for_each_logic(scm_list& args, env_type& env, const char* format){
    scm_list list_heads(args.begin()+1, args.end());
    primitive_confirm_proper_same_sized_lists(list_heads,"for-each",format,1,args);
    primitive_FOR_EACH_applicator(list_heads, args[0].exp, env);
    return G::VOID_DATA_OBJECT;
  }


  // ************************ "copy" helper ************************
  data primitive_list_copy_logic(const data& curr_pair)noexcept{
    if(!curr_pair.is_type(types::par)) return symconst::emptylist;
    data new_pair = data(make_par());
    new_pair.par->first = curr_pair.par->first;
    new_pair.par->second = primitive_list_copy_logic(curr_pair.par->second);
    return new_pair;
  }


  // ************************ "copy!" helper ************************
  data primitive_generic_list_copy_bang_logic(data& dest_pair,data& source_pair)noexcept{
    if(!dest_pair.is_type(types::par) || !source_pair.is_type(types::par)) 
      return data(types::dne);
    dest_pair.par->first = source_pair.par->first;
    return primitive_generic_list_copy_bang_logic(dest_pair.par->second,
                                                  source_pair.par->second);
  }

  template<typename SEQ_PTR>
  data primitive_generic_STATIC_CONTAINER_copy_bang_logic(SEQ_PTR& s1, SEQ_PTR& s2)noexcept{
    for(size_type i = 0, n = s1->size() < s2->size() ? s1->size() : s2->size(); i < n; ++i)
      s1->operator[](i) = s2->operator[](i);
    return data(types::dne);
  }


  // ************************ "count" helper ************************
  data primitive_list_count_logic(scm_list& args, env_type& env){
    num_type count;
    primitive_LIST_COUNT_computation(args[1],count,args[0].exp,env);
    return count;
  }


  // ************************ "ref" helper ************************
  data primitive_list_ref_seeker(const data& curr_pair, const size_type& idx, const char* format,
                                                        const scm_list& args, const size_type& pos=0){
    if(!curr_pair.is_type(types::par))
      THROW_ERR("'ref <list> received out of range index " << idx 
        <<"\n     for list "<<args[0]<<" of size "<<pos<<'!'<<format
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("ref",args));
    if(pos == idx) return curr_pair.par->first;
    return primitive_list_ref_seeker(curr_pair.par->second,idx,format,args,pos+1);
  }


  // ************************ "slice" helper ************************
  data primitive_substring_logic(scm_list& args, const char* format) {
    const auto start = primitive_get_if_valid_string_idx(args, "slice", format);
    // Add the default <size> value as needed
    if(args.size() == 2) args.push_back(num_type(args[0].str->size()-start));
    // confirm given an in-'size_type'-range non-negative size
    if(!primitive_is_valid_index(args[2]))
      THROW_ERR("'slice <string> arg "<<PROFILE(args[2])<<" isn't a proper non-negative integer "
        "<optional-length>!" << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("slice", args));
    // confirm length + start falls w/in range of the sequence
    const auto length = (size_type)args[2].num.extract_inexact();
    if(length + start > args[0].str->size())
      THROW_ERR("'slice <string> received out of range <optional-length> " << length 
        << " from <start-index> " << start << "\n     for string "
        <<args[0]<<" of size "<< args[0].str->size()<<'!' << format 
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("slice", args));
    return make_str(args[0].str->substr(start,length));
  }

  data primitive_subvector_extraction(scm_list& args, const char* format) {
    // confirm given valid in-'size_type'-range non-negative start index
    if(!primitive_is_valid_index(args[1]))
      THROW_ERR("'slice <vector> arg "<<PROFILE(args[1])
        <<" isn't a proper non-negative integer!"<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<FCN_ERR("slice",args));
    const size_type start      = (size_type)args[1].num.extract_inexact();
    const size_type vec_length = args[0].vec->size();
    if(start+1 > vec_length)
      THROW_ERR("'slice <vector> <start-index> "<<start<<" is out of range for vector "
        <<args[0]<<" of length "<<vec_length<<'!'<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<FCN_ERR("slice",args));
    // Add the default <size> value as needed
    if(args.size() == 2) args.push_back(num_type(args[0].vec->size()-start));
    // confirm given valid in-'size_type'-range non-negative length
    if(!primitive_is_valid_index(args[2]))
      THROW_ERR("'slice <vector> <optional-length> arg "<<PROFILE(args[2])
        <<" isn't a proper non-negative integer!"<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<FCN_ERR("slice",args));
    // confirm length + start falls w/in range of the sequence
    const size_type length = (size_type)args[2].num.extract_inexact();
    if(start+length > vec_length)
      THROW_ERR("'slice <vector> length "<<length<<" from <start-index> " 
        <<start<<" is out of range for vector "<<args[0]<<" of length "
        <<vec_length<<'!'<<format<<VALID_SEQUENCE_INDEX_RANGE<<FCN_ERR("slice",args));
    // Extract the subvector
    if(!length) return make_vec(scm_list()); // length = 0 -> '#()
    return make_vec(scm_list(args[0].vec->begin()+start, args[0].vec->begin()+start+length));
  }

  // recursively mk sublist from 'curr_pair's [start,end)
  void primitive_MK_SUBLIST_recur(data& curr_pair,      const size_type& start, 
                                  const size_type& end, const size_type& count, 
                                                        scm_list& list_exp)noexcept{
    if(count == end || !curr_pair.is_type(types::par)) return;
    if(count >= start) list_exp.push_back(curr_pair.par->first);
    primitive_MK_SUBLIST_recur(curr_pair.par->second, start, end, count+1, list_exp);
  }

  data primitive_sublist_extraction(scm_list& args, const char* format){  
    // confirm given valid in-'size_type'-range non-negative start index
    if(!primitive_is_valid_index(args[1]))
      THROW_ERR("'slice <list> index "<<PROFILE(args[1])
        <<" isn't a proper non-negative integer!"<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<FCN_ERR("slice",args));
    const size_type start      = (size_type)args[1].num.extract_inexact();
    const size_type lis_length = (size_type)primitive_guarenteed_list_length(args[0]).extract_inexact();
    if(start+1 > lis_length)
      THROW_ERR("'slice <list> <start-index> "<<start<<" is out of range for list "
        <<args[0]<<" of length "<<lis_length<<'!'<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<FCN_ERR("slice",args));
    // Add the default <size> value as needed
    if(args.size() == 2) args.push_back(num_type(lis_length-start));
    // confirm given valid in-'size_type'-range non-negative length
    if(!primitive_is_valid_index(args[2]))
      THROW_ERR("'slice <list> <optional-length> arg "<<PROFILE(args[2])
        <<" isn't a proper non-negative integer!"<<format<<VALID_SEQUENCE_INDEX_RANGE
        <<FCN_ERR("slice",args));
    // confirm length + start falls w/in range of the sequence
    const size_type length = (size_type)args[2].num.extract_inexact();
    if(start+length > lis_length)
      THROW_ERR("'slice <list> <optional-length> "<<length<<" from <start-index> "<<start
        <<" is out of range for list "<<args[0]<<" of length "<<lis_length<<'!'
        <<format<<VALID_SEQUENCE_INDEX_RANGE<<FCN_ERR("slice",args));
    // Extract the sublist
    if(!length) return symconst::emptylist; // length = 0 -> '()
    scm_list sublist_exp;
    primitive_MK_SUBLIST_recur(args[0], start, start+length, 0, sublist_exp);
    return primitive_LIST_to_CONS_constructor(sublist_exp.begin(),sublist_exp.end());
  }


  // ************************ "set-index!" helper ************************
  void primitive_list_set_index_applicator(data& curr_pair, const size_type& idx, const char* format, 
                                                            scm_list& args, const size_type& pos=0){
    if(!curr_pair.is_type(types::par))
      THROW_ERR("'set-index! <list> received out of range index " << idx 
        <<"\n     for list "<<args[0]<<" of size "<<pos<<'!'<<format
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("set-index!",args));
    if(pos == idx) {
      curr_pair.par->first = args[2];
      return;
    }
    primitive_list_set_index_applicator(curr_pair.par->second,idx,format,args,pos+1);
  }


  // ************************ "swap-indices!" helper ************************
  void primitive_list_swap_indices_applicator(data& curr_pair,const size_type& front_idx,
                                              const size_type& back_idx, data& first_node,
                                              const char* format, scm_list& args, 
                                              const size_type& pos=0){
    if(!curr_pair.is_type(types::par))
      THROW_ERR("'swap-indices! <list> received out of range index pair " << front_idx 
        << " & " << back_idx <<"\n     for list "<<args[0]<<" of size "<<pos<<'!'<<format
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("swap-indices!",args));
    if(pos == front_idx) {
      primitive_list_swap_indices_applicator(curr_pair.par->second,front_idx,back_idx,
                                              curr_pair.par->first,format,args,pos+1);
    } else if(pos == back_idx) {
      std::swap(first_node,curr_pair.par->first);
    } else {
      primitive_list_swap_indices_applicator(curr_pair.par->second,front_idx,back_idx,
                                                        first_node,format,args,pos+1);
    }
  }

  void primitive_list_swap_indices_logic(scm_list& args, const char* format){
    if(!primitive_is_valid_index(args[1])) 
      THROW_ERR("'swap-indices! <list> 2nd arg " << PROFILE(args[1]) << " is an invalid <index>:"
        << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("swap-indices!",args));
    if(!primitive_is_valid_index(args[2])) 
      THROW_ERR("'swap-indices! <list> 3rd arg " << PROFILE(args[2]) << " is an invalid <index>:"
        << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("swap-indices!",args));
    if(args[1].num != args[2].num) {
      data tmp; // dummy first node
      primitive_list_swap_indices_applicator(args[0],(size_type)args[1].num.extract_inexact(),
                                                     (size_type)args[2].num.extract_inexact(),
                                                     tmp,format,args);
    }
  }


  // ************************ "fill!" helper ************************
  data primitive_vector_fill_logic(scm_list& args)noexcept{
    for(size_type i = 0, n = args[0].vec->size(); i < n; ++i)
      args[0].vec->operator[](i) = args[1];
    return G::VOID_DATA_OBJECT;
  }

  data primitive_string_fill_logic(scm_list& args, const char* format){
    if(!args[1].is_type(types::chr))
      THROW_ERR("'fill! <string> for "<<PROFILE(args[0])<<" received non-character fill-value\n     " 
        << PROFILE(args[1]) << '!'<< format << FCN_ERR("fill!", args));
    for(size_type i = 0, n = args[0].str->size(); i < n; ++i)
      args[0].str->operator[](i) = args[1].chr;
    return G::VOID_DATA_OBJECT;
  }

  data primitive_list_fill_logic(data& curr_pair, data& fill_value)noexcept{
    if(!curr_pair.is_type(types::par)) return G::VOID_DATA_OBJECT;
    curr_pair.par->first = fill_value;
    return primitive_list_fill_logic(curr_pair.par->second,fill_value);
  }


  // ************************ "append" helpers ************************
  void shallow_unpack_possibly_dotted_list_into_exp(data& curr_pair, scm_list& args_list)noexcept{
    if(curr_pair.is_type(types::par)) {
      args_list.push_back(curr_pair.par->first);
      shallow_unpack_possibly_dotted_list_into_exp(curr_pair.par->second, args_list); 
    } else if(!data_is_the_empty_expression(curr_pair)) {
      args_list.push_back(curr_pair);
    }
  }

  data make_deep_copy_of_list(data& curr_pair)noexcept{
    if(!curr_pair.is_type(types::par)) return curr_pair;
    data new_pair = data(make_par());
    new_pair.par->first = curr_pair.par->first;
    new_pair.par->second = make_deep_copy_of_list(curr_pair.par->second);
    return new_pair;
  }

  void primitive_appended_deep_copy_of_RHS_to_mutated_LHS(data& LHS, data& RHS)noexcept{
    if(!LHS.par->second.is_type(types::par)) {
      LHS.par->second = make_deep_copy_of_list(RHS);
      return;
    }
    primitive_appended_deep_copy_of_RHS_to_mutated_LHS(LHS.par->second,RHS);
  }


  data primitive_vector_append_logic(scm_list& args, const char* format){
    scm_list appended_vector;
    for(size_type i = 0, n = args.size()-1; i < n; ++i) {
      if(!args[i].is_type(types::vec))
        THROW_ERR("'append <vector> arg #"<<i+1<<' '<<PROFILE(args[i])
          << " isn't a vector:" << format << FCN_ERR("append",args));
      appended_vector.insert(appended_vector.end(), args[i].vec->begin(),
                                                    args[i].vec->end());
    }
    if(appended_vector.empty()) return *args.rbegin();
    if(!args.rbegin()->is_type(types::vec))
      appended_vector.push_back(*args.rbegin());
    else
      appended_vector.insert(appended_vector.end(), args.rbegin()->vec->begin(),
                                                    args.rbegin()->vec->end());
    return make_vec(appended_vector);
  }


  data primitive_string_append_logic(scm_list& args, const char* format){
    auto i = confirm_only_args_of_type(args, types::str);
    if(i != G::MAX_SIZE_TYPE && i+1 != args.size())
      THROW_ERR("'append <string> arg #" << i+1 << ", " << PROFILE(args[i]) 
        << ", isn't a string:" << format << FCN_ERR("append", args));
    scm_string str_val;
    for(size_type j = 0, n = args.size()-1; j < n; ++j)
      str_val += *args[j].str;
    if(str_val.empty()) return *args.rbegin();
    if(i != G::MAX_SIZE_TYPE) {
      if(!args[i].is_type(types::chr))
        THROW_ERR("'append <string> last arg " << PROFILE(args[i])
          << " isn't a character or string!" << format << FCN_ERR("append", args));
      return make_str(str_val + char(args.rbegin()->chr));
    }
    return make_str(str_val + *args.rbegin()->str);
  }


  // PRECONDITIONS: 1) THE FIRST n-1 ARGS MUST HAVE "list?" = TRUE
  //                2) THE LAST ARG MUST NOT BE A CYCLIC LIST
  data primitive_list_append_logic(scm_list& args, const char* format){
    // (append <obj>) = <obj>
    const auto n = args.size();
    // Confirm Precondition 2 
    if(args[n-1].is_type(types::par) && 
      primitive_list_is_acyclic_and_null_terminated(args[n-1]) == list_status::cyclic)
      THROW_ERR("'append <list> last argument "<<PROFILE(args[n-1])
        <<" isn't an acyclic list:"<< format << FCN_ERR("append", args));
    // Confirm Precondition 1
    for(size_type i = 0; i < n-1; ++i) {
      if(data_is_the_empty_expression(args[i])) continue;
      if(!args[i].is_type(types::par))
        THROW_ERR("'append <list> argument #" << i+1 << ' ' << PROFILE(args[i]) 
          << " isn't a pair:" << format << FCN_ERR("append", args));
      else if(auto stat = primitive_list_is_acyclic_and_null_terminated(args[i]); 
        stat == list_status::cyclic) {
        THROW_ERR("'append <list> argument #" << i+1 << ' ' << PROFILE(args[i]) 
          << " isn't an acyclic list:" << format << FCN_ERR("append", args));
      } else if(stat == list_status::no_null)
        THROW_ERR("'append <list> argument #" << i+1 << ' ' << PROFILE(args[i]) 
          << " isn't a '() terminated list:" << format << FCN_ERR("append", args));
    }
    // Link the guarenteed proper lists to one another
    scm_list appended;
    for(size_type i = 0; i < n-1; ++i)
      if(!data_is_the_empty_expression(args[i]))
        shallow_unpack_possibly_dotted_list_into_exp(args[i],appended);
    // Link last object (anything except a circular list)
    if(appended.empty()) return make_deep_copy_of_list(args[n-1]);
    auto appended_lists = primitive_LIST_to_CONS_constructor(appended.begin(), appended.end());
    primitive_appended_deep_copy_of_RHS_to_mutated_LHS(appended_lists,args[n-1]);
    return appended_lists;
  }


  // ************************ "remove" helper ************************
  data primitive_remove_list_logic(data& curr_pair, scm_list& proc, env_type& env) {
    if(curr_pair.is_type(types::par)) {
      scm_list pruning_args(1,curr_pair.par->first);
      if(is_false_scm_condition(proc,pruning_args,env)){
        data new_pair = data(make_par());
        new_pair.par->first = curr_pair.par->first;
        new_pair.par->second = primitive_remove_list_logic(curr_pair.par->second,proc,env);
        return new_pair;
      }
      return primitive_remove_list_logic(curr_pair.par->second, proc, env); 
    }
    return symconst::emptylist;
  }


  // ************************ "remove-first" & "remove-last" helper ************************
  template <bool REMOVING_FIRST, typename SEQUENCE_TYPE>
  auto prm_remove_first_or_last(scm_list& pred, SEQUENCE_TYPE sequence, env_type& env){
    constexpr auto get_begin = [](SEQUENCE_TYPE& s){if constexpr (REMOVING_FIRST) return s.begin(); else return s.rbegin();};
    constexpr auto get_end   = [](SEQUENCE_TYPE& s){if constexpr (REMOVING_FIRST) return s.end();   else return s.rend();};
    auto start = get_begin(sequence), end = get_end(sequence);
    for(; start != end; ++start) {
      scm_list pruning_args(1,*start);
      if(is_true_scm_condition(pred,pruning_args,env)) {
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


  // ************************ "delete" helper ************************
  template<size_type(*get_idx_if_valid)(const scm_list&,const char*,const char*,const size_type&),
           typename SEQUENCE_PTR>
  auto primitive_delete_STATIC_SEQUENCE_logic(scm_list& args, const char* format,
                                                              SEQUENCE_PTR seq_ptr){
    auto new_sequence(*(args[0].*seq_ptr));
    new_sequence.erase(new_sequence.begin()+get_idx_if_valid(args,"delete",format,1));
    return new_sequence;
  }

  data primitive_list_delete_logic(scm_list& args, const char* format){
    if(!primitive_is_valid_index(args[1])) 
      THROW_ERR("'delete <list> 2nd arg " << PROFILE(args[1]) << " is an invalid <index>:"
        << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("delete",args));
    scm_list list_exp;
    shallow_unpack_list_into_exp(args[0],list_exp);
    const auto idx = (size_type)args[1].num.extract_inexact();
    if(idx >= list_exp.size())
      THROW_ERR("'delete <list> received out of range index " << idx 
        <<"\n     for list "<<args[0]<<" of size "<<list_exp.size()<<'!'
        << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("delete",args));
    list_exp.erase(list_exp.begin()+idx);
    return primitive_LIST_to_CONS_constructor(list_exp.begin(), list_exp.end());
  }


  // ************************ "last" helper ************************
  data primitive_list_last_logic(const data& curr_pair)noexcept{
    if(!curr_pair.par->second.is_type(types::par))
      return curr_pair.par->first;
    return primitive_list_last_logic(curr_pair.par->second); 
  }


  // ************************ "init" helper ************************
  data primitive_list_init_logic(const data& curr_pair)noexcept{
    if(!curr_pair.par->second.is_type(types::par)) 
      return symconst::emptylist;
    data new_pair = data(make_par());
    new_pair.par->first = curr_pair.par->first;
    new_pair.par->second = primitive_list_init_logic(curr_pair.par->second);
    return new_pair;
  }


  // ************************ "seq=" helpers ************************
  template<typename SEQUENCE_PTR>
  data primitive_STATIC_SEQUENCE_sequence_eq_logic(scm_list& args, const char* format,
                                                   env_type& env,  const types& t,
                                                   SEQUENCE_PTR seq_ptr){
    // Confirm only given sequences of type t (besides the procedure)
    static constexpr const char * const type_names[2] = {"string", "vector"};
    bool same_sizes = true;
    const size_type total_sequences = args.size();
    for(size_type i = 1, n = total_sequences; i < n; ++i) {
      if(!args[i].is_type(t))
        THROW_ERR("'seq= <"<<type_names[t==types::vec]<<"> arg #"<<i+1
          <<' '<<PROFILE(args[i])<<" isn't a "<<type_names[t==types::vec]<<':'
          << format << FCN_ERR("seq=",args));
      same_sizes = ((args[i].*seq_ptr)->size() == 
                    (args[1].*seq_ptr)->size() && same_sizes);
    }
    if(total_sequences < 3) return G::TRUE_DATA_BOOLEAN; // 0 or 1 sequence -> true
    if(!same_sizes) return G::FALSE_DATA_BOOLEAN; // sequences of != sizes are !=
    // Confirm each sequence is elt=?
    const size_type total_elements = (args[1].*seq_ptr)->size();
    auto& proc = args[0].exp;
    scm_list sequence_args(total_sequences-1);
    for(size_type i = 0; i < total_elements; ++i) { // for each element
      for(size_type j = 1; j < total_sequences; ++j) // in each sequence
        sequence_args[j-1] = (args[j].*seq_ptr)->operator[](i);
      if(is_false_scm_condition(proc,sequence_args,env)) // if elts are !=
        return G::FALSE_DATA_BOOLEAN; // sequences are !=
    }
    return G::TRUE_DATA_BOOLEAN; // else sequences are ==
  }


  data primitive_list_sequence_eq_logic(scm_list& args, const char* format, env_type& env){
    // Confirm only given lists (besides the procedure)
    bool same_sizes = true;
    const size_type total_lists = args.size();
    std::vector<scm_list> lists_as_exps(total_lists-1);
    for(size_type i = 1, n = total_lists; i < n; ++i) {
      if(!data_is_the_empty_expression(args[i])) {
        if(!args[i].is_type(types::par) || 
           primitive_list_is_acyclic_and_null_terminated(args[i]) != list_status::ok)
          THROW_ERR("'seq= <list> arg #"<<i+1<<' '<<PROFILE(args[i])
            <<" isn't a proper null-terminated list:"
            << format << FCN_ERR("seq=", args));
        shallow_unpack_list_into_exp(args[i], lists_as_exps[i-1]);
      }
      same_sizes = lists_as_exps[i-1].size() == lists_as_exps[0].size() && same_sizes;
    }
    if(total_lists < 3) return G::TRUE_DATA_BOOLEAN; // 0 or 1 list -> true
    if(!same_sizes)     return G::FALSE_DATA_BOOLEAN; // lists of != sizes are !=
    // Confirm each list is elt=?
    const size_type total_elements = lists_as_exps[0].size();
    auto& proc = args[0].exp;
    scm_list lis_args(total_lists-1);
    for(size_type i = 0; i < total_elements; ++i) { // for each element
      for(size_type j = 0; j+1 < total_lists; ++j) // in each list
        lis_args[j] = lists_as_exps[j][i];
      if(is_false_scm_condition(proc,lis_args,env)) // if elts are !=
        return G::FALSE_DATA_BOOLEAN; // lists are !=
    }
    return G::TRUE_DATA_BOOLEAN; // else lists are ==
  }


  // ************************ "skip" & "index" helpers ************************
  template <bool(*truth_proc)(scm_list&,scm_list&,env_type&), typename SEQUENCE_PTR>
  data prm_search_STATIC_SEQUENCE_from_left(scm_list& args, SEQUENCE_PTR seq_ptr,
                                                            env_type& env){
    if((args[1].*seq_ptr)->empty()) return G::FALSE_DATA_BOOLEAN;
    const auto& sequence = *(args[1].*seq_ptr);
    for(size_type i = 0, n = sequence.size(); i < n; ++i) {
      scm_list proc_args(1,sequence[i]);
      if(truth_proc(args[0].exp,proc_args,env))
        return num_type(i);
    }
    return G::FALSE_DATA_BOOLEAN;
  }

  template <bool(*truth_proc)(scm_list&,scm_list&,env_type&)>
  data prm_search_list_from_left(data& curr_pair,scm_list& proc,env_type& env,
                                                    const size_type& count=0){
    if(!curr_pair.is_type(types::par)) return G::FALSE_DATA_BOOLEAN;
    scm_list proc_args(1,curr_pair.par->first);
    if(truth_proc(proc,proc_args,env)) return num_type(count);
    return prm_search_list_from_left<truth_proc>(curr_pair.par->second,proc,env,count+1);
  }


  // ************************ "skip-right" & "index-right" helpers ************************
  template <bool(*truth_proc)(scm_list&,scm_list&,env_type&), typename SEQUENCE_PTR>
  data prm_search_STATIC_SEQUENCE_from_right(scm_list& args, SEQUENCE_PTR seq_ptr,
                                                             env_type& env){
    if((args[1].*seq_ptr)->empty()) return G::FALSE_DATA_BOOLEAN;
    const auto& sequence = *(args[1].*seq_ptr);
    for(size_type i = sequence.size(); i-- > 0;) {
      scm_list proc_args(1,sequence[i]);
      if(truth_proc(args[0].exp,proc_args,env))
        return num_type(i);
    }
    return G::FALSE_DATA_BOOLEAN;
  }

  template <bool(*truth_proc)(scm_list&,scm_list&,env_type&)>
  data prm_search_list_from_right(scm_list& args, env_type& env){
    scm_list sequence;
    shallow_unpack_list_into_exp(args[1],sequence);
    for(size_type i = sequence.size(); i-- > 0;) {
      scm_list proc_args(1,sequence[i]);
      if(truth_proc(args[0].exp,proc_args,env))
        return num_type(i);
    }
    return G::FALSE_DATA_BOOLEAN;
  }


  // ************************ "take" "drop" "...-right" helpers ************************
  size_type primitive_get_length_if_valid(scm_list& args,     const char* name, 
                                          const char* format, const char* seq_name,
                                          const size_type& sequence_length){
    if(!primitive_is_valid_index(args[1]))
      THROW_ERR('\''<<name<<" <"<<seq_name<<"> 2nd arg " << PROFILE(args[1]) << " isn't a"
        "\n     proper non-negative integer length!" << format
        << "\n     <length> range: [0," << G::MAX_SIZE_TYPE << ']' 
        << FCN_ERR(name, args));
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(n > sequence_length)
      THROW_ERR('\''<<name<<" <"<<seq_name<<"> 2nd arg " << PROFILE(args[1])
        << " exceeds the length " << sequence_length
        << " of the given " << seq_name << ' '
        << args[0] << FCN_ERR(name, args));
    return n;
  }

  template<data(*primitive_vector_logic)(scm_list&,decltype(make_vec),scm_list&,const char*,const char*),
           data(*primitive_string_logic)(scm_string&,decltype(make_str),scm_list&,const char*,const char*),
           data(*primitive_list_logic)(scm_list&,decltype(primitive_LIST_to_CONS_constructor<scm_node>),scm_list&,const char*,const char*)>
  data primitive_take_drop_template(scm_list& args,const char* name,const char* format){
    if(args.size() != 2) 
      THROW_ERR('\''<<name<<" received incorrect # of args (given " 
        << args.size() << "):" << format 
        << "\n     <length> range: [0," << G::MAX_SIZE_TYPE << ']' 
        << FCN_ERR(name,args));
    switch(is_proper_sequence(args[0],args,name,format)){
      case heist_sequence::vec: return primitive_vector_logic(*args[0].vec,make_vec,args,format,"vector");
      case heist_sequence::str: return primitive_string_logic(*args[0].str,make_str,args,format,"string");
      default:
        scm_list flattened_list;
        shallow_unpack_list_into_exp(args[0], flattened_list);
        return primitive_list_logic(flattened_list,primitive_LIST_to_CONS_constructor,args,format,"list");
    }
  }


  // ************************ "drop" helper ************************
  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data primitive_drop_GENERIC_logic(SEQUENCE_TYPE& sequence, 
                                    SEQUENCE_CTOR make_heist_sequence, 
                                    scm_list& args, const char* format, 
                                                    const char* seq_name){
    // Lists DON'T require a C++ ctor to mk a Heist sequence from iterators
    if constexpr (MAKING_A_LIST) {
      return make_heist_sequence(
        sequence.begin() + 
          primitive_get_length_if_valid(args,"drop",format,seq_name,sequence.size()), 
        sequence.end());
    } else { // Strings & Vectors DO require a C++ ctor prior becoming a heist sequence
      return make_heist_sequence(SEQUENCE_TYPE(
        sequence.begin() + 
          primitive_get_length_if_valid(args,"drop",format,seq_name,sequence.size()),
        sequence.end()));
    }
  }


  // ************************ "drop-right" helper ************************
  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data primitive_drop_right_GENERIC_logic(SEQUENCE_TYPE& sequence, 
                                          SEQUENCE_CTOR make_heist_sequence, 
                                          scm_list& args, const char* format, 
                                                          const char* seq_name){
    if constexpr (MAKING_A_LIST) { // See "primitive_drop_GENERIC_logic"
      return make_heist_sequence(
        sequence.begin(), 
        sequence.end() - 
          primitive_get_length_if_valid(args,"drop-right",format,seq_name,sequence.size()));
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(
        sequence.begin(), 
        sequence.end() - 
          primitive_get_length_if_valid(args,"drop-right",format,seq_name,sequence.size())));
    }
  }


  // ************************ "take" helper ************************
  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data primitive_take_GENERIC_logic(SEQUENCE_TYPE& sequence, SEQUENCE_CTOR make_heist_sequence, 
                                    scm_list& args, const char* format, const char* seq_name){
    if constexpr (MAKING_A_LIST) { // See "primitive_drop_GENERIC_logic"
      return make_heist_sequence(
        sequence.begin(),
        sequence.begin() + 
          primitive_get_length_if_valid(args,"take",format,seq_name,sequence.size()));
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(
        sequence.begin(),
        sequence.begin() + 
          primitive_get_length_if_valid(args,"take",format,seq_name,sequence.size())));
    }
  }


  // ************************ "take-right" helper ************************
  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data primitive_take_right_GENERIC_logic(SEQUENCE_TYPE& sequence, 
                                          SEQUENCE_CTOR make_heist_sequence, 
                                          scm_list& args, const char* format, 
                                                          const char* seq_name){
    if constexpr (MAKING_A_LIST) { // See "primitive_drop_GENERIC_logic"
      return make_heist_sequence(
        sequence.end() - 
          primitive_get_length_if_valid(args,"take-right",format,seq_name,sequence.size()),
        sequence.end());
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(
        sequence.end() - 
          primitive_get_length_if_valid(args,"take-right",format,seq_name,sequence.size()),
        sequence.end()));
    }
  }


  // ************************ "take-while" "drop-while" "...-right-while" helper ************************
  template<data(*primitive_vector_logic)(scm_list&,decltype(make_vec),scm_list&,env_type&),
           data(*primitive_string_logic)(scm_string&,decltype(make_str),scm_list&,env_type&),
           data(*primitive_list_logic)(scm_list&,decltype(primitive_LIST_to_CONS_constructor<scm_node>),scm_list&,env_type&)>
  data primitive_take_drop_while_template(scm_list& args,const char* name,const char* format){
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 2) 
      THROW_ERR('\''<<name<<" received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR(name,args));
    primitive_confirm_data_is_a_procedure(args[0], name, format, args);
    switch(is_proper_sequence(args[1],args,name,format)){
      case heist_sequence::vec: return primitive_vector_logic(*args[1].vec,make_vec,args[0].exp,env);
      case heist_sequence::str: return primitive_string_logic(*args[1].str,make_str,args[0].exp,env);
      default:
        scm_list flattened_list;
        shallow_unpack_list_into_exp(args[1], flattened_list);
        return primitive_list_logic(flattened_list,primitive_LIST_to_CONS_constructor,args[0].exp,env);
    }
  }


  // ************************ "drop-while" helper ************************
  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data primitive_drop_while_GENERIC_logic(SEQUENCE_TYPE& sequence,  
                                          SEQUENCE_CTOR make_heist_sequence, 
                                          scm_list& proc, env_type& env){
    size_type i = 0;
    for(const size_type n = sequence.size(); i < n; ++i){
      scm_list arg(1,sequence[i]);
      if(is_false_scm_condition(proc,arg,env)) break;
    }
    // Lists DON'T require a C++ ctor to mk a Heist sequence from iterators
    if constexpr (MAKING_A_LIST) { 
      return make_heist_sequence(sequence.begin()+i,sequence.end());
    } else { // Strings & Vectors DO require a C++ ctor prior becoming a heist sequence
      return make_heist_sequence(SEQUENCE_TYPE(sequence.begin()+i,sequence.end()));
    }
  }


  // ************************ "drop-right-while" helper ************************
  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data primitive_drop_right_while_GENERIC_logic(SEQUENCE_TYPE& sequence,
                                                SEQUENCE_CTOR make_heist_sequence, 
                                                scm_list& proc, env_type& env){
    size_type i = sequence.size();
    for(; i-- > 0;){
      scm_list arg(1,sequence[i]);
      if(is_false_scm_condition(proc,arg,env)) break;
    }
    if constexpr (MAKING_A_LIST) { // See "primitive_drop_while_GENERIC_logic"
      return make_heist_sequence(sequence.begin(),sequence.begin()+i+1);
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(sequence.begin(),sequence.begin()+i+1));
    }
  }


  // ************************ "take-while" helper ************************
  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data primitive_take_while_GENERIC_logic(SEQUENCE_TYPE& sequence,
                                          SEQUENCE_CTOR make_heist_sequence, 
                                          scm_list& proc, env_type& env){
    size_type i = 0;
    for(const size_type n = sequence.size(); i < n; ++i){
      scm_list arg(1,sequence[i]);
      if(is_false_scm_condition(proc,arg,env)) break;
    }
    if constexpr (MAKING_A_LIST) { // See "primitive_drop_while_GENERIC_logic"
      return make_heist_sequence(sequence.begin(),sequence.begin()+i);
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(sequence.begin(),sequence.begin()+i));
    }
  }


  // ************************ "take-right-while" helper ************************
  template<bool MAKING_A_LIST, typename SEQUENCE_TYPE, typename SEQUENCE_CTOR>
  data primitive_take_right_while_GENERIC_logic(SEQUENCE_TYPE& sequence,
                                                SEQUENCE_CTOR make_heist_sequence, 
                                                scm_list& proc, env_type& env){
    size_type i = sequence.size();
    for(; i-- > 0;){
      scm_list arg(1,sequence[i]);
      if(is_false_scm_condition(proc,arg,env)) break;
    }
    if constexpr (MAKING_A_LIST) { // See "primitive_drop_while_GENERIC_logic"
      return make_heist_sequence(sequence.begin()+i+1,sequence.end());
    } else {
      return make_heist_sequence(SEQUENCE_TYPE(sequence.begin()+i+1,sequence.end()));
    }
  }


  // ************************ "any" & "every" helpers ************************
  template <types SEQUENCE_TYPE, typename SEQUENCE_PTR>
  size_type confirm_proper_STATIC_SEQUENCE_any_every_args(scm_list& args,
                                   SEQUENCE_PTR seq_ptr, const char* name,
                                   const char* format,   const char* seq_name){
    if(args.size() < 2)
      THROW_ERR('\''<<name<<" <"<<seq_name<<"> received insufficient args (only "
        << args.size() << "):" << format << FCN_ERR(name, args));
    primitive_confirm_data_is_a_procedure(args[0], name, format, args);
    size_type min_sequence_length = G::MAX_SIZE_TYPE;
    for(size_type i = 1, n = args.size(); i < n; ++i) {
      // Validate sequence type if working w/ a vector or string (lists already validated)
      if constexpr (SEQUENCE_TYPE == types::vec || SEQUENCE_TYPE == types::str) { 
        if(!args[i].is_type(SEQUENCE_TYPE))
          THROW_ERR('\''<<name<<" <"<<seq_name<<"> arg #"<<i+1<<' '<<PROFILE(args[i])
            <<" isn't a "<<seq_name<<':'<< format << FCN_ERR(name, args));
        if((args[i].*seq_ptr)->size() < min_sequence_length) 
          min_sequence_length = (args[i].*seq_ptr)->size();
      } else {
        if(args[i].exp.size() < min_sequence_length) 
          min_sequence_length = args[i].exp.size();
      }
    }
    return min_sequence_length;
  }

  bool convert_lists_to_exp_matrix_and_return_if_empty(scm_list& args,   scm_list& list_exps, 
                                                       const char* name, const char* format){
    list_exps.push_back(args[0]);
    list_exps.push_back(scm_list());
    shallow_unpack_list_into_exp(args[1], list_exps[1].exp);
    for(size_type i = 2, n = args.size(); i < n; ++i) {
      if(auto stat = is_proper_sequence(args[i],args,name,format); stat == heist_sequence::nul)
        return true;
      else if(stat != heist_sequence::lis)
        THROW_ERR('\''<<name<<" <list> arg #"<<i+1<<' '<<PROFILE(args[i])<<" isn't a proper list:"
          << format << FCN_ERR(name,args));
      list_exps.push_back(scm_list());
      shallow_unpack_list_into_exp(args[i], list_exps[i].exp);
    }
    return false;
  }


  // ************************ "any" helper ************************
  template <types SEQUENCE_TYPE, typename SEQUENCE_PTR>
  data primitive_STATIC_SEQUENCE_any_logic(scm_list& args,       env_type& env, 
                                           SEQUENCE_PTR seq_ptr, const char* seq_name, 
                                                                 const char* format){
    size_type min_length = confirm_proper_STATIC_SEQUENCE_any_every_args<SEQUENCE_TYPE>(args,
      seq_ptr, "any",format, seq_name);
    if(!min_length) return G::FALSE_DATA_BOOLEAN;
    const size_type total_sequences = args.size();
    scm_list& proc = args[0].exp;
    scm_list any_args(total_sequences-1);
    // For each element
    for(size_type i = 0; i < min_length; ++i){
      // In each sequence
      for(size_type j = 1; j < total_sequences; ++j) {
        if constexpr (SEQUENCE_TYPE == types::vec || SEQUENCE_TYPE == types::str) {
          any_args[j-1] = (args[j].*seq_ptr)->operator[](i);
        } else { // lists aren't passed via a smart pointer member
          any_args[j-1] = args[j].exp[i];
        }
      }
      // If set of elements is true
      auto result = execute_application(proc,any_args,env);
      if(is_true(result)) return data_cast(result); // return element set
    }
    return G::FALSE_DATA_BOOLEAN; // else return false
  }

  data primitive_list_any_logic(scm_list& args, env_type& env, const char* format){
    scm_list list_exps;
    if(convert_lists_to_exp_matrix_and_return_if_empty(args,list_exps,"any",format))
      return G::FALSE_DATA_BOOLEAN;
    return primitive_STATIC_SEQUENCE_any_logic<types::exp>(list_exps,env,nullptr,"list",format);
  }


  // ************************ "every" helper ************************
  template <types SEQUENCE_TYPE, typename SEQUENCE_PTR>
  data primitive_STATIC_SEQUENCE_every_logic(scm_list& args, env_type& env, SEQUENCE_PTR seq_ptr, 
                                             const char* seq_name, const char* format){
    size_type min_length = confirm_proper_STATIC_SEQUENCE_any_every_args<SEQUENCE_TYPE>(args,
      seq_ptr, "every",format, seq_name);
    if(!min_length) return G::FALSE_DATA_BOOLEAN;
    const size_type total_sequences = args.size();
    scm_list& proc = args[0].exp;
    scm_list any_args(total_sequences-1);
    // For each element
    for(size_type i = 0; i < min_length; ++i){
      // In each sequence
      for(size_type j = 1; j < total_sequences; ++j) {
        if constexpr (SEQUENCE_TYPE == types::vec || SEQUENCE_TYPE == types::str) {
          any_args[j-1] = (args[j].*seq_ptr)->operator[](i);
        } else { // lists aren't passed via a smart pointer member
          any_args[j-1] = args[j].exp[i];
        }
      }
      // If set of elements is false
      auto result = execute_application(proc,any_args,env);
      if(!is_true(result))  return G::FALSE_DATA_BOOLEAN; // return false
      if(i+1 == min_length) return data_cast(result); // else return last <predicate> result
    }
    return G::FALSE_DATA_BOOLEAN; // else return false
  }

  data primitive_list_every_logic(scm_list& args, env_type& env, const char* format){
    scm_list list_exps;
    convert_lists_to_exp_matrix_and_return_if_empty(args,list_exps,"every",format);
    return primitive_STATIC_SEQUENCE_every_logic<types::exp>(list_exps,env,nullptr,"list",format);
  }

  /******************************************************************************
  * SORTING PRIMITIVE HELPERS: FOR LISTS, VECTORS, & STRINGS
  ******************************************************************************/

  // Convert the given AST sequence to a pair or vector
  data cast_ast_sequence_to_scheme(const types& seq_type, scm_list& sequence, 
                                   const char* name,      const char* format,
                                                          const scm_list& args){
    if(seq_type == types::vec) 
      return data(make_vec(sequence));
    if(seq_type == types::par) 
      return primitive_LIST_to_CONS_constructor(sequence.begin(),sequence.end());
    return mk_string_from_generated_chrs(sequence,args,name,format);
  }


  // Convert the given pair or vector to an AST sequence
  void cast_scheme_sequence_to_ast(data& scm_sequence,scm_list& sequence)noexcept{
    if(scm_sequence.type == types::vec) {
      sequence = *scm_sequence.vec;
    } else if(scm_sequence.type == types::par) {
      shallow_unpack_list_into_exp(scm_sequence,sequence);
    } else {
      for(const auto& ch : *scm_sequence.str)
        sequence.push_back(chr_type(ch));
    }
  }


  void primitive_confirm_sortable_sequence(scm_list& args, const char* name, 
                                                           const char* format){
    if(args.size() != 2)
      THROW_ERR('\''<<name<<" received incorrect # of args!"<<format<<FCN_ERR(name,args));
    primitive_confirm_data_is_a_procedure(args[0], name, format, args);
    is_proper_sequence(args[1],args,name,format);
  }


  // Sort the args[1] vector or list sequence using the args[0] procedure
  data primitive_sort_sequence(scm_list& args, env_type& env, const char* name, 
                                                              const char* format){
    scm_list sequence;
    const types seq_type = args[1].type;
    cast_scheme_sequence_to_ast(args[1],sequence);
    // sort unpacked sequence
    if(sequence.size() > 1) {
      std::sort(sequence.begin(), sequence.end(),
        [procedure=args[0].exp,env=std::move(env)]
        (data& lhs, data& rhs) mutable {
          scm_list args_list(2);
          args_list[0] = lhs, args_list[1] = rhs;
          return is_true_scm_condition(procedure,args_list,env);
        });
    }
    // return the sorted sequence
    return cast_ast_sequence_to_scheme(seq_type,sequence,name,format,args);
  }


  void primitive_MERGE_list_constructor(scm_list& curr_pairs, scm_list& proc, 
                                        scm_list& merged_list, env_type& env){
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
    scm_list args(2);
    args[0] = curr_pairs[0].par->first;
    args[1] = curr_pairs[1].par->first;
    if(is_true_scm_condition(proc,args,env)) {
      merged_list.push_back(args[0]);
      curr_pairs[0] = curr_pairs[0].par->second;
    } else {
      merged_list.push_back(args[1]);
      curr_pairs[1] = curr_pairs[1].par->second;
    }
    primitive_MERGE_list_constructor(curr_pairs, proc, merged_list, env);
  }


  data primitive_MERGE_vector_string_constructor(scm_list& args, scm_list& merged, 
                                                 env_type& env,  const char* format){
    scm_list& proc = args[0].exp;
    scm_list sequence1, sequence2;
    cast_scheme_sequence_to_ast(args[1],sequence1);
    cast_scheme_sequence_to_ast(args[2],sequence2);
    const size_type n1 = sequence1.size(), n2 = sequence2.size();
    size_type i = 0, j = 0;
    // Merge sequences
    for(; i < n1 && j < n2;) {
      scm_list eq_args(2);
      eq_args[0] = sequence1[i], eq_args[1] = sequence2[j];
      if(is_true_scm_condition(proc,eq_args,env))
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


  // Mutates 'sequence_target' by assigning its value to 'sequence_source'
  data mutatable_assign_scm_sequence(data& sequence_target, data&& sequence_source)noexcept{
    if(sequence_target.is_type(types::vec))
      *sequence_target.vec = *sequence_source.vec;
    else if(sequence_target.is_type(types::par))
      *sequence_target.par = *sequence_source.par;
    else
      *sequence_target.str = *sequence_source.str;
    return G::VOID_DATA_OBJECT;
  }


  // primitive "delete-neighbor-dups" & "delete-neighbor-dups!" helper template
  data primitive_DELETE_NEIGHBOR_DUPS_template(scm_list& args,     const char* name, 
                                               const char* format, const bool& mutating_deletion){
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm has a valid argument signature
    primitive_confirm_sortable_sequence(args,name,format);
    // return if deleting duplicates from the empty list
    if(args[1].is_type(types::sym)) {
      if(mutating_deletion) return G::VOID_DATA_OBJECT;
      return args[1];
    }
    // unpack sequence
    scm_list sequence;
    const auto seq_type = args[1].type;
    cast_scheme_sequence_to_ast(args[1],sequence);
    // return if deleting duplicates from an empty sequence
    if(sequence.empty()) {
      if(mutating_deletion) return G::VOID_DATA_OBJECT;
      return cast_ast_sequence_to_scheme(seq_type,sequence,name,format,args);
    }
    // rm duplicates from the sequence
    scm_list new_sequence(1,sequence[0]);
    auto& procedure = args[0].exp;
    for(size_type i=1, j=0, n = sequence.size(); i < n; ++i) {
      scm_list args_list(2);
      args_list[0] = new_sequence[j], args_list[1] = sequence[i];
      if(is_false_scm_condition(procedure,args_list,env))
        new_sequence.push_back(sequence[i]), ++j;
    }
    if(mutating_deletion)
      return mutatable_assign_scm_sequence(args[1],
        cast_ast_sequence_to_scheme(seq_type,new_sequence,name,format,args));
    return cast_ast_sequence_to_scheme(seq_type,new_sequence,name,format,args);
  }

  /******************************************************************************
  * EVAL/APPLY PRIMITIVE HELPERS
  ******************************************************************************/

  // [ EVAL ] Confirms whether given data is the AST's repn of ()
  bool data_is_the_empty_expression(const data& d)noexcept{
    return d.is_type(types::sym) && d.sym==symconst::emptylist;
  }


  // [ EVAL ] Confirms pair is the invocation of an argless procedure
  bool evaling_an_argless_procedure(const par_type& par)noexcept{
    return primitive_IS_THE_EMPTY_LIST(par->second);
  }


  // [ EVAL ] Converts the given pair into an expression ('deep' b/c it 
  //   also recursively converts ALL nested pairs into expressions too)
  void deep_unpack_list_into_exp(data& curr_pair, scm_list& args_list)noexcept{
    if(curr_pair.is_type(types::par)) {
      // Recursively unpack nested lists
      if(curr_pair.par->first.is_type(types::par)) {
        scm_list nested_list;
        deep_unpack_list_into_exp(curr_pair.par->first, nested_list);
        args_list.push_back(nested_list);
      // Convert the empty list to an empty expression
      } else if(data_is_the_empty_expression(curr_pair.par->first)) {
        args_list.push_back(scm_list());
      // Unpack atomic obj
      } else {
        args_list.push_back(curr_pair.par->first);
      }
      deep_unpack_list_into_exp(curr_pair.par->second, args_list); 
    } else if(!primitive_IS_THE_EMPTY_LIST(curr_pair)) {
      args_list.push_back(symconst::period);
      args_list.push_back(curr_pair);
    }
  }


  // [ EVAL ] Confirms correct # of args given
  void prm_EVAL_confirm_correct_number_of_args(scm_list& args, bool& must_reset_global_env, 
                          env_type& local_env, env_type& env, env_type& original_global_env, 
                                                      const char* name, const char* format){
    if(args.size()==2 && args[1].is_type(types::sym)) {
      if(args[1].sym == symconst::null_env) {
        must_reset_global_env = true;
        set_default_global_environment(), args.pop_back();
        env = G::GLOBAL_ENVIRONMENT_POINTER;
      } else if(args[1].sym == symconst::local_env) {
        env = local_env, args.pop_back();
      } else if(args[1].sym == symconst::global_env) {
        args.pop_back(); // global-environment is default
      } else {
        THROW_ERR('\''<<name<<" \""<<args[1].sym<<"\" isn't an evaluation environment:"
          << format << FCN_ERR(name, args));
      }
    }
    // confirm the correct # of arguments were passed
    if(args.size() != 1) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      THROW_ERR('\''<<name<<" received incorrect # of arguments:" << format << FCN_ERR(name, args));
    }
  }


  // [ CPS-EVAL ] Confirms correct # of args given
  void prm_CPS_EVAL_confirm_correct_number_of_args(scm_list& args, bool& must_reset_global_env, 
                                                   env_type& env, env_type& original_global_env, 
                                                         const char* name, const char* format){
    if(args.size()==2 && args[1].is_type(types::sym)) {
      if(args[1].sym == symconst::null_env) {
        must_reset_global_env = true;
        set_default_global_environment(), args.pop_back();
        env = G::GLOBAL_ENVIRONMENT_POINTER;
      } else if(args[1].sym == symconst::global_env) {
        env = G::GLOBAL_ENVIRONMENT_POINTER, args.pop_back();
      } else if(args[1].sym == symconst::local_env) {
        args.pop_back(); // local-environment is default
      } else { 
        THROW_ERR('\''<<name<<" \""<<args[1].sym<<"\" isn't an evaluation environment:"
          << format << FCN_ERR(name, args));
      }
    }
    // confirm the correct # of arguments were passed
    if(args.size() != 1) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      THROW_ERR('\''<<name<<" received incorrect # of arguments:" << format << FCN_ERR(name, args));
    }
  }


  // [ EVAL ] Confirms data needs no further evaluation
  bool prm_EVAL_data_is_self_evaluating(const data& d)noexcept{
    return d.is_type(types::num) || d.is_type(types::str) || 
           d.is_type(types::chr) || d.is_type(types::bol);
  }


  // [ EVAL ] converts scm list of quoted data to its AST repn
  scm_list prm_EVAL_convert_list_to_AST(data& par_data)noexcept{
    scm_list par_as_exp;
    deep_unpack_list_into_exp(par_data, par_as_exp);
    if(evaling_an_argless_procedure(par_data.par)) { // requires sentinel arg
      scm_list argless_call(2);
      argless_call[0] = data_cast(par_as_exp), argless_call[1] = symconst::sentinel_arg;
      par_as_exp = std::move(argless_call);
    }
    return par_as_exp;
  }


  // [ APPLY ] Converts the given pair into an expression ('shallow' b/c it 
  //   doesn't recursively convert ALL nested pairs into expressions too) 
  void shallow_unpack_list_into_exp(data& curr_pair, scm_list& args_list)noexcept{
    if(curr_pair.is_type(types::par)) {
      args_list.push_back(curr_pair.par->first);
      shallow_unpack_list_into_exp(curr_pair.par->second, args_list); 
    }
  }

  /******************************************************************************
  * FORCE-DELAY PRIMITIVE HELPERS
  ******************************************************************************/

  bool data_is_a_delay(const data& d)noexcept{
    return d.is_type(types::exp) && !d.exp.empty() &&
      d.exp[0].is_type(types::sym) && d.exp[0].sym==symconst::delay;
  }


  data force_data_delay(data& d) {
    if(!data_is_a_delay(d))
      THROW_ERR("'force "<<PROFILE(d)<<" isn't a delayed expression:\n     "
        "(force <delayed-expression>)"<<EXP_ERR("(force "<<d.write()<<')'));
    auto delay = d.exp[1].del;
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


  bool data_is_stream_pair(const data& d)noexcept{
    return d.is_type(types::par) && data_is_a_delay(d.par->first) && 
                                    data_is_a_delay(d.par->second);
  }


  bool data_is_stream(const data& d)noexcept{
    return data_is_the_empty_expression(d) || data_is_stream_pair(d);
  }


  void confirm_given_a_stream_pair_arg(scm_list& args, const char* name, 
                                                       const char* format){
    if(args.size() != 1)
      THROW_ERR('\''<<name<<" received incorrect # of args (given "
        << args.size() << "):" << format << FCN_ERR(name,args));
    if(!data_is_stream_pair(args[0]))
      THROW_ERR('\''<<name<<' '<<PROFILE(args[0])<<" isn't a stream-pair:" 
        << format << FCN_ERR(name,args));
  }


  // l-value scar & scdr
  data get_stream_data_car(data& d) {
    if(!data_is_stream_pair(d))
      THROW_ERR("'scar "<<PROFILE(d)<<" isn't a stream-pair:" 
        "\n     (scar <stream-pair>)" << 
        EXP_ERR("(scar " << d.write() << ')'));
    return force_data_delay(d.par->first);
  }


  data get_stream_data_cdr(data& d) {
    if(!data_is_stream_pair(d))
      THROW_ERR("'scdr "<<PROFILE(d)<<" isn't a stream-pair:\n     "
        "(scdr <stream-pair>)" << EXP_ERR("(scdr " << d.write() << ')'));
    data cdr_promise = force_data_delay(d.par->second);
    if(!data_is_stream(cdr_promise))
      THROW_ERR("'scdr forced cdr " << PROFILE(cdr_promise)
        << " isn't a stream:\n     (scdr <stream-pair>)" 
        << EXP_ERR("(scdr " << d.write() << ')'));
    return cdr_promise;
  }


  // r-value scar & scdr (for composition in the sc****r permutations)
  data get_stream_data_car(data&& d, const char * const name, 
                                     const char * const format,
                                     const size_type& nth_scar){
    if(!data_is_stream_pair(d))
      THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scar]
        <<" 'scar' "<<PROFILE(d)<<" isn't a stream-pair:" 
        <<format<<EXP_ERR("(scar "<<d.write()<<')'));
    return force_data_delay(d.par->first);
  }


  data get_stream_data_cdr(data&& d, const char * const name, 
                                     const char * const format,
                                     const size_type& nth_scdr){
    if(!data_is_stream_pair(d))
      THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scdr]
        <<" 'scdr' "<<PROFILE(d)<<" isn't a stream-pair:"
        <<format<<EXP_ERR("(scdr "<<d.write()<<')'));
    data cdr_promise = force_data_delay(d.par->second);
    if(!data_is_stream(cdr_promise))
      THROW_ERR('\''<<name<<STREAM_SCXXXXR_ACCESSOR_NUMBER[nth_scdr]
        <<" 'scdr's forced cdr "<<PROFILE(cdr_promise)
        <<" isn't a stream:"<<format<<EXP_ERR("(scdr "<<d.write()<<')'));
    return cdr_promise;
  }


  // "stream" special form helper fcn: recursively constructs embedded sconses
  data primitive_STREAM_to_SCONS_constructor(const scm_node& obj, 
                                             const scm_node& null_obj,
                                             env_type& env)noexcept{
    if(obj == null_obj) {
      scm_list empty_list(1,symconst::list);
      return empty_list; // becomes '() once forced
    }
    data new_stream_pair = data(make_par());
    new_stream_pair.par->first  = make_delay(scm_list_cast(*obj),env);
    new_stream_pair.par->second = make_delay(
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
    if(count == G::MAX_SIZE_TYPE) {
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
    primitive_confirm_only_given_streams(curr_streams,name,format,1,scm_list());
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
    execute_application(proc,args,env);
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
                                       scm_list& substream, size_type count=0){
    if(count < n && data_is_stream_pair(curr_pair)) {
      substream.push_back(get_stream_data_car(curr_pair));
      primitive_TAKE_SUBSTREAM_seeker(get_stream_data_cdr(curr_pair),n,substream,count+1);
    }
  }


  data primitive_DROP_WHILE_ctor(data&& curr_pair, scm_list& proc, env_type& env){
    if(!data_is_stream_pair(curr_pair))
      return std::move(curr_pair);
    scm_list args(1,get_stream_data_car(curr_pair));
    if(is_false_scm_condition(proc,args,env))
      return std::move(curr_pair);
    return primitive_DROP_WHILE_ctor(get_stream_data_cdr(curr_pair),proc,env);
  }


  void primitive_TAKE_WHILE_ctor(data&& curr_pair, scm_list& proc, 
                                 scm_list& substream, env_type& env){
    if(!data_is_stream_pair(curr_pair)) return;
    scm_list args(1,get_stream_data_car(curr_pair));
    if(is_false_scm_condition(proc,args,env)) return;
    substream.push_back(args[0]);
    primitive_TAKE_WHILE_ctor(get_stream_data_cdr(curr_pair),proc,substream,env);
  }


  void primitive_STREAM_FOLD_accumulator(data&& curr_pair, scm_list& proc, 
                                         data& init_val,   env_type& env,
                                         const bool& folding_left){
    // Return if fully iterated through stream
    if(!data_is_stream_pair(curr_pair)) return;
    // Execute proc, accumulate result, & recurse down the rest of the lists
    if(folding_left) { // stream-fold is preorder
      scm_list args(2);
      args[0] = init_val, args[1] = get_stream_data_car(curr_pair);
      init_val = data_cast(execute_application(proc,args,env));
    }
    primitive_STREAM_FOLD_accumulator(get_stream_data_cdr(curr_pair),
                                      proc,init_val,env,folding_left);
    if(!folding_left) { // stream-fold-right is postorder
      scm_list args(2);
      args[0] = get_stream_data_car(curr_pair), args[1] = init_val;
      init_val = data_cast(execute_application(proc,args,env));
    }
  }


  data primitive_STREAM_FOLD_template(scm_list& args,     const char* name, 
                                      const char* format, const bool& folding_left){
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Convert given proper arg signature
    if(args.size() != 3)
      THROW_ERR('\''<<name<<" received incorrect # of args (given " << args.size() 
        << "):" << format << FCN_ERR(name,args));
    primitive_confirm_data_is_a_procedure(args[0], name, format, args);
    if(data_is_the_empty_expression(args[2])) // folding '() returns seed
      return args[1];
    if(!data_is_stream_pair(args[2]))
      THROW_ERR('\''<<name<<' '<<PROFILE(args[2])<<" isn't a stream:" 
        << format << FCN_ERR(name,args));
    // Apply the procedure on each elt of each list, & accumulate the result
    data init_val = args[1];
    primitive_STREAM_FOLD_accumulator(std::move(args[2]),args[0].exp,
                                      init_val,env,folding_left);
    return init_val; // return the accumulated value
  }

  // ---------------------------------------------------
  // Stream Take/Drop Primitive Data Validation Helpers:
  // ---------------------------------------------------

  void primitive_TEMPLATE_TAKE_DROP_VALIDATION(scm_list& args, const char* name, 
                                                               const char* format){
    if(args.size() != 2) 
      THROW_ERR('\''<<name<<" received incorrect # of args (given "
        << args.size() << "):" << format << FCN_ERR(name, args));
    // Confirm given a valid size
    if(!primitive_is_valid_index(args[1]))
      THROW_ERR('\''<<name<<' '<< PROFILE(args[1]) << " isn't a valid size!"
        << format << FCN_ERR(name, args));
    // Confirm given a stream
    if(!data_is_stream(args[0]))
      THROW_ERR('\''<<name<<' '<< PROFILE(args[0]) << " isn't a stream!"
        << format << FCN_ERR(name, args));
  }


  void primitive_TEMPLATE_TAKE_DROP_WHILE_VALIDATION(scm_list& args, 
                                                     const char* name, 
                                                     const char* format){
    if(args.size() != 2) 
      THROW_ERR('\''<<name<<" received incorrect # of args (given "
        << args.size() << "):" << format << FCN_ERR(name, args));
    // Confirm given a procedure
    primitive_confirm_data_is_a_procedure(args[0], name, format, args);
    // Confirm given a stream
    if(!data_is_stream(args[1]))
      THROW_ERR('\''<<name<<' '<< PROFILE(args[1]) << " isn't a stream!"
        << format << FCN_ERR(name, args));
  }

  /******************************************************************************
  * TYPE-COERCION PRIMITIVE HELPERS
  ******************************************************************************/

  // Determine whether input[i] is at a hex 
  // POSTCONDITION: i is returned if input[i] is _NOT_ at a hex 
  //                else, the index of the hex value's closing ':' is returned.
  size_type is_symbol_hex_val(size_type i, const size_type& n, const scm_string& input)noexcept{
    if(i < n-2 && input[i] == '\\' && input[i+1] == 'x') {
      auto j = i+2; // mv past the '\x' prefix
      while(input[j] && isalnum(input[j])) ++j;
      return (input[j] == ':') ? j : i;
    }
    return i;
  }


  // primitive "symbol->string" conversion helper
  scm_string convert_symbol_to_string(const scm_string& string_val)noexcept{
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
  scm_string convert_string_to_symbol(const scm_string& symbol_val)noexcept{
    if(symbol_val.empty()) return symbol_val;
    scm_string symbol_str;
    // Convert chars in the string to their symbolic versions
    for(const auto& ch : symbol_val) {
      if(isspace(ch)||ch == '('||ch == ')'||ch == '['||ch == ']'||ch == '{'||ch == '}'||
        ch == '`'||ch == '\''||ch == '"'||ch == ','||ch == ';'||ch == '\\'){
        char str[32];
        snprintf(str, 32, "\\x%X:", unsigned(ch));
        symbol_str += str;
      } else {
        symbol_str += ch;
      }
    }
    return symbol_str;
  }


  // primitive "number->string" conversion helper
  bool invalid_NUMBER_TO_STRING_args(const scm_list& args)noexcept{
    return !args[0].is_type(types::num) || 
            (args.size() > 1 && 
              (!args[1].is_type(types::num) || !args[1].num.is_integer() ||
                (args.size() == 3 && 
                  (!args[2].is_type(types::num) || !args[2].num.is_integer() || 
                                                   !args[2].num.is_pos()))));
  }

  // primitive "number->string" conversion helper
  bool no_NUMBER_TO_STRING_precision_change_needed(const scm_list& args, const scm_string& number_as_string)noexcept{
    // No change needed if non-inexact or in scientific notation
    return args.size() < 3 || !args[0].num.is_inexact() || 
      (args[1].num == 10 && number_as_string.find("e") != number_as_string.find("E"));
  }

  /******************************************************************************
  * READER ERROR INPUT PRIMITIVE HELPERS
  ******************************************************************************/

  // Reader error code enumeration
  enum class READER_ERROR {
    early_end_paren,    incomplete_string,     incomplete_expression, 
    incomplete_comment, quoted_end_of_buffer,  quoted_end_of_expression,
    quoted_space, 
  };


  // Confirm reader error is a non-repl-specific fatal error
  constexpr bool is_non_repl_reader_error(const READER_ERROR& err)noexcept{
    return err == READER_ERROR::incomplete_string || 
           err == READER_ERROR::incomplete_expression || 
           err == READER_ERROR::incomplete_comment;
  }


  // Alert error as per read's throw
  void alert_reader_error(FILE* outs, const READER_ERROR& read_error, const scm_string& input)noexcept{
    if(read_error == READER_ERROR::early_end_paren) {
      if(G::USING_ANSI_ESCAPE_SEQUENCES) {
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
      if(G::USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------------------------------\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Reader Macro Around a Non-Expression (Unexpected End of Buffer)!\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------------------------------\x1b[0m\n", outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m------------------------------------------------------------------------------\x1b[0m\n", outs);
      } else {
        fputs("\n-----------------------------------------------------------------------------\n", outs);
        fputs("READ ERROR: Reader Macro Around a Non-Expression (Unexpected End of Buffer)!\n", outs);
        fputs("-----------------------------------------------------------------------------\n", outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("------------------------------------------------------------------------------\n", outs);
      }
    } else if(read_error == READER_ERROR::quoted_end_of_expression) {
      if(G::USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------------------------------\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Reader Macro Around a Non-Expression (Unexpected Closing Paren)!\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------------------------------\x1b[0m\n", outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m------------------------------------------------------------------------------\x1b[0m\n", outs);
      } else {
        fputs("\n-----------------------------------------------------------------------------\n", outs);
        fputs("READ ERROR: Reader Macro Around a Non-Expression (Unexpected Closing Paren)!\n", outs);
        fputs("-----------------------------------------------------------------------------\n", outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("------------------------------------------------------------------------------\n", outs);
      }
    } else if(read_error == READER_ERROR::quoted_space) {
      if(G::USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------------------\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Reader Macro Around a Non-Expression (Unexpected Space)!\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------------------\x1b[0m\n", outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m----------------------------------------------------------------------\x1b[0m\n", outs);
      } else {
        fputs("\n---------------------------------------------------------------------\n", outs);
        fputs("READ ERROR: Reader Macro Around a Non-Expression (Unexpected Space)!\n", outs);
        fputs("---------------------------------------------------------------------\n", outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("----------------------------------------------------------------------\n", outs);
      }
    }
  }

  void alert_non_repl_reader_error(FILE* outs, const READER_ERROR& read_error, const scm_string& input)noexcept{
    if(read_error == READER_ERROR::incomplete_string) {
      if(G::USING_ANSI_ESCAPE_SEQUENCES) {
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
    } else if(read_error == READER_ERROR::incomplete_expression) {
      if(G::USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Incomplete Expression, Missing a Closing ')'!\x1b[0m\n",outs);
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
    } else if(read_error == READER_ERROR::incomplete_comment) {
      if(G::USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------------------\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Incomplete Comment, Missing a Closing \"|#\"!\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------------------\x1b[0m\n",outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m-------------------------------------------------------\x1b[0m\n",outs);
      } else {
        fputs("\n-------------------------------------------------------\n",outs);
        fputs("READ ERROR: Incomplete Comment, Missing a Closing \"|#\"!\n",outs);
        fputs("-------------------------------------------------------\n",outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("-------------------------------------------------------\n",outs);
      }
    }
  }

  void alert_reader_error(FILE* outs, const size_type& read_error_index, const scm_string& input)noexcept{
    if(G::USING_ANSI_ESCAPE_SEQUENCES) {
      fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------\x1b[0m\n", outs);
      fprintf(outs,"\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Unparsable Type In Expression At Index = %03zu\x1b[0m\n",read_error_index);
      fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------\x1b[0m\n", outs);
      fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
      fputs("\x1b[1m--------------------------------------------------------\x1b[0m\n", outs);
    } else {
      fputs("\n--------------------------------------------------------\n", outs);
      fprintf(outs,"READ ERROR: Unparsable Type In Expression At Index = %03zu\n",read_error_index);
      fputs("--------------------------------------------------------\n", outs);
      fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
      fputs("--------------------------------------------------------\n", outs);
    }
  }

  /******************************************************************************
  * OUTPUT PRIMITIVE HELPERS
  ******************************************************************************/

  // Confirm character c is an escaped character
  constexpr bool is_escapable_char(const char& c)noexcept{
    return c=='\''||c=='"'||c=='?'||c=='\\'||c=='a'||
           c=='b' ||c=='f'||c=='n'||c=='r'||c=='t' ||
           c=='v';
  }


  // Constexpr isxdigit
  constexpr bool ishexdigit(const char& c)noexcept{
    return (c>='0' && c<='9')||(c>='A' && c<='F')||(c>='a' && c<='f');
  }


  // Confirm char is an escape hexadecimal char
  constexpr bool is_hex_escape(const char& c, const char& c2)noexcept{
    return c=='x' && ishexdigit(c2);
  }


  // Confirm char is an escape octal char
  constexpr bool is_oct_escape(const char& c)noexcept{
    return c>='0' && c<='7';
  }


  // Retrieve the special character variant of the given escaped character
  constexpr char special_char_variant(const char& c)noexcept{
    switch(c) {
      case 'a': return '\a'; case 'b': return '\b';
      case 'f': return '\f'; case 'n': return '\n';
      case 'r': return '\r'; case 't': return '\t';
      case 'v': return '\v'; default:  return c;
    }
  }


  // Inserts the unescaped hex/oct as a char & rm's its escaped representation
  void insert_hex_oct_escaped_char(scm_string& unescaped, const scm_string& str,size_type& i,const int& base)noexcept{
    unescaped += char(std::stoi(str.substr(i), nullptr, base));
    ++i;
    const auto is_parsed_digit = (base == 16) ? ishexdigit : is_oct_escape;
    while(str[i] && is_parsed_digit(str[i])) ++i;
    --i; // account for 'unescape_chars's for-loop ++i
  }


  // Unescape escaped special characters in the given string (ie "\\n" => "\n")
  scm_string unescape_chars(const scm_string& str)noexcept{
    scm_string unescaped; 
    unescaped.reserve(str.size());
    for(size_type i = 0; i < str.size(); ++i) {
      if(str[i] == '\\' && str[i+1]) {
        if(is_escapable_char(str[i+1])) {
          ++i;
          unescaped += special_char_variant(str[i]);
        } else if(is_hex_escape(str[i+1],str[i+2])) {
          i += 2;
          insert_hex_oct_escaped_char(unescaped,str,i,16);
        } else if(is_oct_escape(str[i+1])) {
          ++i;
          insert_hex_oct_escaped_char(unescaped,str,i,8);
        } else {
          unescaped += str[i];
        }
      } else {
        unescaped += str[i];
      }
    }
    return unescaped;
  }


  // Escape the given char <c>
  // PRECONDITION: c == '"' || c == '\\' || !isprint(c)
  scm_string escaped_char(int c)noexcept{
    c %= 256;
    if(c < 0) c += 256;
    switch(c) {
      case '"':  return "\\\"";
      case '\\': return "\\\\";
      case '\a': return "\\a";
      case '\b': return "\\b";
      case '\f': return "\\f";
      case '\n': return "\\n";
      case '\r': return "\\r";
      case '\t': return "\\t";
      case '\v': return "\\v";
      default: // Escape non-printable chars that are NOT one of the above in hex
        auto left_digit = (c/16), right_digit = (c%16);
        char hex_str[5] = "\\x";
        hex_str[2] = char((left_digit>9?'a'+left_digit-10:'0'+left_digit));
        hex_str[3] = char((right_digit>9?'a'+right_digit-10:'0'+right_digit)); 
        hex_str[4] = 0;
        return scm_string(hex_str,4);
    }
  }


  // Escape special characters in the given string (ie "\n" => "\\n")
  scm_string escape_chars(const scm_string& str)noexcept{
    scm_string escaped;
    for(size_type i = 0, n = str.size(); i < n; ++i) {
      if(str[i] != '"' && str[i] != '\\' && isprint(str[i]))
        escaped += char(str[i]);
      else
        escaped += escaped_char(str[i]);
    }
    return escaped;
  }


  // Confirm given valid output args
  //  => NOTE: 'total_non_port_args' also doubles as the index of 
  //           where the port/string would be if it were given as an arg
  //  => NOTE: returns whether writing to a port (instead of a string)
  bool confirm_valid_output_args(const scm_list& args, FILE*& outs, 
                                 const size_type& total_non_port_args,
                                 const char* name, const char* format){
    // Confirm given enough non-port args if given no port/string
    if(args.size()==total_non_port_args) return true;
    // Confirm given an open output port if received the optional port/string arg
    if(args.size()==total_non_port_args+1) {
      if(args[total_non_port_args].is_type(types::fop) && 
         args[total_non_port_args].fop.is_open()) {
        outs = args[total_non_port_args].fop.port(); 
        return true;
      } else if(args[total_non_port_args].is_type(types::str)) {
        return false;
      } else {
        THROW_ERR('\''<< name <<" arg "<< PROFILE(args[total_non_port_args])
          << " isn't an open output port: "<< format << FCN_ERR(name,args));
      }
    }
    THROW_ERR('\''<<name<<" received incorrect # of args: "<<format<<FCN_ERR(name,args));
    return true;
  }


  data primitive_display_port_logic(const data& obj, FILE*& outs)noexcept{
    if(obj.is_type(types::chr)) {
      fputc(obj.chr, outs);
      if(obj.chr == '\n') // account for printing a newline
        G::LAST_PRINTED_NEWLINE_TO_STDOUT = (outs == stdout);
    } else if(obj.is_type(types::str)) {
      if(!obj.str->empty()) {
        fputs(obj.str->c_str(), outs);
        if(*obj.str->rbegin() == '\n') // account for printed newlines
          G::LAST_PRINTED_NEWLINE_TO_STDOUT = (outs == stdout);  
      }
    } else if(!obj.is_type(types::dne)) {
      fputs(obj.display().c_str(), outs);
    }
    fflush(outs);
    G::LAST_PRINTED_TO_STDOUT = (outs == stdout);
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * INPUT PRIMITIVE HELPERS
  ******************************************************************************/

  // Confirm given valid input args, & return whether at the input file's EOF
  bool confirm_valid_input_args_and_non_EOF(const scm_list& args, FILE*& ins, 
                                            const char* name, bool& reading_stdin,
                                                              bool& reading_string){
    if(!args.empty()) {
      if(args.size() == 1) {
        if((reading_string = args[0].is_type(types::str))) {
          reading_stdin = false;
          return !args[0].str->empty();
        }
        if(args[0].is_type(types::fip) && args[0].fip.is_open()) {
          ins = args[0].fip.port();
          // NOT reading from stdin requires a specialized parsing algorithm
          if(ins != stdin) {
            if(feof(ins)) return false;
            reading_stdin = false; 
          }
        } else {
          THROW_ERR('\''<< name <<" arg " << PROFILE(args[0]) <<
            " isn't an open input port:"
            "\n     ("<< name <<" <optional-open-input-port-or-string>)" <<
            FCN_ERR(name,args));
        }
      } else {
        THROW_ERR('\''<< name <<" received incorrect # of args:" 
          "\n     ("<< name <<" <optional-open-input-port-or-string>)" <<
          FCN_ERR(name,args));
      }
    }
    return true;
  }


  data primitive_read_from_input_port_logic(FILE*& outs, FILE*& ins, const bool& reading_stdin, 
                                                                                env_type& env){
    // Read input
    scm_list read_data(2);
    read_data[0] = symconst::quote;
    if(reading_stdin) {
      if(auto read_result = read_user_input(outs,ins,false); read_result.empty()) {
        read_data[1] = G::VOID_DATA_OBJECT;
      } else {
        read_data[1] = std::move(read_result[0]);
      }
    } else {
      read_data[1] = primitive_read_from_port(outs,ins)[0];
    }
    return data_cast(scm_eval(std::move(read_data), env));
  }


  data primitive_read_from_string_logic(scm_string& outs_str, env_type& env){
    try {
      scm_list read_data;
      // attempt to parse an AST expression from the given string
      parse_input_exp(scm_string(outs_str),read_data);
      if(read_data.empty()) return symconst::emptylist;
      // remove the parsed portion from the original string
      prepare_string_for_AST_generation(outs_str);
      size_type i = 0; // also trim prepending whitespace
      for(size_type n = outs_str.size(); i < n && isspace(outs_str[i]); ++i);
      outs_str.erase(0,i);
      outs_str.erase(0,read_data[0].write().size());
      // return the parsed AST
      scm_list quoted_read_data(2);
      quoted_read_data[0] = symconst::quote, quoted_read_data[1] = std::move(read_data[0]);
      return data_cast(scm_eval(std::move(quoted_read_data),env));
    // throw error otherwise & return void data
    } catch(const READER_ERROR& read_error) {
      if(is_non_repl_reader_error(read_error))
        alert_non_repl_reader_error(G::CURRENT_OUTPUT_PORT,read_error,outs_str);
      else
        alert_reader_error(G::CURRENT_OUTPUT_PORT,read_error,outs_str);
      fflush(G::CURRENT_OUTPUT_PORT);
      throw SCM_EXCEPT::READ;
    } catch(const size_type& read_error_index) {
      alert_reader_error(G::CURRENT_OUTPUT_PORT,read_error_index,outs_str);
      fflush(G::CURRENT_OUTPUT_PORT);
      throw SCM_EXCEPT::READ;
    }
  }

  /******************************************************************************
  * READ PORT PRIMITIVE HELPERS
  ******************************************************************************/

  namespace read_port {
    bool input_ends_with_postfix(const scm_string& postfix, const scm_string& input)noexcept{
      return input.size() >= postfix.size() && input.compare(input.size()-postfix.size(),postfix.size(),postfix) == 0;
    }


    size_type input_ends_with_reader_macro_or_quote(const scm_string& input)noexcept{
      for(const auto& reader_macro_shorthand : G::SHORTHAND_READER_MACRO_REGISTRY)
        if(input_ends_with_postfix(reader_macro_shorthand,input))
          return reader_macro_shorthand.size();
      return 0;
    }


    // Determine the read char's length (for reading from a port)
    // PRECONDITION: input[i] = the 1st char after the '#\' of the char literal
    size_type character_length(const size_type& i, const scm_string& input)noexcept{
      if(auto [ch, name] = data_is_named_char(i,input); !name.empty())
        return name.size() + (input[i] == 'x' && isxdigit(input[i+1]));
      return 1;
    }


    // Parse a char literal from "ins", rm excess letters from "input", & 
    //   mv "ins" back to be 1 char past the end of the char literal
    void parse_char(FILE*& ins, scm_string& input)noexcept{
      const size_type char_start = input.find("#\\")+2;
      if(char_start+2 > input.size()) return; // if parsing a single/null character
      const size_type char_length = character_length(char_start,input);
      if(input.size()-(char_start+char_length) > 0) {
        const size_type excess_read_length = input.size()-(char_start+char_length);
        fseek(ins, -excess_read_length, SEEK_CUR);    // mv "ins" back
        input.erase(input.size()-excess_read_length); // erase the excess parsed
      }
    }


    // Confirm improper use for any quotation/reader-macro shorthand
    bool improper_quotation(scm_string& input)noexcept{
      // Not enough room -> false
      if(input.size() <= 1) return false;
      auto last_ch = *input.rbegin();
      // Not improper ending -> false
      if(!IS_CLOSE_PAREN(last_ch) && !isspace(last_ch)) return false;
      input.pop_back(); // rm last char to check for quote/reader-macro
      bool ends_with_reader_macro_or_quote = input_ends_with_reader_macro_or_quote(input);
      input.push_back(last_ch); // add last char back in
      return ends_with_reader_macro_or_quote;
    }


    // Confirm quotation/reader-macro shorthand found at the end of the file
    bool improper_EOF_quotation(const scm_string& input)noexcept{
      return !input.empty() && input_ends_with_reader_macro_or_quote(input) &&
             !(input.size()>2 && *(input.rbegin()+2)=='#' && *(input.rbegin()+1)=='\\');
    }


    // Confirm just appended a valid open/close paren to 'input'
    bool is_valid_open_exp(const scm_string &input,  const size_type& paren_count,
                           const bool& possible_vect,const bool& found_nonquote_data)noexcept{
      return IS_OPEN_PAREN(*input.rbegin()) && (input.size() < 3 || 
                                                *(input.rbegin()+2) != '#' || 
                                                *(input.rbegin()+1) != '\\')
                                            && (paren_count || 
                                                !found_nonquote_data || 
                                                possible_vect);
    }


    bool is_valid_close_exp(const scm_string &input)noexcept{
      return IS_CLOSE_PAREN(*input.rbegin()) && (input.size() < 3 || 
                                                 *(input.rbegin()+2) != '#' || 
                                                 *(input.rbegin()+1) != '\\');
    }


    // Confirm input is _not_ made up only of nested reader macros
    bool found_non_macro_prior_macro(const scm_string& input) {
      const auto n = input.size();
      size_type i = n, nested_length = 0;
      // Skip past to last expression
      while(i-- > 1) if(IS_END_OF_WORD(input[i-1],input[i])) break;
      if(i) return false; // #f if symbol didn't start at front of input
      // Confirm series of symbols = nested reader macros
      while(i < n) {
        if((nested_length = is_expandable_reader_macro(input,i))) {
          i += nested_length;
        } else {
          return false;
        }
      }
      return true;
    }
  } // -- End namespace read_port



  // Read from a non-stdin port
  // PRECONDITION: feof(ins) MUST RETURN false
  scm_list primitive_read_from_port(FILE* outs, FILE* ins) {
    // input parsing variables & status trackers
    scm_string input;
    int ch;
    bool in_a_string   = false, possible_vect=false,  found_nonquote_data=false;
    bool possible_char = false, confirmed_char=false, parsing_a_char=false;
    bool possible_multi_line_comment_end=false, in_a_single_line_comment=false;
    bool possible_multi_line_comment=false,     in_a_multi_line_comment=false;
    size_type paren_count = 0, macro_length = 0;
    fflush(outs);
    // parse the input port's scheme code
    while((ch = fgetc(ins)) != EOF) {
      // don't include text of a multi-line comment
      if(possible_multi_line_comment) {
        possible_multi_line_comment = false;
        if(ch == '|') {
          possible_vect = possible_char = false; // confirmed neither char nor vector (@ a comment)
          in_a_multi_line_comment = true;
          input.erase(input.end()-1);
          found_nonquote_data = !input.empty(); // if found non-quote data
          continue;
        }
      }
      if(possible_multi_line_comment_end) {
        possible_multi_line_comment_end = false;
        if(ch == '#') {
          in_a_multi_line_comment = false;
          continue;
        }
      }
      if(in_a_multi_line_comment) {
        possible_multi_line_comment_end = (ch == '|');
        continue;
      }

      // don't include text of a single-line comment
      if(in_a_single_line_comment) {
        in_a_single_line_comment = (ch != '\n');
        continue;
      }
      if(!in_a_string && !confirmed_char && ch == ';') {
        in_a_single_line_comment = true;
        continue;
      }

      // check for whether at a potential multi-line comment
      possible_multi_line_comment = (!in_a_string && ch == '#');

      // don't include prefixing whitespace
      if(input.empty() && isspace(ch)) continue;

      // append the character
      input += ch;

      // continue to get the quoted data
      if((macro_length = read_port::input_ends_with_reader_macro_or_quote(input))) {
        // if macro reader symbol is in middle of another symbol (ie e'e)
        //   return back to the previous non-macro symbol & stop parsing
        //   IFF not in the middle of an expression (since only reading 1 
        //   expression at a time)
        found_nonquote_data = false; // flags to keep seeking after current macro
        if(!paren_count) {
          if(input.size() == macro_length) {
            continue;
          } else if(read_port::found_non_macro_prior_macro(input)) {
            fseek(ins, -macro_length, SEEK_CUR); // mv "ins" back
            input.erase(input.size()-macro_length);
            break;
          }
        }
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
      if(possible_vect && !IS_OPEN_PAREN(ch)) possible_vect = false;

      // check if at a string
      if(ch == '"' && (in_a_string || paren_count || !found_nonquote_data)) {
        if(!in_a_string)
          in_a_string = true;
        else if(is_non_escaped_double_quote(input.size()-1,input)) {
          in_a_string = false;
          if(!paren_count) break;
        }
      }

      // check if at an expression or vector => PARSES BOTH!
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
        if(IS_CLOSE_PAREN(ch)) 
             alert_reader_error(outs,READER_ERROR::quoted_end_of_expression,input);
        else alert_reader_error(outs,READER_ERROR::quoted_space,input);
        throw SCM_EXCEPT::READ;
      }

      // continue parsing if in an expression
      else if(paren_count) continue;

      // check if at a char, vector
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

      found_nonquote_data = !input.empty(); // if found non-quote data
    } // -- End of parsing loop


    // If read an empty file
    if(input.empty()) {
      scm_list eof_char(1,chr_type(EOF));
      return eof_char;
    }

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
    if(args.size() != 1) 
      THROW_ERR('\''<<name<<" didn't receive any args: "<<format<<FCN_ERR(name,args));
    if(!args[0].is_type(types::str)) 
      THROW_ERR('\''<<name<<" arg "<<PROFILE(args[0])<<" isn't a string: "<<format
        << FCN_ERR(name,args));
  }


  // Confirm the given filename exists as a file
  bool confirm_file_exists(const char* filename)noexcept{
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
    bool exists = confirm_file_exists(filename.str->c_str());
    FILE* fp = fopen(filename.str->c_str(), file_open_type);
    // if INPUT, confirm file isnt null
    if(fp == nullptr && file_open_type[0] == 'r')
      THROW_ERR('\'' << name << " file \"" << *filename.str
        << "\" doesn't exist (invalid for input):"<<format<<FCN_ERR(name,args));
    // if OUTPUT, confirm file doesn't exist
    if((fp == nullptr || exists) && file_open_type[0] == 'w')
      THROW_ERR('\'' << name << " file \"" << *filename.str 
        << "\" already exists!" << format << FCN_ERR(name,args));
    // if OUTPUT, confirm file doesn't exist
    if((fp == nullptr || !exists) && file_open_type[0] == 'a')
      THROW_ERR('\'' << name << " file \"" << *filename.str 
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
    confirm_given_one_arg(args,name,"<port>");
    if(!args[0].is_type(types::fip) && !args[0].is_type(types::fop))
      THROW_ERR('\''<<name<<" arg "<<PROFILE(args[0])<<" isn't a port: "<<format<<FCN_ERR(name,args));
  }


  // Call an unary procedure with a file's port as its argument
  template<typename port_ctor,typename PORT_GETTER>
  data primitive_CALL_WITH_FILE(scm_list& args,     const char* name,
                                const char* format, PORT_GETTER get_port){
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm given a filename string & a procedure
    if(args.size() != 2)
      THROW_ERR('\'' << name << " received incorrect # of args:" 
        << format << FCN_ERR(name,args));
    primitive_confirm_data_is_a_procedure(args[1], name, format, args);
    // add file to the port registry
    G::PORT_REGISTRY.push_back(get_port(args[0],name,format,args));
    // apply the given procedure w/ a port to the file
    scm_list port_arg(1,port_ctor(G::PORT_REGISTRY.size()-1));
    return data_cast(execute_application(args[1].exp,port_arg,env));
  }


  // Call an argless procedure with a file's port as the default port
  template<typename PORT_GETTER>
  data primitive_WITH_FILE(scm_list& args,     const char* name,
                           const char* format, FILE*& DEFAULT_PORT,
                           PORT_GETTER get_port){
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm given a filename string & a procedure
    if(args.size() != 2)
      THROW_ERR('\'' << name << " received incorrect # of args:" 
        << format << FCN_ERR(name,args));
    primitive_confirm_data_is_a_procedure(args[1], name, format, args);
    // save & set the current port
    FILE* original_port = DEFAULT_PORT;
    DEFAULT_PORT = get_port(args[0], name, format, args);
    // apply the given procedure
    scm_list null_arg_val(2);
    null_arg_val[0] = symconst::quote, null_arg_val[1] = symconst::sentinel_arg;
    auto null_arg   = scm_eval(std::move(null_arg_val),env);
    auto result     = data_cast(execute_application(args[1].exp,null_arg,env));
    // reset the current port
    if(DEFAULT_PORT && DEFAULT_PORT != stdin && 
       DEFAULT_PORT != stdout && DEFAULT_PORT != stderr) fclose(DEFAULT_PORT);
    DEFAULT_PORT = original_port;
    return result;
  }

  /******************************************************************************
  * SYSTEM INTERFACE PRIMITIVE HELPERS
  ******************************************************************************/

  bool prm_cps_load_last_obj_is_EOF(const scm_list& AST)noexcept{
    return !AST.empty() && 
      ((AST.rbegin()->is_type(types::exp) && AST.rbegin()->exp.size()==1 && 
        AST.rbegin()->exp[0].is_type(types::chr) && AST.rbegin()->exp[0].chr==EOF) ||
       (AST.rbegin()->is_type(types::chr) && AST.rbegin()->chr==EOF));
  }


  // PRECONDITION: primitive_data_is_a_procedure(proc) == true
  // NOTE: Has no effect on primitive procedures
  void prm_set_procedure_INLINE_INVOCATION(scm_list& proc, const bool status)noexcept{
    if(proc[0].sym == symconst::procedure) proc[6].bol = status;
  }


  void primitive_LOAD_interpret_file_contents(scm_list& args, env_type& env, const char* format){
    // Load file contents
    if(args.size() != 1)
      THROW_ERR("'load recieved incorrect # of arguments!" << format << FCN_ERR("load", args));
    FILE* ins = confirm_valid_input_file(args[0],"load",format,args);
    size_type exp_count = 1;
    while(!feof(ins)) {
      // Try reading & evaluating an expression
      try {
        scm_eval(scm_list_cast(primitive_read_from_port(G::CURRENT_OUTPUT_PORT,ins)[0]),env);
        ++exp_count;
      // Catch, notify 'load' error occurred, & rethrow
      } catch(const SCM_EXCEPT& load_error) {
        if(load_error == SCM_EXCEPT::EVAL) {
          fprintf(stderr, "  %s>> Load Exception:%s\n     File \"%s\"\n     Expression #%zu\n%s", 
            afmt(AFMT_135), afmt(AFMT_01), args[0].str->c_str(), exp_count, afmt(AFMT_0));
        } else if(load_error == SCM_EXCEPT::READ) {
          fprintf(stderr, "%s>> Load Exception:%s\n   File \"%s\"\n   Expression #%zu\n%s", 
            afmt(AFMT_135), afmt(AFMT_01), args[0].str->c_str(), exp_count, afmt(AFMT_0));
        }
        if(ins) fclose(ins);
        throw load_error;
      }
    }
    if(ins) fclose(ins);
  }


  scm_list primitive_CPS_LOAD_interpret_file_contents(scm_list& args, env_type& env, const char* format){
    // Load file contents
    if(args.size() != 1)
      THROW_ERR("'cps-load recieved incorrect # of arguments!" << format << FCN_ERR("cps-load", args));
    FILE* ins = confirm_valid_input_file(args[0],"cps-load",format,args);
    size_type exp_count = 1;
    scm_list AST;
    while(!feof(ins)) {
      // Try reading & evaluating an expression
      try {
        AST.push_back(primitive_read_from_port(G::CURRENT_OUTPUT_PORT,ins)[0]);
        ++exp_count;
      // Catch, notify 'CPS-load' error occurred, & rethrow
      } catch(const SCM_EXCEPT& load_error) {
        if(load_error == SCM_EXCEPT::READ) {
          fprintf(stderr, "%s>> CPS-Load Exception:%s\n   File \"%s\"\n   Expression #%zu\n%s", 
            afmt(AFMT_135), afmt(AFMT_01), args[0].str->c_str(), exp_count, afmt(AFMT_0));
        }
        if(ins) fclose(ins);
        throw load_error;
      }
    }
    if(ins) fclose(ins);
    // Rm #!eof as last obj in AST (if present)
    if(prm_cps_load_last_obj_is_EOF(AST)) AST.pop_back();
    // Add (void) as only obj iff AST.empty()
    if(AST.empty()) {
      AST.push_back(scm_list(1));
      AST[0].exp[0] = "void";
    }
    // Wrap in scm->cps
    AST.insert(AST.begin(), symconst::scm_cps);
    return scm_eval(std::move(AST),env);
  }

  // -----------------
  // Compiler Helpers:
  // -----------------

  // Recursively generate assignments to vectors as a precomputed AST
  //   => NOTE: The reader's generated AST _ONLY_ contains 1 of 5 types:
  //            types::exp, types::str, types::sym, types::chr, & types::num
  void print_vector_data_assignment(const scm_list& expressions, scm_string& vector_assigns, 
                                    const scm_string& assignment_chain)noexcept{
    for(size_type i = 0, n = expressions.size(); i < n; ++i) {
      switch(expressions[i].type) {
        case types::exp: // EXPRESSION
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = heist::exp_type("+
            std::to_string(expressions[i].exp.size())+");\n";
          print_vector_data_assignment(expressions[i].exp,vector_assigns,
            assignment_chain+'['+std::to_string(i)+"].exp");
          break;
        case types::str: // STRING
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = heist::make_str(\""+ 
            escape_chars(*expressions[i].str) +"\");\n";
          break;
        case types::sym: // SYMBOL
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = \""+
            expressions[i].sym+"\";\n"; 
          break;
        case types::chr: // CHARACTER
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = heist::chr_type('"+
            std::to_string((char)expressions[i].chr)+"');\n"; 
          break;
        default:         // NUMBER
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = heist::num_type("+
            expressions[i].num.str()+");\n";
      }
    }
  }


  void primitive_read_file_being_compiled(scm_list& args, scm_list& expressions, 
                                          const char* name, const char* format){
    FILE* ins = confirm_valid_input_file(args[0],name,format,args);
    size_type exp_count = 1;
    while(!feof(ins)) {
      // Try reading an expression
      try {
        expressions.push_back(scm_list_cast(primitive_read_from_port(G::CURRENT_OUTPUT_PORT,ins)[0]));
        ++exp_count;
      // Catch, notify 'compile' error occurred, & rethrow
      } catch(const SCM_EXCEPT& compile_error) {
        if(compile_error == SCM_EXCEPT::READ)
          fprintf(stderr, "%s>> Compile Exception:%s\n   File \"%s\"\n   Expression #%zu\n%s", 
            afmt(AFMT_135), afmt(AFMT_01), args[0].str->c_str(), exp_count, afmt(AFMT_0));
        if(ins) fclose(ins);
        throw compile_error;
      }
    }
    expressions.pop_back(); // rm EOF character (not part of the source code)
    if(ins) fclose(ins);
  }


  scm_string primitive_generate_precompiled_AST(scm_list& expressions)noexcept{
    // Generate Vector Assignments to explicitly lay out a predetermined AST
    scm_string ast_generator = "heist::exp_type HEIST_PRECOMPILED_READ_AST_EXPS(" + 
                                std::to_string(expressions.size()) + ");\n"
                                "void POPULATE_HEIST_PRECOMPILED_READ_AST_EXPS(){\n";
    print_vector_data_assignment(expressions,ast_generator,"HEIST_PRECOMPILED_READ_AST_EXPS");
    return ast_generator + "}\n";
  }


  data primitive_write_compiled_file(scm_list& args, const scm_string& compiled_filename, 
                                                     const scm_string& ast_generator,
                                                     const char* name){
    FILE* outs = fopen(compiled_filename.c_str(), "w");
    if(outs == nullptr)
      THROW_ERR('\''<<name<<" file \"" << compiled_filename 
        << "\" couldn't be written to!\n     ("<<name<<" <filename-string>)"
        << FCN_ERR(name,args));
    fprintf(outs, "// Heist-Scheme Compiled Source from \"%s\""
                  "\n#include \"%s%cheist_interpreter_headers%cheist_types.hpp\""
                  "\n#define HEIST_INTERPRETING_COMPILED_AST"
                  "\n%s"
                  "\n#include \"%s%cheist_main.cpp\"\n", 
                  args[0].str->c_str(), 
                  HEIST_DIRECTORY_FILE_PATH,char(std::filesystem::path::preferred_separator),
                  char(std::filesystem::path::preferred_separator),
                  ast_generator.c_str(), 
                  HEIST_DIRECTORY_FILE_PATH,char(std::filesystem::path::preferred_separator));
    if(outs) fclose(outs);
    return G::VOID_DATA_OBJECT;
  }


  // Generates a custom or default named compiled file as needed
  data primitive_compile_dispatch(scm_list& args, scm_list& expressions, const char* name) {
    if(args.size() == 2)
      return primitive_write_compiled_file(args,*args[1].str,
        primitive_generate_precompiled_AST(expressions),name);
    return primitive_write_compiled_file(args,"HEIST_COMPILER_OUTPUT.cpp",
      primitive_generate_precompiled_AST(expressions),name);
  }


  // General Layout for Compilation Primitives (For Both CPS-Style & Not)
  data primitive_COMPILE_TEMPLATE(scm_list& args,const char* name,const char* format,const bool cps_style){
    // Read File & Generate an AST-construction string
    if(args.empty() || args.size() > 2) 
      THROW_ERR('\''<<name<<" received incorrect # of args!"
        "\n     ("<<name<<" <filename-string> <optional-compiled-filename>)"<<FCN_ERR(name,args));
    if(args.size() == 2 && !args[1].is_type(types::str))
      THROW_ERR('\''<<name<<" 2nd arg "<<PROFILE(args[1])<<" wasn't a string!"
        "\n     ("<<name<<" <filename-string> <optional-compiled-filename>)"<<FCN_ERR(name,args));
    scm_list expressions;
    primitive_read_file_being_compiled(args,expressions,name,format);
    if(cps_style) {
      scm_list wrapped_exps(1);
      wrapped_exps[0] = scm_list(2);
      wrapped_exps[0].exp[0] = scm_list(1+expressions.size());
      wrapped_exps[0].exp[0].exp[0] = symconst::scm_cps;
      std::move(expressions.begin(),expressions.end(),wrapped_exps[0].exp[0].exp.begin()+1);
      wrapped_exps[0].exp[1] = "id";
      return primitive_compile_dispatch(args,wrapped_exps,name);
    }
    return primitive_compile_dispatch(args,expressions,name);
  }

  /******************************************************************************
  * INTERPRETER GLOBAL-SETTING MANIPULATION PRIMITIVE HELPER
  ******************************************************************************/

  data primitive_TOGGLE_DISPLAY_SETTING(scm_list& args, const char* name, bool& setting){
    if(args.size() > 1)
      THROW_ERR('\'' << name << " received incorrect # of args:\n     ("
        << name << " <optional-bool>)" << FCN_ERR(name,args));
    bool original_setting_status = !setting;
    setting = false;
    if(!args.empty()) setting = !is_true(args);
    return boolean(original_setting_status);
  }

  data primitive_TOGGLE_NUMERIC_SETTING(scm_list& args, const char* name, size_type& SETTING){
    if(args.size() != 1 || !args[0].is_type(types::num) || 
       !args[0].num.is_integer() || !args[0].num.is_pos())
      THROW_ERR('\''<<name<<" didn't receive a positive integer arg:"
        "\n     ("<<name<<" <positive-integer>)"
        "\n     <positive-integer>: (0, " << G::MAX_SIZE_TYPE << ']'
        << FCN_ERR(name,args));
    auto float_num = args[0].num.to_inexact();
    if(float_num < 0 || float_num > G::MAX_SIZE_TYPE)
      THROW_ERR('\''<<name<<" integer arg is out of bounds!"
        "\n     ("<<name<<" <positive-integer>)"
        "\n     <positive-integer>: (0, " << G::MAX_SIZE_TYPE << ']'
        << FCN_ERR(name,args));
    // Set the new setting value, & return the original
    auto original = SETTING;
    SETTING = (size_type)float_num.extract_inexact();
    return num_type(original);
  }

  /******************************************************************************
  * ERROR HANDLING PRIMITIVE HELPERS
  ******************************************************************************/

  void confirm_valid_error_primitive_layout(const scm_list& args, const char* name){
    if(args.size() < 2)
    THROW_ERR('\''<<name<<" requires at least 2 args: a SYMBOL to represent the "
      "errorful entity & a STRING explaining the error!\n     ("<<name<<
      " <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << FCN_ERR(name, args));
  if(!args[0].is_type(types::sym))
    THROW_ERR('\''<<name<<" requires 1st arg "<<PROFILE(args[0])<<" to be a "
      "SYMBOL to represent the errorful entity!\n     ("<<name<<
      " <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << FCN_ERR(name, args));
  if(!args[1].is_type(types::str))
    THROW_ERR('\''<<name<<" requires its 2nd arg "<<PROFILE(args[1])<<
      " to be a STRING explaining the error!\n     ("<<name<<
      " <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << FCN_ERR(name, args));
  }


  void primitive_error_template(const scm_list& args, const char* name, 
                                const char* err_type, const afmts& err_format){
    confirm_valid_error_primitive_layout(args, name);
    // Cook the raw error string
    scm_string error_str(*args[1].str);
    // Alert error
    fprintf(G::CURRENT_OUTPUT_PORT, 
            "\n%s%s%s in %s: %s", afmt(err_format), err_type, afmt(AFMT_01), 
            args[0].sym.c_str(), error_str.c_str());
    // Check for irritants (if provided, these are optional)
    if(args.size() == 3)
      fprintf(G::CURRENT_OUTPUT_PORT, " with irritant %s", args[2].write().c_str());
    else if(args.size() > 3) {
      scm_list irritant_list(args.begin()+2, args.end());
      fprintf(G::CURRENT_OUTPUT_PORT, " with irritants %s", 
              primitive_LIST_to_CONS_constructor(irritant_list.begin(),
                                                 irritant_list.end()
                                                ).write().c_str());
    }
    fprintf(G::CURRENT_OUTPUT_PORT, "%s\n", afmt(AFMT_0));
    fflush(G::CURRENT_OUTPUT_PORT);
    throw SCM_EXCEPT::EVAL;
  }

  /******************************************************************************
  * MACRO EXPANSION HELPERS
  ******************************************************************************/

  data prm_recursively_deep_expand_macros(data&,env_type&);


  // Expand a confirmed pair & w/ symbol as the 1st arg
  data prm_shallow_expand_pair(data& d, env_type& env) {
    scm_list expanded, par_as_exp = prm_EVAL_convert_list_to_AST(d);
    if(!par_as_exp.empty() && par_as_exp[0].is_type(types::sym) && 
       expand_macro_if_in_env(par_as_exp[0].sym,scm_list(par_as_exp.begin()+1,par_as_exp.end()),env,expanded)){
      auto quoted = scm_list(2);
      quoted[0] = symconst::quote;
      quoted[1] = std::move(expanded);
      return data_cast(scm_eval(std::move(quoted),env));
    }
    return G::FALSE_DATA_BOOLEAN;
  }


  // Expand expression
  data prm_recursively_deep_expand_macros_exp(data& d,env_type& env) {
    if(!d.is_type(types::par)) return d;
    data pair = data(make_par());
    if(d.par->first.is_type(types::par))
      pair.par->first = prm_recursively_deep_expand_macros(d.par->first,env);
    else
      pair.par->first = d.par->first;
    pair.par->second = prm_recursively_deep_expand_macros_exp(d.par->second,env);
    return pair;
  }


  // Expand data
  data prm_recursively_deep_expand_macros(data& d,env_type& env) {
    if(!d.is_type(types::par)) return d;
    auto expanded = prm_shallow_expand_pair(d,env);
    if(!expanded.is_type(types::bol) || expanded.bol.val)
      return prm_recursively_deep_expand_macros(expanded,env);
    return prm_recursively_deep_expand_macros_exp(d,env);
  }

  /******************************************************************************
  * GENSYM PRIMITIVE HELPERS
  ******************************************************************************/

  // Returns a freshly generated gensym symbol
  scm_string new_hashed_gensym_arg()noexcept{
    if(G::GENSYM_HASH_IDX_1 != G::MAX_SIZE_TYPE)
      return symconst::gensym_prefix + std::to_string(G::GENSYM_HASH_IDX_2) + 
                                 '_' + std::to_string(G::GENSYM_HASH_IDX_1++);
    return symconst::gensym_prefix + std::to_string(++G::GENSYM_HASH_IDX_2) + 
                               '_' + std::to_string(G::GENSYM_HASH_IDX_1++);
  }


  // If given an arg, return the nth previously generated gensym symbol
  scm_string decremented_hashed_gensym_arg(size_type n, const scm_list& args){
    static constexpr const char * const format = 
      "\n     (gensym <optional-instance-#-to-reference>)"
      "\n     => (gensym n) refers to the symbol generated by the nth last (gensym) call!";
    if(!G::GENSYM_HASH_IDX_2 && G::GENSYM_HASH_IDX_1 < n)
      THROW_ERR("'gensym less than " << n << " gensym instances have been generated!"
        << format << FCN_ERR("gensym",args));
    if(G::GENSYM_HASH_IDX_1 < n) {
      n -= G::GENSYM_HASH_IDX_1;
      return symconst::gensym_prefix + std::to_string(G::GENSYM_HASH_IDX_2 - 1) + 
                                 '_' + std::to_string(G::MAX_SIZE_TYPE - n);
    } else {
      return symconst::gensym_prefix + std::to_string(G::GENSYM_HASH_IDX_2) + 
                                 '_' + std::to_string(G::GENSYM_HASH_IDX_1 - n);
    }
  }

  /******************************************************************************
  * SYNTAX MUTATING PRIMITIVE HELPERS
  ******************************************************************************/

  void confirm_proper_set_syntax_args(scm_list& args,const char* name,const char* format){
    if(args.empty() || args.size() > 2)
      THROW_ERR('\''<<name<<" didn't recieve correct # of args!"
        << format << FCN_ERR(name,args));
    for(size_type i = 0, n = args.size(); i < n; ++i)
      if(!args[i].is_type(types::sym))
        THROW_ERR('\''<<name<<" arg #" << i+1 << ' ' << PROFILE(args[i])
          << " isn't a symbol!" << format << FCN_ERR(name,args));
  }


  void relabel_recursive_calls_in_macro_template(const sym_type& old_label,const sym_type& new_label,scm_list& tmp)noexcept{
    for(size_type i = 0, n = tmp.size(); i < n; ++i) {
      if(tmp[i].is_type(types::exp))
        relabel_recursive_calls_in_macro_template(old_label,new_label,tmp[i].exp);
      else if(tmp[i].is_type(types::sym) && tmp[i].sym == old_label)
        tmp[i].sym = new_label;
    }
  }


  data delete_macro_from_env(const sym_type& label,env_type& env)noexcept{
    for(size_type i = 0, total_frames = env->size(); i < total_frames; ++i){
      // Get Variables & Values Lists of the current frame
      auto& [var_list, val_list, mac_list] = *env->operator[](i);
      // Search Variable-Value List Pair In Frame
      for(size_type j = 0, total_macs = mac_list.size(); j < total_macs; ++j)
        if(label == mac_list[j].label) {
          mac_list.erase(mac_list.begin()+j);
          return G::TRUE_DATA_BOOLEAN;
        }
    }
    return G::FALSE_DATA_BOOLEAN;
  }


  data relabel_macro_in_env(const sym_type& old_label,const sym_type& new_label,env_type& env)noexcept{
    for(size_type i = 0, total_frames = env->size(); i < total_frames; ++i){
      // Get Variables & Values Lists of the current frame
      auto& [var_list, val_list, mac_list] = *env->operator[](i);
      // Search Variable-Value List Pair In Frame
      for(size_type j = 0, total_macs = mac_list.size(); j < total_macs; ++j)
        if(old_label == mac_list[j].label) {
          mac_list[j].label = new_label;
          auto& templates = mac_list[j].templates;
          // Relabel each recursive call in the macro templates as well
          for(size_type k = 0, n = templates.size(); k < n; ++k)
            relabel_recursive_calls_in_macro_template(old_label,new_label,templates[k]);
          return G::TRUE_DATA_BOOLEAN;
        }
    }
    return G::FALSE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * READER MACRO DEFINITION / DELETION PRIMITIVE HELPER
  ******************************************************************************/

  data delete_reader_macro(const scm_string& shorthand)noexcept{
    for(auto short_iter = G::SHORTHAND_READER_MACRO_REGISTRY.begin(), 
             long_iter = G::LONGHAND_READER_MACRO_REGISTRY.begin();
      short_iter != G::SHORTHAND_READER_MACRO_REGISTRY.end();
      ++short_iter, ++long_iter) {
      // Rm shorthand/longhand if found
      if(shorthand == *short_iter) {
        G::SHORTHAND_READER_MACRO_REGISTRY.erase(short_iter);
        G::LONGHAND_READER_MACRO_REGISTRY.erase(long_iter);
        return G::TRUE_DATA_BOOLEAN;
      }
    }
    return G::FALSE_DATA_BOOLEAN;
  }


  void register_reader_macro(const scm_string& shorthand, const scm_string& longhand)noexcept{
    const auto shorthand_len = shorthand.size();
    for(auto short_iter = G::SHORTHAND_READER_MACRO_REGISTRY.begin(), 
             long_iter = G::LONGHAND_READER_MACRO_REGISTRY.begin();
      short_iter != G::SHORTHAND_READER_MACRO_REGISTRY.end();
      ++short_iter, ++long_iter) {
      // Change longhand associated w/ existing shorthand
      if(shorthand == *short_iter) {
        *long_iter = longhand;
        return;
      // Register new reader macro
      } else if(shorthand > *short_iter && shorthand_len >= short_iter->size()) {
        G::SHORTHAND_READER_MACRO_REGISTRY.insert(short_iter,shorthand);
        G::LONGHAND_READER_MACRO_REGISTRY.insert(long_iter,longhand);
        return;
      }
    }
    // Register new reader macro
    G::SHORTHAND_READER_MACRO_REGISTRY.push_back(shorthand);
    G::LONGHAND_READER_MACRO_REGISTRY.push_back(longhand);
  }
} // End of namespace heist
#endif