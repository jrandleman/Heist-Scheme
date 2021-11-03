// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for strings.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_STRINGS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_STRINGS_IMPLEMENTATION_HPP_

#include <regex>

namespace heist::stdlib_strings {

  /******************************************************************************
  * GENERAL VALIDATION
  ******************************************************************************/

  bool data_is_valid_string_size(const data& d)noexcept{
    return d.is_type(types::num) && d.num.is_integer() &&
           !d.num.is_neg() && d.num <= GLOBALS::MAX_SIZE_TYPE;
  }


  size_type confirm_only_char_or_string_args(const data_vector& args)noexcept{
    for(size_type i = 0, n = args.size(); i < n; ++i)
      if(!args[i].is_type(types::str) && !args[i].is_type(types::chr))
        return i;
    return GLOBALS::MAX_SIZE_TYPE;
  }


  void confirm_given_only_string_args(const data_vector& args, const char* name) {
    for(size_type i = 0, n = args.size(); i < n; ++i)
      if(!args[i].is_type(types::str))
        HEIST_THROW_ERR('\''<<name<<" arg #"<<i+1<<' '<<HEIST_PROFILE(args[i])<<" isn't string!"
          "\n     ("<<name<<" <string> ...)" << HEIST_FCN_ERR(name,args));
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


  data primitive_STRING_UNFOLD_template(data_vector& args, const bool unfolding_right, 
                                        const char* name,  const char* format){
    data_vector unfolded;
    primitive_UNFOLD_template(args,unfolded,name,format);
    if(unfolded.empty()) return make_str("");
    string str_val;
    for(size_type i = 0, n = unfolded.size(); i < n; ++i) {
      if(!unfolded[i].is_type(types::chr)) 
        HEIST_THROW_ERR('\''<<name<<" generated value #" << i+1 << ", " << HEIST_PROFILE(unfolded[i]) 
          << ",\n     isn't a character: " << format << HEIST_FCN_ERR(name, args));
      str_val += unfolded[i].chr;
    }
    if(unfolding_right) return make_str(string(str_val.rbegin(),str_val.rend()));
    return make_str(str_val);
  }

  /******************************************************************************
  * STRING-PAD
  ******************************************************************************/

  void confirm_given_string_and_valid_length(data_vector& args, const char* name, const char* format){
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 1st arg " << HEIST_PROFILE(args[0]) << " isn't a string:"
        << format << HEIST_FCN_ERR(name, args));
    if(!data_is_valid_string_size(args[1]))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg " << HEIST_PROFILE(args[1]) << " isn't a"
        "\n     proper non-negative integer length!" << format << 
        "\n     <index> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR(name, args));
  }


  char confirm_valid_string_pad_args(data_vector& args, const char* name, const char* format){
    if(args.size() != 2 && args.size() != 3)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args:" << format << HEIST_FCN_ERR(name, args));
    confirm_given_string_and_valid_length(args, name, format);
    if(args.size() == 3) {
      if(!args[2].is_type(types::chr))
        HEIST_THROW_ERR('\''<<name<<" 3rd arg " << HEIST_PROFILE(args[2]) 
          << " isn't a character:" << format << HEIST_FCN_ERR(name, args));
      return char(args[2].chr);
    }
    return ' ';
  }

  /******************************************************************************
  * STRING-TRIM
  ******************************************************************************/

  void confirm_valid_string_trim_args(data_vector& args, const char* name, const char* format){
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args:" << format << HEIST_FCN_ERR(name, args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 1st arg " << HEIST_PROFILE(args[0]) << " isn't a string:"
        << format << HEIST_FCN_ERR(name, args));
    if(args.size() == 2) primitive_toolkit::confirm_data_is_callable(args[1], args, name, format);  
  }


  data trim_left_of_string(data_vector& args) {
    string str(*args[0].str);
    const size_type n = str.size();
    size_type i = 0;
    if(args.size() == 1) { // no predicate given, trim whitespace
      for(; i < n && isspace(str[i]); ++i);
    } else {
      auto procedure(primitive_toolkit::convert_callable_to_procedure(args[1]));
      for(; i < n; ++i) { // while predicate is true, trim character
        if(execute_application(procedure,data_vector(1,chr_type(str[i]))).is_falsey()) break;
      }
    }
    if(i == n) return make_str("");
    return make_str(str.substr(i));
  }


  data trim_right_of_string(data_vector& args) {
    if(args[0].str->empty()) return make_str("");
    string str(*args[0].str);
    const size_type n = str.size();
    size_type i = n-1;
    if(args.size() == 1) { // no predicate given, trim whitespace
      for(; i > 0 && isspace(str[i]); --i);
      if(i == 0 && isspace(str[i])) return make_str("");
    } else {
      auto procedure(primitive_toolkit::convert_callable_to_procedure(args[1]));
      for(; i > 0; --i) { // while predicate is true, trim character
        if(execute_application(procedure,data_vector(1,chr_type(str[i]))).is_falsey()) break;
      }
      if(i == 0 && execute_application(procedure,data_vector(1,chr_type(str[i]))).is_truthy()) {
        return make_str("");
      }
    }
    if(i == n) return make_str("");
    return make_str(str.substr(0,i+1));
  }

  /******************************************************************************
  * CASE-INSENSITIVE STRING COMPARISONS HELPER
  ******************************************************************************/

  string lowercase_str(const string& s)noexcept{
    string tmp;
    for(const auto& ch : s) tmp += scm_numeric::mklower(ch);
    return tmp;
  }

  /******************************************************************************
  * STRING-CONTAINS
  ******************************************************************************/

  data string_contains_template(data_vector& args,  const char* name, 
                                const char* format, const bool from_left, const bool ci){
    if(args.size() != 2) 
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR(name, args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string:" 
        << format << HEIST_FCN_ERR(name, args));
    if(!args[1].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a string:" 
        << format << HEIST_FCN_ERR(name, args));
    size_type pos;
    // Case-sensitive
    if(!ci) {
      if(from_left) {
        pos = args[0].str->find(*args[1].str);
      } else {
        pos = args[0].str->rfind(*args[1].str);
      }
    // Case-insensitive
    } else {
      if(from_left) {
        pos = lowercase_str(*args[0].str).find(lowercase_str(*args[1].str));
      } else {
        pos = lowercase_str(*args[0].str).rfind(lowercase_str(*args[1].str));
      }
    }
    if(pos == string::npos) return GLOBALS::FALSE_DATA_BOOLEAN;
    return num_type(pos);
  }

  /******************************************************************************
  * STRING-JOIN
  ******************************************************************************/

  enum class STRING_GRAMMARS {INFIX, SUFFIX, PREFIX};


  void confirm_given_list_of_strings(data_vector& args, const char* format, data_vector& strings_list){
    if(!primitive_toolkit::data_is_proper_list(args[0]))
      HEIST_THROW_ERR("'string-join 1st arg " << HEIST_PROFILE(args[0]) 
        << " isn't a proper list:" << format << HEIST_FCN_ERR("string-join",args));
    data iter = args[0];
    while(iter.is_type(types::par)) {
      if(!iter.par->first.is_type(types::str))
        HEIST_THROW_ERR("'string-join <string-list> item "<< HEIST_PROFILE(iter.par->first) 
          << " isn't string:" << format << HEIST_FCN_ERR("string-join",args));
      strings_list.push_back(iter.par->first);
      iter = iter.par->second;
    }
  }


  void confirm_proper_string_join_args(data_vector& args, STRING_GRAMMARS& grammar, 
                                       string& delimiter, data_vector& strings_list){
    static constexpr const char * const format = 
      "\n     (string-join <string-list> <optional-string-delimiter> <optional-grammar>)"
      "\n     <optional-grammar> = 'infix | 'suffix | 'prefix";
    if(args.empty() || args.size() > 3) // confirm enough args
      HEIST_THROW_ERR("'string-join received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("string-join", args));
    confirm_given_list_of_strings(args, format, strings_list); // confirm list of strings
    if(args.size() > 1) { // confirm proper delimiter
      if(!args[1].is_type(types::str))
        HEIST_THROW_ERR("'string-join 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a string:" 
          << format << HEIST_FCN_ERR("string-join", args));
      delimiter = *args[1].str;
    }
    if(args.size() == 3) { // confirm proper grammar
      if(!args[2].is_type(types::sym))
        HEIST_THROW_ERR("'string-join 3rd arg "<<HEIST_PROFILE(args[2])<<" isn't a symbol:" 
          << format << HEIST_FCN_ERR("string-join", args));
      if(args[2].sym == "suffix") grammar = STRING_GRAMMARS::SUFFIX;
      else if(args[2].sym == "prefix") grammar = STRING_GRAMMARS::PREFIX;
      else if(args[2].sym != "infix") {
        HEIST_THROW_ERR("'string-join 3rd arg "<<HEIST_PROFILE(args[2])
          <<" wasn't a proper grammar:" 
          << format << HEIST_FCN_ERR("string-join", args));
      }
    }
  }

  /******************************************************************************
  * STRING-SPLIT
  ******************************************************************************/

  void confirm_proper_string_split_args(data_vector& args,const char* name,const char* format,
                                        string& delimiter,size_type& start_index){
    // confirm proper arg signature
    if(args.empty() || args.size() > 3) 
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR(name, args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string:"
        << format << HEIST_FCN_ERR(name, args));
    if(args.size() > 1) { // confirm proper delimiter
      if(!args[1].is_type(types::str))
        HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a string:" 
          << format << HEIST_FCN_ERR(name, args));
      delimiter = *args[1].str;
      if(args.size() > 2) {
        if(!data_is_valid_string_size(args[2]))
          HEIST_THROW_ERR('\''<<name<<" 3rd arg "<<HEIST_PROFILE(args[2])<<" isn't a valid index:" 
            << format << HEIST_FCN_ERR(name, args));
        start_index = (size_type)args[2].num.extract_inexact();
        if(start_index >= args[0].str->size())
          HEIST_THROW_ERR('\''<<name<<" index "<<args[2].num.str()<<" exceeds capacity of string \""
            << *args[1].str << "\":" << format << HEIST_FCN_ERR(name, args));
      }
    }
  }

  /******************************************************************************
  * STRING-COPY!
  ******************************************************************************/

  size_type get_if_valid_string_idx(const data_vector& args, const char* format){
    // confirm given an in-'size_type'-range non-negative index
    if(!data_is_valid_string_size(args[1]))
      HEIST_THROW_ERR("'string-copy! index "<<args[1]<<" isn't a proper non-negative integer!"
        << format << "\n     <index> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' 
        << HEIST_FCN_ERR("string-copy!",args));
    // confirm index falls w/in range of the sequence
    const size_type i = (size_type)args[1].num.extract_inexact();
    const size_type l = args[0].str->size();
    if(i >= l)
      HEIST_THROW_ERR("'string-copy! received out of range index " << i 
        <<"\n     for string "<<args[0]<<" of size "
        <<l<<'!'<<format<<HEIST_FCN_ERR("string-copy!",args));
    return i;
  }

  /******************************************************************************
  * REGEX HELPERS
  ******************************************************************************/

  void confirm_n_args_and_first_2_args_are_strings(const data_vector& args, const size_type& total_args, 
                                                   const char* format,      const char* name) {
    if(args.size() != total_args)
      HEIST_THROW_ERR('\''<<name<<" didn't receive "<<total_args<<" args!"
        << format << HEIST_FCN_ERR(name, args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string!"
        << format << HEIST_FCN_ERR(name, args));
    if(!args[1].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a string!"
        << format << HEIST_FCN_ERR(name, args));
  }


  data throw_malformed_regex(const data_vector& args, const char* format, const char* name) {
    HEIST_THROW_ERR('\''<<name<<" malformed regex string " << args[1].noexcept_write() << '!'
      << format << HEIST_FCN_ERR(name, args));
    return data(); // never triggered, due to the above throw
  }


  data get_regex_matches(const string& str, const string& regex) {
    std::regex reg(regex);
    std::sregex_iterator currentMatch(str.begin(), str.end(), reg), lastMatch;
    data_vector matches;
    while(currentMatch != lastMatch) { // <lastMatch> implicit assignment as an iterator to ".end()"
      data_vector match_instance;
      if(currentMatch->size() == 1) { // alist (1 substring per regex match)
        match_instance.push_back(num_type(currentMatch->position()));
        match_instance.push_back(make_str(currentMatch->str()));
        matches.push_back(primitive_toolkit::convert_data_vector_to_proper_list(match_instance.begin(),match_instance.end()));
      } else { // 2nd order alist (more than 1 substring per regex match)
        for(size_type i = 0, n = currentMatch->size(); i < n; ++i) {
          data_vector submatch_instance(2);
          submatch_instance[0] = num_type(currentMatch->position(i));
          submatch_instance[1] = make_str(currentMatch->str(i));
          match_instance.push_back(primitive_toolkit::convert_data_vector_to_proper_list(submatch_instance.begin(),submatch_instance.end()));
        }
        matches.push_back(primitive_toolkit::convert_data_vector_to_proper_list(match_instance.begin(),match_instance.end()));
      }
      ++currentMatch;
    }
    return primitive_toolkit::convert_data_vector_to_proper_list(matches.begin(),matches.end());
  }


  data regex_replace(const string& target, const string& regex, const string& replacement){
    std::smatch reg_matches;
    while(std::regex_search(target, reg_matches, std::regex(regex)))
      return make_str(reg_matches.prefix().str() + replacement + reg_matches.suffix().str());
    return make_str(target);
  }


  data regex_replace_all(const string& target, const string& regex, const string& replacement){
    return make_str(string(std::regex_replace(target, std::regex(regex), replacement)));
  }


  template<bool REPLACE_ONE>
  data regex_replace_fcn_generic(string target, const string& regex, 
                                 const data_vector& args,const char* format, const char* name, data&& procedure){
    const std::regex reg(regex);
    std::smatch reg_matches;
    while(std::regex_search(target, reg_matches, reg)) {
      // save prefix, suffix, and matches
      data_vector reg_args(2 + reg_matches.size());
      reg_args[0] = make_str(reg_matches.prefix().str());
      reg_args[1] = make_str(reg_matches.suffix().str());
      for(std::size_t i = 0, n = reg_matches.size(); i < n; ++i)
        reg_args[i+2] = make_str(reg_matches.str(i));
      // pass to given procedure & confirm returned a string
      data result = execute_application(procedure,std::move(reg_args));
      if(!result.is_type(types::str))
        HEIST_THROW_ERR('\''<<name<<" procedure \""<<procedure.fcn.name // skip prefixing ' '
          <<"\" didn't return a string (returned "<<HEIST_PROFILE(result)<<")!"<<format<<HEIST_FCN_ERR(name,args));
      // replace as often as needed
      if constexpr (REPLACE_ONE) {
        return make_str(string(reg_matches.prefix().str() + *result.str + reg_matches.suffix().str()));
      } else {
        target = reg_matches.prefix().str() + *result.str + reg_matches.suffix().str();
      }
    }
    return make_str(string(target));
  }

  auto regex_replace_all_fcn = regex_replace_fcn_generic<false>;
  auto regex_replace_fcn     = regex_replace_fcn_generic<true>;


  // dipatch for "regex-replace" & "regex-replace-all"
  data regex_primitive_replace_application(data_vector& args, const char* format, const char* name,
                                           decltype(regex_replace) str_replace, decltype(regex_replace_fcn) fcn_replace){
    if(args[2].is_type(types::str)) {
      try {
        return str_replace(*args[0].str,*args[1].str,*args[2].str);
      } catch(...) {
        return throw_malformed_regex(args,format,name);
      }
    } else if(primitive_toolkit::data_is_callable(args[2])) {
      try {
        return fcn_replace(*args[0].str,*args[1].str,args,format,name,primitive_toolkit::convert_callable_to_procedure(args[2]));
      } catch(const SCM_EXCEPT& err) {
        throw err; // thrown by the procedure
      } catch(...) {
        return throw_malformed_regex(args,format,name);
      }
    } else {
      HEIST_THROW_ERR('\''<<name<<" last arg "<<HEIST_PROFILE(args[2])<<" isn't a <string> or <callable>!"
        << format << HEIST_FCN_ERR(name,args));
      return data(); // never triggered
    }
  }


  data regex_split_empty_string(const string& target)noexcept{
    data_vector split;
    for(const auto& ch : target)
      split.push_back(make_str(string(1,ch)));
    return primitive_toolkit::convert_data_vector_to_proper_list(split.begin(),split.end());
  }


  data regex_split(string target, const string& regex){
    if(regex.empty()) return regex_split_empty_string(target);
    const std::regex reg(regex);
    std::smatch reg_matches;
    data_vector split;
    while(std::regex_search(target, reg_matches, reg)) {
      split.push_back(make_str(reg_matches.prefix().str()));
      target = reg_matches.suffix().str();
    }
    split.push_back(make_str(target));
    return primitive_toolkit::convert_data_vector_to_proper_list(split.begin(),split.end());
  }

} // End of namespace heist::stdlib_strings

#endif