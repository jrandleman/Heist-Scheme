// Author: Jordan Randleman -- jrandleman@scu.edu -- strings.hpp
// => Defines primitive string functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_STRINGS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_STRINGS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * STRING CONSTRUCTORS
  ******************************************************************************/

  // primitive "make-string" procedure:
  data primitive_MAKE_STRING(data_vector&& args) {
    // confirm valid length given
    const auto args_size = args.size();
    if(!args_size || args_size > 2 || !stdlib_strings::data_is_valid_string_size(args[0]))
      HEIST_THROW_ERR("'make-string didn't receive a proper positive integer size!"
        "\n     (make-string <size> <optional-fill-char>)"
        "\n     <size> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR("make-string", args));
    if(args_size==2 && !args[1].is_type(types::chr))
      HEIST_THROW_ERR("'make-string received a non-character fill value:"
        "\n     Received fill value "<<HEIST_PROFILE(args[1])<<'!'
        << "\n     (make-string <size> <optional-fill-char>)" << HEIST_FCN_ERR("make-string", args));
    // mk a string w/ the the given reserve size
    size_type n = (size_type)args[0].num.extract_inexact();
    return make_str(string(n, (args_size==2 ? args[1].chr : '?')));
  }

  // primitive "string" procedure:
  data primitive_STRING(data_vector&& args) {
    if(args.empty()) return make_str("");
    if(auto i = stdlib_strings::confirm_only_char_or_string_args(args); i != GLOBALS::MAX_SIZE_TYPE)
      HEIST_THROW_ERR("'string arg #" << i+1 << ", " << HEIST_PROFILE(args[i]) << ", isn't a character or string:"
        "\n     (string <char-or-string1> <char2-or-string> ...)" << HEIST_FCN_ERR("string", args));
    string str_val;
    for(const auto& e : args) {
      if(e.is_type(types::chr)) {
        str_val += e.chr;
      } else {
        str_val += *e.str;
      }
    }
    return make_str(str_val);
  }

  // primitive "string-unfold" procedure:
  data primitive_STRING_UNFOLD(data_vector&& args) {
    if(!args.empty() && args.size() < 4) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_UNFOLD,args);
    return stdlib_strings::primitive_STRING_UNFOLD_template(args,false,"string-unfold",
      "\n     (string-unfold <break-condition> <map-callable> <successor-callable> <seed>)");
  }

  // primitive "string-unfold-right" procedure:
  data primitive_STRING_UNFOLD_RIGHT(data_vector&& args) {
    if(!args.empty() && args.size() < 4) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_UNFOLD_RIGHT,args);
    return stdlib_strings::primitive_STRING_UNFOLD_template(args,true,"string-unfold-right",
      "\n     (string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)");
  }

  /******************************************************************************
  * GENERAL STRING PRIMITIVES
  ******************************************************************************/

  // primitive "string-pad" procedure:
  data primitive_STRING_PAD(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_PAD,args);
    char padding_character = stdlib_strings::confirm_valid_string_pad_args(args, "string-pad",
      "\n     (string-pad <string> <length> <optional-character>)");
    const size_type length = (size_type)args[1].num.extract_inexact();
    const size_type n = args[0].str->size();
    if(length > n) return make_str(string(length-n, padding_character) + *args[0].str);
    return make_str(args[0].str->substr(n-length));
  }

  // primitive "string-pad-right" procedure:
  data primitive_STRING_PAD_RIGHT(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_PAD_RIGHT,args);
    char padding_character = stdlib_strings::confirm_valid_string_pad_args(args, "string-pad-right",
      "\n     (string-pad-right <string> <length> <optional-character>)");
    const size_type length = (size_type)args[1].num.extract_inexact();
    const size_type n = args[0].str->size();
    if(length > n) return make_str(*args[0].str + string(length-n, padding_character));
    return make_str(args[0].str->substr(0, length));
  }

  // primitive "string-trim" procedure:
  data primitive_STRING_TRIM(data_vector&& args) {
    stdlib_strings::confirm_valid_string_trim_args(args, "string-trim","\n     (string-trim <string> <optional-predicate>)");
    return stdlib_strings::trim_left_of_string(args);
  }

  // primitive "string-trim-right" procedure:
  data primitive_STRING_TRIM_RIGHT(data_vector&& args) {
    stdlib_strings::confirm_valid_string_trim_args(args, "string-trim-right","\n     (string-trim-right <string> <optional-predicate>)");
    return stdlib_strings::trim_right_of_string(args);
  }

  // primitive "string-trim-both" procedure:
  data primitive_STRING_TRIM_BOTH(data_vector&& args) {
    stdlib_strings::confirm_valid_string_trim_args(args, "string-trim-both","\n     (string-trim-both <string> <optional-predicate>)");
    data_vector right_trim_args;
    right_trim_args.push_back(make_str(*stdlib_strings::trim_left_of_string(args).str));
    if(args.size() == 2) right_trim_args.push_back(args[1]);
    return stdlib_strings::trim_right_of_string(right_trim_args);
  }

  // primitive "string-replace" procedure:
  data primitive_STRING_REPLACE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (string-replace <string1> <string2> <start1-index> <end1-index>)";
    if(!args.empty() && args.size() < 4) 
      return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_REPLACE,args);
    if(args.size() != 4)
      HEIST_THROW_ERR("'string-replace received incorrect # of args (given " 
        << args.size() << "):" << format 
        << HEIST_FCN_ERR("string-replace", args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR("'string-replace 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string:" 
        << format << HEIST_FCN_ERR("string-replace", args));
    if(!args[1].is_type(types::str))
      HEIST_THROW_ERR("'string-replace 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a string:" 
        << format << HEIST_FCN_ERR("string-replace", args));
    auto [start, valid_start] = primitive_toolkit::convert_data_to_size_type(args[2]);
    if(!valid_start)
      HEIST_THROW_ERR("'string-replace 3rd arg "<<HEIST_PROFILE(args[2])<<" isn't a valid index:" 
        << format << "\n     <index> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' 
        << HEIST_FCN_ERR("string-replace", args));
    auto [end, valid_end] = primitive_toolkit::convert_data_to_size_type(args[3]);
    if(!valid_end)
      HEIST_THROW_ERR("'string-replace 4th arg "<<HEIST_PROFILE(args[3])<<" isn't a valid index:" 
        << format << "\n     <index> range: [0," << GLOBALS::MAX_SIZE_TYPE << ']' 
        << HEIST_FCN_ERR("string-replace", args));
    if(end < start)
      HEIST_THROW_ERR("'string-replace <end> index " << end << " must be greater than <start> index " << start
        <<"\n     for string "<<args[0]<<" of size "<<args[0].str->size()<<'!'
        << HEIST_FCN_ERR("string-replace", args));
    // If <start1-index> exceeds the length of the string, return the string
    const auto string1_length = args[0].str->size();
    if(start >= string1_length) return args[0];
    // If <end1-index> exceeds the length of the string, set it to be the length of the string
    if(end >= string1_length) end = !string1_length ? 0 : string1_length-1;
    return make_str(args[0].str->substr(0,start) + *args[1].str + args[0].str->substr(end+1));
  }

  // primitive "string-contains" procedure:
  data primitive_STRING_CONTAINS(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_CONTAINS,args);
    return stdlib_strings::string_contains_template(args, "string-contains", "\n     (string-contains <string> <sub-string>)", true);
  }

  // primitive "string-contains-right" procedure:
  data primitive_STRING_CONTAINS_RIGHT(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_CONTAINS_RIGHT,args);
    return stdlib_strings::string_contains_template(args, "string-contains-right", "\n     (string-contains-right <string> <sub-string>)", false);
  }

  // primitive "string-join" procedure:
  data primitive_STRING_JOIN(data_vector&& args) {
    auto grammar = stdlib_strings::STRING_GRAMMARS::INFIX;
    string delimiter(""), joined_string("");
    data_vector strings_list;
    stdlib_strings::confirm_proper_string_join_args(args, grammar, delimiter, strings_list);
    if(!strings_list.empty()) {
      if(grammar == stdlib_strings::STRING_GRAMMARS::INFIX) {
        joined_string += *strings_list[0].str;
        for(size_type i = 1, n = strings_list.size(); i < n; ++i)
          joined_string += delimiter + *strings_list[i].str;
      } else if(grammar == stdlib_strings::STRING_GRAMMARS::SUFFIX) {
        for(const auto& data_str : strings_list)
          joined_string += *data_str.str + delimiter;
      } else if(grammar == stdlib_strings::STRING_GRAMMARS::PREFIX) {
        for(const auto& data_str : strings_list)
          joined_string += delimiter + *data_str.str;
      }
    }
    return make_str(joined_string);
  }

  // primitive "string-split" procedure:
  data primitive_STRING_SPLIT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (string-split <target-string> <optional-string-delimiter> <optional-start-index>)";
    string delimiter("");
    data_vector strings_list;
    size_type start_index = 0;
    stdlib_strings::confirm_proper_string_split_args(args,"string-split",format,delimiter,start_index);
    // split the string into a list of strings
    const string str(args[0].str->substr(start_index));
    const size_type delim_size = delimiter.size();
    if(!delim_size) {
      for(const auto& letter : str)
        strings_list.push_back(make_str(string(1,letter)));
    } else {
      size_type substr_start = 0;
      for(size_type i = 0, n = str.size(); i < n; ++i) {
        size_type j = 0;
        for(; j < delim_size && i+j < n; ++j)
          if(str[i+j] != delimiter[j]) break;
        if(j == delim_size) { // at a split instance
          strings_list.push_back(make_str(str.substr(substr_start,i-substr_start)));
          i += delim_size-1;
          substr_start = i+1;
        }
      }
      strings_list.push_back(make_str(str.substr(substr_start)));
    }
    // generate a list of strings from the populated data vector
    return primitive_toolkit::convert_data_vector_to_proper_list(strings_list.begin(),strings_list.end());
  }

  // primitive "string-empty?" procedure:
  data primitive_STRING_EMPTYP(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'string-empty? didn't receive exactly 1 string arg!"
        "\n     (string-empty? <string>)" << HEIST_FCN_ERR("string-empty?",args));
    return boolean(args[0].str->empty());
  }

  /******************************************************************************
  * MUTATIVE STRING PRIMITIVES
  ******************************************************************************/

  // primitive "string-swap!" procedure:
  data primitive_STRING_SWAP_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (string-swap! <string> <string>)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_SWAP_BANG,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'string-swap! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << HEIST_FCN_ERR("string-swap!", args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR("'string-swap! 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string:" 
        << format << HEIST_FCN_ERR("string-swap!", args));
    if(!args[1].is_type(types::str))
      HEIST_THROW_ERR("'string-swap! 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a string:" 
        << format << HEIST_FCN_ERR("string-swap!", args));
    string tmp(*args[0].str);
    *args[0].str = *args[1].str;
    *args[1].str = tmp;
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "string-push-back!" procedure:
  data primitive_STRING_PUSH_BACK_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (string-push-back! <string> <char>)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_PUSH_BACK_BANG,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'string-push-back! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << HEIST_FCN_ERR("string-push-back!", args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR("'string-push-back! 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string:" 
        << format << HEIST_FCN_ERR("string-push-back!", args));
    if(!args[1].is_type(types::chr))
      HEIST_THROW_ERR("'string-push-back! 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a character:" 
        << format << HEIST_FCN_ERR("string-push-back!", args));
    args[0].str->push_back(char(args[1].chr));
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "string-push-front!" procedure:
  data primitive_STRING_PUSH_FRONT_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (string-push-front! <string> <char>)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_PUSH_FRONT_BANG,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'string-push-front! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << HEIST_FCN_ERR("string-push-front!", args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR("'string-push-front! 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string:" 
        << format << HEIST_FCN_ERR("string-push-front!", args));
    if(!args[1].is_type(types::chr))
      HEIST_THROW_ERR("'string-push-front! 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a character:" 
        << format << HEIST_FCN_ERR("string-push-front!", args));
    *args[0].str = char(args[1].chr) + *args[0].str;
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "string-pop-back!" procedure:
  data primitive_STRING_POP_BACK_BANG(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'string-pop-back! didn't receive exactly 1 string arg!"
        "\n     (string-pop-back! <string>)" << HEIST_FCN_ERR("string-pop-back!",args));
    if(args[0].str->empty())
      HEIST_THROW_ERR("'string-pop-back! can't pop chars from an empty string!"
        "\n     (string-pop-back! <string>)" << HEIST_FCN_ERR("string-pop-back!",args));
    data last_item = chr_type(*args[0].str->rbegin());
    args[0].str->pop_back();
    return last_item;
  }

  // primitive "string-pop-front!" procedure:
  data primitive_STRING_POP_FRONT_BANG(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'string-pop-front! didn't receive exactly 1 string arg!"
        "\n     (string-pop-front! <string>)" << HEIST_FCN_ERR("string-pop-front!",args));
    if(args[0].str->empty())
      HEIST_THROW_ERR("'string-pop-front! can't pop chars from an empty string!"
        "\n     (string-pop-front! <string>)" << HEIST_FCN_ERR("string-pop-front!",args));
    data first_item = chr_type(*args[0].str->begin());
    args[0].str->erase(args[0].str->begin());
    return first_item;
  }

  // primitive "string-copy!" procedure:
  data primitive_STRING_COPY_BANG(data_vector&& args) {
    static constexpr const char * const format = "\n     (string-copy! <target-string> <target-start-idx> <source-string>)";
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_COPY_BANG,args);
    if(args.size() != 3)
      HEIST_THROW_ERR("'string-copy! received incorrect # of arguments:"<<format<<HEIST_FCN_ERR("string-copy!",args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR("'string-copy! 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string:"<<format<<HEIST_FCN_ERR("string-copy!",args));
    size_type idx = stdlib_strings::get_if_valid_string_idx(args,format);
    if(!args[2].is_type(types::str))
      HEIST_THROW_ERR("'string-copy! 3rd arg "<<HEIST_PROFILE(args[2])<<" isn't a string:"<<format<<HEIST_FCN_ERR("string-copy!",args));
    // splice in the source string
    const auto source_sequence(*args[2].str); // in case copying a string to itself
    const size_type target_size = args[0].str->size();
    const size_type source_size = source_sequence.size();
    if(target_size-idx-1 < source_size){
      args[0].str->erase(args[0].str->begin()+idx, args[0].str->end());
      args[0].str->insert(args[0].str->end(), source_sequence.begin(), source_sequence.end());
    } else {
      args[0].str->erase(args[0].str->begin()+idx, args[0].str->begin()+idx+source_size);
      args[0].str->insert(args[0].str->begin()+idx, source_sequence.begin(), source_sequence.end());
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * STRING COMPARATORS
  ******************************************************************************/

  // primitive "string=?" procedure:
  data primitive_STRING_EQP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_EQP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str != *args[i+1].str)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string<?" procedure:
  data primitive_STRING_LTP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string<?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_LTP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str >= *args[i+1].str)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string>?" procedure:
  data primitive_STRING_GTP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string>?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_GTP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str <= *args[i+1].str)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string<=?" procedure:
  data primitive_STRING_LTEP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string<=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_LTEP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str > *args[i+1].str)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string>=?" procedure:
  data primitive_STRING_GTEP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string>=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_GTEP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str < *args[i+1].str)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci=?" procedure:
  data primitive_STRING_CI_EQP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string-ci=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_CI_EQP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(stdlib_strings::lowercase_str(*args[i].str) != stdlib_strings::lowercase_str(*args[i+1].str))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci<?" procedure:
  data primitive_STRING_CI_LTP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string-ci<?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_CI_LTP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(stdlib_strings::lowercase_str(*args[i].str) >= stdlib_strings::lowercase_str(*args[i+1].str))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci>?" procedure:
  data primitive_STRING_CI_GTP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string-ci>?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_CI_GTP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(stdlib_strings::lowercase_str(*args[i].str) <= stdlib_strings::lowercase_str(*args[i+1].str))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci<=?" procedure:
  data primitive_STRING_CI_LTEP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string-ci<=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_CI_LTEP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(stdlib_strings::lowercase_str(*args[i].str) > stdlib_strings::lowercase_str(*args[i+1].str))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci>=?" procedure:
  data primitive_STRING_CI_GTEP(data_vector&& args) {
    stdlib_strings::confirm_given_only_string_args(args, "string-ci>=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STRING_CI_GTEP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(stdlib_strings::lowercase_str(*args[i].str) < stdlib_strings::lowercase_str(*args[i+1].str))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * REGEX
  ******************************************************************************/

  // primitive "regex-replace": replaces 1st instance w/ a string or using the given callable
  data primitive_REGEX_REPLACE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (regex-replace <target-string> <regex-string> <replacement-string>)"
      "\n     (regex-replace <target-string> <regex-string> <callable>)"
      "\n     -> <callable> ::= (lambda (<prefix>, <suffix>, <match1>, ...) <body>)";
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_REGEX_REPLACE,args);
    stdlib_strings::confirm_n_args_and_first_2_args_are_strings(args,3,format,"regex-replace");
    return stdlib_strings::regex_primitive_replace_application(args,format,"regex-replace",stdlib_strings::regex_replace,stdlib_strings::regex_replace_fcn);
  }


  // primitive "regex-replace-all": replaces all instances w/ a string or using the given callable
  data primitive_REGEX_REPLACE_ALL(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (regex-replace-all <target-string> <regex-string> <replacement-string>)"
      "\n     (regex-replace-all <target-string> <regex-string> <callable>)"
      "\n     -> <callable> ::= (lambda (<prefix>, <suffix>, <match1>, ...) <body>)";
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_REGEX_REPLACE_ALL,args);
    stdlib_strings::confirm_n_args_and_first_2_args_are_strings(args,3,format,"regex-replace-all");
    return stdlib_strings::regex_primitive_replace_application(args,format,"regex-replace-all",stdlib_strings::regex_replace_all,stdlib_strings::regex_replace_all_fcn);
  }


  // primitive "regex-match"
  // => returns an alist of the matched substrings
  //    -> each sublist begins with the position, followed by all match
  data primitive_REGEX_MATCH(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (regex-match <target-string> <regex-string>)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_REGEX_MATCH,args);
    stdlib_strings::confirm_n_args_and_first_2_args_are_strings(args,2,format,"regex-match");
    try {
      return stdlib_strings::get_regex_matches(*args[0].str,*args[1].str);
    } catch(...) {
      return stdlib_strings::throw_malformed_regex(args,format,"regex-match");
    }
  }


  // primitive "regex-split" procedure:
  data primitive_REGEX_SPLIT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (regex-split <target-string> <optional-regex-string> <optional-start-index>)";
    string delimiter("");
    size_type start_index = 0;
    stdlib_strings::confirm_proper_string_split_args(args,"regex-split",format,delimiter,start_index);
    // split the string into a list of strings
    try {
      return stdlib_strings::regex_split(args[0].str->substr(start_index),delimiter);
    } catch(...) {
      return stdlib_strings::throw_malformed_regex(args,format,"regex-split");
    }
  }

} // End of namespace heist

#endif