// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for coercions.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_TYPE_COERCIONS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_TYPE_COERCIONS_IMPLEMENTATION_HPP_

// Helper procedure for symbol->string coercion.
// From: "lib/core/evaluator/dependancies/macro_expander.hpp"
namespace heist { bool string_is_an_escaped_variadic_token(const string& str)noexcept; }

namespace heist::stdlib_type_coercions {

  /******************************************************************************
  * GENERAL VALIDATION
  ******************************************************************************/

  void confirm_given_one_arg(const data_vector& args, const char* name, const char* arg_name = "<obj>"){
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of arguments:"
        "\n     ("<<name<<' '<<arg_name<<')'<<HEIST_FCN_ERR(name,args));
  }


  void confirm_given_one_arg_of_type(const types t, const char* type_name, const data_vector& args, const char* name){
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of arguments:"
        "\n     ("<<name<<" <"<<type_name<<">)"<<HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(t))
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't a "<<type_name<<"!"
        "\n     ("<<name<<" <"<<type_name<<">)"<<HEIST_FCN_ERR(name,args));
  }

  /******************************************************************************
  * SYMBOL->STRING
  ******************************************************************************/

  // Determine whether input[i] is at a hex 
  // POSTCONDITION: i is returned if input[i] is _NOT_ at a hex 
  //                else, the index of the hex value's closing ':' is returned.
  size_type is_symbol_hex_val(size_type i, const size_type& n, const string& input)noexcept{
    if(i < n-2 && input[i] == '\\' && input[i+1] == 'x') {
      auto j = i+2; // mv past the '\x' prefix
      while(input[j] && isalnum(input[j])) ++j;
      return (input[j] == ':') ? j : i;
    }
    return i;
  }


  string convert_symbol_to_string(const string& string_val)noexcept{
    if(string_val.size() <= 2) return string_val;
    string symbol_str;
    for(size_type i = 0, n = string_val.size(); i < n; ++i) {
      if(auto end_hex_idx = is_symbol_hex_val(i,n,string_val); end_hex_idx!=i){
        // i+1 [below] skips past prefixing '\'
        string hex_num(string_val.begin()+i+1, string_val.begin()+end_hex_idx);
        // convert hex# string -> int -> char
        symbol_str += char(std::stoi("0"+hex_num,nullptr,16)); 
        i = end_hex_idx;
      } else {
        symbol_str += string_val[i];
      }
    }
    return symbol_str;
  }

  /******************************************************************************
  * STRING->SYMBOL
  ******************************************************************************/

  void confirm_string_has_no_reader_syntax_substrings(const data_vector& args, const string& s) {
    for(const auto& shorthand : G.SHORTHAND_READER_ALIAS_REGISTRY)
      if(s.find(shorthand) != string::npos)
        HEIST_THROW_ERR("'string->symbol string \"" << s << "\" has reader syntax \"" 
          << shorthand << "\" as a substring (invalid)!\n     (string->symbol <string>)"
          << HEIST_FCN_ERR("string->symbol",args));
  }


  // NOTE: Don't need to verify whether could be interpreted as a character, as the '\' in '#\' would trigger
  //       an error in <confirm_string_has_no_reader_syntax_substrings> as the reader-shorthand for "lambda"s
  void confirm_string_couldnt_be_interpreted_as_a_boolean_or_number(const data_vector& args, const string& s) {
    if(s == symconst::true_ || s == symconst::false_)
      HEIST_THROW_ERR("'string->symbol string \"" << s << "\" can't be a symbol (interpretable as a boolean literal)!"
        "\n     (string->symbol <string>)" << HEIST_FCN_ERR("string->symbol",args));
    if(s == "+nan.0" || s == "-nan.0")
      HEIST_THROW_ERR("'string->symbol string \"" << s << "\" can't be a symbol (interpretable as a numeric literal)!"
        "\n     (string->symbol <string>)" << HEIST_FCN_ERR("string->symbol",args));
    num_type ignore;
    if(convert_string_to_scm_number(s,ignore)) // from lib/core/reader/parser.hpp
      HEIST_THROW_ERR("'string->symbol string \"" << s << "\" can't be a symbol (interpretable as a numeric literal)!"
        "\n     (string->symbol <string>)" << HEIST_FCN_ERR("string->symbol",args));
  }


  void confirm_string_only_has_valid_symbolic_characters(const data_vector& args, const string& s) {
    if(s.find("#|") != string::npos)
      HEIST_THROW_ERR("'string->symbol string \"" << s << "\" has invalid symbolic substring: multi-line comment opener \"#|\"!"
        "\n     (string->symbol <string>)" << HEIST_FCN_ERR("string->symbol",args));
    if(s.find("|#") != string::npos)
      HEIST_THROW_ERR("'string->symbol string \"" << s << "\" has invalid symbolic substring: multi-line comment closer \"|#\"!"
        "\n     (string->symbol <string>)" << HEIST_FCN_ERR("string->symbol",args));
    for(const auto& ch : s)
      if(isspace(ch)||ch == '('||ch == ')'||ch == '['||ch == ']'||ch == '{'||ch == '}'||ch == '"'||ch == ';')
        HEIST_THROW_ERR("'string->symbol string \"" << s << "\" has invalid symbol character '"<<ch<<"'!"
          "\n     (string->symbol <string>)" << HEIST_FCN_ERR("string->symbol",args));
  }


  // primitive "string->symbol" conversion helper
  string convert_string_to_symbol(const data_vector& args, const string& str_val) {
    if(str_val.empty() || string_is_an_escaped_variadic_token(str_val)) return str_val;
    confirm_string_has_no_reader_syntax_substrings(args,str_val);
    confirm_string_couldnt_be_interpreted_as_a_boolean_or_number(args,str_val);
    confirm_string_only_has_valid_symbolic_characters(args,str_val);
    string symbol_str;
    // Convert chars in the string to their symbolic versions
    for(const auto& ch : str_val) {
      if(GLOBALS::USING_CASE_SENSITIVE_SYMBOLS) {
        symbol_str += ch;
      } else {
        symbol_str += scm_numeric::mklower(ch);
      }
    }
    return symbol_str;
  }

  /******************************************************************************
  * NUMBER->STRING
  ******************************************************************************/

  void validate_NUMBER_TO_STRING_args(const data_vector& args, const char* format) {
    if(!args[0].is_type(types::num))
      HEIST_THROW_ERR("'number->string 1st arg " << HEIST_PROFILE(args[0]) << " isn't a number!"
        << format << HEIST_FCN_ERR("number->string", args));
    if(args.size() > 1 && (!args[1].is_type(types::num) || !args[1].num.is_integer() || args[1].num < 2 || args[1].num > 36))
      HEIST_THROW_ERR("'number->string radix (2nd) arg " << HEIST_PROFILE(args[1]) << " isn't an integer between 2 & 36 (inclusive)!"
        << format << HEIST_FCN_ERR("number->string", args));
    if(args.size() == 3 && (!args[2].is_type(types::num) || !args[2].num.is_integer() || !args[2].num.is_pos()))
      HEIST_THROW_ERR("'number->string precision (3rd) arg " << HEIST_PROFILE(args[2]) << " isn't a positive integer!"
        << format << HEIST_FCN_ERR("number->string", args));
  }


  bool no_NUMBER_TO_STRING_precision_change_needed(const data_vector& args, const string& number_as_string)noexcept{
    // No change needed if non-inexact or in scientific notation
    return args.size() < 3 || !args[0].num.is_inexact() || 
      (args[1].num == 10 && number_as_string.find("e") != number_as_string.find("E"));
  }

  /******************************************************************************
  * STRING->NUMBER
  ******************************************************************************/

  // returns 'e', 'i', or 0 (the last denoting no exactness present)
  char parse_exactness_numeric_prefix(const string& numstr) {
    if(numstr.size() > 2 && numstr[0] == '#' && (numstr[1] == 'i' || numstr[1] == 'e'))
      return numstr[1];
    return 0;
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

} // End of namespace heist::stdlib_type_coercions

#endif