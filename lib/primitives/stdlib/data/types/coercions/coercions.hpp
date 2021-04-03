// Author: Jordan Randleman -- jrandleman@scu.edu -- coercions.hpp
// => Defines the primitive type coercion functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_TYPE_COERCIONS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_TYPE_COERCIONS_HPP_

#include "implementation.hpp"

namespace heist {

  // primitive "char->integer" procedure:
  data primitive_COERCE_CHAR_TO_INTEGER(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg(args,"char->integer","<char>");
    if(!args[0].is_type(types::chr))
      HEIST_THROW_ERR("'char->integer arg "<<HEIST_PROFILE(args[0])<<" isn't a character!"
        "\n     (char->integer <char>)" << HEIST_FCN_ERR("char->integer",args));
    return num_type(int(args[0].chr));
  }

  // primitive "integer->char" procedure:
  data primitive_COERCE_INTEGER_TO_CHAR(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (integer->char <non_negative-integer>)"
      "\n     <non_negative-integer> range: [0,255]";
    if(args.size() != 1)
      HEIST_THROW_ERR("'integer->char didn't receive 1 integer arg:" << format 
        << HEIST_FCN_ERR("integer->char",args));
    if(!args[0].is_type(types::num) || !args[0].num.is_integer())
      HEIST_THROW_ERR("'integer->char didn't receive an integer arg:"
        "\n     Received arg " << HEIST_PROFILE(args[0]) << format 
        << HEIST_FCN_ERR("integer->char",args));
    if((args[0].num.is_neg() || args[0].num > 255) && 
       args[0].num != EOF)
      HEIST_THROW_ERR("'integer->char " << HEIST_PROFILE(args[0]) << " isn't a"
        "\n     positive integer ranging from 0 to 255!" << format 
        << HEIST_FCN_ERR("integer->char",args));
    return chr_type(args[0].num.extract_inexact());
  }

  // primitive "number->string" procedure:
  data primitive_COERCE_NUMBER_TO_STRING(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (number->string <number> <optional-radix> <optional-precision>)"
      "\n     <optional-radix> ::= [2,36]"
      "\n     <optional-precision> ::= Integer";
    if(args.size() > 3 || args.empty())
      HEIST_THROW_ERR("'number->string received incorrect # of arguments:"
        << format << HEIST_FCN_ERR("number->string",args));
    // No number or invalid radix/precision given
    stdlib_type_coercions::validate_NUMBER_TO_STRING_args(args,format);
    // Given a radix
    string number_as_string;
    if(args.size() > 1) {
      size_type radix = args[1].num.round().extract_inexact();
      if(radix < 2 || radix > 36)
        HEIST_THROW_ERR("'number->string radix (given "<<radix<<") can only range from 2-36:"
          << format << HEIST_FCN_ERR("number->string", args));
      if(radix != 10) {
        number_as_string = args[0].num.str(radix);
        goto after_number_stringification;
      }
    }
    number_as_string = args[0].num.str();
  after_number_stringification:
    // Alter precision as needed
    if(stdlib_type_coercions::no_NUMBER_TO_STRING_precision_change_needed(args,number_as_string))
      return make_str(number_as_string);
    if(!args[0].num.is_real())
      HEIST_THROW_ERR("'number->string only real numbers can be converted using a precision!"
        "\n     => " << HEIST_PROFILE(args[0]) << " is complex!" << format << HEIST_FCN_ERR("number->string", args));
    const auto dec_pos = number_as_string.find(".");
    if(dec_pos == string::npos) return make_str(number_as_string);
    const auto current_precision = number_as_string.size()-dec_pos-1;
    const auto precision = (size_type)args[2].num.extract_inexact();
    if(precision > current_precision) // pad 0s
      return make_str(number_as_string + string(precision-current_precision,'0'));
    number_as_string.erase(dec_pos+precision+1); // truncate
    return make_str(number_as_string);
  }

  // primitive "string->number" procedure:
  data primitive_COERCE_STRING_TO_NUMBER(data_vector&& args) {
    bool convert_string_to_scm_number(const string&, num_type&)noexcept; // defined in the input parser
    if(args.size() > 2 || args.empty())
      HEIST_THROW_ERR("'string->number received incorrect # of arguments!"
        "\n     (string->number <string> <optional-numeric-radix>)"
        << HEIST_FCN_ERR("string->number", args));
    // no string or invalid radix given
    if(!args[0].is_type(types::str) || 
        (args.size() == 2 && 
          (!args[1].is_type(types::num) || !args[1].num.is_integer()))) 
      return GLOBALS::FALSE_DATA_BOOLEAN;
    // given a radix
    if(args.size() == 2) {
      size_type radix = args[1].num.round().extract_inexact();
      if(radix < 2 || radix > 36)
        HEIST_THROW_ERR("'string->number radix (given "<<radix<<") can only range from 2-36!"
          "\n     (string->number <string> <optional-numeric-radix>)"
          << HEIST_FCN_ERR("string->number", args));
      char exactness_prefix = stdlib_type_coercions::parse_exactness_numeric_prefix(*args[0].str);
      if(radix != 10) {
        auto num = exactness_prefix ? num_type(args[0].str->substr(2), radix) : num_type(*args[0].str, radix);
        if(num.is_nan()) return GLOBALS::FALSE_DATA_BOOLEAN; // invalid conversion
        switch(exactness_prefix) {
          case 'e': return num.to_exact();
          case 'i': return num.to_inexact();
          default:  return num;
        }
      }
    }
    // immediate return if given NaN
    if(*args[0].str == "+nan.0" || *args[0].str == "-nan.0")
      return data(num_type(*args[0].str));
    num_type num;
    if(convert_string_to_scm_number(*args[0].str,num)) return num;
    return GLOBALS::FALSE_DATA_BOOLEAN; // invalid conversion
  }

  // primitive "symbol->string" procedure:
  data primitive_COERCE_SYMBOL_TO_STRING(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg(args,"symbol->string","<symbol>");
    if(!args[0].is_type(types::sym)) return GLOBALS::FALSE_DATA_BOOLEAN;
    return make_str(stdlib_type_coercions::convert_symbol_to_string(args[0].sym));
  }

  // primitive "string->symbol" procedure:
  data primitive_COERCE_STRING_TO_SYMBOL(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg(args,"string->symbol","<string>");
    if(!args[0].is_type(types::str)) return GLOBALS::FALSE_DATA_BOOLEAN;
    return data(stdlib_type_coercions::convert_string_to_symbol(args,*args[0].str)); 
  }

  // primitive "vector->list" procedure:
  data primitive_COERCE_VECTOR_TO_LIST(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg_of_type(types::vec, "vector", args, "vector->list");
    return stdlib_type_coercions::convert_vector_to_list(args[0]);
  }

  // primitive "list->vector" procedure:
  data primitive_COERCE_LIST_TO_VECTOR(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg(args,"list->vector","<list>");
    if(!primitive_toolkit::data_is_proper_list(args[0]))
      HEIST_THROW_ERR("'list->vector arg "<<HEIST_PROFILE(args[0])<<" isn't a proper list!"
        "\n     (list->vector <list>)" << HEIST_FCN_ERR("list->vector",args));
    if(primitive_toolkit::data_is_nil(args[0])) return make_vec(data_vector());
    return stdlib_type_coercions::convert_list_to_vector(args[0]);
  }

  // primitive "string->vector" procedure:
  data primitive_COERCE_STRING_TO_VECTOR(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg_of_type(types::str, "string", args, "string->vector");
    return stdlib_type_coercions::convert_string_to_vector(*args[0].str);
  }

  // primitive "vector->string" procedure:
  data primitive_COERCE_VECTOR_TO_STRING(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg_of_type(types::vec, "vector", args, "vector->string");
    if(args[0].vec->empty()) return make_str("");
    data str;
    if(!stdlib_type_coercions::convert_vector_to_string(args[0],str))
      HEIST_THROW_ERR("'vector->string vector "<<args[0]<<" has a non-character element!"
        "\n     (vector->string <char-vector>)" << HEIST_FCN_ERR("vector->string",args));
    return str;
  }

  // primitive "string->list" procedure:
  data primitive_COERCE_STRING_TO_LIST(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg_of_type(types::str, "string", args, "string->list");
    return stdlib_type_coercions::convert_string_to_list(*args[0].str);
  }

  // primitive "list->string" procedure:
  data primitive_COERCE_LIST_TO_STRING(data_vector&& args) {
    stdlib_type_coercions::confirm_given_one_arg(args,"list->string","<list>");
    if(!primitive_toolkit::data_is_proper_list(args[0]))
      HEIST_THROW_ERR("'list->string arg "<<HEIST_PROFILE(args[0])<<" isn't a proper list!"
        "\n     (list->string <list>)" << HEIST_FCN_ERR("list->string",args));
    if(primitive_toolkit::data_is_nil(args[0])) return make_str("");
    data str;
    if(!stdlib_type_coercions::convert_list_to_string(args[0],str))
      HEIST_THROW_ERR("'list->string list "<<args[0]<<" has a non-character element!"
        "\n     (list->string <char-list>)" << HEIST_FCN_ERR("list->string",args));
    return str;
  }

  // primitive "functor->procedure" procedure:
  data primitive_COERCE_FUNCTOR_TO_PROCEDURE(data_vector&& args) {
    if(args.size() != 1 || !primitive_toolkit::data_is_functor(args[0]))
      HEIST_THROW_ERR("'functor->procedure not given 1 functor!"
        "\n     (functor->procedure <functor>)" << HEIST_FCN_ERR("functor->procedure",args));
    return primitive_toolkit::convert_callable_to_procedure(args[0]);
  }

} // End of namespace heist

#endif