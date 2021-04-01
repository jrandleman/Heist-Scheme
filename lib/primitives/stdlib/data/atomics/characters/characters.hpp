// Author: Jordan Randleman -- jrandleman@scu.edu -- characters.hpp
// => Defines primitive character functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_CHARACTERS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_CHARACTERS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * EOF GENERATOR
  ******************************************************************************/

  data primitive_EOF(data_vector&& args) {
    if(!args.empty()) HEIST_THROW_ERR("'eof is nullary: (eof)" << HEIST_FCN_ERR("eof",args));
    return chr_type(EOF);
  }

  /******************************************************************************
  * CHAR COMPARATORS
  ******************************************************************************/

  // primitive "char=?" procedure:
  data primitive_CHAR_EQP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_EQP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr != args[i+1].chr)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char<?" procedure:
  data primitive_CHAR_LTP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char<?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_LTP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr >= args[i+1].chr)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char>?" procedure:
  data primitive_CHAR_GTP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char>?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_GTP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr <= args[i+1].chr)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char<=?" procedure:
  data primitive_CHAR_LTEP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char<=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_LTEP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr > args[i+1].chr)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char>=?" procedure:
  data primitive_CHAR_GTEP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char>=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_GTEP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr < args[i+1].chr)
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci=?" procedure:
  data primitive_CHAR_CI_EQP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char-ci=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_CI_EQP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) != scm_numeric::mklower(args[i+1].chr))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci<?" procedure:
  data primitive_CHAR_CI_LTP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char-ci<?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_CI_LTP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) >= scm_numeric::mklower(args[i+1].chr))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci>?" procedure:
  data primitive_CHAR_CI_GTP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char-ci>?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_CI_GTP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) <= scm_numeric::mklower(args[i+1].chr))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci<=?" procedure:
  data primitive_CHAR_CI_LTEP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char-ci<=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_CI_LTEP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) > scm_numeric::mklower(args[i+1].chr))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci>=?" procedure:
  data primitive_CHAR_CI_GTEP(data_vector&& args) {
    stdlib_characters::confirm_only_chars_and_at_least_one_arg(args, "char-ci>=?");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CHAR_CI_GTEP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) < scm_numeric::mklower(args[i+1].chr))
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * CHAR ANALYSIS
  ******************************************************************************/

  // primitive "char-alphabetic?" procedure:
  data primitive_CHAR_ALPHABETICP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-alphabetic?");
    return boolean(isalpha(args[0].chr));
  }

  // primitive "char-numeric?" procedure:
  data primitive_CHAR_NUMERICP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-numeric?");
    return boolean(isdigit(args[0].chr));
  }

  // primitive "char-whitespace?" procedure:
  data primitive_CHAR_WHITESPACEP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-whitespace?");
    return boolean(isspace(args[0].chr));
  }

  // primitive "char-upper-case?" procedure:
  data primitive_CHAR_UPPER_CASEP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-upper-case?");
    return boolean(isupper(args[0].chr));
  }

  // primitive "char-lower-case?" procedure:
  data primitive_CHAR_LOWER_CASEP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-lower-case?");
    return boolean(islower(args[0].chr));
  }

  // primitive "char-alphanumeric?" procedure:
  data primitive_CHAR_ALHPANUMERICP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-alphanumeric?");
    return boolean(isalnum(args[0].chr));
  }

  // primitive "char-control?" procedure:
  data primitive_CHAR_CONTROLP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-control?");
    return boolean(iscntrl(args[0].chr));
  }

  // primitive "char-print?" procedure:
  data primitive_CHAR_PRINTP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-print?");
    return boolean(isprint(args[0].chr));
  }

  // primitive "char-graph?" procedure:
  data primitive_CHAR_GRAPHP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-graph?");
    return boolean(isgraph(args[0].chr));
  }

  // primitive "char-punctuation?" procedure:
  data primitive_CHAR_PUNCTUATIONP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-punctuation?");
    return boolean(ispunct(args[0].chr));
  }

  // primitive "char-xdigit?" procedure:
  data primitive_CHAR_XDIGITP(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-xdigit?");
    return boolean(isxdigit(args[0].chr));
  }

  /******************************************************************************
  * CHAR CASE CONTROL
  ******************************************************************************/

  // primitive "char-upcase" procedure:
  data primitive_CHAR_UPCASE(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-upcase");
    return toupper(args[0].chr);
  }

  // primitive "char-downcase" procedure:
  data primitive_CHAR_DOWNCASE(data_vector&& args) {
    stdlib_characters::confirm_given_one_char_arg(args, "char-downcase");
    return tolower(args[0].chr);
  }
} // End of namespace heist

#endif