// Author: Jordan Randleman -- jordanran199@gmail.com -- csv.hpp
// => Defines the primitive csv functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_CSV_HPP_
#define HEIST_SCHEME_CORE_STDLIB_CSV_HPP_

#include "implementation.hpp"

namespace heist {

  data primitive_COERCE_LIST_TO_CSV(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (list->csv <list-of-lists-of-csv-data> <optional-delimiter-char>)"
      "\n     <csv-data> ::= <string> | <number>";
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR("'list->csv received incorrect # of args!" 
        << format << HEIST_FCN_ERR("list->csv",args));
    char delimiter = ',';
    if(args.size() == 2) {
      if(!args[1].is_type(types::chr) || !args[1].chr || args[1].chr == '\n')
        HEIST_THROW_ERR("'list->csv 2nd arg " << HEIST_PROFILE(args[1]) << " isn't a non-nul/newline character!" 
          << format << HEIST_FCN_ERR("list->csv",args));
      delimiter = char(args[1].chr);
    }
    if(args[0].is_type(types::sym) && args[0].sym == symconst::emptylist) // given nil
      return make_str("");
    std::vector<data_vector> csv_matrix;
    stdlib_csv::confirm_proper_LIST_csv_datum<true>(csv_matrix,args[0],args,"list->csv",format);
    return stdlib_csv::generate_csv(csv_matrix,delimiter);
  }


  data primitive_COERCE_VECTOR_TO_CSV(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (vector->csv <vector-of-vectors-of-csv-data> <optional-delimiter-char>)"
      "\n     <csv-data> ::= <string> | <number>";
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR("'vector->csv received incorrect # of args!" 
        << format << HEIST_FCN_ERR("vector->csv",args));
    char delimiter = ',';
    if(args.size() == 2) {
      if(!args[1].is_type(types::chr) || !args[1].chr || args[1].chr == '\n')
        HEIST_THROW_ERR("'vector->csv 2nd arg " << HEIST_PROFILE(args[1]) << " isn't a a non-nul/newline character!" 
          << format << HEIST_FCN_ERR("vector->csv",args));
      delimiter = char(args[1].chr);
    }
    if(args[0].is_type(types::vec) && args[0].vec->empty()) // empty vector
      return make_str("");
    std::vector<data_vector> csv_matrix;
    stdlib_csv::confirm_proper_VECTOR_csv_datum<true>(csv_matrix,args[0],args,"vector->csv",format);
    return stdlib_csv::generate_csv(csv_matrix,delimiter);
  }


  data primitive_COERCE_CSV_TO_LIST(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (csv->list <csv-string> <optional-delimiter-char>)"
      "\n     <csv-data> ::= <string> | <number>";
    char delimiter = stdlib_csv::validate_csv_parsing_args(args,"csv->list",format);
    return stdlib_csv::parse_csv("(list ", delimiter, *args[0].str);
  }


  data primitive_COERCE_CSV_TO_VECTOR(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (csv->vector <csv-string> <optional-delimiter-char>)"
      "\n     <csv-data> ::= <string> | <number>";
    char delimiter = stdlib_csv::validate_csv_parsing_args(args,"csv->vector",format);
    return stdlib_csv::parse_csv("(vector ", delimiter, *args[0].str);
  }


  data primitive_CSV_DATUMP(data_vector&& args) {
    static constexpr const char * const format = "\n     (csv-datum? <datum>)";
    if(args.size() != 1)
      HEIST_THROW_ERR("'csv-datum? not given ! arg!" << format << HEIST_FCN_ERR("csv-datum?",args));
    std::vector<data_vector> csv_matrix;
    return boolean(stdlib_csv::confirm_proper_LIST_csv_datum<false>(csv_matrix,args[0],args,"csv-datum?",format) || 
                   stdlib_csv::confirm_proper_VECTOR_csv_datum<false>(csv_matrix,args[0],args,"csv-datum?",format));
  }

} // End of namespace heist

#endif