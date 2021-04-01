// Author: Jordan Randleman -- jrandleman@scu.edu -- json.hpp
// => Defines the primitive json functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_JSON_HPP_
#define HEIST_SCHEME_CORE_STDLIB_JSON_HPP_

#include "implementation.hpp"

namespace heist {

  // CONVERTING JSON STRINGS TO A PARSABLE SCHEME DATA STRUCT:
  // ,              -> <space>
  // true           -> #t
  // false          -> #f
  // null           -> '()
  // [...]          -> [vector ...]
  // <string>:<obj> -> (list <string> <obj>)

  data primitive_JSON_TO_SCM(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'json->scm didn't receive 1 string arg!" 
        "\n     (json->scm <string>)" << HEIST_FCN_ERR("json->scm", args));
    if(args[0].str->empty()) return GLOBALS::VOID_DATA_OBJECT;
    string input = *args[0].str;
    try { // Try parsing the converted json expression, & throw an error as needed
      data_vector abstract_syntax_tree;
      // Return AST if successfully parsed an expression
      parse_input_exp(stdlib_json::heist_json_parser::convert_json_to_scm(*args[0].str,input),abstract_syntax_tree);
      if(abstract_syntax_tree.empty()) return GLOBALS::VOID_DATA_OBJECT;
      return scm_eval(std::move(abstract_syntax_tree[0]),G.GLOBAL_ENVIRONMENT_POINTER);
    } catch(const READER_ERROR& read_error) {
      stdlib_json::heist_json_parser::print_json_reader_error_alert();
      if(is_non_repl_reader_error(read_error))
           alert_non_repl_reader_error(stdout,read_error,input);
      else alert_reader_error(stdout,read_error,input);
      throw SCM_EXCEPT::READ;
    } catch(const size_type& read_error_index) {
      stdlib_json::heist_json_parser::print_json_reader_error_alert();
      alert_reader_error(stdout,read_error_index,input);
      throw SCM_EXCEPT::READ;
    }
  }


  data primitive_SCM_TO_JSON(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (scm->json <obj> <optional-indent-width>)"
      "\n     <obj> ::= <string>"
      "\n             | <number>"
      "\n             | <'()>    ; -> <null>" 
      "\n             | <alist>  ; -> <map> (keys must be string | number | null | bool!)"
      "\n             | <vector> ; -> <array>"
      "\n             | <boolean>";
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR("'scm->json didn't receive correct # of args:"
        << format << HEIST_FCN_ERR("scm->json",args));
    size_type indent_width = 0;
    if(args.size() == 2) {
      if(!stdlib_json::data_is_valid_index(args[1]))
        HEIST_THROW_ERR("'scm->json 2nd arg " << HEIST_PROFILE(args[0]) << " isn't a valid indent width!" << format 
          << "\n     <optional-indent-width> := [0, " << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR("scm->json",args));
      indent_width = (size_type)args[1].num.extract_inexact();
    }
    return make_str(stdlib_json::heist_json_generator::format_scm_as_json(args[0],indent_width,args,format));
  }


  data primitive_JSON_DATUMP(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'json-datum? didn't receive exactly 1 arg!"
        "\n     (json-datum? <obj>)" << HEIST_FCN_ERR("json-datum?",args));
    return boolean(stdlib_json::is_valid_json_datum(args[0]));
  }

} // End of namespace heist

#endif