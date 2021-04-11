// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for json.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_JSON_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_JSON_IMPLEMENTATION_HPP_

namespace heist::stdlib_json {

  /******************************************************************************
  * GENERAL VALIDATION
  ******************************************************************************/

  bool data_is_valid_index(const data& d)noexcept{
    return d.is_type(types::num) && d.num.is_integer() && 
           !d.num.is_neg() && d.num <= GLOBALS::MAX_SIZE_TYPE;
  }

  /******************************************************************************
  * JSON PARSER HELPERS
  ******************************************************************************/

  // JSON KEYS: only strings
  // JSON VALUES:
  //   0. string
  //   1. number (fraction -> long double, NaN/Inf -> null)
  //   2. object (alist)
  //   3. array (vector)
  //   4. boolean
  //   5. null (empty list)

  // CONVERTING JSON STRINGS TO A PARSABLE SCHEME DATA STRUCT:
  // ,              -> <space>
  // true           -> #t
  // false          -> #f
  // null           -> '()
  // [...]          -> [vector ...]
  // <string>:<obj> -> (list <string> <obj>)

  namespace heist_json_parser {
    void throw_malformed_json_error(const char* reason, const string& original_json) {
      HEIST_THROW_ERR("'json->scm malformed JSON (" << reason << "!)"
        "\n     JSON STRING: \"" << original_json << "\""
        "\n     (json->scm <string>)" << HEIST_FCN_ERR("json->scm", data_vector(1,original_json)));
    }


    void print_json_reader_error_alert() {
      std::cerr << '\n' << HEIST_AFMT(heist::AFMT_131) << "-----------" 
        << HEIST_AFMT(heist::AFMT_01) << "--------------\n"
        << HEIST_AFMT(heist::AFMT_131) << "> JSON->SCM" 
        << HEIST_AFMT(heist::AFMT_01) << " READER ERROR:" << HEIST_AFMT(heist::AFMT_0);
    }


    // PRECONDITION: delimiter = '[' | '{'
    // POSTCONDITION: ')' is added after the final closing end-delimiter
    void add_closing_paren_after_array_or_object_value(size_type i,  const char& delimiter, 
                                                       string& json, const string& original_json){
      ++i; // skip past opening delimiter
      const char end_delimiter = delimiter == '[' ? ']' : '}';
      long long delimiter_count = 1;
      // Find end of expression
      for(size_type n = json.size(); i < n && delimiter_count; ++i) {
        if(json[i] == delimiter) {
          ++delimiter_count;
        } else if(json[i] == end_delimiter) {
          --delimiter_count;
        } else if(is_non_escaped_double_quote(i,json)) { // from lib/core/reader/parser.hpp
          skip_string_literal(i,json);                   // from lib/core/reader/parser.hpp
        } 
      }
      if(i == json.size()) { // verify not at the end of the JSON
        char issue[] = "missing a closing '}'";
        if(delimiter == '[') issue[19] = ']';
        throw_malformed_json_error(issue,original_json);
      }
      json.insert(i+1,")");
    }


    void prepend_string_key_with_list(size_type& i, string& json, const string& original_json){
      size_type j = i;
      for(; j; --j) {
        if(isspace(json[j])) continue;
        if(json[j] == '"') break;
        throw_malformed_json_error("non-string key detected",original_json);
      }
      while(j && j-1 && !is_non_escaped_double_quote(--j,json)); // from lib/core/reader/parser.hpp
      if(!j) throw_malformed_json_error("non-string key detected",original_json);
      json.insert(j,"(list ");
      i += 6;
    }


    // POST CONDITION: 'i' is where the original ':' was in the string (relatively)
    void parse_json_key_val_pair(size_type& i, string& json, const string& original_json) {
      json[i] = ' ';
      prepend_string_key_with_list(i,json,original_json);
      const size_type n = json.size();
      while(i < n && isspace(json[i])) ++i; // mv up to the value
      if(i == n) throw_malformed_json_error("missing value detected",original_json);
      size_type j = i;
      // Insert ')' after array or object
      if(json[j] == '{' || json[j] == '[') {
        add_closing_paren_after_array_or_object_value(j,json[j],json,original_json);
      // Insert ')' after string
      } else if(json[j] == '"') {
        skip_string_literal(j,json); // from lib/core/reader/parser.hpp
        json.insert(j+1, ")");
      } else {
        while(j < n && !isspace(json[j]) && json[j] != ',' && json[j] != '}' && json[j] != ']') ++j;
        json.insert(j, ")");
      }
      --i; // mv 1 space prior the next value to parse
    }


    bool char_is_valid_json_separator(const char& c)noexcept{
      return isspace(c) || c=='[' || c==']' || c=='{' || c=='}' || c==',' || c==':';
    }


    // VALID TOKENS: <string>, <number>, [], {}, <,>, <:>, null, true, false
    void validate_given_json(const string& json) {
      for(size_type i = 0, n = json.size(); i < n; ++i) {
        // skip strings
        if(is_non_escaped_double_quote(i,json)) { // from lib/core/reader/parser.hpp
          skip_string_literal(i,json);            // from lib/core/reader/parser.hpp
          continue;
        }
        // skip separators
        if(char_is_valid_json_separator(json[i])) continue;
        // skip null/true
        if(i+3 < n && 
          ((json[i]=='n' && json[i+1]=='u' && json[i+2]=='l' && json[i+3]=='l') || 
           (json[i]=='t' && json[i+1]=='r' && json[i+2]=='u' && json[i+3]=='e'))) {
          i += 3;
        // skip false
        } else if(i+4 < n && json[i]=='f' && json[i+1]=='a' && json[i+2]=='l' && json[i+3]=='s' && json[i+4]=='e') {
          i += 4;
        // confirm a number, and skip if so
        } else {
          string possible_num;
          while(i < n && !char_is_valid_json_separator(json[i])) possible_num += json[i++];
          if(num_type(possible_num).is_nan())
            throw_malformed_json_error(("invalid JSON token detected: \"" + possible_num + '"').c_str(), json);
        }
      }
    }


    string convert_json_to_scm(string json, const string& original_json) {
      validate_given_json(json);
      for(size_type i = 0; i < json.size(); ++i) {
        if(isspace(json[i])) continue;
        if(is_non_escaped_double_quote(i,json)) { // from lib/core/reader/parser.hpp
          skip_string_literal(i,json);            // from lib/core/reader/parser.hpp
          continue;
        }
        // Whitespace commas
        if(json[i] == ',') {
          json[i] = ' ';
        // Add 'vector ' prefix to arrays
        } else if(json[i] == '[') {
          json.insert(i+1, "vector ");
          i += 7;
        // Add 'list ' prefix to objects
        } else if(json[i] == '{') {
          json.insert(i+1, "list ");
          i += 5;
        // Convert nulls to the empty list
        } else if(i+3 < json.size() && json[i]=='n' && json[i+1]=='u' && json[i+2]=='l' && json[i+3]=='l') {
          json[i] = '\'', json[i+1] = '(', json[i+2] = ')', json[i+3] = ' ';
          i += 4;
        // Convert true -> #t
        } else if(i+3 < json.size() && json[i]=='t' && json[i+1]=='r' && json[i+2]=='u' && json[i+3]=='e') {
          json[i] = '#', json[i+1] = 't', json[i+2] = ' ', json[i+3] = ' ';
          i += 4;
        // Convert false -> #f
        } else if(i+4 < json.size() && json[i]=='f' && json[i+1]=='a' && json[i+2]=='l' && json[i+3]=='s' && json[i+4]=='e') {
          json[i] = '#', json[i+1] = 'f', json[i+2] = ' ', json[i+3] = ' ', json[i+4] = ' ';
          i += 5;
        // Convert key-value pairs to regular pairs
        } else if(json[i] == ':') {
          parse_json_key_val_pair(i,json,original_json);
        }
      }
      return json;
    }
  } // End of namespace heist_json_parser

  /******************************************************************************
  * JSON GENERATOR HELPERS
  ******************************************************************************/

  namespace heist_json_generator {
    string convert_scm_to_json(data&,const data_vector&,const char*);


    void format_JSON_indent_width_recur(const string& json, size_type& i, const size_type n, 
                                        string& formatted, const size_type indent_width, const size_type depth)noexcept{
      formatted += '\n' + string(depth*indent_width, ' ');
      while(i < n) {
        if(is_non_escaped_double_quote(i,json)) { // from lib/core/reader/parser.hpp
          auto start = i;
          skip_string_literal(i,json);            // from lib/core/reader/parser.hpp
          formatted += json.substr(start,(++i)-start);
        } else if(json[i] == '{' || json[i] == '[') {
          formatted += json[i++];
          format_JSON_indent_width_recur(json,i,n,formatted,indent_width,depth+1);
        } else if(json[i] == '}' || json[i] == ']') {
          formatted += '\n' + string((depth-1)*indent_width, ' ') + json[i++];
          return;
        } else if(json[i] == ',') {
          formatted += json[i++] + ('\n' + string(depth*indent_width, ' '));
          while(i < n && json[i] == ' ') ++i; // skip whitespace after comma, disturbs indenting
        } else {
          formatted += json[i++];
        }
      }
    }


    // PRECONDITION: ASSUMES <json> IS VALID JSON
    string format_JSON_indent_width(const string& json, size_type indent_width)noexcept{
      if(json.empty() || !indent_width) return json;
      if(json[0] != '[' && json[0] != '{') return json;
      string formatted(1,json[0]);
      size_type i = 1;
      format_JSON_indent_width_recur(json,i,json.size(),formatted,indent_width,1);
      return formatted;
    }


    void convert_scm_to_json_write_map_pair(string& map_json, data_vector& item, 
                                            const data_vector& args, const char* format){
      if(item[0].is_type(types::str)) {
        map_json += item[0].write() + ": " + convert_scm_to_json(item[1],args,format);
      } else if(item[0].is_type(types::num)) {
        map_json += '"' + item[0].write() + "\": " + convert_scm_to_json(item[1],args,format);
      } else if(item[0].is_type(types::bol)) {
        if(item[0].bol.val) map_json += "\"true\": " + convert_scm_to_json(item[1],args,format);
        else                map_json += "\"false\": " + convert_scm_to_json(item[1],args,format);
      } else {
        map_json += "\"null\": " + convert_scm_to_json(item[1],args,format);
      }
    }


    bool datum_is_a_valid_json_map_key(const data& d)noexcept{
      return d.is_type(types::str) || d.is_type(types::num) || 
             d.is_type(types::bol) || primitive_toolkit::data_is_nil(d);
    }


    string convert_scm_to_json(data& d, const data_vector& args, const char* format) {
      // Convert Empty List
      if(primitive_toolkit::data_is_nil(d)) {
        return "null";
      // Convert Strings
      } else if(d.is_type(types::str)) {
        return d.write();
      // Convert Numbers
      } else if(d.is_type(types::num)) {
        // Coerce fractions to flonums
        if(d.num.is_exact() && !d.num.is_integer())
          return d.num.to_inexact().str();
        return d.num.str();
      // Convert boolean
      } else if(d.is_type(types::bol)) {
        if(d.bol.val) return "true";
        return "false";
      // Convert Vectors -> Array
      } else if(d.is_type(types::vec)) {
        string vec_json(1,'[');
        for(size_type i = 0, n = d.vec->size(); i < n; ++i) {
          vec_json += convert_scm_to_json(d.vec->operator[](i),args,format);
          if(i+1 < n) vec_json += ", ";
        }
        return vec_json + ']';
      // Convert Alist -> Map
      } else if(d.is_type(types::par)) {
        string map_json(1,'{');
        auto alist_exp = primitive_toolkit::convert_proper_list_to_data_vector(d);
        for(size_type i = 0, n = alist_exp.size(); i < n; ++i) {
          if(!alist_exp[i].is_type(types::par))
            HEIST_THROW_ERR("scm->json invalid alist " << HEIST_PROFILE(d) << " elt"
              "\n     " << HEIST_PROFILE(alist_exp[i]) << " can't convert to a map!"
              << format << HEIST_FCN_ERR("scm->json", args));
          auto item = primitive_toolkit::convert_proper_list_to_data_vector(alist_exp[i]);
          if(item.size() != 2)
            HEIST_THROW_ERR("scm->json invalid alist " << HEIST_PROFILE(d) << " key-value pair"
              "\n     " << HEIST_PROFILE(alist_exp[i]) << " can't convert to a map!"
              << format << HEIST_FCN_ERR("scm->json", args));
          if(!datum_is_a_valid_json_map_key(item[0]))
            HEIST_THROW_ERR("scm->json invalid alist " << HEIST_PROFILE(d) << " key"
              "\n     " << HEIST_PROFILE(alist_exp[i]) << " isn't a string|number|null|bool!"
              << format << HEIST_FCN_ERR("scm->json", args));
          convert_scm_to_json_write_map_pair(map_json,item,args,format);
          if(i+1 < n) map_json += ", ";
        }
        return map_json + '}';
      } else {
        HEIST_THROW_ERR("'scm->json invalid scheme datum " << HEIST_PROFILE(d)
          << " can't be converted into JSON!" << format << HEIST_FCN_ERR("scm->json", args));
        return ""; // never triggered
      }
    }


    // Main handler converting <d> to json and formatting it using <indent_width>
    string format_scm_as_json(data& d, const size_type indent_width, const data_vector& args, const char* format) {
      return format_JSON_indent_width(convert_scm_to_json(d,args,format),indent_width);
    }
  } // End of namespace heist_json_generator

  /******************************************************************************
  * JSON VALIDATION HELPER
  ******************************************************************************/

  // validate that <d> is a candidate for json-ification
  bool is_valid_json_datum(data d)noexcept{
    // Empty List
    if(primitive_toolkit::data_is_nil(d)) {
      return true;
    // Strings
    } else if(d.is_type(types::str)) {
      return true;
    // Numbers
    } else if(d.is_type(types::num)) {
      return true;
    // boolean
    } else if(d.is_type(types::bol)) {
      return true;
    // Vectors
    } else if(d.is_type(types::vec)) {
      for(const auto& item : *d.vec)
        if(!is_valid_json_datum(item))
          return false;
      return true;
    // Alist
    } else if(d.is_type(types::par)) {
      auto alist_exp = primitive_toolkit::convert_proper_list_to_data_vector(d);
      for(size_type i = 0, n = alist_exp.size(); i < n; ++i) {
        if(!alist_exp[i].is_type(types::par)) return false;
        auto item = primitive_toolkit::convert_proper_list_to_data_vector(alist_exp[i]);
        if(item.size() != 2) return false;
        if(!heist_json_generator::datum_is_a_valid_json_map_key(item[0])) return false;
        if(!is_valid_json_datum(item[1])) return false;
      }
      return true;
    } else {
      return false;
    }
  }

} // End of namespace heist::stdlib_json

#endif