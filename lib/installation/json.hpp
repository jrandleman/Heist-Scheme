// Author: Jordan Randleman -- jrandleman@scu.edu -- json.hpp
// => C++ Library Equivalent to JavaScript's JSON.parse & JSON.stringify

// PROVIDES 8 TYPES:
//   0. JSON::Datum
//   1. JSON::Types (enumeration: Str, Int, Flo, Obj, Arr, Bol, Nul)
//   2. JSON::Str_type
//   3. JSON::Int_type
//   4. JSON::Flo_type
//   5. JSON::Obj_type
//   6. JSON::Arr_type
//   7. JSON::Bol_type

// PROVIDES 2 PROCEDURES:
//   0. std::pair<JSON::Datum,std::string> JSON::parse(const std::string& json_string)noexcept; // ".second" yields error message
//   1. std::string JSON::stringify(const JSON::Datum& datum, const std::size_t tab_width = 0, const std::size_t offset = 0)noexcept;

// OF NOTE:
//   0. Thread-safe!
//   1. Uses exceptions internally, but these never leak.
//   2. In addition to standard JSON, also supports "//" & "/**/" comments!

#ifndef JSON_HPP_
#define JSON_HPP_

#include <cctype>
#include <cfloat>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <utility>
#include <vector>

namespace JSON {

  /******************************************************************************
  * JSON AST TYPE SYSTEM
  ******************************************************************************/

  enum class Types {Str, Int, Flo, Obj, Arr, Bol, Nul}; // String, Integer, Float, Object, Array, Boolean, Null

  using Str_type = std::string;
  using Int_type = long long;
  using Flo_type = long double;
  using Obj_type = std::vector<std::pair<std::string,struct Datum>>;
  using Arr_type = std::vector<struct Datum>;
  using Bol_type = bool;

  /******************************************************************************
  * JSON AST DATA STRUCTURE
  ******************************************************************************/

  struct Datum {
    // Invariants
    Types type = Types::Nul; // NULL is just a type designation with no associated value.
    union {
      Str_type Str;
      Int_type Int;
      Flo_type Flo;
      Obj_type Obj;
      Arr_type Arr;
      Bol_type Bol;
    };

    // Assignment operator
    void operator=(const Datum& d)noexcept;
    void operator=(Datum&& d)noexcept;

    // Comparison operators
    bool operator==(const Datum& d)const noexcept;
    bool operator!=(const Datum& d)const noexcept{return !(*this == d);}

    // Constructors
    Datum(const Types& t = Types::Nul) noexcept : type(t) {}

    Datum(const Str_type& s) noexcept : type(Types::Str), Str(s) {}
    Datum(const Int_type& i) noexcept : type(Types::Int), Int(i) {}
    Datum(const Flo_type& f) noexcept : type(Types::Flo), Flo(f) {}
    Datum(const Obj_type& o) noexcept : type(Types::Obj), Obj(o) {}
    Datum(const Arr_type& a) noexcept : type(Types::Arr), Arr(a) {}
    Datum(const Bol_type& b) noexcept : type(Types::Bol), Bol(b) {}

    Datum(Str_type&& s) noexcept : type(Types::Str), Str(std::move(s)) {}
    Datum(Int_type&& i) noexcept : type(Types::Int), Int(std::move(i)) {}
    Datum(Flo_type&& f) noexcept : type(Types::Flo), Flo(std::move(f)) {}
    Datum(Obj_type&& o) noexcept : type(Types::Obj), Obj(std::move(o)) {}
    Datum(Arr_type&& a) noexcept : type(Types::Arr), Arr(std::move(a)) {}
    Datum(Bol_type&& b) noexcept : type(Types::Bol), Bol(std::move(b)) {}

    Datum(const Datum& d) noexcept {*this = d;}
    Datum(Datum&& d)      noexcept {*this = std::move(d);}

    // Destructor
    ~Datum()noexcept;
  };


  // Assignment operator (l-value reference)
  void Datum::operator=(const Datum& d)noexcept{
    if(this == &d) return;
    if(type == d.type) {
      switch(type) {
        case Types::Str: Str = d.Str; return;
        case Types::Int: Int = d.Int; return;
        case Types::Flo: Flo = d.Flo; return;
        case Types::Obj: Obj = d.Obj; return;
        case Types::Arr: Arr = d.Arr; return;
        case Types::Bol: Bol = d.Bol; return;
        default:                      return; // Types::Nul
      }
    } else {
      this->~Datum();
      switch(d.type) {
        case Types::Str: new (this) Datum(d.Str); return;
        case Types::Int: new (this) Datum(d.Int); return;
        case Types::Flo: new (this) Datum(d.Flo); return;
        case Types::Obj: new (this) Datum(d.Obj); return;
        case Types::Arr: new (this) Datum(d.Arr); return;
        case Types::Bol: new (this) Datum(d.Bol); return;
        default:         new (this) Datum(d.type);return; // Types::Nul
      }
    }
  }
  
  // Assignment operator (r-value reference)
  void Datum::operator=(Datum&& d)noexcept{
    if(this == &d) return;
    if(type == d.type) {
      switch(type) {
        case Types::Str: Str = std::move(d.Str); return;
        case Types::Int: Int = std::move(d.Int); return;
        case Types::Flo: Flo = std::move(d.Flo); return;
        case Types::Obj: Obj = std::move(d.Obj); return;
        case Types::Arr: Arr = std::move(d.Arr); return;
        case Types::Bol: Bol = std::move(d.Bol); return;
        default:                                 return; // Types::Nul
      }
    } else {
      this->~Datum();
      switch(d.type) {
        case Types::Str: new (this) Datum(std::move(d.Str)); return;
        case Types::Int: new (this) Datum(std::move(d.Int)); return;
        case Types::Flo: new (this) Datum(std::move(d.Flo)); return;
        case Types::Obj: new (this) Datum(std::move(d.Obj)); return;
        case Types::Arr: new (this) Datum(std::move(d.Arr)); return;
        case Types::Bol: new (this) Datum(std::move(d.Bol)); return;
        default:         new (this) Datum(std::move(d.type));return; // Types::Nul
      }
    }
  }

  // Equality operator
  bool Datum::operator==(const Datum& d)const noexcept{
    if(this == &d)     return true;
    if(type != d.type) return false;
    switch(type) {
      case Types::Str: return Str == d.Str;
      case Types::Int: return Int == d.Int;
      case Types::Flo: return Flo == d.Flo;
      case Types::Obj: return Obj == d.Obj;
      case Types::Arr: return Arr == d.Arr;
      case Types::Bol: return Bol == d.Bol;
      default:         return true; // Types::Nul
    }
  }

  // Destructor
  Datum::~Datum()noexcept{
    switch(type) {
      case Types::Str: Str.~Str_type(); return;
      case Types::Int: Int.~Int_type(); return;
      case Types::Flo: Flo.~Flo_type(); return;
      case Types::Obj: Obj.~Obj_type(); return;
      case Types::Arr: Arr.~Arr_type(); return;
      case Types::Bol: Bol.~Bol_type(); return;
      default:                          return; // Types::Nul
    }
  }

  /******************************************************************************
  * INTERNAL ERROR HANDLING MECHANISM
  ******************************************************************************/

  #define signal_error(signal_error_message)({\
    ERROR_MESSAGE = ">> JSON PARSING ERROR: " + std::string(signal_error_message) + \
                  "\n   @FILE: " + std::string(__FILE__) + \
                  "\n   @FUNC: " + std::string(__func__) + \
                  "\n   @LINE: " + std::to_string(__LINE__);\
    throw 1;\
  })

  /******************************************************************************
  * INTERNAL "CARET" ERROR MESSAGE GENERATION
  ******************************************************************************/

  std::string get_caret_error_message(const std::string& str, const std::size_t i, const std::size_t tab_width = 3, 
                                                              const std::size_t max_err_message_width = 80)noexcept{
    const std::size_t start = (i < max_err_message_width/2) ? 0 : i-(max_err_message_width/2);
    const auto& substr = str.substr(start,max_err_message_width);
    std::string caret_str(substr.size(),' '), tab(tab_width, ' ');
    caret_str[i-start] = '^';
    return '\n' + tab + substr + '\n' + tab + caret_str;
  }

  /******************************************************************************
  * STRINGIFICATION
  ******************************************************************************/

  namespace stringify_helpers {
    std::string stringify_Arr(const Arr_type& Arr, const std::size_t currrent_tab_width, const std::size_t tab_width)noexcept;
    std::string stringify_Obj(const Obj_type& Obj, const std::size_t currrent_tab_width, const std::size_t tab_width)noexcept;
    std::string stringify_Flo(const long double& flonum)noexcept;


    std::string stringify_Datum(const Datum& d, const std::size_t currrent_tab_width, const std::size_t tab_width)noexcept{
      std::string str;
      switch(d.type) {
        case Types::Str: str += '"' + d.Str + '"'; break;
        case Types::Int: str += std::to_string(d.Int); break;
        case Types::Flo: str += stringify_Flo(d.Flo); break;
        case Types::Obj: str += stringify_Obj(d.Obj, currrent_tab_width, tab_width); break;
        case Types::Arr: str += stringify_Arr(d.Arr, currrent_tab_width, tab_width); break;
        case Types::Bol: str += d.Bol ? "true" : "false"; break; 
        case Types::Nul: str += "null"; break;
      }
      return str;
    }


    std::string stringify_Arr(const Arr_type& Arr, const std::size_t currrent_tab_width, const std::size_t tab_width)noexcept{
      std::string str(1,'[');
      const std::size_t n = Arr.size();
      for(std::size_t i = 0; i < n; ++i) {
        if(tab_width) str += '\n' + std::string(currrent_tab_width+tab_width, ' ');
        str += stringify_Datum(Arr[i],currrent_tab_width+tab_width,tab_width);
        if(i+1 < n) str += ',';
      }
      if(n && tab_width) str += '\n' + std::string(currrent_tab_width, ' ');
      str += ']';
      return str;
    }


    std::string stringify_Obj(const Obj_type& Obj, const std::size_t currrent_tab_width, const std::size_t tab_width)noexcept{
      std::string str(1,'{');
      const std::size_t n = Obj.size();
      for(std::size_t i = 0; i < n; ++i) {
        if(tab_width) str += '\n' + std::string(currrent_tab_width+tab_width, ' ');
        str += '"' + Obj[i].first + '"';
        str += ':';
        if(tab_width) str += ' ';
        str += stringify_Datum(Obj[i].second, currrent_tab_width+tab_width, tab_width);
        if(i+1 < n) str += ',';
      }
      if(n && tab_width) str += '\n' + std::string(currrent_tab_width, ' ');
      str += '}';
      return str;
    }


    #define LD_SNPRINTF_FORMAT_LOGIC(x) "%."#x"Lg"
    #define LD_SNPRINTF_FORMAT(x) LD_SNPRINTF_FORMAT_LOGIC(x)

    std::string stringify_Flo(const long double& flonum)noexcept{
      char str[128]; // 128 gives AMPLE room to play with
      snprintf(str, 128, LD_SNPRINTF_FORMAT(LDBL_DIG), flonum);
      // add ".0" if no decimal/scientific notation found
      char* p = str;
      while(*p) {
        if(*p == '.' || *p == 'e' || *p == 'E') return str;
        ++p;
      }
      *(p++) = '.';
      *(p++) = '0';
      *p = 0;
      return str;
    }

    #undef LD_SNPRINTF_FORMAT_LOGIC
    #undef LD_SNPRINTF_FORMAT
  } // End of namespace stringify_helpers

  
  std::string stringify(const Datum& datum, const std::size_t tab_width = 0, const std::size_t offset = 0)noexcept{
    return std::string(offset, ' ') + stringify_helpers::stringify_Datum(datum, offset, tab_width);
  }

  /******************************************************************************
  * PARSING
  ******************************************************************************/

  namespace parse_helpers {
    Datum parse_Datum(const std::string& str, const std::size_t n, std::size_t& i, std::string& ERROR_MESSAGE);


    bool char_is_valid_integer_component(const char c)noexcept{
      return (c >= '0' && c <= '9') || c == '+' || c == '-';
    }


    bool char_is_valid_float_component(const char c)noexcept{
      return char_is_valid_integer_component(c) || c == 'e' || c == 'E' || c == '.';
    }


    bool at_single_line_comment_begin(const std::string& str, const std::size_t n, const std::size_t i)noexcept{
      return i+1 < n && str[i] == '/' && str[i+1] == '/';
    }


    bool at_multi_line_comment_begin(const std::string& str, const std::size_t n, const std::size_t i)noexcept{
      return i+1 < n && str[i] == '/' && str[i+1] == '*';
    }


    bool at_multi_line_comment_end(const std::string& str, const std::size_t n, const std::size_t i)noexcept{
      return i+1 < n && str[i] == '*' && str[i+1] == '/';
    }


    // Determine whether str[i] = a non-escaped double-quote char
    bool is_non_escaped_double_quote(const std::string& str, std::size_t i)noexcept{
      if(str[i] != '"') return false;
      if(!i)            return true;
      --i; // mv prior '"'
      std::size_t escape_counter = 0;
      while(i && str[i] == '\\') ++escape_counter, --i;
      if(!i && str[i] == '\\')   ++escape_counter;
      return (escape_counter & 1) == 0; // odd instance of escapes = escaped '"' char
    }


    // PRECONDITION:  str[i] = '"', at the string's start
    // POSTCONDITION: str[i] = '"', at the string's end
    std::string skip_string_literal(const std::string& str, const std::size_t n, std::size_t& i)noexcept{
      std::string skipped_string;
      while(i < n && !is_non_escaped_double_quote(str,++i))
        skipped_string += str[i];
      return skipped_string;
    }


    std::string strip_whitespace_and_comments(const std::string& str, std::string& ERROR_MESSAGE) {
      std::string stripped;
      bool in_single_line_comment = false, in_multi_line_comment = false;
      for(std::size_t i = 0, n = str.size(); i < n; ++i) {
        if(in_single_line_comment) {
          if(str[i] == '\n') in_single_line_comment = false;
        } else if(in_multi_line_comment) {
          if(at_multi_line_comment_end(str,n,i)) in_multi_line_comment = false, ++i;
        } else if(at_single_line_comment_begin(str,n,i)) {
          in_single_line_comment = true;
        } else if(at_multi_line_comment_begin(str,n,i)) {
          in_multi_line_comment = true;
        } else if(str[i] == '"') {
          stripped += '"' + skip_string_literal(str,n,i) + '"';
          if(i >= n) signal_error("INCOMPLETE STRING LITERAL!");
        } else if(!isspace(str[i])) {
          stripped += str[i];
        }
      }
      if(in_multi_line_comment) signal_error("INCOMPLETE MULTI-LINE COMMENT!");
      return stripped;
    }


    Datum parse_string(const std::string& str, const std::size_t n, std::size_t& i, std::string& ERROR_MESSAGE) {
      Datum d = skip_string_literal(str,n,i);
      if(i >= n) signal_error("INCOMPLETE STRING LITERAL!");
      ++i; // mv past closing '"'
      return d;
    }


    Datum parse_bool(const std::string& str, std::size_t& i, std::string& ERROR_MESSAGE) {
      if(str[i] == 't' && (str[i+1] != 'r' || str[i+2] != 'u' || str[i+3] != 'e')) 
        signal_error("UNKOWN JSON TOKEN:" + get_caret_error_message(str,i));
      if(str[i] == 'f' && (str[i+1] != 'a' || str[i+2] != 'l' || str[i+3] != 's' || str[i+4] != 'e')) 
        signal_error("UNKOWN JSON TOKEN:" + get_caret_error_message(str,i));
      if(str[i] == 't') {
        i += 4; 
        return Datum(true);
      } 
      i += 5; 
      return Datum(false);
    }


    Datum parse_null(const std::string& str, std::size_t& i, std::string& ERROR_MESSAGE) {
      if(str[i+1] != 'u' || str[i+2] != 'l' || str[i+3] != 'l') 
        signal_error("UNKOWN JSON TOKEN:" + get_caret_error_message(str,i));
      i += 4;
      return Datum(Types::Nul);
    }


    Datum parse_integer(const std::string& str, const std::size_t n, std::size_t& i, std::string& ERROR_MESSAGE) {
      std::string num_str;
      const auto original_i = i;
      while(i < n && char_is_valid_integer_component(str[i])) num_str += str[i++];
      try {
        return Datum(std::stoll(num_str));
      } catch(...) {
        signal_error("UNKOWN JSON TOKEN:" + get_caret_error_message(str,original_i));
      }
      return Datum();
    }


    Datum parse_float(const std::string& str, const std::size_t n, std::size_t& i, std::string& ERROR_MESSAGE) {
      std::string num_str;
      const auto original_i = i;
      while(i < n && char_is_valid_float_component(str[i])) num_str += str[i++];
      try {
        return Datum(std::stold(num_str));
      } catch(...) {
        signal_error("UNKOWN JSON TOKEN:" + get_caret_error_message(str,original_i));
      }
      return Datum();
    }


    Datum parse_array(const std::string& str, const std::size_t n, std::size_t& i, std::string& ERROR_MESSAGE) {
      Datum d = Arr_type();
      ++i; // mv past opening '['
      while(i < n && str[i] != ']') {
        d.Arr.push_back(parse_Datum(str,n,i,ERROR_MESSAGE));
        if(i >= n) signal_error("INCOMPLETE ARRAY LITERAL!");
        if(str[i] == ',') {
          ++i; // mv past the comma separating values
        } else if(str[i] != ']') {
          signal_error("IMPROPERLY FORMATTED ARRAY LITERAL:" + get_caret_error_message(str,i));
        }
      }
      if(i >= n) signal_error("INCOMPLETE ARRAY LITERAL!");
      ++i; // mv past closing ']'
      return d;
    }


    Datum parse_object(const std::string& str, const std::size_t n, std::size_t& i, std::string& ERROR_MESSAGE) {
      Datum d = Obj_type();
      ++i; // mv past opening '{'
      while(i < n && str[i] != '}') {
        if(str[i] != '"') signal_error("OBJECT KEY ISN'T A STRING:" + get_caret_error_message(str,i));
        std::string key = parse_string(str,n,i,ERROR_MESSAGE).Str;
        if(str[i] != ':') signal_error("IMPROPER OBJECT, KEY ISN'T FOLLOWED BY A ':':" + get_caret_error_message(str,i));
        ++i; // mv past ':'
        Datum val = parse_Datum(str,n,i,ERROR_MESSAGE);
        if(str[i] == ',') {
          ++i; // mv past the comma separating values
        } else if(str[i] != '}') {
          signal_error("IMPROPERLY FORMATTED OBJECT LITERAL:" + get_caret_error_message(str,i));
        }
        d.Obj.push_back(std::make_pair(std::move(key),std::move(val)));
      }
      if(i >= n) signal_error("INCOMPLETE OBJECT LITERAL!");
      ++i; // mv past closing '}'
      return d;
    }


    Datum parse_Datum(const std::string& str, const std::size_t n, std::size_t& i, std::string& ERROR_MESSAGE) {
      if(str[i] == '{') return parse_object(str,n,i,ERROR_MESSAGE);
      if(str[i] == '[') return parse_array(str,n,i,ERROR_MESSAGE);
      if(str[i] == '"') return parse_string(str,n,i,ERROR_MESSAGE);
      if(str[i] == 't' || str[i] == 'f') return parse_bool(str,i,ERROR_MESSAGE);
      if(str[i] == 'n') return parse_null(str,i,ERROR_MESSAGE);
      // parsing a number: determine whether a float or integer
      if(!char_is_valid_float_component(str[i])) signal_error("UNKOWN JSON TOKEN:" + get_caret_error_message(str,i));
      std::size_t j = i;
      while(j < n && char_is_valid_integer_component(str[j])) ++j;
      if(j < n && char_is_valid_float_component(str[j]))
        return parse_float(str,n,i,ERROR_MESSAGE);
      return parse_integer(str,n,i,ERROR_MESSAGE);
    }
  } // End of namespace parse_helpers


  std::pair<JSON::Datum,std::string> parse(const std::string& json_string)noexcept{
    std::string ERROR_MESSAGE;
    try {
      std::size_t i = 0;
      const auto& stripped = parse_helpers::strip_whitespace_and_comments(json_string,ERROR_MESSAGE);
      return std::make_pair(parse_helpers::parse_Datum(stripped,stripped.size(),i,ERROR_MESSAGE), "");
    } catch(...) {
      return std::make_pair(Datum(),ERROR_MESSAGE);
    }
  }
} // End of namespace JSON

#undef signal_error
#endif