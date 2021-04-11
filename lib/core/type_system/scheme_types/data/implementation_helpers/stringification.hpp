// Author: Jordan Randleman -- jordanran199@gmail.com -- stringification.hpp
// => Contains helper functions for "struct data" value stringification for the C++ 
//    Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_DATA_STRINGIFICATION_HELPERS_HPP_
#define HEIST_SCHEME_CORE_DATA_STRINGIFICATION_HELPERS_HPP_

/******************************************************************************
* PRINTER HELPER FUNCTION PROTOTYPES
******************************************************************************/

// DATA PREDICATES
namespace stdlib_streams { bool data_is_stream_pair(const data& d)noexcept; }
namespace primitive_toolkit { bool data_is_proper_list(const data& d)noexcept; }

/******************************************************************************
* CHARACTER ESCAPING & UNESCAPING HELPER PROCEDURES
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
void insert_hex_oct_escaped_char(string& unescaped, const string& str,size_type& i,const int& base)noexcept{
  unescaped += char(std::stoi(str.substr(i), nullptr, base));
  ++i;
  const auto is_parsed_digit = (base == 16) ? ishexdigit : is_oct_escape;
  while(str[i] && is_parsed_digit(str[i])) ++i;
  --i; // account for 'unescape_chars's for-loop ++i
}


// Unescape escaped special characters in the given string (ie "\\n" => "\n")
string unescape_chars(const string& str)noexcept{
  string unescaped; 
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
string escaped_char(int c)noexcept{
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
      return string(hex_str,4);
  }
}


// Escape special characters in the given string (ie "\n" => "\\n")
string escape_chars(const string& str)noexcept{
  string escaped;
  for(size_type i = 0, n = str.size(); i < n; ++i) {
    if(str[i] != '"' && str[i] != '\\' && isprint(str[i]))
      escaped += char(str[i]);
    else
      escaped += escaped_char(str[i]);
  }
  return escaped;
} 

/******************************************************************************
* GENERIC DATA STRINGIFICATION METHOD TYPE ALIAS
******************************************************************************/

using DATA_STRINGIFIER = string(data::*)()const;

/******************************************************************************
* POINTER STRINGIFICATION
******************************************************************************/

template<typename T> // PRECONDITION: <raw_ptr> IS A RAW POINTER
string pointer_to_hexstring(const T raw_ptr)noexcept{
  char str[32];
  snprintf(str, 32, "%zx", size_type(raw_ptr));
  return str;
}

/******************************************************************************
* LIST STRINGIFICATION HELPER FUNCTIONS
******************************************************************************/

// Prototype for stringification helper function
template<DATA_STRINGIFIER to_str>
void stringify_list_recur(string& list_str,const data& slow,const data& fast,par_type cycle_start);

// Confirm data is not the empty list
bool is_not_THE_EMPTY_LIST(const data& pair_data)noexcept{
  return (!pair_data.is_type(types::sym) || pair_data.sym!=symconst::emptylist);
}


// Stringify list recursive helper, ONLY for once the lists is confirmed to be acyclic
template<DATA_STRINGIFIER to_str>
void cio_acyclic_list_str_recur(string& list_str, const data& pair_object) {
  // store car
  if(pair_object.par->first.is_type(types::par)) {
    if(stdlib_streams::data_is_stream_pair(pair_object.par->first)) {
      list_str += "#<stream>";
    } else {
      list_str += '(';
      stringify_list_recur<to_str>(list_str, pair_object.par->first, pair_object.par->first, nullptr);
      list_str += ')';
    }
  } else {
    list_str += (pair_object.par->first.*to_str)();
  }
  // store space if not last item in list
  if(is_not_THE_EMPTY_LIST(pair_object.par->second)) list_str += ' ';
  // store cdr
  if(pair_object.par->second.is_type(types::par)) {
    // check for whether at a cycle
    cio_acyclic_list_str_recur<to_str>(list_str, pair_object.par->second);
  } else if(is_not_THE_EMPTY_LIST(pair_object.par->second)){// don't store last '()
    // store ' . ' since not a null-terminated list
    list_str += G.dot + ' ' + (pair_object.par->second.*to_str)();
  }
}


// Stringify list recursive helper
template<DATA_STRINGIFIER to_str>
void stringify_list_recur(string& list_str, const data& slow, const data& fast, par_type cycle_start) {
  // Check if detected a cycle (simultaneously performs Floyd's Loop Detection algorithm)
  if(fast.is_type(types::par) && fast.par->second.is_type(types::par) && 
     slow.par == fast.par) { 
    auto fast_runner = fast;
    cycle_start = slow.par; // seek cycle's start
      while(cycle_start != fast_runner.par)
        cycle_start = cycle_start->second.par,
        fast_runner = fast_runner.par->second.par->second;
  }
  // store car
  if(slow.par->first.is_type(types::par)) {
    if(stdlib_streams::data_is_stream_pair(slow.par->first)) {
      list_str += "#<stream>";
    } else {
      list_str += '(';
      stringify_list_recur<to_str>(list_str, slow.par->first, slow.par->first, nullptr);
      list_str += ')';
    }
  } else {
    list_str += (slow.par->first.*to_str)();
  }
  // store space if not last item in list
  if(is_not_THE_EMPTY_LIST(slow.par->second)) list_str += ' ';
  // store cdr
  if(slow.par->second.is_type(types::par)) {
    // check for whether at a cycle
    if(slow.par->second.par == cycle_start)
      list_str += "<...cycle>";
    else if(fast.is_type(types::par) && fast.par->second.is_type(types::par))
      stringify_list_recur<to_str>(list_str, slow.par->second, fast.par->second.par->second, cycle_start);
    else
      cio_acyclic_list_str_recur<to_str>(list_str, slow.par->second);
  } else if(is_not_THE_EMPTY_LIST(slow.par->second)){// don't store last '()
    // store ' . ' since not a null-terminated list
    list_str += G.dot + ' ' + (slow.par->second.*to_str)();
  }
}


// Stringify list
template<DATA_STRINGIFIER to_str>
string stringify_list(const data& pair_object) {
  if(stdlib_streams::data_is_stream_pair(pair_object)) return "#<stream>";
  string list_str;
  stringify_list_recur<to_str>(list_str, pair_object.par, pair_object.par, nullptr);
  return '(' + list_str + ')';
}

/******************************************************************************
* VECTOR STRINGIFICATION HELPER FUNCTIONS
******************************************************************************/

// Stringify vector
template<DATA_STRINGIFIER to_str>
string stringify_vect(const vec_type& vector_object) {
  string vect_str("#(");
  for(size_type i = 0, n = vector_object->size(); i < n; ++i) {
    if(vector_object->operator[](i).is_type(types::vec))
      vect_str += stringify_vect<to_str>(vector_object->operator[](i).vec);
    else
      vect_str += (vector_object->operator[](i).*to_str)();
    if(i < n-1) vect_str +=  ' ';
  }
  return vect_str + ')';
}

/******************************************************************************
* EXPRESSION STRINGIFICATION HELPER FUNCTIONS
******************************************************************************/

// Stringify expression recursive helper
template<DATA_STRINGIFIER to_str>
void stringify_expr_rec(const exp_type& exp_object, string& exp_str) {
  if(exp_object.empty()) return; // empty expression
  for(auto d = exp_object.begin(), end = exp_object.end(); d != end; ++d) {
    // Recursively append expressions
    if(d->is_type(types::exp)) {
      exp_str += '(';
      stringify_expr_rec<to_str>(d->exp, exp_str);
      exp_str += ')';
    // Append atomic data
    } else {
      exp_str += ((*d).*to_str)();
    }
    // Add a space if not at the end of the current expression
    if(d+1 != end) exp_str += ' ';
  }
}


// Stringify expression
template<DATA_STRINGIFIER to_str>
string stringify_expr(const exp_type& exp_object) {
  string exp_str;
  stringify_expr_rec<to_str>(exp_object, exp_str);
  return '(' + exp_str + ')';
}

/******************************************************************************
* HASH-TABLE STRINGIFICATION HELPER FUNCTIONS
******************************************************************************/

// Stringify hash-map
template<DATA_STRINGIFIER to_str>
string stringify_hmap(const map_type& map_object) {
  string map_str("$(");
  for(const auto& keyval : map_object->val)
    map_str += (map_object::unhash_key(keyval.first).*to_str)() + ' ' + (keyval.second.*to_str)() + ' ';
  if(map_str.size() > 2)
    *map_str.rbegin() = ')';
  else
    map_str.push_back(')');
  return map_str;
}

/******************************************************************************
* PRETTY STRINGIFICATION HELPER NODE TYPE DEFINITION
******************************************************************************/

using pprint_data = std::vector<struct pprint_datum>;

// Pprint Datum Storing Stringified Data & Length of Stringified Data
struct pprint_datum {
  // <output_len> Denotes length of <exp> or <datum_str> once ouput
  size_type output_len = 0;
  bool is_exp = false, is_sym = false;
  // Either a datum_str non-proper-list-pair or and <exp> of <pprint_datum>
  string datum_str;
  pprint_data exp;
  pprint_datum()                      = default;
  pprint_datum(const pprint_datum& p) = default;
  pprint_datum(pprint_datum&& p)      = default;
  pprint_datum(string&& str,const bool is_symbol=false) : output_len(str.size()),
                                                          is_sym(is_symbol),
                                                          datum_str(std::move(str)) {}
  pprint_datum(const pprint_data& e,const size_type& len) : output_len(len),is_exp(true),exp(e) {}
};

/******************************************************************************
* PRETTY STRINGIFICATION HELPER FUNCTIONS
******************************************************************************/

// Gets the length of <list_as_strs> once output
void get_pprint_data_ouput_length(const pprint_data& list_as_strs,size_type& output_length)noexcept{
  // initial value accounts for outputting both parens & spaces between elts
  output_length = 2 + list_as_strs.size()-1;
  for(const auto& e : list_as_strs) output_length += e.output_len;
}


// Converts Scheme lists of data to an AST list of those data as strings
// NOTE: the <size_type> of the pair denotes the length of the <data> once output 
void stringify_list_data(pprint_data& list_as_strs, size_type& length, const par_type& p) {
  // Strify car
  if(!p->first.is_type(types::par)) {
    list_as_strs.push_back(pprint_datum(p->first.pprint(),p->first.is_type(types::sym)));
  } else if(!primitive_toolkit::data_is_proper_list(p->first)) {
    list_as_strs.push_back(p->first.write());
  } else {
    pprint_data sub_exp;
    size_type sub_exp_len = 0;
    stringify_list_data(sub_exp,sub_exp_len,p->first.par);
    list_as_strs.push_back(pprint_datum(sub_exp,sub_exp_len));
  }
  // Strify cdr
  if(p->second.is_type(types::sym) && p->second.sym == symconst::emptylist) {
    get_pprint_data_ouput_length(list_as_strs,length); // get length of this stringified list as output
    return; // end of list
  }
  if(!p->second.is_type(types::par))
    list_as_strs.push_back(pprint_datum(p->second.pprint(),p->second.is_type(types::sym)));
  else if(!primitive_toolkit::data_is_proper_list(p->second))
    list_as_strs.push_back(p->second.write());
  else
    stringify_list_data(list_as_strs,length,p->second.par);
}


// Stringifies <list_as_strs> w/o any tabs
void print_pprint_data_as_is(const pprint_data& list_as_strs, string& buffer)noexcept{
  buffer += '(';
  for(size_type i = 0, n = list_as_strs.size(); i < n; ++i) {
    if(list_as_strs[i].is_exp)
      print_pprint_data_as_is(list_as_strs[i].exp,buffer);
    else
      buffer += std::move(list_as_strs[i].datum_str);
    if(i+1 != n) buffer += ' ';
  }
  buffer += ')';
}


void pretty_print_pprint_data(const pprint_data&,const size_type&,const size_type&,string&)noexcept;

// Prints a list beginning w/ a non-symbol atom
void pretty_print_list_of_data(const pprint_data& list_as_strs, const size_type& depth, string& buffer, char* tabs)noexcept{
  tabs[2*depth+1] = 0; // shorten tabs to account for specialized stringification
  for(size_type col_length = 2*depth, i = 0, n = list_as_strs.size(); i < n; ++i) {
    if(i && list_as_strs[i].output_len + col_length > G.PPRINT_MAX_COLUMN_WIDTH) {
      buffer += '\n';
      buffer += tabs;
      col_length = 2*depth;
    }
    col_length += list_as_strs[i].output_len + 1; // +1 accounts for spaces
    if(list_as_strs[i].is_exp)
      pretty_print_pprint_data(list_as_strs[i].exp,list_as_strs[i].output_len,depth+1,buffer);
    else
      buffer += std::move(list_as_strs[i].datum_str);
    if(i+1 != n) buffer += ' ';
  }
}


// Show info on the parsed stringified data
void pretty_print_pprint_data(const pprint_data& list_as_strs, const size_type& len,
                              const size_type& depth, string& buffer)noexcept{
  // Print as is if possible
  if(len + 2*depth <= G.PPRINT_MAX_COLUMN_WIDTH || len < 2) {
    print_pprint_data_as_is(list_as_strs,buffer);
    return;
  }
  // Get tab (2 spaces per tab) width as per the depth
  char* tabs = new char [2*depth+3];
  for(size_type i = 0, tabs_len = 2*depth+2; i < tabs_len; ++i) tabs[i] = ' ';
  tabs[2*depth+2] = 0;
  // Open paren
  buffer += '(';
  const size_type n = list_as_strs.size();
  size_type i = 1;
  // If 1st elt is a non-symbol atom, specialize stringification
  if(!list_as_strs[0].is_sym && !list_as_strs[0].is_exp) {
    pretty_print_list_of_data(list_as_strs,depth,buffer,tabs);
    goto end_of_pprint;
  }
  // If 1st elt is a list, hence special stringification case for such applications
  if(list_as_strs[0].is_exp) {
    pretty_print_pprint_data(list_as_strs[0].exp,list_as_strs[0].output_len,depth+1,buffer);
  } else {
    buffer += std::move(list_as_strs[0].datum_str) + ' ';
    // If 2nd elt printable on the current line (another special case)
    if(list_as_strs[1].output_len + list_as_strs[0].output_len + 2*depth < G.PPRINT_MAX_COLUMN_WIDTH){
      i = 2;
      if(list_as_strs[1].is_exp)
        print_pprint_data_as_is(list_as_strs[1].exp,buffer);
      else
        buffer += std::move(list_as_strs[1].datum_str);
    }
  }
  if(i < n) buffer += '\n';
  // Print body of the list
  for(; i < n; ++i) {
    buffer += tabs;
    if(list_as_strs[i].is_exp)
      pretty_print_pprint_data(list_as_strs[i].exp,list_as_strs[i].output_len,depth+1,buffer);
    else
      buffer += std::move(list_as_strs[i].datum_str);
    if(i+1 != n) buffer += '\n';
  }
  // Free <tabs> & add the closing paren
end_of_pprint:
  delete [] tabs;
  buffer += ')';
}


// Pretty printer
string pretty_print(const data& d) {
  // If non-proper-list-pair, print as-is
  if(!primitive_toolkit::data_is_proper_list(d)) return d.write();
  // Else check if pair as string is of valid length
  auto as_string = d.write();
  if(as_string.size() <= G.PPRINT_MAX_COLUMN_WIDTH)
    return as_string;
  // Otherwise get list as string-ified objects
  pprint_data list_as_strs;
  size_type output_length = 0;
  stringify_list_data(list_as_strs,output_length,d.par);
  // Print the list w/ indents
  string buffer;
  pretty_print_pprint_data(list_as_strs,output_length,0,buffer);
  return buffer;
}

/******************************************************************************
* OBJECT STRINGIFICATION HELPER (CHECKS FOR display write pprint METHOD)
******************************************************************************/

template<DATA_STRINGIFIER to_str>
string stringify_obj(const obj_type& object,const char* printer_name) {
  obj_type obj = object;
  while(obj) {
    // search object's local members
    for(size_type i = 0, n = obj->method_names.size(); i < n; ++i) {
      if(obj->method_names[i] == "self->string" || obj->method_names[i] == printer_name) {
        data result = apply_dynamic_method(obj,data_vector(),obj->method_values[i].fcn);
        if(result.is_type(types::str)) return *result.str;
        return (result.*to_str)();
      }
    }
    // search object's prototype
    for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i) {
      if(obj->proto->method_names[i] == "self->string" || obj->proto->method_names[i] == printer_name) {
        obj->method_names.push_back(obj->proto->method_names[i]), obj->method_values.push_back(obj->proto->method_values[i]);
        data result = apply_dynamic_method(obj,data_vector(),obj->method_values[i].fcn);
        if(result.is_type(types::str)) return *result.str;
        return (result.*to_str)();
      }
    }
    // search inherited object prototype
    obj = obj->super;
  }
  return "#<object[0x"+pointer_to_hexstring(object.ptr)+"]>";
}

#endif