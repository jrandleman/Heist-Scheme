// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_input_parser.hpp
// => Parses user's input into an AST for the C++ Heist Scheme Interpreter

#ifndef HEIST_INPUT_PARSER_HPP_
#define HEIST_INPUT_PARSER_HPP_

#include "heist_types.hpp"

/******************************************************************************
* READER HELPER FUNCTIONS
******************************************************************************/

// Determine if at a word boundry (IE if 'c' is NOT part of a variable name)
constexpr bool IS_END_OF_WORD(const char& c, const char& c2) {
  return(!c || isspace(c) || c=='\'' || c=='`'|| c==',' || c=='"' || 
         c=='(' || c==')' || c=='\\' || (c=='#' && (c2=='\\' || c2=='(')));
}

// Check whether data is at the long-hand name of a character
std::pair<chr_type,scm_string> data_is_named_char(const size_type& i, 
                                                  const scm_string& input){
  // char long-hand names & their respective 'char' representations
  static constexpr const char * const char_names[] = {
    "nul",    "space", "newline", "tab",       "vtab",   "page", 
    "return", "esc",   "alarm",   "backspace", "delete",
  };
  static constexpr const chr_type char_syms[] = {
    '\0', ' ', '\n', '\t', '\v', '\f', '\r', '\x1b', '\a', '\b', '\x7f',
  };
  static constexpr const size_type n = sizeof(char_names) / sizeof(char*);
  // current name candidate
  const scm_string name = input.substr(i,9); // "backspace".size() (largest name)
  // seek a long-hand char name instance
  for(size_type j = 0; j < n; ++j)
    if(name.find(char_names[j]) == 0)
      return std::make_pair(char_syms[j], char_names[j]);
  // seek a hexadecimal char name instance
  if(name[0] == 'x' && isxdigit(name[1])) {
    if(isxdigit(name[2]))
      return std::make_pair(std::stoi(name.substr(1,2),nullptr,16), name.substr(0,2));
    return std::make_pair(std::stoi(name.substr(1,1),nullptr,16), name.substr(0,1));
  }
  // didn't find a long-hand instance
  return std::make_pair('\0', "");
}

// Determine whether input[i] = a non-escaped double-quote char 
bool is_non_escaped_double_quote(size_type i, const scm_string& input) {
  if(input[i] != '"') return false;
  if(!i)              return true;
  --i; // mv prior '"'
  size_type escape_counter = 0;
  while(i && input[i] == '\\') ++escape_counter, --i;
  if(!i && input[i] == '\\')   ++escape_counter;
  return (escape_counter & 1) == 0; // odd instance of escapes = escaped '"' char
}

// Determine whether at a paren that isn't a character
bool is_non_char_open_paren(const size_type& i, const scm_string& input) {
  return input[i] == '(' && (i < 2 || input[i-1] != '\\' || input[i-2] != '#');
}
bool is_non_char_close_paren(const size_type& i, const scm_string& input) {
  return input[i] == ')' && (i < 2 || input[i-1] != '\\' || input[i-2] != '#');
}

// Skip past string literal
// PRECONDITION:  input[i] = '"', at the string's start
// POSTCONDITION: input[i] = '"', at the string's end
void skip_string_literal(size_type& i, const scm_string& input) {
  const size_type n = input.size();
  while(i < n && !is_non_escaped_double_quote(++i,input));
}

// Trims spaces betweeen open parens & procedure names/1st-list-elts inside them
// => more consistent formatting aids in parsing the S-Expressions
void rm_optional_spaces_btwn_open_parens_and_proc_names(scm_string& input) {
  for(size_type i = 0; i < input.size(); ++i) {
    if(is_non_escaped_double_quote(i,input)) 
      skip_string_literal(i,input);
    else if(i > 0 && is_non_char_open_paren(i-1,input) && isspace(input[i])) {
      size_type space_count = 0, j = i;
      while(input[j] && isspace(input[j])) ++space_count, ++j;
      input.erase(i,space_count);
    }
  }
}

/******************************************************************************
* READER DATA VALIDATION
******************************************************************************/

// Confirms given a valid scheme expression: ie each '(' has a ')'
bool confirm_valid_scm_expression(const scm_string& input) {
  signed_num paren_count = 0;
  for(size_type i = 0, n = input.size(); i < n; ++i) {
    // account for whether at a paren &/or a string literal
    if(is_non_char_open_paren(i,input))       ++paren_count;
    else if(is_non_char_close_paren(i,input)) --paren_count;
    else if(is_non_escaped_double_quote(i,input)) skip_string_literal(i,input);
    // check whether detected an invalid expression
    if(paren_count<0 || (i==n && input[i-1] != '"')) { 
      if(paren_count < 0) throw READER_ERROR::early_end_paren;   // ')' Found Prior A '('
      else                throw READER_ERROR::incomplete_string; // Improper String
    }
  }
  if(paren_count != 0)
    throw READER_ERROR::incomplete_expression; // Mismatched '(' & ')'
  return (paren_count == 0);
}

/******************************************************************************
* READER QUOTE EXPANSION
******************************************************************************/

// Return size of expandable quotation, hence 0 if NOT expandable
constexpr int is_expandable_quotation(const char& c, const char& c2) {
  return (c==',' && c2=='@') ? 2 : (c=='\'' || c=='`' || c==',') ? 1 : 0; 
}

// Return the expanded prefix of the given quotation shorthand
constexpr const char* expanded_quote_prefix(const char& c, const int& expandable_length) {
  return c=='\'' ? "(quote " : c=='`' ? "(quasiquote " : 
          expandable_length==2 ? "(unquote-splicing " : "(unquote ";
}

// Return "(quote ".size(), "(quasiquote ".size(), "(unquote-splicing ".size(), 
//   or "(unquote ".size() based on the current prefix
constexpr size_type quote_prefix_length(const char& c, const int& expandable_length) {
  return c=='\'' ? 7 : c=='`' ? 12 : expandable_length==2 ? 18 : 9; 
}

// Expands quoted shorthand: '<exp>  => (quote <exp>)
//                           `<exp>  => (quasiquote <exp>)
//                           ,<exp>  => (unquote <exp>)
//                           ,@<exp> => (unquote-splicing <exp>)
// => NOTE: Recusively expands sequential quotations from right to left
//          - IE: '''a = (quote (quote (quote a)))
void expand_quoted_data(scm_string& input) {
  // Trackers for the outermost quote & remaining quotes to expand
  //   => IE sequential-quote recursive-expansion helper variables
  size_type outermost_quote = 0, recusive_quotations = 0;
  int expandable_length = 0; // Tracker for the last expanded quote's length

  for(size_type i = 0; i < input.size(); ++i) {
    // dont expand quotes w/in strings
    if(is_non_escaped_double_quote(i,input)) {
      skip_string_literal(i,input); 
      continue;
    }
    
    // Recursively expand the rest of the quotes, from right to left
    if(recusive_quotations) {
      recusive_quotations -= expandable_length;  // account for addressing this quote
      i = outermost_quote + recusive_quotations; // mv to the next quote on the right
      if(input[i] == '@') --i;                   // mv back to the , in ,@ as needed
      goto quote_expansion_start;                // :P
    }

    // If not recursively expanding a quotation series, get the length of the 
    //   current quotation (if exists)
    expandable_length = is_expandable_quotation(input[i],input[i+1]);

    // if at a non-char ' ` , ,@ (IE _NOT_ at the ' in #\')
    if(expandable_length && (i < 2 || input[i-1] != '\\' || input[i-2] != '#')) {
      if(!input[i+1])              throw READER_ERROR::quoted_end_of_buffer;
      else if(input[i+1]==')')     throw READER_ERROR::quoted_end_of_expression;
      else if(isspace(input[i+1])) throw READER_ERROR::quoted_space;

      // Recursively expand sequential-quotes from right to left
      if(input[i+expandable_length] && 
        is_expandable_quotation(input[i+expandable_length],input[i+expandable_length+1])) {
        outermost_quote = i;     // save the outermost quote's position
        recusive_quotations = 0; // count the total recursive quotations
        int quote_length = 0;    // recusive expandable quotation length counter
        i += expandable_length;
        while((quote_length = is_expandable_quotation(input[i],input[i+1]))) 
          recusive_quotations += quote_length, i += quote_length;
        i -= (input[i-1]=='@') ? 2 : 1; // mv back so input[i] = the rightmost ' ` ,
      }

    quote_expansion_start:

      // assign expansion length to that of the rightmost (and current) quote expansion
      expandable_length = is_expandable_quotation(input[i],input[i+1]); 
      // make the quotation explicit
      const char quote_ch = input[i];
      input.insert(i,expanded_quote_prefix(quote_ch,expandable_length)); // inserts prior 'i'
      i += quote_prefix_length(quote_ch,expandable_length);              // mv i past the expanded prefix
      input.erase(i, expandable_length);                                 // rm "'" "`" "," ",@"
      // insert closing ')' after the quoted expression (quote <exp>)
      size_type j = i + (input[i]=='(');
      const size_type n = input.size();
      if(input[i] == '(') {
        size_type paren_count = 1;
        while(j < n && paren_count) {
          if(is_non_char_open_paren(j,input))       ++paren_count;
          else if(is_non_char_close_paren(j,input)) --paren_count;
          ++j;
        }
      // insert closing ')' after the quoted atom
      } else if(is_non_escaped_double_quote(j,input)) { // quote string literal
        skip_string_literal(j,input), ++j;
      } else {
        // if quoting a char, past the 1st 3 symbols to avoid triggering 'IS_END_OF_WORD' prematurely.
        if(input[j] == '#' && input[j+1] == '\\') j += 3; 
        while(j < n && !IS_END_OF_WORD(input[j],input[j+1])) ++j;
        --i; // move prior 1st char of quotation, in case of double-quoting (ie ''A)
      }
      input.insert(j,")"); // add the closing paren after the expanded quotation.
    }
  }
}

/******************************************************************************
* READER VECTOR LITERAL EXPANSION
******************************************************************************/

// Return whether input[i] is at a non-char start of a vector literal
bool is_vector_literal(const size_type& i, const scm_string& input) {
  return (i+2<input.size() && input[i]=='#' && input[i+1]=='(' && 
         (i<2 || input[i-1]!='\\' || input[i-2]!='#'));
}

// '#(<...>) => '(vector-literal <...>)
void expand_vector_literals(scm_string& input) {
  for(size_type i = 0; i < input.size(); ++i) {
    // dont expand vector literals w/in strings
    if(is_non_escaped_double_quote(i,input)) {
      skip_string_literal(i,input); 
      continue;
    }
    // if at # in a non-char #( (IE _NOT_ at the # in #\#)
    if(is_vector_literal(i,input)) {
      input.erase(i,1);                     // erase #
      input.insert(i+1, "vector-literal "); // splice in "vector-literal " after (
      i += 15;                              // mv past "(vector-literal"
    }
  }
}

/******************************************************************************
* READER NUMBER PARSING FUNCTIONS
******************************************************************************/

// Determine whether char is a valid numeric char
constexpr bool is_valid_number_char(const char& c, const int& base) {
  return is_base_digit(c,base) || c=='.' || c=='/' || (base==10 && (c=='e' || c=='E'));
}

// Confirms whether input[i] is the valid start of a radix-less number literal
constexpr bool is_valid_number_start(const char& c, const char& c2, const int& base) {
  return is_base_digit(c,base) || 
         (c2 && (c=='.' || c=='-' || c=='+') 
             && (is_base_digit(c2,base) || (c!='.'&&c2=='.')));
}

// Returns the lowercase char of the radix if IS a valid radix, else returns 0
// #e=exact, #i=inexact, #b=binary, #o=octal, #d=decimal, #x=hexadecimal
constexpr char is_valid_number_radix(const char& c, const char& c2) {
  const char r = c ? mklower(c2) : 0;
  if(c=='#' && (r=='e' || r=='i' || r=='b' || r=='o' || r=='d' || r=='x'))
    return r;
  return 0;
}

// Confirms whether given an invalid pair of radices, ie 2 exactness or 2 bases
constexpr bool invalid_radix_pair(const char& r1, const char& r2) {
  if(!r1 || !r2) return false;
  if((r1 == 'e' || r1 == 'i') && (r2 == 'e' || r2 == 'i')) return true;
  if(r1 != 'e' && r1 != 'i' && r2 != 'e' && r2 != 'i')     return true;
  return false;
}

// Returns the numeric base to parse the number w/, as per the given radices
constexpr int radix_numerical_base(const char& r1, const char& r2) {
  if(r1 && r1 != 'e' && r1 != 'i')
    return r1 == 'b' ? 2 : r1 == 'o' ? 8 : r1 == 'd' ? 10 : 16;
  if(r2) return r2 == 'b' ? 2 : r2 == 'o' ? 8 : r2 == 'd' ? 10 : 16;
  return 10; // default to decimal
}

// Returns the exactness to convert the number to, as per the given radices
constexpr char radix_numerical_prec(const char& r1, const char& r2) {
  if(r1 == 'e' || r1 == 'i') return r1;
  if(r2 == 'e' || r2 == 'i') return r2;
  return 0; // '0' denotes to deduce the precision
}

// Returns whether at an irrational constant, +nan.0 +inf.o -inf.0
bool is_irrational_const(const size_type& i, const scm_string& input) {
  return input[i+4]=='.' && input[i+5]=='0' && 
         (input[i]=='+' || input[i]=='-') && 
         ((input[i+1]=='n' && input[i+2]=='a' && input[i+3]=='n') || 
          (input[i+1]=='i' && input[i+2]=='n' && input[i+3]=='f'));
}

// Check whether data is a number. 
//   => NOTE: 2.0.0 is a valid Scheme symbol
//            -> Hence must confirm is numeric & NOT symbol prior extraction
bool data_is_number(size_type& i, const scm_string& input, num_type& exp) {
  const size_type n = input.size();
  // check for irrationality
  if(n-i >= 6 && is_irrational_const(i,input)) { // "+inf.0".size() = 6
    exp = num_type(input.substr(i,6));
    i += 5;
    return true;
  }
  
  // check for radices -- may contain both an exactness radix & #-base radix
  const char radix1 = is_valid_number_radix(input[i],input[i+1]);
  const char radix2 = radix1 ? is_valid_number_radix(input[i+2],input[i+3]) : 0;
  if(invalid_radix_pair(radix1, radix2)) return false;
  
  // determine the numeric base & exactness to parse for
  const int  base = radix_numerical_base(radix1,radix2);
  const char prec = radix_numerical_prec(radix1,radix2);
  
  // parse the number after the radix
  const auto num_start = i + ((radix1 && radix2) ? 4 : radix1 ? 2 : 0); // mv past radices
  if(is_valid_number_start(input[num_start],input[num_start+1],base)){
    // account for sign
    scm_string num = (input[num_start]=='-') ? "-" : "";
    size_type period_freq = 0, div_freq = 0, expt_freq = 0;
    size_type j = num_start + (input[num_start]=='-' || input[num_start]=='+');
    
    // parse number
    while(j<n && period_freq<2 && div_freq<2 && expt_freq<2 && is_valid_number_char(input[j],base)) {
      if(input[j]=='.') {
        ++period_freq;
        if(div_freq || expt_freq) return false;
      } else if(input[j]=='/') {
        ++div_freq;
        if(!is_base_digit(input[j+1],base) || period_freq || expt_freq) return false;
      } else if(base == 10 && (input[j]=='e' || input[j]=='E')) {
        ++expt_freq;
        if(input[j+1] == '+' || input[j+1] == '-') num += input[j++];
        if(!is_base_digit(input[j+1],base) || div_freq) return false;
      }
      num += input[j++];
    }
    
    // invalid number if ambiguous precision specification
    if(period_freq == 2 || div_freq == 2 || expt_freq == 2) 
      return false;
    
    // if parsed a valid number
    if(j==n || IS_END_OF_WORD(input[j],input[j+1])) {
      try {
        exp = (base!=10) ? num_type(num,base) : num_type(num);          // base
        if(prec) exp = (prec=='e') ? exp.to_exact() : exp.to_inexact(); // prec
        i = j-1;
        return true;
      } catch(const num_type::error_t& err) {
        return false;
      }
    }
  }
  return false;
}

/******************************************************************************
* READER AST CONSTRUCTION HELPER FUNCTIONS
******************************************************************************/

// Check whether data is a string literal
bool data_is_string(size_type& i, const scm_string& input, scm_string& exp) {
  if(is_non_escaped_double_quote(i,input)) {
    ++i;
    const size_type n = input.size();
    while(i < n && !is_non_escaped_double_quote(i,input)) exp += input[i], ++i;
    return true;
  }
  return false; 
}

// Check whether data is a char
bool data_is_char(size_type& i, const scm_string& input, chr_type& exp) {
  if(input[i] == '#' && input[i+1] == '\\') {
    i += 2;
    if(auto [ch, name] = data_is_named_char(i,input); !name.empty()) 
      exp = ch, i += (name.size()>2) ? name.size()-1 : name.size();
    else exp = input[i];
    return true;
  }
  return false;
}

// Check whether data is a symbol. Assumes confirmed NOT to be in a string & number already.
bool data_is_symbol(size_type& i, const scm_string& input, scm_string& exp) {
  const size_type n = input.size();
  if(i >= n || isspace(input[i]) || input[i] == '(' || input[i] == ')') return false;
  while(i < n && (!isspace(input[i]) && input[i] != '(' && input[i] != ')')) {
    exp += input[i];
    ++i;
  }
  --i;
  return true;
}

// Check whether data is an expression/list. Assumes confirmed NOT to be in a string.
constexpr bool data_is_expression_start(const char& c) {return c == '(';}

// Check whether data is an expression/list. Assumes confirmed NOT to be in a string.
constexpr bool data_is_expression_end(const char& c) {return c == ')';}

/******************************************************************************
* READER MAIN FUNCTIONS -- ABSTRACT SYNTAX TREE CONSTRUCTION
******************************************************************************/

// Parses the scheme expression buffer into a scm_list
void construct_abstract_syntax_tree(size_type& i, const scm_string& input, 
                                                        scm_list& tree) {
  const size_type n = input.size();
  scm_string tmp_str = "", tmp_sym = "";
  chr_type tmp_chr = 0;
  num_type tmp_num = 0;
  for(; i < n; ++i) {
    if(isspace(input[i])) continue;
    tmp_str = tmp_sym = "";
    tmp_num = 0;
    if(data_is_string(i,input,tmp_str))
      tree.push_back(make_str(tmp_str));     // found string atom
    else if(data_is_char(i,input,tmp_chr))
      tree.push_back(data(tmp_chr));         // found char atom
    else if(data_is_number(i,input,tmp_num))
      tree.push_back(data(tmp_num));         // found number atom
    else if(data_is_expression_start(input[i])) {
      scm_list new_list;                     // found new list, recursively parse contents
      construct_abstract_syntax_tree(++i,input,new_list);
      tree.push_back(data(new_list));
    } else if(data_is_expression_end(input[i]))
      return;                                // found end of this list, return
    else if(data_is_symbol(i,input,tmp_sym))
      tree.push_back(data(tmp_sym));         // found symbol atom
    else throw i;                            // unkown type detected, throw index
  }
} 

// Expands scheme input quote-shorthands & returns derived Abstract Syntax Tree
// => GIVEN THE RAW USER INPUT
void parse_input_exp(scm_string&& input, scm_list& abstract_syntax_tree) {
  if(input.empty() || !confirm_valid_scm_expression(input)) return;
  size_type start_index = 0;
  rm_optional_spaces_btwn_open_parens_and_proc_names(input);
  expand_vector_literals(input); // '#(<exp>) => '(vector-literal <exp>)
  expand_quoted_data(input);     // '<exp>    => (quote <exp>)
  construct_abstract_syntax_tree(start_index,input,abstract_syntax_tree);
}
#endif
