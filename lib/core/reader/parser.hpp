// Author: Jordan Randleman -- jordanran199@gmail.com -- parser.hpp
// => Parses user's input into an AST for the C++ Heist Scheme Interpreter
//
// => Parser Function: 
//    void parse_input_exp(string&& input, data_vector& abstract_syntax_tree)

#ifndef HEIST_SCHEME_CORE_READER_PARSER_HPP_
#define HEIST_SCHEME_CORE_READER_PARSER_HPP_

#include "errors.hpp" // reader error handling mechanisms

/******************************************************************************
* READER HELPER FUNCTIONS
******************************************************************************/

namespace heist {

  constexpr bool IS_OPEN_PAREN(const char& c)  noexcept {return c=='('||c=='['||c=='{';}
  constexpr bool IS_CLOSE_PAREN(const char& c) noexcept {return c==')'||c==']'||c=='}';}

  // Determine if at a word boundry (IE if 'c' is NOT part of a variable name)
  constexpr bool IS_END_OF_WORD(const char& c, const char& c2) noexcept {
    return (!c || isspace(c) || c=='"' || IS_OPEN_PAREN(c) || IS_CLOSE_PAREN(c) || 
           (c=='#' && (c2=='\\' || IS_OPEN_PAREN(c2))) || (c=='$' && IS_OPEN_PAREN(c2)));
  }

  bool string_begins_with(const string& str, const char* substr, size_type begin)noexcept{ // begin defaults to 0
    const size_type n = str.size();
    if(begin >= n) return false;
    const char* p = substr;
    for(; begin < n && *p; ++begin, ++p)
      if(str[begin] != *p) return false;
    return !*p;
  }

  // Check whether data is at the long-hand name of a character
  std::pair<chr_type,string> data_is_named_char(const size_type& i, const string& input)noexcept{
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
    const string name = input.substr(i,9); // "backspace".size() (largest name)
    // seek a long-hand char name instance
    for(size_type j = 0; j < n; ++j)
      if(string_begins_with(name,char_names[j]))
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
  bool is_non_escaped_double_quote(size_type i, const string& input) noexcept {
    if(input[i] != '"') return false;
    if(!i)              return true;
    --i; // mv prior '"'
    size_type escape_counter = 0;
    while(i && input[i] == '\\') ++escape_counter, --i;
    if(!i && input[i] == '\\')   ++escape_counter;
    return (escape_counter & 1) == 0; // odd instance of escapes = escaped '"' char
  }

  // Determine whether at a paren that isn't a character
  bool is_non_char_open_paren(const size_type& i, const string& input) noexcept {
    return IS_OPEN_PAREN(input[i]) && (i < 2 || input[i-1] != '\\' || input[i-2] != '#');
  }
  bool is_non_char_close_paren(const size_type& i, const string& input) noexcept {
    return IS_CLOSE_PAREN(input[i]) && (i < 2 || input[i-1] != '\\' || input[i-2] != '#');
  }

  // Determine whether at a comment's start that isn't a character
  bool is_single_line_comment(const size_type& i, const string& input) noexcept {
    return (i<2 || input[i-2]!='#' || input[i-1]!='\\') && input[i]==';';
  }
  bool is_multi_line_comment(const size_type& i, const string& input) noexcept {
    return (i<2 || input[i-2]!='#' || input[i-1]!='\\') && input[i]=='#' && input[i+1]=='|';
  }

  // Skip past string literal
  // PRECONDITION:  input[i] = '"', at the string's start
  // POSTCONDITION: input[i] = '"', at the string's end
  void skip_string_literal(size_type& i, const string& input) noexcept {
    const size_type n = input.size();
    while(i < n && !is_non_escaped_double_quote(++i,input));
  }

  // Skip past single-line comment: input[i] ends directly prior '\n'
  void skip_single_line_comment(size_type& i, const string& input) noexcept {
    while(input[i+1] && input[i+1] != '\n') ++i;
  }

  // Skip past multi-line comment: input[i] ends at '#' of "|#"
  void skip_multi_line_comment(size_type& i, const string& input) noexcept {
    while(input[i+1] && (input[i]!='|'||input[i+1]!='#')) ++i;
    if(input[i+1]) ++i;
  }

  // Remove comments and trim whitespace after open parens
  void strip_comments_and_redundant_whitespace(string& input) {
    for(size_type i = 0; i < input.size();) {
      // Skip past strings & characters
      if(is_non_escaped_double_quote(i,input)) {
        skip_string_literal(i,input);
      } else if(input[i]=='#' && input[i+1]=='\\') {
        i += 3;
      // Trim redundant whitespace
      } else if(i>0 && is_non_char_open_paren(i-1,input) && isspace(input[i])) {
        size_type space_count = 0, j = i;
        while(input[j] && isspace(input[j])) ++space_count, ++j;
        input.erase(i,space_count);
      // Remove single & multi-line comments
      } else if(is_single_line_comment(i,input)) {
        size_type comment_size = 0, j = i;
        while(input[j] && input[j] != '\n') ++comment_size, ++j;
        input.erase(i,comment_size);
      } else if(is_multi_line_comment(i,input)) {
        size_type comment_size = 0, j = i;
        while(input[j+1] && (input[j]!='|'||input[j+1]!='#')) ++comment_size,++j;
        if(!input[j+1]) throw READER_ERROR::incomplete_comment;
        input.erase(i,comment_size+2); // +2 also erases closing '|#'
        continue;                      // ++i would miss adjacent mutli-lines
      }
      ++i;
    }
  }

  // Lowercase-ify all non-char & non-string tokens (case-insensitivity)
  void render_input_cAsE_iNsEnSiTiVe(string& input) noexcept {
    for(size_type i = 0, n = input.size(); i < n; ++i) {
      if(is_non_escaped_double_quote(i,input)) skip_string_literal(i,input);
      else if(i < n-2 && input[i] == '#' && input[i+1] == '\\') i += 3;
      else input[i] = scm_numeric::mklower(input[i]);
    }
  }

  /******************************************************************************
  * READER DATA VALIDATION
  ******************************************************************************/

  // Confirms given a valid scheme expression: ie each '(' has a ')'
  bool confirm_valid_scm_expression(const string& input) {
    long long paren_count = 0;
    for(size_type i = 0, n = input.size(); i < n; ++i) {
      // account for whether at a paren, string literal, or comment
      if(is_non_char_open_paren(i,input))       ++paren_count;
      else if(is_non_char_close_paren(i,input)) --paren_count;
      else if(is_non_escaped_double_quote(i,input)) {
        skip_string_literal(i,input);
        if(i == n) throw READER_ERROR::incomplete_string;
      } else if(is_single_line_comment(i,input)) {
        skip_single_line_comment(i,input);
      } else if(is_multi_line_comment (i,input)) {
        skip_multi_line_comment (i,input);
        if(i == n) throw READER_ERROR::incomplete_comment;
      }
      // check whether detected an invalid expression
      if(paren_count<0 || (i>=n && input[n-1] != '"')) { 
        if(paren_count < 0) throw READER_ERROR::early_end_paren;   // ')' Found Prior '('
        else                throw READER_ERROR::incomplete_string; // Improper String
      }
    }
    if(paren_count != 0)
      throw READER_ERROR::incomplete_expression; // Mismatched '(' & ')'
    return (paren_count == 0);
  }

  /******************************************************************************
  * READER MACROS & QUOTE EXPANSION
  ******************************************************************************/

  // Reader macro identification / error-checking helpers
  bool is_escaped_variadic(const string& input, const size_type i)noexcept{ // do NOT expand \ as a lambda around ...
    if(input[i] != '\\') return false;
    size_type n = input.size(), j = i;
    while(j < n && input[j] == '\\') ++j;
    return input.compare(j,3,"...") == 0;
  }

  bool input_begins_with_prefix(const string& prefix, const string& input, const size_type i)noexcept{
    return input.size()-i-1 >= prefix.size() && !is_escaped_variadic(input,i) && input.compare(i,prefix.size(),prefix) == 0;
  }

  size_type is_expandable_reader_macro(const string& input, const size_type i)noexcept{
    for(size_type j = 0, n = G.SHORTHAND_READER_MACRO_REGISTRY.size(); j < n; ++j)
      if(input_begins_with_prefix(G.SHORTHAND_READER_MACRO_REGISTRY[j],input,i))
        return j;
    return GLOBALS::MAX_SIZE_TYPE;
  }

  void check_for_improper_reader_macro_use(char c) {
    if(!c)                throw READER_ERROR::quoted_end_of_buffer;
    if(IS_CLOSE_PAREN(c)) throw READER_ERROR::quoted_end_of_expression;
    if(isspace(c))        throw READER_ERROR::quoted_space;
  }


  // Reader macro expansion helpers
  void expand_reader_macro(string& input, size_type& i, size_type after_macro_idx, const size_type& macro_idx)noexcept{
    input.insert(after_macro_idx,")"); // insert quote after string literal
    input.erase(i,G.SHORTHAND_READER_MACRO_REGISTRY[macro_idx].size()); // erase shorthand
    input.insert(i,'(' + G.LONGHAND_READER_MACRO_REGISTRY[macro_idx] + ' '); // add longhand
    i += 1 + G.LONGHAND_READER_MACRO_REGISTRY[macro_idx].size(); // mv past longhand
  }

  void expand_around_string_literal(string& input, size_type& i, const size_type& macro_idx, size_type after_macro_idx)noexcept{
    skip_string_literal(after_macro_idx,input);
    expand_reader_macro(input,i,after_macro_idx+1,macro_idx);
  }

  void expand_around_char_literal(string& input, size_type& i, const size_type& macro_idx, size_type after_macro_idx) {
    after_macro_idx += 3;
    const size_type n = input.size();
    while(after_macro_idx < n && !IS_END_OF_WORD(input[after_macro_idx],input[after_macro_idx+1])) 
      ++after_macro_idx;
    if(after_macro_idx > n) throw READER_ERROR::quoted_incomplete_char; // Improper '#\ quotation of incomplete char
    expand_reader_macro(input,i,after_macro_idx,macro_idx);
  }

  void expand_around_expression_literal(string& input, size_type& i, const size_type& macro_idx, size_type after_macro_idx)noexcept{
    after_macro_idx += 1;
    const size_type n = input.size();
    size_type paren_count = 1;
    while(after_macro_idx < n && paren_count) {
      if(is_non_char_open_paren(after_macro_idx,input))       ++paren_count;
      else if(is_non_char_close_paren(after_macro_idx,input)) --paren_count;
      else if(is_non_escaped_double_quote(after_macro_idx,input)) skip_string_literal(after_macro_idx,input);
      ++after_macro_idx;
    }
    expand_reader_macro(input,i,after_macro_idx,macro_idx);
  }

  void expand_around_symbol_or_number_literal(string& input, size_type& i, const size_type& macro_idx, size_type after_macro_idx)noexcept{
    ++after_macro_idx;
    while(!IS_END_OF_WORD(input[after_macro_idx],input[after_macro_idx+1]))
      ++after_macro_idx;
    expand_reader_macro(input,i,after_macro_idx,macro_idx);
  }


  // Returns whether expanded around a literal -- READER MACRO EXPANSION MAIN DISPATCH
  bool expand_around_literal(string& input, size_type& i, const size_type& macro_idx, size_type after_macro_idx) {
    // expanding around string literal
    if(input[after_macro_idx] == '"') {
      expand_around_string_literal(input,i,macro_idx,after_macro_idx);
      return true;
    }
    // expanding around char literal
    if(input[after_macro_idx] == '#' && input[1+after_macro_idx] == '\\') {
      expand_around_char_literal(input,i,macro_idx,after_macro_idx);
      return true;
    }
    // expanding around expression literal
    if(IS_OPEN_PAREN(input[after_macro_idx])) {
      expand_around_expression_literal(input,i,macro_idx,after_macro_idx);
      return true;
    }
    return false;
  }


  // Expands around a nested reader macro
  void expand_around_nested_reader_macros(string& input, size_type& i, const size_type& macro_idx, size_type after_macro_idx){
    // Confirm proper nested reader macro use
    check_for_improper_reader_macro_use(input[after_macro_idx]);
    // Try expanding around a literal
    if(expand_around_literal(input,i,macro_idx,after_macro_idx)) return;
    // Else, expand around a symbol
    expand_around_symbol_or_number_literal(input,i,macro_idx,after_macro_idx);
  }


  // Returns length of nested reader macros AFTER the current reader macro (0 if none)
  size_type is_nested_reader_macro(const string& input, const size_type& i, const size_type& macro_idx)noexcept{
    size_type after_macro_idx = i + G.SHORTHAND_READER_MACRO_REGISTRY[macro_idx].size();
    size_type nested_macro_idx = is_expandable_reader_macro(input,after_macro_idx);
    if(nested_macro_idx == GLOBALS::MAX_SIZE_TYPE) return 0; // reader macro not found
    size_type nested_macro_length = G.SHORTHAND_READER_MACRO_REGISTRY[nested_macro_idx].size();
    while((nested_macro_idx = is_expandable_reader_macro(input,after_macro_idx+nested_macro_length)) != GLOBALS::MAX_SIZE_TYPE)
      nested_macro_length += G.SHORTHAND_READER_MACRO_REGISTRY[nested_macro_idx].size();
    return nested_macro_length;
  }


  // Expands reader macro shorthands: '<exp>  => (quote <exp>)
  // => NOTE: Recusively expands sequential quotations from right to left
  //          - IE: '''a = (quote (quote (quote a)))
  void expand_reader_macro_shorthands(string& input) {
    for(size_type i = 0; i < input.size(); ++i) {
      // Don't expand reader macros in string or char literals
      if(is_non_escaped_double_quote(i,input)) {
        skip_string_literal(i,input); 
        continue; // dont expand quotes/reader-macros w/in strings
      } else if(i+1 < input.size() && input[i] == '#' && input[i+1] == '\\') {
        i += 2; continue; // dont expand quotes/reader-macros as part of a char literal
      }
      // Determine if at a reader macro/quote
      size_type macro_idx = is_expandable_reader_macro(input,i);
      if(macro_idx == GLOBALS::MAX_SIZE_TYPE) continue; // reader macro not found
      // Confirm proper reader macro use
      size_type after_macro_idx = i + G.SHORTHAND_READER_MACRO_REGISTRY[macro_idx].size();
      check_for_improper_reader_macro_use(input[after_macro_idx]);
      size_type nested_macro_length = 0;
      // Expand around char/string/expression literal
      if(expand_around_literal(input,i,macro_idx,after_macro_idx)) continue;
      // Expand around nested reader macros
      if((nested_macro_length = is_nested_reader_macro(input,i,macro_idx))) {
        expand_around_nested_reader_macros(input,i,macro_idx,after_macro_idx+nested_macro_length);
        continue;
      }
      // Expand around symbol or number literal
      expand_around_symbol_or_number_literal(input,i,macro_idx,after_macro_idx);
    }
  }

  /******************************************************************************
  * READER VECTOR & HMAP LITERAL EXPANSION
  ******************************************************************************/

  template<char literal_prefix>
  bool is_vector_or_hmap_literal(const size_type& i, const string& input) noexcept {
    return (i+2<input.size() && input[i]==literal_prefix && IS_OPEN_PAREN(input[i+1]) && 
           (i<2 || input[i-1]!='\\' || input[i-2]!='#'));
  }

  // #(<...>) => (vector-literal <...>)
  // $(<...>) => (hmap-literal <...>)
  template<char literal_prefix,size_type prefix_expansion_length>
  void expand_vector_or_hmap_literals(string& input, const char* prefix_expansion) noexcept {
    for(size_type i = 0; i < input.size(); ++i) {
      // dont expand vector literals w/in strings
      if(is_non_escaped_double_quote(i,input)) {
        skip_string_literal(i,input); 
        continue;
      }
      // if at <literal_prefix> in a non-char <literal_prefix>( (IE _NOT_ at the <literal_prefix> in #\<literal_prefix>)
      if(is_vector_or_hmap_literal<literal_prefix>(i,input)) {
        input.erase(i,1);                    // erase <literal_prefix>
        input.insert(i+1, prefix_expansion); // splice in <prefix_expansion> after (
        i += prefix_expansion_length;        // mv past "(<prefix_expansion>"
      }
    }
  }

  void expand_vector_and_hmap_literals(string& input) noexcept {
    expand_vector_or_hmap_literals<'#',sizeof("vector-literal ")-1>(input,"vector-literal ");
    expand_vector_or_hmap_literals<'$',sizeof("hmap-literal ")-1>(input,"hmap-literal ");
  }

  /******************************************************************************
  * INFIX OPERATOR INF PRECEDENCE SCOPING EXPANSION
  ******************************************************************************/

  // "{" => "(heist:core:inf-precedence "
  void expand_inf_precedence_scoping(string& input) noexcept {
    for(size_type i = 0; i < input.size(); ++i) {
      if(is_non_escaped_double_quote(i,input))
        skip_string_literal(i,input); 
      else if(input[i] == '{' && (i <= 1 || input[i-1] != '\\' || input[i-2] != '#'))
        input = (input.substr(0,i) + '(') + symconst::inf_precedence + (' ' + input.substr(i+1));
    }
  }

  /******************************************************************************
  * READER ALIAS CONVERSION
  ******************************************************************************/

  bool symbol_is_reader_alias(const string& sym, size_type& idx)noexcept{
    auto result = std::find(G.SHORTHAND_READER_ALIAS_REGISTRY.begin(),
                            G.SHORTHAND_READER_ALIAS_REGISTRY.end(),sym);
    if(result != G.SHORTHAND_READER_ALIAS_REGISTRY.end()) {
      idx = result - G.SHORTHAND_READER_ALIAS_REGISTRY.begin();
      return true;
    }
    return false;
  }

  void expand_reader_aliases(data_vector& ast)noexcept{
    if(G.SHORTHAND_READER_ALIAS_REGISTRY.empty()) return;
    size_type idx = 0;
    for(size_type i = 0, n = ast.size(); i < n; ++i) {
      if(ast[i].is_type(types::exp))
        expand_reader_aliases(ast[i].exp);
      else if(ast[i].is_type(types::sym) && symbol_is_reader_alias(ast[i].sym,idx))
        ast[i].sym = G.LONGHAND_READER_ALIAS_REGISTRY[idx];
    }
  }

  /******************************************************************************
  * READER LAMBDA SHORTHAND EXPANSION
  ******************************************************************************/

  bool datum_is_reader_lambda_arg(const data& d)noexcept{
    if(!d.is_type(types::sym) || d.sym.size() < 2 || d.sym[0] != '%') return false;
    if(d.sym.size() == 2 && d.sym[1] == '%') return true;
    for(size_type i = 1, n = d.sym.size(); i < n; ++i)
      if(!isdigit(d.sym[i])) return false;
    return true;
  }

  void parse_reader_lambda_shorthand_args(data_vector& exp, size_type& total_args, bool& has_variadic)noexcept{
    for(auto& datum : exp) {
      if(datum_is_reader_lambda_arg(datum)) {
        if(datum.sym[1] == '%')
          has_variadic = true;
        else if(size_type arg_No = size_type(std::stold(datum.sym.substr(1))); total_args < arg_No)
          total_args = arg_No;
      } else if(datum.is_type(types::exp)) {
        parse_reader_lambda_shorthand_args(datum.exp,total_args,has_variadic);
      }
    }
  }

  void expand_reader_lambda_shorthand(data_vector& ast)noexcept{
    bool has_variadic = false;
    size_type total_args = 0;
    parse_reader_lambda_shorthand_args(ast,total_args,has_variadic);
    ast[0] = symconst::lambda;
    ast.insert(ast.begin()+1,data_vector());
    for(size_type i = 0; i < total_args; ++i) // populate args
      ast[1].exp.push_back('%' + std::to_string(i+1));
    if(has_variadic) { // add variadic arg (if present)
      ast[1].exp.push_back(symconst::dot);
      ast[1].exp.push_back("%%");
    }
  }

  bool is_reader_lambda_shorthand(const data_vector& ast)noexcept{
    return !ast.empty() && ast[0].is_type(types::sym) && ast[0].sym == symconst::reader_lambda;
  }

  // \ -> lambda
  void expand_reader_lambda_shorthands(data_vector& ast)noexcept{
    if(is_reader_lambda_shorthand(ast))
      expand_reader_lambda_shorthand(ast);
    for(auto& datum : ast)
      if(datum.is_type(types::exp))
        expand_reader_lambda_shorthands(datum.exp);
  }

  /******************************************************************************
  * READER INFIX->PREFIX CONVERSION
  ******************************************************************************/

  bool data_is_inf_precedence_tag(const data& d)noexcept{
    return d.is_type(types::sym) && d.sym == symconst::inf_precedence;
  }

  void convert_left_assoc_infix_expr(data_vector& ast, size_type& i)noexcept{
    if(i > 1 && data_is_inf_precedence_tag(ast[i-2])) {
      data tmp = ast[i-1];
      ast[i-1] = ast[i];
      ast[i] = tmp;
      ast.erase(ast.begin()+i-2); // erase inf-precedence tag
    } else {
      data_vector prefix_expr(3);
      prefix_expr[0] = ast[i], prefix_expr[1] = ast[i-1], prefix_expr[2] = ast[i+1];
      ast[i-1] = prefix_expr;
      ast.erase(ast.begin()+i,ast.begin()+i+2);
    }
    i -= 2;
  }

  bool is_infixr_op_in_level(const string& sym, const infix_level_t& level)noexcept{
    for(const auto& op : level) if(!op.first && op.second == sym) return true;
    return false;
  }

  size_type get_last_infixr_op_idx(const data_vector& ast, size_type i, const infix_level_t& level)noexcept{
    for(size_type n = ast.size(); i+1 < n && ast[i].is_type(types::sym) && is_infixr_op_in_level(ast[i].sym,level); i += 2);
    return i - 2;
  }

  void convert_right_assoc_infix_expr(data_vector& ast, size_type& i, const infix_level_t& level)noexcept{
    size_type last_idx = get_last_infixr_op_idx(ast,i,level);
    while(last_idx != i)
      convert_left_assoc_infix_expr(ast,last_idx);
    convert_left_assoc_infix_expr(ast,i);
  }

  void convert_expr_if_infix(data_vector& ast, size_type& i, const infix_level_t& level)noexcept{
    for(const auto& op : level)
      if(ast[i].sym == op.second) {
        if(op.first) convert_left_assoc_infix_expr(ast,i);
        else         convert_right_assoc_infix_expr(ast,i,level);
        return;
      }
  }

  void convert_infix_level_ops_to_prefix_notation(data_vector& ast, const infix_level_t& level)noexcept{
    for(size_type i = 0; i < ast.size(); ++i) {
      if(ast[i].is_type(types::exp)) {
        convert_infix_level_ops_to_prefix_notation(ast[i].exp,level);
      } else if(ast[i].is_type(types::sym)) {
        if(data_is_inf_precedence_tag(ast[i])) { // ignore operators after expanded {} tag
          ++i;
          continue;
        } else if(i > 0 && i+1 < ast.size()) { // ignore prefix/postfix symbols
          convert_expr_if_infix(ast,i,level);
        }
      } 
    }
  }

  void strip_INFIX_ESC_prefix_and_INF_PRECEDENCE_tag(data_vector& ast)noexcept{
    for(size_type i = 0; i < ast.size(); ++i) {
      if(ast[i].is_type(types::exp)) {
        strip_INFIX_ESC_prefix_and_INF_PRECEDENCE_tag(ast[i].exp);
      } else if(ast[i].is_type(types::sym)) { 
        if(ast[i].sym.size() > 2 && ast[i].sym[0] == '#' && ast[i].sym[1] == '!') // infix escape prefix
          ast[i].sym = ast[i].sym.substr(2);
        else if(ast[i].sym == symconst::inf_precedence) // infinite precedence via {}
          ast.erase(ast.begin()+(i--));
      }
    }
  }

  void convert_infix_to_prefix(data_vector& ast)noexcept{
    // expand in descending order (higher precedence first)
    for(auto iter = G.INFIX_TABLE.rbegin(), end = G.INFIX_TABLE.rend(); iter != end; ++iter)
      convert_infix_level_ops_to_prefix_notation(ast,iter->second);
    strip_INFIX_ESC_prefix_and_INF_PRECEDENCE_tag(ast); // #!<symbol> => <symbol>
  }

  /******************************************************************************
  * READER NUMBER PARSING FUNCTIONS
  ******************************************************************************/

  bool parse_radix_exactness_prefix(const string& input, size_type& i, int& base, char& prec)noexcept{
    if(input[i] == '#' && input.size()-i > 2) {
      if(input[i+1] == 'e' || input[i+1] == 'i') {
        if(prec) return false;
        prec = input[i+1];
        i += 2;
        return true;
      }
      if(input[i+1] == 'b' || input[i+1] == 'o' || input[i+1] == 'd' || input[i+1] == 'x') {
        if(base) return false;
        base = input[i+1] == 'b' ? 2 : input[i+1] == 'o' ? 8 : input[i+1] == 'd' ? 10 : 16;
        i += 2;
        return true;
      }
      if(!isdigit(input[i+1])) return false;
      if(input.size()-i > 3 && (input[i+2] == 'r' || input[i+2] == 'R')) {
        if(base) return false;
        base = input[i+1]-'0';
        if(base < 2 || base > 9) return false;
        i += 3;
        return true;
      }
      if(input.size()-i > 4 && isdigit(input[i+2]) && (input[i+3] == 'r' || input[i+3] == 'R')) {
        if(base) return false;
        base = 10 * (input[i+1]-'0') + (input[i+2]-'0');
        if(base < 2 || base > 36) return false;
        i += 4;
        return true;
      }
      return false;
    }
    return true;
  }

  // Returns whether found valid (including no) prefix.
  bool parse_radix_exactness_prefixes(const string& input, size_type& i, int& base, char& prec)noexcept{
    if(!parse_radix_exactness_prefix(input,i,base,prec)) return false;
    if(!parse_radix_exactness_prefix(input,i,base,prec)) return false;
    if(!base) base = 10; // base = 10 by default
    return true;
  }

  // Returns whether succeeded, handles the radix & exactness prefixes
  // => NOTE: <num_type> only handles numeric literals w/o these prefixes
  bool convert_string_to_scm_number(const string& input, num_type& num)noexcept{
    // Given NaN (special case)
    if(input == "+nan.0" || input == "-nan.0") {
      num = num_type("+nan.0");
      return true;
    }
    size_type i = 0;
    int base    = 0;
    char prec   = 0; // prec = 0 denotes use default exactness
    if(!parse_radix_exactness_prefixes(input,i,base,prec)) return false;
    // evaluate the parsed non-NaN token
    num = (base!=10) ? num_type(input.substr(i),base) : num_type(input.substr(i)); // base
    if(prec) num = (prec=='e') ? num.to_exact() : num.to_inexact(); // prec
    return !num.is_nan(); // already accounted for parsing a NaN literal
  }

  // Check whether data is a number. 
  //   => NOTE: 2.0.0 is a valid Scheme symbol
  //            -> Hence must confirm is numeric & NOT symbol prior extraction
  bool data_is_number(size_type& i, const string& input, num_type& exp) noexcept {
    string num;
    const size_type n = input.size();
    size_type j = i;
    while(j < n && !IS_END_OF_WORD(input[j],input[j+1])) num += input[j++];
    if(convert_string_to_scm_number(num,exp)) {
      i = j-1;
      return true;
    }
    return false;
  }

  /******************************************************************************
  * READER AST CONSTRUCTION HELPER FUNCTIONS
  ******************************************************************************/

  // Check whether data is a string literal
  bool data_is_string(size_type& i, const string& input, string& exp) noexcept {
    if(is_non_escaped_double_quote(i,input)) {
      ++i;
      const size_type n = input.size();
      while(i < n && !is_non_escaped_double_quote(i,input)) exp += input[i], ++i;
      return true;
    }
    return false; 
  }

  // Check whether data is a char
  bool data_is_char(size_type& i, const string& input, chr_type& exp) noexcept {
    if(input[i] == '#' && input[i+1] == '\\') {
      i += 2;
      if(auto [ch, name] = data_is_named_char(i,input); !name.empty()) 
        exp = ch, i += name.size()-(name.size()>2);
      else exp = input[i];
      return true;
    }
    return false;
  }

  // Check whether data is a symbol. Assumes confirmed NOT to be in a string & number already.
  bool data_is_symbol(size_type& i, const string& input, string& exp) noexcept {
    const size_type n = input.size();
    if(i >= n || isspace(input[i]) || IS_OPEN_PAREN(input[i]) || IS_CLOSE_PAREN(input[i])) 
      return false;
    while(i < n && (!isspace(input[i]) && !IS_OPEN_PAREN(input[i]) && !IS_CLOSE_PAREN(input[i]))) 
      exp += input[i++];
    --i;
    return true;
  }

  /******************************************************************************
  * READER MAIN FUNCTIONS -- ABSTRACT SYNTAX TREE CONSTRUCTION
  ******************************************************************************/

  // Parses the scheme expression buffer into a data_vector
  void construct_abstract_syntax_tree(size_type& i, const string& input, data_vector& tree) {
    const size_type n = input.size();
    string tmp_str = "", tmp_sym = "";
    chr_type tmp_chr = 0;
    num_type tmp_num = 0;
    for(; i < n; ++i) {
      if(isspace(input[i])) continue;
      tmp_str = tmp_sym = "";
      tmp_num = tmp_chr = 0;
      if(data_is_string(i,input,tmp_str))      // found string atom
        tree.push_back(make_str(unescape_chars(tmp_str)));
      else if(data_is_char(i,input,tmp_chr))   // found char atom
        tree.push_back(data(tmp_chr));
      else if(data_is_number(i,input,tmp_num)) // found number atom
        tree.push_back(data(tmp_num));
      else if(IS_OPEN_PAREN(input[i])) {       // found new list, recursively parse contents
        data_vector new_list;
        construct_abstract_syntax_tree(++i,input,new_list);
        tree.push_back(data(new_list));
      } else if(IS_CLOSE_PAREN(input[i]))      // found end of this list, return
        return;
      else if(data_is_symbol(i,input,tmp_sym)) // found symbol atom
        tree.push_back(data(tmp_sym));
      else throw i;                            // unknown type detected, throw index
    }
  }

  // Mutates <input> prior AST parsing: rming comments, expanding quotes, etc.
  //   => NOTE: returns whether <input> is a candidate for AST parsing
  bool prepare_string_for_AST_generation(string& input) {
    if(input.empty() || !confirm_valid_scm_expression(input)) return false;
    strip_comments_and_redundant_whitespace(input);
    if(!GLOBALS::USING_CASE_SENSITIVE_SYMBOLS) render_input_cAsE_iNsEnSiTiVe(input);
    expand_inf_precedence_scoping(input);   //  "{" => "(heist:core:inf-precedence "
    expand_vector_and_hmap_literals(input); // #(<exp>) => (vector-literal <exp>), $(<exp>) => (hmap-literal <exp>)
    expand_reader_macro_shorthands(input);  // '<exp>   => (quote <exp>)
    return true;
  }

  // Parses <input> into an Abstract Syntax Tree, populated in <abstract_syntax_tree>
  // => GIVEN THE RAW USER INPUT
  void parse_input_exp(string&& input, data_vector& abstract_syntax_tree) {
    if(!prepare_string_for_AST_generation(input)) return;
    size_type start_index = 0;
    construct_abstract_syntax_tree(start_index,input,abstract_syntax_tree);
    expand_reader_aliases(abstract_syntax_tree);
    expand_reader_lambda_shorthands(abstract_syntax_tree);
    convert_infix_to_prefix(abstract_syntax_tree);
  }
} // End of namespace heist
#endif