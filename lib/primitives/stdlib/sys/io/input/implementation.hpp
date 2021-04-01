// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for input.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_INPUT_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_INPUT_IMPLEMENTATION_HPP_

namespace heist::stdlib_input {

  /******************************************************************************
  * VALIDATION
  ******************************************************************************/

  // Confirm given valid input args, & return whether at the input file's EOF
  bool confirm_valid_input_args_and_non_EOF(const data_vector& args, FILE*& ins, const char* name, bool& reading_stdin, bool& reading_string){
    if(!args.empty()) {
      if(args.size() == 1) {
        if((reading_string = args[0].is_type(types::str))) {
          reading_stdin = false;
          return !args[0].str->empty();
        }
        if(args[0].is_type(types::fip) && args[0].fip.is_open()) {
          ins = *args[0].fip.fp;
          // NOT reading from stdin requires a specialized parsing algorithm
          if(ins != stdin) {
            if(feof(ins)) return false;
            reading_stdin = false; 
          }
        } else {
          HEIST_THROW_ERR('\''<< name <<" arg " << HEIST_PROFILE(args[0]) <<
            " isn't an open input port:"
            "\n     ("<< name <<" <optional-open-input-port-or-string>)" <<
            HEIST_FCN_ERR(name,args));
        }
      } else {
        HEIST_THROW_ERR('\''<< name <<" received incorrect # of args:" 
          "\n     ("<< name <<" <optional-open-input-port-or-string>)" <<
          HEIST_FCN_ERR(name,args));
      }
    }
    return true;
  }

  /******************************************************************************
  * READ GENERAL
  ******************************************************************************/

  data read_from_input_port_logic(FILE*& outs, FILE*& ins, const bool& reading_stdin){
    // Read input
    data read_data = data_vector(2);
    read_data.exp[0] = symconst::quote;
    if(reading_stdin) {
      if(auto read_result = read_user_input(outs,ins,false); read_result.empty()) {
        read_data.exp[1] = GLOBALS::VOID_DATA_OBJECT;
      } else {
        read_data.exp[1] = std::move(read_result[0]);
      }
    } else {
      data_vector read_from_port(FILE* outs, FILE* ins); // definition below
      read_data.exp[1] = read_from_port(outs,ins)[0];
    }
    return scm_eval(std::move(read_data), G.GLOBAL_ENVIRONMENT_POINTER);
  }


  data read_from_string_logic(string& outs_str){
    try {
      data_vector read_data;
      // attempt to parse an AST expression from the given string
      parse_input_exp(string(outs_str),read_data);
      if(read_data.empty()) return symconst::emptylist;
      // remove the parsed portion from the original string
      prepare_string_for_AST_generation(outs_str);
      size_type i = 0; // also trim prepending whitespace
      for(size_type n = outs_str.size(); i < n && isspace(outs_str[i]); ++i);
      outs_str.erase(0,i);
      outs_str.erase(0,read_data[0].write().size());
      // return the parsed AST
      data quoted_read_data = data_vector(2);
      quoted_read_data.exp[0] = symconst::quote;
      quoted_read_data.exp[1] = std::move(read_data[0]);
      return scm_eval(std::move(quoted_read_data),G.GLOBAL_ENVIRONMENT_POINTER);
    // throw error otherwise & return void data
    } catch(const READER_ERROR& read_error) {
      if(is_non_repl_reader_error(read_error))
        alert_non_repl_reader_error(stdout,read_error,outs_str);
      else
        alert_reader_error(stdout,read_error,outs_str);
      fflush(stdout);
      throw SCM_EXCEPT::READ;
    } catch(const size_type& read_error_index) {
      alert_reader_error(stdout,read_error_index,outs_str);
      fflush(stdout);
      throw SCM_EXCEPT::READ;
    }
  }

  /******************************************************************************
  * READ PORT
  ******************************************************************************/

  namespace read_port {
    bool input_ends_with_postfix(const string& postfix, const string& input)noexcept{
      return input.size() >= postfix.size() && input.compare(input.size()-postfix.size(),postfix.size(),postfix) == 0;
    }


    bool input_ends_with_char_prefix(const string& input, const size_type end_offset = 0)noexcept{
      return input.size() >= 2+end_offset && *(input.rbegin()+1+end_offset) == '#' && *(input.rbegin()+end_offset) == '\\';
    }


    size_type input_ends_with_reader_macro_or_quote(const string& input)noexcept{
      if(!input_ends_with_char_prefix(input)) // prevents matching lambda "\" against char "#\"
        for(const auto& reader_macro_shorthand : G.SHORTHAND_READER_MACRO_REGISTRY)
          if(input_ends_with_postfix(reader_macro_shorthand,input))
            return reader_macro_shorthand.size();
      return 0;
    }


    // Determine the read char's length (for reading from a port)
    // PRECONDITION: input[i] = the 1st char after the '#\' of the char literal
    size_type character_length(const size_type& i, const string& input)noexcept{
      if(auto [ch, name] = data_is_named_char(i,input); !name.empty())
        return name.size() + (input[i] == 'x' && isxdigit(input[i+1]));
      return 1;
    }


    // Parse a char literal from "ins", rm excess letters from "input", & 
    //   mv "ins" back to be 1 char past the end of the char literal
    void parse_char(FILE*& ins, string& input)noexcept{
      const size_type char_start = input.find("#\\")+2;
      if(char_start+2 > input.size()) return; // if parsing a single/null character
      const size_type char_length = character_length(char_start,input);
      if(input.size()-(char_start+char_length) > 0) {
        const size_type excess_read_length = input.size()-(char_start+char_length);
        fseek(ins, -excess_read_length, SEEK_CUR);    // mv "ins" back
        input.erase(input.size()-excess_read_length); // erase the excess parsed
      }
    }


    // Confirm improper use for any quotation/reader-macro shorthand
    bool improper_quotation(string& input)noexcept{
      // Not enough room -> false
      if(input.size() <= 1) return false;
      auto last_ch = *input.rbegin();
      // Not improper ending -> false
      if((!IS_CLOSE_PAREN(last_ch) && !isspace(last_ch)) || input_ends_with_char_prefix(input,2)) return false;
      input.pop_back(); // rm last char to check for quote/reader-macro
      bool ends_with_reader_macro_or_quote = input_ends_with_reader_macro_or_quote(input);
      input.push_back(last_ch); // add last char back in
      return ends_with_reader_macro_or_quote;
    }


    // Confirm quotation/reader-macro shorthand found at the end of the file
    bool improper_EOF_quotation(const string& input)noexcept{
      return !input.empty() && input_ends_with_reader_macro_or_quote(input) && !input_ends_with_char_prefix(input,1);
    }


    // Confirm just appended a valid open/close paren to 'input'
    bool is_valid_open_exp(const string &input, const size_type& paren_count,
                           const bool& possible_vect,const bool& possible_hmap,const bool& found_non_reader_syntax_data)noexcept{
      return IS_OPEN_PAREN(*input.rbegin()) && !input_ends_with_char_prefix(input,1)
                                            && (paren_count || !found_non_reader_syntax_data || possible_vect || possible_hmap);
    }


    bool is_valid_close_exp(const string &input)noexcept{
      return IS_CLOSE_PAREN(*input.rbegin()) && !input_ends_with_char_prefix(input,1);
    }


    // Confirm input is _not_ made up only of nested reader macros
    bool found_non_macro_prior_macro(const string& input) {
      const auto n = input.size();
      size_type i = n, nested_length = 0;
      // Skip past to last expression
      while(i-- > 1) if(IS_END_OF_WORD(input[i-1],input[i])) break;
      if(i) return false; // #f if symbol didn't start at front of input
      // Confirm series of symbols = nested reader macros
      while(i < n) {
        if((nested_length = is_expandable_reader_macro(input,i))) {
          i += nested_length;
        } else {
          return false;
        }
      }
      return true;
    }
  } // -- End namespace read_port



  string read_expr_from_port(FILE* outs, FILE* ins, size_type& total_read_chars) {
    // input parsing variables & status trackers
    string input;
    int ch;
    bool in_a_string   = false, possible_vect=false,  possible_hmap=false, found_non_reader_syntax_data=false;
    bool possible_char = false, confirmed_char=false, parsing_a_char=false;
    bool possible_multi_line_comment_end=false, in_a_single_line_comment=false;
    bool possible_multi_line_comment=false,     in_a_multi_line_comment=false;
    size_type paren_count = 0, macro_length = 0;
    fflush(outs);
    // parse the input port's scheme code
    while((ch = fgetc(ins)) != EOF) {
      // don't include text of a multi-line comment
      if(possible_multi_line_comment) {
        possible_multi_line_comment = false;
        if(ch == '|') {
          ++total_read_chars;
          possible_vect = possible_char = false; // confirmed neither char nor vector (@ a comment)
          in_a_multi_line_comment = true;
          input.erase(input.end()-1);
          found_non_reader_syntax_data = !input.empty(); // if found non-quote data
          continue;
        }
      }
      if(possible_multi_line_comment_end) {
        possible_multi_line_comment_end = false;
        if(ch == '#') {
          ++total_read_chars;
          in_a_multi_line_comment = false;
          continue;
        }
      }
      if(in_a_multi_line_comment) {
        ++total_read_chars;
        possible_multi_line_comment_end = (ch == '|');
        continue;
      }

      // don't include text of a single-line comment
      if(in_a_single_line_comment) {
        ++total_read_chars;
        in_a_single_line_comment = (ch != '\n');
        continue;
      }
      if(!in_a_string && !confirmed_char && ch == ';') {
        ++total_read_chars;
        in_a_single_line_comment = true;
        continue;
      }

      // check for whether at a potential multi-line comment
      possible_multi_line_comment = (!in_a_string && ch == '#');

      // don't include prefixing whitespace
      if(input.empty() && isspace(ch)) {
        ++total_read_chars;
        continue;
      }

      // append the character
      input += ch;
      ++total_read_chars;

      // continue to get the quoted data
      if(!in_a_string && !read_port::input_ends_with_char_prefix(input,1) && 
         (macro_length = read_port::input_ends_with_reader_macro_or_quote(input))) {
        // if macro reader symbol is in middle of another symbol (ie e'e)
        //   return back to the previous non-macro symbol & stop parsing
        //   IFF not in the middle of an expression (since only reading 1 
        //   expression at a time)
        if(!paren_count && input.size() != macro_length && 
          (found_non_reader_syntax_data || read_port::found_non_macro_prior_macro(input))) {
          fseek(ins, -macro_length, SEEK_CUR); // mv "ins" back
          input.erase(input.size()-macro_length);
          total_read_chars -= macro_length;
          break;
        }
        found_non_reader_syntax_data = false; // flags to keep seeking after current macro
        continue;
      }

      // skip first char of a char literal
      if(confirmed_char) { confirmed_char=false, parsing_a_char=true; continue; }
      // if at the end of a possible char
      if(parsing_a_char && IS_END_OF_WORD(*(input.end()-2), ch)) {
        parsing_a_char = false;
        if(!paren_count) { read_port::parse_char(ins,input); break; }
      } else if(parsing_a_char) continue; // keep parsing the char until complete

      // skip if parsing a string literal
      if(in_a_string && ch != '"') continue;
      // check whether at a char
      if(possible_char && ch != '\\') possible_char = false;
      // check whether at a vector
      if(possible_vect && !IS_OPEN_PAREN(ch)) possible_vect = false;
      // check whether at an hmap
      if(possible_hmap && !IS_OPEN_PAREN(ch)) possible_hmap = false;

      // check if at a string
      if(ch == '"' && !read_port::input_ends_with_char_prefix(input,1) && 
         (in_a_string || paren_count || !found_non_reader_syntax_data)) {
        if(!in_a_string) {
          in_a_string = true;
        } else if(is_non_escaped_double_quote(input.size()-1,input)) {
          in_a_string = false;
          if(!paren_count) break;
        }
      }

      // check if at an expression or vector => PARSES BOTH!
      else if(read_port::is_valid_open_exp(input,paren_count,possible_vect,possible_hmap,
                                                       found_non_reader_syntax_data))
        possible_vect = false, ++paren_count;
      // check if at a closing expression
      else if(read_port::is_valid_close_exp(input)) {
        if(!paren_count) {
          alert_reader_error(outs,READER_ERROR::early_end_paren,input);
          throw SCM_EXCEPT::READ;
        }
        --paren_count;
        if(!paren_count) break;
      }

      // check for improper quotation shorthand use
      else if(read_port::improper_quotation(input)){
        if(IS_CLOSE_PAREN(ch))
             alert_reader_error(outs,READER_ERROR::quoted_end_of_expression,input);
        else alert_reader_error(outs,READER_ERROR::quoted_space,input);
        throw SCM_EXCEPT::READ;
      }

      // continue parsing if in an expression
      else if(paren_count) continue;

      // check if at an hmap
      else if(ch == '$' && !found_non_reader_syntax_data)
        possible_hmap = true;

      // check if at a char, vector
      else if(ch == '#' && !found_non_reader_syntax_data) 
        possible_vect = possible_char = true;
      else if(possible_char && ch == '\\') confirmed_char=true, possible_char=false;

      // check for the end of the current atomic. if so, mv "ins" back to the end
      else if(found_non_reader_syntax_data && !paren_count && input.size() > 2 && 
              IS_END_OF_WORD(*(input.end()-2), ch)) {
        fseek(ins, -2, SEEK_CUR);    // mv "ins" back
        input.erase(input.size()-2); // erase the excess from "input"
        total_read_chars -= 2;
        break;
      }

      found_non_reader_syntax_data = !input.empty(); // if found non-quote data
    } // -- End of parsing loop

    if(!input.empty()) {
      // Confirm file didn't end mid-string or mid-expression
      if(in_a_string || paren_count) {
        if(in_a_string) {
          alert_non_repl_reader_error(outs,READER_ERROR::incomplete_string,input);
        } else { 
          alert_non_repl_reader_error(outs,READER_ERROR::incomplete_expression,input);
        }
        throw SCM_EXCEPT::READ;
      }

      // Parse the char literal if appeared at the end of the buffer
      //   NOTE: possible_char & confirmed_char are ok 2B true w/o further logic
      if(parsing_a_char) {
        read_port::parse_char(ins,input);

      // Confirm didn't quote the end of the buffer
      } else if(read_port::improper_EOF_quotation(input)) {
        alert_reader_error(outs, READER_ERROR::quoted_end_of_buffer,input);
        throw SCM_EXCEPT::READ;
      }
    }
    return input;
  }


  void trim_edge_whitespace(string& sym)noexcept{
    size_type i = 0;
    for(size_type n = sym.size(); i < n && isspace(sym[i]); ++i);
    sym.erase(sym.begin(),sym.begin()+i);
    for(i = sym.size(); i-- != 0;) if(!isspace(sym[i])) break;
      sym.erase(sym.begin()+i+1,sym.end());
  }


  bool is_infix_operator(string sym)noexcept{
    trim_edge_whitespace(sym);
    // Check if symbol is a reader alias for another symbol
    //   (then check if the expansion is an infix operator if so)
    if(size_type idx = 0; symbol_is_reader_alias(sym,idx)) // from lib/core/reader/parser.hpp
      sym = G.LONGHAND_READER_ALIAS_REGISTRY[idx];
    // Check if symbol is an infix operator
    for(const auto& prec_level : G.INFIX_TABLE)
      for(const auto& op : prec_level.second)
        if(op.second == sym) return true;
    return false;
  }


  // Accounts for needing to read in exprs w/ infix operators as well
  string read_1_expr_from_port(FILE* outs, FILE* ins) {
    // Read an expression from the port
    string input, infix_operator;
    size_type total_read_chars = 0, total_operator_chars = 0;
    auto buffer = read_expr_from_port(outs,ins,total_read_chars);
    if(G.INFIX_TABLE.empty() || buffer.empty()) return buffer;

  operator_read_start:
    total_operator_chars = total_read_chars = 0;
    infix_operator = read_expr_from_port(outs,ins,total_operator_chars);
    if(is_infix_operator(infix_operator)) {
      input = read_expr_from_port(outs,ins,total_read_chars);
      if(!input.empty()) {
        buffer += ' ' + infix_operator + ' ' + input;
        goto operator_read_start;
      } else {
        fseek(ins, -total_read_chars-total_operator_chars, SEEK_CUR);
        return buffer;
      }
    } else {
      fseek(ins, -total_operator_chars, SEEK_CUR);
      return buffer;
    }
    return buffer;
  }


  // Read from a non-stdin port
  // PRECONDITION: feof(ins) MUST RETURN false
  data_vector read_from_port(FILE* outs, FILE* ins) {
    auto input = read_1_expr_from_port(outs,ins);
    // If read an empty file
    if(input.empty()) {
      data_vector eof_char(1,chr_type(EOF));
      return eof_char;
    }
    // Try parsing the given input expression, & throw an error as needed
    try {
      data_vector abstract_syntax_tree;
      // Return AST if successfully parsed an expression
      parse_input_exp(std::move(input),abstract_syntax_tree);
      return abstract_syntax_tree;
    } catch(const READER_ERROR& read_error) {
      if(is_non_repl_reader_error(read_error))
           alert_non_repl_reader_error(outs,read_error,input);
      else alert_reader_error(outs,read_error,input);
      throw SCM_EXCEPT::READ;
    } catch(const size_type& read_error_index) {
      alert_reader_error(outs,read_error_index,input);
      throw SCM_EXCEPT::READ;
    }
  }

  /******************************************************************************
  * SLURP FILE
  ******************************************************************************/

  // Confirm given a single string argument
  void confirm_given_one_string_arg(const data_vector& args, const char* name, const char* format){
    if(args.size() != 1) 
      HEIST_THROW_ERR('\''<<name<<" didn't receive any args: "<<format<<HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::str)) 
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't a filename string: "<<format
        << HEIST_FCN_ERR(name,args));
  }


  // Returns a file pointer if 'filename' is the string name of an existing file
  FILE* confirm_valid_input_file(const data& filename, const char* name, 
                                 const char* format,   const data_vector& args) {
    // open the file
    FILE* fp = fopen(filename.str->c_str(), "r");
    if(fp == nullptr)
      HEIST_THROW_ERR('\'' << name << " file \"" << *filename.str
        << "\" doesn't exist (invalid for input):"<<format<<HEIST_FCN_ERR(name,args));
    return fp;
  }

} // End of namespace heist::stdlib_input

#endif