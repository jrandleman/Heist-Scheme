// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for output.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_OUTPUT_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_OUTPUT_IMPLEMENTATION_HPP_

namespace heist::stdlib_output {

  /******************************************************************************
  * VALIDATION
  ******************************************************************************/

  // Confirm given valid output args
  //  => NOTE: 'total_non_port_args' also doubles as the index of 
  //           where the port/string would be if it were given as an arg
  //  => NOTE: returns whether writing to a port (instead of a string)
  bool confirm_valid_output_args(const data_vector& args, FILE*& outs, 
                                 const size_type& total_non_port_args,
                                 const char* name, const char* format){
    // Confirm given enough non-port args if given no port/string
    if(args.size()==total_non_port_args) return true;
    // Confirm given an open output port if received the optional port/string arg
    if(args.size()==total_non_port_args+1) {
      if(args[total_non_port_args].is_type(types::fop) && 
         args[total_non_port_args].fop.is_open()) {
        outs = *args[total_non_port_args].fop.fp; 
        return true;
      } else if(args[total_non_port_args].is_type(types::str)) {
        return false;
      } else {
        HEIST_THROW_ERR('\''<< name <<" arg "<< HEIST_PROFILE(args[total_non_port_args])
          << " isn't an open output port: "<< format << HEIST_FCN_ERR(name,args));
      }
    }
    HEIST_THROW_ERR('\''<<name<<" received incorrect # of args: "<<format<<HEIST_FCN_ERR(name,args));
    return true;
  }

  /******************************************************************************
  * DISPLAY
  ******************************************************************************/

  data display_port_logic(const data& obj, FILE*& outs) {
    if(obj.is_type(types::chr)) {
      fputc(obj.chr, outs);
      if(obj.chr == '\n') // account for printing a newline
        G.LAST_PRINTED_NEWLINE_TO_STDOUT = (outs == stdout);
    } else if(obj.is_type(types::str)) {
      if(!obj.str->empty()) {
        fputs(obj.str->c_str(), outs);
        if(*obj.str->rbegin() == '\n') // account for printed newlines
          G.LAST_PRINTED_NEWLINE_TO_STDOUT = (outs == stdout);  
      }
    } else if(!obj.is_type(types::dne)) {
      fputs(obj.display().c_str(), outs);
    }
    fflush(outs);
    G.LAST_PRINTED_TO_STDOUT = (outs == stdout);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * FORMATTED OUTPUT HELPERS
  ******************************************************************************/

  // SPRINTF TOKEN TYPE
  struct sprintf_token_t { 
    enum class token_t {a, wa, pa, ellipsis, wellipsis, pellipsis, dollar, s, ws, c, wc, b, wb, n};
    token_t token = token_t::a;
    // invariants for more string detail
    int padding = 0; // negative = right, positive = left [ UP TO 3 DIGITS ]
    // invariants for more number detail
    enum class exactness_t {exact, inexact, dflt};
    exactness_t exactness = exactness_t::dflt;
    int precision  = -1;    // -1 denotes 'default'
    int base       = 10;    // base : [2,36]
    int padded_0s  = 0;     // total 0's to pad left with
    bool show_sign = false; // show sign even if positive
    bool upcase    = true;  // capitalize base 11+ strings
    bool commas    = false; // print using commas iff bigint
    // ctor
    sprintf_token_t(token_t t = token_t::a) noexcept : token(t){}
  };


  using sprintf_tokens_t = std::vector<sprintf_token_t>;


  // DIGIT PARSING HELPERS
  // => PRECONDITION: isdigit(c1)
  int extract_up_to_2_digits(char c1, char c2, size_type& i)noexcept{
    int val = 0;
    if(isdigit(c2)) {
      val = (10 * (c1-'0')) + (c2-'0');
      i += 2;
    } else {
      val = c1-'0';
      ++i;
    }
    return val;
  }


  int parse_string_padding(const string& input, size_type i)noexcept{
    if(input[i+1] == 's') return 0;
    int padding_direction = 1 - (2 * (input[i+1] == '-'));
    if(input[i+1] == '-') ++i;
    if(isdigit(input[i+2])) {
      if(isdigit(input[i+3])) {
        return padding_direction * ((100 * (input[i+1] - '0')) + (10 * (input[i+2] - '0')) + (input[i+3] - '0'));
      } else {
        return padding_direction * ((10 * (input[i+1] - '0')) + (input[i+2] - '0'));
      }
    } else {
      return padding_direction * (input[i+1] - '0');
    }
  }


  bool is_string_token(const string& input, size_type i)noexcept{
    if(input[i+1] == 's')    return true;
    if(input[i+1] == '-') ++i; // mv past possible '-' for padding
    if(!isdigit(input[i+1])) return false;
    if(input[i+2] == 's')    return true;
    if(!isdigit(input[i+2])) return false;
    if(input[i+3] == 's')    return true;
    if(!isdigit(input[i+3])) return false;
    return input[i+4] == 's';
  }


  bool is_dollar_token(const string& input, size_type i)noexcept{
    return input[i+1] == '$' || (input[i+1] == ',' && input[i+2] == '$');
  }


  bool is_ellipsis_token(const string& input, size_type i)noexcept{
    return i+2 < input.size() && input[i] == '.' && input[i+1] == '.' && input[i+2] == '.';
  }


  bool is_sprintf_exact(char c)noexcept{
    return c == 'e' || c == 'E';
  }


  bool is_sprintf_inexact(char c)noexcept{
    return c == 'i' || c == 'I';
  }


  bool is_sprintf_radix(char c)noexcept{
    return c == 'r' || c == 'R';
  }
  

  bool is_potential_detailed_number(char c)noexcept{
    return c == '+' || c == ',' || is_sprintf_exact(c) || is_sprintf_inexact(c) || c == '.' || isdigit(c);
  }
  

  bool is_invalid_detailed_number(char c, size_type i, size_type n)noexcept{
    return ((is_sprintf_exact(c) || is_sprintf_inexact(c) || isdigit(c)) && i+1 == n) || (c == '.' && i+2 == n);
  }
  

  bool number_is_positive_non_special_const(const num_type& n)noexcept{
    return n.is_pos() && !n.is_pos_inf() && !n.is_neg_inf() && !n.is_nan();
  }
  

  bool should_show_sign(const num_type& n)noexcept{
    return  (n.is_real() && number_is_positive_non_special_const(n)) || 
            (n.is_complex() && number_is_positive_non_special_const(n.real_part()));
  }


  bool data_is_a_valid_dollar_value(const data& d)noexcept{
    return d.is_type(types::num) && d.num.is_real() && !d.num.is_pos_inf() && !d.num.is_neg_inf() && !d.num.is_nan();
  }


  bool should_insert_commas(const sprintf_token_t& tok, const num_type& n)noexcept{
    return tok.commas && tok.exactness != sprintf_token_t::exactness_t::inexact && 
           tok.precision == -1 && n.is_exact() && n.is_integer();
  }


  void insert_num_commas(string& num_str)noexcept{
    string comma_str;
    for(size_type i = num_str.size(), count = 0; i-- > 0; count = (count + 1) % 3) {
      comma_str += num_str[i];
      if(count == 2 && i) comma_str += ',';
    }
    num_str = string(comma_str.rbegin(),comma_str.rend());
    if(num_str[0] == '-' && num_str[1] == ',') num_str.erase(1,1);
  }


  string generate_dollar_value_string(const num_type& n, const bool adding_commas)noexcept{
    auto str = ((n * 100.0L).round() / 100.0L).str();
    if(str.size() == 1) {
      str = "0.00";
    } else if(*(str.rbegin()+1) == '.') {
      str += '0'; // add an extra '0' to mk it 2 decimal places
    }
    if(!adding_commas) return str;
    const auto decimal_pos = str.find('.');
    auto integer_str = str.substr(0,decimal_pos);
    insert_num_commas(integer_str);
    return integer_str + str.substr(decimal_pos);
  }


  // ERROR HANDLING MACROS
  #define throw_invalid_sprintf_token(...)\
    HEIST_THROW_ERR('\''<<name<<" invalid token detected: \"" << __VA_ARGS__ << '"'\
      << format << HEIST_FCN_ERR(name,args));

  #define throw_invalid_sprintf_precision(...)\
    HEIST_THROW_ERR('\''<<name<<" precision: " << __VA_ARGS__\
      << "\n     -> Must range [1,99]!" << format << HEIST_FCN_ERR(name,args));


  // FORMATTING MAIN HELPERS
  void parse_sprintf_token_stream(string input, sprintf_tokens_t& tokens, str_vector& split_str, 
                                  const char* format, const char* name, data_vector& args){
    size_type i = 0;
    while(i < input.size()) {
      if(input[i] == '%' && i+1 < input.size()) {
        auto next_ch = input[i+1];
        // parse escaped '%'
        if(next_ch == '%') {
          input.erase(++i,1);
          continue;
        }
        // get split-string instance
        split_str.push_back(input.substr(0,i));
        // parse formatting
        if(next_ch == 'a') {
          tokens.push_back(sprintf_token_t::token_t::a), input = input.substr(i+2), i = 0;
        } else if(is_ellipsis_token(input,i+1)) {
          tokens.push_back(sprintf_token_t::token_t::ellipsis), input = input.substr(i+4), i = 0;
        } else if(is_dollar_token(input,i)) {
          sprintf_token_t str_token(sprintf_token_t::token_t::dollar);
          str_token.commas = input[i+1] == ',';
          tokens.push_back(str_token), input = input.substr(input.find('$',i)+1), i = 0;
        } else if(is_string_token(input,i)) {
          sprintf_token_t str_token(sprintf_token_t::token_t::s);
          str_token.padding = parse_string_padding(input,i);
          tokens.push_back(str_token), input = input.substr(input.find('s',i)+1), i = 0;
        } else if(next_ch == 'c') {
          tokens.push_back(sprintf_token_t::token_t::c), input = input.substr(i+2), i = 0;
        } else if(next_ch == 'b') {
          tokens.push_back(sprintf_token_t::token_t::b), input = input.substr(i+2), i = 0;
        } else if(next_ch == 'n' || next_ch == 'N') {
          tokens.push_back(sprintf_token_t::token_t::n), input = input.substr(i+2), i = 0;
        } else if(next_ch == 'p') {
          if(i+2 == input.size()) throw_invalid_sprintf_token("%p");
          if(input[i+2] == 'a') {
            tokens.push_back(sprintf_token_t::token_t::pa), input = input.substr(i+3), i = 0;
          } else if(is_ellipsis_token(input,i+2)) {
            tokens.push_back(sprintf_token_t::token_t::pellipsis), input = input.substr(i+5), i = 0;
          } else {
            char bad_token[4] = {'%','p',input[i+2],'\0'};
            throw_invalid_sprintf_token(bad_token);
          }
        } else if(next_ch == 'w') {
          if(i+2 == input.size()) throw_invalid_sprintf_token("%w");
          auto write_ch = input[i+2];
          if(write_ch == 'a') {
            tokens.push_back(sprintf_token_t::token_t::wa), input = input.substr(i+3), i = 0;
          } else if(is_ellipsis_token(input,i+2)) {
            tokens.push_back(sprintf_token_t::token_t::wellipsis), input = input.substr(i+5), i = 0;
          } else if(write_ch == 's') {
            tokens.push_back(sprintf_token_t::token_t::ws), input = input.substr(i+3), i = 0;
          } else if(write_ch == 'c') {
            tokens.push_back(sprintf_token_t::token_t::wc), input = input.substr(i+3), i = 0;
          } else if(write_ch == 'b') {
            tokens.push_back(sprintf_token_t::token_t::wb), input = input.substr(i+3), i = 0;
          } else {
            char bad_token[4] = {'%','w',write_ch,'\0'};
            throw_invalid_sprintf_token(bad_token);
          }
        // number formatting with exactness/base/precision info
        } else if(is_potential_detailed_number(next_ch)) {
          size_type token_start = i++;
          // confirm minimum # of chars available
          if(is_invalid_detailed_number(next_ch,i,input.size()))
            throw_invalid_sprintf_token(input.substr(token_start));
          // parse sign
          sprintf_token_t num_token(sprintf_token_t::token_t::n);
          if(input[i] == '+') num_token.show_sign = true, ++i;
          if(input[i] == 'n' || input[i] == 'N') {
            tokens.push_back(num_token), input = input.substr(i+1), i = 0;
            continue;
          }
          // parse comma use
          if(input[i] == ',') num_token.commas = true, ++i;
          if(input[i] == 'n' || input[i] == 'N') {
            tokens.push_back(num_token), input = input.substr(i+1), i = 0;
            continue;
          }
          // parse exactness
          if(is_sprintf_exact(input[i]))
            num_token.exactness = sprintf_token_t::exactness_t::exact, ++i;
          else if(is_sprintf_inexact(input[i]))
            num_token.exactness = sprintf_token_t::exactness_t::inexact, ++i;
          if(input[i] == 'n' || input[i] == 'N') {
            tokens.push_back(num_token), input = input.substr(i+1), i = 0;
            continue;
          }
          // parse base
          if(isdigit(input[i]) && (is_sprintf_radix(input[i+1]) || (isdigit(input[i+1]) && is_sprintf_radix(input[i+2])))) {
            int base = extract_up_to_2_digits(input[i],input[i+1],i);
            if(base < 2 || base > 36)
              HEIST_THROW_ERR('\''<<name<<" invalid number base: " << base
                << "\n     -> Must be in range of [2,36]!" << format 
                << HEIST_FCN_ERR(name,args));
            num_token.base = base;
            ++i; // mv past 'r'/'R'
            if(input[i] == 'n' || input[i] == 'N') {
              if(input[i] == 'n') num_token.upcase = false;
              tokens.push_back(num_token), input = input.substr(i+1), i = 0;
              continue;
            }
          }
          // parse 0 padding
          if(isdigit(input[i])) {
            int padded_0s = extract_up_to_2_digits(input[i],input[i+1],i);
            num_token.padded_0s = padded_0s;
            if(input[i] == 'n' || input[i] == 'N') {
              if(input[i] == 'n') num_token.upcase = false;
              tokens.push_back(num_token), input = input.substr(i+1), i = 0;
              continue;
            }
          }
          // parse precision
          if(input[i] == '.') {
            ++i;
            if(!isdigit(input[i])) 
              throw_invalid_sprintf_precision(input[i]-'0');
            int prec = extract_up_to_2_digits(input[i],input[i+1],i);
            num_token.precision = prec;
          }
          // finalize number parsing
          if(input[i] != 'n' && input[i] != 'N')
            throw_invalid_sprintf_token(input.substr(token_start,i+(i<input.size())));
          if(input[i] == 'n') num_token.upcase = false;
          tokens.push_back(num_token), input = input.substr(i+1), i = 0;
        } else {
          char bad_token[3] = {'%',next_ch,'\0'};
          throw_invalid_sprintf_token(bad_token);
        }
      } else {
        ++i;
      } 
    }
    split_str.push_back(input);
  }

  #undef throw_invalid_sprintf_token
  #undef throw_invalid_sprintf_precision


  void unpack_sequence_into_formatted_string(string& formatted, data& d, string(data::*stringify)()const, 
                                             const char* format, const char* name, data_vector& args) {
    // Unpack vectors
    if(d.is_type(types::vec)) {
      for(size_type i = 0, n = d.vec->size(); i < n; ++i) {
        formatted += (d.vec->operator[](i).*stringify)();
        if(i+1 < n) formatted += ' ';
      }
    // Unpack strings
    } else if(d.is_type(types::str)) {
      for(size_type i = 0, n = d.str->size(); i < n; ++i) {
        formatted += (data(d.str->operator[](i)).*stringify)();
        if(i+1 < n) formatted += ' ';
      }
    // Unpack nil (nothing printed)
    } else if(primitive_toolkit::data_is_nil(d)) {
      return;
    // Unpack proper lists
    } else if(d.is_type(types::par) && get_list_status(d) == list_status::proper) {
      auto iter = d;
      while(iter.is_type(types::par)) {
        formatted += (iter.par->first.*stringify)();
        if(iter.par->second.is_type(types::par))
          formatted += ' ';
        iter = iter.par->second;
      }
    } else {
      HEIST_THROW_ERR('\'' << name << " arg " << HEIST_PROFILE(d) << " isn't a sequence"
        " (required by format token)!" << format << HEIST_FCN_ERR(name,args));
    }
  }


  // IMPROPER FORMAT ARG ERROR HANDLING
  #define THROW_BAD_FORMAT_ARG(type_name)\
    HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[i])<<" isn't a "\
              type_name " (required by format token)!"\
              <<format<<HEIST_FCN_ERR(name,args));


  string generated_formatted_string(const string& input, const char* format, const char* name, data_vector& args){
    sprintf_tokens_t tokens; // extracted token data
    str_vector split_str; // splits <input> at each token
    parse_sprintf_token_stream(input, tokens, split_str, format, name, args);
    if(args.size() != split_str.size())
      HEIST_THROW_ERR('\''<<name<<" Element count ("<<args.size()-1<<") doesn't match token count ("<<split_str.size()-1<<")!"
        << format << HEIST_FCN_ERR(name,args));
    if(tokens.size() != split_str.size()-1)
      HEIST_THROW_ERR("-:- FATAL INTERPRETER ERROR -:- IMPROPER SPRINTF TOKEN PARSING -:-"
        "\n     => !!! NUMBER OF TOKENS != TOTAL-SPLIT-STRINGS - 1 !!!"
        "\n     => Please send your code to jordanran199@gmail.com to fix the interpreter's bug!");
    string formatted(split_str[0]), num_str, str_str;
    size_type padding_amount = 0;
    for(size_type i = 1, n = split_str.size(); i < n; ++i) {
      switch(tokens[i-1].token) {
        case sprintf_token_t::token_t::a:  formatted += args[i].display(); break;
        case sprintf_token_t::token_t::wa: formatted += args[i].write();   break;
        case sprintf_token_t::token_t::pa: formatted += args[i].pprint();  break;
        case sprintf_token_t::token_t::ellipsis:
          unpack_sequence_into_formatted_string(formatted,args[i],&data::display,format,name,args); 
          break;
        case sprintf_token_t::token_t::wellipsis:
          unpack_sequence_into_formatted_string(formatted,args[i],&data::write,format,name,args); 
          break;
        case sprintf_token_t::token_t::pellipsis:
          unpack_sequence_into_formatted_string(formatted,args[i],&data::pprint,format,name,args); 
          break;
        case sprintf_token_t::token_t::dollar:  
          if(!data_is_a_valid_dollar_value(args[i])) THROW_BAD_FORMAT_ARG("real finite numeric");
          formatted += generate_dollar_value_string(args[i].num,tokens[i-1].commas); break;
        case sprintf_token_t::token_t::s:  
          if(!args[i].is_type(types::str)) THROW_BAD_FORMAT_ARG("string");
          // apply padding as needed
          str_str = args[i].display();
          padding_amount = (size_type)std::abs(tokens[i-1].padding);
          if(str_str.size() < padding_amount) {
            padding_amount -= str_str.size();
            if(tokens[i-1].padding < 0) { // pad right
              formatted += str_str + string(padding_amount, ' ');
            } else {                      // pad left
              formatted += string(padding_amount, ' ') + str_str;
            }
          } else {
            padding_amount = 0;
            formatted += str_str;
          }
          break;
        case sprintf_token_t::token_t::ws: 
          if(!args[i].is_type(types::str)) THROW_BAD_FORMAT_ARG("string");
          formatted += args[i].write(); break;
        case sprintf_token_t::token_t::c:  
          if(!args[i].is_type(types::chr)) THROW_BAD_FORMAT_ARG("character");
          formatted += args[i].display(); break;
        case sprintf_token_t::token_t::wc: 
          if(!args[i].is_type(types::chr)) THROW_BAD_FORMAT_ARG("character");
          formatted += args[i].write(); break;
        case sprintf_token_t::token_t::b: 
          if(args[i].is_truthy()) formatted += "#t";
          else                    formatted += "#f";
          break;
        case sprintf_token_t::token_t::wb: 
          if(args[i].is_truthy()) formatted += "true";
          else                    formatted += "false";
          break;
        case sprintf_token_t::token_t::n: 
          if(!args[i].is_type(types::num)) THROW_BAD_FORMAT_ARG("number");
          num_type num = args[i].num;
          // alter precision
          if(tokens[i-1].precision != -1) {
            if(!num.is_real())
              HEIST_THROW_ERR('\''<<name<<" arg number "<<num<<" must be real to change precisions!"
                << format << HEIST_FCN_ERR(name,args));
            num_type::inexact_t prec_factor = std::pow(10.0L,num_type::inexact_t(tokens[i-1].precision));
            num = std::trunc(num.extract_inexact() * prec_factor) / prec_factor;
          }
          // alter exactness
          if(tokens[i-1].exactness == sprintf_token_t::exactness_t::exact)
            num = num.to_exact();
          else if(tokens[i-1].exactness == sprintf_token_t::exactness_t::inexact)
            num = num.to_inexact();
          // add sign
          if(tokens[i-1].show_sign && should_show_sign(num)) formatted += '+';
          // alter base
          if(tokens[i-1].base >= 11 && !tokens[i-1].upcase) num_str = stdlib_strings::lowercase_str(num.str(tokens[i-1].base));
          else if(tokens[i-1].base != 10)                   num_str = num.str(tokens[i-1].base);
          else                                              num_str = num.str();
          // pad 0s
          if((size_type)tokens[i-1].padded_0s > (num_str.size() - (num_str[0] == '-'))) {
            if(num_str[0] == '-') {
              num_str.erase(0,1);
              num_str = '-' + string(tokens[i-1].padded_0s - num_str.size(), '0') + num_str;
            } else {
              num_str = string(tokens[i-1].padded_0s - num_str.size(), '0') + num_str;
            }
          }
          // insert commas
          if(should_insert_commas(tokens[i-1],num)) insert_num_commas(num_str);
          formatted += num_str;
      }
      formatted += split_str[i];
    }
    return formatted;
  }


  #undef THROW_BAD_FORMAT_ARG


  // DISPLAYF, WRITEF, PPRINTF VALIDATION
  template<prm_ptr_t OUTPUT_FCN>
  data generic_formatted_output(data_vector& args, const char* format, const char* name) {
    if(args.empty())
      HEIST_THROW_ERR('\''<<name<<" no args received!" << format << HEIST_FCN_ERR(name,args));
    if((!args[0].is_type(types::fop) || !args[0].fop.is_open()) && !args[0].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 1st arg "<<HEIST_PROFILE(args[0])<<" isn't an open output port or string!" 
        << format << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::str)) {
      if(args.size() < 2)
        HEIST_THROW_ERR('\''<<name<<" not enough args (only received an open output port)!" 
          << format << HEIST_FCN_ERR(name,args));
      if(!args[1].is_type(types::str))
        HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a string!" 
          << format << HEIST_FCN_ERR(name,args));
    }
    // no port
    if(args[0].is_type(types::str)){
      return OUTPUT_FCN(data_vector(1,make_str(generated_formatted_string(*args[0].str,format,name,args))));
    }
    // port
    data_vector output_arg(2);
    output_arg[1] = args[0];
    args.erase(args.begin()); // rm port prior parsing formatted string
    output_arg[0] = make_str(generated_formatted_string(*args[0].str,format,name,args));
    return OUTPUT_FCN(std::move(output_arg));
  }

} // End of namespace heist::stdlib_output

#endif