// Author: Jordan Randleman -- jrandleman@scu.edu -- input.hpp
// => Defines the primitive input functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_INPUT_HPP_
#define HEIST_SCHEME_CORE_STDLIB_INPUT_HPP_

#include "implementation.hpp"

namespace heist {

  data primitive_READ(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (read <optional-open-input-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "read", format);
    FILE* ins = primitive_toolkit::get_current_input_port(args, "read", format);
    // Confirm either given an open input port or string or no args
    bool reading_stdin = (ins == stdin), reading_string = false;
    if(!stdlib_input::confirm_valid_input_args_and_non_EOF(args, ins, "read", reading_stdin, reading_string)) 
      return chr_type(EOF);
    if(reading_string)
      return stdlib_input::read_from_string_logic(*args[0].str);
    return stdlib_input::read_from_input_port_logic(outs,ins,reading_stdin);
  }

  data primitive_READ_STRING(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (read-string <optional-open-input-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "read-string", format);
    FILE* ins = primitive_toolkit::get_current_input_port(args, "read-string", format);
    // return string w/ next valid scheme expression, if successfully parsed one
    bool reading_stdin = (ins == stdin), reading_string = false;
    if(!stdlib_input::confirm_valid_input_args_and_non_EOF(args, ins, "read-string", reading_stdin, reading_string)) 
      return chr_type(EOF);
    if(reading_string)
      return make_str(stdlib_input::read_from_string_logic(*args[0].str).write());
    return make_str(stdlib_input::read_from_input_port_logic(outs,ins,reading_stdin).write());
  }

  data primitive_READ_LINE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (read-line <optional-open-input-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "read-line", format);
    FILE* ins = primitive_toolkit::get_current_input_port(args, "read-line", format);
    // Confirm either given an open input port or no args
    bool reading_stdin = (ins == stdin), reading_string = false;
    if(!stdlib_input::confirm_valid_input_args_and_non_EOF(args, ins, "read-line", reading_stdin, reading_string)) 
      return chr_type(EOF);
    // Read a line of input into a string
    if(reading_string) {
      auto line_buffer = args[0].str->substr(0,args[0].str->find('\n'));
      args[0].str->erase(0,args[0].str->find('\n'));
      return make_str(line_buffer);
    }
    string line_buffer;
    fflush(outs);
    int ch = 0;
    while((ch = fgetc(ins)) != '\n' && ch != EOF) line_buffer += ch;
    if(ch == EOF && ins == stdin) clearerr(ins);
    return make_str(line_buffer);
  }

  data primitive_READ_CHAR(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (read-char <optional-open-input-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "read-char", format);
    FILE* ins = primitive_toolkit::get_current_input_port(args, "read-char", format);
    // Confirm either given an open input port or no args
    bool reading_stdin = (ins == stdin), reading_string = false;
    if(!stdlib_input::confirm_valid_input_args_and_non_EOF(args, ins, "read-char", reading_stdin, reading_string)) 
      return chr_type(EOF);
    // Read a char from a string as needed
    if(reading_string) {
      auto ch = args[0].str->operator[](0);
      args[0].str->erase(0,1);
      return chr_type(ch);
    }
    // Read a char from a port as needed
    fflush(outs);
    if(!reading_stdin) return chr_type(getc(ins));
    // Else read 1 char from stdin & throw away the rest of the line
    int ch = getc(stdin);
    if(ch == EOF) {
      clearerr(stdin);
    } else if(ch != '\n') {
      while(getc(stdin) != '\n'); // eat rest of the line
    }
    return chr_type(ch);
  }

  data primitive_PEEK_CHAR(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (peek-char <optional-open-input-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "peek-char", format);
    FILE* ins = primitive_toolkit::get_current_input_port(args, "peek-char", format);
    // Confirm either given an open input port or no args
    bool reading_stdin = (ins == stdin), reading_string = false;
    if(!stdlib_input::confirm_valid_input_args_and_non_EOF(args, ins, "peek-char", reading_stdin, reading_string)) 
      return chr_type(EOF);
  // Peek a char from a string as needed
    if(reading_string) {
      auto ch = args[0].str->operator[](0);
      args[0].str->erase(0,1);
      return chr_type(ch);
    }
    // Peek a char from a port as needed
    fflush(outs);
    if(!reading_stdin) {
      int ch = getc(ins);
      ungetc(ch, ins);
      return chr_type(ch);
    }
    // Else peek 1 char from stdin & throw away the rest of the line
    // NOTE: 'peek-char' from stdin is equivalent to 'read-char' from stdin since
    //       both return 1 char from the stream & throw away the rest of the line
    int ch = getc(stdin);
    if(ch == EOF) {
      clearerr(stdin);
    } else if(ch != '\n') {
      while(getc(stdin) != '\n'); // eat rest of the line
    }
    return chr_type(ch);
  }

  data primitive_CHAR_READYP(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (char-ready? <optional-open-input-port-or-string>)";
    FILE* ins = primitive_toolkit::get_current_input_port(args, "char-ready?", format);
    // Confirm either given an open input port or no args
    bool reading_stdin = (ins == stdin), reading_string = false;
    if(!stdlib_input::confirm_valid_input_args_and_non_EOF(args, ins, "char-ready?", reading_stdin, reading_string)) 
      return GLOBALS::FALSE_DATA_BOOLEAN;
    // Empty strings trigger EOF above, hence will always have a character ready here.
    // Stdin is always assumed to be ready to be read from.
    if(reading_string || reading_stdin) return GLOBALS::TRUE_DATA_BOOLEAN;
    // Peek the non-stdin port for a non-EOF character
    int ch = getc(ins);
    ungetc(ch, ins);
    return boolean(ch != EOF);
  }

  // slurp a port's contents into a string
  data primitive_SLURP_PORT(data_vector&& args){
    static constexpr const char * const format = 
      "\n     (slurp-port <optional-open-input-port-or-string>)";
    FILE* ins = primitive_toolkit::get_current_input_port(args, "slurp-port", format);
    // confirm given a filename string & slurp file if so
    bool reading_stdin = (ins == stdin), reading_string = false;
    if(!stdlib_input::confirm_valid_input_args_and_non_EOF(args, ins, "slurp-port", reading_stdin, reading_string)) 
      return make_str("");
    if(reading_string) return make_str(*args[0].str);
    if(reading_stdin)  return make_str("");
    string buffer;
    int ch = 0;
    while((ch = fgetc(ins)) != EOF) buffer += ch; // slurp entire file
    fclose(ins);
    return make_str(buffer);
  }

  // slurp a file's contents into a string
  data primitive_SLURP_FILE(data_vector&& args){
    // confirm given a filename string & slurp file if so
    stdlib_input::confirm_given_one_string_arg(args,"slurp-file","\n     (slurp-file <filename-string>)");
    FILE* ins = stdlib_input::confirm_valid_input_file(args[0],"slurp-file","\n     (slurp-file <filename-string>)",args);
    string buffer;
    int ch = 0;
    while((ch = fgetc(ins)) != EOF) buffer += ch; // slurp entire file
    fclose(ins);
    return make_str(buffer);
  }

} // End of namespace heist

#endif