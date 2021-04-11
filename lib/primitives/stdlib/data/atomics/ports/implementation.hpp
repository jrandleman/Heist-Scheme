// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for ports.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_PORTS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_PORTS_IMPLEMENTATION_HPP_

namespace heist::stdlib_ports {

  /******************************************************************************
  * ARGUMENT VALIDATION
  ******************************************************************************/

  void confirm_no_args_given(const data_vector& args, const char* name) {
    if(!args.empty()) 
      HEIST_THROW_ERR('\''<<name<<" doesn't accept any args!\n     ("<<name<<')'<<HEIST_FCN_ERR(name,args));
  }


  void confirm_given_one_arg(const data_vector& args, const char* name) {
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" didn't receive exactly 1 arg!\n     ("<<name<<" <filename-string>)"<<HEIST_FCN_ERR(name,args));
  }


  // Confirm given a single string argument
  void confirm_given_one_string_arg(const data_vector& args, const char* name, const char* format){
    if(args.size() != 1) 
      HEIST_THROW_ERR('\''<<name<<" didn't receive any args: "<<format<<HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::str)) 
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't a string: "<<format
        << HEIST_FCN_ERR(name,args));
  }


  // Confirm the port predicate was given 1 port
  void confirm_given_one_port_arg(const data_vector& args, const char* name){
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" didn't receive exactly 1 arg!"
        "\n     ("<<name<<" <port>)" <<HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::fip) && !args[0].is_type(types::fop))
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't a port!"
        "\n     ("<<name<<" <port>)" <<HEIST_FCN_ERR(name,args));
  }

  /******************************************************************************
  * PORT VALIDATION
  ******************************************************************************/

  bool is_nonstd_open_input_port(data& d)noexcept{
    return d.is_type(types::fip) && d.fip.is_open() && *d.fip.fp != stdin;
  }


  bool is_nonstd_open_output_port(data& d)noexcept{
    return d.is_type(types::fop) &&  d.fop.is_open() && *d.fop.fp != stdout && *d.fop.fp != stderr;
  }

  /******************************************************************************
  * CALL-WITH-INPUT-FILE & CALL-WITH-OUTPUT-FILE
  ******************************************************************************/

  // Call an unary procedure with a file's port as its argument
  template<typename port_ctor,typename PORT_GETTER>
  data call_with_file(data_vector& args,  const char* name, const char* format, PORT_GETTER get_port){
    // confirm given a filename string & a procedure
    if(args.size() != 2)
      HEIST_THROW_ERR('\'' << name << " received incorrect # of args:" << format << HEIST_FCN_ERR(name,args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[1], args, name, format);
    // apply the given procedure w/ a port to the file
    return execute_application(procedure,data_vector(1,port_ctor(get_port(args[0],name,format,args))));
  }

  /******************************************************************************
  * WITH-INPUT-FROM-FILE & WITH-OUTPUT-TO-FILE
  ******************************************************************************/

  // Call an argless procedure with a file's port as the default port
  template<typename PORT_GETTER, typename PORT_TYPE>
  data with_file(data_vector& args,  const char* name, const char* format, PORT_TYPE& DEFAULT_PORT, PORT_GETTER get_port){
    // confirm given a filename string & a procedure
    if(args.size() != 2)
      HEIST_THROW_ERR('\'' << name << " received incorrect # of args:" << format << HEIST_FCN_ERR(name,args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[1], args, name, format);
    // save & set the current port
    auto original_port = DEFAULT_PORT;
    DEFAULT_PORT = PORT_TYPE(get_port(args[0], name, format, args));
    // apply the given procedure
    data result;
    try {
      result = execute_application(procedure,data_vector());
    } catch(const SCM_EXCEPT& err) {
      // reset the current port prior re-throwing the error
      DEFAULT_PORT = std::move(original_port);
      throw err;
    }
    // reset the current port
    DEFAULT_PORT = std::move(original_port);
    return result;
  }

  /******************************************************************************
  * PORT OPENING FROM FILENAMES
  ******************************************************************************/

  // Confirm the given filename exists as a file
  bool confirm_file_exists(const char* filename)noexcept{
    FILE* existential_check = fopen(filename, "r");
    const bool exists = existential_check != nullptr;
    if(existential_check) fclose(existential_check);
    return exists;
  }


  // Returns a file pointer if 'filename' is:
  // => INPUT:  the string name of an existing file
  // => OUTPUT: a file we have permission to write to
  FILE* confirm_valid_io_file(const data& filename, const char* name, 
                              const char* format,   const char* file_open_type, const data_vector& args){
    // confirm given a proper filename
    if(!filename.is_type(types::str))
      HEIST_THROW_ERR('\'' << name << ' ' << HEIST_PROFILE(filename)
        << " is not a filename string:" << format << HEIST_FCN_ERR(name,args));
    // if OUTPUT (write), confirm file doesn't exist
    if(file_open_type[0] == 'w' && confirm_file_exists(filename.str->c_str()))
      HEIST_THROW_ERR('\'' << name << " file \"" << *filename.str 
        << "\" already exists!" << format << HEIST_FCN_ERR(name,args));
    // open the file
    FILE* fp = fopen(filename.str->c_str(), file_open_type);
    // if INPUT, confirm file isnt null
    if(fp == nullptr && file_open_type[0] == 'r')
      HEIST_THROW_ERR('\'' << name << " file \"" << *filename.str
        << "\" doesn't exist (invalid for input):"<<format<<HEIST_FCN_ERR(name,args));
    // if OUTPUT (write), confirm file doesn't exist
    if(fp == nullptr)
      HEIST_THROW_ERR('\'' << name << " file \"" << *filename.str 
        << "\" couldn't be opened!" << format << HEIST_FCN_ERR(name,args));
    return fp;
  }


  // Returns a file pointer if 'filename' is the string name of an existing file
  FILE* confirm_valid_input_file(const data& filename, const char* name, 
                                 const char* format,   const data_vector& args) {
    return confirm_valid_io_file(filename, name, format, "r", args);
  }


  // Returns a file pointer if 'filename' is a file we have permission to write
  FILE* confirm_valid_output_file(const data& filename, const char* name, 
                                  const char* format,   const data_vector& args) {
    return confirm_valid_io_file(filename, name, format, "w", args);
  }


  // Returns a file pointer if 'filename' is a file we have permission to append
  FILE* confirm_valid_output_append_file(const data& filename, const char* name, 
                                         const char* format,   const data_vector& args) {
    return confirm_valid_io_file(filename, name, format, "a", args);
  }

  /******************************************************************************
  * PORT-SEEK! & PORT-SEEK-FRONT!
  ******************************************************************************/

  void validate_port_args(data_vector& args, const char* name) {
    static constexpr const auto min_offset = std::numeric_limits<long int>::min();
    static constexpr const auto max_offset = std::numeric_limits<long int>::max();
    if(args.size() != 2)
      HEIST_THROW_ERR('\''<<name<<" didn't receive 2 args!" 
        "\n     (" << name << " <open-port> <integer-offset>)"
        "\n     <integer-offset> ::= [" << min_offset << ", " << max_offset << ']' << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::fip) && !args[0].is_type(types::fop))
      HEIST_THROW_ERR('\''<<name<<" 1st arg " << HEIST_PROFILE(args[0]) << "\n     isn't a port:" 
        "\n     (" << name << " <open-port> <integer-offset>)"
        "\n     <integer-offset> ::= [" << min_offset << ", " << max_offset << ']' << HEIST_FCN_ERR(name,args));
    if(!args[1].is_type(types::num) || !args[1].num.is_integer())
      HEIST_THROW_ERR('\''<<name<<" 2nd arg " << HEIST_PROFILE(args[1]) << "\n     isn't an integer:" 
        "\n     (" << name << " <open-port> <integer-offset>)"
        "\n     <integer-offset> ::= [" << min_offset << ", " << max_offset << ']' << HEIST_FCN_ERR(name,args));
    if(args[1].num < min_offset)
      HEIST_THROW_ERR('\''<<name<<" 2nd arg " << HEIST_PROFILE(args[1]) << "\n     is too low to be a port offset!" 
        "\n     (" << name << " <open-port> <integer-offset>)"
        "\n     <integer-offset> ::= [" << min_offset << ", " << max_offset << ']' << HEIST_FCN_ERR(name,args));
    if(args[1].num > max_offset)
      HEIST_THROW_ERR('\''<<name<<" 2nd arg " << HEIST_PROFILE(args[1]) << "\n     is too high to be a port offset!" 
        "\n     (" << name << " <open-port> <integer-offset>)"
        "\n     <integer-offset> ::= [" << min_offset << ", " << max_offset << ']' << HEIST_FCN_ERR(name,args));
  }


  data generic_port_seek(data_vector& args, const char* name, const int origin){
    validate_port_args(args,name);
    int result = 0;
    if(is_nonstd_open_input_port(args[0])){
      result = fseek(*args[0].fip.fp, (long int)args[1].num.extract_inexact(), origin);
    } else if(is_nonstd_open_output_port(args[0])){
      result = fseek(*args[0].fop.fp, (long int)args[1].num.extract_inexact(), origin);
    }
    return boolean(!result);
  }

} // End of namespace heist::stdlib_ports

#endif