// Author: Jordan Randleman -- jordanran199@gmail.com -- ports.hpp
// => Defines the primitive port functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_PORTS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_PORTS_HPP_

#include "implementation.hpp"

namespace heist {

  data primitive_OPEN_PORTP(data_vector&& args) {
    stdlib_ports::confirm_given_one_port_arg(args,"open-port?");
    if(args[0].is_type(types::fip))
      return boolean(args[0].fip.is_open());
    return boolean(args[0].fop.is_open());
  }

  data primitive_CLOSED_PORTP(data_vector&& args) {
    stdlib_ports::confirm_given_one_port_arg(args,"closed-port?");
    if(args[0].is_type(types::fip))
      return boolean(!args[0].fip.is_open());
    return boolean(!args[0].fop.is_open());
  }

  // retrieve the current default input & output ports
  data primitive_CURRENT_INPUT_PORT(data_vector&& args){
    stdlib_ports::confirm_no_args_given(args,"current-input-port");
    return G.CURRENT_INPUT_PORT;
  }

  data primitive_CURRENT_OUTPUT_PORT(data_vector&& args){
    stdlib_ports::confirm_no_args_given(args,"current-output-port");
    return G.CURRENT_OUTPUT_PORT;
  }

  // call an unary procedure with a file's port as its argument
  data primitive_CALL_WITH_INPUT_FILE(data_vector&& args){
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CALL_WITH_INPUT_FILE,args);
    return stdlib_ports::call_with_file<iport>(
              args,
              "call-with-input-file",
              "\n     (call-with-input-file <filename-string> <unary-callable>)"
              "\n     <unary-callable> must accept a port as its argument!",
              stdlib_ports::confirm_valid_input_file);
  }
  
  data primitive_CALL_WITH_OUTPUT_FILE(data_vector&& args){
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CALL_WITH_OUTPUT_FILE,args);
    return stdlib_ports::call_with_file<oport>(
              args,
              "call-with-output-file",
              "\n     (call-with-output-file <filename-string> <unary-callable>)"
              "\n     <unary-callable> must accept a port as its argument!",
              stdlib_ports::confirm_valid_output_file);
  }

  // call an argless procedure with a file's port as the default port
  data primitive_WITH_INPUT_FROM_FILE(data_vector&& args){
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_WITH_INPUT_FROM_FILE,args);
    return stdlib_ports::with_file(
              args,
              "with-input-from-file",
              "\n     (with-input-from-file <filename-string> <nullary-callable>)",
              G.CURRENT_INPUT_PORT,
              stdlib_ports::confirm_valid_input_file);
  }
  
  data primitive_WITH_OUTPUT_TO_FILE(data_vector&& args){
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_WITH_OUTPUT_TO_FILE,args);
    return stdlib_ports::with_file(
              args,
              "with-output-to-file",
              "\n     (with-output-to-file <filename-string> <nullary-callable>)",
              G.CURRENT_OUTPUT_PORT,
              stdlib_ports::confirm_valid_output_file);
  }

  // retrieve a port for a file
  data primitive_OPEN_INPUT_FILE(data_vector&& args){
    // confirm given a filename string
    stdlib_ports::confirm_given_one_arg(args,"open-input-file");
    return iport(stdlib_ports::confirm_valid_input_file(args[0],"open-input-file","\n     (open-input-file <filename-string>)",args));
  }

  data primitive_OPEN_OUTPUT_FILE(data_vector&& args){ // open iff filename dne
    // confirm given a filename string
    stdlib_ports::confirm_given_one_arg(args,"open-output-file");
    return oport(stdlib_ports::confirm_valid_output_file(args[0],"open-output-file","\n     (open-output-file <filename-string>)",args));
  }

  data primitive_OPEN_OUTPUT_FILE_PLUS(data_vector&& args){ // open via "append"
    // confirm given a filename string
    stdlib_ports::confirm_given_one_arg(args,"open-output-file+");
    return oport(stdlib_ports::confirm_valid_output_append_file(args[0],"open-output-file+","\n     (open-output-file+ <filename-string>)",args));
  }

  data primitive_OPEN_OUTPUT_FILE_BANG(data_vector&& args){ // deletes if exists, and opens anew
    // confirm given a filename string, & rm file if exists
    stdlib_ports::confirm_given_one_string_arg(args, "open-output-file!", "\n     (open-output-file! <filename-string>)");
    try { std::filesystem::remove_all(stdlib_filesystem::coerce_string_to_path(*args[0].str)); } catch(...) {}
    return oport(stdlib_ports::confirm_valid_output_file(args[0],"open-output-file!","\n     (open-output-file! <filename-string>)",args));
  }

  // rewind input or output port
  data primitive_REWIND_PORT_BANG(data_vector&& args){
    stdlib_ports::confirm_given_one_port_arg(args, "rewind-port!");
    if(stdlib_ports::is_nonstd_open_input_port(args[0])){
      rewind(*args[0].fip.fp);
    } else if(stdlib_ports::is_nonstd_open_output_port(args[0])){
      rewind(*args[0].fop.fp);
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  data primitive_PORT_SEEK_FRONT_BANG(data_vector&& args){
    return stdlib_ports::generic_port_seek(args,"port-seek-front!",SEEK_SET);
  }

  data primitive_PORT_SEEK_BANG(data_vector&& args){
    return stdlib_ports::generic_port_seek(args,"port-seek!",SEEK_CUR);
  }

  // close input or output port
  data primitive_CLOSE_PORT(data_vector&& args){
    stdlib_ports::confirm_given_one_port_arg(args, "close-port");
    if(stdlib_ports::is_nonstd_open_input_port(args[0])){
      args[0].fip.close();
    } else if(stdlib_ports::is_nonstd_open_output_port(args[0])){
      args[0].fop.close();
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

} // End of namespace heist

#endif