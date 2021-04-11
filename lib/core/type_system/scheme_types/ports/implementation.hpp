// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Contains method implementations of port objects for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_PORT_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_PORT_IMPLEMENTATION_HPP_

// NOTE: <pointer_to_hexstring> comes from "scheme_types/data/implementation_helpers/stringification.hpp"

namespace heist {

  /******************************************************************************
  * GET CURRENT INPUT/OUTPUT PORT AS FILE POINTERS
  ******************************************************************************/

  // returns "stdout" if !G.CURRENT_OUTPUT_PORT.is_open()
  FILE* noexcept_get_current_output_port()noexcept{
    if(G.CURRENT_OUTPUT_PORT.is_open()) return *G.CURRENT_OUTPUT_PORT.fp;
    return stdout;
  }


  // returns "stdin" if !G.CURRENT_INPUT_PORT.is_open()
  FILE* noexcept_get_current_input_port()noexcept{
    if(G.CURRENT_INPUT_PORT.is_open()) return *G.CURRENT_INPUT_PORT.fp;
    return stdin;
  }

  /******************************************************************************
  * IPORT IMPLEMENTATION
  ******************************************************************************/

  // Closing
  void iport::close()noexcept{
    if(fp && *fp && *fp != stdin) {
      fclose(*fp); 
      *fp = nullptr;
    }
  }


  // Stringification
  string iport::str()const noexcept{
    return "#<input-port[0x" + pointer_to_hexstring(fp.ptr) + "]>";
  }


  // Destructor
  iport::~iport()noexcept{
    if(fp.use_count() == 1 && fp && *fp && *fp != stdin) { 
      fclose(*fp);
      *fp = nullptr;
    }
  }

  /******************************************************************************
  * OUTPORT IMPLEMENTATION
  ******************************************************************************/

  // Closing
  void oport::close()noexcept{
    if(fp && *fp && *fp != stdout && *fp != stderr) {
      fclose(*fp); 
      *fp = nullptr;
    }
  }


  // Stringification
  string oport::str()const noexcept{
    return "#<output-port[0x" + pointer_to_hexstring(fp.ptr) + "]>";
  }


  // Destructor
  oport::~oport()noexcept{
    if(fp.use_count() == 1 && fp && *fp && *fp != stdout && *fp != stderr) { 
      fclose(*fp);
      *fp = nullptr;
    }
  }
}

#endif