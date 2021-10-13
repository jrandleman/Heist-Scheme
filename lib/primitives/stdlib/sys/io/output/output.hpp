// Author: Jordan Randleman -- jordanran199@gmail.com -- output.hpp
// => Defines the primitive output functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_OUTPUT_HPP_
#define HEIST_SCHEME_CORE_STDLIB_OUTPUT_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * UNFORMATTED OUTPUT
  ******************************************************************************/

  data primitive_PPRINT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (pretty-print <obj> <optional-open-output-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "pretty-print", format);
    bool is_port = stdlib_output::confirm_valid_output_args(args, outs, 1, "pretty-print", format);
    if(!args[0].is_type(types::dne)) {
      if(is_port) {
        fputs(args[0].pprint().c_str(), outs);
        fflush(outs);
      } else {
        return make_str(*args[1].str + args[0].pprint());
      }
    }
    G.LAST_PRINTED_TO_STDOUT = (outs == stdout && is_port);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  data primitive_WRITE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (write <obj> <optional-open-output-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "write", format);
    bool is_port = stdlib_output::confirm_valid_output_args(args, outs, 1, "write", format);
    if(!args[0].is_type(types::dne)) {
      if(is_port) {
        fputs(args[0].write().c_str(), outs);
        fflush(outs);
      } else {
        return make_str(*args[1].str + args[0].write());
      }
    }
    G.LAST_PRINTED_TO_STDOUT = (outs == stdout && is_port);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  data primitive_NEWLINE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (newline <optional-open-output-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "newline", format);
    bool is_port = stdlib_output::confirm_valid_output_args(args, outs, 0, "newline", format);
    if(is_port) {
      fputc('\n', outs);
      fflush(outs);
      G.LAST_PRINTED_NEWLINE_TO_STDOUT = G.LAST_PRINTED_TO_STDOUT = (outs == stdout);
    } else {
      return make_str(*args[0].str + '\n');
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  data primitive_DISPLAY(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (display <obj> <optional-open-output-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "display", format);
    bool is_port = stdlib_output::confirm_valid_output_args(args, outs, 1, "display", format);
    if(is_port)
      return stdlib_output::display_port_logic(args[0], outs);
    return make_str(*args[1].str + args[0].display());
  }

  data primitive_WRITE_CHAR(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (write-char <char> <optional-open-output-port-or-string>)";
    FILE* outs = primitive_toolkit::get_current_output_port(args, "write-char", format);
    bool is_port = stdlib_output::confirm_valid_output_args(args, outs, 1, "write-char", format);
    // confirm given a character
    if(!args[0].is_type(types::chr))
      HEIST_THROW_ERR("'write-char arg "<<HEIST_PROFILE(args[0])<<" isn't a character:" 
        << format << HEIST_FCN_ERR("write-char", args));
    if(is_port) {
      fputc(args[0].chr, outs);
      fflush(outs);
      G.LAST_PRINTED_TO_STDOUT = (outs == stdout);
    } else {
      return make_str(*args[1].str + char(args[0].chr));
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * FORMATTED OUTPUT
  ******************************************************************************/

  #define sprintf_formatting_token_format\
    "\n     => <formatted-string> is like C's printf with unique formatting patterns:"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %a = display anything"\
    "\n        %wa = write anything"\
    "\n        %pa = pretty-print anything"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %... = display unpacked sequence"\
    "\n        %w... = write unpacked sequence"\
    "\n        %p... = pretty-print unpacked sequence"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %n = number"\
    "\n        %+n = number (show sign if positive too)"\
    "\n        %,n = number with commas (only for bigints)"\
    "\n        %En = %en = number (coerced to exact)"\
    "\n        %In = %in = number (coerced to inexact)"\
    "\n        %#rn = %#Rn = number (in base <#> from 2 to 36)"\
    "\n        %#n = number (left-padded with 0s to a width of <#> characters)"\
    "\n        %.#n = number (with <#> digits of precision)"\
    "\n        -> IE \"%+e2r.5n\": 5 digits of precision & mk exact in binary w/ sign"\
    "\n        -> NOTE: case of 'n' in \"%n\" denotes case of base >= 11 letters"\
    "\n        -> NOTE: 0-padding & precision MUST be of 2 digits or less!"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %$ = display real finite as a dollar value"\
    "\n        %,$ = display real finite as a dollar value seperated by commas"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %s = display string"\
    "\n        %#s = display string & pad left with # spaces"\
    "\n        %-#s = display string & pad right with # spaces"\
    "\n        %ws = write string"\
    "\n        -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %c = display char"\
    "\n        %wc = write char"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %b  = bool"\
    "\n        %wb = write \"true\" or \"false\" instead of \"#t\" or \"#f\""\
    "\n        ----------------------------------------------------------------------"\
    "\n        %%  = \"%\" (escapes a \"%\")"\
    "\n        ----------------------------------------------------------------------"


  // primitive "sprintf":
  // -> Parse token stream
  // -> Confirm token stream matches args
  // -> Splice in formatted args and return as a new string
  data primitive_SPRINTF(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (sprintf <formatted-string> <optional-arg1> <optional-arg2> ...)"
      sprintf_formatting_token_format;
    if(args.empty())
      HEIST_THROW_ERR("'sprintf no args received!" << format << HEIST_FCN_ERR("sprintf",args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR("'sprintf 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a string!" 
        << format << HEIST_FCN_ERR("sprintf",args));
    return make_str(stdlib_output::generated_formatted_string(*args[0].str,format,"sprintf",args));
  }

  // primitive "displayf":
  data primitive_DISPLAYF(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (displayf <optional-output-port> <formatted-string> <optional-arg1> ...)"
      sprintf_formatting_token_format;
    return stdlib_output::generic_formatted_output<primitive_DISPLAY>(args,format,"displayf");
  }

  // primitive "writef":
  data primitive_WRITEF(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (writef <optional-output-port> <formatted-string> <optional-arg1> ...)"
      sprintf_formatting_token_format;
    return stdlib_output::generic_formatted_output<primitive_WRITE>(args,format,"writef");
  }

  // primitive "pprintf" & "pretty-printf":
  data primitive_PPRINTF(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (pprintf <optional-output-port> <formatted-string> <optional-arg1> ...)"
      sprintf_formatting_token_format;
    return stdlib_output::generic_formatted_output<primitive_PPRINT>(args,format,"pprintf");
  }


  #undef sprintf_formatting_token_format

  /******************************************************************************
  * "fmt:" PREFIXED ANSI ESCAPE CODE PRIMITIVES
  ******************************************************************************/

  #include "fmt_implementation.hpp"

  /******************************************************************************
  * STRING->ART PRIMITIVES
  ******************************************************************************/

  data primitive_COERCE_STRING_TO_ASCII_ART(data_vector&& args) { 
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'string->ascii-art didn't receive 1 string arg!"
        "\n     (string->ascii-art <string>)" << HEIST_FCN_ERR("string->ascii-art",args));
    return make_str(heist_fmt::convert_to_ascii_art(*args[0].str));
  }

  data primitive_COERCE_STRING_TO_SPACE_ART(data_vector&& args) { 
    if(args.size() != 1 || !args[0].is_type(types::str))
      HEIST_THROW_ERR("'string->space-art didn't receive 1 string arg!"
        "\n     (string->space-art <string>)\n     => <nansi> must be inactive!" 
        << HEIST_FCN_ERR("string->space-art",args));
    if(!G.USING_ANSI_ESCAPE_SEQUENCES)
      HEIST_THROW_ERR("'string->space-art can't be used with <nansi> active!"
        "\n     (string->space-art <string>)\n     => <nansi> must be inactive!" 
        << HEIST_FCN_ERR("string->space-art",args));
    return make_str(heist_fmt::convert_to_whitespace_art(*args[0].str));
  }

} // End of namespace heist

#endif