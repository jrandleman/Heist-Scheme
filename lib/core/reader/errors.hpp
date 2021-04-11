// Author: Jordan Randleman -- jordanran199@gmail.com -- errors.hpp
// => Contains the reader error-handling mechanisms for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_READER_ERRORS_HPP_
#define HEIST_SCHEME_CORE_READER_ERRORS_HPP_

namespace heist {
  // Reader error code enumeration
  enum class READER_ERROR {
    early_end_paren,    incomplete_string,      incomplete_expression, 
    incomplete_comment, quoted_end_of_buffer,   quoted_end_of_expression,
    quoted_space,       quoted_incomplete_char,
  };


  // Confirm reader error is a non-repl-specific fatal error
  constexpr bool is_non_repl_reader_error(const READER_ERROR& err)noexcept{
    return err == READER_ERROR::incomplete_string || 
           err == READER_ERROR::incomplete_expression || 
           err == READER_ERROR::incomplete_comment;
  }


  // Alert error as per read's throw
  void alert_reader_error(FILE* outs, const READER_ERROR& read_error, const string& input)noexcept{
    if(read_error == READER_ERROR::early_end_paren) {
      if(G.USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m ')' Found Prior A Matching '('!\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------\x1b[0m\n", outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m-------------------------------------------\x1b[0m\n", outs);
      } else {
        fputs("\n-------------------------------------------\n", outs);
        fputs("READ ERROR: ')' Found Prior A Matching '('!\n", outs);
        fputs("-------------------------------------------\n", outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("-------------------------------------------\n", outs);
      }
    } else if(read_error == READER_ERROR::quoted_end_of_buffer) {
      if(G.USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------------------------------\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Reader Macro Around a Non-Expression (Unexpected End of Buffer)!\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------------------------------\x1b[0m\n", outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m------------------------------------------------------------------------------\x1b[0m\n", outs);
      } else {
        fputs("\n----------------------------------------------------------------------------\n", outs);
        fputs("READ ERROR: Reader Macro Around a Non-Expression (Unexpected End of Buffer)!\n", outs);
        fputs("----------------------------------------------------------------------------\n", outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("----------------------------------------------------------------------------\n", outs);
      }
    } else if(read_error == READER_ERROR::quoted_end_of_expression) {
      if(G.USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m-----------------------------------------------------------------\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Reader Macro Around a Non-Expression (Unexpected Closing Paren)!\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m-----------------------------------------------------------------\x1b[0m\n", outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m----------------------------------------------------------------------------\x1b[0m\n", outs);
      } else {
        fputs("\n----------------------------------------------------------------------------\n", outs);
        fputs("READ ERROR: Reader Macro Around a Non-Expression (Unexpected Closing Paren)!\n", outs);
        fputs("----------------------------------------------------------------------------\n", outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("----------------------------------------------------------------------------\n", outs);
      }
    } else if(read_error == READER_ERROR::quoted_space) {
      if(G.USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------------------\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Reader Macro Around a Non-Expression (Unexpected Space)!\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------------------\x1b[0m\n", outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m--------------------------------------------------------------------\x1b[0m\n", outs);
      } else {
        fputs("\n---------------------------------------------------------------------\n", outs);
        fputs("READ ERROR: Reader Macro Around a Non-Expression (Unexpected Space)!\n", outs);
        fputs("---------------------------------------------------------------------\n", outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("---------------------------------------------------------------------\n", outs);
      }
    } else if(read_error == READER_ERROR::quoted_incomplete_char) {
      if(G.USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Reader Macro Around An Incomplete Character!\x1b[0m\n", outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------\x1b[0m\n", outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m--------------------------------------------------------\x1b[0m\n", outs);
      } else {
        fputs("\n--------------------------------------------------------\n", outs);
        fputs("READ ERROR: Reader Macro Around An Incomplete Character!\n", outs);
        fputs("--------------------------------------------------------\n", outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("--------------------------------------------------------\n", outs);
      }
    } 
  }


  void alert_non_repl_reader_error(FILE* outs, const READER_ERROR& read_error, const string& input)noexcept{
    if(read_error == READER_ERROR::incomplete_string) {
      if(G.USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Incomplete String, Missing a Closing '\"'!\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m------------------------------------------\x1b[0m\n",outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m-----------------------------------------------------\x1b[0m\n",outs);
      } else {
        fputs("\n-----------------------------------------------------\n",outs);
        fputs("READ ERROR: Incomplete String, Missing a Closing '\"'!\n",outs);
        fputs("-----------------------------------------------------\n",outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("-----------------------------------------------------\n",outs);
      }
    } else if(read_error == READER_ERROR::incomplete_expression) {
      if(G.USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Incomplete Expression, Missing a Closing ')'!\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m----------------------------------------------\x1b[0m\n",outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m---------------------------------------------------------\x1b[0m\n",outs);
      } else {
        fputs("\n---------------------------------------------------------\n",outs);
        fputs("READ ERROR: Incomplete expression, Missing a Closing ')'!\n",outs);
        fputs("---------------------------------------------------------\n",outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("---------------------------------------------------------\n",outs);
      }
    } else if(read_error == READER_ERROR::incomplete_comment) {
      if(G.USING_ANSI_ESCAPE_SEQUENCES) {
        fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------------------\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Incomplete Comment, Missing a Closing \"|#\"!\x1b[0m\n",outs);
        fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m--------------------------------------------\x1b[0m\n",outs);
        fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
        fputs("\x1b[1m-------------------------------------------------------\x1b[0m\n",outs);
      } else {
        fputs("\n-------------------------------------------------------\n",outs);
        fputs("READ ERROR: Incomplete Comment, Missing a Closing \"|#\"!\n",outs);
        fputs("-------------------------------------------------------\n",outs);
        fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
        fputs("-------------------------------------------------------\n",outs);
      }
    }
  }
  

  void alert_reader_error(FILE* outs, const size_type& read_error_index, const string& input)noexcept{
    if(G.USING_ANSI_ESCAPE_SEQUENCES) {
      fputs("\n\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------\x1b[0m\n", outs);
      fprintf(outs,"\x1b[1m\x1b[31mREAD ERROR:\x1b[0m\x1b[1m Unparsable Type In Expression At Index = %03zu\x1b[0m\n",read_error_index);
      fputs("\x1b[1m\x1b[31m-----------\x1b[0m\x1b[1m---------------------------------------------\x1b[0m\n", outs);
      fprintf(outs, "\x1b[1m\x1b[31mEXPRESSION:\x1b[0m\x1b[1m\n%s\x1b[0m\n", input.c_str());
      fputs("\x1b[1m--------------------------------------------------------\x1b[0m\n", outs);
    } else {
      fputs("\n--------------------------------------------------------\n", outs);
      fprintf(outs,"READ ERROR: Unparsable Type In Expression At Index = %03zu\n",read_error_index);
      fputs("--------------------------------------------------------\n", outs);
      fprintf(outs, "EXPRESSION:\n%s\n", input.c_str());
      fputs("--------------------------------------------------------\n", outs);
    }
  }
}

#endif