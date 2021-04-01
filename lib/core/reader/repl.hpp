// Author: Jordan Randleman -- jrandleman@scu.edu -- repl.hpp
// => Contains the "read_user_input" & "launch_repl" procedures for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_REPL_HPP_
#define HEIST_SCHEME_CORE_REPL_HPP_

namespace heist {

  /******************************************************************************
  * EVALUATOR & ANALYSIS (FROM "lib/core/evaluator/evaluator.hpp")
  ******************************************************************************/

  // Evaluate <datum> in the context of <env>.
  data scm_eval(data&& datum, env_type& env);

  // Analyze <datum>'s syntax to yield its execution procedure.
  // Toggle <tail_call> & <cps_block> to analyze <datum> in either context.
  exe_fcn_t scm_analyze(data&& datum,const bool tail_call=false,const bool cps_block=false);

  /******************************************************************************
  * REPL INPUT READER
  ******************************************************************************/

  void announce_input(FILE* outs)noexcept{fputs(G.REPL_PROMPT.c_str(), outs);}
  void indent_input(FILE* outs)  noexcept{fputs(G.REPL_TAB.c_str(), outs);}


  // Read & parse user expressions
  data_vector read_user_input(FILE* outs, FILE* ins, const bool& in_repl=true){
    string input, tmp_buffer;
    data_vector abstract_syntax_tree;
    int ch;
    for(;;) {
      // Read input
      fflush(outs);
      tmp_buffer.clear();
      while((ch = fgetc(ins)) != '\n' && ch != EOF) tmp_buffer += ch;
      // Handle EOF Signal
      if(ch == EOF && ins == stdin) {
        clearerr(stdin);
        if(in_repl) return data_vector(1,chr_type(EOF)); // called by REPL
        return data_vector();                            // called by <read>
      }
      // Try parsing the expression, & read more input if unsuccessful 
      try {
        // Return AST if successfully parsed an expression
        parse_input_exp(input+'\n'+tmp_buffer,abstract_syntax_tree);
        return abstract_syntax_tree;
      } catch(const READER_ERROR& read_error) {
        // Continue parsing the current expression/string if incomplete
        if(is_non_repl_reader_error(read_error)) {
          input += '\n'+tmp_buffer;
          if(read_error == READER_ERROR::incomplete_expression) 
            indent_input(outs);
        // Alert user if read an invalid expression, then reprompt for input
        } else {
          if(!input.empty()) alert_reader_error(outs,read_error,input+'\n'+tmp_buffer);
          else               alert_reader_error(outs,read_error,tmp_buffer);
          fputc('\n', outs);
          if(in_repl) announce_input(outs);
          abstract_syntax_tree.clear(), input.clear();
        }
      // Alert user if detected unparsable input (-:- ANOMALY -:-)
      } catch(const size_type& read_error_index) {
        if(!input.empty()) alert_reader_error(outs,read_error_index,input+'\n'+tmp_buffer);
        else               alert_reader_error(outs,read_error_index,tmp_buffer);
        fputc('\n', outs);
        if(in_repl) announce_input(outs);
        abstract_syntax_tree.clear(), input.clear();
      }
    }
    return abstract_syntax_tree;
  }

  /******************************************************************************
  * REPL HELPER FUNCTIONS
  ******************************************************************************/

  // Account for whether REPL should print a newline
  void print_repl_newline(const bool& printed_data)noexcept{ // after printing data
    if(printed_data || (!G.LAST_PRINTED_NEWLINE_TO_STDOUT && G.LAST_PRINTED_TO_STDOUT))
      putchar('\n');
    G.LAST_PRINTED_NEWLINE_TO_STDOUT = G.LAST_PRINTED_TO_STDOUT = false;
  }


  void print_repl_newline()noexcept{ // after printing an error
    putchar('\n'), G.LAST_PRINTED_NEWLINE_TO_STDOUT=G.LAST_PRINTED_TO_STDOUT=false;
  }


  void account_for_whether_printed_data(const data& val,bool& printed_data)noexcept{
    printed_data = !val.is_type(types::dne);
  }


  // Print output object
  void user_print(FILE* outs, data& object) {
    fputs(object.pprint().c_str(), outs);
    fflush(outs);
  }


  // Determine if REPL received the EOF signal to terminate interpretation
  bool repl_detected_EOF_signal(const data_vector& AST)noexcept{
    return AST.size() == 1 && AST[0].is_type(types::chr) && AST[0].chr == EOF;
  }


  // Wrap each entry in "scm->cps" (w/ "id" bound as the topmost cont.) 
  //   if "-cps" cmd-line flag passed
  void cpsify_inputs(data_vector& AST) {
    const size_type n = AST.size();
    data_vector CPS_AST(n);
    for(size_type i = 0; i < n; ++i) {
      CPS_AST[i] = data_vector(2);
      CPS_AST[i].exp[0] = data_vector(2);
      CPS_AST[i].exp[0].exp[0] = symconst::scm_cps;
      CPS_AST[i].exp[0].exp[1] = AST[i];
      CPS_AST[i].exp[1] = "id";
    }
    AST = CPS_AST;
  }


  // Returns (begin (define #it <d>) #it)
  data repl_tag_expression(const data& d)noexcept{
    data tag_exp = data_vector(3);
    tag_exp.exp[0] = symconst::begin;
    tag_exp.exp[1] = data_vector(3);
    tag_exp.exp[1].exp[0] = symconst::define;
    tag_exp.exp[1].exp[1] = "#it";
    tag_exp.exp[1].exp[2] = d;
    tag_exp.exp[2] = "#it";
    return tag_exp;
  }

  /******************************************************************************
  * REPL EXECUTION
  ******************************************************************************/

  int launch_repl() {
    bool printed_data = true;
    print_repl_newline(printed_data);
    for(;;) {
      announce_input(stdout);
      auto AST = read_user_input(stdout,stdin); // AST = Abstract Syntax Tree
      // Handle EOF Signal
      if(repl_detected_EOF_signal(AST)) {
        puts("\nAdios!"); 
        return GLOBALS::HEIST_EXIT_CODE;
      }
      // Convert input to CPS as needed
      if(G.USING_CPS_CMD_LINE_FLAG) cpsify_inputs(AST);
      // Eval each datum given
      for(const auto& input : AST) {
        try {
          auto value = scm_eval(repl_tag_expression(input),G.GLOBAL_ENVIRONMENT_POINTER);
          account_for_whether_printed_data(value,printed_data);
          user_print(stdout, value);
          print_repl_newline(printed_data);
        } catch(const SCM_EXCEPT& eval_throw) {
          if(eval_throw == SCM_EXCEPT::EXIT) { 
            if(!GLOBALS::HEIST_EXIT_CODE) {
              puts("Adios!"); 
            } else {
              HEIST_PRINT_ERR("HEIST SCHEME REPL TERMINATION: EXIT FAILURE (" << GLOBALS::HEIST_EXIT_CODE << ")!");
              puts("");
            }
            return GLOBALS::HEIST_EXIT_CODE; 
          }
          if(eval_throw == SCM_EXCEPT::JUMP)
            HEIST_PRINT_ERR("Uncaught JUMP procedure! JUMPed value: " 
              << HEIST_PROFILE(GLOBALS::JUMP_GLOBAL_PRIMITIVE_ARGUMENT));
          print_repl_newline();
        } catch(...) {
          HEIST_PRINT_ERR("Uncaught C++ Exception Detected! -:- BUG ALERT -:-"
               "\n     Triggered By: " << input << 
               "\n  => Please send your code to jrandleman@scu.edu to fix"
               "\n     the interpreter's bug!"
               "\n  => Terminating Heist Scheme Interpretation.");
          return 1;
        }
      }
    }
  }
}

#endif