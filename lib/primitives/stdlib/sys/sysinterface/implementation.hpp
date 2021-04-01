// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for sysinterface.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_SYSINTERFACE_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_SYSINTERFACE_IMPLEMENTATION_HPP_


// Initialize G.GLOBAL_ENVIRONMENT_POINTER
namespace heist { void set_default_global_environment(); } // from "heist.cpp"


namespace heist::stdlib_sysinterface {

  /******************************************************************************
  * GLOBAL PROCESS INVARIANTS RESET & SET ABSTRACTIONS
  ******************************************************************************/

  // Saves the current <G> value, then resets it to its default values.
  process_invariants_t reset_process_invariant_state() {
    process_invariants_t tmp(std::move(G));
    G = process_invariants_t();
    set_default_global_environment();
    return tmp;
  }


  // Sets the <G> value to the given invariants set.
  void set_process_invariant_state(process_invariants_t&& invariants)noexcept{
    G = std::move(invariants);
  }

  /******************************************************************************
  * INPUT FILE VALIDATION
  ******************************************************************************/

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

  /******************************************************************************
  * LOAD
  ******************************************************************************/

  void interpret_file_contents(data_vector& args, env_type& env, const char* format){
    // Load file contents
    if(args.size() != 1)
      HEIST_THROW_ERR("'load received incorrect # of arguments!" << format << HEIST_FCN_ERR("load", args));
    FILE* ins = confirm_valid_input_file(args[0],"load",format,args);
    FILE* outs = primitive_toolkit::get_current_output_port(args, "load", format);
    size_type exp_count = 1;
    while(!feof(ins)) {
      // Try reading & evaluating an expression
      try {
        scm_eval(std::move(stdlib_input::read_from_port(outs,ins)[0]),env);
        ++exp_count;
      // Catch, notify 'load' error occurred, & rethrow
      } catch(const SCM_EXCEPT& load_error) {
        if(load_error == SCM_EXCEPT::EVAL) {
          fprintf(stderr, "  %s>> Load Exception:%s\n     File \"%s\"\n     Expression #%zu\n%s", 
            HEIST_AFMT(AFMT_135), HEIST_AFMT(AFMT_01), args[0].str->c_str(), exp_count, HEIST_AFMT(AFMT_0));
        } else if(load_error == SCM_EXCEPT::READ) {
          fprintf(stderr, "%s>> Load Exception:%s\n   File \"%s\"\n   Expression #%zu\n%s", 
            HEIST_AFMT(AFMT_135), HEIST_AFMT(AFMT_01), args[0].str->c_str(), exp_count, HEIST_AFMT(AFMT_0));
        }
        if(ins) fclose(ins);
        throw load_error;
      }
    }
    if(ins) fclose(ins);
  }

  /******************************************************************************
  * CPS-LOAD
  ******************************************************************************/

  bool cps_load_last_obj_is_EOF(const data_vector& AST)noexcept{
    return !AST.empty() && 
      ((AST.rbegin()->is_type(types::exp) && AST.rbegin()->exp.size()==1 && 
        AST.rbegin()->exp[0].is_type(types::chr) && AST.rbegin()->exp[0].chr==EOF) ||
       (AST.rbegin()->is_type(types::chr) && AST.rbegin()->chr==EOF));
  }


  data_vector generate_CPS_LOAD_args(const data& continuation)noexcept{
    // adds <id> to account for extra continuation slot added by <generate_fundamental_form_cps>
    data_vector cps_load_arg(2);
    cps_load_arg[0] = continuation;
    cps_load_arg[1] = fcn_type("id",DEFAULT_TOPMOST_CONTINUATION::id); // from /lib/core/type_system/scheme_types/functions/implmentation.hpp
    return cps_load_arg;
  }


  data cps_interpret_file_contents(data_vector& args, env_type& env, const char* format){
    // Load file contents
    if(args.size() != 1)
      HEIST_THROW_ERR("'cps-load received incorrect # of arguments!" << format << HEIST_FCN_ERR("cps-load", args));
    FILE* ins = confirm_valid_input_file(args[0],"cps-load",format,args);
    FILE* outs = primitive_toolkit::get_current_output_port(args, "cps-load", format);
    size_type exp_count = 1;
    data AST = data_vector();
    while(!feof(ins)) {
      // Try reading & evaluating an expression
      try {
        AST.exp.push_back(stdlib_input::read_from_port(outs,ins)[0]);
        ++exp_count;
      // Catch, notify 'CPS-load' error occurred, & rethrow
      } catch(const SCM_EXCEPT& load_error) {
        if(load_error == SCM_EXCEPT::READ) {
          fprintf(stderr, "%s>> CPS-Load Exception:%s\n   File \"%s\"\n   Expression #%zu\n%s", 
            HEIST_AFMT(AFMT_135), HEIST_AFMT(AFMT_01), args[0].str->c_str(), exp_count, HEIST_AFMT(AFMT_0));
        }
        if(ins) fclose(ins);
        throw load_error;
      }
    }
    if(ins) fclose(ins);
    // Rm #!eof as last obj in AST (if present)
    if(cps_load_last_obj_is_EOF(AST.exp)) AST.exp.pop_back();
    // Add (void) as only obj iff AST.empty()
    if(AST.exp.empty()) AST.exp.push_back(data_vector(1,"void"));
    // Wrap in scm->cps
    AST.exp.insert(AST.exp.begin(), symconst::scm_cps);
    return scm_eval(std::move(AST),env);
  }

  /******************************************************************************
  * COMPILATION HELPERS
  ******************************************************************************/

  string convert_char_to_cpp_literal(char c) {
    if(c == '\'') return "\\'";
    if(c == '"') return "\"";
    data d(make_str(string(1,c)));
    auto str = d.write();
    str.pop_back();       // disregard closing "
    return str.substr(1); // disregard opening "
  }

  // Recursively generate assignments to vectors as a precomputed AST
  //   => NOTE: The reader's generated AST _ONLY_ contains 1 of 5 types:
  //            types::exp, types::str, types::sym, types::chr, & types::num
  void print_vector_data_assignment(const data_vector& expressions, string& vector_assigns, 
                                    const string& assignment_chain)noexcept{
    for(size_type i = 0, n = expressions.size(); i < n; ++i) {
      switch(expressions[i].type) {
        case types::exp: // EXPRESSION
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = heist::exp_type("+
            std::to_string(expressions[i].exp.size())+");\n";
          print_vector_data_assignment(expressions[i].exp,vector_assigns,
            assignment_chain+'['+std::to_string(i)+"].exp");
          break;
        case types::str: // STRING
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = heist::make_str(\""+ 
            escape_chars(*expressions[i].str) +"\");\n";
          break;
        case types::sym: // SYMBOL
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = \""+
            expressions[i].sym+"\";\n"; 
          break;
        case types::chr: // CHARACTER
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = heist::chr_type('"+
            convert_char_to_cpp_literal(expressions[i].chr)+"');\n"; 
          break;
        default:         // NUMBER
          vector_assigns += assignment_chain+'['+std::to_string(i)+"] = heist::num_type("+
            expressions[i].num.str()+");\n";
      }
    }
  }


  void read_file_being_compiled(data_vector& args, data_vector& expressions, const char* name, const char* format){
    FILE* ins = confirm_valid_input_file(args[0],name,format,args);
    FILE* outs = primitive_toolkit::get_current_output_port(args, name, format);
    size_type exp_count = 1;
    while(!feof(ins)) {
      // Try reading an expression
      try {
        expressions.push_back(stdlib_input::read_from_port(outs,ins)[0]);
        ++exp_count;
      // Catch, notify 'compile' error occurred, & rethrow
      } catch(const SCM_EXCEPT& compile_error) {
        if(compile_error == SCM_EXCEPT::READ)
          fprintf(stderr, "%s>> Compile Exception:%s\n   File \"%s\"\n   Expression #%zu\n%s", 
            HEIST_AFMT(AFMT_135), HEIST_AFMT(AFMT_01), args[0].str->c_str(), exp_count, HEIST_AFMT(AFMT_0));
        if(ins) fclose(ins);
        throw compile_error;
      }
    }
    expressions.pop_back(); // rm EOF character (not part of the source code)
    if(ins) fclose(ins);
  }


  string generate_precompiled_AST(data_vector& expressions)noexcept{
    // Generate Vector Assignments to explicitly lay out a predetermined AST
    string ast_generator = "heist::exp_type HEIST_PRECOMPILED_READ_AST_EXPS(" + 
                                std::to_string(expressions.size()) + ");\n"
                                "void POPULATE_HEIST_PRECOMPILED_READ_AST_EXPS(){\n";
    print_vector_data_assignment(expressions,ast_generator,"HEIST_PRECOMPILED_READ_AST_EXPS");
    return ast_generator + "}\n";
  }


  data write_compiled_file(data_vector& args, const string& compiled_filename, const string& ast_generator, const char* name){
    FILE* outs = fopen(compiled_filename.c_str(), "w");
    if(outs == nullptr)
      HEIST_THROW_ERR('\''<<name<<" file \"" << compiled_filename 
        << "\" couldn't be written to!\n     ("<<name<<" <filename-string>)"
        << HEIST_FCN_ERR(name,args));
    fprintf(outs, "// Heist-Scheme Compiled Source from \"%s\""
                  "\n#include \"%s%clib%ccore%ctype_system%ctypes.hpp\""
                  "\n#define HEIST_INTERPRETING_COMPILED_AST"
                  "\n%s"
                  "\n#include \"%s%cheist.cpp\"\n", 
                  args[0].str->c_str(), 
                  HEIST_DIRECTORY_FILE_PATH,char(std::filesystem::path::preferred_separator),
                  char(std::filesystem::path::preferred_separator),
                  char(std::filesystem::path::preferred_separator),
                  char(std::filesystem::path::preferred_separator),
                  ast_generator.c_str(), 
                  HEIST_DIRECTORY_FILE_PATH,char(std::filesystem::path::preferred_separator));
    if(outs) fclose(outs);
    return GLOBALS::VOID_DATA_OBJECT;
  }


  // Generates a custom or default named compiled file as needed
  data compile_dispatch(data_vector& args, data_vector& expressions, const char* name) {
    if(args.size() == 2)
      return write_compiled_file(args,*args[1].str,generate_precompiled_AST(expressions),name);
    return write_compiled_file(args,symconst::dflt_compile_name,generate_precompiled_AST(expressions),name);
  }


  // General Layout for Compilation Primitives (For Both CPS-Style & Not)
  data generic_compilation(data_vector& args,const char* name,const char* format,const bool cps_style){
    // Read File & Generate an AST-construction string
    if(args.empty() || args.size() > 2) 
      HEIST_THROW_ERR('\''<<name<<" received incorrect # of args!"
        "\n     ("<<name<<" <filename-string> <optional-compiled-filename>)"<<HEIST_FCN_ERR(name,args));
    if(args.size() == 2 && !args[1].is_type(types::str))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" wasn't a string!"
        "\n     ("<<name<<" <filename-string> <optional-compiled-filename>)"<<HEIST_FCN_ERR(name,args));
    data_vector expressions;
    read_file_being_compiled(args,expressions,name,format);
    if(cps_style) {
      data_vector wrapped_exps(1);
      wrapped_exps[0] = data_vector(2);
      wrapped_exps[0].exp[0] = data_vector(1+expressions.size());
      wrapped_exps[0].exp[0].exp[0] = symconst::scm_cps;
      std::move(expressions.begin(),expressions.end(),wrapped_exps[0].exp[0].exp.begin()+1);
      wrapped_exps[0].exp[1] = "id";
      return compile_dispatch(args,wrapped_exps,name);
    }
    return compile_dispatch(args,expressions,name);
  }

  /******************************************************************************
  * CURRENT TIME & DATE HELPERS
  ******************************************************************************/

  long double convert_us_to_s(const long double& us_duration)noexcept{
    return (std::size_t)(us_duration * 0.001L) / 1E3L;
  }


  string get_current_time_stamp(long long s=0, long long m=0, long long h=0, long long d=0, long long y=0) {
    std::chrono::duration<long long,std::ratio<60*60*24*365>> yr(y);
    std::chrono::duration<long long,std::ratio<60*60*24>> day(d);
    std::chrono::duration<long long,std::ratio<60*60>> hr(h);
    std::chrono::duration<long long,std::ratio<60>> min(m);
    std::chrono::duration<long long,std::ratio<1>> sec(s);
    time_t tt = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()+yr+day+hr+min+sec);
    string date_str = ctime(&tt);
    if(*date_str.rbegin() == '\n') date_str.pop_back();
    return date_str;
  }


  enum class CURRENT_DATE_OFFSET_T {sec, min, hr, day, yr, invalid};

  CURRENT_DATE_OFFSET_T get_offset_date_type(const string& unit) {
    if(unit == "sec")  return CURRENT_DATE_OFFSET_T::sec;
    if(unit == "min")  return CURRENT_DATE_OFFSET_T::min;
    if(unit == "hour") return CURRENT_DATE_OFFSET_T::hr;
    if(unit == "day")  return CURRENT_DATE_OFFSET_T::day;
    if(unit == "year") return CURRENT_DATE_OFFSET_T::yr;
    return CURRENT_DATE_OFFSET_T::invalid;
  }


  void throw_current_date_error(data_vector& args, data& elt, const char* message) {
    static constexpr const char * const format = 
      "\n     (current-date <optional-offset> ...)"
      "\n     => <optional-offset> ::= (<symbolic-unit> <integer-amount>)"
      "\n     => <symbolic-unit> ::= sec | min | hour | day | year";
    static constexpr const auto MAX_LL = std::numeric_limits<long long>::max();
    static constexpr const auto MIN_LL = std::numeric_limits<long long>::min();
    HEIST_THROW_ERR("'current-date offset arg " << HEIST_PROFILE(elt) 
      << message << format << "\n     => <offset> ::= [" << MIN_LL << ", " 
      << MAX_LL << ']' << HEIST_FCN_ERR("current-date", args));
  }


  void parse_current_date_offsets(data_vector& args,long long& s,long long& m,long long& h,long long& d,long long& y){
    static constexpr const auto MAX_LL = std::numeric_limits<long long>::max();
    static constexpr const auto MIN_LL = std::numeric_limits<long long>::min();
    for(auto& elt : args) {
      if(!elt.is_type(types::par) || !elt.par->second.is_type(types::par))
        throw_current_date_error(args,elt," isn't a symbol-number list!");
      if(!elt.par->first.is_type(types::sym))
        throw_current_date_error(args,elt," 1st elt isn't a symbolic-unit!");
      if(!elt.par->second.par->first.is_type(types::num) || !elt.par->second.par->first.num.is_integer())
        throw_current_date_error(args,elt," 2nd elt isn't an integer offset!");
      auto stat = get_offset_date_type(elt.par->first.sym);
      auto offs = elt.par->second.par->first.num.extract_inexact();
      if(offs > MAX_LL || offs < MIN_LL)
        throw_current_date_error(args,elt," 2nd elt exceeds offset bounds!");
      switch(stat) {
        case CURRENT_DATE_OFFSET_T::sec: s = (long long)offs; break;
        case CURRENT_DATE_OFFSET_T::min: m = (long long)offs; break;
        case CURRENT_DATE_OFFSET_T::hr:  h = (long long)offs; break;
        case CURRENT_DATE_OFFSET_T::day: d = (long long)offs; break;
        case CURRENT_DATE_OFFSET_T::yr:  y = (long long)offs; break;
        default: // CURRENT_DATE_OFFSET_T::invalid
          throw_current_date_error(args,elt," 1st elt is an invalid offset symbol!");
      }
    }
  }

} // End of namespace heist::stdlib_sysinterface

#endif