// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_types_toolkit.hpp
// => Defines string-serialization/equality/copying helper fcns for Scheme data
// => Must link at BOTTOM of "heist_types.hpp" to inherit type definitions

#ifndef HEIST_TYPES_TOOLKIT_HPP_
#define HEIST_TYPES_TOOLKIT_HPP_

namespace heist {

  /******************************************************************************
  * HELPER FUNCTION/TYPE PROTOTYPES
  ******************************************************************************/

  enum class list_status {ok, cyclic, no_null};

  list_status primitive_list_is_acyclic_and_null_terminated(const data& curr_pair)noexcept;
  scm_string  stack_trace_str         ()noexcept;
  sym_type    procedure_call_signature(const sym_type& name,const frame_vals& vals)noexcept;
  bool        is_delay                (const scm_list& exp)noexcept;
  bool        data_is_stream_pair     (const data& d)noexcept;
  bool        data_is_proper_list     (const data& d)noexcept;

  /******************************************************************************
  * ANSI ESCAPE SEQUENCE FORMATS & MACRO
  ******************************************************************************/

  enum afmts {
    AFMT_0 = 1, // clear
    AFMT_1,     // bold
    AFMT_31,    // red     (errors)
    AFMT_35,    // magenta (warnings)
    AFMT_131,   // bold red
    AFMT_135,   // bold magenta
    AFMT_01,    // clear bold
    AFMT_32,    // green
  };

  constexpr const char * const ansi_formats[] = {
    "",         "\x1b[0m",         "\x1b[1m",         "\x1b[31m",
    "\x1b[35m", "\x1b[1m\x1b[31m", "\x1b[1m\x1b[35m", "\x1b[0m\x1b[1m",
    "\x1b[32m", 
  };

  #define afmt(ansi_esc) heist::ansi_formats[heist::G::USING_ANSI_ESCAPE_SEQUENCES*ansi_esc]

  /******************************************************************************
  * ERROR HANDLING CODE ENUMERATIONS
  ******************************************************************************/

  enum class SCM_EXCEPT {EXIT, EVAL, READ, JUMP};

  /******************************************************************************
  * GLOBAL "JUMP!" PRIMITIVE ARGUMENT STORAGE
  ******************************************************************************/

  namespace G { data JUMP_GLOBAL_PRIMITIVE_ARGUMENT; } // see catch-jump & jump!

  /******************************************************************************
  * INTERNAL COMMON CONSTANT VALUES
  ******************************************************************************/

  namespace G {
    const auto FALSE_DATA_BOOLEAN    = data(boolean(false));
    const auto TRUE_DATA_BOOLEAN     = data(boolean(true));
    const auto VOID_DATA_OBJECT      = data(types::dne);
    const auto VOID_DATA_EXPRESSION  = scm_list(1,VOID_DATA_OBJECT);
    const auto EMPTY_LIST_EXPRESSION = scm_list(1,symconst::emptylist);
  } // End of namespace G

  /******************************************************************************
  * ERROR HANDLING PRINTER MACROS
  ******************************************************************************/

  #define ERR_HEADER '\n' << afmt(heist::AFMT_1) << __FILE__ << ':' << __func__ << \
                     ':' << __LINE__ << ':' << afmt(heist::AFMT_31) << \
                     " ERROR: \n" << afmt(heist::AFMT_01) << "  => "

  #define BAD_SYNTAX '\n'<<afmt(heist::AFMT_35)<<"  >> Invalid Syntax:"<<afmt(heist::AFMT_01)<<' '

  #define EXP_ERR(errExp)     BAD_SYNTAX << errExp
  #define FCN_ERR(fName,fVal) BAD_SYNTAX << heist::procedure_call_signature(fName,fVal)

  #define PROFILE(dataObj) dataObj<<" of type \""<<dataObj.type_name()<<'"'

  #define PRINT_ERR(...) std::cerr<<ERR_HEADER<<__VA_ARGS__<<'\n'<<heist::stack_trace_str()<<afmt(heist::AFMT_0)
  #define THROW_ERR(...) ({PRINT_ERR(__VA_ARGS__); throw heist::SCM_EXCEPT::EVAL;})

  /******************************************************************************
  * COMMAND-LINE FLAG SET
  ******************************************************************************/

  #define HEIST_COMMAND_LINE_ARGS\
      "> Interpret Script:    -script <script-filename> <argv1> <argv2> ..."\
    "\n> Compile Script:      -compile <script-filename> <optional-compiled-filename>"\
    "\n> Load Script:         -l <script-filename>"\
    "\n> With CPS Evaluation: -cps"\
    "\n> Disable ANSI Colors: -nansi"\
    "\n> Case Insensitivity:  -ci"\
    "\n> Dynamic Call Trace:  -dynamic-call-trace"\
    "\n> Trace Call Args:     -trace-args"\
    "\n> Stack Trace Size:    -trace-limit <non-negative-integer>"\
    "\n> Interpreter Version: --version"\
    "\n> Show This Message:   --help"

  /******************************************************************************
  * PLATFORM IDENTIFICATION
  ******************************************************************************/

  #if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__) || defined(_WIN64)
    #define HEIST_PLATFORM "windows"
    #ifdef _WIN64
      #define HEIST_EXACT_PLATFORM "windows-64"
    #else
      #define HEIST_EXACT_PLATFORM "windows-32"
    #endif
  #elif __APPLE__
    #define HEIST_PLATFORM "apple"
    #include <TargetConditionals.h>
    #if TARGET_IPHONE_SIMULATOR
      #define HEIST_EXACT_PLATFORM "apple-ios-simulator"
    #elif TARGET_OS_IPHONE
      #define HEIST_EXACT_PLATFORM "apple-ios"
    #elif TARGET_OS_MAC
      #define HEIST_EXACT_PLATFORM "apple-osx"
    #else
      #define HEIST_EXACT_PLATFORM "apple"
    #endif
  #elif __linux__
    #define HEIST_PLATFORM       "linux"
    #define HEIST_EXACT_PLATFORM "linux"
  #elif __unix__
    #define HEIST_PLATFORM       "unix"
    #define HEIST_EXACT_PLATFORM "unix"
  #elif defined(_POSIX_VERSION)
    #define HEIST_PLATFORM       "posix"
    #define HEIST_EXACT_PLATFORM "posix"
  #else
    #define HEIST_PLATFORM       "unknown"
    #define HEIST_EXACT_PLATFORM "unknown"
  #endif

  /******************************************************************************
  * STACK TRACE STRINGIFICATION HELPER FUNCTIONS
  ******************************************************************************/

  scm_string stack_trace_str() noexcept {
    if(G::STACK_TRACE.empty() || !G::TRACE_LIMIT) return "";
    scm_string trace(afmt(heist::AFMT_01));
    trace += afmt(heist::AFMT_35);
    trace += "  >> Stack Trace:";
    trace += afmt(heist::AFMT_01);
    auto end = G::STACK_TRACE.size() < G::TRACE_LIMIT ? G::STACK_TRACE.rend() : G::STACK_TRACE.rbegin() + G::TRACE_LIMIT;
    for(auto iter = G::STACK_TRACE.rbegin(); iter != end; ++iter)
      trace += "\n     " + *iter;
    G::STACK_TRACE.clear();
    return (trace + afmt(heist::AFMT_0)) + '\n';
  }

  /******************************************************************************
  * LIST PRINTING HELPER FUNCTIONS
  ******************************************************************************/

  // Prototype for printing helper function
  template<DATA_PRINTER to_str>
  void cio_list_str_recur(scm_string& list_str,const data& slow,const data& fast,par_type cycle_start);

  // Confirm data is not the empty list
  bool is_not_THE_EMPTY_LIST(const data& pair_data)noexcept{
    return (!pair_data.is_type(types::sym) || pair_data.sym!=symconst::emptylist);
  }


  // Stringify list recursive helper, ONLY for once the lists is confirmed to be acyclic
  template<DATA_PRINTER to_str>
  void cio_acyclic_list_str_recur(scm_string& list_str, const data& pair_object) {
    // store car
    if(pair_object.par->first.is_type(types::par)) {
      if(data_is_stream_pair(pair_object.par->first)) {
        list_str += "#<stream>";
      } else {
        list_str += '(';
        cio_list_str_recur<to_str>(list_str, pair_object.par->first, pair_object.par->first, nullptr);
        list_str += ')';
      }
    } else {
      list_str += (pair_object.par->first.*to_str)();
    }
    // store space if not last item in list
    if(is_not_THE_EMPTY_LIST(pair_object.par->second)) list_str += ' ';
    // store cdr
    if(pair_object.par->second.is_type(types::par)) {
      // check for whether at a cycle
      cio_acyclic_list_str_recur<to_str>(list_str, pair_object.par->second);
    } else if(is_not_THE_EMPTY_LIST(pair_object.par->second)){// don't store last '()
      // store ' . ' since not a null-terminated list
      list_str += ". " + (pair_object.par->second.*to_str)();
    }
  }


  // Stringify list recursive helper
  template<DATA_PRINTER to_str>
  void cio_list_str_recur(scm_string& list_str, const data& slow, const data& fast, 
                                                par_type cycle_start) {
    // Check if detected a cycle (simultaneously performs Floyd's Loop Detection algorithm)
    if(fast.is_type(types::par) && fast.par->second.is_type(types::par) && 
       slow.par == fast.par) { 
      auto fast_runner = fast;
      cycle_start = slow.par; // seek cycle's start
        while(cycle_start != fast_runner.par)
          cycle_start = cycle_start->second.par,
          fast_runner = fast_runner.par->second.par->second;
    }
    // store car
    if(slow.par->first.is_type(types::par)) {
      if(data_is_stream_pair(slow.par->first)) {
        list_str += "#<stream>";
      } else {
        list_str += '(';
        cio_list_str_recur<to_str>(list_str, slow.par->first, slow.par->first, nullptr);
        list_str += ')';
      }
    } else {
      list_str += (slow.par->first.*to_str)();
    }
    // store space if not last item in list
    if(is_not_THE_EMPTY_LIST(slow.par->second)) list_str += ' ';
    // store cdr
    if(slow.par->second.is_type(types::par)) {
      // check for whether at a cycle
      if(slow.par->second.par == cycle_start)
        list_str += "<...cycle>";
      else if(fast.is_type(types::par) && fast.par->second.is_type(types::par))
        cio_list_str_recur<to_str>(list_str, slow.par->second, fast.par->second.par->second, cycle_start);
      else
        cio_acyclic_list_str_recur<to_str>(list_str, slow.par->second);
    } else if(is_not_THE_EMPTY_LIST(slow.par->second)){// don't store last '()
      // store ' . ' since not a null-terminated list
      list_str += ". " + (slow.par->second.*to_str)();
    }
  }


  // Stringify list
  template<DATA_PRINTER to_str>
  scm_string cio_list_str(const data& pair_object) {
    if(data_is_stream_pair(pair_object)) return "#<stream>";
    scm_string list_str;
    cio_list_str_recur<to_str>(list_str, pair_object.par, pair_object.par, nullptr);
    return '(' + list_str + ')';
  }

  /******************************************************************************
  * VECTOR PRINTING HELPER FUNCTIONS
  ******************************************************************************/

  // Stringify vector
  template<DATA_PRINTER to_str>
  scm_string cio_vect_str(const vec_type& vector_object) {
    scm_string vect_str("#(");
    for(size_type i = 0, n = vector_object->size(); i < n; ++i) {
      if(vector_object->operator[](i).is_type(types::vec))
        vect_str += cio_vect_str<to_str>(vector_object->operator[](i).vec);
      else
        vect_str += (vector_object->operator[](i).*to_str)();
      if(i < n-1) vect_str +=  ' ';
    }
    return vect_str + ')';
  }

  /******************************************************************************
  * EXPRESSION PRINTING HELPER FUNCTIONS
  ******************************************************************************/

  // Confirm whether 'args' are the EVALUATED sentinel-value representation
  //   or is an otherwise empty expression
  bool no_args_given(const scm_list& args)noexcept{
    return args.empty() || (args.size()==1 && 
           args[0].is_type(types::sym) && 
           args[0].sym == symconst::sentinel_arg);
  }


  // Confirm whether 'd' is the AST's sentinel-value representation
  //   => NOTE: "sentinel-value" = quoted sentinel-arg
  bool data_is_the_SENTINEL_VAL(const data& d)noexcept{
    return d.is_type(types::exp) && d.exp.size() == 2 && 
           d.exp[0].is_type(types::sym) && 
           d.exp[0].sym == symconst::quote && 
           d.exp[1].is_type(types::sym) && 
           d.exp[1].sym == symconst::sentinel_arg;
  }


  // Confirm expression contains more data after 'd'
  template <typename const_scm_node>
  bool is_not_end_of_expression(const const_scm_node& d,const const_scm_node& end)noexcept{
    return d+1 != end && !data_is_the_SENTINEL_VAL(*(d+1));
  }


  // Handle appending delay expressions
  template <typename const_scm_node>
  bool exp_is_delay(const const_scm_node& d, const exp_type& exp_object,scm_string& exp_str)noexcept{
    // Ignore sentinel-arg expressions
    if(data_is_the_SENTINEL_VAL(*d)) {
      if(is_not_end_of_expression(d, exp_object.end())) exp_str += ' ';
      return true;
    }
    // Append delayed expressions as needed
    if(d->is_type(types::exp) && !d->exp.empty()) {
      if(is_delay(d->exp)) {
        exp_str += "#<delay>"; 
        if(is_not_end_of_expression(d, exp_object.end())) exp_str += ' ';
        return true;
      }
    }
    return false;
  }


  // Stringify expression recursive helper
  template<DATA_PRINTER to_str>
  void cio_expr_str_rec(const exp_type& exp_object, scm_string& exp_str) {
    if(no_args_given(exp_object)) return; // empty expression
    for(auto d = exp_object.begin(); d != exp_object.end(); ++d) {
      // Append delay & procedure expressions w/ a special format
      if(exp_is_delay(d, exp_object, exp_str)) 
        continue;
      // Recursively append expressions
      if(d->is_type(types::exp)) {
        exp_str += '(';
        cio_expr_str_rec<to_str>(d->exp, exp_str);
        exp_str += ')';
      // Append atomic data
      } else {
        exp_str += ((*d).*to_str)();
      }
      // Add a space if not at the end of the current expression
      if(d+1 != exp_object.end() && !data_is_the_SENTINEL_VAL(*(d+1))) 
        exp_str += ' ';
    }
  }


  // Stringify expression
  template<DATA_PRINTER to_str>
  scm_string cio_expr_str(const exp_type& exp_object) {
    // Sentinel Values are exclusively used internally, hence are never printed
    if(no_args_given(exp_object)||data_is_the_SENTINEL_VAL(data(exp_object))) return "";
    // Delayed expressions print identically to delayed values
    if(is_delay(exp_object)) return "#<delay>";
    scm_string exp_str;
    cio_expr_str_rec<to_str>(exp_object, exp_str);
    return '(' + exp_str + ')';
  }

  /******************************************************************************
  * HASH-TABLE PRINTING HELPER FUNCTIONS
  ******************************************************************************/

  // Stringify hash-map
  template<DATA_PRINTER to_str>
  scm_string cio_hmap_str(const map_type& map_object) {
    scm_string map_str("$(");
    for(const auto& keyval : map_object->val)
      map_str += (scm_map::unhash_key(keyval.first).*to_str)() + ' ' + (keyval.second.*to_str)() + ' ';
    if(map_str.size() > 2)
      *map_str.rbegin() = ')';
    else
      map_str.push_back(')');
    return map_str;
  }

  /******************************************************************************
  * PRETTY PRINTING HELPER FUNCTIONS
  ******************************************************************************/

  using pprint_data = std::vector<struct pprint_datum>;

  // pprint Datum Storing Stringified Data & Length of Stringified Data
  struct pprint_datum {
    // <output_len> Denotes length of <exp> or <datum_str> once ouput
    size_type output_len = 0;
    bool is_exp = false, is_sym = false;
    // Either a datum_str non-proper-list-pair or and <exp> of <pprint_datum>
    scm_string datum_str;
    pprint_data exp;
    pprint_datum()                      = default;
    pprint_datum(const pprint_datum& p) = default;
    pprint_datum(pprint_datum&& p)      = default;
    pprint_datum(scm_string&& str,const bool is_symbol=false) : output_len(str.size()),
                                                                is_sym(is_symbol),
                                                                datum_str(std::move(str)) {}
    pprint_datum(const pprint_data& e,const size_type& len) : output_len(len),is_exp(true),exp(e) {}
  };


  // Gets the length of <list_as_strs> once output
  void get_pprint_data_ouput_length(const pprint_data& list_as_strs,size_type& output_length)noexcept{
    // initial value accounts for outputting both parens & spaces between elts
    output_length = 2 + list_as_strs.size()-1;
    for(const auto& e : list_as_strs) output_length += e.output_len;
  }


  // Converts Scheme lists of data to an AST list of those data as strings
  // NOTE: the <size_type> of the pair denotes the length of the <data> once output 
  void stringify_list_data(pprint_data& list_as_strs, size_type& length, const par_type& p) {
    // Strify car
    if(!p->first.is_type(types::par)) {
      list_as_strs.push_back(pprint_datum(p->first.pprint(),p->first.is_type(types::sym)));
    } else if(!data_is_proper_list(p->first)) {
      list_as_strs.push_back(p->first.write());
    } else {
      pprint_data sub_exp;
      size_type sub_exp_len = 0;
      stringify_list_data(sub_exp,sub_exp_len,p->first.par);
      list_as_strs.push_back(pprint_datum(sub_exp,sub_exp_len));
    }
    // Strify cdr
    if(p->second.is_type(types::sym) && p->second.sym == symconst::emptylist) {
      get_pprint_data_ouput_length(list_as_strs,length); // get length of this stringified list as output
      return; // end of list
    }
    if(!p->second.is_type(types::par))
      list_as_strs.push_back(pprint_datum(p->second.pprint(),p->second.is_type(types::sym)));
    else if(!data_is_proper_list(p->second))
      list_as_strs.push_back(p->second.write());
    else
      stringify_list_data(list_as_strs,length,p->second.par);
  }


  // Stringifies <list_as_strs> w/o any tabs
  void print_pprint_data_as_is(const pprint_data& list_as_strs, scm_string& buffer)noexcept{
    buffer += '(';
    for(size_type i = 0, n = list_as_strs.size(); i < n; ++i) {
      if(list_as_strs[i].is_exp)
        print_pprint_data_as_is(list_as_strs[i].exp,buffer);
      else
        buffer += std::move(list_as_strs[i].datum_str);
      if(i+1 != n) buffer += ' ';
    }
    buffer += ')';
  }


  void pretty_print_pprint_data(const pprint_data&,const size_type&,const size_type&,scm_string&)noexcept;

  // Prints a list beginning w/ a non-symbol atom
  void pretty_print_list_of_data(const pprint_data& list_as_strs, const size_type& depth, 
                                                 scm_string& buffer, char* tabs)noexcept{
    tabs[2*depth+1] = 0; // shorten tabs to account for specialized printing
    for(size_type col_length = 2*depth, i = 0, n = list_as_strs.size(); i < n; ++i) {
      if(i && list_as_strs[i].output_len + col_length > G::PPRINT_MAX_COLUMN_WIDTH) {
        buffer += '\n';
        buffer += tabs;
        col_length = 2*depth;
      }
      col_length += list_as_strs[i].output_len + 1; // +1 accounts for spaces
      if(list_as_strs[i].is_exp)
        pretty_print_pprint_data(list_as_strs[i].exp,list_as_strs[i].output_len,depth+1,buffer);
      else
        buffer += std::move(list_as_strs[i].datum_str);
      if(i+1 != n) buffer += ' ';
    }
  }


  // Show info on the parsed stringified data
  void pretty_print_pprint_data(const pprint_data& list_as_strs, const size_type& len,
                                const size_type& depth, scm_string& buffer)noexcept{
    // Print as is if possible
    if(len + 2*depth <= G::PPRINT_MAX_COLUMN_WIDTH || len < 2) {
      print_pprint_data_as_is(list_as_strs,buffer);
      return;
    }
    // Get tab (2 spaces per tab) width as per the depth
    char* tabs = new char [2*depth+3];
    for(size_type i = 0, tabs_len = 2*depth+2; i < tabs_len; ++i) tabs[i] = ' ';
    tabs[2*depth+2] = 0;
    // Open paren
    buffer += '(';
    const size_type n = list_as_strs.size();
    size_type i = 1;
    // If 1st elt is a non-symbol atom, specialize printing
    if(!list_as_strs[0].is_sym && !list_as_strs[0].is_exp) {
      pretty_print_list_of_data(list_as_strs,depth,buffer,tabs);
      goto end_of_pprint;
    }
    // If 1st elt is a list, hence special printing case for such applications
    if(list_as_strs[0].is_exp) {
      pretty_print_pprint_data(list_as_strs[0].exp,list_as_strs[0].output_len,depth+1,buffer);
    } else {
      buffer += std::move(list_as_strs[0].datum_str) + ' ';
      // If 2nd elt printable on the current line (another special case)
      if(list_as_strs[1].output_len + list_as_strs[0].output_len + 2*depth < G::PPRINT_MAX_COLUMN_WIDTH){
        i = 2;
        if(list_as_strs[1].is_exp)
          print_pprint_data_as_is(list_as_strs[1].exp,buffer);
        else
          buffer += std::move(list_as_strs[1].datum_str);
      }
    }
    if(i < n) buffer += '\n';
    // Print body of the list
    for(; i < n; ++i) {
      buffer += tabs;
      if(list_as_strs[i].is_exp)
        pretty_print_pprint_data(list_as_strs[i].exp,list_as_strs[i].output_len,depth+1,buffer);
      else
        buffer += std::move(list_as_strs[i].datum_str);
      if(i+1 != n) buffer += '\n';
    }
    // Free <tabs> & add the closing paren
  end_of_pprint:
    delete [] tabs;
    buffer += ')';
  }


  // Pretty printer
  scm_string pretty_print(const data& d) {
    // If non-proper-list-pair, print as-is
    if(!data_is_proper_list(d)) return d.write();
    // Else check if pair as string is of valid length
    auto as_string = d.write();
    if(as_string.size() <= G::PPRINT_MAX_COLUMN_WIDTH)
      return as_string;
    // Otherwise get list as string-ified objects
    pprint_data list_as_strs;
    size_type output_length = 0;
    stringify_list_data(list_as_strs,output_length,d.par);
    // Print the list w/ indents
    scm_string buffer;
    pretty_print_pprint_data(list_as_strs,output_length,0,buffer);
    return buffer;
  }

  /******************************************************************************
  * OBJECT PRINTING HELPER (CHECKS IF OBJECT HAS A display write pprint MEMBER)
  ******************************************************************************/

  scm_list execute_application(data&,scm_list&,env_type& env=G::GLOBAL_ENVIRONMENT_POINTER,const bool tail_call=false,const bool inlined=false);
  scm_list execute_application(data&&,scm_list&,env_type& env=G::GLOBAL_ENVIRONMENT_POINTER,const bool tail_call=false,const bool inlined=false);
  data     extend_method_env_with_SELF_object(obj_type& calling_obj, scm_fcn& procedure)noexcept;
  data     data_cast(const scm_list& l)noexcept;
  data     data_cast(scm_list&& l)noexcept;

  data apply_dynamic_method(obj_type& obj, scm_list arg, scm_fcn procedure_cpy) {
    data procedure = extend_method_env_with_SELF_object(obj,procedure_cpy);
    env_type env = obj->proto->defn_env;
    return data_cast(execute_application(procedure,arg,env));
  }

  template<DATA_PRINTER to_str>
  scm_string cio_obj_str(const obj_type& object,const char* printer_name) {
    obj_type obj = object;
    while(obj) {
      // search object's local members
      for(size_type i = 0, n = obj->method_names.size(); i < n; ++i) {
        if(obj->method_names[i] == "self->string" || obj->method_names[i] == printer_name) {
          data result = apply_dynamic_method(obj,scm_list(1,symconst::sentinel_arg),obj->method_values[i].fcn);
          if(result.is_type(types::str)) return *result.str;
          return (result.*to_str)();
        }
      }
      // search object's prototype
      for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i) {
        if(obj->proto->method_names[i] == "self->string" || obj->proto->method_names[i] == printer_name) {
          obj->method_names.push_back(obj->proto->method_names[i]), obj->method_values.push_back(obj->proto->method_values[i]);
          data result = apply_dynamic_method(obj,scm_list(1,symconst::sentinel_arg),obj->method_values[i].fcn);
          if(result.is_type(types::str)) return *result.str;
          return (result.*to_str)();
        }
      }
      // search inherited object prototype
      obj = obj->inherited;
    }
    return "#<object[0x"+pointer_to_hexstring(object.ptr)+"]>";
  }

  /******************************************************************************
  * PAIR EQUALITY HELPERS (ENABLES COMPARING CIRCULAR LISTS)
  ******************************************************************************/

  bool new_cycle_detected(const data& slow, const data& fast, par_type cycle_start)noexcept{
    return !cycle_start && fast.is_type(types::par) && fast.par->second.is_type(types::par) && slow.par == fast.par;
  }


  void find_cycle_start(const data& slow, const data& fast, par_type& cycle_start)noexcept{
    auto fast_runner = fast;
    cycle_start = slow.par; // seek cycle's start
    while(cycle_start != fast_runner.par)
      cycle_start = cycle_start->second.par,
      fast_runner = fast_runner.par->second.par->second;
  }


  // Equality list recursive helper
  template<DATA_COMPARER same_as>
  bool list_equality_recur(const data& slow1, const data& fast1, const data& slow2, const data& fast2, 
                                                        par_type cycle_start1, par_type cycle_start2){
    // Confirm working with 2 pairs
    if(!slow1.is_type(types::par) || !slow2.is_type(types::par)) 
      return (slow1.*same_as)(slow2);
    // Confirm car elts are equal
    if(!(slow1.par->first.*same_as)(slow2.par->first)) 
      return false;
    // Confirm next 2 elts are pairs
    if(!slow1.par->second.is_type(types::par) || !slow2.par->second.is_type(types::par)) 
      return (slow1.par->second.*same_as)(slow2.par->second);
    // Check if detected a cycle (simultaneously performs Floyd's Loop Detection algorithm)
    if(new_cycle_detected(slow1,fast1,cycle_start1)) find_cycle_start(slow1,fast1,cycle_start1);
    if(new_cycle_detected(slow2,fast2,cycle_start2)) find_cycle_start(slow2,fast2,cycle_start2);
    // Check if at a cycle
    if(slow1.par->second.par == cycle_start1 || slow2.par->second.par == cycle_start2)
      return slow1.par->second.par == cycle_start1 && slow2.par->second.par == cycle_start2;
    // Check the rest of the list
    return list_equality_recur<same_as>(slow1.par->second, fast1.par->second.par->second, 
                                        slow2.par->second, fast2.par->second.par->second, 
                                        cycle_start1, cycle_start2);
  }


  template<DATA_COMPARER same_as>
  bool prm_compare_PAIRs(const par_type& p1, const par_type& p2) {
    return list_equality_recur<same_as>(p1,p1,p2,p2,nullptr,nullptr);
  }

  /******************************************************************************
  * DATA EQUALITY HELPERS
  ******************************************************************************/

  template<DATA_COMPARER same_as>
  bool prm_compare_atomic_values(const data& v1,const data& v2,const types& t) {
    switch(t) {
      case types::undefined: case types::dne: return true;
      case types::num: return v1.num.is_exact() == v2.num.is_exact() && v1.num == v2.num;
      case types::chr: return v1.chr == v2.chr;
      case types::str: return *v1.str == *v2.str;
      case types::sym: return v1.sym == v2.sym;
      case types::bol: return v1.bol.val == v2.bol.val;
      case types::map: return v1.map == v2.map;
      case types::cls: return v1.cls == v2.cls;
      case types::obj: return v1.obj == v2.obj;
      case types::par: return v1.par == v2.par;
      case types::vec: return v1.vec == v2.vec;
      case types::fcn: return v1.fcn == v2.fcn;
      case types::del: return v1.del == v2.del;
      case types::env: return v1.env == v2.env;
      case types::fip: return v1.fip.port_idx == v2.fip.port_idx;
      case types::fop: return v1.fop.port_idx == v2.fop.port_idx;
      case types::syn: return prm_compare_SNTXs<same_as>(v1.syn, v2.syn);
      case types::exp: return prm_compare_EXPRs<same_as>(v1.exp, v2.exp);
      default:         return false;
    }
  }


  template<DATA_COMPARER same_as>
  bool prm_compare_EXPRs(const scm_list& l1, const scm_list& l2) {
    if(l1.size() != l2.size()) return false;
    for(size_type i = 0, n = l1.size(); i < n; ++i)
      if(!(l1[i].*same_as)(l2[i])) return false;
    return true;
  }


  template<DATA_COMPARER same_as>
  bool prm_compare_VECTs(const vec_type& v1, const vec_type& v2) {
    return prm_compare_EXPRs<same_as>(*v1, *v2);
  }


  template<DATA_COMPARER same_as>
  bool prm_compare_SNTXs(const syn_type& s1, const syn_type& s2) {
    frame_var label;
    frame_vars keywords;
    std::vector<scm_list> patterns;
    std::vector<scm_list> templates;
    if(s1.label != s2.label || s1.patterns.size()  != s2.patterns.size()
                            || s1.keywords.size()  != s2.keywords.size()
                            || s1.templates.size() != s2.templates.size()){
      return false;
    }
    for(size_type i = 0, n = s1.keywords.size(); i < n; ++i)
      if(s1.keywords[i] != s2.keywords[i]) return false;
    for(size_type i = 0, n = s1.patterns.size(); i < n; ++i)
      if(!prm_compare_EXPRs<same_as>(s1.patterns[i], s2.patterns[i])) return false;
    for(size_type i = 0, n = s1.templates.size(); i < n; ++i)
      if(!prm_compare_EXPRs<same_as>(s1.templates[i], s2.templates[i])) return false;
    return true;
  }


  template<DATA_COMPARER same_as>
  bool prm_compare_HMAPs(const map_type& m1, const map_type& m2) {
    if(m1->val.size() != m2->val.size()) return false;
    for(auto p1=m1->val.begin(), end=m1->val.end(), p2=m2->val.begin(); p1 != end; ++p1, ++p2)
      if(p1->first != p2->first || !(p1->second.*same_as)(p2->second)) return false;
    return true;
  }


  template<DATA_COMPARER same_as>
  bool prm_compare_OBJs(const obj_type& o1, const obj_type& o2) {
    if(o1->proto != o2->proto                               ||
       o1->member_names.size()  != o2->member_names.size()  || 
       o1->member_values.size() != o2->member_values.size() || 
       o1->method_names.size()  != o2->method_names.size()  || 
       o1->method_values.size() != o2->method_values.size()) return false;
    for(size_type i = 0, n = o1->member_names.size(); i < n; ++i)
      if(o1->member_names[i] != o2->member_names[i]) return false;
    for(size_type i = 0, n = o1->method_names.size(); i < n; ++i)
      if(o1->method_names[i] != o2->method_names[i]) return false;
    for(size_type i = 0, n = o1->member_values.size(); i < n; ++i)
      if(!(o1->member_values[i].*same_as)(o2->member_values[i])) return false;
    for(size_type i = 0, n = o1->method_values.size(); i < n; ++i)
      if(!(o1->method_values[i].*same_as)(o2->method_values[i])) return false;
    return true;
  }


  // returns whether found a valid method
  bool prm_DYNAMIC_OBJeq(const obj_type& object,const data& rhs,const char* eq_name,bool& result) {
    // Search methods for the "self=" printing polymorphic method
    obj_type obj = object;
    while(obj) {
      // search object's local members
      for(size_type i = 0, n = obj->method_names.size(); i < n; ++i) {
        if(obj->method_names[i] == "self=" || obj->method_names[i] == eq_name) {
          data eq_result = apply_dynamic_method(obj,scm_list(1,rhs),obj->method_values[i].fcn);
          result = (eq_result.type != types::bol || eq_result.bol.val);
          return true;
        }
      }
      // search object's prototype
      for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i) {
        if(obj->proto->method_names[i] == "self=" || obj->proto->method_names[i] == eq_name) {
          obj->method_names.push_back(obj->proto->method_names[i]), obj->method_values.push_back(obj->proto->method_values[i]);
          data eq_result = apply_dynamic_method(obj,scm_list(1,rhs),obj->method_values[i].fcn);
          result = (eq_result.type != types::bol || eq_result.bol.val);
          return true;
        }
      }
      // search inherited object prototype
      obj = obj->inherited;
    }
    return false;
  }

  /******************************************************************************
  * DATA DEEP-LIST-COPYING HELPERS
  ******************************************************************************/

  void deep_copy_cycle_link(data& p, par_type& q, par_type& cycle_start) {
    q->first = p.par->first.copy();
    p = p.par->second;
    if(p.par != cycle_start) {
      q->second = par_type(scm_pair());
      q = q->second.par;
    }
  }


  void deep_copy_list_until_cycle_start(data& p, par_type& q, par_type& cycle_start) {
    while(p.par != cycle_start)
      deep_copy_cycle_link(p,q,cycle_start);
  }


  data deep_copy_circular_list(const data& d) {
    // find the start of the cycle
    data slow = d, fast = d;
    while(fast.is_type(types::par) && fast.par->second.is_type(types::par)) {
      slow = slow.par->second;             // move 1 node/iteration
      fast = fast.par->second.par->second; // move 2 nodes/iteration
      if(slow.par == fast.par) break;
    }
    // By now fast.par is where the cycle starts
    // -> Deep-Copy up to the cycle start, copy the cycle link, then copy again till cycle start
    data root = par_type(scm_pair());
    auto q = root.par;
    auto p = d;
    deep_copy_list_until_cycle_start(p,q,fast.par);
    auto cycle_start = q;
    deep_copy_cycle_link(p,q,fast.par); // deep-copy the cycle start
    deep_copy_list_until_cycle_start(p,q,fast.par);
    q->second = cycle_start;
    return root;
  }


  data deep_copy_non_circular_list(const data& d) {
    // note: guarenteed by data::deep_copy <d.type> ::= types::par
    auto p = d;
    data root = par_type(scm_pair());
    auto q = root.par;
    while(p.is_type(types::par)) {
      q->first = p.par->first.copy();
      p = p.par->second;
      if(p.is_type(types::par)) {
        q->second = par_type(scm_pair());
        q = q->second.par;
      }
    }
    q->second = p.copy();
    return root;
  }


  data deep_copy_pair(const data& d) {
    switch(primitive_list_is_acyclic_and_null_terminated(d)) {
      case list_status::ok: 
      case list_status::no_null: 
        return deep_copy_non_circular_list(d);
      default: return deep_copy_circular_list(d);
    }
  }

  /******************************************************************************
  * DATA DEEP-OBJECT-COPYING HELPER
  ******************************************************************************/

  // Search methods for the "self->copy" copying method
  bool dynamic_object_copy(obj_type obj, data& result) {
    // search object's local members
    for(size_type i = 0, n = obj->method_names.size(); i < n; ++i)
      if(obj->method_names[i] == "self->copy") {
        result = apply_dynamic_method(obj,scm_list(1,symconst::sentinel_arg),obj->method_values[i].fcn);
        return true;
      }
    // search object's prototype
    for(size_type i = 0, n = obj->proto->method_names.size(); i < n; ++i) {
      if(obj->proto->method_names[i] == "self->copy") {
        obj->method_names.push_back(obj->proto->method_names[i]), obj->method_values.push_back(obj->proto->method_values[i]);
        result = apply_dynamic_method(obj,scm_list(1,symconst::sentinel_arg),obj->method_values[i].fcn);
        return true;
      }
    }
    return false;
  }


  data deep_copy_obj(const data& d) {
    // deep copy inherited objects
    object_type o;
    o.proto = d.obj->proto; // shallow copy the prototype (these are never deep copied!)
    if(o.inherited) o.inherited = deep_copy_obj(make_obj(*o.inherited)).obj;
    // check for a custom self->copy
    data result;
    if(dynamic_object_copy(d.obj,result)) return result;
    // apply the default object copying mechanism
    o.member_names = d.obj->member_names;
    o.method_names = d.obj->method_names;
    for(const auto& member_val : d.obj->member_values)
      o.member_values.push_back(member_val.copy());
    for(const auto& method_val : d.obj->method_values)
      o.method_values.push_back(method_val.copy());
    return obj_type(std::move(o));
  }
} // End of namespace heist
#endif