// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_types_toolkit.hpp
// => Defines stringification helper fcns to print Scheme data w/ C-style IO
// => Must link at BOTTOM of "heist_types.hpp" to inherit type definitions

#ifndef HEIST_TYPES_TOOLKIT_HPP_
#define HEIST_TYPES_TOOLKIT_HPP_

namespace heist {

  /******************************************************************************
  * PRINTING HELPER FUNCTION PROTOTYPES
  ******************************************************************************/

  sym_type  procedure_call_signature(const sym_type& name,const frame_vals& vals)noexcept;
  frame_var procedure_name          (const scm_list& p)noexcept;
  bool      is_primitive_procedure  (const scm_list& p)noexcept;
  bool      is_compound_procedure   (const scm_list& p)noexcept;
  bool      is_delay                (const scm_list& exp)noexcept;
  bool      data_is_stream_pair     (const data& d)noexcept;
  bool      data_is_proper_list     (const data& d)noexcept;

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
  * ERROR HANDLING PRINTER MACROS
  ******************************************************************************/

  #define ERR_HEADER '\n' << afmt(heist::AFMT_1) << __FILE__ << ':' << __func__ << \
                     ':' << __LINE__ << ':' << afmt(heist::AFMT_31) << \
                     " ERROR: \n" << afmt(heist::AFMT_01) << "  => "

  #define BAD_SYNTAX '\n'<<afmt(heist::AFMT_35)<<"  >> Invalid Syntax:"<<afmt(heist::AFMT_01)<<' '

  #define EXP_ERR(errExp)     BAD_SYNTAX << errExp
  #define FCN_ERR(fName,fVal) BAD_SYNTAX << heist::procedure_call_signature(fName,fVal)

  #define PROFILE(dataObj) dataObj<<" of type \""<<dataObj.type_name()<<'"'

  #define PRINT_ERR(...) std::cerr<<ERR_HEADER<<__VA_ARGS__<<'\n'<<afmt(heist::AFMT_0)
  #define THROW_ERR(...) ({PRINT_ERR(__VA_ARGS__); throw heist::SCM_EXCEPT::EVAL;})

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
    #define HEIST_PLATFORM       "#f"
    #define HEIST_EXACT_PLATFORM "#f"
  #endif

  /******************************************************************************
  * LIST PRINTING HELPER FUNCTIONS
  ******************************************************************************/

  // Prototype for printing helper function
  template<DATA_PRINTER to_str>
  void cio_list_str_recur(scm_string& list_str,const data& slow,
                          const data& fast,par_type cycle_start)noexcept;

  // Confirm data is not the empty list
  bool is_not_THE_EMPTY_LIST(const data& pair_data)noexcept{
    return (!pair_data.is_type(types::sym) || pair_data.sym!=symconst::emptylist);
  }


  // Stringify list recursive helper, ONLY for once the lists is confirmed to be acyclic
  template<DATA_PRINTER to_str>
  void cio_acyclic_list_str_recur(scm_string& list_str, const data& pair_object)noexcept{
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
                                                par_type cycle_start)noexcept{
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
  scm_string cio_list_str(const data& pair_object)noexcept{
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
  scm_string cio_vect_str(const vec_type& vector_object)noexcept{
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


  // Handle appending delay & procedural expressions
  template <typename const_scm_node>
  bool exp_is_delay_or_procedure(const const_scm_node& d, const exp_type& exp_object,
                                                                scm_string& exp_str)noexcept{
    // Ignore sentinel-arg expressions
    if(data_is_the_SENTINEL_VAL(*d)) {
      if(is_not_end_of_expression(d, exp_object.end())) exp_str += ' ';
      return true;
    }
    // Append delayed/procedure expressions as needed
    if(d->is_type(types::exp) && !d->exp.empty()) {
      if(is_delay(d->exp)) {
        exp_str += "#<delay>"; 
        if(is_not_end_of_expression(d, exp_object.end())) exp_str += ' ';
        return true;
      }
      if(is_compound_procedure(d->exp) || is_primitive_procedure(d->exp)){ 
        exp_str += "#<procedure" + procedure_name(d->exp) + '>'; 
        if(is_not_end_of_expression(d, exp_object.end())) exp_str += ' ';
        return true;
      }
    }
    return false;
  }


  // Stringify expression recursive helper
  template<DATA_PRINTER to_str>
  void cio_expr_str_rec(const exp_type& exp_object, scm_string& exp_str)noexcept{
    if(no_args_given(exp_object)) return; // empty expression
    for(auto d = exp_object.begin(); d != exp_object.end(); ++d) {
      // Append delay & procedure expressions w/ a special format
      if(exp_is_delay_or_procedure(d, exp_object, exp_str)) 
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
  scm_string cio_expr_str(const exp_type& exp_object)noexcept{
    // Sentinel Values are exclusively used internally, hence are never printed
    if(no_args_given(exp_object)||data_is_the_SENTINEL_VAL(data(exp_object)))
      return "";
    // Delayed expressions print identically to delayed values
    if(is_delay(exp_object)) 
      return "#<delay>";
    if(is_compound_procedure(exp_object) || is_primitive_procedure(exp_object))
      return "#<procedure" + procedure_name(exp_object) + '>';
    scm_string exp_str;
    cio_expr_str_rec<to_str>(exp_object, exp_str);
    return '(' + exp_str + ')';
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
  void stringify_list_data(pprint_data& list_as_strs, size_type& length, const par_type& p)noexcept{
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
  scm_string pretty_print(const data& d)noexcept{
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
} // End of namespace heist
#endif