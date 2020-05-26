// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_types_toolkit.hpp
// => Defines stringification helper fcns to print Scheme data w/ C-style IO
// => Must link at BOTTOM of "heist_types.hpp" to inherit type definitions

#ifndef HEIST_TYPES_TOOLKIT_HPP_
#define HEIST_TYPES_TOOLKIT_HPP_

/******************************************************************************
* PRINTING HELPER FUNCTION PROTOTYPES
******************************************************************************/

sym_type  procedure_call_signature(const sym_type& name,const frame_vals& vals)noexcept;
frame_var procedure_name          (const scm_list& p)noexcept;
bool      is_primitive_procedure  (const scm_list& p)noexcept;
bool      is_compound_procedure   (const scm_list& p)noexcept;
bool      is_delay                (const scm_list& exp)noexcept;
bool      no_args_given           (const scm_list& args)noexcept;
bool      data_is_stream_pair     (const data& d)noexcept;

/******************************************************************************
* ANSI ESCAPE SEQUENCE FORMATS & MACRO
******************************************************************************/

enum afmts {
  AFMT_0 = 1, // clear
  AFMT_1,     // bold
  AFMT_31,    // red
  AFMT_35,    // magenta
  AFMT_131,   // bold red
  AFMT_135,   // bold magenta
  AFMT_01,    // clear bold
};

constexpr const char * const ansi_formats[] = {
  "",         "\x1b[0m",         "\x1b[1m",         "\x1b[31m",
  "\x1b[35m", "\x1b[1m\x1b[31m", "\x1b[1m\x1b[35m", "\x1b[0m\x1b[1m",
};

#define afmt(ansi_esc) ansi_formats[USING_ANSI_ESCAPE_SEQUENCES*ansi_esc]

/******************************************************************************
* ERROR HANDLING CODE ENUMERATIONS
******************************************************************************/

enum class SCM_EXCEPT {EXIT, EVAL, READ};

/******************************************************************************
* ERROR HANDLING PRINTER MACROS
******************************************************************************/

#define ERR_HEADER '\n' << afmt(AFMT_1) << __FILE__ << ':' << __func__ << \
                   ':' << __LINE__ << ':' << afmt(AFMT_31) << \
                   " ERROR: \n" << afmt(AFMT_01) << "  => "

#define BAD_SYNTAX '\n'<<afmt(AFMT_35)<<"  >> Invalid Syntax:"<<afmt(AFMT_01)<<' '

#define EXP_ERR(errExp)     BAD_SYNTAX << errExp
#define FCN_ERR(fName,fVal) BAD_SYNTAX << procedure_call_signature(fName,fVal)

#define PROFILE(dataObj) dataObj<<" of type \""<<dataObj.type_name()<<'"'

#define PRINT_ERR(...) std::cerr<<ERR_HEADER<<__VA_ARGS__<<'\n'<<afmt(AFMT_0)
#define THROW_ERR(...) ({PRINT_ERR(__VA_ARGS__); throw SCM_EXCEPT::EVAL;})

/******************************************************************************
* LIST PRINTING HELPER FUNCTIONS
******************************************************************************/

// Performs Floyd's Loop Detection algorithm to prevent infinite list printing
par_type FLD(const data& curr_pair)noexcept{
  data slow = curr_pair, fast = curr_pair;
  while(fast.is_type(types::par) && fast.value.par->second.is_type(types::par)){
    slow = slow.value.par->second;                   // move 1 node/iteration
    fast = fast.value.par->second.value.par->second; // move 2 nodes/iteration
    if(slow.value.par == fast.value.par) break;
  }
  // if found end of the list, return nullptr
  if(!fast.is_type(types::par) || !fast.value.par->second.is_type(types::par))
    return nullptr;
  // seek the start of the cycle
  slow = curr_pair;
  while(slow.value.par != fast.value.par)
    slow = slow.value.par->second, 
    fast = fast.value.par->second.value.par->second;
  return slow.value.par;
}


// Confirm data is not the empty list
bool is_not_THE_EMPTY_LIST(const data& pair_data)noexcept{
  return (!pair_data.is_type(types::sym) || pair_data.value.sym!=THE_EMPTY_LIST);
}


// Stringify list recursive helper
void cio_list_str_recur(scm_string& list_str, const par_type& pair_object, 
                                              const par_type& cycle_start)noexcept{
  // store car
  if(pair_object->first.is_type(types::par)) {
    if(data_is_stream_pair(pair_object->first)) {
      list_str += "#<stream>";
    } else {
      list_str += '(';
      cio_list_str_recur(list_str, pair_object->first.value.par, FLD(pair_object->first));
      list_str += ')';
    }
  } else {
    list_str += pair_object->first.cio_str();
  }
  // store space if not last item in list
  if(is_not_THE_EMPTY_LIST(pair_object->second)) list_str += ' ';
  // store cdr
  if(pair_object->second.is_type(types::par)) {
    // check for whether at a cycle
    if(pair_object->second.value.par == cycle_start) {
      list_str += "<...cycle>";
    } else {
      cio_list_str_recur(list_str, pair_object->second.value.par, cycle_start);
    }
  } else if(is_not_THE_EMPTY_LIST(pair_object->second)){// don't store last '()
    // store ' . ' since not a null-terminated list
    list_str += ". " + pair_object->second.cio_str();
  }
}


// Stringify list
scm_string cio_list_str(const data& pair_object)noexcept{
  if(data_is_stream_pair(pair_object)) return "#<stream>";
  scm_string list_str;
  cio_list_str_recur(list_str, pair_object.value.par, FLD(pair_object));
  return '(' + list_str + ')';
}

/******************************************************************************
* VECTOR PRINTING HELPER FUNCTIONS
******************************************************************************/

// Stringify vector
scm_string cio_vect_str(const vec_type& vector_object)noexcept{
  scm_string vect_str("#(");
  for(size_type i = 0, n = vector_object->size(); i < n; ++i) {
    if(vector_object->operator[](i).is_type(types::vec))
      vect_str += cio_vect_str(vector_object->operator[](i).value.vec);
    else
      vect_str += vector_object->operator[](i).cio_str();
    if(i < n-1) vect_str +=  ' ';
  }
  return vect_str + ')';
}

/******************************************************************************
* EXPRESSION PRINTING HELPER FUNCTIONS
******************************************************************************/

// Confirm whether 'd' is the AST's sentinel-value representation
//   => NOTE: "sentinel-value" = quoted sentinel-arg
bool data_is_the_SENTINEL_VAL(const data& d)noexcept{
  return d.is_type(types::exp) && d.value.exp.size() == 2 && 
         d.value.exp[0].is_type(types::sym) && 
         d.value.exp[0].value.sym == symconst::quote && 
         d.value.exp[1].is_type(types::sym) && 
         d.value.exp[1].value.sym == SENTINEL_ARG;
}


// Confirm expression contains more data after 'd'
template <typename const_scm_node>
bool is_not_end_of_expression(const const_scm_node& d,const const_scm_node& end)noexcept{
  return d+1 != end && !data_is_the_SENTINEL_VAL(*(d+1));
}


// Handle appending delay & procedural expressions
template <typename const_scm_node>
bool exp_is_delay_or_procedure(const const_scm_node& d, const scm_list& exp_object,
                                                              scm_string& exp_str)noexcept{
  // Ignore sentinel-arg expressions
  if(data_is_the_SENTINEL_VAL(*d)) {
    if(is_not_end_of_expression(d, exp_object.end())) exp_str += ' ';
    return true;
  }
  // Append delayed/procedure expressions as needed
  if(d->is_type(types::exp) && !d->value.exp.empty()) {
    if(is_delay(d->value.exp)) {
      exp_str += "#<delay>"; 
      if(is_not_end_of_expression(d, exp_object.end())) exp_str += ' ';
      return true;
    }
    if(is_compound_procedure(d->value.exp) || is_primitive_procedure(d->value.exp)){ 
      exp_str += "#<procedure" + procedure_name(d->value.exp) + '>'; 
      if(is_not_end_of_expression(d, exp_object.end())) exp_str += ' ';
      return true;
    }
  }
  return false;
}


// Stringify expression recursive helper
void cio_expr_str_rec(const scm_list& exp_object, scm_string& exp_str)noexcept{
  if(no_args_given(exp_object)) return; // empty expression
  for(auto d = exp_object.begin(); d != exp_object.end(); ++d) {
    // Append delay & procedure expressions w/ a special format
    if(exp_is_delay_or_procedure(d, exp_object, exp_str)) 
      continue;
    // Recursively append expressions
    if(d->is_type(types::exp)) {
      exp_str += '(';
      cio_expr_str_rec(d->value.exp, exp_str);
      exp_str += ')';
    // Append atomic data
    } else {
      exp_str += d->cio_str();
    }
    // Add a space if not at the end of the current expression
    if(d+1 != exp_object.end() && !data_is_the_SENTINEL_VAL(*(d+1))) 
      exp_str += ' ';
  }
}


// Stringify expression
scm_string cio_expr_str(const scm_list& exp_object)noexcept{
  // Sentinel Values are exclusively used internally, hence are never printed
  if(no_args_given(exp_object)||data_is_the_SENTINEL_VAL(data(exp_object)))
    return "";
  // Delayed expressions print identically to delayed values
  if(is_delay(exp_object)) 
    return "#<delay>";
  if(is_compound_procedure(exp_object) || is_primitive_procedure(exp_object))
    return "#<procedure" + procedure_name(exp_object) + '>';
  scm_string exp_str;
  cio_expr_str_rec(exp_object, exp_str);
  return '(' + exp_str + ')';
}
#endif
