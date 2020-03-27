// Author: Jordan Randleman -- scm_eval_types.hpp
// => Contains type aliases for the C++ Scheme Interpreter

#ifndef SCM_EVAL_TYPES_HPP_
#define SCM_EVAL_TYPES_HPP_

#include <functional>
#include <memory>
#include <tuple>
#include "scm_eval_numerics.hpp"

#define ERR_HEADER "\n\x1b[1m" + std::string(__FILE__) + ":" +\
                                 std::string(__func__) + ":" +\
                                 std::to_string(__LINE__) +\
                                 ":\x1b[31m ERROR: \n\x1b[0m\x1b[1m  => "
#define PRINT_ERR(...) std::cerr << ERR_HEADER << __VA_ARGS__ << "\x1b[0m\n"
#define FATAL_ERR(...) ({PRINT_ERR(__VA_ARGS__); throw EXIT_FAILURE;})

using scm_list   = std::vector<struct data>;          // scheme expression list
using scm_node   = scm_list::iterator;                // scheme expression node
using scm_pair   = std::pair<struct data,struct data>;// scheme pair
using scm_string = std::string;                       // string type
using size_type  = std::size_t;                       // numeric type, idxs etc
using signed_num = long long;                         // negative numerics

/******************************************************************************
* MAX NUMBER OF RECURSIVE CALLS
******************************************************************************/

size_type MAX_RECURSIVE_DEPTH = 1000; // see set-max-recursion-depth! primitive

/******************************************************************************
* MAX VALUE FOR SIZE_TYPE
******************************************************************************/

constexpr const auto MAX_SIZE_TYPE = std::numeric_limits<size_type>::max();

/******************************************************************************
* RESERVED VOID ARG NAME, EMPTY-LIST VALUE, & DO-ITERATION KEYWORDS
******************************************************************************/

// NOTE: THE FOLLOWING SYMBOLS ARE RESERVED!

constexpr const char * const SENTINEL_ARG            = "SCM_EVAL-NIL-ARG"; 
constexpr const char * const DO_LETREC_INSTANCE_NAME = "SCM_EVAL-DO-LETREC";
constexpr const char * const PROCEDURE_TAG           = "SCM_EVAL-PROCEDURE";
constexpr const char * const PRIMITIVE_TAG           = "SCM_EVAL-PRIMITIVE";
constexpr const char * const THE_EMPTY_LIST          = "";

/******************************************************************************
* ENVIRONMENT DATA STRUCTURE TYPE ALIASES
******************************************************************************/

// ENVIRONMENTS AS VECTOR OF FRAMES, [i+1] = ENCLOSING ENVIRONMENT OF [i]
//    => FRAMES AS TUPLE OF VECTORS: VARIABLE NAMES & VALUES, MACRO DEFNS

using frame_var   = std::string;
using frame_val   = struct data;
using frame_mac   = struct scm_macro;
using frame_vars  = std::vector<frame_var>;
using frame_vals  = std::vector<frame_val>;
using frame_macs  = std::vector<frame_mac>;
using frame_t     = std::tuple<frame_vars,frame_vals,frame_macs>;
using frame_ptr   = std::shared_ptr<frame_t>;
using environment = std::vector<frame_ptr>;

// FRAME CONSTRUCTOR
auto make_frame = std::make_shared<frame_t,frame_t>;

/******************************************************************************
* DATA TYPE ALIASES, CONSTRUCTORS, & STRUCTS
******************************************************************************/

using exp_type = scm_list;                           // expression
using par_type = std::shared_ptr<scm_pair>;          // pair
using num_type = Scm_numeric;                        // number (float/int/frac)
using str_type = std::shared_ptr<scm_string>;        // string
using chr_type = char;                               // character
using sym_type = scm_string;                         // symbol
using vec_type = std::shared_ptr<scm_list>;          // vector
using bol_type = struct boolean;                     // boolean
using env_type = std::shared_ptr<environment>;       // evironment
using del_type = std::shared_ptr<struct delay_data>; // delay
using prm_type = struct data(*)(scm_list&);          // primitive procedure ptr
using exe_type = std::function<scm_list(env_type&)>; // fcn execution procedure
using cal_type = std::shared_ptr<size_type>;         // recursive depth counter

auto make_par = std::make_shared<scm_pair>;
auto make_str = std::make_shared<scm_string,const scm_string&>;
auto make_vec = std::make_shared<scm_list,const scm_list&>;
auto make_env = std::make_shared<environment>;
auto make_del = std::make_shared<struct delay_data,const scm_list&,const env_type&>;
auto make_cal = std::make_shared<size_type,size_type>;

// helper functions to recursively print scheme pairs & vectors
void print_list(std::ostream& outs, const par_type& pair_object);
void print_vect(std::ostream& outs, const vec_type& vector_object);

// enum of "struct data.value" types
// => expression, pair, number, string, character, symbol, vector, boolean, environment, 
//    delay, primitive fcn ptr, execution procedure, recursive depth counter, undefined value
enum class types {exp, par, num, str, chr, sym, vec, bol, env, del, prm, exe, cal, undefined};

// boolean struct (differentiates from 'number')
struct boolean {
  bool val; 
  boolean(bool b=false) : val(b) {}
  void operator=(const bool b){val=b;}
  boolean& operator=(const boolean& b) = default;
  boolean& operator=(boolean&& b)      = default;
  boolean(const boolean& b)            = default;
  boolean(boolean&& b)                 = default;
};

// struct holds possible "struct data" internal values
struct data_value_field {
  exp_type exp; // expression data
  par_type par; // pair smrt ptr
  num_type num; // numeric data
  str_type str; // string data
  chr_type chr; // char data
  sym_type sym; // symbolic data
  vec_type vec; // vector smrt ptr
  bol_type bol; // boolean data
  env_type env; // environment smrt ptr
  del_type del; // delayed expression
  prm_type prm; // primitive function pointer
  exe_type exe; // function body execution procedure
  cal_type cal; // recursive call counter
  data_value_field() : exp(exp_type{}), par(nullptr), num(num_type()), 
                       str(nullptr), sym(""), vec(nullptr), bol(false), 
                       env(nullptr), del(nullptr), prm(nullptr), cal(nullptr){}
  data_value_field& operator=(const data_value_field& d) = default;
  data_value_field& operator=(data_value_field&& d)      = default;
  data_value_field(const data_value_field& d)            = default;
  data_value_field(data_value_field&& d)                 = default;
};

// data_obj.type           => current type enum
// data_obj.type_name      => current type's name (string)
// data_obj.value.<T>      => current type <T> value
// data_obj.is_type(T)     => data_obj.type == T
// data_obj.set_value(V,T) => data_obj.value.<T> = V
struct data {
  // current type & value held by data object
  types type = types::undefined;
  data_value_field value;

  // check whether data is of a type
  constexpr bool is_type(const types t) const {return type == t;}

  // set data's value
  void set_value(num_type& new_value)   {value.num=new_value,type=types::num;}
  void set_value(exp_type& new_value)   {value.exp=new_value,type=types::exp;}
  void set_value(par_type& new_value)   {value.par=new_value,type=types::par;}
  void set_value(str_type& new_value)   {value.str=new_value,type=types::str;}
  void set_value(chr_type& new_value)   {value.chr=new_value,type=types::chr;}
  void set_value(sym_type& new_value)   {value.sym=new_value,type=types::sym;}
  void set_value(const char* new_value) {value.sym=new_value,type=types::sym;}
  void set_value(vec_type& new_value)   {value.vec=new_value,type=types::vec;}
  void set_value(bol_type& new_value)   {value.bol=new_value,type=types::bol;}
  void set_value(env_type& new_value)   {value.env=new_value,type=types::env;}
  void set_value(del_type& new_value)   {value.del=new_value,type=types::del;}
  void set_value(prm_type& new_value)   {value.prm=new_value,type=types::prm;}
  void set_value(exe_type& new_value)   {value.exe=new_value,type=types::exe;}
  void set_value(cal_type& new_value)   {value.cal=new_value,type=types::cal;}

  // assignment operator
  void operator=(const data& d) {
    type = d.type;
    switch(type) {
      case types::exp: value.exp = d.value.exp; return;
      case types::par: value.par = d.value.par; return;
      case types::num: value.num = d.value.num; return;
      case types::str: value.str = d.value.str; return;
      case types::chr: value.chr = d.value.chr; return;
      case types::sym: value.sym = d.value.sym; return;
      case types::vec: value.vec = d.value.vec; return;
      case types::bol: value.bol = d.value.bol; return;
      case types::env: value.env = d.value.env; return;
      case types::del: value.del = d.value.del; return;
      case types::prm: value.prm = d.value.prm; return;
      case types::exe: value.exe = d.value.exe; return;
      case types::cal: value.cal = d.value.cal; return;
      case types::undefined: return;
    }
  }

  void operator=(data&& d) {
    type = d.type;
    switch(type) {
      case types::exp: value.exp = d.value.exp; return;
      case types::par: value.par = d.value.par; return;
      case types::num: value.num = d.value.num; return;
      case types::str: value.str = d.value.str; return;
      case types::chr: value.chr = d.value.chr; return;
      case types::sym: value.sym = d.value.sym; return;
      case types::vec: value.vec = d.value.vec; return;
      case types::bol: value.bol = d.value.bol; return;
      case types::env: value.env = d.value.env; return;
      case types::del: value.del = d.value.del; return;
      case types::prm: value.prm = d.value.prm; return;
      case types::exe: value.exe = d.value.exe; return;
      case types::cal: value.cal = d.value.cal; return;
      case types::undefined: return;
    }
  }

  // friend output function
  friend std::ostream& operator<<(std::ostream& outs, const data& d) {
    switch(d.type) {
      case types::sym: 
        outs << (d.value.sym==THE_EMPTY_LIST ? "()" : d.value.sym);
        return outs;
      case types::chr: 
        outs << "#\\";
        switch(d.value.chr) {
          case ' ':    outs << "space";     break;
          case '\t':   outs << "tab";       break;
          case '\n':   outs << "newline";   break;
          case '\v':   outs << "vtab";      break;
          case '\f':   outs << "page";      break;
          case '\r':   outs << "return";    break;
          case '\a':   outs << "alarm";     break;
          case '\b':   outs << "backspace"; break;
          case '\0':   outs << "nul";       break;
          case '\x1b': outs << "esc";       break;
          case '\x7f': outs << "delete";    break;
          default: 
            if(isprint(d.value.chr)) outs << d.value.chr;
            else outs << std::hex << 'x' << int((unsigned char)d.value.chr) << std::dec;
        }
        return outs;
      case types::par:       print_list(outs,d.value.par);        return outs;
      case types::vec:       print_vect(outs,d.value.vec);        return outs;
      case types::num:       outs << d.value.num;                 return outs;
      case types::str:       outs << '"'<<*d.value.str<<'"';       return outs;
      case types::bol:       outs << (d.value.bol.val?"#t":"#f"); return outs;
      case types::exp:       outs << "#<scm-expression>";         return outs;
      case types::env:       outs << "#<environment>";            return outs;
      case types::del:       outs << "#<delay>";                  return outs;
      case types::prm:       outs << "#<primitive>";              return outs;
      case types::exe:       outs << "#<procedure-body>";         return outs;
      case types::cal:       outs << "#<recursion-count>";        return outs;
      case types::undefined: outs << "#<undefined>";              return outs;
    }
    return outs;
  }

  // get type's name string
  constexpr const char* type_name() const {
    constexpr const char* const type_names[] = {
      "expression", "pair", "number", "string", "character", "symbol", "vector",
      "boolean", "environment", "delay", "primitive", "execution-procedure", 
      "recursive-call-count", "undefined"
    };
    return type_names[int(type)];
  }

  // constructors
  data()              = default;
  data(const data& d) = default;
  data(data&& d)      = default;
  template<typename T> data(T new_value) {set_value(new_value);}
}; // End struct data

// delay structure
struct delay_data {
  scm_list exp;
  env_type env;
  bool already_forced;
  data result;
  delay_data(scm_list delayed_exp = scm_list{}, env_type delay_env = nullptr) 
    : exp(delayed_exp), env(delay_env), already_forced(false), result(boolean(false)) {}
  delay_data& operator=(const delay_data& d) = default;
  delay_data& operator=(delay_data&& d)      = default;
  delay_data(const delay_data& d)            = default;
  delay_data(delay_data&& d)                 = default;
};

// macro data structure
struct scm_macro {
  frame_var label;
  frame_vars keywords;
  std::vector<scm_list> patterns;
  std::vector<scm_list> templates;
  scm_macro(frame_var u_label = "") : label(u_label) {}
  scm_macro& operator=(const scm_macro& m) = default;
  scm_macro& operator=(scm_macro&& m)      = default;
  scm_macro(const scm_macro& m)            = default;
  scm_macro(scm_macro&& m)                 = default;
};

/******************************************************************************
* PREMADE SYMBOLIC CONSTANTS
******************************************************************************/

namespace symconst {
  const scm_string emptylist = THE_EMPTY_LIST;
  const scm_string sentinel  = SENTINEL_ARG;
  const scm_string do_label  = DO_LETREC_INSTANCE_NAME;
  const scm_string primitive = PRIMITIVE_TAG;
  const scm_string procedure = PROCEDURE_TAG;
  const scm_string null_env  = "null-environment";
  const scm_string lambda    = "lambda";
  const scm_string list      = "list";
  const scm_string vector    = "vector";
  const scm_string cons      = "cons";
  const scm_string append    = "append";
  const scm_string delay     = "delay";
  const scm_string quote     = "quote";
  const scm_string let       = "let";
  const scm_string letrec    = "letrec";
  const scm_string set       = "set!";
  const scm_string defn_syn  = "define-syntax";
  const scm_string define    = "define";
  const scm_string begin     = "begin";
  const scm_string equalp    = "equal?";
  const scm_string if_t      = "if";
  const scm_string else_t    = "else";
  const scm_string cond      = "cond";
  const scm_string and_t     = "and";
  const scm_string or_t      = "or";
  const scm_string false_t   = "#f";
  const scm_string ok        = "ok";
  const scm_string memv      = "memv";
  const scm_string ellipsis  = "...";
  const scm_string period    = ".";
} // End namespace symconst

/******************************************************************************
* LIST PRINTING HELPER FUNCTIONS
******************************************************************************/

// Print list recursively
bool is_not_THE_EMPTY_LIST(const data& pair_data) {
  return (!pair_data.is_type(types::sym) || pair_data.value.sym != THE_EMPTY_LIST);
}

void print_list_recur(std::ostream& outs, const par_type& pair_object) {
  // print car
  if(pair_object->first.is_type(types::par)) {
    outs << '(';
    print_list_recur(outs, pair_object->first.value.par);
    outs << ')';
  } else {
    outs << pair_object->first;
  }
  // print space if not last item in list
  if(is_not_THE_EMPTY_LIST(pair_object->second)) outs << ' ';
  // print cdr
  if(pair_object->second.is_type(types::par)) {
    print_list_recur(outs, pair_object->second.value.par);
  } else if(is_not_THE_EMPTY_LIST(pair_object->second)) { // don't print terminating '()
    // print ' . ' since not a null-terminated list
    outs << ". " << pair_object->second;
  }
}

void print_list(std::ostream& outs, const par_type& pair_object) {
  outs << '(';
  print_list_recur(outs, pair_object);
  outs << ')';
}

/******************************************************************************
* VECTOR PRINTING HELPER FUNCTIONS
******************************************************************************/

void print_vect(std::ostream& outs, const vec_type& vector_object) {
  outs << "#(";
  for(size_type i = 0, n = vector_object->size(); i < n; ++i) {
    if(vector_object->operator[](i).is_type(types::vec))
      print_vect(outs, vector_object->operator[](i).value.vec);
    else
      outs << vector_object->operator[](i);
    if(i < n-1) outs << ' ';
  }
  outs << ')';
}
#endif