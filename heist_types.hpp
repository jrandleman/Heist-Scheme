// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_types.hpp
// => Contains type aliases & structures for the C++ Heist Scheme Interpreter

#ifndef HEIST_TYPES_HPP_
#define HEIST_TYPES_HPP_

#include <functional>
#include <cstdio>
#include <memory>
#include <tuple>
#include "heist_numerics.hpp"

/******************************************************************************
* ABSTRACT SYNTAX TREE & INTERNAL TYPE ALIASES
******************************************************************************/

using scm_list   = std::vector<struct data>;          // scheme expression list
using scm_node   = scm_list::iterator;                // scheme expression node
using scm_pair   = std::pair<struct data,struct data>;// scheme pair
using scm_string = std::string;                       // string type
using size_type  = std::size_t;                       // numeric type, idxs etc
using signed_num = long long;                         // negative numerics

/******************************************************************************
* MAX NUMBER OF RECURSIVE CALLS
******************************************************************************/

size_type MAX_RECURSION_DEPTH = 1000; // see set-max-recursion-depth! primitive

/******************************************************************************
* WHETHER TO USE ANSI ESCAPE SEQUENCES TO FORMAT OUTPUT
******************************************************************************/

bool USING_ANSI_ESCAPE_SEQUENCES = true;

/******************************************************************************
* REPL FORMATTING TRACKER VARIABLES
******************************************************************************/

bool LAST_PRINTED_NEWLINE_TO_STDOUT = false;
bool LAST_PRINTED_TO_STDOUT         = false;

/******************************************************************************
* CURRENT DEFAULT INPUT & OUTPUT PORTS + THE GLOBAL PORT REGISTRY
******************************************************************************/

FILE* CURRENT_INPUT_PORT  = stdin;
FILE* CURRENT_OUTPUT_PORT = stdout;

std::vector<FILE*> PORT_REGISTRY({stdin,stdout});

/******************************************************************************
* THE GLOBAL MACRO LABEL REGISTRY
******************************************************************************/

std::vector<scm_string> MACRO_LABEL_REGISTRY; // optimizes procedure analysis

/******************************************************************************
* MAX VALUE FOR SIZE_TYPE
******************************************************************************/

constexpr const auto MAX_SIZE_TYPE = std::numeric_limits<size_type>::max();

/******************************************************************************
* RESERVED VOID ARG NAME, EMPTY-LIST VALUE, PROCEDURE TAGS, MANGLED NAME PREFIX
******************************************************************************/

// NOTE: THE FOLLOWING SYMBOLS ARE RESERVED!

constexpr const char * const SENTINEL_ARG            = "HEIST-NIL-ARG"; 
constexpr const char * const DO_LETREC_INSTANCE_NAME = "HEIST-DO-LETREC";
constexpr const char * const PROCEDURE_TAG           = "HEIST-PROCEDURE";
constexpr const char * const PRIMITIVE_TAG           = "HEIST-PRIMITIVE";
constexpr const char * const MANGLED_LAMBDA_PREFIX   = "''lambda-";
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
* DATA TYPE ALIASES & CONSTRUCTORS
******************************************************************************/

using exp_type = scm_list;                           // expression
using par_type = std::shared_ptr<scm_pair>;          // pair
using num_type = Scm_numeric;                        // number (float/int/frac)
using str_type = std::shared_ptr<scm_string>;        // string
using chr_type = int;                                // character
using sym_type = scm_string;                         // symbol
using vec_type = std::shared_ptr<scm_list>;          // vector
using bol_type = struct boolean;                     // boolean
using env_type = std::shared_ptr<environment>;       // evironment
using del_type = std::shared_ptr<struct delay_data>; // delay
using prm_type = struct data(*)(scm_list&);          // primitive procedure ptr
using exe_type = std::function<scm_list(env_type&)>; // fcn execution procedure
using cal_type = std::shared_ptr<size_type>;         // recursive call counter
using fip_type = struct iport;                       // file input port
using fop_type = struct oport;                       // file output port

auto make_par = std::make_shared<scm_pair>;
auto make_str = std::make_shared<scm_string,const scm_string&>;
auto make_vec = std::make_shared<scm_list,const scm_list&>;
auto make_env = std::make_shared<environment>;
auto make_del = std::make_shared<struct delay_data,const scm_list&,const env_type&>;
auto make_cal = std::make_shared<size_type,size_type>;

/******************************************************************************
* GLOBAL ENVIRONMENT POINTER
******************************************************************************/

env_type GLOBAL_ENVIRONMENT_POINTER = nullptr;

/******************************************************************************
* DATA PRINTING HELPER FUNCTION PROTOTYPES
******************************************************************************/

scm_string cio_list_str(const data& pair_object);       // to print lists
scm_string cio_vect_str(const vec_type& vector_object); // to print vectors
scm_string cio_expr_str(const scm_list& exp_object);    // to print expressions

/******************************************************************************
* DATA TYPE STRUCTS
******************************************************************************/

// enum of "struct data" types
// => expression, pair, number, string, character, symbol, vector, boolean, environment, delay, primitive fcn ptr, 
//    execution procedure, recursive call counter, input port, output port, does-not-exist, undefined value
enum class types {exp, par, num, str, chr, sym, vec, bol, env, del, prm, exe, cal, fip, fop, dne, undefined};

// boolean struct (differentiates from 'number')
struct boolean {
  bool val; 
  boolean(bool b=false) : val(b) {}
  void operator=(const bool& b){val=b;}
  boolean& operator=(const boolean& b) = default;
  boolean& operator=(boolean&& b)      = default;
  boolean(const boolean& b)            = default;
  boolean(boolean&& b)                 = default;
};

// input port struct (differentiates from 'oport')
struct iport {
  size_type port_idx;
  iport(const size_type& idx = 0) : port_idx(idx) {}
  iport(const iport& ip)            = default;
  iport(iport&& ip)                 = default;
  iport& operator=(const iport& ip) = default;
  iport& operator=(iport&& ip)      = default;
  bool is_open()  const {return PORT_REGISTRY[port_idx] != nullptr;}
  FILE*& port()   const {return PORT_REGISTRY[port_idx];}
};

// output port struct (differentiates from 'iport')
struct oport {
  size_type port_idx;
  oport(const size_type& idx = 1) : port_idx(idx) {}
  oport(const oport& ip)            = default;
  oport(oport&& ip)                 = default;
  oport& operator=(const oport& ip) = default;
  oport& operator=(oport&& ip)      = default;
  bool is_open()  const {return PORT_REGISTRY[port_idx] != nullptr;}
  FILE*& port()   const {return PORT_REGISTRY[port_idx];}
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
  fip_type fip; // file input port
  fop_type fop; // file output port
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
  constexpr bool is_type(const types& t) const {return type == t;}

  // set data's value
  void set_value(num_type& new_value)   {value.num=new_value,type=types::num;}
  void set_value(exp_type& new_value)   {value.exp=new_value,type=types::exp;}
  void set_value(par_type& new_value)   {value.par=new_value,type=types::par;}
  void set_value(str_type& new_value)   {value.str=new_value,type=types::str;}
  void set_value(chr_type& new_value)   {value.chr=new_value,type=types::chr;}
  void set_value(const char& new_value) {value.chr=new_value,type=types::chr;}
  void set_value(sym_type& new_value)   {value.sym=new_value,type=types::sym;}
  void set_value(const char* new_value) {value.sym=new_value,type=types::sym;}
  void set_value(vec_type& new_value)   {value.vec=new_value,type=types::vec;}
  void set_value(bol_type& new_value)   {value.bol=new_value,type=types::bol;}
  void set_value(env_type& new_value)   {value.env=new_value,type=types::env;}
  void set_value(del_type& new_value)   {value.del=new_value,type=types::del;}
  void set_value(prm_type& new_value)   {value.prm=new_value,type=types::prm;}
  void set_value(exe_type& new_value)   {value.exe=new_value,type=types::exe;}
  void set_value(cal_type& new_value)   {value.cal=new_value,type=types::cal;}
  void set_value(fip_type& new_value)   {value.fip=new_value,type=types::fip;}
  void set_value(fop_type& new_value)   {value.fop=new_value,type=types::fop;}

  // assignment operator
  void operator=(const data& d) {
    type = d.type;
    switch(type) {
      case types::sym: value.sym = d.value.sym; return;
      case types::exp: value.exp = d.value.exp; return;
      case types::par: value.par = d.value.par; return;
      case types::num: value.num = d.value.num; return;
      case types::str: value.str = d.value.str; return;
      case types::chr: value.chr = d.value.chr; return;
      case types::vec: value.vec = d.value.vec; return;
      case types::bol: value.bol = d.value.bol; return;
      case types::env: value.env = d.value.env; return;
      case types::del: value.del = d.value.del; return;
      case types::prm: value.prm = d.value.prm; return;
      case types::exe: value.exe = d.value.exe; return;
      case types::cal: value.cal = d.value.cal; return;
      case types::fip: value.fip = d.value.fip; return;
      case types::fop: value.fop = d.value.fop; return;
      default:                                  return;
    }
  }

  void operator=(data&& d) {
    type = d.type;
    switch(type) {
      case types::sym: value.sym = d.value.sym; return;
      case types::exp: value.exp = d.value.exp; return;
      case types::par: value.par = d.value.par; return;
      case types::num: value.num = d.value.num; return;
      case types::str: value.str = d.value.str; return;
      case types::chr: value.chr = d.value.chr; return;
      case types::vec: value.vec = d.value.vec; return;
      case types::bol: value.bol = d.value.bol; return;
      case types::env: value.env = d.value.env; return;
      case types::del: value.del = d.value.del; return;
      case types::prm: value.prm = d.value.prm; return;
      case types::exe: value.exe = d.value.exe; return;
      case types::cal: value.cal = d.value.cal; return;
      case types::fip: value.fip = d.value.fip; return;
      case types::fop: value.fop = d.value.fop; return;
      default:                                  return;
    }
  }

  // get current value as a string (for c-style I/O)
  scm_string cio_str() const {
    switch(type) {
      case types::sym: 
        if(value.sym==THE_EMPTY_LIST)                return "()";
        if(value.sym.find(MANGLED_LAMBDA_PREFIX)==0) return "lambda";
        return value.sym;
      case types::chr: 
        switch(value.chr) {
          case ' ':    return "#\\space";
          case '\t':   return "#\\tab";
          case '\n':   return "#\\newline";
          case '\v':   return "#\\vtab";
          case '\f':   return "#\\page";
          case '\r':   return "#\\return";
          case '\a':   return "#\\alarm";
          case '\b':   return "#\\backspace";
          case '\0':   return "#\\nul";
          case '\x1b': return "#\\esc";
          case '\x7f': return "#\\delete";
          case EOF:    return "#!eof";
          default: 
            if(isprint(value.chr)) return scm_string("#\\") + char(value.chr);
            else {
              std::ostringstream char_str;
              char_str << "#\\x" << std::hex << value.chr << std::dec;
              return char_str.str();
            }
        }
      case types::par:       return cio_list_str(*this);
      case types::vec:       return cio_vect_str(value.vec);
      case types::exp:       return cio_expr_str(value.exp);
      case types::num:       return value.num.cio_str();
      case types::str:       return '"' + *value.str + '"';
      case types::bol:       return (value.bol.val?"#t":"#f");
      case types::env:       return "#<environment>";
      case types::del:       return "#<delay>";
      case types::prm:       return "#<primitive>";
      case types::exe:       return "#<procedure-body>";
      case types::cal:       return "#<recursion-count>";
      case types::fip:       return "#<input-port>";
      case types::fop:       return "#<output-port>";
      case types::dne:       return "";
      case types::undefined: return "#<undefined>";
    }
  }

  // friend output function
  friend std::ostream& operator<<(std::ostream& outs, const data& d) {
    outs << d.cio_str();
    return outs;
  }

  // get type's name string
  constexpr const char* type_name() const {
    constexpr const char* const type_names[] = {
      "expression", "pair", "number", "string", "character", "symbol", "vector",
      "boolean", "environment", "delay", "primitive", "execution-procedure", 
      "recursive-call-count", "input-port", "output-port", "void", "undefined"
    };
    return type_names[int(type)];
  }

  // constructors
  data()              = default;
  data(const data& d) = default;
  data(data&& d)      = default;
  template<typename T> data(T new_value) {set_value(new_value);}
  data(const types &t)                   {type = t;} // to set 'dne
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
  const sym_type emptylist     = THE_EMPTY_LIST;
  const sym_type sentinel      = SENTINEL_ARG;
  const sym_type do_label      = DO_LETREC_INSTANCE_NAME;
  const sym_type primitive     = PRIMITIVE_TAG;
  const sym_type procedure     = PROCEDURE_TAG;
  const sym_type mangle_prefix = MANGLED_LAMBDA_PREFIX;
  const sym_type null_env      = "null-environment";
  const sym_type locl_env      = "local-environment";
  const sym_type lambda        = "lambda";
  const sym_type list          = "list";
  const sym_type stream        = "stream";
  const sym_type vector        = "vector";
  const sym_type cons          = "cons";
  const sym_type append        = "append";
  const sym_type delay         = "delay";
  const sym_type quote         = "quote";
  const sym_type let           = "let";
  const sym_type letrec        = "letrec";
  const sym_type set           = "set!";
  const sym_type defn_syn      = "define-syntax";
  const sym_type define        = "define";
  const sym_type begin         = "begin";
  const sym_type equalp        = "equal?";
  const sym_type if_t          = "if";
  const sym_type else_t        = "else";
  const sym_type cond          = "cond";
  const sym_type and_t         = "and";
  const sym_type or_t          = "or";
  const sym_type memv          = "memv";
  const sym_type ellipsis      = "...";
  const sym_type period        = ".";
} // End namespace symconst

/******************************************************************************
* PRINTING HELPER FUNCTIONS
******************************************************************************/

// Link toolkit here to inherit all of the above type defns
#include "heist_types_toolkit.hpp"

#endif
