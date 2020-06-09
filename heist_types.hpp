// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_types.hpp
// => Contains type aliases & structures for the C++ Heist Scheme Interpreter

#ifndef HEIST_TYPES_HPP_
#define HEIST_TYPES_HPP_

#include <algorithm>
#include <functional>
#include <memory>
#include <tuple>
#include <vector>
#include "heist_numerics.hpp"

namespace heist_scm {

  /******************************************************************************
  * ABSTRACT SYNTAX TREE & INTERNAL TYPE ALIASES
  ******************************************************************************/

  using scm_list   = std::vector<struct data>;          // scheme expression list
  using scm_node   = scm_list::iterator;                // scheme expression node
  using scm_pair   = std::pair<struct data,struct data>;// scheme pair
  using scm_string = std::string;                       // string type
  using size_type  = std::size_t;                       // numeric type, idxs etc

  /******************************************************************************
  * MAX NUMBER OF RECURSIVE CALLS
  ******************************************************************************/

  size_type MAX_RECURSION_DEPTH = 1000; // see set-max-recursion-depth! primitive

  /******************************************************************************
  * WHETHER TO USE ANSI ESCAPE SEQUENCES TO FORMAT OUTPUT
  ******************************************************************************/

  bool USING_ANSI_ESCAPE_SEQUENCES = true; // see set-nansi! primitive

  /******************************************************************************
  * WHETHER SYMBOLS ARE CASE-SENSITIVE
  ******************************************************************************/

  bool USING_CASE_SENSITIVE_SYMBOLS = true; // see set-ci! primitive

  /******************************************************************************
  * REPL PROMPT VARIABLES
  ******************************************************************************/

  scm_string REPL_PROMPT = "> "; // see set-repl-prompt! primitive
  scm_string REPL_TAB    = "  ";

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
  * THE GLOBAL MACRO LABEL REGISTRY & HYGIENIC HASH INDICES
  ******************************************************************************/

  std::vector<scm_string> MACRO_LABEL_REGISTRY; // optimizes procedure analysis

  size_type MACRO_HASH_IDX_1 = 0, MACRO_HASH_IDX_2 = 0;

  /******************************************************************************
  * MAX VALUE FOR SIZE_TYPE
  ******************************************************************************/

  constexpr const auto MAX_SIZE_TYPE = std::numeric_limits<size_type>::max();

  /******************************************************************************
  * PREMADE SYMBOLIC CONSTANTS
  ******************************************************************************/

  // NOTE: ***ALL SYMBOL NAMES BEGINNING WITH "__HEIST-" ARE RESERVED!!!***

  namespace symconst {
    constexpr const char * const emptylist    = "";
    constexpr const char * const sentinel_arg = "__HEIST-NIL-ARG";
    constexpr const char * const do_label     = "__HEIST-DO-LETREC";
    constexpr const char * const primitive    = "__HEIST-PRIMITIVE";
    constexpr const char * const procedure    = "__HEIST-PROCEDURE";
    constexpr const char * const tail_call    = "__HEIST-TAIL-CALL";
    constexpr const char * const null_env     = "null-environment";
    constexpr const char * const locl_env     = "local-environment";
    constexpr const char * const lambda       = "lambda";
    constexpr const char * const stream       = "stream";
    constexpr const char * const scons        = "scons";
    constexpr const char * const vec_literal  = "vector-literal";
    constexpr const char * const delay        = "delay";
    constexpr const char * const quote        = "quote";
    constexpr const char * const quasiquote   = "quasiquote";
    constexpr const char * const unquote      = "unquote";
    constexpr const char * const unquo_splice = "unquote-splicing";
    constexpr const char * const let          = "let";
    constexpr const char * const let_star     = "let*";
    constexpr const char * const letrec       = "letrec";
    constexpr const char * const set          = "set!";
    constexpr const char * const defn_syn     = "define-syntax";
    constexpr const char * const let_syn      = "let-syntax";
    constexpr const char * const letrec_syn   = "letrec-syntax";
    constexpr const char * const syn_rules    = "syntax-rules";
    constexpr const char * const define       = "define";
    constexpr const char * const begin        = "begin";
    constexpr const char * const if_t         = "if";
    constexpr const char * const else_t       = "else";
    constexpr const char * const cond         = "cond";
    constexpr const char * const case_t       = "case";
    constexpr const char * const do_t         = "do";
    constexpr const char * const and_t        = "and";
    constexpr const char * const or_t         = "or";
    constexpr const char * const ellipsis     = "...";
    constexpr const char * const period       = ".";
    constexpr const char * const true_t       = "#t";
    constexpr const char * const false_t      = "#f";
    constexpr const char * const memv         = "memv";
    constexpr const char * const equalp       = "equal?";
    constexpr const char * const append       = "append";
    constexpr const char * const cons         = "cons";
    constexpr const char * const list         = "list";
    constexpr const char * const list_star    = "list*";
    constexpr const char * const vector       = "vector";
    constexpr const char   mangle_prefix[]    = "__HEIST-MLAMBDA-";
  } // End namespace symconst

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
  using num_type = scm_numeric::Snum;                  // number (float/int/frac)
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
  using syn_type = struct scm_macro;                   // syntax-rules object

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

  scm_string cio_list_str(const data& pair_object)       noexcept; // to print lists
  scm_string cio_vect_str(const vec_type& vector_object) noexcept; // to print vectors
  scm_string cio_expr_str(const exp_type& exp_object)    noexcept; // to print expressions

  /******************************************************************************
  * DATA TYPE STRUCTS
  ******************************************************************************/

  // enum of "struct data" types
  // => expression, pair, number, string, character, symbol, vector, boolean, environment, delay, primitive fcn ptr, 
  //    execution procedure, recursive call counter, input port, output port, does-not-exist, syntax-rules, undefined value
  enum class types {exp=1, par, num, str, chr, sym, vec, bol, env, del, prm, exe, cal, fip, fop, dne, syn, undefined};

  // boolean struct (differentiates from 'chr_type')
  struct boolean {
    bool val; 
    boolean(const bool& b=false)         noexcept : val(b) {}
    void operator=(const bool& b)        noexcept {val=b;}
    boolean& operator=(const boolean& b) noexcept = default;
    boolean& operator=(boolean&& b)      noexcept = default;
    boolean(const boolean& b)            noexcept = default;
    boolean(boolean&& b)                 noexcept = default;
    ~boolean()                           noexcept = default;
  };

  // input port struct (differentiates from 'oport')
  struct iport {
    size_type port_idx;
    iport(const size_type& idx = 0)   noexcept : port_idx(idx) {}
    iport(const iport& ip)            noexcept = default;
    iport(iport&& ip)                 noexcept = default;
    ~iport()                          noexcept = default;
    iport& operator=(const iport& ip) noexcept = default;
    iport& operator=(iport&& ip)      noexcept = default;
    bool is_open() const noexcept {return PORT_REGISTRY[port_idx] != nullptr;}
    FILE*& port()  const noexcept {return PORT_REGISTRY[port_idx];}
  };

  // output port struct (differentiates from 'iport')
  struct oport {
    size_type port_idx;
    oport(const size_type& idx = 1)   noexcept : port_idx(idx) {}
    oport(const oport& ip)            noexcept = default;
    oport(oport&& ip)                 noexcept = default;
    ~oport()                          noexcept = default;
    oport& operator=(const oport& ip) noexcept = default;
    oport& operator=(oport&& ip)      noexcept = default;
    bool is_open() const noexcept {return PORT_REGISTRY[port_idx] != nullptr;}
    FILE*& port()  const noexcept {return PORT_REGISTRY[port_idx];}
  };

  // macro data structure
  struct scm_macro {
    frame_var label;
    frame_vars keywords;
    std::vector<scm_list> patterns, templates;
    scm_macro(frame_var u_label = "") noexcept : label(u_label) {}
    scm_macro(const scm_macro& m)     noexcept {*this = m;}
    scm_macro(scm_macro&& m)          noexcept {*this = std::move(m);}
    ~scm_macro()                      noexcept {}
    void operator=(const scm_macro& m)noexcept {
      label     = m.label,    keywords  = m.keywords;
      patterns  = m.patterns, templates = m.templates;
    }
    void operator=(scm_macro&& m) noexcept {
      label     = std::move(m.label),    keywords  = std::move(m.keywords);
      patterns  = std::move(m.patterns), templates = std::move(m.templates);
    }
  };

  // data_obj.type       => current type enum
  // data_obj.type_name  => current type's name (string)
  // data_obj.<T>        => current type <T> value
  // data_obj.is_type(T) => data_obj.type == T
  struct data {
    // current type & value held by data object
    types type = types::undefined;
    
    union {
      sym_type sym; // symbolic data
      exp_type exp; // expression data
      num_type num; // numeric data
      par_type par; // pair smrt ptr
      str_type str; // string smrt ptr
      chr_type chr; // char data
      vec_type vec; // vector smrt ptr
      bol_type bol; // boolean data
      env_type env; // environment smrt ptr
      del_type del; // delayed expression smrt ptr
      prm_type prm; // primitive function pointer
      exe_type exe; // function body execution procedure
      cal_type cal; // recursive call counter smrt ptr
      fip_type fip; // file input port
      fop_type fop; // file output port
      syn_type syn; // syntax-rules object
    };

    // check whether data is of a type
    constexpr bool is_type(const types& t) const noexcept {return type == t;}

    // assignment operator
    void operator=(const data& d) noexcept {
      if(type == d.type) {
        switch(type) {
          case types::sym: sym = d.sym; return;
          case types::exp: exp = d.exp; return;
          case types::par: par = d.par; return;
          case types::num: num = d.num; return;
          case types::str: str = d.str; return;
          case types::chr: chr = d.chr; return;
          case types::vec: vec = d.vec; return;
          case types::bol: bol = d.bol; return;
          case types::env: env = d.env; return;
          case types::del: del = d.del; return;
          case types::prm: prm = d.prm; return;
          case types::exe: exe = d.exe; return;
          case types::cal: cal = d.cal; return;
          case types::fip: fip = d.fip; return;
          case types::fop: fop = d.fop; return;
          case types::syn: syn = d.syn; return;
          default:                      return;
        }
      } else {
        this->~data();
        switch(d.type) {
          case types::sym: new (this) data(d.sym); return;
          case types::exp: new (this) data(d.exp); return;
          case types::par: new (this) data(d.par); return;
          case types::num: new (this) data(d.num); return;
          case types::str: new (this) data(d.str); return;
          case types::chr: new (this) data(d.chr); return;
          case types::vec: new (this) data(d.vec); return;
          case types::bol: new (this) data(d.bol); return;
          case types::env: new (this) data(d.env); return;
          case types::del: new (this) data(d.del); return;
          case types::prm: new (this) data(d.prm); return;
          case types::exe: new (this) data(d.exe); return;
          case types::cal: new (this) data(d.cal); return;
          case types::fip: new (this) data(d.fip); return;
          case types::fop: new (this) data(d.fop); return;
          case types::syn: new (this) data(d.syn); return;
          case types::dne: new (this) data(d.type);return;
          default:         new (this) data();      return; // types::undefined
        }
      }
    }

    // assignment operator
    void operator=(data&& d) noexcept {
      if(type == d.type) {
        switch(type) {
          case types::sym: sym = std::move(d.sym); return;
          case types::exp: exp = std::move(d.exp); return;
          case types::par: par = std::move(d.par); return;
          case types::num: num = std::move(d.num); return;
          case types::str: str = std::move(d.str); return;
          case types::chr: chr = std::move(d.chr); return;
          case types::vec: vec = std::move(d.vec); return;
          case types::bol: bol = std::move(d.bol); return;
          case types::env: env = std::move(d.env); return;
          case types::del: del = std::move(d.del); return;
          case types::prm: prm = std::move(d.prm); return;
          case types::exe: exe = std::move(d.exe); return;
          case types::cal: cal = std::move(d.cal); return;
          case types::fip: fip = std::move(d.fip); return;
          case types::fop: fop = std::move(d.fop); return;
          case types::syn: syn = std::move(d.syn); return;
          default:                                 return;
        }
      } else {
        this->~data();
        switch(d.type) {
          case types::sym: new (this) data(std::move(d.sym)); return;
          case types::exp: new (this) data(std::move(d.exp)); return;
          case types::par: new (this) data(std::move(d.par)); return;
          case types::num: new (this) data(std::move(d.num)); return;
          case types::str: new (this) data(std::move(d.str)); return;
          case types::chr: new (this) data(std::move(d.chr)); return;
          case types::vec: new (this) data(std::move(d.vec)); return;
          case types::bol: new (this) data(std::move(d.bol)); return;
          case types::env: new (this) data(std::move(d.env)); return;
          case types::del: new (this) data(std::move(d.del)); return;
          case types::prm: new (this) data(std::move(d.prm)); return;
          case types::exe: new (this) data(std::move(d.exe)); return;
          case types::cal: new (this) data(std::move(d.cal)); return;
          case types::fip: new (this) data(std::move(d.fip)); return;
          case types::fop: new (this) data(std::move(d.fop)); return;
          case types::syn: new (this) data(std::move(d.syn)); return;
          case types::dne: new (this) data(std::move(d.type));return;
          default:         new (this) data();                 return; // types::undefined
        }
      }
    }

    // get current value as a string (for c-style I/O)
    scm_string cio_str() const noexcept {
      switch(type) {
        case types::sym: 
          if(sym==symconst::emptylist)             return "()";
          if(sym.find(symconst::mangle_prefix)==0) return "lambda";
          return sym;
        case types::chr: 
          switch(chr) {
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
              if(isprint(chr)) return scm_string("#\\") + char(chr);
              else {
                char str[32];
                sprintf(str, "#\\x%x", chr);
                return scm_string(str);
              }
          }
        case types::par: return cio_list_str(*this);
        case types::vec: return cio_vect_str(vec);
        case types::exp: return cio_expr_str(exp);
        case types::num: return num.cio_str();
        case types::str: return '"' + *str + '"';
        case types::bol: return (bol.val?"#t":"#f");
        case types::env: return "#<environment>";
        case types::del: return "#<delay>";
        case types::prm: return "#<primitive>";
        case types::exe: return "#<procedure-body>";
        case types::cal: return "#<recursion-count>";
        case types::fip: return "#<input-port>";
        case types::fop: return "#<output-port>";
        case types::dne: return "";
        case types::syn: return "#<syntax-rules-object>";
        default:         return "#<undefined>"; // types::undefined
      }
    }

    // friend output function
    friend std::ostream& operator<<(std::ostream& outs, const data& d) noexcept {
      outs << d.cio_str();
      return outs;
    }

    // get type's name string
    constexpr const char* type_name() const noexcept {
      constexpr const char* const type_names[] = {
        "null", "expression", "pair", "number", "string", "character", "symbol", "vector",
        "boolean", "environment", "delay", "primitive", "execution-procedure", 
        "recursive-call-count", "input-port", "output-port", "void", "syntax-rules", 
        "undefined"
      };
      return type_names[int(type) * (type!=types::sym || sym[0])]; // idx 0 for '() typename
    }

    // constructors
    data()                          noexcept {}
    data(const par_type& new_value) noexcept : type(types::par), par(new_value) {}
    data(const str_type& new_value) noexcept : type(types::str), str(new_value) {}
    data(const vec_type& new_value) noexcept : type(types::vec), vec(new_value) {}
    data(const env_type& new_value) noexcept : type(types::env), env(new_value) {}
    data(const del_type& new_value) noexcept : type(types::del), del(new_value) {}
    data(const cal_type& new_value) noexcept : type(types::cal), cal(new_value) {}
    data(const num_type& new_value) noexcept : type(types::num), num(new_value) {}
    data(const exp_type& new_value) noexcept : type(types::exp), exp(new_value) {}
    data(const chr_type& new_value) noexcept : type(types::chr), chr(new_value) {}
    data(const char& new_value)     noexcept : type(types::chr), chr(new_value) {}
    data(const char* new_value)     noexcept : type(types::sym), sym(new_value) {}
    data(const sym_type& new_value) noexcept : type(types::sym), sym(new_value) {}
    data(const bol_type& new_value) noexcept : type(types::bol), bol(new_value) {}
    data(const prm_type& new_value) noexcept : type(types::prm), prm(new_value) {}
    data(const exe_type& new_value) noexcept : type(types::exe), exe(new_value) {}
    data(const fip_type& new_value) noexcept : type(types::fip), fip(new_value) {}
    data(const fop_type& new_value) noexcept : type(types::fop), fop(new_value) {}
    data(const syn_type& new_value) noexcept : type(types::syn), syn(new_value) {}

    data(par_type&& new_value) noexcept : type(types::par), par(std::move(new_value)) {}
    data(str_type&& new_value) noexcept : type(types::str), str(std::move(new_value)) {}
    data(vec_type&& new_value) noexcept : type(types::vec), vec(std::move(new_value)) {}
    data(env_type&& new_value) noexcept : type(types::env), env(std::move(new_value)) {}
    data(del_type&& new_value) noexcept : type(types::del), del(std::move(new_value)) {}
    data(cal_type&& new_value) noexcept : type(types::cal), cal(std::move(new_value)) {}
    data(sym_type&& new_value) noexcept : type(types::sym), sym(std::move(new_value)) {}
    data(bol_type&& new_value) noexcept : type(types::bol), bol(std::move(new_value)) {}
    data(prm_type&& new_value) noexcept : type(types::prm), prm(std::move(new_value)) {}
    data(exe_type&& new_value) noexcept : type(types::exe), exe(std::move(new_value)) {}
    data(fip_type&& new_value) noexcept : type(types::fip), fip(std::move(new_value)) {}
    data(fop_type&& new_value) noexcept : type(types::fop), fop(std::move(new_value)) {}
    data(syn_type&& new_value) noexcept : type(types::syn), syn(std::move(new_value)) {}
    data(num_type&& new_value) noexcept : type(types::num), num(std::move(new_value)) {}
    data(exp_type&& new_value) noexcept : type(types::exp), exp(std::move(new_value)) {}
    data(chr_type&& new_value) noexcept : type(types::chr), chr(std::move(new_value)) {}

    data(const types& t) noexcept : type(t) {}            // to set 'dne
    data(types&& t)      noexcept : type(std::move(t)) {} // to set 'dne
    data(const data& d)  noexcept {*this = d;}
    data(data&& d)       noexcept {*this = std::move(d);}
    
    ~data() noexcept {
      switch(type) {
        case types::sym: sym.~sym_type(); return;
        case types::exp: exp.~exp_type(); return;
        case types::par: par.~par_type(); return;
        case types::num: num.~num_type(); return;
        case types::str: str.~str_type(); return;
        case types::vec: vec.~vec_type(); return;
        case types::env: env.~env_type(); return;
        case types::del: del.~del_type(); return;
        case types::exe: exe.~exe_type(); return;
        case types::cal: cal.~cal_type(); return;
        case types::syn: syn.~syn_type(); return;
        default:                          return;
      }
      type = types::undefined;
    }
  }; // End struct data

  // delay structure
  struct delay_data {
    scm_list exp;
    env_type env;
    bool already_forced;
    data result;
    delay_data(const scm_list& delayed_exp = scm_list(), env_type delay_env = nullptr) noexcept
      : exp(delayed_exp), env(delay_env), already_forced(false), result(boolean(false)) {}
    void operator=(const delay_data& d) noexcept {
      exp=d.exp, env=d.env, already_forced=d.already_forced, result=d.result;
    }
    void operator=(delay_data&& d) noexcept {
      exp=std::move(d.exp), env=std::move(d.env);
      already_forced=std::move(d.already_forced), result=std::move(d.result);
    }
    delay_data(const delay_data& d) noexcept {*this = d;}
    delay_data(delay_data&& d)      noexcept {*this = std::move(d);}
    ~delay_data()                   noexcept {}
  };
} // End of namespace heist_scm

/******************************************************************************
* PRINTING HELPER FUNCTIONS
******************************************************************************/

// Link toolkit here to inherit all of the above type defns
#include "heist_types_toolkit.hpp"

#endif