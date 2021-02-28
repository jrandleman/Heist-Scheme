// Author: Jordan Randleman -- jrandleman@scu.edu -- types.hpp
// => Contains type aliases & structures for the C++ Heist Scheme Interpreter

#ifndef TYPES_HPP_
#define TYPES_HPP_

#include <algorithm>
#include <climits>
#include <ctime>
#include <filesystem>
#include <functional>
#include <map>
#include <ratio>
#include <tuple>
#include <unordered_map>
#include <vector>
#include "numerics.hpp"
#include "garbage_collector.hpp"

namespace heist {

  /******************************************************************************
  * ABSTRACT SYNTAX TREE & INTERNAL TYPE ALIASES
  ******************************************************************************/

  using scm_list   = std::vector<struct data>; // scheme expression list
  using scm_node   = scm_list::iterator;
  using scm_pair   = std::pair<struct data,struct data>;
  using scm_string = std::string;
  using size_type  = std::size_t;

  /******************************************************************************
  * INFIX TABLE TYPE ALIASES
  ******************************************************************************/

  using infix_op_t    = std::pair<bool,std::string>; // {left-assoc?, symbol}
  using infix_level_t = std::vector<infix_op_t>;

  /******************************************************************************
  * GLOBAL PROCESS-INDEPENDANT GLOBAL INVARIANTS
  ******************************************************************************/

  namespace GLOBALS {

    /******************************************************************************
    * EXIT SUCCESS CODE TO RETURN
    ******************************************************************************/

    int HEIST_EXIT_CODE = 0;

    /******************************************************************************
    * WHETHER SYMBOLS ARE CASE-SENSITIVE
    ******************************************************************************/

    bool USING_CASE_SENSITIVE_SYMBOLS = true; // see (ci?) primitive

    /******************************************************************************
    * ARGV REGISTRY OF STRINGS
    ******************************************************************************/

    std::vector<tgc_ptr<scm_string>> ARGV;

    /******************************************************************************
    * GLOBAL PORT REGISTRY
    ******************************************************************************/

    std::vector<FILE*> PORT_REGISTRY({stdin,stdout});

    /******************************************************************************
    * STACK TRACE
    ******************************************************************************/

    std::vector<scm_string> STACK_TRACE;

    /******************************************************************************
    * MAX VALUE FOR SIZE_TYPE
    ******************************************************************************/

    constexpr const auto MAX_SIZE_TYPE = std::numeric_limits<size_type>::max();

  } // End of namespace GLOBALS

  /******************************************************************************
  * PREMADE SYMBOLIC CONSTANTS
  ******************************************************************************/

  // NOTE: ***ALL SYMBOL NAMES BEGINNING WITH "heist:core:" ARE RESERVED!!!***

  namespace symconst {
    constexpr const char * const emptylist         = "";
    constexpr const char * const dot               = "*dot*";
    constexpr const char * const tail_call         = "heist:core:tail-call";
    constexpr const char * const continuation      = "heist:core:cps-";              // hashed continuation arg name prefix
    constexpr const char * const pass_continuation = "heist:core:pass-continuation"; // denotes to treat proc as if defn'd in a scm->cps block
    constexpr const char * const cps_app_tag       = "heist:core:app-cps";
    constexpr const char * const gensym_prefix     = "heist:core:gensym-";
    constexpr const char * const reader_lambda     = "heist:core:reader-lambda";
    constexpr const char * const cps_ignore_arg    = "heist:core:ignore";
    constexpr const char * const scm_cps           = "scm->cps";
    constexpr const char * const cps_quote         = "cps-quote";
    constexpr const char * const using_cpsp        = "using-cps?";
    constexpr const char * const defclass          = "defclass";
    constexpr const char * const null_env          = "*null-environment*";
    constexpr const char * const local_env         = "*local-environment*";
    constexpr const char * const global_env        = "*global-environment*";
    constexpr const char * const exit_success      = "*exit-success*";
    constexpr const char * const exit_failure      = "*exit-failure*";
    constexpr const char * const lambda            = "lambda";
    constexpr const char * const fn                = "fn";
    constexpr const char * const vec_literal       = "vector-literal";
    constexpr const char * const map_literal       = "hmap-literal";
    constexpr const char * const delay             = "delay";
    constexpr const char * const quote             = "quote";
    constexpr const char * const quasiquote        = "quasiquote";
    constexpr const char * const unquote           = "unquote";
    constexpr const char * const unquo_splice      = "unquote-splicing";
    constexpr const char * const set               = "set!";
    constexpr const char * const core_syn          = "core-syntax";
    constexpr const char * const defn_syn          = "define-syntax";
    constexpr const char * const syn_rules         = "syntax-rules";
    constexpr const char * const syn_hash          = "syntax-hash";
    constexpr const char * const define            = "define";
    constexpr const char * const begin             = "begin";
    constexpr const char * const if_t              = "if";
    constexpr const char * const ellipsis          = "...";
    constexpr const char * const true_t            = "#t";
    constexpr const char * const false_t           = "#f";
    constexpr const char * const append            = "append";
    constexpr const char * const list              = "list";
    constexpr const char * const list_star         = "list*";
    constexpr const char * const vector            = "vector";
    constexpr const char * const hmap              = "hmap";
    constexpr const char * const definedp          = "defined?";
    constexpr const char * const infix             = "infix!";
    constexpr const char * const infixr            = "infixr!";
    constexpr const char * const unfix             = "unfix!";
    constexpr const char * const defn_reader_alias = "define-reader-alias";
    constexpr const char * const inf_precedence    = "heist:core:inf-precedence";
    constexpr const char * const while_t           = "heist:core:while";
    constexpr const char * const delete_bang       = "delete!";
    constexpr const char * const dflt_compile_name = "a.cpp";
  } // End namespace symconst

  /******************************************************************************
  * ENVIRONMENT DATA STRUCTURE TYPE ALIASES
  ******************************************************************************/

  // ENVIRONMENTS AS NODES WITH A FRAME & POINTER TO THEIR ENCLOSING ENVIRONMENT
  //    => FRAMES AS TUPLE OF VECTORS: VARIABLE NAMES & VALUES, MACRO DEFNS

  using frame_var  = std::string;
  using frame_val  = struct data;
  using frame_mac  = struct scm_macro;
  using frame_vars = std::vector<frame_var>;
  using frame_vals = std::vector<frame_val>;
  using frame_macs = std::vector<frame_mac>;
  using frame_t    = std::tuple<frame_vars,frame_vals,frame_macs>;


  struct environment {
    // Invariants
    tgc_ptr<environment> parent = nullptr; // enclosing environment pointer
    frame_t frame;                         // environment's bindings

    // Getters
    frame_vars& variables()noexcept{return std::get<0>(frame);}
    const frame_vars& variables()const noexcept{return std::get<0>(frame);}
    frame_vals& values()noexcept{return std::get<1>(frame);}
    const frame_vals& values()const noexcept{return std::get<1>(frame);}
    frame_macs& macros()noexcept{return std::get<2>(frame);}
    const frame_macs& macros()const noexcept{return std::get<2>(frame);}

    // Environmental Traversal & Access
    frame_val lookup_variable_value(const frame_var& var, bool& found)const noexcept;
    bool set_variable_value(const frame_var& var, frame_val&& val)noexcept;
    void define_variable(const frame_var& var, frame_val&& val)noexcept;
    void define_macro(const frame_mac& mac)noexcept;
    scm_string getenv(const frame_var& var, bool& found)const;
    bool has_macro(const scm_string& label)const noexcept;
    bool has_variable(const frame_var& var)const noexcept;
    bool erase_variable(const frame_var& var)noexcept;
    bool erase_macro(const scm_string& label)noexcept;
    bool relabel_macro(const scm_string& old_label, const scm_string& new_label)noexcept;

  private:
    // <relabel_macro> helper function
    void relabel_recursive_calls_in_macro_template(const scm_string& old_label,const scm_string& new_label,scm_list& templ8)noexcept;
  };

  /******************************************************************************
  * DATA TYPE ALIASES & CONSTRUCTORS
  ******************************************************************************/

  using exp_type = scm_list;                             // expression
  using par_type = tgc_ptr<scm_pair>;                    // pair
  using num_type = scm_numeric::Snum;                    // number (float/int/frac)
  using str_type = tgc_ptr<scm_string>;                  // string
  using chr_type = int;                                  // character ("int" allows EOF to be a char)
  using sym_type = scm_string;                           // symbol
  using vec_type = tgc_ptr<scm_list>;                    // vector
  using bol_type = struct boolean;                       // boolean
  using env_type = tgc_ptr<environment>;                 // evironment
  using del_type = tgc_ptr<struct scm_delay>;            // delay
  using fcn_type = struct scm_fcn;                       // procedure (compound & primitive)
  using fip_type = struct iport;                         // file input port
  using fop_type = struct oport;                         // file output port
  using syn_type = struct scm_macro;                     // syntax-rules object
  using map_type = tgc_ptr<struct scm_map>;              // hash-map
  using cls_type = tgc_ptr<struct class_prototype>;      // class-prototype
  using obj_type = tgc_ptr<struct object_type>;          // object
  using prc_type = tgc_ptr<struct process_invariants_t>; // process invariants

  /******************************************************************************
  * PROCEDURE HELPER ALIASES
  ******************************************************************************/

  using prm_ptr_t = struct data(*)(scm_list&);          // primitive procedure ptr
  using exe_fcn_t = std::function<struct data(env_type&)>; // fcn execution procedure

  /******************************************************************************
  * DATA PRINTING HELPER FUNCTION PROTOTYPES
  ******************************************************************************/

  using DATA_PRINTER = scm_string(data::*)()const;
  template<DATA_PRINTER to_str>
  scm_string cio_list_str(const data& pair_object);       // to print lists
  template<DATA_PRINTER to_str>
  scm_string cio_vect_str(const vec_type& vector_object); // to print vectors
  template<DATA_PRINTER to_str>
  scm_string cio_expr_str(const exp_type& exp_object);    // to print expressions
  template<DATA_PRINTER to_str>
  scm_string cio_hmap_str(const map_type& map_object);    // to print hash-maps
  template<DATA_PRINTER to_str>
  scm_string cio_obj_str (const obj_type&,const char*);   // to print objects (checks for overloaded methods)
  scm_string escape_chars(const scm_string& str)noexcept; // to escape string special characters
  scm_string pretty_print(const data& d);                 // pretty-printer

  /******************************************************************************
  * POINTER->HEX-STRING
  ******************************************************************************/

  template<typename T> // PRECONDITION: <raw_ptr> IS A RAW POINTER
  scm_string pointer_to_hexstring(const T raw_ptr)noexcept{
    char str[32];
    snprintf(str, 32, "%zx", size_type(raw_ptr));
    return str;
  }

  /******************************************************************************
  * FUNDAMENTAL SCHEME DATUM INTERNAL UNION TYPES
  ******************************************************************************/

  // enum of "struct data" types
  // => expression, pair, number, string, character, symbol, vector, boolean, environment, delay, procedure (compound & primitive),
  //    input port, output port, does-not-exist, syntax-rules, hash-map, class-prototype, object, process, undefined value
  enum class types {exp=1, par, num, str, chr, sym, vec, bol, env, del, fcn, fip, fop, dne, syn, map, cls, obj, prc, undefined};

  /******************************************************************************
  * DATA EQUALITY HELPER FUNCTION PROTOTYPES
  ******************************************************************************/

  using DATA_COMPARER = bool(data::*)(const data&)const;
  template<DATA_COMPARER same_as>
  bool prm_compare_atomic_values(const data& v1,const data& v2,const types& t);
  template<DATA_COMPARER same_as>
  bool prm_compare_PAIRs(const par_type& p1, const par_type& p2);
  template<DATA_COMPARER same_as>
  bool prm_compare_VECTs(const vec_type& v1, const vec_type& v2);
  template<DATA_COMPARER same_as>
  bool prm_compare_EXPRs(const scm_list& l1, const scm_list& l2);
  template<DATA_COMPARER same_as>
  bool prm_compare_HMAPs(const map_type& m1, const map_type& m2);
  template<DATA_COMPARER same_as>
  bool prm_compare_SNTXs(const syn_type& s1, const syn_type& s2);
  template<DATA_COMPARER same_as>
  bool prm_compare_OBJs (const obj_type& o1, const obj_type& o2);
  bool prm_DYNAMIC_OBJeq(const obj_type&,const data&,const char*,bool&);

  /******************************************************************************
  * DATA TYPE STRUCTS
  ******************************************************************************/

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
    bool is_open() const noexcept {return GLOBALS::PORT_REGISTRY[port_idx] != nullptr;}
    FILE*& port()  const noexcept {return GLOBALS::PORT_REGISTRY[port_idx];}
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
    bool is_open() const noexcept {return GLOBALS::PORT_REGISTRY[port_idx] != nullptr;}
    FILE*& port()  const noexcept {return GLOBALS::PORT_REGISTRY[port_idx];}
  };

  // macro data structure
  struct scm_macro {
    frame_var label;
    frame_vars keywords;
    std::vector<frame_vars> hashed_template_ids; // hashed_template_ids[i] = syntax-hashed vars of templates[i]
    std::vector<scm_list> patterns, templates;
    scm_macro(frame_var u_label = "") noexcept : label(u_label) {}
    scm_macro(const scm_macro& m)     noexcept {*this = m;}
    scm_macro(scm_macro&& m)          noexcept {*this = std::move(m);}
    ~scm_macro()                      noexcept {}
    void operator=(const scm_macro& m)noexcept {
      if(this == &m) return;
      label               = m.label,    keywords  = m.keywords;
      patterns            = m.patterns, templates = m.templates;
      hashed_template_ids = m.hashed_template_ids;
    }
    void operator=(scm_macro&& m) noexcept {
      if(this == &m) return;
      label               = std::move(m.label),    keywords  = std::move(m.keywords);
      patterns            = std::move(m.patterns), templates = std::move(m.templates);
      hashed_template_ids = std::move(m.hashed_template_ids);
    }
  };

  // function (compound & primitive) data structure
  struct scm_fcn {
    using depth_t = tgc_ptr<size_type>;
    scm_string name; // name == "" denotes an anonymous procedure
    std::vector<exp_type> param_instances;
    // PRIMITIVE
    prm_ptr_t prm = nullptr;
    // COMPOUND
    std::vector<exe_fcn_t> bodies;
    env_type env        = nullptr ;
    obj_type self       = nullptr;
    depth_t rec_depth   = nullptr;
    unsigned char flags = 1; // is_lambda (as opposed to 'fn) | using_dynamic_scope [only lambda by default]
    scm_fcn() = default;
    // primitive ctors
    scm_fcn(const exp_type& a, const prm_ptr_t& p)noexcept:prm(p) {param_instances.push_back(a);} // partial primitive
    scm_fcn(const scm_string& n, const prm_ptr_t& p)noexcept:name(n),prm(p) {}                    // primitive
    // tail call wrapper ctor (gets returned up)
    scm_fcn(env_type& e,const exe_fcn_t& b)noexcept:env(e){bodies.push_back(b);}
    // lambda ctor
    scm_fcn(const exp_type& p,const exe_fcn_t& b,env_type& e,const frame_var& n)noexcept:name(n),env(e),rec_depth(depth_t(size_type(0))){
      param_instances.push_back(p), bodies.push_back(b);
    }
    // fn ctor
    scm_fcn(const std::vector<exp_type>& ps,const std::vector<exe_fcn_t>& bs,env_type& e,const frame_var& n)noexcept:name(n),param_instances(ps),bodies(bs),
                                                                                                                     env(e),rec_depth(depth_t(size_type(0))),
                                                                                                                     flags(0){}
    scm_fcn(const scm_fcn& f)noexcept{*this = f;}
    scm_fcn(const scm_fcn&& f)noexcept{*this = std::move(f);}
    void operator=(const scm_fcn& f)noexcept;
    void operator=(scm_fcn&& f)noexcept;
    bool operator==(const scm_fcn& f)const noexcept;
    bool is_primitive()const noexcept{return prm;};
    bool is_compound() const noexcept{return !prm;};
    bool is_lambda()const noexcept{return flags & 1;}
    void set_lambda(bool status)noexcept{if(status) flags |= 1; else flags &= ~1;}
    bool is_using_dynamic_scope()const noexcept{return flags & 2;}
    void set_using_dynamic_scope(bool status)noexcept{if(status) flags |= 2; else flags &= ~2;}
    size_type& recursive_depth()noexcept{return *rec_depth;} // PRECONDITION: rec_depth
    size_type  recursive_depth()const noexcept{return *rec_depth;} // PRECONDITION: rec_depth
    scm_string str()const noexcept{return name.empty() ? "#<procedure>" : "#<procedure " + name + '>';}
    scm_string printable_procedure_name()const noexcept{return name.empty() ? "#<procedure>" : name;}
    frame_vars lambda_parameters()const noexcept;
    env_type   get_extended_environment(exp_type& arguments,exe_fcn_t& body);
  };

  // data_obj.type                  => current type enum
  // data_obj.type_name()           => current type's name (string)
  // data_obj.<T>                   => current type <T> value
  // data_obj.is_type(T)            => data_obj.type == T
  // data_obj.noexcept_write()      => <write> but w/o invoking object methods
  // data_obj.write()               => data_obj's value as a machine-readable string
  // data_obj.display()             => data_obj's value as a human-readable string
  // data_obj.pprint()              => <write> with auto-indentation for lists
  // data_obj.eq(datum)             => eq? shallow equality
  // data_obj.eqv(datum)            => eqv? shallow equality + deep equality for strings
  // data_obj.equal(datum)          => equal? deep equality
  // data_obj.noexcept_equal(datum) => <equal?> but w/o invoking object methods
  // data_obj.is_self_evaluating()  => core evaluator should reflect datum
  // data_obj.copy()                => deep-cpy vector|string|pair|hmap|object
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
      fcn_type fcn; // function (primitive & compound)
      fip_type fip; // file input port
      fop_type fop; // file output port
      syn_type syn; // syntax-rules object
      map_type map; // hash-map smrt ptr
      cls_type cls; // class-prototype smrt ptr
      obj_type obj; // object smrt ptr
      prc_type prc; // process smrt ptr
    };

    // check whether data is of a type
    constexpr bool is_type(const types& t) const noexcept {return type == t;}

    // returns a deep copy of *this ::= vector | string | pair | hmap | object
    data copy() const;

    // returns a shallow copy of *this ::= vector | string | pair | hmap | object
    data shallow_copy() const;

    // assignment operator
    void operator=(const data& d) noexcept {
      if(this == &d) return;
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
          case types::fcn: fcn = d.fcn; return;
          case types::fip: fip = d.fip; return;
          case types::fop: fop = d.fop; return;
          case types::syn: syn = d.syn; return;
          case types::map: map = d.map; return;
          case types::cls: cls = d.cls; return;
          case types::obj: obj = d.obj; return;
          case types::prc: prc = d.prc; return;
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
          case types::fcn: new (this) data(d.fcn); return;
          case types::fip: new (this) data(d.fip); return;
          case types::fop: new (this) data(d.fop); return;
          case types::syn: new (this) data(d.syn); return;
          case types::map: new (this) data(d.map); return;
          case types::cls: new (this) data(d.cls); return;
          case types::obj: new (this) data(d.obj); return;
          case types::prc: new (this) data(d.prc); return;
          case types::dne: new (this) data(d.type);return;
          default:         new (this) data();      return; // types::undefined
        }
      }
    }

    // assignment operator
    void operator=(data&& d) noexcept {
      if(this == &d) return;
      if(type == d.type) {
        switch(d.type) { // env,par,str,del,vec,
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
          case types::fcn: fcn = std::move(d.fcn); return;
          case types::fip: fip = std::move(d.fip); return;
          case types::fop: fop = std::move(d.fop); return;
          case types::syn: syn = std::move(d.syn); return;
          case types::map: map = std::move(d.map); return;
          case types::cls: cls = std::move(d.cls); return;
          case types::obj: obj = std::move(d.obj); return;
          case types::prc: prc = std::move(d.prc); return;
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
          case types::fcn: new (this) data(std::move(d.fcn)); return;
          case types::fip: new (this) data(std::move(d.fip)); return;
          case types::fop: new (this) data(std::move(d.fop)); return;
          case types::syn: new (this) data(std::move(d.syn)); return;
          case types::map: new (this) data(std::move(d.map)); return;
          case types::cls: new (this) data(std::move(d.cls)); return;
          case types::obj: new (this) data(std::move(d.obj)); return;
          case types::prc: new (this) data(std::move(d.prc)); return;
          case types::dne: new (this) data(std::move(d.type));return;
          default:         new (this) data();                 return; // types::undefined
        }
      }
    }

    // noexcept version of <write> (doesn't invoke object printing members)
    scm_string noexcept_write() const noexcept {
      switch(type) {
        case types::sym:
          if(sym==symconst::emptylist) return "()";
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
            case EOF:    return "#<eof>";
            default: 
              if(isprint(chr)) return scm_string("#\\") + char(chr);
              else {
                char str[32];
                snprintf(str, 32, "#\\x%x", chr);
                return scm_string(str);
              }
          }
        case types::num: return num.str();
        case types::str: return '"' + escape_chars(*str) + '"';
        case types::bol: if(bol.val) return "#t"; return "#f";
        case types::fcn: return fcn.str();
        case types::cls: return "#<class-prototype[0x"+pointer_to_hexstring(cls.ptr)+"]>";
        case types::obj: return "#<object[0x"+pointer_to_hexstring(obj.ptr)+"]>";
        case types::env: return "#<environment[0x"+pointer_to_hexstring(env.ptr)+"]>";
        case types::prc: return "#<process-invariants[0x"+pointer_to_hexstring(prc.ptr)+"]>";
        case types::del: return "#<delay[0x"+pointer_to_hexstring(del.ptr)+"]>";
        case types::fip: return "#<input-port>";
        case types::fop: return "#<output-port>";
        case types::dne: return "";
        case types::syn: return "#<syntax-rules-object>";
        case types::par: return cio_list_str<&data::noexcept_write>(*this);
        case types::vec: return cio_vect_str<&data::noexcept_write>(vec);
        case types::exp: return cio_expr_str<&data::noexcept_write>(exp);
        case types::map: return cio_hmap_str<&data::noexcept_write>(map);
        default:         return "#<undefined>"; // types::undefined
      }
    }

    scm_string write() const { // machine-readable string
      switch(type) {
        case types::par: return cio_list_str<&data::write>(*this);
        case types::vec: return cio_vect_str<&data::write>(vec);
        case types::exp: return cio_expr_str<&data::write>(exp);
        case types::map: return cio_hmap_str<&data::write>(map);
        case types::obj: return cio_obj_str<&data::write>(obj,"write");
        default: return noexcept_write();
      }
    }

    scm_string display() const { // human-readable string
      switch(type) {
        case types::chr: return scm_string(1,chr);
        case types::str: return *str;
        case types::par: return cio_list_str<&data::display>(*this);
        case types::vec: return cio_vect_str<&data::display>(vec);
        case types::exp: return cio_expr_str<&data::display>(exp);
        case types::map: return cio_hmap_str<&data::display>(map);
        case types::obj: return cio_obj_str<&data::display>(obj,"display");
        default:         return noexcept_write();
      }
    }

    scm_string pprint() const { // pretty-print (<write> w/ list indenting)
      switch(type) {
        case types::par: return pretty_print(*this);
        case types::vec: return cio_vect_str<&data::pprint>(vec);
        case types::exp: return cio_expr_str<&data::pprint>(exp);
        case types::map: return cio_hmap_str<&data::pprint>(map);
        case types::obj: return cio_obj_str<&data::pprint>(obj,"pprint");
        default:         return noexcept_write();
      }
    }

    // friend output function
    friend std::ostream& operator<<(std::ostream& outs, const data& d) noexcept {
      outs << d.noexcept_write();
      return outs;
    }

    // get type's name string
    constexpr const char* type_name() const noexcept {
      constexpr const char* const type_names[] = {
        "null", "expression", "pair", "number", "string", "character", "symbol", "vector",
        "boolean", "environment", "delay", "procedure", "input-port", "output-port", "void", 
        "syntax-rules", "hash-map", "class-prototype", "object", "process-invariants", "undefined"
      };
      return type_names[int(type) * (type!=types::sym || sym[0])]; // idx 0 for '() typename
    }

    // noexcept version of <equal> (doesn't invoke object equality members)
    bool noexcept_equal(const data& d) const noexcept {
      if(type != d.type) return false;
      switch(type) {
        case types::exp: return prm_compare_EXPRs<&data::noexcept_equal>(exp,d.exp);
        case types::par: return prm_compare_PAIRs<&data::noexcept_equal>(par,d.par);
        case types::vec: return prm_compare_VECTs<&data::noexcept_equal>(vec,d.vec);
        case types::map: return prm_compare_HMAPs<&data::noexcept_equal>(map,d.map);
        case types::obj: return prm_compare_OBJs <&data::noexcept_equal>(obj,d.obj);
        default: return prm_compare_atomic_values<&data::noexcept_equal>(*this,d,type);
      }
    }

    bool equal(const data& d) const { // equal?
      bool result = false;
      if(type == types::obj   && prm_DYNAMIC_OBJeq(obj,d,"equal?",result)) return result;
      if(d.type == types::obj && prm_DYNAMIC_OBJeq(d.obj,*this,"equal?",result)) return result;
      if(type != d.type) return false;
      switch(type) {
        case types::exp: return prm_compare_EXPRs<&data::equal>(exp,d.exp);
        case types::par: return prm_compare_PAIRs<&data::equal>(par,d.par);
        case types::vec: return prm_compare_VECTs<&data::equal>(vec,d.vec);
        case types::map: return prm_compare_HMAPs<&data::equal>(map,d.map);
        case types::obj: return prm_compare_OBJs <&data::equal>(obj,d.obj);
        default: return prm_compare_atomic_values<&data::equal>(*this,d,type);
      }
    }

    bool eqv(const data& d) const { // eqv?
      bool result = false;
      if(type == types::obj   && prm_DYNAMIC_OBJeq(obj,d,"eqv?",result)) return result;
      if(d.type == types::obj && prm_DYNAMIC_OBJeq(d.obj,*this,"eqv?",result)) return result;
      if(type != d.type) return false;
      switch(type) {
        case types::par: return prm_compare_PAIRs<&data::eq>(par,d.par);
        case types::vec: return prm_compare_VECTs<&data::eq>(vec,d.vec);
        case types::map: return prm_compare_HMAPs<&data::eq>(map,d.map);
        case types::obj: return prm_compare_OBJs <&data::eq>(obj,d.obj);
        default: return prm_compare_atomic_values<&data::equal>(*this,d,type);
      }
    }

    bool eq(const data& d) const { // eq?
      bool result = false;
      if(type == types::obj   && prm_DYNAMIC_OBJeq(obj,d,"eq?",result)) return result;
      if(d.type == types::obj && prm_DYNAMIC_OBJeq(d.obj,*this,"eq?",result)) return result;
      if(type != d.type) return false;
      if(type == types::str) return str == d.str;
      return prm_compare_atomic_values<&data::equal>(*this,d,type);
    }

    bool is_self_evaluating() const noexcept { // for the core evaluator
      return type != types::exp && type != types::sym;
    }

    // constructors
    data()                          noexcept {}
    data(const par_type& new_value) noexcept : type(types::par), par(new_value) {}
    data(const str_type& new_value) noexcept : type(types::str), str(new_value) {}
    data(const vec_type& new_value) noexcept : type(types::vec), vec(new_value) {}
    data(const env_type& new_value) noexcept : type(types::env), env(new_value) {}
    data(const del_type& new_value) noexcept : type(types::del), del(new_value) {}
    data(const num_type& new_value) noexcept : type(types::num), num(new_value) {}
    data(const exp_type& new_value) noexcept : type(types::exp), exp(new_value) {}
    data(const chr_type& new_value) noexcept : type(types::chr), chr(new_value) {}
    data(const char& new_value)     noexcept : type(types::chr), chr(new_value) {}
    data(const char* new_value)     noexcept : type(types::sym), sym(new_value) {}
    data(const sym_type& new_value) noexcept : type(types::sym), sym(new_value) {}
    data(const bol_type& new_value) noexcept : type(types::bol), bol(new_value) {}
    data(const fcn_type& new_value) noexcept : type(types::fcn), fcn(new_value) {}
    data(const fip_type& new_value) noexcept : type(types::fip), fip(new_value) {}
    data(const fop_type& new_value) noexcept : type(types::fop), fop(new_value) {}
    data(const syn_type& new_value) noexcept : type(types::syn), syn(new_value) {}
    data(const map_type& new_value) noexcept : type(types::map), map(new_value) {}
    data(const cls_type& new_value) noexcept : type(types::cls), cls(new_value) {}
    data(const obj_type& new_value) noexcept : type(types::obj), obj(new_value) {}
    data(const prc_type& new_value) noexcept : type(types::prc), prc(new_value) {}

    data(par_type&& new_value) noexcept : type(types::par), par(std::move(new_value)) {}
    data(str_type&& new_value) noexcept : type(types::str), str(std::move(new_value)) {}
    data(vec_type&& new_value) noexcept : type(types::vec), vec(std::move(new_value)) {}
    data(env_type&& new_value) noexcept : type(types::env), env(std::move(new_value)) {}
    data(del_type&& new_value) noexcept : type(types::del), del(std::move(new_value)) {}
    data(sym_type&& new_value) noexcept : type(types::sym), sym(std::move(new_value)) {}
    data(bol_type&& new_value) noexcept : type(types::bol), bol(std::move(new_value)) {}
    data(fcn_type&& new_value) noexcept : type(types::fcn), fcn(std::move(new_value)) {}
    data(fip_type&& new_value) noexcept : type(types::fip), fip(std::move(new_value)) {}
    data(fop_type&& new_value) noexcept : type(types::fop), fop(std::move(new_value)) {}
    data(syn_type&& new_value) noexcept : type(types::syn), syn(std::move(new_value)) {}
    data(num_type&& new_value) noexcept : type(types::num), num(std::move(new_value)) {}
    data(exp_type&& new_value) noexcept : type(types::exp), exp(std::move(new_value)) {}
    data(chr_type&& new_value) noexcept : type(types::chr), chr(std::move(new_value)) {}
    data(map_type&& new_value) noexcept : type(types::map), map(std::move(new_value)) {}
    data(cls_type&& new_value) noexcept : type(types::cls), cls(std::move(new_value)) {}
    data(obj_type&& new_value) noexcept : type(types::obj), obj(std::move(new_value)) {}
    data(prc_type&& new_value) noexcept : type(types::prc), prc(std::move(new_value)) {}

    data(const types& t) noexcept : type(t) {} // to set 'dne
    data(types&& t)      noexcept : type(t) {} // to set 'dne
    data(const data& d)  noexcept {*this = d;}
    data(data&& d)       noexcept {*this = std::move(d);}
    
    ~data() noexcept {
      switch(type) {
        case types::sym: sym.~sym_type(); return;
        case types::exp: exp.~exp_type(); return;
        case types::num: num.~num_type(); return;
        case types::par: par.~par_type(); return;
        case types::str: str.~str_type(); return;
        case types::vec: vec.~vec_type(); return;
        case types::env: env.~env_type(); return;
        case types::del: del.~del_type(); return;
        case types::fcn: fcn.~fcn_type(); return;
        case types::syn: syn.~syn_type(); return;
        case types::map: map.~map_type(); return;
        case types::cls: cls.~cls_type(); return;
        case types::obj: obj.~obj_type(); return;
        case types::prc: prc.~prc_type(); return;
        default:                          return;
      }
    }
  }; // End struct data

  // delay structure
  struct scm_delay {
    data datum;
    env_type env;
    bool already_forced, in_cps;
    data result;
    scm_delay(const data& delayed_datum = data(), env_type delay_env = nullptr, bool cps = false) noexcept
      : datum(delayed_datum), env(delay_env), already_forced(false), in_cps(cps), result(boolean(false)) {}
    void operator=(const scm_delay& d) noexcept {
      if(this == &d) return;
      datum=d.datum, in_cps=d.in_cps, env=d.env, already_forced=d.already_forced, result=d.result;
    }
    void operator=(scm_delay&& d) noexcept {
      if(this == &d) return;
      datum=std::move(d.datum), in_cps=std::move(d.in_cps), env=std::move(d.env);
      already_forced=std::move(d.already_forced), result=std::move(d.result);
    }
    scm_delay(const scm_delay& d) noexcept {*this = d;}
    scm_delay(scm_delay&& d)      noexcept {*this = std::move(d);}
    ~scm_delay()                  noexcept {}
  };

  // hash-map structure
  struct scm_map {
    std::unordered_map<sym_type,struct data> val;
    static bool hashable(const data& key)noexcept{
      return key.type==types::num||key.type==types::str||key.type==types::chr||
             key.type==types::sym||key.type==types::bol;
    }
    static data unhash_key(sym_type key)noexcept{ // unhash a key back into a datum
      types t = types(*key.rbegin());
      key.pop_back();
      switch(t) {
        case types::sym: return key;
        case types::num: return num_type(key);
        case types::str: return str_type(key);
        case types::bol: return boolean(key[1] == 't');
        case types::chr: return key[0];
        default:         return data(); // ONLY TRIGGERED IF GIVEN INVALID KEY
      }
    }
    static sym_type hash_key(const data& key)noexcept{
      return key.display()+char(key.type);
    }
    auto& operator[](const data& key)noexcept{ // PRECONDITION: hashable(key)
      return val[hash_key(key)];
    }
  };

  // class-prototype structure
  struct class_prototype {
    cls_type super = nullptr; // inherited proto
    env_type defn_env; // environment of class prototype definition
    scm_string class_name;
    std::vector<scm_string> member_names, method_names;
    scm_list member_values, method_values; // default member values of the proto
  };

  // object structure
  struct object_type {
    obj_type super = nullptr; // inherited proto subobject instance
    cls_type proto; // ptr to the prototype object
    std::vector<scm_string> member_names, method_names;
    scm_list member_values, method_values;
  };

  // struct data METHODS
  // returns a deep copy of *this ::= vector | string | pair | hmap | object
  data deep_copy_pair(const data& d);
  data deep_copy_obj(const data& d);
  data data::copy() const {
    scm_map m;
    scm_list new_vec;
    switch(type) {
      case types::str: return str_type(*str);
      case types::par: return deep_copy_pair(*this);
      case types::obj: return deep_copy_obj(*this);
      case types::vec:
        for(size_type i = 0, n = vec->size(); i < n; ++i)
          new_vec.push_back(vec->operator[](i).copy());
        return vec_type(std::move(new_vec));
      case types::map:
        for(const auto& keyval : map->val)
          m.val[keyval.first] = keyval.second.copy();
        return map_type(std::move(m));
      default: return *this;
    }
  }

  // returns a shallow copy of *this ::= vector | string | pair | hmap | object
  data shallow_copy_pair(const data& d);
  data shallow_copy_obj(const data& d);
  data data::shallow_copy() const {
    scm_map m;
    scm_list new_vec;
    switch(type) {
      case types::str: return str_type(*str);
      case types::par: return shallow_copy_pair(*this);
      case types::obj: return shallow_copy_obj(*this);
      case types::vec: return vec_type(*vec);
      case types::map: return map_type(*map);
      default: return *this;
    }
  }

  // compound procedure equality
  bool scm_fcn::operator==(const scm_fcn& f)const noexcept{
    if(is_primitive() || f.is_primitive())
      return prm == f.prm && param_instances.empty() && f.param_instances.empty();
    if(env != f.env || self != f.self || rec_depth != f.rec_depth || name != f.name || 
       flags != f.flags || param_instances.size() != f.param_instances.size())
       return false;
    for(size_type i = 0, n = param_instances.size(); i < n; ++i) {
      if(param_instances[i].size() != f.param_instances[i].size()) return false;
      for(size_type j = 0, m = param_instances[i].size(); j < m; ++j)
        if(!param_instances[i][j].equal(f.param_instances[i][j])) return false;
    }
    return true;
  }

  // compound procedure parameter list extraction
  frame_vars scm_fcn::lambda_parameters()const noexcept{
    frame_vars var_names;
    for(size_type i = 0, n = param_instances[0].size(); i < n; ++i)
      if(param_instances[0][i].is_type(types::sym))
        var_names.push_back(param_instances[0][i].sym);
    return var_names;
  }

  // procedure assignment
  void scm_fcn::operator=(const scm_fcn& f)noexcept{
    if(this == &f) return;
    name = f.name, param_instances = f.param_instances;
    if(f.is_primitive()) {
      prm = f.prm;
    } else {
      bodies = f.bodies, self = f.self, prm = nullptr;
      env = f.env, rec_depth = f.rec_depth, flags = f.flags;
    }
  }

  void scm_fcn::operator=(scm_fcn&& f)noexcept{
    if(this == &f) return;
    name = std::move(f.name), param_instances = std::move(f.param_instances);
    if(f.is_primitive()) {
      prm = std::move(f.prm);
    } else {
      bodies = std::move(f.bodies), self = std::move(f.self), prm = nullptr;
      env = std::move(f.env), rec_depth = std::move(f.rec_depth), flags = std::move(f.flags);
    }
  }

  /******************************************************************************
  * DATA TYPE GC CONSTRUCTORS
  ******************************************************************************/

  str_type make_str(const scm_string& o)                         noexcept{return str_type(o);}
  str_type make_str(scm_string&& o)                              noexcept{return str_type(std::move(o));}
  vec_type make_vec(const scm_list& o)                           noexcept{return vec_type(o);}
  vec_type make_vec(scm_list&& o)                                noexcept{return vec_type(std::move(o));}
  del_type make_del(const data& d,const env_type& e, bool in_cps)noexcept{return del_type(scm_delay(d,e,in_cps));}
  par_type make_par()                                            noexcept{return par_type(scm_pair());}
  map_type make_map(const scm_map& m)                            noexcept{return map_type(m);}
  map_type make_map(scm_map&& m)                                 noexcept{return map_type(std::move(m));}
  env_type make_env()                                            noexcept{return env_type(environment());}
  cls_type make_cls(const class_prototype& c)                    noexcept{return cls_type(c);}
  obj_type make_obj(const object_type& o)                        noexcept{return obj_type(o);}
  obj_type make_obj(object_type&& o)                             noexcept{return obj_type(std::move(o));}

  /******************************************************************************
  * GLOBAL PROCESS-DEPENDANT MUTABLE GLOBAL INVARIANTS
  ******************************************************************************/

  struct process_invariants_t {

    /******************************************************************************
    * MAX NUMBER OF RECURSIVE CALLS
    ******************************************************************************/

    size_type MAX_RECURSION_DEPTH = 1000; // see set-max-recursion-depth! primitive

    /******************************************************************************
    * PRETTY-PRINTER'S MAX COLUMN WIDTH
    ******************************************************************************/

    size_type PPRINT_MAX_COLUMN_WIDTH = 80; // see set-pprint-column-width! prim

    /******************************************************************************
    * WHETHER TO USE ANSI ESCAPE SEQUENCES TO FORMAT OUTPUT
    ******************************************************************************/

    bool USING_ANSI_ESCAPE_SEQUENCES = true; // see set-nansi! primitive

    /******************************************************************************
    * WHETHER "-cps" COMMAND LINE FLAG WAS PASSED
    ******************************************************************************/

    bool USING_CPS_CMD_LINE_FLAG = false;

    /******************************************************************************
    * WHETHER TRACING ALL FUNCTION CALLS (DEBUGGING HELPER)
    ******************************************************************************/

    bool TRACING_ALL_FUNCTION_CALLS = false; // see set-dynamic-call-trace!

    /******************************************************************************
    * NAME OF CURRENT TRACED FUNCTION (EMPTY = NO TRACE)
    ******************************************************************************/

    scm_string TRACED_FUNCTION_NAME = ""; // see trace primitive

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
    * CURRENT DEFAULT INPUT & OUTPUT PORTS
    ******************************************************************************/

    FILE* CURRENT_INPUT_PORT  = stdin;
    FILE* CURRENT_OUTPUT_PORT = stdout;

    /******************************************************************************
    * GENSYM UNIQUE HASHING KEYS
    ******************************************************************************/

    size_type GENSYM_HASH_IDX_1 = 0, GENSYM_HASH_IDX_2 = 0;

    /******************************************************************************
    * THE GLOBAL MACRO LABEL REGISTRY & MACRO/CPS HASH INDICES
    ******************************************************************************/

    std::vector<scm_string> MACRO_LABEL_REGISTRY; // optimizes procedure analysis

    size_type MACRO_HASH_IDX_1 = 0, MACRO_HASH_IDX_2 = 0;

    size_type CPS_HASH_IDX_1 = 0, CPS_HASH_IDX_2 = 0;

    /******************************************************************************
    * THE GLOBAL REGISTRY OF ANALYSIS-TIME GLOBAL MACRO LABELS
    ******************************************************************************/

    std::vector<scm_string> ANALYSIS_TIME_MACRO_LABEL_REGISTRY;

    /******************************************************************************
    * THE GLOBAL REGISTRY OF READER MACROS
    ******************************************************************************/

    std::vector<scm_string> SHORTHAND_READER_MACRO_REGISTRY = std::vector<scm_string>({"`@",",@","`","\\",",","'"});
    std::vector<scm_string> LONGHAND_READER_MACRO_REGISTRY = std::vector<scm_string>({
      "syntax-hash","unquote-splicing","quasiquote",symconst::reader_lambda,"unquote","quote"
    });

    /******************************************************************************
    * THE GLOBAL REGISTRY OF READER ALIASES
    ******************************************************************************/

    std::vector<scm_string> SHORTHAND_READER_ALIAS_REGISTRY, LONGHAND_READER_ALIAS_REGISTRY;

    /******************************************************************************
    * STACK TRACE MODIFIERS
    ******************************************************************************/

    bool TRACE_ARGS = false;

    size_type TRACE_LIMIT = 16;

    /******************************************************************************
    * INFIX SYMBOL READER TABLE
    ******************************************************************************/

    std::map<long long,infix_level_t> INFIX_TABLE;

    /******************************************************************************
    * GLOBAL ENVIRONMENT POINTER
    ******************************************************************************/

    env_type GLOBAL_ENVIRONMENT_POINTER = nullptr;

    /******************************************************************************
    * DOT CHARACTER FOR VARIADIC & PAIR-LITERAL DENOTATION
    ******************************************************************************/

    scm_string dot = "."; // see the "set-dot!" primitive

  }; // End of struct process_invariants_t

  /******************************************************************************
  * CURRENT PROCESS INVARIANTS SET
  ******************************************************************************/

  process_invariants_t G;

} // End of namespace heist

/******************************************************************************
* PRINTING/EQUALITY/DEEP-COPYING HELPER FUNCTIONS
******************************************************************************/

// Link toolkit here to inherit all of the above type defns
#include "toolkits/types_toolkit.hpp"

/******************************************************************************
* FN PARAMETER MATCHING & PROCEDURE ENVIRONMENT EXTENSION
******************************************************************************/

namespace heist {

  bool symbol_is_dot_operator(const sym_type& sym)noexcept{
    return sym == symconst::dot || sym == G.dot;
  }

  bool data_is_dot_operator(const data& d)noexcept{
    return d.is_type(types::sym) && symbol_is_dot_operator(d.sym);
  }

  namespace fn_param_matching {
    scm_string get_possible_signature(const exp_type& params)noexcept{
      scm_string buff;
      for(size_type i = 0, n = params.size(); i < n; ++i) {
        if(params[i].is_type(types::exp)) {
          if(!params[i].exp.empty() && params[i].exp[0].is_type(types::sym) && 
            (params[i].exp[0].sym == symconst::vec_literal || params[i].exp[0].sym == symconst::map_literal)){
            buff += params[i].exp[0].sym == symconst::vec_literal ? '#' : '$';
            buff += '(' + get_possible_signature(exp_type(params[i].exp.begin()+1,params[i].exp.end()));
          } else {
            buff += '(' + get_possible_signature(params[i].exp);
          }
        } else {
          buff += params[i].noexcept_write();
        }
        if(i+1 < n) buff += ' ';
      }
      return buff + ')';
    }

    scm_string get_possible_signatures(const std::vector<exp_type>& param_instances,const scm_string& name)noexcept{
      scm_string buff;
      for(size_type i = 0, n = param_instances.size(); i < n; ++i) {
        if(param_instances[i].empty())
          buff += "\n        (" + name + ')';
        else
          buff += "\n        (" + name + ' ' + get_possible_signature(param_instances[i]);
      }
      return buff;
    }

    bool param_is_token(const data& d)noexcept{
      return d.is_type(types::sym);
    }

    // PRECONDITION: param_is_token(d)
    bool param_is_boolean_literal(const data& d)noexcept{ 
      return d.sym == "#f" || d.sym == "#t";
    }

    bool param_boolean_mismatch(const sym_type& sym, const data& arg)noexcept{ 
      return (sym == "#f") ^ (arg.is_type(types::bol) && !arg.bol.val);
    }

    bool param_is_symbol_literal(const data& d)noexcept{
      return d.is_type(types::exp) && d.exp.size() == 2 && 
             d.exp[0].is_type(types::sym) && d.exp[1].is_type(types::sym) && 
             d.exp[0].sym == symconst::quote;
    }

    bool param_symbol_mismatch(const sym_type& sym, const data& arg)noexcept{
      return !arg.is_type(types::sym) || sym != arg.sym;
    }

    bool param_is_non_container_literal(const data& d)noexcept{
      return !d.is_type(types::exp);
    }

    bool param_non_container_literal_mismatch(const data& lhs, const data& rhs)noexcept{
      if(lhs.type != rhs.type) return true;
      if(lhs.is_type(types::num)) return lhs.num != rhs.num; // use = for numbers (* COMMENT THIS LINE OUT FOR EXACTNESS SENSITIVITY *)
      return !lhs.noexcept_equal(rhs); // noexcept equal since only matching against literals (ie no objects)
    }

    bool param_is_vector_literal(const data& d)noexcept{ // PRECONDITION: d.is_type(types::exp)
      return !d.exp.empty() && d.exp[0].is_type(types::sym) && d.exp[0].sym == symconst::vec_literal;
    }

    bool param_is_hmap_literal(const data& d)noexcept{ // PRECONDITION: d.is_type(types::exp)
      return !d.exp.empty() && d.exp[0].is_type(types::sym) && d.exp[0].sym == symconst::map_literal;
    }


    bool param_parse_hmap_literal(const data&,data&,exp_type&,frame_vars&)noexcept;
    bool param_parse_list_literal(const data&,data&,exp_type&,frame_vars&)noexcept;

    bool param_parse_vector_literal(const data& vec, data& arg, exp_type& values, frame_vars& unpacked_params)noexcept{
      if(!arg.is_type(types::vec)) return false;
      if(vec.exp.size() != arg.vec->size()+1) return false;
      for(size_type i = 1, j = 0, n = vec.exp.size(); i < n; ++i, ++j) {
        // tokens match anything
        if(param_is_token(vec.exp[i])) {
          // booleans are a specialized token instance
          if(param_is_boolean_literal(vec.exp[i])) {
            if(param_boolean_mismatch(vec.exp[i].sym,arg.vec->operator[](j))) return false;
          } else {
            unpacked_params.push_back(vec.exp[i].sym);
            values.push_back(arg.vec->operator[](j));
          }
        // match against quoted non-nil literal symbols
        } else if(param_is_symbol_literal(vec.exp[i])) {
          if(param_symbol_mismatch(vec.exp[i].exp[1].sym,arg.vec->operator[](j))) return false;
        // match against non-container literals
        } else if(param_is_non_container_literal(vec.exp[i])) {
          if(param_non_container_literal_mismatch(vec.exp[i],arg.vec->operator[](j))) return false;
        // match against vector literals
        } else if(param_is_vector_literal(vec.exp[i])) {
          if(!param_parse_vector_literal(vec.exp[i],arg.vec->operator[](j),values,unpacked_params)) return false;
        // match against hmap literals
        } else if(param_is_hmap_literal(vec.exp[i])) {
          if(!param_parse_hmap_literal(vec.exp[i],arg.vec->operator[](j),values,unpacked_params)) return false;
        // match against list literals
        } else {
          if(!param_parse_list_literal(vec.exp[i],arg.vec->operator[](j),values,unpacked_params)) return false;
        }
      }
      return true;
    }

    bool param_parse_hmap_literal(const data& map, data& arg, exp_type& values, frame_vars& unpacked_params)noexcept{
      if(!arg.is_type(types::map)) return false;
      if((map.exp.size()-1)/2 != arg.map->val.size()) return false;
      auto iter = arg.map->val.begin();
      for(size_type i = 1, n = map.exp.size(); i < n; i += 2, ++iter) {
        data elt = scm_map::unhash_key(iter->first);
        for(size_type offset = 0; offset < 2; ++offset) {
          // tokens match anything
          if(param_is_token(map.exp[i+offset])) {
            // booleans are a specialized token instance
            if(param_is_boolean_literal(map.exp[i+offset])) {
              if(param_boolean_mismatch(map.exp[i+offset].sym,elt)) return false;
            } else {
              unpacked_params.push_back(map.exp[i+offset].sym);
              values.push_back(elt);
            }
          // match against quoted non-nil literal symbols
          } else if(param_is_symbol_literal(map.exp[i+offset])) {
            if(param_symbol_mismatch(map.exp[i+offset].exp[1].sym,elt)) return false;
          // match against non-container literals
          } else if(param_is_non_container_literal(map.exp[i+offset])) {
            if(param_non_container_literal_mismatch(map.exp[i+offset],elt)) return false;
          // match against vector literals
          } else if(param_is_vector_literal(map.exp[i+offset])) {
            if(!param_parse_vector_literal(map.exp[i+offset],elt,values,unpacked_params)) return false;
          // match against hmap literals
          } else if(param_is_hmap_literal(map.exp[i+offset])) {
            if(!param_parse_hmap_literal(map.exp[i+offset],elt,values,unpacked_params)) return false;
          // match against list literals
          } else {
            if(!param_parse_list_literal(map.exp[i+offset],elt,values,unpacked_params)) return false;
          }
          if(!offset) elt = iter->second;
        }
      }
      return true;
    }

    bool param_parse_list_literal(const data& lst, data& arg, exp_type& values, frame_vars& unpacked_params)noexcept{
      if(lst.exp.empty()) return arg.is_type(types::sym) && arg.sym == symconst::emptylist; // match NIL
      if(!arg.is_type(types::par)) return false;
      size_type i = 0, n = lst.exp.size();
      auto iter = arg;
      for(; i < n; ++i) {
        // dotted list may match all remaining values
        if(data_is_dot_operator(lst.exp[i])) {
          // pattern match dotted elt
          if(param_is_token(lst.exp[i+1])) {
            // booleans are a specialized token instance
            if(param_is_boolean_literal(lst.exp[i+1])) {
              return !param_boolean_mismatch(lst.exp[i+1].sym,iter);
            } else {
              unpacked_params.push_back(lst.exp[i+1].sym);
              values.push_back(iter);
              return true;
            }
          } 
          // match against quoted non-nil literal symbols
          if(param_is_symbol_literal(lst.exp[i+1]))
            return !param_symbol_mismatch(lst.exp[i+1].exp[1].sym,iter);
          // match against non-container literals
          if(param_is_non_container_literal(lst.exp[i+1]))
            return !param_non_container_literal_mismatch(lst.exp[i+1],iter);
          // match against vector literals
          if(param_is_vector_literal(lst.exp[i+1]))
            return param_parse_vector_literal(lst.exp[i+1],iter,values,unpacked_params);
          // match against hmap literals
          if(param_is_hmap_literal(lst.exp[i+1]))
            return param_parse_hmap_literal(lst.exp[i+1],iter,values,unpacked_params);
          // match against list literals
          return param_parse_list_literal(lst.exp[i+1],iter,values,unpacked_params);
        }
        // non-dotted list must currently match against a pair elt
        if(!iter.is_type(types::par)) return false;
        // tokens match anything
        if(param_is_token(lst.exp[i])) {
          // booleans are a specialized token instance
          if(param_is_boolean_literal(lst.exp[i])) {
            if(param_boolean_mismatch(lst.exp[i].sym,iter.par->first)) return false;
          } else {
            unpacked_params.push_back(lst.exp[i].sym);
            values.push_back(iter.par->first);
          }
        // match against quoted non-nil literal symbols
        } else if(param_is_symbol_literal(lst.exp[i])) {
          if(param_symbol_mismatch(lst.exp[i].exp[1].sym,iter.par->first)) return false;
        // match against non-container literals
        } else if(param_is_non_container_literal(lst.exp[i])) {
          if(param_non_container_literal_mismatch(lst.exp[i],iter.par->first)) return false;
        // match against vector literals
        } else if(param_is_vector_literal(lst.exp[i])) {
          if(!param_parse_vector_literal(lst.exp[i],iter.par->first,values,unpacked_params)) return false;
        // match against hmap literals
        } else if(param_is_hmap_literal(lst.exp[i])) {
          if(!param_parse_hmap_literal(lst.exp[i],iter.par->first,values,unpacked_params)) return false;
        // match against list literals
        } else {
          if(!param_parse_list_literal(lst.exp[i],iter.par->first,values,unpacked_params)) return false;
        }
        iter = iter.par->second;
      }
      return i == n && iter.is_type(types::sym) && iter.sym == symconst::emptylist;
    }


    // ACCOUNT FOR:
    // 0. VARIADICS VIA "."
    // 1. PARAMETER SYMBOLIC TOKENS (MATCH AGAINST ANYTHING)
    // 2. MATCHING SYMBOLS AGAINST QUOTED SYMBOL LITERALS (NOT NIL: FN PARAMETER SHOULD BE () NOT '())
    // 3. MATCHING NON-CONTAINER NON-QUOTED-SYMBOL LITERALS
    // 4. MATCHING & UPPACKING LIST/VECTOR/HMAP CONTAINER LITERALS
    bool is_fn_call_match(const exp_type& params, exp_type& arguments, exp_type& values, frame_vars& unpacked_params)noexcept{
      // confirm emptiness match
      if((params.empty() || !data_is_dot_operator(params[0])) && (params.empty() ^ arguments.empty())) {
        return false;
      } else if(params.empty()) {
        return true;
      }
      size_type i = 0, j = 0, n = params.size(), m = arguments.size();
      for(; i < n && j < m+1; ++i, ++j) { // j < m+1 to match no args against varaidic args
        // variadics match all remaining values
        if(data_is_dot_operator(params[i])) {
          for(auto iter = params.begin()+i, end = params.end(); iter != end; ++iter)
            unpacked_params.push_back(iter->sym);
          values.insert(values.end(),arguments.begin()+j,arguments.end());
          return true;
        }
        if(j == m) return false;
        // tokens match anything
        if(param_is_token(params[i])) {
          // booleans are a specialized token instance
          if(param_is_boolean_literal(params[i])) {
            if(param_boolean_mismatch(params[i].sym,arguments[j])) return false;
          } else {
            unpacked_params.push_back(params[i].sym);
            values.push_back(arguments[j]);
          }
        // match against quoted non-nil literal symbols
        } else if(param_is_symbol_literal(params[i])) {
          if(param_symbol_mismatch(params[i].exp[1].sym,arguments[j])) return false;
        // match against non-container literals
        } else if(param_is_non_container_literal(params[i])) {
          if(param_non_container_literal_mismatch(params[i],arguments[j])) return false;
        // match against vector literals
        } else if(param_is_vector_literal(params[i])) {
          if(!param_parse_vector_literal(params[i],arguments[j],values,unpacked_params)) return false;
        // match against hmap literals
        } else if(param_is_hmap_literal(params[i])) {
          if(!param_parse_hmap_literal(params[i],arguments[j],values,unpacked_params)) return false;
        // match against list literals
        } else {
          if(!param_parse_list_literal(params[i],arguments[j],values,unpacked_params)) return false;
        }
      }
      return i == n && j == m;
    }

    void match_fn_call_signature(const std::vector<exp_type>& param_instances, const std::vector<exe_fcn_t>& bodies, 
                                 const scm_string& name,exp_type& arguments, exp_type& values,frame_vars& unpacked_params, exe_fcn_t& body){
      for(size_type i = 0, n = param_instances.size(); i < n; ++i) {
        if(is_fn_call_match(param_instances[i],arguments,values,unpacked_params)) {
          body = bodies[i];
          return;
        }
        unpacked_params.clear(), values.clear();
      }
      THROW_ERR("'fn arguments "<<data(arguments)<<" don't match any signatures!\n     -> Possible Signatures:" 
        << get_possible_signatures(param_instances,name) << FCN_ERR(name,arguments));
    }
  }; // End of namespace fn_param_matching


  // get the extended environment for the compound procedure given <arguments>
  env_type scm_fcn::get_extended_environment(exp_type& arguments, exe_fcn_t& body){
    // extend the lambda environment
    env_type extend_environment(frame_vars&&,frame_vals&,env_type&,const sym_type&);
    if(is_lambda()) {
      body = bodies[0];
      return extend_environment(lambda_parameters(), arguments, env, name);
    }
    // extend the fn environment
    frame_vars unpacked_params;
    exp_type values;
    fn_param_matching::match_fn_call_signature(param_instances,bodies,printable_procedure_name(),arguments,values,unpacked_params,body);
    return extend_environment(std::move(unpacked_params), values, env, name);
  }

  /******************************************************************************
  * ENVIRONMENT TRAVERSAL & ACCESS METHODS
  ******************************************************************************/

  // Environmental Traversal
  frame_val environment::lookup_variable_value(const frame_var& var, bool& found)const noexcept{
    const auto& vars = variables();
    for(size_type i = 0, n = vars.size(); i < n; ++i) {
      if(var == vars[i]) {
        found = true;
        return values()[i];
      }
    }
    if(parent) return parent->lookup_variable_value(var,found);
    found = false;
    return frame_val();
  }

  // Returns whether found
  bool environment::set_variable_value(const frame_var& var, frame_val&& val)noexcept{
    auto& vars = variables();
    for(size_type i = 0, n = vars.size(); i < n; ++i) {
      if(var == vars[i]) {
        // binding anonymous procedures -> named procedure
        if(val.is_type(types::fcn) && val.fcn.name.empty()) val.fcn.name = var;
        values()[i] = val;
        return true;
      }
    }
    return parent && parent->set_variable_value(var,std::move(val));
  }

  void environment::define_variable(const frame_var& var, frame_val&& val)noexcept{
    // binding anonymous procedures -> named procedure
    if(val.is_type(types::fcn) && val.fcn.name.empty()) val.fcn.name = var;
    auto& vars = variables();
    for(size_type i = 0, n = vars.size(); i < n; ++i) {
      if(var == vars[i]) {
        values()[i] = val;
        return;
      }
    }
    vars.push_back(var);
    values().push_back(val);
  }

  void environment::define_macro(const frame_mac& mac_val)noexcept{
    auto& macs = macros();
    for(auto& mac : macs) {
      if(mac.label == mac_val.label) {
        mac = mac_val;
        return;
      }
    }
    macs.push_back(mac_val);
  }

  // Get variable's name as a string
  scm_string environment::getenv(const frame_var& var, bool& found)const{
    const auto& vars = variables();
    for(size_type i = 0, n = vars.size(); i < n; ++i) {
      if(var == vars[i]) {
        found = true;
        return values()[i].write();
      }
    }
    if(parent) return parent->getenv(var,found);
    found = false;
    return "";
  }

  bool environment::has_macro(const scm_string& label)const noexcept{
    for(const auto& mac : macros())
      if(label == mac.label) return true;
    return parent && parent->has_macro(label);
  }

  bool environment::has_variable(const frame_var& var)const noexcept{
    for(const auto& v : variables())
      if(var == v) return true;
    return parent && parent->has_variable(var);
  }

  // Returns whether found
  bool environment::erase_variable(const frame_var& var)noexcept{
    auto& vars = variables();
    for(size_type i = 0, n = vars.size(); i < n; ++i) {
      if(var == vars[i]) {
        auto& vals = values();
        vars.erase(vars.begin()+i);
        vals.erase(vals.begin()+i);
        return true;
      }
    }
    return parent && parent->erase_variable(var);
  }

  // Returns whether found
  bool environment::erase_macro(const scm_string& label)noexcept{
    auto& macs = macros();
    for(size_type i = 0, n = macs.size(); i < n; ++i) {
      if(label == macs[i].label) {
        macs.erase(macs.begin()+i);
        return true;
      }
    }
    return parent && parent->erase_macro(label);
  }

  // Returns whether found
  bool environment::relabel_macro(const scm_string& old_label, const scm_string& new_label)noexcept{
    for(auto& mac : macros()) {
      if(old_label == mac.label) {
        mac.label = new_label;
        for(auto& templ8 : mac.templates)
          relabel_recursive_calls_in_macro_template(old_label,new_label,templ8);
        return true;
      }
    }
    return parent && parent->relabel_macro(old_label,new_label);
  }


  // <relabel_macro> helper function
  void environment::relabel_recursive_calls_in_macro_template(const scm_string& old_label,const scm_string& new_label,scm_list& templ8)noexcept{
    for(auto& datum : templ8) {
      if(datum.is_type(types::exp))
        relabel_recursive_calls_in_macro_template(old_label,new_label,datum.exp);
      else if(datum.is_type(types::sym) && datum.sym == old_label)
        datum.sym = new_label;
    }
  }
}; // End of namespace heist

#endif