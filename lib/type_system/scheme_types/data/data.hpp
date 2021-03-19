// Author: Jordan Randleman -- jrandleman@scu.edu -- data.hpp
// => Contains Heist-Scheme-Object "data" data structure for the C++ Heist Scheme Interpreter

#ifndef HEIST_DATA_HPP_
#define HEIST_DATA_HPP_

namespace heist {

  /******************************************************************************
  * FUNDAMENTAL SCHEME DATUM INTERNAL UNION TYPES
  ******************************************************************************/

  // enum of "struct data"'s union types
  // => expression, pair, number, string, character, symbol, vector, boolean, environment, delay, procedure (compound & primitive),
  //    input port, output port, does-not-exist, syntax-rules, hash-map, class-prototype, object, process, undefined value
  enum class types {exp=1, par, num, str, chr, sym, vec, bol, env, del, fcn, fip, fop, dne, syn, map, cls, obj, prc, undefined};

  /******************************************************************************
  * CORE SCHEME OBJECT DATA TYPE STRUCT
  ******************************************************************************/

  // data_obj.type                  => current type enum
  // data_obj.type_name()           => current type's name (string)
  // data_obj.<T>                   => current type <T> value
  // data_obj.is_type(T)            => data_obj.type == T
  // data_obj.pointer_address()     => ptr address as a string (if exists, else "")
  //
  // data_obj.noexcept_write()      => <write> but w/o invoking object methods
  // data_obj.write()               => data_obj's value as a machine-readable string
  // data_obj.display()             => data_obj's value as a human-readable string
  // data_obj.pprint()              => <write> with auto-indentation for lists
  //
  // data_obj.eq(datum)             => eq? shallow equality
  // data_obj.eqv(datum)            => eqv? shallow equality + deep equality for strings
  // data_obj.equal(datum)          => equal? deep equality
  // data_obj.noexcept_equal(datum) => <equal?> but w/o invoking object methods
  //
  // data_obj.is_self_evaluating()  => core evaluator should reflect datum
  //
  // data_obj.copy()                => deep-cpy vector|string|pair|hmap|object
  // data_obj.shallow_copy()        => shallow-cpy vector|string|pair|hmap|object
  //
  // data_obj.is_falsey()           => <data_obj> is a falsey value
  // data_obj.is_truthy()           => <data_obj> is NOT a falsey value
  
  struct data {
    // current type (& type checking mechanism) of the data object
    types type = types::undefined;
    bool is_type(const types& t) const noexcept {return type == t;}

    // current value of the data object
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

    // returns a deep copy of *this ::= vector | string | pair | hmap | object
    data copy() const;

    // returns a shallow copy of *this ::= vector | string | pair | hmap | object
    data shallow_copy() const;

    // assignment operator
    void operator=(const data& d) noexcept;
    void operator=(data&& d) noexcept;

    // value stringification
    string noexcept_write() const noexcept; // doesn't invoke object printing members
    string write()   const;                 // machine-readable string
    string display() const;                 // human-readable string
    string pprint()  const;                 // pretty-print (<write> w/ list indenting)
    friend std::ostream& operator<<(std::ostream& outs, const data& d) noexcept {
      outs << d.noexcept_write();
      return outs;
    }

    // pointer-address stringification (if exists)
    string pointer_address()const noexcept;

    // typename stringification
    const char* type_name()const noexcept;

    // noexcept version of <equal> (doesn't invoke object equality members)
    bool noexcept_equal(const data& d) const noexcept;
    bool equal(const data& d) const; // equal?
    bool eqv(const data& d)   const; // eqv?
    bool eq(const data& d)    const; // eq?

    // self evaluation determination (for the core evaluator)
    bool is_self_evaluating() const noexcept {return type != types::exp && type != types::sym;}

    // falsiness/truthiness evaluation
    bool is_falsey() const;
    bool is_truthy() const {return !is_falsey();}

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
    
    // destructor
    ~data() noexcept {
      switch(type) {
        case types::sym: sym.~sym_type(); return;
        case types::exp: exp.~exp_type(); return;
        case types::par: par.~par_type(); return;
        case types::num: num.~num_type(); return;
        case types::str: str.~str_type(); return;
        case types::chr: chr.~chr_type(); return;
        case types::vec: vec.~vec_type(); return;
        case types::bol: bol.~bol_type(); return;
        case types::env: env.~env_type(); return;
        case types::del: del.~del_type(); return;
        case types::fcn: fcn.~fcn_type(); return;
        case types::fip: fip.~fip_type(); return;
        case types::fop: fop.~fop_type(); return;
        case types::syn: syn.~syn_type(); return;
        case types::map: map.~map_type(); return;
        case types::cls: cls.~cls_type(); return;
        case types::obj: obj.~obj_type(); return;
        case types::prc: prc.~prc_type(); return;
        default: return; // types::undefined, types::dne 
      }
    }
  }; // End struct data
}

#endif