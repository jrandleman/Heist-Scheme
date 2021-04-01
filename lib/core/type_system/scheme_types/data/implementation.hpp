// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Contains method implementations of "struct data" for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_DATA_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_DATA_IMPLEMENTATION_HPP_

namespace heist {

  /******************************************************************************
  * HELPER STRINGIFICATION, EQUALITY, & COPYING DEPENDANCIES
  ******************************************************************************/

  #include "implementation_helpers/dynamic_method_applicator.hpp"
  #include "implementation_helpers/stringification.hpp"
  #include "implementation_helpers/equality.hpp"
  #include "implementation_helpers/copying.hpp"

  /******************************************************************************
  * ASSIGNMENT OPERATOR
  ******************************************************************************/

  // assignment operator (l-value reference)
  void data::operator=(const data& d) noexcept {
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


  // assignment operator (r-value reference)
  void data::operator=(data&& d) noexcept {
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

  /******************************************************************************
  * VALUE STRINGIFICATION FUNCTIONS
  ******************************************************************************/

  // noexcept version of <write> (doesn't invoke object printing members)
  string data::noexcept_write() const noexcept {
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
            if(isprint(chr)) {
              return string("#\\") + char(chr);
            } else {
              char str[32];
              snprintf(str, 32, "#\\x%x", chr);
              return string(str);
            }
        }
      case types::num: return num.str();
      case types::str: return '"' + escape_chars(*str) + '"';
      case types::bol: return bol.str();
      case types::fcn: return fcn.str();
      case types::cls: return "#<class-prototype[0x"+pointer_to_hexstring(cls.ptr)+"]>";
      case types::obj: return "#<object[0x"+pointer_to_hexstring(obj.ptr)+"]>";
      case types::env: return "#<environment[0x"+pointer_to_hexstring(env.ptr)+"]>";
      case types::prc: return "#<process-invariants[0x"+pointer_to_hexstring(prc.ptr)+"]>";
      case types::del: return "#<delay[0x"+pointer_to_hexstring(del.ptr)+"]>";
      case types::fip: return fip.str();
      case types::fop: return fop.str();
      case types::dne: return "";
      case types::syn: return syn.str();
      case types::par: return stringify_list<&data::noexcept_write>(*this);
      case types::vec: return stringify_vect<&data::noexcept_write>(vec);
      case types::exp: return stringify_expr<&data::noexcept_write>(exp);
      case types::map: return stringify_hmap<&data::noexcept_write>(map);
      default:         return "#<undefined>"; // types::undefined
    }
  }


  string data::write() const { // machine-readable string
    switch(type) {
      case types::par: return stringify_list<&data::write>(*this);
      case types::vec: return stringify_vect<&data::write>(vec);
      case types::exp: return stringify_expr<&data::write>(exp);
      case types::map: return stringify_hmap<&data::write>(map);
      case types::obj: return stringify_obj<&data::write>(obj,"write");
      default: return noexcept_write();
    }
  }


  string data::display() const { // human-readable string
    switch(type) {
      case types::chr: return string(1,chr);
      case types::str: return *str;
      case types::par: return stringify_list<&data::display>(*this);
      case types::vec: return stringify_vect<&data::display>(vec);
      case types::exp: return stringify_expr<&data::display>(exp);
      case types::map: return stringify_hmap<&data::display>(map);
      case types::obj: return stringify_obj<&data::display>(obj,"display");
      default:         return noexcept_write();
    }
  }


  string data::pprint() const { // pretty-print (<write> w/ list indenting)
    switch(type) {
      case types::par: return pretty_print(*this);
      case types::vec: return stringify_vect<&data::pprint>(vec);
      case types::exp: return stringify_expr<&data::pprint>(exp);
      case types::map: return stringify_hmap<&data::pprint>(map);
      case types::obj: return stringify_obj<&data::pprint>(obj,"pprint");
      default:         return noexcept_write();
    }
  }

  /******************************************************************************
  * POINTER-ADDRESS STRINGIFICATION FUNCTION
  ******************************************************************************/

  // get ptr address as a string (if exists)
  string data::pointer_address()const noexcept{
    switch(type) {
      case types::par: return "0x" + pointer_to_hexstring(par.ptr);
      case types::str: return "0x" + pointer_to_hexstring(str.ptr);
      case types::vec: return "0x" + pointer_to_hexstring(vec.ptr);
      case types::env: return "0x" + pointer_to_hexstring(env.ptr);
      case types::del: return "0x" + pointer_to_hexstring(del.ptr);
      case types::map: return "0x" + pointer_to_hexstring(map.ptr);
      case types::cls: return "0x" + pointer_to_hexstring(cls.ptr);
      case types::obj: return "0x" + pointer_to_hexstring(obj.ptr);
      case types::prc: return "0x" + pointer_to_hexstring(prc.ptr);
      case types::fip: return "0x" + pointer_to_hexstring(fip.fp.ptr);
      case types::fop: return "0x" + pointer_to_hexstring(fop.fp.ptr);
      default: return "";
    }
  }

  /******************************************************************************
  * TYPENAME STRINGIFICATION FUNCTION
  ******************************************************************************/

  const char* data::type_name() const noexcept {
    static const char * const type_names[] = {
      "null", "expression", "pair", "number", "string", "character", "symbol", "vector",
      "boolean", "environment", "delay", "procedure", "input-port", "output-port", "void", 
      "syntax-rules", "hash-map", "class-prototype", "object", "process-invariants", "undefined"
    };
    return type_names[int(type) * (type!=types::sym || sym[0])]; // idx 0 for '() typename
  }

  /******************************************************************************
  * VALUE EQUALITY FUNCTIONS
  ******************************************************************************/

  // noexcept version of <equal> (doesn't invoke object equality members)
  bool data::noexcept_equal(const data& d) const noexcept {
    if(type != d.type) return false;
    switch(type) {
      case types::exp: return prm_compare_EXPRs<&data::noexcept_equal>(exp,d.exp);
      case types::par: return prm_compare_PAIRs<&data::noexcept_equal>(par,d.par);
      case types::vec: return prm_compare_VECTs<&data::noexcept_equal>(vec,d.vec);
      case types::map: return prm_compare_HMAPs<&data::noexcept_equal>(map,d.map);
      case types::obj: return prm_compare_OBJs<&data::noexcept_equal>(obj,d.obj);
      case types::str: return *d.str == *str;
      default:         return eq(d);
    }
  }


  bool data::equal(const data& d) const { // equal?
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
      case types::str: return *d.str == *str;
      default: return eq(d);
    }
  }


  bool data::eqv(const data& d) const { // eqv?
    bool result = false;
    if(type == types::obj   && prm_DYNAMIC_OBJeq(obj,d,"eqv?",result)) return result;
    if(d.type == types::obj && prm_DYNAMIC_OBJeq(d.obj,*this,"eqv?",result)) return result;
    if(type != d.type) return false;
    switch(type) {
      case types::exp: return prm_compare_EXPRs<&data::equal>(exp,d.exp);
      case types::par: return prm_compare_PAIRs<&data::eq>(par,d.par);
      case types::vec: return prm_compare_VECTs<&data::eq>(vec,d.vec);
      case types::map: return prm_compare_HMAPs<&data::eq>(map,d.map);
      case types::obj: return prm_compare_OBJs <&data::eq>(obj,d.obj);
      case types::str: return *d.str == *str;
      default: return eq(d);
    }
  }


  bool data::eq(const data& d) const { // eq?
    bool result = false;
    if(type == types::obj   && prm_DYNAMIC_OBJeq(obj,d,"eq?",result)) return result;
    if(d.type == types::obj && prm_DYNAMIC_OBJeq(d.obj,*this,"eq?",result)) return result;
    if(type != d.type) return false;
    switch(type) {
      case types::sym: return d.sym == sym;
      case types::num: return d.num.is_exact() == num.is_exact() && d.num == num;
      case types::str: return d.str == str;
      case types::par: return par == d.par;
      case types::obj: return obj == d.obj;
      case types::vec: return vec == d.vec;
      case types::map: return map == d.map;
      case types::exp: return prm_compare_EXPRs<&data::equal>(exp,d.exp);
      case types::chr: return d.chr == chr;
      case types::bol: return d.bol == bol;
      case types::env: return d.env == env;
      case types::del: return d.del == del;
      case types::fcn: return d.fcn == fcn;
      case types::fip: return d.fip == fip;
      case types::fop: return d.fop == fop;
      case types::syn: return d.syn == syn;
      case types::cls: return d.cls == cls;
      case types::prc: return d.prc == prc;
      default:         return true; // types::undefined, types::dne
    }
  }

  /******************************************************************************
  * COPYING (DEEP & SHALLOW/STRUCTURAL)
  ******************************************************************************/

  // struct data METHODS
  // returns a deep copy of *this ::= vector | string | pair | hmap | object
  data data::copy() const {
    map_object m;
    data_vector new_vec;
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
  data data::shallow_copy() const {
    map_object m;
    data_vector new_vec;
    switch(type) {
      case types::str: return str_type(*str);
      case types::par: return shallow_copy_pair(*this);
      case types::obj: return shallow_copy_obj(*this);
      case types::vec: return vec_type(*vec);
      case types::map: return map_type(*map);
      default: return *this;
    }
  }

  /******************************************************************************
  * FALSINESS DETERMINATION
  ******************************************************************************/

  bool data::is_falsey() const {
    for(const auto& d : G.FALSEY_VALUES)
      if(equal(d)) return true;
    return false;
  }
}

#endif