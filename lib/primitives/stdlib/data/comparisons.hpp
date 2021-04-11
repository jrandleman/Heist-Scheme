// Author: Jordan Randleman -- jordanran199@gmail.com -- comparisons.hpp
// => Defines primitive comparison functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_COMPARISONS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_COMPARISONS_HPP_

namespace heist {
  // primitive "eq?" procedure:
  data primitive_EQP(data_vector&& args) {
    if(args.empty()) HEIST_THROW_ERR("'eq? received no arguments: (eq? <obj1> <obj2> ...)" << HEIST_FCN_ERR("eq?", args));
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_EQP,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(!args[i].eq(args[i+1])) 
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "eqv?" procedure:
  data primitive_EQVP(data_vector&& args) {
    if(args.empty()) HEIST_THROW_ERR("'eqv? received no arguments: (eqv? <obj1> <obj2> ...)" << HEIST_FCN_ERR("eqv?", args));
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_EQVP,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(!args[i].eqv(args[i+1])) 
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "equal?" procedure:
  data primitive_EQUALP(data_vector&& args) {
    if(args.empty()) HEIST_THROW_ERR("'equal? received no arguments: (equal? <obj1> <obj2> ...)" << HEIST_FCN_ERR("equal?", args));
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_EQUALP,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(!args[i].equal(args[i+1])) 
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "not" procedure:
  data primitive_NOT(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'not didn't recieve exactly 1 arg: (not <obj>)" << HEIST_FCN_ERR("not",args));
    return boolean(args[0].is_falsey());
  }
} // End of namespace heist

#endif