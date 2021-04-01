// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for universes.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_UNIVERSES_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_UNIVERSES_IMPLEMENTATION_HPP_

namespace heist::stdlib_universes {

  cls_type get_universe_class_prototype(data_vector& args, const char* format) {
    bool found = false;
    auto val = G.GLOBAL_ENVIRONMENT_POINTER->lookup_variable_value("universe", found);
    if(!found || !val.is_type(types::cls)) 
      HEIST_THROW_ERR("'heist:core:universe:eval 'universe symbol isn't bound to a class prototype!"
        << format << HEIST_FCN_ERR("heist:core:universe:eval",args)); 
    return val.cls;
  }

} // End of namespace heist::stdlib_universes

#endif