// Author: Jordan Randleman -- jordanran199@gmail.com -- implementation.hpp
// => Defines helper functions for hmaps.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_HMAPS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_HMAPS_IMPLEMENTATION_HPP_


// <HEIST_HASH_MAP_KEY_FORMAT> is #undef'd in "hmaps.hpp"
#define HEIST_HASH_MAP_KEY_FORMAT\
    "\n     => <key> ::= <number>"\
    "\n                | <string>"\
    "\n                | <character>"\
    "\n                | <symbol>"\
    "\n                | <boolean>"


namespace heist::stdlib_hmaps {

  /******************************************************************************
  * HASHMAP VALIDATION
  ******************************************************************************/

  void hmap_confirm_unary_map(const char* name, const char* format, data_vector& args){
    if(args.size() != 1) 
      HEIST_THROW_ERR('\''<<name<<" didn't receive 1 arg!" 
        << format << HEIST_FCN_ERR(name, args));
    if(!args[0].is_type(types::map))
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't a hash-map!" 
        << format << HEIST_FCN_ERR(name, args));
  }


  void hmap_confirm_valid_map_key(const char* name, const char* format, data_vector& args,size_type total_args){
    if(args.size() != total_args) 
      HEIST_THROW_ERR('\''<<name<<" didn't receive "<<total_args<<" args!" 
        << format << HEIST_HASH_MAP_KEY_FORMAT << HEIST_FCN_ERR(name, args));
    if(!args[0].is_type(types::map))
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't a hash-map!" 
        << format << HEIST_HASH_MAP_KEY_FORMAT << HEIST_FCN_ERR(name, args));
    if(!map_object::hashable(args[1]))
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[1])<<" isn't a valid hashable key!" 
        << format << HEIST_HASH_MAP_KEY_FORMAT << HEIST_FCN_ERR(name, args));
  }


  void hmap_confirm_binary_map_key(const char* name, const char* format, data_vector& args){
    hmap_confirm_valid_map_key(name,format,args,2);
  }


  void hmap_confirm_ternary_map_key_val(const char* name, const char* format, data_vector& args){
    hmap_confirm_valid_map_key(name,format,args,3);
  }


  void hmap_confirm_given_2_or_more_maps(const char* name, const char* format, data_vector& args){
    if(args.size() < 2) 
      HEIST_THROW_ERR('\''<<name<<" didn't receive at least 2 args!" 
        << format << HEIST_FCN_ERR(name, args));
    for(size_type i = 0, n = args.size(); i < n; ++i)
      if(!args[i].is_type(types::map))
        HEIST_THROW_ERR('\''<<name<<" arg #"<<i+1<<' '<<HEIST_PROFILE(args[i])<<" isn't a hash-map!" 
          << format << HEIST_FCN_ERR(name, args));
  }


  void hmap_confirm_binary_procedure_map(const char* name, const char* format, data_vector& args){
    if(args.size() != 2)
      HEIST_THROW_ERR('\''<<name<<" didn't receive 2 args!"
        << format << HEIST_FCN_ERR(name,args));
    primitive_toolkit::confirm_data_is_callable(args[0],args,name,format);
    if(!args[1].is_type(types::map))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a hash-map!"
        << format << HEIST_FCN_ERR(name,args));
  }

} // End of namespace heist::stdlib_hmaps

#endif