// Author: Jordan Randleman -- jordanran199@gmail.com -- hmaps.hpp
// => Defines primitive hmap functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_HMAPS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_HMAPS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * HASHMAP CONSTRUCTOR
  ******************************************************************************/

  // primitive "hmap":
  data primitive_HMAP(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (hmap <key1> <val1> <key2> <val2> ...)" HEIST_HASH_MAP_KEY_FORMAT;
    if(args.empty()) return make_map(map_object());
    if(args.size() & 1) 
      HEIST_THROW_ERR("'hmap received uneven # of args!"<<format<<HEIST_FCN_ERR("hmap",args));
    map_object hmap;
    // verify all keys are hashable
    for(size_type i = args.size()-2; i != GLOBALS::MAX_SIZE_TYPE-1; i -= 2) {
      if(!map_object::hashable(args[i]))
        HEIST_THROW_ERR("'hmap key " << HEIST_PROFILE(args[i]) << " isn't hashable!"
          << format << HEIST_FCN_ERR("hmap", args));
      hmap[args[i]] = args[i+1];
    }
    return make_map(std::move(hmap));
  }

  /******************************************************************************
  * HASHMAP ACCESSORS
  ******************************************************************************/

  // primitive "hmap-keys":
  data primitive_HMAP_KEYS(data_vector&& args) {
    stdlib_hmaps::hmap_confirm_unary_map("hmap-keys","\n     (hmap-keys <hash-map>)",args);
    data_vector keys_list;
    for(const auto& keyval : args[0].map->val)
      keys_list.push_back(map_object::unhash_key(keyval.first));
    return primitive_toolkit::convert_data_vector_to_proper_list(keys_list.begin(),keys_list.end());
  }

  // primitive "hmap-vals":
  data primitive_HMAP_VALS(data_vector&& args) {
    stdlib_hmaps::hmap_confirm_unary_map("hmap-vals","\n     (hmap-vals <hash-map>)",args);
    data_vector keys_list;
    for(const auto& keyval : args[0].map->val)
      keys_list.push_back(keyval.second);
    return primitive_toolkit::convert_data_vector_to_proper_list(keys_list.begin(),keys_list.end());
  }

  // primitive "hmap-ref":
  data primitive_HMAP_REF(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_HMAP_REF,args);
    stdlib_hmaps::hmap_confirm_binary_map_key("hmap-ref","\n     (hmap-ref <hash-map> <key>)",args);
    auto hashed_key = map_object::hash_key(args[1]);
    if(!args[0].map->val.count(hashed_key))
      HEIST_THROW_ERR("'hmap-ref arg "<<HEIST_PROFILE(args[1])<<" isn't a key in hash-map "
        << args[0] << "!\n     (hmap-ref <hash-map> <key>)" << HEIST_FCN_ERR("hmap-ref", args));
    return args[0].map->val[hashed_key];
  }

  // primitive "hmap-length":
  data primitive_HMAP_LENGTH(data_vector&& args) {
    stdlib_hmaps::hmap_confirm_unary_map("hmap-length","\n     (hmap-length <hash-map>)",args);
    return num_type(args[0].map->val.size());
  }

  // primitive "hmap-empty?":
  data primitive_HMAP_EMPTYP(data_vector&& args) {
    stdlib_hmaps::hmap_confirm_unary_map("hmap-empty?","\n     (hmap-empty? <hash-map>)",args);
    return boolean(args[0].map->val.empty());
  }

  /******************************************************************************
  * HASHMAP PREDICATES
  ******************************************************************************/

  // primitive "hmap-key?":
  data primitive_HMAP_KEYP(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_HMAP_KEYP,args);
    stdlib_hmaps::hmap_confirm_binary_map_key("hmap-key?","\n     (hmap-key? <hash-map> <key>)",args);
    return boolean(args[0].map->val.count(map_object::hash_key(args[1])));
  }

  // primitive "hmap-hashable?":
  data primitive_HMAP_HASHABLEP(data_vector&& args) {
    if(args.size() != 1) 
      HEIST_THROW_ERR("'hmap-hashable? didn't receive 1 arg!" 
        "\n     (hmap-hashable? <obj>)" << HEIST_FCN_ERR("hmap-hashable?", args));
    return boolean(map_object::hashable(args[0]));
  }

  /******************************************************************************
  * HASHMAP MUTATION
  ******************************************************************************/

  // primitive "hmap-set!":
  data primitive_HMAP_SET_BANG(data_vector&& args) {
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_HMAP_SET_BANG,args);
    stdlib_hmaps::hmap_confirm_ternary_map_key_val("hmap-set!","\n     (hmap-set! <hash-map> <key> <value>)",args);
    (*args[0].map)[args[1]] = args[2];
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "hmap-delete!":
  data primitive_HMAP_DELETE_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_HMAP_DELETE_BANG,args);
    stdlib_hmaps::hmap_confirm_binary_map_key("hmap-delete!","\n     (hmap-delete! <hash-map> <key>)",args);
    return boolean(args[0].map->val.erase(map_object::hash_key(args[1])));
  }

  /******************************************************************************
  * HASHMAP MERGING
  ******************************************************************************/

  // primitive "hmap-merge":
  data primitive_HMAP_MERGE(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_HMAP_MERGE,args);
    stdlib_hmaps::hmap_confirm_given_2_or_more_maps("hmap-merge","\n     (hmap-merge <hash-map-1> <hash-map-2> ...)",args);
    map_object map;
    for(size_type i = args.size(); i-- > 0;)
      for(const auto& keyval : args[i].map->val)
        map.val[keyval.first] = keyval.second; // left arg key vals supersede right arg key vals
    return make_map(std::move(map));
  }

  // primitive "hmap-merge!":
  data primitive_HMAP_MERGE_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_HMAP_MERGE_BANG,args);
    stdlib_hmaps::hmap_confirm_given_2_or_more_maps("hmap-merge!","\n     (hmap-merge! <hash-map-1> <hash-map-2> ...)",args);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      for(const auto& keyval : args[i].map->val)
        if(!args[0].map->val.count(keyval.first))
          args[0].map->val[keyval.first] = keyval.second;
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * HASHMAP ITERATION
  ******************************************************************************/

  // primitive "hmap-for-each", "hmap-for-each-key", "hmap-for-each-val", "hmap-map!"
  // NOTE: WE CREATE A VECTOR OF KEYS THEN ITERATE THRU THAT VECTOR, SO AS TO NOT INVALIDATE
  //       HMAP ITERATORS IN THE LOOP FROM POTENTIALLY DELETING THE KEY IN THE USER'S FCN
  #define GENERATE_HMAP_ITERATION_FCN(FCN_NAME,NAME,...)\
    data FCN_NAME(data_vector&& args) {\
      if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(FCN_NAME,args);\
      stdlib_hmaps::hmap_confirm_binary_procedure_map(NAME,"\n     (" NAME " <callable> <hash-map>)",args);\
      auto procedure(primitive_toolkit::convert_callable_to_procedure(args[0]));\
      size_type n = args[1].map->val.size(), i = 0;\
      str_vector keys(n);\
      for(auto& keyvalue : args[1].map->val)\
        keys[i++] = keyvalue.first;\
      for(i = 0; i < n; ++i) {\
        __VA_ARGS__;\
      }\
      return GLOBALS::VOID_DATA_OBJECT;\
    }

  GENERATE_HMAP_ITERATION_FCN(primitive_HMAP_FOR_EACH_KEY,"hmap-for-each-key",
    execute_application(procedure,data_vector(1,map_object::unhash_key(keys[i])));)

  GENERATE_HMAP_ITERATION_FCN(primitive_HMAP_FOR_EACH_VAL,"hmap-for-each-val",
    execute_application(procedure,data_vector(1,args[1].map->val[keys[i]]));)

  GENERATE_HMAP_ITERATION_FCN(primitive_HMAP_FOR_EACH,"hmap-for-each",
    auto p = make_par();
    p->first = map_object::unhash_key(keys[i]);
    p->second = args[1].map->val[keys[i]];
    execute_application(procedure,data_vector(1,p));)

  GENERATE_HMAP_ITERATION_FCN(primitive_HMAP_MAP_BANG,"hmap-map!",
    args[1].map->val[keys[i]] = execute_application(procedure,data_vector(1,args[1].map->val[keys[i]]));)

  // primitive "hmap-map"
  data primitive_HMAP_MAP(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_HMAP_MAP,args);
    stdlib_hmaps::hmap_confirm_binary_procedure_map("hmap-map","\n     (hmap-map <callable> <hash-map>)",args);
    auto procedure(primitive_toolkit::convert_callable_to_procedure(args[0]));
    size_type n = args[1].map->val.size(), i = 0;
    // extract all keys, then iterate thru keys (avoids iterator invalidation
    // in case the procedure passed by the user [for some reason] erases elts 
    // from the map being iterated through)
    str_vector keys(n);
    for(auto& keyvalue : args[1].map->val)
      keys[i++] = keyvalue.first;
    map_object map;
    for(i = 0; i < n; ++i)
      map.val[keys[i]] = execute_application(procedure,data_vector(1,args[1].map->val[keys[i]]));
    return make_map(std::move(map));
  }

  /******************************************************************************
  * HASHMAP <-> ALIST COERCION
  ******************************************************************************/

  // primitive "hmap->alist"
  data primitive_COERCE_HMAP_TO_ALIST(data_vector&& args) {
    stdlib_hmaps::hmap_confirm_unary_map("hmap->alist","\n     (hmap->alist <hash-map>)",args);
    data_vector alist(args[0].map->val.size());
    size_type i = 0;
    for(auto& keyvalue : args[0].map->val) {
      auto p = make_par();
      p->first = map_object::unhash_key(keyvalue.first);
      p->second = make_par();
      p->second.par->first = keyvalue.second;
      p->second.par->second = symconst::emptylist;
      alist[i++] = std::move(p);
    }
    return primitive_toolkit::convert_data_vector_to_proper_list(alist.begin(),alist.end());
  }

  // primitive "alist->hmap"
  data primitive_COERCE_ALIST_TO_HMAP(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'alist->hmap didn't receive 1 arg!"
        "\n     (alist->hmap <alist>)" << HEIST_FCN_ERR("alist->hmap",args));
    if(!primitive_toolkit::data_is_proper_list(args[0]))
      HEIST_THROW_ERR("'alist->hmap arg "<<HEIST_PROFILE(args[0])<<" isn't an <alist> of proper-list pairs of items!"
        "\n     (alist->hmap <alist>)" << HEIST_FCN_ERR("alist->hmap",args));
    map_object map;
    auto alist_exp = primitive_toolkit::convert_proper_list_to_data_vector(args[0]);
    for(auto& p : alist_exp) {
      if(!p.is_type(types::par))
        HEIST_THROW_ERR("'alist->hmap arg "<<HEIST_PROFILE(args[0])<<" isn't an <alist> of proper-list pairs of items!"
          "\n     (alist->hmap <alist>)" << HEIST_FCN_ERR("alist->hmap",args));
      if(!p.par->second.is_type(types::par))
        HEIST_THROW_ERR("'alist->hmap arg "<<HEIST_PROFILE(args[0])<<" isn't an <alist> of proper-list pairs of items!"
          "\n     (alist->hmap <alist>)" << HEIST_FCN_ERR("alist->hmap",args));
      if(!map_object::hashable(p.par->first))
        HEIST_THROW_ERR("'alist->hmap key "<<HEIST_PROFILE(p.par->first)<<" isn't hashable!"
          "\n     (alist->hmap <alist>)" HEIST_HASH_MAP_KEY_FORMAT << HEIST_FCN_ERR("alist->hmap",args));
      map[p.par->first] = p.par->second.par->first;
    }
    return make_map(std::move(map));
  }

  #undef GENERATE_HMAP_ITERATION_FCN
  #undef HEIST_HASH_MAP_KEY_FORMAT

} // End of namespace heist

#endif