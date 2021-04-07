// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for objects.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_OO_OBJECTS_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_OO_OBJECTS_IMPLEMENTATION_HPP_

namespace heist::stdlib_objects {

  /******************************************************************************
  * OO INTERNAL PRIMITIVE HELPER FUNCTIONS
  ******************************************************************************/

  // Correct arg validation for primitive "heist:core:oo:set-property!":
  void validate_oo_member_setter(data_vector& args, const char* name, const char* format) {
    if(args.size() != 3)
      HEIST_THROW_ERR('\''<<name<<" didn't receive 3 args!"
        <<format<<HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::obj))
      HEIST_THROW_ERR('\''<<name<<" 1st object arg "<<HEIST_PROFILE(args[0])<<" isn't an object!"
        <<format<<HEIST_FCN_ERR(name,args));
    if(!args[1].is_type(types::sym))
      HEIST_THROW_ERR('\''<<name<<" 2nd property-name arg "<<HEIST_PROFILE(args[1])<<" isn't a symbol!"
        <<format<<HEIST_FCN_ERR(name,args));
    if(args[1].sym == "super")
      HEIST_THROW_ERR('\''<<name<<" property <super> can't be set to a new value!"
        <<format<<HEIST_FCN_ERR(name,args));
    if(args[1].sym == "prototype")
      HEIST_THROW_ERR('\''<<name<<" property <prototype> can't be set to a new value!"
        <<format<<HEIST_FCN_ERR(name,args));
  }


  void throw_too_many_values_in_OO_initialization(data_vector& args, cls_type& class_proto_obj, object_type& obj, 
                                                              const char* format, const char* container_name){
    HEIST_THROW_ERR('\''<< class_proto_obj->class_name<<' '<<container_name<<' '<< args[1] << " has more values than"
      "\n     " << data(class_proto_obj) << " has members (has "<<obj.member_values.size()
      <<" members)!" << format << HEIST_FCN_ERR(class_proto_obj->class_name,args));
  }


  data initialize_OO_ctord_object_HMAP(data_vector& args, cls_type& class_proto_obj, object_type& obj, const char* format) {
    const size_type total_members = obj.member_names.size();
    for(auto& keyval : args[1].map->val) {
      auto key = map_object::unhash_key(keyval.first);
      if(!key.is_type(types::sym))
        HEIST_THROW_ERR('\''<< class_proto_obj->class_name<<" member-name key "<<HEIST_PROFILE(key) 
          << " isn't a symbol!" << format << HEIST_FCN_ERR(class_proto_obj->class_name,args));
      if(key.sym == "prototype" || key.sym == "super")
        HEIST_THROW_ERR('\''<< class_proto_obj->class_name<<" invalid member-name key \""<< key
          << "\" (\"super\" & \"prototype\" are initialized internally)!" << format 
          << HEIST_FCN_ERR(class_proto_obj->class_name,args));
      for(size_type i = 0; i < total_members; ++i) {
        if(obj.member_names[i] == key.sym) {
          obj.member_values[i] = keyval.second;
          goto next_member;
        }
      }
      HEIST_THROW_ERR('\''<<class_proto_obj->class_name<<" member-name key \""<< key
        << "\" isn't a member name in class-obj "<<HEIST_PROFILE(args[0])<<'!' 
        << format << HEIST_FCN_ERR(class_proto_obj->class_name,args));
      next_member: continue;
    }
    return make_obj(std::move(obj));
  }


  data initialize_OO_ctord_object_VECT(data_vector& args, cls_type& class_proto_obj, object_type& obj, const char* format){
    if(args[1].vec->size() > (obj.member_values.size() - 2))
      throw_too_many_values_in_OO_initialization(args,class_proto_obj,obj,format,"vector");
    for(size_type i = 0, j = 0, n = args[1].vec->size(); i < n; ++j)
      if(obj.member_names[j] != "prototype" && obj.member_names[j] != "super")
        obj.member_values[j] = args[1].vec->operator[](i++);
    return make_obj(std::move(obj));
  }


  data initialize_OO_ctord_object_LIST(data_vector& args, cls_type& class_proto_obj, object_type& obj, const char* format){
    if(!primitive_toolkit::data_is_proper_list(args[1]))
      HEIST_THROW_ERR('\''<< class_proto_obj->class_name<<" arg "<<HEIST_PROFILE(args[1]) 
        << " isn't a proper list!" << format << HEIST_FCN_ERR(class_proto_obj->class_name,args));
    const size_type n = obj.member_values.size();
    size_type i = 0;
    data iter = args[1];
    while(iter.is_type(types::par)) {
      if(i == n) throw_too_many_values_in_OO_initialization(args,class_proto_obj,obj,format,"list");
      if(obj.member_names[i] == "prototype" || obj.member_names[i] == "super") {
        ++i;
        continue;
      }
      obj.member_values[i++] = iter.par->first;
      iter = iter.par->second;
    }
    return make_obj(std::move(obj));
  }


  void initialize_object_with_prototype_properties_and_inheritance(object_type& obj, cls_type& class_proto_obj) {
    // assign default values (deep copy members)
    obj.proto = class_proto_obj; // ptr to the prototype object
    for(size_type i = 0, n = class_proto_obj->member_values.size(); i < n; ++i)
      obj.member_values.push_back(class_proto_obj->member_values[i].copy());
    obj.member_names = class_proto_obj->member_names;
    obj.method_names = class_proto_obj->method_names, obj.method_values = class_proto_obj->method_values;
    // add the <prototype> member
    obj.member_names.push_back("prototype");
    obj.member_values.push_back(class_proto_obj);
    // add the <super> member
    obj.member_names.push_back("super");
    if(class_proto_obj->super) {
      object_type super_obj;
      initialize_object_with_prototype_properties_and_inheritance(super_obj,class_proto_obj->super);
      obj.super = make_obj(std::move(super_obj));
      obj.member_values.push_back(obj.super);
    } else {
      obj.member_values.push_back(GLOBALS::FALSE_DATA_BOOLEAN);
    }
  }

  /******************************************************************************
  * DEFCLASS OO GENERAL OBJECT ANALYSIS PRIMITIVES HELPERS
  ******************************************************************************/

  void confirm_given_unary_object_arg(data_vector& args, const char* name) {
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" didn't receive 1 arg!"
        "\n     ("<<name<<" <object>)" << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::obj))
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't an object!"
        "\n     ("<<name<<" <object>)" << HEIST_FCN_ERR(name,args));
  }


  string generate_invalid_HEIST_CORE_OO_MEMBER_ACCESS_call(const data_vector& args, const size_type i)noexcept{
    return "(.. "+data(data_vector(args.begin(),args.begin()+i)).write().substr(1);
  }


  bool object_has_property_name(obj_type& obj, const string& name)noexcept{
    return std::find(obj->member_names.begin(),obj->member_names.end(),name) != obj->member_names.end() || 
           std::find(obj->method_names.begin(),obj->method_names.end(),name) != obj->method_names.end();
  }


  void populate_obj_with_new_dynamic_proto_properties(obj_type& obj) {
    auto proto = obj->proto;
    for(size_type i = 0, n = proto->member_names.size(); i < n; ++i) {
      if(!object_has_property_name(obj,proto->member_names[i])) {
        obj->member_names.push_back(proto->member_names[i]);
        obj->member_values.push_back(proto->member_values[i].copy());
      }
    }
    for(size_type i = 0, n = proto->method_names.size(); i < n; ++i) {
      if(!object_has_property_name(obj,proto->method_names[i])) {
        obj->method_names.push_back(proto->method_names[i]);
        obj->method_values.push_back(proto->method_values[i]);
      }
    }
  }


  // recursively converts objects (including object member values) into hmaps
  // <DONT_INCLUDE_SUPER_PROTOTYPE_MEMBERS> -> true iff generating JSON
  template<bool DONT_INCLUDE_SUPER_PROTOTYPE_MEMBERS>
  data recursively_convert_OBJ_to_HMAP(const data& d)noexcept{
    map_object m;
    for(size_type i = 0, n = d.obj->member_names.size(); i < n; ++i) {
      if constexpr (DONT_INCLUDE_SUPER_PROTOTYPE_MEMBERS)
        if(d.obj->member_names[i] == "super" || d.obj->member_names[i] == "prototype") 
          continue;
      if(d.obj->member_values[i].is_type(types::obj))
        m.val[d.obj->member_names[i]+char(types::sym)] = recursively_convert_OBJ_to_HMAP<DONT_INCLUDE_SUPER_PROTOTYPE_MEMBERS>(d.obj->member_values[i]);
      else
        m.val[d.obj->member_names[i]+char(types::sym)] = d.obj->member_values[i];
    }
    return make_map(std::move(m));
  }


  // DEEP conversion of all nested hmaps to an alist
  data recursively_convert_HMAP_to_ALIST(const data& d)noexcept{
    data_vector alist;
    for(const auto& keyval : d.map->val) {
      data p = make_par();
      p.par->first = map_object::unhash_key(keyval.first);
      p.par->second = make_par();
      if(keyval.second.is_type(types::map))
        p.par->second.par->first = recursively_convert_HMAP_to_ALIST(keyval.second);
      else
        p.par->second.par->first = keyval.second;
      p.par->second.par->second = symconst::emptylist;
      alist.push_back(std::move(p));
    }
    return primitive_toolkit::convert_data_vector_to_proper_list(alist.begin(),alist.end());
  }


  // meant to compose with <recursively_convert_OBJ_to_HMAP> 
  data convert_OBJ_HMAP_into_valid_JSON_ALIST_datum(const data& d) {
    data_vector alist;
    for(const auto& keyval : d.map->val) {
      data p = make_par();
      auto key = map_object::unhash_key(keyval.first);
      // convert the key into a JSON string key
      switch(key.type) {
        case types::str:
          p.par->first = make_str(*key.str); break;
        case types::sym:
          p.par->first = make_str(stdlib_type_coercions::convert_symbol_to_string(key.sym)); break;
        default:
          p.par->first = make_str(key.write());
      }
      // convert the value into a valid json value
      p.par->second = make_par();
      if(keyval.second.is_type(types::map))
        p.par->second.par->first = convert_OBJ_HMAP_into_valid_JSON_ALIST_datum(keyval.second);
      else if(stdlib_json::is_valid_json_datum(keyval.second))
        p.par->second.par->first = keyval.second;
      else
        p.par->second.par->first = make_str(keyval.second.display());
      p.par->second.par->second = symconst::emptylist;
      alist.push_back(std::move(p));
    }
    return primitive_toolkit::convert_data_vector_to_proper_list(alist.begin(),alist.end());
  }

} // End of namespace heist::stdlib_objects

#endif