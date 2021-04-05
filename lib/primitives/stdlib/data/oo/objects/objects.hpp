// Author: Jordan Randleman -- jrandleman@scu.edu -- objects.hpp
// => Defines the primitive object-handling functions written in 
//    C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_OO_OBJECTS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_OO_OBJECTS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * DEFCLASS OO SUPPORT INTERNAL PRIMITIVES
  ******************************************************************************/

  // primitive "heist:core:oo:set-property!" procedure:
  data primitive_HEIST_CORE_OO_SET_PROPERTY_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:oo:set-property! <object> <property-name-symbol> <value>)";
    stdlib_objects::validate_oo_member_setter(args,"set-property!",format);
    // Search Prototype/Inherited Prototype
    if(!args[0].obj->set_property(args[1].sym,args[2]))
      HEIST_THROW_ERR("'set-property! 2nd property-name arg "<<HEIST_PROFILE(args[1])
        <<" isn't a property of "<<HEIST_PROFILE(args[0])<<'!'<<format<<HEIST_FCN_ERR("set-property!",args));
    return GLOBALS::VOID_DATA_OBJECT;
  }


  // primitive "heist:core:oo:add-property!" procedure:
  data primitive_HEIST_CORE_OO_ADD_PROPERTY_BANG(data_vector&& args) {
    stdlib_objects::validate_oo_member_setter(args,"add-property!",
      "\n     (heist:core:oo:add-property! <object> <property-name-symbol> <procedure-value>)");
    args[0].obj->define_property(args[1].sym,args[2]);
    return GLOBALS::VOID_DATA_OBJECT;
  }


  // primitive "heist:core:oo:make-object" procedure:
  data primitive_HEIST_CORE_OO_MAKE_OBJECT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:oo:make-object <class-type-object> <optional-container>)"
      "\n     <optional-container> ::= <member-val-hmap>"
      "\n                            | <member-val-vector>"
      "\n                            | <member-val-proper-list>";
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR("'heist:core:oo:make-object received improper # of args!" 
        << format << HEIST_FCN_ERR("heist:core:oo:make-object", args));
    if(!args[0].is_type(types::cls))
      HEIST_THROW_ERR("'heist:core:oo:make-object 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a class-prototype!"
        << format << HEIST_FCN_ERR("heist:core:oo:make-object", args));
    // create the object
    auto& class_proto_obj = args[0].cls;
    object_type obj;
    stdlib_objects::initialize_object_with_prototype_properties_and_inheritance(obj,class_proto_obj);
    // no args (or '()) given
    if(args.size() == 1 || primitive_toolkit::data_is_nil(args[1])) return make_obj(std::move(obj));
    // confirm given a container (hmap | vector | list)
    switch(args[1].type) {
      case types::map: return stdlib_objects::initialize_OO_ctord_object_HMAP(args,class_proto_obj,obj,format);
      case types::vec: return stdlib_objects::initialize_OO_ctord_object_VECT(args,class_proto_obj,obj,format);
      case types::par: return stdlib_objects::initialize_OO_ctord_object_LIST(args,class_proto_obj,obj,format);
      default:
        HEIST_THROW_ERR('\''<< class_proto_obj->class_name<<" arg "<<HEIST_PROFILE(args[1]) 
          << " isn't a container of member values!" << format 
          << HEIST_FCN_ERR('\''+class_proto_obj->class_name,args));
    }
    return data(); // never triggered
  }

  /******************************************************************************
  * DEFCLASS OO GENERAL OBJECT ANALYSIS PRIMITIVES
  ******************************************************************************/

  data primitive_OBJECT_MEMBERS(data_vector&& args) {
    stdlib_objects::confirm_given_unary_object_arg(args,"object-members");
    stdlib_objects::populate_obj_with_new_dynamic_proto_properties(args[0].obj);
    map_object m;
    for(size_type i = 0, n = args[0].obj->member_names.size(); i < n; ++i)
      m.val[args[0].obj->member_names[i]+char(types::sym)] = args[0].obj->member_values[i];
    return make_map(std::move(m));
  }

  data primitive_OBJECT_METHODS(data_vector&& args) {
    stdlib_objects::confirm_given_unary_object_arg(args,"object-methods");
    stdlib_objects::populate_obj_with_new_dynamic_proto_properties(args[0].obj);
    map_object m;
    for(size_type i = 0, n = args[0].obj->method_names.size(); i < n; ++i)
      m.val[args[0].obj->method_names[i]+char(types::sym)] = args[0].obj->method_values[i].fcn.bind_self(args[0].obj);
    return make_map(std::move(m));
  }

  // primitive ".." procedure:
  data primitive_HEIST_CORE_OO_MEMBER_ACCESS(data_vector&& args) {
    static constexpr const char * const format = "\n     (.. <object> <property-1> ...)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_HEIST_CORE_OO_MEMBER_ACCESS,args);
    if(args.size() < 2)
      HEIST_THROW_ERR("'.. not enough args received!" << format << HEIST_FCN_ERR("..",args));
    data value = args[0];
    // get the call value
    for(size_type i = 1, n = args.size(); i < n; ++i) {
      // verify operating on an object that's accessing a symbolic property name
      if(!value.is_type(types::obj))
        HEIST_THROW_ERR("'.. can't access property "<<HEIST_PROFILE(args[i])<<" in non-object "
          << HEIST_PROFILE(value) << '!' << HEIST_FCN_ERR("..",args));
      if(!args[i].is_type(types::sym))
        HEIST_THROW_ERR("'.. can't access non-symbolic property "<<HEIST_PROFILE(args[i])<<" in object "
          << value << '!' << HEIST_FCN_ERR("..",args));
      // check if object has the property
      bool found = false;
      if(i+1 < n) { // if NOT at the end of a call chain, call MUST refer to an object
        value = value.obj->get_property(args[i].sym, found);
      } else { // if at the end of a call chain, could be referencing a method, so save "self" for extension
        obj_type self = value.obj;
        value = value.obj->get_property(args[i].sym, found);
        if(value.is_type(types::fcn)) value.fcn.bind_self(self); // extend method with self
      }
      if(!found)
        HEIST_THROW_ERR("'.. "<<args[i].sym<<" isn't a property in object "<<value<<'!'<<HEIST_FCN_ERR("..",args));
    }
    return value;
  }

  // NOTE: recursively converts object member values into hashmaps as well
  data primitive_COERCE_OBJECT_TO_HMAP(data_vector&& args) {
    stdlib_objects::confirm_given_unary_object_arg(args,"object->hmap");
    return stdlib_objects::recursively_convert_OBJ_to_HMAP<false>(args[0]);
  }

  data primitive_COERCE_OBJECT_TO_ALIST(data_vector&& args) {
    stdlib_objects::confirm_given_unary_object_arg(args,"object->alist");
    return stdlib_objects::recursively_convert_HMAP_to_ALIST(stdlib_objects::recursively_convert_OBJ_to_HMAP<false>(args[0]));
  }

  data primitive_COERCE_OBJECT_TO_JSON(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (object->json <object> <optional-indent-width>)";
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR("'object->json didn't receive correct # of args:"
        << format << HEIST_FCN_ERR("object->json",args));
    if(!args[0].is_type(types::obj))
      HEIST_THROW_ERR("'object->json arg " << HEIST_PROFILE(args[0]) << " isn't an object!"
        << format << HEIST_FCN_ERR("object->json",args));
    size_type indent_width = 0;
    if(args.size() == 2) {
      auto [json_tab_width, success] = primitive_toolkit::convert_data_to_size_type(args[1]);
      if(!success)
        HEIST_THROW_ERR("'object->json 2nd arg " << HEIST_PROFILE(args[1]) << " isn't a valid indent width!" << format 
          << "\n     <optional-indent-width> := [0, " << GLOBALS::MAX_SIZE_TYPE << ']' << HEIST_FCN_ERR("object->json",args));
      indent_width = json_tab_width;
    }
    auto val = stdlib_objects::convert_OBJ_HMAP_into_valid_JSON_ALIST_datum(stdlib_objects::recursively_convert_OBJ_to_HMAP<true>(args[0]));
    return make_str(stdlib_json::heist_json_generator::format_scm_as_json(val,indent_width,args,format));
  }

} // End of namespace heist

#endif