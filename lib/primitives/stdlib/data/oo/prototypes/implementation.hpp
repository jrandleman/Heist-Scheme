// Author: Jordan Randleman -- jrandleman@scu.edu -- implementation.hpp
// => Defines helper functions for prototypes.hpp

#ifndef HEIST_SCHEME_CORE_STDLIB_OO_PROTOTYPES_IMPLEMENTATION_HPP_
#define HEIST_SCHEME_CORE_STDLIB_OO_PROTOTYPES_IMPLEMENTATION_HPP_

namespace heist::stdlib_prototypes {
  
  void confirm_given_unary_class_prototype_arg(data_vector& args, const char* name) {
    if(args.size() != 1)
      HEIST_THROW_ERR('\''<<name<<" didn't receive 1 arg!"
        "\n     ("<<name<<" <class-prototype>)" << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::cls))
      HEIST_THROW_ERR('\''<<name<<" arg "<<HEIST_PROFILE(args[0])<<" isn't a class-prototype!"
        "\n     ("<<name<<" <class-prototype>)" << HEIST_FCN_ERR(name,args));
  }


  void confirm_proper_new_property_args(data_vector& args, const char* name, const char* format) {
    if(args.size() != 3)
      HEIST_THROW_ERR('\''<<name<<" didn't receive 3 args!"
        << format << HEIST_FCN_ERR(name,args));
    if(!args[0].is_type(types::cls))
      HEIST_THROW_ERR('\''<<name<<" 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a class-prototype!"
        << format << HEIST_FCN_ERR(name,args));
    if(!args[1].is_type(types::sym))
      HEIST_THROW_ERR('\''<<name<<" 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a symbol!"
        << format << HEIST_FCN_ERR(name,args));
  }


  void confirm_new_property_name_doesnt_already_exist(data_vector& args, const char* name, const char* format) {
    for(const auto& property_name : args[0].cls->member_names)
      if(property_name == args[1].sym)
        HEIST_THROW_ERR('\''<<name<<" \"" << property_name << "\" is already a member of class-prototype \"" 
          << args[0].cls->class_name << "\"!" << format << HEIST_FCN_ERR(name,args));
    for(const auto& property_name : args[0].cls->method_names)
      if(property_name == args[1].sym)
        HEIST_THROW_ERR('\''<<name<<" \"" << property_name << "\" is already a method of class-prototype \"" 
          << args[0].cls->class_name << "\"!" << format << HEIST_FCN_ERR(name,args));
  }

} // End of namespace heist::stdlib_prototypes

#endif