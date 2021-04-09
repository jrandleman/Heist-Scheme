// Author: Jordan Randleman -- jrandleman@scu.edu -- predicates.hpp
// => Defines primitive type predicate functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_TYPE_PREDICATES_HPP_
#define HEIST_SCHEME_CORE_STDLIB_TYPE_PREDICATES_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * TYPEOF & POINTER-ADDRESS PRIMITIVES
  ******************************************************************************/

  // primitive "typeof" procedure:
  data primitive_TYPEOF(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'typeof received incorrect # of args!\n     (typeof <obj>)" 
        << HEIST_FCN_ERR("typeof", args));
    if(args[0].is_type(types::par) && args[0].par->first.is_type(types::del) && 
                                      args[0].par->second.is_type(types::del)) {
      return "stream";
    }
    return args[0].type_name();
  }

  // primitive "pointer-address" procedure:
  data primitive_POINTER_ADDRESS(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'pointer-address received incorrect # of args!\n     (pointer-address <obj>)" 
        << HEIST_FCN_ERR("pointer-address", args));
    auto pointer_address = args[0].pointer_address();
    if(pointer_address.empty())
      return GLOBALS::FALSE_DATA_BOOLEAN;
    return make_str(pointer_address);
  }

  /******************************************************************************
  * OBJECT CREATION
  ******************************************************************************/

  // primitive "void" procedure:
  data primitive_VOID(data_vector&&)noexcept{
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "undefined" procedure:
  data primitive_UNDEFINED(data_vector&&)noexcept{
    return data();
  }

  /******************************************************************************
  * PREDICATES
  ******************************************************************************/

  // primitive "void?" procedure:
  data primitive_VOIDP(data_vector&& args) { 
    stdlib_type_predicates::confirm_given_one_arg(args, "void?");
    return boolean(args[0].is_type(types::dne));
  }

  // primitive "undefined?" procedure:
  data primitive_UNDEFINEDP(data_vector&& args) { 
    stdlib_type_predicates::confirm_given_one_arg(args, "undefined?");
    return boolean(args[0].is_type(types::undefined));
  }

  // primitive "hmap?" procedure:
  data primitive_HMAPP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "hmap?");
    return boolean(args[0].is_type(types::map));
  }

  // primitive "empty?" procedure:
  data primitive_EMPTYP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "empty?");
    return boolean(primitive_toolkit::data_is_nil(args[0])               || 
                   (args[0].is_type(types::str) && args[0].str->empty()) ||
                   (args[0].is_type(types::vec) && args[0].vec->empty()));
  }

  // primitive "pair?" procedure:
  data primitive_PAIRP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "pair?");
    return boolean(args[0].is_type(types::par));
  }

  // primitive "vector?" procedure:
  data primitive_VECTORP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "vector?");
    return boolean(args[0].is_type(types::vec));
  }

  // primitive "char?" procedure:
  data primitive_CHARP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "char?");
    return boolean(args[0].is_type(types::chr));
  }

  // primitive "number?" procedure:
  data primitive_NUMBERP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "number?");
    return boolean(args[0].is_type(types::num) && !args[0].num.is_nan());
  }

  // primitive "real?" procedure:
  // => "real" denotes a non-complex number -> NOTE hence +nan.0 is "real"
  data primitive_REALP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "real?");
    return boolean(args[0].is_type(types::num) && args[0].num.is_real());
  }

  // primitive "complex?" procedure:
  data primitive_COMPLEXP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "complex?");
    return boolean(args[0].is_type(types::num) && args[0].num.is_complex());
  }

  // primitive "rational?" procedure:
  // => "rational" denotes a # that inexact->exact won't approximate
  data primitive_RATIONALP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "rational?");
    return boolean(args[0].is_type(types::num) && args[0].num.is_rational());
  }

  // primitive "string?" procedure:
  data primitive_STRINGP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "string?");
    return boolean(args[0].is_type(types::str));
  }

  // primitive "symbol?" procedure:
  data primitive_SYMBOLP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "symbol?");
    return boolean(args[0].is_type(types::sym) && args[0].sym != symconst::emptylist);
  }

  // primitive "boolean?" procedure:
  data primitive_BOOLEANP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "boolean?");
    return boolean(args[0].is_type(types::bol));
  }

  // primitive "atom?" procedure:
  data primitive_ATOMP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "atom?");
    return boolean(!args[0].is_type(types::par));
  }

  // primitive "procedure?" procedure:
  data primitive_PROCEDUREP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "procedure?");
    return boolean(args[0].is_type(types::fcn));
  }

  // primitive "input-port?" procedure:
  data primitive_INPUT_PORTP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "input-port?");
    return boolean(args[0].is_type(types::fip));
  }

  // primitive "output-port?" procedure:
  data primitive_OUTPUT_PORTP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "output-port?");
    return boolean(args[0].is_type(types::fop));
  }

  // primitive "eof-object?" procedure:
  data primitive_EOF_OBJECTP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "eof-object?");
    return boolean(args[0].is_type(types::chr) && args[0].chr==EOF);
  }

  // primitive "stream-pair?" procedure:
  data primitive_STREAM_PAIRP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "stream-pair?");
    return boolean(args[0].is_type(types::par) &&
                   args[0].par->first.is_type(types::del) && 
                   args[0].par->second.is_type(types::del));
  }

  // primitive "stream-null?" procedure:
  data primitive_STREAM_NULLP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "stream-null?");
    return boolean(primitive_toolkit::data_is_nil(args[0]));
  }

  // primitive "stream?" procedure:
  data primitive_STREAMP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "stream?");
    return boolean(primitive_toolkit::data_is_nil(args[0]) ||
                   (args[0].is_type(types::par) && 
                    args[0].par->first.is_type(types::del) && 
                    args[0].par->second.is_type(types::del)));
  }

  // primitive "syntax-rules-object?" procedure:
  data primitive_SYNTAX_RULES_OBJECTP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "syntax-rules-object?");
    return boolean(args[0].is_type(types::syn));
  }

  // primitive "seq?" procedure:
  data primitive_SEQP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "seq?");
    return boolean(args[0].is_type(types::vec) ||
                   args[0].is_type(types::str) || 
                   primitive_toolkit::data_is_proper_list(args[0]));
  }

  // primitive "object?" procedure:
  data primitive_OBJECTP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "object?");
    return boolean(args[0].is_type(types::obj));
  }

  // primitive "class-prototype?" procedure:
  data primitive_CLASS_PROTOTYPEP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "class-prototype?");
    return boolean(args[0].is_type(types::cls));
  }

  // primitive "functor?" procedure:
  // functor = object w/ a "self->procedure" method
  data primitive_FUNCTORP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "functor?");
    return boolean(primitive_toolkit::data_is_functor(args[0]));
  }

  // primitive "callable?" procedure:
  // callable = procedure? or class-prototype? or functor?
  data primitive_CALLABLEP(data_vector&& args) {
    stdlib_type_predicates::confirm_given_one_arg(args, "callable?");
    return boolean(primitive_toolkit::data_is_callable(args[0]));
  }

} // End of namespace heist

#endif