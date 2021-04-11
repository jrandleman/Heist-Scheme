// Author: Jordan Randleman -- jordanran199@gmail.com -- pairs.hpp
// => Defines primitive pair functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_PAIRS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_PAIRS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * PAIR HANDLERS
  ******************************************************************************/

  // primitive "cons" procedure:
  data primitive_CONS(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CONS,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'cons didn't received 2 arguments: (cons <car-obj> <cdr-obj>)" << HEIST_FCN_ERR("cons", args));
    data new_pair = data(make_par());
    new_pair.par->first = args[0];
    new_pair.par->second = args[1];
    return new_pair;
  }

  // primitive "car" procedure:
  data primitive_CAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "car");
    return args[0].par->first;
  }

  // primitive "cdr" procedure:
  data primitive_CDR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdr");
    return args[0].par->second;
  }

  // primitive "null?" procedure:
  data primitive_NULLP(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'null? didn't receive exactly 1 arg!\n     (null? <obj>)" << HEIST_FCN_ERR("null?",args));
    return boolean(primitive_toolkit::data_is_nil(args[0]));
  }

  // primitive "set-car!" procedure:
  data primitive_SETCAR_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SETCAR_BANG,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'set-car! received incorrect # of arguments:"
        "\n     (set-car! <pair> <obj>)" << HEIST_FCN_ERR("set-car!", args));
    if(!args[0].is_type(types::par))
      HEIST_THROW_ERR("'set-car!'s 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a pair:"
        "\n     (set-car! <pair> <obj>)" << HEIST_FCN_ERR("set-car!", args));
    args[0].par->first = args[1];
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "set-cdr!" procedure:
  data primitive_SETCDR_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SETCDR_BANG,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'set-cdr! received incorrect # of arguments:"
        "\n     (set-cdr! <pair> <obj>)" << HEIST_FCN_ERR("set-cdr!", args));
    if(!args[0].is_type(types::par))
      HEIST_THROW_ERR("'set-cdr!'s 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a pair:"
        "\n     (set-cdr! <pair> <obj>)" << HEIST_FCN_ERR("set-cdr!", args));
    args[0].par->second = args[1];
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "pair-swap!" procedure:
  data primitive_PAIR_SWAP_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_PAIR_SWAP_BANG,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'pair-swap! received incorrect # of args (only "
        << args.size() << "):\n     (pair-swap! <pair1> <pair2>)" 
        << HEIST_FCN_ERR("pair-swap!", args));
    if(!args[0].is_type(types::par))
      HEIST_THROW_ERR("'pair-swap! 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a pair: "
        "\n     (pair-swap! <pair1> <pair2>)" << HEIST_FCN_ERR("pair-swap!",args));
    if(!args[1].is_type(types::par))
      HEIST_THROW_ERR("'pair-swap! 2nd arg "<<HEIST_PROFILE(args[1])<<" isn't a pair: "
        "\n     (pair-swap! <pair1> <pair2>)" << HEIST_FCN_ERR("pair-swap!",args));
    auto tmp(*args[0].par);
    *args[0].par = *args[1].par;
    *args[1].par = tmp;
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * CAR/CDR COMBINATIONS
  ******************************************************************************/

  // primitive "caar" procedure:
  data primitive_CAAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "caar", "1st", args);
    return args[0].par->first.par->first;
  }

  // primitive "cadr" procedure:
  data primitive_CADR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cadr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cadr", "1st", args);
    return args[0].par->second.par->first;
  }

  // primitive "cdar" procedure:
  data primitive_CDAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cdar", "1st", args);
    return args[0].par->first.par->second;
  }

  // primitive "cddr" procedure:
  data primitive_CDDR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cddr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cddr", "1st", args);
    return args[0].par->second.par->second;
  }

  // ----------

  // primitive "caaar" procedure:
  data primitive_CAAAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caaar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "caaar", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first, "caaar", "2nd", args);
    return args[0].par->first.par->first.par->first;
  }

  // primitive "caadr" procedure:
  data primitive_CAADR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caadr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "caadr", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second, "caadr", "1st", args);
    return args[0].par->second.par->first.par->first;
  }

  // primitive "cadar" procedure:
  data primitive_CADAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cadar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cadar", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first, "cadar", "1st", args);
    return args[0].par->first.par->second.par->first;
  }

  // primitive "caddr" procedure:
  data primitive_CADDR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caddr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "caddr", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second, "caddr", "2nd", args);
    return args[0].par->second.par->second.par->first;
  }

  // primitive "cdaar" procedure:
  data primitive_CDAAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdaar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cdaar", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first, "cdaar", "2nd", args);
    return args[0].par->first.par->first.par->second;
  }

  // primitive "cdadr" procedure:
  data primitive_CDADR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdadr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cdadr", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second, "cdadr", "1st", args);
    return args[0].par->second.par->first.par->second;
  }

  // primitive "cddar" procedure:
  data primitive_CDDAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cddar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cddar", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first, "cddar", "1st", args);
    return args[0].par->first.par->second.par->second;
  }

  // primitive "cdddr" procedure:
  data primitive_CDDDR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdddr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cdddr", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second, "cdddr", "2nd", args);
    return args[0].par->second.par->second.par->second;
  }

  // ----------

  // primitive "caaaar" procedure:
  data primitive_CAAAAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caaaar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "caaaar", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first, "caaaar", "2nd", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first.par->first, "caaaar", "3rd", args);
    return args[0].par->first.par->first.par->first.par->first;
  }

  // primitive "caaadr" procedure:
  data primitive_CAAADR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caaadr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "caaadr", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second, "caaadr", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second.par->first, "caaadr", "2nd", args);
    return args[0].par->second.par->first.par->first.par->first;
  }

  // primitive "caadar" procedure:
  data primitive_CAADAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caadar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "caadar", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first, "caadar", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first.par->second, "caadar", "2nd", args);
    return args[0].par->first.par->second.par->first.par->first;
  }

  // primitive "caaddr" procedure:
  data primitive_CAADDR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caaddr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "caaddr", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second, "caaddr", "2nd", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second.par->second, "caaddr", "1st", args);
    return args[0].par->second.par->second.par->first.par->first;
  }

  // primitive "cadaar" procedure:
  data primitive_CADAAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cadaar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cadaar", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first, "cadaar", "2nd", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first.par->first, "cadaar", "1st", args);
    return args[0].par->first.par->first.par->second.par->first;
  }

  // primitive "cadadr" procedure:
  data primitive_CADADR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cadadr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cadadr", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second, "cadadr", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second.par->first, "cadadr", "2nd", args);
    return args[0].par->second.par->first.par->second.par->first;
  }

  // primitive "caddar" procedure:
  data primitive_CADDAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "caddar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "caddar", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first, "caddar", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first.par->second, "caddar", "2nd", args);
    return args[0].par->first.par->second.par->second.par->first;
  }

  // primitive "cadddr" procedure:
  data primitive_CADDDR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cadddr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cadddr", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second, "cadddr", "2nd", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second.par->second, "cadddr", "3rd", args);
    return args[0].par->second.par->second.par->second.par->first;
  }


  // primitive "cdaaar" procedure:
  data primitive_CDAAAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdaaar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cdaaar", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first, "cdaaar", "2nd", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first.par->first, "cdaaar", "3rd", args);
    return args[0].par->first.par->first.par->first.par->second;
  }

  // primitive "cdaadr" procedure:
  data primitive_CDAADR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdaadr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cdaadr", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second, "cdaadr", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second.par->first, "cdaadr", "2nd", args);
    return args[0].par->second.par->first.par->first.par->second;
  }

  // primitive "cdadar" procedure:
  data primitive_CDADAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdadar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cdadar", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first, "cdadar", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first.par->second, "cdadar", "2nd", args);
    return args[0].par->first.par->second.par->first.par->second;
  }

  // primitive "cdaddr" procedure:
  data primitive_CDADDR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdaddr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cdaddr", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second, "cdaddr", "2nd", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second.par->second, "cdaddr", "1st", args);
    return args[0].par->second.par->second.par->first.par->second;
  }

  // primitive "cddaar" procedure:
  data primitive_CDDAAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cddaar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cddaar", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->first, "cddaar", "2nd", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first.par->first, "cddaar", "1st", args);
    return args[0].par->first.par->first.par->second.par->second;
  }

  // primitive "cddadr" procedure:
  data primitive_CDDADR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cddadr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cddadr", "1st", args);
    stdlib_pairs::confirm_nth_car_is_pair(args[0].par->second, "cddadr", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second.par->first, "cddadr", "2nd", args);
    return args[0].par->second.par->first.par->second.par->second;
  }

  // primitive "cdddar" procedure:
  data primitive_CDDDAR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cdddar");
    stdlib_pairs::confirm_nth_car_is_pair(args[0], "cdddar", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first, "cdddar", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->first.par->second, "cdddar", "2nd", args);
    return args[0].par->first.par->second.par->second.par->second;
  }

  // primitive "cddddr" procedure:
  data primitive_CDDDDR(data_vector&& args) {
    stdlib_pairs::confirm_given_a_pair_arg(args, "cddddr");
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0], "cddddr", "1st", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second, "cddddr", "2nd", args);
    stdlib_pairs::confirm_nth_cdr_is_pair(args[0].par->second.par->second, "cddddr", "3rd", args);
    return args[0].par->second.par->second.par->second.par->second;
  }

} // End of namespace heist

#endif