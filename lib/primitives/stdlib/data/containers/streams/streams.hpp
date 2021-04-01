// Author: Jordan Randleman -- jrandleman@scu.edu -- streams.hpp
// => Defines the primitive stream functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_STREAMS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_STREAMS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * DELAY? & FORCE PRIMITIVES
  ******************************************************************************/

  // primitive "delay?" predicate procedure:
  data primitive_DELAYP(data_vector&& args) {
    stdlib_streams::confirm_given_one_arg(args,"delay?");
    return boolean(args[0].is_type(types::del));
  }

  // primitive "force" procedure:
  data primitive_FORCE(data_vector&& args) {
    stdlib_streams::confirm_given_one_arg(args,"force","<delayed-expression>");
    return stdlib_streams::force_data_delay(args[0]); // "call-by-need" evaluation
  }

  /******************************************************************************
  * STREAM ACCESSORS
  ******************************************************************************/

  // primitive "scar" procedure:
  data primitive_SCAR(data_vector&& args) {
    stdlib_streams::confirm_given_a_stream_pair_arg(args, "scar", "\n     (scar <stream-pair>)");
    return stdlib_streams::force_data_delay(args[0].par->first);
  }

  // primitive "scdr" procedure:
  data primitive_SCDR(data_vector&& args) {
    stdlib_streams::confirm_given_a_stream_pair_arg(args, "scdr", "\n     (scdr <stream-pair>)");
    data cdr_promise = stdlib_streams::force_data_delay(args[0].par->second);
    if(!stdlib_streams::data_is_stream(cdr_promise))
      HEIST_THROW_ERR("'scdr forced cdr " << HEIST_PROFILE(cdr_promise)
        << " isn't a stream:\n     (scdr <stream-pair>)" << HEIST_FCN_ERR("scdr", args));
    return cdr_promise;
  }

  // primitive "stream-length" procedure:
  data primitive_STREAM_LENGTH(data_vector&& args) {
    stdlib_streams::confirm_given_one_arg(args,"stream-length","<stream>");
    // Length of '() = 0
    if(primitive_toolkit::data_is_nil(args[0])) return num_type();
    // Confirm given a stream pair, if not '()
    stdlib_streams::confirm_given_a_stream_pair_arg(args,"stream-length","\n     (stream-length <stream>)");
    return num_type(stdlib_streams::stream_length(stdlib_streams::get_stream_data_cdr(args[0])));
  }

  /******************************************************************************
  * SCAR/SCDR COMBINATIONS
  ******************************************************************************/

  // primitive "scaar" procedure:
  data primitive_SCAAR(data_vector&& args) {
    static constexpr const char * const name   = "scaar";
    static constexpr const char * const format = "\n     (scaar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,2);
  }

  // primitive "scadr" procedure:
  data primitive_SCADR(data_vector&& args) {
    static constexpr const char * const name   = "scadr";
    static constexpr const char * const format = "\n     (scadr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1);
  }

  // primitive "scdar" procedure:
  data primitive_SCDAR(data_vector&& args) {
    static constexpr const char * const name   = "scdar";
    static constexpr const char * const format = "\n     (scdar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,1);
  }

  // primitive "scddr" procedure:
  data primitive_SCDDR(data_vector&& args) {
    static constexpr const char * const name   = "scddr";
    static constexpr const char * const format = "\n     (scddr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2);
  }

  // ----------

  // primitive "scaaar" procedure:
  data primitive_SCAAAR(data_vector&& args) {
    static constexpr const char * const name   = "scaaar";
    static constexpr const char * const format = "\n     (scaaar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,3);
  }

  // primitive "scaadr" procedure:
  data primitive_SCAADR(data_vector&& args) {
    static constexpr const char * const name   = "scaadr";
    static constexpr const char * const format = "\n     (scaadr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2);
  }

  // primitive "scadar" procedure:
  data primitive_SCADAR(data_vector&& args) {
    static constexpr const char * const name   = "scadar";
    static constexpr const char * const format = "\n     (scadar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2);
  }

  // primitive "scaddr" procedure:
  data primitive_SCADDR(data_vector&& args) {
    static constexpr const char * const name   = "scaddr";
    static constexpr const char * const format = "\n     (scaddr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,1);
  }

  // primitive "scdaar" procedure:
  data primitive_SCDAAR(data_vector&& args) {
    static constexpr const char * const name   = "scdaar";
    static constexpr const char * const format = "\n     (scdaar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,1);
  }

  // primitive "scdadr" procedure:
  data primitive_SCDADR(data_vector&& args) {
    static constexpr const char * const name   = "scdadr";
    static constexpr const char * const format = "\n     (scdadr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2);
  }

  // primitive "scddar" procedure:
  data primitive_SCDDAR(data_vector&& args) {
    static constexpr const char * const name   = "scddar";
    static constexpr const char * const format = "\n     (scddar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2);
  }

  // primitive "scdddr" procedure:
  data primitive_SCDDDR(data_vector&& args) {
    static constexpr const char * const name   = "scdddr";
    static constexpr const char * const format = "\n     (scdddr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,3);
  }

  // ----------

  // primitive "scaaaar" procedure:
  data primitive_SCAAAAR(data_vector&& args) {
    static constexpr const char * const name   = "scaaaar";
    static constexpr const char * const format = "\n     (scaaaar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,3),name,format,4);
  }

  // primitive "scaaadr" procedure:
  data primitive_SCAAADR(data_vector&& args) {
    static constexpr const char * const name   = "scaaadr";
    static constexpr const char * const format = "\n     (scaaadr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,3);
  }

  // primitive "scaadar" procedure:
  data primitive_SCAADAR(data_vector&& args) {
    static constexpr const char * const name   = "scaadar";
    static constexpr const char * const format = "\n     (scaadar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,3);
  }

  // primitive "scaaddr" procedure:
  data primitive_SCAADDR(data_vector&& args) {
    static constexpr const char * const name   = "scaaddr";
    static constexpr const char * const format = "\n     (scaaddr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,1),name,format,2);
  }

  // primitive "scadaar" procedure:
  data primitive_SCADAAR(data_vector&& args) {
    static constexpr const char * const name   = "scadaar";
    static constexpr const char * const format = "\n     (scadaar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,1),name,format,3);
  }

  // primitive "scadadr" procedure:
  data primitive_SCADADR(data_vector&& args) {
    static constexpr const char * const name   = "scadadr";
    static constexpr const char * const format = "\n     (scadadr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,2);
  }

  // primitive "scaddar" procedure:
  data primitive_SCADDAR(data_vector&& args) {
    static constexpr const char * const name   = "scaddar";
    static constexpr const char * const format = "\n     (scaddar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,2);
  }

  // primitive "scadddr" procedure:
  data primitive_SCADDDR(data_vector&& args) {
    static constexpr const char * const name   = "scadddr";
    static constexpr const char * const format = "\n     (scadddr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,3),name,format,1);
  }


  // primitive "scdaaar" procedure:
  data primitive_SCDAAAR(data_vector&& args) {
    static constexpr const char * const name   = "scdaaar";
    static constexpr const char * const format = "\n     (scdaaar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,3),name,format,1);
  }

  // primitive "scdaadr" procedure:
  data primitive_SCDAADR(data_vector&& args) {
    static constexpr const char * const name   = "scdaadr";
    static constexpr const char * const format = "\n     (scdaadr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,2);
  }

  // primitive "scdadar" procedure:
  data primitive_SCDADAR(data_vector&& args) {
    static constexpr const char * const name   = "scdadar";
    static constexpr const char * const format = "\n     (scdadar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,2);
  }

  // primitive "scdaddr" procedure:
  data primitive_SCDADDR(data_vector&& args) {
    static constexpr const char * const name   = "scdaddr";
    static constexpr const char * const format = "\n     (scdaddr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,1),name,format,3);
  }

  // primitive "scddaar" procedure:
  data primitive_SCDDAAR(data_vector&& args) {
    static constexpr const char * const name   = "scddaar";
    static constexpr const char * const format = "\n     (scddaar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,1),name,format,2);
  }

  // primitive "scddadr" procedure:
  data primitive_SCDDADR(data_vector&& args) {
    static constexpr const char * const name   = "scddadr";
    static constexpr const char * const format = "\n     (scddadr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_car(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,3);
  }

  // primitive "scdddar" procedure:
  data primitive_SCDDDAR(data_vector&& args) {
    static constexpr const char * const name   = "scdddar";
    static constexpr const char * const format = "\n     (scdddar <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,3);
  }

  // primitive "scddddr" procedure:
  data primitive_SCDDDDR(data_vector&& args) {
    static constexpr const char * const name   = "scddddr";
    static constexpr const char * const format = "\n     (scddddr <stream-pair>)";
    stdlib_streams::confirm_given_a_stream_pair_arg(args,name,format);
    return stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(stdlib_streams::get_stream_data_cdr(
            stdlib_streams::get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,3),name,format,4);
  }

  /******************************************************************************
  * STREAM CONTROL FEATURES
  ******************************************************************************/

  // primitive "stream-for-each" procedure:
  data primitive_STREAM_FOR_EACH(data_vector&& args) {
    // Confirm given minimum # of args needed
    static constexpr const char * const format = 
      "\n     (stream-for-each <callable> <stream1> <stream2> ...)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STREAM_FOR_EACH,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'stream-for-each received insufficient args (only "
        << args.size() << "):" << format << HEIST_FCN_ERR("stream-for-each", args));
    // Confirm only given streams
    data_vector stream_heads(args.begin()+1, args.end());
    stdlib_streams::confirm_only_given_streams(stream_heads,"stream-for-each",format,1,args);
    // Apply the procedure on each elt of each stream
    auto procedure(primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "stream-for-each", format));
    stdlib_streams::stream_for_each(stream_heads, procedure);
    return GLOBALS::VOID_DATA_OBJECT;
  }

  // primitive "stream-ref" procedure:
  data primitive_STREAM_REF(data_vector&& args) {
    // Confirm appropriate # of args given
    static constexpr const char * const format = 
      "\n     (stream-ref <stream-pair> <index>)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STREAM_REF,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'stream-ref received incorrect # of args (given "
        << args.size() << "):" << format << HEIST_FCN_ERR("stream-ref", args));
    // Confirm given a stream-pair
    if(!stdlib_streams::data_is_stream_pair(args[0]))
      HEIST_THROW_ERR("'stream-ref " << HEIST_PROFILE(args[0]) << " isn't a stream-pair!"
        << format << HEIST_FCN_ERR("stream-ref", args));
    // Confirm given a valid index
    if(!stdlib_streams::data_is_valid_index(args[1]))
      HEIST_THROW_ERR("'stream-ref " << HEIST_PROFILE(args[1]) << " isn't a valid <index>!"
        << format << HEIST_FCN_ERR("stream-ref", args));
    // Get the stream's index'th item
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(!n) return stdlib_streams::get_stream_data_car(args[0]);
    data item = stdlib_streams::stream_ref_drop(stdlib_streams::get_stream_data_cdr(args[0]), n, "stream-ref", format);
    if(!stdlib_streams::data_is_stream_pair(item))
      HEIST_THROW_ERR("'stream-ref <index> " << n << " is out of range!"
        << format << HEIST_FCN_ERR("stream-ref", args));
    return stdlib_streams::get_stream_data_car(item);
  }

  // primitive "stream-drop" procedure:
  data primitive_STREAM_DROP(data_vector&& args) {
    // Confirm appropriate # of args given
    static constexpr const char * const format = "\n     (stream-drop <stream> <n>)";
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STREAM_DROP,args);
    stdlib_streams::validate_stream_take_drop_args(args, "stream-drop", format);
    // Get the a substream after dropping 'size' items from given stream
    if(primitive_toolkit::data_is_nil(args[0])) return args[0];
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(!n)     return args[0];
    if(n == 1) return stdlib_streams::get_stream_data_cdr(args[0]);
    return stdlib_streams::stream_ref_drop(stdlib_streams::get_stream_data_cdr(args[0]), n, "stream-drop", format, 1, false);
  }

  // primitive "stream-drop-while" procedure:
  data primitive_STREAM_DROP_WHILE(data_vector&& args) {
    static constexpr const char * const format = "\n     (stream-drop-while <predicate> <stream>)";
    // Confirm appropriate # of args given
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STREAM_DROP_WHILE,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'stream-drop-while received incorrect # of args (given "
        << args.size() << "):" << format << HEIST_FCN_ERR("stream-drop-while", args));
    // Confirm given a procedure
    primitive_toolkit::confirm_data_is_callable(args[0], args, "stream-drop-while", format);
    // Confirm given a stream
    if(!stdlib_streams::data_is_stream(args[1]))
      HEIST_THROW_ERR("'stream-drop-while "<< HEIST_PROFILE(args[1]) << " isn't a stream!"
        << format << HEIST_FCN_ERR("stream-drop-while", args));
    // Get keep dropping items while 'predicate' is true, then return result
    if(primitive_toolkit::data_is_nil(args[1])) return args[1];
    auto procedure = primitive_toolkit::convert_callable_to_procedure(args[0]);
    return stdlib_streams::stream_drop_while(std::move(args[1]), procedure);
  }

  // primitive "stream-take" procedure:
  data primitive_STREAM_TAKE(data_vector&& args){
    // Confirm appropriate # of args given
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STREAM_TAKE,args);
    stdlib_streams::validate_stream_take_drop_args(args, "stream-take", "\n     (stream-take <stream> <n>)");
    // Get the a substream after dropping 'size' items from given stream
    if(primitive_toolkit::data_is_nil(args[0])) return args[0];
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(!n) return data(symconst::emptylist);
    data_vector substream;
    stdlib_streams::stream_take(std::move(args[0]),n,substream);
    return stdlib_streams::convert_data_vector_to_stream(substream.begin(),substream.end());
  }

  // primitive "stream-reverse" procedure:
  data primitive_STREAM_REVERSE(data_vector&& args) {
    // Confirm given a single stream arg
    stdlib_streams::confirm_given_one_arg(args,"stream-reverse","<stream>");
    if(!stdlib_streams::data_is_stream(args[0]))
      HEIST_THROW_ERR("'stream-reverse "<<HEIST_PROFILE(args[0])<<" isn't a stream:" 
        "\n     (stream-reverse <stream>)" << HEIST_FCN_ERR("stream-reverse",args));
    // Convert stream to a data_vector, reverse, & revert to a stream
    if(primitive_toolkit::data_is_nil(args[0])) return args[0];
    data_vector stream_as_exp;
    stdlib_streams::unpack_stream_into_exp(std::move(args[0]), stream_as_exp);
    std::reverse(stream_as_exp.begin(),stream_as_exp.end());
    return stdlib_streams::convert_data_vector_to_stream(stream_as_exp.begin(),stream_as_exp.end());
  }

  // primitive "stream-fold" procedure:
  data primitive_STREAM_FOLD(data_vector&& args) {
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STREAM_FOLD,args);
    return stdlib_streams::stream_fold<true>(args, "stream-fold", "\n     (stream-fold <callable> <seed> <stream>)");
  }

  // primitive "stream-fold-right" procedure:
  data primitive_STREAM_FOLD_RIGHT(data_vector&& args) {
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STREAM_FOLD_RIGHT,args);
    return stdlib_streams::stream_fold<false>(args, "stream-fold-right", "\n     (stream-fold-right <callable> <seed> <stream>)");
  }

  // primitive "stream->list" procedure:
  data primitive_STREAM_TO_LIST(data_vector&& args) {
    // Confirm given proper args (same signature as stream-drop & stream-take)
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_STREAM_TO_LIST,args);
    stdlib_streams::validate_stream_take_drop_args(args, "stream->list", "\n     (stream->list <stream> <size>)");
    // Invoke stream-take, convert substream -> exp -> list
    if(primitive_toolkit::data_is_nil(args[0])) return args[0];
    auto substream = primitive_STREAM_TAKE(std::move(args));
    data_vector stream_as_exp;
    stdlib_streams::unpack_stream_into_exp(std::move(substream), stream_as_exp);
    return primitive_toolkit::convert_data_vector_to_proper_list(stream_as_exp.begin(),stream_as_exp.end());
  }

  // primitive "list->stream" procedure:
  data primitive_LIST_TO_STREAM(data_vector&& args) {
    // Confirm given a single proper list arg
    stdlib_streams::confirm_given_one_arg(args,"list->stream","<list>");
    if(!primitive_toolkit::data_is_proper_list(args[0]))
      HEIST_THROW_ERR("'list->stream "<<HEIST_PROFILE(args[0])<<" isn't a proper list:" 
        "\n     (list->stream <list>)" << HEIST_FCN_ERR("list->stream",args));
    // Convert list -> exp -> stream
    if(primitive_toolkit::data_is_nil(args[0])) return args[0];
    auto par_as_exp = primitive_toolkit::convert_proper_list_to_data_vector(args[0]);
    if(par_as_exp.empty()) return data(symconst::emptylist);
    return stdlib_streams::convert_data_vector_to_stream(par_as_exp.begin(),par_as_exp.end());
  }

} // End of namespace heist

#endif