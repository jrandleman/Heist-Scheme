// Author: Jordan Randleman -- jrandleman@scu.edu -- seqs.hpp
// => Defines primitive sequence functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_SEQS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_SEQS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * ALGORITHMIC PRIMITIVES: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  // Sequence description for generic algorithm primitives
  #define SEQUENCE_DESCRIPTION "\n     <sequence> ::= <list> | <vector> | <string>"

  // primitive "empty" procedure (given <sequence>, return its empty version):
  data primitive_EMPTY(data_vector&& args) {
    stdlib_seqs::confirm_given_one_sequence_arg(args,"empty");
    if(primitive_toolkit::data_is_nil(args[0])) return symconst::emptylist;
    if(args[0].is_type(types::par))             return symconst::emptylist;
    if(args[0].is_type(types::vec))             return make_vec(data_vector());
    if(args[0].is_type(types::str))             return make_str("");
    HEIST_THROW_ERR("'empty given arg "<<HEIST_PROFILE(args[0])<<" isn't a proper sequence!" 
        "\n     (empty <sequence>)" SEQUENCE_DESCRIPTION << HEIST_FCN_ERR("empty",args));
    return GLOBALS::VOID_DATA_OBJECT; // never used due to the throw above
  }

  // primitive "length" procedure:
  data primitive_LENGTH(data_vector&& args) {
    stdlib_seqs::confirm_given_one_sequence_arg(args,"length");
    return stdlib_seqs::compute_length(args,"length","\n     (length <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "length+" procedure:
  // => return #f on circular lists (instead of error)
  data primitive_LENGTH_PLUS(data_vector&& args) {
    stdlib_seqs::confirm_given_one_sequence_arg(args, "length+");
    if(primitive_toolkit::data_is_circular_list(args[0]))
      return GLOBALS::FALSE_DATA_BOOLEAN;
    return stdlib_seqs::compute_length(args,"length+","\n     (length+ <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "reverse" procedure:
  data primitive_REVERSE(data_vector&& args) {
    stdlib_seqs::confirm_given_one_sequence_arg(args,"reverse");
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"reverse",
      "\n     (reverse <sequence>)" SEQUENCE_DESCRIPTION)) {
      case stdlib_seqs::heist_sequence::vec: 
        return make_vec(data_vector(args[0].vec->rbegin(), args[0].vec->rend()));
      case stdlib_seqs::heist_sequence::str: 
        return make_str(string(args[0].str->rbegin(), args[0].str->rend()));
      case stdlib_seqs::heist_sequence::nul: 
        return args[0];
      default:
        return stdlib_seqs::reverse_list(args[0]);
    }
  }

  // primitive "reverse!" procedure:
  data primitive_REVERSE_BANG(data_vector&& args) {
    stdlib_seqs::confirm_given_one_sequence_arg(args,"reverse!");
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"reverse!",
      "\n     (reverse! <sequence>)" SEQUENCE_DESCRIPTION)) {
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::reverse_bang_random_access_seq(args[0], &data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::reverse_bang_random_access_seq(args[0], &data::str);
      case stdlib_seqs::heist_sequence::nul:
        return GLOBALS::VOID_DATA_OBJECT;
      default:
        return stdlib_seqs::reverse_bang_list(args[0]);
    }
  }

  // primitive "fold" procedure:
  data primitive_FOLD(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (fold <callable> <init> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_FOLD,args);
    if(args.size() < 3) 
      HEIST_THROW_ERR("'fold received insufficient args (only " 
        << args.size() << "):" << format << HEIST_FCN_ERR("fold",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "fold", format);
    switch(stdlib_seqs::is_proper_sequence(args[2],args,"fold",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::fold_random_access_seq<true,types::vec>(procedure, args, format, &data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::fold_random_access_seq<true,types::str>(procedure, args, format, &data::str);
      case stdlib_seqs::heist_sequence::nul:
        return args[1];
      default:
        return stdlib_seqs::fold_list<true>(procedure, args, format);
    }
  }

  // primitive "fold-right" procedure:
  data primitive_FOLD_RIGHT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (fold-right <callable> <init> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_FOLD_RIGHT,args);
    if(args.size() < 3) 
      HEIST_THROW_ERR("'fold-right received insufficient args (only " 
        << args.size() << "):" << format << HEIST_FCN_ERR("fold-right",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "fold-right", format);
    switch(stdlib_seqs::is_proper_sequence(args[2],args,"fold-right",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::fold_random_access_seq<false,types::vec>(procedure, args, format, &data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::fold_random_access_seq<false,types::str>(procedure, args, format, &data::str);
      case stdlib_seqs::heist_sequence::nul:
        return args[1];
      default:
        return stdlib_seqs::fold_list<false>(procedure, args, format);
    }
  }

  // primitive "filter" procedure:
  data primitive_FILTER(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (filter <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_FILTER,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'filter received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("filter",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "filter", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"filter",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::random_access_seq_selective_iteration<stdlib_seqs::is_true_scm_condition>(procedure, args, types::vec, &data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::random_access_seq_selective_iteration<stdlib_seqs::is_true_scm_condition>(procedure, args, types::str, &data::str);
      case stdlib_seqs::heist_sequence::nul:
        return args[1];
      default:
        return stdlib_seqs::list_selective_iteration<stdlib_seqs::is_true_scm_condition>(procedure,args[1]);
    }
  }

  // primitive "map" procedure:
  data primitive_MAP(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (map <callable> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MAP,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'map received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("map",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "map", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"map",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::random_access_seq_map<types::vec>(procedure, args, "map", format, &data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::random_access_seq_map<types::str>(procedure, args, "map", format, &data::str);
      default:
        return stdlib_seqs::list_map(procedure, args, format);
    }
  }

  // primitive "map!" procedure:
  data primitive_MAP_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (map! <callable> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MAP_BANG,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'map! received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("map!",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "map!", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"map!",format)) {
      case stdlib_seqs::heist_sequence::vec:
        *args[1].vec = *stdlib_seqs::random_access_seq_map<types::vec>(procedure, args, "map!", format, &data::vec).vec;
        return GLOBALS::VOID_DATA_OBJECT;
      case stdlib_seqs::heist_sequence::str:
        *args[1].str = *stdlib_seqs::random_access_seq_map<types::str>(procedure, args, "map!", format, &data::str).str;
        return GLOBALS::VOID_DATA_OBJECT;
      default:
        return stdlib_seqs::list_map_bang(procedure, args, format);
    }
  }

  // primitive "for-each" procedure:
  data primitive_FOR_EACH(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (for-each <callable> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_FOR_EACH,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'for-each received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("for-each",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "for-each", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"for-each",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::random_access_seq_for_each<types::vec>(procedure, args, format, &data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::random_access_seq_for_each<types::str>(procedure, args, format, &data::str);
      default:
        return stdlib_seqs::list_for_each(procedure, args, format);
    }
  }

  // primitive "seq-copy!" procedure: 
  // NOTE: Copies elts from <source> over <dest>'s elts. 
  //       <dest>.size() is unaffected.
  data primitive_SEQ_COPY_BANG(data_vector&& args) { // 
    static constexpr const char * const format = 
      "\n     (seq-copy! <dest-sequence> <source-sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SEQ_COPY_BANG,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'seq-copy! received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("seq-copy!",args));
    if(args[0].type != args[1].type)
      HEIST_THROW_ERR("'seq-copy! args have mismatched types!" 
        << format << HEIST_FCN_ERR("seq-copy!",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"seq-copy!",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::random_access_seq_copy_bang(args[0].vec,args[1].vec);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::random_access_seq_copy_bang(args[0].str,args[1].str);
      case stdlib_seqs::heist_sequence::nul: return GLOBALS::VOID_DATA_OBJECT;
      default:                               return stdlib_seqs::list_copy_bang(args[0],args[1]);
    }
  }

  // primitive "count" procedure:
  data primitive_COUNT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (count <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_COUNT,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'count received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("count",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "count", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"count",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::random_access_seq_count(procedure, args, &data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::random_access_seq_count(procedure, args, &data::str);
      case stdlib_seqs::heist_sequence::nul:
        return num_type();
      default:
        return stdlib_seqs::list_count(args[1],procedure);
    }
  }

  // primitive "ref" procedure:
  data primitive_REF(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (ref <sequence> <index>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_REF,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'ref received incorrect # of args (given " 
        << args.size() << "):" << format 
        << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("ref",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"ref",format)) {
      case stdlib_seqs::heist_sequence::nul:
        HEIST_THROW_ERR("'ref 1st arg '() of type \"null\" has no elements to reference:" 
          << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("ref",args));
      case stdlib_seqs::heist_sequence::vec:
        return args[0].vec->operator[](stdlib_seqs::get_if_valid_vector_idx(args,"ref",format));
      case stdlib_seqs::heist_sequence::str:
        return args[0].str->operator[](stdlib_seqs::get_if_valid_string_idx(args,"ref",format));
      default:
        return stdlib_seqs::list_ref(args, format);
    }
  }

  // primitive "slice" procedure (generic 'sublist 'subvector 'substring):
  data primitive_SLICE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (slice <sequence> <start-index> <optional-length>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SLICE,args);
    if(args.size() < 2 || args.size() > 3)
      HEIST_THROW_ERR("'slice received incorrect # of args (given " 
        << args.size() << "):"<<format<<VALID_SEQUENCE_INDEX_RANGE<<HEIST_FCN_ERR("slice",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"slice",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::vector_slice(args,format);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::string_slice(args,format);
      default:                               return stdlib_seqs::list_slice(args,format);
    }
  }

  // primitive "set-index!" procedure ('vector-set! 'string-set! SRFIs):
  data primitive_SET_INDEX_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (set-index! <sequence> <index> <obj>)" SEQUENCE_DESCRIPTION;
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SET_INDEX_BANG,args);
    if(args.size() != 3) 
      HEIST_THROW_ERR("'set-index! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("set-index!",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"set-index!",format)) {
      case stdlib_seqs::heist_sequence::nul:
        HEIST_THROW_ERR("'set-index! 1st arg '() of type \"null\" has no elements to set:" 
          << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("set-index!",args));
      case stdlib_seqs::heist_sequence::vec:
        args[0].vec->operator[](stdlib_seqs::get_if_valid_vector_idx(args,"set-index!",format)) = args[2];
        return GLOBALS::VOID_DATA_OBJECT;
      case stdlib_seqs::heist_sequence::str:
        if(!args[2].is_type(types::chr))
          HEIST_THROW_ERR("'set-index! <string> for "<<HEIST_PROFILE(args[0])<<" received non-character set-value\n     " 
            << HEIST_PROFILE(args[2]) << '!' << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("set-index!",args));
        args[0].str->operator[](stdlib_seqs::get_if_valid_string_idx(args,"set-index!",format)) = args[2].chr;
        return GLOBALS::VOID_DATA_OBJECT;
      default:
        if(!stdlib_seqs::data_is_valid_index(args[1])) 
          HEIST_THROW_ERR("'set-index! <list> 2nd arg " << HEIST_PROFILE(args[1]) << " is an invalid <index>:"
            << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("set-index!",args));
        stdlib_seqs::list_set_index(args[0],(size_type)args[1].num.extract_inexact(),format,args);
        return GLOBALS::VOID_DATA_OBJECT;
    }
  }

  // primitive "swap-indices!" procedure:
  data primitive_SWAP_INDICES_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (swap-indices! <sequence> <index> <index>)" SEQUENCE_DESCRIPTION;
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SWAP_INDICES_BANG,args);
    if(args.size() != 3) 
      HEIST_THROW_ERR("'swap-indices! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("swap-indices!",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"swap-indices!",format)) {
      case stdlib_seqs::heist_sequence::nul:
        HEIST_THROW_ERR("'swap-indices! 1st arg '() of type \"null\" has no elements to swap:" 
          << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("swap-indices!",args));
      case stdlib_seqs::heist_sequence::vec:
        std::swap(args[0].vec->operator[](stdlib_seqs::get_if_valid_vector_idx(args,"swap-indices!",format)), 
                  args[0].vec->operator[](stdlib_seqs::get_if_valid_vector_idx(args,"swap-indices!",format,2)));
        return GLOBALS::VOID_DATA_OBJECT;
      case stdlib_seqs::heist_sequence::str:
        std::swap(args[0].str->operator[](stdlib_seqs::get_if_valid_string_idx(args,"swap-indices!",format)), 
                  args[0].str->operator[](stdlib_seqs::get_if_valid_string_idx(args,"swap-indices!",format,2)));
        return GLOBALS::VOID_DATA_OBJECT;
      default:
        stdlib_seqs::list_swap_indices(args,format);
        return GLOBALS::VOID_DATA_OBJECT;
    }
  }

  // primitive "fill!" procedure:
  data primitive_FILL_BANG(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (fill! <sequence> <fill-value>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_FILL_BANG,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'fill! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << HEIST_FCN_ERR("fill!",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"fill!",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::vector_fill_bang(args);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::string_fill_bang(args,format);
      default:                               return stdlib_seqs::list_fill_bang(args[0],args[1]);
    }
  }

  // primitive "append" procedure:
  // (append) = '()
  // (append <obj>) = <obj>
  // (append <empty-sequence1> ... <empty-sequenceN> <obj>) = <obj>
  // (append <sequence1> ... <sequenceN> <obj>) = <sequence1> | ... | <sequenceN> | <obj>
  data primitive_APPEND(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (append <sequence1> ... <sequenceN> <obj>)" SEQUENCE_DESCRIPTION;
    if(args.empty())     return data(symconst::emptylist);
    if(args.size() == 1) return args[0];
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"append",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::vector_append(args,format);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::string_append(args,format);
      default:                               return stdlib_seqs::list_append(args,format);
    }
  }

  // primitive "remove" procedure:
  data primitive_REMOVE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (remove <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_REMOVE,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'remove received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("remove",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "remove", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"remove",format)) {
      case stdlib_seqs::heist_sequence::nul:
        return args[1];
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::random_access_seq_selective_iteration<stdlib_seqs::is_false_scm_condition>(procedure, args, types::vec, &data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::random_access_seq_selective_iteration<stdlib_seqs::is_false_scm_condition>(procedure, args, types::str, &data::str);
      default:
        return stdlib_seqs::list_selective_iteration<stdlib_seqs::is_false_scm_condition>(procedure,args[1]);
    }
  }

  // primitive "remove-first" procedure:
  data primitive_REMOVE_FIRST(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (remove-first <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_REMOVE_FIRST,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'remove-first received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("remove-first",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "remove-first", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"remove-first",format)) {
      case stdlib_seqs::heist_sequence::nul:
        return args[1];
      case stdlib_seqs::heist_sequence::vec:
        return make_vec(stdlib_seqs::remove_first_or_last<true>(procedure,*args[1].vec));
      case stdlib_seqs::heist_sequence::str:
        return make_str(stdlib_seqs::remove_first_or_last<true>(procedure,*args[1].str));
      default:
        auto par_as_exp = primitive_toolkit::convert_proper_list_to_data_vector(args[1]);
        par_as_exp = stdlib_seqs::remove_first_or_last<true>(procedure,par_as_exp);
        return primitive_toolkit::convert_data_vector_to_proper_list(par_as_exp.begin(),par_as_exp.end());
    }
  }

  // primitive "remove-last" procedure:
  data primitive_REMOVE_LAST(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (remove-last <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_REMOVE_LAST,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'remove-last received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("remove-last",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "remove-last", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"remove-last",format)) {
      case stdlib_seqs::heist_sequence::nul:
        return args[1];
      case stdlib_seqs::heist_sequence::vec:
        return make_vec(stdlib_seqs::remove_first_or_last<false>(procedure,*args[1].vec));
      case stdlib_seqs::heist_sequence::str:
        return make_str(stdlib_seqs::remove_first_or_last<false>(procedure,*args[1].str));
      default:
        auto par_as_exp = primitive_toolkit::convert_proper_list_to_data_vector(args[1]);
        par_as_exp = stdlib_seqs::remove_first_or_last<false>(procedure,par_as_exp);
        return primitive_toolkit::convert_data_vector_to_proper_list(par_as_exp.begin(),par_as_exp.end());
    }
  }

  // primitive "delete" procedure:
  data primitive_DELETE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (delete <sequence> <index>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_DELETE,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'delete received incorrect # of args (given " 
        << args.size() << "):" << format 
        << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("delete",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"delete",format)) {
      case stdlib_seqs::heist_sequence::nul:
        HEIST_THROW_ERR("'delete 1st arg '() of type \"null\" has no elements to reference:" 
          << format << VALID_SEQUENCE_INDEX_RANGE << HEIST_FCN_ERR("delete",args));
      case stdlib_seqs::heist_sequence::vec:
        return make_vec(stdlib_seqs::random_access_seq_delete<stdlib_seqs::get_if_valid_vector_idx>(args,format,&data::vec));
      case stdlib_seqs::heist_sequence::str:
        return make_str(stdlib_seqs::random_access_seq_delete<stdlib_seqs::get_if_valid_string_idx>(args,format,&data::str));
      default:
        return stdlib_seqs::list_delete(args,format);
    }
  }

  // primitive "last" procedure (last elt):
  data primitive_LAST(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (last <sequence>)" SEQUENCE_DESCRIPTION;
      stdlib_seqs::confirm_given_one_sequence_arg(args,"last");
    if(stdlib_seqs::data_is_empty(args[0]))
      HEIST_THROW_ERR("'last empty sequence arg "<<HEIST_PROFILE(args[0])<<" has no last elt!" 
        << format << HEIST_FCN_ERR("last",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"last",format)) {
      case stdlib_seqs::heist_sequence::vec: return *args[0].vec->rbegin();
      case stdlib_seqs::heist_sequence::str: return *args[0].str->rbegin();
      default:                               return stdlib_seqs::list_last(args[0]);
    }
  }

  // primitive "tail" procedure (all except head):
  data primitive_TAIL(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (tail <sequence>)" SEQUENCE_DESCRIPTION;
    stdlib_seqs::confirm_given_one_sequence_arg(args,"tail");
    if(stdlib_seqs::data_is_empty(args[0]))
      HEIST_THROW_ERR("'tail empty sequence arg "<<HEIST_PROFILE(args[0])<<" has no tail!" 
        << format << HEIST_FCN_ERR("tail",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"tail",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return make_vec(data_vector(args[0].vec->begin()+1,args[0].vec->end()));
      case stdlib_seqs::heist_sequence::str:
        return make_str(string(args[0].str->begin()+1,args[0].str->end()));
      default:
        return args[0].par->second.shallow_copy();
    }
  }

  // primitive "head" procedure (first elt):
  data primitive_HEAD(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (head <sequence>)" SEQUENCE_DESCRIPTION;
    stdlib_seqs::confirm_given_one_sequence_arg(args,"head");
    if(stdlib_seqs::data_is_empty(args[0]))
      HEIST_THROW_ERR("'head empty sequence arg "<<HEIST_PROFILE(args[0])<<" has no 1st elt!" 
        << format << HEIST_FCN_ERR("head",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"head",format)) {
      case stdlib_seqs::heist_sequence::vec: return *args[0].vec->begin();
      case stdlib_seqs::heist_sequence::str: return *args[0].str->begin();
      default:                               return args[0].par->first;
    }
  }

  // primitive "init" procedure (all except last):
  data primitive_INIT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (init <sequence>)" SEQUENCE_DESCRIPTION;
    stdlib_seqs::confirm_given_one_sequence_arg(args,"init");
    if(stdlib_seqs::data_is_empty(args[0]))
      HEIST_THROW_ERR("'init empty sequence arg "<<HEIST_PROFILE(args[0])<<" has no init!" 
        << format << HEIST_FCN_ERR("init",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"init",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return make_vec(data_vector(args[0].vec->begin(),args[0].vec->end()-1));
      case stdlib_seqs::heist_sequence::str:
        return make_str(string(args[0].str->begin(),args[0].str->end()-1));
      default:
        return stdlib_seqs::list_init(args[0]);
    }
  }

  // primitive "seq=" procedure:
  data primitive_SEQ_EQ(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (seq= <elt=?> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.empty()) HEIST_THROW_ERR("'seq= didn't receive any args:" << format << HEIST_FCN_ERR("seq=", args));
    if(args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SEQ_EQ,args);
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "seq=", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"seq=",format)) {
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::random_access_seq_eq<types::vec>(procedure,args,format,&data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::random_access_seq_eq<types::str>(procedure,args,format,&data::str);
      default:
        return stdlib_seqs::list_seq_eq(procedure,args,format);
    }
  }

  // primitive "skip" procedure:
  data primitive_SKIP(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (skip <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SKIP,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'skip received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("skip",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "skip", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"skip",format)) {
      case stdlib_seqs::heist_sequence::nul: 
        return GLOBALS::FALSE_DATA_BOOLEAN;
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::search_random_access_seq_from_left<stdlib_seqs::is_false_scm_condition>(procedure,args,&data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::search_random_access_seq_from_left<stdlib_seqs::is_false_scm_condition>(procedure,args,&data::str);
      default:
        return stdlib_seqs::search_list_from_left<stdlib_seqs::is_false_scm_condition>(procedure,args[1]);
    }
  }

  // primitive "skip-right" procedure:
  data primitive_SKIP_RIGHT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (skip-right <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SKIP_RIGHT,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'skip-right received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("skip-right",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "skip-right", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"skip-right",format)) {
      case stdlib_seqs::heist_sequence::nul: 
        return args[1];
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::search_random_access_seq_from_right<stdlib_seqs::is_false_scm_condition>(procedure,args,&data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::search_random_access_seq_from_right<stdlib_seqs::is_false_scm_condition>(procedure,args,&data::str);
      default:
        return stdlib_seqs::search_list_from_right<stdlib_seqs::is_false_scm_condition>(procedure,args);
    }
  }

  // primitive "index" procedure:
  data primitive_INDEX(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (index <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_INDEX,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'index received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("index",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "index", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"index",format)) {
      case stdlib_seqs::heist_sequence::nul:
        return GLOBALS::FALSE_DATA_BOOLEAN;
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::search_random_access_seq_from_left<stdlib_seqs::is_true_scm_condition>(procedure,args,&data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::search_random_access_seq_from_left<stdlib_seqs::is_true_scm_condition>(procedure,args,&data::str);
      default:
        return stdlib_seqs::search_list_from_left<stdlib_seqs::is_true_scm_condition>(procedure,args[1]);
    }
  }

  // primitive "index-right" procedure:
  data primitive_INDEX_RIGHT(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (index-right <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_INDEX_RIGHT,args);
    if(args.size() != 2) 
      HEIST_THROW_ERR("'index-right received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("index-right",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "index-right", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"index-right",format)) {
      case stdlib_seqs::heist_sequence::nul:
        return args[1];
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::search_random_access_seq_from_right<stdlib_seqs::is_true_scm_condition>(procedure,args,&data::vec);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::search_random_access_seq_from_right<stdlib_seqs::is_true_scm_condition>(procedure,args,&data::str);
      default:
        return stdlib_seqs::search_list_from_right<stdlib_seqs::is_true_scm_condition>(procedure,args);
    }
  }

  // primitive "drop" procedure:
  data primitive_DROP(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_DROP,args);
    return stdlib_seqs::take_drop_template<
      stdlib_seqs::drop_GENERIC_logic<false>,
      stdlib_seqs::drop_GENERIC_logic<false>,
      stdlib_seqs::drop_GENERIC_logic<true>>(
        args, "drop", "\n     (drop <sequence> <length>)" SEQUENCE_DESCRIPTION
      );
  }

  // primitive "drop-right" procedure:
  data primitive_DROP_RIGHT(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_DROP_RIGHT,args);
    return stdlib_seqs::take_drop_template<
      stdlib_seqs::drop_right_GENERIC_logic<false>,
      stdlib_seqs::drop_right_GENERIC_logic<false>,
      stdlib_seqs::drop_right_GENERIC_logic<true>>(
        args, "drop-right", "\n     (drop-right <sequence> <length>)" SEQUENCE_DESCRIPTION
      );
  }

  // primitive "take" procedure:
  data primitive_TAKE(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_TAKE,args);
    return stdlib_seqs::take_drop_template<
      stdlib_seqs::take_GENERIC_logic<false>,
      stdlib_seqs::take_GENERIC_logic<false>,
      stdlib_seqs::take_GENERIC_logic<true>>(
        args, "take", "\n     (take <sequence> <length>)" SEQUENCE_DESCRIPTION
      );
  }

  // primitive "take-right" procedure:
  data primitive_TAKE_RIGHT(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_TAKE_RIGHT,args);
    return stdlib_seqs::take_drop_template<
      stdlib_seqs::take_right_GENERIC_logic<false>,
      stdlib_seqs::take_right_GENERIC_logic<false>,
      stdlib_seqs::take_right_GENERIC_logic<true>>(
        args, "take-right", "\n     (take-right <sequence> <length>)" SEQUENCE_DESCRIPTION
      );
  }

  // primitive "drop-while" procedure:
  data primitive_DROP_WHILE(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_DROP_WHILE,args);
    return stdlib_seqs::take_drop_while_template<
      stdlib_seqs::drop_while_GENERIC_logic<false>,
      stdlib_seqs::drop_while_GENERIC_logic<false>,
      stdlib_seqs::drop_while_GENERIC_logic<true>>(
        args, "drop-while", "\n     (drop-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION
      );
  }

  // primitive "drop-right-while" procedure:
  data primitive_DROP_RIGHT_WHILE(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_DROP_RIGHT_WHILE,args);
    return stdlib_seqs::take_drop_while_template<
      stdlib_seqs::drop_right_while_GENERIC_logic<false>,
      stdlib_seqs::drop_right_while_GENERIC_logic<false>,
      stdlib_seqs::drop_right_while_GENERIC_logic<true>>(
        args, "drop-right-while", "\n     (drop-right-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION
      );
  }

  // primitive "take-while" procedure:
  data primitive_TAKE_WHILE(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_TAKE_WHILE,args);
    return stdlib_seqs::take_drop_while_template<
      stdlib_seqs::take_while_GENERIC_logic<false>,
      stdlib_seqs::take_while_GENERIC_logic<false>,
      stdlib_seqs::take_while_GENERIC_logic<true>>(
        args, "take-while", "\n     (take-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION
      );
  }

  // primitive "take-right-while" procedure:
  data primitive_TAKE_RIGHT_WHILE(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_TAKE_RIGHT_WHILE,args);
    return stdlib_seqs::take_drop_while_template<
      stdlib_seqs::take_right_while_GENERIC_logic<false>,
      stdlib_seqs::take_right_while_GENERIC_logic<false>,
      stdlib_seqs::take_right_while_GENERIC_logic<true>>(
        args, "take-right-while", "\n     (take-right-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION
      );
  }

  // primitive "any" procedure:
  data primitive_ANY(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (any <predicate> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ANY,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'any received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("any",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "any", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"any",format)) {
      case stdlib_seqs::heist_sequence::nul:
        return GLOBALS::FALSE_DATA_BOOLEAN;
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::random_access_seq_any<types::vec>(procedure,args,&data::vec,format);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::random_access_seq_any<types::str>(procedure,args,&data::str,format);
      default:
        return stdlib_seqs::list_any(procedure,args,format);
    }
  }

  // primitive "every" procedure:
  data primitive_EVERY(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (every <predicate> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_EVERY,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'every received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("every",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "every", format);
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"every",format)) {
      case stdlib_seqs::heist_sequence::nul:
        return GLOBALS::FALSE_DATA_BOOLEAN;
      case stdlib_seqs::heist_sequence::vec:
        return stdlib_seqs::random_access_seq_every<types::vec>(procedure,args,&data::vec,format);
      case stdlib_seqs::heist_sequence::str:
        return stdlib_seqs::random_access_seq_every<types::str>(procedure,args,&data::str,format);
      default:
        return stdlib_seqs::list_every(procedure,args,format);
    }
  }

  // primitive "conj" procedure:
  data primitive_CONJ(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (conj <obj> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_CONJ,args);
    if(args.size() != 2)
      HEIST_THROW_ERR("'conj received incorrect # of arguments:" 
        << format << HEIST_FCN_ERR("conj",args));
    data_vector new_vec;
    switch(stdlib_seqs::is_proper_sequence(args[1],args,"conj",format)) {
      case stdlib_seqs::heist_sequence::vec:
        new_vec.insert(new_vec.end(),args[1].vec->begin(),args[1].vec->end());
        new_vec.push_back(args[0]);
        return make_vec(new_vec);
      case stdlib_seqs::heist_sequence::str:
        if(!args[0].is_type(types::chr))
          HEIST_THROW_ERR("'conj <string> 1st arg "<<HEIST_PROFILE(args[0])<<" isn't a character:"
            << format << HEIST_FCN_ERR("conj",args));
        return make_str((*args[1].str) + char(args[0].chr));
      default:
        data new_pair = data(make_par());
        new_pair.par->first = args[0];
        new_pair.par->second = args[1];
        return new_pair;
    }
  }

  /******************************************************************************
  * COERCION PRIMITIVES: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  // primitive "seq->list" procedure:
  data primitive_COERCE_SEQ_TO_LIST(data_vector&& args) {
    static constexpr const char * const format = "\n     (seq->list <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 1) 
      HEIST_THROW_ERR("'seq->list didn't receive exactly 1 arg!" << format << HEIST_FCN_ERR("seq->list",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"seq->list",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::convert_vector_to_list(args[0]);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::convert_string_to_list(*args[0].str);
      default: return args[0].shallow_copy();
    }
  }

  // primitive "seq->vector" procedure:
  data primitive_COERCE_SEQ_TO_VECTOR(data_vector&& args) {
    static constexpr const char * const format = "\n     (seq->vector <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 1) 
      HEIST_THROW_ERR("'seq->vector didn't receive exactly 1 arg!" << format << HEIST_FCN_ERR("seq->vector",args));
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"seq->vector",format)) {
      case stdlib_seqs::heist_sequence::vec: return args[0].shallow_copy();
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::convert_string_to_vector(*args[0].str);
      default: return stdlib_seqs::convert_list_to_vector(args[0]);
    }
  }

  // primitive "seq->string" procedure:
  data primitive_COERCE_SEQ_TO_STRING(data_vector&& args) {
    static constexpr const char * const format = "\n     (seq->string <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 1) 
      HEIST_THROW_ERR("'seq->string didn't receive exactly 1 arg!" << format << HEIST_FCN_ERR("seq->string",args));
    data str;
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"seq->string",format)) {
      case stdlib_seqs::heist_sequence::str: return args[0].shallow_copy();
      case stdlib_seqs::heist_sequence::vec: 
        if(!stdlib_seqs::convert_vector_to_string(args[0],str))
          HEIST_THROW_ERR("'seq->string vector " << args[0] << " has a non-character element!"
            << format << HEIST_FCN_ERR("seq->string",args));
        return str;
      default: 
        if(!stdlib_seqs::convert_list_to_string(args[0],str))
          HEIST_THROW_ERR("'seq->string list " << args[0] << " has a non-character element!"
            << format << HEIST_FCN_ERR("seq->string",args));
        return str;
    }
  }

  /******************************************************************************
  * SET PRIMITIVES: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  // primitive "union" procedure:
  data primitive_UNION(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (union <elt=?> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ANY,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'union received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("union",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "union", format);
    args.erase(args.begin());
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"union",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::vector_union(procedure,args,format);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::string_union(procedure,args,format);
      default:                               return stdlib_seqs::list_union(procedure,args,format);
    }
  }

  // primitive "intersection" procedure:
  data primitive_INTERSECTION(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (intersection <elt=?> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ANY,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'intersection received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("intersection",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "intersection", format);
    args.erase(args.begin());
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"intersection",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::vector_intersection(procedure,args,format);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::string_intersection(procedure,args,format);
      default:                               return stdlib_seqs::list_intersection(procedure,args,format);
    }
  }

  // primitive "symmetric-difference" procedure:
  data primitive_SYMMETRIC_DIFFERENCE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (symmetric-difference <elt=?> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ANY,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'symmetric-difference received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("symmetric-difference",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "symmetric-difference", format);
    args.erase(args.begin());
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"symmetric-difference",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::vector_sym_diff(procedure,args,format);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::string_sym_diff(procedure,args,format);
      default:                               return stdlib_seqs::list_sym_diff(procedure,args,format);
    }
  }

  // primitive "difference" procedure:
  data primitive_DIFFERENCE(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (difference <elt=?> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ANY,args);
    if(args.size() < 2) 
      HEIST_THROW_ERR("'difference received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("difference",args));
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "difference", format);
    args.erase(args.begin());
    switch(stdlib_seqs::is_proper_sequence(args[0],args,"difference",format)) {
      case stdlib_seqs::heist_sequence::vec: return stdlib_seqs::vector_diff(procedure,args,format);
      case stdlib_seqs::heist_sequence::str: return stdlib_seqs::string_diff(procedure,args,format);
      default:                               return stdlib_seqs::list_diff(procedure,args,format);
    }
  }

  /******************************************************************************
  * SORTING PRIMITIVES: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  // primitive "sort" procedure:
  data primitive_SORT(data_vector&& args) {
    constexpr const char * const format = 
      "\n     (sort <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SORT,args);
    // confirm has a valid argument signature
    stdlib_seqs::confirm_sortable_sequence(args, "sort", format);
    // return if sorting the empty list
    if(args[1].is_type(types::sym)) return args[1];
    // sort the sequence
    return stdlib_seqs::sort_sequence(args, "sort", format);
  }

  // primitive "sort!" procedure:
  data primitive_SORT_BANG(data_vector&& args) {
    constexpr const char * const format = 
      "\n     (sort! <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SORT_BANG,args);
    // confirm has a valid argument signature
    stdlib_seqs::confirm_sortable_sequence(args, "sort!", format);
    // return if sorting the empty list (already sorted)
    if(args[1].is_type(types::sym)) return GLOBALS::VOID_DATA_OBJECT;
    // set the sequence to its sorted variant
    return stdlib_seqs::mutatable_assign_scm_sequence(args[1],
      stdlib_seqs::sort_sequence(args,"sort!",format));
  }

  // primitive "sorted?" procedure:
  data primitive_SORTEDP(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_SORTEDP,args);
    // confirm has a valid argument signature
    stdlib_seqs::confirm_sortable_sequence(args, "sorted?", "\n     (sorted? <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
    // return if sorting the empty list
    if(args[1].is_type(types::sym)) return GLOBALS::TRUE_DATA_BOOLEAN;
    // unpack the sequence
    data_vector sequence;
    stdlib_seqs::cast_scheme_sequence_to_ast(args[1],sequence);
    // confirm the unpacked sequence is sorted as per the args[0] procedure
    if(sequence.size() > 1) {
      auto procedure = primitive_toolkit::convert_callable_to_procedure(args[0]);
      for(size_type i = 0, n = sequence.size(); i+1 < n; ++i) {
        data_vector args_list(2);
        args_list[0] = sequence[i], args_list[1] = sequence[i+1];
        if(stdlib_seqs::is_false_scm_condition(procedure,std::move(args_list)))
          return GLOBALS::FALSE_DATA_BOOLEAN;
      }
    }
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "merge" procedure:
  data primitive_MERGE(data_vector&& args) { 
    constexpr const char * const format = 
      "\n     (merge <predicate> <sequence1> <sequence2>)" SEQUENCE_DESCRIPTION;
    if(!args.empty() && args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MERGE,args);
    // Confirm given correct # of args needed
    if(args.size() != 3) 
      HEIST_THROW_ERR("'merge received incorrect # of args (given " 
        << args.size() << "):" << format << HEIST_FCN_ERR("merge",args));
    // Confirm given a procedure
    auto procedure = primitive_toolkit::validate_callable_and_convert_to_procedure(args[0], args, "merge", format);
    // Confirm given only proper lists, only vectors, or only strings
    stdlib_seqs::is_proper_sequence(args[1],args,"merge",format);
    stdlib_seqs::is_proper_sequence(args[2],args,"merge",format);
    // If given '() and a proper list, return the proper list
    if((args[1].is_type(types::sym) && args[2].is_type(types::par)) || 
       (args[2].is_type(types::sym) && args[1].is_type(types::par)))
      return args[1].is_type(types::sym) ? args[2] : args[1];
    // Confirm given sequences are either 2 proper lists, vectors, or strings
    if(args[1].type != args[2].type)
      HEIST_THROW_ERR("'merge sequences " << HEIST_PROFILE(args[1]) << " and "
        << HEIST_PROFILE(args[2]) << "\n     are not matching sequence types!"
        << format << HEIST_FCN_ERR("merge",args));
    // If merging vectors or strings: 
    data_vector merged_sequence;
    if(!args[1].is_type(types::par)) 
      return stdlib_seqs::random_access_seq_merge(args,merged_sequence,format);
    // Else apply the procedure on each list elt & merge args as per the result into a list
    data_vector list_heads(args.begin()+1, args.end());
    stdlib_seqs::list_merge(list_heads,procedure,merged_sequence);
    return primitive_toolkit::convert_data_vector_to_proper_list(merged_sequence.begin(),merged_sequence.end());
  }

  // primitive "delete-neighbor-dups" procedure:
  data primitive_DELETE_NEIGHBOR_DUPS(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_DELETE_NEIGHBOR_DUPS,args);
    return stdlib_seqs::generic_DELETE_NEIGHBOR_DUPS<false>(args,"delete-neighbor-dups", 
      "\n     (delete-neighbor-dups <elt=?> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "delete-neighbor-dups!" procedure:
  data primitive_DELETE_NEIGHBOR_DUPS_BANG(data_vector&& args) {
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_DELETE_NEIGHBOR_DUPS_BANG,args);
    return stdlib_seqs::generic_DELETE_NEIGHBOR_DUPS<true>(args,"delete-neighbor-dups!", 
      "\n     (delete-neighbor-dups! <elt=?> <sequence>)" SEQUENCE_DESCRIPTION);
  }

} // End of namespace heist

#undef SEQUENCE_DESCRIPTION
#undef VALID_SEQUENCE_INDEX_RANGE
#endif