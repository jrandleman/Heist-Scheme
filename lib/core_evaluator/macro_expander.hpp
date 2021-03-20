// Author: Jordan Randleman -- jrandleman@scu.edu -- macro_expander.hpp
// => Contains 3 procedures to expand macros with the C++ Heist Scheme Interpreter

// PROVIDED PROCEDURES:
//   0. bool expand_macro_if_in_env(const string& label,data_vector args,env_type& env,data_vector& expanded)  // expand into <expanded> & return success
//   1. bool is_macro_argument_label(const data& d, const str_vector& keywords)                                // <d> is a syntax-rules identifier
//   2. bool data_is_ellipsis(const data& d)                                                                   // <d> is the "..." symbol

#ifndef HEIST_MACRO_EXPANDER_HPP_
#define HEIST_MACRO_EXPANDER_HPP_

/******************************************************************************
* REPRESENTING MACRO SYNTACTIC EXTENSIONS -- SYNTAX-RULES PARSING TYPE ALIASES
******************************************************************************/

// (define-syntax <label>
//   (syntax-rules (<keywords>)
//     ((<pattern>) <template>)
//     ((<pattern>) <template>)
//     ((<pattern>) <template>))
//
// => token strings in the <keywords> list allow those symbols, and only 
//    those symbols, in places where they are mentioned w/in <pattern>s

// EXAMPLES:
// ; Redefining λ to expand to 'lambda
// (define-syntax λ
//   (syntax-rules ()
//     ((λ () body ...) (lambda () body ...))
//     ((λ (arg ...) body ...) (lambda (arg ...) body ...))))
//
// ; Macro simulating variadic multiplication if '* were a binary operation
// (define-syntax multiply-all 
//   (syntax-rules ()
//     ((multiply-all) 1)
//     ((multiply-all a) a)
//     ((multiply-all a b ...) (* a (multiply-all b ...)))))


// 0. Each macro Id entry in <MACRO_ID_TABLE> (ie <MACRO_ID_VAR_TABLE.first>) represents 
//      an instance of a macro identifier [in the pattern] to be expanded [in the template]
//      into a value(s) [from the matched expression].
// 1. <macId_name> gets the symbolic name of the identifier in question.
// 2. <macId_val_pos_map> gets a vector of value-position pairs
//    - "values" are those which the "name" should expand into
//      * >1 "value" indicates a variadic expansion of the "name"
//    - a "position" is a vector of indices to locate the "value" in the 
//      pattern-matched expression (multiple idxs to traverse the nested vectors)
// 3. <macId_values> returns a flatmap of the values in <macId_val_pos_map>
//    - this is maintained alongside <macId_val_pos_map> in a seperate structure 
//      internally in order to have fast reads (rather than generating a new instance
//      from <macId_val_pos_map> each time)

// 0. <VARARG_POSITIONS> (ie <MACRO_ID_VAR_TABLE.second>) tracks all of the position idx 
//      vectors of '...' symbols found in the macro pattern (1st idx of each row in the matrix
//      is detracted by 1 to disregard the intitial '_' & line up w/ the values of <MACRO_ID_TABLE>'s
//      idxs of values [from <macId_val_pos_map>] in the matched expression)

// 0. <MacroId_varArg_posPair> Holds 2 vectors, each holding nested vectors of position idxs 
//    (of an elt w/in nested vectors) for:
//    - .first: the current macro Id being parsed
//    - .second: the current variadic '...' symbol being detected

using macId_position_t = std::vector<size_type>;
using MacroId_varArg_posPair = std::pair<macId_position_t,macId_position_t>;
using MACRO_ID_VAL_POS_PAIR = std::pair<data_vector,macId_position_t>;
using MACRO_ID_VAL_POS_PAIRS = std::vector<MACRO_ID_VAL_POS_PAIR>;
using MACRO_ID = std::tuple<string,MACRO_ID_VAL_POS_PAIRS,data_vector>;
using MACRO_ID_TABLE = std::vector<MACRO_ID>;
using VARARG_POSITIONS = std::vector<macId_position_t>;
using MACRO_ID_VAR_TABLE = std::pair<MACRO_ID_TABLE,VARARG_POSITIONS>;

// Node elt in the variadic expansion process
struct macro_expansion_node {
  string id_name;                             // Identifier name being expanded
  std::vector<macro_expansion_node> children;   // Variadic subgroup children
  std::vector<macId_position_t> positions;      // Position vector(s) of leaf node value(s)
  data_vector values;                              // Leaf node value(s)
  bool is_variadic = false;                     // Determines whether value corresponds to ...
  bool is_leaf()const{return children.empty();} // Determines valid elt: true ? value : children
  macro_expansion_node(const string& name, const bool& variadic_node = false) : id_name(name), is_variadic(variadic_node) {}
};

// - Topmost node (ie node of in <MACRO_EXPANSION_TREE> is a symbol w/ children)
//   * NON-VARIADIC identifiers are repn'd by nodes that are both ROOTS & a LEAF
// - Leaves are ultimate values to be expanded into
// - Intermediate nodes repn any multi-layered variadic expansion [ie (a ...) ...]
using MACRO_EXPANSION_TREES_t = std::vector<macro_expansion_node>;


// Accessors
string& macId_name(MACRO_ID& macId_instance)                        noexcept{return std::get<0>(macId_instance);}
MACRO_ID_VAL_POS_PAIRS& macId_val_pos_map(MACRO_ID& macId_instance) noexcept{return std::get<1>(macId_instance);}
data_vector& macId_values(MACRO_ID& macId_instance)                 noexcept{return std::get<2>(macId_instance);}

/******************************************************************************
* REPRESENTING MACRO SYNTACTIC EXTENSIONS -- GENERAL PARSING HELPER FUNCTIONS
******************************************************************************/

// Confirm whether the given word is a keyword
bool is_keyword(const string& word, const str_vector& keywords)noexcept{
  return std::find(keywords.begin(), keywords.end(), word) != keywords.end();
}


bool data_is_ellipsis(const data& d)noexcept{
  return d.is_type(types::sym) && d.sym == symconst::ellipsis;
}


// Primitive symbolic literals: #t #f '()
bool is_primitive_symbolic_literal(const data& obj)noexcept{
  return obj.is_type(types::sym) && 
    (obj.sym == symconst::true_t || obj.sym == symconst::false_t || obj.sym == symconst::emptylist);
}


// Confirm <pat_entity> is a potential macro identifier
// => WARNING: Doesn't check for whether is a keyword (used _only_ in expansion)!
bool is_symbolic_macro_identifier(const data& pat_entity)noexcept{
  return pat_entity.is_type(types::sym) && !is_primitive_symbolic_literal(pat_entity) &&
         pat_entity.sym != symconst::ellipsis;
}


// Confirm <pat_entity> is a macro argument (non-keyword) name
bool is_macro_argument_label(const data& pat_entity, const str_vector& keywords)noexcept{
  return is_symbolic_macro_identifier(pat_entity) && !is_keyword(pat_entity.sym, keywords);
}


// Confirm whether 'pattern' is argless but was given 'args' (or vise versa)
bool incompatible_void_arg_use(const data_vector& pattern, const data_vector& args)noexcept{
  return (pattern.size() == 1) ^ args.empty(); // pattern_is_argless ^ args_is_argless;
}


// Associate a pattern's macro identifier to the objects it will expand into
void register_macro_identifier_expansion_values(MACRO_ID_TABLE& ID_TO_VAL_MAP,const string& id_name, 
                                                data_vector&& expansion_values,const macId_position_t& macId_pos_vector)noexcept{
  for(auto& id : ID_TO_VAL_MAP) {
    if(macId_name(id) == id_name) {
      // Add to the flatmap of values
      auto& id_values = macId_values(id);
      id_values.insert(id_values.end(), expansion_values.begin(), expansion_values.end());
      // Add to the map of values-to-positions
      auto& val_pos_map = macId_val_pos_map(id);
      val_pos_map.push_back(std::make_pair(expansion_values,macId_pos_vector));
      return;
    }
  }
  MACRO_ID_VAL_POS_PAIRS val_pos_pairs(1,std::make_pair(expansion_values,macId_pos_vector));
  ID_TO_VAL_MAP.push_back(std::make_tuple(id_name,val_pos_pairs,expansion_values));
}

/******************************************************************************
* REPRESENTING MACRO SYNTACTIC EXTENSIONS -- PATTERN MATCHING HELPER FUNCTIONS
******************************************************************************/

bool compare_pattern_args_exp_match(const data_vector&,const data_vector&,const str_vector&,MACRO_ID_VAR_TABLE&,
                                    const size_type&,MacroId_varArg_posPair)noexcept;


// Verify if pat_elt is a keyword that arg_elt is the same keyword
bool mismatched_keywords(const data& pat_elt, const data& arg_elt, const str_vector& keywords)noexcept{
  if(pat_elt.is_type(types::sym) && is_keyword(pat_elt.sym,keywords))
    return !arg_elt.is_type(types::sym) || arg_elt.sym != pat_elt.sym;
  return false;
}


// Confirm given 2 incompatible atomics
bool mismatched_atomics(const data& pat_entity, const data& arg_entity)noexcept{
  if(is_primitive_symbolic_literal(pat_entity))
     return !is_primitive_symbolic_literal(arg_entity) || pat_entity.sym != arg_entity.sym;
  if(pat_entity.is_type(types::sym) || pat_entity.is_type(types::exp)) return false;
  return !pat_entity.noexcept_equal(arg_entity);
}


// Confirm the 2 given pattern/arg elts are mismatched subexpressions
bool mismatched_subexpressions(const data& pat_elt, const data& arg_elt, const str_vector& keywords, 
                               MACRO_ID_VAR_TABLE& MID_VARG_PAIR, MacroId_varArg_posPair macId_varArg_vecs, 
                               const size_type& args_idx, const size_type& pat_idx)noexcept{
  if(!pat_elt.is_type(types::exp) || !arg_elt.is_type(types::exp)) return true;
  if(pat_elt.exp.empty()) return !arg_elt.exp.empty();
  macId_varArg_vecs.first.push_back(args_idx), macId_varArg_vecs.second.push_back(pat_idx);
  return !compare_pattern_args_exp_match(pat_elt.exp,arg_elt.exp,keywords,MID_VARG_PAIR,0,macId_varArg_vecs);
}


// Handle '...' pattern analysis
bool account_for_pattern_ellipsis_and_return_whether_no_match(const data_vector& args_exp, size_type& args_idx, const data& pat_obj_prior_ellipsis,
                                                              const size_type& number_args_left_after_variadic, const str_vector& keywords,
                                                              MACRO_ID_VAR_TABLE& MID_VARG_PAIR,MacroId_varArg_posPair macId_varArg_vecs,
                                                              const size_type& pat_idx)noexcept{
  // Start associating objs based on the first obj prior "..."'s position
  --args_idx;
  // Confirm enough room in <args_exp> for the variadic
  const auto& args_size = args_exp.size();
  if(number_args_left_after_variadic + args_idx >= args_size) return true;
  const auto va_objs_end = args_size - number_args_left_after_variadic;
  // Confirm each variadic obj in <args_exp> matches the layout of <pat_obj_prior_ellipsis>
  // Symbol Identifiers may expand to _any_ form
  if(pat_obj_prior_ellipsis.is_type(types::sym)) {
    macId_varArg_vecs.first.push_back(args_idx);
    register_macro_identifier_expansion_values(MID_VARG_PAIR.first,pat_obj_prior_ellipsis.sym,
                                                                   data_vector(args_exp.begin() + args_idx, 
                                                                            args_exp.begin() + va_objs_end),
                                                                   macId_varArg_vecs.first);
    const auto number_of_va_objs_in_args = va_objs_end - args_idx;
    args_idx += number_of_va_objs_in_args - 1; // advance <args_idx> to the last va obj associated
  // Expression Identifiers _may only_ expand into expressions of the same form
  } else {
    for(; args_idx < va_objs_end; ++args_idx)
      if(mismatched_subexpressions(pat_obj_prior_ellipsis,args_exp[args_idx],keywords,MID_VARG_PAIR,macId_varArg_vecs,args_idx,pat_idx))
        return true;
    --args_idx; // move back to the last associated obj (accounts for '++' in loop returning to)
  }
  // Save current position vector for ... identifier
  macId_varArg_vecs.second.push_back(pat_idx+1);
  MID_VARG_PAIR.second.push_back(macId_varArg_vecs.second);
  macId_varArg_vecs.second.pop_back();
  return false;
}


// Confirm whether the pattern sub-expression matches the 'args' sub-expression
bool compare_pattern_args_exp_match(const data_vector& pat_exp, const data_vector& args_exp, const str_vector& keywords,
                                    MACRO_ID_VAR_TABLE& MID_VARG_PAIR, const size_type& pat_idx_start, 
                                    MacroId_varArg_posPair macId_varArg_vecs)noexcept{
  // Confirm whether <pat_exp> & <args_exp> match one another
  size_type pat_idx = pat_idx_start, args_idx = 0, args_size = args_exp.size(), pat_size = pat_exp.size();
  for(; pat_idx < pat_size && args_idx < args_size; ++pat_idx, ++args_idx){
    // Check for proper "..." use in the pat_exp definition
    if(data_is_ellipsis(pat_exp[pat_idx])) { // Guarenteed pat_idx > 0 by syntax-rules analysis
      macId_varArg_vecs.second.push_back(pat_idx);
      MID_VARG_PAIR.second.push_back(macId_varArg_vecs.second);
      macId_varArg_vecs.second.pop_back();
      if(account_for_pattern_ellipsis_and_return_whether_no_match(args_exp, args_idx,pat_exp[pat_idx-1], pat_size-pat_idx-1,
                                                                  keywords, MID_VARG_PAIR,macId_varArg_vecs,pat_idx-1)){
        return false;
      }
    // Register the pat_exp's identifier & associated expansion value
    } else if(is_macro_argument_label(pat_exp[pat_idx],keywords)) {
      if(pat_idx+1 == pat_size || !data_is_ellipsis(pat_exp[pat_idx+1])) {
        macId_varArg_vecs.first.push_back(args_idx);
        register_macro_identifier_expansion_values(MID_VARG_PAIR.first,pat_exp[pat_idx].sym,data_vector(1,args_exp[args_idx]),macId_varArg_vecs.first);
        macId_varArg_vecs.first.pop_back();
      }
    // Verify matching subexpressions
    } else if(pat_exp[pat_idx].is_type(types::exp)) {
      if(!args_exp[args_idx].is_type(types::exp) || 
        ((pat_idx+1 == pat_size || !data_is_ellipsis(pat_exp[pat_idx+1])) &&
          mismatched_subexpressions(pat_exp[pat_idx],args_exp[args_idx],keywords,MID_VARG_PAIR,macId_varArg_vecs,args_idx,pat_idx))) {
        return false;
      }
    // Verify literal & keyword use are aligned
    } else if(mismatched_atomics(pat_exp[pat_idx],args_exp[args_idx]) || 
              mismatched_keywords(pat_exp[pat_idx],args_exp[args_idx],keywords)){
      return false;
    }
  }
  // Register the last identifier if variadic portion of expansion @ the end of the pattern & empty in args
  if(pat_idx+1 == pat_size && data_is_ellipsis(pat_exp[pat_idx]) && args_idx == args_size) {
    if(is_macro_argument_label(pat_exp[pat_idx-1],keywords)) {
      macId_varArg_vecs.second.push_back(pat_idx);
      MID_VARG_PAIR.second.push_back(macId_varArg_vecs.second);
      macId_varArg_vecs.second.pop_back();
      macId_varArg_vecs.first.push_back(args_idx-1);
      register_macro_identifier_expansion_values(MID_VARG_PAIR.first,pat_exp[pat_idx-1].sym,data_vector(1,args_exp[args_idx-1]),macId_varArg_vecs.first);
      macId_varArg_vecs.first.pop_back();
      return true;
    }
    return !account_for_pattern_ellipsis_and_return_whether_no_match(args_exp, args_idx, pat_exp[pat_idx-1], pat_size-pat_idx-1, 
                                                                     keywords, MID_VARG_PAIR, macId_varArg_vecs, pat_idx-1);
  }
  // Verify both <pat_exp> & <arg_exp> have been fully iterated
  return pat_idx == pat_size && args_idx == args_size;
}


// Confirm the given arg combo matches the given pattern (in terms of layout)
bool is_pattern_match(const data_vector& args,const str_vector& keywords,const data_vector& pattern,
                                                         MACRO_ID_VAR_TABLE& MID_VARG_PAIR)noexcept{
  if(incompatible_void_arg_use(pattern,args)) return false;
  MacroId_varArg_posPair macId_varArg_vecs;
  if(!compare_pattern_args_exp_match(pattern,args,keywords,MID_VARG_PAIR,1,macId_varArg_vecs)){
    MID_VARG_PAIR.first.clear();
    MID_VARG_PAIR.second.clear();
    return false;
  }
  return true;
}


// Returns whether the given args correspond to the given macro
bool is_macro_match(const data_vector& args, const syn_type& mac, size_type& match_idx, 
                                            MACRO_ID_VAR_TABLE& MID_VARG_PAIR)noexcept{
  for(size_type i = 0, n = mac.patterns.size(); i < n; ++i) {
    if(is_pattern_match(args, mac.keywords, mac.patterns[i], MID_VARG_PAIR)) {
      match_idx = i;
      return true;
    }
  }
  return false;
}

/******************************************************************************
* MACRO SYNTACTIC EXTENSIONS -- EXPANSION HELPER FUNCTIONS
******************************************************************************/

// Recursively prints <MACRO_EXPANSION_TREES> children subgroups
void recur_stringify_MACRO_EXPANSION_TREES(const MACRO_EXPANSION_TREES_t& children,string& buffer)noexcept{
  for(const auto& child : children) {
    if(!child.is_variadic) {
      buffer += "NON-VARIADIC = " + data(child.values).noexcept_write() + ',';
    } else if(child.is_leaf()) {
      buffer += data(child.values).noexcept_write() + ',';
    } else {
      buffer += '[';
      recur_stringify_MACRO_EXPANSION_TREES(child.children,buffer);
      buffer += ']';
    }
  }
}

// Stringifies <MACRO_EXPANSION_TREES> contents
string stringify_MACRO_EXPANSION_TREES(const MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES)noexcept{
  string buffer("     ======================\n     MACRO_EXPANSION_TREES:\n     ");
  // for each tree
  for(const auto& tree : MACRO_EXPANSION_TREES) {
    buffer += "  " + tree.id_name + ": ";
    // print leaves immediately
    if(!tree.is_variadic) {
      buffer += "NON-VARIADIC = " + data(tree.values).noexcept_write() + "\n     ";
    // recursively print subgroups
    } else if(tree.is_leaf()) {
      buffer += "LEAF: " + data(tree.values).noexcept_write() + "\n     ";
    } else {
      buffer += '[';
      recur_stringify_MACRO_EXPANSION_TREES(tree.children,buffer);
      buffer += "]\n     ";
    }
  }
  return buffer + "======================";
}


// Generate a unique hashed id_name for the expanded symbol
string hash_macro_expansion_identifier(const string& id_name,const bool& finished_expanding = false)noexcept{
  static size_type IDX_1 = 0, IDX_2 = 0;
  if(finished_expanding) {
    IDX_1 = IDX_2 = 0;
    return "";
  } else {
    if(IDX_1 != GLOBALS::MAX_SIZE_TYPE)
      return id_name + '-' + std::to_string(IDX_2) + '-' + std::to_string(IDX_1++);
    return id_name + '-' + std::to_string(++IDX_2) + '-' + std::to_string(IDX_1++);
  }
}


// Changes all <id_name> in <id_node> & below to be <tagged_symbol>
void propagate_new_tagged_identifier_name(const string& tagged_symbol,macro_expansion_node& id_node)noexcept{
  id_node.id_name = tagged_symbol;
  if(id_node.is_leaf()) return;
  for(auto& child : id_node.children)
    propagate_new_tagged_identifier_name(tagged_symbol,child);
}


// Get idx of <id_name> in <MACRO_EXPANSION_TREES>. Returns <GLOBALS::MAX_SIZE_TYPE> if not found.
size_type find_macro_identifier_leaf_index(const MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES,const string& id_name)noexcept{
  for(size_type i = 0, n = MACRO_EXPANSION_TREES.size(); i < n; ++i)
    if(MACRO_EXPANSION_TREES[i].id_name == id_name) return i;
  return GLOBALS::MAX_SIZE_TYPE;
}


// Expand level-1 ids, tag all nested variadic ids, wrench up tagged children of nested variadic ids
void tag_and_expand_identifiers_while_wrenching_up_children(const size_type expansion_No, data_vector& expansions,
                                                                  MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES)noexcept{
  for(size_type i = 0, n = expansions.size(); i < n; ++i) {
    if(expansions[i].is_type(types::exp)) {
      tag_and_expand_identifiers_while_wrenching_up_children(expansion_No,expansions[i].exp,MACRO_EXPANSION_TREES);
    } else if(is_symbolic_macro_identifier(expansions[i])) {
      auto val_idx = find_macro_identifier_leaf_index(MACRO_EXPANSION_TREES,expansions[i].sym);
      if(val_idx == GLOBALS::MAX_SIZE_TYPE) continue; // symbol != variadic macro identifier
      // Splice in level-1 (non-nested) ... value
      if(MACRO_EXPANSION_TREES[val_idx].is_leaf()) {
        expansions[i] = MACRO_EXPANSION_TREES[val_idx].values[expansion_No];
      // Tag nested ... identifier to be expanded further, & wrench up the associated child node as a new (tagged) root
      } else {
        auto tagged_symbol = hash_macro_expansion_identifier(expansions[i].sym);
        expansions[i].sym = tagged_symbol; // tag symbol
        MACRO_EXPANSION_TREES.push_back(MACRO_EXPANSION_TREES[val_idx].children[expansion_No]); // wrench up child
        propagate_new_tagged_identifier_name(tagged_symbol,*MACRO_EXPANSION_TREES.rbegin()); // tag up wrenched child
      }
    }
  }
}


// <verify_all_identifiers_have_same_variadic_length> helper
void confirm_identifier_variadic_length_is_consistent(size_type& total_expansions,     const data_vector& exp,
                                                      const data_vector& expanded_exp, const data_vector& args,
                                                      const string& name,              const size_type& result, 
                                                      const MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES){
  if(total_expansions == GLOBALS::MAX_SIZE_TYPE) {
    total_expansions = result;
  } else if(total_expansions != result && result != GLOBALS::MAX_SIZE_TYPE) {
    THROW_ERR("'syntax-rules Different variadic identifiers can't expand in the same template expression!"
      "\n     Length 1 = " << total_expansions << ", Length 2 = " << result << 
      "\n     In subexpression: [ " << exp << " ]"
      "\n     Of template expansion: [ " << expanded_exp << " ]\n" 
      << stringify_MACRO_EXPANSION_TREES(MACRO_EXPANSION_TREES) << FCN_ERR(name,args));
  }
}


// Returns the length that the identifiers match (throw error if any are off)
size_type verify_all_identifiers_have_same_variadic_length(const data_vector& args, const string& name, const data_vector& exp, 
                                                           const data_vector& expanded_exp, const MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES){
  size_type total_expansions = GLOBALS::MAX_SIZE_TYPE;
  for(auto& elt : exp) {
    if(elt.is_type(types::exp)) {
      confirm_identifier_variadic_length_is_consistent(total_expansions,exp,expanded_exp,args,name,
        verify_all_identifiers_have_same_variadic_length(args,name,elt.exp,expanded_exp,MACRO_EXPANSION_TREES),
        MACRO_EXPANSION_TREES);
    } else if(is_symbolic_macro_identifier(elt)) {
      auto val_idx = find_macro_identifier_leaf_index(MACRO_EXPANSION_TREES,elt.sym);
      if(val_idx == GLOBALS::MAX_SIZE_TYPE) continue; // symbol != variadic macro identifier
      confirm_identifier_variadic_length_is_consistent(total_expansions,exp,expanded_exp,args,name,
        MACRO_EXPANSION_TREES[val_idx].is_leaf() ? MACRO_EXPANSION_TREES[val_idx].values.size() : 
                                                   MACRO_EXPANSION_TREES[val_idx].children.size(),
        MACRO_EXPANSION_TREES);
    }
  }
  return total_expansions;
}


// Non-Variadics have been expanded, expand all (possibly nested) variadics identifiers
// NOTE: Traverses in POST-ORDER!
void expand_macro_variadic_identifiers(const data_vector& args, const string& name, data_vector& expanded_exp,
                                                             MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES){
  for(size_type i = 0; i < expanded_exp.size(); ++i) {
    if(i+1 < expanded_exp.size() && data_is_ellipsis(expanded_exp[i+1])) {
      // Expand variadic symbolic identifer immediately (no ctoring of any expression)
      if(is_symbolic_macro_identifier(expanded_exp[i])) {
        auto val_idx = find_macro_identifier_leaf_index(MACRO_EXPANSION_TREES,expanded_exp[i].sym);
        if(val_idx == GLOBALS::MAX_SIZE_TYPE) continue; // symbol != variadic macro identifier
        // confirm expanding into a non-nested variadic identifier
        if(!MACRO_EXPANSION_TREES[val_idx].is_leaf())
          THROW_ERR("'syntax-rules Misplaced \"...\" after improper non-nested variadic identifier [ " 
            << expanded_exp[i].sym << " ]\n     in [ " << expanded_exp << " ]\n"
            << stringify_MACRO_EXPANSION_TREES(MACRO_EXPANSION_TREES) << FCN_ERR(name,args));
        // erase the identifier & "...", then insert the variadic expansion values
        auto& expansions = MACRO_EXPANSION_TREES[val_idx].values;
        expanded_exp.erase(expanded_exp.begin()+i,expanded_exp.begin()+i+2);
        expanded_exp.insert(expanded_exp.begin()+i,expansions.begin(),expansions.end());
        i += expansions.size() - 1; // -1 accounts for loop's "++i"
      // Expand variadic expressions by constructing N expressions filled in w/ N values
      } else if(expanded_exp[i].is_type(types::exp)) {
        // verify ... follows an expression using the same # of expansion values per identifier
        size_type total_expansions = verify_all_identifiers_have_same_variadic_length(args,name,expanded_exp[i].exp,
                                                                                      expanded_exp,MACRO_EXPANSION_TREES);
        if(total_expansions == GLOBALS::MAX_SIZE_TYPE)
          THROW_ERR("'syntax-rules Misplaced \"...\" after non-variadic subexpression [ " 
            << expanded_exp[i].exp << " ]\n     in [ " << expanded_exp << " ]" << FCN_ERR(name,args));
        data_vector expansions(total_expansions,expanded_exp[i].exp);
        // tag <expansions> nested identifiers, tag associated tree groups & 
        //   wrench them up to be a root (WHILE KEEPING THE NEW ROOTS IN PLACE)
        for(size_type i = 0, n = expansions.size(); i < n; ++i)
          tag_and_expand_identifiers_while_wrenching_up_children(i,expansions[i].exp,MACRO_EXPANSION_TREES);
        // expand the ctord exps & re-traverse to expand any nested ...
        expanded_exp.erase(expanded_exp.begin()+i,expanded_exp.begin()+i+2);
        expanded_exp.insert(expanded_exp.begin()+i,expansions.begin(),expansions.end());
        --i; // mv back to account for loop's "++" & completely re-traverse expanded exp
      // NOTE: SHOULD NEVER BE INVOKED, BUT KEPT HERE AS A SAFETY GUARD
      } else {
        THROW_ERR("'syntax-rules Misplaced \"...\" after non-variadic identifier [ " 
          << expanded_exp[i].sym << " ]\n     in [ " << expanded_exp << " ]\n"
          << stringify_MACRO_EXPANSION_TREES(MACRO_EXPANSION_TREES) << FCN_ERR(name,args));
      }
    // Parse the nested non-variadic expression
    } else if(expanded_exp[i].is_type(types::exp)) {
      expand_macro_variadic_identifiers(args,name,expanded_exp[i].exp,MACRO_EXPANSION_TREES);
    }
  } // End of for
}


// Expands non-variadics, and guarentees:
//   0. No expressions begin w/ ...
//   2. Any SYMBOLIC identifier followed by ... is variadic
//      => NOTE: EXPRESSIONS FOLLOWED BY ... HAVE __NOT__ BEEN VERIFIED THO !!!
void expand_non_variadic_macro_symbols(const data_vector& args, const string& name, data_vector& expanded_exp, 
                                                             MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES){
  for(size_type i = 0; i < expanded_exp.size(); ++i) {
    if(is_symbolic_macro_identifier(expanded_exp[i])) {
      // Expand non-variadic identifiers
      auto val_idx = find_macro_identifier_leaf_index(MACRO_EXPANSION_TREES,expanded_exp[i].sym);
      if(i+1 == expanded_exp.size() || !data_is_ellipsis(expanded_exp[i+1])) {
        if(val_idx != GLOBALS::MAX_SIZE_TYPE && !MACRO_EXPANSION_TREES[val_idx].is_variadic)
          expanded_exp[i] = MACRO_EXPANSION_TREES[val_idx].values[0];
      // Skip past ... if at a variadic identifier (handled in <expand_macro_variadic_identifiers>)
      } else if(val_idx != GLOBALS::MAX_SIZE_TYPE && MACRO_EXPANSION_TREES[val_idx].is_variadic) {
        ++i;
      // Catch non-macro syntax identifiers OR non-variadic syntax identifier followed by ...
      } else {
        THROW_ERR("'syntax-rules Misplaced \"...\" after non-variadic identifier [ " 
          << expanded_exp[i].sym << " ]\n     in [ " << expanded_exp << " ]" << FCN_ERR(name,args));
      }
    } else if(expanded_exp[i].is_type(types::exp)) {
      // Recursively expand symbolic identifiers
      expand_non_variadic_macro_symbols(args,name,expanded_exp[i].exp,MACRO_EXPANSION_TREES);
      // Skip past variadics after expressions (handled in <expand_macro_variadic_identifiers>)
      if(i+1 < expanded_exp.size() && data_is_ellipsis(expanded_exp[i+1])) ++i;
    } else if(data_is_ellipsis(expanded_exp[i])) {
      if(i) {
        THROW_ERR("'syntax-rules Misplaced \"...\" after non-variadic identifier [ " 
          << expanded_exp[i-1].sym << " ]\n     in [ " << expanded_exp << " ]" << FCN_ERR(name,args));
      } else {
        THROW_ERR("'syntax-rules Misplaced \"...\" at front of a template expression!" 
          << expanded_exp << FCN_ERR(name,args));
      }
    }
  }
}

/******************************************************************************
* MACRO SYNTACTIC EXTENSIONS -- UNWRAP ESCAPED VARIADIC TOKENS
******************************************************************************/

bool string_is_an_escaped_variadic_token(const string& str)noexcept{
  if(str.size() < 4 || str.compare(str.size()-3,3,"...")) 
    return false;
  for(size_type i = 0, n = str.size()-3; i < n; ++i)
    if(str[i] != '\\') return false;
  return true;
}


bool datum_is_an_escaped_variadic_token(const data& d)noexcept{
  return d.is_type(types::sym) && string_is_an_escaped_variadic_token(d.sym);
}


void unwrap_macro_escaped_variadic_tokens(data_vector& expanded)noexcept{
  for(size_type i = 0, n = expanded.size(); i < n; ++i) {
    if(expanded[i].is_type(types::exp))
      unwrap_macro_escaped_variadic_tokens(expanded[i].exp);
    else if(datum_is_an_escaped_variadic_token(expanded[i]))
      expanded[i].sym = expanded[i].sym.substr(1);
  }
}

/******************************************************************************
* MACRO SYNTACTIC EXTENSIONS -- DYNAMICALLY HASH syntax-hash IDENTIFIERS
******************************************************************************/

// Generate a unique hashed variant of the given macro arg name for safe expansion
// NOTE: Max Unique Hashes = (expt 18446744073709551615 18446744073709551615)
string safe_expansion_hashed_macro_arg(const string& label)noexcept{
  if(G.MACRO_HASH_IDX_1 != GLOBALS::MAX_SIZE_TYPE)
    return "heist:core:sh:" + label + '-' 
      + std::to_string(G.MACRO_HASH_IDX_2) + '-' 
      + std::to_string(G.MACRO_HASH_IDX_1++);
  return "heist:core:sh:" + label + '-' 
    + std::to_string(++G.MACRO_HASH_IDX_2) + '-' 
    + std::to_string(G.MACRO_HASH_IDX_1++);
}


void recursively_apply_syntax_hash_to_identifiers(data_vector& expanded_exp, const str_vector& hashed_ids, const str_vector& to_hash_ids)noexcept{
  const auto n = to_hash_ids.size();
  for(auto& datum : expanded_exp) {
    if(datum.is_type(types::sym)) {
      for(size_type i = 0; i < n; ++i) {
        if(to_hash_ids[i] == datum.sym) {
          datum.sym = hashed_ids[i];
          break;
        }
      }
    } else if(datum.is_type(types::exp)) {
      recursively_apply_syntax_hash_to_identifiers(datum.exp,hashed_ids,to_hash_ids);
    }
  }
}


void apply_syntax_hash_to_identifiers(data_vector& expanded_exp, const str_vector& to_hash_ids)noexcept{
  // Get the dynamic (runtime) hash of each <syntax-hash> identifier in the macro
  str_vector hashed_ids(to_hash_ids.size());
  for(size_type i = 0, n = to_hash_ids.size(); i < n; ++i)
    hashed_ids[i] = safe_expansion_hashed_macro_arg(to_hash_ids[i]);
  recursively_apply_syntax_hash_to_identifiers(expanded_exp,hashed_ids,to_hash_ids);
}

/******************************************************************************
* MACRO SYNTACTIC EXTENSIONS -- MATRIX DATA CLEANING HELPER FCNS PRIOR TREE
******************************************************************************/

void remove_VA_POS_MATRIX_duplicate_instances(VARARG_POSITIONS& VA_POS_MATRIX)noexcept{
  const auto sameRows = [](auto& row1, auto& row2) {
    if(row1.size() != row2.size()) return false;
    for(size_type i = 0, n = row1.size(); i < n; ++i)
      if(row1[i] != row2[i]) return false;
    return true;
  };
  for(size_type i = 0; i < VA_POS_MATRIX.size(); ++i)
    for(size_type j = i+1; j < VA_POS_MATRIX.size();) {
      if(sameRows(VA_POS_MATRIX[i],VA_POS_MATRIX[j]))
        VA_POS_MATRIX.erase(VA_POS_MATRIX.begin()+j);
      else
         ++j;
    }
}


// Correlate positions in pattern moreso to those in args by negating result of skipping initial '_'
void decrement_first_elt_of_each_VA_POS_VECTOR(VARARG_POSITIONS& VA_POS_MATRIX)noexcept{
  for(size_type i = 0, n = VA_POS_MATRIX.size(); i < n; ++i) --VA_POS_MATRIX[i][0];
}


// Compose the above 2 helper fcns functions to clean the position data
void clean_VA_POS_MATRIX_for_MACRO_ID_comparision(VARARG_POSITIONS& VA_POS_MATRIX)noexcept{
  remove_VA_POS_MATRIX_duplicate_instances(VA_POS_MATRIX);
  decrement_first_elt_of_each_VA_POS_VECTOR(VA_POS_MATRIX);
  // Sort variadics based on descending # of idxs, 
  //   to process subgroups of nested ... prior outer ...
  std::sort(VA_POS_MATRIX.begin(),VA_POS_MATRIX.end(),
    [](macId_position_t& e1,macId_position_t& e2){return e1.size()>e2.size();});
}


// Break down sequential grouped instances of variadic identifier matches into individual instances
void split_ID_TO_VAL_MAP_children_into_unique_entries(MACRO_ID_TABLE& ID_TO_VAL_MAP)noexcept{
  for(auto& id_val_pos_tuple : ID_TO_VAL_MAP) {
    auto& val_pos_pairs = macId_val_pos_map(id_val_pos_tuple);
    for(size_type i = 0; i < val_pos_pairs.size(); ++i) {
      // Expand the set of values into single value instances
      if(val_pos_pairs[i].first.size() > 1) {
        MACRO_ID_VAL_POS_PAIRS indiv_val_pos_instances;
        auto posv = val_pos_pairs[i].second;
        for(size_type j = 1, n = val_pos_pairs[i].first.size(); j < n; ++j) {
          ++(*posv.rbegin());
          indiv_val_pos_instances.push_back(std::make_pair(data_vector(1,val_pos_pairs[i].first[j]),posv));
        }
        // Erase the excess values in the original value set
        val_pos_pairs[i].first.erase(val_pos_pairs[i].first.begin()+1,val_pos_pairs[i].first.end());
        // Add the values/positions as individual instances
        val_pos_pairs.insert(val_pos_pairs.begin()+i+1,std::make_move_iterator(indiv_val_pos_instances.begin()),
                                                       std::make_move_iterator(indiv_val_pos_instances.end()));
      }
    }
  }
}


// Determine if <id_posv> begins w/ the elts in <prefix>
bool id_posv_begins_with_prefix(const macId_position_t& id_posv, const macId_position_t& prefix)noexcept{
  if(id_posv.size() < prefix.size()) return false;
  for(size_type i = 0, n = prefix.size(); i < n; ++i)
    if(id_posv[i] != prefix[i]) return false;
  return true;
}


// init <va_prefix> w/ the prefix values of <id_posv>
void get_new_VA_POSV_prefix(macId_position_t& va_prefix, const macId_position_t& id_posv)noexcept{
  std::copy(id_posv.begin(),id_posv.begin()+va_prefix.size(),va_prefix.begin());
}


// Cleans & reorganizes the <MACRO_ID_VAR_TABLE> table for easier analysis
void clean_MID_VARG_PAIR_for_macro_expansion_analysis(MACRO_ID_VAR_TABLE& MID_VARG_PAIR) {
  split_ID_TO_VAL_MAP_children_into_unique_entries(MID_VARG_PAIR.first);
  clean_VA_POS_MATRIX_for_MACRO_ID_comparision(MID_VARG_PAIR.second);
}


// Adds values & positions to a <macro_expansion_node>
// => NOTE: LAST UNUSED ARG IS JUST TO MATCH THE SAME FCN PTR TYPE AS <extract_id_children_subgroup>
void accumulate_id_leaf_values_and_positions(macro_expansion_node& id_node, MACRO_ID_VAL_POS_PAIR& val_pos_pair, MACRO_EXPANSION_TREES_t&)noexcept{
  id_node.values.insert(id_node.values.end(),val_pos_pair.first.begin(),val_pos_pair.first.end());
  id_node.positions.push_back(val_pos_pair.second);
}


// Confirm <posv_matrix> contains <sought_posv>
bool matrix_contains_vector(const macId_position_t& sought_posv, const std::vector<macId_position_t>& posv_matrix)noexcept{
  for(auto& posv : posv_matrix) {
    if(posv.size() != sought_posv.size()) continue;
    bool same_posv = true;
    for(size_type i = 0, n = posv.size(); i < n; ++i)
      if(posv[i] != sought_posv[i]) {
        same_posv = false;
        break;
      }
    if(same_posv) return true;
  }
  return false;
}


// Recursive search for <extract_id_children_subgroup>, returns whether found position in subgroup
bool extract_id_children_subgroup_recur(macro_expansion_node& child, MACRO_ID_VAL_POS_PAIR& val_pos_pair)noexcept{
  if(child.is_leaf()) return matrix_contains_vector(val_pos_pair.second,child.positions);
  for(auto& grand_child : child.children)
    if(extract_id_children_subgroup_recur(grand_child,val_pos_pair))
      return true;
  return false;
}


// Extracts the subgroup from <generated_subgroups> containing <val_pos_pair> & puts it into <id_node>
// => NOTE: if <generated_subgroups> DOESN'T have <val_pos_pair>, it is assumed to be already in <id_node>
void extract_id_children_subgroup(macro_expansion_node& id_node, MACRO_ID_VAL_POS_PAIR& val_pos_pair,
                                                        MACRO_EXPANSION_TREES_t& generated_subgroups)noexcept{
  for(size_type i = 0, n = generated_subgroups.size(); i < n; ++i)
    if(extract_id_children_subgroup_recur(generated_subgroups[i],val_pos_pair)) {
      id_node.children.push_back(generated_subgroups[i]);
      generated_subgroups.erase(generated_subgroups.begin()+i);
      return; // found the subgroup, no more search needed!
    }
}


// Constructs <MACRO_EXPANSION_TREES> based on <ID_TO_VAL_MAP> & <VA_POS_MATRIX>
// (transformation yields an easier means to expand nested variadic expressions by)
void derive_and_construct_macro_expansion_tree(MACRO_ID_TABLE& ID_TO_VAL_MAP, VARARG_POSITIONS& VA_POS_MATRIX, 
                                                      MACRO_EXPANSION_TREES_t& MACRO_EXPANSION_TREES)noexcept{
  // For each identifier instance
  for(auto& macId_instance : ID_TO_VAL_MAP) {
    // Create the macro identifier expansion tree's root
    macro_expansion_node macId_node(macId_name(macId_instance));
    auto& val_pos_map = macId_val_pos_map(macId_instance);

    // For each ... instance
    for(auto& va_posv : VA_POS_MATRIX) {
      auto sought_id_posv_prefix = va_posv;
      --(*sought_id_posv_prefix.rbegin()); // 1st identifier associated w/ ... appears 1 idx prior ...
      
      // If identifier does match against ... instance
      if(id_posv_begins_with_prefix(val_pos_map[0].second,sought_id_posv_prefix)) {
        macId_node.is_variadic = true;
        sought_id_posv_prefix.pop_back(); // rm last idx to match against (no longer relevant to match)

        // Mk a subgroup node for the variadic expansion
        macro_expansion_node subgroup_node(macId_node.id_name,true);

        // If !macId_node.is_leaf(), keep a buffer of the current children subgroups
        //   from which to derive a higher level of subgroups (from nested ...)
        MACRO_EXPANSION_TREES_t generated_subgroups;

        // Fcn to build up the tree, based on whether currently aggregating leaves or combining subgroups
        void(*build_macro_expansion_tree)(macro_expansion_node&,MACRO_ID_VAL_POS_PAIR&,MACRO_EXPANSION_TREES_t&)noexcept = nullptr;
        // Add the leaf values as needed
        if(macId_node.is_leaf()) {
          accumulate_id_leaf_values_and_positions(subgroup_node,val_pos_map[0],generated_subgroups);
          build_macro_expansion_tree = accumulate_id_leaf_values_and_positions;
        // Get the current subgroup set as needed
        } else {
          generated_subgroups = std::move(macId_node.children);
          macId_node.children.clear();
          extract_id_children_subgroup(subgroup_node,val_pos_map[0],generated_subgroups);
          build_macro_expansion_tree = extract_id_children_subgroup;
        }

        // For each value instance of the identifier 
        for(size_type i = 1, n = val_pos_map.size(); i < n; ++i) {
          // if value posv matches the current ... subgroup instance
          if(id_posv_begins_with_prefix(val_pos_map[i].second,sought_id_posv_prefix)) {
            build_macro_expansion_tree(subgroup_node,val_pos_map[i],generated_subgroups);
          // if value posv matches a new ... subgroup instance
          } else {
            // AT A NEW SUBGROUP!
            get_new_VA_POSV_prefix(sought_id_posv_prefix,val_pos_map[i].second);
            // Add the current subgroup as a child, & reset the current subgroup node
            macId_node.children.push_back(subgroup_node);
            subgroup_node = macro_expansion_node(macId_node.id_name,true);
            build_macro_expansion_tree(subgroup_node,val_pos_map[i],generated_subgroups);
          }
        } // End of for

        // Add the current subgroup as a child
        macId_node.children.push_back(subgroup_node);

      } // End of if
    } // End of for
    if(!macId_node.is_variadic) {
      // Save the leaf non-variadic value (in this instance, root = leaf)
      macId_node.values = val_pos_map[0].first;
      macId_node.positions.push_back(val_pos_map[0].second);
    }

    // Register the generated macro id expansion tree
    if(macId_node.is_variadic && macId_node.children.size() == 1) 
      MACRO_EXPANSION_TREES.push_back(std::move(macId_node.children[0]));
    else
      MACRO_EXPANSION_TREES.push_back(std::move(macId_node));
  } // End of for
}


void expand_macro(const data_vector& args, const string& name, data_vector& expanded_exp, 
                                                  MACRO_ID_VAR_TABLE& MID_VARG_PAIR){
  MACRO_EXPANSION_TREES_t MACRO_EXPANSION_TREES;
  clean_MID_VARG_PAIR_for_macro_expansion_analysis(MID_VARG_PAIR);
  derive_and_construct_macro_expansion_tree(MID_VARG_PAIR.first,MID_VARG_PAIR.second,MACRO_EXPANSION_TREES);
  expand_non_variadic_macro_symbols(args,name,expanded_exp,MACRO_EXPANSION_TREES);
  expand_macro_variadic_identifiers(args,name,expanded_exp,MACRO_EXPANSION_TREES);
  unwrap_macro_escaped_variadic_tokens(expanded_exp);
  hash_macro_expansion_identifier("",true); // reset hash idxs
}

/******************************************************************************
* MACRO SYNTACTIC EXTENSIONS -- ELLIPSIS HASHING & UNHASHING
******************************************************************************/

bool data_is_hashable_ellipsis(const data& d)noexcept{
  return data_is_ellipsis(d) || datum_is_an_escaped_variadic_token(d);
}

bool data_is_hashed_ellipsis(const data& d)noexcept{
  return d.is_type(types::sym) && string_begins_with(d.sym,symconst::ellipsis_hash);
}

void hash_all_ellipsis_in_macro_args(data_vector& args)noexcept{
  for(size_type i = 0, n = args.size(); i < n; ++i) {
    if(args[i].is_type(types::exp)) {
      hash_all_ellipsis_in_macro_args(args[i].exp);
    } else if(data_is_hashable_ellipsis(args[i])) {
      args[i].sym = symconst::ellipsis_hash + args[i].sym;
    }
  }
}

void unhash_all_ellipsis_in_macro_args(data_vector& args)noexcept{
  static const size_type ellipsis_hash_prefix_length = strlen(symconst::ellipsis_hash);
  for(size_type i = 0, n = args.size(); i < n; ++i) {
    if(args[i].is_type(types::exp)) {
      unhash_all_ellipsis_in_macro_args(args[i].exp);
    } else if(data_is_hashed_ellipsis(args[i])) {
      args[i].sym = args[i].sym.substr(ellipsis_hash_prefix_length);
    }
  }
}

/******************************************************************************
* MACRO SYNTACTIC EXTENSIONS -- SYNTAX TRANSFORMER APPLICATION/EXECUTION
******************************************************************************/

data_vector convert_transformer_data_result_to_syntax(data&& result, const data_vector& macro_expr) {
  data result_as_syntax;
  if(!convert_data_to_evaluable_syntax(result,result_as_syntax))
    THROW_ERR("Syntax Transformer Callable for macro \"" << macro_expr 
      << "\" didn't result in an evaluable datum:\n     " 
      << PROFILE(result) << EXP_ERR(macro_expr));
  if(!result_as_syntax.is_type(types::exp)) {
    data_vector begin_expr(2);
    begin_expr[0] = symconst::begin;
    begin_expr[1] = std::move(result_as_syntax);
    return begin_expr;
  }
  return result_as_syntax.exp;
}


data derive_quoted_macro_exression(const data_vector& macro_expr,env_type& env) {
  data_vector quoted_macro_expr(2);
  quoted_macro_expr[0] = symconst::quote;
  quoted_macro_expr[1] = macro_expr;
  return scm_eval(std::move(quoted_macro_expr),env);
}


// Expands the given syntax-transformer procedure & returns success status (ie whether matched)
void apply_syntax_transformer_callable(const data_vector& args,const fcn_type& mac,
                                       data_vector& expanded_exp,env_type& env) {
  data_vector macro_expr(args.size()+1);
  macro_expr[0] = mac.name;
  std::copy(args.begin(), args.end(), macro_expr.begin() + 1);
  data_vector transformer_args(1,derive_quoted_macro_exression(macro_expr,env));
  expanded_exp = convert_transformer_data_result_to_syntax(execute_application(mac,transformer_args,env),macro_expr);
}


// Expands the given syntax-rules object & returns success status (ie whether matched)
bool execute_syntax_rules_transform(const data_vector& args,const syn_type& mac,
                                    data_vector& expanded_exp,MACRO_ID_VAR_TABLE& MID_VARG_PAIR) {
  size_type match_idx = 0; // idx of the pattern & template w/in 'mac' that the label & args match
  if(is_macro_match(args, mac, match_idx, MID_VARG_PAIR)) {
    expanded_exp = mac.templates[match_idx]; // prefilled, then has contents expanded into it
    if(!mac.hashed_template_ids[match_idx].empty()) // dynamic <syntax-hash> application
      apply_syntax_hash_to_identifiers(expanded_exp,mac.hashed_template_ids[match_idx]);
    expand_macro(args, mac.label, expanded_exp, MID_VARG_PAIR);
    return true;
  }
  return false;
}

/******************************************************************************
* MACRO SYNTACTIC EXTENSIONS -- EXPANSION MAIN FUNCTIONS
******************************************************************************/

// Confirm whether 'application_label' is a potential macro label
bool application_is_a_potential_macro(const string& application_label, 
                                      const str_vector& label_registry)noexcept{
  if(application_label.empty()) return false;
  for(const auto& label : label_registry)
    if(label == application_label)
      return true;
  return false;
}


// Returns whether the given label & args form a macro found in 'macs'.
// If true, it also transforms the macro by expanding it into 'expanded_exp'
bool handle_macro_transformation(const string& label,const data_vector& args, 
                                 const frame_macs& macs,data_vector& expanded_exp,
                                 env_type& env){
  //  Map of pattern identifier & expansion value pairs
  MACRO_ID_VAR_TABLE MID_VARG_PAIR;
  // Search for macro matches
  for(const auto& mac : macs) {
    // Syntax-rules object
    if(mac.is_type(types::syn)) {
      if(label == mac.syn.label && execute_syntax_rules_transform(args,mac.syn,expanded_exp,MID_VARG_PAIR)) return true;
    // Syntax-transformer procedure (extracted from a callable)
    } else if(mac.is_type(types::fcn)) {
      if(label == mac.fcn.name) {
        apply_syntax_transformer_callable(args,mac.fcn,expanded_exp,env);
        return true;
      }
    // Unkown macro value: ERROR!
    } else {
      THROW_ERR("HEIST MACRO EXPANDER: UNKNOWN MACRO VALUE " << PROFILE(mac)
        << "\n     CURRENTLY ONLY SUPPORTING SYNTAX-RULES OBJECTS & PROCEDURES!" 
        << FCN_ERR(label,args));
    }
  }
  return false;
}


// Returns whether the given label & args form a macro found in 'env'.
// If true, it also transforms the macro by expanding it into 'expanded_exp'
bool expand_macro_if_in_env(const string& label,data_vector args, 
                            env_type& env,data_vector& expanded_exp){
  env_type env_iterator = env;
  hash_all_ellipsis_in_macro_args(args);
  while(env_iterator != nullptr) {
    if(handle_macro_transformation(label,args,env_iterator->macros(),expanded_exp,env)) {
      unhash_all_ellipsis_in_macro_args(expanded_exp);
      return true;
    }
    env_iterator = env_iterator->parent;
  }
  return false;
}

#endif