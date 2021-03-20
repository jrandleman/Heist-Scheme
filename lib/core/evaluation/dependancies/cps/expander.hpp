// Author: Jordan Randleman -- jrandleman@scu.edu -- expander.hpp
// => Contains 5 procedures related to CPS expansion for the C++ Heist Scheme Interpreter

// PROVIDED PROCEDURES:
//   0. data_vector generate_fundamental_form_cps(const data&) // convert a datum to be a CPS expression
//   1. bool        data_is_cps_atomic(const data&)            // if datum evaluates to itself in a CPS context
//   2. bool        is_cps_application(const data_vector&)     // if expression is an application tagged by <generate_fundamental_form_cps>
//   3. string      generate_unique_cps_hash()                 // generate a unique/hashed continuation name
//   4. string      generate_unique_cps_value_hash()           // generate a unique/hashed non-continuation value name

#ifndef HEIST_CPS_EXPANDER_HPP_
#define HEIST_CPS_EXPANDER_HPP_

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION
******************************************************************************/

// Get: void optimize_CPS_code_generation(data_vector& cps_exp)
#include "optimizer.hpp"

/******************************************************************************
* CONTINUATION PASSING STYLE CORE SYNTAX EXPANSION HELPERS
******************************************************************************/

// Expand data's core syntax macros
data cps_recursively_deep_expand_core_macros(const data& d) {
  return recursively_deep_expand_syntax_macros(d,G.GLOBAL_ENVIRONMENT_POINTER,true);
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION UNIQUE CONTINUATION & VALUE GENERATION
******************************************************************************/

// Generate a unique hashed variant of a cps identifier name
// NOTE: Max Unique Hashes = (expt 18446744073709551615 18446744073709551615)
string generate_unique_cps_hash()noexcept{
  if(G.CPS_HASH_IDX_1 != GLOBALS::MAX_SIZE_TYPE)
    return symconst::continuation
      + std::to_string(G.CPS_HASH_IDX_2) + '_' + std::to_string(G.CPS_HASH_IDX_1++);
  return symconst::continuation
    + std::to_string(++G.CPS_HASH_IDX_2) + '_' + std::to_string(G.CPS_HASH_IDX_1++);
}


// Generate a unique hashed variant of a cps value name
// NOTE: Max Unique Hashes = (expt 18446744073709551615 18446744073709551615)
string generate_unique_cps_value_hash()noexcept{
  if(G.CPS_VALUE_HASH_IDX_1 != GLOBALS::MAX_SIZE_TYPE)
    return symconst::cps_generated_val
      + std::to_string(G.CPS_VALUE_HASH_IDX_2) + '_' + std::to_string(G.CPS_VALUE_HASH_IDX_1++);
  return symconst::cps_generated_val
    + std::to_string(++G.CPS_VALUE_HASH_IDX_2) + '_' + std::to_string(G.CPS_VALUE_HASH_IDX_1++);
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION PREDICATES
******************************************************************************/

// Determine if an application was tagged by <generate_fundamental_form_cps>
bool is_cps_application(const data_vector& exp)noexcept{
  return is_tagged_list(exp,symconst::cps_app_tag);
}


// CPS atomics may be returned as is: (cps <cps-atomic>) -> <cps-atomic>
bool data_is_cps_atomic(const data& d)noexcept{
  return !d.is_type(types::exp)                      || 
         is_tagged_list(d.exp,symconst::syn_rules)   || 
         is_tagged_list(d.exp,symconst::quote)       ||
         is_tagged_list(d.exp,symconst::delay)       ||
         is_tagged_list(d.exp,symconst::using_cpsp)  || 
         is_tagged_list(d.exp,symconst::definedp)    ||
         is_tagged_list(d.exp,symconst::delete_bang) ||
         is_tagged_list(d.exp,symconst::infix)       ||
         is_tagged_list(d.exp,symconst::infixr)      ||
         is_tagged_list(d.exp,symconst::unfix)       ||
         is_tagged_list(d.exp,symconst::defn_reader_alias);
}


// Heist-specific checker to not prefix C++ derived special forms w/ application tag
bool is_HEIST_cpp_derived_special_form(const string& app)noexcept{
  return app == symconst::cps_quote || app == symconst::scm_cps    || app == symconst::map_literal ||
         app == symconst::while_t   || app == symconst::vec_literal;
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE DEFINE EXPANSION HELPER(S)
******************************************************************************/

// Generates the procedure to set <var> to <val> after binding <var> as a lambda arg
data_vector get_cps_defn_set_procedure(const data& continuation,const data& var,const data& val){
  data_vector set_exp(2);
  // Set atomic values and pass to the continuation
  if(data_is_cps_atomic(val)) {
    set_exp[0] = continuation;
    set_exp[1] = data_vector(3);
    set_exp[1].exp[0] = symconst::set;
    set_exp[1].exp[1] = var;
    set_exp[1].exp[2] = val;
  // Cps-ify non-atomic values, passing a lambda as a continuation that in 
  //   turn sets the received value & passes such to the continuation given here as an arg
  } else {
    set_exp[0] = generate_fundamental_form_cps(val,false,false);
    set_exp[1] = data_vector(3);
    set_exp[1].exp[0] = symconst::lambda;
    set_exp[1].exp[1] = data_vector(1,generate_unique_cps_value_hash()); // "defn-val"
    set_exp[1].exp[2] = data_vector(2);
    set_exp[1].exp[2].exp[0] = continuation;
    set_exp[1].exp[2].exp[1] = data_vector(3);
    set_exp[1].exp[2].exp[1].exp[0] = symconst::set;
    set_exp[1].exp[2].exp[1].exp[1] = var;
    set_exp[1].exp[2].exp[1].exp[2] = set_exp[1].exp[1].exp[0]; // defn-val
  }
  return set_exp;
}


data_vector convert_proc_defn_to_lambda_defn(const data_vector& defn_exp)noexcept{
  data_vector lambda_defn(3);
  lambda_defn[0] = symconst::define;
  lambda_defn[1] = defn_exp[1].exp[0]; // proc name
  lambda_defn[2] = data_vector(defn_exp.size());
  lambda_defn[2].exp[0] = symconst::lambda;
  lambda_defn[2].exp[1] = data_vector(defn_exp[1].exp.begin()+1,defn_exp[1].exp.end()); // args
  std::copy(defn_exp.begin()+2,defn_exp.end(),lambda_defn[2].exp.begin()+2); // append body to lambda
  return lambda_defn;
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE BEGIN EXPANSION HELPER(S)
******************************************************************************/

// Generates a CPS definition in the middle of a BEGIN or LAMBDA BODY sequence, w/ <rest_exp>
//   being the remaining expressions AFTER this definition in the sequence
// PRECONDITION: !rest_exp.empty()
data_vector generate_mid_seq_cps_var_defn(const data_vector& defn_exp, const data& rest_exp){
  data_vector cps_defn(3);
  cps_defn[0] = symconst::lambda;
  cps_defn[1] = data_vector(1,generate_unique_cps_hash()); // topmost continuation "k"
  cps_defn[2] = data_vector(2);

  cps_defn[2].exp[1] = GLOBALS::FALSE_DATA_BOOLEAN; // initially bind defined symbol to #f
  cps_defn[2].exp[0] = data_vector(3);
  cps_defn[2].exp[0].exp[0] = symconst::lambda;
  cps_defn[2].exp[0].exp[1] = data_vector(1,defn_exp[1]); // defined symbol as an argument
  cps_defn[2].exp[0].exp[2] = data_vector(2);

  // Bind Var to Value
  cps_defn[2].exp[0].exp[2].exp[0] = data_vector(3);
  cps_defn[2].exp[0].exp[2].exp[0].exp[0] = symconst::lambda;
  cps_defn[2].exp[0].exp[2].exp[0].exp[1] = data_vector(1,generate_unique_cps_hash()); // continuation "k1" of set!
  cps_defn[2].exp[0].exp[2].exp[0].exp[2] = get_cps_defn_set_procedure(cps_defn[2].exp[0].exp[2].exp[0].exp[1].exp[0],
                                                                       defn_exp[1],defn_exp[2]);

  // Continue w/ expression after binding [SELF IS THE "k1" CONTINUATION OF THE EXPRESSION ABOVE]
  cps_defn[2].exp[0].exp[2].exp[1] = data_vector(3);
  cps_defn[2].exp[0].exp[2].exp[1].exp[0] = symconst::lambda;
  cps_defn[2].exp[0].exp[2].exp[1].exp[1] = data_vector(1,symconst::cps_ignore_arg); // result of set!
  cps_defn[2].exp[0].exp[2].exp[1].exp[2] = data_vector(2);

  // Pass <rest_exp> of expression to the topmost continuation if CPS-ATOMIC
  if(data_is_cps_atomic(rest_exp)) {
    cps_defn[2].exp[0].exp[2].exp[1].exp[2].exp[0] = cps_defn[1].exp[0];
    cps_defn[2].exp[0].exp[2].exp[1].exp[2].exp[1] = rest_exp;
  // Else CPS-ify <rest_exp> of expression & pass it the topmost continuation
  } else {
    cps_defn[2].exp[0].exp[2].exp[1].exp[2].exp[0] = generate_fundamental_form_cps(rest_exp,false,false);
    cps_defn[2].exp[0].exp[2].exp[1].exp[2].exp[1] = cps_defn[1].exp[0];
  }
  return cps_defn;
}


// Generates the CPS expression needed to evaluate <rest_exp> after defining new syntax
data_vector generate_fundamental_form_cps_syn_defn_REST_EXP_continuation(const data& continuation,const data& rest_exp){
  data_vector rest_cont(2);
  // Pass <rest_exp> of expression to the topmost continuation if CPS-ATOMIC
  if(data_is_cps_atomic(rest_exp)) {
    rest_cont[0] = continuation;
    rest_cont[1] = rest_exp;
  // Else CPS-ify <rest_exp> of expression & pass it the topmost continuation
  } else {
    rest_cont[0] = generate_fundamental_form_cps(rest_exp,false,false);
    rest_cont[1] = continuation;
  }
  return rest_cont;
}


// Generates a CPS syntax definition in the middle of a BEGIN or LAMBDA BODY sequence, w/ <rest_exp>
//   being the remaining expressions AFTER this syntax definition in the sequence
// PRECONDITION: !rest_exp.empty()
data_vector generate_mid_seq_cps_syn_defn(const data_vector& defn_exp, const data& rest_exp){
  const bool atomic_syntax_rules = data_is_cps_atomic(defn_exp[2]);
  data_vector cps_defn(3 + atomic_syntax_rules);
  cps_defn[0] = symconst::lambda;
  cps_defn[1] = data_vector(1,generate_unique_cps_hash()); // topmost continuation "k"
  // Atomic Syntax-Rules reduces # of lambdas needed by 1
  if(atomic_syntax_rules) {
    cps_defn[2] = data_vector(3);
    cps_defn[2].exp[0] = symconst::defn_syn;
    cps_defn[2].exp[1] = defn_exp[1];
    cps_defn[2].exp[2] = defn_exp[2];
    cps_defn[3] = generate_fundamental_form_cps_syn_defn_REST_EXP_continuation(cps_defn[1].exp[0],rest_exp);
    return cps_defn;
  }
  cps_defn[2] = data_vector(2);
  cps_defn[2].exp[0] = generate_fundamental_form_cps(defn_exp[2],false,false);
  cps_defn[2].exp[1] = data_vector(4);
  cps_defn[2].exp[1].exp[0] = symconst::lambda;
  cps_defn[2].exp[1].exp[1] = data_vector(1,generate_unique_cps_value_hash()); // "syntax-object"

  cps_defn[2].exp[1].exp[2] = data_vector(3);
  cps_defn[2].exp[1].exp[2].exp[0] = symconst::defn_syn;
  cps_defn[2].exp[1].exp[2].exp[1] = defn_exp[1];
  cps_defn[2].exp[1].exp[2].exp[2] = cps_defn[2].exp[1].exp[1].exp[0];
  cps_defn[2].exp[1].exp[3] = generate_fundamental_form_cps_syn_defn_REST_EXP_continuation(cps_defn[1].exp[0],rest_exp);
  return cps_defn;
}


template<data_vector(*generate_begin_defn)(const data_vector&,const data&)>
data_vector generate_begin_mid_seq_defn(const data_vector& defn_exp,const data& begin){
  if(begin.exp.size() == 3 && data_is_cps_atomic(begin.exp[2])) {
    return generate_begin_defn(defn_exp,begin.exp[2]);
  } else {
    data_vector begin_tail(begin.exp.size()-1);
    begin_tail[0] = symconst::begin;
    std::copy(begin.exp.begin()+2,begin.exp.end(),begin_tail.begin()+1);
    return generate_begin_defn(defn_exp,begin_tail);
  }
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE LAMBDA EXPANSION HELPER(S)
******************************************************************************/

void get_cps_lambda_body(const data_vector& lambda_exp, data_vector& lambda_cps){
  lambda_cps[2] = data_vector(2); // lambda body
  // If single-expression body, NO NEED FOR "BEGIN"
  if(lambda_exp.size() == 3) { 
    if(data_is_cps_atomic(lambda_exp[2])) {
      lambda_cps[2].exp[0] = *lambda_cps[1].exp.rbegin(); // DYNAMIC CONTINUATION
      lambda_cps[2].exp[1] = lambda_exp[2];
    } else {
      lambda_cps[2].exp[0] = generate_fundamental_form_cps(lambda_exp[2],false,false);
      lambda_cps[2].exp[1] = *lambda_cps[1].exp.rbegin(); // DYNAMIC CONTINUATION
    }
  // If multi-expression body, WRAP W/ "BEGIN"
  } else {
    data_vector begin_recur(lambda_exp.size()-1);
    begin_recur[0] = symconst::begin;
    std::copy(lambda_exp.begin()+2,lambda_exp.end(),begin_recur.begin()+1);
    lambda_cps[2] = data_vector(2);
    lambda_cps[2].exp[0] = generate_fundamental_form_cps(begin_recur,false,false);
    lambda_cps[2].exp[1] = *lambda_cps[1].exp.rbegin();
  }
}


// PRECONDITION: lambda.capacity() == 3
void generate_cps_lambda_form(const data& code, data_vector& lambda) {
  confirm_valid_lambda(code.exp);
  lambda[0] = symconst::lambda;
  lambda[1] = data_vector(1,generate_unique_cps_hash()); // "k"
  lambda[2] = data_vector(2);
  lambda[2].exp[0] = lambda[1].exp[0];
  lambda[2].exp[1] = data_vector(3);
  lambda[2].exp[1].exp[0] = symconst::lambda;
  if(code.exp[1].exp.empty()) { // ARGLESS
    lambda[2].exp[1].exp[1] = data_vector(1,generate_unique_cps_hash()); // "dyn-k"
  } else { // N ARGS
    const auto param_len = code.exp[1].exp.size();
    lambda[2].exp[1].exp[1] = data_vector(param_len+1);
    std::copy(code.exp[1].exp.begin(),code.exp[1].exp.end(),lambda[2].exp[1].exp[1].exp.begin());
    lambda[2].exp[1].exp[1].exp[param_len] = generate_unique_cps_hash(); // "dyn-k"
  }
  get_cps_lambda_body(code.exp,lambda[2].exp[1].exp);
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE FN EXPANSION HELPER(S)
******************************************************************************/

data_vector fn_unwrap_inner_lambda(const data_vector& lambda_exp)noexcept{
  return data_vector(lambda_exp[2].exp[1].exp.begin()+1,lambda_exp[2].exp[1].exp.end());
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE IF EXPANSION HELPER(S)
******************************************************************************/

data_vector get_cps_IF_consequent(const data& code, const data& continuation){
  data_vector consequent(2);
  if(data_is_cps_atomic(code.exp[2])) { // (k <atomic-consequent>)
    consequent[0] = continuation;
    consequent[1] = code.exp[2];
  } else { // ((cps <consequent>) k)
    consequent[0] = generate_fundamental_form_cps(code.exp[2],false,false);
    consequent[1] = continuation;
  }
  return consequent;
}

// PRECONDITION: Assumes IF alternative exists
data_vector get_cps_IF_alternative(const data& code, const data& continuation){
  data_vector alternative(2);
  if(data_is_cps_atomic(code.exp[3])) { // (k <atomic-alternative>)
    alternative[0] = continuation;
    alternative[1] = code.exp[3];
  } else { // ((cps <alternative>) k)
    alternative[0] = generate_fundamental_form_cps(code.exp[3],false,false);
    alternative[1] = continuation;
  }
  return alternative;
}

data_vector get_cps_IF_VOID_alternative(const data& continuation){
  data_vector void_alternative(2);
  void_alternative[0] = continuation; // continuation
  void_alternative[1] = data_vector(1,"void");  // add (void)
  return void_alternative;
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE DEFCLASS EXPANSION HELPER(S)
******************************************************************************/

bool cps_is_non_atomc_defclass_property(const data& d, const string& ctor_name)noexcept{
  return d.is_type(types::exp) && d.exp.size() == 2 && 
         d.exp[0].is_type(types::sym) && d.exp[0].sym != ctor_name && d.exp[1].is_type(types::exp);
}

bool cps_defclass_requires_outlined_properties(data_vector& defclass_expr, 
                                               std::vector<data_vector>& stripped_property_values, 
                                               str_vector& stripped_property_names) {
  validate_defclass(defclass_expr);
  const auto ctor_name = defclass_expr[1].sym;
  bool requires_outlined_properties = false;
  for(size_type i = 3, n = defclass_expr.size(); i < n; ++i) {
    if(cps_is_non_atomc_defclass_property(defclass_expr[i],ctor_name)) {
      stripped_property_names.push_back(defclass_expr[i].exp[0].sym);
      stripped_property_values.push_back(defclass_expr[i].exp[1].exp);
      defclass_expr.erase(defclass_expr.begin()+i);
      --i; // account for loop's "++i" after having rm'd an item from <defclass_expr>
      requires_outlined_properties = true;
    }
  }
  return requires_outlined_properties;
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE MACRO EXPANSION
******************************************************************************/

data_vector cps_generate_macro_defn(const data& code,const bool topmost_call, const string& mac_defn_statement){
  void confirm_valid_define_syntax(const data_vector&);
  confirm_valid_define_syntax(code.exp);
  data_vector cps_defn_syn(3);
  cps_defn_syn[0] = symconst::lambda;
  cps_defn_syn[1] = data_vector(1,generate_unique_cps_hash()); // "k"
  cps_defn_syn[2] = data_vector(2);
  if(data_is_cps_atomic(code.exp[2])) {
    cps_defn_syn[2].exp[0] = cps_defn_syn[1].exp[0];
    cps_defn_syn[2].exp[1] = data_vector(3);
    cps_defn_syn[2].exp[1].exp[0] = mac_defn_statement;
    cps_defn_syn[2].exp[1].exp[1] = code.exp[1];
    cps_defn_syn[2].exp[1].exp[2] = code.exp[2];
    return cps_defn_syn;
  }
  cps_defn_syn[2].exp[0] = generate_fundamental_form_cps(code.exp[2],false,false);
  cps_defn_syn[2].exp[1] = data_vector(3);
  cps_defn_syn[2].exp[1].exp[0] = symconst::lambda;
  cps_defn_syn[2].exp[1].exp[1] = data_vector(1,generate_unique_cps_value_hash()); // "syntax-object"

  cps_defn_syn[2].exp[1].exp[2] = data_vector(2);
  cps_defn_syn[2].exp[1].exp[2].exp[0] = cps_defn_syn[1].exp[0];
  cps_defn_syn[2].exp[1].exp[2].exp[1] = data_vector(3);
  cps_defn_syn[2].exp[1].exp[2].exp[1].exp[0] = mac_defn_statement;
  cps_defn_syn[2].exp[1].exp[2].exp[1].exp[1] = code.exp[1];
  cps_defn_syn[2].exp[1].exp[2].exp[1].exp[2] = cps_defn_syn[2].exp[1].exp[1].exp[0];
  if(topmost_call) optimize_CPS_code_generation(cps_defn_syn);
  return cps_defn_syn;
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE CORE EXPANSION DISPATCH
******************************************************************************/

// NOTE: <topmost_call> signals to optimize the result prior returning
data_vector generate_fundamental_form_cps(const data& code,const bool topmost_call,const bool core_unexpanded){
  // EXPAND CORE SYNTAX 
  if(core_unexpanded)
    return generate_fundamental_form_cps(cps_recursively_deep_expand_core_macros(code),topmost_call,false);

  // ATOMIC DATUM OR EXPRESSION
  if(data_is_cps_atomic(code)) {
    data_vector lambda(3);
    lambda[0] = symconst::lambda;
    lambda[1] = data_vector(1,generate_unique_cps_hash()); // "k"
    lambda[2] = data_vector(2);
    lambda[2].exp[0] = lambda[1].exp[0];
    lambda[2].exp[1] = code;
    return lambda;

  // DEFCLASS
  } else if(is_tagged_list(code.exp,symconst::defclass)) {
    // Check if need to strip-out any property defns (if they have non-atomic values)
    std::vector<data_vector> stripped_property_values;
    str_vector stripped_property_names;
    // If must strip out defns
    auto defclass_expr = code.exp; // trasformation may mutate the <deflcass> expression
    if(cps_defclass_requires_outlined_properties(defclass_expr,stripped_property_values,stripped_property_names)){
      data_vector begin_expr(2+stripped_property_names.size());
      begin_expr[0] = symconst::begin;
      begin_expr[1] = defclass_expr;
      size_type j = 2;
      for(size_type i = 0, n = stripped_property_names.size(); i < n; ++i, ++j) {
        begin_expr[j] = data_vector(4);
        begin_expr[j].exp[0] = "proto-add-property!";
        begin_expr[j].exp[1] = begin_expr[1].exp[1]; // prototype name
        begin_expr[j].exp[2] = data_vector(2);
        begin_expr[j].exp[2].exp[0] = symconst::quote;
        begin_expr[j].exp[2].exp[1] = stripped_property_names[i];
        begin_expr[j].exp[3] = stripped_property_values[i];
      }
      return generate_fundamental_form_cps(begin_expr,topmost_call,false);
    // No external definitions needed! Treat as if cps-atomic.
    } else {
      data_vector lambda(3);
      lambda[0] = symconst::lambda;
      lambda[1] = data_vector(1,generate_unique_cps_hash()); // "k"
      lambda[2] = data_vector(2);
      lambda[2].exp[0] = lambda[1].exp[0];
      lambda[2].exp[1] = code;
      return lambda;
    }

  // DEFINE-SYNTAX
  } else if(is_tagged_list(code.exp,symconst::defn_syn)) {
    return cps_generate_macro_defn(code,topmost_call,symconst::defn_syn);

  // CORE-SYNTAX
  } else if(is_tagged_list(code.exp,symconst::core_syn)) {
    return cps_generate_macro_defn(code,topmost_call,symconst::core_syn);

  // SET!
  } else if(is_tagged_list(code.exp,symconst::set)) {
    confirm_valid_assignment(code.exp);
    data_vector lambda(3);
    lambda[0] = symconst::lambda;
    lambda[1] = data_vector(1,generate_unique_cps_hash()); // "k"
    lambda[2] = data_vector(2);
    if(data_is_cps_atomic(code.exp[2])) {
      lambda[2].exp[0] = lambda[1].exp[0];
      lambda[2].exp[1] = code.exp;
      return lambda;
    }
    lambda[2].exp[0] = generate_fundamental_form_cps(code.exp[2],false,false);
    lambda[2].exp[1] = data_vector(3);
    lambda[2].exp[1].exp[0] = symconst::lambda;
    lambda[2].exp[1].exp[1] = data_vector(1,generate_unique_cps_value_hash()); // "value"
    lambda[2].exp[1].exp[2] = data_vector(2);
    lambda[2].exp[1].exp[2].exp[0] = lambda[1].exp[0];
    lambda[2].exp[1].exp[2].exp[1] = data_vector(3);
    lambda[2].exp[1].exp[2].exp[1].exp[0] = symconst::set;
    lambda[2].exp[1].exp[2].exp[1].exp[1] = code.exp[1];
    lambda[2].exp[1].exp[2].exp[1].exp[2] = lambda[2].exp[1].exp[1].exp[0];
    if(topmost_call) optimize_CPS_code_generation(lambda);
    return lambda;

  // BEGIN
  } else if(is_tagged_list(code.exp,symconst::begin)) {
    data_vector lambda(3);
    lambda[0] = symconst::lambda;
    lambda[1] = data_vector(1,generate_unique_cps_hash()); // "k"
    lambda[2] = data_vector(2);
    // 0 Args
    if(code.exp.size() == 1) {
      lambda[2].exp[0] = lambda[1].exp[0];
      lambda[2].exp[1] = data_vector(1,"void");
    // 1 Arg
    } else if(code.exp.size() == 2) {
      if(data_is_cps_atomic(code.exp[1])) {
        lambda[2].exp[0] = lambda[1].exp[0];
        lambda[2].exp[1] = code.exp[1];
      } else {
        lambda[2].exp[0] = generate_fundamental_form_cps(code.exp[1],false,false);
        lambda[2].exp[1] = lambda[1].exp[0];
      }
    // N Args
    } else {
      bool rec_idx = !data_is_cps_atomic(code.exp[1]);
      if(rec_idx) {
        if(is_tagged_list(code.exp[1].exp,symconst::define)) {
          if(code.exp[1].exp[1].is_type(types::exp)) // Convert procedure definition to a lambda defn
            lambda[2].exp[0] = generate_begin_mid_seq_defn<generate_mid_seq_cps_var_defn>(
                                                           convert_proc_defn_to_lambda_defn(code.exp[1].exp),code.exp);
          else
            lambda[2].exp[0] = generate_begin_mid_seq_defn<generate_mid_seq_cps_var_defn>(code.exp[1].exp,code.exp);
          lambda[2].exp[1] = lambda[1].exp[0];
          if(topmost_call) optimize_CPS_code_generation(lambda);
          return lambda;
        } else if(is_tagged_list(code.exp[1].exp,symconst::defn_syn)) {
          lambda[2].exp[0] = generate_begin_mid_seq_defn<generate_mid_seq_cps_syn_defn>(code.exp[1].exp,code.exp);
          lambda[2].exp[1] = lambda[1].exp[0];
          if(topmost_call) optimize_CPS_code_generation(lambda);
          return lambda;
        } else {
          lambda[2].exp[0] = generate_fundamental_form_cps(code.exp[1],false,false);
        }
      } else {
        lambda[2].exp[1] = code.exp[1];
      }
      lambda[2].exp[rec_idx] = data_vector(3);
      lambda[2].exp[rec_idx].exp[0] = symconst::lambda;
      lambda[2].exp[rec_idx].exp[1] = data_vector(1,symconst::cps_ignore_arg);
      lambda[2].exp[rec_idx].exp[2] = data_vector(2);
      if(code.exp.size() == 3 && data_is_cps_atomic(code.exp[2])) { // 2 ARGS, THE LAST BEING CPS-ATOMIC
        lambda[2].exp[rec_idx].exp[2].exp[0] = lambda[1].exp[0];
        lambda[2].exp[rec_idx].exp[2].exp[1] = code.exp[2];
      } else { // 2+ ARGS, IF 2, 2ND != CPS-ATOMIC
        data begin_recur(data_vector(code.exp.size()-1));
        begin_recur.exp[0] = symconst::begin;
        std::copy(code.exp.begin()+2, code.exp.end(), begin_recur.exp.begin()+1);
        lambda[2].exp[rec_idx].exp[2].exp[0] = generate_fundamental_form_cps(begin_recur,false,false);
        lambda[2].exp[rec_idx].exp[2].exp[1] = lambda[1].exp[0];
      }
    }
    if(topmost_call) optimize_CPS_code_generation(lambda);
    return lambda;

  // LAMBDA
  } else if(is_tagged_list(code.exp,symconst::lambda)) {
    if(is_opt_arg_lambda(code.exp)) // convert optional-args <lambda> to a <fn>
      return generate_fundamental_form_cps(convert_lambda_opt_args_to_fn(code.exp),topmost_call,false);
    data_vector lambda(3);
    generate_cps_lambda_form(code,lambda);
    if(topmost_call) optimize_CPS_code_generation(lambda);
    return lambda;

  // FN
  } else if(is_tagged_list(code.exp,symconst::fn)) {
    validate_fn(code.exp);
    data_vector fn_exp(3);
    fn_exp[0] = symconst::lambda;
    fn_exp[1] = data_vector(1,generate_unique_cps_hash()); // "k"
    fn_exp[2] = data_vector(2);
    fn_exp[2].exp[0] = fn_exp[1].exp[0];
    fn_exp[2].exp[1] = data_vector(code.exp.size());
    fn_exp[2].exp[1].exp[0] = symconst::fn;
    for(size_type i = 1, n = code.exp.size(); i < n; ++i) {
      data_vector lambda(3);
      data lambda_exp(data_vector(1+code.exp[i].exp.size()));
      lambda_exp.exp[0] = symconst::lambda;
      std::copy(code.exp[i].exp.begin(),code.exp[i].exp.end(),lambda_exp.exp.begin()+1);
      generate_cps_lambda_form(lambda_exp,lambda);
      fn_exp[2].exp[1].exp[i] = fn_unwrap_inner_lambda(lambda);
    }
    if(topmost_call) optimize_CPS_code_generation(fn_exp);
    return fn_exp;

  // IF
  } else if(is_tagged_list(code.exp,symconst::if_t)) {
    confirm_valid_if(code.exp);
    data_vector lambda(3);
    lambda[0] = symconst::lambda;
    lambda[1] = data_vector(1,generate_unique_cps_hash()); // "k"
    // Atomic IF test
    if(data_is_cps_atomic(code.exp[1])) { 
      lambda[2] = data_vector(4);
      lambda[2].exp[0] = symconst::if_t;
      lambda[2].exp[1] = code.exp[1];
      lambda[2].exp[2] = get_cps_IF_consequent(code,lambda[1].exp[0]);
      if(code.exp.size() > 3) // Has IF alternative
        lambda[2].exp[3] = get_cps_IF_alternative(code,lambda[1].exp[0]);
      else // add pass (void) as alternative if none given
        lambda[2].exp[3] = get_cps_IF_VOID_alternative(lambda[1].exp[0]);
      if(topmost_call) optimize_CPS_code_generation(lambda);
      return lambda;
    }
    // Non-Atomic IF test
    lambda[2] = data_vector(2);
    lambda[2].exp[0] = generate_fundamental_form_cps(code.exp[1],false,false);
    lambda[2].exp[1] = data_vector(3);
    lambda[2].exp[1].exp[0] = symconst::lambda;
    lambda[2].exp[1].exp[1] = data_vector(1,generate_unique_cps_value_hash()); // "test-result"
    lambda[2].exp[1].exp[2] = data_vector(4);
    lambda[2].exp[1].exp[2].exp[0] = symconst::if_t;
    lambda[2].exp[1].exp[2].exp[1] = lambda[2].exp[1].exp[1].exp[0];
    lambda[2].exp[1].exp[2].exp[2] = get_cps_IF_consequent(code,lambda[1].exp[0]);
    if(code.exp.size() > 3) // Has IF alternative
      lambda[2].exp[1].exp[2].exp[3] = get_cps_IF_alternative(code,lambda[1].exp[0]);
    else // add pass (void) as alternative if none given
      lambda[2].exp[1].exp[2].exp[3] = get_cps_IF_VOID_alternative(lambda[1].exp[0]);
    if(topmost_call) optimize_CPS_code_generation(lambda);
    return lambda;

  // DEFINE
  } else if(is_tagged_list(code.exp,symconst::define)) {
    confirm_valid_definition(code.exp);
    if(is_obj_property_definition(code.exp)) { // DYNAMIC PROPERTY ADDITION
      return generate_fundamental_form_cps(convert_obj_property_defintion_to_method_call(code.exp),topmost_call,false);
    } else if(!code.exp[1].is_type(types::exp)) { // DEFINING VARIABLE
      data_vector cps_defn(3);
      cps_defn[0] = symconst::lambda;
      cps_defn[1] = data_vector(1,generate_unique_cps_hash()); // topmost continuation "k"
      cps_defn[2] = data_vector(2);
      cps_defn[2].exp[1] = GLOBALS::FALSE_DATA_BOOLEAN; // initially bind defined symbol to #f
      cps_defn[2].exp[0] = data_vector(3);
      cps_defn[2].exp[0].exp[0] = symconst::lambda;
      cps_defn[2].exp[0].exp[1] = data_vector(1,code.exp[1]); // defined symbol as an argument, and bind via set! (below)
      cps_defn[2].exp[0].exp[2] = get_cps_defn_set_procedure(cps_defn[1].exp[0],code.exp[1],code.exp[2]);
      if(topmost_call) optimize_CPS_code_generation(cps_defn);
      return cps_defn;
    } else { // DEFINING PROCEDURE
      auto cps_defn = generate_fundamental_form_cps(convert_proc_defn_to_lambda_defn(code.exp),false,false); // cps lambda defn
      if(topmost_call) optimize_CPS_code_generation(cps_defn);
      return cps_defn;
    }

  // APPLICATION
  } else {
    if(code.exp.empty())
      THROW_ERR("'scm->cps CAN'T EVAL AN EMPTY EXPRESSION!" << EXP_ERR(code.exp));
    // Applications are expanded @eval-time (accounts for expanding macros)
    if(code.exp[0].is_type(types::sym) && is_HEIST_cpp_derived_special_form(code.exp[0].sym))
      return code.exp; // Don't tag applications of HEIST's C++ derived forms
    // Tag any other application
    bool no_tag = !is_cps_application(code.exp);
    data_vector app(code.exp.size()+no_tag);
    if(no_tag) app[0] = symconst::cps_app_tag;
    std::copy(code.exp.begin(),code.exp.end(),app.begin()+no_tag);
    return app;
  }
}

#endif