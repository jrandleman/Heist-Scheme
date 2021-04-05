// Author: Jordan Randleman -- jrandleman@scu.edu -- optimizer.hpp
// => Contains the "optimize_CPS_code_generation" procedure for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_CPS_OPTIMIZER_HPP_
#define HEIST_SCHEME_CORE_CPS_OPTIMIZER_HPP_

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 1
******************************************************************************/

// Whether <cps_exp> contains <sym>
bool CPS_exp_contains_symbol(const data_vector& cps_exp,const string& sym)noexcept{
  for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
    if((cps_exp[i].is_type(types::exp) && CPS_exp_contains_symbol(cps_exp[i].exp,sym)) ||
       (cps_exp[i].is_type(types::sym) && cps_exp[i].sym == sym))
      return true;
  return false;
}


// Is a lambda of 1 arg
bool is_unary_arg_lambda_cps_exp(const data_vector& cps_exp)noexcept{
  return cps_exp.size() > 2 && is_tagged_list(cps_exp,symconst::lambda) && 
         cps_exp[1].is_type(types::exp) && cps_exp[1].exp.size() == 1 && 
         cps_exp[1].exp[0].is_type(types::sym);
}


// Optimizable (pass 1) CPS lambda
bool is_optimizable_CPS_pass_1_exp(const data_vector& cps_exp)noexcept{
  return cps_exp.size() == 3 && is_unary_arg_lambda_cps_exp(cps_exp) && 
         cps_exp[2].is_type(types::exp) && cps_exp[2].exp.size() == 2 &&
         cps_exp[2].exp[0].is_type(types::exp) && cps_exp[2].exp[1].is_type(types::sym) &&
         cps_exp[2].exp[1].sym == cps_exp[1].exp[0].sym &&
         !CPS_exp_contains_symbol(cps_exp[2].exp[0].exp,cps_exp[2].exp[1].sym);
}


// (lambda (a) (<expression-w/o-a> a)) => <expression-w/o-a>
void CPS_lambda_unwrapping_optimization_pass_1(data_vector& cps_exp)noexcept{
  if(is_optimizable_CPS_pass_1_exp(cps_exp)) {
    auto temp = cps_exp[2].exp[0].exp;
    cps_exp = temp;
    CPS_lambda_unwrapping_optimization_pass_1(cps_exp);
  } else {
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
      if(cps_exp[i].is_type(types::exp))
        CPS_lambda_unwrapping_optimization_pass_1(cps_exp[i].exp);
  }
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 2
******************************************************************************/

bool data_is_continuation_parameter(const data&)noexcept;

void replace_all_instances_of_symB_with_symA(data_vector& cps_exp,const string& symA,const string& symB)noexcept{
  for(size_type i = 0, n = cps_exp.size(); i < n; ++i) {
    if(cps_exp[i].is_type(types::exp))
      replace_all_instances_of_symB_with_symA(cps_exp[i].exp,symA,symB);
    else if(cps_exp[i].is_type(types::sym) && cps_exp[i].sym == symB)
      cps_exp[i].sym = symA;
  }
}


// Optimizable (pass 2) CPS lambda
bool is_optimizable_CPS_pass_2_exp(const data_vector& cps_exp)noexcept{
  return cps_exp.size() == 2 && cps_exp[0].is_type(types::exp) && 
         data_is_continuation_parameter(cps_exp[1]) && 
         is_unary_arg_lambda_cps_exp(cps_exp[0].exp) && cps_exp[0].exp.size() == 3 &&
         cps_exp[0].exp[2].is_type(types::exp) &&
         data_is_continuation_parameter(cps_exp[0].exp[1].exp[0]) &&
         !CPS_exp_contains_symbol(cps_exp[0].exp,cps_exp[1].sym);
}


// ((lambda (b) <exp-w/o-a>) a) => <exp-w/o-a>,
// 1. With each reference to <b> replaced by <a>
// 2. Iff both <a> & <b> are continuations
void CPS_lambda_unwrapping_optimization_pass_2(data_vector& cps_exp)noexcept{
  if(is_optimizable_CPS_pass_2_exp(cps_exp)) {
    const auto a_param = cps_exp[1].sym, b_param = cps_exp[0].exp[1].exp[0].sym;
    auto temp = cps_exp[0].exp[2].exp;
    cps_exp = temp;
    replace_all_instances_of_symB_with_symA(cps_exp,a_param,b_param);
    CPS_lambda_unwrapping_optimization_pass_2(cps_exp);
  } else {
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
      if(cps_exp[i].is_type(types::exp))
        CPS_lambda_unwrapping_optimization_pass_2(cps_exp[i].exp);
  }
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 3
******************************************************************************/

// Replaces the 1 instance of <symB> w/ <objA>
bool replace_instance_of_symB_with_objA(data_vector& cps_exp,const data& objA,const string& symB)noexcept{
  for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
    if(cps_exp[i].is_type(types::exp) && replace_instance_of_symB_with_objA(cps_exp[i].exp,objA,symB)) {
      return true;
    } else if(cps_exp[i].is_type(types::sym) && cps_exp[i].sym == symB) {
      cps_exp[i] = objA;
      return true;
    }
  return false;
}


// counts instances of <sym> w/in <cps_exp>
void CPS_exp_count_instances_of_symbol(const data_vector& cps_exp,const string& sym,size_type& count)noexcept{
  for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
    if(cps_exp[i].is_type(types::exp)) {
      CPS_exp_count_instances_of_symbol(cps_exp[i].exp,sym,count);
      if(count > 2) return;
    } else if(cps_exp[i].is_type(types::sym) && cps_exp[i].sym == sym) {
      if(++count > 2) return;
    }
}


// Optimizable (pass 3) CPS lambda
bool is_optimizable_CPS_pass_3_exp(const data_vector& cps_exp)noexcept{
  if(cps_exp.size() == 2 && cps_exp[0].is_type(types::exp) && 
    is_unary_arg_lambda_cps_exp(cps_exp[0].exp) && cps_exp[0].exp.size() == 3 &&
    cps_exp[0].exp[2].is_type(types::exp) && data_is_continuation_parameter(cps_exp[0].exp[1].exp[0])) {
      size_type count = 0;
      CPS_exp_count_instances_of_symbol(cps_exp[0].exp[2].exp,cps_exp[0].exp[1].exp[0].sym,count);
      return count < 2;
    }
  return false;
}


// ((lambda (k) <exp-w/-only-1-instance-of-k>) <obj>) => <exp-w/-only-1-instance-of-k> 
// 1. <k> is a continuation
// 2. That 1 instance of <k> is replaced w/ <obj>
void CPS_lambda_unwrapping_optimization_pass_3(data_vector& cps_exp)noexcept{
  if(is_optimizable_CPS_pass_3_exp(cps_exp)) {
    const data a_obj = cps_exp[1];
    const auto b_param = cps_exp[0].exp[1].exp[0].sym;
    auto temp = cps_exp[0].exp[2].exp;
    cps_exp = temp;
    replace_instance_of_symB_with_objA(cps_exp,a_obj,b_param);
    CPS_lambda_unwrapping_optimization_pass_3(cps_exp);
  } else {
    for(size_type i = 0, n = cps_exp.size(); i < n; ++i)
      if(cps_exp[i].is_type(types::exp))
        CPS_lambda_unwrapping_optimization_pass_3(cps_exp[i].exp);
  }
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 4
******************************************************************************/

// Expand the symconst::cps_ignore_arg lambda application
void expand_CPS_lambda_pass_4_application(data_vector& cps_exp,const size_type& i)noexcept{
  data_vector unwrapped_exp(cps_exp[i].exp[0].exp.size()-1);
  unwrapped_exp[0] = cps_exp[i].exp[1];
  std::move(cps_exp[i].exp[0].exp.begin()+2,cps_exp[i].exp[0].exp.end(),unwrapped_exp.begin()+1);
  cps_exp.erase(cps_exp.begin()+i); // erase optimized lambda application
  cps_exp.insert(cps_exp.begin()+i, // insert expanded application
                 std::make_move_iterator(unwrapped_exp.begin()),
                 std::make_move_iterator(unwrapped_exp.end())); 
}


// Optimizable (pass 4) CPS lambda
bool is_optimizable_CPS_pass_4_exp(const data_vector& cps_exp)noexcept{
  return cps_exp.size() == 2 && cps_exp[0].is_type(types::exp) &&
         is_unary_arg_lambda_cps_exp(cps_exp[0].exp) &&
         cps_exp[0].exp[1].exp[0].is_type(types::sym) &&
         cps_exp[0].exp[1].exp[0].sym == symconst::cps_ignore_arg;
}


// ((lambda (ignore) <exp> ...) <obj>) => <obj> <exp> ...
void CPS_lambda_unwrapping_optimization_pass_4(data_vector& cps_exp)noexcept{
  for(size_type i = 0, n = cps_exp.size(); i < n; ++i) {
    if(cps_exp[i].is_type(types::exp)) {
      if(is_optimizable_CPS_pass_4_exp(cps_exp[i].exp)) {
        expand_CPS_lambda_pass_4_application(cps_exp,i);
        CPS_lambda_unwrapping_optimization_pass_4(cps_exp);
        return;
      }
      CPS_lambda_unwrapping_optimization_pass_4(cps_exp[i].exp);
    }
  }
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- PASS 5
******************************************************************************/

// Revert the "lambda-set!" definition transformation
void expand_CPS_lambda_pass_5_definition(data_vector& cps_exp,const size_type& i)noexcept{
  data_vector unwrapped_exp(cps_exp[i].exp[0].exp.size()-2);
  std::move(cps_exp[i].exp[0].exp.begin()+2,cps_exp[i].exp[0].exp.end(),unwrapped_exp.begin());
  unwrapped_exp[0].exp[0].sym = symconst::define;
  cps_exp.erase(cps_exp.begin()+i); // erase optimized lambda application
  cps_exp.insert(cps_exp.begin()+i, // insert expanded application
                 std::make_move_iterator(unwrapped_exp.begin()),
                 std::make_move_iterator(unwrapped_exp.end()));
}


// Optimizable (pass 5) CPS lambda
bool is_optimizable_CPS_pass_5_exp(const data_vector& cps_exp)noexcept{
  return cps_exp.size() == 2 && cps_exp[0].is_type(types::exp) &&
         is_unary_arg_lambda_cps_exp(cps_exp[0].exp) &&
         ((cps_exp[1].is_type(types::bol) && !cps_exp[1].bol.val) || 
          (cps_exp[1].is_type(types::sym) && cps_exp[1].sym == symconst::false_)) && 
         cps_exp[0].exp[2].is_type(types::exp) && cps_exp[0].exp[2].exp.size() == 3 && 
         cps_exp[0].exp[2].exp[0].is_type(types::sym) && cps_exp[0].exp[2].exp[0].sym == symconst::set;
}


// Pass 5: Reifying Definitions
// ((lambda (<name>) (set! <name> <val>) <exp> ...) #f) => (define <name> <val>) <exp> ...
void CPS_lambda_unwrapping_optimization_pass_5(data_vector& cps_exp)noexcept{
  for(size_type i = 0, n = cps_exp.size(); i < n; ++i) {
    if(cps_exp[i].is_type(types::exp)) {
      if(is_optimizable_CPS_pass_5_exp(cps_exp[i].exp)) {
        expand_CPS_lambda_pass_5_definition(cps_exp,i);
        CPS_lambda_unwrapping_optimization_pass_5(cps_exp);
        return;
      }
      CPS_lambda_unwrapping_optimization_pass_5(cps_exp[i].exp);
    }
  }
}

/******************************************************************************
* CONTINUATION-PASSING-STYLE EXPANSION OPTIMIZATION -- DISPATCH
******************************************************************************/

// Perform several optimization passes on the generated CPS to reduce lambda count 
// NOTE: CPS atomics are already optimized @expansion-time
void optimize_CPS_code_generation(data_vector& cps_exp)noexcept{
  CPS_lambda_unwrapping_optimization_pass_1(cps_exp);
  CPS_lambda_unwrapping_optimization_pass_2(cps_exp);
  CPS_lambda_unwrapping_optimization_pass_3(cps_exp);
  CPS_lambda_unwrapping_optimization_pass_4(cps_exp);
  CPS_lambda_unwrapping_optimization_pass_5(cps_exp);
}

#endif