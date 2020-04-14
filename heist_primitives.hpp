// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_primitives.hpp
// => Defines primitive functions written in C++ for the Heist Scheme Interpreter

// PRIMITIVE FUNCTION MANDATORY TYPE SIGNATURE: 
// -> struct data(*)(scm_list&)

#ifndef HEIST_PRIMITIVES_HPP_
#define HEIST_PRIMITIVES_HPP_

#include "heist_primitives_toolkit.hpp"

/******************************************************************************
* ARITHMETIC PRIMITIVES
******************************************************************************/

// primitive "+" procedure
data primitive_ADD(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "+", "(+ <num1> <num2> ...)");
  num_type sum = 0;
  for(size_type i = 0, n = args.size(); i < n; ++i)
    sum += args[i].value.num;
  return data(sum);
}

// primitive "-" procedure: BOTH NEGATION & SUBTRACTION
data primitive_SUB(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "-", "(- <num1> <num2> ...)");
  num_type difference = args[0].value.num;
  if(args.size()==1) return -1 * difference;        // negation
  for(size_type i = 1, n = args.size(); i < n; ++i) // subtraction
    difference -= args[i].value.num;
  return data(difference);
}

// primitive "*" procedure:
data primitive_MUL(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "*", "(* <num1> <num2> ...)");
  num_type product = 1;
  for(size_type i = 0, n = args.size(); i < n; ++i)
    product *= args[i].value.num;
  return data(product);
}

// primitive "/" procedure:
data primitive_DIV(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "/", "(/ <num1> <num2> ...)");
  if(args.size()==1) return data(1 / args[0].value.num);
  num_type dividend = args[0].value.num;
  for(size_type i = 1, n = args.size(); i < n; ++i)
    dividend /= args[i].value.num;
  return data(dividend);
}

/******************************************************************************
* MISCELLANEOUS NUMERIC PRIMITIVE OPERATIONS 
******************************************************************************/

// primitive "abs" procedure
data primitive_ABS(scm_list& args) {
  confirm_unary_numeric(args, "abs", "(abs <num>)");
  return data(args[0].value.num.abs());
}

// primitive "expt" procedure
data primitive_EXPT(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "expt", "(expt <num1> <num2>)");
  confirm_n_args(2, args, "expt", "(expt <num1> <num2>)");
  return data((args[0].value.num ^ args[1].value.num));
}

// primitive "max" procedure
data primitive_MAX(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "max", "(max <num1> <num2> ...)");
  num_type max = args[0].value.num;
  for(size_type i = 1, n = args.size(); i < n; ++i)
    if(args[i].value.num > max)
      max = args[i].value.num;
  return max;
}

// primitive "min" procedure
data primitive_MIN(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "min", "(min <num1> <num2> ...)");
  num_type min = args[0].value.num;
  for(size_type i = 1, n = args.size(); i < n; ++i)
    if(args[i].value.num < min)
      min = args[i].value.num;
  return min;
}

// primitive "quotient" procedure
data primitive_QUOTIENT(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "quotient", "(quotient <num1> <num2>)");
  confirm_n_args(2, args, "quotient", "(quotient <num1> <num2>)");
  return data(args[0].value.num.quotient(args[1].value.num));
}

// primitive "remainder" procedure
data primitive_REMAINDER(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "remainder", "(remainder <num1> <num2>)");
  confirm_n_args(2, args, "remainder", "(remainder <num1> <num2>)");
  return data((args[0].value.num % args[1].value.num));
}

// primitive "modulo" procedure
data primitive_MODULO(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "modulo", "(modulo <num1> <num2>)");
  confirm_n_args(2, args, "modulo", "(modulo <num1> <num2>)");
  return data(args[0].value.num.modulo(args[1].value.num));
}

// primitive "exp" procedure
data primitive_EXP(scm_list& args) {
  confirm_unary_numeric(args, "exp", "(exp <num>)");
  return data(args[0].value.num.exp());
}

// primitive "log" procedure -- NATURAL LOGARITHM
data primitive_LOG(scm_list& args) {
  confirm_unary_numeric(args, "log", "(log <num>)");
  return data(args[0].value.num.log());
}

// primitive "sqrt" procedure
data primitive_SQRT(scm_list& args) {
  confirm_unary_numeric(args, "sqrt", "(sqrt <num>)");
  return data(args[0].value.num.sqrt());
}

// primitive "gcd" procedure
data primitive_GCD(scm_list& args) {
  if(no_args_given(args)) return num_type(0);
  confirm_no_numeric_primitive_errors(args, "gcd", "(gcd <num1> <num2> ...)");
  if(args.size() == 1) return args[0];
  // GCD is associative
  num_type gcd_val(args[0].value.num);
  for(size_type i = 1, n = args.size(); i < n; ++i)
    gcd_val = gcd_val.gcd(args[i].value.num);
  return gcd_val;
}

// primitive "lcm" procedure
data primitive_LCM(scm_list& args) {
  if(no_args_given(args)) return num_type(1);
  confirm_no_numeric_primitive_errors(args, "lcm", "(lcm <num1> <num2> ...)");
  if(args.size() == 1) return args[0];
  // LCM is associative
  num_type lcm_val(args[0].value.num);
  for(size_type i = 1, n = args.size(); i < n; ++i)
    lcm_val = lcm_val.lcm(args[i].value.num);
  return lcm_val;
}

/******************************************************************************
* MISCELLANEOUS NUMERIC PREDICATE PRIMITIVES 
******************************************************************************/

// primitive "odd?" procedure
data primitive_ODDP(scm_list& args) {
  confirm_unary_numeric(args, "odd?", "(odd? <num>)");
  return data(boolean(args[0].value.num.is_odd()));
}

// primitive "even?" procedure
data primitive_EVENP(scm_list& args) {
  confirm_unary_numeric(args, "even?", "(even? <num>)");
  return data(boolean(args[0].value.num.is_even()));
}

// primitive "positive?" procedure
data primitive_POSITIVEP(scm_list& args) {
  confirm_unary_numeric(args, "positive?", "(positive? <num>)");
  return data(boolean(args[0].value.num.is_pos()));
}

// primitive "negative?" procedure
data primitive_NEGATIVEP(scm_list& args) {
  confirm_unary_numeric(args, "negative?", "(negative? <num>)");
  return data(boolean(args[0].value.num.is_neg()));
}

// primitive "zero?" procedure
data primitive_ZEROP(scm_list& args) {
  confirm_unary_numeric(args, "zero?", "(zero? <num>)");
  return data(boolean(args[0].value.num.is_zero()));
}

// primitive "infinite?" procedure
data primitive_INFINITEP(scm_list& args) {
  confirm_unary_numeric(args, "infinite?", "(infinite? <num>)");
  return data(boolean(args[0].value.num.is_pos_inf() || 
                      args[0].value.num.is_neg_inf()));
}

// primitive "finite?" procedure (same as primitive "real?")
data primitive_FINITEP(scm_list& args) {
  confirm_unary_numeric(args, "finite?", "(finite? <num>)");
  return data(boolean(args[0].value.num.is_real()));
}

// primitive "nan?" procedure
data primitive_NANP(scm_list& args) {
  confirm_unary_numeric(args, "nan?", "(nan? <num>)");
  return data(boolean(args[0].value.num.is_nan()));
}

/******************************************************************************
* NUMERIC ROUNDING PRIMITIVES 
******************************************************************************/

// primitive "ceiling" procedure -- ROUNDS UP
data primitive_CEILING(scm_list& args) {
  confirm_unary_numeric(args, "ceiling", "(ceiling <num>)");
  return data(args[0].value.num.ceil());
}

// primitive "floor" procedure -- ROUNDS DOWN
data primitive_FLOOR(scm_list& args) {
  confirm_unary_numeric(args, "floor", "(floor <num>)");
  return data(args[0].value.num.floor());
}

// primitive "truncate" procedure -- ROUNDS TOWARDS ZERO
data primitive_TRUNCATE(scm_list& args) {
  confirm_unary_numeric(args, "truncate", "(truncate <num>)");
  return data(args[0].value.num.trunc());
}

// primitive "round" procedure -- ROUNDS TOWARDS THE NEAREST INT
data primitive_ROUND(scm_list& args) {
  confirm_unary_numeric(args, "round", "(round <num>)");
  return data(args[0].value.num.round());
}

/******************************************************************************
* NUMERIC PRECISION COERCION & INTEGER ANALYSIS PRIMITIVES 
******************************************************************************/

// primitive "inexact->exact" procedure
data primitive_COERCE_INEXACT_TO_EXACT(scm_list& args) {
  confirm_unary_numeric(args, "inexact->exact", "(inexact->exact <num>)");
  return data(args[0].value.num.to_exact());
}

// primitive "exact->inexact" procedure
data primitive_COERCE_EXACT_TO_INEXACT(scm_list& args) {
  confirm_unary_numeric(args, "exact->inexact", "(exact->inexact <num>)");
  return data(args[0].value.num.to_inexact());
}

// primitive "exact?" procedure
data primitive_EXACTP(scm_list& args) {
  confirm_unary_numeric(args, "exact?", "(exact? <num>)");
  return data(boolean(args[0].value.num.is_exact()));
}

// primitive "inexact?" procedure
data primitive_INEXACTP(scm_list& args) {
  confirm_unary_numeric(args, "inexact?", "(inexact? <num>)");
  return data(boolean(args[0].value.num.is_inexact()));
}

// primitive "integer?" procedure
data primitive_INTEGERP(scm_list& args) {
  confirm_unary_numeric(args, "integer?", "(integer? <num>)");
  return data(boolean(args[0].value.num.is_integer()));
}

// primitive "numerator" procedure
data primitive_NUMERATOR(scm_list& args) {
  confirm_unary_numeric(args, "numerator", "(numerator <num>)");
  return data(args[0].value.num.extract_numerator());
}

// primitive "denominator" procedure
data primitive_DENOMINATOR(scm_list& args) {
  confirm_unary_numeric(args, "denominator", "(denominator <num>)");
  return data(args[0].value.num.extract_denominator());
}

/******************************************************************************
* NUMERIC TRIGONOMETRIC PRIMITIVES -- IN RADIANS
******************************************************************************/

// primitive "sin" procedure
data primitive_SIN(scm_list& args) {
  confirm_unary_numeric(args, "sin", "(sin <num>)");
  return data(args[0].value.num.sin());
}

// primitive "cos" procedure
data primitive_COS(scm_list& args) {
  confirm_unary_numeric(args, "cos", "(cos <num>)");
  return data(args[0].value.num.cos());
}

// primitive "tan" procedure
data primitive_TAN(scm_list& args) {
  confirm_unary_numeric(args, "tan", "(tan <num>)");
  return data(args[0].value.num.tan());
}

// primitive "asin" procedure
data primitive_ASIN(scm_list& args) {
  confirm_unary_numeric(args, "asin", "(asin <num>)");
  return data(args[0].value.num.asin());
}

// primitive "acos" procedure
data primitive_ACOS(scm_list& args) {
  confirm_unary_numeric(args, "acos", "(acos <num>)");
  return data(args[0].value.num.acos());
}

// primitive "atan" procedure
data primitive_ATAN(scm_list& args) {
  confirm_unary_numeric(args, "atan", "(atan <num>)");
  return data(args[0].value.num.atan());
}

// primitive "sinh" procedure
data primitive_SINH(scm_list& args) {
  confirm_unary_numeric(args, "sinh", "(sinh <num>)");
  return data(args[0].value.num.sinh());
}

// primitive "cosh" procedure
data primitive_COSH(scm_list& args) {
  confirm_unary_numeric(args, "cosh", "(cosh <num>)");
  return data(args[0].value.num.cosh());
}

// primitive "tanh" procedure
data primitive_TANH(scm_list& args) {
  confirm_unary_numeric(args, "tanh", "(tanh <num>)");
  return data(args[0].value.num.tanh());
}

// primitive "asinh" procedure
data primitive_ASINH(scm_list& args) {
  confirm_unary_numeric(args, "asinh", "(asinh <num>)");
  return data(args[0].value.num.asinh());
}

// primitive "acosh" procedure
data primitive_ACOSH(scm_list& args) {
  confirm_unary_numeric(args, "acosh", "(acosh <num>)");
  return data(args[0].value.num.acosh());
}

// primitive "atanh" procedure
data primitive_ATANH(scm_list& args) {
  confirm_unary_numeric(args, "atanh", "(atanh <num>)");
  return data(args[0].value.num.atanh());
}

/******************************************************************************
* NUMERIC RANDOM NUMBER GENERATOR PRIMITIVES 
******************************************************************************/

// primitive "random" procedure -- SEED DEFAULTS TO CURRENT TIME SINCE EPOCH
data primitive_RANDOM(scm_list& args) {
  if(args.size() > 1)
    THROW_ERR("'random recieved more than 1 arg:" 
      "\n     (random <optional-numeric-seed>)" << FCN_ERR("random",args));
  if(!no_args_given(args) && args.size()==1 && !args[0].is_type(types::num))
    THROW_ERR("'random recieved non-numeric arg " << PROFILE(args[0])
      << ":\n     (random <optional-numeric-seed>)" << FCN_ERR("random",args));
  if(no_args_given(args))
    return data(num_type::random());
  return data(num_type::random(args[0].value.num));
}

/******************************************************************************
* NUMERIC COMPARISON PRIMITIVES
******************************************************************************/

// primitive "=" procedure:
data primitive_EQ(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "=", "(= <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num != args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive ">" procedure:
data primitive_GT(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, ">", "(> <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num <= args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "<" procedure:
data primitive_LT(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "<", "(< <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num >= args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive ">=" procedure:
data primitive_GTE(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, ">=", "(>= <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num < args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "<=" procedure:
data primitive_LTE(scm_list& args) {
  confirm_no_numeric_primitive_errors(args, "<=", "(<= <num1> <num2> ...)");
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].value.num > args[i+1].value.num) 
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

/******************************************************************************
* GENERAL COMPARISON PRIMITIVES
******************************************************************************/

// primitive "eq?" procedure:
data primitive_EQP(scm_list& args) {
  if(no_args_given(args))
    THROW_ERR("'eq? recieved no arguments: (eq? <obj1> <obj2> ...)" << FCN_ERR("eq?", args));
  // compare types & values
  for(size_type i = 0, n = args.size(); i+1 < n; ++i) {
    if(args[i].type != args[i+1].type) 
      return FALSE_DATA_BOOLEAN;
    if(args[i].is_type(types::str)) { // compare strings via pointers
      if(args[i].value.str != args[i+1].value.str)
        return FALSE_DATA_BOOLEAN;
    } else if(!prm_compare_atomic_values(args[i].value,args[i+1].value,args[i].type)) {
      return FALSE_DATA_BOOLEAN;
    }
  }
  return TRUE_DATA_BOOLEAN;
}

// primitive "eqv?" procedure:
data primitive_EQVP(scm_list& args) {
  if(no_args_given(args))
    THROW_ERR("'eqv? recieved no arguments: (eqv? <obj1> <obj2> ...)" << FCN_ERR("eqv?", args));
  // compare types & values
  for(size_type i = 0, n = args.size(); i+1 < n; ++i)
    if(args[i].type != args[i+1].type || !prm_compare_atomic_values(args[i].value,args[i+1].value,args[i].type))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "equal?" procedure:
data primitive_EQUALP(scm_list& args) {
  if(no_args_given(args))
    THROW_ERR("'equal? recieved no arguments: (equal? <obj1> <obj2> ...)" << FCN_ERR("equal?", args));
  for(size_type i = 0, n = args.size(); i+1 < n; ++i) {
    if(args[i].type != args[i+1].type) // compare types
      return FALSE_DATA_BOOLEAN;
    if(args[i].is_type(types::exp)) {  // compare sub-lists
      if(!prm_compare_SCM_LISTs(args[i].value.exp,args[i+1].value.exp))
        return FALSE_DATA_BOOLEAN;
    } else if(args[i].is_type(types::par)) {
      if(!prm_compare_PAIRs(args[i].value.par,args[i+1].value.par))
        return FALSE_DATA_BOOLEAN;
    } else if(args[i].is_type(types::vec)) {
      if(!prm_compare_VECTs(args[i].value.vec,args[i+1].value.vec))
        return FALSE_DATA_BOOLEAN;
    } else if(!prm_compare_atomic_values(args[i].value,args[i+1].value,args[i].type))
        return FALSE_DATA_BOOLEAN;     // compare values
  }
  return TRUE_DATA_BOOLEAN;
}

// primitive "not" procedure:
data primitive_NOT(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'not recieved incorrect # of arguments: (not <obj>)");
  bool data_is_false = args[0].is_type(types::bol) && !args[0].value.bol.val;
  return data(boolean(data_is_false));
}

/******************************************************************************
* CHARACTER PRIMITIVES
******************************************************************************/

// -----------------
// Char Comparators:
// -----------------

// primitive "char=?" procedure:
data primitive_CHAR_EQ(scm_list& args) {
  confirm_given_char_string_args(args, "char=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr != args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char<?" procedure:
data primitive_CHAR_LT(scm_list& args) {
  confirm_given_char_string_args(args, "char<?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr >= args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char>?" procedure:
data primitive_CHAR_GT(scm_list& args) {
  confirm_given_char_string_args(args, "char>?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr <= args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char<=?" procedure:
data primitive_CHAR_LTE(scm_list& args) {
  confirm_given_char_string_args(args, "char<=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr > args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char>=?" procedure:
data primitive_CHAR_GTE(scm_list& args) {
  confirm_given_char_string_args(args, "char>=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(args[i].value.chr < args[i+1].value.chr)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci=?" procedure:
data primitive_CHAR_CI_EQ(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) != mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci<?" procedure:
data primitive_CHAR_CI_LT(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci<?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) >= mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci>?" procedure:
data primitive_CHAR_CI_GT(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci>?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) <= mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci<=?" procedure:
data primitive_CHAR_CI_LTE(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci<=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) > mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "char-ci>=?" procedure:
data primitive_CHAR_CI_GTE(scm_list& args) {
  confirm_given_char_string_args(args, "char-ci>=?", "char");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(mklower(args[i].value.chr) < mklower(args[i+1].value.chr))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// --------------
// Char Analysis:
// --------------

// primitive "char-alphabetic?" procedure:
data primitive_CHAR_ALPHABETICP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-alphabetic?");
  return boolean(isalpha(args[0].value.chr));
}

// primitive "char-numeric?" procedure:
data primitive_CHAR_NUMERICP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-numeric?");
  return boolean(isdigit(args[0].value.chr));
}

// primitive "char-whitespace?" procedure:
data primitive_CHAR_WHITESPACEP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-whitespace?");
  return boolean(isspace(args[0].value.chr));
}

// primitive "char-upper-case?" procedure:
data primitive_CHAR_UPPER_CASEP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-upper-case?");
  return boolean(isupper(args[0].value.chr));
}

// primitive "char-lower-case?" procedure:
data primitive_CHAR_LOWER_CASEP(scm_list& args) {
  confirm_given_one_char_arg(args, "char-lower-case?");
  return boolean(islower(args[0].value.chr));
}

// ------------------
// Char Case Control:
// ------------------

// primitive "char-upcase" procedure:
data primitive_CHAR_UPCASE(scm_list& args) {
  confirm_given_one_char_arg(args, "char-upcase");
  return toupper(args[0].value.chr);
}

// primitive "char-downcase" procedure:
data primitive_CHAR_DOWNCASE(scm_list& args) {
  confirm_given_one_char_arg(args, "char-downcase");
  return tolower(args[0].value.chr);
}

/******************************************************************************
* STRING PRIMITIVES
******************************************************************************/

// primitive "make-string" procedure:
data primitive_MAKE_STRING(scm_list& args) {
  // confirm valid length given
  if(no_args_given(args) || args.size() > 2 || !primitive_is_valid_size(args[0]))
    THROW_ERR("'make-string didn't recieve a proper positive integer size!"
      "\n     (make-string <size> <optional-fill-char>)"
      "\n     <size> range: (0," << MAX_SIZE_TYPE << ']'
      << FCN_ERR("make-string", args));
  if(args.size()==2 && !args[1].is_type(types::chr))
    THROW_ERR("'make-string recieved a non-character fill value:"
      "\n     Received fill value "<<PROFILE(args[1])<<'!'
      << "\n     (make-string <size> <optional-fill-char>)"
      << FCN_ERR("make-string", args));
  // mk a string w/ the the given reserve size
  size_type n = (size_type)args[0].value.num.extract_inexact();
  return make_str(scm_string(n, (args.size()==2 ? args[1].value.chr : '?')));
}

// primitive "string" procedure:
data primitive_STRING(scm_list& args) {
  if(no_args_given(args)) return make_str("");
  if(auto i = confirm_only_args_of_type(args, types::chr); i != -1)
    THROW_ERR("'string arg #" << i+1 << ", " << PROFILE(args[i]) 
      << ",\n     isn't a character: (string <char1> <char2> ...)"
      << FCN_ERR("string", args));
  scm_string str_val;
  for(const auto& ch : args) str_val += ch.value.chr;
  return make_str(str_val);
}

// primitive "string-length" procedure:
data primitive_STRING_LENGTH(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 1, "string-length", "(string-length <string>)");
  return num_type(args[0].value.str->size());
}

// primitive "string-ref" procedure:
data primitive_STRING_REF(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 2, "string-ref", "(string-ref <string> <index>)");
  auto i = primitive_confirm_valid_string_idx(args, "string-ref", "(string-ref <string> <index>)");
  return args[0].value.str->operator[](i);
}

// primitive "string-set!" procedure:
data primitive_STRING_SET(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 3, "string-set!", "(string-set! <string> <index> <char>)");
  auto i = primitive_confirm_valid_string_idx(args, "string-set!", "(string-set! <string> <index> <char>)");
  if(!args[2].is_type(types::chr))
    THROW_ERR("'string-set! recieved non-character set-value \n     " 
      << PROFILE(args[2]) << "!\n     (string-set! <string> <index> <char>)"
      << FCN_ERR("string-set!", args));
  args[0].value.str->operator[](i) = args[2].value.chr;
  return data(types::dne);
}

// primitive "substring" procedure:
data primitive_SUBSTRING(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 3, "substring", "(substring <string> <start> <end>)");
  auto start = primitive_confirm_valid_string_idx(args, "string-ref", "(string-ref <string> <end>)");
  // confirm given an in-'size_type'-range non-negative end index
  if(!primitive_is_valid_index(args[2]))
    THROW_ERR("'substring arg "<<PROFILE(args[2])<<" isn't a proper non-negative integer <end> index!"
      "\n     (substring <string> <start> <end>)"
      "\n     <start> & <end> range: [0," << MAX_SIZE_TYPE << ']'
      << FCN_ERR("substring", args));
  // confirm index falls w/in range of the container
  const size_type end = (size_type)args[2].value.num.extract_inexact();
  if(end > args[0].value.str->size())
    THROW_ERR("'substring recieved out of range <end> index " << end 
      <<"\n     for string "<<args[0]<<" of size "<< args[0].value.str->size()<<'!'
      << FCN_ERR("substring", args));
  return make_str(args[0].value.str->substr(start,end));
}

// primitive "string-append" procedure:
data primitive_STRING_APPEND(scm_list& args) {
  if(no_args_given(args)) return make_str("");
  if(auto i = confirm_only_args_of_type(args, types::str); i != -1)
    THROW_ERR("'string-append arg #" << i+1 << ", " << PROFILE(args[i]) 
      << ",\n     isn't a string: (string-append <string1> <string2> ...)"
      << FCN_ERR("string-append", args));
  scm_string str_val;
  for(const auto& str : args) str_val += *str.value.str;
  return make_str(str_val);
}

// primitive "string-copy" procedure:
data primitive_STRING_COPY(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 1, "string-copy", "(string-copy <string>)");
  return make_str(*args[0].value.str);
}

// primitive "string-fill!" procedure:
data primitive_STRING_FILL(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 2, "string-fill!", "(string-fill! <string> <char>)");
  if(!args[1].is_type(types::chr))
    THROW_ERR("'string-fill! recieved non-character fill-value\n     " 
      << PROFILE(args[1]) << "!\n     (string-fill! <string> <char>)"
      << FCN_ERR("string-fill!", args));
  for(size_type i = 0, n = args[0].value.str->size(); i < n; ++i)
    args[0].value.str->operator[](i) = args[1].value.chr;
  return data(types::dne);
}

// -------------------
// String Comparators:
// -------------------

// primitive "string=?" procedure:
data primitive_STRING_EQ(scm_list& args) {
  confirm_given_char_string_args(args, "string=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str != *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string<?" procedure:
data primitive_STRING_LT(scm_list& args) {
  confirm_given_char_string_args(args, "string<?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str >= *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string>?" procedure:
data primitive_STRING_GT(scm_list& args) {
  confirm_given_char_string_args(args, "string>?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str <= *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string<=?" procedure:
data primitive_STRING_LTE(scm_list& args) {
  confirm_given_char_string_args(args, "string<=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str > *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string>=?" procedure:
data primitive_STRING_GTE(scm_list& args) {
  confirm_given_char_string_args(args, "string>=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(*args[i].value.str < *args[i+1].value.str)
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci=?" procedure:
data primitive_STRING_CI_EQ(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) != lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci<?" procedure:
data primitive_STRING_CI_LT(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci<?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) >= lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci>?" procedure:
data primitive_STRING_CI_GT(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci>?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) <= lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci<=?" procedure:
data primitive_STRING_CI_LTE(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci<=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) > lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

// primitive "string-ci>=?" procedure:
data primitive_STRING_CI_GTE(scm_list& args) {
  confirm_given_char_string_args(args, "string-ci>=?", "string");
  for(size_type i = 0, n = args.size(); i < n-1; ++i)
    if(lowercase_str(*args[i].value.str) < lowercase_str(*args[i+1].value.str))
      return FALSE_DATA_BOOLEAN;
  return TRUE_DATA_BOOLEAN;
}

/******************************************************************************
* PAIR PRIMITIVES
******************************************************************************/

// --------------
// Pair Handlers:
// --------------

// primitive "cons" procedure:
data primitive_CONS(scm_list& args) {
  if(args.size() != 2)
    THROW_ERR("'cons didn't recieved 2 arguments: (cons <car-obj> <cdr-obj>)"
      << FCN_ERR("cons", args));
  data new_pair = data(make_par());
  new_pair.value.par->first = args[0];
  new_pair.value.par->second = args[1];
  return new_pair;
}

// primitive "car" procedure:
data primitive_CAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "car", "\n     (car <pair>)");
  return args[0].value.par->first;
}

// primitive "cdr" procedure:
data primitive_CDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdr", "\n     (cdr <pair>)");
  return args[0].value.par->second;
}

// primitive "null?" procedure:
data primitive_NULLP(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'null? recieved incorrect # of arguments: (null? <obj>)"
      << FCN_ERR("null?", args));
  return data(boolean(data_is_the_empty_expression(args[0])));
}

// primitive "set-car!" procedure:
data primitive_SETCAR(scm_list& args) {
  if(args.size() != 2)
    THROW_ERR("'set-car! recieved incorrect # of arguments:"
      "\n     (set-car! <pair> <obj>)" << FCN_ERR("set-car!", args));
  if(!args[0].is_type(types::par))
    THROW_ERR("'set-car!'s 1st arg "<<PROFILE(args[0])<<" isn't a pair:"
      "\n     (set-car! <pair> <obj>)" << FCN_ERR("set-car!", args));
  args[0].value.par->first = args[1];
  return data(types::dne); // return is implementation dependant, We've chosen 'ok symbol
}

// primitive "set-cdr!" procedure:
data primitive_SETCDR(scm_list& args) {
  if(args.size() != 2)
    THROW_ERR("'set-cdr! recieved incorrect # of arguments:"
      "\n     (set-cdr! <pair> <obj>)" << FCN_ERR("set-cdr!", args));
  if(!args[0].is_type(types::par))
    THROW_ERR("'set-cdr!'s 1st arg "<<PROFILE(args[0])<<" isn't a pair:"
      "\n     (set-cdr! <pair> <obj>)" << FCN_ERR("set-cdr!", args));
  args[0].value.par->second = args[1];
  return data(types::dne); // return is implementation dependant, We've chosen 'ok symbol
}

// ---------------------
// Car/Cdr Combinations:
// ---------------------

// primitive "caar" procedure:
data primitive_CAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caar", "\n     (caar <pair>)");
  confirm_nth_car_is_pair(args[0], "caar", "1st", args);
  return args[0].value.par->first.value.par->first;
}

// primitive "cadr" procedure:
data primitive_CADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadr", "\n     (cadr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cadr", "1st", args);
  return args[0].value.par->second.value.par->first;
}

// primitive "cdar" procedure:
data primitive_CDAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdar", "\n     (cdar <pair>)");
  confirm_nth_car_is_pair(args[0], "cdar", "1st", args);
  return args[0].value.par->first.value.par->second;
}

// primitive "cddr" procedure:
data primitive_CDDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddr", "\n     (cddr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cddr", "1st", args);
  return args[0].value.par->second.value.par->second;
}

// ----------

// primitive "caaar" procedure:
data primitive_CAAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caaar", "\n     (caaar <pair>)");
  confirm_nth_car_is_pair(args[0], "caaar", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->first, "caaar", "2nd", args);
  return args[0].value.par->first.value.par->first.value.par->first;
}

// primitive "caadr" procedure:
data primitive_CAADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caadr", "\n     (caadr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "caadr", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->second, "caadr", "1st", args);
  return args[0].value.par->second.value.par->first.value.par->first;
}

// primitive "cadar" procedure:
data primitive_CADAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadar", "\n     (cadar <pair>)");
  confirm_nth_car_is_pair(args[0], "cadar", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first, "cadar", "1st", args);
  return args[0].value.par->first.value.par->second.value.par->first;
}

// primitive "caddr" procedure:
data primitive_CADDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caddr", "\n     (caddr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "caddr", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second, "caddr", "2nd", args);
  return args[0].value.par->second.value.par->second.value.par->first;
}

// primitive "cdaar" procedure:
data primitive_CDAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdaar", "\n     (cdaar <pair>)");
  confirm_nth_car_is_pair(args[0], "cdaar", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->first, "cdaar", "2nd", args);
  return args[0].value.par->first.value.par->first.value.par->second;
}

// primitive "cdadr" procedure:
data primitive_CDADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdadr", "\n     (cdadr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cdadr", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->second, "cdadr", "1st", args);
  return args[0].value.par->second.value.par->first.value.par->second;
}

// primitive "cddar" procedure:
data primitive_CDDAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddar", "\n     (cddar <pair>)");
  confirm_nth_car_is_pair(args[0], "cddar", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first, "cddar", "1st", args);
  return args[0].value.par->first.value.par->second.value.par->second;
}

// primitive "cdddr" procedure:
data primitive_CDDDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdddr", "\n     (cdddr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cdddr", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second, "cdddr", "2nd", args);
  return args[0].value.par->second.value.par->second.value.par->second;
}

// ----------

// primitive "caaaar" procedure:
data primitive_CAAAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caaaar", "\n     (caaaar <pair>)");
  confirm_nth_car_is_pair(args[0], "caaaar", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->first, "caaaar", "2nd", args);
  confirm_nth_car_is_pair(args[0].value.par->first.value.par->first, "caaaar", "3rd", args);
  return args[0].value.par->first.value.par->first.value.par->first.value.par->first;
}

// primitive "caaadr" procedure:
data primitive_CAAADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caaadr", "\n     (caaadr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "caaadr", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->second, "caaadr", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->second.value.par->first, "caaadr", "2nd", args);
  return args[0].value.par->second.value.par->first.value.par->first.value.par->first;
}

// primitive "caadar" procedure:
data primitive_CAADAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caadar", "\n     (caadar <pair>)");
  confirm_nth_car_is_pair(args[0], "caadar", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first, "caadar", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->first.value.par->second, "caadar", "2nd", args);
  return args[0].value.par->first.value.par->second.value.par->first.value.par->first;
}

// primitive "caaddr" procedure:
data primitive_CAADDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caaddr", "\n     (caaddr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "caaddr", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second, "caaddr", "2nd", args);
  confirm_nth_car_is_pair(args[0].value.par->second.value.par->second, "caaddr", "1st", args);
  return args[0].value.par->second.value.par->second.value.par->first.value.par->first;
}

// primitive "cadaar" procedure:
data primitive_CADAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadaar", "\n     (cadaar <pair>)");
  confirm_nth_car_is_pair(args[0], "cadaar", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->first, "cadaar", "2nd", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first.value.par->first, "cadaar", "1st", args);
  return args[0].value.par->first.value.par->first.value.par->second.value.par->first;
}

// primitive "cadadr" procedure:
data primitive_CADADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadadr", "\n     (cadadr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cadadr", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->second, "cadadr", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second.value.par->first, "cadadr", "2nd", args);
  return args[0].value.par->second.value.par->first.value.par->second.value.par->first;
}

// primitive "caddar" procedure:
data primitive_CADDAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "caddar", "\n     (caddar <pair>)");
  confirm_nth_car_is_pair(args[0], "caddar", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first, "caddar", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first.value.par->second, "caddar", "2nd", args);
  return args[0].value.par->first.value.par->second.value.par->second.value.par->first;
}

// primitive "cadddr" procedure:
data primitive_CADDDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cadddr", "\n     (cadddr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cadddr", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second, "cadddr", "2nd", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second.value.par->second, "cadddr", "3rd", args);
  return args[0].value.par->second.value.par->second.value.par->second.value.par->first;
}


// primitive "cdaaar" procedure:
data primitive_CDAAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdaaar", "\n     (cdaaar <pair>)");
  confirm_nth_car_is_pair(args[0], "cdaaar", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->first, "cdaaar", "2nd", args);
  confirm_nth_car_is_pair(args[0].value.par->first.value.par->first, "cdaaar", "3rd", args);
  return args[0].value.par->first.value.par->first.value.par->first.value.par->second;
}

// primitive "cdaadr" procedure:
data primitive_CDAADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdaadr", "\n     (cdaadr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cdaadr", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->second, "cdaadr", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->second.value.par->first, "cdaadr", "2nd", args);
  return args[0].value.par->second.value.par->first.value.par->first.value.par->second;
}

// primitive "cdadar" procedure:
data primitive_CDADAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdadar", "\n     (cdadar <pair>)");
  confirm_nth_car_is_pair(args[0], "cdadar", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first, "cdadar", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->first.value.par->second, "cdadar", "2nd", args);
  return args[0].value.par->first.value.par->second.value.par->first.value.par->second;
}

// primitive "cdaddr" procedure:
data primitive_CDADDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdaddr", "\n     (cdaddr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cdaddr", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second, "cdaddr", "2nd", args);
  confirm_nth_car_is_pair(args[0].value.par->second.value.par->second, "cdaddr", "1st", args);
  return args[0].value.par->second.value.par->second.value.par->first.value.par->second;
}

// primitive "cddaar" procedure:
data primitive_CDDAAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddaar", "\n     (cddaar <pair>)");
  confirm_nth_car_is_pair(args[0], "cddaar", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->first, "cddaar", "2nd", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first.value.par->first, "cddaar", "1st", args);
  return args[0].value.par->first.value.par->first.value.par->second.value.par->second;
}

// primitive "cddadr" procedure:
data primitive_CDDADR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddadr", "\n     (cddadr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cddadr", "1st", args);
  confirm_nth_car_is_pair(args[0].value.par->second, "cddadr", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second.value.par->first, "cddadr", "2nd", args);
  return args[0].value.par->second.value.par->first.value.par->second.value.par->second;
}

// primitive "cdddar" procedure:
data primitive_CDDDAR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cdddar", "\n     (cdddar <pair>)");
  confirm_nth_car_is_pair(args[0], "cdddar", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first, "cdddar", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->first.value.par->second, "cdddar", "2nd", args);
  return args[0].value.par->first.value.par->second.value.par->second.value.par->second;
}

// primitive "cddddr" procedure:
data primitive_CDDDDR(scm_list& args) {
  confirm_given_a_pair_arg(args, "cddddr", "\n     (cddddr <pair>)");
  confirm_nth_cdr_is_pair(args[0], "cddddr", "1st", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second, "cddddr", "2nd", args);
  confirm_nth_cdr_is_pair(args[0].value.par->second.value.par->second, "cddddr", "3rd", args);
  return args[0].value.par->second.value.par->second.value.par->second.value.par->second;
}

/******************************************************************************
* LIST PRIMITIVES
******************************************************************************/

// primitive "list" procedure:
data primitive_LIST(scm_list& args) {
  if(no_args_given(args)) return data(symconst::emptylist); // "(list)" = "'()"
  return primitive_LIST_to_CONS_constructor(args.begin(), args.end());
}

// primitive "length" procedure:
data primitive_LENGTH(scm_list& args) {
  if(primitive_validate_list_and_return_if_empty(args,"length")) 
    return num_type();
  num_type count;
  primitive_LENGTH_computation(args[0].value.par->second,count);
  return count;
}

// primitive "circular-list?" procedure:
data primitive_CIRCULAR_LISTP(scm_list& args) {
  // list? requires 1 arg
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'circular-list? recieved incorrect # of arguments:"
      "\n     (circular-list? <obj>)" << FCN_ERR("circular-list?", args));
  // if not pair, GUARENTEED not a circular list
  if(!args[0].is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  if(primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::cyclic)
    return TRUE_DATA_BOOLEAN;
  return FALSE_DATA_BOOLEAN;
}

// primitive "dotted-list?" procedure:
data primitive_DOTTED_LISTP(scm_list& args) {
  // list? requires 1 arg
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'dotted-list? recieved incorrect # of arguments:"
      "\n     (dotted-list? <obj>)" << FCN_ERR("dotted-list?", args));
  // if not pair, GUARENTEED not a dotted list
  if(!args[0].is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  if(primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::no_null)
    return TRUE_DATA_BOOLEAN;
  return FALSE_DATA_BOOLEAN;
}

// primitive "list?" procedure:
//   => where 'list' := finite & null-terminated pair sequence
data primitive_LISTP(scm_list& args) {
  // list? requires 1 arg
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'list? recieved incorrect # of arguments: (list? <obj>)"
      << FCN_ERR("list?", args));
  // the empty list is a list
  if(data_is_the_empty_expression(args[0]))
    return TRUE_DATA_BOOLEAN;
  // if non-empty list & not pair, GUARENTEED not a list
  if(!args[0].is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  // valid lists are finite & terminate with '()
  if(primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::ok)
    return TRUE_DATA_BOOLEAN;
  return FALSE_DATA_BOOLEAN;
}

// primitive "alist?" procedure:
data primitive_ALISTP(scm_list& args) {
  // alist? requires 1 arg
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'alist? recieved incorrect # of arguments: (alist? <obj>)"
      << FCN_ERR("alist?", args));
  // if not pair, GUARENTEED not an association list
  if(!args[0].is_type(types::par))
    return FALSE_DATA_BOOLEAN;
  // valid association lists are finite, terminate with '(), and only contain other pairs
  return boolean(
          (primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::ok) && 
           primitive_list_only_contains_pairs(args[0]));
}

// primitive "append" procedure:
// PRECONDITIONS: 1) THE FIRST n-1 ARGS MUST HAVE "list?" = TRUE
//                2) THE LAST ARG MUST NOT BE A CYCLIC LIST
data primitive_APPEND(scm_list& args) {
  // (append) = '()
  if(no_args_given(args)) return data(symconst::emptylist);
  // (append <obj>) = <obj>
  const size_type n = args.size();
  if(n == 1)              return args[0];
  // (append '() <obj>) = <obj>
  if(data_is_the_empty_expression(args[0])) return args[1];
  // Confirm Precondition 2 
  if(args[n-1].is_type(types::par) && 
    primitive_list_is_acyclic_and_null_terminated(args[n-1]) == list_status::cyclic)
    THROW_ERR("'append - last argument "<<PROFILE(args[n-1])<<" isn't an acyclic list:"
      "\n     (append <list> <obj>)" << FCN_ERR("append", args));
  // Confirm Precondition 1
  for(size_type i = 0; i < n-1; ++i) {
    if(!args[i].is_type(types::par))
      THROW_ERR("'append argument #" << i+1 << ' ' << PROFILE(args[i]) << " isn't a pair:"
        "\n     (append <list> <obj>)" << FCN_ERR("append", args));
    else if(auto stat = primitive_list_is_acyclic_and_null_terminated(args[i]); 
      stat == list_status::cyclic) {
      THROW_ERR("'append argument #" << i+1 << ' ' << PROFILE(args[i]) 
        << " isn't an acyclic list:\n     (append <list> <obj>)" << FCN_ERR("append", args));
    } else if(stat == list_status::no_null)
      THROW_ERR("'append argument #" << i+1 << ' ' << PROFILE(args[i]) << " isn't a '() terminated list:"
        "\n     (append <list> <obj>)" << FCN_ERR("append", args));
  }
  // Link the lists to one another
  for(size_type i = 0; i < n-1; ++i)
    primitive_APPEND_list_linker(args[i],args[i+1]);
  return args[0];
}

// primitive "reverse" procedure:
data primitive_REVERSE(scm_list& args) {
  if(primitive_validate_list_and_return_if_empty(args,"reverse")) return args[0];
  scm_list par_as_exp;
  shallow_unpack_list_into_exp(args[0], par_as_exp);
  std::reverse(par_as_exp.begin(),par_as_exp.end());
  return primitive_LIST(par_as_exp);
}

// primitive "sublist" procedure:
data primitive_SUBLIST(scm_list& args) {
  scm_string format = "\n     (sublist <list> <start-index> <end-index>)"
                      "\n     <index> range: [0," +
                      std::to_string(MAX_SIZE_TYPE) + ']';
  return primitive_list_sublist_extraction(args, "sublist", format.c_str(), false);
}

// primitive "list-tail" procedure:
data primitive_LIST_TAIL(scm_list& args) {
  scm_string format = "\n     (list-tail <list> <index>)"
                      "\n     <index> range: [0," +
                      std::to_string(MAX_SIZE_TYPE) + ']';
  return primitive_list_sublist_extraction(args, "list-tail", format.c_str(), true);
}

// primitive "list-head" procedure:
data primitive_LIST_HEAD(scm_list& args) {
  scm_string format = "\n     (list-head <list> <index>)"
                      "\n     <index> range: [0," +
                      std::to_string(MAX_SIZE_TYPE) + ']';
  if(!args.empty()) args.insert(args.begin()+1, num_type(0)); // insert 0 as <start>
  return primitive_list_sublist_extraction(args, "list-head", format.c_str(), false);
}

// primitive "list-ref" procedure:
data primitive_LIST_REF(scm_list& args) {
  scm_string format = "\n     (list-ref <list> <index>)"
                      "\n     <index> range: [0," +
                      std::to_string(MAX_SIZE_TYPE) + ']';
  return primitive_list_sublist_extraction(args,"list-ref",format.c_str(),true).value.par->first;
}

// ----------------------
// List Control Features:
// ----------------------

// primitive "for-each" procedure:
data primitive_FOR_EACH(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Get the head of each list
  scm_list list_heads = (args.size() > 1) ? scm_list(args.begin()+1, args.end()) : scm_list{};
  primitive_FOREACH_MAP_get_list_heads(args, list_heads, 
    "for-each", "\n     (for-each <procedure> <list1> <list2> ...)");
  // Apply the procedure on each elt of each list
  primitive_FOR_EACH_applicator(list_heads, args[0].value.exp, env);
  return data(types::dne);
}

// primitive "map" procedure:
data primitive_MAP(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Get the head of each list
  scm_list list_heads = (args.size() > 1) ? scm_list(args.begin()+1, args.end()) : scm_list{};
  primitive_FOREACH_MAP_get_list_heads(args, list_heads, 
    "map", "\n     (map <procedure> <list1> <list2> ...)");
  // Apply the procedure on each elt of each list & store the result
  scm_list mapped_list;
  primitive_MAP_list_constructor(list_heads, args[0].value.exp, mapped_list, env);
  return primitive_LIST(mapped_list); // return a list of the results
}

// primitive "filter" procedure:
data primitive_FILTER(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given correct # of args needed
  static constexpr const char * const format = "\n     (filter <procedure> <list>)";
  if(args.size() != 2) 
    THROW_ERR("'filter recieved incorrect # of args (given " 
      << count_args(args) << "):" << format << FCN_ERR("filter", args));
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], "filter", format, args);
  // Confirm only given proper lists of the same length
  scm_list list_head({args[1]});
  primitive_confirm_proper_same_sized_lists(list_head,"filter",format,1,args);
  // Apply the procedure on each elt of each list, & store args that eval'd to 'true'
  scm_list filtered_list;
  primitive_FILTER_list_constructor(list_head[0], args[0].value.exp, filtered_list, env);
  return primitive_LIST(filtered_list); // return a list of the valid values
}

// primitive "fold-left" procedure:
data primitive_FOLD_LEFT(scm_list& args) {
  return primitive_FOLD_template(args, "fold-left", 
    "\n     (fold-left <procedure> <init> <list1> <list2> ...)",true);
}

// primitive "fold-right" procedure:
data primitive_FOLD_RIGHT(scm_list& args) {
  return primitive_FOLD_template(args, "fold-right", 
    "\n     (fold-right <procedure> <init> <list1> <list2> ...)",false);
}

// primitive "unfold" procedure:
data primitive_UNFOLD(scm_list& args) {
  scm_list unfolded;
  primitive_UNFOLD_template(args,unfolded,"unfold",
    "\n     (unfold <break-condition> <map-procdure> <increment-procedure> <seed>)");
  return primitive_LIST(unfolded);
}

// -----------------------
// List Member Extraction:
// -----------------------

// primitive "memq" procedure:
data primitive_MEMQ(scm_list& args) {
  return primitive_MEM_template(args, "memq", primitive_EQP);
}

// primitive "memv" procedure:
data primitive_MEMV(scm_list& args) {
  return primitive_MEM_template(args, "memv", primitive_EQVP);
}

// primitive "member" procedure:
data primitive_MEMBER(scm_list& args) {
  return primitive_MEM_template(args, "member", primitive_EQUALP);
}

// primitive "assq" procedure:
data primitive_ASSQ(scm_list& args) {
  return primitive_ASSOCIATION_template(args, "assq", primitive_EQP);
}

// primitive "assv" procedure:
data primitive_ASSV(scm_list& args) {
  return primitive_ASSOCIATION_template(args, "assv", primitive_EQVP);
}

// primitive "assoc" procedure:
data primitive_ASSOC(scm_list& args) {
  return primitive_ASSOCIATION_template(args, "assoc", primitive_EQUALP);
}

/******************************************************************************
* VECTOR PRIMITIVES
******************************************************************************/

// primitive "vector" procedure:
data primitive_VECTOR(scm_list& args) {
  if(no_args_given(args)) return data(make_vec(scm_list{}));
  return data(make_vec(scm_list(args.begin(), args.end())));
}

// primitive "make-vector" procedure:
data primitive_MAKE_VECTOR(scm_list& args) {
  // confirm valid length given
  if(no_args_given(args) || args.size() > 2 || !primitive_is_valid_size(args[0]))
    THROW_ERR("'make-vector didn't recieve a proper positive integer size!"
      "\n     (make-vector <size> <optional-fill-value>)"
      "\n     <size> range: (0," << MAX_SIZE_TYPE << ']' << FCN_ERR("make-vector", args));
  // mk a vector w/ the the given reserve size
  size_type n = (size_type)args[0].value.num.extract_inexact();
  data vect(make_vec(scm_list(n)));
  // fill vector as needed
  if(args.size() == 2)
    for(size_type i = 0; i < n; ++i)
      vect.value.vec->operator[](i) = args[1];
  return vect;
}

// primitive "vector-length" procedure:
data primitive_VECTOR_LENGTH(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 1, "vector-length", "(vector-length <vector>)");
  return num_type(args[0].value.vec->size());
}

// primitive "vector-ref" procedure:
data primitive_VECTOR_REF(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 2, "vector-ref", "(vector-ref <vector> <index>)");
  auto i = primitive_confirm_valid_vector_idx(args, "vector-ref", "(vector-ref <vector> <index>)");
  return args[0].value.vec->operator[](i);
}

// primitive "vector-set!" procedure:
data primitive_VECTOR_SET(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 3, "vector-set!", "(vector-set! <vector> <index> <obj>)");
  auto i = primitive_confirm_valid_vector_idx(args, "vector-set!", "(vector-set! <vector> <index> <obj>)");
  args[0].value.vec->operator[](i) = args[2];
  return data(types::dne);
}

// primitive "vector-fill!" procedure:
data primitive_VECTOR_FILL(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 2, "vector-fill!", "(vector-fill! <vector> <obj>)");
  for(size_type i = 0, n = args[0].value.vec->size(); i < n; ++i)
    args[0].value.vec->operator[](i) = args[1];
  return data(types::dne);
}

// primitive "vector-grow" procedure:
data primitive_VECTOR_GROW(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 2, "vector-grow", "(vector-grow <vector> <size>)");
  if(!primitive_is_valid_size(args[1]))
    THROW_ERR("'vector-grow didn't recieve a proper positive integer size!"
      "\n     (vector-grow <vector> <size>)"
      "\n     <size> range: (0," << MAX_SIZE_TYPE << ']' << FCN_ERR("vector-grow", args));
  if(args[1].value.num < args[0].value.vec->size())
    THROW_ERR("'vector-grow "<<args[1].value.num<<" is too small to expand "
      << args[0] << " of size " << args[0].value.vec->size() << " with!"
      "\n     (vector-grow <vector> <size>)"
      "\n     <size> range: (0," << MAX_SIZE_TYPE << ']' << FCN_ERR("vector-grow", args));
  if(args[1].value.num == args[0].value.vec->size())
    return args[0]; // nothing to expand
  scm_list expanded_vec((size_type)args[1].value.num.extract_inexact());
  std::copy(args[0].value.vec->begin(), args[0].value.vec->end(), expanded_vec.begin());
  return make_vec(expanded_vec);
}

// primitive "subvector" procedure:
data primitive_SUBVECTOR(scm_list& args) {
  scm_string format = "\n     (subvector <vector> <start-index> <end-index>)"
                      "\n     <index> range: [0," +
                      std::to_string(MAX_SIZE_TYPE) + ']';
  return primitive_subvector_extraction(args, "subvector", format.c_str(), false);
}

// primitive "vector-tail" procedure:
data primitive_VECTOR_TAIL(scm_list& args) {
  scm_string format = "\n     (vector-tail <vector> <index>)"
                      "\n     <index> range: [0," +
                      std::to_string(MAX_SIZE_TYPE) + ']';
  return primitive_subvector_extraction(args, "vector-tail", format.c_str(), true);
}

// primitive "vector-head" procedure:
data primitive_VECTOR_HEAD(scm_list& args) {
  scm_string format = "\n     (vector-head <vector> <index>)"
                      "\n     <index> range: [0," +
                      std::to_string(MAX_SIZE_TYPE) + ']';
  if(!args.empty()) args.insert(args.begin()+1, num_type(0)); // insert 0 as <start>
  return primitive_subvector_extraction(args, "vector-head", format.c_str(), false);
}

// primitive "vector-map" procedure:
data primitive_VECTOR_MAP(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given minimum # of args needed
  static constexpr const char * const format = 
    "\n     (vector-map <procedure> <vector1> <vector2> ...)";
  if(args.size() < 2) 
    THROW_ERR("'vector-map recieved insufficient args (only " 
      << count_args(args) << "):" << format << FCN_ERR("vector-map", args));
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], "vector-map", format, args);
  // Confirm only given vectors of the same length
  scm_list vectors(args.begin()+1, args.end());
  primitive_confirm_same_sized_vector(vectors,format,args);
  // Apply the procedure on each elt of each vector & store the result
  scm_list mapped_vector;
  primitive_MAP_vector_constructor(vectors, args[0].value.exp, mapped_vector, env);
  return make_vec(mapped_vector); // return a vector of the results
}

// primitive "vector-unfold" procedure:
data primitive_VECTOR_UNFOLD(scm_list& args) {
  scm_list unfolded;
  primitive_UNFOLD_template(args,unfolded,"vector-unfold",
    "\n     (vector-unfold <break-condition> <map-procdure> <increment-procedure> <seed>)");
  return make_vec(unfolded);
}

/******************************************************************************
* SORTING PRIMITIVES: FOR BOTH LISTS & VECTORS
******************************************************************************/

// primitive "sort" procedure:
data primitive_SORT(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm has a valid argument signature
  primitive_confirm_sortable_container(args, "sort", 
    "\n     (sort <procedure> <list>)\n     (sort <procedure> <vector>)");
  // return if sorting the empty list
  if(args[1].is_type(types::sym)) return args[1];
  // sort the container
  return primitive_sort_container(args, env);
}

// primitive "sort!" procedure:
data primitive_SORT_BANG(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm has a valid argument signature
  primitive_confirm_sortable_container(args, "sort!", 
    "\n     (sort! <procedure> <list>)\n     (sort! <procedure> <vector>)");
  // return if sorting the empty list (already sorted)
  if(args[1].is_type(types::sym)) return data(types::dne);
  // set the container to its sorted variant
  if(args[1].is_type(types::vec))
    *args[1].value.vec = *primitive_sort_container(args, env).value.vec;
  else
    *args[1].value.par = *primitive_sort_container(args, env).value.par;
  return data(types::dne);
}

// primitive "sorted?" procedure:
data primitive_SORTEDP(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm has a valid argument signature
  primitive_confirm_sortable_container(args, "sorted?", 
    "\n     (sorted? <procedure> <list>)\n     (sorted? <procedure> <vector>)");
  // return if sorting the empty list
  if(args[1].is_type(types::sym)) return args[1];
  // unpack the container
  scm_list container;
  const bool sorting_a_vector = args[1].is_type(types::vec);
  cast_scheme_container_to_ast(sorting_a_vector,args[1],container);
  // confirm the unpacked container is sorted as per the args[0] procedure
  if(container.size() > 1) {
    auto& procedure = args[0].value.exp;
    for(size_type i = 0, n = container.size(); i < n-1; ++i) {
      scm_list args_list({container[i], container[i+1]});
      if(!is_true_scm_condition(procedure,args_list,env))
        return FALSE_DATA_BOOLEAN;
    }
  }
  return TRUE_DATA_BOOLEAN;
}

// primitive "merge" procedure:
data primitive_MERGE(scm_list& args) { 
  constexpr const char * const format = 
    "\n     (merge <procedure> <list1> <list2>)"
    "\n     (merge <procedure> <vector1> <vector2>)";
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given minimum # of args needed
  if(args.size() != 3) 
    THROW_ERR("'merge recieved incorrect # of args (given " 
      << count_args(args) << "):" << format << FCN_ERR("merge",args));
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], "merge", format, args);
  // Confirm given only proper lists or only vectors
  for(size_type i = 1, n = args.size(); i < n; ++i)
    if(scm_list arg_list({args[i]}); 
       !args[i].is_type(types::vec) && !primitive_LISTP(arg_list).value.bol.val)
      THROW_ERR("'merge arg #" << i+1 << ' ' << PROFILE(args[i]) 
        << " isn't a proper list or vector!" << format << FCN_ERR("merge",args));
  // If given '() and a proper list, return the proper list
  if((args[1].is_type(types::sym) && args[2].is_type(types::par)) || 
     (args[2].is_type(types::sym) && args[1].is_type(types::par)))
    return args[1].is_type(types::sym) ? args[2] : args[1];
  // Confirm given containers are either 2 proper lists or 2 vectors
  if(args[1].type != args[2].type)
    THROW_ERR("'merge containers " << PROFILE(args[1]) << " and "
      << PROFILE(args[2]) << "\n     are not both proper lists or both vectors!"
      << format << FCN_ERR("merge",args));
  // If merging vectors: 
  scm_list merged_container;
  if(args[1].is_type(types::vec)) {
    primitive_MERGE_vector_constructor(*args[1].value.vec,*args[2].value.vec, 
                                       args[0].value.exp,merged_container,env);
    return make_vec(merged_container);
  }
  // Else apply the procedure on each list elt & merge args as per the result
  scm_list list_heads(args.begin()+1, args.end());
  primitive_MERGE_list_constructor(list_heads,args[0].value.exp,merged_container,env);
  return primitive_LIST(merged_container); // return a list of the results
}

// primitive "delete-neighbor-dups" procedure:
data primitive_DELETE_NEIGHBOR_DUPS(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm has a valid argument signature
  primitive_confirm_sortable_container(args,"delete-neighbor-dups",
    "\n     (delete-neighbor-dups <binary-equality-procedure> <list>)"
    "\n     (delete-neighbor-dups <binary-equality-procedure> <vector>)");
  // return if deleting duplicates from the empty list
  if(args[1].is_type(types::sym)) return args[1];
  // unpack container
  scm_list container;
  const bool is_vector = args[1].is_type(types::vec);
  cast_scheme_container_to_ast(is_vector,args[1],container);
  // return if deleting duplicates from an empty container
  if(container.empty()) 
    return cast_ast_container_to_scheme(is_vector,container);
  // rm duplicates from the container
  scm_list new_container({container[0]});
  auto& procedure = args[0].value.exp;
  for(size_type i=1, j=0, n = container.size(); i < n; ++i) {
    scm_list args_list({new_container[j],container[i]});
    if(!is_true_scm_condition(procedure,args_list,env))
      new_container.push_back(container[i]), ++j;
  }
  return cast_ast_container_to_scheme(is_vector,new_container);
}

/******************************************************************************
* TYPE-CHECKING PRIMITIVES
******************************************************************************/

// primitive "pair?" procedure:
data primitive_PAIRP(scm_list& args) {
  confirm_given_one_arg(args, "pair?");
  return data(boolean(args[0].is_type(types::par)));
}

// primitive "vector?" procedure:
data primitive_VECTORP(scm_list& args) {
  confirm_given_one_arg(args, "vector?");
  return data(boolean(args[0].is_type(types::vec)));
}

// primitive "char?" procedure:
data primitive_CHARP(scm_list& args) {
  confirm_given_one_arg(args, "char?");
  return data(boolean(args[0].is_type(types::chr)));
}

// primitive "number?" procedure:
data primitive_NUMBERP(scm_list& args) {
  confirm_given_one_arg(args, "number?");
  return data(boolean(args[0].is_type(types::num)));
}

// primitive "real?" procedure:
// => "real" denotes a # that's neither NaN nor Inf
data primitive_REALP(scm_list& args) {
  confirm_given_one_arg(args, "real?");
  return data(boolean(
    args[0].is_type(types::num) && args[0].value.num.is_real()));
}

// primitive "rational?" procedure:
// => "rational" denotes a # that inexact->exact won't approximate
data primitive_RATIONALP(scm_list& args) {
  confirm_given_one_arg(args, "rational?");
  return data(boolean(
    args[0].is_type(types::num) && args[0].value.num.is_rational()));
}

// primitive "string?" procedure:
data primitive_STRINGP(scm_list& args) {
  confirm_given_one_arg(args, "string?");
  return data(boolean(args[0].is_type(types::str)));
}

// primitive "symbol?" procedure:
data primitive_SYMBOLP(scm_list& args) {
  confirm_given_one_arg(args, "symbol?");
  return data(boolean(args[0].is_type(types::sym) && 
                      args[0].value.sym != THE_EMPTY_LIST));
}

// primitive "boolean?" procedure:
data primitive_BOOLEANP(scm_list& args) {
  confirm_given_one_arg(args, "boolean?");
  return data(boolean(args[0].is_type(types::bol)));
}

// primitive "atom?" procedure:
data primitive_ATOMP(scm_list& args) {
  confirm_given_one_arg(args, "atom?");
  return data(boolean(!primitive_PAIRP(args).value.bol.val));
}

// primitive "procedure?" procedure:
data primitive_PROCEDUREP(scm_list& args) {
  confirm_given_one_arg(args, "procedure?");
  bool is_procedure = args[0].is_type(types::exp) &&
                      args[0].value.exp[0].is_type(types::sym) &&
                      (args[0].value.exp[0].value.sym == PROCEDURE_TAG || 
                       args[0].value.exp[0].value.sym == PRIMITIVE_TAG);
  return data(boolean(is_procedure));
}

// primitive "input-port?" procedure:
data primitive_INPUT_PORTP(scm_list& args) {
  confirm_given_one_arg(args, "input-port?");
  return data(boolean(args[0].is_type(types::fip)));
}

// primitive "output-port?" procedure:
data primitive_OUTPUT_PORTP(scm_list& args) {
  confirm_given_one_arg(args, "output-port?");
  return data(boolean(args[0].is_type(types::fop)));
}

// primitive "eof-object?" procedure:
data primitive_EOF_OBJECTP(scm_list& args) {
  confirm_given_one_arg(args, "eof-object?");
  return data(boolean(args[0].is_type(types::chr) && args[0].value.chr==EOF));
}

// primitive "stream-pair?" procedure:
data primitive_STREAM_PAIRP(scm_list& args) {
  confirm_given_one_arg(args, "stream-pair?");
  return data(boolean(args[0].is_type(types::par) && 
                      data_is_a_delay(args[0].value.par->first) && 
                      data_is_a_delay(args[0].value.par->second)));
}

// primitive "stream-null?" procedure:
data primitive_STREAM_NULLP(scm_list& args) {
  confirm_given_one_arg(args, "stream-null?");
  return data(boolean(data_is_the_empty_expression(args[0])));
}

// primitive "stream?" procedure:
data primitive_STREAMP(scm_list& args) {
  confirm_given_one_arg(args, "stream?");
  return data(boolean(data_is_the_empty_expression(args[0]) || 
                      (args[0].is_type(types::par) && 
                       data_is_a_delay(args[0].value.par->first) && 
                       data_is_a_delay(args[0].value.par->second))));
}

/******************************************************************************
* EVAL PRIMITIVE
******************************************************************************/

// primitive "eval" procedure:
data primitive_EVAL(scm_list& args) {
  // extract the local environment
  auto local_env = args.rbegin()->value.env;
  args.pop_back();
  // use the initial/local environment if passed 'null-environment or
  //   'local-environment as a 2nd arg
  auto env = GLOBAL_ENVIRONMENT_POINTER;
  if(args.size()==2 && args[1].is_type(types::sym)) {
    if(args[1].value.sym == "null-environment")  
      env = setup_environment(), args.pop_back();
    else if(args[1].value.sym == "local-environment") 
      env = local_env, args.pop_back();
    else
      THROW_ERR("'eval \""<<args[1].value.sym<<"\" isn't an evaluation environment:"
        "\n     (eval <data> <optional-environment>)" 
        "\n     -> Pass 'null-environment to eval in the empty environment!"
        "\n     -> Pass 'local-environment to eval in the local environment!"
        "\n     -> Don't pass an environment to eval in the global environment!" << 
        FCN_ERR("eval", args));
  }
  // confirm the correct # of arguments were passed
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'eval recieved incorrect # of arguments:"
      "\n     (eval <data> <optional-environment>)" << FCN_ERR("eval", args));
  // if arg is self-evaluating, return arg
  if(args[0].is_type(types::num) || args[0].is_type(types::str) || 
     args[0].is_type(types::chr) || args[0].is_type(types::bol)) {
    return args[0];
  }
  // if arg is a symbol, eval the symbol
  if(args[0].is_type(types::sym)) {
    // confirm arg is not '()
    if(args[0].value.sym == THE_EMPTY_LIST) 
      THROW_ERR("'eval can't evaluate '() (nil to eval):"
        "\n     (eval <data> <optional-environment>)" << FCN_ERR("eval", args));
    return data_cast(scm_eval(scm_list({args[0]}),env));
  }
  // else confirm arg is a proper list
  if(!args[0].is_type(types::par))
    THROW_ERR("'eval didn't recieve an evaluable expression:\n     "<<PROFILE(args[0])
      << "\n     (eval <data> <optional-environment>)" << FCN_ERR("eval", args));
  if(scm_list first_arg({args[0]}); !primitive_LISTP(first_arg).value.bol.val)
    THROW_ERR("'eval recieved an improper list: "<<PROFILE(args[0])
      << "\n     (eval <data> <optional-environment>)" << FCN_ERR("eval", args));
  // eval list contents
  scm_list par_as_exp;
  deep_unpack_list_into_exp(args[0], par_as_exp);
  if(evaling_an_argless_procedure(args[0].value.par)) // requires sentinel arg
    par_as_exp = scm_list({data_cast(par_as_exp), symconst::sentinel});
  return data_cast(scm_eval(std::move(par_as_exp),env));
}

/******************************************************************************
* APPLY PRIMITIVE
******************************************************************************/

// primitive "apply" procedure:
data primitive_APPLY(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm the correct # of arguments were passed
  static constexpr const char * const format = "\n     (apply <procedure> <argument-list>)";
  if(args.size() != 2)
    THROW_ERR("'apply recieved incorrect # of arguments:" << format << FCN_ERR("apply",args));
  // confirm 1st arg is a procedure
  primitive_confirm_data_is_a_procedure(args[0], "apply", format, args);
  // confirm 2nd arg is a finite, nul-terminated list
  if(scm_list second_arg({args[1]}); !primitive_LISTP(second_arg).value.bol.val)
    THROW_ERR("'apply arg #2 " << PROFILE(args[0]) << " isn't a proper list!"
      << format << FCN_ERR("apply",args));
  // apply arguments in list to the procedure
  scm_list args_list;
  shallow_unpack_list_into_exp(args[1], args_list);
  return data_cast(execute_application(std::move(args[0].value.exp),args_list,env));
}

/******************************************************************************
* FORCE-DELAY PRIMITIVES
******************************************************************************/

// primitive "delay?" predicate procedure:
data primitive_DELAYP(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'delay? recieved incorrect # of arguments: (delay? <obj>)"
      << FCN_ERR("delay?", args));
  return boolean(data_is_a_delay(args[0]));
}

// primitive "force" procedure:
data primitive_FORCE(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'force recieved incorrect # of arguments:"
      "\n     (force <delayed-expression>)" << FCN_ERR("force", args));
  return force_data_delay(args[0]); // "call-by-need" evaluation
}

/******************************************************************************
* STREAM PRIMITIVES
******************************************************************************/

// primitive "scar" procedure:
data primitive_SCAR(scm_list& args) {
  confirm_given_a_stream_pair_arg(args, "scar", "\n     (scar <stream-pair>)");
  return force_data_delay(args[0].value.par->first);
}

// primitive "scdr" procedure:
data primitive_SCDR(scm_list& args) {
  confirm_given_a_stream_pair_arg(args, "scdr", "\n     (scdr <stream-pair>)");
  data cdr_promise = force_data_delay(args[0].value.par->second);
  if(!data_is_stream(cdr_promise))
    THROW_ERR("'scdr forced cdr " << PROFILE(cdr_promise)
      << " isn't a stream:\n     (scdr <stream-pair>)" << FCN_ERR("scdr", args));
  return cdr_promise;
}

// primitive "stream-length" procedure:
data primitive_STREAM_LENGTH(scm_list& args) {
  static constexpr const char * const format = "\n     (stream-length <stream>)";
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR("'stream-length recieved incorrect # of arguments:"
      << format << FCN_ERR("stream-length", args));
  // Length of '() = 0
  if(data_is_the_empty_expression(args[0])) return num_type();
  // Confirm given a stream pair, if not '()
  confirm_given_a_stream_pair_arg(args, "stream-length", format);
  num_type count;
  primitive_STREAM_LENGTH_computation(get_stream_data_cdr(args[0]),count);
  return count;
}

// -----------------------
// Scar/Scdr Combinations:
// -----------------------

// primitive "scaar" procedure:
data primitive_SCAAR(scm_list& args) {
  static constexpr const char * const name   = "scaar";
  static constexpr const char * const format = "\n     (scaar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_car(std::move(args[0]),name,format,1),name,format,2);
}

// primitive "scadr" procedure:
data primitive_SCADR(scm_list& args) {
  static constexpr const char * const name   = "scadr";
  static constexpr const char * const format = "\n     (scadr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1);
}

// primitive "scdar" procedure:
data primitive_SCDAR(scm_list& args) {
  static constexpr const char * const name   = "scdar";
  static constexpr const char * const format = "\n     (scdar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_car(std::move(args[0]),name,format,1),name,format,1);
}

// primitive "scddr" procedure:
data primitive_SCDDR(scm_list& args) {
  static constexpr const char * const name   = "scddr";
  static constexpr const char * const format = "\n     (scddr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2);
}

// ----------

// primitive "scaaar" procedure:
data primitive_SCAAAR(scm_list& args) {
  static constexpr const char * const name   = "scaaar";
  static constexpr const char * const format = "\n     (scaaar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_car(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,3);
}

// primitive "scaadr" procedure:
data primitive_SCAADR(scm_list& args) {
  static constexpr const char * const name   = "scaadr";
  static constexpr const char * const format = "\n     (scaadr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_car(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2);
}

// primitive "scadar" procedure:
data primitive_SCADAR(scm_list& args) {
  static constexpr const char * const name   = "scadar";
  static constexpr const char * const format = "\n     (scadar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_cdr(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2);
}

// primitive "scaddr" procedure:
data primitive_SCADDR(scm_list& args) {
  static constexpr const char * const name   = "scaddr";
  static constexpr const char * const format = "\n     (scaddr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_cdr(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,1);
}

// primitive "scdaar" procedure:
data primitive_SCDAAR(scm_list& args) {
  static constexpr const char * const name   = "scdaar";
  static constexpr const char * const format = "\n     (scdaar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_car(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,1);
}

// primitive "scdadr" procedure:
data primitive_SCDADR(scm_list& args) {
  static constexpr const char * const name   = "scdadr";
  static constexpr const char * const format = "\n     (scdadr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_car(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2);
}

// primitive "scddar" procedure:
data primitive_SCDDAR(scm_list& args) {
  static constexpr const char * const name   = "scddar";
  static constexpr const char * const format = "\n     (scddar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_cdr(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2);
}

// primitive "scdddr" procedure:
data primitive_SCDDDR(scm_list& args) {
  static constexpr const char * const name   = "scdddr";
  static constexpr const char * const format = "\n     (scdddr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_cdr(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,3);
}

// ----------

// primitive "scaaaar" procedure:
data primitive_SCAAAAR(scm_list& args) {
  static constexpr const char * const name   = "scaaaar";
  static constexpr const char * const format = "\n     (scaaaar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_car(get_stream_data_car(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,3),name,format,4);
}

// primitive "scaaadr" procedure:
data primitive_SCAAADR(scm_list& args) {
  static constexpr const char * const name   = "scaaadr";
  static constexpr const char * const format = "\n     (scaaadr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_car(get_stream_data_car(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,3);
}

// primitive "scaadar" procedure:
data primitive_SCAADAR(scm_list& args) {
  static constexpr const char * const name   = "scaadar";
  static constexpr const char * const format = "\n     (scaadar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_car(get_stream_data_cdr(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,3);
}

// primitive "scaaddr" procedure:
data primitive_SCAADDR(scm_list& args) {
  static constexpr const char * const name   = "scaaddr";
  static constexpr const char * const format = "\n     (scaaddr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_car(get_stream_data_cdr(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,1),name,format,2);
}

// primitive "scadaar" procedure:
data primitive_SCADAAR(scm_list& args) {
  static constexpr const char * const name   = "scadaar";
  static constexpr const char * const format = "\n     (scadaar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_cdr(get_stream_data_car(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,1),name,format,3);
}

// primitive "scadadr" procedure:
data primitive_SCADADR(scm_list& args) {
  static constexpr const char * const name   = "scadadr";
  static constexpr const char * const format = "\n     (scadadr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_cdr(get_stream_data_car(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,2);
}

// primitive "scaddar" procedure:
data primitive_SCADDAR(scm_list& args) {
  static constexpr const char * const name   = "scaddar";
  static constexpr const char * const format = "\n     (scaddar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_cdr(get_stream_data_cdr(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,2);
}

// primitive "scadddr" procedure:
data primitive_SCADDDR(scm_list& args) {
  static constexpr const char * const name   = "scadddr";
  static constexpr const char * const format = "\n     (scadddr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_car(get_stream_data_cdr(get_stream_data_cdr(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,3),name,format,1);
}


// primitive "scdaaar" procedure:
data primitive_SCDAAAR(scm_list& args) {
  static constexpr const char * const name   = "scdaaar";
  static constexpr const char * const format = "\n     (scdaaar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_car(get_stream_data_car(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,3),name,format,1);
}

// primitive "scdaadr" procedure:
data primitive_SCDAADR(scm_list& args) {
  static constexpr const char * const name   = "scdaadr";
  static constexpr const char * const format = "\n     (scdaadr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_car(get_stream_data_car(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,2);
}

// primitive "scdadar" procedure:
data primitive_SCDADAR(scm_list& args) {
  static constexpr const char * const name   = "scdadar";
  static constexpr const char * const format = "\n     (scdadar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_car(get_stream_data_cdr(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,2);
}

// primitive "scdaddr" procedure:
data primitive_SCDADDR(scm_list& args) {
  static constexpr const char * const name   = "scdaddr";
  static constexpr const char * const format = "\n     (scdaddr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_car(get_stream_data_cdr(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,1),name,format,3);
}

// primitive "scddaar" procedure:
data primitive_SCDDAAR(scm_list& args) {
  static constexpr const char * const name   = "scddaar";
  static constexpr const char * const format = "\n     (scddaar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_cdr(get_stream_data_car(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,2),name,format,1),name,format,2);
}

// primitive "scddadr" procedure:
data primitive_SCDDADR(scm_list& args) {
  static constexpr const char * const name   = "scddadr";
  static constexpr const char * const format = "\n     (scddadr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_cdr(get_stream_data_car(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,3);
}

// primitive "scdddar" procedure:
data primitive_SCDDDAR(scm_list& args) {
  static constexpr const char * const name   = "scdddar";
  static constexpr const char * const format = "\n     (scdddar <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_cdr(get_stream_data_cdr(
          get_stream_data_car(std::move(args[0]),name,format,1),name,format,1),name,format,2),name,format,3);
}

// primitive "scddddr" procedure:
data primitive_SCDDDDR(scm_list& args) {
  static constexpr const char * const name   = "scddddr";
  static constexpr const char * const format = "\n     (scddddr <stream-pair>)";
  confirm_given_a_stream_pair_arg(args,name,format);
  return get_stream_data_cdr(get_stream_data_cdr(get_stream_data_cdr(
          get_stream_data_cdr(std::move(args[0]),name,format,1),name,format,2),name,format,3),name,format,4);
}

// ------------------------
// Stream Control Features:
// ------------------------

// primitive "stream-for-each" procedure:
data primitive_STREAM_FOR_EACH(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given minimum # of args needed
  static constexpr const char * const format = 
    "\n     (stream-for-each <procedure> <stream1> <stream2> ...)";
  if(args.size() < 2) 
    THROW_ERR("'stream-for-each recieved insufficient args (only "
      << count_args(args) << "):" << format << FCN_ERR("stream-for-each", args));
  // Confirm given a procedure
  primitive_confirm_data_is_a_procedure(args[0], "stream-for-each", format, args);
  // Confirm only given streams
  scm_list stream_heads(args.begin()+1, args.end());
  primitive_confirm_only_given_streams(stream_heads,"stream-for-each",format,1,args);
  // Apply the procedure on each elt of each stream
  primitive_STREAM_FOR_EACH_applicator(stream_heads, args[0].value.exp, env);
  return data(types::dne);
}

// primitive "stream-ref" procedure:
data primitive_STREAM_REF(scm_list& args) {
  // Confirm appropriate # of args given
  static constexpr const char * const format = 
    "\n     (stream-ref <stream-pair> <index>)";
  if(args.size() != 2) 
    THROW_ERR("'stream-ref recieved incorrect # of args (given "
      << count_args(args) << "):" << format << FCN_ERR("stream-ref", args));
  // Confirm given a stream-pair
  if(!data_is_stream_pair(args[0]))
    THROW_ERR("'stream-ref " << PROFILE(args[0]) << " isn't a stream-pair!"
      << format << FCN_ERR("stream-ref", args));
  // Confirm given a valid index
  if(!primitive_is_valid_index(args[1]))
    THROW_ERR("'stream-ref " << PROFILE(args[1]) << " isn't a valid index!"
      << format << FCN_ERR("stream-ref", args));
  // Get the stream's index'th item
  const size_type n = (size_type)args[1].value.num.extract_inexact();
  if(!n) return get_stream_data_car(args[0]);
  data item = primitive_REF_DROP_SUBSTREAM_seeker(get_stream_data_cdr(args[0]), 
                                                  n, "stream-ref", format);
  if(!data_is_stream_pair(item))
    THROW_ERR("'stream-ref index " << n << " is out of range!"
      << format << FCN_ERR("stream-ref", args));
  return get_stream_data_car(item);
}

// primitive "stream-drop" procedure:
data primitive_STREAM_DROP(scm_list& args) {
  // Confirm appropriate # of args given
  static constexpr const char * const format = "\n     (stream-drop <n> <stream>)";
  primitive_TEMPLATE_TAKE_DROP_VALIDATION(args, "stream-drop", format);
  // Get the a substream after dropping 'size' items from given stream
  if(data_is_the_empty_expression(args[1])) return args[1];
  const size_type n = (size_type)args[0].value.num.extract_inexact();
  if(!n)     return args[1];
  if(n == 1) return get_stream_data_cdr(args[1]);
  return primitive_REF_DROP_SUBSTREAM_seeker(get_stream_data_cdr(args[1]), 
                                             n, "stream-drop", format, 1, false);
}

// primitive "stream-drop-while" procedure:
data primitive_STREAM_DROP_WHILE(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm appropriate # of args given
  static constexpr const char * const format = "\n     (stream-drop-while <predicate> <stream>)";
  primitive_TEMPLATE_TAKE_DROP_WHILE_VALIDATION(args, "stream-drop-while", format);
  // Get keep dropping items while 'predicate' is true, then return result
  if(data_is_the_empty_expression(args[1])) return args[1];
  return primitive_DROP_WHILE_ctor(std::move(args[1]), args[0].value.exp, 
                                   env, "stream-drop-while", format);
}

// primitive "stream-take" procedure:
data primitive_STREAM_TAKE(scm_list& args){
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm appropriate # of args given
  static constexpr const char * const format = "\n     (stream-take <n> <stream>)";
  primitive_TEMPLATE_TAKE_DROP_VALIDATION(args, "stream-take", format);
  // Get the a substream after dropping 'size' items from given stream
  if(data_is_the_empty_expression(args[1])) return args[1];
  const size_type n = (size_type)args[0].value.num.extract_inexact();
  if(!n) return data(symconst::emptylist);
  scm_list substream;
  primitive_TAKE_SUBSTREAM_seeker(std::move(args[1]),n,substream,"stream-take",format);
  return primitive_STREAM_to_SCONS_constructor(substream.begin(),substream.end(),env);
}

// primitive "stream-take-while" procedure:
data primitive_STREAM_TAKE_WHILE(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm appropriate # of args given
  static constexpr const char * const format = "\n     (stream-take-while <predicate> <stream>)";
  primitive_TEMPLATE_TAKE_DROP_WHILE_VALIDATION(args, "stream-take-while", format);
  // Get keep dropping items while 'predicate' is true, then return result
  if(data_is_the_empty_expression(args[1])) return args[1];
  scm_list substream;
  primitive_TAKE_WHILE_ctor(std::move(args[1]), args[0].value.exp, substream,
                            env, "stream-take-while", format);
  if(substream.empty()) return data(symconst::emptylist);
  return primitive_STREAM_to_SCONS_constructor(substream.begin(),substream.end(),env);
}

// primitive "stream-reverse" procedure:
data primitive_STREAM_REVERSE(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given a single stream arg
  static constexpr const char * const format = "\n     (stream-reverse <stream>)";
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'stream-reverse recieved incorrect # of args (given "
      << count_args(args) << "):" << format << FCN_ERR("stream-reverse",args));
  if(!data_is_stream(args[0]))
    THROW_ERR("'stream-reverse "<<PROFILE(args[0])<<" isn't a stream:" 
      << format << FCN_ERR("stream-reverse",args));
  // Convert stream to a scm_list, reverse, & revert to a stream
  if(data_is_the_empty_expression(args[0])) return args[0];
  scm_list stream_as_exp;
  unpack_stream_into_exp(std::move(args[0]), stream_as_exp);
  std::reverse(stream_as_exp.begin(),stream_as_exp.end());
  return primitive_STREAM_to_SCONS_constructor(stream_as_exp.begin(),stream_as_exp.end(),env);
}

// primitive "stream-fold-left" procedure:
data primitive_STREAM_FOLD_LEFT(scm_list& args) {
  return primitive_STREAM_FOLD_template(args, "stream-fold-left", 
          "\n     (stream-fold-left <procedure> <seed> <stream>)", true);
}

// primitive "stream-fold-right" procedure:
data primitive_STREAM_FOLD_RIGHT(scm_list& args) {
  return primitive_STREAM_FOLD_template(args, "stream-fold-right", 
          "\n     (stream-fold-right <procedure> <seed> <stream>)", false);
}

// primitive "stream->list" procedure:
data primitive_CONVERT_STREAM_LIST(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given proper args (same signature as stream-drop & stream-take)
  primitive_TEMPLATE_TAKE_DROP_VALIDATION(args, "stream->list", 
                                          "\n     (stream->list <size> <stream>)");
  // Invoke stream-take, convert substream -> exp -> list
  if(data_is_the_empty_expression(args[0])) return args[0];
  args.push_back(env); // reinsert env for stream-take
  auto substream = primitive_STREAM_TAKE(args);
  scm_list stream_as_exp;
  unpack_stream_into_exp(std::move(substream), stream_as_exp);
  return primitive_LIST(stream_as_exp);
}

// primitive "list->stream" procedure:
data primitive_CONVERT_LIST_STREAM(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm given a single proper list arg
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'list->stream recieved incorrect # of args (given " << count_args(args) 
      << "):\n     (list->stream <size> <list>)" << FCN_ERR("list->stream",args));
  if(!primitive_LISTP(args).value.bol.val)
    THROW_ERR("'list->stream "<<PROFILE(args[0])<<" isn't a proper list:" 
      "\n     (list->stream <size> <list>)" << FCN_ERR("list->stream",args));
  // Convert list -> exp -> stream
  if(data_is_the_empty_expression(args[0])) return args[0];
  scm_list par_as_exp;
  shallow_unpack_list_into_exp(args[0], par_as_exp);
  if(par_as_exp.empty()) return data(symconst::emptylist);
  return primitive_STREAM_to_SCONS_constructor(par_as_exp.begin(),par_as_exp.end(),env);
}

// -----------------------------------------------------------
// Infinite Stream Handler Primitives Defined In Heist Scheme:
// -----------------------------------------------------------

// primitive "stream-map" procedure:
constexpr const char * const primitive_HEIST_STREAM_MAP = R"(
(define (stream-map proc . streams)
  (if (not (procedure? proc))
      (error 'stream-map 
              "Arg #1 isn't a procedure!\n          (stream-map <procedure> <stream1> <stream2> ...)\n         "
              proc))
  (for-each (lambda (s) 
              (if (not (stream? s)) 
                  (error 'stream-map
                         "Recieved a non stream!\n          (stream-map <procedure> <stream1> <stream2> ...)\n         " 
                         s)))
            streams)

  (define (stream-map streams)
    (if (stream-null? (car streams))
        stream-null
        (scons
          (apply proc (map scar streams))
          (stream-map (map scdr streams)))))

  (stream-map streams))
)";

// primitive "stream-filter" procedure:
constexpr const char * const primitive_HEIST_STREAM_FILTER = R"(
(define (stream-filter pred? s)
  (if (not (procedure? pred?))
      (error 'stream-filter 
             "Arg #1 isn't a procedure!\n          (stream-filter <predicate> <stream>)\n         " 
             pred?))
  (if (not (stream? s)) 
      (error 'stream-filter 
             "Arg #2 isn't a stream!\n          (stream-filter <predicate> <stream>)\n         " 
             s))

  (define (stream-filter s)
    (if (stream-null? s) 
        stream-null
        (if (pred? (scar s))
            (scons (scar s) (stream-filter (scdr s)))
            (stream-filter (scdr s)))))

  (stream-filter s))
)";

// primitive "stream-from" procedure:
constexpr const char * const primitive_HEIST_STREAM_FROM = R"(
(define (stream-from first . step)
  (define inc-proc 0)
  (if (not (number? first))
      (error 'stream-from 
             "Arg #1 isn't a number!\n          (stream-from <seed-number> <optional-step>)\n         " 
             first))
  (if (> (length step) 1)
      (error 'stream-from 
             "Recieved more than 1 step!\n          (stream-from <seed-number> <optional-step>)\n         " 
             step))
  (if (null? step)
      (set! inc-proc (lambda (n) (+ n 1)))
      (if (number? (car step))
          (begin (define step-val (car step)) 
                 (set! inc-proc (lambda (n) (+ n step-val))))
          (error 'stream-from 
                 "Step (arg #2) isn't a number!\n          (stream-from <seed-number> <optional-step>)\n         " 
                 (car step))))

  (define (stream-from n)
    (scons n (stream-from (inc-proc n))))

  (stream-from first))
)";

// primitive "stream-unfold" procedure:
constexpr const char * const primitive_HEIST_STREAM_UNFOLD = R"(
(define (stream-unfold break-cond map-proc inc-proc seed)
  (if (not (procedure? break-cond))
      (error 'stream-unfold 
             "Arg #1 isn't a procedure!\n          (stream-unfold <break-condition> <map-procdure> <increment-procedure> <seed>)\n         " 
             break-cond))
  (if (not (procedure? map-proc))
      (error 'stream-unfold 
             "Arg #2 isn't a procedure!\n          (stream-unfold <break-condition> <map-procdure> <increment-procedure> <seed>)\n         " 
             map-proc))
  (if (not (procedure? inc-proc))
      (error 'stream-unfold 
             "Arg #3 isn't a procedure!\n          (stream-unfold <break-condition> <map-procdure> <increment-procedure> <seed>)\n         " 
             inc-proc))
  
  (define (stream-unfold seed)
    (if (break-cond seed)
        stream-null
        (scons (map-proc seed) (stream-unfold (inc-proc seed)))))

  (stream-unfold seed))
)";


// primitive "stream-iterate" procedure:
constexpr const char * const primitive_HEIST_STREAM_ITERATE = R"(
(define (stream-iterate inc-proc seed)
  (if (not (procedure? inc-proc))
      (error 'stream-iterate 
             "Arg #1 isn't a procedure!\n          (stream-iterate <increment-procedure> <seed>)\n         " 
             inc-proc))
  
  (define (stream-iterate seed)
    (scons seed (stream-iterate (inc-proc seed))))

  (stream-iterate seed))
)";

// primitive "stream-zip" procedure:
constexpr const char * const primitive_HEIST_STREAM_ZIP = R"(
(define (stream-zip . streams)
  (for-each (lambda (s) 
              (if (not (stream? s)) 
                  (error 'stream-zip 
                         "Recieved a non stream!\n          (stream-zip <stream1> <stream2> ...)\n         " 
                         s)))
            streams)
  
  (apply stream-map (cons (lambda (. l) l) streams)))
)";

// primitive "stream-constant" procedure:
constexpr const char * const primitive_HEIST_STREAM_CONSTANT = R"(
(define (stream-constant . objs)
  (define (stream-constant obj-list)
    (if (null? obj-list)
        (stream-constant objs)
        (scons (car obj-list) (stream-constant (cdr obj-list)))))

  (if (null? objs)
      stream-null
      (stream-constant objs)))
)";

// primitive "stream-append" procedure:
constexpr const char * const primitive_HEIST_STREAM_APPEND = R"(
(define (stream-append s . streams)
  (for-each (lambda (s) 
              (if (not (stream? s)) 
                  (error 'stream-append 
                         "Recieved a non stream!\n          (stream-append <stream1> <stream2> ...)\n         " 
                         s)))
            (cons s streams))

  (define (stream-append s streams)
    (if (null? s)
        (if (null? streams)
            stream-null
            (stream-append (car streams) (cdr streams)))
        (scons (scar s) (stream-append (scdr s) streams))))

  (if (null? streams)
      s
      (stream-append s streams)))
)";

// primitive "stream-interleave" procedure:
constexpr const char * const primitive_HEIST_STREAM_INTERLEAVE = R"(
(define (stream-interleave stream1 stream2)
  (if (not (stream? stream1))
      (error 'stream-interleave 
             "Arg #1 isn't a stream!\n          (stream-interleave <stream1> <stream2>)\n         " 
             stream1))
  (if (not (stream? stream2))
      (error 'stream-interleave 
             "Arg #2 isn't a stream!\n          (stream-interleave <stream2> <stream2>)\n         " 
             stream2))
  
  (define (stream-interleave stream1 stream2)
    (if (stream-null? stream1)
        stream2
        (scons (scar stream1) (stream-interleave stream2 (scdr stream1)))))

  (stream-interleave stream1 stream2))
)";

/******************************************************************************
* TYPE COERCION PRIMITIVES
******************************************************************************/

// primitive "char->integer" procedure:
data primitive_COERCE_CHAR_TO_INTEGER(scm_list& args) {
  confirm_given_one_char_arg(args, "char->integer");
  return num_type(int(args[0].value.chr));
}

// primitive "integer->char" procedure:
data primitive_COERCE_INTEGER_TO_CHAR(scm_list& args) {
  static constexpr const char * const format = 
    "\n     (integer->char <non_negative-integer>)"
    "\n     <non_negative-integer> range: [0,255]";
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR("'integer->char didn't recieve 1 integer arg:" << format 
      << FCN_ERR("integer->char",args));
  if(!args[0].is_type(types::num) || !args[0].value.num.is_integer())
    THROW_ERR("'integer->char didn't recieve an integer arg:"
      "\n     Received arg " << PROFILE(args[0]) << format 
      << FCN_ERR("integer->char",args));
  if((args[0].value.num.is_neg() || args[0].value.num > 255) && 
     args[0].value.num != EOF)
    THROW_ERR("'integer->char " << PROFILE(args[0]) << " isn't a"
      "\n     positive integer ranging from 0 to 255!" << format 
      << FCN_ERR("integer->char",args));
  return chr_type(args[0].value.num.extract_inexact());
}

// primitive "number->string" procedure:
data primitive_COERCE_NUMBER_TO_STRING(scm_list& args) {
  if(args.size() > 2 || no_args_given(args))
    THROW_ERR("'number->string recieved incorrect # of arguments:"
      "\n     (number->string <number> <optional-numeric-radix>)"
      << FCN_ERR("number->string", args));
  // no number or invalid radix given
  if(!args[0].is_type(types::num) || 
      (args.size() == 2 && 
        (!args[1].is_type(types::num) || !args[1].value.num.is_integer()))) 
    return FALSE_DATA_BOOLEAN;
  // given a radix
  if(args.size() == 2) {
    size_type radix = args[1].value.num.round().extract_inexact();
    if(radix < 2 || radix > 36)
      THROW_ERR("'number->string radix (given "<<radix<<") can only range from 2-36:"
        "\n     (number->string <number> <optional-numeric-radix>)"
        << FCN_ERR("number->string", args));
    if(radix != 10)
      return make_str(args[0].value.num.cio_str(radix));
  }
  return make_str(args[0].value.num.cio_str());
}

// primitive "string->number" procedure:
data primitive_COERCE_STRING_TO_NUMBER(scm_list& args) {
  if(args.size() > 2 || no_args_given(args))
    THROW_ERR("'string->number recieved incorrect # of arguments!"
      "\n     (string->number <string> <optional-numeric-radix>)"
      << FCN_ERR("string->number", args));
  // no string or invalid radix given
  if(!args[0].is_type(types::str) || 
      (args.size() == 2 && 
        (!args[1].is_type(types::num) || !args[1].value.num.is_integer()))) 
    return FALSE_DATA_BOOLEAN;
  // given a radix
  if(args.size() == 2) {
    size_type radix = args[1].value.num.round().extract_inexact();
    if(radix < 2 || radix > 36)
      THROW_ERR("'string->number radix (given "<<radix<<") can only range from 2-36!"
        "\n     (string->number <string> <optional-numeric-radix>)"
        << FCN_ERR("string->number", args));
    if(radix != 10) {
      try {
        return data(num_type(*args[0].value.str, radix));
      } catch(const num_type::error_t& err) {
        return FALSE_DATA_BOOLEAN;
      }
    }
  }
  try {
    return data(num_type(*args[0].value.str));
  } catch(const num_type::error_t& err) {
    return FALSE_DATA_BOOLEAN;
  }
}

// primitive "symbol->string" procedure:
data primitive_COERCE_SYMBOL_TO_STRING(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'symbol->string recieved incorrect # of arguments:"
      "\n     (symbol->string <symbol>)" << FCN_ERR("symbol->string", args));
  if(!args[0].is_type(types::sym))
    return FALSE_DATA_BOOLEAN;

  return make_str(convert_symbol_to_string(args[0].value.sym));
}

// primitive "string->symbol" procedure:
data primitive_COERCE_STRING_TO_SYMBOL(scm_list& args) {
  if(args.size() != 1 || no_args_given(args))
    THROW_ERR("'string->symbol recieved incorrect # of arguments:"
      "\n     (string->symbol <string>)" << FCN_ERR("string->symbol", args));
  if(!args[0].is_type(types::str))
    return FALSE_DATA_BOOLEAN;
  return data(convert_string_to_symbol(*args[0].value.str)); 
}

// primitive "vector->list" procedure:
data primitive_COERCE_VECTOR_TO_LIST(scm_list& args) {
  primitive_confirm_valid_vector_arg(args, 1, "vector->list", "(vector->list <vector>)");
  return primitive_LIST(*args[0].value.vec);
}

// primitive "list->vector" procedure:
data primitive_COERCE_LIST_TO_VECTOR(scm_list& args) {
  if(primitive_validate_list_and_return_if_empty(args, "list->vector"))
    return make_vec(scm_list{});
  data new_vec(make_vec(scm_list{}));
  shallow_unpack_list_into_exp(args[0], *new_vec.value.vec);
  return new_vec;
}

// primitive "string->list" procedure:
data primitive_STRING_TO_LIST(scm_list& args) {
  primitive_confirm_valid_string_arg(args, 1, "string->list", "(string->list <string>)");
  scm_list char_list;
  for(const auto& ch : *args[0].value.str)
    char_list.push_back(ch);
  return primitive_LIST(char_list);
}

// primitive "list->string" procedure:
data primitive_LIST_TO_STRING(scm_list& args) {
  if(primitive_validate_list_and_return_if_empty(args, "list->string"))
    return make_str("");
  scm_list char_list;
  scm_string char_str;
  shallow_unpack_list_into_exp(args[0], char_list);
  for(const auto ch : char_list) {
    if(!ch.is_type(types::chr))
      THROW_ERR("'list->string list obj "<<PROFILE(ch)<<" isn't a character!"
        "\n     (list->string <char-list>)"<<FCN_ERR("list->string",args));
    else
      char_str += ch.value.chr;
  }
  return make_str(char_str);
}

/******************************************************************************
* OUTPUT PRIMITIVES
******************************************************************************/

data primitive_WRITE(scm_list& args) {
  FILE* outs = CURRENT_OUTPUT_PORT;
  confirm_valid_output_args(args, outs, 1, "write", 
                            "\n     (write <obj> <optional-open-output-port>)");
  if(!args[0].is_type(types::dne)) {
    fputs(args[0].cio_str().c_str(), outs);
    fflush(outs);
  }
  LAST_PRINTED_TO_STDOUT = (outs == stdout);
  return data(types::dne);
}

data primitive_NEWLINE(scm_list& args) {
  FILE* outs = CURRENT_OUTPUT_PORT;
  confirm_valid_output_args(args, outs, 0, "newline", 
                            "\n     (newline <optional-open-output-port>)");
  fputc('\n', outs);
  fflush(outs);
  LAST_PRINTED_NEWLINE_TO_STDOUT = LAST_PRINTED_TO_STDOUT = (outs == stdout);
  return data(types::dne);
}

data primitive_DISPLAY(scm_list& args) {
  FILE* outs = CURRENT_OUTPUT_PORT;
  confirm_valid_output_args(args, outs, 1, "display", 
                            "\n     (display <obj> <optional-open-output-port>)");
  if(args[0].is_type(types::chr)) {
    fputc(args[0].value.chr, outs);
    if(args[0].value.chr == '\n') // account for printing a newline
      LAST_PRINTED_NEWLINE_TO_STDOUT = (outs == stdout);
  } else if(args[0].is_type(types::str)) {
    // rm extra escaping '\' chars
    scm_string str(*args[0].value.str);
    if(str.empty()) return data(types::dne);
    unescape_chars(str);          // cook the raw string
    fputs(str.c_str(), outs);
    if(*str.rbegin() == '\n')     // account for printed newlines
      LAST_PRINTED_NEWLINE_TO_STDOUT = (outs == stdout);
  } else {
    if(args[0].is_type(types::dne)) return data(types::dne);
    fputs(args[0].cio_str().c_str(), outs);
  }
  fflush(outs);
  LAST_PRINTED_TO_STDOUT = (outs == stdout);
  return data(types::dne);
}

data primitive_WRITE_CHAR(scm_list& args) {
  FILE* outs = CURRENT_OUTPUT_PORT;
  confirm_valid_output_args(args, outs, 1, "write-char", 
                            "\n     (write-char <char> <optional-open-output-port>)");
  // confirm given a character
  if(!args[0].is_type(types::chr))
    THROW_ERR("'write-char arg "<<PROFILE(args[0])<<" isn't a character:" 
      "\n     (write-char <char> <optional-open-output-port>)"
      << FCN_ERR("write-char", args));
  fputc(args[0].value.chr, outs);
  fflush(outs);
  LAST_PRINTED_TO_STDOUT = (outs == stdout);
  return data(types::dne);
}

/******************************************************************************
* INPUT PRIMITIVES
******************************************************************************/

data primitive_READ(scm_list& args) {
  // Extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // Confirm either given an open input port or no args
  FILE* outs = CURRENT_OUTPUT_PORT, *ins = CURRENT_INPUT_PORT;
  bool reading_stdin = (CURRENT_INPUT_PORT == stdin);
  if(!confirm_valid_input_args_and_non_EOF(args, ins, "read", reading_stdin)) 
    return chr_type(EOF);
  // Read input
  if(reading_stdin)
    return data_cast(scm_eval(scm_list({
      symconst::quote, read_user_input(outs,ins,false)[0]
    }), env));
  return data_cast(scm_eval(scm_list({
    symconst::quote, primitive_read_from_port(outs,ins)[0]
  }), env));
}

data primitive_READ_STRING(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // return AST if successfully parsed an expression
  confirm_given_one_string_arg(args,"read-string","\n     (read-string <string>)");
  try {
    scm_list read_data;
    parse_input_exp(std::move(*args[0].value.str),read_data);
    if(read_data.empty()) return symconst::emptylist;
    return data_cast(scm_eval(scm_list({symconst::quote,read_data[0]}),env));
  // throw error otherwise & return void data
  } catch(const READER_ERROR& read_error) {
    if(is_non_repl_reader_error(read_error))
      alert_non_repl_reader_error(CURRENT_OUTPUT_PORT,read_error,*args[0].value.str);
    else
      alert_reader_error(CURRENT_OUTPUT_PORT,read_error,*args[0].value.str);
  } catch(const size_type& read_error_index) {
    alert_reader_error(CURRENT_OUTPUT_PORT,read_error_index,*args[0].value.str);
  }
  fflush(CURRENT_OUTPUT_PORT);
  return data(types::dne);
}

data primitive_READ_CHAR(scm_list& args) {
  // Confirm either given an open input port or no args
  FILE* outs = CURRENT_OUTPUT_PORT, *ins = CURRENT_INPUT_PORT;
  bool reading_stdin = (CURRENT_INPUT_PORT == stdin);
  if(!confirm_valid_input_args_and_non_EOF(args, ins, "read-char", reading_stdin)) 
    return chr_type(EOF);
  // Read a char from a port as needed
  fflush(outs);
  if(!reading_stdin) return data(chr_type(getc(ins)));
  // Else read 1 char from stdin & throw away the rest of the line
  fputs("  ", stdout);
  int ch = getc(stdin);
  if(ch!='\n') while(getc(stdin) != '\n'); // eat rest of the line
  return data(chr_type(ch));
}

data primitive_PEEK_CHAR(scm_list& args) {
  // Confirm either given an open input port or no args
  FILE* outs = CURRENT_OUTPUT_PORT, *ins = CURRENT_INPUT_PORT;
  bool reading_stdin = (CURRENT_INPUT_PORT == stdin);
  if(!confirm_valid_input_args_and_non_EOF(args, ins, "peek-char", reading_stdin)) 
    return chr_type(EOF);
  // Peek a char from a port as needed
  fflush(outs);
  if(!reading_stdin) {
    int ch = getc(ins);
    ungetc(ch, ins);
    return data(chr_type(ch));
  }
  // Else peek 1 char from stdin & throw away the rest of the line
  // NOTE: 'peek-char' from stdin is equivalent to 'read-char' from stdin since
  //       both return 1 char from the stream & throw away the rest of the line
  fputs("  ", stdout);
  int ch = getc(stdin);
  if(ch!='\n') while(getc(stdin) != '\n'); // eat rest of the line
  return data(chr_type(ch));
}

data primitive_CHAR_READYP(scm_list& args) {
  // Confirm either given an open input port or no args
  FILE* ins = CURRENT_INPUT_PORT;
  bool reading_stdin = (CURRENT_INPUT_PORT == stdin);
  if(!confirm_valid_input_args_and_non_EOF(args, ins, "char-ready?", reading_stdin)) 
    return data(boolean(false));
  // Stdin is always flushed, hence a char will never be waiting in its buffer
  if(reading_stdin) return data(boolean(false));
  // Peek the non-stdin port for a non-EOF character
  int ch = getc(ins);
  ungetc(ch, ins);
  return data(boolean(ch != EOF));
}

/******************************************************************************
* PORT PRIMITIVES
******************************************************************************/

// file & ports predicates
data primitive_FILEP(scm_list& args) {
  confirm_given_one_string_arg(args, "file?", "(file? <potential-filename-string>)");
  return data(boolean(confirm_file_exists(args[0].value.str->c_str())));
}

data primitive_OPEN_PORTP(scm_list& args) {
  confirm_valid_port_predicate_arg(args,"open-port?","(open-port? <port>)");
  if(args[0].is_type(types::fip))
    return data(boolean(args[0].value.fip.is_open()));
  return data(boolean(args[0].value.fop.is_open()));
}

data primitive_CLOSED_PORTP(scm_list& args) {
  confirm_valid_port_predicate_arg(args,"closed-port?","(closed-port? <port>)");
  if(args[0].is_type(types::fip))
    return data(boolean(!args[0].value.fip.is_open()));
  return data(boolean(!args[0].value.fop.is_open()));
}

// retrieve the current default input & output ports
data primitive_CURRENT_INPUT_PORT(scm_list& args){
  if(!no_args_given(args)) 
    THROW_ERR("'current-input-port doesn't take arguments: (current-input-port)"
      << FCN_ERR("current-input-port", args));
  for(size_type i = 0, n = PORT_REGISTRY.size(); i < n; ++i)
    if(CURRENT_INPUT_PORT == PORT_REGISTRY[i]) 
      return iport(i);
  return iport{};
}

data primitive_CURRENT_OUTPUT_PORT(scm_list& args){
  if(!no_args_given(args)) 
    THROW_ERR("'current-output-port doesn't take arguments: (current-output-port)"
      << FCN_ERR("current-output-port", args));
  for(size_type i = 0, n = PORT_REGISTRY.size(); i < n; ++i)
    if(CURRENT_OUTPUT_PORT == PORT_REGISTRY[i]) 
      return oport(i);
  return oport{};
}

// call an unary procedure with a file's port as its argument
data primitive_CALL_WITH_INPUT_FILE(scm_list& args){
  return primitive_CALL_WITH_FILE<iport>(
            args,
            "call-with-input-file",
            "\n     (call-with-input-file <filename-string> <unary-procedure>)"
            "\n     <unary-procedure> must accept a port as its argument!",
            confirm_valid_input_file);
}
data primitive_CALL_WITH_OUTPUT_FILE(scm_list& args){
  return primitive_CALL_WITH_FILE<oport>(
            args,
            "call-with-output-file",
            "\n     (call-with-output-file <filename-string> <unary-procedure>)"
            "\n     <unary-procedure> must accept a port as its argument!",
            confirm_valid_output_file);
}

// call an argless procedure with a file's port as the default port
data primitive_WITH_INPUT_FROM_FILE(scm_list& args){
  return primitive_WITH_FILE(
            args,
            "with-input-from-file",
            "\n     (with-input-from-file <filename-string> <argless-procedure>)",
            CURRENT_INPUT_PORT,
            confirm_valid_input_file);
}
data primitive_WITH_OUTPUT_TO_FILE(scm_list& args){
  return primitive_WITH_FILE(
            args,
            "with-output-to-file",
            "\n     (with-output-to-file <filename-string> <argless-procedure>)",
            CURRENT_OUTPUT_PORT,
            confirm_valid_output_file);
}

// retrieve a port for a file
data primitive_OPEN_INPUT_FILE(scm_list& args){
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm given a filename string
  static constexpr const char * const format = "\n     (open-input-file <filename-string>)";
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR("'open-input-file recieved incorrect # of args:" << format 
      << FCN_ERR("open-input-file", args));
  PORT_REGISTRY.push_back(confirm_valid_input_file(args[0],"open-input-file",format,args));
  return iport(PORT_REGISTRY.size()-1);
}

data primitive_OPEN_OUTPUT_FILE(scm_list& args){
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm given a filename string
  static constexpr const char * const format = "\n     (open-output-file <filename-string>)";
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR("'open-output-file recieved incorrect # of args:" << format
      << FCN_ERR("open-output-file", args));
  PORT_REGISTRY.push_back(confirm_valid_output_file(args[0],"open-output-file",format,args));
  return oport(PORT_REGISTRY.size()-1);
}

// slurp a file's contents as a string
data primitive_SLURP_FILE(scm_list& args){
  // confirm given a filename string & slurp file if so
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR("'slurp-file recieved incorrect # of args:"
      "\n     (slurp-file <filename-string>)"<<FCN_ERR("slurp-file",args));
  FILE* ins = confirm_valid_input_file(args[0],"slurp-file","(slurp-file <filename-string>)",args);
  scm_string buffer;
  int ch = 0;
  while((ch = fgetc(ins)) != EOF) buffer += ch;
  fclose(ins);
  return make_str(buffer); // slurp file
}

// close ports
data primitive_CLOSE_INPUT_PORT(scm_list& args){
  confirm_valid_close_port_arg(args, "input", types::fip);
  if(args[0].value.fip.is_open() && 
    args[0].value.fip.port() != stdin){
    fclose(args[0].value.fip.port());
    args[0].value.fip.port() = nullptr;
  }
  return data(types::dne);
}

data primitive_CLOSE_OUTPUT_PORT(scm_list& args){
  confirm_valid_close_port_arg(args, "output", types::fop);
  if(args[0].value.fop.is_open() && 
    args[0].value.fop.port() != stdout && args[0].value.fop.port() != stderr){
    fclose(args[0].value.fop.port());
    args[0].value.fop.port() = nullptr;
  }
  return data(types::dne);
}

/******************************************************************************
* SYSTEM INTERFACE PRIMITIVES
******************************************************************************/

// Load a script into the global environment
data primitive_LOAD(scm_list& args) {
  return primitive_LOAD_TEMPLATE(args,GLOBAL_ENVIRONMENT_POINTER,"load");
}

// Load a script into the local environment
data primitive_LOAD_LOCAL(scm_list& args) {
  auto env = args.rbegin()->value.env;
  args.pop_back();
  return primitive_LOAD_TEMPLATE(args,env,"load-local");
}

// Make a system call, returns #f if can't use 'system, and the call's success
//   status if can use the system
data primitive_SYSTEM(scm_list& args) {
  if(args.size() > 1)
    THROW_ERR("'system recieved incorrect # of args!"
      "\n     (system <optional-system-call-string>)"<<FCN_ERR("system",args));
  if(!no_args_given(args) && !args[0].is_type(types::str))
    THROW_ERR("'system "<<PROFILE(args[0])<<" isn't a string!"
      "\n     (system <optional-system-call-string>)"<<FCN_ERR("system",args));
  // return false if CAN'T use 'system'
  if(!std::system(nullptr)) return FALSE_DATA_BOOLEAN;
  // return true if just checking whether may use the system (no args given)
  if(no_args_given(args)) return TRUE_DATA_BOOLEAN;
  return num_type(std::system(args[0].value.str->c_str()));
}

// Given a string of a variable name, returns a string of that variable's value
data primitive_GETENV(scm_list& args) {
  // extract the environment
  auto env = args.rbegin()->value.env;
  args.pop_back();
  // confirm proper arg signature
  if(no_args_given(args) || args.size() != 1)
    THROW_ERR("'getenv recieved incorrect # of args!"
      "\n     (getenv <variable-name-string>)"<<FCN_ERR("getenv",args));
  if(!no_args_given(args) && !args[0].is_type(types::str))
    THROW_ERR("'getenv "<<PROFILE(args[0])<<" isn't a string!"
      "\n     (getenv <variable-name-string>)"<<FCN_ERR("getenv",args));
  // search each environment frame
  auto var = *args[0].value.str;
  for(size_type i = 0, total_frames = env->size(); i < total_frames; ++i){
    // Get Variables & Values Lists of the current frame
    auto& [var_list, val_list, mac_list] = *env->operator[](i);
    // Search Variable-Value List Pair In Frame
    for(size_type j = 0, total_vars = var_list.size(); j < total_vars; ++j)
      if(var == var_list[j] && !val_list[j].is_type(types::undefined))
        return make_str(val_list[j].cio_str());
  }
  return FALSE_DATA_BOOLEAN;
}

/******************************************************************************
* CURRENT TIME PRIMITIVE
******************************************************************************/

data primitive_SECONDS_SINCE_EPOCH(scm_list& args) {
  if(!no_args_given(args))
    THROW_ERR("'seconds-since-epoch doesn't take any arguments:"
      "\n     (seconds-since-epoch)" << FCN_ERR("seconds-since-epoch",args));
  return num_type(std::chrono::duration_cast<std::chrono::seconds>(
                  std::chrono::system_clock::now().time_since_epoch()).count());
}

/******************************************************************************
* RESET MAX RECURSION DEPTH PRIMITIVE
******************************************************************************/

data primitive_SET_MAX_RECURSION_DEPTH(scm_list& args) {
  // Confirm valid maximum recursion arg
  if(no_args_given(args) || args.size() != 1 || !args[0].is_type(types::num) || 
     !args[0].value.num.is_integer() || !args[0].value.num.is_pos())
    THROW_ERR("'set-max-recursion-depth! didn't recieve a positive integer arg:"
      "\n     (set-max-recursion-depth! <positive-integer>)"
      "\n     <positive-integer>: (0, " << MAX_SIZE_TYPE << ']'
      << FCN_ERR("set-max-recursion-depth!", args));
  auto float_num = args[0].value.num.to_inexact();
  if(float_num < 0 || float_num > MAX_SIZE_TYPE)
    THROW_ERR("'set-max-recursion-depth! integer arg is out of bounds!"
      "\n     (set-max-recursion-depth! <positive-integer>)"
      "\n     <positive-integer>: (0, " << MAX_SIZE_TYPE << ']'
      << FCN_ERR("set-max-recursion-depth!", args));
  // Set the cap on max recursive calls, & return the original
  auto original = MAX_RECURSION_DEPTH;
  MAX_RECURSION_DEPTH = (size_type)float_num.extract_inexact();
  return num_type(original);
}

/******************************************************************************
* TOGGLE ANSI ESCAPE SEQUENCES
******************************************************************************/

// Defaults to disabling ANSI escape sequences, if not given a boolean.
// Returns whether ANSI escapes sequences were disabled prior this call
data primitive_SET_NANSI(scm_list& args) {
  if(args.size() > 1)
    THROW_ERR("'set-nansi! recieved incorrect # of args:"
      "\n     (set-nansi! <optional-bool>)"
      << FCN_ERR("set-nansi!",args));
  bool original_nansi_status = !USING_ANSI_ESCAPE_SEQUENCES;
  USING_ANSI_ESCAPE_SEQUENCES = false;
  if(!no_args_given(args)) 
    USING_ANSI_ESCAPE_SEQUENCES = !is_true(args);
  return boolean(original_nansi_status);
}

/******************************************************************************
* ERROR HANDLING PRIMITIVE
******************************************************************************/

data primitive_ERROR(scm_list& args) {
  // Confirm valid error layout (a tad ironic)
  if(args.size() < 2)
    THROW_ERR("'error requires at least 2 args: a SYMBOL to represent the errorful entity & a STRING explaining the error!"
      "\n     (error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << FCN_ERR("error", args));
  if(!args[0].is_type(types::sym))
    THROW_ERR("'error requires 1st arg "<<PROFILE(args[0])<<" to be a SYMBOL to represent the errorful entity!"
      "\n     (error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << FCN_ERR("error", args));
  if(!args[1].is_type(types::str))
    THROW_ERR("'error requires its 2nd arg "<<PROFILE(args[1])<<" to be a STRING explaining the error!"
      "\n     (error <errorful-obj-symbol> <error-string> <optional-errorful-objs>)"
      << FCN_ERR("error", args));
  // Cook the raw error string
  scm_string error_str(*args[1].value.str);
  unescape_chars(error_str); 
  // Alert error
  fprintf(CURRENT_OUTPUT_PORT, 
          "\n%sException%s in %s: %s", afmt(AFMT_131), afmt(AFMT_01), 
          args[0].value.sym.c_str(), error_str.c_str());
  // Check for irritants (if provided, these are optional)
  if(args.size() == 3)
    fprintf(CURRENT_OUTPUT_PORT, " with irritant %s", args[2].cio_str().c_str());
  else if(args.size() > 3) {
    scm_list irritant_list(args.begin()+2, args.end());
    fprintf(CURRENT_OUTPUT_PORT, " with irritants %s", primitive_LIST(irritant_list).cio_str().c_str());
  }
  fprintf(CURRENT_OUTPUT_PORT, "%s\n", afmt(AFMT_0));
  fflush(CURRENT_OUTPUT_PORT);
  throw SCM_EXCEPT::EVAL;
  return data{};
}

/******************************************************************************
* REGISTRY OF PRIMITIVES DEFINED _IN_ HEIST SCHEME TO EVAL PRIOR ANYTHING ELSE
******************************************************************************/

constexpr const char * const PRIMITIVES_DEFINED_IN_HEIST_SCHEME[] = {
  primitive_HEIST_STREAM_MAP,        primitive_HEIST_STREAM_FILTER, 
  primitive_HEIST_STREAM_FROM,       primitive_HEIST_STREAM_UNFOLD, 
  primitive_HEIST_STREAM_ITERATE,    primitive_HEIST_STREAM_ZIP, 
  primitive_HEIST_STREAM_CONSTANT,   primitive_HEIST_STREAM_APPEND, 
  primitive_HEIST_STREAM_INTERLEAVE, 
};

void evaluate_primtives_written_in_heist_scheme(env_type& env) {
  scm_list heist_stream_map_prim;
  for(const auto& heist_prim : PRIMITIVES_DEFINED_IN_HEIST_SCHEME) {
    heist_stream_map_prim.clear();
    parse_input_exp(heist_prim, heist_stream_map_prim);
    scm_eval(std::move(heist_stream_map_prim[0].value.exp),env);
  }
}

/******************************************************************************
* REGISTRY OF PRIMITIVES ALSO REQUIRING AN ENVIRONMENT (TO APPLY A PROCEDURE)
******************************************************************************/

constexpr const auto ENV_REQUIRING_PRIMITIVES = {
  primitive_EVAL,      primitive_APPLY,         primitive_FOR_EACH,    
  primitive_MAP,       primitive_FILTER,        primitive_VECTOR_MAP,
  primitive_MERGE,     primitive_SORT,          primitive_SORT_BANG,   
  primitive_SORTEDP,   primitive_READ,          primitive_READ_STRING, 
  primitive_FOLD_LEFT, primitive_FOLD_RIGHT,    primitive_DELETE_NEIGHBOR_DUPS,
  primitive_UNFOLD,    primitive_VECTOR_UNFOLD, primitive_GETENV,

  primitive_STREAM_FOR_EACH,     primitive_STREAM_DROP_WHILE, 
  primitive_STREAM_TAKE,         primitive_STREAM_TAKE_WHILE, 
  primitive_STREAM_REVERSE,      primitive_CONVERT_STREAM_LIST, 
  primitive_CONVERT_LIST_STREAM, primitive_STREAM_FOLD_LEFT,
  primitive_STREAM_FOLD_RIGHT, 

  primitive_CALL_WITH_INPUT_FILE, primitive_CALL_WITH_OUTPUT_FILE, 
  primitive_WITH_INPUT_FROM_FILE, primitive_WITH_OUTPUT_TO_FILE,
  primitive_OPEN_INPUT_FILE,      primitive_OPEN_OUTPUT_FILE,
  primitive_LOAD_LOCAL,
};

constexpr bool primitive_requires_environment(const prm_type& prm) {
  for(const auto& p : ENV_REQUIRING_PRIMITIVES)
    if(p == prm) return true;
  return false;
}

/******************************************************************************
* PRIMITIVE NAMES & OBJECTS AS FRAME VARS & VALS FOR THE GLOBAL ENVIRONMENT
******************************************************************************/

auto primitive_procedure_objects = 
  frame_vals({
    data(scm_list({symconst::primitive, primitive_ADD, "+"})),
    data(scm_list({symconst::primitive, primitive_SUB, "-"})),
    data(scm_list({symconst::primitive, primitive_MUL, "*"})),
    data(scm_list({symconst::primitive, primitive_DIV, "/"})),

    data(scm_list({symconst::primitive, primitive_ABS,       "abs"})),
    data(scm_list({symconst::primitive, primitive_EXPT,      "expt"})),
    data(scm_list({symconst::primitive, primitive_MAX,       "max"})),
    data(scm_list({symconst::primitive, primitive_MIN,       "min"})),
    data(scm_list({symconst::primitive, primitive_QUOTIENT,  "quotient"})),
    data(scm_list({symconst::primitive, primitive_REMAINDER, "remainder"})),
    data(scm_list({symconst::primitive, primitive_MODULO,    "modulo"})),
    data(scm_list({symconst::primitive, primitive_EXP,       "exp"})),
    data(scm_list({symconst::primitive, primitive_LOG,       "log"})),
    data(scm_list({symconst::primitive, primitive_SQRT,      "sqrt"})),
    data(scm_list({symconst::primitive, primitive_GCD,       "gcd"})),
    data(scm_list({symconst::primitive, primitive_LCM,       "lcm"})),

    data(scm_list({symconst::primitive, primitive_ODDP,      "odd?"})),
    data(scm_list({symconst::primitive, primitive_EVENP,     "even?"})),
    data(scm_list({symconst::primitive, primitive_POSITIVEP, "positive?"})),
    data(scm_list({symconst::primitive, primitive_NEGATIVEP, "negative?"})),
    data(scm_list({symconst::primitive, primitive_ZEROP,     "zero?"})),
    data(scm_list({symconst::primitive, primitive_INFINITEP, "infinite?"})),
    data(scm_list({symconst::primitive, primitive_FINITEP,   "finite?"})),
    data(scm_list({symconst::primitive, primitive_NANP,      "nan?"})),

    data(scm_list({symconst::primitive, primitive_CEILING,  "ceiling"})),
    data(scm_list({symconst::primitive, primitive_FLOOR,    "floor"})),
    data(scm_list({symconst::primitive, primitive_TRUNCATE, "truncate"})),
    data(scm_list({symconst::primitive, primitive_ROUND,    "round"})),

    data(scm_list({symconst::primitive, primitive_COERCE_INEXACT_TO_EXACT, "inexact->exact"})),
    data(scm_list({symconst::primitive, primitive_COERCE_EXACT_TO_INEXACT, "exact->inexact"})),
    data(scm_list({symconst::primitive, primitive_EXACTP,                  "exact?"})),
    data(scm_list({symconst::primitive, primitive_INEXACTP,                "inexact?"})),
    data(scm_list({symconst::primitive, primitive_INTEGERP,                "integer?"})),
    data(scm_list({symconst::primitive, primitive_NUMERATOR,               "numerator"})),
    data(scm_list({symconst::primitive, primitive_DENOMINATOR,             "denominator"})),

    data(scm_list({symconst::primitive, primitive_SIN,   "sin"})),
    data(scm_list({symconst::primitive, primitive_COS,   "cos"})),
    data(scm_list({symconst::primitive, primitive_TAN,   "tan"})),
    data(scm_list({symconst::primitive, primitive_ASIN,  "asin"})),
    data(scm_list({symconst::primitive, primitive_ACOS,  "acos"})),
    data(scm_list({symconst::primitive, primitive_ATAN,  "atan"})),
    data(scm_list({symconst::primitive, primitive_SINH,  "sinh"})),
    data(scm_list({symconst::primitive, primitive_COSH,  "cosh"})),
    data(scm_list({symconst::primitive, primitive_TANH,  "tanh"})),
    data(scm_list({symconst::primitive, primitive_ASINH, "asinh"})),
    data(scm_list({symconst::primitive, primitive_ACOSH, "acosh"})),
    data(scm_list({symconst::primitive, primitive_ATANH, "atanh"})),

    data(scm_list({symconst::primitive, primitive_RANDOM, "random"})),

    data(scm_list({symconst::primitive, primitive_EQ,  "="})),
    data(scm_list({symconst::primitive, primitive_GT,  ">"})),
    data(scm_list({symconst::primitive, primitive_LT,  "<"})),
    data(scm_list({symconst::primitive, primitive_GTE, ">="})),
    data(scm_list({symconst::primitive, primitive_LTE, "<="})),

    data(scm_list({symconst::primitive, primitive_EQP,    "eq?"})),
    data(scm_list({symconst::primitive, primitive_EQVP,   "eqv?"})),
    data(scm_list({symconst::primitive, primitive_EQUALP, "equal?"})),
    data(scm_list({symconst::primitive, primitive_NOT,    "not"})),

    data(scm_list({symconst::primitive, primitive_CHAR_EQ,          "char=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_LT,          "char<?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_GT,          "char>?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_LTE,         "char<=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_GTE,         "char>=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_EQ,       "char-ci=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_LT,       "char-ci<?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_GT,       "char-ci>?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_LTE,      "char-ci<=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_CI_GTE,      "char-ci>=?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_ALPHABETICP, "char-alphabetic?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_NUMERICP,    "char-numeric?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_WHITESPACEP, "char-whitespace?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_UPPER_CASEP, "char-upper-case?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_LOWER_CASEP, "char-lower-case?"})),
    data(scm_list({symconst::primitive, primitive_CHAR_UPCASE,      "char-upcase"})),
    data(scm_list({symconst::primitive, primitive_CHAR_DOWNCASE,    "char-downcase"})),

    data(scm_list({symconst::primitive, primitive_MAKE_STRING,   "make-string"})),
    data(scm_list({symconst::primitive, primitive_STRING,        "string"})),
    data(scm_list({symconst::primitive, primitive_STRING_LENGTH, "string-length"})),
    data(scm_list({symconst::primitive, primitive_STRING_REF,    "string-ref"})),
    data(scm_list({symconst::primitive, primitive_STRING_SET,    "string-set!"})),
    data(scm_list({symconst::primitive, primitive_SUBSTRING,     "substring"})),
    data(scm_list({symconst::primitive, primitive_STRING_APPEND, "string-append"})),
    data(scm_list({symconst::primitive, primitive_STRING_COPY,   "string-copy"})),
    data(scm_list({symconst::primitive, primitive_STRING_FILL,   "string-fill!"})),
    data(scm_list({symconst::primitive, primitive_STRING_EQ,     "string=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_LT,     "string<?"})),
    data(scm_list({symconst::primitive, primitive_STRING_GT,     "string>?"})),
    data(scm_list({symconst::primitive, primitive_STRING_LTE,    "string<=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_GTE,    "string>=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_EQ,  "string-ci=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_LT,  "string-ci<?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_GT,  "string-ci>?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_LTE, "string-ci<=?"})),
    data(scm_list({symconst::primitive, primitive_STRING_CI_GTE, "string-ci>=?"})),

    data(scm_list({symconst::primitive, primitive_CONS,   "cons"})),
    data(scm_list({symconst::primitive, primitive_CAR,    "car"})),
    data(scm_list({symconst::primitive, primitive_CDR,    "cdr"})),
    data(scm_list({symconst::primitive, primitive_NULLP,  "null?"})),
    data(scm_list({symconst::primitive, primitive_SETCAR, "set-car!"})),
    data(scm_list({symconst::primitive, primitive_SETCDR, "set-cdr!"})),

    data(scm_list({symconst::primitive, primitive_CAAR, "caar"})),
    data(scm_list({symconst::primitive, primitive_CADR, "cadr"})),
    data(scm_list({symconst::primitive, primitive_CDAR, "cdar"})),
    data(scm_list({symconst::primitive, primitive_CDDR, "cddr"})),
    data(scm_list({symconst::primitive, primitive_CAAAR, "caaar"})),
    data(scm_list({symconst::primitive, primitive_CAADR, "caadr"})),
    data(scm_list({symconst::primitive, primitive_CADAR, "cadar"})),
    data(scm_list({symconst::primitive, primitive_CADDR, "caddr"})),
    data(scm_list({symconst::primitive, primitive_CDAAR, "cdaar"})),
    data(scm_list({symconst::primitive, primitive_CDADR, "cdadr"})),
    data(scm_list({symconst::primitive, primitive_CDDAR, "cddar"})),
    data(scm_list({symconst::primitive, primitive_CDDDR, "cdddr"})),
    data(scm_list({symconst::primitive, primitive_CAAAAR, "caaaar"})),
    data(scm_list({symconst::primitive, primitive_CAAADR, "caaadr"})),
    data(scm_list({symconst::primitive, primitive_CAADAR, "caadar"})),
    data(scm_list({symconst::primitive, primitive_CAADDR, "caaddr"})),
    data(scm_list({symconst::primitive, primitive_CADAAR, "cadaar"})),
    data(scm_list({symconst::primitive, primitive_CADADR, "cadadr"})),
    data(scm_list({symconst::primitive, primitive_CADDAR, "caddar"})),
    data(scm_list({symconst::primitive, primitive_CADDDR, "cadddr"})),
    data(scm_list({symconst::primitive, primitive_CDAAAR, "cdaaar"})),
    data(scm_list({symconst::primitive, primitive_CDAADR, "cdaadr"})),
    data(scm_list({symconst::primitive, primitive_CDADAR, "cdadar"})),
    data(scm_list({symconst::primitive, primitive_CDADDR, "cdaddr"})),
    data(scm_list({symconst::primitive, primitive_CDDAAR, "cddaar"})),
    data(scm_list({symconst::primitive, primitive_CDDADR, "cddadr"})),
    data(scm_list({symconst::primitive, primitive_CDDDAR, "cdddar"})),
    data(scm_list({symconst::primitive, primitive_CDDDDR, "cddddr"})),

    data(scm_list({symconst::primitive, primitive_LIST,           "list"})),
    data(scm_list({symconst::primitive, primitive_LENGTH,         "length"})),
    data(scm_list({symconst::primitive, primitive_CIRCULAR_LISTP, "circular-list?"})),
    data(scm_list({symconst::primitive, primitive_DOTTED_LISTP,   "dotted-list?"})),
    data(scm_list({symconst::primitive, primitive_LISTP,          "list?"})),
    data(scm_list({symconst::primitive, primitive_ALISTP,         "alist?"})),
    data(scm_list({symconst::primitive, primitive_APPEND,         "append"})),
    data(scm_list({symconst::primitive, primitive_REVERSE,        "reverse"})),
    data(scm_list({symconst::primitive, primitive_SUBLIST,        "sublist"})),
    data(scm_list({symconst::primitive, primitive_LIST_TAIL,      "list-tail"})),
    data(scm_list({symconst::primitive, primitive_LIST_HEAD,      "list-head"})),
    data(scm_list({symconst::primitive, primitive_LIST_REF,       "list-ref"})),

    data(scm_list({symconst::primitive, primitive_FOR_EACH,   "for-each"})),
    data(scm_list({symconst::primitive, primitive_MAP,        "map"})),
    data(scm_list({symconst::primitive, primitive_FILTER,     "filter"})),
    data(scm_list({symconst::primitive, primitive_FOLD_LEFT,  "fold-left"})),
    data(scm_list({symconst::primitive, primitive_FOLD_RIGHT, "fold-right"})),
    data(scm_list({symconst::primitive, primitive_UNFOLD,     "unfold"})),

    data(scm_list({symconst::primitive, primitive_MEMQ,   "memq"})),
    data(scm_list({symconst::primitive, primitive_MEMV,   "memv"})),
    data(scm_list({symconst::primitive, primitive_MEMBER, "member"})),
    data(scm_list({symconst::primitive, primitive_ASSQ,   "assq"})),
    data(scm_list({symconst::primitive, primitive_ASSV,   "assv"})),
    data(scm_list({symconst::primitive, primitive_ASSOC,  "assoc"})),

    data(scm_list({symconst::primitive, primitive_VECTOR,        "vector"})),
    data(scm_list({symconst::primitive, primitive_MAKE_VECTOR,   "make-vector"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_LENGTH, "vector-length"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_REF,    "vector-ref"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_SET,    "vector-set!"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_FILL,   "vector-fill!"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_GROW,   "vector-grow"})),
    data(scm_list({symconst::primitive, primitive_SUBVECTOR,     "subvector"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_HEAD,   "vector-head"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_TAIL,   "vector-tail"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_MAP,    "vector-map"})),
    data(scm_list({symconst::primitive, primitive_VECTOR_UNFOLD, "vector-unfold"})),

    data(scm_list({symconst::primitive, primitive_SORT,                 "sort"})),
    data(scm_list({symconst::primitive, primitive_SORT_BANG,            "sort!"})),
    data(scm_list({symconst::primitive, primitive_SORTEDP,              "sorted?"})),
    data(scm_list({symconst::primitive, primitive_MERGE,                "merge"})),
    data(scm_list({symconst::primitive, primitive_DELETE_NEIGHBOR_DUPS, "delete-neighbor-dups"})),

    data(scm_list({symconst::primitive, primitive_PAIRP,        "pair?"})),
    data(scm_list({symconst::primitive, primitive_VECTORP,      "vector?"})),
    data(scm_list({symconst::primitive, primitive_CHARP,        "char?"})),
    data(scm_list({symconst::primitive, primitive_NUMBERP,      "number?"})),
    data(scm_list({symconst::primitive, primitive_REALP,        "real?"})),
    data(scm_list({symconst::primitive, primitive_RATIONALP,    "rational?"})),
    data(scm_list({symconst::primitive, primitive_STRINGP,      "string?"})),
    data(scm_list({symconst::primitive, primitive_SYMBOLP,      "symbol?"})),
    data(scm_list({symconst::primitive, primitive_BOOLEANP,     "boolean?"})),
    data(scm_list({symconst::primitive, primitive_ATOMP,        "atom?"})),
    data(scm_list({symconst::primitive, primitive_PROCEDUREP,   "procedure?"})),
    data(scm_list({symconst::primitive, primitive_INPUT_PORTP,  "input-port?"})),
    data(scm_list({symconst::primitive, primitive_OUTPUT_PORTP, "output-port?"})),
    data(scm_list({symconst::primitive, primitive_EOF_OBJECTP,  "eof-object?"})),
    data(scm_list({symconst::primitive, primitive_STREAM_PAIRP, "stream-pair?"})),
    data(scm_list({symconst::primitive, primitive_STREAM_NULLP, "stream-null?"})),
    data(scm_list({symconst::primitive, primitive_STREAMP,      "stream?"})),

    data(scm_list({symconst::primitive, primitive_EVAL,  "eval"})),
    data(scm_list({symconst::primitive, primitive_APPLY, "apply"})),

    data(scm_list({symconst::primitive, primitive_DELAYP, "delay?"})),
    data(scm_list({symconst::primitive, primitive_FORCE,  "force"})),

    data(scm_list({symconst::primitive, primitive_SCAR,                "scar"})),
    data(scm_list({symconst::primitive, primitive_SCDR,                "scdr"})),
    data(scm_list({symconst::primitive, primitive_STREAM_LENGTH,       "stream-length"})),
    data(scm_list({symconst::primitive, primitive_STREAM_FOR_EACH,     "stream-for-each"})),
    data(scm_list({symconst::primitive, primitive_STREAM_REF,          "stream-ref"})),
    data(scm_list({symconst::primitive, primitive_STREAM_DROP,         "stream-drop"})),
    data(scm_list({symconst::primitive, primitive_STREAM_DROP_WHILE,   "stream-drop-while"})),
    data(scm_list({symconst::primitive, primitive_STREAM_TAKE,         "stream-take"})),
    data(scm_list({symconst::primitive, primitive_STREAM_TAKE_WHILE,   "stream-take-while"})),
    data(scm_list({symconst::primitive, primitive_STREAM_REVERSE,      "stream-reverse"})),
    data(scm_list({symconst::primitive, primitive_STREAM_FOLD_LEFT,    "stream-fold-left"})),
    data(scm_list({symconst::primitive, primitive_STREAM_FOLD_RIGHT,   "stream-fold-right"})),
    data(scm_list({symconst::primitive, primitive_CONVERT_STREAM_LIST, "stream->list"})),
    data(scm_list({symconst::primitive, primitive_CONVERT_LIST_STREAM, "list->stream"})),

    data(scm_list({symconst::primitive, primitive_SCAAR, "scaar"})),
    data(scm_list({symconst::primitive, primitive_SCADR, "scadr"})),
    data(scm_list({symconst::primitive, primitive_SCDAR, "scdar"})),
    data(scm_list({symconst::primitive, primitive_SCDDR, "scddr"})),
    data(scm_list({symconst::primitive, primitive_SCAAAR, "scaaar"})),
    data(scm_list({symconst::primitive, primitive_SCAADR, "scaadr"})),
    data(scm_list({symconst::primitive, primitive_SCADAR, "scadar"})),
    data(scm_list({symconst::primitive, primitive_SCADDR, "scaddr"})),
    data(scm_list({symconst::primitive, primitive_SCDAAR, "scdaar"})),
    data(scm_list({symconst::primitive, primitive_SCDADR, "scdadr"})),
    data(scm_list({symconst::primitive, primitive_SCDDAR, "scddar"})),
    data(scm_list({symconst::primitive, primitive_SCDDDR, "scdddr"})),
    data(scm_list({symconst::primitive, primitive_SCAAAAR, "scaaaar"})),
    data(scm_list({symconst::primitive, primitive_SCAAADR, "scaaadr"})),
    data(scm_list({symconst::primitive, primitive_SCAADAR, "scaadar"})),
    data(scm_list({symconst::primitive, primitive_SCAADDR, "scaaddr"})),
    data(scm_list({symconst::primitive, primitive_SCADAAR, "scadaar"})),
    data(scm_list({symconst::primitive, primitive_SCADADR, "scadadr"})),
    data(scm_list({symconst::primitive, primitive_SCADDAR, "scaddar"})),
    data(scm_list({symconst::primitive, primitive_SCADDDR, "scadddr"})),
    data(scm_list({symconst::primitive, primitive_SCDAAAR, "scdaaar"})),
    data(scm_list({symconst::primitive, primitive_SCDAADR, "scdaadr"})),
    data(scm_list({symconst::primitive, primitive_SCDADAR, "scdadar"})),
    data(scm_list({symconst::primitive, primitive_SCDADDR, "scdaddr"})),
    data(scm_list({symconst::primitive, primitive_SCDDAAR, "scddaar"})),
    data(scm_list({symconst::primitive, primitive_SCDDADR, "scddadr"})),
    data(scm_list({symconst::primitive, primitive_SCDDDAR, "scdddar"})),
    data(scm_list({symconst::primitive, primitive_SCDDDDR, "scddddr"})),

    data(scm_list({symconst::primitive, primitive_COERCE_CHAR_TO_INTEGER,  "char->integer"})),
    data(scm_list({symconst::primitive, primitive_COERCE_INTEGER_TO_CHAR,  "integer->char"})),
    data(scm_list({symconst::primitive, primitive_COERCE_NUMBER_TO_STRING, "number->string"})),
    data(scm_list({symconst::primitive, primitive_COERCE_STRING_TO_NUMBER, "string->number"})),
    data(scm_list({symconst::primitive, primitive_COERCE_STRING_TO_SYMBOL, "string->symbol"})),
    data(scm_list({symconst::primitive, primitive_COERCE_SYMBOL_TO_STRING, "symbol->string"})),
    data(scm_list({symconst::primitive, primitive_COERCE_VECTOR_TO_LIST,   "vector->list"})),
    data(scm_list({symconst::primitive, primitive_COERCE_LIST_TO_VECTOR,   "list->vector"})),
    data(scm_list({symconst::primitive, primitive_STRING_TO_LIST,          "string->list"})),
    data(scm_list({symconst::primitive, primitive_LIST_TO_STRING,          "list->string"})),

    data(scm_list({symconst::primitive, primitive_WRITE,      "write"})),
    data(scm_list({symconst::primitive, primitive_DISPLAY,    "display"})),
    data(scm_list({symconst::primitive, primitive_NEWLINE,    "newline"})),
    data(scm_list({symconst::primitive, primitive_WRITE_CHAR, "write-char"})),

    data(scm_list({symconst::primitive, primitive_READ,        "read"})),
    data(scm_list({symconst::primitive, primitive_READ_STRING, "read-string"})),
    data(scm_list({symconst::primitive, primitive_READ_CHAR,   "read-char"})),
    data(scm_list({symconst::primitive, primitive_PEEK_CHAR,   "peek-char"})),
    data(scm_list({symconst::primitive, primitive_CHAR_READYP, "char-ready?"})),

    data(scm_list({symconst::primitive, primitive_FILEP,                 "file?"})),
    data(scm_list({symconst::primitive, primitive_OPEN_PORTP,            "open-port?"})),
    data(scm_list({symconst::primitive, primitive_CLOSED_PORTP,          "closed-port?"})),
    data(scm_list({symconst::primitive, primitive_CURRENT_INPUT_PORT,    "current-input-port"})),
    data(scm_list({symconst::primitive, primitive_CURRENT_OUTPUT_PORT,   "current-output-port"})),
    data(scm_list({symconst::primitive, primitive_CALL_WITH_INPUT_FILE,  "call-with-input-file"})),
    data(scm_list({symconst::primitive, primitive_CALL_WITH_OUTPUT_FILE, "call-with-output-file"})),
    data(scm_list({symconst::primitive, primitive_WITH_INPUT_FROM_FILE,  "with-input-from-file"})),
    data(scm_list({symconst::primitive, primitive_WITH_OUTPUT_TO_FILE,   "with-output-to-file"})),
    data(scm_list({symconst::primitive, primitive_OPEN_INPUT_FILE,       "open-input-file"})),
    data(scm_list({symconst::primitive, primitive_OPEN_OUTPUT_FILE,      "open-output-file"})),
    data(scm_list({symconst::primitive, primitive_SLURP_FILE,            "slurp-file"})),
    data(scm_list({symconst::primitive, primitive_CLOSE_INPUT_PORT,      "close-input-port"})),
    data(scm_list({symconst::primitive, primitive_CLOSE_OUTPUT_PORT,     "close-output-port"})),

    data(scm_list({symconst::primitive, primitive_LOAD,       "load"})),
    data(scm_list({symconst::primitive, primitive_LOAD_LOCAL, "load-local"})),
    data(scm_list({symconst::primitive, primitive_SYSTEM,     "system"})),
    data(scm_list({symconst::primitive, primitive_GETENV,     "getenv"})),

    data(scm_list({symconst::primitive, primitive_SECONDS_SINCE_EPOCH, "seconds-since-epoch"})),

    data(scm_list({symconst::primitive, primitive_SET_MAX_RECURSION_DEPTH, "set-max-recursion-depth!"})),

    data(scm_list({symconst::primitive, primitive_SET_NANSI, "set-nansi!"})),

    data(scm_list({symconst::primitive, primitive_ERROR, "error"})),
  });

frame_vars primitive_procedure_names() {
  const auto n = primitive_procedure_objects.size();
  frame_vars names(n);
  for(size_type i = 0; i < n; ++i)
    names[i] = primitive_procedure_objects[i].value.exp[2].value.sym;
  return names;
}
#endif
