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

namespace heist {

  // primitive "+" procedure
  data primitive_ADD(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "+", "(+ <num1> <num2> ...)");
    num_type sum;
    for(size_type i = 0, n = args.size(); i < n; ++i)
      sum += args[i].num;
    return data(sum);
  }

  // primitive "-" procedure: BOTH NEGATION & SUBTRACTION
  data primitive_SUB(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "-", "(- <num1> <num2> ...)");
    num_type difference = args[0].num;
    if(args.size()==1) return -1 * difference;        // negation
    for(size_type i = 1, n = args.size(); i < n; ++i) // subtraction
      difference -= args[i].num;
    return data(difference);
  }

  // primitive "*" procedure:
  data primitive_MUL(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "*", "(* <num1> <num2> ...)");
    num_type product("1");
    for(size_type i = 0, n = args.size(); i < n; ++i)
      product *= args[i].num;
    return data(product);
  }

  // primitive "/" procedure:
  data primitive_DIV(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "/", "(/ <num1> <num2> ...)");
    if(args.size()==1) return data(1 / args[0].num);
    num_type dividend = args[0].num;
    for(size_type i = 1, n = args.size(); i < n; ++i)
      dividend /= args[i].num;
    return data(dividend);
  }

  /******************************************************************************
  * MISCELLANEOUS NUMERIC PRIMITIVE OPERATIONS 
  ******************************************************************************/

  // primitive "abs" procedure
  data primitive_ABS(scm_list& args) {
    confirm_unary_numeric(args, "abs", "(abs <num>)");
    return data(args[0].num.abs());
  }

  // primitive "expt" procedure
  data primitive_EXPT(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "expt", "(expt <num1> <num2>)");
    confirm_2_args(args, "expt", "(expt <num1> <num2>)");
    return data((args[0].num.expt(args[1].num)));
  }

  // primitive "expt-mod" procedure [more efficient (modulo (expt x y) z)]
  data primitive_EXPT_MOD(scm_list& args) {
    // Confirm valid arguments
    static constexpr const char * const format = "\n     (expt-mod <num1> <num2> <num3>)";
    if(args.size() != 3)
      THROW_ERR("'expt-mod invalid # of args given!" << format << FCN_ERR("expt-mod",args));
    for(size_type i = 0; i < 3; ++i)
      if(!args[i].is_type(types::num) || !args[i].num.is_integer() || args[i].num.is_neg())
        THROW_ERR("'expt-mod arg #" << i+1 << ' ' << PROFILE(args[i]) << " isn't a non-negative integer!" 
          << format << FCN_ERR("expt-mod",args));
    // Perform Repeated Squares
    auto &x = args[0].num, &z = args[2].num;
    auto y_bitstr = args[1].num.str(2);
    num_type f("1");
    // For each bit, from left to right
    for(size_type i = 0, n = y_bitstr.size(); i < n; ++i) {
      f = (f * f).modulo(z);
      if(y_bitstr[i] & 1) f = (f * x).modulo(z); // if ith bit is 1
    }
    return f;
  }

  // primitive "max" procedure
  data primitive_MAX(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "max", "(max <num1> <num2> ...)");
    num_type max = args[0].num;
    for(size_type i = 1, n = args.size(); i < n; ++i)
      if(args[i].num > max)
        max = args[i].num;
    return max;
  }

  // primitive "min" procedure
  data primitive_MIN(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "min", "(min <num1> <num2> ...)");
    num_type min = args[0].num;
    for(size_type i = 1, n = args.size(); i < n; ++i)
      if(args[i].num < min)
        min = args[i].num;
    return min;
  }

  // primitive "quotient" procedure
  data primitive_QUOTIENT(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "quotient", "(quotient <num1> <num2>)");
    confirm_2_args(args, "quotient", "(quotient <num1> <num2>)");
    return data(args[0].num.quotient(args[1].num));
  }

  // primitive "remainder" procedure
  data primitive_REMAINDER(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "remainder", "(remainder <num1> <num2>)");
    confirm_2_args(args, "remainder", "(remainder <num1> <num2>)");
    return data((args[0].num % args[1].num));
  }

  // primitive "modulo" procedure
  data primitive_MODULO(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "modulo", "(modulo <num1> <num2>)");
    confirm_2_args(args, "modulo", "(modulo <num1> <num2>)");
    return data(args[0].num.modulo(args[1].num));
  }

  // primitive "exp" procedure
  data primitive_EXP(scm_list& args) {
    confirm_unary_numeric(args, "exp", "(exp <num>)");
    return data(args[0].num.exp());
  }

  // primitive "log" procedure -- NATURAL LOGARITHM
  data primitive_LOG(scm_list& args) {
    confirm_unary_numeric(args, "log", "(log <num>)");
    return data(args[0].num.log());
  }

  // primitive "sqrt" procedure
  data primitive_SQRT(scm_list& args) {
    confirm_unary_numeric(args, "sqrt", "(sqrt <num>)");
    return data(args[0].num.sqrt());
  }

  // primitive "gcd" procedure
  data primitive_GCD(scm_list& args) {
    if(args.empty()) return num_type();
    confirm_no_numeric_primitive_errors(args, "gcd", "(gcd <num1> <num2> ...)");
    if(args.size() == 1) return args[0];
    // GCD is associative
    num_type gcd_val(args[0].num);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      gcd_val = gcd_val.gcd(args[i].num);
    return gcd_val;
  }

  // primitive "lcm" procedure
  data primitive_LCM(scm_list& args) {
    if(args.empty()) return num_type("1");
    confirm_no_numeric_primitive_errors(args, "lcm", "(lcm <num1> <num2> ...)");
    if(args.size() == 1) return args[0];
    // LCM is associative
    num_type lcm_val(args[0].num);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      lcm_val = lcm_val.lcm(args[i].num);
    return lcm_val;
  }

  // primitive "modf" procedure
  // (define (modf num) (cons (truncate num) (remainder num (truncate num))))
  data primitive_MODF(scm_list& args) {
    confirm_unary_numeric(args, "modf", "(modf <num>)");
    data num_pair = data(make_par());
    if(args[0].num.is_integer()) {
      num_pair.par->first  = args[0];
      num_pair.par->second = num_type();
      return num_pair;
    }
    num_type::inexact_t integral;
    num_type::inexact_t fractional = std::modf(args[0].num.extract_inexact(), &integral);
    num_pair.par->first  = num_type(integral);
    num_pair.par->second = num_type(fractional);
    return num_pair;
  }

  /******************************************************************************
  * MISCELLANEOUS NUMERIC PREDICATE PRIMITIVES 
  ******************************************************************************/

  // primitive "odd?" procedure
  data primitive_ODDP(scm_list& args) {
    confirm_unary_numeric(args, "odd?", "(odd? <num>)");
    return data(boolean(args[0].num.is_odd()));
  }

  // primitive "even?" procedure
  data primitive_EVENP(scm_list& args) {
    confirm_unary_numeric(args, "even?", "(even? <num>)");
    return data(boolean(args[0].num.is_even()));
  }

  // primitive "positive?" procedure
  data primitive_POSITIVEP(scm_list& args) {
    confirm_unary_numeric(args, "positive?", "(positive? <num>)");
    return data(boolean(args[0].num.is_pos()));
  }

  // primitive "negative?" procedure
  data primitive_NEGATIVEP(scm_list& args) {
    confirm_unary_numeric(args, "negative?", "(negative? <num>)");
    return data(boolean(args[0].num.is_neg()));
  }

  // primitive "zero?" procedure
  data primitive_ZEROP(scm_list& args) {
    confirm_unary_numeric(args, "zero?", "(zero? <num>)");
    return data(boolean(args[0].num.is_zero()));
  }

  // primitive "infinite?" procedure
  data primitive_INFINITEP(scm_list& args) {
    confirm_unary_numeric(args, "infinite?", "(infinite? <num>)");
    return data(boolean(args[0].num.is_pos_inf() || 
                        args[0].num.is_neg_inf()));
  }

  // primitive "finite?" procedure (same as primitive "real?")
  data primitive_FINITEP(scm_list& args) {
    confirm_unary_numeric(args, "finite?", "(finite? <num>)");
    return data(boolean(args[0].num.is_real()));
  }

  // primitive "nan?" procedure
  data primitive_NANP(scm_list& args) {
    confirm_unary_numeric(args, "nan?", "(nan? <num>)");
    return data(boolean(args[0].num.is_nan()));
  }

  /******************************************************************************
  * NUMERIC ROUNDING PRIMITIVES 
  ******************************************************************************/

  // primitive "ceiling" procedure -- ROUNDS UP
  data primitive_CEILING(scm_list& args) {
    confirm_unary_numeric(args, "ceiling", "(ceiling <num>)");
    return data(args[0].num.ceil());
  }

  // primitive "floor" procedure -- ROUNDS DOWN
  data primitive_FLOOR(scm_list& args) {
    confirm_unary_numeric(args, "floor", "(floor <num>)");
    return data(args[0].num.floor());
  }

  // primitive "truncate" procedure -- ROUNDS TOWARDS ZERO
  data primitive_TRUNCATE(scm_list& args) {
    confirm_unary_numeric(args, "truncate", "(truncate <num>)");
    return data(args[0].num.trunc());
  }

  // primitive "round" procedure -- ROUNDS TOWARDS THE NEAREST INT
  data primitive_ROUND(scm_list& args) {
    confirm_unary_numeric(args, "round", "(round <num>)");
    return data(args[0].num.round());
  }

  /******************************************************************************
  * NUMERIC PRECISION COERCION & INTEGER ANALYSIS PRIMITIVES 
  ******************************************************************************/

  // primitive "inexact->exact" procedure
  data primitive_COERCE_INEXACT_TO_EXACT(scm_list& args) {
    confirm_unary_numeric(args, "inexact->exact", "(inexact->exact <num>)");
    return data(args[0].num.to_exact());
  }

  // primitive "exact->inexact" procedure
  data primitive_COERCE_EXACT_TO_INEXACT(scm_list& args) {
    confirm_unary_numeric(args, "exact->inexact", "(exact->inexact <num>)");
    return data(args[0].num.to_inexact());
  }

  // primitive "exact?" procedure
  data primitive_EXACTP(scm_list& args) {
    confirm_unary_numeric(args, "exact?", "(exact? <num>)");
    return data(boolean(args[0].num.is_exact()));
  }

  // primitive "inexact?" procedure
  data primitive_INEXACTP(scm_list& args) {
    confirm_unary_numeric(args, "inexact?", "(inexact? <num>)");
    return data(boolean(args[0].num.is_inexact()));
  }

  // primitive "integer?" procedure
  data primitive_INTEGERP(scm_list& args) {
    confirm_unary_numeric(args, "integer?", "(integer? <num>)");
    return data(boolean(args[0].num.is_integer()));
  }

  // primitive "numerator" procedure
  data primitive_NUMERATOR(scm_list& args) {
    confirm_unary_numeric(args, "numerator", "(numerator <num>)");
    return data(args[0].num.extract_numerator());
  }

  // primitive "denominator" procedure
  data primitive_DENOMINATOR(scm_list& args) {
    confirm_unary_numeric(args, "denominator", "(denominator <num>)");
    return data(args[0].num.extract_denominator());
  }

  // primitive "make-log-base" procedure
  // (define (make-log-base n) (lambda (num) (/ (log num) (log n))))
  data primitive_MAKE_LOG_BASE(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    confirm_unary_numeric(args, "make-log-base", "(make-log-base <num>)");
    scm_list new_log(3);
    new_log[0] = symconst::lambda;
    new_log[1] = scm_list(1, "num");
    new_log[2] = scm_list(3);
    new_log[2].exp[0] = "/";
    new_log[2].exp[1] = scm_list(2);
    new_log[2].exp[1].exp[0] = "log";
    new_log[2].exp[1].exp[1] = "num";
    new_log[2].exp[2] = scm_list(2);
    new_log[2].exp[2].exp[0] = "log";
    new_log[2].exp[2].exp[1] = args[0];
    return data_cast(scm_eval(std::move(new_log),env));
  }

  /******************************************************************************
  * NUMERIC TRIGONOMETRIC PRIMITIVES -- IN RADIANS
  ******************************************************************************/

  // primitive "sin" procedure
  data primitive_SIN(scm_list& args) {
    confirm_unary_numeric(args, "sin", "(sin <num>)");
    return data(args[0].num.sin());
  }

  // primitive "cos" procedure
  data primitive_COS(scm_list& args) {
    confirm_unary_numeric(args, "cos", "(cos <num>)");
    return data(args[0].num.cos());
  }

  // primitive "tan" procedure
  data primitive_TAN(scm_list& args) {
    confirm_unary_numeric(args, "tan", "(tan <num>)");
    return data(args[0].num.tan());
  }

  // primitive "asin" procedure
  data primitive_ASIN(scm_list& args) {
    confirm_unary_numeric(args, "asin", "(asin <num>)");
    return data(args[0].num.asin());
  }

  // primitive "acos" procedure
  data primitive_ACOS(scm_list& args) {
    confirm_unary_numeric(args, "acos", "(acos <num>)");
    return data(args[0].num.acos());
  }

  // primitive "atan" procedure
  data primitive_ATAN(scm_list& args) {
    confirm_unary_numeric(args, "atan", "(atan <num>)");
    return data(args[0].num.atan());
  }

  // primitive "sinh" procedure
  data primitive_SINH(scm_list& args) {
    confirm_unary_numeric(args, "sinh", "(sinh <num>)");
    return data(args[0].num.sinh());
  }

  // primitive "cosh" procedure
  data primitive_COSH(scm_list& args) {
    confirm_unary_numeric(args, "cosh", "(cosh <num>)");
    return data(args[0].num.cosh());
  }

  // primitive "tanh" procedure
  data primitive_TANH(scm_list& args) {
    confirm_unary_numeric(args, "tanh", "(tanh <num>)");
    return data(args[0].num.tanh());
  }

  // primitive "asinh" procedure
  data primitive_ASINH(scm_list& args) {
    confirm_unary_numeric(args, "asinh", "(asinh <num>)");
    return data(args[0].num.asinh());
  }

  // primitive "acosh" procedure
  data primitive_ACOSH(scm_list& args) {
    confirm_unary_numeric(args, "acosh", "(acosh <num>)");
    return data(args[0].num.acosh());
  }

  // primitive "atanh" procedure
  data primitive_ATANH(scm_list& args) {
    confirm_unary_numeric(args, "atanh", "(atanh <num>)");
    return data(args[0].num.atanh());
  }

  /******************************************************************************
  * NUMERIC BITWISE OPERATION PRIMITIVES 
  ******************************************************************************/

  // primitive "logand" procedure
  data primitive_LOGAND(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "logand", "(logand <num1> <num2>)");
    confirm_2_args(args, "logand", "(logand <num1> <num2>)");
    return data(args[0].num & args[1].num);
  }

  // primitive "logor" procedure
  data primitive_LOGOR(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "logor", "(logor <num1> <num2>)");
    confirm_2_args(args, "logor", "(logor <num1> <num2>)");
    return data(args[0].num | args[1].num);
  }

  // primitive "logxor" procedure
  data primitive_LOGXOR(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "logxor", "(logxor <num1> <num2>)");
    confirm_2_args(args, "logxor", "(logxor <num1> <num2>)");
    return data(args[0].num ^ args[1].num);
  }

  // primitive "lognot" procedure
  data primitive_LOGNOT(scm_list& args) {
    confirm_unary_numeric(args, "lognot", "(lognot <num>)");
    return data(~args[0].num);
  }

  // primitive "loglsl" procedure
  data primitive_LOGLSL(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "loglsl", "(loglsl <num> <shift-amount>)");
    confirm_2_args(args, "loglsl", "(loglsl <num> <shift-amount>)");
    return data(args[0].num << args[1].num);
  }

  // primitive "loglsr" procedure
  data primitive_LOGLSR(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "loglsr", "(loglsr <num> <shift-amount>)");
    confirm_2_args(args, "loglsr", "(loglsr <num> <shift-amount>)");
    return data(args[0].num >> args[1].num);
  }

  // primitive "logasr" procedure
  data primitive_LOGASR(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "logasr", "(logasr <num> <shift-amount>)");
    confirm_2_args(args, "logasr", "(logasr <num> <shift-amount>)");
    return data(args[0].num.asr(args[1].num));
  }

  // primitive "logbit?" procedure
  data primitive_LOGBITP(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "logbit?", "(logbit? <num> <bit-No>)");
    confirm_2_args(args, "logbit?", "(logbit? <num> <bit-No>)");
    return data((args[0].num >> args[1].num) & 1);
  }

  // primitive "logbit1" procedure
  data primitive_LOGBIT1(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "logbit1", "(logbit1 <num> <bit-No>)");
    confirm_2_args(args, "logbit1", "(logbit1 <num> <bit-No>)");
    return data(args[0].num | (1 << args[1].num));
  }

  // primitive "logbit0" procedure
  data primitive_LOGBIT0(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "logbit0", "(logbit0 <num> <bit-No>)");
    confirm_2_args(args, "logbit0", "(logbit0 <num> <bit-No>)");
    return data(args[0].num & ~(1 << args[1].num));
  }

  // primitive "logbit~" procedure
  data primitive_LOGBIT_CMPL(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "logbit~", "(logbit~ <num> <bit-No>)");
    confirm_2_args(args, "logbit~", "(logbit~ <num> <bit-No>)");
    return data(args[0].num ^ (1 << args[1].num));
  }

  /******************************************************************************
  * NUMERIC RANDOM NUMBER GENERATOR PRIMITIVES 
  ******************************************************************************/

  // primitive "random" procedure -- SEED DEFAULTS TO CURRENT TIME SINCE EPOCH
  data primitive_RANDOM(scm_list& args) {
    if(args.size() > 1)
      THROW_ERR("'random received more than 1 arg:" 
        "\n     (random <optional-numeric-seed>)" << FCN_ERR("random",args));
    if(args.size()==1 && !args[0].is_type(types::num))
      THROW_ERR("'random received non-numeric arg " << PROFILE(args[0])
        << ":\n     (random <optional-numeric-seed>)" << FCN_ERR("random",args));
    if(args.empty())
      return data(num_type::random());
    return data(num_type::random(args[0].num));
  }

  /******************************************************************************
  * NUMERIC COMPARISON PRIMITIVES
  ******************************************************************************/

  // primitive "=" procedure:
  data primitive_EQ(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "=", "(= <num1> <num2> ...)");
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num != args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive ">" procedure:
  data primitive_GT(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, ">", "(> <num1> <num2> ...)");
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num <= args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "<" procedure:
  data primitive_LT(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "<", "(< <num1> <num2> ...)");
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num >= args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive ">=" procedure:
  data primitive_GTE(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, ">=", "(>= <num1> <num2> ...)");
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num < args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "<=" procedure:
  data primitive_LTE(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "<=", "(<= <num1> <num2> ...)");
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num > args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * GENERAL COMPARISON PRIMITIVES
  ******************************************************************************/

  // primitive "eq?" procedure:
  data primitive_EQP(scm_list& args) {
    if(args.empty())
      THROW_ERR("'eq? received no arguments: (eq? <obj1> <obj2> ...)" << FCN_ERR("eq?", args));
    // compare types & values
    for(size_type i = 0, n = args.size(); i+1 < n; ++i) {
      if(args[i].type != args[i+1].type) 
        return G::FALSE_DATA_BOOLEAN;
      if(args[i].is_type(types::str)) { // compare strings via pointers
        if(args[i].str != args[i+1].str)
          return G::FALSE_DATA_BOOLEAN;
      } else if(!prm_compare_atomic_values(args[i],args[i+1],args[i].type)) {
        return G::FALSE_DATA_BOOLEAN;
      }
    }
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "eqv?" procedure:
  data primitive_EQVP(scm_list& args) {
    if(args.empty())
      THROW_ERR("'eqv? received no arguments: (eqv? <obj1> <obj2> ...)" << FCN_ERR("eqv?", args));
    // compare types & values
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].type != args[i+1].type || !prm_compare_atomic_values(args[i],args[i+1],args[i].type))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "equal?" procedure:
  data primitive_EQUALP(scm_list& args) {
    if(args.empty())
      THROW_ERR("'equal? received no arguments: (equal? <obj1> <obj2> ...)" << FCN_ERR("equal?", args));
    for(size_type i = 0, n = args.size(); i+1 < n; ++i) {
      if(args[i].type != args[i+1].type) // compare types
        return G::FALSE_DATA_BOOLEAN;
      if(args[i].is_type(types::exp)) { // compare sub-lists
        if(!prm_compare_EXPRs(args[i].exp,args[i+1].exp))
          return G::FALSE_DATA_BOOLEAN;
      } else if(args[i].is_type(types::par)) {
        if(!prm_compare_PAIRs(args[i].par,args[i+1].par))
          return G::FALSE_DATA_BOOLEAN;
      } else if(args[i].is_type(types::vec)) {
        if(!prm_compare_VECTs(args[i].vec,args[i+1].vec))
          return G::FALSE_DATA_BOOLEAN;
      } else if(!prm_compare_atomic_values(args[i],args[i+1],args[i].type))
          return G::FALSE_DATA_BOOLEAN; // compare values
    }
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "not" procedure:
  data primitive_NOT(scm_list& args) {
    confirm_given_one_arg(args,"not");
    return data(boolean(args[0].is_type(types::bol) && !args[0].bol.val));
  }

  /******************************************************************************
  * CHARACTER PRIMITIVES
  ******************************************************************************/

  // -----------------
  // Char Comparators:
  // -----------------

  // primitive "char=?" procedure:
  data primitive_CHAR_EQ(scm_list& args) {
    confirm_given_char_string_args(args, "char=?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr != args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char<?" procedure:
  data primitive_CHAR_LT(scm_list& args) {
    confirm_given_char_string_args(args, "char<?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr >= args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char>?" procedure:
  data primitive_CHAR_GT(scm_list& args) {
    confirm_given_char_string_args(args, "char>?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr <= args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char<=?" procedure:
  data primitive_CHAR_LTE(scm_list& args) {
    confirm_given_char_string_args(args, "char<=?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr > args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char>=?" procedure:
  data primitive_CHAR_GTE(scm_list& args) {
    confirm_given_char_string_args(args, "char>=?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr < args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci=?" procedure:
  data primitive_CHAR_CI_EQ(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci=?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) != scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci<?" procedure:
  data primitive_CHAR_CI_LT(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci<?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) >= scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci>?" procedure:
  data primitive_CHAR_CI_GT(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci>?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) <= scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci<=?" procedure:
  data primitive_CHAR_CI_LTE(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci<=?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) > scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci>=?" procedure:
  data primitive_CHAR_CI_GTE(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci>=?", types::chr);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) < scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // --------------
  // Char Analysis:
  // --------------

  // primitive "char-alphabetic?" procedure:
  data primitive_CHAR_ALPHABETICP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-alphabetic?");
    return boolean(isalpha(args[0].chr));
  }

  // primitive "char-numeric?" procedure:
  data primitive_CHAR_NUMERICP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-numeric?");
    return boolean(isdigit(args[0].chr));
  }

  // primitive "char-whitespace?" procedure:
  data primitive_CHAR_WHITESPACEP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-whitespace?");
    return boolean(isspace(args[0].chr));
  }

  // primitive "char-upper-case?" procedure:
  data primitive_CHAR_UPPER_CASEP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-upper-case?");
    return boolean(isupper(args[0].chr));
  }

  // primitive "char-lower-case?" procedure:
  data primitive_CHAR_LOWER_CASEP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-lower-case?");
    return boolean(islower(args[0].chr));
  }

  // ------------------
  // Char Case Control:
  // ------------------

  // primitive "char-upcase" procedure:
  data primitive_CHAR_UPCASE(scm_list& args) {
    confirm_given_one_char_arg(args, "char-upcase");
    return toupper(args[0].chr);
  }

  // primitive "char-downcase" procedure:
  data primitive_CHAR_DOWNCASE(scm_list& args) {
    confirm_given_one_char_arg(args, "char-downcase");
    return tolower(args[0].chr);
  }

  /******************************************************************************
  * STRING PRIMITIVES
  ******************************************************************************/

  // primitive "make-string" procedure:
  data primitive_MAKE_STRING(scm_list& args) {
    // confirm valid length given
    if(args.empty() || args.size() > 2 || !primitive_is_valid_size(args[0]))
      THROW_ERR("'make-string didn't receive a proper positive integer size!"
        "\n     (make-string <size> <optional-fill-char>)"
        "\n     <size> range: (0," << G::MAX_SIZE_TYPE << ']'
        << FCN_ERR("make-string", args));
    if(args.size()==2 && !args[1].is_type(types::chr))
      THROW_ERR("'make-string received a non-character fill value:"
        "\n     Received fill value "<<PROFILE(args[1])<<'!'
        << "\n     (make-string <size> <optional-fill-char>)"
        << FCN_ERR("make-string", args));
    // mk a string w/ the the given reserve size
    size_type n = (size_type)args[0].num.extract_inexact();
    return make_str(scm_string(n, (args.size()==2 ? args[1].chr : '?')));
  }

  // primitive "string" procedure:
  data primitive_STRING(scm_list& args) {
    if(args.empty()) return make_str("");
    if(auto i = confirm_only_args_of_type(args, types::chr); i != G::MAX_SIZE_TYPE)
      THROW_ERR("'string arg #" << i+1 << ", " << PROFILE(args[i]) 
        << ",\n     isn't a character: (string <char1> <char2> ...)"
        << FCN_ERR("string", args));
    scm_string str_val;
    for(const auto& ch : args) str_val += ch.chr;
    return make_str(str_val);
  }

  // primitive "string-unfold" procedure:
  data primitive_STRING_UNFOLD(scm_list& args) {
    return primitive_STRING_UNFOLD_template(args,false,"string-unfold",
      "\n     (string-unfold <break-condition> <map-procedure> <successor-procedure> <seed>)");
  }

  // primitive "string-unfold-right" procedure:
  data primitive_STRING_UNFOLD_RIGHT(scm_list& args) {
    return primitive_STRING_UNFOLD_template(args,true,"string-unfold-right",
      "\n     (string-unfold-right <break-condition> <map-procedure> <successor-procedure> <seed>)");
  }

  // primitive "string-pad" procedure:
  data primitive_STRING_PAD(scm_list& args) {
    char padding_character = confirm_valid_string_pad_args(args, "string-pad",
      "\n     (string-pad <string> <length> <optional-character>)");
    const size_type length = (size_type)args[1].num.extract_inexact();
    const size_type n = args[0].str->size();
    if(length > n)
      return make_str(scm_string(length-n, padding_character) + *args[0].str);
    return make_str(args[0].str->substr(n-length));
  }

  // primitive "string-pad-right" procedure:
  data primitive_STRING_PAD_RIGHT(scm_list& args) {
    char padding_character = confirm_valid_string_pad_args(args, "string-pad-right",
      "\n     (string-pad-right <string> <length> <optional-character>)");
    const size_type length = (size_type)args[1].num.extract_inexact();
    const size_type n = args[0].str->size();
    if(length > n)
      return make_str(*args[0].str + scm_string(length-n, padding_character));
    return make_str(args[0].str->substr(0, length));
  }

  // primitive "string-trim" procedure:
  data primitive_STRING_TRIM(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    confirm_valid_string_trim_args(args, "string-trim",
      "\n     (string-trim <string> <optional-predicate>)");
    return prm_trim_left_of_string(args,env);
  }

  // primitive "string-trim-right" procedure:
  data primitive_STRING_TRIM_RIGHT(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    confirm_valid_string_trim_args(args, "string-trim-right",
      "\n     (string-trim-right <string> <optional-predicate>)");
    return prm_trim_right_of_string(args,env);
  }

  // primitive "string-trim-both" procedure:
  data primitive_STRING_TRIM_BOTH(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    confirm_valid_string_trim_args(args, "string-trim-both",
      "\n     (string-trim-both <string> <optional-predicate>)");
    scm_list right_trim_args;
    right_trim_args.push_back(make_str(*prm_trim_left_of_string(args,env).str));
    if(args.size() == 2)
      right_trim_args.push_back(args[1]);
    return prm_trim_right_of_string(right_trim_args,env);
  }

  // primitive "string-replace" procedure:
  data primitive_STRING_REPLACE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (string-replace <string1> <string2> <start1> <end1>)";
    if(args.size() != 4)
      THROW_ERR("'string-replace recieved incorrect # of args (given " 
        << args.size() << "):" << format 
        << FCN_ERR("string-replace", args));
    if(!args[0].is_type(types::str))
      THROW_ERR("'string-replace 1st arg "<<PROFILE(args[0])<<" isn't a string:" 
        << format << FCN_ERR("string-replace", args));
    if(!args[1].is_type(types::str))
      THROW_ERR("'string-replace 2nd arg "<<PROFILE(args[1])<<" isn't a string:" 
        << format << FCN_ERR("string-replace", args));
    const auto start = primitive_get_if_valid_str_or_vec_idx(args,"string-replace",
                                                             format,"string",2,0,&data::str);
    const auto end   = primitive_get_if_valid_str_or_vec_idx(args,"string-replace",
                                                             format,"string",3,0,&data::str);
    if(end < start)
      THROW_ERR("'string-replace <end> index " << end << " must be greater than <start> index " << start
        <<"\n     for string "<<args[0]<<" of size "<<args[0].str->size()<<'!'
        << FCN_ERR("string-replace", args));
    return make_str(args[0].str->substr(0,start) +  
                    *args[1].str + 
                    args[0].str->substr(end+1));
  }

  // primitive "string-contains" procedure:
  data primitive_STRING_CONTAINS(scm_list& args) {
    return prm_string_contains_template(args, "string-contains", 
      "\n     (string-contains <string> <sub-string>)", true);
  }

  // primitive "string-contains-right" procedure:
  data primitive_STRING_CONTAINS_RIGHT(scm_list& args) {
    return prm_string_contains_template(args, "string-contains-right", 
      "\n     (string-contains-right <string> <sub-string>)", false);
  }

  // primitive "string-join" procedure:
  data primitive_STRING_JOIN(scm_list& args) {
    STRING_GRAMMARS grammar = STRING_GRAMMARS::INFIX;
    scm_string delimiter(""), joined_string("");
    scm_list strings_list;
    confirm_proper_string_join_args(args, grammar, delimiter, strings_list);
    if(!strings_list.empty()) {
      if(grammar == STRING_GRAMMARS::INFIX) {
        joined_string += *strings_list[0].str;
        for(size_type i = 1, n = strings_list.size(); i < n; ++i)
          joined_string += delimiter + *strings_list[i].str;
      } else if(grammar == STRING_GRAMMARS::SUFFIX) {
        for(const auto& data_str : strings_list)
          joined_string += *data_str.str + delimiter;
      } else if(grammar == STRING_GRAMMARS::PREFIX) {
        for(const auto& data_str : strings_list)
          joined_string += delimiter + *data_str.str;
      }
    }
    return make_str(joined_string);
  }

  // primitive "string-split" procedure:
  data primitive_STRING_SPLIT(scm_list& args) {
    scm_string delimiter("");
    scm_list strings_list;
    confirm_proper_string_split_args(args,delimiter);
    // split the string into a list of strings
    const scm_string str(*args[0].str);
    const size_type delim_size = delimiter.size();
    if(!delim_size) {
      for(const auto& letter : str)
        strings_list.push_back(make_str(scm_string(1,letter)));
    } else {
      size_type substr_start = 0;
      for(size_type i = 0, n = str.size(); i < n; ++i) {
        size_type j = 0;
        for(; j < delim_size && i+j < n; ++j)
          if(str[i+j] != delimiter[j]) break;
        if(j == delim_size) { // at a split instance
          strings_list.push_back(make_str(str.substr(substr_start,i-substr_start)));
          i += delim_size-1;
          substr_start = i+1;
        }
      }
      strings_list.push_back(make_str(str.substr(substr_start)));
    }
    return primitive_LIST_to_CONS_constructor(strings_list.begin(),strings_list.end());
  }

  // primitive "string-swap!" procedure:
  data primitive_STRING_SWAP_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (string-swap! <string> <string>)";
    if(args.size() != 2)
      THROW_ERR("'string-swap! recieved incorrect # of args (given " 
        << args.size() << "):" << format 
        << FCN_ERR("string-swap!", args));
    if(!args[0].is_type(types::str))
      THROW_ERR("'string-swap! 1st arg "<<PROFILE(args[0])<<" isn't a string:" 
        << format << FCN_ERR("string-swap!", args));
    if(!args[1].is_type(types::str))
      THROW_ERR("'string-swap! 2nd arg "<<PROFILE(args[1])<<" isn't a string:" 
        << format << FCN_ERR("string-swap!", args));
    scm_string tmp(*args[0].str);
    *args[0].str = *args[1].str;
    *args[1].str = tmp;
    return G::VOID_DATA_OBJECT;
  }

  // primitive "string-push!" procedure:
  data primitive_STRING_PUSH_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (string-push! <string> <char>)";
    if(args.size() != 2)
      THROW_ERR("'string-push! recieved incorrect # of args (given " 
        << args.size() << "):" << format 
        << FCN_ERR("string-push!", args));
    if(!args[0].is_type(types::str))
      THROW_ERR("'string-push! 1st arg "<<PROFILE(args[0])<<" isn't a string:" 
        << format << FCN_ERR("string-push!", args));
    if(!args[1].is_type(types::chr))
      THROW_ERR("'string-push! 2nd arg "<<PROFILE(args[1])<<" isn't a character:" 
        << format << FCN_ERR("string-push!", args));
    args[0].str->push_back(char(args[1].chr));
    return G::VOID_DATA_OBJECT;
  }

  // primitive "string-empty?" procedure:
  data primitive_STRING_EMPTYP(scm_list& args) {
    primitive_confirm_valid_string_arg(args, 1, "string-empty?", "\n     (string-empty? <string>)");
    return data(boolean(args[0].str->empty()));
  }

  // primitive "string-copy!" procedure:
  data primitive_STRING_COPY_BANG(scm_list& args) {
    return primitive_STATIC_SEQUENCE_COPY_BANG_template(args, "string-copy!", 
      "\n     (string-copy! <target-string> <target-start-idx> <source-string>)",
      types::str, "string", &data::str);
  }

  // -------------------
  // String Comparators:
  // -------------------

  // primitive "string=?" procedure:
  data primitive_STRING_EQP(scm_list& args) {
    confirm_given_char_string_args(args, "string=?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str != *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string<?" procedure:
  data primitive_STRING_LT(scm_list& args) {
    confirm_given_char_string_args(args, "string<?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str >= *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string>?" procedure:
  data primitive_STRING_GT(scm_list& args) {
    confirm_given_char_string_args(args, "string>?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str <= *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string<=?" procedure:
  data primitive_STRING_LTE(scm_list& args) {
    confirm_given_char_string_args(args, "string<=?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str > *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string>=?" procedure:
  data primitive_STRING_GTE(scm_list& args) {
    confirm_given_char_string_args(args, "string>=?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str < *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci=?" procedure:
  data primitive_STRING_CI_EQ(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci=?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) != lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci<?" procedure:
  data primitive_STRING_CI_LT(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci<?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) >= lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci>?" procedure:
  data primitive_STRING_CI_GT(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci>?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) <= lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci<=?" procedure:
  data primitive_STRING_CI_LTE(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci<=?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) > lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci>=?" procedure:
  data primitive_STRING_CI_GTE(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci>=?", types::str);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) < lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * SYMBOL-APPEND PRIMITIVE
  ******************************************************************************/

  data primitive_SYMBOL_APPEND(scm_list& args) {
    if(args.empty())
      THROW_ERR("'symbol-append recieved incorrect # of args!"
        "\n     (symbol-append <symbol-1> ... <symbol-N>)" 
        << FCN_ERR("symbol-append", args));
    if(auto idx=confirm_only_args_of_type(args,types::sym);idx!=G::MAX_SIZE_TYPE)
      THROW_ERR("'symbol-append arg #" << idx+1 << ' ' << PROFILE(args[idx])
        <<" isn't a symbol!\n     (symbol-append <symbol-1> ... <symbol-N>)" 
        << FCN_ERR("symbol-append", args));
    scm_string appended_symbol;
    for(size_type i = 0, n = args.size(); i < n; ++i)
      appended_symbol += args[i].sym;
    return appended_symbol;
  }

  /******************************************************************************
  * TYPEOF PRIMITIVE
  ******************************************************************************/

  data primitive_TYPEOF(scm_list& args) {
    if(args.size() != 1)
      THROW_ERR("'typeof recieved incorrect # of args!\n     (typeof <obj>)" 
        << FCN_ERR("typeof", args));
    if(data_is_stream_pair(args[0]))           return "#<stream>";
    if(primitive_data_is_a_procedure(args[0])) return "#<procedure>";
    if(data_is_a_delay(args[0]))               return "#<delay>";
    return args[0].type_name();
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
      THROW_ERR("'cons didn't received 2 arguments: (cons <car-obj> <cdr-obj>)"
        << FCN_ERR("cons", args));
    data new_pair = data(make_par());
    new_pair.par->first = args[0];
    new_pair.par->second = args[1];
    return new_pair;
  }

  // primitive "car" procedure:
  data primitive_CAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "car");
    return args[0].par->first;
  }

  // primitive "cdr" procedure:
  data primitive_CDR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdr");
    return args[0].par->second;
  }

  // primitive "null?" procedure:
  data primitive_NULLP(scm_list& args) {
    confirm_given_one_arg(args,"null?");
    return data(boolean(data_is_the_empty_expression(args[0])));
  }

  // primitive "set-car!" procedure:
  data primitive_SETCAR(scm_list& args) {
    if(args.size() != 2)
      THROW_ERR("'set-car! received incorrect # of arguments:"
        "\n     (set-car! <pair> <obj>)" << FCN_ERR("set-car!", args));
    if(!args[0].is_type(types::par))
      THROW_ERR("'set-car!'s 1st arg "<<PROFILE(args[0])<<" isn't a pair:"
        "\n     (set-car! <pair> <obj>)" << FCN_ERR("set-car!", args));
    args[0].par->first = args[1];
    return G::VOID_DATA_OBJECT;
  }

  // primitive "set-cdr!" procedure:
  data primitive_SETCDR(scm_list& args) {
    if(args.size() != 2)
      THROW_ERR("'set-cdr! received incorrect # of arguments:"
        "\n     (set-cdr! <pair> <obj>)" << FCN_ERR("set-cdr!", args));
    if(!args[0].is_type(types::par))
      THROW_ERR("'set-cdr!'s 1st arg "<<PROFILE(args[0])<<" isn't a pair:"
        "\n     (set-cdr! <pair> <obj>)" << FCN_ERR("set-cdr!", args));
    args[0].par->second = args[1];
    return G::VOID_DATA_OBJECT;
  }

  // primitive "pair-swap!" procedure:
  data primitive_PAIR_SWAP_BANG(scm_list& args) {
    if(args.size() != 2)
      THROW_ERR("'pair-swap! received incorrect # of args (only "
        << args.size() << "):\n     (pair-swap! <pair1> <pair2>)" 
        << FCN_ERR("pair-swap!", args));
    if(!args[0].is_type(types::par))
      THROW_ERR("'pair-swap! 1st arg "<<PROFILE(args[0])<<" isn't a pair: "
        "\n     (pair-swap! <pair1> <pair2>)" << FCN_ERR("pair-swap!",args));
    if(!args[1].is_type(types::par))
      THROW_ERR("'pair-swap! 2nd arg "<<PROFILE(args[1])<<" isn't a pair: "
        "\n     (pair-swap! <pair1> <pair2>)" << FCN_ERR("pair-swap!",args));
    scm_pair tmp(*args[0].par);
    *args[0].par = *args[1].par;
    *args[1].par = tmp;
    return G::VOID_DATA_OBJECT;
  }

  // ---------------------
  // Car/Cdr Combinations:
  // ---------------------

  // primitive "caar" procedure:
  data primitive_CAAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caar");
    confirm_nth_car_is_pair(args[0], "caar", "1st", args);
    return args[0].par->first.par->first;
  }

  // primitive "cadr" procedure:
  data primitive_CADR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cadr");
    confirm_nth_cdr_is_pair(args[0], "cadr", "1st", args);
    return args[0].par->second.par->first;
  }

  // primitive "cdar" procedure:
  data primitive_CDAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdar");
    confirm_nth_car_is_pair(args[0], "cdar", "1st", args);
    return args[0].par->first.par->second;
  }

  // primitive "cddr" procedure:
  data primitive_CDDR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cddr");
    confirm_nth_cdr_is_pair(args[0], "cddr", "1st", args);
    return args[0].par->second.par->second;
  }

  // ----------

  // primitive "caaar" procedure:
  data primitive_CAAAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caaar");
    confirm_nth_car_is_pair(args[0], "caaar", "1st", args);
    confirm_nth_car_is_pair(args[0].par->first, "caaar", "2nd", args);
    return args[0].par->first.par->first.par->first;
  }

  // primitive "caadr" procedure:
  data primitive_CAADR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caadr");
    confirm_nth_cdr_is_pair(args[0], "caadr", "1st", args);
    confirm_nth_car_is_pair(args[0].par->second, "caadr", "1st", args);
    return args[0].par->second.par->first.par->first;
  }

  // primitive "cadar" procedure:
  data primitive_CADAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cadar");
    confirm_nth_car_is_pair(args[0], "cadar", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->first, "cadar", "1st", args);
    return args[0].par->first.par->second.par->first;
  }

  // primitive "caddr" procedure:
  data primitive_CADDR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caddr");
    confirm_nth_cdr_is_pair(args[0], "caddr", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->second, "caddr", "2nd", args);
    return args[0].par->second.par->second.par->first;
  }

  // primitive "cdaar" procedure:
  data primitive_CDAAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdaar");
    confirm_nth_car_is_pair(args[0], "cdaar", "1st", args);
    confirm_nth_car_is_pair(args[0].par->first, "cdaar", "2nd", args);
    return args[0].par->first.par->first.par->second;
  }

  // primitive "cdadr" procedure:
  data primitive_CDADR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdadr");
    confirm_nth_cdr_is_pair(args[0], "cdadr", "1st", args);
    confirm_nth_car_is_pair(args[0].par->second, "cdadr", "1st", args);
    return args[0].par->second.par->first.par->second;
  }

  // primitive "cddar" procedure:
  data primitive_CDDAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cddar");
    confirm_nth_car_is_pair(args[0], "cddar", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->first, "cddar", "1st", args);
    return args[0].par->first.par->second.par->second;
  }

  // primitive "cdddr" procedure:
  data primitive_CDDDR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdddr");
    confirm_nth_cdr_is_pair(args[0], "cdddr", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->second, "cdddr", "2nd", args);
    return args[0].par->second.par->second.par->second;
  }

  // ----------

  // primitive "caaaar" procedure:
  data primitive_CAAAAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caaaar");
    confirm_nth_car_is_pair(args[0], "caaaar", "1st", args);
    confirm_nth_car_is_pair(args[0].par->first, "caaaar", "2nd", args);
    confirm_nth_car_is_pair(args[0].par->first.par->first, "caaaar", "3rd", args);
    return args[0].par->first.par->first.par->first.par->first;
  }

  // primitive "caaadr" procedure:
  data primitive_CAAADR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caaadr");
    confirm_nth_cdr_is_pair(args[0], "caaadr", "1st", args);
    confirm_nth_car_is_pair(args[0].par->second, "caaadr", "1st", args);
    confirm_nth_car_is_pair(args[0].par->second.par->first, "caaadr", "2nd", args);
    return args[0].par->second.par->first.par->first.par->first;
  }

  // primitive "caadar" procedure:
  data primitive_CAADAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caadar");
    confirm_nth_car_is_pair(args[0], "caadar", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->first, "caadar", "1st", args);
    confirm_nth_car_is_pair(args[0].par->first.par->second, "caadar", "2nd", args);
    return args[0].par->first.par->second.par->first.par->first;
  }

  // primitive "caaddr" procedure:
  data primitive_CAADDR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caaddr");
    confirm_nth_cdr_is_pair(args[0], "caaddr", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->second, "caaddr", "2nd", args);
    confirm_nth_car_is_pair(args[0].par->second.par->second, "caaddr", "1st", args);
    return args[0].par->second.par->second.par->first.par->first;
  }

  // primitive "cadaar" procedure:
  data primitive_CADAAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cadaar");
    confirm_nth_car_is_pair(args[0], "cadaar", "1st", args);
    confirm_nth_car_is_pair(args[0].par->first, "cadaar", "2nd", args);
    confirm_nth_cdr_is_pair(args[0].par->first.par->first, "cadaar", "1st", args);
    return args[0].par->first.par->first.par->second.par->first;
  }

  // primitive "cadadr" procedure:
  data primitive_CADADR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cadadr");
    confirm_nth_cdr_is_pair(args[0], "cadadr", "1st", args);
    confirm_nth_car_is_pair(args[0].par->second, "cadadr", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->second.par->first, "cadadr", "2nd", args);
    return args[0].par->second.par->first.par->second.par->first;
  }

  // primitive "caddar" procedure:
  data primitive_CADDAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "caddar");
    confirm_nth_car_is_pair(args[0], "caddar", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->first, "caddar", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->first.par->second, "caddar", "2nd", args);
    return args[0].par->first.par->second.par->second.par->first;
  }

  // primitive "cadddr" procedure:
  data primitive_CADDDR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cadddr");
    confirm_nth_cdr_is_pair(args[0], "cadddr", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->second, "cadddr", "2nd", args);
    confirm_nth_cdr_is_pair(args[0].par->second.par->second, "cadddr", "3rd", args);
    return args[0].par->second.par->second.par->second.par->first;
  }


  // primitive "cdaaar" procedure:
  data primitive_CDAAAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdaaar");
    confirm_nth_car_is_pair(args[0], "cdaaar", "1st", args);
    confirm_nth_car_is_pair(args[0].par->first, "cdaaar", "2nd", args);
    confirm_nth_car_is_pair(args[0].par->first.par->first, "cdaaar", "3rd", args);
    return args[0].par->first.par->first.par->first.par->second;
  }

  // primitive "cdaadr" procedure:
  data primitive_CDAADR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdaadr");
    confirm_nth_cdr_is_pair(args[0], "cdaadr", "1st", args);
    confirm_nth_car_is_pair(args[0].par->second, "cdaadr", "1st", args);
    confirm_nth_car_is_pair(args[0].par->second.par->first, "cdaadr", "2nd", args);
    return args[0].par->second.par->first.par->first.par->second;
  }

  // primitive "cdadar" procedure:
  data primitive_CDADAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdadar");
    confirm_nth_car_is_pair(args[0], "cdadar", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->first, "cdadar", "1st", args);
    confirm_nth_car_is_pair(args[0].par->first.par->second, "cdadar", "2nd", args);
    return args[0].par->first.par->second.par->first.par->second;
  }

  // primitive "cdaddr" procedure:
  data primitive_CDADDR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdaddr");
    confirm_nth_cdr_is_pair(args[0], "cdaddr", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->second, "cdaddr", "2nd", args);
    confirm_nth_car_is_pair(args[0].par->second.par->second, "cdaddr", "1st", args);
    return args[0].par->second.par->second.par->first.par->second;
  }

  // primitive "cddaar" procedure:
  data primitive_CDDAAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cddaar");
    confirm_nth_car_is_pair(args[0], "cddaar", "1st", args);
    confirm_nth_car_is_pair(args[0].par->first, "cddaar", "2nd", args);
    confirm_nth_cdr_is_pair(args[0].par->first.par->first, "cddaar", "1st", args);
    return args[0].par->first.par->first.par->second.par->second;
  }

  // primitive "cddadr" procedure:
  data primitive_CDDADR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cddadr");
    confirm_nth_cdr_is_pair(args[0], "cddadr", "1st", args);
    confirm_nth_car_is_pair(args[0].par->second, "cddadr", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->second.par->first, "cddadr", "2nd", args);
    return args[0].par->second.par->first.par->second.par->second;
  }

  // primitive "cdddar" procedure:
  data primitive_CDDDAR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cdddar");
    confirm_nth_car_is_pair(args[0], "cdddar", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->first, "cdddar", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->first.par->second, "cdddar", "2nd", args);
    return args[0].par->first.par->second.par->second.par->second;
  }

  // primitive "cddddr" procedure:
  data primitive_CDDDDR(scm_list& args) {
    confirm_given_a_pair_arg(args, "cddddr");
    confirm_nth_cdr_is_pair(args[0], "cddddr", "1st", args);
    confirm_nth_cdr_is_pair(args[0].par->second, "cddddr", "2nd", args);
    confirm_nth_cdr_is_pair(args[0].par->second.par->second, "cddddr", "3rd", args);
    return args[0].par->second.par->second.par->second.par->second;
  }

  /******************************************************************************
  * LIST PRIMITIVES
  ******************************************************************************/

  // primitive "list" procedure:
  data primitive_LIST(scm_list& args)noexcept{
    if(args.empty()) return data(symconst::emptylist); // (list) = '()
    return primitive_LIST_to_CONS_constructor(args.begin(), args.end());
  }

  // primitive "list*" procedure:
  data primitive_LIST_STAR(scm_list& args)noexcept{
    if(args.empty()) return data(symconst::emptylist); // (list*) = '()
    return primitive_LIST_STAR_to_CONS_constructor(args.begin(), args.end());
  }

  // primitive "make-list" procedure:
  data primitive_MAKE_LIST(scm_list& args) {
    if(args.size() != 2)
      THROW_ERR("'make-list received incorrect # of args (only "
        << args.size() << "):\n     (make-list <size> <fill-value>)"
        << FCN_ERR("make-list", args));
    if(!primitive_is_valid_size(args[0]))
      THROW_ERR("'make-list didn't receive a proper positive integer size!"
        "\n     (make-list <size> <fill-value>)"
        "\n     <size> range: (0," << G::MAX_SIZE_TYPE << ']' << FCN_ERR("make-list", args));
    // mk a list w/ n copies of the given <fill-value>
    size_type n = (size_type)args[0].num.extract_inexact();
    if(!n) return data(symconst::emptylist);
    scm_list mk_list_args(n, args[1]);
    return primitive_LIST_to_CONS_constructor(mk_list_args.begin(), 
                                              mk_list_args.end());
  }

  // primitive "iota" procedure:
  data primitive_IOTA(scm_list& args) {
    return primitive_IOTA_generic<true>(args,"iota",
      "\n     (iota <count> <optional-start-number> <optional-step-number>)",
      primitive_LIST_to_CONS_constructor<scm_node>);
  }

  // primitive "circular-list" procedure:
  data primitive_CIRCULAR_LIST(scm_list& args)noexcept{
    if(args.empty()) return data(symconst::emptylist);
    return primitive_CIRCULAR_LIST_to_CONS_constructor(args.begin(), args.end());
  }

  // primitive "circular-list?" procedure:
  data primitive_CIRCULAR_LISTP(scm_list& args) {
    confirm_given_one_arg(args,"circular-list?");
    // if not pair, GUARENTEED not a circular list
    if(!args[0].is_type(types::par))
      return G::FALSE_DATA_BOOLEAN;
    if(primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::cyclic)
      return G::TRUE_DATA_BOOLEAN;
    return G::FALSE_DATA_BOOLEAN;
  }

  // primitive "list*?" procedure:
  data primitive_LIST_STARP(scm_list& args) {
    confirm_given_one_arg(args, "list*?");
    // if not pair, GUARENTEED not a dotted list
    if(!args[0].is_type(types::par))
      return G::FALSE_DATA_BOOLEAN;
    if(primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::no_null)
      return G::TRUE_DATA_BOOLEAN;
    return G::FALSE_DATA_BOOLEAN;
  }

  // primitive "list?" procedure:
  //   => where 'list' := finite & null-terminated pair sequence
  data primitive_LISTP(scm_list& args) {
    confirm_given_one_arg(args,"list?");
    if(data_is_proper_list(args[0]))
      return G::TRUE_DATA_BOOLEAN;
    return G::FALSE_DATA_BOOLEAN;
  }

  // primitive "alist?" procedure:
  data primitive_ALISTP(scm_list& args) {
    confirm_given_one_arg(args, "alist?");
    // if not pair, GUARENTEED not an association list
    if(!args[0].is_type(types::par))
      return G::FALSE_DATA_BOOLEAN;
    // valid association lists are finite, terminate with '(), and only contain other pairs
    return boolean(
            (primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::ok) && 
             primitive_list_only_contains_pairs(args[0]));
  }

  // primitive "last-pair" procedure:
  data primitive_LAST_PAIR(scm_list& args) {
    if(primitive_validate_list_and_return_if_empty(args,"last-pair"))
      THROW_ERR("'last-pair 1st arg " << PROFILE(args[0]) 
        << " isn't a proper null-terminated list:\n     (last-pair <non-empty-list>)"
        <<FCN_ERR("last-pair",args));
    return primitive_LAST_iteration(args[0],true);
  }

  // primitive "unfold" procedure:
  data primitive_UNFOLD(scm_list& args) {
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,"unfold",
      "\n     (unfold <break-condition> <map-procedure> <successor-procedure> <seed>)");
    return primitive_LIST_to_CONS_constructor(unfolded.begin(),unfolded.end());
  }

  // primitive "unfold-right" procedure:
  data primitive_UNFOLD_RIGHT(scm_list& args) {
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,"unfold-right",
      "\n     (unfold-right <break-condition> <map-procedure> <successor-procedure> <seed>)");
    return primitive_LIST_to_CONS_constructor(unfolded.rbegin(),unfolded.rend());
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
  data primitive_VECTOR(scm_list& args)noexcept{
    if(args.empty()) return data(make_vec(scm_list()));
    return data(make_vec(scm_list(args.begin(), args.end())));
  }

  // primitive "make-vector" procedure:
  data primitive_MAKE_VECTOR(scm_list& args) {
    // confirm valid length given
    if(args.empty() || args.size() > 2 || !primitive_is_valid_size(args[0]))
      THROW_ERR("'make-vector didn't receive a proper positive integer size!"
        "\n     (make-vector <size> <optional-fill-value>)"
        "\n     <size> range: (0," << G::MAX_SIZE_TYPE << ']' << FCN_ERR("make-vector", args));
    // mk a vector w/ the the given reserve size
    size_type n = (size_type)args[0].num.extract_inexact();
    data vect(make_vec(scm_list(n)));
    // fill vector as needed
    if(args.size() == 2)
      for(size_type i = 0; i < n; ++i)
        vect.vec->operator[](i) = args[1];
    return vect;
  }

  // primitive "vector-push!" procedure:
  data primitive_VECTOR_PUSH_BANG(scm_list& args) {
    primitive_confirm_valid_vector_arg(args, 2, "vector-push!", "\n     (vector-push! <vector> <obj>)");
    args[0].vec->push_back(args[1]);
    return G::VOID_DATA_OBJECT;
  }

  // primitive "vector-iota" procedure:
  data primitive_VECTOR_IOTA(scm_list& args) {
    return primitive_IOTA_generic<false>(args,"vector-iota",
      "\n     (vector-iota <count> <optional-start-number> <optional-step-number>)",
      make_vec);
  }

  // primitive "vector-empty?" procedure:
  data primitive_VECTOR_EMPTYP(scm_list& args) {
    primitive_confirm_valid_vector_arg(args, 1, "vector-empty?", "\n     (vector-empty? <vector>)");
    return data(boolean(args[0].vec->empty()));
  }

  // primitive "vector-grow" procedure:
  data primitive_VECTOR_GROW(scm_list& args) {
    primitive_confirm_valid_vector_arg(args, 2, "vector-grow", "\n     (vector-grow <vector> <size>)");
    if(!primitive_is_valid_size(args[1]))
      THROW_ERR("'vector-grow didn't receive a proper positive integer size!"
        "\n     (vector-grow <vector> <size>)"
        "\n     <size> range: (0," << G::MAX_SIZE_TYPE << ']' << FCN_ERR("vector-grow", args));
    if(args[1].num < args[0].vec->size())
      THROW_ERR("'vector-grow "<<args[1].num.str()<<" is too small to expand "
        << args[0] << " of size " << args[0].vec->size() << " with!"
        "\n     (vector-grow <vector> <size>)"
        "\n     <size> range: (0," << G::MAX_SIZE_TYPE << ']' << FCN_ERR("vector-grow", args));
    if(args[1].num == args[0].vec->size())
      return args[0]; // nothing to expand
    scm_list expanded_vec((size_type)args[1].num.extract_inexact());
    std::copy(args[0].vec->begin(), args[0].vec->end(), expanded_vec.begin());
    return make_vec(expanded_vec);
  }

  // primitive "vector-unfold" procedure:
  data primitive_VECTOR_UNFOLD(scm_list& args) {
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,"vector-unfold",
      "\n     (vector-unfold <break-condition> <map-procedure> <successor-procedure> <seed>)");
    return make_vec(unfolded);
  }

  // primitive "vector-unfold-right" procedure:
  data primitive_VECTOR_UNFOLD_RIGHT(scm_list& args) {
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,"vector-unfold-right",
      "\n     (vector-unfold-right <break-condition> <map-procedure> <successor-procedure> <seed>)");
    return make_vec(scm_list(unfolded.rbegin(),unfolded.rend()));
  }

  // primitive "vector-copy!" procedure:
  data primitive_VECTOR_COPY_BANG(scm_list& args) {
    return primitive_STATIC_SEQUENCE_COPY_BANG_template(args, "vector-copy!", 
      "\n     (vector-copy! <target-vector> <target-start-idx> <source-vector>)",
      types::vec, "vector", &data::vec);
  }

  // primitive "vector-swap!" procedure:
  data primitive_VECTOR_SWAP_BANG(scm_list& args) {
    if(args.size() != 2)
      THROW_ERR("'vector-swap! received incorrect # of args (only "
        << args.size() << "):\n     (vector-swap! <vector1> <vector2>)" 
        << FCN_ERR("vector-swap!", args));
    if(!args[0].is_type(types::vec))
      THROW_ERR("'vector-swap! 1st arg "<<PROFILE(args[0])<<" isn't a vector: "
        "\n     (vector-swap! <vector1> <vector2>)" << FCN_ERR("vector-swap!",args));
    if(!args[1].is_type(types::vec))
      THROW_ERR("'vector-swap! 2nd arg "<<PROFILE(args[1])<<" isn't a vector: "
        "\n     (vector-swap! <vector1> <vector2>)" << FCN_ERR("vector-swap!",args));
    scm_list tmp(*args[0].vec);
    *args[0].vec = *args[1].vec;
    *args[1].vec = tmp;
    return G::VOID_DATA_OBJECT;
  }

  // primitive "vector-binary-search" procedure:
  data primitive_VECTOR_BINARY_SEARCH(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (vector-binary-search <vector> <value> <3-way-comparison>)"
      "\n     ; Suppose values a & b"
      "\n     ; a < b -> (<3-way-comparison> a b) < 0"
      "\n     ; a = b -> (<3-way-comparison> a b) = 0"
      "\n     ; a > b -> (<3-way-comparison> a b) > 0";
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm proper args
    if(args.size() != 3)
      THROW_ERR("'vector-binary-search received incorrect # of args (given "
        << args.size() << "):" << format << FCN_ERR("vector-binary-search", args));
    if(!args[0].is_type(types::vec))
      THROW_ERR("'vector-binary-search 1st arg "<<PROFILE(args[0])<<" isn't a vector:"
        << format << FCN_ERR("vector-binary-search", args));
    primitive_confirm_data_is_a_procedure(args[2], "vector-binary-search", format, args);
    // Perform binary search
    if(args[0].vec->empty()) return G::FALSE_DATA_BOOLEAN;
    const auto& vec   = *args[0].vec;
    const auto& value = args[1];
          auto& proc  = args[2].exp;
    size_type high    = vec.size()-1, 
              low     = 0;
    while (low <= high) {
      const auto mid = low + (high-low)/2; // no overflow on mid
      scm_list bsearch_args(2);
      bsearch_args[0] = vec[mid], bsearch_args[1] = value;
      auto cmp_result = data_cast(execute_application(proc,bsearch_args,env));
      if(!cmp_result.is_type(types::num))
        THROW_ERR("'vector-binary-search result "<<PROFILE(cmp_result)<<
          " from procedure "<<args[2]<<"\n     applied to args "<<vec[mid]
          <<" and "<<value<<" isn't a number:"<< format << FCN_ERR("vector-binary-search", args));
      if(cmp_result.num.is_zero()) // found <value> in <vec>
        return num_type(mid);
      if(cmp_result.num.is_neg())
        low = mid + 1;
      else {
        if(!mid) return G::FALSE_DATA_BOOLEAN;
        high = mid - 1;
      }
    }
    return G::FALSE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * ALGORITHMIC PRIMITIVES: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  // Sequence description for generic algorithm primitives
  #define SEQUENCE_DESCRIPTION\
    "\n     <sequence> = <list> || <vector> || <string>"

  // primitive "empty" procedure (given <sequence>, return its empty version):
  data primitive_EMPTY(scm_list& args) {
    confirm_given_one_sequence_arg(args,"empty");
    if(args[0].is_type(types::par) || data_is_the_empty_expression(args[0]))
      return symconst::emptylist;
    if(args[0].is_type(types::vec)) return make_vec(scm_list());
    if(args[0].is_type(types::str)) return make_str("");
    THROW_ERR("'empty given arg "<<PROFILE(args[0])<<" isn't a proper sequence!" 
        "\n     (empty <sequence>)" SEQUENCE_DESCRIPTION << FCN_ERR("empty",args));
    return G::VOID_DATA_OBJECT; // never used due to the throw above
  }

  // primitive "length" procedure:
  data primitive_LENGTH(scm_list& args) {
    confirm_given_one_sequence_arg(args,"length");
    return primitive_compute_seq_length(args,"length",
      "\n     (length <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "length+" procedure:
  // => return #f on circular lists (instead of error)
  data primitive_LENGTH_PLUS(scm_list& args) {
    confirm_given_one_sequence_arg(args, "length+");
    if(args[0].is_type(types::par) &&
      primitive_list_is_acyclic_and_null_terminated(args[0]) == list_status::cyclic){
      return G::FALSE_DATA_BOOLEAN;
    }
    return primitive_compute_seq_length(args,"length+",
      "\n     (length+ <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "reverse" procedure:
  data primitive_REVERSE(scm_list& args) {
    confirm_given_one_sequence_arg(args,"reverse");
    switch(is_proper_sequence(args[0],args,"reverse",
      "\n     (reverse <sequence>)" SEQUENCE_DESCRIPTION)) {
      case heist_sequence::vec: 
        return primitive_reverse_STATIC_SEQUENCE_logic<scm_list>(args[0],&data::vec,make_vec);
      case heist_sequence::str: 
        return primitive_reverse_STATIC_SEQUENCE_logic<scm_string>(args[0],&data::str,make_str);
      case heist_sequence::nul: 
        return args[0];
      default:
        return primitive_list_reverse_logic(args[0]);
    }
  }

  // primitive "reverse!" procedure:
  data primitive_REVERSE_BANG(scm_list& args) {
    confirm_given_one_sequence_arg(args,"reverse!");
    switch(is_proper_sequence(args[0],args,"reverse!",
      "\n     (reverse! <sequence>)" SEQUENCE_DESCRIPTION)) {
      case heist_sequence::vec:
        return primitive_reverse_bang_STATIC_SEQUENCE_logic(args[0], &data::vec);
      case heist_sequence::str:
        return primitive_reverse_bang_STATIC_SEQUENCE_logic(args[0], &data::str);
      case heist_sequence::nul:
        return G::VOID_DATA_OBJECT;
      default:
        return primitive_list_reverse_bang_logic(args[0]);
    }
  }

  // primitive "fold" procedure:
  data primitive_FOLD(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (fold <procedure> <init> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() < 3) 
      THROW_ERR("'fold received insufficient args (only " 
        << args.size() << "):" << format << FCN_ERR("fold",args));
    primitive_confirm_data_is_a_procedure(args[0], "fold", format, args);
    switch(is_proper_sequence(args[2],args,"fold",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_FOLD_template(args, "fold", 
          format, true, types::vec, "vector", &data::vec, env);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_FOLD_template(args, "fold", 
          format, true, types::str, "string", &data::str, env);
      case heist_sequence::nul:
        return args[1];
      default:
        return primitive_FOLD_template(args, "fold", format, true, env);
    }
  }

  // primitive "fold-right" procedure:
  data primitive_FOLD_RIGHT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (fold-right <procedure> <init> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() < 3) 
      THROW_ERR("'fold-right received insufficient args (only " 
        << args.size() << "):" << format << FCN_ERR("fold-right",args));
    primitive_confirm_data_is_a_procedure(args[0], "fold-right", format, args);
    switch(is_proper_sequence(args[2],args,"fold-right",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_FOLD_template(args, "fold-right", 
          format, false, types::vec, "vector", &data::vec, env);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_FOLD_template(args, "fold-right", 
          format, false, types::str, "string", &data::str, env);
      case heist_sequence::nul:
        return args[1];
      default:
        return primitive_FOLD_template(args, "fold-right", format, false, env);
    }
  }

  // primitive "filter" procedure:
  data primitive_FILTER(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    static constexpr const char * const format = 
      "\n     (filter <procedure> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 2) 
      THROW_ERR("'filter received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("filter",args));
    primitive_confirm_data_is_a_procedure(args[0], "filter", format, args);
    switch(is_proper_sequence(args[1],args,"filter",format)) {
      case heist_sequence::vec:
        return prm_sequence_selective_iteration_template<is_true_scm_condition>(args, 
          types::vec, &data::vec, env);
      case heist_sequence::str:
        return prm_sequence_selective_iteration_template<is_true_scm_condition>(args, 
          types::str, &data::str, env);
      case heist_sequence::nul:
        return args[1];
      default:
        return primitive_list_filter_logic(args, env);
    }
  }

  // primitive "map" procedure:
  data primitive_MAP(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    static constexpr const char * const format = 
      "\n     (map <procedure> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() < 2) 
      THROW_ERR("'map received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("map",args));
    primitive_confirm_data_is_a_procedure(args[0], "map", format, args);
    switch(is_proper_sequence(args[1],args,"map",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_MAP_template(args, "map", 
          format, types::vec, "vector", &data::vec, env);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_MAP_template(args, "map", 
          format, types::str, "string", &data::str, env);
      default:
        return primitive_list_map_logic(args, env, format);
    }
  }

  // primitive "map!" procedure:
  data primitive_MAP_BANG(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    static constexpr const char * const format = 
      "\n     (map! <procedure> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() < 2) 
      THROW_ERR("'map! received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("map!",args));
    primitive_confirm_data_is_a_procedure(args[0], "map!", format, args);
    switch(is_proper_sequence(args[1],args,"map!",format)) {
      case heist_sequence::vec:
        *args[1].vec = *primitive_STATIC_SEQUENCE_MAP_template(args, "map!",
          format, types::vec, "vector", &data::vec, env).vec;
        return G::VOID_DATA_OBJECT;
      case heist_sequence::str:
        *args[1].str = *primitive_STATIC_SEQUENCE_MAP_template(args, "map!",
          format, types::str, "string", &data::str, env).str;
        return G::VOID_DATA_OBJECT;
      default:
        scm_list list_heads(args.begin()+1, args.end());
        primitive_confirm_proper_same_sized_lists(list_heads,"map!",format,1,args);
        primitive_MAP_BANG_list_constructor(list_heads, args[0].exp, env);
        return G::VOID_DATA_OBJECT;
    }
  }

  // primitive "for-each" procedure:
  data primitive_FOR_EACH(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    static constexpr const char * const format = 
      "\n     (for-each <procedure> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() < 2) 
      THROW_ERR("'for-each received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("for-each",args));
    primitive_confirm_data_is_a_procedure(args[0], "for-each", format, args);
    switch(is_proper_sequence(args[1],args,"for-each",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_FOR_EACH_template(args, "for-each", 
          format, types::vec, "vector", &data::vec, env);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_FOR_EACH_template(args, "for-each", 
          format, types::str, "string", &data::str, env);
      default:
        return primitive_list_for_each_logic(args,env,format);
    }
  }

  // primitive "copy" procedure:
  data primitive_COPY(scm_list& args) {
    confirm_given_one_sequence_arg(args,"copy");
    switch(is_proper_sequence(args[0],args,"copy",
      "\n     (copy <sequence>)" SEQUENCE_DESCRIPTION)) {
      case heist_sequence::vec: return make_vec(*args[0].vec);
      case heist_sequence::str: return make_str(*args[0].str);
      case heist_sequence::nul: return data(symconst::emptylist);
      default:                  return primitive_list_copy_logic(args[0]);
    }
  }

  // primitive "copy!" procedure: 
  // NOTE: Copies elts from <source> over <dest>'s elts. 
  //       <dest>.size() is unaffected.
  data primitive_COPY_BANG(scm_list& args) { // 
    static constexpr const char * const format = 
      "\n     (copy! <dest-sequence> <source-sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 2)
      THROW_ERR("'copy! received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("copy!",args));
    if(args[0].type != args[1].type)
      THROW_ERR("'copy! args have mismatched types!" 
        << format << FCN_ERR("copy!",args));
    switch(is_proper_sequence(args[0],args,"copy!",format)) {
      case heist_sequence::vec: return primitive_generic_STATIC_CONTAINER_copy_bang_logic(args[0].vec,args[1].vec);
      case heist_sequence::str: return primitive_generic_STATIC_CONTAINER_copy_bang_logic(args[0].str,args[1].str);
      case heist_sequence::nul: return data(types::dne);
      default:                  return primitive_generic_list_copy_bang_logic(args[0],args[1]);
    }
  }

  // primitive "count" procedure:
  data primitive_COUNT(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    static constexpr const char * const format = 
      "\n     (count <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 2) 
      THROW_ERR("'count received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("count",args));
    primitive_confirm_data_is_a_procedure(args[0], "count", format, args);
    switch(is_proper_sequence(args[1],args,"count",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_COUNT_template(args, &data::vec, env);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_COUNT_template(args, &data::str, env);
      case heist_sequence::nul:
        return num_type();
      default:
        return primitive_list_count_logic(args,env);
    }
  }

  // primitive "ref" procedure:
  data primitive_REF(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (ref <sequence> <index>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 2) 
      THROW_ERR("'ref received incorrect # of args (given " 
        << args.size() << "):" << format 
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("ref",args));
    switch(is_proper_sequence(args[0],args,"ref",format)) {
      case heist_sequence::nul:
        THROW_ERR("'ref 1st arg '() of type \"null\" has no elements to reference:" 
          << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("ref",args));
      case heist_sequence::vec:
        return args[0].vec->operator[](primitive_get_if_valid_vector_idx(args,"ref",format));
      case heist_sequence::str:
        return args[0].str->operator[](primitive_get_if_valid_string_idx(args,"ref",format));
      default:
        if(!primitive_is_valid_index(args[1])) 
          THROW_ERR("'ref <list> 2nd arg " << PROFILE(args[1]) << " is an invalid <index>:"
            << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("ref",args));
        return primitive_list_ref_seeker(args[0],(size_type)args[1].num.extract_inexact(),format,args);
    }
  }

  // primitive "slice" procedure (generic 'sublist 'subvector 'substring):
  data primitive_SLICE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (slice <sequence> <start-index> <optional-length>)" SEQUENCE_DESCRIPTION;
    if(args.size() < 2 || args.size() > 3)
      THROW_ERR("'slice received incorrect # of args (given " 
        << args.size() << "):"<<format<<VALID_SEQUENCE_INDEX_RANGE<<FCN_ERR("slice",args));
    switch(is_proper_sequence(args[0],args,"slice",format)) {
      case heist_sequence::vec: return primitive_subvector_extraction(args,format);
      case heist_sequence::str: return primitive_substring_logic(args,format);
      default:                  return primitive_sublist_extraction(args,format);
    }
  }

  // primitive "set-index!" procedure ('vector-set! 'string-set! SRFIs):
  data primitive_SET_INDEX_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (set-index! <sequence> <index> <obj>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 3) 
      THROW_ERR("'set-index! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("set-index!",args));
    switch(is_proper_sequence(args[0],args,"set-index!",format)) {
      case heist_sequence::nul:
        THROW_ERR("'set-index! 1st arg '() of type \"null\" has no elements to set:" 
          << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("set-index!",args));
      case heist_sequence::vec:
        args[0].vec->operator[](primitive_get_if_valid_vector_idx(args,"set-index!",format)) = args[2];
        return G::VOID_DATA_OBJECT;
      case heist_sequence::str:
        if(!args[2].is_type(types::chr))
          THROW_ERR("'set-index! <string> for "<<PROFILE(args[0])<<" received non-character set-value\n     " 
            << PROFILE(args[2]) << '!' << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("set-index!",args));
        args[0].str->operator[](primitive_get_if_valid_string_idx(args,"set-index!",format)) = args[2].chr;
        return G::VOID_DATA_OBJECT;
      default:
        if(!primitive_is_valid_index(args[1])) 
          THROW_ERR("'set-index! <list> 2nd arg " << PROFILE(args[1]) << " is an invalid <index>:"
            << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("set-index!",args));
        primitive_list_set_index_applicator(args[0],(size_type)args[1].num.extract_inexact(),format,args);
        return G::VOID_DATA_OBJECT;
    }
  }

  // primitive "swap-indices!" procedure:
  data primitive_SWAP_INDICES_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (swap-indices! <sequence> <index> <index>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 3) 
      THROW_ERR("'swap-indices! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("swap-indices!",args));
    switch(is_proper_sequence(args[0],args,"swap-indices!",format)) {
      case heist_sequence::nul:
        THROW_ERR("'swap-indices! 1st arg '() of type \"null\" has no elements to swap:" 
          << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("swap-indices!",args));
      case heist_sequence::vec:
        std::swap(args[0].vec->operator[](primitive_get_if_valid_vector_idx(args,"swap-indices!",format)), 
                  args[0].vec->operator[](primitive_get_if_valid_vector_idx(args,"swap-indices!",format,2)));
        return G::VOID_DATA_OBJECT;
      case heist_sequence::str:
        std::swap(args[0].str->operator[](primitive_get_if_valid_string_idx(args,"swap-indices!",format)), 
                  args[0].str->operator[](primitive_get_if_valid_string_idx(args,"swap-indices!",format,2)));
        return G::VOID_DATA_OBJECT;
      default:
        primitive_list_swap_indices_logic(args,format);
        return G::VOID_DATA_OBJECT;
    }
  }

  // primitive "fill!" procedure:
  data primitive_FILL_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (fill! <sequence> <fill-value>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 2) 
      THROW_ERR("'fill! received incorrect # of args (given " 
        << args.size() << "):" << format 
        << FCN_ERR("fill!",args));
    switch(is_proper_sequence(args[0],args,"fill!",format)) {
      case heist_sequence::vec: return primitive_vector_fill_logic(args);
      case heist_sequence::str: return primitive_string_fill_logic(args,format);
      default:                  return primitive_list_fill_logic(args[0],args[1]);
    }
  }

  // primitive "append" procedure:
  // (append) = '()
  // (append <obj>) = <obj>
  // (append <empty-sequence1> ... <empty-sequenceN> <obj>) = <obj>
  // (append <sequence1> ... <sequenceN> <obj>) = <sequence1> | ... | <sequenceN> | <obj>
  data primitive_APPEND(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (append <sequence1> ... <sequenceN> <obj>)" SEQUENCE_DESCRIPTION;
    if(args.empty())     return data(symconst::emptylist);
    if(args.size() == 1) return args[0];
    switch(is_proper_sequence(args[0],args,"append",format)) {
      case heist_sequence::vec: return primitive_vector_append_logic(args,format);
      case heist_sequence::str: return primitive_string_append_logic(args,format);
      default:                  return primitive_list_append_logic(args,format);
    }
  }

  // primitive "remove" procedure:
  data primitive_REMOVE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (remove <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 2) 
      THROW_ERR("'remove received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("remove",args));
    primitive_confirm_data_is_a_procedure(args[0], "remove", format, args);
    switch(is_proper_sequence(args[1],args,"remove",format)) {
      case heist_sequence::nul:
        return args[1];
      case heist_sequence::vec:
        return prm_sequence_selective_iteration_template<is_false_scm_condition>(args, 
          types::vec, &data::vec, env);
      case heist_sequence::str:
        return prm_sequence_selective_iteration_template<is_false_scm_condition>(args, 
          types::str, &data::str, env);
      default:
        return primitive_remove_list_logic(args[1],args[0].exp,env);
    }
  }

  // primitive "remove-first" procedure:
  data primitive_REMOVE_FIRST(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (remove-first <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 2) 
      THROW_ERR("'remove-first received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("remove-first",args));
    primitive_confirm_data_is_a_procedure(args[0], "remove-first", format, args);
    switch(is_proper_sequence(args[1],args,"remove-first",format)) {
      case heist_sequence::nul:
        return args[1];
      case heist_sequence::vec:
        return make_vec(prm_remove_first_or_last<true>(args[0].exp,*args[1].vec,env));
      case heist_sequence::str:
        return make_str(prm_remove_first_or_last<true>(args[0].exp,*args[1].str,env));
      default:
        scm_list par_as_exp;
        shallow_unpack_list_into_exp(args[1],par_as_exp);
        par_as_exp = prm_remove_first_or_last<true>(args[0].exp,par_as_exp,env);
        return primitive_LIST_to_CONS_constructor(par_as_exp.begin(),par_as_exp.end());
    }
  }

  // primitive "remove-last" procedure:
  data primitive_REMOVE_LAST(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (remove-last <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 2) 
      THROW_ERR("'remove-last received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("remove-last",args));
    primitive_confirm_data_is_a_procedure(args[0], "remove-last", format, args);
    switch(is_proper_sequence(args[1],args,"remove-last",format)) {
      case heist_sequence::nul:
        return args[1];
      case heist_sequence::vec:
        return make_vec(prm_remove_first_or_last<false>(args[0].exp,*args[1].vec,env));
      case heist_sequence::str:
        return make_str(prm_remove_first_or_last<false>(args[0].exp,*args[1].str,env));
      default:
        scm_list par_as_exp;
        shallow_unpack_list_into_exp(args[1],par_as_exp);
        par_as_exp = prm_remove_first_or_last<false>(args[0].exp,par_as_exp,env);
        return primitive_LIST_to_CONS_constructor(par_as_exp.begin(),par_as_exp.end());
    }
  }

  // primitive "delete" procedure:
  data primitive_DELETE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (delete <sequence> <index>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 2) 
      THROW_ERR("'delete received incorrect # of args (given " 
        << args.size() << "):" << format 
        << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("delete",args));
    switch(is_proper_sequence(args[0],args,"delete",format)) {
      case heist_sequence::nul:
        THROW_ERR("'delete 1st arg '() of type \"null\" has no elements to reference:" 
          << format << VALID_SEQUENCE_INDEX_RANGE << FCN_ERR("delete",args));
      case heist_sequence::vec:
        return make_vec(primitive_delete_STATIC_SEQUENCE_logic<primitive_get_if_valid_vector_idx>(
                  args,format,&data::vec));
      case heist_sequence::str:
        return make_str(primitive_delete_STATIC_SEQUENCE_logic<primitive_get_if_valid_string_idx>(
                  args,format,&data::str));
      default:
        return primitive_list_delete_logic(args,format);
    }
  }

  // primitive "last" procedure (last elt):
  data primitive_LAST(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (last <sequence>)" SEQUENCE_DESCRIPTION;
      confirm_given_one_sequence_arg(args,"last");
    if(data_is_empty(args[0]))
      THROW_ERR("'last empty sequence arg "<<PROFILE(args[0])<<" has no last elt!" 
        << format << FCN_ERR("last",args));
    switch(is_proper_sequence(args[0],args,"last",format)) {
      case heist_sequence::vec: return *args[0].vec->rbegin();
      case heist_sequence::str: return *args[0].str->rbegin();
      default:                  return primitive_list_last_logic(args[0]);
    }
  }

  // primitive "tail" procedure (all except head):
  data primitive_TAIL(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (tail <sequence>)" SEQUENCE_DESCRIPTION;
    confirm_given_one_sequence_arg(args,"tail");
    if(data_is_empty(args[0]))
      THROW_ERR("'tail empty sequence arg "<<PROFILE(args[0])<<" has no tail!" 
        << format << FCN_ERR("tail",args));
    switch(is_proper_sequence(args[0],args,"tail",format)) {
      case heist_sequence::vec:
        return make_vec(scm_list(args[0].vec->begin()+1,args[0].vec->end()));
      case heist_sequence::str:
        return make_str(scm_string(args[0].str->begin()+1,args[0].str->end()));
      default:
        return primitive_list_copy_logic(args[0].par->second);
    }
  }

  // primitive "head" procedure (first elt):
  data primitive_HEAD(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (head <sequence>)" SEQUENCE_DESCRIPTION;
    confirm_given_one_sequence_arg(args,"head");
    if(data_is_empty(args[0]))
      THROW_ERR("'head empty sequence arg "<<PROFILE(args[0])<<" has no 1st elt!" 
        << format << FCN_ERR("head",args));
    switch(is_proper_sequence(args[0],args,"head",format)) {
      case heist_sequence::vec: return *args[0].vec->begin();
      case heist_sequence::str: return *args[0].str->begin();
      default:                  return args[0].par->first;
    }
  }

  // primitive "init" procedure (all except last):
  data primitive_INIT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (init <sequence>)" SEQUENCE_DESCRIPTION;
    confirm_given_one_sequence_arg(args,"init");
    if(data_is_empty(args[0]))
      THROW_ERR("'init empty sequence arg "<<PROFILE(args[0])<<" has no init!" 
        << format << FCN_ERR("init",args));
    switch(is_proper_sequence(args[0],args,"init",format)) {
      case heist_sequence::vec:
        return make_vec(scm_list(args[0].vec->begin(),args[0].vec->end()-1));
      case heist_sequence::str:
        return make_str(scm_string(args[0].str->begin(),args[0].str->end()-1));
      default:
        return primitive_list_init_logic(args[0]);
    }
  }

  // primitive "seq=" procedure:
  data primitive_SEQ_EQ(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (seq= <elt=?> <sequence1> <sequence2> ...)"
      SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      THROW_ERR("'seq= didn't recieve any args:" 
        << format << FCN_ERR("seq=", args));
    primitive_confirm_data_is_a_procedure(args[0], "seq=", format, args);
    switch(is_proper_sequence(args[1],args,"seq=",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_sequence_eq_logic(args,format,env,types::vec,&data::vec);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_sequence_eq_logic(args,format,env,types::str,&data::str);
      default:
        return primitive_list_sequence_eq_logic(args,format,env);
    }
  }

  // primitive "skip" procedure:
  data primitive_SKIP(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (skip <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 2) 
      THROW_ERR("'skip received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("skip",args));
    primitive_confirm_data_is_a_procedure(args[0], "skip", format, args);
    switch(is_proper_sequence(args[1],args,"skip",format)) {
      case heist_sequence::nul: 
        return G::FALSE_DATA_BOOLEAN;
      case heist_sequence::vec:
        return prm_search_STATIC_SEQUENCE_from_left<is_false_scm_condition>(args,&data::vec,env);
      case heist_sequence::str:
        return prm_search_STATIC_SEQUENCE_from_left<is_false_scm_condition>(args,&data::str,env);
      default:
        return prm_search_list_from_left<is_false_scm_condition>(args[1],args[0].exp,env);
    }
  }

  // primitive "skip-right" procedure:
  data primitive_SKIP_RIGHT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (skip-right <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 2) 
      THROW_ERR("'skip-right received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("skip-right",args));
    primitive_confirm_data_is_a_procedure(args[0], "skip-right", format, args);
    switch(is_proper_sequence(args[1],args,"skip-right",format)) {
      case heist_sequence::nul: 
        return args[1];
      case heist_sequence::vec:
        return prm_search_STATIC_SEQUENCE_from_right<is_false_scm_condition>(args,&data::vec,env);
      case heist_sequence::str:
        return prm_search_STATIC_SEQUENCE_from_right<is_false_scm_condition>(args,&data::str,env);
      default:
        return prm_search_list_from_right<is_false_scm_condition>(args,env);
    }
  }

  // primitive "index" procedure:
  data primitive_INDEX(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (index <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 2) 
      THROW_ERR("'index received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("index",args));
    primitive_confirm_data_is_a_procedure(args[0], "index", format, args);
    switch(is_proper_sequence(args[1],args,"index",format)) {
      case heist_sequence::nul:
        return G::FALSE_DATA_BOOLEAN;
      case heist_sequence::vec:
        return prm_search_STATIC_SEQUENCE_from_left<is_true_scm_condition>(args,&data::vec,env);
      case heist_sequence::str:
        return prm_search_STATIC_SEQUENCE_from_left<is_true_scm_condition>(args,&data::str,env);
      default:
        return prm_search_list_from_left<is_true_scm_condition>(args[1],args[0].exp,env);
    }
  }

  // primitive "index-right" procedure:
  data primitive_INDEX_RIGHT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (index-right <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 2) 
      THROW_ERR("'index-right received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("index-right",args));
    primitive_confirm_data_is_a_procedure(args[0], "index-right", format, args);
    switch(is_proper_sequence(args[1],args,"index-right",format)) {
      case heist_sequence::nul:
        return args[1];
      case heist_sequence::vec:
        return prm_search_STATIC_SEQUENCE_from_right<is_true_scm_condition>(args,&data::vec,env);
      case heist_sequence::str:
        return prm_search_STATIC_SEQUENCE_from_right<is_true_scm_condition>(args,&data::str,env);
      default:
        return prm_search_list_from_right<is_true_scm_condition>(args,env);
    }
  }

  // primitive "drop" procedure:
  data primitive_DROP(scm_list& args) {
    return primitive_take_drop_template 
      <primitive_drop_GENERIC_logic<false>, 
       primitive_drop_GENERIC_logic<false>, 
       primitive_drop_GENERIC_logic<true>> 
      (args, "drop", "\n     (drop <sequence> <length>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "drop-right" procedure:
  data primitive_DROP_RIGHT(scm_list& args) {
    return primitive_take_drop_template 
      <primitive_drop_right_GENERIC_logic<false>, 
       primitive_drop_right_GENERIC_logic<false>, 
       primitive_drop_right_GENERIC_logic<true>> 
      (args, "drop-right", "\n     (drop-right <sequence> <length>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "take" procedure:
  data primitive_TAKE(scm_list& args) {
    return primitive_take_drop_template 
      <primitive_take_GENERIC_logic<false>, 
       primitive_take_GENERIC_logic<false>, 
       primitive_take_GENERIC_logic<true>> 
      (args, "take", "\n     (take <sequence> <length>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "take-right" procedure:
  data primitive_TAKE_RIGHT(scm_list& args) {
    return primitive_take_drop_template 
      <primitive_take_right_GENERIC_logic<false>, 
       primitive_take_right_GENERIC_logic<false>, 
       primitive_take_right_GENERIC_logic<true>> 
      (args, "take-right", "\n     (take-right <sequence> <length>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "drop-while" procedure:
  data primitive_DROP_WHILE(scm_list& args) {
    return primitive_take_drop_while_template
      <primitive_drop_while_GENERIC_logic<false>, 
       primitive_drop_while_GENERIC_logic<false>, 
       primitive_drop_while_GENERIC_logic<true>> 
      (args, "drop-while", "\n     (drop-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "drop-right-while" procedure:
  data primitive_DROP_RIGHT_WHILE(scm_list& args) {
    return primitive_take_drop_while_template 
      <primitive_drop_right_while_GENERIC_logic<false>, 
       primitive_drop_right_while_GENERIC_logic<false>, 
       primitive_drop_right_while_GENERIC_logic<true>> 
      (args, "drop-right-while", "\n     (drop-right-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "take-while" procedure:
  data primitive_TAKE_WHILE(scm_list& args) {
    return primitive_take_drop_while_template 
      <primitive_take_while_GENERIC_logic<false>, 
       primitive_take_while_GENERIC_logic<false>, 
       primitive_take_while_GENERIC_logic<true>> 
      (args, "take-while", "\n     (take-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "take-right-while" procedure:
  data primitive_TAKE_RIGHT_WHILE(scm_list& args) {
    return primitive_take_drop_while_template 
      <primitive_take_right_while_GENERIC_logic<false>, 
       primitive_take_right_while_GENERIC_logic<false>, 
       primitive_take_right_while_GENERIC_logic<true>> 
      (args, "take-right-while", "\n     (take-right-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "any" procedure:
  data primitive_ANY(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    static constexpr const char * const format = 
      "\n     (any <predicate> <sequence1> <sequence2> ...)" 
      SEQUENCE_DESCRIPTION;
    if(args.size() < 2) 
      THROW_ERR("'any received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("any",args));
    primitive_confirm_data_is_a_procedure(args[0], "any", format, args);
    switch(is_proper_sequence(args[1],args,"any",format)) {
      case heist_sequence::nul:
        return G::FALSE_DATA_BOOLEAN;
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_any_logic<types::vec>(args,env,&data::vec,"vector",format);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_any_logic<types::str>(args,env,&data::str,"string",format);
      default:
        return primitive_list_any_logic(args,env,format);
    }
  }

  // primitive "every" procedure:
  data primitive_EVERY(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    static constexpr const char * const format = 
      "\n     (every <predicate> <sequence1> <sequence2> ...)" 
      SEQUENCE_DESCRIPTION;
    if(args.size() < 2) 
      THROW_ERR("'every received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("every",args));
    primitive_confirm_data_is_a_procedure(args[0], "every", format, args);
    switch(is_proper_sequence(args[1],args,"every",format)) {
      case heist_sequence::nul:
        return G::FALSE_DATA_BOOLEAN;
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_every_logic<types::vec>(args,env,&data::vec,"vector",format);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_every_logic<types::str>(args,env,&data::str,"string",format);
      default:
        return primitive_list_every_logic(args,env,format);
    }
  }

  // primitive "conj" procedure:
  data primitive_CONJ(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (conj <obj> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() != 2)
      THROW_ERR("'conj received incorrect # of arguments:" 
        << format << FCN_ERR("conj",args));
    scm_list new_vec;
    switch(is_proper_sequence(args[1],args,"conj",format)) {
      case heist_sequence::vec:
        new_vec.insert(new_vec.end(),args[1].vec->begin(),args[1].vec->end());
        new_vec.push_back(args[0]);
        return make_vec(new_vec);
      case heist_sequence::str:
        if(!args[0].is_type(types::chr))
          THROW_ERR("'conj <string> 1st arg "<<PROFILE(args[0])<<" isn't a character:"
            << format << FCN_ERR("conj",args));
        return make_str((*args[1].str) + char(args[0].chr));
      default:
        data new_pair = data(make_par());
        new_pair.par->first = args[0];
        new_pair.par->second = args[1];
        return new_pair;
    }
  }

  /******************************************************************************
  * SORTING PRIMITIVES: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  // primitive "sort" procedure:
  data primitive_SORT(scm_list& args) {
    constexpr const char * const format = 
      "\n     (sort <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm has a valid argument signature
    primitive_confirm_sortable_sequence(args, "sort", format);
    // return if sorting the empty list
    if(args[1].is_type(types::sym)) return args[1];
    // sort the sequence
    return primitive_sort_sequence(args, env, "sort", format);
  }

  // primitive "sort!" procedure:
  data primitive_SORT_BANG(scm_list& args) {
    constexpr const char * const format = 
      "\n     (sort! <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm has a valid argument signature
    primitive_confirm_sortable_sequence(args, "sort!", format);
    // return if sorting the empty list (already sorted)
    if(args[1].is_type(types::sym)) return G::VOID_DATA_OBJECT;
    // set the sequence to its sorted variant
    return mutatable_assign_scm_sequence(args[1],
      primitive_sort_sequence(args,env,"sort!",format));
  }

  // primitive "sorted?" procedure:
  data primitive_SORTEDP(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm has a valid argument signature
    primitive_confirm_sortable_sequence(args, "sorted?", 
      "\n     (sorted? <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
    // return if sorting the empty list
    if(args[1].is_type(types::sym)) return G::TRUE_DATA_BOOLEAN;
    // unpack the sequence
    scm_list sequence;
    cast_scheme_sequence_to_ast(args[1],sequence);
    // confirm the unpacked sequence is sorted as per the args[0] procedure
    if(sequence.size() > 1) {
      auto& procedure = args[0].exp;
      for(size_type i = 0, n = sequence.size(); i+1 < n; ++i) {
        scm_list args_list(2);
        args_list[0] = sequence[i], args_list[1] = sequence[i+1];
        if(is_false_scm_condition(procedure,args_list,env))
          return G::FALSE_DATA_BOOLEAN;
      }
    }
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "merge" procedure:
  data primitive_MERGE(scm_list& args) { 
    constexpr const char * const format = 
      "\n     (merge <predicate> <sequence1> <sequence2>)"
      SEQUENCE_DESCRIPTION;
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm given correct # of args needed
    if(args.size() != 3) 
      THROW_ERR("'merge received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("merge",args));
    // Confirm given a procedure
    primitive_confirm_data_is_a_procedure(args[0], "merge", format, args);
    // Confirm given only proper lists, only vectors, or only strings
    is_proper_sequence(args[1],args,"merge",format);
    is_proper_sequence(args[2],args,"merge",format);
    // If given '() and a proper list, return the proper list
    if((args[1].is_type(types::sym) && args[2].is_type(types::par)) || 
       (args[2].is_type(types::sym) && args[1].is_type(types::par)))
      return args[1].is_type(types::sym) ? args[2] : args[1];
    // Confirm given sequences are either 2 proper lists, vectors, or strings
    if(args[1].type != args[2].type)
      THROW_ERR("'merge sequences " << PROFILE(args[1]) << " and "
        << PROFILE(args[2]) << "\n     are not matching sequence types!"
        << format << FCN_ERR("merge",args));
    // If merging vectors or strings: 
    scm_list merged_sequence;
    if(!args[1].is_type(types::par)) 
      return primitive_MERGE_vector_string_constructor(args,merged_sequence,env,format);
    // Else apply the procedure on each list elt & merge args as per the result into a list
    scm_list list_heads(args.begin()+1, args.end());
    primitive_MERGE_list_constructor(list_heads,args[0].exp,merged_sequence,env);
    return primitive_LIST_to_CONS_constructor(merged_sequence.begin(),merged_sequence.end());
  }

  // primitive "delete-neighbor-dups" procedure:
  data primitive_DELETE_NEIGHBOR_DUPS(scm_list& args) {
    return primitive_DELETE_NEIGHBOR_DUPS_template(args,"delete-neighbor-dups",
      "\n     (delete-neighbor-dups <equality-predicate> <sequence>)"
      SEQUENCE_DESCRIPTION,false);
  }

  // primitive "delete-neighbor-dups!" procedure:
  data primitive_DELETE_NEIGHBOR_DUPS_BANG(scm_list& args) {
    return primitive_DELETE_NEIGHBOR_DUPS_template(args,"delete-neighbor-dups!",
      "\n     (delete-neighbor-dups! <equality-predicate> <sequence>)"
      SEQUENCE_DESCRIPTION,true);
  }

  #undef SEQUENCE_DESCRIPTION // End of generic algorithm primitives

  /******************************************************************************
  * TYPE-CHECKING PRIMITIVES
  ******************************************************************************/

  // primitive "void" procedure:
  data primitive_VOID(scm_list&)noexcept{return G::VOID_DATA_OBJECT;}

  // primitive "void?" procedure:
  data primitive_VOIDP(scm_list& args) { 
    confirm_given_one_arg(args, "void?");
    return data(boolean(args[0].is_type(types::dne)));
  }

  // primitive "undefined" procedure:
  data primitive_UNDEFINED(scm_list&)noexcept{return data();}

  // primitive "undefined?" procedure:
  data primitive_UNDEFINEDP(scm_list& args) { 
    confirm_given_one_arg(args, "undefined?");
    return data(boolean(args[0].is_type(types::undefined)));
  }

  // primitive "empty?" procedure:
  data primitive_EMPTYP(scm_list& args) {
    confirm_given_one_arg(args, "empty?");
    return data(boolean(data_is_empty(args[0])));
  }

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
    return data(boolean(args[0].is_type(types::num) && !args[0].num.is_nan()));
  }

  // primitive "real?" procedure:
  // => "real" denotes a # that's neither NaN nor Inf
  data primitive_REALP(scm_list& args) {
    confirm_given_one_arg(args, "real?");
    return data(boolean(
      args[0].is_type(types::num) && args[0].num.is_real()));
  }

  // primitive "rational?" procedure:
  // => "rational" denotes a # that inexact->exact won't approximate
  data primitive_RATIONALP(scm_list& args) {
    confirm_given_one_arg(args, "rational?");
    return data(boolean(
      args[0].is_type(types::num) && args[0].num.is_rational()));
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
                        args[0].sym != symconst::emptylist));
  }

  // primitive "boolean?" procedure:
  data primitive_BOOLEANP(scm_list& args) {
    confirm_given_one_arg(args, "boolean?");
    return data(boolean(args[0].is_type(types::bol)));
  }

  // primitive "atom?" procedure:
  data primitive_ATOMP(scm_list& args) {
    confirm_given_one_arg(args, "atom?");
    return data(boolean(!primitive_PAIRP(args).bol.val));
  }

  // primitive "procedure?" procedure:
  data primitive_PROCEDUREP(scm_list& args) {
    confirm_given_one_arg(args, "procedure?");
    return data(boolean(primitive_data_is_a_procedure(args[0])));
  }

  // primitive "cps-procedure?" procedure:
  data primitive_CPS_PROCEDUREP(scm_list& args) {
    confirm_given_one_arg(args, "cps-procedure?");
    return data(boolean(args[0].is_type(types::exp) && args[0].exp[0].is_type(types::sym) && 
      args[0].exp[0].sym == symconst::procedure && !args[0].exp[1].exp.empty() && 
      data_is_continuation_parameter(*args[0].exp[1].exp.rbegin())));
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
    return data(boolean(args[0].is_type(types::chr) && args[0].chr==EOF));
  }

  // primitive "stream-pair?" procedure:
  data primitive_STREAM_PAIRP(scm_list& args) {
    confirm_given_one_arg(args, "stream-pair?");
    return data(boolean(args[0].is_type(types::par) && 
                        data_is_a_delay(args[0].par->first) && 
                        data_is_a_delay(args[0].par->second)));
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
                         data_is_a_delay(args[0].par->first) && 
                         data_is_a_delay(args[0].par->second))));
  }

  // primitive "syntax-rules-object?" procedure:
  data primitive_SYNTAX_RULES_OBJECTP(scm_list& args) {
    confirm_given_one_arg(args, "syntax-rules-object?");
    return data(boolean(args[0].is_type(types::syn)));
  }

  // primitive "seq?" procedure:
  data primitive_SEQP(scm_list& args) {
    confirm_given_one_arg(args, "seq?");
    return data(boolean(args[0].is_type(types::vec) || 
                        args[0].is_type(types::str) || 
                        data_is_proper_list(args[0])));
  }

  /******************************************************************************
  * EVAL PRIMITIVE
  ******************************************************************************/

  // primitive "eval" procedure:
  data primitive_EVAL(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (eval <data> <optional-environment>)" 
      "\n     -> Pass 'null-environment to eval in the empty environment!"
      "\n     -> Pass 'local-environment to eval in the local environment!"
      "\n     -> Pass 'global-environment to eval in the global environment (default)!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    // use the initial/local environment if passed 'null-environment or
    //   'local-environment as a 2nd arg
    auto original_global_env = G::GLOBAL_ENVIRONMENT_POINTER;
    auto env = G::GLOBAL_ENVIRONMENT_POINTER;
    bool must_reset_global_env = false;
    prm_EVAL_confirm_correct_number_of_args(args,must_reset_global_env,local_env,env,
                                                   original_global_env,"eval",format);
    // if arg is self-evaluating, return arg
    if(prm_EVAL_data_is_self_evaluating(args[0])) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      return args[0];
    }
    // if arg is a symbol, eval the symbol
    if(args[0].is_type(types::sym)) {
      // confirm arg is not '()
      if(args[0].sym == symconst::emptylist) {
        if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
        THROW_ERR("'eval can't evaluate '() (nil to eval):" << format << FCN_ERR("eval", args));
      }
      try {
        auto result = data_cast(scm_eval(scm_list(1, args[0]),env));
        if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
        return result;
      } catch(const SCM_EXCEPT& eval_throw) {
        if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
        throw eval_throw;
      }
      return G::VOID_DATA_OBJECT;
    }
    // else confirm arg is a proper list
    if(!args[0].is_type(types::par)) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      THROW_ERR("'eval didn't receive an evaluable expression:\n     "<<PROFILE(args[0])
        << format << FCN_ERR("eval", args));
    }
    if(!data_is_proper_list(args[0])) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      THROW_ERR("'eval received an improper list: "<<PROFILE(args[0])
        << format << FCN_ERR("eval", args));
    }
    // eval list contents
    try {
      auto result = data_cast(scm_eval(prm_EVAL_convert_list_to_AST(args[0]),env));
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      return result;
    } catch(const SCM_EXCEPT& eval_throw) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      throw eval_throw;
    }
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * CPS-EVAL PRIMITIVE
  ******************************************************************************/

  // primitive "cps-eval" procedure:
  data primitive_CPS_EVAL(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (cps-eval <data> <optional-environment> <continuation>)" 
      "\n     -> Pass 'null-environment to cps-eval in the empty environment!"
      "\n     -> Pass 'local-environment to cps-eval in the local environment (default)!"
      "\n     -> Pass 'global-environment to cps-eval in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      THROW_ERR("'cps-eval received incorrect # of arguments:"
        << format << FCN_ERR("cps-eval", args));
    // Extract the continuation & confirm its a procedure
    auto continuation = *args.rbegin();
    primitive_confirm_data_is_a_procedure(continuation, "cps-eval", format, args);
    // set the continuation to be inlined on application
    prm_set_procedure_INLINE_INVOCATION(continuation.exp, true);
    args.pop_back();
    // use the initial/global environment if passed 'null-environment or
    //   'global-environment as a 2nd arg
    bool must_reset_global_env = false;
    auto original_global_env = G::GLOBAL_ENVIRONMENT_POINTER;
    auto env = local_env;
    prm_CPS_EVAL_confirm_correct_number_of_args(args,must_reset_global_env,env,
                                                  original_global_env,"cps-eval",format);
    // Reset "inline"ing of the continuation if EVALing in the 'null-environment
    if(must_reset_global_env) prm_set_procedure_INLINE_INVOCATION(continuation.exp,false);
    // if arg is self-evaluating, return arg
    if(prm_EVAL_data_is_self_evaluating(args[0])) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      scm_list cps_eval_args(1,continuation);
      return data_cast(execute_application(scm_analyze(generate_fundamental_form_cps(args[0]),false,true)(env),cps_eval_args,env));
    }
    // if arg is a symbol, cps-eval the symbol
    if(args[0].is_type(types::sym)) {
      // confirm arg is not '()
      if(args[0].sym == symconst::emptylist) {
        if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
        THROW_ERR("'cps-eval can't evaluate '() (nil to eval):"
          << format << FCN_ERR("cps-eval", args));
      }
      try {
        scm_list cps_eval_args(1,continuation);
        auto result = data_cast(execute_application(scm_analyze(generate_fundamental_form_cps(args[0]),false,true)(env),cps_eval_args,env));
        if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
        return result;
      } catch(const SCM_EXCEPT& eval_throw) {
        if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
        throw eval_throw;
      }
      return G::VOID_DATA_OBJECT;
    }
    // else confirm arg is a proper list
    if(!args[0].is_type(types::par)) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      THROW_ERR("'cps-eval didn't receive an evaluable expression:\n     "<<PROFILE(args[0])
        << format << FCN_ERR("cps-eval", args));
    }
    if(!data_is_proper_list(args[0])) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      THROW_ERR("'cps-eval received an improper list: "<<PROFILE(args[0])
        << format << FCN_ERR("cps-eval", args));
    }
    try {
      scm_list cps_eval_args(1,continuation);
      auto result = data_cast(
        execute_application(scm_analyze(generate_fundamental_form_cps(
          prm_EVAL_convert_list_to_AST(args[0])),false,true)(env),cps_eval_args,env));
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      return result;
    } catch(const SCM_EXCEPT& eval_throw) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      throw eval_throw;
    }
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * APPLY PRIMITIVE
  ******************************************************************************/

  // primitive "apply" procedure:
  data primitive_APPLY(scm_list& args) {
    // get whether in a tail call
    bool tail_call = args.rbegin()->bol.val;
    args.pop_back();
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm the correct # of arguments were passed
    static constexpr const char * const format = "\n     (apply <procedure> <argument-list>)";
    if(args.size() != 2)
      THROW_ERR("'apply received incorrect # of arguments:" << format << FCN_ERR("apply",args));
    // confirm 1st arg is a procedure
    primitive_confirm_data_is_a_procedure(args[0], "apply", format, args);
    // confirm 2nd arg is a finite, nul-terminated list
    if(!data_is_proper_list(args[1]))
      THROW_ERR("'apply 2nd arg " << PROFILE(args[0]) << " isn't a proper list!"
        << format << FCN_ERR("apply",args));
    // apply arguments in list to the procedure
    scm_list args_list;
    shallow_unpack_list_into_exp(args[1], args_list);
    if(args_list.empty()) args_list.push_back(symconst::sentinel_arg);
    return data_cast(execute_application(args[0].exp,args_list,env,tail_call));
  }

  /******************************************************************************
  * FORCE-DELAY PRIMITIVES
  ******************************************************************************/

  // primitive "delay?" predicate procedure:
  data primitive_DELAYP(scm_list& args) {
    confirm_given_one_arg(args,"delay?");
    return boolean(data_is_a_delay(args[0]));
  }

  // primitive "force" procedure:
  data primitive_FORCE(scm_list& args) {
    confirm_given_one_arg(args,"force","<delayed-expression>");
    return force_data_delay(args[0]); // "call-by-need" evaluation
  }

  /******************************************************************************
  * STREAM PRIMITIVES
  ******************************************************************************/

  // primitive "scar" procedure:
  data primitive_SCAR(scm_list& args) {
    confirm_given_a_stream_pair_arg(args, "scar", "\n     (scar <stream-pair>)");
    return force_data_delay(args[0].par->first);
  }

  // primitive "scdr" procedure:
  data primitive_SCDR(scm_list& args) {
    confirm_given_a_stream_pair_arg(args, "scdr", "\n     (scdr <stream-pair>)");
    data cdr_promise = force_data_delay(args[0].par->second);
    if(!data_is_stream(cdr_promise))
      THROW_ERR("'scdr forced cdr " << PROFILE(cdr_promise)
        << " isn't a stream:\n     (scdr <stream-pair>)" << FCN_ERR("scdr", args));
    return cdr_promise;
  }

  // primitive "stream-length" procedure:
  data primitive_STREAM_LENGTH(scm_list& args) {
    confirm_given_one_arg(args,"stream-length","<stream>");
    // Length of '() = 0
    if(data_is_the_empty_expression(args[0])) return num_type();
    // Confirm given a stream pair, if not '()
    confirm_given_a_stream_pair_arg(args,"stream-length","\n     (stream-length <stream>)");
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
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm given minimum # of args needed
    static constexpr const char * const format = 
      "\n     (stream-for-each <procedure> <stream1> <stream2> ...)";
    if(args.size() < 2) 
      THROW_ERR("'stream-for-each received insufficient args (only "
        << args.size() << "):" << format << FCN_ERR("stream-for-each", args));
    // Confirm given a procedure
    primitive_confirm_data_is_a_procedure(args[0], "stream-for-each", format, args);
    // Confirm only given streams
    scm_list stream_heads(args.begin()+1, args.end());
    primitive_confirm_only_given_streams(stream_heads,"stream-for-each",format,1,args);
    // Apply the procedure on each elt of each stream
    primitive_STREAM_FOR_EACH_applicator(stream_heads, args[0].exp, env);
    return G::VOID_DATA_OBJECT;
  }

  // primitive "stream-ref" procedure:
  data primitive_STREAM_REF(scm_list& args) {
    // Confirm appropriate # of args given
    static constexpr const char * const format = 
      "\n     (stream-ref <stream-pair> <index>)";
    if(args.size() != 2) 
      THROW_ERR("'stream-ref received incorrect # of args (given "
        << args.size() << "):" << format << FCN_ERR("stream-ref", args));
    // Confirm given a stream-pair
    if(!data_is_stream_pair(args[0]))
      THROW_ERR("'stream-ref " << PROFILE(args[0]) << " isn't a stream-pair!"
        << format << FCN_ERR("stream-ref", args));
    // Confirm given a valid index
    if(!primitive_is_valid_index(args[1]))
      THROW_ERR("'stream-ref " << PROFILE(args[1]) << " isn't a valid <index>!"
        << format << FCN_ERR("stream-ref", args));
    // Get the stream's index'th item
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(!n) return get_stream_data_car(args[0]);
    data item = primitive_REF_DROP_SUBSTREAM_seeker(get_stream_data_cdr(args[0]), 
                                                    n, "stream-ref", format);
    if(!data_is_stream_pair(item))
      THROW_ERR("'stream-ref <index> " << n << " is out of range!"
        << format << FCN_ERR("stream-ref", args));
    return get_stream_data_car(item);
  }

  // primitive "stream-drop" procedure:
  data primitive_STREAM_DROP(scm_list& args) {
    // Confirm appropriate # of args given
    static constexpr const char * const format = "\n     (stream-drop <stream> <n>)";
    primitive_TEMPLATE_TAKE_DROP_VALIDATION(args, "stream-drop", format);
    // Get the a substream after dropping 'size' items from given stream
    if(data_is_the_empty_expression(args[0])) return args[0];
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(!n)     return args[0];
    if(n == 1) return get_stream_data_cdr(args[0]);
    return primitive_REF_DROP_SUBSTREAM_seeker(get_stream_data_cdr(args[0]), 
                                               n, "stream-drop", format, 1, false);
  }

  // primitive "stream-drop-while" procedure:
  data primitive_STREAM_DROP_WHILE(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm appropriate # of args given
    primitive_TEMPLATE_TAKE_DROP_WHILE_VALIDATION(args, "stream-drop-while", 
      "\n     (stream-drop-while <predicate> <stream>)");
    // Get keep dropping items while 'predicate' is true, then return result
    if(data_is_the_empty_expression(args[1])) return args[1];
    return primitive_DROP_WHILE_ctor(std::move(args[1]), args[0].exp, env);
  }

  // primitive "stream-take" procedure:
  data primitive_STREAM_TAKE(scm_list& args){
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm appropriate # of args given
    primitive_TEMPLATE_TAKE_DROP_VALIDATION(args, "stream-take", 
      "\n     (stream-take <stream> <n>)");
    // Get the a substream after dropping 'size' items from given stream
    if(data_is_the_empty_expression(args[0])) return args[0];
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(!n) return data(symconst::emptylist);
    scm_list substream;
    primitive_TAKE_SUBSTREAM_seeker(std::move(args[0]),n,substream);
    return primitive_STREAM_to_SCONS_constructor(substream.begin(),substream.end(),env);
  }

  // primitive "stream-take-while" procedure:
  data primitive_STREAM_TAKE_WHILE(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm appropriate # of args given
    primitive_TEMPLATE_TAKE_DROP_WHILE_VALIDATION(args, "stream-take-while", 
      "\n     (stream-take-while <predicate> <stream>)");
    // Get keep dropping items while 'predicate' is true, then return result
    if(data_is_the_empty_expression(args[1])) return args[1];
    scm_list substream;
    primitive_TAKE_WHILE_ctor(std::move(args[1]), args[0].exp, substream, env);
    if(substream.empty()) return data(symconst::emptylist);
    return primitive_STREAM_to_SCONS_constructor(substream.begin(),substream.end(),env);
  }

  // primitive "stream-reverse" procedure:
  data primitive_STREAM_REVERSE(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm given a single stream arg
    confirm_given_one_arg(args,"stream-reverse","<stream>");
    if(!data_is_stream(args[0]))
      THROW_ERR("'stream-reverse "<<PROFILE(args[0])<<" isn't a stream:" 
        "\n     (stream-reverse <stream>)" << FCN_ERR("stream-reverse",args));
    // Convert stream to a scm_list, reverse, & revert to a stream
    if(data_is_the_empty_expression(args[0])) return args[0];
    scm_list stream_as_exp;
    unpack_stream_into_exp(std::move(args[0]), stream_as_exp);
    std::reverse(stream_as_exp.begin(),stream_as_exp.end());
    return primitive_STREAM_to_SCONS_constructor(stream_as_exp.begin(),stream_as_exp.end(),env);
  }

  // primitive "stream-fold" procedure:
  data primitive_STREAM_FOLD(scm_list& args) {
    return primitive_STREAM_FOLD_template(args, "stream-fold", 
            "\n     (stream-fold <procedure> <seed> <stream>)", true);
  }

  // primitive "stream-fold-right" procedure:
  data primitive_STREAM_FOLD_RIGHT(scm_list& args) {
    return primitive_STREAM_FOLD_template(args, "stream-fold-right", 
            "\n     (stream-fold-right <procedure> <seed> <stream>)", false);
  }

  // primitive "stream->list" procedure:
  data primitive_CONVERT_STREAM_LIST(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm given proper args (same signature as stream-drop & stream-take)
    primitive_TEMPLATE_TAKE_DROP_VALIDATION(args, "stream->list", 
                                            "\n     (stream->list <stream> <size>)");
    // Invoke stream-take, convert substream -> exp -> list
    if(data_is_the_empty_expression(args[0])) return args[0];
    args.push_back(env); // reinsert env for stream-take
    auto substream = primitive_STREAM_TAKE(args);
    scm_list stream_as_exp;
    unpack_stream_into_exp(std::move(substream), stream_as_exp);
    return primitive_LIST_to_CONS_constructor(stream_as_exp.begin(),stream_as_exp.end());
  }

  // primitive "list->stream" procedure:
  data primitive_CONVERT_LIST_STREAM(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm given a single proper list arg
    confirm_given_one_arg(args,"list->stream","<list>");
    if(!data_is_proper_list(args[0]))
      THROW_ERR("'list->stream "<<PROFILE(args[0])<<" isn't a proper list:" 
        "\n     (list->stream <list>)" << FCN_ERR("list->stream",args));
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
        (syntax-error 'stream-map 
                "1st arg isn't a procedure!\n               (stream-map <procedure> <stream1> <stream2> ...)\n               "
                proc))
    (for-each (lambda (s) 
                (if (not (stream? s)) 
                    (syntax-error 'stream-map
                           "received a non stream!\n               (stream-map <procedure> <stream1> <stream2> ...)\n               " 
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
        (syntax-error 'stream-filter 
               "1st arg isn't a procedure!\n               (stream-filter <predicate> <stream>)\n               " 
               pred?))
    (if (not (stream? s)) 
        (syntax-error 'stream-filter 
               "2nd arg isn't a stream!\n               (stream-filter <predicate> <stream>)\n               " 
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
  (define (stream-from first . optional-step)
    (define (stream-from-iter n suc-proc)
      (scons n (stream-from-iter (suc-proc n) suc-proc)))
    (define step 
      (if (null? optional-step)
          1
          (if (null? (cdr optional-step))
              (if (number? (car optional-step))
                  (car optional-step)
                  (syntax-error 'stream-from 
                     "2nd arg isn't a number!\n               (stream-from <seed-number> <optional-step>)\n               " 
                     (car optional-step)))
              (syntax-error 'stream-from 
                 "received more than 1 step!\n               (stream-from <seed-number> <optional-step>)\n               " 
                 step))))
    (if (number? first)
        (stream-from-iter first (lambda (num) (+ num step)))
        (syntax-error 'stream-from 
           "1st arg isn't a number!\n               (stream-from <seed-number> <optional-step>)\n               " 
           first)))
  )";

  // primitive "stream-unfold" procedure:
  constexpr const char * const primitive_HEIST_STREAM_UNFOLD = R"(
  (define (stream-unfold break-cond map-proc suc-proc seed)
    (if (not (procedure? break-cond))
        (syntax-error 'stream-unfold 
               "1st arg isn't a procedure!\n               (stream-unfold <break-condition> <map-procedure> <successor-procedure> <seed>)\n               " 
               break-cond))
    (if (not (procedure? map-proc))
        (syntax-error 'stream-unfold 
               "2nd arg isn't a procedure!\n               (stream-unfold <break-condition> <map-procedure> <successor-procedure> <seed>)\n               " 
               map-proc))
    (if (not (procedure? suc-proc))
        (syntax-error 'stream-unfold 
               "3rd arg isn't a procedure!\n               (stream-unfold <break-condition> <map-procedure> <successor-procedure> <seed>)\n               " 
               suc-proc))
    
    (define (stream-unfold seed)
      (if (break-cond seed)
          stream-null
          (scons (map-proc seed) (stream-unfold (suc-proc seed)))))

    (stream-unfold seed))
  )";


  // primitive "stream-iterate" procedure:
  constexpr const char * const primitive_HEIST_STREAM_ITERATE = R"(
  (define (stream-iterate suc-proc seed)
    (if (not (procedure? suc-proc))
        (syntax-error 'stream-iterate 
               "1st arg isn't a procedure!\n               (stream-iterate <successor-procedure> <seed>)\n               " 
               suc-proc))
    
    (define (stream-iterate seed)
      (scons seed (stream-iterate (suc-proc seed))))

    (stream-iterate seed))
  )";

  // primitive "stream-zip" procedure:
  constexpr const char * const primitive_HEIST_STREAM_ZIP = R"(
  (define (stream-zip . streams)
    (for-each (lambda (s) 
                (if (not (stream? s)) 
                    (syntax-error 'stream-zip 
                           "received a non stream!\n               (stream-zip <stream1> <stream2> ...)\n               " 
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
                    (syntax-error 'stream-append 
                           "received a non stream!\n               (stream-append <stream1> <stream2> ...)\n               " 
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
        (syntax-error 'stream-interleave 
               "1st arg isn't a stream!\n               (stream-interleave <stream1> <stream2>)\n               " 
               stream1))
    (if (not (stream? stream2))
        (syntax-error 'stream-interleave 
               "2nd arg isn't a stream!\n               (stream-interleave <stream2> <stream2>)\n               " 
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

  // primitive "char->int" procedure:
  data primitive_COERCE_CHAR_TO_INT(scm_list& args) {
    confirm_given_one_char_arg(args, "char->int");
    return num_type(int(args[0].chr));
  }

  // primitive "int->char" procedure:
  data primitive_COERCE_INT_TO_CHAR(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (int->char <non_negative-integer>)"
      "\n     <non_negative-integer> range: [0,255]";
    if(args.size() != 1)
      THROW_ERR("'int->char didn't receive 1 integer arg:" << format 
        << FCN_ERR("int->char",args));
    if(!args[0].is_type(types::num) || !args[0].num.is_integer())
      THROW_ERR("'int->char didn't receive an integer arg:"
        "\n     Received arg " << PROFILE(args[0]) << format 
        << FCN_ERR("int->char",args));
    if((args[0].num.is_neg() || args[0].num > 255) && 
       args[0].num != EOF)
      THROW_ERR("'int->char " << PROFILE(args[0]) << " isn't a"
        "\n     positive integer ranging from 0 to 255!" << format 
        << FCN_ERR("int->char",args));
    return chr_type(args[0].num.extract_inexact());
  }

  // primitive "number->string" procedure:
  data primitive_COERCE_NUMBER_TO_STRING(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (number->string <number> <optional-radix> <optional-precision>)";
    if(args.size() > 3 || args.empty())
      THROW_ERR("'number->string received incorrect # of arguments:"
        << format << FCN_ERR("number->string",args));
    // No number or invalid radix/precision given
    if(invalid_NUMBER_TO_STRING_args(args)) return G::FALSE_DATA_BOOLEAN;
    // Given a radix
    scm_string number_as_string;
    if(args.size() > 1) {
      size_type radix = args[1].num.round().extract_inexact();
      if(radix < 2 || radix > 36)
        THROW_ERR("'number->string radix (given "<<radix<<") can only range from 2-36:"
          << format << FCN_ERR("number->string", args));
      if(radix != 10) {
        number_as_string = args[0].num.str(radix);
        goto after_number_stringification;
      }
    }
    number_as_string = args[0].num.str();
  after_number_stringification:
    // Alter precision as needed
    if(no_NUMBER_TO_STRING_precision_change_needed(args,number_as_string))
      return make_str(number_as_string);
    const auto dec_pos = number_as_string.find(".");
    if(dec_pos == scm_string::npos) return make_str(number_as_string);
    const auto current_precision = number_as_string.size()-dec_pos-1;
    const auto precision = (size_type)args[2].num.extract_inexact();
    if(precision > current_precision) // pad 0s
      return make_str(number_as_string + scm_string(precision-current_precision,'0'));
    number_as_string.erase(dec_pos+precision+1); // truncate
    return make_str(number_as_string);
  }

  // primitive "string->number" procedure:
  data primitive_COERCE_STRING_TO_NUMBER(scm_list& args) {
    if(args.size() > 2 || args.empty())
      THROW_ERR("'string->number received incorrect # of arguments!"
        "\n     (string->number <string> <optional-numeric-radix>)"
        << FCN_ERR("string->number", args));
    // no string or invalid radix given
    if(!args[0].is_type(types::str) || 
        (args.size() == 2 && 
          (!args[1].is_type(types::num) || !args[1].num.is_integer()))) 
      return G::FALSE_DATA_BOOLEAN;
    // given a radix
    if(args.size() == 2) {
      size_type radix = args[1].num.round().extract_inexact();
      if(radix < 2 || radix > 36)
        THROW_ERR("'string->number radix (given "<<radix<<") can only range from 2-36!"
          "\n     (string->number <string> <optional-numeric-radix>)"
          << FCN_ERR("string->number", args));
      if(radix != 10) {
        auto num = num_type(*args[0].str, radix);
        if(num.is_nan()) return G::FALSE_DATA_BOOLEAN; // invalid conversion
        return data(num);
      }
    }
    // immediate return if given NaN
    if(*args[0].str == "+nan.0" || *args[0].str == "-nan.0")
      return data(num_type(*args[0].str));
    auto num = num_type(*args[0].str);
    if(num.is_nan()) return G::FALSE_DATA_BOOLEAN; // invalid conversion
    return data(num);
  }

  // primitive "symbol->string" procedure:
  data primitive_COERCE_SYMBOL_TO_STRING(scm_list& args) {
    confirm_given_one_arg(args,"symbol->string","<symbol>");
    if(!args[0].is_type(types::sym)) return G::FALSE_DATA_BOOLEAN;
    return make_str(convert_symbol_to_string(args[0].sym));
  }

  // primitive "string->symbol" procedure:
  data primitive_COERCE_STRING_TO_SYMBOL(scm_list& args) {
    confirm_given_one_arg(args,"string->symbol","<string>");
    if(!args[0].is_type(types::str)) return G::FALSE_DATA_BOOLEAN;
    return data(convert_string_to_symbol(*args[0].str)); 
  }

  // primitive "vector->list" procedure:
  data primitive_COERCE_VECTOR_TO_LIST(scm_list& args) {
    primitive_confirm_valid_vector_arg(args, 1, "vector->list", "\n     (vector->list <vector>)");
    return primitive_LIST_to_CONS_constructor(args[0].vec->begin(),args[0].vec->end());
  }

  // primitive "list->vector" procedure:
  data primitive_COERCE_LIST_TO_VECTOR(scm_list& args) {
    if(primitive_validate_list_and_return_if_empty(args, "list->vector"))
      return make_vec(scm_list());
    data new_vec(make_vec(scm_list()));
    shallow_unpack_list_into_exp(args[0], *new_vec.vec);
    return new_vec;
  }

  // primitive "string->vector" procedure:
  data primitive_STRING_TO_VECTOR(scm_list& args) {
    primitive_confirm_valid_string_arg(args, 1, "string->vector", "\n     (string->vector <string>)");
    scm_list char_vect;
    for(const auto& ch : *args[0].str)
      char_vect.push_back(ch);
    return make_vec(char_vect);
  }

  // primitive "vector->string" procedure:
  data primitive_VECTOR_TO_STRING(scm_list& args) {
    primitive_confirm_valid_vector_arg(args, 1, "vector->string", "\n     (vector->string <vector>)");
    if(args[0].vec->empty()) return make_str("");
    const scm_list& vect = *args[0].vec;
    scm_string str_val;
    for(size_type i = 0, n = vect.size(); i < n; ++i) {
      if(!vect[i].is_type(types::chr)) 
        THROW_ERR("'vector->string vector item #" << i+1 << ", " << PROFILE(vect[i]) 
          << ",\n     isn't a character: (vector->string <vector>)" 
          << FCN_ERR("vector->string", args));
      str_val += vect[i].chr;
    }
    return make_str(str_val);
  }

  // primitive "string->list" procedure:
  data primitive_STRING_TO_LIST(scm_list& args) {
    primitive_confirm_valid_string_arg(args, 1, "string->list", "\n     (string->list <string>)");
    scm_list char_list;
    for(const auto& ch : *args[0].str)
      char_list.push_back(ch);
    return primitive_LIST_to_CONS_constructor(char_list.begin(),char_list.end());
  }

  // primitive "list->string" procedure:
  data primitive_LIST_TO_STRING(scm_list& args) {
    if(primitive_validate_list_and_return_if_empty(args, "list->string"))
      return make_str("");
    scm_list char_list;
    scm_string char_str;
    shallow_unpack_list_into_exp(args[0], char_list);
    for(size_type i = 0, n = char_list.size(); i < n; ++i) {
      if(!char_list[i].is_type(types::chr)) {
        THROW_ERR("'list->string list item #"<<i+1<<", "<<PROFILE(char_list[i])
          << " isn't a character!\n     (list->string <char-list>)"
          << FCN_ERR("list->string",args));
      } else {
        char_str += char(char_list[i].chr);
      }
    }
    return make_str(char_str);
  }

  /******************************************************************************
  * OUTPUT PRIMITIVES
  ******************************************************************************/

  data primitive_PPRINT(scm_list& args) {
    FILE* outs = G::CURRENT_OUTPUT_PORT;
    bool is_port = confirm_valid_output_args(args, outs, 1, "pretty-print", 
                    "\n     (pretty-print <obj> <optional-open-output-port-or-string>)");
    if(!args[0].is_type(types::dne)) {
      if(is_port) {
        fputs(args[0].pprint().c_str(), outs);
        fflush(outs);
      } else {
        return make_str(*args[1].str + args[0].pprint());
      }
    }
    G::LAST_PRINTED_TO_STDOUT = (outs == stdout && is_port);
    return G::VOID_DATA_OBJECT;
  }

  data primitive_WRITE(scm_list& args) {
    FILE* outs = G::CURRENT_OUTPUT_PORT;
    bool is_port = confirm_valid_output_args(args, outs, 1, "write", 
                    "\n     (write <obj> <optional-open-output-port-or-string>)");
    if(!args[0].is_type(types::dne)) {
      if(is_port) {
        fputs(args[0].write().c_str(), outs);
        fflush(outs);
      } else {
        return make_str(*args[1].str + args[0].write());
      }
    }
    G::LAST_PRINTED_TO_STDOUT = (outs == stdout && is_port);
    return G::VOID_DATA_OBJECT;
  }

  data primitive_NEWLINE(scm_list& args) {
    FILE* outs = G::CURRENT_OUTPUT_PORT;
    bool is_port = confirm_valid_output_args(args, outs, 0, "newline", 
                    "\n     (newline <optional-open-output-port-or-string>)");
    if(is_port) {
      fputc('\n', outs);
      fflush(outs);
      G::LAST_PRINTED_NEWLINE_TO_STDOUT = G::LAST_PRINTED_TO_STDOUT = (outs == stdout);
    } else {
      return make_str(*args[0].str + '\n');
    }
    return G::VOID_DATA_OBJECT;
  }

  data primitive_DISPLAY(scm_list& args) {
    FILE* outs = G::CURRENT_OUTPUT_PORT;
    bool is_port = confirm_valid_output_args(args, outs, 1, "display", 
                    "\n     (display <obj> <optional-open-output-port-or-string>)");
    if(is_port)
      return primitive_display_port_logic(args[0], outs);
    return make_str(*args[1].str + args[0].display());
  }

  data primitive_WRITE_CHAR(scm_list& args) {
    FILE* outs = G::CURRENT_OUTPUT_PORT;
    bool is_port = confirm_valid_output_args(args, outs, 1, "write-char", 
                    "\n     (write-char <char> <optional-open-output-port-or-string>)");
    // confirm given a character
    if(!args[0].is_type(types::chr))
      THROW_ERR("'write-char arg "<<PROFILE(args[0])<<" isn't a character:" 
        "\n     (write-char <char> <optional-open-output-port-or-string>)"
        << FCN_ERR("write-char", args));
    if(is_port) {
      fputc(args[0].chr, outs);
      fflush(outs);
      G::LAST_PRINTED_TO_STDOUT = (outs == stdout);
    } else {
      return make_str(*args[1].str + char(args[0].chr));
    }
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * INPUT PRIMITIVES
  ******************************************************************************/

  data primitive_READ(scm_list& args) {
    // Extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // Confirm either given an open input port or string or no args
    FILE* outs = G::CURRENT_OUTPUT_PORT, *ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "read", reading_stdin, reading_string)) 
      return chr_type(EOF);
    if(reading_string)
      return primitive_read_from_string_logic(*args[0].str,env);
    return primitive_read_from_input_port_logic(outs,ins,reading_stdin,env);
  }

  data primitive_READ_STRING(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // return string w/ next valid scheme expression, if successfully parsed one
    FILE* outs = G::CURRENT_OUTPUT_PORT, *ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "read-string", reading_stdin, reading_string)) 
      return make_str("");
    if(reading_string)
      return make_str(primitive_read_from_string_logic(*args[0].str,env).write());
    return make_str(primitive_read_from_input_port_logic(outs,ins,reading_stdin,env).write());
  }

  data primitive_READ_LINE(scm_list& args) {
    // Confirm either given an open input port or no args
    FILE* outs = G::CURRENT_OUTPUT_PORT, *ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "read-line", reading_stdin, reading_string)) 
      return make_str("");
    // Read a line of input into a string
    if(reading_string) {
      auto line_buffer = args[0].str->substr(0,args[0].str->find('\n'));
      args[0].str->erase(0,args[0].str->find('\n'));
      return make_str(line_buffer);
    }
    scm_string line_buffer;
    fflush(outs);
    int ch = 0;
    while((ch = fgetc(ins)) != '\n' && ch != EOF) line_buffer += ch;
    return make_str(line_buffer);
  }

  data primitive_READ_CHAR(scm_list& args) {
    // Confirm either given an open input port or no args
    FILE* outs = G::CURRENT_OUTPUT_PORT, *ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "read-char", reading_stdin, reading_string)) 
      return chr_type(EOF);
    // Read a char from a string as needed
    if(reading_string) {
      auto ch = args[0].str->operator[](0);
      args[0].str->erase(0,1);
      return chr_type(ch);
    }
    // Read a char from a port as needed
    fflush(outs);
    if(!reading_stdin) return chr_type(getc(ins));
    // Else read 1 char from stdin & throw away the rest of the line
    int ch = getc(stdin);
    if(ch!='\n') while(getc(stdin) != '\n'); // eat rest of the line
    return chr_type(ch);
  }

  data primitive_PEEK_CHAR(scm_list& args) {
    // Confirm either given an open input port or no args
    FILE* outs = G::CURRENT_OUTPUT_PORT, *ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "peek-char", reading_stdin, reading_string)) 
      return chr_type(EOF);
  // Peek a char from a string as needed
    if(reading_string) {
      auto ch = args[0].str->operator[](0);
      args[0].str->erase(0,1);
      return chr_type(ch);
    }
    // Peek a char from a port as needed
    fflush(outs);
    if(!reading_stdin) {
      int ch = getc(ins);
      ungetc(ch, ins);
      return chr_type(ch);
    }
    // Else peek 1 char from stdin & throw away the rest of the line
    // NOTE: 'peek-char' from stdin is equivalent to 'read-char' from stdin since
    //       both return 1 char from the stream & throw away the rest of the line
    int ch = getc(stdin);
    if(ch!='\n') while(getc(stdin) != '\n'); // eat rest of the line
    return chr_type(ch);
  }

  data primitive_CHAR_READYP(scm_list& args) {
    // Confirm either given an open input port or no args
    FILE* ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "char-ready?", reading_stdin, reading_string)) 
      return G::FALSE_DATA_BOOLEAN;
    // Empty strings trigger EOF above, hence will always have a character ready here
    if(reading_string) return G::TRUE_DATA_BOOLEAN;
    // Stdin is always flushed, hence a char will never be waiting in its buffer
    if(reading_stdin)  return G::FALSE_DATA_BOOLEAN;
    // Peek the non-stdin port for a non-EOF character
    int ch = getc(ins);
    ungetc(ch, ins);
    return data(boolean(ch != EOF));
  }

  // slurp a port's contents into a string
  data primitive_SLURP_PORT(scm_list& args){
    // confirm given a filename string & slurp file if so
    FILE* ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "slurp-port", reading_stdin, reading_string)) 
      return make_str("");
    if(reading_string) return make_str(*args[0].str);
    if(reading_stdin)  return make_str("");
    scm_string buffer;
    int ch = 0;
    while((ch = fgetc(ins)) != EOF) buffer += ch; // slurp entire file
    fclose(ins);
    return make_str(buffer);
  }

  // slurp a file's contents into a string
  data primitive_SLURP_FILE(scm_list& args){
    // confirm given a filename string & slurp file if so
    confirm_given_one_arg(args,"slurp-file","<filename-string>");
    FILE* ins = confirm_valid_input_file(args[0],"slurp-file","(slurp-file <filename-string>)",args);
    scm_string buffer;
    int ch = 0;
    while((ch = fgetc(ins)) != EOF) buffer += ch; // slurp entire file
    fclose(ins);
    return make_str(buffer);
  }

  /******************************************************************************
  * PORT PRIMITIVES
  ******************************************************************************/

  // file & ports predicates
  data primitive_FILEP(scm_list& args) {
    confirm_given_one_string_arg(args, "file?", "\n     (file? <filename-string>)");
    return data(boolean(confirm_file_exists(args[0].str->c_str())));
  }

  // returns whether succeed deleting given filename-string
  data primitive_DELETE_FILE_BANG(scm_list& args) {
    confirm_given_one_string_arg(args, "delete-file!", "\n     (delete-file! <filename-string>)");
    return data(boolean(std::remove(args[0].str->c_str()) == 0));
  }

  // returns whether succeed deleting given filename-string
  data primitive_RENAME_FILE_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (rename-file! <old-name-string> <new-name-string>)";
    if(args.size() != 2) 
      THROW_ERR("'rename-file! didn't receive any args:"<<format 
        << FCN_ERR("rename-file!",args));
    for(size_type i = 0; i < 2; ++i)
      if(!args[i].is_type(types::str)) 
        THROW_ERR("'rename-file! arg "<<PROFILE(args[i])<<" isn't a string:"<<format 
          << FCN_ERR("rename-file!",args));
    return data(boolean(std::rename(args[0].str->c_str(),
                                    args[1].str->c_str()) == 0));
  }

  data primitive_OPEN_PORTP(scm_list& args) {
    confirm_valid_port_predicate_arg(args,"open-port?","\n     (open-port? <port>)");
    if(args[0].is_type(types::fip))
      return data(boolean(args[0].fip.is_open()));
    return data(boolean(args[0].fop.is_open()));
  }

  data primitive_CLOSED_PORTP(scm_list& args) {
    confirm_valid_port_predicate_arg(args,"closed-port?","\n     (closed-port? <port>)");
    if(args[0].is_type(types::fip))
      return data(boolean(!args[0].fip.is_open()));
    return data(boolean(!args[0].fop.is_open()));
  }

  // retrieve the current default input & output ports
  data primitive_CURRENT_INPUT_PORT(scm_list& args){
    if(!args.empty()) 
      THROW_ERR("'current-input-port doesn't take arguments: (current-input-port)"
        << FCN_ERR("current-input-port", args));
    for(size_type i = 0, n = G::PORT_REGISTRY.size(); i < n; ++i)
      if(G::CURRENT_INPUT_PORT == G::PORT_REGISTRY[i]) 
        return iport(i);
    return iport();
  }

  data primitive_CURRENT_OUTPUT_PORT(scm_list& args){
    if(!args.empty()) 
      THROW_ERR("'current-output-port doesn't take arguments: (current-output-port)"
        << FCN_ERR("current-output-port", args));
    for(size_type i = 0, n = G::PORT_REGISTRY.size(); i < n; ++i)
      if(G::CURRENT_OUTPUT_PORT == G::PORT_REGISTRY[i]) 
        return oport(i);
    return oport();
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
              G::CURRENT_INPUT_PORT,
              confirm_valid_input_file);
  }
  data primitive_WITH_OUTPUT_TO_FILE(scm_list& args){
    return primitive_WITH_FILE(
              args,
              "with-output-to-file",
              "\n     (with-output-to-file <filename-string> <argless-procedure>)",
              G::CURRENT_OUTPUT_PORT,
              confirm_valid_output_file);
  }

  // retrieve a port for a file
  data primitive_OPEN_INPUT_FILE(scm_list& args){
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm given a filename string
    confirm_given_one_arg(args,"open-input-file","<filename-string>");
    G::PORT_REGISTRY.push_back(confirm_valid_input_file(args[0],"open-input-file",
      "\n     (open-input-file <filename-string>)",args));
    return iport(G::PORT_REGISTRY.size()-1);
  }

  data primitive_OPEN_OUTPUT_FILE(scm_list& args){
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm given a filename string
    confirm_given_one_arg(args,"open-output-file","<filename-string>");
    G::PORT_REGISTRY.push_back(confirm_valid_output_file(args[0],"open-output-file",
      "\n     (open-output-file <filename-string>)",args));
    return oport(G::PORT_REGISTRY.size()-1);
  }

  // close input or output port
  data primitive_CLOSE_PORT(scm_list& args){
    if(args.size() != 1)
      THROW_ERR("'close-port received incorrect # of args:" 
        "\n     (close-port <input-or-output-port>)" 
        << FCN_ERR("close-port", args));
    if(!args[0].is_type(types::fip) && !args[0].is_type(types::fop))
      THROW_ERR("'close-port arg " << PROFILE(args[0])
        << "\n     isn't a port:\n     (close-port <input-or-output-port>)"
        << FCN_ERR("close-port",args));
    if(args[0].is_type(types::fip) && args[0].fip.is_open() && 
                                      args[0].fip.port() != stdin){
      fclose(args[0].fip.port());
      args[0].fip.port() = nullptr;
    } else if(args[0].is_type(types::fop) && args[0].fop.is_open() && 
                                             args[0].fop.port() != stdout && 
                                             args[0].fop.port() != stderr){
      fclose(args[0].fop.port());
      args[0].fop.port() = nullptr;
    }
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * SYSTEM INTERFACE PRIMITIVES
  ******************************************************************************/

  // Load a script into the global environment
  data primitive_LOAD(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (load <filename-string> <optional-environment>)"
      "\n     -> Pass 'null-environment to load in the empty environment!"
      "\n     -> Pass 'local-environment to load in the local environment!"
      "\n     -> Pass 'global-environment to load in the global environment (default)!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    // determine which environment to load <filename-string> wrt to
    auto env = G::GLOBAL_ENVIRONMENT_POINTER;
    if(args.size()==2 && args[1].is_type(types::sym)) {
      if(args[1].sym == symconst::null_env) {
        // Reset "G::GLOBAL_ENVIRONMENT_POINTER" to its default state
        set_default_global_environment(), args.pop_back();
        try {
          primitive_LOAD_interpret_file_contents(args,G::GLOBAL_ENVIRONMENT_POINTER,format);
        } catch(const SCM_EXCEPT& eval_throw) {
          G::GLOBAL_ENVIRONMENT_POINTER = env;
          throw eval_throw;
        }
        // Reset G::GLOBAL_ENVIRONMENT_POINTER to its original bindings
        G::GLOBAL_ENVIRONMENT_POINTER = env; 
        return G::VOID_DATA_OBJECT;
      } else if(args[1].sym == symconst::local_env) {
        env = local_env, args.pop_back();
      } else if(args[1].sym == symconst::global_env) {
        args.pop_back(); // global-environment is default
      } else {
        THROW_ERR("'load \""<<args[1].sym<<"\" isn't an evaluation environment:"
          << format << FCN_ERR("load", args));
      }
    }
    primitive_LOAD_interpret_file_contents(args,env,format);
    return G::VOID_DATA_OBJECT;
  }

  // Load a script into the global environment, convert it to CPS, and pass it to the given continuation
  data primitive_CPS_LOAD(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (cps-load <filename-string> <optional-environment> <continuation-procedure>)"
      "\n     -> Pass 'null-environment to cps-load in the empty environment!"
      "\n     -> Pass 'local-environment to cps-load in the local environment (default)!"
      "\n     -> Pass 'global-environment to cps-load in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    if(args.size() < 2)
      THROW_ERR("'cps-load recieved incorrect # of args!" << format << FCN_ERR("cps-load",args));
    // extract the continuation
    auto continuation = *(args.rbegin());
    primitive_confirm_data_is_a_procedure(continuation, "cps-load", format, args);
    // set the continuation to be inlined on application
    prm_set_procedure_INLINE_INVOCATION(continuation.exp, true);
    args.pop_back();
    // determine which environment to load <filename-string> wrt to
    auto env = local_env;
    if(args.size()==2 && args[1].is_type(types::sym)) {
      if(args[1].sym == symconst::null_env) {
        // Reset "inline"ing of the continuation, no need in null-environment
        prm_set_procedure_INLINE_INVOCATION(continuation.exp,false);
        // Reset "G::GLOBAL_ENVIRONMENT_POINTER" to its default state
        set_default_global_environment(), args.pop_back();
        try {
          scm_list cps_load_arg(1,continuation);
          // pass the continuation to the loaded file
          auto result = data_cast(execute_application(
            primitive_CPS_LOAD_interpret_file_contents(args,G::GLOBAL_ENVIRONMENT_POINTER,format),
            cps_load_arg,G::GLOBAL_ENVIRONMENT_POINTER));
          // Reset G::GLOBAL_ENVIRONMENT_POINTER to its original bindings
          G::GLOBAL_ENVIRONMENT_POINTER = env;
          return result;
        } catch(const SCM_EXCEPT& eval_throw) {
          G::GLOBAL_ENVIRONMENT_POINTER = env;
          throw eval_throw;
        }
      } else if(args[1].sym == symconst::global_env) {
        env = G::GLOBAL_ENVIRONMENT_POINTER; args.pop_back();
      } else if(args[1].sym == symconst::local_env) {
        args.pop_back(); // local-environment is default
      } else {
        THROW_ERR("'cps-load \""<<args[1].sym<<"\" isn't an evaluation environment:"
          << format << FCN_ERR("cps-load", args));
      }
    }
    // pass the continuation to the loaded file
    scm_list cps_load_arg(1,continuation);
    return data_cast(execute_application(primitive_CPS_LOAD_interpret_file_contents(args,env,format),cps_load_arg,env));
  }

  // Compiles a given filename's file's Heist-Scheme code into a C++ File
  data primitive_COMPILE(scm_list& args){
    return primitive_COMPILE_TEMPLATE(args,"compile",
      "\n     (compile <filename-string> <optional-compiled-filename>)",false);
  }

  // Compiles a given file w/ a ((scm->cps <file-contents>) id) wrapper
  data primitive_CPS_COMPILE(scm_list& args){
    return primitive_COMPILE_TEMPLATE(args,"cps-compile",
      "\n     (cps-compile <filename-string> <optional-compiled-filename>)",true);
  }

  // Make a system call, returns #f if can't use 'system, 
  //   and the call's success status if can use the system
  data primitive_SYSTEM(scm_list& args) {
    if(args.size() > 1)
      THROW_ERR("'system received incorrect # of args!"
        "\n     (system <optional-system-call-string>)"<<FCN_ERR("system",args));
    if(!args.empty() && !args[0].is_type(types::str))
      THROW_ERR("'system "<<PROFILE(args[0])<<" isn't a string!"
        "\n     (system <optional-system-call-string>)"<<FCN_ERR("system",args));
    // return false if CAN'T use 'system'
    if(!std::system(nullptr)) return G::FALSE_DATA_BOOLEAN;
    // return true if just checking whether may use the system (no args given)
    if(args.empty()) return G::TRUE_DATA_BOOLEAN;
    return num_type(std::system(args[0].str->c_str()));
  }

  // Given a string of a variable name, returns a string of that variable's value
  data primitive_GETENV(scm_list& args) {
    // extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    // confirm proper arg signature
    confirm_given_one_arg(args,"getenv","<variable-name-string>");
    if(!args.empty() && !args[0].is_type(types::str))
      THROW_ERR("'getenv "<<PROFILE(args[0])<<" isn't a string!"
        "\n     (getenv <variable-name-string>)"<<FCN_ERR("getenv",args));
    // search each environment frame
    auto var = *args[0].str;
    for(size_type i = 0, total_frames = env->size(); i < total_frames; ++i){
      // Get Variables & Values Lists of the current frame
      auto& [var_list, val_list, mac_list] = *env->operator[](i);
      // Search Variable-Value List Pair In Frame
      for(size_type j = 0, total_vars = var_list.size(); j < total_vars; ++j)
        if(var == var_list[j] && !val_list[j].is_type(types::undefined))
          return make_str(val_list[j].write());
    }
    return G::FALSE_DATA_BOOLEAN;
  }

  // Returns a string of Heist Scheme's command-line args & their descriptions
  data primitive_COMMAND_LINE(scm_list& args) {
    if(!args.empty())
      THROW_ERR("'command-line doesn't take any arguments:"
        "\n     (command-line)" << FCN_ERR("command-line",args));
    return make_str("> Interpret Script:    -script <script-filename>"
                  "\n> Compile Script:      -compile <script-filename> <optional-compiled-filename>"
                  "\n> With CPS Evaluation: -cps"
                  "\n> Disable ANSI Colors: -nansi"
                  "\n> Case Insensitivity:  -ci");
  }

  // Returns a string of the current working directory
  data primitive_GETCWD(scm_list& args) {
    if(!args.empty())
      THROW_ERR("'getcwd doesn't take any arguments:"
        "\n     (getcwd)" << FCN_ERR("getcwd",args));
    return make_str(std::filesystem::current_path());
  }

  // Returns a string of the parent directory of the given path string
  data primitive_DIRNAME(scm_list& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      THROW_ERR("'dirname didn't get a filepath <string> arg:"
        "\n     (dirname <filepath-string>)" << FCN_ERR("dirname",args));
    return make_str(std::filesystem::path(*args[0].str).parent_path());
  }

  // Returns a string of the directory leading to the heist interpreter
  data primitive_HEIST_DIRNAME(scm_list& args) {
    if(!args.empty())
      THROW_ERR("'heist-dirname doesn't take any arguments:"
        "\n     (heist-dirname)" << FCN_ERR("heist-dirname",args));
    return make_str(HEIST_DIRECTORY_FILE_PATH);
  }

  /******************************************************************************
  * CURRENT TIME PRIMITIVE
  ******************************************************************************/

  data primitive_SECONDS_SINCE_EPOCH(scm_list& args) {
    if(!args.empty())
      THROW_ERR("'seconds-since-epoch doesn't take any arguments:"
        "\n     (seconds-since-epoch)" << FCN_ERR("seconds-since-epoch",args));
    return num_type(std::chrono::duration_cast<std::chrono::seconds>(
                    std::chrono::system_clock::now().time_since_epoch()).count());
  }

  /******************************************************************************
  * INTERPRETER INVARIANTS MANIPULATION PRIMITIVES
  ******************************************************************************/

  // Defaults to disabling ANSI escape sequences, if not given a boolean.
  // Returns whether ANSI escapes sequences were disabled prior this call
  data primitive_SET_NANSI(scm_list& args) {
    return primitive_TOGGLE_DISPLAY_SETTING(args, "set-nansi!", 
                                            G::USING_ANSI_ESCAPE_SEQUENCES);
  }

  // Defaults to enabling case-sensitivity, if not given a boolean.
  // Returns whether case-sensitivity was active prior this call
  data primitive_SET_CI(scm_list& args) {
    return primitive_TOGGLE_DISPLAY_SETTING(args, "set-ci!", 
                                            G::USING_CASE_SENSITIVE_SYMBOLS);
  }

  data primitive_SET_PPRINT_COLUMN_WIDTH_BANG(scm_list& args) {
    return primitive_TOGGLE_NUMERIC_SETTING(args,"set-pprint-column-width!",
                                            G::PPRINT_MAX_COLUMN_WIDTH);
  }

  data primitive_SET_MAX_RECURSION_DEPTH(scm_list& args) {
    return primitive_TOGGLE_NUMERIC_SETTING(args,"set-max-recursion-depth!",
                                            G::MAX_RECURSION_DEPTH);
  }

  // Changes the REPL's line-by-line prompt from the default "> "
  data primitive_SET_REPL_PROMPT(scm_list& args) {
    confirm_given_one_arg(args,"set-repl-prompt!","<prompt-string>");
    if(!args[0].is_type(types::str))
      THROW_ERR("'set-repl-prompt! "<<PROFILE(args[0])<<" isn't a string:"
        "\n     (set-repl-prompt! <prompt-string>)" << FCN_ERR("set-repl-prompt!",args));
    if(args[0].str->empty()) {
      G::REPL_PROMPT = G::REPL_TAB = "";
      return G::VOID_DATA_OBJECT;
    }
    G::REPL_PROMPT = *args[0].str;
    G::REPL_TAB = scm_string(G::REPL_PROMPT.size()-1, ' ');
    return G::VOID_DATA_OBJECT;
  }

  // Toggles Procedure Call Tracing (returns the previous state prior toggle)
  data primitive_SET_TRACE_CALLS_BANG(scm_list& args) {
    if(args.size() != 1)
      THROW_ERR("'set-trace-calls! expects 1 arg: (set-trace-calls! <bool>)"
        "\n     (set-trace-calls! <bool>)" << FCN_ERR("set-trace-calls!",args));
    bool prior_state = G::TRACING_ALL_FUNCTION_CALLS;
    G::TRACING_ALL_FUNCTION_CALLS = (!args[0].is_type(types::bol) || args[0].bol.val);
    return boolean(prior_state);
  }

  /******************************************************************************
  * CONTROL-FLOW PRIMITIVES: EXITING, ERROR HANDLING, INLINING, & JUMPING
  ******************************************************************************/

  data primitive_EXIT(scm_list&) {
    throw SCM_EXCEPT::EXIT; // regardless of args, 'exit' exits
    return data();
  }

  data primitive_ERROR(scm_list& args) {
    primitive_error_template(args,"error","Exception",AFMT_131);
    return data();
  }

  data primitive_SYNTAX_ERROR(scm_list& args) {
    primitive_error_template(args,"syntax-error","Invalid Syntax",AFMT_135);
    return data();
  }

  // Invoke <proc> w/ args in the current environment, ie w/ Dynamic Scope!
  data primitive_CALL_CE(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      THROW_ERR("'call/ce recieved incorrect # of args!"
        "\n     (call/ce <proc> <arg1> ... <argN>)" << FCN_ERR("call/ce",args));
    primitive_confirm_data_is_a_procedure(args[0], "call/ce", 
      "\n     (call/ce <proc> <arg1> ... <argN>)", args);
    scm_list call_ce_args(args.begin()+1,args.end());
    if(call_ce_args.empty()) call_ce_args.push_back(symconst::sentinel_arg);
    return data_cast(execute_application(args[0].exp,call_ce_args,env,false,true));
  }

  // Propagates "call/ce" across this invocation & every subsequent invocation 
  //   resulting from this invocation (ie "deep" call/ce)
  data primitive_INLINE(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      THROW_ERR("'inline recieved incorrect # of args!"
        "\n     (inline <proc> <arg1> ... <argN>)" << FCN_ERR("inline",args));
    primitive_confirm_data_is_a_procedure(args[0], "inline", 
      "\n     (inline <proc> <arg1> ... <argN>)", args);
    scm_list inline_args(args.begin()+1,args.end());
    if(inline_args.empty()) inline_args.push_back(symconst::sentinel_arg);
    G::USING_INLINE_INVOCATIONS = true;
    try {
      auto result = data_cast(execute_application(args[0].exp,inline_args,env));
      G::USING_INLINE_INVOCATIONS = false;
      return result;
    } catch(const SCM_EXCEPT& call_ce_error) {
      G::USING_INLINE_INVOCATIONS = false;
      throw call_ce_error;
    }
  }

  data primitive_JUMP_BANG(scm_list& args) {
    if(args.size() > 1)
      THROW_ERR("'jump! recieved incorrect # of args!"
        "\n     (jump! <optional-arg>)" << FCN_ERR("jump!",args));
    if(args.size() == 1)
      G::JUMP_GLOBAL_PRIMITIVE_ARGUMENT = args[0];
    else
      G::JUMP_GLOBAL_PRIMITIVE_ARGUMENT = data(); // undefined jump! value
    throw SCM_EXCEPT::JUMP;
    return data();
  }

  data primitive_CATCH_JUMP(scm_list& args) {
    // extract the local environment
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      THROW_ERR("'catch-jump recieved incorrect # of args!"
        "\n     (catch-jump <proc> <arg1> ... <argN>)" << FCN_ERR("catch-jump",args));
    primitive_confirm_data_is_a_procedure(args[0], "catch-jump", 
      "\n     (catch-jump <proc> <arg1> ... <argN>)", args);
    scm_list catch_jump_args(args.begin()+1,args.end());
    if(catch_jump_args.empty()) catch_jump_args.push_back(symconst::sentinel_arg);
    const bool inline_status = G::USING_INLINE_INVOCATIONS;
    try {
      return data_cast(execute_application(args[0].exp,catch_jump_args,env));
    } catch(const SCM_EXCEPT& jump_error) {
      G::USING_INLINE_INVOCATIONS = inline_status;
      if(jump_error == SCM_EXCEPT::JUMP)
        return G::JUMP_GLOBAL_PRIMITIVE_ARGUMENT;
      throw jump_error;
    }
    return data();
  }

  // Returns quoted list of data macro-expanded (returns data as-is if not a macro):
  data primitive_EXPAND(scm_list& args) {
    // Extract the environment
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 1)
      THROW_ERR("'expand expects 1 arg: (expand <quoted-macro-exp>)"
        "\n     (expand <quoted-macro-exp>)" << FCN_ERR("expand",args));
    // Atomics can't be macro applications
    if(!args[0].is_type(types::par)) return args[0];
    // Expand Macro as needed
    return prm_recursively_deep_expand_macros(args[0],env);
  }

  // Invoke <proc> w/ args & trace the application (esp. helpful to trace recursion)
  // NOTE: The procedure MUST be a NAMED procedure (no anonymous lambda tracing support)!
  data primitive_TRACE(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      THROW_ERR("'trace recieved incorrect # of args!"
        "\n     (trace <proc> <arg1> ... <argN>)" << FCN_ERR("trace",args));
    primitive_confirm_data_is_a_procedure(args[0], "trace", 
      "\n     (trace <proc> <arg1> ... <argN>)", args);
    scm_list trace_args(args.begin()+1,args.end());
    if(trace_args.empty()) trace_args.push_back(symconst::sentinel_arg);
    // Set name of the function to trace
    if(args[0].exp[0].sym == symconst::procedure)
      G::TRACED_FUNCTION_NAME = args[0].exp[5].sym;
    else
      G::TRACED_FUNCTION_NAME = args[0].exp[2].sym;
    auto result = data_cast(execute_application(args[0].exp,trace_args,env));
    G::TRACED_FUNCTION_NAME = "";
    return result;
  }

  /******************************************************************************
  * GENSYM PRIMITIVES
  ******************************************************************************/

  data primitive_GENSYM(scm_list& args) {
    if(args.size() > 1)
      THROW_ERR("'gensym recieved incorrect # of args!"
        "\n     (gensym <optional-instance-#-to-reference>)" << FCN_ERR("gensym",args));
    if(args.size() == 1) {
      if(!args[0].is_type(types::num) || !args[0].num.is_integer() || args[0].num.is_neg())
        THROW_ERR("'gensym arg " << PROFILE(args[0]) << " isn't a non-negative integer!"
          "\n     (gensym <optional-instance-#-to-reference>)" << FCN_ERR("gensym",args));
      return decremented_hashed_gensym_arg((size_type)args[0].num.extract_inexact(),args);
    }
    return hygienically_hashed_gensym_arg();
  }


  data primitive_SOWN_GENSYM(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (sown-gensym <seed>)"
      "\n     => <seed> = number | char | symbol | boolean";
    if(args.size() != 1)
      THROW_ERR("'sown-gensym recieved incorrect # of args!" 
        << format << FCN_ERR("sown-gensym",args));
    if(!args[0].is_type(types::num) && !args[0].is_type(types::chr) && 
       !args[0].is_type(types::sym) && !args[0].is_type(types::bol)) {
      THROW_ERR("'sown-gensym arg "<<PROFILE(args[0])<<" isn't a valid 'sown-gensym key!" 
        << format << FCN_ERR("sown-gensym",args));
    }
    return symconst::gensym_prefix + scm_string("SOWN-") + args[0].write();
  }

  /******************************************************************************
  * SYNTAX PREDICATE PRIMITIVES
  ******************************************************************************/

  data primitive_CORE_SYNTAXP(scm_list& args) {
    if(args.size() != 1 || !args[0].is_type(types::sym))
      THROW_ERR("'core-syntax? didn't recieve 1 symbolic arg!"
        "\n     (core-syntax? <symbol>)" << FCN_ERR("core-syntax?",args));
    for(const auto& core_syntax_label : G::ANALYSIS_TIME_MACRO_LABEL_REGISTRY)
      if(core_syntax_label == args[0].sym)
        return G::TRUE_DATA_BOOLEAN;
    return G::FALSE_DATA_BOOLEAN;
  }


  data primitive_RUNTIME_SYNTAXP(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.size() != 1 || !args[0].is_type(types::sym))
      THROW_ERR("'runtime-syntax? didn't recieve 1 symbolic arg!"
        "\n     (runtime-syntax? <symbol>)" << FCN_ERR("runtime-syntax?",args));
    // Confirm not core-syntax
    for(const auto& core_syntax_label : G::ANALYSIS_TIME_MACRO_LABEL_REGISTRY)
      if(core_syntax_label == args[0].sym)
        return G::FALSE_DATA_BOOLEAN;
    // Search for macro in the environment
    const auto& label = args[0].sym;
    for(size_type i = 0, total_frames = env->size(); i < total_frames; ++i){
      // Get Variables & Values Lists of the current frame
      auto& [var_list, val_list, mac_list] = *env->operator[](i);
      // Search Variable-Value List Pair In Frame
      for(size_type j = 0, total_macs = mac_list.size(); j < total_macs; ++j)
        if(label == mac_list[j].label)
          return G::TRUE_DATA_BOOLEAN;
    }
    return G::FALSE_DATA_BOOLEAN;
  }


  data primitive_READER_SYNTAXP(scm_list& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      THROW_ERR("'reader-syntax? didn't recieve 1 string arg!"
        "\n     => Must be a string to avoid expansion by the reader if IS syntax!"
        "\n     (reader-syntax? <string>)" << FCN_ERR("reader-syntax?",args));
    auto& sought_shorthand = *args[0].str;
    for(const auto& reader_syntax_label : G::SHORTHAND_READER_MACRO_REGISTRY)
      if(reader_syntax_label == sought_shorthand)
        return G::TRUE_DATA_BOOLEAN;
    return G::FALSE_DATA_BOOLEAN;
  }

  /******************************************************************************
  * SYNTAX MUTATING PRIMITIVES
  ******************************************************************************/

  data primitive_SET_CORE_SYNTAX_BANG(scm_list& args) {
    confirm_proper_set_syntax_args(args,"set-core-syntax!",
      "\n     (set-core-syntax! <old-name-symbol> <optional-new-name-symbol>)" 
      "\n     => LEAVING OUT <new-name-symbol> DELETES <old-name-symbol>");
    // Modify the registry
    size_type i = 0, n = G::ANALYSIS_TIME_MACRO_LABEL_REGISTRY.size();
    for(; i < n; ++i)
      if(G::ANALYSIS_TIME_MACRO_LABEL_REGISTRY[i] == args[0].sym) {
        if(args.size() == 1) // Rm from registry
          G::ANALYSIS_TIME_MACRO_LABEL_REGISTRY.erase(G::ANALYSIS_TIME_MACRO_LABEL_REGISTRY.begin()+i);
        else                 // Change name in registry
          G::ANALYSIS_TIME_MACRO_LABEL_REGISTRY[i] = args[1].sym;
        break;
      }
    if(i == n) return G::FALSE_DATA_BOOLEAN; // not found
    // Delete core syntax as needed
    if(args.size() == 1)
      return delete_macro_from_env(args[0].sym,G::GLOBAL_ENVIRONMENT_POINTER);
    // Relabel core syntax as needed
    return relabel_macro_in_env(args[0].sym,args[1].sym,G::GLOBAL_ENVIRONMENT_POINTER);
  }


  data primitive_SET_RUNTIME_SYNTAX_BANG(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    confirm_proper_set_syntax_args(args,"set-runtime-syntax!",
      "\n     (set-runtime-syntax! <old-name-symbol> <optional-new-name-symbol>)" 
      "\n     => LEAVING OUT <new-name-symbol> DELETES <old-name-symbol>");
    // NOTE: Can't check registry, since same name may be dispersed across environment frames
    // Delete core syntax as needed
    if(args.size() == 1)
      return delete_macro_from_env(args[0].sym,env);
    // Relabel core syntax as needed
    auto result = relabel_macro_in_env(args[0].sym,args[1].sym,env);
    if(result.bol.val) G::MACRO_LABEL_REGISTRY.push_back(args[1].sym); // if found, add to cumulative registry
    return result;
  }

  /******************************************************************************
  * READER MACRO DEFINITION PRIMITIVE
  ******************************************************************************/

  data primitive_DEFINE_READER_SYNTAX(scm_list& args) {
    if(args.empty() || args.size() > 2 || !args[0].is_type(types::str) || 
      (args.size() == 2 && !args[1].is_type(types::str)))
      THROW_ERR("'define-reader-syntax improper arg signature!"
        "\n     (define-reader-syntax <shorthand-string> <optional-longhand-string>)"
        << FCN_ERR("define-reader-syntax",args));
    // Delete Reader Macro
    if(args.size() == 1) return delete_reader_macro(*args[0].str);
    // Define/Redefine Reader Macro
    register_reader_macro(*args[0].str,*args[1].str);
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * MATHEMATICAL CONSTANTS
  ******************************************************************************/

  constexpr const char* const MATHEMATICAL_FLONUM_CONSTANTS = R"(
  (define fl-e (exp 1)) ; Bound to the mathematical constant e

  (define fl-1/e (/ fl-e)) ; Bound to 1/e

  (define fl-e-2 (exp 2)) ; Bound to e^2

  (define fl-pi (acos -1)) ; Bound to the mathematical constant π

  (define fl-1/pi (/ fl-pi)) ; Bound to 1/π

  (define fl-2pi (* fl-pi 2)) ; Bound to 2π

  (define fl-pi/2 (/ fl-pi 2)) ; Bound to π/2

  (define fl-pi/4 (/ fl-pi 4)) ; Bound to π/4

  (define fl-pi-squared (* fl-pi fl-pi)) ; Bound to π^2

  (define fl-rad/deg (/ fl-pi 180)) ; Bound to π/180, the number of radians in a degree.

  (define fl-deg/rad (/ fl-rad/deg)) ; Bound to 180/π, the number of degrees in a radian.

  (define fl-2/pi (/ 2 fl-pi)) ; Bound to 2/π

  (define fl-2/sqrt-pi (/ 2 (sqrt fl-pi))) ; Bound to 2/√π

  (define fl-e-pi/4 (exp fl-pi/4)) ; Bound to e^(π/4)

  (define fl-log2-e (/ (log 2))) ; Bound to log2(e)

  (define fl-log10-e (/ (log 10))) ; Bound to log10(e)

  (define fl-log-2 (log 2)) ; Bound to loge(2)

  (define fl-1/log-2 (/ fl-log-2)) ; Bound to 1/loge(2)

  (define fl-log-3 (log 3)) ; Bound to loge(3)

  (define fl-log-pi (log fl-pi)) ; Bound to loge(π)

  (define fl-log-10 (log 10)) ; Bound to loge(10)

  (define fl-1/log-10 (/ fl-log-10)) ; Bound to 1/loge(10)

  (define fl-sqrt-2 (sqrt 2)) ; Bound to √2

  (define fl-sqrt-3 (sqrt 3)) ; Bound to √3

  (define fl-sqrt-5 (sqrt 5)) ; Bound to √5

  (define fl-sqrt-10 (sqrt 10)) ; Bound to √10

  (define fl-1/sqrt-2 (/ fl-sqrt-2)) ; Bound to 1/√2

  (define fl-cbrt-2 (expt 2 (/ 3))) ; Bound to 2^(1/3)

  (define fl-cbrt-3 (expt 3 (/ 3))) ; Bound to 3^(1/3)

  (define fl-4thrt-2 (expt 2 (/ 4))) ; Bound to 2^(1/4)

  (define fl-phi 1.618033988749895) ; Bound to the mathematical constant φ

  (define fl-log-phi (log fl-phi)) ; Bound to loge(φ)

  (define fl-1/log-phi (/ fl-log-phi)) ; Bound to 1/loge(φ)

  (define fl-euler 0.5772156649015329) ; Bound to the mathematical constant γ (Euler's constant)

  (define fl-e-euler (exp fl-euler)) ; Bound to e^γ

  (define fl-sin-1 (sin 1)) ; Bound to sin(1)

  (define fl-cos-1 (cos 1)) ; Bound to cos(1)

  (define fl-gamma-1/2 (sqrt fl-pi)) ; Bound to Γ(1/2) = √π

  (define fl-gamma-1/3 2.678938534707748) ; Bound to Γ(1/3)

  (define fl-gamma-2/3 1.3541179394264) ; Bound to Γ(2/3)
  )";

  /******************************************************************************
  * CURRY MACRO TO ENABLE CURRING LAMBDAS, CALL/CC, CPS->SCM
  ******************************************************************************/

  constexpr const char* const HEIST_CURRIED_LAMBDA_ID_CPS_PRIM_DEFINITIONS = R"(
  ; CURRIED LAMBDAS via Macro
  ;   => NOTE: It is UNDEFINED BEHAVIOR to have a VARIADIC CURRIED lambda
  ;            IE: (define f (curry (x . xs) x)) ;; INVALID!
  ;   => 'lambda alternative that will curry its arguments
  ;      => IE (define K (curry (a b) a)) => (define K (lambda (a) (lambda (b) a)))
  ;            ((K 1) 2) = (K 1 2) ;; BOTH OF THESE MEANS OF APPLICATION WORK IDENTICALLY!
  (core-syntax curry ()
    ((_ () body ...)
      (lambda () body ...))
    ((_ (arg) body ...) 
      (lambda (x . xs)
        (fold (lambda (f a) (f a)) 
              (lambda (a) a)
              (cons (lambda (arg) body ...)
                    (cons x xs)))))
    ((_ (arg rest-args ...) body ...)
      (lambda (x . xs)
        (define curried-lambdas
          (lambda (arg) (curry (rest-args ...) body ...)))
        (fold (lambda (f a) (f a)) 
              (lambda (a) a)
              (cons curried-lambdas
                    (cons x xs))))))

  ; COMPOSE PROCS (RETURNS A PROC OF N ARGS APPLYING THEM TO THE COMPOSITION)
  (define (compose proc . procs)
    (define fst-proc (last (cons proc procs)))
    (define rest-procs (init (cons proc procs)))
    (lambda (. args)
      (define fst-result (apply fst-proc args))
      (fold (lambda (result fcn) (fcn result)) fst-result rest-procs)))

  ; IDENTITY PRIMITIVE PROCEDURE
  (define (id a) a)

  ; CALL/CC (ONLY WORKS IN scm->cps BLOCKS!)
  (define (heist:core:pass-continuation-call/cc f k)
    (define (heist:core:pass-continuation-k-kestrel a b) (k a)) ; ignore 2nd cont. passed by CPS-ification
    (f heist:core:pass-continuation-k-kestrel k))
  (define call/cc heist:core:pass-continuation-call/cc)
  (define call-with-current-continuation heist:core:pass-continuation-call/cc)

  ; CPS-EVAL & CPS-LOAD
  (define cps-eval heist:core:pass-continuation-cps-eval)
  (define cps-load heist:core:pass-continuation-cps-load)

  ; CPS->SCM (BINDS 'id TO THE GIVEN PROC'S CONTINUATION)[FOR scm->cps & REGULAR CODE HIGHER-ORDER FCN INTEROP]
  (define (heist:core:pass-continuation-cps->scm proc k)
    (k (lambda (. args) (apply proc (append args (cons id '()))))))
  (define cps->scm heist:core:pass-continuation-cps->scm)
  )";

  /******************************************************************************
  * DEFSTRUCT MACRO FOR SIMPLE OO
  ******************************************************************************/

  constexpr const char* const HEIST_DEFSTRUCT_DEFINITION = R"(
  ;; Convert each <member-name> instance from <member-list> in <method-body-list>
  ;;   to be (<struct-name>-<member-name> this) instead
  (define (heist:core:ctor-defmethod-body struct-name member-list method-body-list)
    (define member-setters (map (lambda (s) (cons (symbol-append 'set- s '!) s)) member-list))
    (define (member-name? d) (and (symbol? d) (memq d member-list)))
    (define (member-setter? d) (and (symbol? d) (assq d member-setters)))
    (define (generate-setter exp)
      (set-car! exp (symbol-append 'set- struct-name '- (cdr (assq (car exp) member-setters)) '!))
      (set-cdr! exp (cons 'this (cdr exp))))
    (define (expand-method-exp exp)
      (if (not (null? exp))
          (begin (if (member-name? (car exp))
                     (set-car! exp (list (symbol-append struct-name '- (car exp)) 'this))
                     (if (member-setter? (car exp))
                         (generate-setter exp)
                         (if (pair? (car exp)) (expand-method-exp (car exp)))))
                 (expand-method-exp (cdr exp)))))
    (expand-method-exp method-body-list)
    method-body-list)


  ;; Return a quoted syntax-rules list for defining a <struct-name> method macro
  (define (heist:core:ctor-defmethod-syntax-rules struct-name)
    `(syntax-rules ()
      ((_ (method-name arg ...) body ...) ; METHOD W/ ARGS
        (eval `(define (,(symbol-append ',(symbol-append struct-name '>) 'method-name) this arg ...)
                  ,@(heist:core:ctor-defmethod-body ',struct-name (,(symbol-append struct-name '>slots)) '(body ...)))) 'local-environment)
      ((_ (method-name) body ...) ; METHOD W/O ARGS
        (eval `(define (,(symbol-append ',(symbol-append struct-name '>) 'method-name) this)
                  ,@(heist:core:ctor-defmethod-body ',struct-name (,(symbol-append struct-name '>slots)) '(body ...)))  'local-environment))))


  ;; DEFINES A "STRUCTURE" OBJECT BASED ON VECTOR ACCESS
  ;; => CREATES A CTOR, GETTER, SETTER, PREDICATE, ANALYSIS, AND METHOD-GENERATOR FOR THE STRUCTURE
  ;;    => WARNING: IT IS UNDEFINED BEHAVIOR TO HAVE "this" BE A MEMBER NAME OF ANY STRUCT
  ;; => (defstruct <name> <member-name-1> ... <member-name-N>)
  ;;    -> CTOR: Returns a <name> object w/ member values of <member-val-1> ... <member-val-N>
  ;;             (make-<name> <member-val-1> ... <member-val-N>) 
  ;;    -> GETTER: Returns <member-name> value (or #f if DNE) of <name> structure object <object>
  ;;             (<name>-<member-name> <object>) 
  ;;    -> SETTER: Sets <member-name> value to <new-val> of <name> structure object <object> (returns #f if dne)
  ;;             (set-<name>-<member-name>! <object> <new-val>) 
  ;;    -> PREDICATE: Returns whether <object> is a <name> struct
  ;;             (<name>? <object>) 
  ;;    -> ANALYSIS: Returns a quoted list of <name> struct's member names
  ;;             (<name>>slots) 
  ;;    -> METHOD-GENERATOR: Defines an interface to create struct methods
  ;;             (defmethod-<name> (<method-name> <arg-1> ... <arg-N>) <body>)
  ;;             => INVOKING METHODS: (<name>><method-name> <object> <arg-1> ... <arg-N>)
  ;;             => ADVANTAGES OF METHODS:
  ;;                1) <object> ARG IS AUTOMATICALLY ADDED AS this
  ;;                2) MEMBER SETTERS DON'T NEED THE OBJECT OR STRUCT NAME IN THE INVOCATION
  ;;                3) MEMBER GETTERS MAY BE INVOKED JUST BY USING THE MEMBER NAME
  ;;                >>> Suppose: (defstruct student name id) 
  ;;                             (define (printf . d) (for-each display d))
  ;;                   ; Writing:
  ;;                   (defmethod-student (greet your-name)
  ;;                      (set-id! (+ id 1))
  ;;                      (printf "Hello " your-name 
  ;;                              ", my name is " name " and my id is "
  ;;                              id ", great to meet you!\n"))
  ;;                   ; Gets expanded into:
  ;;                   (define (student>greet this your-name)
  ;;                      (set-student-id! this (+ (student-id this) 1))
  ;;                      (printf "Hello " your-name 
  ;;                              ", my name is " (student-name this) " and my id is "
  ;;                              (student-id this) ", great to meet you!\n"))
  (core-syntax defstruct ()
    ((_ name field ...)
      (eval (list 'define (cons (symbol-append 'make- 'name) '(field ...))
                '(vector 'name field ...)) 'local-environment)
      (eval (list 'define (list (symbol-append 'name '- 'field) 'obj)
                '(define res (assq 'field (map cons '(field ...) (iota (length '#(field ...)) 1))))
                '(if res
                     (ref obj (cdr res))
                     #f)) 'local-environment) ...
      (eval (list 'define (list (symbol-append 'set- 'name '- 'field '!) 'obj 'new-val)
                '(define res (assq 'field (map cons '(field ...) (iota (length '#(field ...)) 1))))
                '(if res
                     (set-index! obj (cdr res) new-val)
                     #f)) 'local-environment) ...
      (eval (list 'define (list (symbol-append 'name '?) 'obj)
                '(and (vector? obj)
                      (= (length obj) (+ 1 (length '#(field ...))))
                      (eq? (head obj) 'name))) 'local-environment)
      (eval (list 'define (list (symbol-append 'name '>slots)) 
                ''(field ...)) 'local-environment)
      (eval (list 'define-syntax (symbol-append 'defmethod- 'name)
                '(eval (heist:core:ctor-defmethod-syntax-rules 'name))) 'local-environment)))
  )";

  /******************************************************************************
  * TLAMBDA MACRO FOR AUTOMATED PREDICATED LAMBDA ARGUMENTS
  ******************************************************************************/

  constexpr const char* const HEIST_TLAMBDA_PRIM_DEFINITION = R"(
  ;; <tlambda-exp>'s args may be of form <symbol> or <(<pred?> <symbol>)>
  ;; where args are default checked against 'pred? (if such is present)
  (define (heist:core:tlambda->lambda tlambda-exp . err-message)
    (define err-prefix 
            (if (null? err-message) 
                "\"" 
                (append (car err-message) " \"")))
    (define pred?-errors '())
    (define (pred?->error pred?)
      (list 'if (list 'not pred?) 
                (list 'error ''tlambda 
                              (append (write pred? err-prefix) "\" Failed!")
                              (cadr pred?))))
    (define lambda-args 
      (map (lambda (arg) 
              (if (pair? arg)
                  (begin (set! pred?-errors 
                               (cons (pred?->error arg) pred?-errors))
                         (cadr arg))
                  arg))
           (cadr tlambda-exp)))
    (if (null? pred?-errors)
        tlambda-exp
        (cons 'lambda 
              (cons lambda-args 
                    (cons (cons 'begin pred?-errors) 
                          (cddr tlambda-exp))))))


  ;; Typed-Lambda Macro to automate predicates on arguments
  ;; Ex1: (tlambda ((string? s) any-arg (number? n)) <body>) ; predicated & arbitrary args
  ;; Ex2: (tlambda "optional-description" ((string? s) any-arg) <body>) ; optional descriptor
  ;; Ex3: (tlambda ((string? s) . ((lambda (ns) (every even? ns)) numbers)) <body>) ; predicated variadic 
  (core-syntax tlambda ()
    ((_ () b ...) (lambda () b ...)) ; 0 args
    ((_ (a ...) b ...)               ; N args
      (eval (heist:core:tlambda->lambda (cons 'lambda (cons (list 'a ...) '(b ...))))
            'local-environment))
    ((_ err-message (a ...) b ...)  ; optional-descriptor & N args
      (eval (heist:core:tlambda->lambda (cons 'lambda (cons (list 'a ...) '(b ...))) err-message)
            'local-environment)))
  )";

  /******************************************************************************
  * REGISTRY OF PRIMITIVES DEFINED _IN_ HEIST SCHEME TO EVAL PRIOR ANYTHING ELSE
  ******************************************************************************/

  // Single primitive procedures
  constexpr const char * const PRIMITIVES_DEFINED_IN_HEIST_SCHEME[] = {
    primitive_HEIST_STREAM_MAP,        primitive_HEIST_STREAM_FILTER, 
    primitive_HEIST_STREAM_FROM,       primitive_HEIST_STREAM_UNFOLD, 
    primitive_HEIST_STREAM_ITERATE,    primitive_HEIST_STREAM_ZIP, 
    primitive_HEIST_STREAM_CONSTANT,   primitive_HEIST_STREAM_APPEND, 
    primitive_HEIST_STREAM_INTERLEAVE, 
  };

  // Multi-expression primitive procedures
  constexpr const char * const PRIMITIVE_EXPRESSIONS_DEFINED_IN_HEIST_SCHEME[] {
    MATHEMATICAL_FLONUM_CONSTANTS, HEIST_CURRIED_LAMBDA_ID_CPS_PRIM_DEFINITIONS, 
    HEIST_DEFSTRUCT_DEFINITION,    HEIST_TLAMBDA_PRIM_DEFINITION, 
  };

  void evaluate_primitives_written_in_heist_scheme(env_type& env) {
    // Process individual heist scheme primitive procedures
    for(const auto& heist_prim : PRIMITIVES_DEFINED_IN_HEIST_SCHEME) {
      scm_list heist_scheme_prim;
      parse_input_exp(heist_prim, heist_scheme_prim);
      scm_eval(std::move(heist_scheme_prim[0].exp),env);
    }
    // Process individual heist scheme primitive multi-expression procedures
    scm_list heist_scheme_prim;
    for(const auto& heist_prim_exp : PRIMITIVE_EXPRESSIONS_DEFINED_IN_HEIST_SCHEME){
      heist_scheme_prim.clear();
      parse_input_exp(heist_prim_exp, heist_scheme_prim);
      for(auto& primitive_scm_procedure : heist_scheme_prim)
        scm_eval(std::move(primitive_scm_procedure.exp),env);
    }
  }

  /******************************************************************************
  * REGISTRY OF PRIMITIVES ALSO REQUIRING AN ENVIRONMENT (TO APPLY A PROCEDURE)
  ******************************************************************************/

  constexpr const prm_type ENV_REQUIRING_PRIMITIVES[] = {
    primitive_EVAL,             primitive_APPLY,                primitive_FOR_EACH,
    primitive_MAP,              primitive_FILTER,               primitive_MERGE,
    primitive_SORT,             primitive_SORT_BANG,            primitive_SORTEDP,
    primitive_READ,             primitive_READ_STRING,          primitive_FOLD,
    primitive_FOLD_RIGHT,       primitive_DELETE_NEIGHBOR_DUPS, primitive_UNFOLD,
    primitive_UNFOLD_RIGHT,     primitive_VECTOR_UNFOLD,        primitive_VECTOR_UNFOLD_RIGHT,
    primitive_STRING_UNFOLD,    primitive_STRING_UNFOLD_RIGHT,  primitive_GETENV,
    primitive_STRING_TRIM_BOTH, primitive_STRING_TRIM,          primitive_STRING_TRIM_RIGHT,
    primitive_COUNT,            primitive_SEQ_EQ,               primitive_DELETE_NEIGHBOR_DUPS_BANG,
    primitive_MAP_BANG,         primitive_INDEX,                primitive_INDEX_RIGHT,
    primitive_SKIP,             primitive_SKIP_RIGHT,           primitive_VECTOR_BINARY_SEARCH,
    primitive_EVERY,            primitive_ANY,                  primitive_TAKE_WHILE,
    primitive_TAKE_RIGHT_WHILE, primitive_DROP_WHILE,           primitive_DROP_RIGHT_WHILE,
    primitive_MAKE_LOG_BASE,    primitive_REMOVE_FIRST,         primitive_REMOVE_LAST, 
    primitive_REMOVE,

    primitive_STREAM_FOR_EACH,     primitive_STREAM_DROP_WHILE,
    primitive_STREAM_TAKE,         primitive_STREAM_TAKE_WHILE,
    primitive_STREAM_REVERSE,      primitive_CONVERT_STREAM_LIST,
    primitive_CONVERT_LIST_STREAM, primitive_STREAM_FOLD,
    primitive_STREAM_FOLD_RIGHT, 

    primitive_CALL_WITH_INPUT_FILE, primitive_CALL_WITH_OUTPUT_FILE,
    primitive_WITH_INPUT_FROM_FILE, primitive_WITH_OUTPUT_TO_FILE,
    primitive_OPEN_INPUT_FILE,      primitive_OPEN_OUTPUT_FILE,
    primitive_LOAD,                 primitive_CATCH_JUMP,
    primitive_INLINE,               primitive_CALL_CE, 
    primitive_CPS_EVAL,             primitive_CPS_LOAD, 
    primitive_EXPAND,               primitive_TRACE, 
    primitive_RUNTIME_SYNTAXP,      primitive_SET_RUNTIME_SYNTAX_BANG, 
  };

#ifndef HEIST_CPP_INTEROP_HPP_ // @NOT-EMBEDDED-IN-C++
  constexpr bool primitive_requires_environment(const prm_type& prm)noexcept{
    for(const auto& p : ENV_REQUIRING_PRIMITIVES)
      if(p == prm) return true;
    return false;
  }
#else // @EMBEDDED-IN-C++
  namespace G { std::vector<prm_type> USER_DEFINED_PRIMITIVES_REQUIRING_ENV; }
  bool primitive_requires_environment(const prm_type& prm)noexcept{
    for(const auto& p : ENV_REQUIRING_PRIMITIVES)
      if(p == prm) return true;
    for(const auto& p : G::USER_DEFINED_PRIMITIVES_REQUIRING_ENV)
      if(p == prm) return true;
    return false;
  }
#endif

  /******************************************************************************
  * PRIMITIVE NAMES & OBJECTS AS FRAME VARS & VALS FOR THE GLOBAL ENVIRONMENT
  ******************************************************************************/

  constexpr const std::pair<prm_type,const char*>primitive_procedure_declarations[]={
    std::make_pair(primitive_ADD, "+"),
    std::make_pair(primitive_SUB, "-"),
    std::make_pair(primitive_MUL, "*"),
    std::make_pair(primitive_DIV, "/"),

    std::make_pair(primitive_ABS,       "abs"),
    std::make_pair(primitive_EXPT,      "expt"),
    std::make_pair(primitive_EXPT_MOD,  "expt-mod"),
    std::make_pair(primitive_MAX,       "max"),
    std::make_pair(primitive_MIN,       "min"),
    std::make_pair(primitive_QUOTIENT,  "quotient"),
    std::make_pair(primitive_REMAINDER, "remainder"),
    std::make_pair(primitive_MODULO,    "modulo"),
    std::make_pair(primitive_EXP,       "exp"),
    std::make_pair(primitive_LOG,       "log"),
    std::make_pair(primitive_SQRT,      "sqrt"),
    std::make_pair(primitive_GCD,       "gcd"),
    std::make_pair(primitive_LCM,       "lcm"),
    std::make_pair(primitive_MODF,      "modf"),

    std::make_pair(primitive_ODDP,      "odd?"),
    std::make_pair(primitive_EVENP,     "even?"),
    std::make_pair(primitive_POSITIVEP, "positive?"),
    std::make_pair(primitive_NEGATIVEP, "negative?"),
    std::make_pair(primitive_ZEROP,     "zero?"),
    std::make_pair(primitive_INFINITEP, "infinite?"),
    std::make_pair(primitive_FINITEP,   "finite?"),
    std::make_pair(primitive_NANP,      "nan?"),

    std::make_pair(primitive_CEILING,  "ceiling"),
    std::make_pair(primitive_FLOOR,    "floor"),
    std::make_pair(primitive_TRUNCATE, "truncate"),
    std::make_pair(primitive_ROUND,    "round"),

    std::make_pair(primitive_COERCE_INEXACT_TO_EXACT, "inexact->exact"),
    std::make_pair(primitive_COERCE_EXACT_TO_INEXACT, "exact->inexact"),
    std::make_pair(primitive_EXACTP,                  "exact?"),
    std::make_pair(primitive_INEXACTP,                "inexact?"),
    std::make_pair(primitive_INTEGERP,                "integer?"),
    std::make_pair(primitive_NUMERATOR,               "numerator"),
    std::make_pair(primitive_DENOMINATOR,             "denominator"),
    std::make_pair(primitive_MAKE_LOG_BASE,           "make-log-base"),

    std::make_pair(primitive_SIN,   "sin"),
    std::make_pair(primitive_COS,   "cos"),
    std::make_pair(primitive_TAN,   "tan"),
    std::make_pair(primitive_ASIN,  "asin"),
    std::make_pair(primitive_ACOS,  "acos"),
    std::make_pair(primitive_ATAN,  "atan"),
    std::make_pair(primitive_SINH,  "sinh"),
    std::make_pair(primitive_COSH,  "cosh"),
    std::make_pair(primitive_TANH,  "tanh"),
    std::make_pair(primitive_ASINH, "asinh"),
    std::make_pair(primitive_ACOSH, "acosh"),
    std::make_pair(primitive_ATANH, "atanh"),

    std::make_pair(primitive_LOGAND,      "logand"),
    std::make_pair(primitive_LOGOR,       "logor"),
    std::make_pair(primitive_LOGXOR,      "logxor"),
    std::make_pair(primitive_LOGNOT,      "lognot"),
    std::make_pair(primitive_LOGLSL,      "loglsl"),
    std::make_pair(primitive_LOGLSR,      "loglsr"),
    std::make_pair(primitive_LOGASR,      "logasr"),
    std::make_pair(primitive_LOGBITP,     "logbit?"),
    std::make_pair(primitive_LOGBIT1,     "logbit1"),
    std::make_pair(primitive_LOGBIT0,     "logbit0"),
    std::make_pair(primitive_LOGBIT_CMPL, "logbit~"),

    std::make_pair(primitive_RANDOM, "random"),

    std::make_pair(primitive_EQ,  "="),
    std::make_pair(primitive_GT,  ">"),
    std::make_pair(primitive_LT,  "<"),
    std::make_pair(primitive_GTE, ">="),
    std::make_pair(primitive_LTE, "<="),

    std::make_pair(primitive_EQP,    "eq?"),
    std::make_pair(primitive_EQVP,   "eqv?"),
    std::make_pair(primitive_EQUALP, "equal?"),
    std::make_pair(primitive_NOT,    "not"),

    std::make_pair(primitive_CHAR_EQ,          "char=?"),
    std::make_pair(primitive_CHAR_LT,          "char<?"),
    std::make_pair(primitive_CHAR_GT,          "char>?"),
    std::make_pair(primitive_CHAR_LTE,         "char<=?"),
    std::make_pair(primitive_CHAR_GTE,         "char>=?"),
    std::make_pair(primitive_CHAR_CI_EQ,       "char-ci=?"),
    std::make_pair(primitive_CHAR_CI_LT,       "char-ci<?"),
    std::make_pair(primitive_CHAR_CI_GT,       "char-ci>?"),
    std::make_pair(primitive_CHAR_CI_LTE,      "char-ci<=?"),
    std::make_pair(primitive_CHAR_CI_GTE,      "char-ci>=?"),
    std::make_pair(primitive_CHAR_ALPHABETICP, "char-alphabetic?"),
    std::make_pair(primitive_CHAR_NUMERICP,    "char-numeric?"),
    std::make_pair(primitive_CHAR_WHITESPACEP, "char-whitespace?"),
    std::make_pair(primitive_CHAR_UPPER_CASEP, "char-upper-case?"),
    std::make_pair(primitive_CHAR_LOWER_CASEP, "char-lower-case?"),
    std::make_pair(primitive_CHAR_UPCASE,      "char-upcase"),
    std::make_pair(primitive_CHAR_DOWNCASE,    "char-downcase"),

    std::make_pair(primitive_MAKE_STRING,           "make-string"),
    std::make_pair(primitive_STRING,                "string"),
    std::make_pair(primitive_STRING_UNFOLD,         "string-unfold"),
    std::make_pair(primitive_STRING_UNFOLD_RIGHT,   "string-unfold-right"),
    std::make_pair(primitive_STRING_PAD,            "string-pad"),
    std::make_pair(primitive_STRING_PAD_RIGHT,      "string-pad-right"),
    std::make_pair(primitive_STRING_TRIM,           "string-trim"),
    std::make_pair(primitive_STRING_TRIM_RIGHT,     "string-trim-right"),
    std::make_pair(primitive_STRING_TRIM_BOTH,      "string-trim-both"),
    std::make_pair(primitive_STRING_REPLACE,        "string-replace"),
    std::make_pair(primitive_STRING_CONTAINS,       "string-contains"),
    std::make_pair(primitive_STRING_CONTAINS_RIGHT, "string-contains-right"),
    std::make_pair(primitive_STRING_JOIN,           "string-join"),
    std::make_pair(primitive_STRING_SPLIT,          "string-split"),
    std::make_pair(primitive_STRING_SWAP_BANG,      "string-swap!"),
    std::make_pair(primitive_STRING_PUSH_BANG,      "string-push!"),
    std::make_pair(primitive_STRING_EMPTYP,         "string-empty?"),
    std::make_pair(primitive_STRING_COPY_BANG,      "string-copy!"),

    std::make_pair(primitive_STRING_EQP,    "string=?"),
    std::make_pair(primitive_STRING_LT,     "string<?"),
    std::make_pair(primitive_STRING_GT,     "string>?"),
    std::make_pair(primitive_STRING_LTE,    "string<=?"),
    std::make_pair(primitive_STRING_GTE,    "string>=?"),
    std::make_pair(primitive_STRING_CI_EQ,  "string-ci=?"),
    std::make_pair(primitive_STRING_CI_LT,  "string-ci<?"),
    std::make_pair(primitive_STRING_CI_GT,  "string-ci>?"),
    std::make_pair(primitive_STRING_CI_LTE, "string-ci<=?"),
    std::make_pair(primitive_STRING_CI_GTE, "string-ci>=?"),

    std::make_pair(primitive_SYMBOL_APPEND, "symbol-append"),

    std::make_pair(primitive_TYPEOF, "typeof"),

    std::make_pair(primitive_CONS,           "cons"),
    std::make_pair(primitive_CAR,            "car"),
    std::make_pair(primitive_CDR,            "cdr"),
    std::make_pair(primitive_NULLP,          "null?"),
    std::make_pair(primitive_SETCAR,         "set-car!"),
    std::make_pair(primitive_SETCDR,         "set-cdr!"),
    std::make_pair(primitive_PAIR_SWAP_BANG, "pair-swap!"),

    std::make_pair(primitive_CAAR, "caar"),
    std::make_pair(primitive_CADR, "cadr"),
    std::make_pair(primitive_CDAR, "cdar"),
    std::make_pair(primitive_CDDR, "cddr"),
    std::make_pair(primitive_CAAAR, "caaar"),
    std::make_pair(primitive_CAADR, "caadr"),
    std::make_pair(primitive_CADAR, "cadar"),
    std::make_pair(primitive_CADDR, "caddr"),
    std::make_pair(primitive_CDAAR, "cdaar"),
    std::make_pair(primitive_CDADR, "cdadr"),
    std::make_pair(primitive_CDDAR, "cddar"),
    std::make_pair(primitive_CDDDR, "cdddr"),
    std::make_pair(primitive_CAAAAR, "caaaar"),
    std::make_pair(primitive_CAAADR, "caaadr"),
    std::make_pair(primitive_CAADAR, "caadar"),
    std::make_pair(primitive_CAADDR, "caaddr"),
    std::make_pair(primitive_CADAAR, "cadaar"),
    std::make_pair(primitive_CADADR, "cadadr"),
    std::make_pair(primitive_CADDAR, "caddar"),
    std::make_pair(primitive_CADDDR, "cadddr"),
    std::make_pair(primitive_CDAAAR, "cdaaar"),
    std::make_pair(primitive_CDAADR, "cdaadr"),
    std::make_pair(primitive_CDADAR, "cdadar"),
    std::make_pair(primitive_CDADDR, "cdaddr"),
    std::make_pair(primitive_CDDAAR, "cddaar"),
    std::make_pair(primitive_CDDADR, "cddadr"),
    std::make_pair(primitive_CDDDAR, "cdddar"),
    std::make_pair(primitive_CDDDDR, "cddddr"),

    std::make_pair(primitive_LIST,           "list"),
    std::make_pair(primitive_LIST_STAR,      "list*"),
    std::make_pair(primitive_MAKE_LIST,      "make-list"),
    std::make_pair(primitive_IOTA,           "iota"),
    std::make_pair(primitive_CIRCULAR_LIST,  "circular-list"),
    std::make_pair(primitive_CIRCULAR_LISTP, "circular-list?"),
    std::make_pair(primitive_LIST_STARP,     "list*?"),
    std::make_pair(primitive_LISTP,          "list?"),
    std::make_pair(primitive_ALISTP,         "alist?"),
    std::make_pair(primitive_LAST_PAIR,      "last-pair"),
    std::make_pair(primitive_UNFOLD,         "unfold"),
    std::make_pair(primitive_UNFOLD_RIGHT,   "unfold-right"),

    std::make_pair(primitive_MEMQ,   "memq"),
    std::make_pair(primitive_MEMV,   "memv"),
    std::make_pair(primitive_MEMBER, "member"),
    std::make_pair(primitive_ASSQ,   "assq"),
    std::make_pair(primitive_ASSV,   "assv"),
    std::make_pair(primitive_ASSOC,  "assoc"),

    std::make_pair(primitive_VECTOR,               "vector"),
    std::make_pair(primitive_MAKE_VECTOR,          "make-vector"),
    std::make_pair(primitive_VECTOR_PUSH_BANG,     "vector-push!"),
    std::make_pair(primitive_VECTOR_IOTA,          "vector-iota"),
    std::make_pair(primitive_VECTOR_EMPTYP,        "vector-empty?"),
    std::make_pair(primitive_VECTOR_GROW,          "vector-grow"),
    std::make_pair(primitive_VECTOR_UNFOLD,        "vector-unfold"),
    std::make_pair(primitive_VECTOR_UNFOLD_RIGHT,  "vector-unfold-right"),
    std::make_pair(primitive_VECTOR_COPY_BANG,     "vector-copy!"),
    std::make_pair(primitive_VECTOR_SWAP_BANG,     "vector-swap!"),
    std::make_pair(primitive_VECTOR_BINARY_SEARCH, "vector-binary-search"),

    std::make_pair(primitive_EMPTY,             "empty"),
    std::make_pair(primitive_LENGTH,            "length"),
    std::make_pair(primitive_LENGTH_PLUS,       "length+"),
    std::make_pair(primitive_REVERSE,           "reverse"),
    std::make_pair(primitive_REVERSE_BANG,      "reverse!"),
    std::make_pair(primitive_FOLD,              "fold"),
    std::make_pair(primitive_FOLD_RIGHT,        "fold-right"),
    std::make_pair(primitive_FILTER,            "filter"),
    std::make_pair(primitive_MAP,               "map"),
    std::make_pair(primitive_MAP_BANG,          "map!"),
    std::make_pair(primitive_FOR_EACH,          "for-each"),
    std::make_pair(primitive_COPY,              "copy"),
    std::make_pair(primitive_COPY_BANG,         "copy!"),
    std::make_pair(primitive_COUNT,             "count"),
    std::make_pair(primitive_REF,               "ref"),
    std::make_pair(primitive_SLICE,             "slice"),
    std::make_pair(primitive_SET_INDEX_BANG,    "set-index!"),
    std::make_pair(primitive_SWAP_INDICES_BANG, "swap-indices!"),
    std::make_pair(primitive_FILL_BANG,         "fill!"),
    std::make_pair(primitive_APPEND,            "append"),
    std::make_pair(primitive_REMOVE,            "remove"),
    std::make_pair(primitive_REMOVE_FIRST,      "remove-first"),
    std::make_pair(primitive_REMOVE_LAST,       "remove-last"),
    std::make_pair(primitive_DELETE,            "delete"),
    std::make_pair(primitive_LAST,              "last"),
    std::make_pair(primitive_TAIL,              "tail"),
    std::make_pair(primitive_HEAD,              "head"),
    std::make_pair(primitive_INIT,              "init"),
    std::make_pair(primitive_SEQ_EQ,            "seq="),
    std::make_pair(primitive_SKIP,              "skip"),
    std::make_pair(primitive_SKIP_RIGHT,        "skip-right"),
    std::make_pair(primitive_INDEX,             "index"),
    std::make_pair(primitive_INDEX_RIGHT,       "index-right"),
    std::make_pair(primitive_DROP,              "drop"),
    std::make_pair(primitive_DROP_RIGHT,        "drop-right"),
    std::make_pair(primitive_TAKE,              "take"),
    std::make_pair(primitive_TAKE_RIGHT,        "take-right"),
    std::make_pair(primitive_TAKE_WHILE,        "take-while"),
    std::make_pair(primitive_TAKE_RIGHT_WHILE,  "take-right-while"),
    std::make_pair(primitive_DROP_WHILE,        "drop-while"),
    std::make_pair(primitive_DROP_RIGHT_WHILE,  "drop-right-while"),
    std::make_pair(primitive_ANY,               "any"),
    std::make_pair(primitive_EVERY,             "every"),
    std::make_pair(primitive_CONJ,              "conj"),

    std::make_pair(primitive_SORT,                      "sort"),
    std::make_pair(primitive_SORT_BANG,                 "sort!"),
    std::make_pair(primitive_SORTEDP,                   "sorted?"),
    std::make_pair(primitive_MERGE,                     "merge"),
    std::make_pair(primitive_DELETE_NEIGHBOR_DUPS,      "delete-neighbor-dups"),
    std::make_pair(primitive_DELETE_NEIGHBOR_DUPS_BANG, "delete-neighbor-dups!"),

    std::make_pair(primitive_VOID,                 "void"),
    std::make_pair(primitive_VOIDP,                "void?"),
    std::make_pair(primitive_UNDEFINED,            "undefined"),
    std::make_pair(primitive_UNDEFINEDP,           "undefined?"),
    std::make_pair(primitive_EMPTYP,               "empty?"),
    std::make_pair(primitive_PAIRP,                "pair?"),
    std::make_pair(primitive_VECTORP,              "vector?"),
    std::make_pair(primitive_CHARP,                "char?"),
    std::make_pair(primitive_NUMBERP,              "number?"),
    std::make_pair(primitive_REALP,                "real?"),
    std::make_pair(primitive_RATIONALP,            "rational?"),
    std::make_pair(primitive_STRINGP,              "string?"),
    std::make_pair(primitive_SYMBOLP,              "symbol?"),
    std::make_pair(primitive_BOOLEANP,             "boolean?"),
    std::make_pair(primitive_ATOMP,                "atom?"),
    std::make_pair(primitive_PROCEDUREP,           "procedure?"),
    std::make_pair(primitive_CPS_PROCEDUREP,       "cps-procedure?"),
    std::make_pair(primitive_INPUT_PORTP,          "input-port?"),
    std::make_pair(primitive_OUTPUT_PORTP,         "output-port?"),
    std::make_pair(primitive_EOF_OBJECTP,          "eof-object?"),
    std::make_pair(primitive_STREAM_PAIRP,         "stream-pair?"),
    std::make_pair(primitive_STREAM_NULLP,         "stream-null?"),
    std::make_pair(primitive_STREAMP,              "stream?"),
    std::make_pair(primitive_SYNTAX_RULES_OBJECTP, "syntax-rules-object?"),
    std::make_pair(primitive_SEQP,                 "seq?"),

    std::make_pair(primitive_EVAL,     "eval"),
    std::make_pair(primitive_CPS_EVAL, "heist:core:pass-continuation-cps-eval"),
    std::make_pair(primitive_APPLY,    "apply"),

    std::make_pair(primitive_DELAYP, "delay?"),
    std::make_pair(primitive_FORCE,  "force"),

    std::make_pair(primitive_SCAR,                "scar"),
    std::make_pair(primitive_SCDR,                "scdr"),
    std::make_pair(primitive_STREAM_LENGTH,       "stream-length"),
    std::make_pair(primitive_STREAM_FOR_EACH,     "stream-for-each"),
    std::make_pair(primitive_STREAM_REF,          "stream-ref"),
    std::make_pair(primitive_STREAM_DROP,         "stream-drop"),
    std::make_pair(primitive_STREAM_DROP_WHILE,   "stream-drop-while"),
    std::make_pair(primitive_STREAM_TAKE,         "stream-take"),
    std::make_pair(primitive_STREAM_TAKE_WHILE,   "stream-take-while"),
    std::make_pair(primitive_STREAM_REVERSE,      "stream-reverse"),
    std::make_pair(primitive_STREAM_FOLD,         "stream-fold"),
    std::make_pair(primitive_STREAM_FOLD_RIGHT,   "stream-fold-right"),
    std::make_pair(primitive_CONVERT_STREAM_LIST, "stream->list"),
    std::make_pair(primitive_CONVERT_LIST_STREAM, "list->stream"),

    std::make_pair(primitive_SCAAR, "scaar"),
    std::make_pair(primitive_SCADR, "scadr"),
    std::make_pair(primitive_SCDAR, "scdar"),
    std::make_pair(primitive_SCDDR, "scddr"),
    std::make_pair(primitive_SCAAAR, "scaaar"),
    std::make_pair(primitive_SCAADR, "scaadr"),
    std::make_pair(primitive_SCADAR, "scadar"),
    std::make_pair(primitive_SCADDR, "scaddr"),
    std::make_pair(primitive_SCDAAR, "scdaar"),
    std::make_pair(primitive_SCDADR, "scdadr"),
    std::make_pair(primitive_SCDDAR, "scddar"),
    std::make_pair(primitive_SCDDDR, "scdddr"),
    std::make_pair(primitive_SCAAAAR, "scaaaar"),
    std::make_pair(primitive_SCAAADR, "scaaadr"),
    std::make_pair(primitive_SCAADAR, "scaadar"),
    std::make_pair(primitive_SCAADDR, "scaaddr"),
    std::make_pair(primitive_SCADAAR, "scadaar"),
    std::make_pair(primitive_SCADADR, "scadadr"),
    std::make_pair(primitive_SCADDAR, "scaddar"),
    std::make_pair(primitive_SCADDDR, "scadddr"),
    std::make_pair(primitive_SCDAAAR, "scdaaar"),
    std::make_pair(primitive_SCDAADR, "scdaadr"),
    std::make_pair(primitive_SCDADAR, "scdadar"),
    std::make_pair(primitive_SCDADDR, "scdaddr"),
    std::make_pair(primitive_SCDDAAR, "scddaar"),
    std::make_pair(primitive_SCDDADR, "scddadr"),
    std::make_pair(primitive_SCDDDAR, "scdddar"),
    std::make_pair(primitive_SCDDDDR, "scddddr"),

    std::make_pair(primitive_COERCE_CHAR_TO_INT,      "char->int"),
    std::make_pair(primitive_COERCE_INT_TO_CHAR,      "int->char"),
    std::make_pair(primitive_COERCE_NUMBER_TO_STRING, "number->string"),
    std::make_pair(primitive_COERCE_STRING_TO_NUMBER, "string->number"),
    std::make_pair(primitive_COERCE_STRING_TO_SYMBOL, "string->symbol"),
    std::make_pair(primitive_COERCE_SYMBOL_TO_STRING, "symbol->string"),
    std::make_pair(primitive_COERCE_VECTOR_TO_LIST,   "vector->list"),
    std::make_pair(primitive_COERCE_LIST_TO_VECTOR,   "list->vector"),
    std::make_pair(primitive_STRING_TO_VECTOR,        "string->vector"),
    std::make_pair(primitive_VECTOR_TO_STRING,        "vector->string"),
    std::make_pair(primitive_STRING_TO_LIST,          "string->list"),
    std::make_pair(primitive_LIST_TO_STRING,          "list->string"),

    std::make_pair(primitive_PPRINT,     "pprint"),
    std::make_pair(primitive_PPRINT,     "pretty-print"),
    std::make_pair(primitive_WRITE,      "write"),
    std::make_pair(primitive_DISPLAY,    "display"),
    std::make_pair(primitive_NEWLINE,    "newline"),
    std::make_pair(primitive_WRITE_CHAR, "write-char"),

    std::make_pair(primitive_READ,        "read"),
    std::make_pair(primitive_READ_STRING, "read-string"),
    std::make_pair(primitive_READ_LINE,   "read-line"),
    std::make_pair(primitive_READ_CHAR,   "read-char"),
    std::make_pair(primitive_PEEK_CHAR,   "peek-char"),
    std::make_pair(primitive_CHAR_READYP, "char-ready?"),
    std::make_pair(primitive_SLURP_PORT,  "slurp-port"),
    std::make_pair(primitive_SLURP_FILE,  "slurp-file"),

    std::make_pair(primitive_FILEP,                 "file?"),
    std::make_pair(primitive_DELETE_FILE_BANG,      "delete-file!"),
    std::make_pair(primitive_RENAME_FILE_BANG,      "rename-file!"),
    std::make_pair(primitive_OPEN_PORTP,            "open-port?"),
    std::make_pair(primitive_CLOSED_PORTP,          "closed-port?"),
    std::make_pair(primitive_CURRENT_INPUT_PORT,    "current-input-port"),
    std::make_pair(primitive_CURRENT_OUTPUT_PORT,   "current-output-port"),
    std::make_pair(primitive_CALL_WITH_INPUT_FILE,  "call-with-input-file"),
    std::make_pair(primitive_CALL_WITH_OUTPUT_FILE, "call-with-output-file"),
    std::make_pair(primitive_WITH_INPUT_FROM_FILE,  "with-input-from-file"),
    std::make_pair(primitive_WITH_OUTPUT_TO_FILE,   "with-output-to-file"),
    std::make_pair(primitive_OPEN_INPUT_FILE,       "open-input-file"),
    std::make_pair(primitive_OPEN_OUTPUT_FILE,      "open-output-file"),
    std::make_pair(primitive_CLOSE_PORT,            "close-port"),

    std::make_pair(primitive_LOAD,          "load"),
    std::make_pair(primitive_CPS_LOAD,      "heist:core:pass-continuation-cps-load"),
    std::make_pair(primitive_SYSTEM,        "system"),
    std::make_pair(primitive_GETENV,        "getenv"),
    std::make_pair(primitive_COMMAND_LINE,  "command-line"),
    std::make_pair(primitive_COMPILE,       "compile"),
    std::make_pair(primitive_CPS_COMPILE,   "cps-compile"),
    std::make_pair(primitive_GETCWD,        "getcwd"),
    std::make_pair(primitive_DIRNAME,       "dirname"),
    std::make_pair(primitive_HEIST_DIRNAME, "heist-dirname"),

    std::make_pair(primitive_SECONDS_SINCE_EPOCH, "seconds-since-epoch"),

    std::make_pair(primitive_SET_NANSI,                    "set-nansi!"),
    std::make_pair(primitive_SET_CI,                       "set-ci!"),
    std::make_pair(primitive_SET_PPRINT_COLUMN_WIDTH_BANG, "set-pprint-column-width!"),
    std::make_pair(primitive_SET_MAX_RECURSION_DEPTH,      "set-max-recursion-depth!"),
    std::make_pair(primitive_SET_REPL_PROMPT,              "set-repl-prompt!"),
    std::make_pair(primitive_SET_TRACE_CALLS_BANG,         "set-trace-calls!"),

    std::make_pair(primitive_EXIT,            "exit"),
    std::make_pair(primitive_ERROR,           "error"),
    std::make_pair(primitive_SYNTAX_ERROR,    "syntax-error"),
    std::make_pair(primitive_CALL_CE,         "call/ce"),
    std::make_pair(primitive_CALL_CE,         "call-with-current-environment"),
    std::make_pair(primitive_INLINE,          "inline"),
    std::make_pair(primitive_JUMP_BANG,       "jump!"),
    std::make_pair(primitive_CATCH_JUMP,      "catch-jump"),
    std::make_pair(primitive_EXPAND,          "expand"),
    std::make_pair(primitive_TRACE,           "trace"),

    std::make_pair(primitive_GENSYM,      "gensym"),
    std::make_pair(primitive_SOWN_GENSYM, "sown-gensym"),

    std::make_pair(primitive_CORE_SYNTAXP,    "core-syntax?"),
    std::make_pair(primitive_RUNTIME_SYNTAXP, "runtime-syntax?"),
    std::make_pair(primitive_READER_SYNTAXP,  "reader-syntax?"),

    std::make_pair(primitive_SET_CORE_SYNTAX_BANG,    "set-core-syntax!"),
    std::make_pair(primitive_SET_RUNTIME_SYNTAX_BANG, "set-runtime-syntax!"),

    std::make_pair(primitive_DEFINE_READER_SYNTAX, "define-reader-syntax"),
  };

  frame_vals primitive_procedure_objects()noexcept{
    constexpr const auto n = sizeof(primitive_procedure_declarations) / sizeof(primitive_procedure_declarations[0]);
    frame_vals primitive_procedures(n);
    for(size_type i = 0; i < n; ++i) {
      primitive_procedures[i] = scm_list(3);
      primitive_procedures[i].exp[0] = symconst::primitive;
      primitive_procedures[i].exp[1] = primitive_procedure_declarations[i].first;
      primitive_procedures[i].exp[2] = primitive_procedure_declarations[i].second;
    }
    return primitive_procedures;
  }

  frame_vars primitive_procedure_names()noexcept{
    constexpr const auto n = sizeof(primitive_procedure_declarations) / sizeof(primitive_procedure_declarations[0]);
    frame_vars names(n);
    for(size_type i = 0; i < n; ++i)
      names[i] = primitive_procedure_declarations[i].second;
    return names;
  }
} // End of namespace heist
#undef VALID_SEQUENCE_INDEX_RANGE // Sequence bounds only tested by primitives (def in toolkit)
#endif