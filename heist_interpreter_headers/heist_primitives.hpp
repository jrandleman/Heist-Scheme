// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_primitives.hpp
// => Defines primitive functions written in C++ for the Heist Scheme Interpreter

// PRIMITIVE FUNCTION MANDATORY TYPE SIGNATURE: 
// -> struct data(*)(scm_list&)

#ifndef HEIST_PRIMITIVES_HPP_
#define HEIST_PRIMITIVES_HPP_

#include "toolkits/heist_primitives_toolkit.hpp"

/******************************************************************************
* ARITHMETIC PRIMITIVES
******************************************************************************/

namespace heist {

  // primitive "+" procedure
  data primitive_ADD(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "+", "(+ <num1> <num2> ...)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("+",primitive_ADD,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("*",primitive_MUL,args);
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
    confirm_unary_real_numeric(args, "abs", "(abs <real>)");
    return data(args[0].num.abs());
  }

  // primitive "expt" procedure
  data primitive_EXPT(scm_list& args) {
    confirm_no_numeric_primitive_errors(args, "expt", "(expt <num1> <num2> ...)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("expt",primitive_EXPT,args);
    if(args.size() < 2)
      THROW_ERR("'expt less than 2 args given!\n     (expt <num1> <num2> ...)" 
        << FCN_ERR("expt",args));
    num_type pow(args.rbegin()->num);
    for(size_type i = args.size()-1; i-- > 0;)
      pow = args[i].num.expt(pow);
    return pow;
  }

  // primitive "expt-mod" procedure [more efficient (modulo (expt x y) z)]
  data primitive_EXPT_MOD(scm_list& args) {
    // Confirm valid arguments
    static constexpr const char * const format = "\n     (expt-mod <real1> <real2> <real3>)";
    if(args.empty() || args.size() > 3)
      THROW_ERR("'expt-mod invalid # of args given!" << format << FCN_ERR("expt-mod",args));
    for(size_type i = 0, n = args.size(); i < n; ++i)
      if(!args[i].is_type(types::num) || !args[i].num.is_integer() || args[i].num.is_neg())
        THROW_ERR("'expt-mod arg #" << i+1 << ' ' << PROFILE(args[i]) << " isn't a non-negative integer!" 
          << format << FCN_ERR("expt-mod",args));
    if(args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("expt-mod",primitive_EXPT_MOD,args);
    // Perform Repeated Squares
    auto &x = args[0].num, &z = args[2].num;
    auto y_bitstr = args[1].num.to_exact().str(2);
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
    confirm_no_real_numeric_primitive_errors(args, "max", "(max <real1> <real2> ...)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("max",primitive_MAX,args);
    num_type max = args[0].num;
    for(size_type i = 1, n = args.size(); i < n; ++i)
      if(args[i].num > max)
        max = args[i].num;
    return max;
  }

  // primitive "min" procedure
  data primitive_MIN(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "min", "(min <real1> <real2> ...)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("min",primitive_MIN,args);
    num_type min = args[0].num;
    for(size_type i = 1, n = args.size(); i < n; ++i)
      if(args[i].num < min)
        min = args[i].num;
    return min;
  }

  // primitive "quotient" procedure
  data primitive_QUOTIENT(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "quotient", "(quotient <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("quotient",primitive_QUOTIENT,args);
    confirm_2_args(args, "quotient", "(quotient <real1> <real2>)");
    return data(args[0].num.quotient(args[1].num));
  }

  // primitive "remainder" procedure
  data primitive_REMAINDER(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "remainder", "(remainder <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("remainder",primitive_REMAINDER,args);
    confirm_2_args(args, "remainder", "(remainder <real1> <real2>)");
    return data((args[0].num % args[1].num));
  }

  // primitive "divmod" procedure
  data primitive_DIVMOD(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "divmod", "(divmod <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("divmod",primitive_DIVMOD,args);
    confirm_2_args(args, "divmod", "(divmod <real1> <real2>)");
    auto result = args[0].num.divmod(args[1].num);
    data divmod_pair = data(make_par());
    divmod_pair.par->first = std::move(result.first);
    divmod_pair.par->second = std::move(result.second);
    return divmod_pair;
  }

  // primitive "modulo" procedure
  data primitive_MODULO(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "modulo", "(modulo <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("modulo",primitive_MODULO,args);
    confirm_2_args(args, "modulo", "(modulo <real1> <real2>)");
    return data(args[0].num.modulo(args[1].num));
  }

  // primitive "exp" procedure
  data primitive_EXP(scm_list& args) {
    confirm_unary_numeric(args, "exp", "(exp <num>)");
    return data(args[0].num.exp());
  }

  // primitive "log" procedure -- NATURAL LOGARITHM
  data primitive_LOG(scm_list& args) {
    if(args.empty() || args.size() > 2)
      THROW_ERR("'log didn't receive correct number of args!"
        << "\n     (log <num> <optional-base>)" << FCN_ERR("log", args));
    if(!args[0].is_type(types::num))
      THROW_ERR("'log 1st <num> arg "<<PROFILE(args[0])<<" isn't a number!"
        << "\n     (log <num> <optional-base>)" << FCN_ERR("log", args));
    // Perform natural log if only given one number
    if(args.size() == 1) return data(args[0].num.log());
    // Perform log base conversion
    if(!args[1].is_type(types::num))
      THROW_ERR("'log 2nd <base> arg "<<PROFILE(args[1])<<" isn't a number!"
        << "\n     (log <num> <optional-base>)" << FCN_ERR("log", args));
    return data(args[0].num.log() / args[1].num.log());
  }

  // primitive "sqrt" procedure
  data primitive_SQRT(scm_list& args) {
    confirm_unary_numeric(args, "sqrt", "(sqrt <num>)");
    return data(args[0].num.sqrt());
  }

  // primitive "gcd" procedure
  data primitive_GCD(scm_list& args) {
    if(args.empty()) return num_type();
    confirm_no_real_numeric_primitive_errors(args, "gcd", "(gcd <real1> <real2> ...)");
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
    confirm_no_real_numeric_primitive_errors(args, "lcm", "(lcm <real1> <real2> ...)");
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
    confirm_unary_real_numeric(args, "modf", "(modf <real>)");
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

  //  primitive "npr": P(n,r) = n! / (n - r)!
  data primitive_NPR(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "npr", "(npr <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("npr",primitive_NPR,args);
    confirm_2_args(args, "npr", "(npr <real1> <real2>)");
    auto n = args[0].num;
    return prm_factorial(std::move(args[0].num),num_type("1")) / 
           prm_factorial(n-args[1].num,num_type("1"));
  }

  //  primitive "ncr": C(n,r) = n! / (r! * (n - r)!)
  data primitive_NCR(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "ncr", "(ncr <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("ncr",primitive_NCR,args);
    confirm_2_args(args, "ncr", "(ncr <real1> <real2>)");
    auto n_r_difference = args[0].num - args[1].num;
    return prm_factorial(std::move(args[0].num),num_type("1")) / 
           (prm_factorial(std::move(args[1].num),num_type("1")) * prm_factorial(std::move(n_r_difference),num_type("1")));
  }

  /******************************************************************************
  * COMPLEX NUMBER PRIMITIVES
  ******************************************************************************/

  // primitive "make-polar" procedure
  data primitive_MAKE_POLAR(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "make-polar", "(make-polar <real-mag> <real-ang>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("make-polar",primitive_MAKE_POLAR,args);
    confirm_2_args(args, "make-polar", "(make-polar <real-mag> <real-ang>)");
    return num_type::make_polar(args[0].num,args[1].num);
  }

  // primitive "make-rectangular" procedure
  data primitive_MAKE_RECTANGULAR(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "make-rectangular", "(make-polar <real-real> <real-imag>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("make-rectangular",primitive_MAKE_RECTANGULAR,args);
    confirm_2_args(args, "make-rectangular", "(make-rectangular <real-real> <real-imag>)");
    return num_type::make_rectangular(args[0].num,args[1].num);
  }

  // primitive "real-part" procedure
  data primitive_REAL_PART(scm_list& args) {
    confirm_unary_numeric(args, "real-part", "(real-part <num>)");
    return args[0].num.real_part();
  }

  // primitive "imag-part" procedure
  data primitive_IMAG_PART(scm_list& args) {
    confirm_unary_numeric(args, "imag-part", "(imag-part <num>)");
    return args[0].num.imag_part();
  }

  // primitive "magnitude" procedure
  data primitive_MAGNITUDE(scm_list& args) {
    confirm_unary_numeric(args, "magnitude", "(magnitude <num>)");
    return args[0].num.magnitude();
  }

  // primitive "angle" procedure
  data primitive_ANGLE(scm_list& args) {
    confirm_unary_numeric(args, "angle", "(angle <num>)");
    return args[0].num.angle();
  }

  // primitive "conjugate" procedure
  data primitive_CONJUGATE(scm_list& args) {
    confirm_unary_numeric(args, "conjugate", "(conjugate <num>)");
    return args[0].num.conjugate();
  }

  /******************************************************************************
  * MISCELLANEOUS NUMERIC PREDICATE PRIMITIVES 
  ******************************************************************************/

  // primitive "odd?" procedure
  data primitive_ODDP(scm_list& args) {
    confirm_unary_real_numeric(args, "odd?", "(odd? <real>)");
    return data(boolean(args[0].num.is_odd()));
  }

  // primitive "even?" procedure
  data primitive_EVENP(scm_list& args) {
    confirm_unary_real_numeric(args, "even?", "(even? <real>)");
    return data(boolean(args[0].num.is_even()));
  }

  // primitive "positive?" procedure
  data primitive_POSITIVEP(scm_list& args) {
    confirm_unary_real_numeric(args, "positive?", "(positive? <real>)");
    return data(boolean(args[0].num.is_pos()));
  }

  // primitive "negative?" procedure
  data primitive_NEGATIVEP(scm_list& args) {
    confirm_unary_real_numeric(args, "negative?", "(negative? <real>)");
    return data(boolean(args[0].num.is_neg()));
  }

  // primitive "zero?" procedure
  data primitive_ZEROP(scm_list& args) {
    confirm_unary_numeric(args, "zero?", "(zero? <num>)");
    return data(boolean(args[0].num.is_zero()));
  }

  // primitive "not-positive?" procedure
  data primitive_NOT_POSITIVEP(scm_list& args) {
    confirm_unary_real_numeric(args, "not-positive?", "(not-positive? <real>)");
    return data(boolean(!args[0].num.is_pos()));
  }

  // primitive "not-negative?" procedure
  data primitive_NOT_NEGATIVEP(scm_list& args) {
    confirm_unary_real_numeric(args, "not-negative?", "(not-negative? <real>)");
    return data(boolean(!args[0].num.is_neg()));
  }

  // primitive "not-zero?" procedure
  data primitive_NOT_ZEROP(scm_list& args) {
    confirm_unary_numeric(args, "not-zero?", "(not-zero? <num>)");
    return data(boolean(!args[0].num.is_zero()));
  }

  // primitive "infinite?" procedure
  data primitive_INFINITEP(scm_list& args) {
    confirm_unary_real_numeric(args, "infinite?", "(infinite? <real>)");
    return data(boolean(args[0].num.is_pos_inf() || 
                        args[0].num.is_neg_inf()));
  }

  // primitive "finite?" procedure (same as primitive "real?")
  data primitive_FINITEP(scm_list& args) {
    confirm_unary_real_numeric(args, "finite?", "(finite? <real>)");
    return data(boolean(args[0].num.is_real() && !args[0].num.is_nan() && 
                        !args[0].num.is_pos_inf() && !args[0].num.is_neg_inf()));
  }

  // primitive "nan?" procedure
  data primitive_NANP(scm_list& args) {
    confirm_unary_real_numeric(args, "nan?", "(nan? <real>)");
    return data(boolean(args[0].num.is_nan()));
  }

  /******************************************************************************
  * NUMERIC ROUNDING PRIMITIVES 
  ******************************************************************************/

  // primitive "ceiling" procedure -- ROUNDS UP
  data primitive_CEILING(scm_list& args) {
    confirm_unary_real_numeric(args, "ceiling", "(ceiling <real>)");
    return data(args[0].num.ceil());
  }

  // primitive "floor" procedure -- ROUNDS DOWN
  data primitive_FLOOR(scm_list& args) {
    confirm_unary_real_numeric(args, "floor", "(floor <real>)");
    return data(args[0].num.floor());
  }

  // primitive "truncate" procedure -- ROUNDS TOWARDS ZERO
  data primitive_TRUNCATE(scm_list& args) {
    confirm_unary_real_numeric(args, "truncate", "(truncate <real>)");
    return data(args[0].num.trunc());
  }

  // primitive "round" procedure -- ROUNDS TOWARDS THE NEAREST INT
  data primitive_ROUND(scm_list& args) {
    confirm_unary_real_numeric(args, "round", "(round <real>)");
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

  // primitive "bigint?" procedure
  data primitive_BIGINTP(scm_list& args) {
    confirm_unary_numeric(args, "bigint?", "(bigint? <num>)");
    return data(boolean(args[0].num.is_exact() && args[0].num.is_integer()));
  }

  // primitive "numerator" procedure
  data primitive_NUMERATOR(scm_list& args) {
    confirm_unary_real_numeric(args, "numerator", "(numerator <real>)");
    return data(args[0].num.extract_numerator());
  }

  // primitive "denominator" procedure
  data primitive_DENOMINATOR(scm_list& args) {
    confirm_unary_real_numeric(args, "denominator", "(denominator <real>)");
    return data(args[0].num.extract_denominator());
  }

  // primitive "make-log-base" procedure
  // (define (make-log-base n) (lambda (num) (log num n)))
  data primitive_MAKE_LOG_BASE(scm_list& args) {
    confirm_unary_real_numeric(args, "make-log-base", "(make-log-base <real>)");
    scm_list new_log(3);
    new_log[0] = symconst::lambda;
    new_log[1] = scm_list(1, "num");
    new_log[2] = scm_list(3);
    new_log[2].exp[0] = "log";
    new_log[2].exp[1] = "num";
    new_log[2].exp[2] = args[0];
    return data_cast(scm_eval(std::move(new_log),G::GLOBAL_ENVIRONMENT_POINTER));
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

  // primitive "atan" procedure (supports atan2)
  data primitive_ATAN(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (atan <num>)\n     (atan <real> <real>) ; atan2 in C++";
    if(args.empty() || args.size() > 2)
      THROW_ERR("'atan received incorrect # of args!" << format << FCN_ERR("atan",args));
    if(!args[0].is_type(types::num))
      THROW_ERR("'atan 1st arg " << PROFILE(args[0]) << " isn't a number!" 
        << format << FCN_ERR("atan",args));
    if(args.size() == 1) return data(args[0].num.atan()); // atan
    confirm_no_real_numeric_primitive_errors(args, "atan", format);
    return data(args[0].num.atan2(args[1].num)); // atan2
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
    confirm_no_real_numeric_primitive_errors(args, "logand", "(logand <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("logand",primitive_LOGAND,args);
    confirm_2_args(args, "logand", "(logand <real1> <real2>)");
    return data(args[0].num & args[1].num);
  }

  // primitive "logor" procedure
  data primitive_LOGOR(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "logor", "(logor <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("logor",primitive_LOGOR,args);
    confirm_2_args(args, "logor", "(logor <real1> <real2>)");
    return data(args[0].num | args[1].num);
  }

  // primitive "logxor" procedure
  data primitive_LOGXOR(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "logxor", "(logxor <real1> <real2>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("logxor",primitive_LOGXOR,args);
    confirm_2_args(args, "logxor", "(logxor <real1> <real2>)");
    return data(args[0].num ^ args[1].num);
  }

  // primitive "lognot" procedure
  data primitive_LOGNOT(scm_list& args) {
    confirm_unary_real_numeric(args, "lognot", "(lognot <real>)");
    return data(~args[0].num);
  }

  // primitive "loglsl" procedure
  data primitive_LOGLSL(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "loglsl", "(loglsl <real> <shift-amount>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("loglsl",primitive_LOGLSL,args);
    confirm_2_args(args, "loglsl", "(loglsl <real> <shift-amount>)");
    return data(args[0].num << args[1].num);
  }

  // primitive "loglsr" procedure
  data primitive_LOGLSR(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "loglsr", "(loglsr <real> <shift-amount>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("loglsr",primitive_LOGLSR,args);
    confirm_2_args(args, "loglsr", "(loglsr <real> <shift-amount>)");
    return data(args[0].num >> args[1].num);
  }

  // primitive "logasr" procedure
  data primitive_LOGASR(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "logasr", "(logasr <real> <shift-amount>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("logasr",primitive_LOGASR,args);
    confirm_2_args(args, "logasr", "(logasr <real> <shift-amount>)");
    return data(args[0].num.asr(args[1].num));
  }

  // primitive "logbit?" procedure
  data primitive_LOGBITP(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "logbit?", "(logbit? <real> <bit-No>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("logbit?",primitive_LOGBITP,args);
    confirm_2_args(args, "logbit?", "(logbit? <real> <bit-No>)");
    return data((args[0].num >> args[1].num) & 1);
  }

  // primitive "logbit1" procedure
  data primitive_LOGBIT1(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "logbit1", "(logbit1 <real> <bit-No>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("logbit1",primitive_LOGBIT1,args);
    confirm_2_args(args, "logbit1", "(logbit1 <real> <bit-No>)");
    return data(args[0].num | (1 << args[1].num));
  }

  // primitive "logbit0" procedure
  data primitive_LOGBIT0(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "logbit0", "(logbit0 <real> <bit-No>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("logbit0",primitive_LOGBIT0,args);
    confirm_2_args(args, "logbit0", "(logbit0 <real> <bit-No>)");
    return data(args[0].num & ~(1 << args[1].num));
  }

  // primitive "logbit~" procedure
  data primitive_LOGBIT_CMPL(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "logbit~", "(logbit~ <real> <bit-No>)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("logbit~",primitive_LOGBIT_CMPL,args);
    confirm_2_args(args, "logbit~", "(logbit~ <real> <bit-No>)");
    return data(args[0].num ^ (1 << args[1].num));
  }

  /******************************************************************************
  * NUMERIC RANDOM NUMBER GENERATOR PRIMITIVES 
  ******************************************************************************/

  // primitive "random" procedure -- SEED DEFAULTS TO CURRENT TIME SINCE EPOCH
  data primitive_RANDOM(scm_list& args) {
    if(args.size() > 1)
      THROW_ERR("'random received more than 1 arg:" 
        "\n     (random <optional-real-numeric-seed>)" << FCN_ERR("random",args));
    if(args.size()==1 && (!args[0].is_type(types::num) || !args[0].num.is_real()))
      THROW_ERR("'random received non-real-numeric arg " << PROFILE(args[0])
        << ":\n     (random <optional-real-numeric-seed>)" << FCN_ERR("random",args));
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("=",primitive_EQ,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num != args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive ">" procedure:
  data primitive_GT(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, ">", "(> <real1> <real2> ...)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL(">",primitive_GT,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num <= args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "<" procedure:
  data primitive_LT(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "<", "(< <real1> <real2> ...)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("<",primitive_LT,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num >= args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive ">=" procedure:
  data primitive_GTE(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, ">=", "(>= <real1> <real2> ...)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL(">=",primitive_GTE,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num < args[i+1].num) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "<=" procedure:
  data primitive_LTE(scm_list& args) {
    confirm_no_real_numeric_primitive_errors(args, "<=", "(<= <real1> <real2> ...)");
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("<=",primitive_LTE,args);
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
    if(args.empty()) THROW_ERR("'eq? received no arguments: (eq? <obj1> <obj2> ...)" << FCN_ERR("eq?", args));
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("eq?",primitive_EQP,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(!args[i].eq(args[i+1])) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "eqv?" procedure:
  data primitive_EQVP(scm_list& args) {
    if(args.empty()) THROW_ERR("'eqv? received no arguments: (eqv? <obj1> <obj2> ...)" << FCN_ERR("eqv?", args));
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("eqv?",primitive_EQVP,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(!args[i].eqv(args[i+1])) 
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "equal?" procedure:
  data primitive_EQUALP(scm_list& args) {
    if(args.empty()) THROW_ERR("'equal? received no arguments: (equal? <obj1> <obj2> ...)" << FCN_ERR("equal?", args));
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("equal?",primitive_EQUALP,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(!args[i].equal(args[i+1])) 
        return G::FALSE_DATA_BOOLEAN;
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

  // --------------
  // EOF Generator:
  // --------------

  data primitive_EOF(scm_list& args) {
    confirm_no_args_given(args,"eof");
    return chr_type(EOF);
  }

  // -----------------
  // Char Comparators:
  // -----------------

  // primitive "char=?" procedure:
  data primitive_CHAR_EQ(scm_list& args) {
    confirm_given_char_string_args(args, "char=?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char=?",primitive_CHAR_EQ,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr != args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char<?" procedure:
  data primitive_CHAR_LT(scm_list& args) {
    confirm_given_char_string_args(args, "char<?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char<?",primitive_CHAR_LT,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr >= args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char>?" procedure:
  data primitive_CHAR_GT(scm_list& args) {
    confirm_given_char_string_args(args, "char>?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char>?",primitive_CHAR_GT,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr <= args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char<=?" procedure:
  data primitive_CHAR_LTE(scm_list& args) {
    confirm_given_char_string_args(args, "char<=?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char<=?",primitive_CHAR_LTE,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr > args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char>=?" procedure:
  data primitive_CHAR_GTE(scm_list& args) {
    confirm_given_char_string_args(args, "char>=?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char>=?",primitive_CHAR_GTE,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(args[i].chr < args[i+1].chr)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci=?" procedure:
  data primitive_CHAR_CI_EQ(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci=?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char-ci=?",primitive_CHAR_CI_EQ,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) != scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci<?" procedure:
  data primitive_CHAR_CI_LT(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci<?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char-ci<?",primitive_CHAR_CI_LT,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) >= scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci>?" procedure:
  data primitive_CHAR_CI_GT(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci>?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char-ci>?",primitive_CHAR_CI_GT,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) <= scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci<=?" procedure:
  data primitive_CHAR_CI_LTE(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci<=?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char-ci<=?",primitive_CHAR_CI_LTE,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(scm_numeric::mklower(args[i].chr) > scm_numeric::mklower(args[i+1].chr))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "char-ci>=?" procedure:
  data primitive_CHAR_CI_GTE(scm_list& args) {
    confirm_given_char_string_args(args, "char-ci>=?", types::chr);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("char-ci>=?",primitive_CHAR_CI_GTE,args);
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

  // primitive "char-alphanumeric?" procedure:
  data primitive_CHAR_ALHPANUMERICP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-alphanumeric?");
    return boolean(isalnum(args[0].chr));
  }

  // primitive "char-control?" procedure:
  data primitive_CHAR_CONTROLP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-control?");
    return boolean(iscntrl(args[0].chr));
  }

  // primitive "char-print?" procedure:
  data primitive_CHAR_PRINTP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-print?");
    return boolean(isprint(args[0].chr));
  }

  // primitive "char-graph?" procedure:
  data primitive_CHAR_GRAPHP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-graph?");
    return boolean(isgraph(args[0].chr));
  }

  // primitive "char-punctuation?" procedure:
  data primitive_CHAR_PUNCTUATIONP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-punctuation?");
    return boolean(ispunct(args[0].chr));
  }

  // primitive "char-xdigit?" procedure:
  data primitive_CHAR_XDIGITP(scm_list& args) {
    confirm_given_one_char_arg(args, "char-xdigit?");
    return boolean(isxdigit(args[0].chr));
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
    if(auto i = confirm_only_args_of_type(args, types::chr, types::str); i != G::MAX_SIZE_TYPE)
      THROW_ERR("'string arg #" << i+1 << ", " << PROFILE(args[i]) 
        << ", isn't a character or string:\n     (string <char-or-string1> <char2-or-string> ...)"
        << FCN_ERR("string", args));
    scm_string str_val;
    for(const auto& e : args) {
      if(e.is_type(types::chr)) 
        str_val += e.chr;
      else
        str_val += *e.str;
    }
    return make_str(str_val);
  }

  // primitive "string-unfold" procedure:
  data primitive_STRING_UNFOLD(scm_list& args) {
    if(!args.empty() && args.size() < 4) return GENERATE_PRIMITIVE_PARTIAL("string-unfold",primitive_STRING_UNFOLD,args);
    return primitive_STRING_UNFOLD_template(args,false,"string-unfold",
      "\n     (string-unfold <break-condition> <map-callable> <successor-callable> <seed>)");
  }

  // primitive "string-unfold-right" procedure:
  data primitive_STRING_UNFOLD_RIGHT(scm_list& args) {
    if(!args.empty() && args.size() < 4) return GENERATE_PRIMITIVE_PARTIAL("string-unfold-right",primitive_STRING_UNFOLD_RIGHT,args);
    return primitive_STRING_UNFOLD_template(args,true,"string-unfold-right",
      "\n     (string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)");
  }

  // primitive "string-pad" procedure:
  data primitive_STRING_PAD(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-pad",primitive_STRING_PAD,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-pad-right",primitive_STRING_PAD_RIGHT,args);
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
    confirm_valid_string_trim_args(args, "string-trim",
      "\n     (string-trim <string> <optional-predicate>)");
    return prm_trim_left_of_string(args);
  }

  // primitive "string-trim-right" procedure:
  data primitive_STRING_TRIM_RIGHT(scm_list& args) {
    confirm_valid_string_trim_args(args, "string-trim-right",
      "\n     (string-trim-right <string> <optional-predicate>)");
    return prm_trim_right_of_string(args);
  }

  // primitive "string-trim-both" procedure:
  data primitive_STRING_TRIM_BOTH(scm_list& args) {
    confirm_valid_string_trim_args(args, "string-trim-both",
      "\n     (string-trim-both <string> <optional-predicate>)");
    scm_list right_trim_args;
    right_trim_args.push_back(make_str(*prm_trim_left_of_string(args).str));
    if(args.size() == 2)
      right_trim_args.push_back(args[1]);
    return prm_trim_right_of_string(right_trim_args);
  }

  // primitive "string-replace" procedure:
  data primitive_STRING_REPLACE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (string-replace <string1> <string2> <start1> <end1>)";
      if(!args.empty() && args.size() < 4) return GENERATE_PRIMITIVE_PARTIAL("string-replace",primitive_STRING_REPLACE,args);
    if(args.size() != 4)
      THROW_ERR("'string-replace received incorrect # of args (given " 
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-contains",primitive_STRING_CONTAINS,args);
    return prm_string_contains_template(args, "string-contains", 
      "\n     (string-contains <string> <sub-string>)", true);
  }

  // primitive "string-contains-right" procedure:
  data primitive_STRING_CONTAINS_RIGHT(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-contains-right",primitive_STRING_CONTAINS_RIGHT,args);
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
    static constexpr const char * const format = 
      "\n     (string-split <target-string> <optional-string-delimiter> <optional-start-index>)";
    scm_string delimiter("");
    scm_list strings_list;
    size_type start_index = 0;
    confirm_proper_string_split_args(args,"string-split",format,delimiter,start_index);
    // split the string into a list of strings
    const scm_string str(args[0].str->substr(start_index));
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-swap!",primitive_STRING_SWAP_BANG,args);
    if(args.size() != 2)
      THROW_ERR("'string-swap! received incorrect # of args (given " 
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-push!",primitive_STRING_PUSH_BANG,args);
    if(args.size() != 2)
      THROW_ERR("'string-push! received incorrect # of args (given " 
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
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("string-copy!",primitive_STRING_COPY_BANG,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string=?",primitive_STRING_EQP,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str != *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string<?" procedure:
  data primitive_STRING_LT(scm_list& args) {
    confirm_given_char_string_args(args, "string<?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string<?",primitive_STRING_LT,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str >= *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string>?" procedure:
  data primitive_STRING_GT(scm_list& args) {
    confirm_given_char_string_args(args, "string>?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string>?",primitive_STRING_GT,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str <= *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string<=?" procedure:
  data primitive_STRING_LTE(scm_list& args) {
    confirm_given_char_string_args(args, "string<=?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string<=?",primitive_STRING_LTE,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str > *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string>=?" procedure:
  data primitive_STRING_GTE(scm_list& args) {
    confirm_given_char_string_args(args, "string>=?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string>=?",primitive_STRING_GTE,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(*args[i].str < *args[i+1].str)
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci=?" procedure:
  data primitive_STRING_CI_EQ(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci=?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-ci=?",primitive_STRING_CI_EQ,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) != lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci<?" procedure:
  data primitive_STRING_CI_LT(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci<?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-ci<?",primitive_STRING_CI_LT,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) >= lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci>?" procedure:
  data primitive_STRING_CI_GT(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci>?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-ci>?",primitive_STRING_CI_GT,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) <= lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci<=?" procedure:
  data primitive_STRING_CI_LTE(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci<=?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-ci<=?",primitive_STRING_CI_LTE,args);
    for(size_type i = 0, n = args.size(); i < n-1; ++i)
      if(lowercase_str(*args[i].str) > lowercase_str(*args[i+1].str))
        return G::FALSE_DATA_BOOLEAN;
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "string-ci>=?" procedure:
  data primitive_STRING_CI_GTE(scm_list& args) {
    confirm_given_char_string_args(args, "string-ci>=?", types::str);
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("string-ci>=?",primitive_STRING_CI_GTE,args);
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
      THROW_ERR("'symbol-append received incorrect # of args!"
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
  * TYPEOF, DEEP-COPYING, & SHALLOW-COPYING PRIMITIVES
  ******************************************************************************/

  data primitive_TYPEOF(scm_list& args) {
    if(args.size() != 1)
      THROW_ERR("'typeof received incorrect # of args!\n     (typeof <obj>)" 
        << FCN_ERR("typeof", args));
    if(data_is_stream_pair(args[0])) return "#<stream>";
    if(data_is_a_delay(args[0]))     return "#<delay>";
    return args[0].type_name();
  }

  data primitive_COPY(scm_list& args) {
    if(args.size() != 1)
      THROW_ERR("'copy received incorrect # of args!\n     (copy <obj>)" 
        "\n     -> Deep-Copy: Vector | String | List/Dotted/Circular | Hmap | Object"
        << FCN_ERR("copy", args));
    return args[0].copy();
  }

  data primitive_SHALLOW_COPY(scm_list& args) {
    if(args.size() != 1)
      THROW_ERR("'shallow-copy received incorrect # of args!\n     (shallow-copy <obj>)" 
        "\n     -> Shallow-Copy: Vector | String | List/Dotted/Circular | Hmap | Object"
        << FCN_ERR("shallow-copy", args));
    return args[0].shallow_copy();
  }

  /******************************************************************************
  * HASHMAP PRIMITIVE
  ******************************************************************************/

  // primitive "hmap" (hash-map constructor):
  data primitive_HMAP(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (hmap <key1> <val1> <key2> <val2> ...)" HEIST_HASH_MAP_KEY_FORMAT;
    if(args.empty()) return make_map(scm_map());
    if(args.size() & 1) 
      THROW_ERR("'hmap received uneven # of args!"<<format<<FCN_ERR("hmap",args));
    scm_map hmap;
    // verify all keys are hashable
    for(size_type i = args.size()-2; i != G::MAX_SIZE_TYPE-1; i -= 2) {
      if(!scm_map::hashable(args[i]))
        THROW_ERR("'hmap key " << PROFILE(args[i]) << " isn't hashable!"
          << format << FCN_ERR("hmap", args));
      hmap[args[i]] = args[i+1];
    }
    return make_map(std::move(hmap));
  }

  // primitive "hmap-keys":
  data primitive_HMAP_KEYS(scm_list& args) {
    hmap_confirm_unary_map("hmap-keys","\n     (hmap-keys <hash-map>)",args);
    scm_list keys_list;
    for(const auto& keyval : args[0].map->val)
      keys_list.push_back(scm_map::unhash_key(keyval.first));
    return primitive_LIST_to_CONS_constructor(keys_list.begin(),keys_list.end());
  }

  // primitive "hmap-vals":
  data primitive_HMAP_VALS(scm_list& args) {
    hmap_confirm_unary_map("hmap-vals","\n     (hmap-vals <hash-map>)",args);
    scm_list keys_list;
    for(const auto& keyval : args[0].map->val)
      keys_list.push_back(keyval.second);
    return primitive_LIST_to_CONS_constructor(keys_list.begin(),keys_list.end());
  }

  // primitive "hmap-key?":
  data primitive_HMAP_KEYP(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("hmap-key?",primitive_HMAP_KEYP,args);
    hmap_confirm_binary_map_key("hmap-key?","\n     (hmap-key? <hash-map> <key>)",args);
    return boolean(args[0].map->val.count(scm_map::hash_key(args[1])));
  }

  // primitive "hmap-hashable?":
  data primitive_HMAP_HASHABLEP(scm_list& args) {
    if(args.size() != 1) 
      THROW_ERR("'hmap-hashable? didn't receive 1 arg!" 
        "\n     (hmap-hashable? <obj>)" << FCN_ERR("hmap-hashable?", args));
    return boolean(scm_map::hashable(args[0]));
  }

  // primitive "hmap-ref":
  data primitive_HMAP_REF(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("hmap-ref",primitive_HMAP_REF,args);
    hmap_confirm_binary_map_key("hmap-ref","\n     (hmap-ref <hash-map> <key>)",args);
    auto hashed_key = scm_map::hash_key(args[1]);
    if(!args[0].map->val.count(hashed_key))
      THROW_ERR("'hmap-ref arg "<<PROFILE(args[1])<<" isn't a key in hash-map "
        << args[0] << "!\n     (hmap-ref <hash-map> <key>)" << FCN_ERR("hmap-ref", args));
    return args[0].map->val[hashed_key];
  }

  // primitive "hmap-set!":
  data primitive_HMAP_SET_BANG(scm_list& args) {
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("hmap-set!",primitive_HMAP_SET_BANG,args);
    hmap_confirm_ternary_map_key_val("hmap-set!","\n     (hmap-set! <hash-map> <key> <value>)",args);
    (*args[0].map)[args[1]] = args[2];
    return G::VOID_DATA_OBJECT;
  }

  // primitive "hmap-delete!":
  data primitive_HMAP_DELETE_BANG(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("hmap-delete!",primitive_HMAP_DELETE_BANG,args);
    hmap_confirm_binary_map_key("hmap-delete!","\n     (hmap-delete! <hash-map> <key>)",args);
    return boolean(args[0].map->val.erase(scm_map::hash_key(args[1])));
  }

  // primitive "hmap-length":
  data primitive_HMAP_LENGTH(scm_list& args) {
    hmap_confirm_unary_map("hmap-length","\n     (hmap-length <hash-map>)",args);
    return num_type(args[0].map->val.size());
  }

  // primitive "hmap-empty?":
  data primitive_HMAP_EMPTYP(scm_list& args) {
    hmap_confirm_unary_map("hmap-empty?","\n     (hmap-empty? <hash-map>)",args);
    return boolean(args[0].map->val.empty());
  }

  // primitive "hmap-merge":
  data primitive_HMAP_MERGE(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("hmap-merge",primitive_HMAP_MERGE,args);
    hmap_confirm_given_2_or_more_maps("hmap-merge","\n     (hmap-merge <hash-map-1> <hash-map-2> ...)",args);
    scm_map map;
    for(size_type i = args.size(); i-- > 0;)
      for(const auto& keyval : args[i].map->val)
        map.val[keyval.first] = keyval.second; // left arg key vals supersede right arg key vals
    return make_map(std::move(map));
  }

  // primitive "hmap-merge!":
  data primitive_HMAP_MERGE_BANG(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("hmap-merge!",primitive_HMAP_MERGE_BANG,args);
    hmap_confirm_given_2_or_more_maps("hmap-merge!","\n     (hmap-merge! <hash-map-1> <hash-map-2> ...)",args);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      for(const auto& keyval : args[i].map->val)
        if(!args[0].map->val.count(keyval.first))
          args[0].map->val[keyval.first] = keyval.second;
    return G::VOID_DATA_OBJECT;
  }

  // primitive "hmap-for-each", "hmap-for-each-key", "hmap-for-each-val", "hmap-map!"
  // NOTE: WE CREATE A VECTOR OF KEYS THEN ITERATE THRU THAT VECTOR, SO AS TO NOT INVALIDATE
  //       HMAP ITERATORS IN THE LOOP FROM POTENTIALLY DELETING THE KEY IN THE USER'S FCN
  #define GENERATE_HMAP_ITERATION_FCN(FCN_NAME,NAME,...)\
    data FCN_NAME(scm_list& args) {\
      if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL(NAME,FCN_NAME,args);\
      hmap_confirm_binary_procedure_map(NAME,"\n     (" NAME " <callable> <hash-map>)",args);\
      auto procedure(primitive_extract_callable_procedure(args[0]));\
      size_type n = args[1].map->val.size(), i = 0;\
      std::vector<scm_string> keys(n);\
      for(auto& keyvalue : args[1].map->val)\
        keys[i++] = keyvalue.first;\
      for(i = 0; i < n; ++i) {\
        __VA_ARGS__;\
      }\
      return G::VOID_DATA_OBJECT;\
    }

  GENERATE_HMAP_ITERATION_FCN(primitive_HMAP_FOR_EACH_KEY,"hmap-for-each-key",
    scm_list arg(1,scm_map::unhash_key(keys[i]));
    execute_application(procedure,arg););

  GENERATE_HMAP_ITERATION_FCN(primitive_HMAP_FOR_EACH_VAL,"hmap-for-each-val",
    scm_list arg(1,args[1].map->val[keys[i]]);
    execute_application(procedure,arg););

  GENERATE_HMAP_ITERATION_FCN(primitive_HMAP_FOR_EACH,"hmap-for-each",
    auto p = make_par();
    p->first = scm_map::unhash_key(keys[i]);
    p->second = args[1].map->val[keys[i]];
    scm_list arg(1,p);
    execute_application(procedure,arg););

  GENERATE_HMAP_ITERATION_FCN(primitive_HMAP_MAP_BANG,"hmap-map!",
    scm_list arg(1,args[1].map->val[keys[i]]);
    args[1].map->val[keys[i]] = data_cast(execute_application(procedure,arg)););

  // primitive "hmap-map"
  data primitive_HMAP_MAP(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("hmap-map",primitive_HMAP_MAP,args);
    hmap_confirm_binary_procedure_map("hmap-map","\n     (hmap-map <callable> <hash-map>)",args);
    auto procedure(primitive_extract_callable_procedure(args[0]));
    size_type n = args[1].map->val.size(), i = 0;
    // extract all keys, then iterate thru keys (avoids iterator invalidation
    // in case the procedure passed by the user [for some reason] erases elts 
    // from the map being iterated through)
    std::vector<scm_string> keys(n);
    for(auto& keyvalue : args[1].map->val)
      keys[i++] = keyvalue.first;
    scm_map map;
    for(i = 0; i < n; ++i) {
      scm_list arg(1,args[1].map->val[keys[i]]);
      map.val[keys[i]] = data_cast(execute_application(procedure,arg));
    }
    return make_map(std::move(map));
  }

  // primitive "hmap->alist"
  data primitive_HMAP_TO_ALIST(scm_list& args) {
    hmap_confirm_unary_map("hmap->alist","\n     (hmap->alist <hash-map>)",args);
    scm_list alist(args[0].map->val.size());
    size_type i = 0;
    for(auto& keyvalue : args[0].map->val) {
      auto p = make_par();
      p->first = scm_map::unhash_key(keyvalue.first);
      p->second = make_par();
      p->second.par->first = keyvalue.second;
      p->second.par->second = symconst::emptylist;
      alist[i++] = std::move(p);
    }
    return primitive_LIST_to_CONS_constructor(alist.begin(),alist.end());
  }

  // primitive "alist->hmap"
  data primitive_ALIST_TO_HMAP(scm_list& args) {
    if(args.size() != 1)
      THROW_ERR("'alist->hmap didn't receive 1 arg!"
        "\n     (alist->hmap <alist>)" << FCN_ERR("alist->hmap",args));
    if(!data_is_proper_list(args[0]))
      THROW_ERR("'alist->hmap arg "<<PROFILE(args[0])<<" isn't an <alist> of proper-list pairs of items!"
        "\n     (alist->hmap <alist>)" << FCN_ERR("alist->hmap",args));
    scm_list alist_exp;
    scm_map map;
    shallow_unpack_list_into_exp(args[0],alist_exp);
    for(auto& p : alist_exp) {
      if(!p.is_type(types::par))
        THROW_ERR("'alist->hmap arg "<<PROFILE(args[0])<<" isn't an <alist> of proper-list pairs of items!"
          "\n     (alist->hmap <alist>)" << FCN_ERR("alist->hmap",args));
      if(!p.par->second.is_type(types::par))
        THROW_ERR("'alist->hmap arg "<<PROFILE(args[0])<<" isn't an <alist> of proper-list pairs of items!"
          "\n     (alist->hmap <alist>)" << FCN_ERR("alist->hmap",args));
      if(!scm_map::hashable(p.par->first))
        THROW_ERR("'alist->hmap key "<<PROFILE(p.par->first)<<" isn't hashable!"
          "\n     (alist->hmap <alist>)" HEIST_HASH_MAP_KEY_FORMAT << FCN_ERR("alist->hmap",args));
      map[p.par->first] = p.par->second.par->first;
    }
    return make_map(std::move(map));
  }

  #undef GENERATE_HMAP_ITERATION_FCN
  #undef HEIST_HASH_MAP_KEY_FORMAT

  /******************************************************************************
  * PAIR PRIMITIVES
  ******************************************************************************/

  // --------------
  // Pair Handlers:
  // --------------

  // primitive "cons" procedure:
  data primitive_CONS(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("cons",primitive_CONS,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("set-car!",primitive_SETCAR,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("set-cdr!",primitive_SETCDR,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("pair-swap!",primitive_PAIR_SWAP_BANG,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("make-list",primitive_MAKE_LIST,args);
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
    if(!args.empty() && args.size() < 4) return GENERATE_PRIMITIVE_PARTIAL("unfold",primitive_UNFOLD,args);
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,"unfold",
      "\n     (unfold <break-condition> <map-callable> <successor-callable> <seed>)");
    return primitive_LIST_to_CONS_constructor(unfolded.begin(),unfolded.end());
  }

  // primitive "unfold-right" procedure:
  data primitive_UNFOLD_RIGHT(scm_list& args) {
    if(!args.empty() && args.size() < 4) return GENERATE_PRIMITIVE_PARTIAL("unfold-right",primitive_UNFOLD_RIGHT,args);
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,"unfold-right",
      "\n     (unfold-right <break-condition> <map-callable> <successor-callable> <seed>)");
    return primitive_LIST_to_CONS_constructor(unfolded.rbegin(),unfolded.rend());
  }

  // primitive "get-all-combinations" procedure:
  data primitive_GET_ALL_COMBINATIONS(scm_list& args) {
    if(args.size() != 1)
      THROW_ERR("'get-all-combinations didn't receive 1 arg!"
        "\n     (get-all-combinations <proper-list>)" << FCN_ERR("get-all-combinations",args));
    if(!data_is_proper_list(args[0]))
      THROW_ERR("'get-all-combinations arg " << PROFILE(args[0]) << " isn't a proper list!"
          "\n     (get-all-combinations <proper-list>)" << FCN_ERR("get-all-combinations",args));
    scm_list list_list;
    shallow_unpack_list_into_exp(args[0], list_list);
    auto result = prm_all_combos(list_list);
    scm_list combinations_list;
    for(auto& lis : result)
      combinations_list.push_back(primitive_LIST_to_CONS_constructor(lis.begin(),lis.end()));
    return primitive_LIST_to_CONS_constructor(combinations_list.begin(),combinations_list.end());
  }

  // -----------------------
  // List Member Extraction:
  // -----------------------

  // primitive "memq" procedure:
  data primitive_MEMQ(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("memq",primitive_MEMQ,args);
    return primitive_MEM_template(args, "memq", primitive_EQP);
  }

  // primitive "memv" procedure:
  data primitive_MEMV(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("memv",primitive_MEMV,args);
    return primitive_MEM_template(args, "memv", primitive_EQVP);
  }

  // primitive "member" procedure:
  data primitive_MEMBER(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("member",primitive_MEMBER,args);
    return primitive_MEM_template(args, "member", primitive_EQUALP);
  }

  // primitive "assq" procedure:
  data primitive_ASSQ(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("assq",primitive_ASSQ,args);
    return primitive_ASSOCIATION_template(args, "assq", primitive_EQP);
  }

  // primitive "assv" procedure:
  data primitive_ASSV(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("assv",primitive_ASSV,args);
    return primitive_ASSOCIATION_template(args, "assv", primitive_EQVP);
  }

  // primitive "assoc" procedure:
  data primitive_ASSOC(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("assoc",primitive_ASSOC,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("vector-push!",primitive_VECTOR_PUSH_BANG,args);
    primitive_confirm_valid_vector_arg(args, 2, "vector-push!", "\n     (vector-push! <vector> <obj>)");
    args[0].vec->push_back(args[1]);
    return G::VOID_DATA_OBJECT;
  }

  // primitive "vector-iota" procedure:
  data primitive_VECTOR_IOTA(scm_list& args) {
    return primitive_IOTA_generic<false,vec_type(*)(scm_list&&)>(args,"vector-iota",
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("vector-grow",primitive_VECTOR_GROW,args);
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
    return make_vec(std::move(expanded_vec));
  }

  // primitive "vector-unfold" procedure:
  data primitive_VECTOR_UNFOLD(scm_list& args) {
    if(!args.empty() && args.size() < 4) return GENERATE_PRIMITIVE_PARTIAL("vector-unfold",primitive_VECTOR_UNFOLD,args);
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,"vector-unfold",
      "\n     (vector-unfold <break-condition> <map-callable> <successor-callable> <seed>)");
    return make_vec(std::move(unfolded));
  }

  // primitive "vector-unfold-right" procedure:
  data primitive_VECTOR_UNFOLD_RIGHT(scm_list& args) {
    if(!args.empty() && args.size() < 4) return GENERATE_PRIMITIVE_PARTIAL("vector-unfold-right",primitive_VECTOR_UNFOLD_RIGHT,args);
    scm_list unfolded;
    primitive_UNFOLD_template(args,unfolded,"vector-unfold-right",
      "\n     (vector-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)");
    return make_vec(scm_list(unfolded.rbegin(),unfolded.rend()));
  }

  // primitive "vector-copy!" procedure:
  data primitive_VECTOR_COPY_BANG(scm_list& args) {
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("vector-copy!",primitive_VECTOR_COPY_BANG,args);
    return primitive_STATIC_SEQUENCE_COPY_BANG_template(args, "vector-copy!", 
      "\n     (vector-copy! <target-vector> <target-start-idx> <source-vector>)",
      types::vec, "vector", &data::vec);
  }

  // primitive "vector-swap!" procedure:
  data primitive_VECTOR_SWAP_BANG(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("vector-swap!",primitive_VECTOR_SWAP_BANG,args);
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
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("vector-binary-search",primitive_VECTOR_BINARY_SEARCH,args);
    // Confirm proper args
    if(args.size() != 3)
      THROW_ERR("'vector-binary-search received incorrect # of args (given "
        << args.size() << "):" << format << FCN_ERR("vector-binary-search", args));
    if(!args[0].is_type(types::vec))
      THROW_ERR("'vector-binary-search 1st arg "<<PROFILE(args[0])<<" isn't a vector:"
        << format << FCN_ERR("vector-binary-search", args));
    auto proc = validate_and_extract_callable(args[2], "vector-binary-search", format, args);
    // Perform binary search
    if(args[0].vec->empty()) return G::FALSE_DATA_BOOLEAN;
    const auto& vec   = *args[0].vec;
    const auto& value = args[1];
    size_type high    = vec.size()-1, 
              low     = 0;
    while (low <= high) {
      const auto mid = low + (high-low)/2; // no overflow on mid
      scm_list bsearch_args(2);
      bsearch_args[0] = vec[mid], bsearch_args[1] = value;
      auto cmp_result = data_cast(execute_application(proc,bsearch_args));
      if(!cmp_result.is_type(types::num))
        THROW_ERR("'vector-binary-search result "<<PROFILE(cmp_result)<<
          " from callable "<<args[2]<<"\n     applied to args "<<vec[mid]
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

  // primitive "vector-get-all-combinations" procedure:
  data primitive_VECTOR_GET_ALL_COMBINATIONS(scm_list& args) {
    if(args.size() != 1)
      THROW_ERR("'vector-get-all-combinations didn't receive 1 arg!"
        "\n     (vector-get-all-combinations <vector>)" << FCN_ERR("vector-get-all-combinations",args));
    if(!args[0].is_type(types::vec))
      THROW_ERR("'vector-get-all-combinations arg " << PROFILE(args[0]) << " isn't a vector!"
          "\n     (vector-get-all-combinations <vector>)" << FCN_ERR("vector-get-all-combinations",args));
    auto result = prm_all_combos(*args[0].vec);
    scm_list combinations_vect;
    for(auto& vect : result)
      combinations_vect.push_back(make_vec(vect));
    return make_vec(combinations_vect);
  }

  /******************************************************************************
  * ALGORITHMIC PRIMITIVES: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  // Sequence description for generic algorithm primitives
  #define SEQUENCE_DESCRIPTION\
    "\n     <sequence> ::= <list> | <vector> | <string>"

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
        return primitive_reverse_STATIC_SEQUENCE_logic<scm_list,vec_type(*)(scm_list&&)>(args[0],&data::vec,make_vec);
      case heist_sequence::str: 
        return primitive_reverse_STATIC_SEQUENCE_logic<scm_string,str_type(*)(scm_string&&)>(args[0],&data::str,make_str);
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
      "\n     (fold <callable> <init> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("fold",primitive_FOLD,args);
    if(args.size() < 3) 
      THROW_ERR("'fold received insufficient args (only " 
        << args.size() << "):" << format << FCN_ERR("fold",args));
    auto procedure = validate_and_extract_callable(args[0], "fold", format, args);
    switch(is_proper_sequence(args[2],args,"fold",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_FOLD_template(procedure, args, "fold", 
          format, true, types::vec, "vector", &data::vec);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_FOLD_template(procedure, args, "fold", 
          format, true, types::str, "string", &data::str);
      case heist_sequence::nul:
        return args[1];
      default:
        return primitive_FOLD_template(procedure, args, "fold", format, true);
    }
  }

  // primitive "fold-right" procedure:
  data primitive_FOLD_RIGHT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (fold-right <callable> <init> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("fold-right",primitive_FOLD_RIGHT,args);
    if(args.size() < 3) 
      THROW_ERR("'fold-right received insufficient args (only " 
        << args.size() << "):" << format << FCN_ERR("fold-right",args));
    auto procedure = validate_and_extract_callable(args[0], "fold-right", format, args);
    switch(is_proper_sequence(args[2],args,"fold-right",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_FOLD_template(procedure, args, "fold-right", 
          format, false, types::vec, "vector", &data::vec);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_FOLD_template(procedure, args, "fold-right", 
          format, false, types::str, "string", &data::str);
      case heist_sequence::nul:
        return args[1];
      default:
        return primitive_FOLD_template(procedure, args, "fold-right", format, false);
    }
  }

  // primitive "filter" procedure:
  data primitive_FILTER(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (filter <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("filter",primitive_FILTER,args);
    if(args.size() != 2) 
      THROW_ERR("'filter received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("filter",args));
    auto procedure = validate_and_extract_callable(args[0], "filter", format, args);
    switch(is_proper_sequence(args[1],args,"filter",format)) {
      case heist_sequence::vec:
        return prm_sequence_selective_iteration_template<is_true_scm_condition>(procedure, args, types::vec, &data::vec);
      case heist_sequence::str:
        return prm_sequence_selective_iteration_template<is_true_scm_condition>(procedure, args, types::str, &data::str);
      case heist_sequence::nul:
        return args[1];
      default:
        return primitive_list_filter_logic(procedure, args);
    }
  }

  // primitive "map" procedure:
  data primitive_MAP(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (map <callable> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("map",primitive_MAP,args);
    if(args.size() < 2) 
      THROW_ERR("'map received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("map",args));
    auto procedure = validate_and_extract_callable(args[0], "map", format, args);
    switch(is_proper_sequence(args[1],args,"map",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_MAP_template(procedure, args, "map", 
          format, types::vec, "vector", &data::vec);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_MAP_template(procedure, args, "map", 
          format, types::str, "string", &data::str);
      default:
        return primitive_list_map_logic(procedure, args, format);
    }
  }

  // primitive "map!" procedure:
  data primitive_MAP_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (map! <callable> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("map!",primitive_MAP_BANG,args);
    if(args.size() < 2) 
      THROW_ERR("'map! received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("map!",args));
    auto procedure = validate_and_extract_callable(args[0], "map!", format, args);
    switch(is_proper_sequence(args[1],args,"map!",format)) {
      case heist_sequence::vec:
        *args[1].vec = *primitive_STATIC_SEQUENCE_MAP_template(procedure, args, "map!",
          format, types::vec, "vector", &data::vec).vec;
        return G::VOID_DATA_OBJECT;
      case heist_sequence::str:
        *args[1].str = *primitive_STATIC_SEQUENCE_MAP_template(procedure, args, "map!",
          format, types::str, "string", &data::str).str;
        return G::VOID_DATA_OBJECT;
      default:
        scm_list list_heads(args.begin()+1, args.end());
        primitive_confirm_proper_same_sized_lists(list_heads,"map!",format,1,args);
        primitive_MAP_BANG_list_constructor(list_heads, procedure);
        return G::VOID_DATA_OBJECT;
    }
  }

  // primitive "for-each" procedure:
  data primitive_FOR_EACH(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (for-each <callable> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("for-each",primitive_FOR_EACH,args);
    if(args.size() < 2) 
      THROW_ERR("'for-each received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("for-each",args));
    auto procedure = validate_and_extract_callable(args[0], "for-each", format, args);
    switch(is_proper_sequence(args[1],args,"for-each",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_FOR_EACH_template(procedure, args, "for-each", 
          format, types::vec, "vector", &data::vec);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_FOR_EACH_template(procedure, args, "for-each", 
          format, types::str, "string", &data::str);
      default:
        return primitive_list_for_each_logic(procedure, args, format);
    }
  }

  // primitive "seq-copy!" procedure: 
  // NOTE: Copies elts from <source> over <dest>'s elts. 
  //       <dest>.size() is unaffected.
  data primitive_SEQ_COPY_BANG(scm_list& args) { // 
    static constexpr const char * const format = 
      "\n     (seq-copy! <dest-sequence> <source-sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("seq-copy!",primitive_SEQ_COPY_BANG,args);
    if(args.size() != 2)
      THROW_ERR("'seq-copy! received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("seq-copy!",args));
    if(args[0].type != args[1].type)
      THROW_ERR("'seq-copy! args have mismatched types!" 
        << format << FCN_ERR("seq-copy!",args));
    switch(is_proper_sequence(args[0],args,"seq-copy!",format)) {
      case heist_sequence::vec: return primitive_generic_STATIC_CONTAINER_copy_bang_logic(args[0].vec,args[1].vec);
      case heist_sequence::str: return primitive_generic_STATIC_CONTAINER_copy_bang_logic(args[0].str,args[1].str);
      case heist_sequence::nul: return data(types::dne);
      default:                  return primitive_generic_list_copy_bang_logic(args[0],args[1]);
    }
  }

  // primitive "count" procedure:
  data primitive_COUNT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (count <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("count",primitive_COUNT,args);
    if(args.size() != 2) 
      THROW_ERR("'count received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("count",args));
    auto procedure = validate_and_extract_callable(args[0], "count", format, args);
    switch(is_proper_sequence(args[1],args,"count",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_COUNT_template(procedure, args, &data::vec);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_COUNT_template(procedure, args, &data::str);
      case heist_sequence::nul:
        return num_type();
      default:
        return primitive_list_count_logic(procedure,args);
    }
  }

  // primitive "ref" procedure:
  data primitive_REF(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (ref <sequence> <index>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("ref",primitive_REF,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("slice",primitive_SLICE,args);
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
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("set-index!",primitive_SET_INDEX_BANG,args);
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
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("swap-indices!",primitive_SWAP_INDICES_BANG,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("fill!",primitive_FILL_BANG,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("remove",primitive_REMOVE,args);
    if(args.size() != 2) 
      THROW_ERR("'remove received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("remove",args));
    auto procedure = validate_and_extract_callable(args[0], "remove", format, args);
    switch(is_proper_sequence(args[1],args,"remove",format)) {
      case heist_sequence::nul:
        return args[1];
      case heist_sequence::vec:
        return prm_sequence_selective_iteration_template<is_false_scm_condition>(procedure, args, types::vec, &data::vec);
      case heist_sequence::str:
        return prm_sequence_selective_iteration_template<is_false_scm_condition>(procedure, args, types::str, &data::str);
      default:
        return primitive_remove_list_logic(args[1],procedure);
    }
  }

  // primitive "remove-first" procedure:
  data primitive_REMOVE_FIRST(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (remove-first <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("remove-first",primitive_REMOVE_FIRST,args);
    if(args.size() != 2) 
      THROW_ERR("'remove-first received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("remove-first",args));
    auto procedure = validate_and_extract_callable(args[0], "remove-first", format, args);
    switch(is_proper_sequence(args[1],args,"remove-first",format)) {
      case heist_sequence::nul:
        return args[1];
      case heist_sequence::vec:
        return make_vec(prm_remove_first_or_last<true>(procedure,*args[1].vec));
      case heist_sequence::str:
        return make_str(prm_remove_first_or_last<true>(procedure,*args[1].str));
      default:
        scm_list par_as_exp;
        shallow_unpack_list_into_exp(args[1],par_as_exp);
        par_as_exp = prm_remove_first_or_last<true>(procedure,par_as_exp);
        return primitive_LIST_to_CONS_constructor(par_as_exp.begin(),par_as_exp.end());
    }
  }

  // primitive "remove-last" procedure:
  data primitive_REMOVE_LAST(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (remove-last <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("remove-last",primitive_REMOVE_LAST,args);
    if(args.size() != 2) 
      THROW_ERR("'remove-last received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("remove-last",args));
    auto procedure = validate_and_extract_callable(args[0], "remove-last", format, args);
    switch(is_proper_sequence(args[1],args,"remove-last",format)) {
      case heist_sequence::nul:
        return args[1];
      case heist_sequence::vec:
        return make_vec(prm_remove_first_or_last<false>(procedure,*args[1].vec));
      case heist_sequence::str:
        return make_str(prm_remove_first_or_last<false>(procedure,*args[1].str));
      default:
        scm_list par_as_exp;
        shallow_unpack_list_into_exp(args[1],par_as_exp);
        par_as_exp = prm_remove_first_or_last<false>(procedure,par_as_exp);
        return primitive_LIST_to_CONS_constructor(par_as_exp.begin(),par_as_exp.end());
    }
  }

  // primitive "delete" procedure:
  data primitive_DELETE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (delete <sequence> <index>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("delete",primitive_DELETE,args);
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
      "\n     (seq= <elt=?> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.empty()) THROW_ERR("'seq= didn't receive any args:" << format << FCN_ERR("seq=", args));
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("seq=",primitive_SEQ_EQ,args);
    auto procedure = validate_and_extract_callable(args[0], "seq=", format, args);
    switch(is_proper_sequence(args[1],args,"seq=",format)) {
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_sequence_eq_logic(procedure,args,format,types::vec,&data::vec);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_sequence_eq_logic(procedure,args,format,types::str,&data::str);
      default:
        return primitive_list_sequence_eq_logic(procedure,args,format);
    }
  }

  // primitive "skip" procedure:
  data primitive_SKIP(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (skip <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("skip",primitive_SKIP,args);
    if(args.size() != 2) 
      THROW_ERR("'skip received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("skip",args));
    auto procedure = validate_and_extract_callable(args[0], "skip", format, args);
    switch(is_proper_sequence(args[1],args,"skip",format)) {
      case heist_sequence::nul: 
        return G::FALSE_DATA_BOOLEAN;
      case heist_sequence::vec:
        return prm_search_STATIC_SEQUENCE_from_left<is_false_scm_condition>(procedure,args,&data::vec);
      case heist_sequence::str:
        return prm_search_STATIC_SEQUENCE_from_left<is_false_scm_condition>(procedure,args,&data::str);
      default:
        return prm_search_list_from_left<is_false_scm_condition>(procedure,args[1]);
    }
  }

  // primitive "skip-right" procedure:
  data primitive_SKIP_RIGHT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (skip-right <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("skip-right",primitive_SKIP_RIGHT,args);
    if(args.size() != 2) 
      THROW_ERR("'skip-right received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("skip-right",args));
    auto procedure = validate_and_extract_callable(args[0], "skip-right", format, args);
    switch(is_proper_sequence(args[1],args,"skip-right",format)) {
      case heist_sequence::nul: 
        return args[1];
      case heist_sequence::vec:
        return prm_search_STATIC_SEQUENCE_from_right<is_false_scm_condition>(procedure,args,&data::vec);
      case heist_sequence::str:
        return prm_search_STATIC_SEQUENCE_from_right<is_false_scm_condition>(procedure,args,&data::str);
      default:
        return prm_search_list_from_right<is_false_scm_condition>(procedure,args);
    }
  }

  // primitive "index" procedure:
  data primitive_INDEX(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (index <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("index",primitive_INDEX,args);
    if(args.size() != 2) 
      THROW_ERR("'index received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("index",args));
    auto procedure = validate_and_extract_callable(args[0], "index", format, args);
    switch(is_proper_sequence(args[1],args,"index",format)) {
      case heist_sequence::nul:
        return G::FALSE_DATA_BOOLEAN;
      case heist_sequence::vec:
        return prm_search_STATIC_SEQUENCE_from_left<is_true_scm_condition>(procedure,args,&data::vec);
      case heist_sequence::str:
        return prm_search_STATIC_SEQUENCE_from_left<is_true_scm_condition>(procedure,args,&data::str);
      default:
        return prm_search_list_from_left<is_true_scm_condition>(procedure,args[1]);
    }
  }

  // primitive "index-right" procedure:
  data primitive_INDEX_RIGHT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (index-right <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("index-right",primitive_INDEX_RIGHT,args);
    if(args.size() != 2) 
      THROW_ERR("'index-right received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("index-right",args));
    auto procedure = validate_and_extract_callable(args[0], "index-right", format, args);
    switch(is_proper_sequence(args[1],args,"index-right",format)) {
      case heist_sequence::nul:
        return args[1];
      case heist_sequence::vec:
        return prm_search_STATIC_SEQUENCE_from_right<is_true_scm_condition>(procedure,args,&data::vec);
      case heist_sequence::str:
        return prm_search_STATIC_SEQUENCE_from_right<is_true_scm_condition>(procedure,args,&data::str);
      default:
        return prm_search_list_from_right<is_true_scm_condition>(procedure,args);
    }
  }

  // primitive "drop" procedure:
  data primitive_DROP(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("drop",primitive_DROP,args);
    return primitive_take_drop_template 
      <primitive_drop_GENERIC_logic<false>, 
       primitive_drop_GENERIC_logic<false>, 
       primitive_drop_GENERIC_logic<true>> 
      (args, "drop", "\n     (drop <sequence> <length>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "drop-right" procedure:
  data primitive_DROP_RIGHT(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("drop-right",primitive_DROP_RIGHT,args);
    return primitive_take_drop_template 
      <primitive_drop_right_GENERIC_logic<false>, 
       primitive_drop_right_GENERIC_logic<false>, 
       primitive_drop_right_GENERIC_logic<true>> 
      (args, "drop-right", "\n     (drop-right <sequence> <length>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "take" procedure:
  data primitive_TAKE(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("take",primitive_TAKE,args);
    return primitive_take_drop_template 
      <primitive_take_GENERIC_logic<false>, 
       primitive_take_GENERIC_logic<false>, 
       primitive_take_GENERIC_logic<true>> 
      (args, "take", "\n     (take <sequence> <length>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "take-right" procedure:
  data primitive_TAKE_RIGHT(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("take-right",primitive_TAKE_RIGHT,args);
    return primitive_take_drop_template 
      <primitive_take_right_GENERIC_logic<false>, 
       primitive_take_right_GENERIC_logic<false>, 
       primitive_take_right_GENERIC_logic<true>> 
      (args, "take-right", "\n     (take-right <sequence> <length>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "drop-while" procedure:
  data primitive_DROP_WHILE(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("drop-while",primitive_DROP_WHILE,args);
    return primitive_take_drop_while_template
      <primitive_drop_while_GENERIC_logic<false>, 
       primitive_drop_while_GENERIC_logic<false>, 
       primitive_drop_while_GENERIC_logic<true>> 
      (args, "drop-while", "\n     (drop-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "drop-right-while" procedure:
  data primitive_DROP_RIGHT_WHILE(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("drop-right-while",primitive_DROP_RIGHT_WHILE,args);
    return primitive_take_drop_while_template 
      <primitive_drop_right_while_GENERIC_logic<false>, 
       primitive_drop_right_while_GENERIC_logic<false>, 
       primitive_drop_right_while_GENERIC_logic<true>> 
      (args, "drop-right-while", "\n     (drop-right-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "take-while" procedure:
  data primitive_TAKE_WHILE(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("take-while",primitive_TAKE_WHILE,args);
    return primitive_take_drop_while_template 
      <primitive_take_while_GENERIC_logic<false>, 
       primitive_take_while_GENERIC_logic<false>, 
       primitive_take_while_GENERIC_logic<true>> 
      (args, "take-while", "\n     (take-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "take-right-while" procedure:
  data primitive_TAKE_RIGHT_WHILE(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("take-right-while",primitive_TAKE_RIGHT_WHILE,args);
    return primitive_take_drop_while_template 
      <primitive_take_right_while_GENERIC_logic<false>, 
       primitive_take_right_while_GENERIC_logic<false>, 
       primitive_take_right_while_GENERIC_logic<true>> 
      (args, "take-right-while", "\n     (take-right-while <predicate> <sequence>)" SEQUENCE_DESCRIPTION);
  }

  // primitive "any" procedure:
  data primitive_ANY(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (any <predicate> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("any",primitive_ANY,args);
    if(args.size() < 2) 
      THROW_ERR("'any received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("any",args));
    auto procedure = validate_and_extract_callable(args[0], "any", format, args);
    switch(is_proper_sequence(args[1],args,"any",format)) {
      case heist_sequence::nul:
        return G::FALSE_DATA_BOOLEAN;
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_any_logic<types::vec>(procedure,args,&data::vec,"vector",format);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_any_logic<types::str>(procedure,args,&data::str,"string",format);
      default:
        return primitive_list_any_logic(procedure,args,format);
    }
  }

  // primitive "every" procedure:
  data primitive_EVERY(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (every <predicate> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("every",primitive_EVERY,args);
    if(args.size() < 2) 
      THROW_ERR("'every received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("every",args));
    auto procedure = validate_and_extract_callable(args[0], "every", format, args);
    switch(is_proper_sequence(args[1],args,"every",format)) {
      case heist_sequence::nul:
        return G::FALSE_DATA_BOOLEAN;
      case heist_sequence::vec:
        return primitive_STATIC_SEQUENCE_every_logic<types::vec>(procedure,args,&data::vec,"vector",format);
      case heist_sequence::str:
        return primitive_STATIC_SEQUENCE_every_logic<types::str>(procedure,args,&data::str,"string",format);
      default:
        return primitive_list_every_logic(procedure,args,format);
    }
  }

  // primitive "conj" procedure:
  data primitive_CONJ(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (conj <obj> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("conj",primitive_CONJ,args);
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

  // primitive "union" procedure:
  data primitive_UNION(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (union <predicate> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("union",primitive_ANY,args);
    if(args.size() < 2) 
      THROW_ERR("'union received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("union",args));
    auto procedure = validate_and_extract_callable(args[0], "union", format, args);
    args.erase(args.begin());
    switch(is_proper_sequence(args[0],args,"union",format)) {
      case heist_sequence::vec: return prm_perform_vector_union(procedure,args,format);
      case heist_sequence::str: return prm_perform_string_union(procedure,args,format);
      default: return prm_perform_list_union(procedure,args,format);
    }
  }

  // primitive "intersection" procedure:
  data primitive_INTERSECTION(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (intersection <predicate> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("intersection",primitive_ANY,args);
    if(args.size() < 2) 
      THROW_ERR("'intersection received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("intersection",args));
    auto procedure = validate_and_extract_callable(args[0], "intersection", format, args);
    args.erase(args.begin());
    switch(is_proper_sequence(args[0],args,"intersection",format)) {
      case heist_sequence::vec: return prm_perform_vector_intersection(procedure,args,format);
      case heist_sequence::str: return prm_perform_string_intersection(procedure,args,format);
      default: return prm_perform_list_intersection(procedure,args,format);
    }
  }

  // primitive "symmetric-difference" procedure:
  data primitive_SYMMETRIC_DIFFERENCE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (symmetric-difference <predicate> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("symmetric-difference",primitive_ANY,args);
    if(args.size() < 2) 
      THROW_ERR("'symmetric-difference received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("symmetric-difference",args));
    auto procedure = validate_and_extract_callable(args[0], "symmetric-difference", format, args);
    args.erase(args.begin());
    switch(is_proper_sequence(args[0],args,"symmetric-difference",format)) {
      case heist_sequence::vec: return prm_perform_vector_sym_diff(procedure,args,format);
      case heist_sequence::str: return prm_perform_string_sym_diff(procedure,args,format);
      default: return prm_perform_list_sym_diff(procedure,args,format);
    }
  }

  // primitive "difference" procedure:
  data primitive_DIFFERENCE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (difference <predicate> <sequence1> <sequence2> ...)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("difference",primitive_ANY,args);
    if(args.size() < 2) 
      THROW_ERR("'difference received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("difference",args));
    auto procedure = validate_and_extract_callable(args[0], "difference", format, args);
    args.erase(args.begin());
    switch(is_proper_sequence(args[0],args,"difference",format)) {
      case heist_sequence::vec: return prm_perform_vector_diff(procedure,args,format);
      case heist_sequence::str: return prm_perform_string_diff(procedure,args,format);
      default: return prm_perform_list_diff(procedure,args,format);
    }
  }

  /******************************************************************************
  * SORTING PRIMITIVES: FOR SEQUENCES (LISTS, VECTORS, & STRINGS)
  ******************************************************************************/

  // primitive "sort" procedure:
  data primitive_SORT(scm_list& args) {
    constexpr const char * const format = 
      "\n     (sort <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("sort",primitive_SORT,args);
    // confirm has a valid argument signature
    primitive_confirm_sortable_sequence(args, "sort", format);
    // return if sorting the empty list
    if(args[1].is_type(types::sym)) return args[1];
    // sort the sequence
    return primitive_sort_sequence(args, "sort", format);
  }

  // primitive "sort!" procedure:
  data primitive_SORT_BANG(scm_list& args) {
    constexpr const char * const format = 
      "\n     (sort! <predicate> <sequence>)" SEQUENCE_DESCRIPTION;
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("sort!",primitive_SORT_BANG,args);
    // confirm has a valid argument signature
    primitive_confirm_sortable_sequence(args, "sort!", format);
    // return if sorting the empty list (already sorted)
    if(args[1].is_type(types::sym)) return G::VOID_DATA_OBJECT;
    // set the sequence to its sorted variant
    return mutatable_assign_scm_sequence(args[1],
      primitive_sort_sequence(args,"sort!",format));
  }

  // primitive "sorted?" procedure:
  data primitive_SORTEDP(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("sorted?",primitive_SORTEDP,args);
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
      auto procedure = primitive_extract_callable_procedure(args[0]);
      for(size_type i = 0, n = sequence.size(); i+1 < n; ++i) {
        scm_list args_list(2);
        args_list[0] = sequence[i], args_list[1] = sequence[i+1];
        if(is_false_scm_condition(procedure,args_list))
          return G::FALSE_DATA_BOOLEAN;
      }
    }
    return G::TRUE_DATA_BOOLEAN;
  }

  // primitive "merge" procedure:
  data primitive_MERGE(scm_list& args) { 
    constexpr const char * const format = 
      "\n     (merge <predicate> <sequence1> <sequence2>)" SEQUENCE_DESCRIPTION;
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("merge",primitive_MERGE,args);
    // Confirm given correct # of args needed
    if(args.size() != 3) 
      THROW_ERR("'merge received incorrect # of args (given " 
        << args.size() << "):" << format << FCN_ERR("merge",args));
    // Confirm given a procedure
    auto procedure = validate_and_extract_callable(args[0], "merge", format, args);
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
      return primitive_MERGE_vector_string_constructor(args,merged_sequence,format);
    // Else apply the procedure on each list elt & merge args as per the result into a list
    scm_list list_heads(args.begin()+1, args.end());
    primitive_MERGE_list_constructor(list_heads,procedure,merged_sequence);
    return primitive_LIST_to_CONS_constructor(merged_sequence.begin(),merged_sequence.end());
  }

  // primitive "delete-neighbor-dups" procedure:
  data primitive_DELETE_NEIGHBOR_DUPS(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("delete-neighbor-dups",primitive_DELETE_NEIGHBOR_DUPS,args);
    return primitive_DELETE_NEIGHBOR_DUPS_template(args,"delete-neighbor-dups",
      "\n     (delete-neighbor-dups <equality-predicate> <sequence>)"
      SEQUENCE_DESCRIPTION,false);
  }

  // primitive "delete-neighbor-dups!" procedure:
  data primitive_DELETE_NEIGHBOR_DUPS_BANG(scm_list& args) {
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("delete-neighbor-dups!",primitive_DELETE_NEIGHBOR_DUPS_BANG,args);
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

  // primitive "hmap?" procedure:
  data primitive_HMAPP(scm_list& args) {
    confirm_given_one_arg(args, "hmap?");
    return data(boolean(args[0].is_type(types::map)));
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
  // => "real" denotes a non-complex number -> NOTE hence +nan.0 is "real"
  data primitive_REALP(scm_list& args) {
    confirm_given_one_arg(args, "real?");
    return data(boolean(
      args[0].is_type(types::num) && args[0].num.is_real()));
  }

  // primitive "complex?" procedure:
  data primitive_COMPLEXP(scm_list& args) {
    confirm_given_one_arg(args, "complex?");
    return data(boolean(
      args[0].is_type(types::num) && args[0].num.is_complex()));
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
    return data(boolean(args[0].is_type(types::fcn)));
  }

  // primitive "cps-procedure?" procedure:
  data primitive_CPS_PROCEDUREP(scm_list& args) {
    confirm_given_one_arg(args, "cps-procedure?");
    return data(boolean(args[0].is_type(types::fcn) && !args[0].fcn.param_instances.empty() && 
      !args[0].fcn.param_instances[0].empty() && data_is_continuation_parameter(*args[0].fcn.param_instances[0].rbegin())));
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

  // primitive "object?" procedure:
  data primitive_OBJECTP(scm_list& args) {
    confirm_given_one_arg(args, "object?");
    return boolean(args[0].is_type(types::obj));
  }

  // primitive "class-prototype?" procedure:
  data primitive_CLASS_PROTOTYPEP(scm_list& args) {
    confirm_given_one_arg(args, "class-prototype?");
    return boolean(args[0].is_type(types::cls));
  }

  // primitive "functor?" procedure:
  // functor = object w/ a "self->procedure" method
  data primitive_FUNCTORP(scm_list& args) {
    confirm_given_one_arg(args, "functor?");
    return boolean(primitive_data_is_a_functor(args[0]));
  }

  // primitive "callable?" procedure:
  // callable = procedure? or functor?
  data primitive_CALLABLEP(scm_list& args) {
    confirm_given_one_arg(args, "callable?");
    return boolean(primitive_data_is_a_callable(args[0]));
  }

  /******************************************************************************
  * EVAL PRIMITIVE
  ******************************************************************************/

  // primitive "eval" procedure:
  data primitive_EVAL(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (eval <data> <optional-environment>)" 
      "\n     -> Pass *null-environment* to eval in the empty environment!"
      "\n     -> Pass *local-environment* to eval in the local environment (default)!"
      "\n     -> Pass *global-environment* to eval in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    // Set *local-environment* evaluation as default
    if(args.size() == 1) args.push_back(symconst::local_env);
    // use the initial/local environment if passed *null-environment* or
    //   *local-environment* as a 2nd arg
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
      "\n     -> Pass *null-environment* to cps-eval in the empty environment!"
      "\n     -> Pass *local-environment* to cps-eval in the local environment (default)!"
      "\n     -> Pass *global-environment* to cps-eval in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      THROW_ERR("'cps-eval received incorrect # of arguments:"
        << format << FCN_ERR("cps-eval", args));
    // Extract the continuation & confirm its a procedure
    auto continuation = *args.rbegin();
    auto continuation_procedure = validate_and_extract_callable(continuation, "cps-eval", format, args);
    // set the continuation to be inlined on application
    prm_set_procedure_INLINE_INVOCATION(continuation_procedure, true);
    args.pop_back();
    // use the initial/global environment if passed *null-environment* or
    //   *global-environment* as a 2nd arg
    bool must_reset_global_env = false;
    auto original_global_env = G::GLOBAL_ENVIRONMENT_POINTER;
    auto env = local_env;
    prm_CPS_EVAL_confirm_correct_number_of_args(args,must_reset_global_env,env,
                                                  original_global_env,"cps-eval",format);
    // Reset "inline"ing of the continuation if EVALing in the *null-environment*
    if(must_reset_global_env) prm_set_procedure_INLINE_INVOCATION(continuation_procedure,false);
    // if arg is self-evaluating, return arg
    if(prm_EVAL_data_is_self_evaluating(args[0])) {
      if(must_reset_global_env) G::GLOBAL_ENVIRONMENT_POINTER = original_global_env;
      scm_list cps_eval_args(1,continuation);
      return data_cast(execute_application(data_cast(scm_analyze(generate_fundamental_form_cps(args[0]),false,true)(env)),cps_eval_args,env));
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
        auto result = data_cast(execute_application(data_cast(scm_analyze(generate_fundamental_form_cps(args[0]),false,true)(env)),cps_eval_args,env));
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
        execute_application(data_cast(scm_analyze(generate_fundamental_form_cps(
          prm_EVAL_convert_list_to_AST(args[0])),false,true)(env)),cps_eval_args,env));
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
    // confirm the correct # of arguments were passed
    static constexpr const char * const format = "\n     (apply <callable> <argument-list>)";
    if(args.size() != 2)
      THROW_ERR("'apply received incorrect # of arguments:" << format << FCN_ERR("apply",args));
    // confirm 1st arg is a callable
    primitive_confirm_data_is_a_callable(args[0], "apply", format, args);
    // confirm 2nd arg is a finite, nul-terminated list
    if(!data_is_proper_list(args[1]))
      THROW_ERR("'apply 2nd arg " << PROFILE(args[1]) << " isn't a proper list!"
        << format << FCN_ERR("apply",args));
    // apply arguments in list to the callable
    scm_list args_list;
    shallow_unpack_list_into_exp(args[1], args_list);
    if(args_list.empty()) args_list.push_back(symconst::sentinel_arg);
    return data_cast(execute_callable(args[0],args_list,G::GLOBAL_ENVIRONMENT_POINTER,tail_call));
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
    // Confirm given minimum # of args needed
    static constexpr const char * const format = 
      "\n     (stream-for-each <callable> <stream1> <stream2> ...)";
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("stream-for-each",primitive_STREAM_FOR_EACH,args);
    if(args.size() < 2) 
      THROW_ERR("'stream-for-each received insufficient args (only "
        << args.size() << "):" << format << FCN_ERR("stream-for-each", args));
    // Confirm only given streams
    scm_list stream_heads(args.begin()+1, args.end());
    primitive_confirm_only_given_streams(stream_heads,"stream-for-each",format,1,args);
    // Apply the procedure on each elt of each stream
    auto procedure(validate_and_extract_callable(args[0], "stream-for-each", format, args));
    primitive_STREAM_FOR_EACH_applicator(stream_heads, procedure);
    return G::VOID_DATA_OBJECT;
  }

  // primitive "stream-ref" procedure:
  data primitive_STREAM_REF(scm_list& args) {
    // Confirm appropriate # of args given
    static constexpr const char * const format = 
      "\n     (stream-ref <stream-pair> <index>)";
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("stream-ref",primitive_STREAM_REF,args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("stream-drop",primitive_STREAM_DROP,args);
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
    // Confirm appropriate # of args given
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("stream-drop-while",primitive_STREAM_DROP_WHILE,args);
    primitive_TEMPLATE_TAKE_DROP_WHILE_VALIDATION(args, "stream-drop-while", 
      "\n     (stream-drop-while <predicate> <stream>)");
    // Get keep dropping items while 'predicate' is true, then return result
    if(data_is_the_empty_expression(args[1])) return args[1];
    auto procedure = primitive_extract_callable_procedure(args[0]);
    return primitive_DROP_WHILE_ctor(std::move(args[1]), procedure);
  }

  // primitive "stream-take" procedure:
  data primitive_STREAM_TAKE(scm_list& args){
    // Confirm appropriate # of args given
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("stream-take",primitive_STREAM_TAKE,args);
    primitive_TEMPLATE_TAKE_DROP_VALIDATION(args, "stream-take", 
      "\n     (stream-take <stream> <n>)");
    // Get the a substream after dropping 'size' items from given stream
    if(data_is_the_empty_expression(args[0])) return args[0];
    const size_type n = (size_type)args[1].num.extract_inexact();
    if(!n) return data(symconst::emptylist);
    scm_list substream;
    primitive_TAKE_SUBSTREAM_seeker(std::move(args[0]),n,substream);
    return primitive_STREAM_to_SCONS_constructor(substream.begin(),substream.end());
  }

  // primitive "stream-take-while" procedure:
  data primitive_STREAM_TAKE_WHILE(scm_list& args) {
    // Confirm appropriate # of args given
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("stream-take-while",primitive_STREAM_TAKE_WHILE,args);
    primitive_TEMPLATE_TAKE_DROP_WHILE_VALIDATION(args, "stream-take-while", 
      "\n     (stream-take-while <predicate> <stream>)");
    // Get keep dropping items while 'predicate' is true, then return result
    if(data_is_the_empty_expression(args[1])) return args[1];
    scm_list substream;
    auto procedure = primitive_extract_callable_procedure(args[0]);
    primitive_STREAM_TAKE_WHILE_ctor(std::move(args[1]), procedure, substream);
    if(substream.empty()) return data(symconst::emptylist);
    return primitive_STREAM_to_SCONS_constructor(substream.begin(),substream.end());
  }

  // primitive "stream-reverse" procedure:
  data primitive_STREAM_REVERSE(scm_list& args) {
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
    return primitive_STREAM_to_SCONS_constructor(stream_as_exp.begin(),stream_as_exp.end());
  }

  // primitive "stream-fold" procedure:
  data primitive_STREAM_FOLD(scm_list& args) {
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("stream-fold",primitive_STREAM_FOLD,args);
    return primitive_STREAM_FOLD_template(args, "stream-fold", 
            "\n     (stream-fold <callable> <seed> <stream>)", true);
  }

  // primitive "stream-fold-right" procedure:
  data primitive_STREAM_FOLD_RIGHT(scm_list& args) {
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("stream-fold-right",primitive_STREAM_FOLD_RIGHT,args);
    return primitive_STREAM_FOLD_template(args, "stream-fold-right", 
            "\n     (stream-fold-right <callable> <seed> <stream>)", false);
  }

  // primitive "stream->list" procedure:
  data primitive_CONVERT_STREAM_LIST(scm_list& args) {
    // Confirm given proper args (same signature as stream-drop & stream-take)
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("stream->list",primitive_CONVERT_STREAM_LIST,args);
    primitive_TEMPLATE_TAKE_DROP_VALIDATION(args, "stream->list", 
                                            "\n     (stream->list <stream> <size>)");
    // Invoke stream-take, convert substream -> exp -> list
    if(data_is_the_empty_expression(args[0])) return args[0];
    auto substream = primitive_STREAM_TAKE(args);
    scm_list stream_as_exp;
    unpack_stream_into_exp(std::move(substream), stream_as_exp);
    return primitive_LIST_to_CONS_constructor(stream_as_exp.begin(),stream_as_exp.end());
  }

  // primitive "list->stream" procedure:
  data primitive_CONVERT_LIST_STREAM(scm_list& args) {
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
    return primitive_STREAM_to_SCONS_constructor(par_as_exp.begin(),par_as_exp.end());
  }

  /******************************************************************************
  * TYPE COERCION PRIMITIVES
  ******************************************************************************/

  // primitive "char->integer" procedure:
  data primitive_COERCE_CHAR_TO_INTEGER(scm_list& args) {
    confirm_given_one_char_arg(args, "char->integer");
    return num_type(int(args[0].chr));
  }

  // primitive "integer->char" procedure:
  data primitive_COERCE_INTEGER_TO_CHAR(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (integer->char <non_negative-integer>)"
      "\n     <non_negative-integer> range: [0,255]";
    if(args.size() != 1)
      THROW_ERR("'integer->char didn't receive 1 integer arg:" << format 
        << FCN_ERR("integer->char",args));
    if(!args[0].is_type(types::num) || !args[0].num.is_integer())
      THROW_ERR("'integer->char didn't receive an integer arg:"
        "\n     Received arg " << PROFILE(args[0]) << format 
        << FCN_ERR("integer->char",args));
    if((args[0].num.is_neg() || args[0].num > 255) && 
       args[0].num != EOF)
      THROW_ERR("'integer->char " << PROFILE(args[0]) << " isn't a"
        "\n     positive integer ranging from 0 to 255!" << format 
        << FCN_ERR("integer->char",args));
    return chr_type(args[0].num.extract_inexact());
  }

  // primitive "number->string" procedure:
  data primitive_COERCE_NUMBER_TO_STRING(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (number->string <number> <optional-radix> <optional-precision>)"
      "\n     <optional-radix> ::= [2,36]"
      "\n     <optional-precision> ::= Integer";
    if(args.size() > 3 || args.empty())
      THROW_ERR("'number->string received incorrect # of arguments:"
        << format << FCN_ERR("number->string",args));
    // No number or invalid radix/precision given
    validate_NUMBER_TO_STRING_args(args,format);
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
    if(!args[0].num.is_real())
      THROW_ERR("'number->string only real numbers can be converted using a precision!"
        "\n     => " << PROFILE(args[0]) << " is complex!" << format << FCN_ERR("number->string", args));
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
    bool convert_string_to_scm_number(const scm_string&, num_type&)noexcept; // defined in the input parser
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
      char exactness_prefix = parse_exactness_numeric_prefix(*args[0].str);
      if(radix != 10) {
        auto num = exactness_prefix ? num_type(args[0].str->substr(2), radix) : num_type(*args[0].str, radix);
        if(num.is_nan()) return G::FALSE_DATA_BOOLEAN; // invalid conversion
        switch(exactness_prefix) {
          case 'e': return num.to_exact();
          case 'i': return num.to_inexact();
          default:  return num;
        }
      }
    }
    // immediate return if given NaN
    if(*args[0].str == "+nan.0" || *args[0].str == "-nan.0")
      return data(num_type(*args[0].str));
    num_type num;
    if(convert_string_to_scm_number(*args[0].str,num)) return num;
    return G::FALSE_DATA_BOOLEAN; // invalid conversion
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

  // primitive "functor->procedure" procedure:
  data primitive_FUNCTOR_TO_PROCEDURE(scm_list& args) {
    if(args.size() != 1 || !primitive_data_is_a_functor(args[0]))
      THROW_ERR("'functor->procedure not given 1 functor!"
        "\n     (functor->procedure <functor>)" << FCN_ERR("functor->procedure",args));
    return primitive_extract_callable_procedure(args[0]);
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
  * FORMATTING PRIMITIVES
  ******************************************************************************/

  // FORMATTING OPTIONS
  // --------
  // %a = display anything
  // %wa = write anything
  // --------
  // %n = write number
  // %+n = number (show sign if positive too)
  // %,n = number with commas (only for bigints)
  // %En = %en = to-exact number ; same as "%n" (inexact->exact <number>)
  // %In = %in = to-inexact number ; same as "%n" (exact->inexact <number>)
  // %#n = number in base # [throws an error if <#> !in-range of [2,36]]
  // %.#n = to-exact number ; same as "%n" (number->string <number> 10 <#>)
  //       -> "+%e2.5n" -> 5 digits of precision & mk exact in binary w/ sign
  //       -> NOTE: case of 'n' in "%n" denotes case of base >= 11 letters
  // --------
  // %$ = display finite real as its dollar value
  // --------
  // %s = display string
  // %ws = write string
  // --------
  // %c = display char
  // %wc = write char
  // --------
  // %b  = bool
  // %wb = write "true" or "false" instead of "#t" or "#f"
  // --------
  // %%  = "%" (ie "%%c" prints "%c" (ie doesn't seek to replace %c with a character))

  #define sprintf_formatting_token_format\
    "\n     => <formatted-string> is like C's printf with unique formatting patterns:"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %a = display anything"\
    "\n        %wa = write anything"\
    "\n        %pa = pretty-print anything"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %n = number"\
    "\n        %+n = number (show sign if positive too)"\
    "\n        %,n = number with commas (only for bigints)"\
    "\n        %En = %en = number (coerced to exact)"\
    "\n        %In = %in = number (coerced to inexact)"\
    "\n        %#n = number (in base <#>)"\
    "\n        %.#n = number (with <#> digits of precision)"\
    "\n        -> IE \"%+e2.5n\": 5 digits of precision & mk exact in binary w/ sign"\
    "\n        -> NOTE: case of 'n' in \"%n\" denotes case of base >= 11 letters"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %$ = display real finite as a dollar value"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %s = display string"\
    "\n        %ws = write string"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %c = display char"\
    "\n        %wc = write char"\
    "\n        ----------------------------------------------------------------------"\
    "\n        %b  = bool"\
    "\n        %wb = write \"true\" or \"false\" instead of \"#t\" or \"#f\""\
    "\n        ----------------------------------------------------------------------"\
    "\n        %%  = \"%\" (escapes a \"%\")"\
    "\n        ----------------------------------------------------------------------"


  // primitive "sprintf":
  // -> Parse token stream
  // -> Confirm token stream matches args
  // -> Splice in formatted args and return as a new string
  data primitive_SPRINTF(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (sprintf <formatted-string> <optional-arg1> <optional-arg2> ...)"
      sprintf_formatting_token_format;
    if(args.empty())
      THROW_ERR("'sprintf no args received!" << format << FCN_ERR("sprintf",args));
    if(!args[0].is_type(types::str))
      THROW_ERR("'sprintf 1st arg "<<PROFILE(args[0])<<" isn't a string!" 
        << format << FCN_ERR("sprintf",args));
    return make_str(generated_formatted_string(*args[0].str,format,"sprintf",args));
  }

  // primitive "displayf":
  data primitive_DISPLAYF(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (displayf <optional-output-port> <formatted-string> <optional-arg1> ...)"
      sprintf_formatting_token_format;
    return generic_formatted_output_prm<primitive_DISPLAY>(args,format,"displayf");
  }

  // primitive "writef":
  data primitive_WRITEF(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (writef <optional-output-port> <formatted-string> <optional-arg1> ...)"
      sprintf_formatting_token_format;
    return generic_formatted_output_prm<primitive_WRITE>(args,format,"writef");
  }

  // primitive "pprintf" & "pretty-printf":
  data primitive_PPRINTF(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (pprintf <optional-output-port> <formatted-string> <optional-arg1> ...)"
      sprintf_formatting_token_format;
    return generic_formatted_output_prm<primitive_PPRINT>(args,format,"pprintf");
  }


  #undef sprintf_formatting_token_format

  /******************************************************************************
  * INPUT PRIMITIVES
  ******************************************************************************/

  data primitive_READ(scm_list& args) {
    // Confirm either given an open input port or string or no args
    FILE* outs = G::CURRENT_OUTPUT_PORT, *ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "read", reading_stdin, reading_string)) 
      return chr_type(EOF);
    if(reading_string)
      return primitive_read_from_string_logic(*args[0].str);
    return primitive_read_from_input_port_logic(outs,ins,reading_stdin);
  }

  data primitive_READ_STRING(scm_list& args) {
    // return string w/ next valid scheme expression, if successfully parsed one
    FILE* outs = G::CURRENT_OUTPUT_PORT, *ins = G::CURRENT_INPUT_PORT;
    bool reading_stdin = (G::CURRENT_INPUT_PORT == stdin), reading_string = false;
    if(!confirm_valid_input_args_and_non_EOF(args, ins, "read-string", reading_stdin, reading_string)) 
      return make_str("");
    if(reading_string)
      return make_str(primitive_read_from_string_logic(*args[0].str).write());
    return make_str(primitive_read_from_input_port_logic(outs,ins,reading_stdin).write());
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
    FILE* ins = confirm_valid_input_file(args[0],"slurp-file","\n     (slurp-file <filename-string>)",args);
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
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("rename-file!",primitive_RENAME_FILE_BANG,args);
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
    confirm_no_args_given(args,"current-input-port");
    for(size_type i = 0, n = G::PORT_REGISTRY.size(); i < n; ++i)
      if(G::CURRENT_INPUT_PORT == G::PORT_REGISTRY[i]) 
        return iport(i);
    return iport();
  }

  data primitive_CURRENT_OUTPUT_PORT(scm_list& args){
    confirm_no_args_given(args,"current-output-port");
    for(size_type i = 0, n = G::PORT_REGISTRY.size(); i < n; ++i)
      if(G::CURRENT_OUTPUT_PORT == G::PORT_REGISTRY[i]) 
        return oport(i);
    return oport();
  }

  // call an unary procedure with a file's port as its argument
  data primitive_CALL_WITH_INPUT_FILE(scm_list& args){
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("call-with-input-file",primitive_CALL_WITH_INPUT_FILE,args);
    return primitive_CALL_WITH_FILE<iport>(
              args,
              "call-with-input-file",
              "\n     (call-with-input-file <filename-string> <unary-callable>)"
              "\n     <unary-callable> must accept a port as its argument!",
              confirm_valid_input_file);
  }
  data primitive_CALL_WITH_OUTPUT_FILE(scm_list& args){
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("call-with-output-file",primitive_CALL_WITH_OUTPUT_FILE,args);
    return primitive_CALL_WITH_FILE<oport>(
              args,
              "call-with-output-file",
              "\n     (call-with-output-file <filename-string> <unary-callable>)"
              "\n     <unary-callable> must accept a port as its argument!",
              confirm_valid_output_file);
  }

  // call an argless procedure with a file's port as the default port
  data primitive_WITH_INPUT_FROM_FILE(scm_list& args){
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("with-input-from-file",primitive_WITH_INPUT_FROM_FILE,args);
    return primitive_WITH_FILE(
              args,
              "with-input-from-file",
              "\n     (with-input-from-file <filename-string> <nullary-callable>)",
              G::CURRENT_INPUT_PORT,
              confirm_valid_input_file);
  }
  data primitive_WITH_OUTPUT_TO_FILE(scm_list& args){
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("with-output-to-file",primitive_WITH_OUTPUT_TO_FILE,args);
    return primitive_WITH_FILE(
              args,
              "with-output-to-file",
              "\n     (with-output-to-file <filename-string> <nullary-callable>)",
              G::CURRENT_OUTPUT_PORT,
              confirm_valid_output_file);
  }

  // retrieve a port for a file
  data primitive_OPEN_INPUT_FILE(scm_list& args){
    // confirm given a filename string
    confirm_given_one_arg(args,"open-input-file","<filename-string>");
    G::PORT_REGISTRY.push_back(confirm_valid_input_file(args[0],"open-input-file",
      "\n     (open-input-file <filename-string>)",args));
    return iport(G::PORT_REGISTRY.size()-1);
  }

  data primitive_OPEN_OUTPUT_FILE(scm_list& args){ // open iff filename dne
    // confirm given a filename string
    confirm_given_one_arg(args,"open-output-file","<filename-string>");
    G::PORT_REGISTRY.push_back(confirm_valid_output_file(args[0],"open-output-file",
      "\n     (open-output-file <filename-string>)",args));
    return oport(G::PORT_REGISTRY.size()-1);
  }

  data primitive_OPEN_OUTPUT_FILE_PLUS(scm_list& args){ // open via "append"
    // confirm given a filename string
    confirm_given_one_arg(args,"open-output-file+","<filename-string>");
    G::PORT_REGISTRY.push_back(confirm_valid_output_append_file(args[0],"open-output-file+",
      "\n     (open-output-file+ <filename-string>)",args));
    return oport(G::PORT_REGISTRY.size()-1);
  }

  data primitive_OPEN_OUTPUT_FILE_BANG(scm_list& args){ // deletes if exists, and opens anew
    // confirm given a filename string, & rm file if exists
    confirm_given_one_string_arg(args, "open-output-file!", "\n     (open-output-file! <filename-string>)");
    std::remove(args[0].str->c_str());
    G::PORT_REGISTRY.push_back(confirm_valid_output_file(args[0],"open-output-file!",
      "\n     (open-output-file! <filename-string>)",args));
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
      "\n     -> Pass *null-environment* to load in the empty environment!"
      "\n     -> Pass *local-environment* to load in the local environment (default)!"
      "\n     -> Pass *global-environment* to load in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    // Set *local-environment* evaluation as default
    if(args.size() == 1) args.push_back(symconst::local_env);
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
        args.pop_back(); // *global-environment* is default
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
      "\n     (cps-load <filename-string> <optional-environment> <continuation-callable>)"
      "\n     -> Pass *null-environment* to cps-load in the empty environment!"
      "\n     -> Pass *local-environment* to cps-load in the local environment (default)!"
      "\n     -> Pass *global-environment* to cps-load in the global environment!";
    // extract the local environment
    auto local_env = args.rbegin()->env;
    args.pop_back();
    if(args.size() < 2)
      THROW_ERR("'cps-load received incorrect # of args!" << format << FCN_ERR("cps-load",args));
    // extract the continuation
    auto continuation = *(args.rbegin());
    auto continuation_procedure = validate_and_extract_callable(continuation, "cps-load", format, args);
    // set the continuation to be inlined on application
    prm_set_procedure_INLINE_INVOCATION(continuation_procedure, true);
    args.pop_back();
    // determine which environment to load <filename-string> wrt to
    auto env = local_env;
    if(args.size()==2 && args[1].is_type(types::sym)) {
      if(args[1].sym == symconst::null_env) {
        // Reset "inline"ing of the continuation, no need in *null-environment*
        prm_set_procedure_INLINE_INVOCATION(continuation_procedure,false);
        // Reset "G::GLOBAL_ENVIRONMENT_POINTER" to its default state
        set_default_global_environment(), args.pop_back();
        try {
          scm_list cps_load_arg(1,continuation);
          // pass the continuation to the loaded file
          auto result = data_cast(execute_application(
            data_cast(primitive_CPS_LOAD_interpret_file_contents(args,G::GLOBAL_ENVIRONMENT_POINTER,format)),
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
        args.pop_back(); // *local-environment* is default
      } else {
        THROW_ERR("'cps-load \""<<args[1].sym<<"\" isn't an evaluation environment:"
          << format << FCN_ERR("cps-load", args));
      }
    }
    // pass the continuation to the loaded file
    scm_list cps_load_arg(1,continuation);
    return data_cast(execute_application(data_cast(primitive_CPS_LOAD_interpret_file_contents(args,env,format)),cps_load_arg,env));
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
    confirm_no_args_given(args,"command-line");
    return make_str(HEIST_COMMAND_LINE_ARGS);
  }

  // Returns a string of the current working directory
  data primitive_GETCWD(scm_list& args) {
    confirm_no_args_given(args,"getcwd");
    return make_str(std::filesystem::current_path());
  }

  // Returns a string of the parent directory of the given path string
  data primitive_DIRNAME(scm_list& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      THROW_ERR("'dirname didn't get a filepath <string> arg:"
        "\n     (dirname <filepath-string>)" << FCN_ERR("dirname",args));
    return make_str(std::filesystem::path(*args[0].str).parent_path());
  }

  /******************************************************************************
  * CURRENT TIME PRIMITIVES
  ******************************************************************************/

  data primitive_SECONDS_SINCE_EPOCH(scm_list& args) {
    confirm_no_args_given(args,"seconds-since-epoch");
    return num_type(std::chrono::duration_cast<std::chrono::seconds>(
                    std::chrono::system_clock::now().time_since_epoch()).count());
  }

  data primitive_TIME(scm_list& args) {
    if(args.empty())
      THROW_ERR("'time received incorrect # of args!"
        "\n     (time <callable> <arg1> ... <argN>)" << FCN_ERR("time",args));
    primitive_confirm_data_is_a_callable(args[0], "time", 
      "\n     (time <callable> <arg1> ... <argN>)", args);
    scm_list time_args(args.begin()+1,args.end());
    if(time_args.empty()) time_args.push_back(symconst::sentinel_arg);
    auto start = std::chrono::high_resolution_clock::now();
    auto result = execute_callable(args[0],time_args);
    auto end = std::chrono::high_resolution_clock::now();
    // return a pair: (cons <time> <result>)
    data p = make_par();
    p.par->first = num_type(convert_us_to_s(std::chrono::duration_cast<std::chrono::microseconds>(end-start).count()));
    p.par->second = data_cast(result);
    return p;
  }

  data primitive_CURRENT_DATE(scm_list& args) {
    if(args.empty()) return make_str(get_current_time_stamp());
    long long s=0, m=0, h=0, d=0, y=0;
    parse_current_date_offsets(args,s,m,h,d,y);
    return make_str(get_current_time_stamp(s,m,h,d,y));
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

  data primitive_NANSIP(scm_list& args) {
    confirm_no_args_given(args,"nansi?");
    return boolean(!G::USING_ANSI_ESCAPE_SEQUENCES);
  }

  // Defaults to enabling case-sensitivity, if not given a boolean.
  // Returns whether case-sensitivity was active prior this call
  data primitive_SET_CI(scm_list& args) {
    return primitive_TOGGLE_DISPLAY_SETTING(args, "set-ci!", 
                                            G::USING_CASE_SENSITIVE_SYMBOLS);
  }

  data primitive_CIP(scm_list& args) {
    confirm_no_args_given(args,"ci?");
    return boolean(!G::USING_CASE_SENSITIVE_SYMBOLS);
  }

  data primitive_SET_PPRINT_COLUMN_WIDTH_BANG(scm_list& args) {
    return primitive_TOGGLE_NUMERIC_SETTING(args,"set-pprint-column-width!",
                                            G::PPRINT_MAX_COLUMN_WIDTH);
  }

  data primitive_PPRINT_COLUMN_WIDTH(scm_list& args) {
    confirm_no_args_given(args,"pprint-column-width");
    return num_type(G::PPRINT_MAX_COLUMN_WIDTH);
  }

  data primitive_SET_MAX_RECURSION_DEPTH_BANG(scm_list& args) {
    return primitive_TOGGLE_NUMERIC_SETTING(args,"set-max-recursion-depth!",
                                            G::MAX_RECURSION_DEPTH);
  }

  data primitive_MAX_RECURSION_DEPTH(scm_list& args) {
    confirm_no_args_given(args,"max-recursion-depth");
    return num_type(G::MAX_RECURSION_DEPTH);
  }

  // Changes the REPL's line-by-line prompt from the default "> "
  data primitive_SET_REPL_PROMPT_BANG(scm_list& args) {
    confirm_given_one_arg(args,"set-repl-prompt!","<prompt-string>");
    if(!args[0].is_type(types::str))
      THROW_ERR("'set-repl-prompt! "<<PROFILE(args[0])<<" isn't a string:"
        "\n     (set-repl-prompt! <prompt-string>)" << FCN_ERR("set-repl-prompt!",args));
    scm_string old_prompt = G::REPL_PROMPT;
    if(args[0].str->empty()) {
      G::REPL_PROMPT = G::REPL_TAB = "";
      return make_str(old_prompt);
    }
    G::REPL_PROMPT = *args[0].str;
    G::REPL_TAB = scm_string(G::REPL_PROMPT.size(), ' ');
    return make_str(old_prompt);
  }

  data primitive_REPL_PROMPT(scm_list& args) {
    confirm_no_args_given(args,"repl-prompt");
    return make_str(G::REPL_PROMPT);
  }

  // Toggles Dynamic Procedure Call Tracing (returns the previous state prior toggle)
  data primitive_SET_DYNAMIC_CALL_TRACE_BANG(scm_list& args) {
    return primitive_TOGGLE_BOOLEAN_SETTING(args,"set-dynamic-call-trace!",
                                            G::TRACING_ALL_FUNCTION_CALLS);
  }

  data primitive_DYNAMIC_CALL_TRACEP(scm_list& args) {
    confirm_no_args_given(args,"dynamic-call-trace?");
    return boolean(G::TRACING_ALL_FUNCTION_CALLS);
  }

  // Toggles Procedure Argument Call Tracing (returns the previous state prior toggle)
  data primitive_SET_TRACE_ARGS_BANG(scm_list& args) {
    return primitive_TOGGLE_BOOLEAN_SETTING(args,"set-trace-args!",G::TRACE_ARGS);
  }

  data primitive_TRACE_ARGSP(scm_list& args) {
    confirm_no_args_given(args,"trace-args?");
    return boolean(G::TRACE_ARGS);
  }

  data primitive_SET_DOT_BANG(scm_list& args) {
    if(args.size() != 1 || !args[0].is_type(types::chr))
      THROW_ERR("'set-dot! didn't receive 1 character!"
        "\n     (set-dot! <char>)" << FCN_ERR("set-dot!",args));
    data original_dot = symconst::dot[0];
    symconst::dot[0] = args[0].chr;
    return original_dot;
  }

  data primitive_DOT(scm_list& args) {
    if(!args.empty())
      THROW_ERR("'dot doesn't accept any args: (dot)" << FCN_ERR("dot",args));
    return data(symconst::dot[0]);
  }

  /******************************************************************************
  * CONTROL-FLOW PRIMITIVES: EXITING, ERROR HANDLING, INLINING, & JUMPING
  ******************************************************************************/

  data primitive_EXIT(scm_list& args) {
    static constexpr const auto MAX_INT = std::numeric_limits<int>::max();
    static constexpr const auto MIN_INT = std::numeric_limits<int>::min();
    if(args.size() > 1)
      THROW_ERR("'exit received more than 1 argument!"
        "\n     (exit <optional-integer-exit-code>)"
        "\n     => <optional-integer-exit-code> bounds: [" 
        << MIN_INT << ", " << MAX_INT <<']' << FCN_ERR("exit",args));
    if(args.empty()) throw SCM_EXCEPT::EXIT;
    if(!args[0].is_type(types::num) || !args[0].num.is_integer())
      THROW_ERR("'exit didn't receive an integer argument!"
        "\n     (exit <optional-integer-exit-code>)"
        "\n     => <optional-integer-exit-code> bounds: [" 
        << MIN_INT << ", " << MAX_INT <<']' << FCN_ERR("exit",args));
    if(args[0].num > MAX_INT || args[0].num < MIN_INT)
      THROW_ERR("'exit integer argument doesn't fall w/in proper bounds!"
        "\n     (exit <optional-integer-exit-code>)"
        "\n     => <optional-integer-exit-code> bounds: [" 
        << MIN_INT << ", " << MAX_INT <<']' << FCN_ERR("exit",args));
    G::HEIST_EXIT_CODE = (int)args[0].num.extract_inexact();
    throw SCM_EXCEPT::EXIT;
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
      THROW_ERR("'call/ce received incorrect # of args!"
        "\n     (call/ce <callable> <arg1> ... <argN>)" << FCN_ERR("call/ce",args));
    primitive_confirm_data_is_a_callable(args[0], "call/ce", 
      "\n     (call/ce <callable> <arg1> ... <argN>)", args);
    scm_list call_ce_args(args.begin()+1,args.end());
    if(call_ce_args.empty()) call_ce_args.push_back(symconst::sentinel_arg);
    return data_cast(execute_callable(args[0],call_ce_args,env,false,true));
  }

  // Propagates "call/ce" across this invocation & every subsequent invocation 
  //   resulting from this invocation (ie "deep" call/ce)
  data primitive_INLINE(scm_list& args) {
    auto env = args.rbegin()->env;
    args.pop_back();
    if(args.empty())
      THROW_ERR("'inline received incorrect # of args!"
        "\n     (inline <callable> <arg1> ... <argN>)" << FCN_ERR("inline",args));
    primitive_confirm_data_is_a_callable(args[0], "inline", 
      "\n     (inline <callable> <arg1> ... <argN>)", args);
    scm_list inline_args(args.begin()+1,args.end());
    if(inline_args.empty()) inline_args.push_back(symconst::sentinel_arg);
    G::USING_INLINE_INVOCATIONS = true;
    try {
      auto result = data_cast(execute_callable(args[0],inline_args,env));
      G::USING_INLINE_INVOCATIONS = false;
      return result;
    } catch(const SCM_EXCEPT& call_ce_error) {
      G::USING_INLINE_INVOCATIONS = false;
      throw call_ce_error;
    }
  }

  data primitive_JUMP_BANG(scm_list& args) {
    if(args.size() > 1)
      THROW_ERR("'jump! received incorrect # of args!"
        "\n     (jump! <optional-arg>)" << FCN_ERR("jump!",args));
    if(args.size() == 1)
      G::JUMP_GLOBAL_PRIMITIVE_ARGUMENT = args[0];
    else
      G::JUMP_GLOBAL_PRIMITIVE_ARGUMENT = G::VOID_DATA_OBJECT;
    throw SCM_EXCEPT::JUMP;
    return data();
  }

  data primitive_CATCH_JUMP(scm_list& args) {
    if(args.empty())
      THROW_ERR("'catch-jump received incorrect # of args!"
        "\n     (catch-jump <callable> <arg1> ... <argN>)" << FCN_ERR("catch-jump",args));
    primitive_confirm_data_is_a_callable(args[0], "catch-jump", 
      "\n     (catch-jump <callable> <arg1> ... <argN>)", args);
    scm_list catch_jump_args(args.begin()+1,args.end());
    if(catch_jump_args.empty()) catch_jump_args.push_back(symconst::sentinel_arg);
    const bool inline_status = G::USING_INLINE_INVOCATIONS;
    try {
      return data_cast(execute_callable(args[0],catch_jump_args));
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
    if(args.empty())
      THROW_ERR("'trace received incorrect # of args!"
        "\n     (trace <procedure> <arg1> ... <argN>)" << FCN_ERR("trace",args));
    primitive_confirm_data_is_a_procedure(args[0], "trace", 
      "\n     (trace <procedure> <arg1> ... <argN>)", args);
    scm_list trace_args(args.begin()+1,args.end());
    if(trace_args.empty()) trace_args.push_back(symconst::sentinel_arg);
    // Set name of the function to trace
    G::TRACED_FUNCTION_NAME = args[0].fcn.name;
    auto result = data_cast(execute_application(args[0],trace_args));
    G::TRACED_FUNCTION_NAME = "";
    return result;
  }

  /******************************************************************************
  * GENSYM PRIMITIVES
  ******************************************************************************/

  data primitive_GENSYM(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (gensym <optional-instance-#-to-reference>)"
      "\n     => (gensym n) refers to the symbol generated by the nth last (gensym) call!";
    if(args.size() > 1)
      THROW_ERR("'gensym received incorrect # of args!" << format << FCN_ERR("gensym",args));
    if(args.size() == 1) {
      if(!args[0].is_type(types::num) || !args[0].num.is_integer() || args[0].num.is_neg())
        THROW_ERR("'gensym arg " << PROFILE(args[0]) << " isn't a non-negative integer!"
          << format << FCN_ERR("gensym",args));
      return decremented_hashed_gensym_arg((size_type)args[0].num.extract_inexact(),args);
    }
    return new_hashed_gensym_arg();
  }


  data primitive_SOWN_GENSYM(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (sown-gensym <seed>)"
      "\n     => <seed> = number | symbol | boolean";
    if(args.size() != 1)
      THROW_ERR("'sown-gensym received incorrect # of args!" 
        << format << FCN_ERR("sown-gensym",args));
    if(!args[0].is_type(types::num) && !args[0].is_type(types::sym) && !args[0].is_type(types::bol))
      THROW_ERR("'sown-gensym arg "<<PROFILE(args[0])<<" isn't a valid 'sown-gensym key!" 
        << format << FCN_ERR("sown-gensym",args));
    return symconst::gensym_prefix + scm_string("SOWN-") + args[0].write();
  }

  /******************************************************************************
  * SYNTAX PREDICATE PRIMITIVES
  ******************************************************************************/

  data primitive_CORE_SYNTAXP(scm_list& args) {
    if(args.size() != 1 || !args[0].is_type(types::sym))
      THROW_ERR("'core-syntax? didn't receive 1 symbolic arg!"
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
      THROW_ERR("'runtime-syntax? didn't receive 1 symbolic arg!"
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
      THROW_ERR("'reader-syntax? didn't receive 1 string arg!"
        "\n     => Must be a string to avoid expansion by the reader if IS syntax!"
        "\n     (reader-syntax? <string>)" << FCN_ERR("reader-syntax?",args));
    auto& sought_shorthand = *args[0].str;
    for(const auto& reader_syntax_label : G::SHORTHAND_READER_MACRO_REGISTRY)
      if(reader_syntax_label == sought_shorthand)
        return G::TRUE_DATA_BOOLEAN;
    return G::FALSE_DATA_BOOLEAN;
  }


  data primitive_READER_ALIASP(scm_list& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      THROW_ERR("'reader-alias? didn't receive 1 string arg!"
        "\n     => Must be a string to avoid expansion by the reader if IS syntax!"
        "\n     (reader-alias? <string>)" << FCN_ERR("reader-alias?",args));
    auto& sought_shorthand = *args[0].str;
    for(const auto& reader_syntax_label : G::SHORTHAND_READER_ALIAS_REGISTRY)
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
  * READER MACRO/ALIAS DEFINITION & ANALYSIS PRIMITIVE
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

  data primitive_READER_SYNTAX_LIST(scm_list& args) {
    confirm_no_args_given(args,"reader-syntax-list");
    scm_list pairs;
    for(size_type i = 0, n = G::SHORTHAND_READER_MACRO_REGISTRY.size(); i < n; ++i) {
      pairs.push_back(make_par());
      pairs[i].par->first = make_str(G::SHORTHAND_READER_MACRO_REGISTRY[i]);
      pairs[i].par->second = make_str(G::LONGHAND_READER_MACRO_REGISTRY[i]);
    }
    return primitive_LIST_to_CONS_constructor(pairs.begin(),pairs.end());
  }

  data primitive_READER_ALIAS_LIST(scm_list& args) {
    confirm_no_args_given(args,"reader-alias-list");
    scm_list pairs;
    for(size_type i = 0, n = G::SHORTHAND_READER_ALIAS_REGISTRY.size(); i < n; ++i) {
      pairs.push_back(make_par());
      pairs[i].par->first = make_str(G::SHORTHAND_READER_ALIAS_REGISTRY[i]);
      pairs[i].par->second = make_str(G::LONGHAND_READER_ALIAS_REGISTRY[i]);
    }
    return primitive_LIST_to_CONS_constructor(pairs.begin(),pairs.end());
  }

  /******************************************************************************
  * INFIX TABLE ALIST
  ******************************************************************************/

  data primitive_INFIX_LIST(scm_list& args) {
    confirm_no_args_given(args,"infix-list");
    return get_infix_list();
  }

  /******************************************************************************
  * JSON PARSING AND GENERATION PRIMITIVES
  ******************************************************************************/

  // CONVERTING JSON STRINGS TO A PARSABLE SCHEME DATA STRUCT:
  // ,              -> <space>
  // true           -> #t
  // false          -> #f
  // null           -> '()
  // [...]          -> [vector ...]
  // <string>:<obj> -> (list <string> <obj>)

  data primitive_JSON_TO_SCM(scm_list& args) {
    if(args.size() != 1 || !args[0].is_type(types::str))
      THROW_ERR("'json->scm didn't receive 1 string arg!" 
        "\n     (json->scm <string>)" << FCN_ERR("json->scm", args));
    if(args[0].str->empty()) return G::VOID_DATA_OBJECT;
    scm_string input = *args[0].str;
    try { // Try parsing the converted json expression, & throw an error as needed
      scm_list abstract_syntax_tree;
      // Return AST if successfully parsed an expression
      parse_input_exp(heist_json_parser::convert_json_to_scm(*args[0].str,input),abstract_syntax_tree);
      if(abstract_syntax_tree.empty()) return G::VOID_DATA_OBJECT;
      return data_cast(scm_eval(scm_list_cast(abstract_syntax_tree[0]),G::GLOBAL_ENVIRONMENT_POINTER));
    } catch(const READER_ERROR& read_error) {
      heist_json_parser::print_json_reader_error_alert();
      if(is_non_repl_reader_error(read_error))
           alert_non_repl_reader_error(G::CURRENT_OUTPUT_PORT,read_error,input);
      else alert_reader_error(G::CURRENT_OUTPUT_PORT,read_error,input);
      throw SCM_EXCEPT::READ;
    } catch(const size_type& read_error_index) {
      heist_json_parser::print_json_reader_error_alert();
      alert_reader_error(G::CURRENT_OUTPUT_PORT,read_error_index,input);
      throw SCM_EXCEPT::READ;
    }
  }


  data primitive_SCM_TO_JSON(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (scm->json <obj> <optional-indent-width>)"
      "\n     <obj> ::= <string>"
      "\n             | <number>"
      "\n             | <'()>    ; -> <null>" 
      "\n             | <alist>  ; -> <map> (keys must be string | number | null | bool!)"
      "\n             | <vector> ; -> <array>"
      "\n             | <boolean>";
    if(args.empty() || args.size() > 2)
      THROW_ERR("'scm->json didn't receive correct # of args:"
        << format << FCN_ERR("scm->json",args));
    size_type indent_width = 0;
    if(args.size() == 2) {
      if(!primitive_is_valid_index(args[1]))
        THROW_ERR("'scm->json 2nd arg " << PROFILE(args[0]) << " isn't a valid indent width!" << format 
          << "\n     <optional-indent-width> := [0, " << G::MAX_SIZE_TYPE << ']' << FCN_ERR("scm->json",args));
      indent_width = (size_type)args[1].num.extract_inexact();
    }
    return make_str(heist_json_generator::format_scm_as_json(args[0],indent_width,args,format));
  }


  data primitive_JSON_DATUMP(scm_list& args) {
    confirm_given_one_arg(args,"json-datum?");
    return boolean(is_valid_json_datum(args[0]));
  }

  /******************************************************************************
  * REGEX PRIMITIVES
  ******************************************************************************/

  // primitive "regex-replace": replaces 1st instance w/ a string or using the given callable
  data primitive_REGEX_REPLACE(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (regex-replace <target-string> <regex-string> <replacement-string>)"
      "\n     (regex-replace <target-string> <regex-string> <callable>)"
      "\n     -> <callable> ::= (lambda (<prefix>, <suffix>, <match1>, ...) <body>)";
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("regex-replace",primitive_REGEX_REPLACE,args);
    confirm_n_args_and_first_2_args_are_strings(args,3,format,"regex-replace");
    return regex_primitive_replace_application(args,format,"regex-replace",regex_replace,regex_replace_fcn);
  }


  // primitive "regex-replace-all": replaces all instances w/ a string or using the given callable
  data primitive_REGEX_REPLACE_ALL(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (regex-replace-all <target-string> <regex-string> <replacement-string>)"
      "\n     (regex-replace-all <target-string> <regex-string> <callable>)"
      "\n     -> <callable> ::= (lambda (<prefix>, <suffix>, <match1>, ...) <body>)";
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("regex-replace-all",primitive_REGEX_REPLACE_ALL,args);
    confirm_n_args_and_first_2_args_are_strings(args,3,format,"regex-replace-all");
    return regex_primitive_replace_application(args,format,"regex-replace-all",regex_replace_all,regex_replace_all_fcn);
  }


  // primitive "regex-match"
  // => returns an alist of the matched substrings
  //    -> each sublist begins with the position, followed by all match
  data primitive_REGEX_MATCH(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (regex-match <target-string> <regex-string>)";
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("regex-match",primitive_REGEX_MATCH,args);
    confirm_n_args_and_first_2_args_are_strings(args,2,format,"regex-match");
    try {
      return get_regex_matches(*args[0].str,*args[1].str);
    } catch(...) {
      return throw_malformed_regex(args,format,"regex-match");
    }
  }


  // primitive "regex-split" procedure:
  data primitive_REGEX_SPLIT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (regex-split <target-string> <optional-regex-string> <optional-start-index>)";
    scm_string delimiter("");
    size_type start_index = 0;
    confirm_proper_string_split_args(args,"regex-split",format,delimiter,start_index);
    // split the string into a list of strings
    try {
      return regex_split(args[0].str->substr(start_index),delimiter);
    } catch(...) {
      return throw_malformed_regex(args,format,"regex-split");
    }
  }

  /******************************************************************************
  * DEFCLASS OO SUPPORT INTERNAL PRIMITIVES
  ******************************************************************************/

  // primitive "heist:core:oo:set-member!" procedure:
  data primitive_HEIST_CORE_OO_SET_MEMBER_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:oo:set-member! <object> <member-name-symbol> <value>)";
    validate_oo_member_setter(args,"heist:core:oo:set-member!",format,"member");
    // Search Prototype/Inherited Prototype
    if(!set_new_object_member_value(args[0].obj->proto,args[0].obj,args[1].sym,args[2]))
      THROW_ERR("'heist:core:oo:set-member! 2nd member-name arg "<<PROFILE(args[1])
        <<" isn't a member of "<<PROFILE(args[0])<<'!'<<format<<FCN_ERR("heist:core:oo:set-member!",args));
    return G::VOID_DATA_OBJECT;
  }


  // primitive "heist:core:oo:compare-classes" procedure:
  data primitive_HEIST_CORE_OO_COMPARE_CLASSES(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:oo:compare-classes <obj> <class-prototype>)";
    if(args.size() != 2)
      THROW_ERR("'heist:core:oo:compare-classes didn't receive 2 args!"
        <<format<<FCN_ERR("heist:core:oo:compare-classes",args));
    if(!args[1].is_type(types::cls))
      THROW_ERR("'heist:core:oo:compare-classes 2nd arg "<<PROFILE(args[1])<<" isn't a class-prototype!"
        <<format<<FCN_ERR("heist:core:oo:compare-classes",args));
    if(!args[0].is_type(types::obj)) return G::FALSE_DATA_BOOLEAN;
    return boolean(args[0].obj->proto == args[1].cls);
  }


  // primitive "heist:core:oo:make-object" procedure:
  data primitive_HEIST_CORE_OO_MAKE_OBJECT(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:oo:make-object <class-type-object> <optional-container>)"
      "\n     <optional-container> ::= <member-val-hmap>"
      "\n                            | <member-val-vector>"
      "\n                            | <member-val-proper-list>";
    if(args.empty() || args.size() > 2)
      THROW_ERR("'heist:core:oo:make-object received improper # of args!" 
        << format << FCN_ERR("heist:core:oo:make-object", args));
    if(!args[0].is_type(types::cls))
      THROW_ERR("'heist:core:oo:make-object 1st arg "<<PROFILE(args[0])<<" isn't a class-prototype!"
        << format << FCN_ERR("heist:core:oo:make-object", args));
    // create the object
    auto& class_proto_obj = args[0].cls;
    object_type obj;
    initialize_object_with_prototype_properties_and_inheritance(obj,class_proto_obj);
    // no args (or '()) given
    if(args.size() == 1 || data_is_the_empty_expression(args[1])) return make_obj(std::move(obj));
    // confirm given a container (hmap | vector | list)
    switch(args[1].type) {
      case types::map: return initialize_OO_ctord_object_HMAP(args,class_proto_obj,obj,format);
      case types::vec: return initialize_OO_ctord_object_VECT(args,class_proto_obj,obj,format);
      case types::par: return initialize_OO_ctord_object_LIST(args,class_proto_obj,obj,format);
      default:
        THROW_ERR("'make-"<< class_proto_obj->class_name<<" arg "<<PROFILE(args[1]) 
          << " isn't a container of member values!" << format 
          << FCN_ERR("'make-"+class_proto_obj->class_name,args));
    }
    return data(); // never triggered
  }


  // primitive "heist:core:oo:register-member!" procedure:
  data primitive_HEIST_CORE_OO_REGISTER_MEMBER(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:oo:register-member! <object> <member-name-symbol> <default-value>)";
    validate_oo_member_setter(args,"add-member!",format,"member");
    // Set local member if already exists
    for(size_type i = 0, n = args[0].obj->member_names.size(); i < n; ++i) {
      if(args[0].obj->member_names[i] == args[1].sym) {
        args[0].obj->member_values[i] = args[2];
        return G::VOID_DATA_OBJECT;
      }
    }
    // Confirm Member doesn't already exist as a local method
    for(size_type i = 0, n = args[0].obj->method_names.size(); i < n; ++i) {
      if(args[0].obj->method_names[i] == args[1].sym) {
        THROW_ERR("'add-member! member-name arg "<<PROFILE(args[1])
        <<" is already defined as a method of "<<PROFILE(args[0])<<'!'
        <<format<<FCN_ERR("'add-member!",args));
      }
    }
    // define a setter for the new member
    define_setter_method_for_member(*args[0].obj, args[0].obj->proto->defn_env, args[1].sym);
    // add the new member name & assign it the given value
    args[0].obj->member_names.push_back(args[1].sym);
    args[0].obj->member_values.push_back(args[2]);
    return G::VOID_DATA_OBJECT;
  }


  // primitive "heist:core:oo:register-method!" procedure:
  data primitive_HEIST_CORE_OO_REGISTER_METHOD(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (heist:core:oo:register-method! <object> <method-name-symbol> <procedure-value>)";
    validate_oo_member_setter(args,"add-method!",format,"method");
    primitive_confirm_data_is_a_procedure(args[2], "add-method!", format, args);
    // Set local method if already exists
    for(size_type i = 0, n = args[0].obj->method_names.size(); i < n; ++i) {
      if(args[0].obj->method_names[i] == args[1].sym) {
        args[0].obj->method_values[i] = args[2];
        return G::VOID_DATA_OBJECT;
      }
    }
    // Confirm Member doesn't already exist as a local method
    for(size_type i = 0, n = args[0].obj->member_names.size(); i < n; ++i) {
      if(args[0].obj->member_names[i] == args[1].sym) {
        THROW_ERR("'add-method! method-name arg "<<PROFILE(args[1])
        <<" is already defined as a member of "<<PROFILE(args[0])<<'!'
        <<format<<FCN_ERR("'add-method!",args));
      }
    }
    // add the new method name & assign it the given value
    args[0].obj->method_names.push_back(args[1].sym);
    args[0].obj->method_values.push_back(args[2]);
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * DEFCLASS OO GENERAL OBJECT ANALYSIS PRIMITIVES
  ******************************************************************************/

  data primitive_OBJECT_MEMBERS(scm_list& args) {
    confirm_given_unary_object_arg(args,"object-members");
    scm_map m;
    for(size_type i = 0, n = args[0].obj->member_names.size(); i < n; ++i)
      m.val[args[0].obj->member_names[i]+char(types::sym)] = args[0].obj->member_values[i];
    return make_map(std::move(m));
  }

  data primitive_OBJECT_METHODS(scm_list& args) {
    confirm_given_unary_object_arg(args,"object-methods");
    scm_map m;
    for(size_type i = 0, n = args[0].obj->method_names.size(); i < n; ++i)
      m.val[args[0].obj->method_names[i]+char(types::sym)] = extend_method_env_with_SELF_object(args[0].obj,args[0].obj->method_values[i].fcn);
    return make_map(std::move(m));
  }

  // primitive ".." procedure:
  data primitive_HEIST_CORE_OO_MEMBER_ACCESS(scm_list& args) {
    static constexpr const char * const format = "\n     (.. <object> <property-1> ...)";
    if(args.size() == 1) return GENERATE_PRIMITIVE_PARTIAL("..",primitive_HEIST_CORE_OO_MEMBER_ACCESS,args);
    if(args.size() < 2)
      THROW_ERR("'.. not enough args received!" << format << FCN_ERR("..",args));
    data value = args[0];
    // get the call value
    for(size_type i = 1, n = args.size(); i < n; ++i) {
      if(!value.is_type(types::obj))
        THROW_ERR("'.. can't access property "<<PROFILE(args[i])<<" in non-object "
          << PROFILE(value) << '!' << FCN_ERR("..",args));
      if(!args[i].is_type(types::sym))
        THROW_ERR("'.. can't access non-symbolic property "<<PROFILE(args[i])<<" in object "
          << value << '!' << FCN_ERR("..",args));
      // Search local members & methods, the proto, and the proto inheritances
      bool is_member = false;
      if(!seek_call_value_in_local_object(value,args[i].sym,is_member))
        THROW_ERR("'.. "<<args[i].sym<<" isn't a property in object "
          << value << '!' << FCN_ERR("..",args));
      // if found a method, confirm at the last item in the call chain
      if(!is_member && i+1 < n)
        THROW_ERR("'.. can't access property "<<args[i].sym
          <<" of method "<<PROFILE(value)<<'!'<< FCN_ERR("..",args));
    }
    return value;
  }

  // NOTE: recursively converts object member values into hashmaps as well
  data primitive_OBJECT_TO_HMAP(scm_list& args) {
    confirm_given_unary_object_arg(args,"object->hmap");
    return prm_recursively_convert_OBJ_to_HMAP<false>(args[0]);
  }

  data primitive_OBJECT_TO_ALIST(scm_list& args) {
    confirm_given_unary_object_arg(args,"object->alist");
    return prm_recursively_convert_HMAP_to_ALIST(prm_recursively_convert_OBJ_to_HMAP<false>(args[0]));
  }

  data primitive_OBJECT_TO_JSON(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (object->json <object> <optional-indent-width>)";
    if(args.empty() || args.size() > 2)
      THROW_ERR("'object->json didn't receive correct # of args:"
        << format << FCN_ERR("object->json",args));
    if(!args[0].is_type(types::obj))
      THROW_ERR("'object->json arg " << PROFILE(args[0]) << " isn't an object!"
        << format << FCN_ERR("object->json",args));
    size_type indent_width = 0;
    if(args.size() == 2) {
      if(!primitive_is_valid_index(args[1]))
        THROW_ERR("'object->json 2nd arg " << PROFILE(args[1]) << " isn't a valid indent width!" << format 
          << "\n     <optional-indent-width> := [0, " << G::MAX_SIZE_TYPE << ']' << FCN_ERR("object->json",args));
      indent_width = (size_type)args[1].num.extract_inexact();
    }
    auto val = prm_convert_OBJ_HMAP_into_valid_JSON_ALIST_datum(prm_recursively_convert_OBJ_to_HMAP<true>(args[0]));
    return make_str(heist_json_generator::format_scm_as_json(val,indent_width,args,format));
  }

  /******************************************************************************
  * DEFCLASS OO GENERAL CLASS-PROTOTYPE ANALYSIS PRIMITIVES
  ******************************************************************************/

  data primitive_PROTO_NAME(scm_list& args) {
    confirm_given_unary_class_prototype_arg(args,"proto-name");
    return args[0].cls->class_name;
  }

  data primitive_PROTO_MEMBERS(scm_list& args) {
    confirm_given_unary_class_prototype_arg(args,"proto-members");
    scm_list member_names;
    for(const auto& name : args[0].cls->member_names)
      member_names.push_back(name);
    return primitive_LIST_to_CONS_constructor(member_names.begin(),member_names.end());
  }

  data primitive_PROTO_METHODS(scm_list& args) {
    confirm_given_unary_class_prototype_arg(args,"proto-methods");
    scm_list method_names;
    for(const auto& name : args[0].cls->method_names)
      method_names.push_back(name);
    return primitive_LIST_to_CONS_constructor(method_names.begin(),method_names.end());
  }

  data primitive_PROTO_SUPER(scm_list& args) {
    confirm_given_unary_class_prototype_arg(args,"proto-super");
    if(!args[0].cls->inherited) return G::FALSE_DATA_BOOLEAN;
    return args[0].cls->inherited;
  }

  data primitive_PROTO_ADD_MEMBER_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (proto-add-member! <class-prototype> <member-name-symbol> <default-value>)";
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("proto-add-member!",primitive_PROTO_ADD_MEMBER_BANG,args);
    confirm_proper_new_property_args(args,"proto-add-member!",format);
    // Verify new member name isn't already the name of a member or method
    confirm_new_property_name_doesnt_already_exist(args,"proto-add-member!",format);
    // Define setter for the new member & the new member
    define_setter_method_for_member(*args[0].cls,args[0].cls->defn_env,args[1].sym);
    args[0].cls->member_names.push_back(args[1].sym);
    args[0].cls->member_values.push_back(args[2]);
    return G::VOID_DATA_OBJECT;
  }

  data primitive_PROTO_ADD_METHOD_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (proto-add-method! <class-prototype> <method-name-symbol> <procedure-value>)";
    if(!args.empty() && args.size() < 3) return GENERATE_PRIMITIVE_PARTIAL("proto-add-method!",primitive_PROTO_ADD_METHOD_BANG,args);
    confirm_proper_new_property_args(args,"proto-add-method!",format);
    auto procedure = validate_and_extract_callable(args[2], "proto-add-method!", format, args);
    // Verify new method name isn't already the name of a member or method
    confirm_new_property_name_doesnt_already_exist(args,"proto-add-method!",format);
    // Define the new method
    args[0].cls->method_names.push_back(args[1].sym);
    args[0].cls->method_values.push_back(procedure);
    return G::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * COROUTINE CYCLING PRIMITIVE
  ******************************************************************************/

  // ;; Circular invocation of procedures yielding one another
  // ;; WARNING: If none of the coroutines terminate, this procedure won't either!
  // (define (cycle-coroutines! . coroutines)
  //   (define (cycle coroutines)
  //     (cycle (map (lambda (c) (if (coroutine? c) (c.next) (jump! c)))
  //                 coroutines)))
  //   (catch-jump cycle coroutines))
  data primitive_CYCLE_COROUTINES_BANG(scm_list& args) {
    static constexpr const char * const format = 
      "\n     (cycle-coroutines! <coroutine-1> <coroutine-2> ...)";
    auto coro_proto = prm_get_coroutine_class_prototype(args,format);
    size_type i = 0, n = args.size();
    for(;;) {
      for(i = 0; i < n; ++i) {
        if(!datum_is_a_coroutine(args[i],coro_proto)) return args[i];
        args[i] = prm_invoke_coroutine_NEXT_method(args[i],format);
      }
    }
    return data(); // never triggered
  }

  /******************************************************************************
  * "fmt:" PREFIXED ANSI ESCAPE CODE PRIMITIVES
  ******************************************************************************/

  #include "toolkits/heist_fmt_toolkit.hpp"

  /******************************************************************************
  * STRING->ART PRIMITIVES
  ******************************************************************************/

  data primitive_STRING_TO_ASCII_ART(scm_list& args) { 
    if(args.size() != 1 || !args[0].is_type(types::str))
      THROW_ERR("'string->ascii-art didn't receive 1 string arg!"
        "\n     (string->ascii-art <string>)" << FCN_ERR("string->ascii-art",args));
    return make_str(heist_fmt::convert_to_ascii_art(*args[0].str));
  }

  data primitive_STRING_TO_SPACE_ART(scm_list& args) { 
    if(args.size() != 1 || !args[0].is_type(types::str))
      THROW_ERR("'string->space-art didn't receive 1 string arg!"
        "\n     (string->space-art <string>)\n     => <nansi> must be inactive!" 
        << FCN_ERR("string->space-art",args));
    if(!G::USING_ANSI_ESCAPE_SEQUENCES)
      THROW_ERR("'string->space-art can't be used with <nansi> active!"
        "\n     (string->space-art <string>)\n     => <nansi> must be inactive!" 
        << FCN_ERR("string->space-art",args));
    return make_str(heist_fmt::convert_to_whitespace_art(*args[0].str));
  }

  /******************************************************************************
  * EVALUATE PRIMITIVES DEFINED DIRECTLY IN HEIST SCHEME
  ******************************************************************************/

  void evaluate_primitives_written_in_heist_scheme() {
    FILE* ins = std::fopen(HEIST_DIRECTORY_FILE_PATH "/heist_interpreter_headers/heist_primitives.scm", "r");
    scm_string heist_prim_exp;
    char buffer[1001];
    while(std::fgets(buffer,1000,ins))
      heist_prim_exp += buffer;
    std::fclose(ins);
    scm_list heist_scheme_prim_defns;
    parse_input_exp(std::move(heist_prim_exp), heist_scheme_prim_defns);
    for(auto& primitive_scm_prim_defn : heist_scheme_prim_defns)
      scm_eval(std::move(primitive_scm_prim_defn.exp),G::GLOBAL_ENVIRONMENT_POINTER);
  }

  /******************************************************************************
  * REGISTRY OF PRIMITIVES ALSO REQUIRING AN ENVIRONMENT (TO APPLY A PROCEDURE)
  ******************************************************************************/

  constexpr const prm_ptr_t PRIMITIVES_REQUIRING_CURRENT_ENVIRONMENT[] = {
    primitive_EVAL,            primitive_CPS_EVAL,
    primitive_LOAD,            primitive_CPS_LOAD, 
    primitive_INLINE,          primitive_CALL_CE, 
    primitive_GETENV,          primitive_EXPAND,
    primitive_RUNTIME_SYNTAXP, primitive_SET_RUNTIME_SYNTAX_BANG, 
  };

#ifndef HEIST_CPP_INTEROP_HPP_ // @NOT-EMBEDDED-IN-C++
  constexpr bool primitive_requires_environment(const prm_ptr_t& prm)noexcept{
    for(const auto& p : PRIMITIVES_REQUIRING_CURRENT_ENVIRONMENT)
      if(p == prm) return true;
    return false;
  }
#else // @EMBEDDED-IN-C++
  namespace G { std::vector<prm_ptr_t> USER_DEFINED_PRIMITIVES_REQUIRING_ENV; }
  bool primitive_requires_environment(const prm_ptr_t& prm)noexcept{
    for(const auto& p : PRIMITIVES_REQUIRING_CURRENT_ENVIRONMENT)
      if(p == prm) return true;
    for(const auto& p : G::USER_DEFINED_PRIMITIVES_REQUIRING_ENV)
      if(p == prm) return true;
    return false;
  }
#endif

  /******************************************************************************
  * PRIMITIVE NAMES & OBJECTS AS FRAME VARS & VALS FOR THE GLOBAL ENVIRONMENT
  ******************************************************************************/

  constexpr const std::pair<prm_ptr_t,const char*>primitive_procedure_declarations[]={
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
    std::make_pair(primitive_DIVMOD,    "divmod"),
    std::make_pair(primitive_MODULO,    "modulo"),
    std::make_pair(primitive_EXP,       "exp"),
    std::make_pair(primitive_LOG,       "log"),
    std::make_pair(primitive_SQRT,      "sqrt"),
    std::make_pair(primitive_GCD,       "gcd"),
    std::make_pair(primitive_LCM,       "lcm"),
    std::make_pair(primitive_MODF,      "modf"),
    std::make_pair(primitive_NPR,       "npr"),
    std::make_pair(primitive_NCR,       "ncr"),

    std::make_pair(primitive_MAKE_POLAR,       "make-polar"),
    std::make_pair(primitive_MAKE_RECTANGULAR, "make-rectangular"),
    std::make_pair(primitive_REAL_PART,        "real-part"),
    std::make_pair(primitive_IMAG_PART,        "imag-part"),
    std::make_pair(primitive_MAGNITUDE,        "magnitude"),
    std::make_pair(primitive_ANGLE,            "angle"),
    std::make_pair(primitive_CONJUGATE,        "conjugate"),

    std::make_pair(primitive_ODDP,          "odd?"),
    std::make_pair(primitive_EVENP,         "even?"),
    std::make_pair(primitive_POSITIVEP,     "positive?"),
    std::make_pair(primitive_NEGATIVEP,     "negative?"),
    std::make_pair(primitive_ZEROP,         "zero?"),
    std::make_pair(primitive_NOT_POSITIVEP, "not-positive?"),
    std::make_pair(primitive_NOT_NEGATIVEP, "not-negative?"),
    std::make_pair(primitive_NOT_ZEROP,     "not-zero?"),
    std::make_pair(primitive_INFINITEP,     "infinite?"),
    std::make_pair(primitive_FINITEP,       "finite?"),
    std::make_pair(primitive_NANP,          "nan?"),

    std::make_pair(primitive_CEILING,  "ceiling"),
    std::make_pair(primitive_FLOOR,    "floor"),
    std::make_pair(primitive_TRUNCATE, "truncate"),
    std::make_pair(primitive_ROUND,    "round"),

    std::make_pair(primitive_COERCE_INEXACT_TO_EXACT, "inexact->exact"),
    std::make_pair(primitive_COERCE_EXACT_TO_INEXACT, "exact->inexact"),
    std::make_pair(primitive_EXACTP,                  "exact?"),
    std::make_pair(primitive_INEXACTP,                "inexact?"),
    std::make_pair(primitive_INTEGERP,                "integer?"),
    std::make_pair(primitive_BIGINTP,                 "bigint?"),
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

    std::make_pair(primitive_EOF,                "eof"),
    std::make_pair(primitive_CHAR_EQ,            "char=?"),
    std::make_pair(primitive_CHAR_LT,            "char<?"),
    std::make_pair(primitive_CHAR_GT,            "char>?"),
    std::make_pair(primitive_CHAR_LTE,           "char<=?"),
    std::make_pair(primitive_CHAR_GTE,           "char>=?"),
    std::make_pair(primitive_CHAR_CI_EQ,         "char-ci=?"),
    std::make_pair(primitive_CHAR_CI_LT,         "char-ci<?"),
    std::make_pair(primitive_CHAR_CI_GT,         "char-ci>?"),
    std::make_pair(primitive_CHAR_CI_LTE,        "char-ci<=?"),
    std::make_pair(primitive_CHAR_CI_GTE,        "char-ci>=?"),
    std::make_pair(primitive_CHAR_ALPHABETICP,   "char-alphabetic?"),
    std::make_pair(primitive_CHAR_NUMERICP,      "char-numeric?"),
    std::make_pair(primitive_CHAR_WHITESPACEP,   "char-whitespace?"),
    std::make_pair(primitive_CHAR_UPPER_CASEP,   "char-upper-case?"),
    std::make_pair(primitive_CHAR_LOWER_CASEP,   "char-lower-case?"),
    std::make_pair(primitive_CHAR_ALHPANUMERICP, "char-alphanumeric?"),
    std::make_pair(primitive_CHAR_CONTROLP,      "char-control?"),
    std::make_pair(primitive_CHAR_PRINTP,        "char-print?"),
    std::make_pair(primitive_CHAR_GRAPHP,        "char-graph?"),
    std::make_pair(primitive_CHAR_PUNCTUATIONP,  "char-punctuation?"),
    std::make_pair(primitive_CHAR_XDIGITP,       "char-xdigit?"),
    std::make_pair(primitive_CHAR_UPCASE,        "char-upcase"),
    std::make_pair(primitive_CHAR_DOWNCASE,      "char-downcase"),

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

    std::make_pair(primitive_TYPEOF,       "typeof"),
    std::make_pair(primitive_COPY,         "copy"),
    std::make_pair(primitive_SHALLOW_COPY, "shallow-copy"),

    std::make_pair(primitive_HMAP,             "hmap"),
    std::make_pair(primitive_HMAP_KEYS,        "hmap-keys"),
    std::make_pair(primitive_HMAP_VALS,        "hmap-vals"),
    std::make_pair(primitive_HMAP_KEYP,        "hmap-key?"),
    std::make_pair(primitive_HMAP_HASHABLEP,   "hmap-hashable?"),
    std::make_pair(primitive_HMAP_REF,         "hmap-ref"),
    std::make_pair(primitive_HMAP_SET_BANG,    "hmap-set!"),
    std::make_pair(primitive_HMAP_DELETE_BANG, "hmap-delete!"),
    std::make_pair(primitive_HMAP_LENGTH,      "hmap-length"),
    std::make_pair(primitive_HMAP_EMPTYP,      "hmap-empty?"),
    std::make_pair(primitive_HMAP_MERGE,       "hmap-merge"),
    std::make_pair(primitive_HMAP_MERGE_BANG,  "hmap-merge!"),
    std::make_pair(primitive_HMAP_MAP_BANG,    "hmap-map!"),
    std::make_pair(primitive_HMAP_MAP,         "hmap-map"),
    std::make_pair(primitive_HMAP_FOR_EACH,    "hmap-for-each"),
    std::make_pair(primitive_HMAP_FOR_EACH_KEY,"hmap-for-each-key"),
    std::make_pair(primitive_HMAP_FOR_EACH_VAL,"hmap-for-each-val"),
    std::make_pair(primitive_HMAP_TO_ALIST,    "hmap->alist"),
    std::make_pair(primitive_ALIST_TO_HMAP,    "alist->hmap"),

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

    std::make_pair(primitive_LIST,                 "list"),
    std::make_pair(primitive_LIST_STAR,            "list*"),
    std::make_pair(primitive_MAKE_LIST,            "make-list"),
    std::make_pair(primitive_IOTA,                 "iota"),
    std::make_pair(primitive_CIRCULAR_LIST,        "circular-list"),
    std::make_pair(primitive_CIRCULAR_LISTP,       "circular-list?"),
    std::make_pair(primitive_LIST_STARP,           "list*?"),
    std::make_pair(primitive_LISTP,                "list?"),
    std::make_pair(primitive_ALISTP,               "alist?"),
    std::make_pair(primitive_LAST_PAIR,            "last-pair"),
    std::make_pair(primitive_UNFOLD,               "unfold"),
    std::make_pair(primitive_UNFOLD_RIGHT,         "unfold-right"),
    std::make_pair(primitive_GET_ALL_COMBINATIONS, "get-all-combinations"),

    std::make_pair(primitive_MEMQ,   "memq"),
    std::make_pair(primitive_MEMV,   "memv"),
    std::make_pair(primitive_MEMBER, "member"),
    std::make_pair(primitive_ASSQ,   "assq"),
    std::make_pair(primitive_ASSV,   "assv"),
    std::make_pair(primitive_ASSOC,  "assoc"),

    std::make_pair(primitive_VECTOR,                      "vector"),
    std::make_pair(primitive_MAKE_VECTOR,                 "make-vector"),
    std::make_pair(primitive_VECTOR_PUSH_BANG,            "vector-push!"),
    std::make_pair(primitive_VECTOR_IOTA,                 "vector-iota"),
    std::make_pair(primitive_VECTOR_EMPTYP,               "vector-empty?"),
    std::make_pair(primitive_VECTOR_GROW,                 "vector-grow"),
    std::make_pair(primitive_VECTOR_UNFOLD,               "vector-unfold"),
    std::make_pair(primitive_VECTOR_UNFOLD_RIGHT,         "vector-unfold-right"),
    std::make_pair(primitive_VECTOR_COPY_BANG,            "vector-copy!"),
    std::make_pair(primitive_VECTOR_SWAP_BANG,            "vector-swap!"),
    std::make_pair(primitive_VECTOR_BINARY_SEARCH,        "vector-binary-search"),
    std::make_pair(primitive_VECTOR_GET_ALL_COMBINATIONS, "vector-get-all-combinations"),

    std::make_pair(primitive_EMPTY,                "empty"),
    std::make_pair(primitive_LENGTH,               "length"),
    std::make_pair(primitive_LENGTH_PLUS,          "length+"),
    std::make_pair(primitive_REVERSE,              "reverse"),
    std::make_pair(primitive_REVERSE_BANG,         "reverse!"),
    std::make_pair(primitive_FOLD,                 "fold"),
    std::make_pair(primitive_FOLD_RIGHT,           "fold-right"),
    std::make_pair(primitive_FILTER,               "filter"),
    std::make_pair(primitive_MAP,                  "map"),
    std::make_pair(primitive_MAP_BANG,             "map!"),
    std::make_pair(primitive_FOR_EACH,             "for-each"),
    std::make_pair(primitive_SEQ_COPY_BANG,        "seq-copy!"),
    std::make_pair(primitive_COUNT,                "count"),
    std::make_pair(primitive_REF,                  "ref"),
    std::make_pair(primitive_SLICE,                "slice"),
    std::make_pair(primitive_SET_INDEX_BANG,       "set-index!"),
    std::make_pair(primitive_SWAP_INDICES_BANG,    "swap-indices!"),
    std::make_pair(primitive_FILL_BANG,            "fill!"),
    std::make_pair(primitive_APPEND,               "append"),
    std::make_pair(primitive_REMOVE,               "remove"),
    std::make_pair(primitive_REMOVE_FIRST,         "remove-first"),
    std::make_pair(primitive_REMOVE_LAST,          "remove-last"),
    std::make_pair(primitive_DELETE,               "delete"),
    std::make_pair(primitive_LAST,                 "last"),
    std::make_pair(primitive_TAIL,                 "tail"),
    std::make_pair(primitive_HEAD,                 "head"),
    std::make_pair(primitive_INIT,                 "init"),
    std::make_pair(primitive_SEQ_EQ,               "seq="),
    std::make_pair(primitive_SKIP,                 "skip"),
    std::make_pair(primitive_SKIP_RIGHT,           "skip-right"),
    std::make_pair(primitive_INDEX,                "index"),
    std::make_pair(primitive_INDEX_RIGHT,          "index-right"),
    std::make_pair(primitive_DROP,                 "drop"),
    std::make_pair(primitive_DROP_RIGHT,           "drop-right"),
    std::make_pair(primitive_TAKE,                 "take"),
    std::make_pair(primitive_TAKE_RIGHT,           "take-right"),
    std::make_pair(primitive_TAKE_WHILE,           "take-while"),
    std::make_pair(primitive_TAKE_RIGHT_WHILE,     "take-right-while"),
    std::make_pair(primitive_DROP_WHILE,           "drop-while"),
    std::make_pair(primitive_DROP_RIGHT_WHILE,     "drop-right-while"),
    std::make_pair(primitive_ANY,                  "any"),
    std::make_pair(primitive_EVERY,                "every"),
    std::make_pair(primitive_CONJ,                 "conj"),
    std::make_pair(primitive_UNION,                "union"),
    std::make_pair(primitive_INTERSECTION,         "intersection"),
    std::make_pair(primitive_SYMMETRIC_DIFFERENCE, "symmetric-difference"),
    std::make_pair(primitive_DIFFERENCE,           "difference"),

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
    std::make_pair(primitive_HMAPP,                "hmap?"),
    std::make_pair(primitive_EMPTYP,               "empty?"),
    std::make_pair(primitive_PAIRP,                "pair?"),
    std::make_pair(primitive_VECTORP,              "vector?"),
    std::make_pair(primitive_CHARP,                "char?"),
    std::make_pair(primitive_NUMBERP,              "number?"),
    std::make_pair(primitive_REALP,                "real?"),
    std::make_pair(primitive_COMPLEXP,             "complex?"),
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
    std::make_pair(primitive_OBJECTP,              "object?"),
    std::make_pair(primitive_CLASS_PROTOTYPEP,     "class-prototype?"),
    std::make_pair(primitive_FUNCTORP,             "functor?"),
    std::make_pair(primitive_CALLABLEP,            "callable?"),

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

    std::make_pair(primitive_COERCE_CHAR_TO_INTEGER,  "char->integer"),
    std::make_pair(primitive_COERCE_INTEGER_TO_CHAR,  "integer->char"),
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
    std::make_pair(primitive_FUNCTOR_TO_PROCEDURE,    "functor->procedure"),

    std::make_pair(primitive_PPRINT,     "pprint"),
    std::make_pair(primitive_PPRINT,     "pretty-print"),
    std::make_pair(primitive_WRITE,      "write"),
    std::make_pair(primitive_DISPLAY,    "display"),
    std::make_pair(primitive_NEWLINE,    "newline"),
    std::make_pair(primitive_WRITE_CHAR, "write-char"),

    std::make_pair(primitive_SPRINTF,  "sprintf"),
    std::make_pair(primitive_DISPLAYF, "displayf"),
    std::make_pair(primitive_WRITEF,   "writef"),
    std::make_pair(primitive_PPRINTF,  "pprintf"),
    std::make_pair(primitive_PPRINTF,  "pretty-printf"),

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
    std::make_pair(primitive_OPEN_OUTPUT_FILE_PLUS, "open-output-file+"),
    std::make_pair(primitive_OPEN_OUTPUT_FILE_BANG, "open-output-file!"),
    std::make_pair(primitive_CLOSE_PORT,            "close-port"),

    std::make_pair(primitive_LOAD,         "load"),
    std::make_pair(primitive_CPS_LOAD,     "heist:core:pass-continuation-cps-load"),
    std::make_pair(primitive_SYSTEM,       "system"),
    std::make_pair(primitive_GETENV,       "getenv"),
    std::make_pair(primitive_COMMAND_LINE, "command-line"),
    std::make_pair(primitive_COMPILE,      "compile"),
    std::make_pair(primitive_CPS_COMPILE,  "cps-compile"),
    std::make_pair(primitive_GETCWD,       "getcwd"),
    std::make_pair(primitive_DIRNAME,      "dirname"),
    
    std::make_pair(primitive_SECONDS_SINCE_EPOCH, "seconds-since-epoch"),
    std::make_pair(primitive_TIME,                "time"),
    std::make_pair(primitive_CURRENT_DATE,        "current-date"),

    std::make_pair(primitive_SET_NANSI,                    "set-nansi!"),
    std::make_pair(primitive_NANSIP,                       "nansi?"),
    std::make_pair(primitive_SET_CI,                       "set-ci!"),
    std::make_pair(primitive_CIP,                          "ci?"),
    std::make_pair(primitive_SET_PPRINT_COLUMN_WIDTH_BANG, "set-pprint-column-width!"),
    std::make_pair(primitive_PPRINT_COLUMN_WIDTH,          "pprint-column-width"),
    std::make_pair(primitive_SET_MAX_RECURSION_DEPTH_BANG, "set-max-recursion-depth!"),
    std::make_pair(primitive_MAX_RECURSION_DEPTH,          "max-recursion-depth"),
    std::make_pair(primitive_SET_REPL_PROMPT_BANG,         "set-repl-prompt!"),
    std::make_pair(primitive_REPL_PROMPT,                  "repl-prompt"),
    std::make_pair(primitive_SET_DYNAMIC_CALL_TRACE_BANG,  "set-dynamic-call-trace!"),
    std::make_pair(primitive_DYNAMIC_CALL_TRACEP,          "dynamic-call-trace?"),
    std::make_pair(primitive_SET_TRACE_ARGS_BANG,          "set-trace-args!"),
    std::make_pair(primitive_TRACE_ARGSP,                  "trace-args?"),
    std::make_pair(primitive_SET_DOT_BANG,                 "set-dot!"),
    std::make_pair(primitive_DOT,                          "dot"),

    std::make_pair(primitive_EXIT,         "exit"),
    std::make_pair(primitive_ERROR,        "error"),
    std::make_pair(primitive_SYNTAX_ERROR, "syntax-error"),
    std::make_pair(primitive_CALL_CE,      "call/ce"),
    std::make_pair(primitive_CALL_CE,      "call-with-current-environment"),
    std::make_pair(primitive_INLINE,       "inline"),
    std::make_pair(primitive_JUMP_BANG,    "jump!"),
    std::make_pair(primitive_CATCH_JUMP,   "catch-jump"),
    std::make_pair(primitive_EXPAND,       "expand"),
    std::make_pair(primitive_TRACE,        "trace"),

    std::make_pair(primitive_GENSYM,      "gensym"),
    std::make_pair(primitive_SOWN_GENSYM, "sown-gensym"),

    std::make_pair(primitive_CORE_SYNTAXP,    "core-syntax?"),
    std::make_pair(primitive_RUNTIME_SYNTAXP, "runtime-syntax?"),
    std::make_pair(primitive_READER_SYNTAXP,  "reader-syntax?"),
    std::make_pair(primitive_READER_ALIASP,   "reader-alias?"),

    std::make_pair(primitive_SET_CORE_SYNTAX_BANG,    "set-core-syntax!"),
    std::make_pair(primitive_SET_RUNTIME_SYNTAX_BANG, "set-runtime-syntax!"),

    std::make_pair(primitive_DEFINE_READER_SYNTAX, "define-reader-syntax"),
    std::make_pair(primitive_READER_SYNTAX_LIST,   "reader-syntax-list"),
    std::make_pair(primitive_READER_ALIAS_LIST,    "reader-alias-list"),

    std::make_pair(primitive_INFIX_LIST,  "infix-list"),

    std::make_pair(primitive_JSON_TO_SCM, "json->scm"),
    std::make_pair(primitive_SCM_TO_JSON, "scm->json"),
    std::make_pair(primitive_JSON_DATUMP, "json-datum?"),

    std::make_pair(primitive_REGEX_REPLACE,     "regex-replace"),
    std::make_pair(primitive_REGEX_REPLACE_ALL, "regex-replace-all"),
    std::make_pair(primitive_REGEX_MATCH,       "regex-match"),
    std::make_pair(primitive_REGEX_SPLIT,       "regex-split"),

    std::make_pair(primitive_HEIST_CORE_OO_SET_MEMBER_BANG, "heist:core:oo:set-member!"),
    std::make_pair(primitive_HEIST_CORE_OO_COMPARE_CLASSES, "heist:core:oo:compare-classes"),
    std::make_pair(primitive_HEIST_CORE_OO_MAKE_OBJECT,     "heist:core:oo:make-object"),
    std::make_pair(primitive_HEIST_CORE_OO_REGISTER_MEMBER, "heist:core:oo:register-member!"),
    std::make_pair(primitive_HEIST_CORE_OO_REGISTER_METHOD, "heist:core:oo:register-method!"),

    std::make_pair(primitive_OBJECT_MEMBERS,              "object-members"),
    std::make_pair(primitive_OBJECT_METHODS,              "object-methods"),
    std::make_pair(primitive_HEIST_CORE_OO_MEMBER_ACCESS, ".."),
    std::make_pair(primitive_OBJECT_TO_HMAP,              "object->hmap"),
    std::make_pair(primitive_OBJECT_TO_ALIST,             "object->alist"),
    std::make_pair(primitive_OBJECT_TO_JSON,              "object->json"),

    std::make_pair(primitive_PROTO_NAME,            "proto-name"),
    std::make_pair(primitive_PROTO_MEMBERS,         "proto-members"),
    std::make_pair(primitive_PROTO_METHODS,         "proto-methods"),
    std::make_pair(primitive_PROTO_SUPER,           "proto-super"),
    std::make_pair(primitive_PROTO_ADD_MEMBER_BANG, "proto-add-member!"),
    std::make_pair(primitive_PROTO_ADD_METHOD_BANG, "proto-add-method!"),

    std::make_pair(primitive_CYCLE_COROUTINES_BANG, "cycle-coroutines!"),

    std::make_pair(primitive_fmt_reset,"fmt:reset"),
    std::make_pair(primitive_fmt_clear,"fmt:clear"),
    std::make_pair(primitive_fmt_bold, "fmt:bold"),
    std::make_pair(primitive_fmt_line, "fmt:line"),
    std::make_pair(primitive_fmt_rev,  "fmt:rev"),

    std::make_pair(primitive_fmt_black,   "fmt:black"),
    std::make_pair(primitive_fmt_red,     "fmt:red"),
    std::make_pair(primitive_fmt_green,   "fmt:green"),
    std::make_pair(primitive_fmt_yellow,  "fmt:yellow"),
    std::make_pair(primitive_fmt_blue,    "fmt:blue"),
    std::make_pair(primitive_fmt_magenta, "fmt:magenta"),
    std::make_pair(primitive_fmt_cyan,    "fmt:cyan"),
    std::make_pair(primitive_fmt_white,   "fmt:white"),
    std::make_pair(primitive_fmt_black1,  "fmt:black1"),  std::make_pair(primitive_fmt_black2,  "fmt:black2"),
    std::make_pair(primitive_fmt_black3,  "fmt:black3"),  std::make_pair(primitive_fmt_black4,  "fmt:black4"),
    std::make_pair(primitive_fmt_black5,  "fmt:black5"),  std::make_pair(primitive_fmt_black6,  "fmt:black6"),
    std::make_pair(primitive_fmt_black7,  "fmt:black7"),  std::make_pair(primitive_fmt_black8,  "fmt:black8"),
    std::make_pair(primitive_fmt_red1,    "fmt:red1"),    std::make_pair(primitive_fmt_red2,    "fmt:red2"),
    std::make_pair(primitive_fmt_red3,    "fmt:red3"),    std::make_pair(primitive_fmt_red4,    "fmt:red4"),
    std::make_pair(primitive_fmt_red5,    "fmt:red5"),    std::make_pair(primitive_fmt_red6,    "fmt:red6"),
    std::make_pair(primitive_fmt_red7,    "fmt:red7"),    std::make_pair(primitive_fmt_red8,    "fmt:red8"),
    std::make_pair(primitive_fmt_green1,  "fmt:green1"),  std::make_pair(primitive_fmt_green2,  "fmt:green2"),
    std::make_pair(primitive_fmt_green3,  "fmt:green3"),  std::make_pair(primitive_fmt_green4,  "fmt:green4"),
    std::make_pair(primitive_fmt_green5,  "fmt:green5"),  std::make_pair(primitive_fmt_green6,  "fmt:green6"),
    std::make_pair(primitive_fmt_green7,  "fmt:green7"),  std::make_pair(primitive_fmt_green8,  "fmt:green8"),
    std::make_pair(primitive_fmt_yellow1, "fmt:yellow1"), std::make_pair(primitive_fmt_yellow2, "fmt:yellow2"),
    std::make_pair(primitive_fmt_yellow3, "fmt:yellow3"), std::make_pair(primitive_fmt_yellow4, "fmt:yellow4"),
    std::make_pair(primitive_fmt_yellow5, "fmt:yellow5"), std::make_pair(primitive_fmt_yellow6, "fmt:yellow6"),
    std::make_pair(primitive_fmt_yellow7, "fmt:yellow7"), std::make_pair(primitive_fmt_yellow8, "fmt:yellow8"),
    std::make_pair(primitive_fmt_blue1,   "fmt:blue1"),   std::make_pair(primitive_fmt_blue2,   "fmt:blue2"),
    std::make_pair(primitive_fmt_blue3,   "fmt:blue3"),   std::make_pair(primitive_fmt_blue4,   "fmt:blue4"),
    std::make_pair(primitive_fmt_blue5,   "fmt:blue5"),   std::make_pair(primitive_fmt_blue6,   "fmt:blue6"),
    std::make_pair(primitive_fmt_blue7,   "fmt:blue7"),   std::make_pair(primitive_fmt_blue8,   "fmt:blue8"),
    std::make_pair(primitive_fmt_magenta1,"fmt:magenta1"),std::make_pair(primitive_fmt_magenta2,"fmt:magenta2"),
    std::make_pair(primitive_fmt_magenta3,"fmt:magenta3"),std::make_pair(primitive_fmt_magenta4,"fmt:magenta4"),
    std::make_pair(primitive_fmt_magenta5,"fmt:magenta5"),std::make_pair(primitive_fmt_magenta6,"fmt:magenta6"),
    std::make_pair(primitive_fmt_magenta7,"fmt:magenta7"),std::make_pair(primitive_fmt_magenta8,"fmt:magenta8"),
    std::make_pair(primitive_fmt_cyan1,   "fmt:cyan1"),   std::make_pair(primitive_fmt_cyan2,   "fmt:cyan2"),
    std::make_pair(primitive_fmt_cyan3,   "fmt:cyan3"),   std::make_pair(primitive_fmt_cyan4,   "fmt:cyan4"),
    std::make_pair(primitive_fmt_cyan5,   "fmt:cyan5"),   std::make_pair(primitive_fmt_cyan6,   "fmt:cyan6"),
    std::make_pair(primitive_fmt_cyan7,   "fmt:cyan7"),   std::make_pair(primitive_fmt_cyan8,   "fmt:cyan8"),
    std::make_pair(primitive_fmt_white1,  "fmt:white1"),  std::make_pair(primitive_fmt_white2,  "fmt:white2"),
    std::make_pair(primitive_fmt_white3,  "fmt:white3"),  std::make_pair(primitive_fmt_white4,  "fmt:white4"),
    std::make_pair(primitive_fmt_white5,  "fmt:white5"),  std::make_pair(primitive_fmt_white6,  "fmt:white6"),
    std::make_pair(primitive_fmt_white7,  "fmt:white7"),  std::make_pair(primitive_fmt_white8,  "fmt:white8"),

    std::make_pair(primitive_fmt_bblack,   "fmt:bblack"),
    std::make_pair(primitive_fmt_bred,     "fmt:bred"),
    std::make_pair(primitive_fmt_bgreen,   "fmt:bgreen"),
    std::make_pair(primitive_fmt_byellow,  "fmt:byellow"),
    std::make_pair(primitive_fmt_bblue,    "fmt:bblue"),
    std::make_pair(primitive_fmt_bmagenta, "fmt:bmagenta"),
    std::make_pair(primitive_fmt_bcyan,    "fmt:bcyan"),
    std::make_pair(primitive_fmt_bwhite,   "fmt:bwhite"),
    std::make_pair(primitive_fmt_bblack1,  "fmt:bblack1"),  std::make_pair(primitive_fmt_bblack2,  "fmt:bblack2"),
    std::make_pair(primitive_fmt_bblack3,  "fmt:bblack3"),  std::make_pair(primitive_fmt_bblack4,  "fmt:bblack4"),
    std::make_pair(primitive_fmt_bblack5,  "fmt:bblack5"),  std::make_pair(primitive_fmt_bblack6,  "fmt:bblack6"),
    std::make_pair(primitive_fmt_bblack7,  "fmt:bblack7"),  std::make_pair(primitive_fmt_bblack8,  "fmt:bblack8"),
    std::make_pair(primitive_fmt_bred1,    "fmt:bred1"),    std::make_pair(primitive_fmt_bred2,    "fmt:bred2"),
    std::make_pair(primitive_fmt_bred3,    "fmt:bred3"),    std::make_pair(primitive_fmt_bred4,    "fmt:bred4"),
    std::make_pair(primitive_fmt_bred5,    "fmt:bred5"),    std::make_pair(primitive_fmt_bred6,    "fmt:bred6"),
    std::make_pair(primitive_fmt_bred7,    "fmt:bred7"),    std::make_pair(primitive_fmt_bred8,    "fmt:bred8"),
    std::make_pair(primitive_fmt_bgreen1,  "fmt:bgreen1"),  std::make_pair(primitive_fmt_bgreen2,  "fmt:bgreen2"),
    std::make_pair(primitive_fmt_bgreen3,  "fmt:bgreen3"),  std::make_pair(primitive_fmt_bgreen4,  "fmt:bgreen4"),
    std::make_pair(primitive_fmt_bgreen5,  "fmt:bgreen5"),  std::make_pair(primitive_fmt_bgreen6,  "fmt:bgreen6"),
    std::make_pair(primitive_fmt_bgreen7,  "fmt:bgreen7"),  std::make_pair(primitive_fmt_bgreen8,  "fmt:bgreen8"),
    std::make_pair(primitive_fmt_byellow1, "fmt:byellow1"), std::make_pair(primitive_fmt_byellow2, "fmt:byellow2"),
    std::make_pair(primitive_fmt_byellow3, "fmt:byellow3"), std::make_pair(primitive_fmt_byellow4, "fmt:byellow4"),
    std::make_pair(primitive_fmt_byellow5, "fmt:byellow5"), std::make_pair(primitive_fmt_byellow6, "fmt:byellow6"),
    std::make_pair(primitive_fmt_byellow7, "fmt:byellow7"), std::make_pair(primitive_fmt_byellow8, "fmt:byellow8"),
    std::make_pair(primitive_fmt_bblue1,   "fmt:bblue1"),   std::make_pair(primitive_fmt_bblue2,   "fmt:bblue2"),
    std::make_pair(primitive_fmt_bblue3,   "fmt:bblue3"),   std::make_pair(primitive_fmt_bblue4,   "fmt:bblue4"),
    std::make_pair(primitive_fmt_bblue5,   "fmt:bblue5"),   std::make_pair(primitive_fmt_bblue6,   "fmt:bblue6"),
    std::make_pair(primitive_fmt_bblue7,   "fmt:bblue7"),   std::make_pair(primitive_fmt_bblue8,   "fmt:bblue8"),
    std::make_pair(primitive_fmt_bmagenta1,"fmt:bmagenta1"),std::make_pair(primitive_fmt_bmagenta2,"fmt:bmagenta2"),
    std::make_pair(primitive_fmt_bmagenta3,"fmt:bmagenta3"),std::make_pair(primitive_fmt_bmagenta4,"fmt:bmagenta4"),
    std::make_pair(primitive_fmt_bmagenta5,"fmt:bmagenta5"),std::make_pair(primitive_fmt_bmagenta6,"fmt:bmagenta6"),
    std::make_pair(primitive_fmt_bmagenta7,"fmt:bmagenta7"),std::make_pair(primitive_fmt_bmagenta8,"fmt:bmagenta8"),
    std::make_pair(primitive_fmt_bcyan1,   "fmt:bcyan1"),   std::make_pair(primitive_fmt_bcyan2,   "fmt:bcyan2"),
    std::make_pair(primitive_fmt_bcyan3,   "fmt:bcyan3"),   std::make_pair(primitive_fmt_bcyan4,   "fmt:bcyan4"),
    std::make_pair(primitive_fmt_bcyan5,   "fmt:bcyan5"),   std::make_pair(primitive_fmt_bcyan6,   "fmt:bcyan6"),
    std::make_pair(primitive_fmt_bcyan7,   "fmt:bcyan7"),   std::make_pair(primitive_fmt_bcyan8,   "fmt:bcyan8"),
    std::make_pair(primitive_fmt_bwhite1,  "fmt:bwhite1"),  std::make_pair(primitive_fmt_bwhite2,  "fmt:bwhite2"),
    std::make_pair(primitive_fmt_bwhite3,  "fmt:bwhite3"),  std::make_pair(primitive_fmt_bwhite4,  "fmt:bwhite4"),
    std::make_pair(primitive_fmt_bwhite5,  "fmt:bwhite5"),  std::make_pair(primitive_fmt_bwhite6,  "fmt:bwhite6"),
    std::make_pair(primitive_fmt_bwhite7,  "fmt:bwhite7"),  std::make_pair(primitive_fmt_bwhite8,  "fmt:bwhite8"),

    std::make_pair(primitive_STRING_TO_ASCII_ART,"string->ascii-art"),
    std::make_pair(primitive_STRING_TO_SPACE_ART,"string->space-art"),
  };

  frame_vals primitive_procedure_objects()noexcept{
    constexpr const auto n = sizeof(primitive_procedure_declarations) / sizeof(primitive_procedure_declarations[0]);
    frame_vals primitive_procedures(n);
    for(size_type i = 0; i < n; ++i)
      primitive_procedures[i] = scm_fcn(primitive_procedure_declarations[i].second,primitive_procedure_declarations[i].first);
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