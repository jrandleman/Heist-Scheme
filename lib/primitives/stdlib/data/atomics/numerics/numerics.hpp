// Author: Jordan Randleman -- jordanran199@gmail.com -- numerics.hpp
// => Defines primitive numeric functions written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_NUMERICS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_NUMERICS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * ARITHMETIC PRIMITIVES
  ******************************************************************************/

  // primitive "+" procedure
  data primitive_ADD(data_vector&& args) {
    stdlib_numerics::confirm_only_numbers_and_at_least_one_arg(args, "+", "(+ <num1> <num2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_ADD,args);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      args[0].num += args[i].num;
    return args[0];
  }

  // primitive "-" procedure: BOTH NEGATION & SUBTRACTION
  data primitive_SUB(data_vector&& args) {
    stdlib_numerics::confirm_only_numbers_and_at_least_one_arg(args, "-", "(- <num1> <num2> ...)");
    if(args.size()==1) return -args[0].num;           // negation
    for(size_type i = 1, n = args.size(); i < n; ++i) // subtraction
      args[0].num -= args[i].num;
    return args[0];
  }

  // primitive "*" procedure:
  data primitive_MUL(data_vector&& args) {
    stdlib_numerics::confirm_only_numbers_and_at_least_one_arg(args, "*", "(* <num1> <num2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MUL,args);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      args[0].num *= args[i].num;
    return args[0];
  }

  // primitive "/" procedure:
  data primitive_DIV(data_vector&& args) {
    stdlib_numerics::confirm_only_numbers_and_at_least_one_arg(args, "/", "(/ <num1> <num2> ...)");
    if(args.size()==1) return 1 / args[0].num;
    for(size_type i = 1, n = args.size(); i < n; ++i)
      args[0].num /= args[i].num;
    return args[0];
  }

  /******************************************************************************
  * MISCELLANEOUS NUMERIC PRIMITIVE OPERATIONS 
  ******************************************************************************/

  // primitive "abs" procedure
  data primitive_ABS(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "abs", "(abs <real>)");
    return args[0].num.abs();
  }

  // primitive "expt" procedure
  data primitive_EXPT(data_vector&& args) {
    stdlib_numerics::confirm_only_numbers_and_at_least_one_arg(args, "expt", "(expt <num1> <num2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_EXPT,args);
    num_type pow(std::move(args.rbegin()->num));
    for(size_type i = args.size()-1; i-- > 0;)
      pow = args[i].num.expt(pow);
    return pow;
  }

  // primitive "expt-mod" procedure [more efficient (modulo (expt x y) z)]
  data primitive_EXPT_MOD(data_vector&& args) {
    // Confirm valid arguments
    static constexpr const char * const format = "\n     (expt-mod <real1> <real2> <real3>)";
    if(args.empty() || args.size() > 3)
      HEIST_THROW_ERR("'expt-mod invalid # of args given!" << format << HEIST_FCN_ERR("expt-mod",args));
    for(size_type i = 0, n = args.size(); i < n; ++i)
      if(!args[i].is_type(types::num) || !args[i].num.is_integer() || args[i].num.is_neg())
        HEIST_THROW_ERR("'expt-mod arg #" << i+1 << ' ' << HEIST_PROFILE(args[i]) << " isn't a non-negative integer!" 
          << format << HEIST_FCN_ERR("expt-mod",args));
    if(args.size() < 3) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_EXPT_MOD,args);
    // Perform Repeated Squares
    const auto &x = args[0].num, &z = args[2].num;
    const auto y_bitstr = args[1].num.to_exact().str(2);
    num_type f("1");
    // For each bit, from left to right
    for(size_type i = 0, n = y_bitstr.size(); i < n; ++i) {
      f = (f * f).modulo(z);
      if(y_bitstr[i] & 1) f = (f * x).modulo(z); // if ith bit is 1
    }
    return f;
  }

  // primitive "max" procedure
  data primitive_MAX(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "max", "(max <real1> <real2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MAX,args);
    num_type max = std::move(args[0].num);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      if(args[i].num > max)
        max = std::move(args[i].num);
    return max;
  }

  // primitive "min" procedure
  data primitive_MIN(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "min", "(min <real1> <real2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MIN,args);
    num_type min = std::move(args[0].num);
    for(size_type i = 1, n = args.size(); i < n; ++i)
      if(args[i].num < min)
        min = std::move(args[i].num);
    return min;
  }

  // primitive "quotient" procedure
  data primitive_QUOTIENT(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "quotient", "(quotient <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_QUOTIENT,args);
    stdlib_numerics::confirm_2_args(args, "quotient", "(quotient <real1> <real2>)");
    return args[0].num.quotient(args[1].num);
  }

  // primitive "remainder" procedure
  data primitive_REMAINDER(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "remainder", "(remainder <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_REMAINDER,args);
    stdlib_numerics::confirm_2_args(args, "remainder", "(remainder <real1> <real2>)");
    return args[0].num % args[1].num;
  }

  // primitive "divmod" procedure
  data primitive_DIVMOD(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "divmod", "(divmod <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_DIVMOD,args);
    stdlib_numerics::confirm_2_args(args, "divmod", "(divmod <real1> <real2>)");
    auto result = args[0].num.divmod(args[1].num);
    data divmod_pair = make_par();
    divmod_pair.par->first = std::move(result.first);
    divmod_pair.par->second = std::move(result.second);
    return divmod_pair;
  }

  // primitive "modulo" procedure
  data primitive_MODULO(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "modulo", "(modulo <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MODULO,args);
    stdlib_numerics::confirm_2_args(args, "modulo", "(modulo <real1> <real2>)");
    return args[0].num.modulo(args[1].num);
  }

  // primitive "exp" procedure
  data primitive_EXP(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "exp", "(exp <num>)");
    return args[0].num.exp();
  }

  // primitive "log" procedure -- NATURAL LOGARITHM
  data primitive_LOG(data_vector&& args) {
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR("'log didn't receive correct number of args!"
        << "\n     (log <num> <optional-base>)" << HEIST_FCN_ERR("log", args));
    if(!args[0].is_type(types::num))
      HEIST_THROW_ERR("'log 1st <num> arg "<<HEIST_PROFILE(args[0])<<" isn't a number!"
        << "\n     (log <num> <optional-base>)" << HEIST_FCN_ERR("log", args));
    // Perform natural log if only given one number
    if(args.size() == 1) return args[0].num.log();
    // Perform log base conversion
    if(!args[1].is_type(types::num))
      HEIST_THROW_ERR("'log 2nd <base> arg "<<HEIST_PROFILE(args[1])<<" isn't a number!"
        << "\n     (log <num> <optional-base>)" << HEIST_FCN_ERR("log", args));
    return args[0].num.log() / args[1].num.log();
  }

  // primitive "sqrt" procedure
  data primitive_SQRT(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "sqrt", "(sqrt <num>)");
    return args[0].num.sqrt();
  }

  // primitive "gcd" procedure
  data primitive_GCD(data_vector&& args) {
    if(args.empty()) return num_type();
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "gcd", "(gcd <real1> <real2> ...)");
    if(args.size() == 1) return args[0];
    // GCD is associative
    for(size_type i = 1, n = args.size(); i < n; ++i)
      args[0].num = args[0].num.gcd(args[i].num);
    return args[0].num;
  }

  // primitive "lcm" procedure
  data primitive_LCM(data_vector&& args) {
    if(args.empty()) return num_type("1");
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "lcm", "(lcm <real1> <real2> ...)");
    if(args.size() == 1) return args[0];
    // LCM is associative
    for(size_type i = 1, n = args.size(); i < n; ++i)
      args[0].num = args[0].num.lcm(args[i].num);
    return args[0].num;
  }

  // primitive "modf" procedure
  // (define (modf num) (cons (truncate num) (remainder num (truncate num))))
  data primitive_MODF(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "modf", "(modf <real>)");
    data num_pair = make_par();
    if(args[0].num.is_integer()) {
      num_pair.par->first  = std::move(args[0]);
      num_pair.par->second = num_type();
      return num_pair;
    }
    num_type::inexact_t integral;
    num_type::inexact_t fractional = std::modf(args[0].num.extract_inexact(), &integral);
    num_pair.par->first  = num_type(integral);
    num_pair.par->second = num_type(fractional);
    return num_pair;
  }

  //  primitive "npr": npr(n,r) = n! / (n - r)!
  data primitive_NPR(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "npr", "(npr <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_NPR,args);
    stdlib_numerics::confirm_2_args(args, "npr", "(npr <real1> <real2>)");
    auto n = args[0].num;
    return stdlib_numerics::factorial(std::move(args[0].num),num_type("1")) / 
           stdlib_numerics::factorial(n-args[1].num,num_type("1"));
  }

  //  primitive "ncr": ncr(n,r) = n! / (r! * (n - r)!)
  data primitive_NCR(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "ncr", "(ncr <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_NCR,args);
    stdlib_numerics::confirm_2_args(args, "ncr", "(ncr <real1> <real2>)");
    auto n_r_difference = args[0].num - args[1].num;
    return stdlib_numerics::factorial(std::move(args[0].num),num_type("1")) / 
           (stdlib_numerics::factorial(std::move(args[1].num),num_type("1")) * stdlib_numerics::factorial(std::move(n_r_difference),num_type("1")));
  }

  /******************************************************************************
  * COMPLEX NUMBER PRIMITIVES
  ******************************************************************************/

  // primitive "make-polar" procedure
  data primitive_MAKE_POLAR(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "make-polar", "(make-polar <real-mag> <real-ang>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MAKE_POLAR,args);
    stdlib_numerics::confirm_2_args(args, "make-polar", "(make-polar <real-mag> <real-ang>)");
    return num_type::make_polar(args[0].num,args[1].num);
  }

  // primitive "make-rectangular" procedure
  data primitive_MAKE_RECTANGULAR(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "make-rectangular", "(make-polar <real-real> <real-imag>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_MAKE_RECTANGULAR,args);
    stdlib_numerics::confirm_2_args(args, "make-rectangular", "(make-rectangular <real-real> <real-imag>)");
    return num_type::make_rectangular(args[0].num,args[1].num);
  }

  // primitive "real-part" procedure
  data primitive_REAL_PART(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "real-part", "(real-part <num>)");
    return args[0].num.real_part();
  }

  // primitive "imag-part" procedure
  data primitive_IMAG_PART(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "imag-part", "(imag-part <num>)");
    return args[0].num.imag_part();
  }

  // primitive "magnitude" procedure
  data primitive_MAGNITUDE(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "magnitude", "(magnitude <num>)");
    return args[0].num.magnitude();
  }

  // primitive "angle" procedure
  data primitive_ANGLE(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "angle", "(angle <num>)");
    return args[0].num.angle();
  }

  // primitive "conjugate" procedure
  data primitive_CONJUGATE(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "conjugate", "(conjugate <num>)");
    return args[0].num.conjugate();
  }

  /******************************************************************************
  * MISCELLANEOUS NUMERIC PREDICATE PRIMITIVES 
  ******************************************************************************/

  // primitive "odd?" procedure
  data primitive_ODDP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "odd?", "(odd? <integer>)");
    if(!args[0].num.is_integer())
      HEIST_THROW_ERR("'odd? " << HEIST_PROFILE(args[0]) << " isn't an integer!" 
        "\n     (odd? <integer>)" << HEIST_FCN_ERR("odd?", args));
    return boolean(args[0].num.is_odd());
  }

  // primitive "even?" procedure
  data primitive_EVENP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "even?", "(even? <integer>)");
    if(!args[0].num.is_integer())
      HEIST_THROW_ERR("'even? " << HEIST_PROFILE(args[0]) << " isn't an integer!" 
        "\n     (even? <integer>)" << HEIST_FCN_ERR("even?", args));
    return boolean(args[0].num.is_even());
  }

  // primitive "positive?" procedure
  data primitive_POSITIVEP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "positive?", "(positive? <real>)");
    return boolean(args[0].num.is_pos());
  }

  // primitive "negative?" procedure
  data primitive_NEGATIVEP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "negative?", "(negative? <real>)");
    return boolean(args[0].num.is_neg());
  }

  // primitive "zero?" procedure
  data primitive_ZEROP(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "zero?", "(zero? <num>)");
    return boolean(args[0].num.is_zero());
  }

  // primitive "not-positive?" procedure
  data primitive_NOT_POSITIVEP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "not-positive?", "(not-positive? <real>)");
    return boolean(!args[0].num.is_pos());
  }

  // primitive "not-negative?" procedure
  data primitive_NOT_NEGATIVEP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "not-negative?", "(not-negative? <real>)");
    return boolean(!args[0].num.is_neg());
  }

  // primitive "not-zero?" procedure
  data primitive_NOT_ZEROP(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "not-zero?", "(not-zero? <num>)");
    return boolean(!args[0].num.is_zero());
  }

  // primitive "infinite?" procedure
  data primitive_INFINITEP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "infinite?", "(infinite? <real>)");
    return boolean(args[0].num.is_pos_inf() || 
                   args[0].num.is_neg_inf());
  }

  // primitive "finite?" procedure (same as primitive "real?")
  data primitive_FINITEP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "finite?", "(finite? <real>)");
    return boolean(args[0].num.is_real() && !args[0].num.is_nan() && 
                   !args[0].num.is_pos_inf() && !args[0].num.is_neg_inf());
  }

  // primitive "nan?" procedure
  data primitive_NANP(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "nan?", "(nan? <real>)");
    return boolean(args[0].num.is_nan());
  }

  /******************************************************************************
  * NUMERIC ROUNDING PRIMITIVES 
  ******************************************************************************/

  // primitive "ceiling" procedure -- ROUNDS UP
  data primitive_CEILING(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "ceiling", "(ceiling <real>)");
    return args[0].num.ceil();
  }

  // primitive "floor" procedure -- ROUNDS DOWN
  data primitive_FLOOR(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "floor", "(floor <real>)");
    return args[0].num.floor();
  }

  // primitive "truncate" procedure -- ROUNDS TOWARDS ZERO
  data primitive_TRUNCATE(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "truncate", "(truncate <real>)");
    return args[0].num.trunc();
  }

  // primitive "round" procedure -- ROUNDS TOWARDS THE NEAREST INT
  data primitive_ROUND(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "round", "(round <real>)");
    return args[0].num.round();
  }

  /******************************************************************************
  * NUMERIC PRECISION COERCION & INTEGER ANALYSIS PRIMITIVES 
  ******************************************************************************/

  // primitive "inexact->exact" procedure
  data primitive_COERCE_INEXACT_TO_EXACT(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "inexact->exact", "(inexact->exact <num>)");
    return args[0].num.to_exact();
  }

  // primitive "exact->inexact" procedure
  data primitive_COERCE_EXACT_TO_INEXACT(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "exact->inexact", "(exact->inexact <num>)");
    return args[0].num.to_inexact();
  }

  // primitive "exact?" procedure
  data primitive_EXACTP(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "exact?", "(exact? <num>)");
    return boolean(args[0].num.is_exact());
  }

  // primitive "inexact?" procedure
  data primitive_INEXACTP(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "inexact?", "(inexact? <num>)");
    return boolean(args[0].num.is_inexact());
  }

  // primitive "integer?" procedure
  data primitive_INTEGERP(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "integer?", "(integer? <num>)");
    return boolean(args[0].num.is_integer());
  }

  // primitive "bigint?" procedure
  data primitive_BIGINTP(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "bigint?", "(bigint? <num>)");
    return boolean(args[0].num.is_exact() && args[0].num.is_integer());
  }

  // primitive "numerator" procedure
  data primitive_NUMERATOR(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "numerator", "(numerator <real>)");
    return args[0].num.extract_numerator();
  }

  // primitive "denominator" procedure
  data primitive_DENOMINATOR(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "denominator", "(denominator <real>)");
    return args[0].num.extract_denominator();
  }

  // primitive "make-log-base" procedure
  // (define (make-log-base n) (lambda (num) (log num n)))
  data primitive_MAKE_LOG_BASE(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "make-log-base", "(make-log-base <real>)");
    data_vector new_log(3);
    new_log[0] = symconst::lambda;
    new_log[1] = data_vector(1,"num");
    new_log[2] = data_vector(3);
    new_log[2].exp[0] = "log";
    new_log[2].exp[1] = "num";
    new_log[2].exp[2] = std::move(args[0]);
    return scm_eval(std::move(new_log),G.GLOBAL_ENVIRONMENT_POINTER);
  }

  /******************************************************************************
  * NUMERIC TRIGONOMETRIC PRIMITIVES -- IN RADIANS
  ******************************************************************************/

  // primitive "sin" procedure
  data primitive_SIN(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "sin", "(sin <num>)");
    return args[0].num.sin();
  }

  // primitive "cos" procedure
  data primitive_COS(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "cos", "(cos <num>)");
    return args[0].num.cos();
  }

  // primitive "tan" procedure
  data primitive_TAN(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "tan", "(tan <num>)");
    return args[0].num.tan();
  }

  // primitive "asin" procedure
  data primitive_ASIN(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "asin", "(asin <num>)");
    return args[0].num.asin();
  }

  // primitive "acos" procedure
  data primitive_ACOS(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "acos", "(acos <num>)");
    return args[0].num.acos();
  }

  // primitive "atan" procedure (supports atan2)
  data primitive_ATAN(data_vector&& args) {
    static constexpr const char * const format = 
      "\n     (atan <num>)\n     (atan <real> <real>) ; atan2 in C++";
    if(args.empty() || args.size() > 2)
      HEIST_THROW_ERR("'atan received incorrect # of args!" << format << HEIST_FCN_ERR("atan",args));
    if(!args[0].is_type(types::num))
      HEIST_THROW_ERR("'atan 1st arg " << HEIST_PROFILE(args[0]) << " isn't a number!" 
        << format << HEIST_FCN_ERR("atan",args));
    if(args.size() == 1) return args[0].num.atan(); // atan
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "atan", format);
    return args[0].num.atan2(args[1].num); // atan2
  }

  // primitive "sinh" procedure
  data primitive_SINH(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "sinh", "(sinh <num>)");
    return args[0].num.sinh();
  }

  // primitive "cosh" procedure
  data primitive_COSH(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "cosh", "(cosh <num>)");
    return args[0].num.cosh();
  }

  // primitive "tanh" procedure
  data primitive_TANH(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "tanh", "(tanh <num>)");
    return args[0].num.tanh();
  }

  // primitive "asinh" procedure
  data primitive_ASINH(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "asinh", "(asinh <num>)");
    return args[0].num.asinh();
  }

  // primitive "acosh" procedure
  data primitive_ACOSH(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "acosh", "(acosh <num>)");
    return args[0].num.acosh();
  }

  // primitive "atanh" procedure
  data primitive_ATANH(data_vector&& args) {
    stdlib_numerics::confirm_unary_numeric(args, "atanh", "(atanh <num>)");
    return args[0].num.atanh();
  }

  /******************************************************************************
  * NUMERIC BITWISE OPERATION PRIMITIVES 
  ******************************************************************************/

  // primitive "logand" procedure
  data primitive_LOGAND(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "logand", "(logand <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGAND,args);
    stdlib_numerics::confirm_2_args(args, "logand", "(logand <real1> <real2>)");
    return args[0].num & args[1].num;
  }

  // primitive "logor" procedure
  data primitive_LOGOR(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "logor", "(logor <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGOR,args);
    stdlib_numerics::confirm_2_args(args, "logor", "(logor <real1> <real2>)");
    return args[0].num | args[1].num;
  }

  // primitive "logxor" procedure
  data primitive_LOGXOR(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "logxor", "(logxor <real1> <real2>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGXOR,args);
    stdlib_numerics::confirm_2_args(args, "logxor", "(logxor <real1> <real2>)");
    return args[0].num ^ args[1].num;
  }

  // primitive "lognot" procedure
  data primitive_LOGNOT(data_vector&& args) {
    stdlib_numerics::confirm_unary_real_numeric(args, "lognot", "(lognot <real>)");
    return ~args[0].num;
  }

  // primitive "loglsl" procedure
  data primitive_LOGLSL(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "loglsl", "(loglsl <real> <shift-amount>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGLSL,args);
    stdlib_numerics::confirm_2_args(args, "loglsl", "(loglsl <real> <shift-amount>)");
    return args[0].num << args[1].num;
  }

  // primitive "loglsr" procedure
  data primitive_LOGLSR(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "loglsr", "(loglsr <real> <shift-amount>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGLSR,args);
    stdlib_numerics::confirm_2_args(args, "loglsr", "(loglsr <real> <shift-amount>)");
    return args[0].num >> args[1].num;
  }

  // primitive "logasr" procedure
  data primitive_LOGASR(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "logasr", "(logasr <real> <shift-amount>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGASR,args);
    stdlib_numerics::confirm_2_args(args, "logasr", "(logasr <real> <shift-amount>)");
    return args[0].num.asr(args[1].num);
  }

  // primitive "logbit?" procedure
  data primitive_LOGBITP(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "logbit?", "(logbit? <real> <bit-No>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGBITP,args);
    stdlib_numerics::confirm_2_args(args, "logbit?", "(logbit? <real> <bit-No>)");
    return (args[0].num >> args[1].num) & 1;
  }

  // primitive "logbit1" procedure
  data primitive_LOGBIT1(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "logbit1", "(logbit1 <real> <bit-No>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGBIT1,args);
    stdlib_numerics::confirm_2_args(args, "logbit1", "(logbit1 <real> <bit-No>)");
    return args[0].num | (1 << args[1].num);
  }

  // primitive "logbit0" procedure
  data primitive_LOGBIT0(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "logbit0", "(logbit0 <real> <bit-No>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGBIT0,args);
    stdlib_numerics::confirm_2_args(args, "logbit0", "(logbit0 <real> <bit-No>)");
    return args[0].num & ~(1 << args[1].num);
  }

  // primitive "logbit~" procedure
  data primitive_LOGBIT_CMPL(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "logbit~", "(logbit~ <real> <bit-No>)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LOGBIT_CMPL,args);
    stdlib_numerics::confirm_2_args(args, "logbit~", "(logbit~ <real> <bit-No>)");
    return args[0].num ^ (1 << args[1].num);
  }

  /******************************************************************************
  * NUMERIC PSUEDO RANDOM NUMBER GENERATOR PRIMITIVE
  ******************************************************************************/

  // primitive "random" procedure -- SEED DEFAULTS TO CURRENT TIME SINCE EPOCH
  data primitive_RANDOM(data_vector&& args) {
    if(args.size() > 1)
      HEIST_THROW_ERR("'random received more than 1 arg:" 
        "\n     (random <optional-real-numeric-seed>)" << HEIST_FCN_ERR("random",args));
    if(args.size()==1 && (!args[0].is_type(types::num) || !args[0].num.is_real()))
      HEIST_THROW_ERR("'random received non-real-numeric arg " << HEIST_PROFILE(args[0])
        << ":\n     (random <optional-real-numeric-seed>)" << HEIST_FCN_ERR("random",args));
    if(args.empty()) return num_type::random();
    return num_type::random(args[0].num);
  }

  /******************************************************************************
  * NUMERIC COMPARISON PRIMITIVES
  ******************************************************************************/

  // primitive "=" procedure:
  data primitive_EQ(data_vector&& args) {
    stdlib_numerics::confirm_only_numbers_and_at_least_one_arg(args, "=", "(= <num1> <num2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_EQ,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num != args[i+1].num) 
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive ">" procedure:
  data primitive_GT(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, ">", "(> <real1> <real2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_GT,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num <= args[i+1].num) 
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "<" procedure:
  data primitive_LT(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "<", "(< <real1> <real2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LT,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num >= args[i+1].num) 
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive ">=" procedure:
  data primitive_GTE(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, ">=", "(>= <real1> <real2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_GTE,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num < args[i+1].num) 
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }

  // primitive "<=" procedure:
  data primitive_LTE(data_vector&& args) {
    stdlib_numerics::confirm_only_reals_and_at_least_one_arg(args, "<=", "(<= <real1> <real2> ...)");
    if(args.size() == 1) return primitive_toolkit::GENERATE_PRIMITIVE_PARTIAL(primitive_LTE,args);
    for(size_type i = 0, n = args.size(); i+1 < n; ++i)
      if(args[i].num > args[i+1].num) 
        return GLOBALS::FALSE_DATA_BOOLEAN;
    return GLOBALS::TRUE_DATA_BOOLEAN;
  }
} // End of namespace heist

#endif