// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_numerics.hpp
// => Defines Numeric class for Heist Scheme Numbers (BigInt, etc.)

#ifndef HEIST_NUMERICS_HPP_
#define HEIST_NUMERICS_HPP_

#include <cctype>
#include <cfloat>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <limits>
#include <random>
#include <string>
#include <type_traits>
#include <utility>

/***
 * -------------
 * CAPABILITIES:
 * -------------
 *
 * NOTE: Use the _n suffix for int, float, and string <Snum> literals!
 *       => Use _n2, _n8, & _n16 for bin, oct, & hex string <Snum> literals!
 *
 * +
 * -
 * *
 * /
 *
 * abs
 * expt
 * quotient
 * remainder // %
 * modulo
 * 
 * equal? // ==
 * max    // >
 * min    // <
 * 
 * exp
 * log
 * sqrt
 * 
 * odd?
 * even?
 * positive?
 * negative?
 * zero?
 * 
 * ceiling  -- ROUNDS UP
 * floor    -- ROUNDS DOWN
 * truncate -- ROUNDS TOWARDS ZERO
 * round    -- ROUNDS TOWARDS THE NEAREST INT
 * 
 * inexact->exact
 * exact->inexact
 * exact?
 * inexact?
 *
 * integer?
 * numerator
 * denominator
 * 
 * gcd
 * lcm
 * 
 * sin,   cos,   tan
 * asin,  acos,  atan
 * sinh,  cosh,  tanh
 * asinh, acosh, atanh
 *
 * logand, logor,  logxor, lognot // & | ^ ~
 * loglsl, loglsr, logasr         // << >> sign-extending>>
 * 
 * random -- BOTH w/ & w/o a seed (defaults to the current time since epoch)
 **/

/******************************************************************************
* FASTER COMPARISON AGAINST "1" & "0" (#undef @END-OF-NUMERICS)
******************************************************************************/

#define EXACT_T_IS_1(IS1_STR) (IS1_STR[0] == '1' && !IS1_STR[1])
#define EXACT_T_IS_0(IS0_STR) (IS0_STR[0] == '0' && !IS0_STR[1])

/******************************************************************************
* COMPILE-TIME SPRINTF FORMATTING FOR inexact_t
******************************************************************************/

#define LD_FIXED_SPRINTF_FORMAT_LOGIC(x) "%."#x"Lf"
#define LD_FIXED_SPRINTF_FORMAT(x) LD_FIXED_SPRINTF_FORMAT_LOGIC(x)

#define LD_SPRINTF_FORMAT_LOGIC(x) "%."#x"Lg"
#define LD_SPRINTF_FORMAT(x) LD_SPRINTF_FORMAT_LOGIC(x)

/******************************************************************************
* NUMERIC BASE CONVERSION HELPER FUNCTIONS
******************************************************************************/

namespace scm_numeric {

  // Constexpr tolower, isdigit, & isalpha equivalents
  constexpr char mklower(const char& d) noexcept {
    return d + (32 * (d >= 'A') * (d <= 'Z'));
  }

  constexpr bool isbase10digit(const char& d) noexcept {
    return d >= '0' && d <= '9';
  }

  constexpr bool islowercase(const char& d) noexcept {
    return d >= 'a' && d <= 'z';
  }

  // Hashes valid digit 'd' to a number ranging 0-35
  constexpr int base_digit_numeric_hash(const char& d) noexcept {
    return islowercase(d) ? d-'a'+10 : isbase10digit(d) ? d-'0' : 36;
  }

  // Confirms whether digit 'd' is a valid base-'b' numeric digit
  constexpr bool is_base_digit(const char& d, const int& b) noexcept {
    return base_digit_numeric_hash(mklower(d)) < b;
  }

  /******************************************************************************
  * COMPILE-TIME EXPONENTIATION (HELPER TO COMPUTE RATIONALITY LIMIT)
  ******************************************************************************/

  constexpr auto constexpr_pow(const long double& base,const std::size_t& pow)noexcept{
    auto f = 1.0L; // repeated squares algorithm
    for(auto i = sizeof(std::size_t)*8; i-- > 0;) {
      f *= f;
      if((pow >> i) & 1) f *= base;
    }
    return f;
  }

  /******************************************************************************
  * NUMERIC CLASS
  ******************************************************************************/

  class Snum {
  public:
    // ***************** CLASS TYPES & FLOATING POINT CONSTANTS *****************

    using exact_t   = std::string;           // fixnum big-int/fractional
    using inexact_t = long double;           // flonum floating point
    static constexpr auto INEXACT_PRECISION = LDBL_DIG;
    static constexpr auto RATIONALITY_LIMIT = constexpr_pow(10.0L,LDBL_DIG);
    static constexpr auto INEXACT_INF       = std::numeric_limits<inexact_t>::infinity();
    static constexpr auto INEXACT_NAN       = std::numeric_limits<inexact_t>::quiet_NaN();
    static constexpr auto INEXACT_MAX       = LDBL_MAX;
    #ifdef LDBL_TRUE_MIN
    static constexpr auto INEXACT_MIN       = LDBL_TRUE_MIN;
    #else
    static constexpr auto INEXACT_MIN       = LDBL_MIN;
    #endif
    static constexpr auto INEXACT_EPSILON   = LDBL_EPSILON;


    // ********************** PRECISION CONVERSION METHODS **********************

    Snum to_inexact()          const noexcept;
    Snum to_exact()            const noexcept;
    Snum extract_numerator()   const noexcept;
    Snum extract_denominator() const noexcept;


    // ************************ PRIMITIVE TYPES COERCION ************************

    inexact_t extract_inexact() const noexcept;
    exact_t   extract_exact()   const noexcept;


    // ************************ MISCELLANEOUS PREDICATES ************************

    // Whether Number is Integeral
    bool is_integer() const noexcept;

    // Whether Number is Exact (Fractional) or Inexact (Float)
    constexpr bool is_exact()   const noexcept {return stat == status::success && !is_float;}
    constexpr bool is_inexact() const noexcept {return stat == status::pinf || stat == status::ninf || is_float;}
    
    // Sign Check
    constexpr bool is_pos()  const noexcept {return stat != status::nan && sign == signs::pos;}
    constexpr bool is_zero() const noexcept {return stat == status::success && sign == signs::zero;}
    constexpr bool is_neg()  const noexcept {return stat != status::nan && sign == signs::neg;}

    // Special State Getters
    constexpr bool is_pos_inf() const noexcept {return stat == status::pinf;}
    constexpr bool is_neg_inf() const noexcept {return stat == status::ninf;}
    constexpr bool is_nan()     const noexcept {return stat == status::nan;}

    // Parity check
    bool is_even() const noexcept;
    bool is_odd()  const noexcept {return !is_even();}

    // Realness & Rationality check
    constexpr bool is_real()     const noexcept {return stat == status::success;}
              bool is_rational() const noexcept;


    // ******************************* CTORS/DTOR *******************************

    Snum()              noexcept {}
    Snum(const Snum& s) noexcept {*this = s;}
    Snum(Snum&& s)      noexcept {*this = std::move(s);}
    ~Snum()             noexcept {}
    Snum(const exact_t& data) noexcept {construct_number(data);}
    Snum(const exact_t& data, const int& base) noexcept {*this=convert_base_N_to_dec(base,data);}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    Snum(NumericData data) noexcept {construct_number(data);}
    Snum(const inexact_t& data) noexcept;


    // *********************** TO_STRING OUTPUT GENERATORS **********************

    // get current value as a std::string (for c-style I/O)
    std::string cpp_str() const noexcept;

    // get current value as a string in 'base' radix form
    std::string cpp_str(const int& base) const noexcept;

    // puts operator for std::ostream
    friend std::ostream& operator<<(std::ostream& outs, const scm_numeric::Snum& n) noexcept {
      outs << n.cpp_str();
      return outs;
    }


    // ********************** ARITHMETIC OPERATOR OVERLOADS *********************

    // assignment operator
    Snum& operator=(const Snum& s) noexcept;
    Snum& operator=(Snum&& s) noexcept;

    // overloaded arithmetic operators
    Snum  operator+ (const Snum& s) const noexcept;
    Snum& operator+=(const Snum& s)noexcept{*this = *this + s; return *this;}
    Snum  operator-()const noexcept;
    Snum  operator- (const Snum& s) const noexcept{return *this + -s;}
    Snum& operator-=(const Snum& s)noexcept{*this = *this - s; return *this;}
    Snum  operator* (const Snum& s) const noexcept;
    Snum& operator*=(const Snum& s)noexcept{*this = *this * s; return *this;}
    Snum  operator/ (const Snum& s) const noexcept;
    Snum& operator/=(const Snum& s)noexcept{*this = *this / s; return *this;}
    Snum  operator% (const Snum& s) const noexcept;
    Snum& operator%=(const Snum& s)noexcept{*this = *this % s; return *this;}

    // overloaded friend arithmetic operators
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator+(const NumericData& lhs,const Snum& rhs)noexcept{return rhs + lhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator-(const NumericData& lhs,const Snum& rhs)noexcept{return -rhs + lhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator*(const NumericData& lhs,const Snum& rhs)noexcept{return rhs * lhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator/(const NumericData& lhs,const Snum& rhs)noexcept{Snum tmp=lhs; return tmp / rhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator%(const NumericData& lhs,const Snum& rhs)noexcept{Snum tmp=lhs; return tmp % rhs;}


    // ************************ RANDOM NUMBER GENERATION ************************

    // pseudo random big int number generator
    static Snum random(Snum seed = std::chrono::system_clock::now().time_since_epoch().count())noexcept;


    // **************** MISCELLANEOUS DERIVED NUMERIC OPERATIONS ****************

    // Exponentiation
    Snum  expt(const Snum& s) const noexcept;
    Snum& expt_eq(const Snum& s)noexcept{*this = expt(s); return *this;}

    // greatest common denominator
    Snum gcd(const Snum& s) const noexcept;
    // least common multiple
    Snum lcm(const Snum& s) const noexcept;

    // exponential function
    Snum exp()  const noexcept {return UNDERLYING_CPP_GENERIC_FCN(12);}

    // sqrt function
    Snum sqrt() const noexcept {return expt(0.5L);}

    // NATURAL logarithm
    Snum log()  const noexcept;

    // absolute value
    Snum abs()  const noexcept {return (is_neg() || is_neg_inf()) ? -*this : *this;}

    // quotient & modulo of being div'd by arg
    Snum quotient(const Snum& s) const noexcept;
    Snum modulo  (const Snum& s) const noexcept;


    // **************************** ROUNDING METHODS ****************************

    Snum ceil()  const noexcept {return ROUNDING_GENERIC_FCN(0);}
    Snum floor() const noexcept {return ROUNDING_GENERIC_FCN(1);}
    Snum trunc() const noexcept {return ROUNDING_GENERIC_FCN(2);}
    Snum round() const noexcept {return ROUNDING_GENERIC_FCN(3);}


    // ****************** TRIGONOMETRIC METHODS -:- IN RADIANS ******************

    Snum sin()   const noexcept {return UNDERLYING_CPP_GENERIC_FCN(0);}
    Snum cos()   const noexcept {return UNDERLYING_CPP_GENERIC_FCN(1);}
    Snum tan()   const noexcept {return UNDERLYING_CPP_GENERIC_FCN(2);}
    Snum asin()  const noexcept {return UNDERLYING_CPP_GENERIC_FCN(3);}
    Snum acos()  const noexcept {return UNDERLYING_CPP_GENERIC_FCN(4);}
    Snum atan()  const noexcept {return UNDERLYING_CPP_GENERIC_FCN(5);}
    Snum sinh()  const noexcept {return UNDERLYING_CPP_GENERIC_FCN(6);}
    Snum cosh()  const noexcept {return UNDERLYING_CPP_GENERIC_FCN(7);}
    Snum tanh()  const noexcept {return UNDERLYING_CPP_GENERIC_FCN(8);}
    Snum asinh() const noexcept {return UNDERLYING_CPP_GENERIC_FCN(9);}
    Snum acosh() const noexcept {return UNDERLYING_CPP_GENERIC_FCN(10);}
    Snum atanh() const noexcept {return UNDERLYING_CPP_GENERIC_FCN(11);}


    // ************************** COMPARISON OPERATORS **************************

    // overloaded equality/comparison operators
    bool operator==(const Snum& s) const noexcept;
    bool operator!=(const Snum& s) const noexcept {return !(*this == s);}
    bool operator< (const Snum& s) const noexcept;
    bool operator> (const Snum& s) const noexcept;
    bool operator<=(const Snum& s) const noexcept {return !(*this > s);}
    bool operator>=(const Snum& s) const noexcept {return !(*this < s);}
    bool operator!()               const noexcept {return is_zero();}

    // overloaded friend equality/comparison operators
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend bool operator==(const NumericData& lhs, const Snum& rhs) noexcept {return rhs == lhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend bool operator!=(const NumericData& lhs, const Snum& rhs) noexcept {return rhs != lhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend bool operator< (const NumericData& lhs, const Snum& rhs) noexcept {return rhs > lhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend bool operator> (const NumericData& lhs, const Snum& rhs) noexcept {return rhs < lhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend bool operator<=(const NumericData& lhs, const Snum& rhs) noexcept {return rhs >= lhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend bool operator>=(const NumericData& lhs, const Snum& rhs) noexcept {return rhs <= lhs;}


    // *************************** LOGICAL-BIT OPERATORS **************************

    // overloaded bitwise operators
    Snum operator~()const noexcept;
    Snum operator& (const Snum& rhs) const noexcept;
    Snum operator| (const Snum& rhs) const noexcept;
    Snum operator^ (const Snum& rhs) const noexcept;
    Snum operator<<(const Snum& rhs) const noexcept;
    Snum operator>>(const Snum& rhs) const noexcept;
    Snum asr       (const Snum& rhs) const noexcept;

    Snum& operator&= (const Snum& rhs) noexcept {*this = *this & rhs;  return *this;}
    Snum& operator|= (const Snum& rhs) noexcept {*this = *this | rhs;  return *this;}
    Snum& operator^= (const Snum& rhs) noexcept {*this = *this ^ rhs;  return *this;}
    Snum& operator<<=(const Snum& rhs) noexcept {*this = *this << rhs; return *this;}
    Snum& operator>>=(const Snum& rhs) noexcept {*this = *this >> rhs; return *this;}
    Snum& asr_eq     (const Snum& rhs) noexcept {*this = asr(rhs);     return *this;}

    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator&(const NumericData& lhs,const Snum& rhs) noexcept{Snum tmp=lhs; return tmp & rhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator|(const NumericData& lhs,const Snum& rhs) noexcept{Snum tmp=lhs; return tmp | rhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator^(const NumericData& lhs,const Snum& rhs) noexcept{Snum tmp=lhs; return tmp ^ rhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator<<(const NumericData& lhs,const Snum& rhs)noexcept{Snum tmp=lhs; return tmp << rhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator>>(const NumericData& lhs,const Snum& rhs)noexcept{Snum tmp=lhs; return tmp >> rhs;}



  private:
    // Private types (& size_type max) for numerical construction
    enum class precisions { exact, inexact, invalid  };
    enum class status     { pinf, ninf, nan, success };
    enum class signs      { neg,  zero, pos          };
    using size_type                     = std::size_t;
    static constexpr auto SIZE_TYPE_MAX = std::numeric_limits<size_type>::max();

    // Internal Numerical Representation Invariants
    signs sign  = signs::zero;
    status stat = status::success;

    exact_t numerator   = "0";  // exact numer
    exact_t denominator = "1";  // exact denom
    inexact_t float_num = 0.0L; // inexact floating point
    bool is_float       = false;

    // Special State Setters
              void set_zero() noexcept;
    constexpr void set_pinf() noexcept {stat = status::pinf, sign = signs::pos;}
    constexpr void set_ninf() noexcept {stat = status::ninf, sign = signs::neg;}

    // Perform fixed floating point conversion to a string
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    std::string convert_numeric_to_str(const NumericData& n) const noexcept {return std::to_string(n);};
    std::string convert_numeric_to_str(const inexact_t& n) const noexcept;
    // Convert a inexact_t integer to a string
    exact_t inexact_integer_to_str(const inexact_t& num) const noexcept;
    // Simplify the current number (simplifies exact number/converts to float as needed)
    void simplify_numerics()noexcept;
    // Adjust the current number's float invariants
    void adjust_float_invariants()noexcept;
    // Adjust invariants once stat != status::success
    constexpr void set_failed_status()noexcept;

    // Rm whitespace from number string & trims padding 0's
    void trim_data_string(exact_t& num_str)noexcept;

    // Confirm given string is a viable candidate to be a number
    precisions confirm_valid_string(const exact_t& num_str)noexcept;

    // Parse the _confirmed_ valid number
    void parse_integer(exact_t::iterator ch)noexcept;

    // Assign NaN or Inf if given such as a string, and return whether did so
    bool is_irrational(const exact_t& num_str)noexcept;

    // Construct number from the given data
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    void construct_number(NumericData num_str)noexcept{construct_number(convert_numeric_to_str(num_str));}
    void construct_number(exact_t num_str)noexcept;

    // Convert a base-n big int string into a decimal Snum
    Snum convert_base_N_to_dec(const int& base, exact_t bnum) const noexcept;
    // Converts the Snum decimal representation into the <base> # system
    exact_t convert_dec_to_base_N(const int& base, const Snum& dnum) const noexcept;
    // Evaluates the numerator & denominator of a base-N # & divides their results
    Snum form_decimal_fraction(const int& base, const size_type& i, const exact_t& bnum, const bool& is_neg) const noexcept;
    // Evaluates the integral & fractional of a base-N # & combines their results
    Snum form_decimal_floating_pt(const int& base, const size_type& i, const exact_t& bnum, const bool& is_neg) const noexcept;
    // Evaluates the numerator & denominator of a decimal # & divides their results
    exact_t form_base_N_fraction(const int& base,const Snum& dnum) const noexcept;
    // Evaluates the integral & fractional of a decimal # & combines their results
    exact_t form_base_N_floating_pt(const int& base, const Snum& dnum) const noexcept;
    // Confirms base is in the range of [2,36]
    constexpr bool confirm_base_in_range(const int& base) const noexcept;
    // Confirms <bnum> only contains digits in the range of the <base> # system
    bool confirm_valid_base_digits(const int& base, const exact_t& bnum) const noexcept;
    // Converts a decimal digit into a base-36 digit
    const char * base_digit_conversion(const Snum& digit, const int& base) const noexcept;

    // Fast comparison for whether *this == -1
    bool is_negative_one() const noexcept;
    // Performs 2's complement negation on binary string bin_str
    void big_int_BINARY_2sCMPL_NEGATION(exact_t& bin_str) const noexcept;
    // Extend bit_str1 w/ b1 & bit_str2 w/ b2 so they have the same width
    void big_int_BINARY_extend(exact_t& bit_str1, exact_t& bit_str2, const char& b1, const char& b2) const noexcept;
    // Sign-extend bit_str1 & bit_str2 to match lengths
    void signed_big_int_BINARY_extend(exact_t& bit_str1, exact_t& bit_str2) const noexcept;
    // Unsigned-extend bit_str1 & bit_str2 to match lengths
    void unsigned_big_int_BINARY_extend(exact_t& bit_str1, exact_t& bit_str2) const noexcept;
    // Convert possibly-signed binary string to a Snum
    Snum big_int_revert_BINARY_to_Snum(exact_t bin_str, const bool& is_negative_binary) const noexcept;
    // Convert a Snum to a no-fraction binary string (degrades fractions to inexact_t)
    bool get_non_fraction_binary_string(Snum n, exact_t& bit_str, const bool& is_signed) const noexcept;
    // Convert 2 Snums to no-fraction binary strings (degrades fractions to inexact_t)
    bool get_non_fraction_binary_strings(const Snum& n1, exact_t& bit_str1, const Snum& n2, exact_t& bit_str2) const noexcept;
    // Template outlining structure of LSR & ASR (logical/arithmetic shift right)
    template<bool LOGIC_RIGHT_SHIFT> Snum GENERIC_SHIFT_RIGHT(const Snum& rhs) const noexcept;

    // Erases redundant RHS padding 0s, trimming fractionals
    void reduce_redundant_RHS_0s(exact_t& fractional) const noexcept;
    // Pad 0 to the LHS of a <base> fractional as needed for improved accuracy
    void pad_LHS_base_N_decimal_0s_as_needed(const int& base, const exact_t& dnum, exact_t& bnum) const noexcept;
    // Converts the given <base> floating point fractional to the decimal # system
    exact_t convert_base_N_decimal_to_dec(const int& base, exact_t bnum) const noexcept;
    // Converts the given decimal floating point fractional to the <base> # system
    exact_t convert_dec_decimal_to_base_N(const int& base, exact_t dnum) const noexcept;

    // Coerce the given int string into float. Returns the success status.
    status coerce_int_to_float(inexact_t& num, const exact_t& data_to_coerce, const bool data_is_neg) const noexcept;
    // Coerce the given fraction string into float. Returns the success status.
    status coerce_fraction_to_float(inexact_t& num, const exact_t& numer, const exact_t& denom, const bool data_is_neg) const noexcept;

    // Decompose the numerator into SIZE_TYPE_MAX factors (enables repeated squaring)
    std::pair<std::pair<size_type,size_type>,Snum::status> decompose_int_into_SIZE_TYPE_MAX() const noexcept;
    // Perform the "repeated squares" algorithm (rapid exponentiation)
    Snum repeated_squares(const Snum& a, const size_type& b) const noexcept;

    // Generic rounding fcn template for ceil, floor, trunc, & round
    Snum ROUNDING_GENERIC_FCN(const size_type ROUNDING_TYPE_ID) const noexcept;
    // Generic cpp-fcn-invocation template
    Snum UNDERLYING_CPP_GENERIC_FCN(const size_type CPP_FCN_TYPE_ID) const noexcept;

    // Returns whether big-int a > big-int b
    bool big_int_gt(const exact_t& a, const exact_t& b) const noexcept;
    // Returns whether big-int a < big-int b
    bool big_int_lt(const exact_t& a, const exact_t& b) const noexcept;
    // Sums 2 strings of decimal digits into a single string
    exact_t big_int_abs_val_sum(const exact_t& a, const exact_t& b) const noexcept;
    // Multiplies 2 strings of decimal digits into a single string
    exact_t big_int_abs_val_mul(const exact_t& a, const exact_t& b) const noexcept;
    // MUL '* HELPER: Sums 2 strings of decimal digits into a single string
    void big_int_abs_val_MUL_HELPER_sum(exact_t& a, const exact_t& b) const noexcept;
    // Returns the absolute difference (in a single string) of 2 strings of decimal digits
    exact_t big_int_abs_val_diff(const exact_t& a, const exact_t& b) const noexcept;

    // Returns gcd of flt a & b; PRECONDITION: a > b
    constexpr inexact_t inexact_t_GCD(const inexact_t a, const inexact_t b) const noexcept;
  }; // End class Snum 

  /******************************************************************************
  * RANDOM NUMBER GENERATION
  ******************************************************************************/

  Snum Snum::random(Snum seed) noexcept {
    inexact_t seed_flt = 0;
    // Coerce seed to float as needed
    if(seed.is_float) {
      seed_flt = seed.float_num;
    } else if(auto tmp = seed.to_inexact(); tmp.stat != status::success) {
      // Float conversion failed: chop off numerator's 1st INEXACT_PRECISION/2 digits
      // & denominator to be used as a substitute seed
      if(seed.numerator.size() > INEXACT_PRECISION/2)   seed.numerator.erase(INEXACT_PRECISION/2);
      if(seed.denominator.size() > INEXACT_PRECISION/2) seed.denominator.erase(INEXACT_PRECISION/2);
      seed_flt = std::stold(seed.numerator) + std::stold(seed.denominator);
    } else {
      seed_flt = tmp.float_num;
    }
    
    // initialize the random #'s generator & invariants
    exact_t rand_vals;
    auto rand_gen = std::minstd_rand0((size_type)seed_flt);
    const auto size_decrement = rand_gen() % 10; // 1st digit
    const auto rand_val_total_digits = INEXACT_PRECISION - size_decrement;

    // generate a random number with 'rand_val_total_digits' digits
    while(rand_vals.size() < rand_val_total_digits)
      rand_vals += std::to_string(rand_gen());
    if(rand_vals.size() > rand_val_total_digits)
      rand_vals.erase(rand_val_total_digits);
    return Snum(rand_vals);
  }

  /******************************************************************************
  * CHECK IF NUMBER IS AN INTEGRAL, & EXACT/INEXACT CONVERSIONS
  ******************************************************************************/

  bool Snum::is_integer() const noexcept {
    if(stat != status::success)                return false; // NaN & +- inf != integers
    if(is_zero())                              return true;  // 0 is an integer
    if(!is_float && EXACT_T_IS_1(denominator)) return true;
    if(is_float) { // test if float has a fractional
      inexact_t integral;
      inexact_t fractional = std::modf(float_num, &integral);
      return fractional == 0;
    }
    return false;
  }


  Snum Snum::to_inexact() const noexcept {
    if(stat != status::success || is_float) return *this;
    Snum tmp;
    tmp.stat = coerce_fraction_to_float(tmp.float_num,numerator,denominator,is_neg());
    if(tmp.stat != status::success) {
      tmp.set_failed_status(); return tmp;
    }
    tmp.is_float = true;
    tmp.adjust_float_invariants();
    return tmp;
  }


  Snum Snum::to_exact() const noexcept {
    if(stat != status::success || !is_float) return *this;
    Snum tmp;
    // If zero, return immediately
    if(float_num == 0) return tmp;
    // If integer, convert directly as is
    if(is_integer()) {
      tmp.numerator = inexact_integer_to_str(float_num);
      tmp.sign = sign;
      return tmp;
    }
    // convert fractional portion of float to a fraction, then add such to the 
    //   integral portion to form an 'exact' approximation
    inexact_t integral;
    inexact_t fractional      = std::modf(float_num, &integral);
    exact_t unsigned_integral = std::to_string(size_type(integral));
    size_type fractional_prec = (unsigned_integral.size() >= INEXACT_PRECISION) 
                                ? 0 : INEXACT_PRECISION-unsigned_integral.size();
    fractional                = std::round(fractional * std::pow(10.0L, fractional_prec));
    exact_t unsigned_numer    = std::to_string(size_type(fractional));
    exact_t unsigned_denom    = std::to_string(size_type(std::pow(10.0L, fractional_prec)));

    auto exact_num = Snum(unsigned_integral) + Snum(unsigned_numer + "/" + unsigned_denom);
    if(is_neg()) exact_num.sign = signs::neg;
    return exact_num;
  }


  Snum Snum::extract_numerator() const noexcept {
    auto tmp = to_exact();
    if(tmp.stat != status::success || tmp.is_float) {
      tmp.set_failed_status(); return tmp;
    }
    tmp.denominator = "1";
    return tmp;
  }


  Snum Snum::extract_denominator() const noexcept {
    auto tmp = to_exact().abs();
    if(tmp.stat != status::success || tmp.is_float) {
      tmp.set_failed_status(); return tmp;
    }
    tmp.numerator = tmp.denominator;
    tmp.denominator = "1";
    return tmp;
  }

  /******************************************************************************
  * PRIMITIVE TYPES COERCION
  ******************************************************************************/

  Snum::inexact_t Snum::extract_inexact() const noexcept {
    if(auto tmp = to_inexact(); tmp.stat != status::success) {
      if(tmp.is_nan())     return INEXACT_NAN;
      if(tmp.is_pos_inf()) return INEXACT_INF;
      return -1 * INEXACT_INF;
    } else if(tmp.is_neg()) { 
      return tmp.float_num * -1;
    } else {
      return tmp.float_num;
    }
  }


  Snum::exact_t Snum::extract_exact() const noexcept {
    auto tmp = to_exact();
    if(tmp.stat != status::success) {
      if(tmp.is_nan())     return "+nan.0";
      if(tmp.is_pos_inf()) return "+inf.0";
      return "-inf.0";
    }
    if(tmp.is_zero()) return "0";
    if(tmp.is_pos()) {
      if(EXACT_T_IS_1(tmp.denominator))
        return tmp.numerator;
      return tmp.numerator + '/' + tmp.denominator;
    }
    if(EXACT_T_IS_1(tmp.denominator))
      return '-' + tmp.numerator;
    return '-' + tmp.numerator + '/' + tmp.denominator;
  }

  /******************************************************************************
  * NUMBER SETTERS TO ZERO, PINF, NINF
  ******************************************************************************/

  void Snum::set_zero() noexcept {
    sign = signs::zero;
    numerator = "0", denominator = "1";
    is_float = false;  // coerce back to 'exact' once 0
    stat = status::success;
  }

  /******************************************************************************
  * NUMBER SIMPLIFICATION & TO-STRING HELPER FUNCTIONS
  ******************************************************************************/

  // Perform fixed floating point conversion to a string
  // NOTE: std::to_string only has a default precision of 6, so we use sprintf
  // NOTE: the result is _NOT_ an exact_t integer, but a string of the float
  std::string Snum::convert_numeric_to_str(const inexact_t& n) const noexcept {
    char outs[64];
    sprintf(outs, LD_FIXED_SPRINTF_FORMAT(LDBL_DIG), n);
    exact_t num_str(outs);
    // rm redundant 0s to the right of the decimal
    if(num_str.find_first_of('.') != std::string::npos && *num_str.rbegin() == '0') {
      size_type i = num_str.size()-1;
      for(; i>1 && num_str[i-1]!='.' && num_str[i]=='0'; --i);
      num_str.erase(i+1);
    }
    return num_str;
  }

  // Convert a inexact_t integer to a string
  Snum::exact_t Snum::inexact_integer_to_str(const inexact_t& num)const noexcept{
    auto str = std::to_string(num);
    auto iter = str.end()-1;
    while(*iter == '0') --iter; // move past 0's to the right of the decimal
    if(*iter == '.')    --iter; // skip the decimal if nothing after it
    return exact_t(str.begin(),iter+1);
  }

  // Simplify the current number (simplifies exact number/converts to float as needed)
  void Snum::simplify_numerics() noexcept {
    if(is_float && float_num < 0) float_num *= -1;
    if(stat != status::success || is_zero() || is_float || EXACT_T_IS_1(denominator)) return; 
    if(numerator == denominator) numerator = denominator = "1";
    else if(numerator.size() <= INEXACT_PRECISION && denominator.size() <= INEXACT_PRECISION) {
      inexact_t N = std::stold(numerator), D = std::stold(denominator);
      inexact_t greatest_common_divisor = inexact_t_GCD(N>D?N:D,N>D?D:N);
      numerator   = inexact_integer_to_str(N/greatest_common_divisor);
      denominator = inexact_integer_to_str(D/greatest_common_divisor);
    } else {
      coerce_fraction_to_float(float_num,numerator,denominator,is_neg());
      is_float = true;
    }
  }

  // Adjust the current number's float invariants
  void Snum::adjust_float_invariants() noexcept {
    if(std::isnan(float_num)) {stat=status::nan; return;}
    // account for float's sign
    is_float = true;
    if(float_num == 0)
      set_zero();
    else if(float_num < 0)
      sign = signs::neg, float_num *= -1;
    else
      sign = signs::pos;
    if(float_num == INEXACT_INF) { // if float = inf
      if(float_num < 0) set_ninf();
      else              set_pinf();
    }
  }

  // Adjust invariants once stat != status::success
  // => 'nan' status need only be set, which is handled prior this invocation
  constexpr void Snum::set_failed_status() noexcept {
    if(is_neg_inf()) set_ninf();
    else if(is_pos_inf()) set_pinf();
  }

  /******************************************************************************
  * ASSIGNMENT
  ******************************************************************************/

  // assignment operator
  Snum& Snum::operator=(const Snum& s) noexcept {
    sign        = s.sign;
    stat        = s.stat;
    numerator   = s.numerator;
    denominator = s.denominator;
    float_num   = s.float_num;
    is_float    = s.is_float;
    return *this;
  }

  Snum& Snum::operator=(Snum&& s) noexcept {
    sign        = std::move(s.sign);
    stat        = std::move(s.stat);
    numerator   = std::move(s.numerator);
    denominator = std::move(s.denominator);
    float_num   = std::move(s.float_num);
    is_float    = std::move(s.is_float);
    return *this;
  }

  /******************************************************************************
  * ADDITION
  ******************************************************************************/

  Snum Snum::operator+(const Snum& s) const noexcept {
    Snum tmp;
    if(stat != status::success || s.stat != status::success) {
      // NaN if either NaN, or subtracting inf from inf
      if(is_nan() || s.is_nan() || (is_pos_inf() && s.is_neg_inf()) || (is_neg_inf() && s.is_pos_inf())) {
        tmp.stat = status::nan; return tmp;
      // adding any finite value to +inf = +inf
      } else if(is_pos_inf() || s.is_pos_inf()) {
        tmp.set_pinf(); return tmp;
      // adding any finite value to -inf = -inf
      } else if(is_neg_inf() || s.is_neg_inf()) {
        tmp.set_ninf(); return tmp;
      } 
    }

    // n + 0 = n
    if(is_zero()) return s;
    if(s.is_zero()) return *this;

    // Sum Ints
    if(!is_float && !s.is_float) {
      // cross-multiply & add
      exact_t new_numer1 = big_int_abs_val_mul(numerator,s.denominator);
      exact_t new_numer2 = big_int_abs_val_mul(s.numerator,denominator);
      tmp.denominator    = big_int_abs_val_mul(denominator,s.denominator);
      if(sign == s.sign) {
        tmp.numerator = big_int_abs_val_sum(new_numer1,new_numer2);
        if(is_neg()) tmp.sign = signs::neg;
        else         tmp.sign = signs::pos; // if either 0, would've already returned by now.
      } else {
        bool negative_diff = big_int_gt(new_numer1,new_numer2) ? is_neg() : s.is_neg();
        tmp.numerator = big_int_abs_val_diff(new_numer1,new_numer2);
        if(EXACT_T_IS_0(tmp.numerator)) tmp.set_zero();
        else if(negative_diff)          tmp.sign = signs::neg;
        else                            tmp.sign = signs::pos;
      }
      tmp.simplify_numerics();
      return tmp;
    }

    // Sum Floats
    if(is_float && s.is_float) {
      auto lhs = float_num, rhs = s.float_num;
      if(is_neg())   lhs *= -1;
      if(s.is_neg()) rhs *= -1;
      tmp.float_num = lhs + rhs;
    
    // Coerce Int to Float then Sum Floats
    } else {
      inexact_t converted_float = 0;
      if(is_float && !s.is_float)
        tmp.stat = coerce_fraction_to_float(converted_float, s.numerator, s.denominator, s.is_neg());
      else tmp.stat = coerce_fraction_to_float(converted_float, numerator, denominator, is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status();return tmp;
      }
      auto rhs = is_float ? float_num : s.float_num;
      if((is_float && is_neg()) || (s.is_float && s.is_neg())) rhs *= -1;
      tmp.float_num = converted_float + rhs;
    }

    // handle floating-point addition invariant changes
    tmp.adjust_float_invariants();
    return tmp;
  }

  /******************************************************************************
  * NEGATION
  ******************************************************************************/

  Snum Snum::operator-() const noexcept {
    auto tmp = *this;
    if(tmp.is_pos_inf())       tmp.set_ninf();
    else if(tmp.is_neg_inf())  tmp.set_pinf();
    else if(tmp.is_pos())      tmp.sign = signs::neg; 
    else if(tmp.is_neg())      tmp.sign = signs::pos; 
    return tmp;
  }

  /******************************************************************************
  * MULTIPLICATION
  ******************************************************************************/

  Snum  Snum::operator*(const Snum& s) const noexcept {
    Snum tmp;
    // n * 0 = 0
    if(is_zero() || s.is_zero()) return Snum();
    if(stat != status::success || s.stat != status::success) {
      // NaN if either NaN
      if(is_nan() || s.is_nan()) {
        tmp.stat = status::nan; return tmp;
      // +inf.0 * <non-neg> = +inf.0, -inf.0 * <neg> = +inf.0
      } else if((is_pos_inf() && !s.is_neg_inf() && !s.is_neg()) || 
                (s.is_pos_inf() && !is_neg_inf() && !is_neg()) || 
                (is_neg_inf() && s.is_neg()) || 
                (s.is_neg_inf() && is_neg()) || 
                (is_neg_inf() && s.is_neg_inf())) {
        tmp.set_pinf(); return tmp;
      // +inf.0 * <neg> = -inf.0, -inf.0 * <non-neg> = -inf.0
      } else if(is_neg_inf() || s.is_neg_inf() || is_pos_inf() || s.is_pos_inf()) {
        tmp.set_ninf(); return tmp;
      }
    }

    // Multiply Ints
    if(!is_float && !s.is_float) {
      tmp.numerator = big_int_abs_val_mul(numerator,s.numerator);
      tmp.denominator = big_int_abs_val_mul(denominator,s.denominator);
      tmp.sign = (sign == s.sign) ? signs::pos : signs::neg;
      tmp.simplify_numerics();
      return tmp;
    }

    // Multiply Floats
    if(is_float && s.is_float) {
      auto lhs = float_num, rhs = s.float_num;
      if(is_neg())   lhs *= -1;
      if(s.is_neg()) rhs *= -1;
      tmp.float_num = lhs * rhs;
    
    // Coerce Int to Float then Multiply Floats
    } else {
      inexact_t converted_float = 0;
      if(is_float && !s.is_float)
        tmp.stat = coerce_fraction_to_float(converted_float, s.numerator, s.denominator, s.is_neg());
      else tmp.stat = coerce_fraction_to_float(converted_float, numerator, denominator, is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status(); return tmp;
      }
      auto rhs = is_float ? float_num : s.float_num;
      if((is_float && is_neg()) || (s.is_float && s.is_neg())) rhs *= -1;
      tmp.float_num = converted_float * rhs;
    }

    // handle floating-point multiplication invariant changes
    tmp.adjust_float_invariants();
    return tmp;
  }

  /******************************************************************************
  * DIVISION
  ******************************************************************************/

  Snum Snum::operator/(const Snum& s) const noexcept {
    auto tmp = s;
    // any div with NaN = n/0 = inf/inf = NaN
    if(is_nan() || tmp.is_nan() || tmp.is_zero() || 
      ((is_pos_inf() || is_neg_inf()) && (tmp.is_pos_inf() || tmp.is_neg_inf()))){
      tmp.stat=status::nan;return tmp;
    }
    // real# / inf = 0
    if(tmp.is_pos_inf() || tmp.is_neg_inf()) return Snum();
    // invert numeric value, then multiply
    if(tmp.is_float)
      tmp.float_num = 1.0L / tmp.float_num;
    else
      std::swap<exact_t>(tmp.numerator,tmp.denominator);
    return *this * tmp;
  }

  /******************************************************************************
  * REMAINDER
  ******************************************************************************/

  Snum Snum::operator%(const Snum& s) const noexcept {
    // a % b:
    //   let a / b = x.yyyyyyyyyy <infin y's>
    //   let z = (a / b) - x
    //   then a % b = z * b
    auto tmp = s;
    const auto abs_this = this->abs();
    const auto abs_s = s.abs();
    // any mod with NaN = n % 0 = NaN
    if(is_nan() || s.is_nan() || s.is_zero()) {
      tmp.stat=status::nan;return tmp;
    }
    // n % n = 0
    if(abs_this == abs_s) return Snum();
    // for n < m, n % m = n
    if(abs_this < abs_s) return *this;
    // modding by inf: if n != inf, 1; else, 0
    if(abs_s.is_pos_inf()) {
      if(!abs_this.is_pos_inf()) return *this;
      return Snum();
    }

    // apply the above algorithm
    tmp = *this / s;
    inexact_t div_res;
    if(!tmp.is_float) { // coerce fractional to floating-point
      tmp.stat = coerce_fraction_to_float(div_res,tmp.numerator,tmp.denominator,tmp.is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status();return tmp;
      }
    } else {
      div_res = tmp.float_num;
    }

    inexact_t integral;
    inexact_t fractional = std::modf(div_res,&integral);
    auto mod_result = (s * fractional);
    mod_result.adjust_float_invariants();

    // int % int : int
    if(is_integer() && s.is_integer()) 
      mod_result = mod_result.round().to_exact();
    if(!mod_result.is_zero())
      mod_result.sign = sign;
    return mod_result;
  }

  /******************************************************************************
  * EXPONENTIATION
  ******************************************************************************/

  Snum Snum::expt(const Snum& s) const noexcept {
    Snum tmp, pow(s);
    // n^NaN = NaN^n = NaN
    if(is_nan() || pow.is_nan()) {
      tmp.stat = status::nan;return tmp;
    }
    // n^0 = 1
    if(pow.is_zero()) {
      tmp.numerator = "1", tmp.sign = signs::pos; return tmp;
    }
    // 0^pos = n^-inf = +inf^neg = -inf^neg = 0, 0^neg = NaN
    if(is_zero() || pow.is_neg_inf() || ((is_pos_inf() || is_neg_inf()) && pow.is_neg())) {
      if(is_zero() && (s.is_neg() || s.is_neg_inf())) {
        tmp.stat = status::nan; return tmp;
      }
      return Snum();
    }
    // +inf^n = n^+inf = +inf
    if(pow.is_pos_inf() || is_pos_inf()) {
      tmp.set_pinf(); return tmp;
    }
    // -inf^even = +inf, -inf^odd = -inf
    if(is_neg_inf()) {
      if(pow.is_even()) tmp.set_pinf();
      else              tmp.set_ninf();
      return tmp;
    }

    // Coerce Fractional Power to Float
    if(!pow.is_float && !EXACT_T_IS_1(pow.denominator)) { 
      tmp.stat = coerce_fraction_to_float(pow.float_num,pow.numerator,pow.denominator,pow.is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status();return tmp;
      }
      pow.is_float = true;
    }

    // Int Power Exponentiation
    if(!pow.is_float) {
      // For integer n, a^n:
      //   let q = n / SIZE_TYPE_MAX // integer division, hence the quotient
      //   let r = n % SIZE_TYPE_MAX // remainder
      //   Thus a^n = a^(q*SIZE_TYPE_MAX + r) 
      //            = ((a^SIZE_TYPE_MAX)^q) * a^r

      // We thus decompose 'n' into 'SIZE_TYPE_MAX' blocks in order to facilitate 
      //   extracting the power's binary (for the repeated-squaring algorithm)
      //   => Uses C++17 Structured Binding
      auto [unum_decomposition, decomp_status] = pow.abs().decompose_int_into_SIZE_TYPE_MAX(); 
      if(decomp_status != status::success){
        if(decomp_status == status::nan)  {tmp.stat = status::nan;return tmp;} // n^NaN = NaN
        if(decomp_status == status::ninf) return Snum();                // n^-inf = 0
        if(decomp_status == status::pinf) {tmp.set_pinf();return tmp;}         // n^+inf = +inf
      }
      auto [unum_amount, unum_remainder] = unum_decomposition;
      Snum lhs("1");
      if(unum_amount > 0) {
        lhs = repeated_squares(*this, SIZE_TYPE_MAX);          // (a^SIZE_TYPE_MAX)
        lhs = repeated_squares(lhs, unum_amount);              // ((a^SIZE_TYPE_MAX)^q)
        if(is_neg() && (unum_amount&1)) lhs.sign = signs::neg; // neg^odd_pow = neg
        else                            lhs.sign = signs::pos;
      }
      tmp = repeated_squares(*this, unum_remainder);            // a^r
      if(is_neg() && (unum_remainder&1)) tmp.sign = signs::neg; // neg^odd_pow = neg
      else                               tmp.sign = signs::pos;

      if(pow.is_neg()) return 1/(lhs * tmp); // 1 / (((a^SIZE_TYPE_MAX)^q) * a^r)
      return lhs * tmp;                      // ((a^SIZE_TYPE_MAX)^q) * a^r
    }

    // Account for the Negative Power (since using std::pow)
    if(pow.is_neg() && pow.float_num > 0)
      pow.float_num *= -1;

    // Float-Float Exponentiation
    inexact_t base_flt = 0;
    if(is_float) {
      base_flt = is_neg() ? -float_num : float_num;

    // Coerce Int Base to Float, then Float-Float Exponentiation
    } else {
      tmp.stat = coerce_fraction_to_float(base_flt,numerator,denominator,is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status();return tmp;
      }
    }
    tmp.float_num = std::pow(base_flt, pow.float_num);
    tmp.adjust_float_invariants();
    return tmp;
  }

  /******************************************************************************
  * GREATEST COMMON DENOMINATOR
  ******************************************************************************/

  Snum Snum::gcd(const Snum& s) const noexcept {
    Snum tmp;
    // GCD of NaN = GCD of Float = NaN
    if(stat == status::nan || s.stat == status::nan || !is_integer() || !s.is_integer()) {
      tmp.stat=status::nan; return tmp;
    }
    
    // GCD of 0 = abs value of the other arg
    if(is_zero())   return s.abs();
    if(s.is_zero()) return this->abs();
    
    // account for GCD of inf
    if(stat != status::success)   return s;
    if(s.stat != status::success) return *this;
    inexact_t lhs = 0, rhs = 0;
    
    // Coerce this' Fractional to Float as Needed
    if(is_float) lhs = float_num;
    else {
      tmp.stat = coerce_fraction_to_float(lhs, numerator, denominator, is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status(); return tmp;
      }
    }
    
    // Coerce s' Fractional to Float as Needed
    if(s.is_float) rhs = s.float_num;
    else {
      tmp.stat = coerce_fraction_to_float(rhs, s.numerator, s.denominator, s.is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status(); return tmp;
      }
    }
    
    // return the GCD
    tmp.float_num = inexact_t_GCD(lhs>rhs?lhs:rhs,lhs>rhs?rhs:lhs);
    tmp.adjust_float_invariants();
    return tmp.to_exact();
  }

  /******************************************************************************
  * LEAST COMMON MULTIPLE
  ******************************************************************************/

  Snum Snum::lcm(const Snum& s) const noexcept {
    // Note that lcm(a,b) = |a*b|/gcd(a,b) = (|a|/gcd(a,b)) * |b|
    Snum tmp;
    
    // LCM of NaN = LCM of Float = NaN
    if(stat == status::nan || s.stat == status::nan || !is_integer() || !s.is_integer()) {
      tmp.stat=status::nan; return tmp;
    }
    
    // LCM of 0,0 = 0 (special case)
    if(is_zero() && s.is_zero()) return Snum();
    
    // account for LCM of inf
    if(stat != status::success)   return *this;
    if(s.stat != status::success) return s;
    
    // get GCD
    auto greatest_common_divisor = gcd(s);
    if(greatest_common_divisor.stat != status::success)
      return greatest_common_divisor;

    return ((abs() / greatest_common_divisor) * s.abs()).to_exact(); // LCM
  }

  /******************************************************************************
  * NATURAL LOGARITHM
  ******************************************************************************/

  Snum Snum::log() const noexcept {
    Snum tmp(*this);
    // ln(nan) = ln(-inf) = ln(-n) = ln(0) = NaN, ln(+inf) = +inf
    if(tmp.stat != status::success || tmp.is_zero() || tmp.is_neg()) {
      if(tmp.is_pos_inf()) tmp.set_pinf();
      else                 tmp.stat = status::nan;
      return tmp;
    }
    // Coerce Fractionals Into Floats 
    inexact_t ln_base = 1;
    if(tmp.is_float) {
      ln_base = tmp.float_num;
    } else {
      tmp.stat = coerce_fraction_to_float(ln_base,tmp.numerator,tmp.denominator,false); // ln args are never < 0
      if(tmp.stat != status::success) {
        if(tmp.is_pos_inf()) tmp.set_pinf();
        else                 tmp.stat = status::nan;
        return tmp;
      }
    }
    // Compute the Natural Log of 'ln_base'
    return Snum(std::log(ln_base)); 
  }

  /******************************************************************************
  * QUOTIENT & MODULO OF BEING DIV'D BY ARG
  ******************************************************************************/

  Snum Snum::quotient(const Snum& s) const noexcept {
    auto tmp = *this / s;
    if(tmp.stat != status::success) {tmp.set_failed_status(); return tmp;}
    if(!tmp.is_float) tmp = tmp.to_inexact();
    if(tmp.stat != status::success) {tmp.set_failed_status(); return tmp;}
    return tmp.trunc().to_exact();
  }

  Snum Snum::modulo(const Snum& s) const noexcept {
    // Credit for this Algorithm (modE) goes to Daan Leijen of the University of Utrecht. 
    // Proof (see page 5):
    // "https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf"
    auto rem = *this % s;
    if(rem.stat != status::success) {rem.set_failed_status(); return rem;}
    if(rem.is_neg())
      rem = s.is_pos() ? rem + s : rem - s;
    return rem;
  }

  /******************************************************************************
  * OVERLOADED EQUALITY/COMPARISON OPERATORS
  ******************************************************************************/

  bool Snum::operator==(const Snum& s) const noexcept {
    if((stat != status::success && stat == s.stat) || (is_zero() && s.is_zero()))
      return true;
    if(sign != s.sign) 
      return false;
    if(is_float && s.is_float)
      return float_num == s.float_num;
    if(!is_float && !s.is_float)
      return numerator == s.numerator && denominator == s.denominator;
    else
      return (*this - s).is_zero();
  }

  bool Snum::operator<(const Snum& s) const noexcept {
    if(is_nan() || s.is_nan() || is_pos_inf() || s.is_neg_inf() || (is_zero() && s.is_zero())) 
      return false;
    if(is_neg_inf() || s.is_pos_inf())
      return true;
    if(sign != s.sign)
      return is_neg() || (is_zero() && s.is_pos());

    if(is_float && s.is_float)
      return float_num < s.float_num;
    else if(!is_float && !s.is_float) {
      exact_t lhs = big_int_abs_val_mul(numerator,s.denominator);
      exact_t rhs = big_int_abs_val_mul(s.numerator,denominator);
      return big_int_lt(lhs,rhs);
    } else {
      return (*this - s).is_neg();
    }
  }

  bool Snum::operator>(const Snum& s) const noexcept {
    if(is_nan() || s.is_nan() || is_neg_inf() || s.is_pos_inf() || (is_zero() && s.is_zero())) 
      return false;
    if(is_pos_inf() || s.is_neg_inf())
      return true;
    if(sign != s.sign)
      return is_pos() || (is_zero() && s.is_neg());

    if(is_float && s.is_float)
      return float_num > s.float_num;
    else if(!is_float && !s.is_float) {
      exact_t lhs = big_int_abs_val_mul(numerator,s.denominator);
      exact_t rhs = big_int_abs_val_mul(s.numerator,denominator);
      return big_int_gt(lhs,rhs);
    } else {
      return (*this - s).is_pos();
    }
  }

  /******************************************************************************
  * LOGICAL SHIFT LEFT
  ******************************************************************************/

  Snum Snum::operator<<(const Snum& rhs) const noexcept {
    // num << inf = num << NaN = num << <negative-or-inexect_t> = NaN
    if(stat != status::success || rhs.stat != status::success || 
      !rhs.is_integer() || rhs.is_neg()) {
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // 0 << num = 0, num << 0 = num
    if(is_zero() || rhs.is_zero()) return *this;
    // pos-num << n = pos-num * 2^n
    if(!is_neg()) return *this * Snum("2").expt(rhs);
    // Extract binary string
    exact_t bin_str;
    if(!get_non_fraction_binary_string(*this,bin_str,true)){ // <<ing a negative
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Logical Left Shift
    const auto shift_amount = (size_type)rhs.extract_inexact();
    if(auto pos = bin_str.find('.'); pos != exact_t::npos) { // shifting an inexact_t
      if(pos+1 == bin_str.size()) {
        bin_str.insert(pos,shift_amount,'0'); // pad 0s
      } else {
        bin_str.append(shift_amount, '0');    // pad 0s
        bin_str.erase(pos,1);                 // rm '.'
        bin_str.insert(pos+shift_amount,"."); // reinsert '.' shifted over
      }
    } else { // shifting exact_t
      bin_str.append(shift_amount, '0');      // pad 0s
    }
    // Revert binary string to a decminal reresentation
    return big_int_revert_BINARY_to_Snum(bin_str, bin_str[0] == '1');
  }

  /******************************************************************************
  * LOGICAL SHIFT RIGHT
  ******************************************************************************/

  Snum Snum::operator>>(const Snum& rhs) const noexcept {
    return GENERIC_SHIFT_RIGHT<true>(rhs);
  }

  /******************************************************************************
  * ARITHMETIC SHIFT RIGHT
  ******************************************************************************/

  Snum Snum::asr(const Snum& rhs) const noexcept {
    return GENERIC_SHIFT_RIGHT<false>(rhs);
  }

  /******************************************************************************
  * BITWISE NOT
  ******************************************************************************/

  Snum Snum::operator~() const noexcept {
    // ~inf = ~NaN = NaN
    if(stat != status::success){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // ~0 = 1
    if(is_zero()) return Snum("1");
    // ~(-1) = 0
    if(is_negative_one()) return Snum(); 
    // Extract binary string
    exact_t bin_str;
    if(!get_non_fraction_binary_string(*this,bin_str,true)){ // "~" is ALWAYS a signed operation
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Bitwise NOT
    exact_t bin_str_result(bin_str.size(), '0');
    for(auto i = bin_str.size();i-- > 0;)
      bin_str_result[i] = char(bin_str[i] == '0') + '0';
    // Revert binary string to a decminal reresentation
    return big_int_revert_BINARY_to_Snum(bin_str_result, 
      bin_str_result[0] == '1');
  }

  /******************************************************************************
  * BITWISE AND
  ******************************************************************************/

  Snum Snum::operator&(const Snum& rhs) const noexcept {
    // num & inf = num & NaN = NaN
    if(stat != status::success || rhs.stat != status::success) {
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // num & 0 = 0
    if(is_zero() || rhs.is_zero()) return Snum(); 
    // num & -1 = num & num = num
    if(is_negative_one()) return rhs; 
    if(rhs.is_negative_one() || *this == rhs) 
      return *this;
    // Extract binary strings
    exact_t bin_str_lhs, bin_str_rhs;
    if(!get_non_fraction_binary_strings(*this,bin_str_lhs,rhs,bin_str_rhs)){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Bitwise AND
    exact_t bin_str_result(bin_str_lhs.size(), '0');
    for(auto i = bin_str_lhs.size(); i-- > 0;)
      bin_str_result[i] = char(bin_str_lhs[i] == '1' && bin_str_rhs[i] == '1') + '0';
    // Revert binary string to a decminal reresentation
    return big_int_revert_BINARY_to_Snum(bin_str_result, 
      (is_neg() || rhs.is_neg()) && bin_str_result[0] == '1');
  }

  /******************************************************************************
  * BITWISE OR
  ******************************************************************************/

  Snum Snum::operator|(const Snum& rhs) const noexcept {
    // num | inf = num | NaN = NaN
    if(stat != status::success || rhs.stat != status::success) {
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // num | 0 = num | num = num
    if(is_zero()) return rhs;
    if(rhs.is_zero() || *this == rhs) return *this;
    // num | -1 = -1
    if(is_negative_one()) return *this; 
    if(rhs.is_negative_one()) return rhs;
    // Extract binary strings
    exact_t bin_str_lhs, bin_str_rhs;
    if(!get_non_fraction_binary_strings(*this,bin_str_lhs,rhs,bin_str_rhs)){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Bitwise OR
    exact_t bin_str_result(bin_str_lhs.size(), '0');
    for(auto i = bin_str_lhs.size(); i-- > 0;)
      bin_str_result[i] = char(bin_str_lhs[i] == '1' || bin_str_rhs[i] == '1') + '0';
    // Revert binary string to a decminal reresentation
    return big_int_revert_BINARY_to_Snum(bin_str_result, 
      (is_neg() || rhs.is_neg()) && bin_str_result[0] == '1');
  }

  /******************************************************************************
  * BITWISE XOR
  ******************************************************************************/

  Snum Snum::operator^(const Snum& rhs) const noexcept {
    // num ^ inf = num ^ NaN = NaN
    if(stat != status::success || rhs.stat != status::success) {
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // num ^ 0 = num
    if(is_zero())     return rhs;
    if(rhs.is_zero()) return *this;
    // num ^ num = 0
    if(*this == rhs) return Snum();
    // num ^ -1 = ~num
    if(is_negative_one()) return ~rhs; 
    if(rhs.is_negative_one()) return ~(*this);
    // Extract binary strings
    exact_t bin_str_lhs, bin_str_rhs;
    if(!get_non_fraction_binary_strings(*this,bin_str_lhs,rhs,bin_str_rhs)){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Bitwise XOR
    exact_t bin_str_result(bin_str_lhs.size(), '0');
    for(auto i = bin_str_lhs.size(); i-- > 0;)
      bin_str_result[i] = char((bin_str_lhs[i]=='1') ^ (bin_str_rhs[i]=='1')) + '0';
    // Revert binary string to a decminal reresentation
    return big_int_revert_BINARY_to_Snum(bin_str_result, 
      (is_neg() || rhs.is_neg()) && bin_str_result[0] == '1');
  }

  /******************************************************************************
  * PARITY CHECK
  ******************************************************************************/

  bool Snum::is_even() const noexcept {
    if(stat == status::nan)     return false;
    if(stat != status::success) return true; // +inf.0 & -inf.0 are even
    if(is_zero())               return true; // 0 is even
    if(is_float) return std::fmod(float_num,2) == 0;
    if(EXACT_T_IS_1(denominator)) // check if bigint is even
      return !numerator.empty() && ((*numerator.rbegin() & 1) == 0);
    inexact_t num = 0;
    auto coerced=coerce_fraction_to_float(num,numerator,denominator,is_neg());
    if(coerced == status::pinf || coerced == status::ninf) return true;
    return (coerced == status::success) && (std::fmod(num,2) == 0);
  }

  /******************************************************************************
  * RATIONALITY CHECK
  ******************************************************************************/

  bool Snum::is_rational() const noexcept {
    if(stat != status::success) return false; // Inf & NaN are irrational
    if(!is_float)               return true;  // exact numbers are rational
    return std::abs(float_num) < RATIONALITY_LIMIT;
  }

  /******************************************************************************
  * CIO STRING (TO_STRING THE CURRENT VALUE)
  ******************************************************************************/

  // get current value as a string (for c-style I/O)
  std::string Snum::cpp_str() const noexcept {
    // Return NaN/Inf as needed
    if(stat != Snum::status::success) {
      if(is_nan())     return "+nan.0";
      if(is_neg_inf()) return "-inf.0";
      return "+inf.0";
    }
    // Return Numeric Value as std::string
    if(is_float) {
      char str[64];
      str[0] = is_neg() * '-'; // Prepend Negative Sign as Needed
      sprintf(str+is_neg(), LD_SPRINTF_FORMAT(LDBL_DIG), float_num);
      if(float_num >= Snum::RATIONALITY_LIMIT || !is_integer())
        return str;
      char* p = str; // Print ".0" after flonums w/o a fractional
      while(*(++p));
      *p++ = '.'; *p++ = '0'; *p = 0;
      return str;
    } else if(EXACT_T_IS_1(denominator)) {
      if(is_pos()) return numerator;
      return "-" + numerator;
    } else {
      if(is_pos()) return numerator + '/' + denominator;;
      return "-" + numerator + '/' + denominator;;
    }
  }

  // get current value as a string in 'base' radix form
  std::string Snum::cpp_str(const int& base) const noexcept {
    return convert_dec_to_base_N(base, *this);
  }

  /******************************************************************************
  * BITWISE OPERATION HELPERS
  ******************************************************************************/

  // Fast comparison for whether *this == -1
  bool Snum::is_negative_one() const noexcept {
    return stat == status::success && sign == signs::neg && 
      ((!is_float && EXACT_T_IS_1(numerator) && EXACT_T_IS_1(denominator)) ||
       (is_float && float_num == 1.0L));
  }

  // Performs 2's complement negation on binary string bin_str
  void Snum::big_int_BINARY_2sCMPL_NEGATION(exact_t& bin_str) const noexcept {
    auto i = bin_str.size();
    for(;i-- > 0 && bin_str[i] != '1';); // seek the rightmost '1'
    if(i+1 == 0) return;
    for(;i-- > 0;) // complement each bit afterwards
      bin_str[i] = char(bin_str[i] - (bin_str[i] == '1') + (bin_str[i] == '0'));
  }

  // Extend bit_str1 w/ b1 & bit_str2 w/ b2 so they have the same width
  // PRECONDITION: bit_str1 & bit_str2 MUST be NON-EMPTY!
  void Snum::big_int_BINARY_extend(exact_t& bit_str1, exact_t& bit_str2, 
                                          const char& b1,    const char& b2) const noexcept {
    if(bit_str1.size() > bit_str2.size()) {
      bit_str2.insert(0, exact_t(bit_str1.size() - bit_str2.size(), b2));
    } else if(bit_str1.size() < bit_str2.size()) {
      bit_str1.insert(0, exact_t(bit_str2.size() - bit_str1.size(), b1));
    }
  }

  // Sign-extend bit_str1 & bit_str2 to match lengths
  // PRECONDITIONS: 1) bit_str1 & bit_str2 MUST be NON-EMPTY!
  //                2) sign bits MUST be present in BOTH bit_str1 & bit_str2
  void Snum::signed_big_int_BINARY_extend(exact_t& bit_str1, 
                                                 exact_t& bit_str2) const noexcept {
    big_int_BINARY_extend(bit_str1,bit_str2,bit_str1[0],bit_str2[0]);
  }

  // Unsigned-extend bit_str1 & bit_str2 to match lengths
  // PRECONDITION: bit_str1 & bit_str2 MUST be NON-EMPTY!
  void Snum::unsigned_big_int_BINARY_extend(exact_t& bit_str1, 
                                                   exact_t& bit_str2) const noexcept {
    big_int_BINARY_extend(bit_str1,bit_str2,'0','0');
  }

  // Convert possibly-signed binary string to a Snum
  Snum Snum::big_int_revert_BINARY_to_Snum(exact_t bin_str, 
                                        const bool& is_negative_binary) const noexcept {
    if(is_negative_binary) {
      big_int_BINARY_2sCMPL_NEGATION(bin_str);
      return -(Snum(bin_str,2));
    }
    return Snum(bin_str,2);
  }

  // Convert a Snum to a no-fraction binary string 
  //   (degrades fractions to inexact_t & returns success status)
  bool Snum::get_non_fraction_binary_string(Snum n, exact_t& bit_str, 
                                                   const bool& is_signed) const noexcept {
    // Coerce fractions to inextact_t as needed
    if(n.stat == status::success && !n.is_float && !EXACT_T_IS_1(n.denominator)){
      n = n.to_inexact();
    }
    if(n.stat != status::success) return false;
    // Get number as unsigned binary string of its absolute value
    bit_str = n.cpp_str(2);
    if(bit_str[0] == '-') bit_str.erase(0,1);
    // Account for sign
    if(is_signed) {
      if(n.is_neg()) big_int_BINARY_2sCMPL_NEGATION(bit_str);
      bit_str = char('0' + char(n.is_neg())) + bit_str;
    }
    return true;
  }

  // Convert 2 Snums to no-fraction binary strings 
  //   (degrades fractions to inexact_t & returns success status)
  bool Snum::get_non_fraction_binary_strings(const Snum& n1, exact_t& bit_str1, 
                                                    const Snum& n2, exact_t& bit_str2)const noexcept{
    const bool is_signed = n1.is_neg() || n2.is_neg();
    if(!get_non_fraction_binary_string(n1,bit_str1,is_signed) ||
       !get_non_fraction_binary_string(n2,bit_str2,is_signed)) {
      return false;
    }
    if(is_signed)
      signed_big_int_BINARY_extend(bit_str1, bit_str2);
    else
      unsigned_big_int_BINARY_extend(bit_str1, bit_str2);
    return true;
  }

  // Template outlining structure of LSR & ASR (logical/arithmetic shift right)
  template<bool LOGIC_RIGHT_SHIFT>
  Snum Snum::GENERIC_SHIFT_RIGHT(const Snum& rhs) const noexcept {
    // num >> inf = num >> NaN = num >> <negative-or-inexect_t> = NaN
    if(stat != status::success || rhs.stat != status::success || 
      !rhs.is_integer() || rhs.is_neg()) {
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // 0 >> num = 0, num >> 0 = num
    if(is_zero() || rhs.is_zero()) return *this;
    // Extract binary string
    exact_t bin_str;
    if(!get_non_fraction_binary_string(*this,bin_str,is_neg())){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Logical/Arithmetic Right Shift
    const auto shift_amount = (size_type)rhs.extract_inexact();
    char fill_char = 0;
    if constexpr (LOGIC_RIGHT_SHIFT) fill_char = '0'; else fill_char = char(is_neg() + '0');
    if(auto pos = bin_str.find('.'); pos != exact_t::npos) { // shifting an inexact_t
      if(pos == 0) {
        bin_str.insert(1,shift_amount,fill_char);            // pad 0s
      } else {
        bin_str = exact_t(shift_amount,fill_char) + bin_str; // pad 0s
        bin_str.erase(pos+shift_amount,1);                   // rm '.'
        bin_str.insert(pos,".");                             // reinsert '.' shifted over
      }
    } else { // shifting exact_t
      bin_str = exact_t(shift_amount,fill_char) + bin_str;   // pad 0s
    }
    bin_str.erase(bin_str.size()-shift_amount);
    // Revert binary string to a decminal reresentation
    return big_int_revert_BINARY_to_Snum(bin_str, is_neg() && bin_str[0] == '1');
  }

  /******************************************************************************
  * EXPONENTIATION HELPER FUNCTIONS
  ******************************************************************************/

  // Decomposes numerator into the quotient/remainder of dividing by SIZE_TYPE_MAX.
  //   This is in order to have C++ primitive numbers we can get the bits of to
  //   perform the repeated squares algorithm.
  // POSTCONDITION: inner pair's 1st = # of SIZE_TYPE_MAX factors the numerator had,
  //                inner pair's 2nd = remainder of numerator / SIZE_TYPE_MAX,
  //                success status contains the result of its attempted 
  //                        numerator coercions to floats
  std::pair<std::pair<Snum::size_type,Snum::size_type>,Snum::status> 
  Snum::decompose_int_into_SIZE_TYPE_MAX() const noexcept {
    const Snum unum_Snum(SIZE_TYPE_MAX);
    const auto div_res = (*this / unum_Snum), mod_res = (*this % unum_Snum);
    inexact_t div_flt, mod_flt;
    // Coerce Division Result to a Float
    if(div_res.is_float) {
      div_flt = div_res.float_num;
    } else {
      auto div_coercion = coerce_fraction_to_float(div_flt, div_res.numerator, 
                                                            div_res.denominator, 
                                                            div_res.is_neg());
      if(div_coercion != status::success)
        return std::make_pair(std::make_pair(0,0), div_coercion);
    }
    // Coerce remainder Result to a Float
    if(mod_res.is_float) {
      mod_flt = mod_res.float_num;
    } else {
      auto mod_coercion = coerce_fraction_to_float(mod_flt,mod_res.numerator,mod_res.denominator,mod_res.is_neg());
      if(mod_coercion != status::success)
        return std::make_pair(std::make_pair(0,0), mod_coercion);
    }
    // Cast Floats to 'size_type's => THE LOSS OF PRECISION IS INTENTIONAL.
    // -> div_flt's loss of precision is accounted for via 'mod_flt'
    // -> mod_flt's loss of precision is, __by definition__, 0%. This is because 
    //    performing remainder on 2 integers ('SIZE_TYPE_MAX' & the 'numerator' here) 
    //    ALWAYS produces an integer result.
    return std::make_pair(std::make_pair((size_type)div_flt, (size_type)mod_flt), status::success);
  }


  // Perform the repeated squares algorithm for polynomial-time exponentiation
  Snum Snum::repeated_squares(const Snum& a, const size_type& b) const noexcept { // a^b
    constexpr const int BITS_PER_BYTE = 8;
    Snum f("1");
    // REPEATED SQUARING A9: for each b bit, from left to right
    for(int i = sizeof(size_type)*BITS_PER_BYTE-1; i >= 0; --i) {
      f *= f;
      if((b >> i) & 1) f *= a; // if ith bit is 1
    }
    return f;
  }

  /******************************************************************************
  * ROUNDING HELPER FUNCTION
  ******************************************************************************/

  // Generic rounding fcn template for ceil, floor, trunc, & round
  Snum Snum::ROUNDING_GENERIC_FCN(const size_type ROUNDING_TYPE_ID) const noexcept {
    // nothing to round if not finite or an int (including 0)
    if(stat != status::success || is_integer()) return *this;
    Snum tmp(*this);
    // Coerce Fractional to Float as Needed
    inexact_t flt_val = 0;
    if(tmp.is_float)
      flt_val = tmp.is_neg() ? -tmp.float_num : tmp.float_num;
    else {
      tmp.stat = coerce_fraction_to_float(flt_val,tmp.numerator,tmp.denominator,tmp.is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status(); return tmp;
      }
    }
    // round the value
    inexact_t integral;
    inexact_t fractional = std::modf(flt_val, &integral);
    switch(ROUNDING_TYPE_ID) { // NOTE: 'trunc' (id=2) just returns the integral as-is
      case 0: if(fractional != 0 && integral > 0) integral += 1; break; // ceil
      case 1: if(fractional != 0 && integral < 0) integral -= 1; break; // floor
      case 3: if(std::abs(fractional) >= 0.5)                           // round
                integral += 1-2*(integral<0); // (integral < 0) ? -1 : 1;
    }
    tmp.float_num = integral;
    tmp.adjust_float_invariants();
    return tmp;
  }

  /******************************************************************************
  * UNSIGNED BIG INT STRING ARITHMETIC HELPER FUNCTIONS
  ******************************************************************************/

  // Returns whether big-int a > big-int b
  bool Snum::big_int_gt(const exact_t& a, const exact_t& b) const noexcept {
    return a.size() > b.size() || (a.size() == b.size() && a > b);
  }
  // Returns whether big-int a < big-int b
  bool Snum::big_int_lt(const exact_t& a, const exact_t& b) const noexcept {
    return a.size() < b.size() || (a.size() == b.size() && a < b);
  }

  // Sums 2 strings of decimal digits into a single string
  Snum::exact_t Snum::big_int_abs_val_sum(const exact_t& a, const exact_t& b) const noexcept{
    if(EXACT_T_IS_0(a)) return b;
    if(EXACT_T_IS_0(b)) return a;
    // get iterators to traverse the string with
    auto ch1 = a.rbegin(), ch2 = b.rbegin();
    const auto end1 = a.rend(), end2 = b.rend();
    exact_t sum; // reserve total possible digits in addition
    if(a.size() > b.size()) sum.reserve(a.size() + 1);
    else                    sum.reserve(b.size() + 1);
    char carry_over = 0;
    // add the 2 #s together
    for(; ch1 != end1 && ch2 != end2; ++ch1, ++ch2) {
      auto ch_sum = *ch1-'0' + *ch2-'0' + carry_over;
      carry_over = (ch_sum > 9);
      sum += ch_sum + '0' - 10 * carry_over;
    }
    // propogate the last 'carry_over' across the rest of 
    //   the digits of the larger # (if applicable)
    const auto& end_of_str = (ch1 != end1) ? end1 : end2;
    auto begin_of_str = (ch1 != end1) ? ch1 : ch2;
    while(begin_of_str != end_of_str) {
      auto ch_sum = *begin_of_str-'0' + carry_over;
      carry_over = (ch_sum > 9);
      sum += ch_sum + '0' - 10 * carry_over;
      ++begin_of_str;
    }
    // append the final "carry_over" bit, IF = 1
    if(carry_over == 1) sum += '1';
    // return the # in reverse (since appended addition 
    //   result starting from the 'ones' position)
    return exact_t(sum.rbegin(), sum.rend());
  }


  // Multiplies 2 strings of decimal digits into a single string
  Snum::exact_t Snum::big_int_abs_val_mul(const exact_t& a, const exact_t& b) const noexcept {
    if(EXACT_T_IS_0(a) || EXACT_T_IS_0(b)) return "0";
    if(EXACT_T_IS_1(a))                    return b;
    if(EXACT_T_IS_1(b))                    return a;
    const bool a_is_gt_b = big_int_gt(a,b);
    const exact_t& big   = a_is_gt_b ? a : b;
    const exact_t& small = a_is_gt_b ? b : a;
    exact_t product; product.reserve(a.size() + b.size()); product += '0'; // product of *
    exact_t digit_mul_instance; digit_mul_instance.reserve(big.size()+1);
    char carry_over = 0;
    size_type i = 0;
    const auto &end1 = small.rend(), &end2 = big.rend();
    const auto& start2 = big.rbegin();
    // for each digit in the small#
    for(auto ch1 = small.rbegin(); ch1 != end1; ++ch1, ++i, carry_over=0) { 
      digit_mul_instance.clear(); // pad i zeros
      for(size_type j = 0; j != i; ++j) digit_mul_instance += '0';
      for(auto ch2 = start2; ch2 != end2; ++ch2) { // for each digit in the big#
        char mulres = (*ch1-'0') * (*ch2-'0') + carry_over;
        carry_over = mulres / 10;                  // 10's digit is carried over
        digit_mul_instance += (mulres % 10) + '0'; // 1's digit is accounted for to be summed later
      }
      // add carry_over if present
      if(carry_over > 0) digit_mul_instance += carry_over+'0';
      // Add small#'s digit-multiplication w/ the big# to the product
      big_int_abs_val_MUL_HELPER_sum(product,digit_mul_instance);
    }
    return exact_t(product.rbegin(), product.rend());
  }


  // MUL '* HELPER: Sums 2 strings of decimal digits into a single string
  void Snum::big_int_abs_val_MUL_HELPER_sum(exact_t& a, const exact_t& b) const noexcept{ // <a> = product
    if(EXACT_T_IS_0(a)) {a.clear(); a += b; return;}
    if(EXACT_T_IS_0(b)) return;
    // get iterators to traverse the string with
    auto ch1 = a.begin(), end1 = a.end();
    auto ch2 = b.begin(), end2 = b.end();
    char carry_over = 0;
    // add the 2 #s together
    for(; ch1 != end1 && ch2 != end2; ++ch2) {
      char ch_sum = *ch1-'0' + *ch2-'0' + carry_over;
      carry_over = (ch_sum > 9);
      *ch1++ = ch_sum + '0' - 10 * carry_over;
    }
    // NOTE: only 1 (if any at all) of the below 2 while loops are ever invoked!
    while(ch1 != end1) { // propogate the last 'carry_over' across the rest of a
      char ch_sum = *ch1-'0' + carry_over;
      carry_over = (ch_sum > 9);
      *ch1++ = ch_sum + '0' - 10 * carry_over;
    }
    while(ch2 != end2) { // propogate the last 'carry_over' across the b into a
      char ch_sum = *ch2-'0' + carry_over;
      carry_over = (ch_sum > 9);
      a += ch_sum + '0' - 10 * carry_over;
      ++ch2;
    }
    if(carry_over == 1) a += '1'; // append the final "carry_over" bit, IF = 1
  }


  // Returns the absolute difference (in a single string) 
  //   of 2 strings of decimal digits
  Snum::exact_t Snum::big_int_abs_val_diff(const exact_t& a, const exact_t& b) const noexcept {
    if(EXACT_T_IS_0(a)) return b;
    if(EXACT_T_IS_0(b)) return a;
    if(a == b)          return "0";
    const bool a_is_gt_b = big_int_gt(a,b);
    const exact_t& big   = a_is_gt_b ? a : b;
    exact_t small; small.reserve(big.size());
    // pad the front of 'small' w/ 0's
    if(a.size() != b.size()) {
      if(a_is_gt_b) {
        for(size_type i = 0, padding_length = a.size()-b.size(); i<padding_length; ++i) small += '0';
        for(const auto& ch : b) small += ch; // now small.size() = big.size()
      } else {
        for(size_type i = 0, padding_length = b.size()-a.size(); i<padding_length; ++i) small += '0';
        for(const auto& ch : a) small += ch; // now small.size() = big.size()
      }
    } else if(a_is_gt_b) {
      small += b;
    } else {
      small += a;
    }
    // perform subtraction
    exact_t diff; diff.reserve(big.size());
    char carry_over = 0;
    const auto& end1 = big.rend();
    auto ch1 = big.rbegin();
    for(auto ch2 = small.rbegin(); ch1 != end1; ++ch1, ++ch2) {
      // carry the one (or 0) as needed
      char small_digit = *ch2-'0' + carry_over; 
      char big_digit = *ch1-'0';
      carry_over = (big_digit < small_digit);
      // add 10 to the big# digit if carrying over
      diff += ((big_digit + (carry_over * 10)) - small_digit) + '0'; 
    }

    // rm extra 0 padding front of sub (if present)
    size_type diff_start_offset = 0;
    for(auto zero_test = diff.rbegin(), last = diff.rend(); zero_test != last; ++zero_test) {
      if(*zero_test != '0') break;
      ++diff_start_offset;
    }
    return exact_t(diff.rbegin()+diff_start_offset, diff.rend()); // sub'd right->left
  }


  // returns gcd of flt a & b
  // PRECONDITION: a > b
  constexpr Snum::inexact_t Snum::inexact_t_GCD(const inexact_t a, const inexact_t b) const noexcept {
    if(b == 0) return a;
    return inexact_t_GCD(b,std::fmod(a,b));
  }

  /******************************************************************************
  * UNDERLYING CPP-INVOCATION HELPER FUNCTION
  ******************************************************************************/

  // Generic cpp-fcn-invocation template
  Snum Snum::UNDERLYING_CPP_GENERIC_FCN(const size_type CPP_FCN_TYPE_ID) const noexcept {
    Snum tmp;
    constexpr size_type EXP_ID = 12;
    // Fcn (non-exp) of NaN or Inf is NaN
    if(stat == status::nan || (stat != status::success && CPP_FCN_TYPE_ID != EXP_ID)) {
      tmp.stat = status::nan; return tmp;
    } else if(CPP_FCN_TYPE_ID == EXP_ID && is_pos_inf()) { // e^+inf = +inf
      tmp.set_pinf(); return tmp;
    } else if(CPP_FCN_TYPE_ID == EXP_ID && is_neg_inf()) { // e^-inf = 0
      return Snum();
    }
    // Coerce Fractional to Float as Needed
    inexact_t flt_val = 0;
    if(is_float)
      flt_val = is_neg() ? -float_num : float_num;
    else {
      tmp.stat = coerce_fraction_to_float(flt_val,numerator,denominator,is_neg());
      if(tmp.stat != status::success) {
        tmp.set_failed_status(); return tmp;
      }
    }
    // retrieve the cpp fcn's value
    switch(CPP_FCN_TYPE_ID) {
      case 0:  flt_val = std::sin(flt_val); break; // sin
      case 1:  flt_val = std::cos(flt_val); break; // cos
      case 2:  flt_val = std::tan(flt_val); break; // tan

      case 3:  flt_val = std::asin(flt_val); break; // asin
      case 4:  flt_val = std::acos(flt_val); break; // acos
      case 5:  flt_val = std::atan(flt_val); break; // atan

      case 6:  flt_val = std::sinh(flt_val); break; // sinh
      case 7:  flt_val = std::cosh(flt_val); break; // cosh
      case 8:  flt_val = std::tanh(flt_val); break; // tanh

      case 9:  flt_val = std::asinh(flt_val); break; // asinh
      case 10: flt_val = std::acosh(flt_val); break; // acosh
      case 11: flt_val = std::atanh(flt_val); break; // atanh

      case 12: flt_val = std::exp(flt_val); break; // exp
    }
    tmp.float_num = flt_val;
    tmp.adjust_float_invariants();
    return tmp;
  }

  /******************************************************************************
  * CONSTRUCT/PARSE NUMBER HELPER FCNS
  ******************************************************************************/

  // rm whitespace from number string & trims padding 0's
  void Snum::trim_data_string(exact_t& num_str) noexcept {
    exact_t trimmed;
    for(const auto& ch : num_str) if(!isspace(ch)) trimmed += ch;
    if(trimmed.size()<2) {
      num_str=trimmed;
      sign = (!trimmed.empty() && trimmed[0] != '0') ? signs::pos : signs::zero;
      return;
    }

    // Determine the number's sign (& erase if present)
    if(trimmed[0] == '-') sign = signs::neg;
    else                  sign = signs::pos;
    if(trimmed[0] == '-' || trimmed[0] == '+') trimmed.erase(trimmed.begin());
    size_type i = 0; 

    // erase prefixing 0's
    while(trimmed[i+1] == '0' && trimmed[i] == '0') 
      trimmed.erase(i,1);

    if(trimmed.size()<2) {num_str=trimmed;return;}

    // erase tailing 0's IFF number is floating point
    if(trimmed.find('.') != exact_t::npos) {
      i = trimmed.size()-1;
      while(i > 0 && trimmed[i] == '0' && trimmed[i-1] == '0') 
        trimmed.erase(i,1);
    }
    num_str = trimmed;
  }


  // Confirm given string is a viable candidate to be a number
  Snum::precisions Snum::confirm_valid_string(const exact_t& num_str) noexcept {
    exact_t str(num_str);
    trim_data_string(str);
    if(str.empty()) return precisions::invalid;

    bool found_decimal=false, found_division=false, found_exponential=false;
    size_type i = 0;
    if(str[i] == '-' || str[i] == '+') ++i; // mv past sign

    while(str[i]) {
      if(str[i] == '/') {
        // invalid dual division OR inexact & exact division
        if(found_division || found_decimal || found_exponential) return precisions::invalid;
        found_division = true;
      } else if(str[i] == '.') {
        // invalid dual decimal points OR inexact & exact division OR decimal after the exponential
        if(found_division || found_decimal || found_exponential) return precisions::invalid;
        found_decimal = true;
      } else if(str[i]=='e' || str[i]=='E') {
        // invalid dual exponents OR inexact & exact division
        if(found_division || found_exponential) return precisions::invalid;
        found_exponential = true;
        if(str[i+1] == '-' || str[i+1] == '+') ++i;       // mv past optional sign after exponential
        if(!isdigit(str[i+1]))return precisions::invalid; // must have a digit following the exponential
      } else if(!isdigit(str[i])) {
        return precisions::invalid;                       // otherwise, MUST be a digit
      }
      ++i;
    }
    // confirm that '/' has nums on either side
    if(found_division) { 
      i = str.size()-1;
      while(i > 0 && str[i] != '/' && !isdigit(str[i])) --i;
      if(str[i] == '/') return precisions::invalid;
      i = 0;
      while(str[i] && str[i] != '/' && !isdigit(str[i])) ++i;
      if(str[i] == '/') return precisions::invalid;
    }
    // confirm actually contains a digit
    i = 0; 
    while(str[i]) {
      if(isdigit(str[i])) 
        return (found_decimal || found_exponential) ? precisions::inexact 
                                                    : precisions::exact;
      ++i;
    }
    return precisions::invalid;
  }

  /******************************************************************************
  * CONSTRUCT/PARSE NUMBER MAIN FCNS
  ******************************************************************************/

  // Direct ctor for <inexact_t>s (no indirection via <std::to_string> needed)
  Snum::Snum(const inexact_t& data) noexcept {
    if(data == 0) return;
    is_float = true, float_num = data;
    if(data < 0) {
      sign = signs::neg;
      float_num *= -1;
    } else {
      sign = signs::pos;
    }
  }


  // Parse the _confirmed_ valid number
  void Snum::parse_integer(exact_t::iterator ch) noexcept {
    numerator = "";                                      // empty dflt value for numerator
    while(*ch == '0' && *(ch+1) && *(ch+1) != '/') ++ch; // rm padding lhs 0s
    while(*ch && *ch != '/')  {numerator += *ch; ++ch;}
    if(*ch == '/') {
      ++ch;                                              // mv past '/'
      if(*ch) denominator = "";                          // empty dflt value for denominator
      while(*ch == '0' && *(ch+1)) ++ch;                 // rm padding rhs 0s
      while(*ch) {denominator += *ch; ++ch;}
    }
    else denominator = '1';
  }


  // Assign NaN or Inf if given such as a string, and return whether did so
  bool Snum::is_irrational(const exact_t& num_str) noexcept {
    if(num_str == "+inf.0") { set_pinf(); return true; }
    if(num_str == "-inf.0") { set_ninf(); return true; }
    if(num_str == "+nan.0" || num_str == "-nan.0") { 
      stat=status::nan; return true; 
    }
    return false;
  }


  // Construct number out of the given string
  void Snum::construct_number(exact_t num_str) noexcept {
    // Check if NaN or Inf
    if(is_irrational(num_str)) return;
    // Confirm given valid number
    auto precision = confirm_valid_string(num_str);
    if(precision == precisions::invalid) {
      stat = status::nan; return;
    }
    if(precision == precisions::inexact) is_float = true;
    trim_data_string(num_str);
    // Determine the number's value
    if(precision == precisions::exact) {
      parse_integer(num_str.begin()); // parse the integer exact fraction
      if(EXACT_T_IS_0(denominator)) 
        stat = status::nan, numerator = "0"; // can't divide by 0
    } else {
      stat = coerce_int_to_float(float_num, num_str, is_neg());
    }
    if(stat == status::success &&
       ((precision == precisions::exact && EXACT_T_IS_0(numerator)) || 
        (precision != precisions::exact && float_num == 0)))
      set_zero();
    simplify_numerics();
  }

  /******************************************************************************
  * BASE CONVERSION INTEGER HELPER FCNS
  ******************************************************************************/

  // Confirms <base> is w/in range of possible number systems
  constexpr bool Snum::confirm_base_in_range(const int& base) const noexcept {
    return base > 1 && base < 37;
  }


  // Confirms <bnum> only contains digits in range of the <base> number system
  bool Snum::confirm_valid_base_digits(const int& base, const exact_t& bnum) const noexcept {
    for(const auto& digit : bnum)
      if(!is_base_digit(digit,base))
        return false;
    return true;
  }


  // Returns <digit>'s base-36 representation
  const char * Snum::base_digit_conversion(const Snum& digit, const int& base) const noexcept {
    static constexpr const char * const base_36_digits[] = {
      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
      "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", 
      "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
      "U", "V", "W", "X", "Y", "Z"
    };
    return base_36_digits[std::stoi(digit.round().extract_exact()) % base];
  }


  // Evaluates the numerator & denominator of a base-N # & divides their results
  Snum Snum::form_decimal_fraction(const int& base,const size_type& i,const exact_t& bnum,const bool& is_neg)const noexcept{
    if(i == 0) {
      Snum tmp; tmp.stat = status::nan; return tmp; // Invalid Snum Given: Can't begin with '/'!
    }
    return convert_base_N_to_dec(base,bnum.substr(0,i)) 
            / convert_base_N_to_dec(base,bnum.substr(i+1, bnum.size()-i-1)) 
            * (is_neg ? -1 : 1);
  }


  // Evaluates the integral & fractional of a base-N # & combines their results
  Snum Snum::form_decimal_floating_pt(const int& base,const size_type& i,const exact_t& bnum,const bool& is_neg)const noexcept{
    auto fractional = convert_base_N_decimal_to_dec(base,bnum.substr(i+1, bnum.size()-i-1));
    if(fractional == "+nan.0") {Snum tmp; tmp.stat = status::nan; return tmp;}
    if(i == 0) return Snum("0." + fractional) * (is_neg ? -1 : 1);
    return (convert_base_N_to_dec(base,bnum.substr(0,i)) + Snum("0." + fractional)) * (is_neg ? -1 : 1);
  }


  // Evaluates the numerator & denominator of a decimal # & divides their results
  Snum::exact_t Snum::form_base_N_fraction(const int& base,const Snum& dnum) const noexcept {
    auto numer = convert_dec_to_base_N(base,dnum.numerator);
    if(numer == "+nan.0") return numer;
    if(!EXACT_T_IS_1(dnum.denominator)) {
      auto denom = convert_dec_to_base_N(base,dnum.denominator);
      if(denom == "+nan.0") return denom;
      if(dnum.is_neg()) return '-' + numer + '/' + denom;
      return numer + '/' + denom;
    }
    if(dnum.is_neg()) return '-' + numer;
    return numer;
  }


  // Evaluates the integral & fractional of a decimal # & combines their results
  Snum::exact_t Snum::form_base_N_floating_pt(const int& base, const Snum& dnum) const noexcept {
    exact_t dnum_str = convert_numeric_to_str(dnum.float_num);
    size_type decimal_idx = dnum_str.find_first_of('.');
    if(decimal_idx == 1 && dnum_str[0] == '0') // only a fractional, no integral
      return (dnum.is_neg() ? "-0." : "0.") + 
              convert_dec_decimal_to_base_N(base,dnum_str.substr(
                decimal_idx+1, dnum_str.size()-decimal_idx-1)
              );
    auto integral_part = convert_dec_to_base_N(base,dnum_str.substr(0,decimal_idx));
    if(integral_part == "+nan.0") return integral_part;
    return (dnum.is_neg() ? "-" : "") + integral_part + "." + 
              convert_dec_decimal_to_base_N(base,dnum_str.substr(
                decimal_idx+1, dnum_str.size()-decimal_idx-1)
              );
  }

  /******************************************************************************
  * BASE CONVERSION INTEGER MAIN FCNS
  ******************************************************************************/

  // Convert a base-n big int string into a decimal Snum
  // PRECONDITION: <bnum> must be an INTEGER
  Snum Snum::convert_base_N_to_dec(const int& base, exact_t bnum) const noexcept {
    if(!confirm_base_in_range(base)) {
      // Invalid Snum Base Given, Must be in Range: [2,36]!
      Snum tmp; tmp.stat = status::nan; return tmp; 
    }
    if(base == 10 || EXACT_T_IS_0(bnum) || bnum == "0.0")
      return bnum; // return if nothing to convert

    // Account for sign
    bool is_neg = (bnum[0] == '-');
    if(bnum[0] == '-' || bnum[0] == '+') bnum.erase(0,1); // rm sign

    // Account for division & floating points
    for(size_type i = 0, n = bnum.size(); i < n; ++i) {
      if(bnum[i] == '/')
        return form_decimal_fraction(base,i,bnum,is_neg);
      if(bnum[i] == '.')
        return form_decimal_floating_pt(base,i,bnum,is_neg);
    }

    // Confirm valid digit use
    if(!confirm_valid_base_digits(base,bnum)){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }

    // ------------------------------------------------------------------------------
    // APPROACH: 
    //   1) From right to left, break the base# string <bnum> into substrings 
    //      of the maximum length that can be handled by "std::stoull".
    //   2) Sum the "std::stoull" of each substring multiplied by <base> to the power
    //      of the # digit the substring would begin at relative to its position 
    //      in <bnum>, counting said digits from right to left.
    // ------------------------------------------------------------------------------
    // MAX_STOULL_LENGTH = max-number-of-bits / bits-per-base-digit
    const auto MAX_STOULL_LENGTH = sizeof(unsigned long long) * 8 / size_type(std::ceil(std::log2(base)));
    const auto n = bnum.size();

    // Start of base# substring to splice out, & current digit #
    size_type start = n > MAX_STOULL_LENGTH ? n-MAX_STOULL_LENGTH : n;
    size_type digit_count = 0;
    Snum decimal_num;

    // While-Loop ONLY initially launches if must account for digits to the 
    //   right of <start>. If triggered, functionally becomes while(true)
    while(n > MAX_STOULL_LENGTH) {
      // perform "2)" from "APPROACH" above
      decimal_num += std::stoull(bnum.substr(start,MAX_STOULL_LENGTH),nullptr,base) * (Snum(base).expt(digit_count));
      digit_count += MAX_STOULL_LENGTH;     // account for the current digit#
      if(start <= MAX_STOULL_LENGTH) break; // no more substrings, break
      start -= MAX_STOULL_LENGTH;           // shift <start> to the next substr
    }

    // Account for digits remaining to the left of <start>
    if(start) decimal_num += std::stoull(bnum.substr(0, start),nullptr,base) * (Snum(base).expt(digit_count));
    return (is_neg) ? (-1 * decimal_num) : decimal_num;
  }


  // Converts the <dnum> decimal # into a <base> #
  //   => NOTE: approximates the conversion if dnum > INEXACT_PRECISION
  // PRECONDITION: <dnum> must be an INTEGER
  Snum::exact_t Snum::convert_dec_to_base_N(const int& base, const Snum& dnum) const noexcept {
    // return if invalid Snum base given, must be in range: [2,36]!
    if(!confirm_base_in_range(base)) return "+nan.0"; 
    
    // Return self if inf or NaN
    if(dnum.stat != status::success || dnum.is_zero()) 
      return dnum.extract_exact();

    // Divide & conquer conversion among '/' '.' components as needed
    if(!dnum.is_integer()) {
      if(!dnum.is_float) 
        return form_base_N_fraction(base,dnum);
      return form_base_N_floating_pt(base,dnum);
    }
    
    // Return if nothing to convert
    if(base == 10) return dnum.extract_exact();
    Snum D = dnum.abs();
    exact_t bnum;
    
    // From right to left, determine the next digit via %
    while(D >= base) {
      bnum += base_digit_conversion(D % base, base);
      D = (D / base).floor();
    }
    bnum += base_digit_conversion(D, base);
    // Processed from right to left
    if(dnum.is_neg()) return '-' + exact_t(bnum.rbegin(), bnum.rend());
    return exact_t(bnum.rbegin(), bnum.rend());
  }

  /******************************************************************************
  * BASE CONVERSION FLOATING POINT HELPER FCNS
  ******************************************************************************/

  // Erases redundant RHS padding 0s, trimming fractionals
  void Snum::reduce_redundant_RHS_0s(exact_t& fractional) const noexcept {
    if(fractional.empty()) return;
    auto last_non_zero = fractional.end()-1;
    while(last_non_zero != fractional.begin() && *last_non_zero == '0') 
      --last_non_zero;
    fractional.erase(last_non_zero+1,fractional.end());
  }


  // Pad 0 to the LHS of a <base> fractional as needed for improved accuracy
  void Snum::pad_LHS_base_N_decimal_0s_as_needed(const int& base, const exact_t& dnum, exact_t& bnum) const noexcept {
    const Snum scm_dnum("0."+dnum);
    while(Snum("0."+convert_base_N_decimal_to_dec(base,'0'+bnum)) > scm_dnum)     // while padding a 0 = more accuracy
      bnum.insert(0,"0");                                                                // pad a 0
    if((Snum("0."+convert_base_N_decimal_to_dec(base,'0'+bnum)) - scm_dnum).abs() // if padding an extra 0
         <                                                                               // yields more accuracy than
         (Snum("0."+convert_base_N_decimal_to_dec(base,bnum)) - scm_dnum).abs()){ // not padding an extra zero
      bnum.insert(0,"0");                                                                // pad an extra 0
    }
  }

  /******************************************************************************
  * BASE CONVERSION FLOATING POINT MAIN FCNS
  ******************************************************************************/

  // Converts the given <base> floating point fractional to the decimal # system
  // POSTCONDITION: RETURNS THE <base> FRACTIONAL AS A DECIMAL FRACTIONAL
  Snum::exact_t Snum::convert_base_N_decimal_to_dec(const int& base, exact_t bnum) const noexcept {
    // Return if nothing to convert or given an invalid "base" string of digits
    if(base == 10 || EXACT_T_IS_0(bnum))      return bnum;
    if(!confirm_valid_base_digits(base,bnum)) return "+nan.0";
    // --------------------------------------------------------------------------------
    // Approach: 
    // *) NOTE: - 'd' denotes a fractional digit, 'D' denotes an integral digit
    //          - di & Di represent same digit, on either side of the decimal
    //          - N[B] denotes number N in base B
    // 1) Suppose <base> fractional digits "b1 b2 b3 ... bN"
    // 2) We desire to find decimal fractional digits "d1 d2 d3 ... dN" such that:
    //    (D1 D2 D3 ... DN)[10] / (10^N)[10] = (B1 B2 D3 ... BN)[10] / (B^N)[10]
    // 3) Hence: (D1 D2 D3 ... DN)[10] = (10^N)[10] * (B1 B2 D3 ... BN)[10] / (B^N)[10]
    //    Thus:  d1 d2 d3 ... dN = (b1 b2 b3 ... BN)[10] / (B^N)[10]
    // --------------------------------------------------------------------------------
    // only get precision up to INEXACT_PRECISION decimal points
    if(bnum.size() > INEXACT_PRECISION) bnum.erase(INEXACT_PRECISION);
    // perform the above algorithm
    auto decimals = (convert_base_N_to_dec(base,bnum) / (Snum(base).expt(bnum.size()))).extract_inexact();
    auto decimal_fractional = convert_numeric_to_str(decimals);
    // both NaN and Inf are undefined for base-N -> decimal conversion
    if(decimal_fractional == "nan" || decimal_fractional == "inf") return "+nan.0"; 
    // if fraction rounded to 1.0, revert it back to 0.999999 ... 
    // else, erase up to (& including) the LHS decimal from "convert_numeric_to_str"
    if(decimal_fractional[0] != '0') 
      decimal_fractional = exact_t(INEXACT_PRECISION,'9');
    else
      decimal_fractional.erase(0,decimal_fractional.find_first_of('.')+1);
    // erase redundant RHS padding 0s
    reduce_redundant_RHS_0s(decimal_fractional);
    return decimal_fractional;
  }


  // Converts the given decimal floating point fractional to the <base> # system
  // POSTCONDITION: RETURNS THE DECIMAL FRACTIONAL AS A <base> FRACTIONAL
  Snum::exact_t Snum::convert_dec_decimal_to_base_N(const int& base, exact_t dnum) const noexcept {
    // Return if nothing to convert or given an invalid "base" string of digits
    if(base == 10 || Snum(dnum).is_zero()) return dnum;
    if(!confirm_valid_base_digits(10,dnum))       return "+nan.0";
    // --------------------------------------------------------------------------------
    // Approach: 
    // *) NOTE: - 'd' denotes a fractional digit, 'D' denotes an integral digit
    //          - di & Di represent same digit, on either side of the decimal
    //          - N[B] denotes number N in base B
    // 1) Suppose decimal fractional digits "d1 d2 d3 ... dN"
    // 2) We desire to find <base> fractional digits "b1 b2 b3 ... bN" such that:
    //    (D1 D2 D3 ... DN)[10] / (10^N)[10] = (B1 B2 D3 ... BN)[10] / (B^N)[10]
    // 3) Hence: (B1 B2 D3 ... BN)[10] = (B^N)[10] * (D1 D2 D3 ... DN)[10] / (10^N)[10]
    //    Thus:  b1 b2 b3 ... bN = ( (B^N)[10] * (d1 d2 d3 ... dN)[10] )[B]
    // --------------------------------------------------------------------------------
    // only get precision up to INEXACT_PRECISION decimal points
    if(dnum.size() > INEXACT_PRECISION) dnum.erase(INEXACT_PRECISION);
    // perform the above algorithm
    auto base_fractional = convert_dec_to_base_N(base, (Snum("0."+dnum) * 
                                                 (Snum(base).expt(INEXACT_PRECISION))).round());
    if(base_fractional == "+nan.0") return base_fractional;
    // the above algorithm has no way to account for 0s padding "b1 b2 b3...bN"'s
    //   LHS, hence continuously pad 0s & compare w/ its decimal form until the
    //   padding ceases to yeild improved accuracy
    pad_LHS_base_N_decimal_0s_as_needed(base,dnum,base_fractional);
    // erase redundant RHS padding 0s
    reduce_redundant_RHS_0s(base_fractional);
    return base_fractional;
  }

  /******************************************************************************
  * PRECISION COERCION HELPER FCNS
  ******************************************************************************/

  // Coerce the given int string into float. Returns the success status, 
  // IE whether became a float as requested, OR resulted in +- inf.
  Snum::status Snum::coerce_int_to_float(inexact_t& num, const exact_t& data_to_coerce, const bool data_is_neg) const noexcept {
    num = 0;
    try {                                   // try parsing out the floating point
      num = std::stold(data_to_coerce); 
    } catch(const std::out_of_range& err) { // catch error if out of range
      return (data_is_neg) ? status::ninf : status::pinf;
    }
    return status::success;
  }


  // Coerce the given fraction string into float. Returns the success status, 
  // IE whether became a float as requested, OR resulted in +- inf.
  Snum::status Snum::coerce_fraction_to_float(inexact_t& num,const exact_t& numer,const exact_t& denom,const bool data_is_neg)const noexcept{
    num = 0;
    if(EXACT_T_IS_0(denom)) return status::nan;     // n/0 = NaN
    if(EXACT_T_IS_0(numer)) return status::success; // 0/n = 0
    if(numer == denom){num=data_is_neg?-1:1;return status::success;} // n/n = 1
    
    // Coerce the numerator & denominator to inexact_t's
    inexact_t num_double, den_double;
    const auto num_coercion_res = coerce_int_to_float(num_double, numer, data_is_neg);
    const auto den_coercion_res = coerce_int_to_float(den_double, denom, data_is_neg);
    
    // Check for the success status of either conversion
    if(den_coercion_res != status::success) { // denom is inf
      if(num_coercion_res != status::success) // numer is inf
        return status::nan;   // inf/inf = NaN
      return status::success; // n/inf = 0
    } else if(num_coercion_res != status::success) {
      return data_is_neg ? status::ninf : status::pinf; // inf/n = inf
    }

    // return success status of the conversion
    num = num_double / den_double;
    if(data_is_neg) num *= -1;
    return status::success;
  }
} // End of namespace scm_numeric

/******************************************************************************
* "_n" LITERAL SUFFIX FOR SCHEME NUMERICS
******************************************************************************/

scm_numeric::Snum operator"" _n(unsigned long long int n)    {return scm_numeric::Snum(n);}
scm_numeric::Snum operator"" _n(long double n)               {return scm_numeric::Snum(n);}
scm_numeric::Snum operator"" _n(const char* n, std::size_t)  {return scm_numeric::Snum(n);}
scm_numeric::Snum operator"" _n2(const char* n, std::size_t) {return scm_numeric::Snum(n,2);} // Binary
scm_numeric::Snum operator"" _n8(const char* n, std::size_t) {return scm_numeric::Snum(n,8);} // Octal
scm_numeric::Snum operator"" _n16(const char* n, std::size_t){return scm_numeric::Snum(n,16);}// Hexadecimal

#undef EXACT_T_IS_1 // @END-OF-NUMERICS
#undef EXACT_T_IS_0
#undef LD_FIXED_SPRINTF_FORMAT_LOGIC
#undef LD_FIXED_SPRINTF_FORMAT
#undef LD_SPRINTF_FORMAT_LOGIC
#undef LD_SPRINTF_FORMAT
#endif