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
#include <vector>

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
* COMPILE-TIME SNPRINTF FORMATTING FOR inexact_t
******************************************************************************/

#define LD_FIXED_SNPRINTF_FORMAT_LOGIC(x) "%."#x"Lf"
#define LD_FIXED_SNPRINTF_FORMAT(x) LD_FIXED_SNPRINTF_FORMAT_LOGIC(x)

#define LD_SNPRINTF_FORMAT_LOGIC(x) "%."#x"Lg"
#define LD_SNPRINTF_FORMAT(x) LD_SNPRINTF_FORMAT_LOGIC(x)

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
  * LONG DOUBLE GCD (FOR RATIONAL-NUMBER REDUCTION)
  ******************************************************************************/

  constexpr long double long_double_GCD(const long double& a, const long double& b)noexcept{
    if(b == 0) return a;
    return long_double_GCD(b,std::fmod(a,b));
  }

  /******************************************************************************
  * UNSIGNED* ARRAY EQUALITY COMPARISON
  ******************************************************************************/

  constexpr bool unsigned_arrays_are_equal(const unsigned* a, const std::size_t& a_len,
                                           const unsigned* b, const std::size_t& b_len)noexcept{
    if(a_len != b_len) return false;
    for(std::size_t i = 0; i < a_len; ++i)
      if(a[i] != b[i]) return false;
    return true;
  }

  /******************************************************************************
  * NUMERIC CLASS
  ******************************************************************************/

  class Snum {
  public:
    // ***************** CLASS TYPES & FLOATING POINT CONSTANTS *****************

    using exact_val_t = unsigned;
    using exact_t     = exact_val_t*; // fixnum big-int/fractional
    using inexact_t   = long double;  // flonum floating point
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
    std::string extract_exact() const noexcept;


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
    ~Snum()             noexcept;
    Snum(const std::string& data) noexcept {construct_number(data);}
    Snum(std::string data, const int& base) noexcept {*this=convert_base_N_to_dec(base,data);}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    Snum(NumericData data) noexcept {construct_number(data);}
    Snum(const inexact_t& data) noexcept;


    // *********************** TO_STRING OUTPUT GENERATORS **********************

    // get current value as a std::string (for c-style I/O)
    std::string str() const noexcept;

    // get current value as a string in 'base' radix form
    std::string str(const int& base) const noexcept;

    // puts operator for std::ostream
    friend std::ostream& operator<<(std::ostream& outs, const scm_numeric::Snum& n) noexcept {
      outs << n.str();
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
    Snum atan2(const Snum& denom)const noexcept;


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
    using size_type = std::size_t;
    static constexpr size_type SIZE_TYPE_MAX = -1;

    // Internal Numerical Representation Invariants
    signs sign  = signs::zero;
    status stat = status::success;

    exact_t numerator   = nullptr; // exact numer
    exact_t denominator = nullptr; // exact denom
    size_type nlen      = 0;       // numerator length
    size_type dlen      = 0;       // denominator length
    size_type ncapacity = 0;       // current numerator reserved capacity
    size_type dcapacity = 0;       // current denominator reserved capacity
    inexact_t float_num = 0.0L;    // inexact floating point
    bool is_float       = false;

    // Special State Setters
              void set_zero() noexcept;
    constexpr void set_pinf() noexcept {stat = status::pinf, sign = signs::pos;}
    constexpr void set_ninf() noexcept {stat = status::ninf, sign = signs::neg;}

    // Destructive Numerator/Denominator Resizing Members
    void resize_numerator(const size_type& new_size)noexcept;
    void resize_denominator(const size_type& new_size)noexcept;

    // exact_t, inexact_t, std::string, and C++ numeric conversions
    std::string convert_exact_to_string(const exact_t n, const size_type& len) const noexcept;
    long double exact_to_ld(const exact_t exact_num, const size_type& len) const noexcept;

    // Perform fixed floating point conversion to a string
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    std::string convert_numeric_to_str(const NumericData& n) const noexcept {return std::to_string(n);};
    std::string convert_numeric_to_str(const inexact_t& n) const noexcept;
    // Coerce a inexact_t integer to an exact_t
    template<bool COERCING_NUMERATOR>
    void inexact_integer_to_exact_t(Snum& exact_num, const long double& d)const noexcept;
    // Simplify the current number (simplifies exact number/converts to float as needed)
    void simplify_numerics()noexcept;
    // Adjust the current number's float invariants
    void adjust_float_invariants()noexcept;
    // Adjust invariants once stat != status::success
    constexpr void set_failed_status()noexcept;

    // Rm whitespace from number string & trims padding 0's
    void trim_data_string(std::string& num_str)noexcept;

    // Confirm given string is a viable candidate to be a number
    precisions confirm_valid_string(const std::string& num_str)noexcept;

    // Parse the _confirmed_ valid number
    void parse_integer(std::string& s)noexcept;

    // Assign NaN or Inf if given such as a string, and return whether did so
    bool is_irrational(const std::string& num_str)noexcept;

    // Construct number from the given data
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    void construct_number(NumericData num_str)noexcept{construct_number(convert_numeric_to_str(num_str));}
    void construct_number(std::string num_str)noexcept;

    // Convert a base-n big int string into a decimal Snum
    Snum convert_base_N_to_dec(const int& base, std::string bnum) const noexcept;
    // Converts the Snum decimal fractional representation into the <base> # system
    std::string convert_dec_to_base_N_flonum(const int& base, const Snum& dnum) const noexcept;
    // Converts the Snum decimal representation into the <base> # system
    std::string convert_dec_to_base_N(const int& base, const Snum& dnum) const noexcept;
    // Evaluates the numerator & denominator of a base-N # & divides their results
    Snum form_decimal_fraction(const int& base, const size_type& i, const std::string& bnum, const bool& is_neg) const noexcept;
    // Evaluates the integral & fractional of a base-N # & combines their results
    Snum form_decimal_floating_pt(const int& base, const size_type& i, const std::string& bnum, const bool& is_neg) const noexcept;
    // Evaluates the numerator & denominator of a decimal # & divides their results
    std::string form_base_N_fraction(const int& base,const Snum& dnum) const noexcept;
    // Evaluates the integral & fractional of a decimal # & combines their results
    std::string form_base_N_floating_pt(const int& base, const Snum& dnum) const noexcept;
    // Check if any early cases of conversion can be leveraged
    std::string convert_dec_to_base_N_preliminary_check(const int& base, const Snum& dnum)const noexcept;

    // Fast comparison for whether *this == -1
    bool is_negative_one() const noexcept;
    // Performs 2's complement negation on binary string bin_str
    void big_int_BINARY_2sCMPL_NEGATION(std::string& bin_str) const noexcept;
    // Extend bit_str1 w/ b1 & bit_str2 w/ b2 so they have the same width
    void big_int_BINARY_extend(std::string& bit_str1, std::string& bit_str2, const char& b1, const char& b2) const noexcept;
    // Sign-extend bit_str1 & bit_str2 to match lengths
    void signed_big_int_BINARY_extend(std::string& bit_str1, std::string& bit_str2) const noexcept;
    // Unsigned-extend bit_str1 & bit_str2 to match lengths
    void unsigned_big_int_BINARY_extend(std::string& bit_str1, std::string& bit_str2) const noexcept;
    // Convert possibly-signed binary string to a Snum
    Snum big_int_revert_BINARY_to_Snum(std::string bin_str, const bool& is_negative_binary) const noexcept;
    // Convert a Snum to a no-fraction binary string (degrades fractions to inexact_t)
    bool get_non_fraction_binary_string(Snum n, std::string& bit_str, const bool& is_signed) const noexcept;
    // Convert 2 Snums to no-fraction binary strings (degrades fractions to inexact_t)
    bool get_non_fraction_binary_strings(const Snum& n1, std::string& bit_str1, const Snum& n2, std::string& bit_str2) const noexcept;
    // Template outlining structure of LSR & ASR (logical/arithmetic shift right)
    template<bool LOGIC_RIGHT_SHIFT> Snum GENERIC_SHIFT_RIGHT(const Snum& rhs) const noexcept;

    // Erases redundant RHS padding 0s, trimming fractionals
    void reduce_redundant_RHS_0s(std::string& fractional) const noexcept;
    // Pad 0 to the LHS of a <base> fractional as needed for improved accuracy
    void pad_LHS_base_N_decimal_0s_as_needed(const int& base, const std::string& dnum, std::string& bnum) const noexcept;
    // Converts the given <base> floating point fractional to the decimal # system
    std::string convert_base_N_decimal_to_dec(const int& base, std::string bnum) const noexcept;
    // Converts the given decimal floating point fractional to the <base> # system
    std::string convert_dec_decimal_to_base_N(const int& base, std::string dnum) const noexcept;

    // Coerce the given int string into float. Returns the success status.
    status coerce_int_to_float(inexact_t& num, const std::string& data_to_coerce, const bool data_is_neg) const noexcept;
    // Coerce the given fraction into float. Returns the success status.
    status coerce_fraction_to_float(inexact_t& num) const noexcept;

    // Decompose the numerator into SIZE_TYPE_MAX factors (enables repeated squaring)
    std::pair<std::pair<size_type,size_type>,Snum::status> decompose_int_into_SIZE_TYPE_MAX() const noexcept;
    // Perform the "repeated squares" algorithm (rapid exponentiation)
    Snum repeated_squares(const Snum& a, const size_type& b) const noexcept;

    // Generic rounding fcn template for ceil, floor, trunc, & round
    Snum ROUNDING_GENERIC_FCN(const size_type ROUNDING_TYPE_ID) const noexcept;
    // Generic cpp-fcn-invocation template
    Snum UNDERLYING_CPP_GENERIC_FCN(const size_type CPP_FCN_TYPE_ID) const noexcept;
    // Float Extraction for underlying cpp-fcn invocations
    bool extract_float_val(Snum& tmp, inexact_t& value) const noexcept;

    // Returns whether big-int a > big-int b
    bool big_int_gt(const exact_t a, const size_type& a_len, const exact_t b, const size_type& b_len) const noexcept;
    // Returns whether big-int a < big-int b
    bool big_int_lt(const exact_t a, const size_type& a_len, const exact_t b, const size_type& b_len) const noexcept;
    // Sums 2 vectors of decimal digits into a single vector
    void big_int_abs_val_sum(exact_t& sum,size_type& sum_len,const exact_t a,const size_type& a_len,const exact_t b,const size_type& b_len)const noexcept;
    // Multiplies 2 vectors of decimal digits into a single vector
    void big_int_abs_val_mul(exact_t& product,size_type& prod_len,const exact_t a,const size_type& a_len,
                                                                  const exact_t b,const size_type& b_len)const noexcept;
    // MUL '* HELPER: Sums 2 vectors of decimal digits into a single vector
    void big_int_abs_val_MUL_HELPER_sum(exact_t& a, size_type& a_len, const exact_t b, const size_type& b_len)const noexcept;
    // Returns the absolute difference (in a single vector) of 2 vectors of decimal digits
    void big_int_abs_val_diff(exact_t& diff, size_type& diff_len, const exact_t a, const size_type& a_len, 
                                                                  const exact_t b, const size_type& b_len)const noexcept;
  }; // End class Snum 

  /******************************************************************************
  * RANDOM NUMBER GENERATION
  ******************************************************************************/

  Snum Snum::random(Snum seed) noexcept {
    inexact_t seed_flt = 0;
    // Coerce seed to float as needed
    if(!seed.is_zero()) {
      if(seed.is_float) {
        seed_flt = seed.float_num;
      } else if(auto tmp = seed.to_inexact(); tmp.stat != status::success) {
        // Float conversion failed: chop off numerator's 1st INEXACT_PRECISION/2 digits
        // & denominator to be used as a substitute seed
        if(seed.nlen > INEXACT_PRECISION/2) seed.nlen = INEXACT_PRECISION/2;
        if(seed.dlen > INEXACT_PRECISION/2) seed.dlen = INEXACT_PRECISION/2;
        seed_flt = seed.exact_to_ld(seed.numerator, seed.nlen) + 
                   seed.exact_to_ld(seed.denominator, seed.dlen);
      } else {
        seed_flt = tmp.float_num;
      }
    }
    // initialize the random #'s generator & invariants
    std::string rand_vals;
    auto rand_gen = std::minstd_rand0((size_type)seed_flt);
    const auto rand_val_total_digits = INEXACT_PRECISION - (rand_gen() % 10); // - 1st digit

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
    if(stat != status::success) return false; // NaN & +- inf != integers
    if(is_zero())               return true;  // 0 is an integer
    if(!is_float && dlen == 1 && denominator[0] == 1) return true;
    if(is_float) { // test if float has a fractional
      inexact_t integral;
      inexact_t fractional = std::modf(float_num, &integral);
      return fractional == 0;
    }
    return false;
  }


  Snum Snum::to_inexact() const noexcept {
    if(stat != status::success || is_float || is_zero()) return *this;
    Snum tmp;
    tmp.stat = coerce_fraction_to_float(tmp.float_num);
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
      inexact_integer_to_exact_t<true>(tmp,float_num);
      tmp.resize_denominator(1);
      tmp.denominator[tmp.dlen++] = 1;
      tmp.sign = sign;
      return tmp;
    }
    // convert fractional portion of float to a fraction, then add such to the 
    //   integral portion to form an 'exact' approximation
    inexact_t integral;
    inexact_t unsigned_fractional = std::modf(float_num, &integral);
    auto unsigned_integral        = std::to_string(size_type(integral));
    size_type fractional_prec = (unsigned_integral.size() < INEXACT_PRECISION) * 
                                (INEXACT_PRECISION-unsigned_integral.size());
    auto exact_num = Snum(unsigned_integral) + 
                     Snum(std::to_string(size_type(std::round(unsigned_fractional * 
                                                              std::pow(10.0L, fractional_prec)))) + 
                          '/' + std::to_string(size_type(std::pow(10.0L, fractional_prec))));
    if(is_neg()) exact_num.sign = signs::neg;
    return exact_num;
  }


  Snum Snum::extract_numerator() const noexcept {
    auto tmp = to_exact();
    if(tmp.is_zero()) return tmp;
    if(tmp.stat != status::success || tmp.is_float) {
      tmp.set_failed_status(); return tmp;
    }
    if(!tmp.ncapacity || !tmp.nlen) return Snum(); // 0
    tmp.resize_denominator(1);
    tmp.denominator[tmp.dlen++] = 1;
    return tmp;
  }


  Snum Snum::extract_denominator() const noexcept {
    auto tmp = to_exact().abs();
    if(tmp.is_zero()) return Snum("1");
    if(tmp.stat != status::success || tmp.is_float) {
      tmp.set_failed_status(); return tmp;
    }
    tmp.resize_numerator(tmp.dlen);
    tmp.nlen = tmp.dlen;
    for(size_type i = 0; i < tmp.dlen; ++i)
      tmp.numerator[i] = tmp.denominator[i];
    tmp.resize_denominator(1);
    tmp.denominator[tmp.dlen++] = 1;
    return tmp;
  }

  /******************************************************************************
  * PRIMITIVE TYPES COERCION
  ******************************************************************************/

  Snum::inexact_t Snum::extract_inexact() const noexcept {
    if(is_zero()) return 0.0L;
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

  std::string Snum::extract_exact() const noexcept {
    if(is_zero()) return "0";
    auto tmp = to_exact();
    if(tmp.stat != status::success) {
      if(tmp.is_nan())     return "+nan.0";
      if(tmp.is_pos_inf()) return "+inf.0";
      return "-inf.0";
    }
    if(tmp.is_zero()) return "0";
    if(tmp.is_pos()) {
      if(tmp.dlen == 1 && tmp.denominator[0] == 1)
        return convert_exact_to_string(tmp.numerator,tmp.nlen);
      return convert_exact_to_string(tmp.numerator,tmp.nlen) + '/' + convert_exact_to_string(tmp.denominator,tmp.dlen);
    }
    if(tmp.dlen == 1 && tmp.denominator[0] == 1)
      return '-' + convert_exact_to_string(tmp.numerator,tmp.nlen);
    return '-' + convert_exact_to_string(tmp.numerator,tmp.nlen) + '/' + convert_exact_to_string(tmp.denominator,tmp.dlen);
  }

  /******************************************************************************
  * SET NUMBER TO ZERO
  ******************************************************************************/

  void Snum::set_zero() noexcept {
    sign = signs::zero;
    resize_numerator(1);
    numerator[nlen++] = 0;
    resize_denominator(1);
    denominator[dlen++] = 1;
    is_float = false; // coerce back to 'exact' once 0
    stat = status::success;
  }

  /******************************************************************************
  * DESTRUCTIVE NUMERATOR/DENOMINATOR RESIZING MEMBERS
  ******************************************************************************/

  // POST CONDITION: ncapacity >= new_size, nlen = 0, 
  //                 if ncapacity < new_size, numerator is reallocated
  void Snum::resize_numerator(const size_type& new_size)noexcept{
    if(ncapacity >= new_size) { nlen = 0; return; }
    if(numerator && ncapacity) delete [] numerator;
    numerator = new exact_val_t [new_size];
    ncapacity = new_size, nlen = 0;
  }

  // POST CONDITION: dcapacity >= new_size, dlen = 0, 
  //                 if dcapacity < new_size, denominator is reallocated
  void Snum::resize_denominator(const size_type& new_size)noexcept{
    if(dcapacity >= new_size) { dlen = 0; return; }
    if(denominator && dcapacity) delete [] denominator;
    denominator = new exact_val_t [new_size];
    dcapacity = new_size, dlen = 0;
  }

  /******************************************************************************
  * EXACT_T, INEXACT_T, & TO-STRING CONVERSION HELPER FUNCTIONS
  ******************************************************************************/

  std::string Snum::convert_exact_to_string(const exact_t exact_num, const size_type& len)const noexcept{
    std::string str;
    str.reserve(len);
    for(size_type i = 0; i < len; ++i)
      str += char(exact_num[i])+'0';
    return str;
  }

  long double Snum::exact_to_ld(const exact_t exact_num, const size_type& len) const noexcept {
    long double num = 0;
    for(size_type i = 0; i != len; ++i)
      num += exact_num[i] * std::pow(10.0L,(long double)(len-i-1));
    return num;
  }

  template<bool COERCING_NUMERATOR>
  void Snum::inexact_integer_to_exact_t(Snum& exact_num, const long double& d)const noexcept{
    // Estimate the length of snprintfing <d> 
    // => NOTE: 16 is ample padding for the log10 estimate when converting <d>
    const auto estimate_len = std::size_t(std::log10(std::abs(d)))+16; 
    char* d_str = new char [estimate_len];
    size_type end_pos = snprintf(d_str, estimate_len, "%.0Lf", std::abs(d));
    if constexpr (COERCING_NUMERATOR) {
      exact_num.resize_numerator(end_pos);
      for(size_type i = 0; i < end_pos; ++i)
        exact_num.numerator[exact_num.nlen++] = d_str[i]-'0';
    } else {
      exact_num.resize_denominator(end_pos);
      for(size_type i = 0; i < end_pos; ++i)
        exact_num.denominator[exact_num.dlen++] = d_str[i]-'0';
    }
    delete [] d_str;
  }

  // Perform fixed floating point conversion to a string
  // NOTE: std::to_string only has a default precision of 6, so we use snprintf
  // NOTE: the result is _NOT_ an std::string integer, but a string of the float
  std::string Snum::convert_numeric_to_str(const inexact_t& n) const noexcept {
    char outs[64];
    snprintf(outs, 64, LD_FIXED_SNPRINTF_FORMAT(LDBL_DIG), n);
    std::string num_str(outs);
    // rm redundant 0s to the right of the decimal
    if(num_str.find_first_of('.') != std::string::npos && *num_str.rbegin() == '0'){
      size_type i = num_str.size()-1;
      for(; i>1 && num_str[i-1]!='.' && num_str[i]=='0'; --i);
      num_str.erase(i+1);
    }
    return num_str;
  }

  /******************************************************************************
  * NUMBER SIMPLIFICATION & TO-STRING HELPER FUNCTIONS
  ******************************************************************************/

  // Simplify the current number (simplifies exact number/converts to float as needed)
  void Snum::simplify_numerics() noexcept {
    if(stat != status::success) { set_failed_status(); return; }
    if(is_float || is_zero() || (dlen == 1 && denominator[0] == 1)) return; 
    if(unsigned_arrays_are_equal(numerator,nlen,denominator,dlen)) {
      resize_numerator(1), resize_denominator(1);
      numerator[nlen++] = 1, denominator[dlen++] = 1;
    } else if(nlen <= INEXACT_PRECISION && dlen <= INEXACT_PRECISION) {
      inexact_t N = exact_to_ld(numerator,nlen);
      inexact_t D = exact_to_ld(denominator,dlen);
      inexact_t greatest_common_divisor = long_double_GCD(N,D);
      inexact_integer_to_exact_t<true>(*this,N/greatest_common_divisor);
      inexact_integer_to_exact_t<false>(*this,D/greatest_common_divisor);
    } else {
      using exactVec_t = std::vector<exact_val_t>;
      void BIGNUM_GET_REDUCED_core(exactVec_t&,exactVec_t&,exactVec_t&,exactVec_t&)noexcept;
      exactVec_t numVec(nlen,0), denVec(dlen,0), reducedNum, reducedDen;
      for(size_type i = 0; i < nlen; ++i) numVec[i] = numerator[i];
      for(size_type i = 0; i < dlen; ++i) denVec[i] = denominator[i];
      BIGNUM_GET_REDUCED_core(numVec,denVec,reducedNum,reducedDen);
      resize_numerator(reducedNum.size());
      resize_denominator(reducedDen.size());
      for(size_type i = 0, n = reducedNum.size(); i < n; ++i) numerator[nlen++] = reducedNum[i];
      for(size_type i = 0, n = reducedDen.size(); i < n; ++i) denominator[dlen++] = reducedDen[i];
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
    if(this == &s) return *this;
    is_float  = s.is_float;
    sign      = s.sign;
    stat      = s.stat;
    if(is_float) {
      float_num = s.float_num;
    } else {
      if(numerator && ncapacity)   delete [] numerator;
      if(denominator && dcapacity) delete [] denominator;
      numerator = denominator = nullptr;
      if(s.numerator && s.ncapacity) {
        numerator = new exact_val_t [s.ncapacity];
        for(size_type i = 0; i < s.nlen; ++i) numerator[i] = s.numerator[i];
      }
      if(s.denominator && s.dcapacity) {
        denominator = new exact_val_t [s.dcapacity];
        for(size_type i = 0; i < s.dlen; ++i) denominator[i] = s.denominator[i];
      }
      nlen      = s.nlen;
      dlen      = s.dlen;
      ncapacity = s.ncapacity;
      dcapacity = s.dcapacity;
    }
    return *this;
  }

  Snum& Snum::operator=(Snum&& s) noexcept {
    if(this == &s) return *this;
    is_float  = std::move(s.is_float);
    float_num = std::move(s.float_num);
    sign      = std::move(s.sign);
    stat      = std::move(s.stat);
    if(s.numerator && s.ncapacity) {
      if(numerator && ncapacity) delete [] numerator;
      numerator = s.numerator;
      nlen = s.nlen;
      ncapacity = s.ncapacity;
      s.ncapacity = s.nlen = 0;
      s.numerator = nullptr;
    }
    if(s.denominator && s.dcapacity) {
      if(denominator && dcapacity) delete [] denominator;
      denominator = s.denominator;
      dlen = s.dlen;
      dcapacity = s.dcapacity;
      s.dcapacity = s.dlen = 0;
      s.denominator = nullptr;
    }
    s.sign = signs::zero; // moved from Snum's become 0
    return *this;
  }

  /******************************************************************************
  * ADDITION
  ******************************************************************************/

  Snum Snum::operator+(const Snum& s) const noexcept {
    Snum tmp;
    if(stat != status::success || s.stat != status::success) {
      // NaN if either NaN, or subtracting inf from inf
      if(is_nan() || s.is_nan() || (is_pos_inf() && s.is_neg_inf()) || (is_neg_inf() && s.is_pos_inf())){
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

    // Sum Rational Numbers
    if(!is_float && !s.is_float) {
      // cross-multiply & add
      size_type numer1_size = 0, numer2_size = 0;
      exact_t new_numer1 = new exact_val_t [nlen + s.dlen];
      big_int_abs_val_mul(new_numer1,numer1_size,numerator,nlen,s.denominator,s.dlen);
      exact_t new_numer2 = new exact_val_t [s.nlen + dlen];
      big_int_abs_val_mul(new_numer2,numer2_size,s.numerator,s.nlen,denominator,dlen);
      tmp.resize_denominator(dlen + s.dlen);
      big_int_abs_val_mul(tmp.denominator, tmp.dlen, denominator, dlen, s.denominator, s.dlen);
      if(sign == s.sign) {
        tmp.resize_numerator((numer1_size > numer2_size ? numer1_size+1 : numer2_size+1));
        big_int_abs_val_sum(tmp.numerator,tmp.nlen,new_numer1,numer1_size,new_numer2,numer2_size);
        delete [] new_numer1; delete [] new_numer2;
        if(is_neg()) tmp.sign = signs::neg;
        else         tmp.sign = signs::pos; // if either 0, would've already returned by now.
      } else {
        bool negative_diff = big_int_gt(new_numer1,numer1_size,new_numer2,numer2_size) ? is_neg() : s.is_neg();
        tmp.resize_numerator((numer1_size > numer2_size ? numer1_size : numer2_size));
        big_int_abs_val_diff(tmp.numerator,tmp.nlen,new_numer1,numer1_size,new_numer2,numer2_size);
        delete [] new_numer1; delete [] new_numer2;
        if(tmp.nlen == 1 && tmp.numerator[0] == 0) tmp.set_zero();
        else if(negative_diff) tmp.sign = signs::neg;
        else                   tmp.sign = signs::pos;
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
        tmp.stat = s.coerce_fraction_to_float(converted_float);
      else tmp.stat = coerce_fraction_to_float(converted_float);
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
    if(tmp.is_pos_inf())      tmp.set_ninf();
    else if(tmp.is_neg_inf()) tmp.set_pinf();
    else if(tmp.is_pos())     tmp.sign = signs::neg; 
    else if(tmp.is_neg())     tmp.sign = signs::pos; 
    return tmp;
  }

  /******************************************************************************
  * MULTIPLICATION
  ******************************************************************************/

  Snum Snum::operator*(const Snum& s) const noexcept {
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

    // Multiply Rational Numbers
    if(!is_float && !s.is_float) {
      tmp.resize_numerator(nlen + s.nlen);
      big_int_abs_val_mul(tmp.numerator,tmp.nlen,numerator,nlen,s.numerator,s.nlen);
      tmp.resize_denominator(dlen + s.dlen);
      big_int_abs_val_mul(tmp.denominator,tmp.dlen,denominator,dlen,s.denominator,s.dlen);
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
        tmp.stat = s.coerce_fraction_to_float(converted_float);
      else tmp.stat = coerce_fraction_to_float(converted_float);
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
    else {
      std::swap<exact_t>(tmp.numerator,tmp.denominator);
      std::swap<size_type>(tmp.nlen,tmp.dlen);
      std::swap<size_type>(tmp.ncapacity,tmp.dcapacity);
    }
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
    // n % n = 0 % n = 0
    if(abs_this == abs_s || is_zero()) return Snum();
    // for n < m, n % m = n
    if(abs_this < abs_s) return *this;
    // modding by inf: if n != inf, 1; else, 0
    if(abs_s.is_pos_inf()) {
      if(!abs_this.is_pos_inf()) return *this;
      return Snum();
    }

    // apply the above algorithm iff not handling 2 ints
    if(!(abs_this.is_exact() && abs_this.is_integer()) || !(abs_s.is_exact() && abs_s.is_integer())) {
      tmp = *this / s;
      inexact_t div_res;
      bool exact_division = !tmp.is_float;
      if(exact_division) { // coerce fractional to floating-point
        tmp.stat = tmp.coerce_fraction_to_float(div_res);
        if(tmp.stat != status::success) {
          tmp.set_failed_status();return tmp;
        }
      } else {
        div_res = tmp.float_num;
        if(tmp.is_neg()) div_res *= -1.0L;
      }
      inexact_t integral;
      inexact_t fractional = std::modf(div_res,&integral);
      auto mod_result = (s * fractional);
      mod_result.adjust_float_invariants();
      // exact % exact : exact (iff exact / exact == exact)
      if(exact_division) 
        mod_result = mod_result.round().to_exact();
      if(!mod_result.is_zero())
        mod_result.sign = sign;
      return mod_result;

    // use the bignum variant
    } else {
      using exactVec_t = std::vector<exact_val_t>;
      void BIGNUM_UNSIGNED_DIVIDE_core(exactVec_t,exactVec_t,exactVec_t&,exactVec_t&)noexcept;
      tmp = Snum("1");
      exactVec_t aVec(nlen,0), bVec(s.nlen,0), quotient, remainder;
      for(size_type i = 0; i < nlen; ++i) aVec[i] = numerator[i];
      for(size_type i = 0; i < s.nlen; ++i) bVec[i] = s.numerator[i];
      BIGNUM_UNSIGNED_DIVIDE_core(aVec,bVec,quotient,remainder);
      if(remainder.size() == 1 && remainder[0] == 0) return Snum(); // remainder is 0
      tmp.resize_numerator(remainder.size());
      for(size_type i = 0, n = remainder.size(); i < n; ++i) tmp.numerator[tmp.nlen++] = remainder[i];
      tmp.sign = sign;
      return tmp;
    }
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
      tmp.resize_numerator(1), tmp.resize_denominator(1);
      tmp.numerator[tmp.nlen++] = 1;
      tmp.denominator[tmp.dlen++] = 1;
      tmp.sign = signs::pos; 
      return tmp;
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
    // n^1 = n
    if((pow.is_float && pow.float_num == 1.0) || 
       (!pow.is_float && pow.nlen == 1 && pow.numerator[0] == 1 && pow.dlen == 1 && pow.denominator[0] == 1))
      return *this;

    // Coerce Fractional Power to Float
    if(!pow.is_float && (pow.dlen != 1 || pow.denominator[0] != 1)) { 
      tmp.stat = pow.coerce_fraction_to_float(pow.float_num);
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
      tmp.stat = coerce_fraction_to_float(base_flt);
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

    // GCD of non-bigint
    if(!(is_exact() && is_integer()) || !(s.is_exact() && s.is_integer())) {
      inexact_t lhs = 0, rhs = 0;
      // Coerce this' Fractional to Float as Needed
      if(is_float) lhs = float_num;
      else {
        tmp.stat = coerce_fraction_to_float(lhs);
        if(tmp.stat != status::success) {
          tmp.set_failed_status(); return tmp;
        }
      }
      
      // Coerce s' Fractional to Float as Needed
      if(s.is_float) rhs = s.float_num;
      else {
        tmp.stat = s.coerce_fraction_to_float(rhs);
        if(tmp.stat != status::success) {
          tmp.set_failed_status(); return tmp;
        }
      }
      
      // return the GCD
      tmp.float_num = long_double_GCD(lhs,rhs);
      tmp.adjust_float_invariants();
      return tmp;

    // GCD of bigints
    } else {
      using exactVec_t = std::vector<exact_val_t>;
      exactVec_t BIGNUM_UNSIGNED_GCD_core(exactVec_t&,exactVec_t&)noexcept;
      tmp = Snum("1");
      exactVec_t aVec(nlen,0), bVec(s.nlen,0);
      for(size_type i = 0; i < nlen; ++i) aVec[i] = numerator[i];
      for(size_type i = 0; i < s.nlen; ++i) bVec[i] = s.numerator[i];
      auto gcd_bignum = BIGNUM_UNSIGNED_GCD_core(aVec,bVec);
      tmp.resize_numerator(gcd_bignum.size());
      for(size_type i = 0, n = gcd_bignum.size(); i < n; ++i) tmp.numerator[tmp.nlen++] = gcd_bignum[i];
      return tmp;
    }
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
    return (abs() / greatest_common_divisor) * s.abs(); // LCM
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
      tmp.stat = tmp.coerce_fraction_to_float(ln_base);
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
    // Not handling 2 ints
    if(!(is_exact() && is_integer()) || !(s.is_exact() && s.is_integer())) {
      auto tmp = *this / s;
      if(tmp.stat != status::success) {tmp.set_failed_status(); return tmp;}
      bool exact_division = !tmp.is_float;
      if(exact_division) tmp = tmp.to_inexact();
      if(tmp.stat != status::success) {tmp.set_failed_status(); return tmp;}
      if(exact_division) return tmp.trunc().to_exact();
      return tmp.trunc();

    // Handling 2 ints
    } else {
      using exactVec_t = std::vector<exact_val_t>;
      void BIGNUM_UNSIGNED_DIVIDE_core(exactVec_t,exactVec_t,exactVec_t&,exactVec_t&)noexcept;
      Snum tmp("1");
      exactVec_t aVec(nlen,0), bVec(s.nlen,0), quotient, remainder;
      for(size_type i = 0; i < nlen; ++i) aVec[i] = numerator[i];
      for(size_type i = 0; i < s.nlen; ++i) bVec[i] = s.numerator[i];
      BIGNUM_UNSIGNED_DIVIDE_core(aVec,bVec,quotient,remainder);
      if(quotient.size() == 1 && quotient[0] == 0) return Snum(); // quotient is 0
      tmp.resize_numerator(quotient.size());
      for(size_type i = 0, n = quotient.size(); i < n; ++i) tmp.numerator[tmp.nlen++] = quotient[i];
      if(sign != s.sign) tmp.sign = signs::neg;
      return tmp;
    }
  }

  Snum Snum::modulo(const Snum& s) const noexcept {
    // Credit for this Algorithm (modE) goes to Daan Leijen of the University of Utrecht. 
    // Proof (see page 5):
    // "https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf"
    auto rem = *this % s;
    if(rem.stat != status::success) {rem.set_failed_status(); return rem;}
    if(rem.is_neg()) {
      if(s.is_pos()) return rem + s;
      return rem - s;
    }
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
      return unsigned_arrays_are_equal(numerator,nlen,s.numerator,s.nlen) && 
             unsigned_arrays_are_equal(denominator,dlen,s.denominator,s.dlen);
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
      return (float_num * (1 - 2 * is_neg())) < (s.float_num * (1 - 2 * s.is_neg()));
    else if(!is_float && !s.is_float) {
      size_type lhs_len = 0, rhs_len = 0;
      exact_t lhs = new exact_val_t [nlen + s.dlen];
      big_int_abs_val_mul(lhs,lhs_len,numerator,nlen,s.denominator,s.dlen);
      exact_t rhs = new exact_val_t [s.nlen + dlen];
      big_int_abs_val_mul(rhs,rhs_len,s.numerator,s.nlen,denominator,dlen);
      bool result = big_int_lt(lhs,lhs_len,rhs,rhs_len);
      delete [] lhs; delete [] rhs;
      return result ^ is_neg();
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
      return (float_num * (1 - 2 * is_neg())) > (s.float_num * (1 - 2 * s.is_neg()));
    else if(!is_float && !s.is_float) {
      size_type lhs_len = 0, rhs_len = 0;
      exact_t lhs = new exact_val_t [nlen + s.dlen];
      big_int_abs_val_mul(lhs,lhs_len,numerator,nlen,s.denominator,s.dlen);
      exact_t rhs = new exact_val_t [s.nlen + dlen];
      big_int_abs_val_mul(rhs,rhs_len,s.numerator,s.nlen,denominator,dlen);
      bool result = big_int_gt(lhs,lhs_len,rhs,rhs_len);
      delete [] lhs; delete [] rhs;
      return result ^ is_neg();
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
    std::string bin_str;
    if(!get_non_fraction_binary_string(*this,bin_str,true)){ // <<ing a negative
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Logical Left Shift
    const auto shift_amount = (size_type)rhs.extract_inexact();
    if(auto pos = bin_str.find('.'); pos != std::string::npos) { // shifting an inexact_t
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
    // ~0 = -1
    if(is_zero()) return Snum("-1");
    // ~(-1) = 0
    if(is_negative_one()) return Snum(); 
    // Extract binary string
    std::string bin_str;
    if(!get_non_fraction_binary_string(*this,bin_str,true)){ // "~" is ALWAYS a signed operation
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Bitwise NOT
    std::string bin_str_result(bin_str.size(), '0');
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
    std::string bin_str_lhs, bin_str_rhs;
    if(!get_non_fraction_binary_strings(*this,bin_str_lhs,rhs,bin_str_rhs)){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Bitwise AND
    std::string bin_str_result(bin_str_lhs.size(), '0');
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
    std::string bin_str_lhs, bin_str_rhs;
    if(!get_non_fraction_binary_strings(*this,bin_str_lhs,rhs,bin_str_rhs)){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Bitwise OR
    std::string bin_str_result(bin_str_lhs.size(), '0');
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
    std::string bin_str_lhs, bin_str_rhs;
    if(!get_non_fraction_binary_strings(*this,bin_str_lhs,rhs,bin_str_rhs)){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Bitwise XOR
    std::string bin_str_result(bin_str_lhs.size(), '0');
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
    if(dlen == 1 && denominator[0] == 1) // check if bigint is even
      // return !numerator.empty() && ((*numerator.rbegin() & 1) == 0); 
      return nlen && (numerator[nlen-1] & 1) == 0;
    inexact_t num = 0;
    auto coerced = coerce_fraction_to_float(num);
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
  std::string Snum::str() const noexcept {
    // Return NaN/Inf as needed
    if(stat != Snum::status::success) {
      if(is_nan())     return "+nan.0";
      if(is_neg_inf()) return "-inf.0";
      return "+inf.0";
    }
    // Return Numeric Value as std::string
    if(is_zero()) {
      return "0";
    } else if(is_float) {
      char str[64];
      auto append_neg = is_neg();
      str[0] = append_neg * '-'; // Prepend Negative Sign as Needed
      snprintf(str+append_neg, 64-append_neg, LD_SNPRINTF_FORMAT(LDBL_DIG), float_num);
      if(float_num >= Snum::RATIONALITY_LIMIT || !is_integer())
        return str;
      char* p = str; // Print ".0" after flonums w/o a fractional
      while(*(++p)); // 64 w/ our format is plenty to add ".0" w/o hitting bad memory
      *p++ = '.'; *p++ = '0'; *p = 0;
      return str;
    } else if(dlen == 1 && denominator[0] == 1) {
      if(!is_neg()) return convert_exact_to_string(numerator,nlen);
      return '-' + convert_exact_to_string(numerator,nlen);
    } else {
      if(!is_neg()) return convert_exact_to_string(numerator,nlen) + '/' + convert_exact_to_string(denominator,dlen);
      return '-' + convert_exact_to_string(numerator,nlen) + '/' + convert_exact_to_string(denominator,dlen);
    }
  }

  // get current value as a string in 'base' radix form
  std::string Snum::str(const int& base) const noexcept {
    return convert_dec_to_base_N(base, *this);
  }

  /******************************************************************************
  * BITWISE OPERATION HELPERS
  ******************************************************************************/

  // Fast comparison for whether *this == -1
  bool Snum::is_negative_one() const noexcept {
    return stat == status::success && sign == signs::neg && 
      ((!is_float && nlen == 1 && numerator[0] == 1 && dlen == 1 && denominator[0] == 1) ||
       (is_float && float_num == 1.0L));
  }

  // Performs 2's complement negation on binary string bin_str
  void Snum::big_int_BINARY_2sCMPL_NEGATION(std::string& bin_str) const noexcept {
    auto i = bin_str.size();
    for(;i-- > 0 && bin_str[i] != '1';); // seek the rightmost '1'
    if(i+1 == 0) return;
    for(;i-- > 0;) // complement each bit afterwards
      bin_str[i] = char(bin_str[i] - (bin_str[i] == '1') + (bin_str[i] == '0'));
  }

  // Extend bit_str1 w/ b1 & bit_str2 w/ b2 so they have the same width
  // PRECONDITION: bit_str1 & bit_str2 MUST be NON-EMPTY!
  void Snum::big_int_BINARY_extend(std::string& bit_str1, std::string& bit_str2, 
                                   const char& b1,        const char& b2) const noexcept {
    if(bit_str1.size() > bit_str2.size()) {
      bit_str2.insert(0, std::string(bit_str1.size() - bit_str2.size(), b2));
    } else if(bit_str1.size() < bit_str2.size()) {
      bit_str1.insert(0, std::string(bit_str2.size() - bit_str1.size(), b1));
    }
  }

  // Sign-extend bit_str1 & bit_str2 to match lengths
  // PRECONDITIONS: 1) bit_str1 & bit_str2 MUST be NON-EMPTY!
  //                2) sign bits MUST be present in BOTH bit_str1 & bit_str2
  void Snum::signed_big_int_BINARY_extend(std::string& bit_str1, std::string& bit_str2) const noexcept {
    big_int_BINARY_extend(bit_str1,bit_str2,bit_str1[0],bit_str2[0]);
  }

  // Unsigned-extend bit_str1 & bit_str2 to match lengths
  // PRECONDITION: bit_str1 & bit_str2 MUST be NON-EMPTY!
  void Snum::unsigned_big_int_BINARY_extend(std::string& bit_str1, 
                                            std::string& bit_str2) const noexcept {
    big_int_BINARY_extend(bit_str1,bit_str2,'0','0');
  }

  // Convert possibly-signed binary string to a Snum
  Snum Snum::big_int_revert_BINARY_to_Snum(std::string bin_str, 
                                        const bool& is_negative_binary) const noexcept {
    if(is_negative_binary) {
      big_int_BINARY_2sCMPL_NEGATION(bin_str);
      return -(Snum(bin_str,2));
    }
    return Snum(bin_str,2);
  }

  // Convert a Snum to a no-fraction binary string 
  //   (degrades fractions to inexact_t & returns success status)
  bool Snum::get_non_fraction_binary_string(Snum n, std::string& bit_str, 
                                                    const bool& is_signed)const noexcept{
    // Coerce fractions to inextact_t as needed
    if(n.stat == status::success && !n.is_float && (n.dlen != 1 || n.denominator[0] != 1)){
      n = n.to_inexact();
    }
    if(n.stat != status::success) return false;
    // Get number as unsigned binary string of its absolute value
    bit_str = n.str(2);
    if(bit_str[0] == '-') bit_str.erase(0,1);
    // Account for sign
    if(is_signed) {
      if(n.is_neg()) {
        big_int_BINARY_2sCMPL_NEGATION(bit_str);
        bit_str = '0' + bit_str;
      } else {
        bit_str = '1' + bit_str;
      }
    }
    return true;
  }

  // Convert 2 Snums to no-fraction binary strings 
  //   (degrades fractions to inexact_t & returns success status)
  bool Snum::get_non_fraction_binary_strings(const Snum& n1, std::string& bit_str1, 
                                             const Snum& n2, std::string& bit_str2)const noexcept{
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
    std::string bin_str;
    if(!get_non_fraction_binary_string(*this,bin_str,is_neg())){
      Snum tmp; tmp.stat = status::nan; return tmp;
    }
    // Perform Logical/Arithmetic Right Shift
    const auto shift_amount = (size_type)rhs.extract_inexact();
    char fill_char = 0;
    if constexpr (LOGIC_RIGHT_SHIFT) fill_char = '0'; else fill_char = char(is_neg() + '0');
    if(auto pos = bin_str.find('.'); pos != std::string::npos) { // shifting an inexact_t
      if(pos == 0) {
        bin_str.insert(1,shift_amount,fill_char);                // pad 0s
      } else {
        bin_str = std::string(shift_amount,fill_char) + bin_str; // pad 0s
        bin_str.erase(pos+shift_amount,1);                       // rm '.'
        bin_str.insert(pos,".");                                 // reinsert '.' shifted over
      }
    } else { // shifting exact_t
      bin_str = std::string(shift_amount,fill_char) + bin_str;   // pad 0s
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
      auto div_coercion = div_res.coerce_fraction_to_float(div_flt);
      if(div_coercion != status::success)return std::make_pair(std::make_pair(0,0), div_coercion);
    }
    // Coerce remainder Result to a Float
    if(mod_res.is_float) {
      mod_flt = mod_res.float_num;
    } else {
      auto mod_coercion = mod_res.coerce_fraction_to_float(mod_flt);
      if(mod_coercion != status::success)return std::make_pair(std::make_pair(0,0), mod_coercion);
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
      tmp.stat = tmp.coerce_fraction_to_float(flt_val);
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
  * UNSIGNED BIG INT ARITHMETIC HELPER FUNCTIONS
  ******************************************************************************/

  // Returns whether big-int a > big-int b
  bool Snum::big_int_gt(const exact_t a, const size_type& a_len, const exact_t b, const size_type& b_len)const noexcept{
    if(a_len != b_len) return a_len > b_len;
    for(size_type i = 0; i < a_len; ++i) {
      if(a[i] != b[i]) return (a[i] > b[i]);
    }
    return false; // a = b
  }


  // Returns whether big-int a < big-int b
  bool Snum::big_int_lt(const exact_t a, const size_type& a_len, const exact_t b, const size_type& b_len)const noexcept{
    if(a_len != b_len) return a_len < b_len;
    for(size_type i = 0; i < a_len; ++i) {
      if(a[i] != b[i]) return (a[i] < b[i]);
    }
    return false; // a = b
  }


  // Sums 2 arrays of decimal digits into a single array
  // PRECONDITION: sum MUST HAVE A CAPACITY OF >= max(a_len, b_len) + 1
  void Snum::big_int_abs_val_sum(exact_t& sum, size_type& sum_len, 
                                 const exact_t a, const size_type& a_len, 
                                 const exact_t b, const size_type& b_len)const noexcept{
    if(a_len == 1 && a[0] == 0) { // 0 + b = b
      sum_len = b_len;
      for(size_type i = 0; i < sum_len; ++i) sum[i] = b[i];
      return;
    }
    if(b_len == 1 && b[0] == 0) { // a + 0 = a
      sum_len = a_len;
      for(size_type i = 0; i < sum_len; ++i) sum[i] = a[i];
      return;
    }
    // add the 2 #s together in reverse (primary school addition)
    size_type a_idx = a_len-1, b_idx = b_len-1;
    exact_val_t carry_over = 0;
    for(; a_idx != SIZE_TYPE_MAX && b_idx != SIZE_TYPE_MAX; --a_idx, --b_idx) {
      exact_val_t d_sum = a[a_idx] + b[b_idx] + carry_over;
      carry_over = (d_sum > 9);
      sum[sum_len++] = d_sum - 10 * carry_over;
    }
    // propogate the last 'carry_over' across the rest of the 
    //   digits of the larger # (if applicable)
    // => NOTE: Only <= 1 of the 2 loops below will every be triggered
    while(a_idx != SIZE_TYPE_MAX) {
      exact_val_t d_sum = a[a_idx--] + carry_over;
      carry_over = (d_sum > 9);
      sum[sum_len++] = d_sum - 10 * carry_over;
    }
    while(b_idx != SIZE_TYPE_MAX) {
      exact_val_t d_sum = b[b_idx--] + carry_over;
      carry_over = (d_sum > 9);
      sum[sum_len++] = d_sum - 10 * carry_over;
    }
    // append the final "carry_over" bit, IF = 1
    if(carry_over == 1) sum[sum_len++] = 1;
    // Reverse the sum (generated in reverse)
    for(size_type i = 0, n = sum_len/2; i < n; ++i) {
      sum[i] ^= sum[sum_len-1-i];
      sum[sum_len-1-i] ^= sum[i];
      sum[i] ^= sum[sum_len-1-i];
    }
  }


  // Multiplies 2 arrays of decimal digits into a single array
  // PRECONDITION: product MUST HAVE A CAPACITY OF >= a_len + b_len
  void Snum::big_int_abs_val_mul(exact_t& product,size_type& prod_len,
                                 const exact_t a,const size_type& a_len,
                                 const exact_t b,const size_type& b_len)const noexcept{
    if((a_len==1 && a[0]==0) || (b_len==1 && b[0]==0)) { // 0 * N = 0
      product[0] = 0, prod_len = 1;
      return;
    }
    if(a_len == 1 && a[0] == 1) { // 1 * b = b
      prod_len = b_len;
      for(size_type i = 0; i < b_len; ++i) product[i] = b[i];
      return;
    }
    if(b_len == 1 && b[0] == 1) { // a * 1 = a
      prod_len = a_len;
      for(size_type i = 0; i < a_len; ++i) product[i] = a[i];
      return;
    }
    const bool    a_is_gt_b = big_int_gt(a,a_len,b,b_len);
    const exact_t big       = a_is_gt_b ? a : b;
    const exact_t small     = a_is_gt_b ? b : a;
    const auto&   big_len   = a_is_gt_b ? a_len : b_len;
    const auto&   small_len = a_is_gt_b ? b_len : a_len;
    prod_len = 0;
    product[prod_len++] = 0; // product of *
    // Primary School Multiplication
    exact_val_t carry_over = 0;
    exact_t digit_mul_instance = new exact_val_t [big_len+small_len];
    // for each digit in the small#
    for(size_type digit_No=0, dmi_idx=0, small_idx=small_len-1; 
        small_idx != SIZE_TYPE_MAX; 
        --small_idx, ++digit_No, dmi_idx=0, carry_over=0){
      // pad digit_No zeros to digit_mul_instance
      while(dmi_idx != digit_No) 
        digit_mul_instance[dmi_idx++] = 0; 
      // for each digit in the big#
      for(size_type big_idx = big_len-1; big_idx != SIZE_TYPE_MAX; --big_idx) { 
        exact_val_t mulres = small[small_idx] * big[big_idx] + carry_over;
        carry_over = mulres / 10;                    // 10's digit is carried over
        digit_mul_instance[dmi_idx++] = mulres % 10; // 1's digit is accounted for to be summed later
      }
      // add carry_over if present
      if(carry_over > 0) digit_mul_instance[dmi_idx++] = carry_over;
      // Add small#'s digit-multiplication w/ the big# to the product
      big_int_abs_val_MUL_HELPER_sum(product,prod_len,digit_mul_instance,dmi_idx);
    }
    delete [] digit_mul_instance;
    // Reverse the product (generated in reverse)
    for(size_type i = 0, n = prod_len/2; i < n; ++i) {
      product[i] ^= product[prod_len-1-i];
      product[prod_len-1-i] ^= product[i];
      product[i] ^= product[prod_len-1-i];
    }
  }


  // MUL '* HELPER: Sums 2 arrays of decimal digits into a single array
  // PRECONDITION: a MUST HAVE A CAPACITY OF >= b_len
  void Snum::big_int_abs_val_MUL_HELPER_sum(exact_t& a, size_type& a_len, 
                                            const exact_t b, const size_type& b_len)const noexcept{ // <a> = product
    if(b_len == 1 && b[0] == 0) return; // a + 0 = a
    if(a_len == 1 && a[0] == 0) {       // 0 + b = b
      a_len = b_len;
      for(size_type i = 0; i < b_len; ++i) a[i] = b[i];
      return;
    }
    size_type a_idx = 0, b_idx = 0;
    exact_val_t carry_over = 0;
    // add the 2 #s together
    while(a_idx != a_len && b_idx != b_len) {
      exact_val_t d_sum = a[a_idx] + b[b_idx++] + carry_over;
      carry_over = (d_sum > 9);
      a[a_idx++] = d_sum - 10 * carry_over;
    }
    // NOTE: only 1 (if any at all) of the below 2 while loops are ever invoked!
    while(a_idx != a_len) { // propogate the last 'carry_over' across the rest of a
      exact_val_t d_sum = a[a_idx] + carry_over;
      carry_over = (d_sum > 9);
      a[a_idx++] = d_sum - 10 * carry_over;
    }
    while(b_idx != b_len) { // propogate the last 'carry_over' across the b into a
      exact_val_t d_sum = b[b_idx++] + carry_over;
      carry_over = (d_sum > 9);
      a[a_len++] = d_sum - 10 * carry_over;
    }
    if(carry_over == 1) a[a_len++] = 1; // append the final "carry_over" bit, IF = 1
  }


  // Returns the absolute difference (in a single array) of 2 arrays of decimal digits
  // PRECONDITION: diff MUST HAVE A CAPACITY OF >= max(a_len,b_len)
  void Snum::big_int_abs_val_diff(exact_t& diff, size_type& diff_len, const exact_t a, const size_type& a_len, 
                                                                      const exact_t b, const size_type& b_len)const noexcept{
    if(a_len == 1 && a[0] == 0) { // |0 - b| = b
      diff_len = b_len;
      for(size_type i = 0; i < diff_len; ++i) diff[i] = b[i];
      return;
    }
    if(b_len == 1 && b[0] == 0) { // |a - 0| = a
      diff_len = a_len;
      for(size_type i = 0; i < diff_len; ++i) diff[i] = a[i];
      return;
    }
    if(unsigned_arrays_are_equal(a,a_len,b,b_len)) { // |N -N| = 0
      diff_len = 1, diff[0] = 0; return;
    }
    const bool a_is_gt_b     = big_int_gt(a,a_len,b,b_len);
    const exact_t big        = a_is_gt_b ? a : b;
    const size_type& big_len = a_is_gt_b ? a_len : b_len;
    exact_t small            = new exact_val_t [big_len]; // reserves big.size()
    size_type small_len      = 0;
    // pad the front of 'small' w/ 0's
    if(a_len != b_len) {
      if(a_is_gt_b) {
        for(size_type i = 0, padding_length = a_len-b_len; i<padding_length; ++i) small[small_len++] = 0;
        for(size_type i = 0; i < b_len; ++i) small[small_len++] = b[i]; // now small.size() = big.size()
      } else {
        for(size_type i = 0, padding_length = b_len-a_len; i<padding_length; ++i) small[small_len++] = 0;
        for(size_type i = 0; i < a_len; ++i) small[small_len++] = a[i]; // now small.size() = big.size()
      }
    } else if(a_is_gt_b) {
      small_len = b_len;
      for(size_type i = 0; i < b_len; ++i) small[i] = b[i];

    } else {
      small_len = a_len;
      for(size_type i = 0; i < a_len; ++i) small[i] = a[i];
    }
    // perform subtraction
    exact_val_t carry_over = 0;
    size_type big_idx = big_len-1;
    diff_len = 0;
    for(size_type small_idx = small_len-1; big_idx != SIZE_TYPE_MAX; --big_idx, --small_idx) {
      // carry the one (or 0) as needed
      exact_val_t small_digit = small[small_idx] + carry_over;
      carry_over = big[big_idx] < small_digit;
      // add 10 to the big# digit if carrying over
      diff[diff_len++] = big[big_idx] + (carry_over * 10) - small_digit; 
    }
    delete [] small;
    // rm extra 0 padding front of sub (if present)
    for(size_type zero_test = diff_len-1; zero_test != SIZE_TYPE_MAX; --zero_test) {
      if(diff[zero_test]) break;
      --diff_len;
    }
    // Reverse the difference (generated in reverse)
    for(size_type i = 0, n = diff_len/2; i < n; ++i) {
      diff[i] ^= diff[diff_len-1-i];
      diff[diff_len-1-i] ^= diff[i];
      diff[i] ^= diff[diff_len-1-i];
    }
  }

  /******************************************************************************
  * UNDERLYING CPP-INVOCATION HELPER FUNCTION
  ******************************************************************************/

  // Returns whether succeeded
  bool Snum::extract_float_val(Snum& tmp, inexact_t& value) const noexcept {
    if(is_float) {
      value = is_neg() ? -float_num : float_num;
      return true;
    }
    tmp.stat = coerce_fraction_to_float(value);
    if(tmp.stat != status::success) return false;
    return true;
  }


  // Generic cpp-fcn-invocation template
  Snum Snum::UNDERLYING_CPP_GENERIC_FCN(const size_type CPP_FCN_TYPE_ID) const noexcept {
    Snum tmp;
    constexpr const size_type EXP_ID = 12;
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
    if(!is_zero() && !extract_float_val(tmp,flt_val)) {
      tmp.set_failed_status(); return tmp;
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
  * UNDERLYING CPP-ATAN2-INVOCATION HELPER FUNCTION
  ******************************************************************************/

  Snum Snum::atan2(const Snum& denom) const noexcept {
    Snum tmp;
    if(stat != status::success || denom.stat != status::success || (is_zero() && denom.is_zero())) {
      tmp.stat = status::nan; return tmp;
    }
    // Coerce Fractional to Float as Needed
    inexact_t numer_val = 0, denom_val = 0;
    if(!is_zero() && !extract_float_val(tmp,numer_val)){
      tmp.set_failed_status(); return tmp;
    }
    if(!denom.is_zero() && !denom.extract_float_val(tmp,denom_val)){
      tmp.set_failed_status(); return tmp;
    }
    tmp.float_num = std::atan2(numer_val,denom_val);
    tmp.adjust_float_invariants();
    return tmp;
  }

  /******************************************************************************
  * CONSTRUCT/PARSE NUMBER HELPER FCNS
  ******************************************************************************/

  // rm whitespace from number string & trims padding 0's
  void Snum::trim_data_string(std::string& num_str) noexcept {
    std::string trimmed;
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
    if(trimmed.find('.') != std::string::npos) {
      i = trimmed.size()-1;
      while(i > 0 && trimmed[i] == '0' && trimmed[i-1] == '0') 
        trimmed.erase(i,1);
    }
    num_str = trimmed;
  }


  // Confirm given string is a viable candidate to be a number
  Snum::precisions Snum::confirm_valid_string(const std::string& num_str) noexcept {
    std::string str(num_str);
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
  * CONSTRUCT/DESTRUCTOR/PARSE NUMBER MAIN FCNS
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


  Snum::~Snum() noexcept {
    if(numerator && ncapacity)   delete [] numerator; 
    if(denominator && dcapacity) delete [] denominator;
    numerator = denominator = nullptr;
    nlen = dlen = ncapacity = dcapacity = 0;
    sign = signs::zero;
  }


  // Parse the _confirmed_ valid number
  void Snum::parse_integer(std::string& s) noexcept {
    size_type frac_begin = s.find('/');
    if(frac_begin == std::string::npos) frac_begin = s.size();
    resize_numerator(frac_begin+1);
    auto ch = s.begin();
    while(*ch == '0' && *(ch+1) && *(ch+1) != '/') ++ch; // rm padding lhs 0s
    while(*ch && *ch != '/') numerator[nlen++] = *ch++ -'0';
    if(*ch == '/' && *(ch+1)) {
      ++ch;                                    // mv past '/'
      resize_denominator(s.size()-frac_begin); // empty dflt value for denominator
      while(*ch == '0' && *(ch+1)) ++ch;       // rm padding rhs 0s
      while(*ch) denominator[dlen++] = *ch++ - '0';
    } 
    if(dlen == 0) {
      resize_denominator(1);
      denominator[dlen++] = 1;
    }
  }


  // Assign NaN or Inf if given such as a string, and return whether did so
  bool Snum::is_irrational(const std::string& num_str) noexcept {
    if(num_str == "+inf.0") { set_pinf(); return true; }
    if(num_str == "-inf.0") { set_ninf(); return true; }
    if(num_str == "+nan.0" || num_str == "-nan.0") { 
      stat=status::nan; return true; 
    }
    return false;
  }


  // Construct number out of the given string
  void Snum::construct_number(std::string num_str) noexcept {
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
      parse_integer(num_str); // parse the integer exact fraction
      if(dlen == 1 && denominator[0] == 0) 
        stat = status::nan; // can't divide by 0
    } else {
      stat = coerce_int_to_float(float_num, num_str, is_neg());
    }
    if(stat == status::success &&
       ((precision == precisions::exact && nlen == 1 && numerator[0] == 0) || 
        (precision != precisions::exact && float_num == 0)))
      set_zero();
    simplify_numerics();
  }

  /******************************************************************************
  * BASE CONVERSION INTEGER HELPER FCNS
  ******************************************************************************/

  // Confirms <base> is w/in range [2,36] of possible number systems
  constexpr bool confirm_base_in_range(const int& base) noexcept {
    return base > 1 && base < 37;
  }


  // Confirms <bnum> only contains digits in range of the <base> number system
  bool confirm_valid_base_digits(const int& base, const std::string& bnum) noexcept {
    for(const auto& digit : bnum)
      if(!is_base_digit(digit,base))
        return false;
    return true;
  }


  // Returns <digit>'s base-36 representation
  const char * base_digit_conversion(const Snum& digit, const int& base) noexcept {
    static constexpr const char * const base_36_digits[] = {
      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
      "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", 
      "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
      "U", "V", "W", "X", "Y", "Z"
    };
    return base_36_digits[std::stoi(digit.round().extract_exact()) % base];
  }


  constexpr char HASH_BASE_10_TO_BASE_N(unsigned n)noexcept{
    if(n < 10) return n + '0';
    return n - 10 + 'A';
  }

  constexpr unsigned HASH_BASE_N_TO_BASE_10(char n)noexcept{
    if(n <= '9') return n - '0';
    if(n <= 'Z') return n - 'A' + 10;
    return n - 'a' + 10;
  }


  // Evaluates the numerator & denominator of a base-N # & divides their results
  Snum Snum::form_decimal_fraction(const int& base,const size_type& i,const std::string& bnum,const bool& is_neg)const noexcept{
    if(i == 0) {
      Snum tmp; tmp.stat = status::nan; return tmp; // Invalid Snum Given: Can't begin with '/'!
    }
    return convert_base_N_to_dec(base,bnum.substr(0,i)) 
            / convert_base_N_to_dec(base,bnum.substr(i+1, bnum.size()-i-1)) 
            * (is_neg ? -1 : 1);
  }


  // Evaluates the integral & fractional of a base-N # & combines their results
  Snum Snum::form_decimal_floating_pt(const int& base,const size_type& i,const std::string& bnum,const bool& is_neg)const noexcept{
    auto fractional = convert_base_N_decimal_to_dec(base,bnum.substr(i+1, bnum.size()-i-1));
    if(fractional == "+nan.0") {Snum tmp; tmp.stat = status::nan; return tmp;}
    if(i == 0) return Snum("0." + fractional) * (is_neg ? -1 : 1);
    return (convert_base_N_to_dec(base,bnum.substr(0,i)) + Snum("0." + fractional)) * (is_neg ? -1 : 1);
  }


  // Evaluates the numerator & denominator of a decimal # & divides their results
  std::string Snum::form_base_N_fraction(const int& base,const Snum& dnum) const noexcept {
    auto numer = convert_dec_to_base_N(base,convert_exact_to_string(dnum.numerator,dnum.nlen));
    if(numer == "+nan.0") return numer;
    if(dnum.dlen != 1 || dnum.denominator[0] != 1) {
      auto denom = convert_dec_to_base_N(base,convert_exact_to_string(dnum.denominator,dnum.dlen));
      if(denom == "+nan.0") return denom;
      if(dnum.is_neg()) return '-' + numer + '/' + denom;
      return numer + '/' + denom;
    }
    if(dnum.is_neg()) return '-' + numer;
    return numer;
  }


  // Evaluates the integral & fractional of a decimal # & combines their results
  std::string Snum::form_base_N_floating_pt(const int& base, const Snum& dnum) const noexcept {
    std::string dnum_str = convert_numeric_to_str(dnum.float_num);
    size_type decimal_idx = dnum_str.find_first_of('.');
    if(decimal_idx == 1 && dnum_str[0] == '0') // only a fractional, no integral
      return (dnum.is_neg() ? "-0." : "0.") + 
              convert_dec_decimal_to_base_N(base,dnum_str.substr(
                decimal_idx+1, dnum_str.size()-decimal_idx-1)
              );
    auto integral_part = convert_dec_to_base_N_flonum(base,dnum_str.substr(0,decimal_idx));
    if(integral_part == "+nan.0") return integral_part;
    return (dnum.is_neg() ? "-" : "") + integral_part + "." + 
              convert_dec_decimal_to_base_N(base,dnum_str.substr(
                decimal_idx+1, dnum_str.size()-decimal_idx-1)
              );
  }

  // Check if any early cases of conversion can be leveraged
  std::string Snum::convert_dec_to_base_N_preliminary_check(const int& base, const Snum& dnum)const noexcept{
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

    return ""; // No cases found
  }

  /******************************************************************************
  * BASE CONVERSION INTEGER MAIN FCNS
  ******************************************************************************/

  // Convert a base-n big int string into a decimal Snum
  // PRECONDITION: <bnum> must be an INTEGER
  Snum Snum::convert_base_N_to_dec(const int& base, std::string bnum) const noexcept {
    if(!confirm_base_in_range(base)) {
      // Invalid Snum Base Given, Must be in Range: [2,36]!
      Snum tmp; tmp.stat = status::nan; return tmp; 
    }
    if(base == 10 || (bnum[0] == '0' && !bnum[1]) || bnum == "0.0")
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

    // Convert bigint base N -> base 10
    using exactVec_t = std::vector<exact_val_t>;
    Snum BASE_10_HORNER_SCHEME(const exactVec_t&,const Snum&)noexcept;
    void convert_string_to_exactVec(const std::string&, exactVec_t&)noexcept;
    exactVec_t baseVec;
    convert_string_to_exactVec(bnum,baseVec);
    if(is_neg) return Snum("-1") * BASE_10_HORNER_SCHEME(baseVec,base);
    return BASE_10_HORNER_SCHEME(baseVec,base);
  }


  std::string Snum::convert_dec_to_base_N_flonum(const int& base, const Snum& dnum)const noexcept{
    if(auto early_case = convert_dec_to_base_N_preliminary_check(base,dnum); !early_case.empty())
      return early_case;

    Snum D = dnum.abs();
    std::string bnum;
    
    // From right to left, determine the next digit via %
    while(D >= base) {
      bnum += base_digit_conversion(D % base, base);
      D = (D / base).floor();
    }
    bnum += base_digit_conversion(D, base);
    // Processed from right to left
    if(dnum.is_neg()) return '-' + std::string(bnum.rbegin(), bnum.rend());
    return std::string(bnum.rbegin(), bnum.rend());
  }


  // Converts the <dnum> decimal # into a <base> #
  //   => NOTE: approximates the conversion if dnum > INEXACT_PRECISION
  // PRECONDITION: <dnum> must be an INTEGER
  std::string Snum::convert_dec_to_base_N(const int& base, const Snum& dnum) const noexcept {
    if(auto early_case = convert_dec_to_base_N_preliminary_check(base,dnum); !early_case.empty())
      return early_case;

    // Convert bigint
    using exactVec_t = std::vector<exact_val_t>;
    exactVec_t BASE_N_REPEATED_DIV(exactVec_t&,exactVec_t&)noexcept;
    std::string convert_exactVec_to_string(const exactVec_t& arr)noexcept;

    Snum D = dnum.to_exact().abs();
    exactVec_t decVec(D.nlen), baseVec(1 + (base > 9));
    for(size_type i = 0; i < D.nlen; ++i) decVec[i] = D.numerator[i];
    if(base < 10) {
      baseVec[0] = base;
    } else {
      baseVec[0] = base/10, baseVec[1] = base%10;
    }
    if(dnum.is_neg()) {
      if(dnum.is_float)
        return '-' + convert_exactVec_to_string(BASE_N_REPEATED_DIV(decVec,baseVec)) + ".0";
      return '-' + convert_exactVec_to_string(BASE_N_REPEATED_DIV(decVec,baseVec));
    }
    if(dnum.is_float)
      return convert_exactVec_to_string(BASE_N_REPEATED_DIV(decVec,baseVec)) + ".0";
    return convert_exactVec_to_string(BASE_N_REPEATED_DIV(decVec,baseVec));
  }

  /******************************************************************************
  * BASE CONVERSION FLOATING POINT HELPER FCNS
  ******************************************************************************/

  // Erases redundant RHS padding 0s, trimming fractionals
  void Snum::reduce_redundant_RHS_0s(std::string& fractional) const noexcept {
    if(fractional.empty()) return;
    auto last_non_zero = fractional.end()-1;
    while(last_non_zero != fractional.begin() && *last_non_zero == '0') 
      --last_non_zero;
    fractional.erase(last_non_zero+1,fractional.end());
  }


  // Pad 0 to the LHS of a <base> fractional as needed for improved accuracy
  void Snum::pad_LHS_base_N_decimal_0s_as_needed(const int& base, const std::string& dnum, std::string& bnum) const noexcept {
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
  std::string Snum::convert_base_N_decimal_to_dec(const int& base, std::string bnum) const noexcept {
    // Return if nothing to convert or given an invalid "base" string of digits
    if(base == 10 || (bnum[0] == '0' && !bnum[1])) return bnum;
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
      decimal_fractional = std::string(INEXACT_PRECISION,'9');
    else
      decimal_fractional.erase(0,decimal_fractional.find_first_of('.')+1);
    // erase redundant RHS padding 0s
    reduce_redundant_RHS_0s(decimal_fractional);
    return decimal_fractional;
  }


  // Converts the given decimal floating point fractional to the <base> # system
  // POSTCONDITION: RETURNS THE DECIMAL FRACTIONAL AS A <base> FRACTIONAL
  std::string Snum::convert_dec_decimal_to_base_N(const int& base, std::string dnum) const noexcept {
    // Return if nothing to convert or given an invalid "base" string of digits
    if(base == 10 || Snum(dnum).is_zero()) return dnum;
    if(!confirm_valid_base_digits(10,dnum)) return "+nan.0";
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
    auto base_fractional = convert_dec_to_base_N_flonum(base, (Snum("0."+dnum) * 
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
  Snum::status Snum::coerce_int_to_float(inexact_t& num, const std::string& data_to_coerce, const bool data_is_neg) const noexcept {
    num = 0;
    try {                                   // try parsing out the floating point
      num = std::stold(data_to_coerce); 
    } catch(const std::out_of_range& err) { // catch error if out of range
      return (data_is_neg) ? status::ninf : status::pinf;
    } catch(...) {
      fprintf(stderr, "\n>> WARNING: _BIGINT_ BUG DETECTED AT %s:%s:%d,"
                      "\n   ERROR CONVERTING STRING TO A FLOAT!"
                      "\n>> PLEASE CONTACT jrandleman@scu.edu TO HELP FIX THIS BUG!"
                      "\n>> ERRORFUL STRING BEING COERCED: \"%s\"\n\n", 
              __FILE__,__func__,__LINE__,data_to_coerce.c_str());
      return status::nan;
    }
    return status::success;
  }


  // Coerce *this number (a confirmed fraction) into a float. Returns the success status, 
  // IE whether became a float as requested, OR resulted in +- inf.
  Snum::status Snum::coerce_fraction_to_float(inexact_t& num)const noexcept{
    const bool data_is_neg = is_neg();
    num = 0;
    if(dlen == 1 && denominator[0] == 0) return status::nan;        // n/0 = NaN
    if(nlen == 1 && numerator[0] == 0) return status::success;      // 0/n = 0
    if(unsigned_arrays_are_equal(numerator,nlen,denominator,dlen)){ // n/n = 1
      num = 1 - 2 * data_is_neg; // data_is_neg ? -1 : 1
      return status::success;
    }
    
    // Coerce the numeratorator & denominator to inexact_t's
    inexact_t num_double, den_double;
    const auto num_coercion_res = coerce_int_to_float(num_double, convert_exact_to_string(numerator,nlen), data_is_neg);
    const auto den_coercion_res = coerce_int_to_float(den_double, convert_exact_to_string(denominator,dlen), data_is_neg);
    // Check for the success status of either conversion
    if(den_coercion_res != status::success) { // denominator is inf
      if(num_coercion_res != status::success) // numerator is inf
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

  /******************************************************************************
  * BIGNUM REDUCTION, GCD, AND DIVISION HELPER FUNCTIONS
  ******************************************************************************/

  using exactVec_t = std::vector<Snum::exact_val_t>;

  namespace bit_to_decArr_conversion_helpers {
    // PERFORMS OPERATION IN REVERSE ON A REVERSE STRING
    void double_decArr(exactVec_t& decArr)noexcept{
      Snum::exact_val_t carry_over = 0;
      for(auto& digit : decArr) {
        auto mulres = 2 * digit + carry_over;
        carry_over = mulres/10;
        digit = mulres % 10;
      }
      if(carry_over) decArr.push_back(carry_over);
    }

    // PERFORMS OPERATION IN REVERSE ON A REVERSE STRING
    void increment_decArr(exactVec_t& decArr)noexcept{
      Snum::exact_val_t carry_over = 0;
      for(auto& digit : decArr) {
        auto sumres = 1 + digit + carry_over;
        carry_over = sumres/10;
        digit = sumres % 10;
        if(!carry_over) return;
      }
      if(carry_over) decArr.push_back(carry_over);
    }
  } // End of namespace bit_to_decArr_conversion_helpers



  namespace dec_to_bit_string_conversion_helpers {
    void rm_prepending_0s(Snum::exact_t& quotient, std::size_t& decArr_len)noexcept{
      std::size_t i = 0;
      while(i+1 < decArr_len && quotient[i] == 0) ++i;
      quotient += i; // advance ptr past 0s
      decArr_len -= i;
    }

    // NOTE: LEAVES QUOTIENT IN <decArr> ARG, RETURNS REMAINDER
    Snum::exact_val_t decStr_div2(Snum::exact_t& decArr, std::size_t& decArr_len)noexcept{
      if(!decArr_len) return 0;
      Snum::exact_val_t remainder = decArr[decArr_len-1] & 1;
      for(std::size_t i = 0, compare_num = 0; i < decArr_len; ++i) {
        compare_num += decArr[i];
        decArr[i] = compare_num/2;
        compare_num = (compare_num & 1) * 10; // remainder * 10
      }
      rm_prepending_0s(decArr,decArr_len);
      return remainder;
    }

    void repeatedDiv_recur(exactVec_t& binArr, Snum::exact_t decArr, std::size_t decArr_len)noexcept{
      binArr.push_back(decStr_div2(decArr,decArr_len));
      if(decArr_len != 1 || decArr[0] > 1)
        repeatedDiv_recur(binArr,decArr,decArr_len);
      else
        binArr.push_back(decArr[0]);
    }
  } // End of namespace dec_to_bit_string_conversion_helpers



  namespace binary_division_helpers {
    void remove_prepending_0s(exactVec_t& binArr)noexcept{
      std::size_t n = binArr.size()-1, i = 0;
      while(binArr[i] == 0 && i < n) ++i;
      binArr.erase(binArr.begin(),binArr.begin()+i);
    }

    // NON-MUTATING Means to rm prepending 0s
    void remove_prepending_0s_by_advancing_ptrs(Snum::exact_t& binArr, std::size_t& binArr_len)noexcept{
      std::size_t i = 0;
      while(binArr[i] == 0 && i < binArr_len-1) ++i;
      binArr += i, binArr_len -= i;
    }

    bool binArr_GTE(exactVec_t& lhs, exactVec_t& rhs)noexcept{
      // Use ptrs to skip past prefixing 0s w/o extra allocation
      Snum::exact_t lhs_data = lhs.data(), rhs_data = rhs.data();
      std::size_t lhs_len = lhs.size(), rhs_len = rhs.size();
      if(*lhs_data == 0) remove_prepending_0s_by_advancing_ptrs(lhs_data,lhs_len);
      if(*rhs_data == 0) remove_prepending_0s_by_advancing_ptrs(rhs_data,rhs_len);
      // Perform Binary >=
      if(lhs_len != rhs_len) return lhs_len >= rhs_len;
      for(std::size_t i = 0; i < lhs_len && i < rhs_len; ++i)
        if(lhs_data[i] != rhs_data[i])
          return lhs_data[i] > rhs_data[i];
      return true; // lhs == rhs
    }

    // PERFORMS - IN-PLACE IN <lhs>
    void binArr_SUBTRACT(exactVec_t& lhs, const exactVec_t& rhs)noexcept{
      if(lhs.size() < rhs.size()) return;
      // RM PRECEDING 0S TOO !!!
      Snum::exact_val_t carry_over = 0;
      std::size_t i = lhs.size()-1;
      std::size_t SIZE_TYPE_MAX = static_cast<std::size_t>(-1);
      for(std::size_t j = rhs.size()-1; i != SIZE_TYPE_MAX && j != SIZE_TYPE_MAX; --i, --j) {
        Snum::exact_val_t small_digit = rhs[j] + carry_over; // carry the one (or 0) as needed
        carry_over = lhs[i] < small_digit;
        lhs[i] = lhs[i] + (carry_over * 2) - small_digit; // add 2 to the big# digit if carrying over
      }
      // Propagate the last carry over
      for(; i != SIZE_TYPE_MAX; --i) {
        Snum::exact_val_t small_digit = carry_over;
        carry_over = lhs[i] < small_digit;
        lhs[i] = lhs[i] + (carry_over * 2) - small_digit;
      }
      // Remove prepending 0s
      if(lhs[0] == 0) remove_prepending_0s(lhs);
    }

    // Credit for pseudocode to: https://en.wikipedia.org/wiki/Division_algorithm
    void binArr_UNSIGNED_DIVIDE(exactVec_t& N, exactVec_t& D, exactVec_t& Q, exactVec_t& R)noexcept{
      // Check for Div by 0
      if(N.empty() || D.empty() || (D.size() == 1 && D[0] == 0)) return;
      // Perform long binary division
      for(std::size_t i = 0, n = N.size(); i < n; ++i) {
        R.push_back(0); // LEFT SHIFT BY 1
        *R.rbegin() = N[i];
        if(binArr_GTE(R,D)) {
          binArr_SUBTRACT(R,D);
          Q[i] = 1;
        }
      }
      if(Q[0] == 0) remove_prepending_0s(Q);
      if(R[0] == 0) remove_prepending_0s(R);
    }
  } // End of namespace binary_division_helpers



  // Suppose Binary string is stored as "1101" = 13
  // Uses the Horner Scheme for conversion
  exactVec_t b2Vec_to_b10Vec(const exactVec_t& binArr)noexcept{
    exactVec_t decArr;
    decArr.push_back(0);
    for(const auto& bit : binArr) {
      bit_to_decArr_conversion_helpers::double_decArr(decArr);
      if(bit) bit_to_decArr_conversion_helpers::increment_decArr(decArr);
    }
    // Reverse decArr (generated in reverse)
    const std::size_t decArr_len = decArr.size();
    for(std::size_t i = 0, n = decArr_len/2; i < n; ++i) {
      decArr[i] ^= decArr[decArr_len-1-i];
      decArr[decArr_len-1-i] ^= decArr[i];
      decArr[i] ^= decArr[decArr_len-1-i];
    }
    return decArr;
  }

  // Converts <decArr> to <exactVec_t>
  exactVec_t b10Vec_to_b2Vec(exactVec_t& decVec)noexcept{
    Snum::exact_t decArr = decVec.data();
    std::size_t decArr_len = decVec.size();
    // Generate bit string
    exactVec_t binArr;
    dec_to_bit_string_conversion_helpers::repeatedDiv_recur(binArr,decArr,decArr_len);
    // Reverse the conversion (generated in reverse)
    if(binArr.size() > 1 && *binArr.rbegin() == 0) binArr.pop_back(); // prefixing 0
    const std::size_t binArr_len = binArr.size();
    for(std::size_t i = 0, n = binArr_len/2; i < n; ++i) {
      binArr[i] ^= binArr[binArr_len-1-i];
      binArr[binArr_len-1-i] ^= binArr[i];
      binArr[i] ^= binArr[binArr_len-1-i];
    }
    return binArr;
  }

  // Performs decimal->binary, binary division, binary->decimal
  void BIGNUM_UNSIGNED_DIVIDE_core(exactVec_t Nbignum, exactVec_t Dbignum,
                                   exactVec_t& Qdec,    exactVec_t& Rdec)noexcept{
    auto Nbin = b10Vec_to_b2Vec(Nbignum);               // Decimal->Binary
    auto Dbin = b10Vec_to_b2Vec(Dbignum);               // Decimal->Binary
    exactVec_t Q(Nbin.size(),0), R(1,0);                   // Quotient & Remainder
    binary_division_helpers::binArr_UNSIGNED_DIVIDE(Nbin,Dbin,Q,R);
    Qdec = b2Vec_to_b10Vec(Q); // Binary->Decimal
    Rdec = b2Vec_to_b10Vec(R); // Binary->Decimal
  }

  exactVec_t BIGNUM_UNSIGNED_GCD_core(exactVec_t& Abignum, exactVec_t& Bbignum)noexcept{
    if(Bbignum.empty() || (Bbignum.size() == 1 && Bbignum[0] == 0)) return Abignum;
    exactVec_t Q, R;
    BIGNUM_UNSIGNED_DIVIDE_core(Abignum,Bbignum,Q,R);
    return BIGNUM_UNSIGNED_GCD_core(Bbignum,R);
  }

  void BIGNUM_GET_REDUCED_core(exactVec_t& Nbignum, exactVec_t& Dbignum, exactVec_t& Nresult, exactVec_t& Dresult)noexcept{
    auto GCDbignum = BIGNUM_UNSIGNED_GCD_core(Nbignum,Dbignum);
    if(GCDbignum.size() == 1 && GCDbignum[0] == 1) {
      Nresult = Nbignum, Dresult = Dbignum;
      return;
    }
    exactVec_t R; // dummy remainder is ignored
    BIGNUM_UNSIGNED_DIVIDE_core(Nbignum,GCDbignum,Nresult,R);
    R.clear();
    BIGNUM_UNSIGNED_DIVIDE_core(Dbignum,GCDbignum,Dresult,R);
  }

  /******************************************************************************
  * BIGNUM BASE CONVERSION (ANY BASE TO ANY BASE, FROM 2-36)
  ******************************************************************************/

  // FAST Specialization macro to convert base 10 to base 4,8,16,32
  #define BASE_10_TO_BASE_2_POWER_CONVERTER(FUNCTION_NAME,BASE_2_LOG,CONVERSION_EQ) \
    exactVec_t FUNCTION_NAME(exactVec_t& decVec)noexcept{                           \
      if(decVec.empty()) return exactVec_t();                                       \
      /* Decimal->Binary */                                                         \
      auto binVec = b10Vec_to_b2Vec(decVec);                                        \
      const auto binVec_len = binVec.size();                                        \
      std::size_t i = (binVec_len % BASE_2_LOG), j = 0;                             \
      exactVec_t bNVec(binVec_len/BASE_2_LOG + bool(i));                            \
      /* prepend 0s to make bitstring length a factor of <BASE_2_LOG> */            \
      if(i) {                                                                       \
        exactVec_t prefix(BASE_2_LOG-i,0);                                          \
        binVec.insert(binVec.begin(),prefix.begin(),prefix.end());                  \
        i = 0;                                                                      \
      }                                                                             \
      while(i < binVec_len) {                                                       \
        bNVec[j++] = CONVERSION_EQ;                                                 \
        i += BASE_2_LOG;                                                            \
      }                                                                             \
      return bNVec;                                                                 \
    }
  // DECIMAL->BASE4
  BASE_10_TO_BASE_2_POWER_CONVERTER(b10Vec_to_b4Vec,2,binVec[i]*2+binVec[i+1])
  // DECIMAL->OCTAL
  BASE_10_TO_BASE_2_POWER_CONVERTER(b10Vec_to_b8Vec,3,binVec[i]*4+binVec[i+1]*2+binVec[i+2])
  // DECIMAL->HEX
  BASE_10_TO_BASE_2_POWER_CONVERTER(b10Vec_to_b16Vec,4,binVec[i]*8+binVec[i+1]*4+binVec[i+2]*2+binVec[i+3])
  // DECIMAL->BASE32
  BASE_10_TO_BASE_2_POWER_CONVERTER(b10Vec_to_b32Vec,5,binVec[i]*16+binVec[i+1]*8+binVec[i+2]*4+binVec[i+3]*2+binVec[i+4])

  #undef BASE_10_TO_BASE_2_POWER_CONVERTER

  // PRECONDITION: exactVec.size() <= 2
  Snum::exact_val_t convert_exactVec_to_UNSIGNED(const exactVec_t& v)noexcept{
    if(v.size() == 2) return v[0] * 10 + v[1];
    return v[0];
  }

  // BASE 10 -> BASE N
  exactVec_t BASE_N_REPEATED_DIV(exactVec_t& decNum, exactVec_t& baseNum)noexcept{
    if(baseNum.empty() || (baseNum.size() == 1 && baseNum[0] == 0)) return decNum;
    // Perform specialized power-of-2 base conversion
    if(baseNum.size() == 1) {
      if(baseNum[0] == 2) return b10Vec_to_b2Vec(decNum);
      if(baseNum[0] == 4) return b10Vec_to_b4Vec(decNum);
      if(baseNum[0] == 8) return b10Vec_to_b8Vec(decNum);
    } else if(baseNum.size() == 2) {
      if(baseNum[0] == 1) {
        if(baseNum[1] == 6) return b10Vec_to_b16Vec(decNum);
        if(baseNum[1] == 0) return decNum;
      } else if(baseNum[0] == 3 && baseNum[1] == 2) {
        return b10Vec_to_b32Vec(decNum);
      }
    }
    // Perform repeated division a9
    exactVec_t Q, R, baseNVec;
    scm_numeric::BIGNUM_UNSIGNED_DIVIDE_core(decNum,baseNum,Q,R);
    baseNVec.push_back(convert_exactVec_to_UNSIGNED(R));
    while(Q.size() != 1 || Q[0] != 0) {
      decNum.clear(), R.clear();
      scm_numeric::BIGNUM_UNSIGNED_DIVIDE_core(Q,baseNum,decNum,R);
      baseNVec.push_back(convert_exactVec_to_UNSIGNED(R));
      Q = decNum;
    }
    return exactVec_t(baseNVec.rbegin(),baseNVec.rend()); // conversion is generated in reverse
  }

  // BASE N -> BASE 10
  Snum BASE_10_HORNER_SCHEME(const exactVec_t& baseVec, const Snum& Base)noexcept{
    if(baseVec.empty()) return Snum();
    // Perform horner scheme a9
    Snum decVal;
    for(const auto& digit : baseVec)
      decVal = decVal * Base + digit;
    return decVal;
  }

  // Converts an <exactVec_t> to a std::string
  std::string convert_exactVec_to_string(const exactVec_t& arr)noexcept{
    std::string s;
    s.reserve(arr.size());
    for(std::size_t i = 0, n = arr.size(); i < n; ++i) s += HASH_BASE_10_TO_BASE_N(arr[i]);
    return s;
  }

  // Converts a std::string to a <exactVec_t>
  void convert_string_to_exactVec(const std::string& s, exactVec_t& arr)noexcept{
    arr.reserve(s.size());
    for(const auto& ch : s) arr.push_back(HASH_BASE_N_TO_BASE_10(ch));
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

#undef LD_FIXED_SNPRINTF_FORMAT_LOGIC
#undef LD_FIXED_SNPRINTF_FORMAT
#undef LD_SNPRINTF_FORMAT_LOGIC
#undef LD_SNPRINTF_FORMAT
#endif