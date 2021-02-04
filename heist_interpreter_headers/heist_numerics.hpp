// Author: Jordan Randleman -- jrandleman@scu.edu -- heist_numerics.hpp
// => Defines Numeric class for Heist Scheme Complex Numbers
// => NOTE: ACTS AS A WRAPPER AROUND 2 <class Snum_real> OBJECTS!

#ifndef HEIST_NUMERICS_HPP_
#define HEIST_NUMERICS_HPP_

#include "toolkits/heist_numerics_toolkit.hpp"

/***
 * -------------
 * CAPABILITIES:
 * -------------
 *
 * NOTE: Use the _n suffix for int, float, and string <Snum> literals!
 *       => Use _n2, _n8, & _n16 for bin, oct, & hex string <Snum> literals!
 *
 * (make-polar <real> <real>)
 * (make-rectangular <real> <real>)
 * (real-part <number>)
 * (imag-part <number>)
 * (conjugate <number>)
 * (magnitude <number>)
 * (angle <number>)
 * (complex? <number>)
 * (real? <number>)
 *
 * (+ <number> <number>)
 * (- <number> <number>)
 * (* <number> <number>)
 * (/ <number> <number>)
 *
 * (abs <real>)
 * (expt <number> <number>)
 * (quotient <real> <real>)
 * (remainder <real> <real>) // %
 * (modulo <real> <real>)
 * 
 * (equal? <number> <number>) // ==
 * (> <real> <real>)
 * (< <real> <real>)
 * 
 * (exp <number>)
 * (log <number>)
 * (sqrt <number>)
 * 
 * (odd? <real>)
 * (even? <real>)
 * (positive? <real>)
 * (negative? <real>)
 * (zero? <number>)
 * 
 * (ceiling <real>)  -- ROUNDS UP
 * (floor <real>)    -- ROUNDS DOWN
 * (truncate <real>) -- ROUNDS TOWARDS ZERO
 * (round <real>)    -- ROUNDS TOWARDS THE NEAREST INT
 * 
 * (inexact->exact <number>)
 * (exact->inexact <number>)
 * (exact? <number>)
 * (inexact? <number>)
 *
 * (integer? <number>)
 * (rational? <number>)
 * (infinite? <number>)
 * (nan? <number>)
 * (numerator <real>)
 * (denominator <real>)
 * 
 * (gcd <real> <real>)
 * (lcm <real> <real>)
 * 
 * (sin <number>),   (cos <number>),   (tan <number>)
 * (asin <number>),  (acos <number>),  (atan <number>), (atan2 <real> <real>)
 * (sinh <number>),  (cosh <number>),  (tanh <number>)
 * (asinh <number>), (acosh <number>), (atanh <number>)
 *
 * (logand <real>), (logor <real>),  (logxor <real>), (lognot <real>)     // & | ^ ~
 * (loglsl <real> <real>), (loglsr <real> <real>), (logasr <real> <real>) // << >> sign-extending>>
 * 
 * (random <optional-real>) -- BOTH w/ & w/o a seed (defaults to the current time since epoch)
 **/

namespace scm_numeric {

  // Complex Scheme Numeric Class
  class Snum {
    friend class Snum_real; // to enable Snum->Snum_real coercion

  public:
    // ******************************* CLASS TYPES & GENERAL STATS *******************************

    using inexact_t = Snum_real::inexact_t;  // flonum floating point
    static constexpr auto INEXACT_PRECISION = Snum_real::INEXACT_PRECISION;
    static constexpr auto RATIONALITY_LIMIT = Snum_real::RATIONALITY_LIMIT;
    static constexpr auto INEXACT_INF       = Snum_real::INEXACT_INF;
    static constexpr auto INEXACT_NAN       = Snum_real::INEXACT_NAN;
    static constexpr auto INEXACT_MAX       = Snum_real::INEXACT_MAX;
    static constexpr auto INEXACT_MIN       = Snum_real::INEXACT_MIN;
    static constexpr auto INEXACT_EPSILON   = Snum_real::INEXACT_EPSILON;


    // ******************************* CTORS *******************************

    // Default to 0
    Snum()noexcept{}
    // Given an Snum_real
    Snum(const Snum_real& realNum)noexcept : real(realNum) {}
    Snum(Snum_real&& realNum)noexcept : real(std::move(realNum)) {}
    // Given Snum_real real & imag components
    Snum(const Snum_real& realNum,const Snum_real& imagNum)noexcept : real(realNum), imag(imagNum) {synchronize_component_exactness();}
    Snum(Snum_real&& realNum,Snum_real&& imagNum)noexcept : real(std::move(realNum)), imag(std::move(imagNum)) {synchronize_component_exactness();}
    // Copy Ctors
    Snum(const Snum& c)noexcept : real(c.real), imag(c.imag) {}
    Snum(Snum&& c)noexcept : real(std::move(c.real)), imag(std::move(c.imag)) {}
    // Given a C++ numeric real
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    Snum(NumericData data) noexcept {*this = Snum_real(data);}
    // Given 2 C++ numeric reals
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    Snum(NumericData real_data, NumericData imag_data) noexcept {real = Snum_real(real_data), imag = Snum_real(imag_data);}
    // Given a string (w/ or w/o a radix base)
    Snum(const std::string& data) noexcept {construct_complex_number<false>(data);}
    Snum(const std::string& data, const int& base) noexcept {construct_complex_number<true>(data,base);}

    // Generate an Snum given 2 real Snum's repning the polar values of an Snum
    static Snum make_polar(const Snum& mag, const Snum& ang)noexcept{
      if(mag.is_nan() || ang.is_nan() || !mag.imag.is_zero() || !ang.imag.is_zero())
        return Snum_real("+nan.0");
      return Snum(mag.real * ang.real.cos(), mag.real * ang.real.sin());
    }

    // Generate an Snum given 2 real Snum's repning the rectangular values of an Snum
    static Snum make_rectangular(const Snum& r, const Snum& i)noexcept{
      if(r.is_nan() || i.is_nan() || !r.imag.is_zero() || !i.imag.is_zero())
        return Snum_real("+nan.0");
      return Snum(r.real, i.real);
    }


    // ******************************* ASSIGNMENT *******************************
    Snum& operator=(const Snum& c)noexcept{real = c.real, imag = c.imag; return *this;}
    Snum& operator=(Snum&& c)noexcept{real = std::move(c.real), imag = std::move(c.imag); return *this;}


    // ******************************* ACCESSORS *******************************
    Snum real_part()const noexcept{if(is_nan()) return Snum_real("+nan.0"); return real;}
    Snum imag_part()const noexcept{if(is_nan()) return Snum_real("+nan.0"); return imag;}
    Snum conjugate()const noexcept{if(is_nan()) return Snum_real("+nan.0"); return Snum(real,-imag);}
    Snum magnitude()const noexcept;
    Snum angle()    const noexcept;


    // ********************** PRECISION CONVERSION METHODS **********************

    Snum to_inexact()          const noexcept {return Snum(real.to_inexact(), imag.to_inexact());}
    Snum to_exact()            const noexcept {return Snum(real.to_exact(), imag.to_exact());}
    Snum extract_numerator()   const noexcept {if(imag.is_zero()) return real.extract_numerator(); return Snum_real("+nan.0");}
    Snum extract_denominator() const noexcept {if(imag.is_zero()) return real.extract_denominator(); return Snum_real("+nan.0");}


    // ************************ PRIMITIVE TYPES COERCION ************************

    inexact_t extract_inexact() const noexcept {if(imag.is_zero()) return real.extract_inexact(); return INEXACT_NAN;}
    std::string extract_exact() const noexcept {if(imag.is_zero()) return real.extract_exact(); return "+nan.0";}


    // ************************ MISCELLANEOUS PREDICATES ************************

    // Whether Number is Integeral
    bool is_integer() const noexcept {return imag.is_zero() && real.is_integer();}

    // Whether Number is Exact (Fractional) or Inexact (Float)
    constexpr bool is_exact()   const noexcept {return real.is_exact() && imag.is_exact();}
    constexpr bool is_inexact() const noexcept {return (real.is_inexact() && (imag.is_inexact() || imag.is_zero())) || (real.is_zero() && imag.is_inexact());}
    
    // Sign Check
    constexpr bool is_pos()  const noexcept {return imag.is_zero() && real.is_pos();}
    constexpr bool is_zero() const noexcept {return imag.is_zero() && real.is_zero();}
    constexpr bool is_neg()  const noexcept {return imag.is_zero() && real.is_neg();}

    // Special State Getters
    constexpr bool is_pos_inf() const noexcept {return imag.is_zero() && real.is_pos_inf();}
    constexpr bool is_neg_inf() const noexcept {return imag.is_zero() && real.is_neg_inf();}
    constexpr bool is_nan()     const noexcept {return imag.is_nan() || real.is_nan();}

    // Parity check
    bool is_even() const noexcept {return imag.is_zero() && real.is_even();}
    bool is_odd()  const noexcept {return imag.is_zero() && real.is_odd();}

    // Realness, Rationality, & Complexity check
    constexpr bool is_real()     const noexcept {return imag.is_zero();} // +nan.0 is considered <real>
              bool is_rational() const noexcept {return imag.is_zero() && real.is_rational();}
              bool is_complex() const noexcept  {return !is_nan() && !imag.is_zero();}


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

    // overloaded arithmetic operators
    Snum  operator+ (const Snum& s) const noexcept{return Snum(real + s.real, imag + s.imag);}
    Snum& operator+=(const Snum& s)noexcept{*this = *this + s; return *this;}
    Snum  operator-()const noexcept{return Snum(-real, -imag);}
    Snum  operator- (const Snum& s) const noexcept{return *this + -s;}
    Snum& operator-=(const Snum& s)noexcept{*this = *this - s; return *this;}
    // (x+yi)(u+vi) = (xu-yv)+(xv+yu)i
    Snum  operator* (const Snum& s) const noexcept;
    Snum& operator*=(const Snum& s)noexcept{*this = *this * s; return *this;}
    // (a+bi)/(c+di) = [(ac+bd)/(c*c+d*d)]+[(bc-ad)/(c*c+d*d)]i
    Snum  operator/ (const Snum& s) const noexcept;
    Snum& operator/=(const Snum& s)noexcept{*this = *this / s; return *this;}
    Snum  operator% (const Snum& s) const noexcept{if(imag.is_zero() && s.imag.is_zero()) return real % s.real; return Snum_real("+nan.0");}
    Snum& operator%=(const Snum& s)noexcept{*this = *this % s; return *this;}
    std::pair<Snum,Snum> divmod(const Snum& s) const noexcept;

    Snum& operator++()   noexcept{*this = *this + Snum(Snum_real("1")); return *this;}
    Snum& operator--()   noexcept{*this = *this - Snum(Snum_real("1")); return *this;}
    Snum  operator++(int)noexcept{Snum tmp = *this; *this = *this + Snum(Snum_real("1")); return tmp;}
    Snum  operator--(int)noexcept{Snum tmp = *this; *this = *this - Snum(Snum_real("1")); return tmp;}

    // overloaded friend arithmetic operators
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator+(const NumericData& lhs,const Snum& rhs)noexcept{return rhs + Snum_real(lhs);}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator-(const NumericData& lhs,const Snum& rhs)noexcept{return -rhs + Snum_real(lhs);}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator*(const NumericData& lhs,const Snum& rhs)noexcept{return rhs * Snum_real(lhs);}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator/(const NumericData& lhs,const Snum& rhs)noexcept{Snum tmp=Snum_real(lhs); return tmp / rhs;}
    template<typename NumericData,typename=typename std::enable_if<std::is_arithmetic<NumericData>::value,NumericData>::type>
    friend Snum operator%(const NumericData& lhs,const Snum& rhs)noexcept{Snum tmp=Snum_real(lhs); return tmp % rhs;}

    friend Snum operator+(const Snum_real& lhs,const Snum& rhs)noexcept{return rhs + lhs;}
    friend Snum operator-(const Snum_real& lhs,const Snum& rhs)noexcept{return -rhs + lhs;}
    friend Snum operator*(const Snum_real& lhs,const Snum& rhs)noexcept{return rhs * lhs;}
    friend Snum operator/(const Snum_real& lhs,const Snum& rhs)noexcept{Snum tmp=lhs; return tmp / rhs;}
    friend Snum operator%(const Snum_real& lhs,const Snum& rhs)noexcept{Snum tmp=lhs; return tmp % rhs;}


    // ************************ RANDOM NUMBER GENERATION ************************

    // pseudo random big int number generator
    static Snum random(Snum seed = Snum_real(std::chrono::system_clock::now().time_since_epoch().count()))noexcept{
      return Snum_real::random(seed.real);
    }


    // **************** MISCELLANEOUS DERIVED NUMERIC OPERATIONS ****************

    // Exponentiation
    // => w = rL(theta), z = c + di
    // => w^z = [(r^c) * exp(-d * theta)] * [cos(d*ln(r) + c*theta) + isin(d*ln(r) + c*theta)]
    Snum  expt(const Snum& s) const noexcept;
    Snum& expt_eq(const Snum& s)noexcept{*this = expt(s); return *this;}

    // greatest common denominator
    Snum gcd(const Snum& s) const noexcept{if(imag.is_zero() && s.imag.is_zero()) return real.gcd(s.real); return Snum_real("+nan.0");}
    // least common multiple
    Snum lcm(const Snum& s) const noexcept{if(imag.is_zero() && s.imag.is_zero()) return real.lcm(s.real); return Snum_real("+nan.0");}

    // exponential function
    Snum exp()  const noexcept;

    // sqrt function
    Snum sqrt() const noexcept;

    // NATURAL logarithm: ln(z) = (1/2)ln(a^2 + b^2) + atan2(b,a)i
    Snum log()  const noexcept;

    // absolute value
    Snum abs()  const noexcept {if(imag.is_zero()) return real.abs(); return Snum_real("+nan.0");}

    // quotient & modulo of being div'd by arg
    Snum quotient(const Snum& s) const noexcept {if(imag.is_zero() && s.imag.is_zero()) return real.quotient(s.real); return Snum_real("+nan.0");}
    Snum modulo  (const Snum& s) const noexcept {if(imag.is_zero() && s.imag.is_zero()) return real.modulo(s.real); return Snum_real("+nan.0");}


    // **************************** ROUNDING METHODS ****************************

    Snum ceil()  const noexcept {if(imag.is_zero()) return real.ceil(); return Snum_real("+nan.0");}
    Snum floor() const noexcept {if(imag.is_zero()) return real.floor(); return Snum_real("+nan.0");}
    Snum trunc() const noexcept {if(imag.is_zero()) return real.trunc(); return Snum_real("+nan.0");}
    Snum round() const noexcept {if(imag.is_zero()) return real.round(); return Snum_real("+nan.0");}


    // ****************** TRIGONOMETRIC METHODS -:- IN RADIANS ******************

    Snum sin()   const noexcept;
    Snum cos()   const noexcept;
    Snum tan()   const noexcept;
    Snum asin()  const noexcept;
    Snum acos()  const noexcept;
    Snum atan()  const noexcept;
    Snum atan2(const Snum& denom) const noexcept;
    Snum sinh()  const noexcept;
    Snum cosh()  const noexcept;
    Snum tanh()  const noexcept;
    Snum asinh() const noexcept;
    Snum acosh() const noexcept;
    Snum atanh() const noexcept;


    // ************************** COMPARISON OPERATORS **************************

    // overloaded equality/comparison operators
    bool operator==(const Snum& s) const noexcept {return real == s.real && imag == s.imag;}
    bool operator!=(const Snum& s) const noexcept {return real != s.real || imag != s.imag;}
    bool operator< (const Snum& s) const noexcept {return imag.is_zero() && s.imag.is_zero() && real < s.real;}
    bool operator> (const Snum& s) const noexcept {return imag.is_zero() && s.imag.is_zero() && real > s.real;}
    bool operator<=(const Snum& s) const noexcept {return !(*this > s);}
    bool operator>=(const Snum& s) const noexcept {return !(*this < s);}
    bool operator!()               const noexcept {return is_zero();}
    explicit operator bool()       const noexcept {return !is_zero();}

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

    friend bool operator==(const Snum_real& lhs, const Snum& rhs) noexcept {return rhs == lhs;}
    friend bool operator!=(const Snum_real& lhs, const Snum& rhs) noexcept {return rhs != lhs;}
    friend bool operator< (const Snum_real& lhs, const Snum& rhs) noexcept {return rhs > lhs;}
    friend bool operator> (const Snum_real& lhs, const Snum& rhs) noexcept {return rhs < lhs;}
    friend bool operator<=(const Snum_real& lhs, const Snum& rhs) noexcept {return rhs >= lhs;}
    friend bool operator>=(const Snum_real& lhs, const Snum& rhs) noexcept {return rhs <= lhs;}


    // *************************** LOGICAL-BIT OPERATORS **************************

    // overloaded bitwise operators
    Snum operator~()const noexcept {if(imag.is_zero()) return ~real; return Snum_real("+nan.0");}
    Snum operator& (const Snum& rhs) const noexcept {if(imag.is_zero() && rhs.imag.is_zero()) return real & rhs.real; return Snum_real("+nan.0");}
    Snum operator| (const Snum& rhs) const noexcept {if(imag.is_zero() && rhs.imag.is_zero()) return real | rhs.real; return Snum_real("+nan.0");}
    Snum operator^ (const Snum& rhs) const noexcept {if(imag.is_zero() && rhs.imag.is_zero()) return real ^ rhs.real; return Snum_real("+nan.0");}
    Snum operator<<(const Snum& rhs) const noexcept {if(imag.is_zero() && rhs.imag.is_zero()) return real << rhs.real; return Snum_real("+nan.0");}
    Snum operator>>(const Snum& rhs) const noexcept {if(imag.is_zero() && rhs.imag.is_zero()) return real >> rhs.real; return Snum_real("+nan.0");}
    Snum asr       (const Snum& rhs) const noexcept {if(imag.is_zero() && rhs.imag.is_zero()) return real.asr(rhs.real); return Snum_real("+nan.0");}

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

    friend Snum operator&(const Snum_real& lhs,const Snum& rhs) noexcept{Snum tmp=lhs; return tmp & rhs;}
    friend Snum operator|(const Snum_real& lhs,const Snum& rhs) noexcept{Snum tmp=lhs; return tmp | rhs;}
    friend Snum operator^(const Snum_real& lhs,const Snum& rhs) noexcept{Snum tmp=lhs; return tmp ^ rhs;}
    friend Snum operator<<(const Snum_real& lhs,const Snum& rhs)noexcept{Snum tmp=lhs; return tmp << rhs;}
    friend Snum operator>>(const Snum_real& lhs,const Snum& rhs)noexcept{Snum tmp=lhs; return tmp >> rhs;}


  private:
    // ******************************* PRIVATE MEMBER VALUES *******************************
    Snum_real real, imag;

    // ******************************* ADJUST MEMBERS TO MATCH EXACTNESS *******************************
    void synchronize_component_exactness()noexcept;

    // ******************************* PARSE STRING TO CONSTRUCT A NUMBER *******************************
    template<bool USING_A_BASE>
    void construct_complex_number(const std::string& data,const int& base = 10) noexcept;
  }; // End of class Snum

  /******************************************************************************
  * NARROWING OF COMPLEX SCHEME NUMERICS TO REAL SCHEME NUMERICS
  ******************************************************************************/

  Snum_real::Snum_real(const Snum& s)noexcept{
    *this = s.real;
  }

  Snum_real& Snum_real::operator=(const Snum& s)noexcept{
    *this = s.real;
    return *this;
  }

  /******************************************************************************
  * STRING PARSING CONSTRUCTOR HELPER
  ******************************************************************************/

  template<bool USING_A_BASE>
  void Snum::construct_complex_number(const std::string& data, const int& base) noexcept {
    // Verify using an appropriate base (as needed)
    if constexpr (USING_A_BASE) {
      if(!confirm_base_in_range(base)) {
        real = Snum_real("+nan.0");
        return;
      }
    }

    // Parse the given string into either 2 complex number components, OR into 1 real
    if(data.empty()) {
      real= Snum_real("+nan.0");
      return;
    }

    // Check for +i or -i
    const auto n = data.size();
    if(2 == n && (data[0] == '+' || data[0] == '-') && data[1] == 'i') {
      imag = (data[0] == '+') ? Snum_real("1") : Snum_real("-1");
      return;
    }

    // Check for whether a possible real
    if(*data.rbegin() != 'i') {
      if constexpr (USING_A_BASE) {
        real = Snum_real(data, base);
      } else {
        real = Snum_real(data);
      }
      return;
    }

    // Skip past initial sign
    std::size_t i = base-base; // optimized away to 0, and uses <base> to silence "unused var" GCC template warning
    if(data[i] == '+' || data[i] == '-') ++i;

    // Parse for number in 2 parts (real & imag)
    for(; i < n; ++i) {
      // Skip past next char after flonum exponential
      if(data[i] == 'e' || data[i] == 'E') {
        ++i; 
      // Check for a sign operation (if found not after an exponential, then a complex number)
      } else if(data[i] == '+' || data[i] == '-') {
        // Account for only having 'i' as imag
        bool imag_is_only_i = data[i+1] == 'i' && i+2 == n;
        if(imag_is_only_i) {
          if(data[i] == '+') imag = Snum_real("1");
          else               imag = Snum_real("-1");
        }
        if constexpr (USING_A_BASE) {
          real = Snum_real(std::string(data.begin(),data.begin()+i), base);
          if(!imag_is_only_i)
            imag = Snum_real(std::string(data.begin()+i,data.end()-1), base); // -1 to account for 'i'
        } else {
          real = Snum_real(std::string(data.begin(),data.begin()+i));
          if(!imag_is_only_i)
            imag = Snum_real(std::string(data.begin()+i,data.end()-1)); // -1 to account for 'i'
        }
        synchronize_component_exactness();
        return;
      }
    }

    // Only 1 part detected (presumably only the imaginary section)
    if constexpr (USING_A_BASE) {
      imag = Snum_real(std::string(data.begin(),data.end()-1), base);
    } else {
      imag = Snum_real(std::string(data.begin(),data.end()-1));
    }
  }

  /******************************************************************************
  * COMPLEX POLAR ACCESSORS
  ******************************************************************************/

  Snum Snum::magnitude()const noexcept{
    if(is_nan()) return Snum_real("+nan.0"); 
    if(imag.is_zero()) return real.abs();
    if(real.is_zero()) return imag.abs();
    return (real.expt(2) + imag.expt(2)).sqrt();
  }


  Snum Snum::angle()const noexcept{
    // (angle NaN) = (angle 0/0) = NaN
    if(is_nan() || (real.is_zero() && imag.is_zero())) {
      return Snum_real("+nan.0");
    }
    if(real.is_zero()) {
      if(imag.is_neg())
        return Snum_real(-1.0L * std::acos(0.0L)); // (angle 0-ni) = -pi/2
      return Snum_real(std::acos(0.0L));           // (angle 0+ni) = pi/2
    }
    if(imag.is_zero()) {
      if(real.is_neg())
        return Snum_real(std::acos(-1.0L)); // (angle -n) = pi
      return Snum_real();                   // (angle n) = 0
    }
    return imag.atan2(real); // USE ATAN2, ___NOT___ ATAN (jesus this bug was killing me)
  }

  /******************************************************************************
  * COMPLEX STRINGIFICATION
  ******************************************************************************/

  std::string Snum::str() const noexcept {
    if(is_nan()) return "+nan.0";
    if(imag.is_zero())
      return real.str();
    if(imag.is_pos_inf() || imag.is_neg_inf())
      return real.str() + imag.str() + 'i';
    if(imag.is_neg())
      return real.str() + imag.str() + 'i';
    return real.str() + '+' + imag.str() + 'i';
  }


  // get current value as a string in 'base' radix form
  std::string Snum::str(const int& base) const noexcept {
    if(is_nan()) return "+nan.0";
    if(imag.is_zero())
      return real.str(base);
    if(imag.is_pos_inf() || imag.is_neg_inf())
      return real.str(base) + imag.str() + 'i';
    if(imag.is_neg())
      return real.str(base) + imag.str(base) + 'i';
    return real.str(base) + '+' + imag.str(base) + 'i';
  }

  /******************************************************************************
  * MULTIPLICATION, DIVIDE, & DIVMOD
  ******************************************************************************/

  // (x+yi)(u+vi) = (xu-yv)+(xv+yu)i
  Snum Snum::operator*(const Snum& s) const noexcept {
    if(is_nan() || s.is_nan()) return Snum_real("+nan.0");
    bool this_is_real = is_real(), s_is_real = s.is_real();
    if(this_is_real && s_is_real) return real * s.real;
    if(this_is_real)              return Snum(real * s.real, real * s.imag);
    if(s_is_real)                 return Snum(real * s.real, imag * s.real);
    return Snum((real*s.real)-(imag*s.imag), (real*s.imag)+(imag*s.real));
    // // THE BELOW IS AN ALTERNATIVE IMPLEMENTATION, 
    // // BUT IT DOESN'T PRESERVE EXACTNESS AS WELL AS THE ABOVE
    // // ------------------------------------------------------
    // // z1 * z2 = (r1*r2)L(theta1 + theta2)
    // auto new_r = magnitude().real * s.magnitude().real;
    // auto new_theta = angle().real + s.angle().real;
    // return Snum(new_r * new_theta.cos(), new_r * new_theta.sin());
  }


  // (a+bi)/(c+di) = [(ac+bd)/(c*c+d*d)]+[(bc-ad)/(c*c+d*d)]i
  Snum Snum::operator/(const Snum& s) const noexcept {
    if(is_nan() || s.is_nan()) return Snum_real("+nan.0");
    bool this_is_real = is_real(), s_is_real = s.is_real();
    if(this_is_real && s_is_real) return real / s.real;
    if(s_is_real)                 return Snum(real / s.real, imag / s.real);
    auto denom = (s.real*s.real)+(s.imag*s.imag);
    return Snum(((real*s.real)+(imag*s.imag))/denom, ((imag*s.real)-(real*s.imag))/denom);
    // // THE BELOW IS AN ALTERNATIVE IMPLEMENTATION, 
    // // BUT IT DOESN'T PRESERVE EXACTNESS AS WELL AS THE ABOVE
    // // ------------------------------------------------------
    // // z1 / z2 = (r1/r2)L(theta1 - theta2)
    // auto new_r = magnitude().real / s.magnitude().real;
    // auto new_theta = angle().real - s.angle().real;
    // return Snum(new_r * new_theta.cos(), new_r * new_theta.sin());
  }


  // a.divmod(b) -> std::make_pair(a.quotient(b),a%b)
  std::pair<Snum,Snum> Snum::divmod(const Snum& s) const noexcept {
    // more efficient implementation for 2 real bigints
    if(is_exact() && is_integer() && s.is_exact() && s.is_integer()) {
      // using exactVec_t = std::vector<exact_val_t>;
      // void BIGNUM_UNSIGNED_DIVIDE_core(exactVec_t,exactVec_t,exactVec_t&,exactVec_t&)noexcept;
      exactVec_t aVec(real.nlen,0), bVec(s.real.nlen,0), quotient, remainder;
      for(std::size_t i = 0; i < real.nlen; ++i) aVec[i] = real.numerator[i];
      for(std::size_t i = 0; i < s.real.nlen; ++i) bVec[i] = s.real.numerator[i];
      BIGNUM_UNSIGNED_DIVIDE_core(aVec,bVec,quotient,remainder);
      Snum_real div("1"), mod("1");
      // assign div
      if(quotient.size() == 1 && quotient[0] == 0) {
        div = Snum_real(); // quotient is 0
      } else {
        div.resize_numerator(quotient.size());
        for(std::size_t i = 0, n = quotient.size(); i < n; ++i) div.numerator[div.nlen++] = quotient[i];
        if(real.sign != s.real.sign) div.sign = Snum_real::signs::neg;
      }
      // assign mod
      if(remainder.size() == 1 && remainder[0] == 0) {
        mod = Snum_real(); // remainder is 0
      } else {
        mod.resize_numerator(remainder.size());
        for(std::size_t i = 0, n = remainder.size(); i < n; ++i) mod.numerator[mod.nlen++] = remainder[i];
        mod.sign = real.sign;
      }
      return std::make_pair(Snum(std::move(div)),Snum(std::move(mod)));
    }
    return std::make_pair(quotient(s),operator%(s));
  }

  /******************************************************************************
  * EXPONENTIATION
  ******************************************************************************/

  // Exponentiation
  // => w = rL(theta), z = c + di
  // => w^z = [(r^c) * exp(-d * theta)] * [cos(d*ln(r) + c*theta) + isin(d*ln(r) + c*theta)]
  Snum Snum::expt(const Snum& s) const noexcept {
    if(is_nan() || s.is_nan()) return Snum_real("+nan.0");
    if(is_real() && s.is_real()) {
      // if NaN (ie sqrt(-1)), could have a complex result (attempted below)
      if(auto result = real.expt(s.real); !result.is_nan()) 
        return result;
    }
    Snum_real r = magnitude().real, t = angle().real;
    Snum_real trig_arg = (s.imag * r.log()) + (s.real * t);
    return r.expt(s.real) * (-s.imag * t).exp() * Snum(trig_arg.cos(), trig_arg.sin());
  }

  /******************************************************************************
  * EXP, LOG, AND SQRT
  ******************************************************************************/

  // exponential function
  Snum Snum::exp()  const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.exp();
    return Snum(std::exp(1.0L)).expt(*this);
  }

  // sqrt function
  Snum Snum::sqrt() const noexcept {
    if(is_nan()) return Snum_real("+nan.0");
    if(imag.is_zero()) {
      if(!real.is_neg()) return real.sqrt();
      return Snum(Snum_real(),(-real).sqrt());
    }
    return expt(Snum_real(0.5L));
  }

  // NATURAL logarithm: ln(z) = (1/2)ln(a^2 + b^2) + atan2(b,a)i
  Snum Snum::log()  const noexcept {
    if(is_nan() || (imag.is_zero() && real.is_zero())) return Snum_real("+nan.0");
    if(imag.is_zero()) {
      if(real.is_pos()) return real.log();
      return Snum((-real).log(), std::acos(-1.0L)); // ln(-n) = ln(n)+(pi)i
    }
    return Snum(Snum_real("1/2") * (real.expt(2)+imag.expt(2)).log(), angle().real);
  }

  /******************************************************************************
  * TRIGONOMETRIC METHODS
  ******************************************************************************/

  // NOTE: THE FOLLOWING OPERATIONS HAVE DOMAIN LIMITS FOR REALS:
  //       => asin, acos, acosh, atanh
  //       Where bypassing their limits results in a complex result!
  //       => Hence (for reals) we attempt the op, retrying as complex if NaN

  // sin(a+bi) = sin(a)cosh(b) + cos(a)sinh(b)i
  Snum Snum::sin()   const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.sin();
    return Snum(real.sin() * imag.cosh(), real.cos() * imag.sinh());
  }
  // cos(a+bi) = cos(a)cosh(b) + sin(a)sinh(b)i
  Snum Snum::cos()   const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.cos();
    return Snum(real.cos() * imag.cosh(), real.sin() * imag.sinh());
  }
  // tan(a+bi) = [sin(2a) + sinh(2b)i] / [cos(2a) + cosh(2b)]
  Snum Snum::tan()   const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.tan();
    return Snum((2*real).sin(),(2*imag).sinh()) / ((2*real).cos()+(2*imag).cosh());
  }
  // asin(z) = -i * ln(sqrt(1-z^2) + (z*i))
  Snum Snum::asin()  const noexcept {
    if(is_nan()) return Snum_real("+nan.0");
    if(imag.is_zero())
      if(auto result = real.asin(); !result.is_nan()) 
        return result;
    return Snum(0,-1) * ((1 - expt(Snum_real(2))).sqrt() + (*this * Snum(0,1))).log();
  }
  // acos(z) = (1/2)pi - asin(z)
  Snum Snum::acos()  const noexcept {
    if(is_nan()) return Snum_real("+nan.0");
    if(imag.is_zero()) {
      if(auto result = real.acos(); !result.is_nan()) 
        return result;
      // if real is beyond upper bound of 1, real of result is always 0
      auto result = std::acos(0.0L) - asin();
      if(real > 1) return Snum(Snum_real(),result.imag);
      return result;
    }
    return std::acos(0.0L) - asin();
  }
  // atan(z) = (1/(2i))ln((i-z)/(i+z))
  Snum Snum::atan()  const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.atan();
    return (1/Snum(0,2)) * ((Snum(0,1) - *this) / (Snum(0,1) + *this)).log();
  }
  // atan2(<real>,<real>) => UNDEFINED FOR COMPLEX ARGUMENTS
  Snum Snum::atan2(const Snum& denom) const noexcept {
    if(is_nan() || denom.is_nan() || !imag.is_zero() || !denom.imag.is_zero())
      return Snum_real("+nan.0");
    return real.atan2(denom.real);
  }
  // sinh(a+bi) = sinh(a)cos(b) + cosh(a)sin(b)i
  Snum Snum::sinh()  const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.sinh();
    return Snum(real.sinh() * imag.cos(), real.cosh() * imag.sin());
  }
  // cosh(a+bi) = cosh(a)cos(b) + sinh(a)sin(b)i
  Snum Snum::cosh()  const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.cosh();
    return Snum(real.cosh() * imag.cos(), real.sinh() * imag.sin());
  }
  // tanh(a+bi) = [sinh(2a) + sin(2b)i] / [cosh(2a) + cos(2b)]
  Snum Snum::tanh()  const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.tanh();
    return Snum((2*real).sinh(),(2*imag).sin()) / ((2*real).cosh()+(2*imag).cos());
  }
  // asinh(z) = ln(z + sqrt(z^2 + 1))
  Snum Snum::asinh() const noexcept {
    if(is_nan())       return Snum_real("+nan.0");
    if(imag.is_zero()) return real.asinh();
    return (*this + (expt(Snum_real(2)) + 1).sqrt()).log();
  }
  // acosh(z) = ln(z + sqrt(z + 1) * sqrt(z - 1))
  Snum Snum::acosh() const noexcept {
    if(is_nan()) return Snum_real("+nan.0");
    if(imag.is_zero())
      if(auto result = real.acosh(); !result.is_nan()) 
        return result;
    return (*this + ((*this + 1).sqrt() * (*this - 1).sqrt())).log();
  }
  // atanh(z) = (1/2) * ln((1+z)/(1-z))
  Snum Snum::atanh() const noexcept {
    if(is_nan()) return Snum_real("+nan.0");
    if(imag.is_zero())
      if(auto result = real.atanh(); !result.is_nan()) 
        return result;
    return Snum_real("1/2") * ((1 + *this) / (1 - *this)).log();
  }

  /******************************************************************************
  * CONSTRUCTION EXACTNESS SYNCHRONIZATION
  ******************************************************************************/

  void Snum::synchronize_component_exactness()noexcept{
    if(real.is_inexact() && imag.is_exact() && !imag.is_zero())
      imag = imag.to_inexact();
    else if(real.is_exact() && !real.is_zero() && imag.is_inexact())
      real = real.to_inexact();
  }
} // End of namespace scm_numeric

/******************************************************************************
* "_n" LITERAL SUFFIX FOR SCHEME COMPLEX NUMERICS
******************************************************************************/

scm_numeric::Snum operator"" _n(unsigned long long int n)    {return scm_numeric::Snum(scm_numeric::Snum_real(n));}
scm_numeric::Snum operator"" _n(long double n)               {return scm_numeric::Snum(scm_numeric::Snum_real(n));}
scm_numeric::Snum operator"" _n(const char* n, std::size_t)  {return scm_numeric::Snum(n);}
scm_numeric::Snum operator"" _n2(const char* n, std::size_t) {return scm_numeric::Snum(n,2);} // Binary
scm_numeric::Snum operator"" _n8(const char* n, std::size_t) {return scm_numeric::Snum(n,8);} // Octal
scm_numeric::Snum operator"" _n16(const char* n, std::size_t){return scm_numeric::Snum(n,16);}// Hexadecimal

#endif