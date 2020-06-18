// Author: Jordan Randleman -- jrandleman@scu.edu -- embedded_heist_demo.cpp
// => Demos Heist's ability to act as an embedded scripting langauge w/in C++
#include "../heist_cpp_interop.hpp"


// C++ Factorial Function
auto big_int_cpp_factorial_recur(const heist::num_type& n,const heist::num_type& p=1){
  if(n == 0) return p;
  return big_int_cpp_factorial_recur(n-1,p*n);
}


// Heist C++ primitive signature: heist::data(*)(std::vector<heist::data>&)
heist::data my_factorial(std::vector<heist::data>& args){ // C++ Hiest User-Defined Primitive
  if(args.size() != 1 || !args[0].is_type(heist::types::num)) {
    std::cerr << "Invalid Arguments given to '!: " << args << '\n';
    return heist::data(); // empty data is of type "undefined" by default
  }
  return big_int_cpp_factorial_recur(args[0].num);
}



int main() {
  // Bind our C++ primitive to a symbol in Heist
  heist::defun("!", my_factorial);


  // Get the result of multiplying 50! by 2, using our factorial
  //   primitive defined in C++ above
  heist::data result = heist::eval("(* (! 50) 2)");
  std::cout << "(* (! 50) 2) = " << result << '\n';


  // Get the result of applying a list of arguments to a Heist procedure,
  //   in this case we choose our ! primitive (but could be any procedure)

  // -:- NOTE: heist::num_type LITERALS _MUST_ BE FOLLOWED BY THE "_n" SUFFIX! -:-
  //           (otherwise they get mixed up with characters)

  heist::data fact_of_5 = heist::apply("!", {5_n});
  std::cout << "Printed by C++, 5! = " << fact_of_5 << '\n';


  // Define a variable in the global environment with our 5! value
  heist::defvar("factorial-of-5", fact_of_5);


  // Evaluate Heist code directly via a string heist literal
  //   (equivalent to passing the string to "heist::eval")
  R"(

    (display "Printed by Heist, 5! = ")
    (display factorial-of-5)
    (newline)
  
  )"_heist;


  // Shadow our ! primitive with a new definition in Heist
  R"(

    (define (! n)
      (define (! n p)
        (if (zero? n)
            p
            (! (- n 1) (* n p))))
      (! n 1))

    (display "Shadowing our C++ '! w/ a Heist '!:\n\t50! = ")
    (display (! 50))
    (display "\nAdios!\n")

  )"_heist;
}