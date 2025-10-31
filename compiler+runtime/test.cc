#include <iostream>
#include <boost/multiprecision/cpp_int.hpp>
#include <boost/multiprecision/cpp_dec_float.hpp>

// Use a namespace alias for brevity
namespace mp = boost::multiprecision;

int main() {
    // Define a 50-decimal-digit floating-point number
    mp::cpp_dec_float_50 my_dec_float("1234567890.1234567890");

    // Define an arbitrary-precision integer
    mp::cpp_int my_int("98765432109876543210");

    // Add the cpp_int to the cpp_dec_float.
    // The cpp_int is implicitly converted to cpp_dec_float_50 for the operation.
    // mp::cpp_dec_float_50 sum = my_dec_float + my_int;
    double c = 1.0;


    // Set the output precision to show all significant digits
    std::cout.precision(std::numeric_limits<mp::cpp_dec_float_50>::max_digits10);
    
    // Print the result
    std::cout << "cpp_dec_float: " << my_dec_float << std::endl;
    std::cout << "cpp_int:       " << my_int << std::endl;
    // std::cout << "Sum:           " << sum << std::endl;
    std::cout << "Sum of big int and double:           " << c + my_int.convert_to<double>() << std::endl;

    return 0;
}

