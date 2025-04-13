#pragma once
#include <string>
#include <cmath>
#include <limits>
#include <boost/multiprecision/cpp_int.hpp>

#include <jank/runtime/object.hpp>

namespace jank::runtime::obj
{
  using native_bigint = boost::multiprecision::cpp_int;

  using bigint_ptr = native_box<struct bigint>;

  struct bigint : gc
  {
    static constexpr object_type obj_type{ object_type::bigint };
    // cpp_int likely allocates, so not pointer_free
    static constexpr native_bool pointer_free{ false };

    // --- Constructors ---
    bigint() = default; // May not be needed
    bigint(native_bigint const &val);
    bigint(native_bigint &&val);
    // Consider constructors from string, long long later if needed

    // --- behavior::object_like ---
    native_bool equal(object const &other) const;
    native_persistent_string to_string() const;
    void to_string(util::string_builder &buff) const;
    native_persistent_string to_code_string() const;
    native_hash to_hash() const;

    // --- behavior::comparable ---
    // native_integer compare(object const& other) const;
    // native_integer compare(bigint const& other) const; // Specific overload

    // --- behavior::number_like ---
    native_integer to_integer() const;
    native_real to_real() const;

    object base{ obj_type };
    native_bigint data;
    mutable native_hash hash{}; // Cache hash if needed
  };

  // --- Operators involving native_bigint and native_real ---
  // Ensure native_real is defined (it's in object.hpp via native_box.hpp)

  inline native_bool operator>(native_real const l, native_bigint const &r)
  {
    // Consider potential exceptions during conversion for very large/small numbers
    try
    {
      return l > r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      return l > 0;
    } // Example approximation
  }

  inline native_bool operator>(native_bigint const &l, native_real const r)
  {
    try
    {
      return l.convert_to<native_real>() > r;
    }
    catch(...)
    { /* Handle exception? */
      return !(r > 0);
    } // Example approximation
  }

  inline native_bool operator<(native_real const l, native_bigint const &r)
  {
    try
    {
      return l < r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      return l < 0;
    } // Example approximation
  }

  inline native_bool operator<(native_bigint const &l, native_real const r)
  {
    try
    {
      return l.convert_to<native_real>() < r;
    }
    catch(...)
    { /* Handle exception? */
      return !(r < 0);
    } // Example approximation
  }

  inline native_bool operator>=(native_real const l, native_bigint const &r)
  {
    return !(l < r);
  }

  inline native_bool operator>=(native_bigint const &l, native_real const r)
  {
    return !(l < r);
  }

  inline native_bool operator<=(native_real const l, native_bigint const &r)
  {
    return !(l > r);
  }

  inline native_bool operator<=(native_bigint const &l, native_real const r)
  {
    return !(l > r);
  }

  // NOTE: Equality comparison between float/bigint is tricky. Use tolerance.
  inline native_bool operator==(native_real const l, native_bigint const &r)
  {
    try
    {
      native_real r_real = r.convert_to<native_real>();
      // Check if the absolute difference is within std::numeric_limits<native_real>::epsilon()
      return std::fabs(l - r_real) < std::numeric_limits<native_real>::epsilon();
    }
    catch(...)
    {
      // Conversion failed, they cannot be equal in this context
      return false;
    }
  }

  inline native_bool operator==(native_bigint const &l, native_real const r)
  {
    try
    {
      native_real l_real = l.convert_to<native_real>();
      // Check if the absolute difference is within std::numeric_limits<native_real>::epsilon()
      return std::fabs(l_real - r) < std::numeric_limits<native_real>::epsilon();
    }
    catch(...)
    {
      // Conversion failed, they cannot be equal in this context
      return false;
    }
  }

  inline native_bool operator!=(native_real const l, native_bigint const &r)
  {
    return !(l == r);
  }

  inline native_bool operator!=(native_bigint const &l, native_real const r)
  {
    return !(l == r);
  }

  inline native_real operator+(native_real const l, native_bigint const &r)
  {
    // Consider potential exceptions
    try
    {
      return l + r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      return l;
    }
  }

  inline native_real operator+(native_bigint const &l, native_real const r)
  {
    // Consider potential exceptions
    try
    {
      return l.convert_to<native_real>() + r;
    }
    catch(...)
    { /* Handle exception? */
      return r;
    }
  }

  inline native_real operator-(native_real const l, native_bigint const &r)
  {
    try
    {
      return l - r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      return l;
    }
  }

  inline native_real operator-(native_bigint const &l, native_real const r)
  {
    try
    {
      return l.convert_to<native_real>() - r;
    }
    catch(...)
    { /* Handle exception? */
      return -r;
    }
  }

  inline native_real operator*(native_real const l, native_bigint const &r)
  {
    try
    {
      return l * r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      return 0.0;
    }
  }

  inline native_real operator*(native_bigint const &l, native_real const r)
  {
    try
    {
      return l.convert_to<native_real>() * r;
    }
    catch(...)
    { /* Handle exception? */
      return 0.0;
    }
  }

  inline native_real operator/(native_real const l, native_bigint const &r)
  {
    if(r == 0)
    {
      throw std::runtime_error("Division by zero"); // Exact check
    }
    try
    {
      return l / r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      return l > 0 ? INFINITY : -INFINITY;
    }
  }

  inline native_real operator/(native_bigint const &l, native_real const r)
  {
    if(r == 0.0)
    {
      throw std::runtime_error("Division by zero");
    }
    try
    {
      return l.convert_to<native_real>() / r;
    }
    catch(...)
    { /* Handle exception? */
      return l > 0 ? INFINITY : -INFINITY;
    }
  }

} // namespace jank::runtime::obj

// Forward declare make_box specialization if needed (likely not if using templates)
// namespace jank::runtime {
//  extern template obj::bigint_ptr make_box<obj::bigint>(obj::native_bigint const&);
//  extern template obj::bigint_ptr make_box<obj::bigint>(obj::native_bigint&&);
// }
