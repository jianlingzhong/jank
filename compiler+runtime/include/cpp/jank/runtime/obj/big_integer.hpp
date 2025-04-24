#pragma once
#include <boost/multiprecision/cpp_int.hpp>

#include <jank/runtime/object.hpp>
#include <jank/type.hpp>

namespace jank::runtime::obj
{
  struct integer;
  using integer_ptr = native_box<integer>;
  struct ratio;
  using ratio_ptr = native_box<ratio>;
  struct real;
  using real_ptr = native_box<real>;
}

namespace jank::runtime::obj
{
  using native_big_integer = boost::multiprecision::cpp_int;
  struct big_integer;
  using big_integer_ptr = native_box<struct big_integer>;

  struct big_integer : gc
  {
    static constexpr object_type obj_type{ object_type::big_integer };
    static constexpr native_bool pointer_free{ false };

    big_integer();
    big_integer(big_integer &&) noexcept = default;
    big_integer(big_integer const &) = default;
    big_integer(native_big_integer const &val);
    big_integer(native_big_integer &&val);
    big_integer(native_integer val);
    explicit big_integer(native_persistent_string_view const &s);

    /* behavior::object_like */
    native_bool equal(object const &) const;
    native_persistent_string to_string() const;
    void to_string(util::string_builder &buff) const;
    native_persistent_string to_code_string() const;
    native_hash to_hash() const;

    /* behavior::comparable */
    native_integer compare(object const &) const;

    /* behavior::comparable extended */
    native_integer compare(big_integer const &) const;

    /* behavior::number_like */
    native_integer to_integer() const;
    native_real to_real() const;

    static big_integer_ptr gcd(big_integer_ptr, big_integer_ptr);

    object base{ obj_type };
    native_big_integer data;
  };

  // --- Kept Arithmetic Operators ---
  // BigInt + BigInt
  // big_integer_ptr operator+(big_integer_ptr l, big_integer_ptr r);
  // BigInt + Integer
  // big_integer_ptr operator+(big_integer_ptr l, integer_ptr r);
  // big_integer_ptr operator+(integer_ptr l, big_integer_ptr r);
  // big_integer_ptr operator+(big_integer_ptr l, native_integer r);
  // big_integer_ptr operator+(native_integer l, big_integer_ptr r);
  // BigInt + Real (promotes to Real)
  // real_ptr operator+(big_integer_ptr l, real_ptr r);
  // real_ptr operator+(real_ptr l, big_integer_ptr r);
  // real_ptr operator+(big_integer_ptr l, native_real r);
  // // BigInt + Ratio (promotes to Ratio)
  // ratio_ptr operator+(big_integer_ptr l, ratio_ptr r);
  // ratio_ptr operator+(ratio_ptr l, big_integer_ptr r);
  operator+(native_integer l, native_big_integer const &r);

  // BigInt - BigInt
  big_integer_ptr operator-(big_integer_ptr l, big_integer_ptr r);
  // BigInt - Integer
  big_integer_ptr operator-(big_integer_ptr l, integer_ptr r);
  big_integer_ptr operator-(integer_ptr l, big_integer_ptr r);
  big_integer_ptr operator-(big_integer_ptr l, native_integer r);
  big_integer_ptr operator-(native_integer l, big_integer_ptr r);
  // BigInt - Real (promotes to Real)
  // real_ptr operator-(big_integer_ptr l, real_ptr r);
  // real_ptr operator-(real_ptr l, big_integer_ptr r);
  // real_ptr operator-(big_integer_ptr l, native_real r);
  // BigInt - Ratio (promotes to Ratio)
  // ratio_ptr operator-(big_integer_ptr l, ratio_ptr r);
  // ratio_ptr operator-(ratio_ptr l, big_integer_ptr r);


  // BigInt * BigInt
  big_integer_ptr operator*(big_integer_ptr l, big_integer_ptr r);
  // BigInt * Integer
  big_integer_ptr operator*(big_integer_ptr l, integer_ptr r);
  big_integer_ptr operator*(integer_ptr l, big_integer_ptr r);
  big_integer_ptr operator*(big_integer_ptr l, native_integer r);
  big_integer_ptr operator*(native_integer l, big_integer_ptr r);
  // BigInt * Real (promotes to Real)
  // real_ptr operator*(big_integer_ptr l, real_ptr r);
  // real_ptr operator*(real_ptr l, big_integer_ptr r);
  // real_ptr operator*(big_integer_ptr l, native_real r);
  // BigInt * Ratio (promotes to Ratio or BigInt)
  // object_ptr operator*(big_integer_ptr l, ratio_ptr r);
  // object_ptr operator*(ratio_ptr l, big_integer_ptr r);


  // BigInt / BigInt (promotes to BigInt or Ratio)
  big_integer_ptr operator/(big_integer_ptr l, big_integer_ptr r);
  // BigInt / Integer (promotes to BigInt or Ratio)
  big_integer_ptr operator/(big_integer_ptr l, integer_ptr r);
  big_integer_ptr operator/(integer_ptr l, big_integer_ptr r);
  big_integer_ptr operator/(big_integer_ptr l, native_integer r);
  big_integer_ptr operator/(native_integer l, big_integer_ptr r);
  // BigInt / Real (promotes to Real)
  // real_ptr operator/(big_integer_ptr l, real_ptr r);
  // real_ptr operator/(real_ptr l, big_integer_ptr r);
  // real_ptr operator/(big_integer_ptr l, native_real r);
  // BigInt / Ratio (promotes to Ratio or BigInt)
  // object_ptr operator/(big_integer_ptr l, ratio_ptr r);
  // object_ptr operator/(ratio_ptr l, big_integer_ptr r);


  // --- Kept Comparison Operators ---
  native_bool operator==(big_integer_ptr l, big_integer_ptr r);
  native_bool operator==(big_integer_ptr l, native_integer r);
  native_bool operator==(integer_ptr l, big_integer_ptr r);
  native_bool operator==(native_integer l, big_integer_ptr r);
  // native_bool operator==(big_integer_ptr l, real_ptr r);
  // native_bool operator==(real_ptr l, big_integer_ptr r);
  // native_bool operator==(big_integer_ptr l, native_real r);
  // native_bool operator==(native_real l, big_integer_ptr r);
  // native_bool operator==(big_integer_ptr l, ratio_ptr r);
  // native_bool operator==(ratio_ptr l, big_integer_ptr r);

  // != (implemented using ==)
  native_bool operator!=(big_integer_ptr l, big_integer_ptr r);
  native_bool operator!=(big_integer_ptr l, integer_ptr r);
  native_bool operator!=(integer_ptr l, big_integer_ptr r);
  native_bool operator!=(big_integer_ptr l, native_integer r);
  native_bool operator!=(native_integer l, big_integer_ptr r);
  // native_bool operator!=(big_integer_ptr l, real_ptr r);
  // native_bool operator!=(real_ptr l, big_integer_ptr r);
  // native_bool operator!=(big_integer_ptr l, native_real r);
  // native_bool operator!=(big_integer_ptr l, ratio_ptr r);
  // native_bool operator!=(ratio_ptr l, big_integer_ptr r);

  // <
  native_bool operator<(big_integer_ptr l, big_integer_ptr r);
  native_bool operator<(big_integer_ptr l, integer_ptr r);
  native_bool operator<(integer_ptr l, big_integer_ptr r);
  native_bool operator<(big_integer_ptr l, native_integer r);
  native_bool operator<(native_integer l, big_integer_ptr r);
  // native_bool operator<(big_integer_ptr l, real_ptr r);
  // native_bool operator<(real_ptr l, big_integer_ptr r);
  // native_bool operator<(big_integer_ptr l, native_real r);
  // native_bool operator<(big_integer_ptr l, ratio_ptr r);
  // native_bool operator<(ratio_ptr l, big_integer_ptr r);

  // <=
  native_bool operator<=(big_integer_ptr l, big_integer_ptr r);
  native_bool operator<=(big_integer_ptr l, integer_ptr r);
  native_bool operator<=(integer_ptr l, big_integer_ptr r);
  native_bool operator<=(big_integer_ptr l, native_integer r);
  native_bool operator<=(native_integer l, big_integer_ptr r);
  // native_bool operator<=(big_integer_ptr l, real_ptr r);
  // native_bool operator<=(real_ptr l, big_integer_ptr r);
  // native_bool operator<=(big_integer_ptr l, native_real r);
  // native_bool operator<=(big_integer_ptr l, ratio_ptr r);
  // native_bool operator<=(ratio_ptr l, big_integer_ptr r);

  // >
  native_bool operator>(big_integer_ptr l, big_integer_ptr r);
  native_bool operator>(big_integer_ptr l, integer_ptr r);
  native_bool operator>(integer_ptr l, big_integer_ptr r);
  native_bool operator>(big_integer_ptr l, native_integer r);
  native_bool operator>(native_integer l, big_integer_ptr r);
  // native_bool operator>(big_integer_ptr l, real_ptr r);
  // native_bool operator>(real_ptr l, big_integer_ptr r);
  // native_bool operator>(big_integer_ptr l, native_real r);
  // // native_bool operator>(native_real l, native_big_integer r);
  // native_bool operator>(big_integer_ptr l, ratio_ptr r);
  // native_bool operator>(ratio_ptr l, big_integer_ptr r);

  // >=
  native_bool operator>=(big_integer_ptr l, big_integer_ptr r);
  native_bool operator>=(big_integer_ptr l, integer_ptr r);
  native_bool operator>=(integer_ptr l, big_integer_ptr r);
  native_bool operator>=(big_integer_ptr l, native_integer r);
  native_bool operator>=(native_integer l, big_integer_ptr r);
  // native_bool operator>=(big_integer_ptr l, real_ptr r);
  // native_bool operator>=(real_ptr l, big_integer_ptr r);
  // native_bool operator>=(big_integer_ptr l, native_real r);
  // native_bool operator>=(big_integer_ptr l, ratio_ptr r);
  // native_bool operator>=(ratio_ptr l, big_integer_ptr r);


} // namespace jank::runtime::obj

namespace std
{
  template <>
  struct hash<jank::runtime::obj::big_integer_ptr>
  {
    size_t operator()(jank::runtime::obj::big_integer_ptr const o) const noexcept
    {
      return o ? o->to_hash() : 0;
    }
  };

  template <>
  struct hash<jank::runtime::obj::big_integer>
  {
    size_t operator()(jank::runtime::obj::big_integer const &o) const noexcept
    {
      return o.to_hash();
    }
  };
} // namespace std
