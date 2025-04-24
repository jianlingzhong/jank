#include <functional>
#include <limits>
#include <stdexcept>
#include <cstddef>
#include <cmath>

#include <jank/runtime/core/make_box.hpp>
#include <jank/runtime/obj/big_integer.hpp>
#include <jank/runtime/obj/number.hpp>
#include <jank/runtime/obj/ratio.hpp>
#include <jank/runtime/rtti.hpp>
#include <jank/runtime/visit.hpp>
#include <jank/type.hpp>
#include <jank/util/fmt.hpp>

namespace jank::runtime::obj
{
  big_integer::big_integer()
    : data(0)
  {
  }

  big_integer::big_integer(native_big_integer const &val)
    : data(val)
  {
  }

  big_integer::big_integer(native_big_integer &&val)
    : data(std::move(val))
  {
  }

  big_integer::big_integer(native_integer val)
    : data(val)
  {
  }

  big_integer::big_integer(native_persistent_string_view const &s)
  {
    try
    {
      data.assign(std::string(s));
    }
    catch(std::exception const &e)
    {
      throw std::runtime_error(
        util::format("Failed to construct BigInteger from string '{}': {}", s, e.what()));
    }
  }

  native_bool big_integer::equal(object const &o) const
  {
    if(o.type == object_type::big_integer)
    {
      return data == expect_object<big_integer>(&o)->data;
    }
    if(o.type == object_type::integer)
    {
      return data == expect_object<integer>(&o)->data;
    }
    if(o.type == object_type::real)
    {
      // Use comparison with native_real
      return std::fabs(this->to_real() - expect_object<real>(&o)->data)
        < std::numeric_limits<native_real>::epsilon();
    }
    // if(o.type == object_type::ratio)
    // {
    //   /* By definition, an integer should never equal to a ratio. */
    //   return false;
    // }
    return false;
  }

  native_persistent_string big_integer::to_string() const
  {
    return data.str();
  }

  void big_integer::to_string(util::string_builder &buff) const
  {
    buff(data.str());
  }

  native_persistent_string big_integer::to_code_string() const
  {
    return to_string();
  }

  template <class T>
  inline void hash_combine(std::size_t &seed, T const &v)
  {
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }

  native_hash big_integer::to_hash() const
  {
    auto const &backend = data.backend();
    auto const *limbs = backend.limbs();
    auto const size = backend.size();
    auto const sign = backend.sign();

    std::size_t seed = static_cast<std::size_t>(sign); // Hash sign as 0 or 1
    for(unsigned i = 0; i < size; ++i)
    {
      hash_combine(seed, limbs[i]);
    }
    return static_cast<native_hash>(seed);
  }

  big_integer_ptr big_integer::gcd(big_integer_ptr l, big_integer_ptr r)
  {
    return make_box<big_integer>(boost::multiprecision::gcd(l->data, r->data));
  }

  native_integer big_integer::compare(object const &o) const
  {
    return visit_number_like(
      [this](auto const typed_o) -> native_integer {
        /* Converts to native_integer for now. */
        auto const i = to_integer();
        return (i > typed_o->data) - (i < typed_o->data);
      },
      [&]() -> native_integer {
        throw std::runtime_error{ util::format("not comparable: {}", runtime::to_string(&o)) };
      },
      &o);
  }

  native_integer big_integer::compare(big_integer const &o) const
  {
    return data.compare(o.data);
  }

  native_integer to_native_integer_wrapped(native_big_integer const &d)
  {
    // This implementation extracts the lower bits corresponding to a native_integer,
    // effectively performing arithmetic modulo 2^N (N=64 typically).

    using LimbType = boost::multiprecision::limb_type;
    constexpr std::size_t bits_per_limb = sizeof(LimbType) * CHAR_BIT;
    // Use unsigned long long for bit manipulation, then cast at the end
    constexpr std::size_t target_bits = sizeof(unsigned long long) * CHAR_BIT;

    auto const &backend = d.backend();
    auto const *limbs_ptr = backend.limbs();
    auto const num_limbs = backend.size();
    auto const sign = backend.sign();

    unsigned long long unsigned_result = 0;

    // Iterate through limbs contributing to the target bit width
    for(std::size_t i = 0;; ++i)
    {
      std::size_t current_limb_start_bit = i * bits_per_limb;
      if(current_limb_start_bit >= target_bits)
      {
        break; // Past target width
      }
      if(i >= num_limbs)
      {
        break; // Ran out of limbs
      }

      LimbType current_limb = limbs_ptr[i];

      // Mask if only part of the limb contributes
      std::size_t bits_to_take = bits_per_limb;
      if(current_limb_start_bit + bits_to_take > target_bits)
      {
        bits_to_take = target_bits - current_limb_start_bit;
        // Create mask: (1 << bits_to_take) - 1
        // Avoid shifting by full width or more
        if(bits_to_take < bits_per_limb)
        {
          LimbType mask = (static_cast<LimbType>(1) << bits_to_take) - 1;
          current_limb &= mask;
        } // else: no mask needed as we take the whole limb (up to target_bits)
      }

      // Combine into result
      unsigned_result |= (static_cast<unsigned long long>(current_limb) << current_limb_start_bit);
    }

    // Handle sign using two's complement logic for wrapping
    // Check the boolean sign variable directly
    if(sign) // If true, the original number was negative
    {
      return static_cast<native_integer>(~unsigned_result + 1);
    }
    else
    {
      return static_cast<long long>(unsigned_result);
    }
  }

  native_integer big_integer::to_integer() const
  {
    return to_native_integer_wrapped(data);
  }

  native_real big_integer::to_real() const
  {
    try
    {
      return data.template convert_to<native_real>();
    }
    catch(std::overflow_error const &)
    {
      return data < 0 ? -std::numeric_limits<native_real>::infinity()
                      : std::numeric_limits<native_real>::infinity();
    }
    catch(std::exception const &e)
    {
      throw std::runtime_error(
        util::format("Error converting BigInteger to native_real: {}", e.what()));
    }
  }

  // --- Kept Arithmetic Operators Implementation ---

  // Addition
  // big_integer_ptr operator+(big_integer_ptr l, big_integer_ptr r)
  // {
  //   return make_box<big_integer>(l->data + r->data);
  // }

  // big_integer_ptr operator+(big_integer_ptr l, integer_ptr r)
  // {
  //   return make_box<big_integer>(l->data + r->data);
  // }

  // big_integer_ptr operator+(integer_ptr l, big_integer_ptr r)
  // {
  //   return make_box<big_integer>(l->data + r->data);
  // }

  // big_integer_ptr operator+(big_integer_ptr l, native_integer r)
  // {
  //   return make_box<big_integer>(l->data + r);
  // }

  // big_integer_ptr operator+(native_integer l, big_integer_ptr r)
  // {
  //   return make_box<big_integer>(l + r->data);
  // }

  // Subtraction
  big_integer_ptr operator-(big_integer_ptr l, big_integer_ptr r)
  {
    return make_box<big_integer>(l->data - r->data);
  }

  big_integer_ptr operator-(big_integer_ptr l, integer_ptr r)
  {
    return make_box<big_integer>(l->data - r->data);
  }

  big_integer_ptr operator-(integer_ptr l, big_integer_ptr r)
  {
    return make_box<big_integer>(l->data - r->data);
  }

  big_integer_ptr operator-(big_integer_ptr l, native_integer r)
  {
    return make_box<big_integer>(l->data - r);
  }

  big_integer_ptr operator-(native_integer l, big_integer_ptr r)
  {
    return make_box<big_integer>(l - r->data);
  }

  // Multiplication
  big_integer_ptr operator*(big_integer_ptr l, big_integer_ptr r)
  {
    return make_box<big_integer>(l->data * r->data);
  }

  big_integer_ptr operator*(big_integer_ptr l, integer_ptr r)
  {
    return make_box<big_integer>(l->data * r->data);
  }

  big_integer_ptr operator*(integer_ptr l, big_integer_ptr r)
  {
    return make_box<big_integer>(l->data * r->data);
  }

  big_integer_ptr operator*(big_integer_ptr l, native_integer r)
  {
    return make_box<big_integer>(l->data * r);
  }

  big_integer_ptr operator*(native_integer l, big_integer_ptr r)
  {
    return make_box<big_integer>(l * r->data);
  }

  // Division
  big_integer_ptr operator/(big_integer_ptr l, big_integer_ptr r)
  {
    return make_box<big_integer>(l->data / r->data);
  }

  big_integer_ptr operator/(big_integer_ptr l, integer_ptr r)
  {
    return make_box<big_integer>(l->data / r->data);
  }

  big_integer_ptr operator/(integer_ptr l, big_integer_ptr r)
  {
    return make_box<big_integer>(l->data) / r;
  }

  big_integer_ptr operator/(big_integer_ptr l, native_integer r)
  {
    return l / make_box<big_integer>(r);
  }

  big_integer_ptr operator/(native_integer l, big_integer_ptr r)
  {
    return make_box<big_integer>(l) / r;
  }

  // --- Kept Comparison Operators Implementation ---
  native_bool operator==(big_integer_ptr l, big_integer_ptr r)
  {
    return l->data == r->data;
  }

  native_bool operator==(big_integer_ptr l, native_integer r)
  {
    return l->data == r;
  }

  // native_bool operator==(big_integer_ptr l, ratio_ptr r)
  // {
  // }

  native_bool operator==(integer_ptr l, big_integer_ptr r)
  {
    return l->data == r->data;
  }

  native_bool operator==(native_integer l, big_integer_ptr r)
  {
    return l == r->data;
  }

  native_bool operator!=(big_integer_ptr l, big_integer_ptr r)
  {
    return !(l->data == r->data);
  }

  native_bool operator!=(big_integer_ptr l, integer_ptr r)
  {
    return !(l == r);
  }

  native_bool operator!=(integer_ptr l, big_integer_ptr r)
  {
    return !(l == r);
  }

  native_bool operator!=(big_integer_ptr l, native_integer r)
  {
    return !(l == r);
  }

  native_bool operator!=(native_integer l, big_integer_ptr r)
  {
    return !(l == r);
  }

  native_bool operator<(big_integer_ptr l, big_integer_ptr r)
  {
    return l->data < r->data;
  }

  native_bool operator<(big_integer_ptr l, integer_ptr r)
  {
    return l->data < r->data;
  }

  native_bool operator<(integer_ptr l, big_integer_ptr r)
  {
    return l->data < r->data;
  }

  native_bool operator<(big_integer_ptr l, native_integer r)
  {
    return l->data < r;
  }

  native_bool operator<(native_integer l, big_integer_ptr r)
  {
    return l < r->data;
  }

  native_bool operator<=(big_integer_ptr l, big_integer_ptr r)
  {
    return l->data <= r->data;
  }

  native_bool operator<=(big_integer_ptr l, integer_ptr r)
  {
    return l->data <= r->data;
  }

  native_bool operator<=(integer_ptr l, big_integer_ptr r)
  {
    return l->data <= r->data;
  }

  native_bool operator<=(big_integer_ptr l, native_integer r)
  {
    return l->data <= r;
  }

  native_bool operator<=(native_integer l, big_integer_ptr r)
  {
    return l <= r->data;
  }

  native_bool operator>(big_integer_ptr l, big_integer_ptr r)
  {
    return l->data > r->data;
  }

  native_bool operator>(big_integer_ptr l, integer_ptr r)
  {
    return l->data > r->data;
  }

  native_bool operator>(integer_ptr l, big_integer_ptr r)
  {
    return l->data > r->data;
  }

  native_bool operator>(big_integer_ptr l, native_integer r)
  {
    return l->data > r;
  }

  native_bool operator>(native_integer l, big_integer_ptr r)
  {
    return l > r->data;
  }

  native_bool operator>=(big_integer_ptr l, big_integer_ptr r)
  {
    return l->data >= r->data;
  }

  native_bool operator>=(big_integer_ptr l, integer_ptr r)
  {
    return l->data >= r->data;
  }

  native_bool operator>=(integer_ptr l, big_integer_ptr r)
  {
    return l->data >= r->data;
  }

  native_bool operator>=(big_integer_ptr l, native_integer r)
  {
    return l->data >= r;
  }

  native_bool operator>=(native_integer l, big_integer_ptr r)
  {
    return l >= r->data;
  }

  // native_bool operator>(native_real l, native_big_integer r)
  // {
  //   return l > r;
  // }

} // namespace jank::runtime::obj
