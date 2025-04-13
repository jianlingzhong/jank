#include <random>
#include <boost/multiprecision/cpp_int.hpp>
#include <boost/rational.hpp>
#include <stdexcept> // For std::runtime_error in division

#include <jank/runtime/core/math.hpp>
#include <jank/runtime/behavior/number_like.hpp>
#include <jank/runtime/visit.hpp>
#include <jank/runtime/obj/number.hpp>
#include <jank/runtime/core/make_box.hpp>
#include <jank/util/fmt.hpp>
#include <jank/runtime/obj/bigint.hpp>
#include <boost/numeric/conversion/cast.hpp> // Add this include

namespace jank::runtime
{
  template <typename T>
  static auto to_number(T const &t)
  {
    if constexpr(std::same_as<T, obj::ratio_data>)
    {
      return t.to_real();
    }
    else
    {
      return t;
    }
  }

  inline native_real operator+(obj::native_bigint const &l, native_real const r)
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

  inline native_real operator+(native_real const l, obj::native_bigint const &r)
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

  inline native_real operator-(obj::native_bigint const &l, native_real const r)
  {
    // Consider potential exceptions
    try
    {
      return l.convert_to<native_real>() - r;
    }
    catch(...)
    { /* Handle exception? */
      return -r; // Approximation if l is huge
    }
  }

  inline native_real operator-(native_real const l, obj::native_bigint const &r)
  {
    // Consider potential exceptions
    try
    {
      return l - r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      return l; // Approximation if r is huge
    }
  }

  inline native_real operator*(obj::native_bigint const &l, native_real const r)
  {
    // Consider potential exceptions
    try
    {
      return l.convert_to<native_real>() * r;
    }
    catch(...)
    { /* Handle exception? */
      return 0.0; // Approximation
    }
  }

  inline native_real operator*(native_real const l, obj::native_bigint const &r)
  {
    // Consider potential exceptions
    try
    {
      return l * r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      return 0.0; // Approximation
    }
  }

  inline native_real operator/(obj::native_bigint const &l, native_real const r)
  {
    if(r == 0.0)
    {
      throw std::runtime_error("Division by zero");
    }
    // Consider potential exceptions
    try
    {
      return l.convert_to<native_real>() / r;
    }
    catch(...)
    { /* Handle exception? */
      // Approximation: return infinity based on sign of l
      return l.sign() > 0 ? std::numeric_limits<native_real>::infinity()
                          : -std::numeric_limits<native_real>::infinity();
    }
  }

  inline native_real operator/(native_real const l, obj::native_bigint const &r)
  {
    if(r == 0) // Exact check is fine for bigint
    {
      throw std::runtime_error("Division by zero");
    }
    // Consider potential exceptions
    try
    {
      return l / r.convert_to<native_real>();
    }
    catch(...)
    { /* Handle exception? */
      // Approximation: return 0 if r is huge
      return 0.0;
    }
  }
  inline native_bool operator==(native_real const l, obj::native_bigint const &r)
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

  inline native_bool operator==(obj::native_bigint const &l, native_real const r)
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
  /* TODO: visit_number_like */
  object_ptr add(object_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const r) -> object_ptr {
        return visit_number_like(
          [](auto const typed_r, auto const typed_l) -> object_ptr {
            return make_box(typed_l + typed_r->data);
          },
          r,
          typed_l->data);
      },
      l,
      r);
  }

  object_ptr add(obj::integer_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> object_ptr {
        return make_box(typed_l + typed_r->data);
      },
      r,
      l->data);
  }

  object_ptr add(object_ptr const l, obj::integer_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> object_ptr {
        return make_box(typed_l->data + typed_r);
      },
      l,
      r->data);
  }

  native_integer add(obj::integer_ptr const l, obj::integer_ptr const r)
  {
    return l->data + r->data;
  }

  native_real add(obj::real_ptr const l, obj::real_ptr const r)
  {
    return l->data + r->data;
  }

  native_real add(obj::real_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real { return typed_l + typed_r->data; },
      r,
      l->data);
  }

  native_real add(object_ptr const l, obj::real_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real { return typed_l->data + typed_r; },
      l,
      r->data);
  }

  native_real add(obj::real_ptr const l, obj::integer_ptr const r)
  {
    return l->data + static_cast<native_real>(r->data);
  }

  native_real add(obj::integer_ptr const l, obj::real_ptr const r)
  {
    return static_cast<native_real>(l->data) + r->data;
  }

  native_real add(object_ptr const l, native_real const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real { return typed_l->data + typed_r; },
      l,
      r);
  }

  native_real add(native_real const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real { return typed_l + typed_r->data; },
      r,
      l);
  }

  native_real add(native_real const l, native_real const r)
  {
    return l + r;
  }

  native_real add(native_integer const l, native_real const r)
  {
    return static_cast<native_real>(l) + r;
  }

  native_real add(native_real const l, native_integer const r)
  {
    return l + static_cast<native_real>(r);
  }

  object_ptr add(object_ptr const l, native_integer const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> object_ptr {
        return make_box(typed_l->data + typed_r);
      },
      l,
      r);
  }

  object_ptr add(native_integer const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> object_ptr {
        return make_box(typed_l + typed_r->data);
      },
      r,
      l);
  }

  native_integer add(native_integer const l, native_integer const r)
  {
    return l + r;
  }

  obj::native_bigint add(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    return l + r;
  }

  obj::native_bigint add(obj::native_bigint const &l, native_integer const r)
  {
    return l + r;
  }

  obj::native_bigint add(native_integer const l, obj::native_bigint const &r)
  {
    return l + r;
  }

  native_real add(obj::native_bigint const &l, native_real const r)
  {
    // Convert bigint to real for addition
    return l.convert_to<native_real>() + r;
  }

  native_real add(native_real const l, obj::native_bigint const &r)
  {
    // Convert bigint to real for addition
    return l + r.convert_to<native_real>();
  }

  object_ptr add(obj::ratio_data const &l, obj::native_bigint const &r)
  {
    // Convert bigint to integer (potentially lossy) to use existing ratio + integer op
    // TODO: Implement true ratio + bigint arithmetic if ratio supports bigint
    return l + r.convert_to<native_integer>();
  }

  object_ptr add(obj::native_bigint const &l, obj::ratio_data const &r)
  {
    // Convert bigint to integer (potentially lossy) to use existing integer + ratio op
    // TODO: Implement true bigint + ratio arithmetic if ratio supports bigint
    return l.convert_to<native_integer>() + r;
  }

  obj::bigint_ptr add(obj::bigint_ptr const l, obj::bigint_ptr const r)
  {
    return make_box(add(l->data, r->data));
  }

  obj::bigint_ptr add(obj::bigint_ptr const l, obj::integer_ptr const r)
  {
    return make_box(add(l->data, r->data));
  }

  obj::bigint_ptr add(obj::integer_ptr const l, obj::bigint_ptr const r)
  {
    return make_box(add(l->data, r->data));
  }

  native_real add(obj::bigint_ptr const l, obj::real_ptr const r)
  {
    return add(l->data, r->data);
  }

  native_real add(obj::real_ptr const l, obj::bigint_ptr const r)
  {
    return add(l->data, r->data);
  }

  object_ptr add(obj::bigint_ptr const l, obj::ratio_ptr const r)
  {
    return add(l->data, r->data);
  }

  object_ptr add(obj::ratio_ptr const l, obj::bigint_ptr const r)
  {
    return add(l->data, r->data);
  }

  object_ptr add(obj::bigint_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [l_data = l->data](auto const typed_r) -> object_ptr {
        return make_box(add(l_data, typed_r->data));
      },
      r);
  }

  object_ptr add(object_ptr const l, obj::bigint_ptr const r)
  {
    return visit_number_like(
      [r_data = r->data](auto const typed_l) -> object_ptr {
        return make_box(add(typed_l->data, r_data));
      },
      l);
  }

  object_ptr add(obj::native_bigint const &l, object_ptr const r)
  {
    return visit_number_like(
      [l](auto const typed_r) -> object_ptr { return make_box(add(l, typed_r->data)); },
      r);
  }

  object_ptr add(object_ptr const l, obj::native_bigint const &r)
  {
    return visit_number_like(
      [r](auto const typed_l) -> object_ptr { return make_box(add(typed_l->data, r)); },
      l);
  }

  object_ptr sub(object_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const r) -> object_ptr {
        return visit_number_like(
          [](auto const typed_r, auto const typed_l) -> object_ptr {
            return make_box(typed_l - typed_r->data);
          },
          r,
          typed_l->data);
      },
      l,
      r);
  }

  object_ptr sub(obj::integer_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> object_ptr {
        return make_box(typed_l - typed_r->data);
      },
      r,
      l->data);
  }

  object_ptr sub(object_ptr const l, obj::integer_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> object_ptr {
        return make_box(typed_l->data - typed_r);
      },
      l,
      r->data);
  }

  native_integer sub(obj::integer_ptr const l, obj::integer_ptr const r)
  {
    return l->data - r->data;
  }

  native_real sub(obj::real_ptr const l, obj::real_ptr const r)
  {
    return l->data - r->data;
  }

  native_real sub(obj::real_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real { return typed_l - typed_r->data; },
      r,
      l->data);
  }

  native_real sub(object_ptr const l, obj::real_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real { return typed_l->data - typed_r; },
      l,
      r->data);
  }

  native_real sub(obj::real_ptr const l, obj::integer_ptr const r)
  {
    return l->data - static_cast<native_real>(r->data);
  }

  native_real sub(obj::integer_ptr const l, obj::real_ptr const r)
  {
    return static_cast<native_real>(l->data) - r->data;
  }

  native_real sub(object_ptr const l, native_real const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real { return typed_l->data - typed_r; },
      l,
      r);
  }

  native_real sub(native_real const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real { return typed_l - typed_r->data; },
      r,
      l);
  }

  native_real sub(native_real const l, native_real const r)
  {
    return l - r;
  }

  native_real sub(native_integer const l, native_real const r)
  {
    return static_cast<native_real>(l) - r;
  }

  native_real sub(native_real const l, native_integer const r)
  {
    return l - static_cast<native_real>(r);
  }

  object_ptr sub(object_ptr const l, native_integer const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> object_ptr {
        return make_box(typed_l->data - typed_r);
      },
      l,
      r);
  }

  object_ptr sub(native_integer const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> object_ptr {
        return make_box(typed_l - typed_r->data);
      },
      r,
      l);
  }

  native_integer sub(native_integer const l, native_integer const r)
  {
    return l - r;
  }

  // --- bigint sub ---
  obj::native_bigint sub(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    return l - r;
  }

  obj::native_bigint sub(obj::native_bigint const &l, native_integer const r)
  {
    return l - r;
  }

  obj::native_bigint sub(native_integer const l, obj::native_bigint const &r)
  {
    return l - r;
  }

  native_real sub(obj::native_bigint const &l, native_real const r)
  {
    return l - r;
  }

  native_real sub(native_real const l, obj::native_bigint const &r)
  {
    return l - r;
  }

  object_ptr sub(obj::ratio_data const &l, obj::native_bigint const &r)
  {
    // Convert bigint to integer (potentially lossy)
    // TODO: Implement true ratio - bigint arithmetic
    return l - r.convert_to<native_integer>();
  }

  object_ptr sub(obj::native_bigint const &l, obj::ratio_data const &r)
  {
    // Convert bigint to integer (potentially lossy)
    // TODO: Implement true bigint - ratio arithmetic
    return l.convert_to<native_integer>() - r;
  }

  obj::bigint_ptr sub(obj::bigint_ptr const l, obj::bigint_ptr const r)
  {
    return make_box(sub(l->data, r->data));
  }

  obj::bigint_ptr sub(obj::bigint_ptr const l, obj::integer_ptr const r)
  {
    return make_box(sub(l->data, r->data));
  }

  obj::bigint_ptr sub(obj::integer_ptr const l, obj::bigint_ptr const r)
  {
    return make_box(sub(l->data, r->data));
  }

  native_real sub(obj::bigint_ptr const l, obj::real_ptr const r)
  {
    return sub(l->data, r->data);
  }

  native_real sub(obj::real_ptr const l, obj::bigint_ptr const r)
  {
    return sub(l->data, r->data);
  }

  object_ptr sub(obj::bigint_ptr const l, obj::ratio_ptr const r)
  {
    return sub(l->data, r->data);
  }

  object_ptr sub(obj::ratio_ptr const l, obj::bigint_ptr const r)
  {
    return sub(l->data, r->data);
  }

  object_ptr sub(obj::bigint_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [l_data = l->data](auto const typed_r) -> object_ptr {
        return make_box(sub(l_data, typed_r->data));
      },
      r);
  }

  object_ptr sub(object_ptr const l, obj::bigint_ptr const r)
  {
    return visit_number_like(
      [r_data = r->data](auto const typed_l) -> object_ptr {
        return make_box(sub(typed_l->data, r_data));
      },
      l);
  }

  object_ptr sub(obj::native_bigint const &l, object_ptr const r)
  {
    return visit_number_like(
      [l](auto const typed_r) -> object_ptr { return make_box(sub(l, typed_r->data)); },
      r);
  }

  object_ptr sub(object_ptr const l, obj::native_bigint const &r)
  {
    return visit_number_like(
      [r](auto const typed_l) -> object_ptr { return make_box(sub(typed_l->data, r)); },
      l);
  }

  object_ptr mul(object_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const r) -> object_ptr {
        return visit_number_like(
          [](auto const typed_r, auto const typed_l) -> object_ptr {
            return make_box(typed_l * typed_r->data);
          },
          r,
          typed_l->data);
      },
      l,
      r);
  }

  object_ptr mul(obj::integer_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> object_ptr {
        return make_box(typed_l * typed_r->data);
      },
      r,
      l->data);
  }

  object_ptr mul(object_ptr const l, obj::integer_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> object_ptr {
        return make_box(typed_l->data * typed_r);
      },
      l,
      r->data);
  }

  native_integer mul(obj::integer_ptr const l, obj::integer_ptr const r)
  {
    return l->data * r->data;
  }

  native_real mul(obj::real_ptr const l, obj::real_ptr const r)
  {
    return l->data * r->data;
  }

  native_real mul(obj::real_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real { return typed_l * typed_r->data; },
      r,
      l->data);
  }

  native_real mul(object_ptr const l, obj::real_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real { return typed_l->data * typed_r; },
      l,
      r->data);
  }

  native_real mul(obj::real_ptr const l, obj::integer_ptr const r)
  {
    return l->data * static_cast<native_real>(r->data);
  }

  native_real mul(obj::integer_ptr const l, obj::real_ptr const r)
  {
    return static_cast<native_real>(l->data) * r->data;
  }

  native_real mul(object_ptr const l, native_real const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real { return typed_l->data * typed_r; },
      l,
      r);
  }

  native_real mul(native_real const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real { return typed_l * typed_r->data; },
      r,
      l);
  }

  native_real mul(native_real const l, native_real const r)
  {
    return l * r;
  }

  native_real mul(native_integer const l, native_real const r)
  {
    return static_cast<native_real>(l) * r;
  }

  native_real mul(native_real const l, native_integer const r)
  {
    return l * static_cast<native_real>(r);
  }

  object_ptr mul(object_ptr const l, native_integer const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> object_ptr {
        return make_box(typed_l->data * typed_r);
      },
      l,
      r);
  }

  object_ptr mul(native_integer const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> object_ptr {
        return make_box(typed_l * typed_r->data);
      },
      r,
      l);
  }

  native_integer mul(native_integer const l, native_integer const r)
  {
    return l * r;
  }

  // --- bigint mul ---
  obj::native_bigint mul(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    return l * r;
  }

  obj::native_bigint mul(obj::native_bigint const &l, native_integer const r)
  {
    return l * r;
  }

  obj::native_bigint mul(native_integer const l, obj::native_bigint const &r)
  {
    return l * r;
  }

  native_real mul(obj::native_bigint const &l, native_real const r)
  {
    return l * r;
  }

  native_real mul(native_real const l, obj::native_bigint const &r)
  {
    return l * r;
  }

  object_ptr mul(obj::ratio_data const &l, obj::native_bigint const &r)
  {
    // Convert bigint to integer (potentially lossy)
    // TODO: Implement true ratio * bigint arithmetic
    return l * r.convert_to<native_integer>();
  }

  object_ptr mul(obj::native_bigint const &l, obj::ratio_data const &r)
  {
    // Convert bigint to integer (potentially lossy)
    // TODO: Implement true bigint * ratio arithmetic
    return l.convert_to<native_integer>() * r;
  }

  obj::bigint_ptr mul(obj::bigint_ptr const l, obj::bigint_ptr const r)
  {
    return make_box(mul(l->data, r->data));
  }

  obj::bigint_ptr mul(obj::bigint_ptr const l, obj::integer_ptr const r)
  {
    return make_box(mul(l->data, r->data));
  }

  obj::bigint_ptr mul(obj::integer_ptr const l, obj::bigint_ptr const r)
  {
    return make_box(mul(l->data, r->data));
  }

  native_real mul(obj::bigint_ptr const l, obj::real_ptr const r)
  {
    return mul(l->data, r->data);
  }

  native_real mul(obj::real_ptr const l, obj::bigint_ptr const r)
  {
    return mul(l->data, r->data);
  }

  object_ptr mul(obj::bigint_ptr const l, obj::ratio_ptr const r)
  {
    return mul(l->data, r->data);
  }

  object_ptr mul(obj::ratio_ptr const l, obj::bigint_ptr const r)
  {
    return mul(l->data, r->data);
  }

  object_ptr mul(obj::bigint_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [l_data = l->data](auto const typed_r) -> object_ptr {
        return make_box(mul(l_data, typed_r->data));
      },
      r);
  }

  object_ptr mul(object_ptr const l, obj::bigint_ptr const r)
  {
    return visit_number_like(
      [r_data = r->data](auto const typed_l) -> object_ptr {
        return make_box(mul(typed_l->data, r_data));
      },
      l);
  }

  object_ptr mul(obj::native_bigint const &l, object_ptr const r)
  {
    return visit_number_like(
      [l](auto const typed_r) -> object_ptr { return make_box(mul(l, typed_r->data)); },
      r);
  }

  object_ptr mul(object_ptr const l, obj::native_bigint const &r)
  {
    return visit_number_like(
      [r](auto const typed_l) -> object_ptr { return make_box(mul(typed_l->data, r)); },
      l);
  }

  object_ptr div(object_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const r) -> object_ptr {
        return visit_number_like(
          [](auto const typed_r, auto const typed_l) -> object_ptr {
            using LType = typename std::decay_t<decltype(typed_l)>::value_type;
            using RType = typename std::decay_t<decltype(typed_r)>::value_type;

            if constexpr(std::is_same_v<LType, obj::bigint> || std::is_same_v<RType, obj::bigint>)
            {
              // Use bigint's % operator if either is bigint. Promote non-bigint int/ratio if needed.
              // Result should be bigint. Need conversion logic.
              // For simplicity, convert both to bigint first (might be lossy for ratio/real).
              obj::native_bigint l_big = typed_l->to_integer(); // Simplification
              obj::native_bigint r_big = typed_r->to_integer(); // Simplification
              if(r_big == 0)
              {
                throw erase(make_box("Division by zero"));
              }
              return make_box(l_big % r_big);
            }
            else if constexpr(std::is_same_v<LType, obj::real> || std::is_same_v<RType, obj::real>
                              || std::is_same_v<LType, obj::ratio>
                              || std::is_same_v<RType, obj::ratio>)
            {
              // Use fmod if real/ratio involved (original behavior)
              auto const typed_l_data{ to_number(typed_l->data) }; // Converts ratio to real
              auto const typed_r_data{ to_number(typed_r->data) }; // Converts ratio to real
              return make_box(std::fmod(typed_l_data, typed_r_data));
            }
            else // Both must be integer
            {
              if(typed_r->data == 0)
              {
                throw erase(make_box("Division by zero"));
              }
              return make_box(typed_l->data / typed_r->data);
            }
          },
          r,
          typed_l); // Pass typed_l itself, not just data, for access to methods like to_integer
      },
      l,
      r);
  }

  object_ptr quot(object_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const r) -> object_ptr {
        return visit_number_like(
          [](auto const typed_r, auto const typed_l) -> object_ptr {
            using LType = typename std::decay_t<decltype(typed_l)>::value_type;
            using RType = typename std::decay_t<decltype(typed_r)>::value_type;

            if constexpr(std::is_same_v<LType, obj::bigint> || std::is_same_v<RType, obj::bigint>)
            {
              obj::native_bigint l_big = typed_l->to_integer(); // Simplification
              obj::native_bigint r_big = typed_r->to_integer(); // Simplification
              if(r_big == 0)
              {
                throw erase(make_box("Quotient division by zero"));
              }
              // Integer division for quotient
              return make_box(l_big / r_big);
            }
            else if constexpr(std::is_same_v<LType, obj::real> || std::is_same_v<RType, obj::real>
                              || std::is_same_v<LType, obj::ratio>
                              || std::is_same_v<RType, obj::ratio>)
            {
              // Use floating point division and truncate (original behavior)
              auto const typed_l_data{ to_number(typed_l->data) };
              auto const typed_r_data{ to_number(typed_r->data) };
              if(typed_r_data == 0.0)
              {
                throw erase(make_box("Quotient division by zero"));
              }
              return make_box(static_cast<native_integer>(typed_l_data / typed_r_data));
            }
            else // Both must be integer
            {
              if(typed_r->data == 0)
              {
                throw erase(make_box("Quotient division by zero"));
              }
              return make_box(typed_l->data / typed_r->data);
            }
          },
          r,
          typed_l);
      },
      l,
      r);
  }

  object_ptr inc(object_ptr const l)
  {
    return visit_number_like(
      [](auto const typed_l) -> object_ptr {
        using T = typename std::decay_t<decltype(typed_l)>::value_type;
        if constexpr(std::is_same_v<T, obj::bigint>)
        {
          return make_box(typed_l->data + 1ll);
        }
        else if constexpr(std::is_same_v<T, obj::ratio>)
        {
          // Ratio + 1ll
          return typed_l->data + 1ll;
        }
        else
        { // integer, real
          return make_box(typed_l->data + 1ll);
        }
      },
      l);
  }

  object_ptr dec(object_ptr const l)
  {
    return visit_number_like(
      [](auto const typed_l) -> object_ptr {
        using T = typename std::decay_t<decltype(typed_l)>::value_type;
        if constexpr(std::is_same_v<T, obj::bigint>)
        {
          return make_box(typed_l->data - 1ll);
        }
        else if constexpr(std::is_same_v<T, obj::ratio>)
        {
          // Ratio - 1ll
          return typed_l->data - 1ll;
        }
        else
        { // integer, real
          return make_box(typed_l->data - 1ll);
        }
      },
      l);
  }

  native_bool is_zero(object_ptr const l)
  {
    return visit_number_like(
      [](auto const typed_l) -> native_bool {
        // Use exact comparison for integer types
        using T = typename std::decay_t<decltype(typed_l)>::value_type;
        if constexpr(std::is_same_v<T, obj::integer> || std::is_same_v<T, obj::bigint>)
        {
          return typed_l->data == 0;
        }
        else
        { // real, ratio
          // Use tolerance for floating point / potentially inexact ratio
          // Or convert ratio exactly? Let's convert ratio to real for now.
          return std::fabs(typed_l->to_real()) < 1e-9; // Adjust tolerance as needed
        }
      },
      l);
  }

  native_bool is_pos(object_ptr const l)
  {
    return visit_number_like(
      [](auto const typed_l) -> native_bool {
        using T = typename std::decay_t<decltype(typed_l)>::value_type;
        if constexpr(std::is_same_v<T, obj::integer> || std::is_same_v<T, obj::bigint>)
        {
          return typed_l->data > 0;
        }
        else
        { // real, ratio
          return typed_l->to_real() > 0.0;
        }
      },
      l);
  }

  native_bool is_neg(object_ptr const l)
  {
    return visit_number_like(
      [](auto const typed_l) -> native_bool {
        using T = typename std::decay_t<decltype(typed_l)>::value_type;
        if constexpr(std::is_same_v<T, obj::integer> || std::is_same_v<T, obj::bigint>)
        {
          return typed_l->data < 0;
        }
        else
        { // real, ratio
          return typed_l->to_real() < 0.0;
        }
      },
      l);
  }

  native_bool is_even(object_ptr const l)
  {
    if(l->type == object_type::integer)
    {
      return expect_object<obj::integer>(l)->data % 2 == 0;
    }
    else if(l->type == object_type::bigint)
    {
      return expect_object<obj::bigint>(l)->data % 2 == 0;
    }
    else
    {
      throw erase(make_box("is_even requires an integer or bigint"));
    }
  }

  native_bool is_odd(object_ptr const l)
  {
    if(l->type == object_type::integer)
    {
      return expect_object<obj::integer>(l)->data % 2 != 0; // Use != 0 for robustness
    }
    else if(l->type == object_type::bigint)
    {
      return expect_object<obj::bigint>(l)->data % 2 != 0; // Use != 0
    }
    else
    {
      throw erase(make_box("is_odd requires an integer or bigint"));
    }
  }

  native_bool is_equiv(object_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const r) -> native_bool {
        return visit_number_like(
          [](auto const typed_r, auto const typed_l) -> native_bool {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"
            return typed_l->data == typed_r->data;
#pragma clang diagnostic pop
          },
          r,
          typed_l);
      },
      l,
      r);
  }

  native_integer bit_not(object_ptr const l)
  {
    if(l->type == object_type::integer)
    {
      return ~expect_object<obj::integer>(l)->data;
    }
    else if(l->type == object_type::bigint)
    {
      return ~expect_object<obj::bigint>(l)->data.convert_to<native_integer>();
    }
    else
    {
      throw erase(make_box("bit_not requires an integer or bigint"));
    }
  }

  object_ptr bit_and(object_ptr const l, object_ptr const r)
  {
    if((l->type == object_type::integer || l->type == object_type::bigint)
       && (r->type == object_type::integer || r->type == object_type::bigint))
    {
      obj::native_bigint l_big = (l->type == object_type::bigint)
        ? expect_object<obj::bigint>(l)->data
        : obj::native_bigint(expect_object<obj::integer>(l)->data);
      obj::native_bigint r_big = (r->type == object_type::bigint)
        ? expect_object<obj::bigint>(r)->data
        : obj::native_bigint(expect_object<obj::integer>(r)->data);
      return make_box(l_big & r_big);
    }
    else
    {
      throw erase(make_box("bit_and requires integers or bigints"));
    }
  }

  object_ptr bit_or(object_ptr const l, object_ptr const r)
  {
    if((l->type == object_type::integer || l->type == object_type::bigint)
       && (r->type == object_type::integer || r->type == object_type::bigint))
    {
      obj::native_bigint l_big = (l->type == object_type::bigint)
        ? expect_object<obj::bigint>(l)->data
        : obj::native_bigint(expect_object<obj::integer>(l)->data);
      obj::native_bigint r_big = (r->type == object_type::bigint)
        ? expect_object<obj::bigint>(r)->data
        : obj::native_bigint(expect_object<obj::integer>(r)->data);
      return make_box(l_big | r_big);
    }
    else
    {
      throw erase(make_box("bit_or requires integers or bigints"));
    }
  }

  object_ptr bit_xor(object_ptr const l, object_ptr const r)
  {
    if((l->type == object_type::integer || l->type == object_type::bigint)
       && (r->type == object_type::integer || r->type == object_type::bigint))
    {
      obj::native_bigint l_big = (l->type == object_type::bigint)
        ? expect_object<obj::bigint>(l)->data
        : obj::native_bigint(expect_object<obj::integer>(l)->data);
      obj::native_bigint r_big = (r->type == object_type::bigint)
        ? expect_object<obj::bigint>(r)->data
        : obj::native_bigint(expect_object<obj::integer>(r)->data);
      return make_box(l_big ^ r_big);
    }
    else
    {
      throw erase(make_box("bit_xor requires integers or bigints"));
    }
  }

  object_ptr bit_and_not(object_ptr const l, object_ptr const r)
  {
    if((l->type == object_type::integer || l->type == object_type::bigint)
       && (r->type == object_type::integer || r->type == object_type::bigint))
    {
      obj::native_bigint l_big = (l->type == object_type::bigint)
        ? expect_object<obj::bigint>(l)->data
        : obj::native_bigint(expect_object<obj::integer>(l)->data);
      obj::native_bigint r_big = (r->type == object_type::bigint)
        ? expect_object<obj::bigint>(r)->data
        : obj::native_bigint(expect_object<obj::integer>(r)->data);
      return make_box(l_big & (~r_big));
    }
    else
    {
      throw erase(make_box("bit_and_not requires integers or bigints"));
    }
  }

  object_ptr bit_shift_left(object_ptr const l, object_ptr const r)
  {
    if((l->type == object_type::integer || l->type == object_type::bigint)
       && (r->type == object_type::integer)) // Shift amount must be standard integer
    {
      obj::native_bigint l_big = (l->type == object_type::bigint)
        ? expect_object<obj::bigint>(l)->data
        : obj::native_bigint(expect_object<obj::integer>(l)->data);
      native_integer shift = expect_object<obj::integer>(r)->data;
      if(shift < 0)
      {
        throw erase(make_box("Shift amount cannot be negative"));
      }
      // Note: Boost cpp_int handles large shifts correctly
      return make_box(l_big << shift);
    }
    else
    {
      throw erase(
        make_box("bit_shift_left requires an integer/bigint and an integer shift amount"));
    }
  }

  object_ptr bit_shift_right(object_ptr const l, object_ptr const r)
  {
    if((l->type == object_type::integer || l->type == object_type::bigint)
       && (r->type == object_type::integer)) // Shift amount must be standard integer
    {
      obj::native_bigint l_big = (l->type == object_type::bigint)
        ? expect_object<obj::bigint>(l)->data
        : obj::native_bigint(expect_object<obj::integer>(l)->data);
      native_integer shift = expect_object<obj::integer>(r)->data;
      if(shift < 0)
      {
        throw erase(make_box("Shift amount cannot be negative"));
      }
      return make_box(l_big >> shift); // Arithmetic shift
    }
    else
    {
      throw erase(
        make_box("bit_shift_right requires an integer/bigint and an integer shift amount"));
    }
  }

  object_ptr bit_unsigned_shift_right(object_ptr const l, object_ptr const r)
  {
    if((l->type == object_type::integer || l->type == object_type::bigint)
       && (r->type == object_type::integer)) // Shift amount must be standard integer
    {
      obj::native_bigint l_big = (l->type == object_type::bigint)
        ? expect_object<obj::bigint>(l)->data
        : obj::native_bigint(expect_object<obj::integer>(l)->data);
      native_integer shift = expect_object<obj::integer>(r)->data;
      if(shift < 0)
      {
        throw erase(make_box("Shift amount cannot be negative"));
      }
      // If l_big is negative, standard >> might fill with 1s.
      // Clojure's unsigned shift behaves differently.
      // For now, using standard >>.
      // TODO: Implement proper logical shift if needed, potentially via bit manipulation or conversion to unsigned multiprecision type.
      return make_box(l_big >> shift);
    }
    else
    {
      throw erase(make_box(
        "bit_unsigned_shift_right requires an integer/bigint and an integer shift amount"));
    }
  }

  native_bool lt(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    return l < r;
  }

  native_bool lt(obj::native_bigint const &l, native_integer const r)
  {
    return l < r;
  }

  native_bool lt(native_integer const l, obj::native_bigint const &r)
  {
    return l < r;
  }

  native_bool lt(obj::native_bigint const &l, native_real const r)
  {
    try
    {
      return l < r;
    }
    catch(std::runtime_error const &)
    {
      return l < 0;
    }
  }

  native_bool lt(native_real const l, obj::native_bigint const &r)
  {
    try
    {
      return l < r;
    }
    catch(std::runtime_error const &)
    {
      return l > 0;
    }
  }

  native_bool lt(native_integer const l, native_integer const r)
  {
    return l < r;
  }

  // --- bigint lt ---
  native_bool lt(obj::bigint_ptr const l, obj::bigint_ptr const r)
  {
    return lt(l->data, r->data);
  }

  native_bool lt(obj::bigint_ptr const l, obj::integer_ptr const r)
  {
    return lt(l->data, r->data);
  }

  native_bool lt(obj::integer_ptr const l, obj::bigint_ptr const r)
  {
    return lt(l->data, r->data);
  }

  native_bool lt(obj::bigint_ptr const l, obj::real_ptr const r)
  {
    return lt(l->data, r->data);
  }

  native_bool lt(obj::real_ptr const l, obj::bigint_ptr const r)
  {
    return lt(l->data, r->data);
  }

  native_bool lt(obj::bigint_ptr const l, obj::ratio_ptr const r)
  {
    // Use ratio.cpp implementation (converts bigint to int64)
    return l->data < r->data;
  }

  native_bool lt(obj::ratio_ptr const l, obj::bigint_ptr const r)
  {
    // Use ratio.cpp implementation (converts bigint to int64)
    return l->data < r->data;
  }

  native_bool lt(obj::bigint_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [l_data = l->data](auto const typed_r) -> native_bool { return lt(l_data, typed_r->data); },
      r);
  }

  native_bool lt(object_ptr const l, obj::bigint_ptr const r)
  {
    return visit_number_like(
      [r_data = r->data](auto const typed_l) -> native_bool { return lt(typed_l->data, r_data); },
      l);
  }

  native_bool lt(obj::native_bigint const &l, object_ptr const r)
  {
    return visit_number_like(
      [l](auto const typed_r) -> native_bool { return lt(l, typed_r->data); },
      r);
  }

  native_bool lt(object_ptr const l, obj::native_bigint const &r)
  {
    return visit_number_like(
      [r](auto const typed_l) -> native_bool { return lt(typed_l->data, r); },
      l);
  }

  native_bool lte(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    return l <= r;
  }

  native_bool lte(obj::native_bigint const &l, native_integer const r)
  {
    return l <= r;
  }

  native_bool lte(native_integer const l, obj::native_bigint const &r)
  {
    return l <= r;
  }

  native_bool lte(obj::native_bigint const &l, native_real const r)
  {
    try
    {
      return l <= r;
    }
    catch(std::runtime_error const &)
    {
      return l < 0;
    }
  }

  native_bool lte(native_real const l, obj::native_bigint const &r)
  {
    try
    {
      return l <= r;
    }
    catch(std::runtime_error const &)
    {
      return l > 0;
    }
  }

  native_bool lte(native_integer const l, native_integer const r)
  {
    return l <= r;
  }

  // --- bigint lte ---
  native_bool lte(obj::bigint_ptr const l, obj::bigint_ptr const r)
  {
    return lte(l->data, r->data);
  }

  native_bool lte(obj::bigint_ptr const l, obj::integer_ptr const r)
  {
    return lte(l->data, r->data);
  }

  native_bool lte(obj::integer_ptr const l, obj::bigint_ptr const r)
  {
    return lte(l->data, r->data);
  }

  native_bool lte(obj::bigint_ptr const l, obj::real_ptr const r)
  {
    return lte(l->data, r->data);
  }

  native_bool lte(obj::real_ptr const l, obj::bigint_ptr const r)
  {
    return lte(l->data, r->data);
  }

  native_bool lte(obj::bigint_ptr const l, obj::ratio_ptr const r)
  {
    return l->data <= r->data;
  } // Uses ratio.cpp impl

  native_bool lte(obj::ratio_ptr const l, obj::bigint_ptr const r)
  {
    return l->data <= r->data;
  } // Uses ratio.cpp impl

  native_bool lte(obj::bigint_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [l_data = l->data](auto const typed_r) -> native_bool { return lte(l_data, typed_r->data); },
      r);
  }

  native_bool lte(object_ptr const l, obj::bigint_ptr const r)
  {
    return visit_number_like(
      [r_data = r->data](auto const typed_l) -> native_bool { return lte(typed_l->data, r_data); },
      l);
  }

  native_bool lte(obj::native_bigint const &l, object_ptr const r)
  {
    return visit_number_like(
      [l](auto const typed_r) -> native_bool { return lte(l, typed_r->data); },
      r);
  }

  native_bool lte(object_ptr const l, obj::native_bigint const &r)
  {
    return visit_number_like(
      [r](auto const typed_l) -> native_bool { return lte(typed_l->data, r); },
      l);
  }

  object_ptr min(object_ptr const l, object_ptr const r)
  {
    return lt(l, r) ? l : r;
  }

  obj::native_bigint min(obj::native_bigint const &l, native_integer const r)
  {
    return std::min(l, obj::native_bigint(r));
  }

  obj::native_bigint min(native_integer const l, obj::native_bigint const &r)
  {
    return std::min(obj::native_bigint(l), r);
  }

  obj::native_bigint min(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    return std::min(l, r);
  }

  native_real min(obj::native_bigint const &l, native_real const r)
  {
    return std::min(l, r);
  }

  native_real min(native_real const l, obj::native_bigint const &r)
  {
    return std::min(l, r);
  }

  object_ptr min(object_ptr const l, object_ptr const r)
  {
    return lt(l, r) ? l : r;
  }

  obj::native_bigint max(obj::native_bigint const &l, native_integer const r)
  {
    return std::max(l, obj::native_bigint(r));
  }

  obj::native_bigint max(native_integer const l, obj::native_bigint const &r)
  {
    return std::max(obj::native_bigint(l), r);
  }

  obj::native_bigint max(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    return std::max(l, r);
  }

  native_real max(obj::native_bigint const &l, native_real const r)
  {
    return std::max(l, r);
  }

  native_real max(native_real const l, obj::native_bigint const &r)
  {
    return std::max(l, r);
  }

  object_ptr max(object_ptr const l, object_ptr const r)
  {
    return lt(l, r) ? r : l;
  }

  obj::native_bigint max(obj::native_bigint const &l, native_integer const r)
  {
    return std::max(l, obj::native_bigint(r));
  }

  obj::native_bigint max(native_integer const l, obj::native_bigint const &r)
  {
    return std::max(obj::native_bigint(l), r);
  }

  obj::native_bigint max(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    return std::max(l, r);
  }

  native_real max(obj::native_bigint const &l, native_real const r)
  {
    return std::max(l, r);
  }

  native_real max(native_real const l, obj::native_bigint const &r)
  {
    return std::max(l, r);
  }

  object_ptr max(object_ptr const l, object_ptr const r)
  {
    return lt(l, r) ? r : l;
  }

  object_ptr abs(object_ptr const l)
  {
    return visit_number_like(
      [](auto const typed_l) -> object_ptr {
        using T = typename std::decay_t<decltype(typed_l)>::value_type;
        if constexpr(std::is_same_v<T, obj::bigint>)
        {
          return make_box(boost::multiprecision::abs(typed_l->data));
        }
        else if constexpr(std::is_same_v<T, obj::integer>)
        {
          return make_box(std::abs(typed_l->data));
        }
        else
        { // real, ratio -> convert to real
          return make_box(std::fabs(typed_l->to_real()));
        }
      },
      l);
  }

  native_real sqrt(object_ptr const l)
  {
    return visit_number_like(
      [](auto const typed_l) -> native_real { return std::sqrt(typed_l->to_real()); },
      l);
  }

  native_real sqrt(obj::integer_ptr const l)
  {
    return std::sqrt(l->data);
  }

  native_real sqrt(obj::real_ptr const l)
  {
    return std::sqrt(l->data);
  }

  native_real sqrt(native_integer const l)
  {
    return std::sqrt(l);
  }

  native_real sqrt(native_real const l)
  {
    return std::sqrt(l);
  }

  native_real pow(object_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const r) -> native_real {
        return visit_number_like(
          [](auto const typed_r, auto const typed_l) -> native_real {
            return std::pow(typed_l->to_real(), typed_r->to_real());
          },
          r,
          typed_l);
      },
      l,
      r);
  }

  native_real pow(obj::integer_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real {
        return std::pow(typed_l->to_real(), static_cast<native_real>(typed_r->to_integer()));
      },
      r,
      typed_l);
  }

  native_real pow(object_ptr const l, obj::integer_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real {
        return std::pow(typed_l->to_real(), static_cast<native_real>(typed_r->data));
      },
      l,
      r);
  }

  native_real pow(obj::integer_ptr const l, obj::integer_ptr const r)
  {
    return std::pow(l->data, r->data);
  }

  native_real pow(obj::real_ptr const l, obj::real_ptr const r)
  {
    return std::pow(l->data, r->data);
  }

  native_real pow(obj::real_ptr const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real {
        return std::pow(typed_l->to_real(), typed_r->to_real());
      },
      r,
      l);
  }

  native_real pow(object_ptr const l, obj::real_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real {
        return std::pow(typed_l->to_real(), typed_r->to_real());
      },
      l,
      r);
  }

  native_real pow(obj::real_ptr const l, obj::integer_ptr const r)
  {
    return std::pow(l->data, static_cast<native_real>(r->data));
  }

  native_real pow(obj::integer_ptr const l, obj::real_ptr const r)
  {
    return std::pow(static_cast<native_real>(l->data), r->data);
  }

  native_real pow(object_ptr const l, native_real const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real {
        return std::pow(typed_l->to_real(), typed_r);
      },
      l,
      r);
  }

  native_real pow(native_real const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real {
        return std::pow(typed_l, typed_r->to_real());
      },
      r,
      l);
  }

  native_real pow(native_real const l, native_real const r)
  {
    return std::pow(l, r);
  }

  native_real pow(native_integer const l, native_real const r)
  {
    return std::pow(static_cast<native_real>(l), r);
  }

  native_real pow(native_real const l, native_integer const r)
  {
    return std::pow(l, static_cast<native_real>(r));
  }

  native_real pow(object_ptr const l, native_integer const r)
  {
    return visit_number_like(
      [](auto const typed_l, auto const typed_r) -> native_real {
        return std::pow(typed_l->to_real(), static_cast<native_real>(typed_r));
      },
      l,
      r);
  }

  native_real pow(native_integer const l, object_ptr const r)
  {
    return visit_number_like(
      [](auto const typed_r, auto const typed_l) -> native_real {
        return std::pow(static_cast<native_real>(l), typed_r->to_real());
      },
      r,
      l);
  }

  native_integer pow(obj::native_bigint const &l, obj::native_bigint const &r)
  {
    // pow for multiprecision integers can be complex.
    // Boost provides powm (modular exponentiation) and integer pow.
    // For a real result consistent with others, convert base to real.
    // Beware of large exponents causing overflow in real.
    return std::pow(l, r);
  }

  native_bigint pow(obj::native_bigint const &l, native_integer const r)
  {
    return std::pow(l, r);
  }

  native_integer to_int(object_ptr const l)
  {
    return visit_number_like(
      [](auto const typed_l) -> native_integer { return typed_l->to_integer(); },
      l);
  }

  native_integer to_int(obj::bigint_ptr const l)
  {
    return l->to_integer();
  }

  native_bool is_number(object_ptr const o)
  {
    return visit_number_like([=](auto const) -> native_bool { return true; },
                             [=]() -> native_bool { return false; },
                             o);
  }

  native_bool is_bigint(object_ptr const o)
  {
    return o->type == object_type::bigint;
  }
}
