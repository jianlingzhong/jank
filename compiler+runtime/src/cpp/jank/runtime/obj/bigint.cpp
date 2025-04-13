#include <jank/runtime/obj/bigint.hpp>
#include <jank/runtime/obj/nil.hpp>
#include <boost/container_hash/hash.hpp>
#include <boost/lexical_cast.hpp>
#include <jank/runtime/rtti.hpp>

namespace jank::runtime::obj
{

  bigint::bigint(native_bigint const &val)
    : data{ val }
  {
  }

  bigint::bigint(native_bigint &&val)
    : data{ std::move(val) }
  {
  }

  native_bool bigint::equal(object const &other) const
  {
    if(other.type != object_type::bigint)
    {
      return false;
    }

    return this->data == expect_object<bigint>(&other)->data;
  }

  native_persistent_string bigint::to_string() const
  {
    // Use boost::lexical_cast to convert the native_bigint to std::string
    // Assuming native_persistent_string can be constructed from std::string
    return boost::lexical_cast<std::string>(data);
  }

  void bigint::to_string(util::string_builder &buff) const
  {
    // Append the string representation of the native_bigint to the builder
    buff(boost::lexical_cast<std::string>(data));
  }

  native_persistent_string bigint::to_code_string() const
  {
    // For numbers, the code string representation is usually the same as to_string
    return boost::lexical_cast<std::string>(data);
  }

  native_hash bigint::to_hash() const
  {
    // Use boost::hash to compute the hash of the underlying native_bigint
    // Requires <boost/container_hash/hash.hpp>
    return boost::hash<native_bigint>{}(data);
  }

  // native_integer bigint::compare(object const& other) const { return -1; }
  // native_integer bigint::compare(bigint const& other) const { return -1; }
  native_integer bigint::to_integer() const
  {
    // If 'data' is outside the range of native_integer,
    // the result is determined by C++'s narrowing conversion rules
    // (typically wraps around, effectively truncating upper bits).
    return data.convert_to<native_integer>();
  }

  native_real bigint::to_real() const
  {
    // If 'data' cannot be represented exactly by native_real (double),
    // it will be converted to the nearest representable value,
    // potentially losing precision (truncating lower bits) or becoming infinity.
    return data.convert_to<native_real>();
  }

} // namespace jank::runtime::obj

// Instantiate make_box if forward declared (likely not needed with templates)
// namespace jank::runtime {
//  template obj::bigint_ptr make_box<obj::bigint>(obj::native_bigint const&);
//  template obj::bigint_ptr make_box<obj::bigint>(obj::native_bigint&&);
// }
