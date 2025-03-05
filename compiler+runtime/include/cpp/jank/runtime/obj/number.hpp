#pragma once

#include <jank/runtime/object.hpp>

namespace jank::runtime::obj
{
  using boolean_ptr = native_box<struct boolean>;

  struct boolean : gc
  {
    static constexpr object_type obj_type{ object_type::boolean };
    static constexpr native_bool pointer_free{ true };

    static boolean_ptr true_const();
    static boolean_ptr false_const();

    boolean() = default;
    boolean(boolean &&) noexcept = default;
    boolean(boolean const &) = default;
    boolean(native_bool const d);

    /* behavior::object_like */
    native_bool equal(object const &) const;
    native_persistent_string to_string() const;
    void to_string(util::string_builder &buff) const;
    native_persistent_string to_code_string() const;
    native_hash to_hash() const;

    /* behavior::comparable */
    native_integer compare(object const &) const;

    /* behavior::comparable extended */
    native_integer compare(boolean const &) const;

    object base{ obj_type };
    native_bool data{};
  };

  using integer_ptr = native_box<struct integer>;

  struct integer : gc
  {
    static constexpr object_type obj_type{ object_type::integer };
    static constexpr native_bool pointer_free{ true };

    integer() = default;
    integer(integer &&) noexcept = default;
    integer(integer const &) = default;
    integer(native_integer const d);

    /* behavior::object_like */
    native_bool equal(object const &) const;
    native_persistent_string to_string() const;
    void to_string(util::string_builder &buff) const;
    native_persistent_string to_code_string() const;
    native_hash to_hash() const;

    /* behavior::comparable */
    native_integer compare(object const &) const;

    /* behavior::comparable extended */
    native_integer compare(integer const &) const;

    /* behavior::number_like */
    native_integer to_integer() const;
    native_real to_real() const;

    native_integer data{};
    object base{ obj_type };
  };
  
  using big_integer_ptr = native_box<struct big_integer>;

  struct big_integer : gc
  {
    static constexpr object_type obj_type{ object_type::big_integer };
    static constexpr native_bool pointer_free{ false };

    big_integer() = default;
    big_integer(big_integer &&) noexcept = default;
    big_integer(big_integer const &) = default;
    big_integer(native_persistent_string const & d);
    big_integer(native_integer const d);

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

    native_persistent_string data{};
    object base{ obj_type };
  };

  struct real : gc
  {
    static constexpr object_type obj_type{ object_type::real };
    static constexpr native_bool pointer_free{ true };

    real() = default;
    real(real &&) noexcept = default;
    real(real const &) = default;
    real(native_real const d);

    /* behavior::object_like */
    native_bool equal(object const &) const;
    native_persistent_string to_string() const;
    void to_string(util::string_builder &buff) const;
    native_persistent_string to_code_string() const;
    native_hash to_hash() const;

    /* behavior::comparable */
    native_integer compare(object const &) const;

    /* behavior::comparable extended */
    native_integer compare(real const &) const;

    /* behavior::number_like */
    native_integer to_integer() const;
    native_real to_real() const;

    native_real data{};
    object base{ obj_type };
  };
}
