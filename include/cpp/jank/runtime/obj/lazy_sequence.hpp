#pragma once

#include <jank/runtime/behavior/seqable.hpp>

namespace jank::runtime
{
  namespace obj
  {
    using cons = static_object<object_type::cons>;
    using cons_ptr = native_box<cons>;
  }

  template <>
  struct static_object<object_type::lazy_sequence> : gc
  {
    static constexpr native_bool pointer_free{ false };

    static_object() = default;
    static_object(static_object &&) = default;
    static_object(static_object const &) = default;
    static_object(object_ptr fn);
    static_object(object_ptr fn, object_ptr sequence);

    /* behavior::objectable */
    native_bool equal(object const &) const;
    native_persistent_string to_string() const;
    void to_string(fmt::memory_buffer &buff) const;
    native_hash to_hash() const;

    /* behavior::seqable */
    native_box<static_object> seq() const;
    native_box<static_object> fresh_seq() const;

    /* behavior::sequenceable */
    object_ptr first() const;
    native_box<static_object> next() const;
    native_box<static_object> next_in_place();
    object_ptr next_in_place_first();
    obj::cons_ptr cons(object_ptr head) const;

  private:
    object_ptr resolve_fn() const;
    object_ptr resolve_seq() const;

  public:
    /* TODO: Synchronize. */
    object base{ object_type::lazy_sequence };
    mutable object_ptr fn{};
    mutable object_ptr fn_result{};
    mutable object_ptr sequence{};
  };

  namespace obj
  {
    using lazy_sequence = static_object<object_type::lazy_sequence>;
    using lazy_sequence_ptr = native_box<lazy_sequence>;
  }
}
