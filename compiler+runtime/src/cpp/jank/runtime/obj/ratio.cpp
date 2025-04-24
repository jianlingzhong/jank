#include <limits>
#include <cmath>
#include <boost/multiprecision/cpp_int.hpp>

#include <jank/runtime/obj/ratio.hpp>
#include <jank/runtime/visit.hpp>

namespace jank::runtime::obj
{
  static constexpr auto epsilon{ std::numeric_limits<native_real>::epsilon() };

  ratio_data::ratio_data(big_integer const &num, big_integer const &denom)
    : numerator{ make_box<big_integer>(num) }
    , denominator{ make_box<big_integer>(denom) }
  {
    if(denominator == 0ll)
    {
      throw std::invalid_argument{ "Ratio denominator cannot be zero." };
    }
    auto const gcd{ big_integer::gcd(numerator, denominator) };
    this->numerator = this->numerator / gcd;
    this->denominator = this->denominator / gcd;

    if(denominator < 0ll)
    {
      this->numerator = -1ll * this->numerator;
      this->denominator = -1ll * this->denominator;
    }
  }

  ratio_data::ratio_data(object_ptr numerator, object_ptr denominator)
  {
    ratio_data(expect_object<big_integer>(numerator), expect_object<big_integer>(denominator));
  }

  ratio_data::ratio_data(native_integer const numerator, native_integer const denominator)
  {
    ratio_data(native_big_integer(numerator), native_big_integer(denominator));
  }

  ratio::ratio(ratio_data const &data)
    : data{ data }
  {
  }

  object_ptr ratio::create(big_integer_ptr numerator, big_integer_ptr denominator)
  {
    ratio_data const data{ numerator, denominator };
    if(data.denominator->data == 1ll)
    {
      return make_box<big_integer>(data.numerator);
    }
    return make_box<ratio>(data);
  }

  object_ptr ratio::create(native_integer const numerator, native_integer const denominator)
  {
    return create(make_box<big_integer>(numerator), make_box<big_integer>(denominator));
  }

  native_real ratio_data::to_real() const
  {
    return numerator->to_real() / denominator->to_real();
  }

  native_integer ratio_data::to_integer() const
  {
    auto res = numerator / denominator;
    return res->to_integer();
  }

  native_real ratio::to_real() const
  {
    return data.to_real();
  }

  native_integer ratio::to_integer() const
  {
    return data.to_integer();
  }

  void ratio::to_string(util::string_builder &buff) const
  {
    buff(data.numerator->to_string())('/')(data.denominator->to_string());
  }

  native_persistent_string ratio::to_string() const
  {
    util::string_builder buff;
    to_string(buff);
    return buff.release();
  }

  native_persistent_string ratio::to_code_string() const
  {
    return to_string();
  }

  native_hash ratio::to_hash() const
  {
    return hash::combine(data.numerator->to_hash(), data.denominator->to_hash());
  }

  native_bool ratio::equal(object const &o) const
  {
    if(o.type == object_type::integer)
    {
      return data == expect_object<integer>(&o)->data;
    }

    if(o.type == object_type::real)
    {
      return data == expect_object<real>(&o)->data;
    }

    if(o.type == object_type::ratio)
    {
      return data == expect_object<ratio>(&o)->data;
    }

    return false;
  }

  native_integer ratio::compare(object const &o) const
  {
    return visit_number_like(
      [this](auto const typed_o) -> native_integer {
        return (data > typed_o->data) - (data < typed_o->data);
      },
      &o);
  }

  native_integer ratio::compare(ratio const &o) const
  {
    return (data > o.data) - (data < o.data);
  }

  object_ptr operator+(ratio_data const &l, ratio_data const &r)
  {
    auto const denom{ l.denominator * r.denominator };
    auto const num{ (l.numerator * r.denominator) + (r.numerator * l.denominator) };
    return ratio::create(num, denom);
  }

  ratio_ptr operator+(integer_ptr const l, ratio_data const &r)
  {
    return l->data + r;
  }

  ratio_ptr operator+(ratio_data const &l, integer_ptr const r)
  {
    return r + l;
  }

  native_real operator+(real_ptr const l, ratio_data const &r)
  {
    return l->data + r.to_real();
  }

  native_real operator+(ratio_data const &l, real_ptr const r)
  {
    return l.to_real() + r->data;
  }

  native_real operator+(ratio_data const &l, native_real const r)
  {
    return l.to_real() + r;
  }

  native_real operator+(native_real const l, ratio_data const &r)
  {
    return l + r.to_real();
  }

  ratio_ptr operator+(ratio_data const &l, native_integer const r)
  {
    return make_box<ratio>(ratio_data(l.numerator + (r * l.denominator), l.denominator));
  }

  ratio_ptr operator+(native_integer const l, ratio_data const &r)
  {
    return r + l;
  }

  object_ptr operator-(ratio_data const &l, ratio_data const &r)
  {
    auto const denom{ l.denominator * r.denominator };
    auto const num{ (l.numerator * r.denominator) - (r.numerator * l.denominator) };
    return ratio::create(num, denom);
  }

  ratio_ptr operator-(integer_ptr const l, ratio_data const &r)
  {
    return l->data - r;
  }

  ratio_ptr operator-(ratio_data const &l, integer_ptr const r)
  {
    return l - r->data;
  }

  native_real operator-(real_ptr const l, ratio_data const &r)
  {
    return l->data - r.to_real();
  }

  native_real operator-(ratio_data const &l, real_ptr const r)
  {
    return l.to_real() - r->data;
  }

  native_real operator-(ratio_data const &l, native_real const r)
  {
    return l.to_real() - r;
  }

  native_real operator-(native_real const l, ratio_data const &r)
  {
    return l - r.to_real();
  }

  ratio_ptr operator-(ratio_data const &l, native_integer const r)
  {
    return make_box<ratio>(ratio_data(l.numerator - (r * l.denominator), l.denominator));
  }

  ratio_ptr operator-(native_integer const l, ratio_data const &r)
  {
    return make_box<ratio>(ratio_data((l * r.denominator) - r.numerator, r.denominator));
  }

  object_ptr operator*(ratio_data const &l, ratio_data const &r)
  {
    return ratio::create(l.numerator * r.numerator, l.denominator * r.denominator);
  }

  object_ptr operator*(integer_ptr const l, ratio_data const &r)
  {
    return ratio_data(l->data, 1ll) * r;
  }

  object_ptr operator*(ratio_data const &l, integer_ptr const r)
  {
    return l * ratio_data(r->data, 1ll);
  }

  native_real operator*(real_ptr const l, ratio_data const &r)
  {
    return l->data * r.to_real();
  }

  native_real operator*(ratio_data const &l, real_ptr const r)
  {
    return l.to_real() * r->data;
  }

  native_real operator*(ratio_data const &l, native_real const r)
  {
    return l.to_real() * r;
  }

  native_real operator*(native_real const l, ratio_data const &r)
  {
    return l * r.to_real();
  }

  object_ptr operator*(ratio_data const &l, native_integer const r)
  {
    return l * ratio_data(r, 1ll);
  }

  object_ptr operator*(native_integer const l, ratio_data const &r)
  {
    return r * l;
  }

  object_ptr operator/(ratio_data const &l, ratio_data const &r)
  {
    return ratio::create(l.numerator * r.denominator, l.denominator * r.numerator);
  }

  object_ptr operator/(integer_ptr const l, ratio_data const &r)
  {
    return ratio_data(l->data, 1ll) / r;
  }

  ratio_ptr operator/(ratio_data const &l, integer_ptr const r)
  {
    return l / r->data;
  }

  native_real operator/(real_ptr const l, ratio_data const &r)
  {
    return l->data / r.to_real();
  }

  native_real operator/(ratio_data const &l, real_ptr const r)
  {
    return l.to_real() / r->data;
  }

  native_real operator/(ratio_data const &l, native_real const r)
  {
    return l.to_real() / r;
  }

  native_real operator/(native_real const l, ratio_data const &r)
  {
    return l / r.to_real();
  }

  ratio_ptr operator/(ratio_data const &l, native_integer const r)
  {
    return make_box<ratio>(ratio_data(l.numerator, l.denominator * r));
  }

  object_ptr operator/(native_integer const l, ratio_data const &r)
  {
    return ratio_data(l, 1ll) / r;
  }

  native_bool operator==(ratio_data const &l, ratio_data const &r)
  {
    return l.numerator->data == r.numerator->data && l.denominator->data == r.denominator->data;
  }

  // native_bool operator==(integer_ptr const l, ratio_data const &r)
  // {
  //   /* By definition an integer should never equal to a ratio. */
  //   return false;
  // }

  native_bool operator==(ratio_data const &l, integer_ptr const r)
  {
    return l.numerator->data == r->data * l.denominator->data;
  }

  native_bool operator==(real_ptr const l, ratio_data const &r)
  {
    return std::fabs(l->data - r) < epsilon;
  }

  native_bool operator==(ratio_data const &l, real_ptr const r)
  {
    return r == l;
  }

  native_bool operator==(ratio_data const &l, native_real const r)
  {
    return std::fabs(l - r) < epsilon;
  }

  native_bool operator==(native_real const l, ratio_data const &r)
  {
    return r == l;
  }

  // native_bool operator==(ratio_data const &l, native_integer const r)
  // {
  //   return l.numerator == r * l.denominator;
  // }

  // // native_bool operator==(native_integer const l, ratio_data const &r)
  // // {
  // //   return l * r.denominator == r.numerator;
  // // }

  native_bool operator<(ratio_data const &l, ratio_data const &r)
  {
    return l.numerator->data * r.denominator->data < r.numerator->data * l.denominator->data;
  }

  native_bool operator<=(ratio_data const &l, ratio_data const &r)
  {
    return l.numerator * r.denominator <= r.numerator * l.denominator;
  }

  native_bool operator<(integer_ptr const l, ratio_data const &r)
  {
    return l->data * r.denominator->data < r.numerator->data;
  }

  native_bool operator<(ratio_data const &l, integer_ptr const r)
  {
    return l.numerator->data < r->data * l.denominator->data;
  }

  native_bool operator<=(integer_ptr const l, ratio_data const &r)
  {
    return l->data * r.denominator <= r.numerator;
  }

  native_bool operator<=(ratio_data const &l, integer_ptr const r)
  {
    return l.numerator <= r->data * l.denominator;
  }

  native_bool operator<(real_ptr const l, ratio_data const &r)
  {
    return l->data < r.to_real();
  }

  native_bool operator<(ratio_data const &l, real_ptr const r)
  {
    return l.to_real() < r->data;
  }

  native_bool operator<=(real_ptr const l, ratio_data const &r)
  {
    return l->data <= r.to_real();
  }

  native_bool operator<=(ratio_data const &l, real_ptr const r)
  {
    return l.to_real() <= r->data;
  }

  native_bool operator<(ratio_data const &l, native_real const r)
  {
    return l.to_real() < r;
  }

  native_bool operator<(native_real const l, ratio_data const &r)
  {
    return l < r.to_real();
  }

  native_bool operator<=(ratio_data const &l, native_real const r)
  {
    return l.to_real() <= r;
  }

  native_bool operator<=(native_real const l, ratio_data const &r)
  {
    return l <= r.to_real();
  }

  native_bool operator<(ratio_data const &l, native_integer const r)
  {
    return l.numerator->data < r * l.denominator->data;
  }

  native_bool operator<(native_integer const l, ratio_data const &r)
  {
    return l * r.denominator->data < r.numerator->data;
  }

  native_bool operator<=(ratio_data const &l, native_integer const r)
  {
    return l.numerator <= r * l.denominator;
  }

  native_bool operator<=(native_integer const l, ratio_data const &r)
  {
    return l * r.denominator <= r.numerator;
  }

  native_bool operator>(ratio_data const &l, ratio_data const &r)
  {
    return l.numerator * r.denominator > r.numerator * l.denominator;
  }

  native_bool operator>(integer_ptr const l, ratio_data const &r)
  {
    return l->data * r.denominator > r.numerator;
  }

  native_bool operator>(ratio_data const &l, integer_ptr const r)
  {
    return l.numerator > r->data * l.denominator;
  }

  native_bool operator>(real_ptr const l, ratio_data const &r)
  {
    return l->data > r.to_real();
  }

  native_bool operator>(ratio_data const &l, real_ptr const r)
  {
    return l.to_real() > r->data;
  }

  native_bool operator>(ratio_data const &l, native_real const r)
  {
    return l.to_real() > r;
  }

  native_bool operator>(native_real const l, ratio_data const &r)
  {
    return l > r.to_real();
  }

  native_bool operator>(ratio_data const &l, native_integer const r)
  {
    return l.numerator > r * l.denominator;
  }

  native_bool operator>(native_integer const l, ratio_data const &r)
  {
    return l * r.denominator > r.numerator;
  }

  native_bool operator>=(ratio_data const &l, ratio_data const &r)
  {
    return l.numerator * r.denominator >= r.numerator * l.denominator;
  }

  native_bool operator>=(integer_ptr const l, ratio_data const &r)
  {
    return l->data * r.denominator >= r.numerator;
  }

  native_bool operator>=(ratio_data const &l, integer_ptr const r)
  {
    return l.numerator >= r->data * l.denominator;
  }

  native_bool operator>=(real_ptr const l, ratio_data const &r)
  {
    return l->data >= r.to_real();
  }

  native_bool operator>=(ratio_data const &l, real_ptr const r)
  {
    return l.to_real() >= r->data;
  }

  native_bool operator>=(ratio_data const &l, native_real const r)
  {
    return l.to_real() >= r;
  }

  native_bool operator>=(native_real const l, ratio_data const &r)
  {
    return l >= r.to_real();
  }

  native_bool operator>=(ratio_data const &l, native_integer const r)
  {
    return l.numerator >= r * l.denominator;
  }

  native_bool operator>=(native_integer const l, ratio_data const &r)
  {
    return l * r.denominator >= r.numerator;
  }

  native_bool operator>(native_bool l, ratio_data const &r)
  {
    return (l ? 1ll : 0ll) > r;
  }

  native_bool operator<(native_bool l, ratio_data const &r)
  {
    return (l ? 1ll : 0ll) < r;
  }

  native_bool operator>(ratio_data const &l, native_bool const r)
  {
    return l > (r ? 1ll : 0ll);
  }

  native_bool operator<(ratio_data const &l, native_bool const r)
  {
    return l < (r ? 1ll : 0ll);
  }
}
