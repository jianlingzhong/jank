#include <stdexcept>
#include <limits>
#include <string>
#include <cmath>

#include <jank/runtime/obj/big_integer.hpp>
#include <jank/runtime/obj/number.hpp>
#include <jank/runtime/core/make_box.hpp>
#include <jank/runtime/core/math.hpp>
#include <jank/runtime/rtti.hpp>

/* This must go last; doctest and glog both define CHECK and family. */
#include <doctest/doctest.h>

namespace jank::runtime::obj
{
  // Helper constants for tests
  static obj::native_big_integer const big_zero{ 0 };
  static obj::native_big_integer const big_ten{ 10 };
  static obj::native_big_integer const big_neg_ten{ -10 };
  static obj::native_big_integer const big_large_pos{ "12345678901234567890" };
  static obj::native_big_integer const big_large_neg{ "-12345678901234567890" };
  static long long const ll_max_val = std::numeric_limits<long long>::max();
  static long long const ll_min_val = std::numeric_limits<long long>::min();
  static obj::native_big_integer const big_ll_max{ ll_max_val };
  static obj::native_big_integer const big_ll_min{ ll_min_val };
  static obj::native_big_integer const big_above_ll_max = big_ll_max + 1;
  static obj::native_big_integer const big_below_ll_min = big_ll_min - 1;

  TEST_SUITE("big_integer")
  {
    TEST_CASE("Default constructor")
    {
      auto const bi_ptr{ make_box<obj::big_integer>() };
      CHECK_EQ(bi_ptr->data, big_zero);
      CHECK_FALSE(bi_ptr->data.backend().sign());
    }

    TEST_CASE("Constructor from native_big_integer (lvalue)")
    {
      auto const bi_ptr{ make_box<obj::big_integer>(big_large_pos) };
      CHECK_EQ(bi_ptr->data, big_large_pos);

      auto const bi_neg_ptr{ make_box<obj::big_integer>(big_large_neg) };
      CHECK_EQ(bi_neg_ptr->data, big_large_neg);
      CHECK(bi_neg_ptr->data.backend().sign()); // Check sign explicitly
    }

    TEST_CASE("Constructor from native_big_integer (rvalue)")
    {
      obj::native_big_integer temp_pos = big_large_pos;
      auto const bi_ptr{ make_box<obj::big_integer>(std::move(temp_pos)) };
      CHECK_EQ(bi_ptr->data, big_large_pos);

      obj::native_big_integer temp_neg = big_large_neg;
      auto const bi_neg_ptr{ make_box<obj::big_integer>(std::move(temp_neg)) };
      CHECK_EQ(bi_neg_ptr->data, big_large_neg);
      CHECK(bi_neg_ptr->data.backend().sign());
    }

    TEST_CASE("Constructor from native_integer (long long)")
    {
      auto const bi_pos_ptr{ make_box<obj::big_integer>(12345ll) };
      CHECK_EQ(bi_pos_ptr->data, 12345);

      auto const bi_neg_ptr{ make_box<obj::big_integer>(-54321ll) };
      CHECK_EQ(bi_neg_ptr->data, -54321);
      CHECK(bi_neg_ptr->data.backend().sign());

      auto const bi_zero_ptr{ make_box<obj::big_integer>(0ll) };
      CHECK_EQ(bi_zero_ptr->data, 0);
      CHECK_FALSE(bi_zero_ptr->data.backend().sign());
    }

    TEST_CASE("Constructor from string")
    {
      SUBCASE("Base 10")
      {
        auto const bi_ptr{ make_box<obj::big_integer>("12345678901234567890") };
        CHECK_EQ(bi_ptr->data, big_large_pos);
        CHECK_FALSE(bi_ptr->data.backend().sign());

        auto const bi_neg_ptr{ make_box<obj::big_integer>("-98765432109876543210") };
        CHECK_EQ(bi_neg_ptr->data, obj::native_big_integer("-98765432109876543210"));
        CHECK(bi_neg_ptr->data.backend().sign());

        auto const bi_zero_ptr{ make_box<obj::big_integer>("0") };
        CHECK_EQ(bi_zero_ptr->data, 0);
      }
      // Note: Requires BOOST_MP_STANDALONE=OFF or custom string parsing for other bases
      // SUBCASE("Base 16") {
      //   auto const bi_ptr{make_box<obj::big_integer>("1a2b3c4d5e6f", 16)};
      //   CHECK_EQ(bi_ptr->data, obj::native_big_integer("0x1a2b3c4d5e6f"));
      // }
      // SUBCASE("Base 8") {
      //   auto const bi_ptr{make_box<obj::big_integer>("12345670", 8)};
      //   CHECK_EQ(bi_ptr->data, obj::native_big_integer("012345670"));
      // }
      SUBCASE("Invalid string")
      {
        CHECK_THROWS_AS(make_box<obj::big_integer>("invalid"), std::runtime_error);
        CHECK_THROWS_AS(make_box<obj::big_integer>("123zz"), std::runtime_error);
        CHECK_EQ(make_box<obj::big_integer>("")->data, 0);
      }
    }

    TEST_CASE("equal()")
    {
      auto const a{ make_box<obj::big_integer>(big_large_pos) };
      auto const a_copy{ make_box<obj::big_integer>(big_large_pos) };
      auto const b{ make_box<obj::big_integer>(big_large_neg) };
      auto const zero{ make_box<obj::big_integer>(0ll) };
      auto const small_int{ make_box<obj::integer>(123) };
      auto const large_int{ make_box<obj::integer>(ll_max_val) }; // Fits in long long

      CHECK(a->equal(*erase(a_copy)));
      CHECK_FALSE(a->equal(*erase(b)));
      CHECK_FALSE(a->equal(*erase(zero)));
      CHECK_FALSE(zero->equal(*erase(a)));

      // Compare with integer
      auto const big_small_int{ make_box<obj::big_integer>(123ll) };
      CHECK(big_small_int->equal(*erase(small_int)));
      CHECK_FALSE(a->equal(*erase(small_int)));
      CHECK(make_box<obj::big_integer>(ll_max_val)->equal(*erase(large_int)));
    }

    TEST_CASE("to_string() / to_code_string()")
    {
      auto const pos{ make_box<obj::big_integer>(big_large_pos) };
      auto const neg{ make_box<obj::big_integer>(big_large_neg) };
      auto const zero{ make_box<obj::big_integer>(0ll) };

      CHECK_EQ(pos->to_string(), native_persistent_string(big_large_pos.str()));
      CHECK_EQ(neg->to_string(), native_persistent_string(big_large_neg.str()));
      CHECK_EQ(zero->to_string(), native_persistent_string("0"));

      CHECK_EQ(pos->to_code_string(), pos->to_string());
      CHECK_EQ(neg->to_code_string(), neg->to_string());
      CHECK_EQ(zero->to_code_string(), zero->to_string());
    }

    TEST_CASE("to_hash()")
    {
      auto const a{ make_box<obj::big_integer>(big_large_pos) };
      auto const a_copy{ make_box<obj::big_integer>(big_large_pos) };
      auto const b{ make_box<obj::big_integer>(big_large_neg) };
      auto const zero{ make_box<obj::big_integer>(0ll) };

      CHECK_EQ(a->to_hash(), a_copy->to_hash());
      CHECK_NE(a->to_hash(), b->to_hash());
      CHECK_NE(a->to_hash(), zero->to_hash());
      CHECK_NE(b->to_hash(), zero->to_hash());
    }

    TEST_CASE("compare()")
    {
      auto const five{ make_box<obj::big_integer>(5ll) };
      auto const ten{ make_box<obj::big_integer>(10ll) };
      auto const neg_five{ make_box<obj::big_integer>(-5ll) };
      auto const big_pos{ make_box<obj::big_integer>(big_large_pos) };
      auto const big_neg{ make_box<obj::big_integer>(big_large_neg) };

      auto const int_five{ make_box<obj::integer>(5ll) };

      // BigInt vs BigInt
      CHECK_EQ(five->compare(*erase(five)), 0);
      // CHECK_LT(five->compare(*erase(ten)), 0);
      // CHECK_GT(ten->compare(*erase(five)), 0);
      // CHECK_LT(neg_five->compare(*erase(five)), 0);
      // CHECK_GT(five->compare(*erase(neg_five)), 0);
      // CHECK_LT(big_neg->compare(*erase(big_pos)), 0);
      // CHECK_GT(big_pos->compare(*erase(big_neg)), 0);

      // // BigInt vs Integer
      // CHECK_EQ(five->compare(*erase(int_five)), 0);
      // CHECK_LT(five->compare(*erase(make_box<obj::integer>(10ll))), 0);
      // CHECK_GT(ten->compare(*erase(int_five)), 0);
      // CHECK_LT(neg_five->compare(*erase(int_five)), 0);
      // CHECK_GT(make_box<obj::big_integer>(-5)->compare(*erase(make_box<obj::integer>(-10ll))),
      //          0); // -5 > -10
    }
    TEST_CASE("to_integer()")
    {
      auto const in_range_pos{ make_box<obj::big_integer>(12345ll) };
      auto const in_range_neg{ make_box<obj::big_integer>(-54321ll) };
      auto const max_ll{ make_box<obj::big_integer>(ll_max_val) };
      auto const min_ll{ make_box<obj::big_integer>(ll_min_val) };
      auto const over_max{ make_box<obj::big_integer>(big_above_ll_max) };
      auto const under_min{ make_box<obj::big_integer>(big_below_ll_min) };
      auto const zero{ make_box<obj::big_integer>(0ll) };

      CHECK_EQ(in_range_pos->to_integer(), 12345ll);
      CHECK_EQ(in_range_neg->to_integer(), -54321ll);
      CHECK_EQ(max_ll->to_integer(), ll_max_val);
      CHECK_EQ(min_ll->to_integer(), ll_min_val);
      CHECK_EQ(zero->to_integer(), 0ll);

      CHECK_EQ(over_max->to_integer(), ll_min_val);
      CHECK_EQ(under_min->to_integer(), ll_max_val);
    }

    TEST_CASE("to_real()")
    {
      auto const small_pos{ make_box<obj::big_integer>(123ll) };
      auto const small_neg{ make_box<obj::big_integer>(-456ll) };
      auto const zero{ make_box<obj::big_integer>(0ll) };
      auto const large_pos{ make_box<obj::big_integer>(big_large_pos) };
      auto const large_neg{ make_box<obj::big_integer>(big_large_neg) };

      CHECK_EQ(small_pos->to_real(), doctest::Approx(123.0));
      CHECK_EQ(small_neg->to_real(), doctest::Approx(-456.0));
      CHECK_EQ(zero->to_real(), doctest::Approx(0.0));

      double large_pos_d = large_pos->to_real();
      CHECK((large_pos_d > 0.0));

      double large_neg_d = large_neg->to_real();
      CHECK((large_neg_d < 0.0));
    }

    TEST_CASE("inc() / dec()")
    {
      auto const five{ make_box<obj::big_integer>(5ll) };
      auto const neg_five{ make_box<obj::big_integer>(-5ll) };
      auto const zero{ make_box<obj::big_integer>(0ll) };
      auto const big{ make_box<obj::big_integer>(big_large_pos) };

      CHECK_EQ(expect_object<obj::big_integer>(inc(five))->data, 6);
      CHECK_EQ(expect_object<obj::big_integer>(inc(neg_five))->data, -4);
      CHECK_EQ(expect_object<obj::big_integer>(inc(zero))->data, 1);
      CHECK_EQ(expect_object<obj::big_integer>(inc(big))->data, big_large_pos + 1);

      CHECK_EQ(expect_object<obj::big_integer>(dec(five))->data, 4);
      CHECK_EQ(expect_object<obj::big_integer>(dec(neg_five))->data, -6);
      CHECK_EQ(expect_object<obj::big_integer>(dec(zero))->data, -1);
      CHECK_EQ(expect_object<obj::big_integer>(dec(big))->data, big_large_pos - 1);
    }

    TEST_CASE("add() / sub()")
    {
      auto const a{ make_box<obj::big_integer>(big_large_pos) };
      auto const b{ make_box<obj::big_integer>(big_large_neg) };
      auto const c{ make_box<obj::big_integer>(100ll) };
      auto const int_val{ make_box<obj::integer>(50ll) };

      CHECK_EQ(expect_object<obj::big_integer>(add(a, c))->data, big_large_pos + 100);
      CHECK_EQ(expect_object<obj::big_integer>(add(c, a))->data, big_large_pos + 100);
      CHECK_EQ(expect_object<obj::big_integer>(add(a, b))->data, big_large_pos + big_large_neg);
      CHECK_EQ(expect_object<obj::big_integer>(add(a, int_val))->data, big_large_pos + 50);
      CHECK_EQ(expect_object<obj::big_integer>(add(int_val, a))->data, big_large_pos + 50);

      CHECK_EQ(expect_object<obj::big_integer>(sub(a, c))->data, big_large_pos - 100);
      CHECK_EQ(expect_object<obj::big_integer>(sub(c, a))->data, 100 - big_large_pos);
      CHECK_EQ(expect_object<obj::big_integer>(sub(a, b))->data, big_large_pos - big_large_neg);
      CHECK_EQ(expect_object<obj::big_integer>(sub(a, int_val))->data, big_large_pos - 50);
      CHECK_EQ(expect_object<obj::big_integer>(sub(int_val, a))->data, 50 - big_large_pos);
    }

    TEST_CASE("mul()")
    {
      auto const a{ make_box<obj::big_integer>(big_large_pos) };
      auto const b{ make_box<obj::big_integer>(-2ll) };
      auto const c{ make_box<obj::big_integer>(100ll) };
      auto const zero{ make_box<obj::big_integer>(0ll) };
      auto const int_val{ make_box<obj::integer>(3ll) };

      CHECK_EQ(expect_object<obj::big_integer>(mul(a, c))->data, big_large_pos * 100);
      CHECK_EQ(expect_object<obj::big_integer>(mul(c, a))->data, big_large_pos * 100);
      CHECK_EQ(expect_object<obj::big_integer>(mul(a, b))->data, big_large_pos * -2);
      CHECK_EQ(expect_object<obj::big_integer>(mul(a, zero))->data, 0);
      CHECK_EQ(expect_object<obj::big_integer>(mul(zero, a))->data, 0);
      CHECK_EQ(expect_object<obj::big_integer>(mul(a, int_val))->data, big_large_pos * 3);
      CHECK_EQ(expect_object<obj::big_integer>(mul(int_val, a))->data, big_large_pos * 3);
    }

    TEST_CASE("div()")
    {
      auto const a{ make_box<obj::big_integer>(big_large_pos) };
      auto const b{ make_box<obj::big_integer>(-2ll) };
      auto const c{ make_box<obj::big_integer>(100ll) };
      auto const zero{ make_box<obj::big_integer>(0ll) };
      auto const int_val{ make_box<obj::integer>(3ll) };
      auto const two{ make_box<obj::big_integer>(2ll) };

      CHECK_EQ(expect_object<obj::big_integer>(div(a, c))->data, big_large_pos / 100);
      CHECK_EQ(expect_object<obj::big_integer>(div(a, b))->data, big_large_pos / -2);
      CHECK_EQ(expect_object<obj::big_integer>(div(c, two))->data, 50);

      CHECK_THROWS_AS(div(a, zero), std::runtime_error);
      CHECK_THROWS_AS(div(a, make_box<obj::integer>(0ll)), std::runtime_error);

      CHECK_EQ(expect_object<obj::big_integer>(div(a, int_val))->data, big_large_pos / 3);

      auto div_int_result = div(c, int_val);
      CHECK(div_int_result->type == object_type::big_integer);
      CHECK_EQ(expect_object<obj::big_integer>(div_int_result)->data, 33);
    }

    TEST_CASE("is_zero(), is_pos(), is_neg()")
    {
      auto const pos{ make_box<obj::big_integer>(big_large_pos) };
      auto const neg{ make_box<obj::big_integer>(big_large_neg) };
      auto const zero{ make_box<obj::big_integer>(0ll) };

      CHECK(is_pos(pos));
      CHECK_FALSE(is_neg(pos));
      CHECK_FALSE(is_zero(pos));

      CHECK(is_neg(neg));
      CHECK_FALSE(is_pos(neg));
      CHECK_FALSE(is_zero(neg));

      CHECK(is_zero(zero));
      CHECK_FALSE(is_pos(zero));
      CHECK_FALSE(is_neg(zero));
    }

    TEST_CASE("is_equiv()")
    {
      auto const a{ make_box<obj::big_integer>(123ll) };
      auto const a_copy{ make_box<obj::big_integer>(123ll) };
      auto const b{ make_box<obj::big_integer>(-123ll) };
      auto const int_a{ make_box<obj::integer>(123ll) };

      CHECK(is_equiv(a, a_copy));
      CHECK_FALSE(is_equiv(a, b));
      CHECK(is_equiv(a, int_a));
      CHECK_FALSE(is_equiv(b, int_a));
    }

    TEST_CASE("abs()")
    {
      auto const pos{ make_box<obj::big_integer>(big_large_pos) };
      auto const neg{ make_box<obj::big_integer>(big_large_neg) };
      auto const zero{ make_box<obj::big_integer>(0ll) };

      CHECK_EQ(expect_object<obj::big_integer>(abs(pos))->data, big_large_pos);
      CHECK_EQ(expect_object<obj::big_integer>(abs(neg))->data, big_large_pos);
      CHECK_EQ(expect_object<obj::big_integer>(abs(zero))->data, 0);
    }

    // TEST_CASE("sqrt()")
    // {
    //   auto const four{ make_box<obj::big_integer>(4ll) };
    //   auto const nine{ make_box<obj::big_integer>(9ll) };
    //   auto const two{ make_box<obj::big_integer>(2ll) };
    //   auto const neg{ make_box<obj::big_integer>(-4ll) };

    //   CHECK_EQ(expect_object<obj::real>(sqrt(four))->data, doctest::Approx(2.0));
    //   CHECK_EQ(expect_object<obj::real>(sqrt(nine))->data, doctest::Approx(3.0));
    //   CHECK_EQ(expect_object<obj::real>(sqrt(two))->data, doctest::Approx(std::sqrt(2.0)));
    // }

    // TEST_CASE("pow()")
    // {
    //   auto const two{ make_box<obj::big_integer>(2ll) };
    //   auto const three{ make_box<obj::big_integer>(3ll) };
    //   auto const ten{ make_box<obj::big_integer>(10ll) };
    //   auto const neg_two{ make_box<obj::big_integer>(-2ll) };
    //   auto const zero{ make_box<obj::big_integer>(0ll) };

    //   CHECK_EQ(expect_object<obj::big_integer>(pow(two, three))->data, 8);
    //   CHECK_EQ(expect_object<obj::big_integer>(pow(three, two))->data, 9);
    //   CHECK_EQ(expect_object<obj::big_integer>(pow(ten, zero))->data, 1);
    //   CHECK_EQ(expect_object<obj::big_integer>(pow(two, ten))->data, 1024);
    //   CHECK_EQ(expect_object<obj::big_integer>(pow(neg_two, two))->data, 4);
    //   CHECK_EQ(expect_object<obj::big_integer>(pow(neg_two, three))->data, -8);

    //   CHECK_EQ(expect_object<obj::big_integer>(pow(zero, ten))->data, 0);
    //   CHECK_EQ(expect_object<obj::big_integer>(pow(zero, zero))->data, 1);

    //   auto pow_neg_exp = pow(two, make_box<obj::big_integer>(-3ll));
    //   CHECK(pow_neg_exp->type == object_type::big_integer);
    //   CHECK_EQ(expect_object<obj::big_integer>(pow_neg_exp)->data, 0);
    // }
  }
} // namespace jank::runtime
