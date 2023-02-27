#include "optional.h"
#include "test_classes.h"
#include "test_object.h"
#include "gtest/gtest.h"
#include <stdexcept>
#include <string>
#include <vector>

namespace {
struct no_default_ctor : test_object {
  using test_object::test_object;
  no_default_ctor() = delete;
};

struct only_movable : test_object {
  using test_object::test_object;
  only_movable(only_movable const&) = delete;
  only_movable& operator=(only_movable const&) = delete;

  only_movable(only_movable&& other) noexcept : test_object(std::move(other)) {}

  only_movable& operator=(only_movable&& other) noexcept {
    static_cast<test_object&>(*this) = std::move(other);
    return *this;
  }
};
} // namespace

TEST(optional_testing, default_ctor) {
  optional<no_default_ctor> a;
  EXPECT_FALSE(static_cast<bool>(a));
}

TEST(optional_testing, default_ctor_2) {
  test_object::no_new_instances_guard g;
  optional<test_object> a;
  EXPECT_FALSE(static_cast<bool>(a));
  g.expect_no_instances();
}

TEST(optional_testing, deref_access) {
  optional<test_object> a(42);
  EXPECT_EQ(42, a->operator int());
  EXPECT_EQ(42, std::as_const(a)->operator int());
}

TEST(optional_testing, value_ctor) {
  optional<int> a(42);
  EXPECT_TRUE(static_cast<bool>(a));
  EXPECT_EQ(42, *a);
  EXPECT_EQ(42, *std::as_const(a));
}

TEST(optional_testing, reset) {
  test_object::no_new_instances_guard g;
  optional<test_object> a(42);
  EXPECT_TRUE(static_cast<bool>(a));
  a.reset();
  EXPECT_FALSE(static_cast<bool>(a));
  g.expect_no_instances();
}

TEST(optional_testing, dtor) {
  test_object::no_new_instances_guard g;
  optional<test_object> a(42);
  EXPECT_TRUE(static_cast<bool>(a));
  EXPECT_EQ(42, *a);
}

TEST(optional_testing, copy_ctor) {
  test_object::no_new_instances_guard g;
  optional<test_object> a(42);
  optional<test_object> b = a;
  EXPECT_TRUE(static_cast<bool>(b));
  EXPECT_EQ(42, *b);
}

TEST(optional_testing, copy_ctor_empty) {
  test_object::no_new_instances_guard g;
  optional<test_object> a;
  optional<test_object> b = a;
  EXPECT_FALSE(static_cast<bool>(b));
}

TEST(optional_testing, move_ctor) {
  test_object::no_new_instances_guard g;
  optional<only_movable> a(42);
  optional<only_movable> b = std::move(a);
  EXPECT_TRUE(static_cast<bool>(b));
  EXPECT_EQ(42, *b);
}

TEST(optional_testing, move_ctor_empty) {
  test_object::no_new_instances_guard g;
  optional<test_object> a;
  optional<test_object> b = std::move(a);
  EXPECT_FALSE(static_cast<bool>(b));
}

TEST(optional_testing, assignment_empty_empty) {
  test_object::no_new_instances_guard g;
  optional<test_object> a, b;
  b = a;
  EXPECT_FALSE(static_cast<bool>(b));
}

TEST(optional_testing, assignment_to_empty) {
  test_object::no_new_instances_guard g;
  optional<test_object> a(42), b;
  b = a;
  EXPECT_TRUE(static_cast<bool>(b));
  EXPECT_EQ(42, *b);
}

TEST(optional_testing, assignment_from_empty) {
  test_object::no_new_instances_guard g;
  optional<test_object> a, b(42);
  b = a;
  EXPECT_FALSE(static_cast<bool>(b));
}

TEST(optional_testing, assignment) {
  test_object::no_new_instances_guard g;
  optional<test_object> a(42), b(41);
  b = a;
  EXPECT_TRUE(static_cast<bool>(b));
  EXPECT_EQ(42, *b);
}

TEST(optional_testing, move_assignment_empty_empty) {
  test_object::no_new_instances_guard g;
  optional<only_movable> a, b;
  b = std::move(a);
  EXPECT_FALSE(static_cast<bool>(b));
}

TEST(optional_testing, move_assignment_to_empty) {
  test_object::no_new_instances_guard g;
  optional<only_movable> a(42), b;
  b = std::move(a);
  EXPECT_TRUE(static_cast<bool>(b));
  EXPECT_EQ(42, *b);
}

TEST(optional_testing, move_assignment_from_empty) {
  test_object::no_new_instances_guard g;
  optional<only_movable> a, b(42);
  b = std::move(a);
  EXPECT_FALSE(static_cast<bool>(b));
}

TEST(optional_testing, move_assignment) {
  test_object::no_new_instances_guard g;
  optional<only_movable> a(42), b(41);
  b = std::move(a);
  EXPECT_TRUE(static_cast<bool>(b));
  EXPECT_EQ(42, *b);
}

TEST(optional_testing, nullopt_ctor) {
  optional<test_object> a = nullopt;
  EXPECT_FALSE(static_cast<bool>(a));
}

TEST(optional_testing, nullopt_assignment) {
  optional<test_object> a(42);
  a = nullopt;
  EXPECT_FALSE(static_cast<bool>(a));
  EXPECT_TRUE(noexcept(a = nullopt));
}

struct mytype {
  mytype(int, int, int, std::unique_ptr<int>) {}
};

TEST(optional_testing, in_place_ctor) {
  optional<mytype> a(in_place, 1, 2, 3, std::unique_ptr<int>());
  EXPECT_TRUE(static_cast<bool>(a));
}

TEST(optional_testing, emplace) {
  optional<mytype> a;
  a.emplace(1, 2, 3, std::unique_ptr<int>());
  EXPECT_TRUE(static_cast<bool>(a));
}
namespace {
struct throw_in_ctor {
  struct exception : std::exception {
    using std::exception::exception;
  };

  throw_in_ctor(int, int) {
    if (enable_throw)
      throw exception();
  }

  static inline bool enable_throw = false;
};
} // namespace

TEST(optional_testing, emplace_throw) {
  optional<throw_in_ctor> a(in_place, 1, 2);
  throw_in_ctor::enable_throw = true;
  EXPECT_THROW(a.emplace(3, 4), throw_in_ctor::exception);
  EXPECT_FALSE(static_cast<bool>(a));
}

TEST(optional_testing, comparison_non_empty_and_non_empty) {
  optional<int> a(41), b(42);
  EXPECT_FALSE(a == b);
  EXPECT_TRUE(a != b);
  EXPECT_TRUE(a < b);
  EXPECT_TRUE(a <= b);
  EXPECT_FALSE(a > b);
  EXPECT_FALSE(a >= b);

  EXPECT_TRUE(a == a);
  EXPECT_FALSE(a != a);
  EXPECT_FALSE(a < a);
  EXPECT_TRUE(a <= a);
  EXPECT_FALSE(a > a);
  EXPECT_TRUE(a >= a);

  EXPECT_FALSE(b == a);
  EXPECT_TRUE(b != a);
  EXPECT_FALSE(b < a);
  EXPECT_FALSE(b <= a);
  EXPECT_TRUE(b > a);
  EXPECT_TRUE(b >= a);
}

TEST(optional_testing, comparison_non_empty_and_empty) {
  optional<int> a(41), b;
  EXPECT_FALSE(a == b);
  EXPECT_TRUE(a != b);
  EXPECT_FALSE(a < b);
  EXPECT_FALSE(a <= b);
  EXPECT_TRUE(a > b);
  EXPECT_TRUE(a >= b);

  EXPECT_FALSE(b == a);
  EXPECT_TRUE(b != a);
  EXPECT_TRUE(b < a);
  EXPECT_TRUE(b <= a);
  EXPECT_FALSE(b > a);
  EXPECT_FALSE(b >= a);
}

TEST(optional_testing, comparison_empty_and_empty) {
  optional<int> a, b;
  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);
  EXPECT_FALSE(a < b);
  EXPECT_TRUE(a <= b);
  EXPECT_FALSE(a > b);
  EXPECT_TRUE(a >= b);

  EXPECT_TRUE(a == a);
  EXPECT_FALSE(a != a);
  EXPECT_FALSE(a < a);
  EXPECT_TRUE(a <= a);
  EXPECT_FALSE(a > a);
  EXPECT_TRUE(a >= a);

  EXPECT_TRUE(b == a);
  EXPECT_FALSE(b != a);
  EXPECT_FALSE(b < a);
  EXPECT_TRUE(b <= a);
  EXPECT_FALSE(b > a);
  EXPECT_TRUE(b >= a);
}
namespace {
struct cvalue {
  constexpr cvalue() : value(0) {}

  constexpr cvalue(int value) : value(value) {}

  constexpr cvalue(cvalue const& other) : value(other.value) {}

  constexpr cvalue& operator=(cvalue const& other) {
    value = other.value + 1;
    return *this;
  }

  constexpr int get() const {
    return value;
  }

private:
  int value;
};
} // namespace

TEST(traits, destructor) {
  using optional1 = optional<int>;
  using optional2 = optional<std::string>;
  ASSERT_TRUE(std::is_trivially_destructible_v<optional1>);
  ASSERT_FALSE(std::is_trivially_destructible_v<optional2>);
}

TEST(traits, default_constructor) {
  using optional1 = optional<std::vector<int>>;
  using optional2 = optional<no_default_t>;
  using optional3 = optional<throwing_default_t>;
  ASSERT_TRUE(std::is_default_constructible_v<optional1>);
  ASSERT_TRUE(std::is_default_constructible_v<optional2>);
}

TEST(traits, copy_constructor) {
  using optional1 = optional<no_copy_t>;
  using optional2 = optional<std::vector<std::string>>;
  using optional3 = optional<dummy_t>;
  using optional4 = optional<non_trivial_copy_t>;
  ASSERT_FALSE(std::is_copy_constructible_v<optional1>);
  ASSERT_TRUE(std::is_copy_constructible_v<optional2>);
  ASSERT_FALSE(std::is_trivially_copy_constructible_v<optional2>);
  ASSERT_TRUE(std::is_trivially_copy_constructible_v<optional3>);
  ASSERT_FALSE(std::is_trivially_copy_constructible_v<optional4>);
}

TEST(traits, move_constructor) {
  using optional1 = optional<no_move_t>;
  using optional2 = optional<std::string>;
  using optional3 = optional<dummy_t>;
  using optional4 = optional<throwing_move_operator_t>;
  ASSERT_FALSE(std::is_move_constructible_v<optional1>);
  ASSERT_TRUE(std::is_move_constructible_v<optional2>);
  ASSERT_TRUE(std::is_move_constructible_v<optional3>);
  ASSERT_FALSE(std::is_trivially_move_constructible_v<optional2>);
  ASSERT_TRUE(std::is_trivially_move_constructible_v<optional3>);
  ASSERT_TRUE(std::is_move_constructible_v<optional4>);
}

TEST(traits, copy_assignment) {
  using optional1 = optional<no_copy_t>;
  using optional2 = optional<no_copy_assignment_t>;
  using optional3 = optional<non_trivial_copy_assignment_t>;
  using optional4 = optional<non_trivial_copy_t>;
  using optional5 = optional<dummy_t>;
  using optional6 = optional<no_copy_t>;
  ASSERT_FALSE(std::is_copy_assignable_v<optional1>);
  ASSERT_FALSE(std::is_copy_assignable_v<optional2>);
  ASSERT_TRUE(std::is_copy_assignable_v<optional3>);
  ASSERT_TRUE(std::is_copy_assignable_v<optional4>);
  ASSERT_TRUE(std::is_copy_assignable_v<optional5>);
  ASSERT_FALSE(std::is_trivially_copy_assignable_v<optional3>);
  ASSERT_FALSE(std::is_trivially_copy_assignable_v<optional4>);
  ASSERT_TRUE(std::is_trivially_copy_assignable_v<optional5>);
  ASSERT_FALSE(std::is_copy_assignable_v<optional6>);
}

TEST(traits, move_assignment) {
  using optional1 = optional<no_move_t>;
  using optional2 = optional<no_move_assignment_t>;
  using optional3 = optional<std::vector<double>>;
  using optional4 = optional<std::string>;
  using optional5 = optional<dummy_t>;
  using optional6 = optional<throwing_move_operator_t>;
  using optional7 = optional<no_move_t>;
  ASSERT_FALSE(std::is_move_assignable_v<optional1>);
  ASSERT_FALSE(std::is_move_assignable_v<optional2>);
  ASSERT_TRUE(std::is_move_assignable_v<optional3>);
  ASSERT_TRUE(std::is_move_assignable_v<optional4>);
  ASSERT_TRUE(std::is_move_assignable_v<optional5>);
  ASSERT_FALSE(std::is_trivially_move_assignable_v<optional3>);
  ASSERT_FALSE(std::is_trivially_move_assignable_v<optional4>);
  ASSERT_TRUE(std::is_trivially_move_assignable_v<optional5>);
  ASSERT_TRUE(std::is_move_assignable_v<optional6>);
  ASSERT_FALSE(std::is_move_assignable_v<optional7>);
}

static_assert([] {
  optional<cvalue> a;
  return !static_cast<bool>(a);
}());

static_assert([] {
  optional<cvalue> a(nullopt);
  return !static_cast<bool>(a);
}());

static_assert([] {
  optional<cvalue> a(42);
  return (*a).get() == 42;
}());

static_assert([] {
  optional<cvalue> a(in_place, 42);
  return (*a).get() == 42;
}());

static_assert([] {
  optional<cvalue> a(42);
  return (*std::as_const(a)).get() == 42;
}());

static_assert([] {
  optional<cvalue> a(42);
  return a->get() == 42;
}());

static_assert([] {
  optional<cvalue> a(42);
  return std::as_const(a)->get() == 42;
}());

static_assert([] {
  optional<int> a(42);
  return a == a;
}());

static_assert([] {
  optional<int> a(42), b(43);
  return a != b;
}());

static_assert([] {
  optional<int> a(42), b(43);
  return a < b;
}());

static_assert([] {
  optional<int> a(42), b(43);
  return a <= b;
}());

static_assert([] {
  optional<int> a(43), b(42);
  return a > b;
}());

static_assert([] {
  optional<int> a(43), b(42);
  return a >= b;
}());

static_assert([] {
  optional<int> a(43);
  optional<int> b(a);
  return a == b;
}());

static_assert([] {
  optional<int> a(43);
  optional<int> b(std::move(a));
  return b && *b == 43;
}());

static_assert([] {
  optional<int> a(43), b(42);
  a = b;
  return a == b;
}());

static_assert([] {
  optional<int> a(43), b(42);
  a = std::move(b);
  return *a == 42;
}());
