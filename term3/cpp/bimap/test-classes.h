#pragma once

#include <cmath>
#include <unordered_set>
#include <utility>

#include "gtest/gtest.h"

struct test_object {
  int a = 0;
  test_object() = default;
  explicit test_object(int b) : a(b) {}
  test_object(test_object&& other) noexcept {
    std::swap(a, other.a);
  }
  friend bool operator<(test_object const& c, test_object const& b) {
    return c.a < b.a;
  }
  friend bool operator==(test_object const& c, test_object const& b) {
    return c.a == b.a;
  }
};

struct vector_compare {
  using vec = std::pair<int, int>;
  enum distance_type { euclidean, manhattan };

  explicit vector_compare(distance_type p = euclidean) : type(p) {}

  bool operator()(vec a, vec b) const {
    if (type == euclidean) {
      return euc(a) < euc(b);
    } else {
      return man(a) < man(b);
    }
  }

private:
  static double euc(vec x) {
    return sqrt(x.first * x.first + x.second * x.second);
  }

  static double man(vec x) {
    return abs(x.first) + abs(x.second);
  }

  distance_type type;
};

struct non_default_constructible {
  non_default_constructible() = delete;
  explicit non_default_constructible(int b) : a(b) {}
  non_default_constructible(non_default_constructible const&) = default;
  friend bool operator<(non_default_constructible const& c,
                        non_default_constructible const& b) {
    return c.a < b.a;
  }
  friend bool operator==(non_default_constructible const& c,
                         non_default_constructible const& b) {
    return c.a == b.a;
  }

private:
  int a;
};

class address_checking_object {
private:
  static std::unordered_set<address_checking_object const*> addresses;

  void add_instance() const;
  void remove_instance() const;
  void assert_exists() const;

  int value;

  static size_t copy_throw_countdown;
  static void process_copying();

public:
  static void expect_no_instances();

  static void set_copy_throw_countdown(size_t new_countdown);

  /* implicit */ operator int() const;

  address_checking_object();
  /* implicit */ address_checking_object(int value);
  address_checking_object(address_checking_object const& other);
  address_checking_object& operator=(address_checking_object const& other);
  ~address_checking_object();
};
