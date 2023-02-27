#pragma once

#include "gtest/gtest.h"

struct dummy_t {};

struct no_default_t {
  no_default_t() = delete;
};

struct throwing_default_t {
  throwing_default_t() {
    throw std::exception();
  }
};

struct throwing_move_operator_t {
  static size_t swap_called;
  throwing_move_operator_t() = default;
  throwing_move_operator_t(throwing_move_operator_t&&) noexcept(false) {
    throw std::exception();
  }
  throwing_move_operator_t& operator=(throwing_move_operator_t&&) = default;
};

inline size_t throwing_move_operator_t::swap_called = 0;

void swap(throwing_move_operator_t&, throwing_move_operator_t&) {
  throwing_move_operator_t::swap_called += 1;
}

struct no_copy_t {
  no_copy_t(const no_copy_t&) = delete;
};

struct no_move_t {
  no_move_t(no_move_t&&) = delete;
};

struct non_trivial_copy_t {
  explicit non_trivial_copy_t(int x) noexcept : x{x} {}
  non_trivial_copy_t(const non_trivial_copy_t& other) noexcept
      : x{other.x + 1} {}

  int x;
};

struct non_trivial_copy_assignment_t {
  explicit non_trivial_copy_assignment_t(int x) noexcept : x{x} {}
  non_trivial_copy_assignment_t&
  operator=(const non_trivial_copy_assignment_t& other) {
    if (this != &other) {
      x = other.x + 5;
    }
    return *this;
  };

  int x;
};

struct no_move_assignment_t {
  no_move_assignment_t& operator=(no_move_assignment_t&&) = delete;
};

struct no_copy_assignment_t {
  no_copy_assignment_t& operator=(const no_copy_assignment_t&) = delete;
};
