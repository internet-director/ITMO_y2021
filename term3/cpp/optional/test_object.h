#pragma once

#include <set>

struct test_object {
  struct no_new_instances_guard;

  test_object() = delete;
  test_object(int data);
  test_object(test_object const& other);
  ~test_object();

  test_object& operator=(test_object const& c);
  operator int() const;

private:
  void check_this() const;

private:
  int data;

  static std::set<test_object const*> instances;
};

struct test_object::no_new_instances_guard {
  no_new_instances_guard();

  no_new_instances_guard(no_new_instances_guard const&) = delete;
  no_new_instances_guard& operator=(no_new_instances_guard const&) = delete;

  ~no_new_instances_guard();

  void expect_no_instances() const;

private:
  std::set<test_object const*> old_instances;
};