#include "shared-ptr.h"
#include "tests-extra/test-object.h"
#include <gtest/gtest.h>

template <typename T>
struct custom_deleter {
  explicit custom_deleter(bool* deleted) : deleted(deleted) {}

  void operator()(T* object) {
    *deleted = true;
    delete object;
  }

private:
  bool* deleted;
};

struct base {};

struct derived : base {
  explicit derived(bool* deleted) : deleted(deleted) {}

  ~derived() {
    *deleted = true;
  }

private:
  bool* deleted;
};

TEST(shared_ptr_testing, ptr_ctor_inheritance) {
  bool deleted = false;
  { shared_ptr<base> p(new derived(&deleted)); }
  EXPECT_TRUE(deleted);
}

TEST(shared_ptr_testing, reset_ptr_inheritance) {
  bool deleted = false;
  {
    shared_ptr<base> p;
    p.reset(new derived(&deleted));
  }
  EXPECT_TRUE(deleted);
}

TEST(shared_ptr_testing, weak_ptr_copy_ctor) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;
  weak_ptr<test_object> r = q;
}

TEST(shared_ptr_testing, weak_ptr_copy_ctor_nullptr) {
  weak_ptr<test_object> p;
  weak_ptr<test_object> q = p;
}

TEST(shared_ptr_testing, weak_ptr_move_ctor) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;
  weak_ptr<test_object> r = std::move(q);
  shared_ptr<test_object> s = r.lock();
  EXPECT_TRUE(p == s);
}

TEST(shared_ptr_testing, weak_ptr_move_ctor_nullptr) {
  weak_ptr<test_object> p;
  weak_ptr<test_object> q = p;
}

TEST(shared_ptr_testing, weak_ptr_assignment_operator) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p1(new test_object(42));
  weak_ptr<test_object> q1 = p1;
  shared_ptr<test_object> p2(new test_object(43));
  weak_ptr<test_object> q2 = p2;

  q1 = q2;

  EXPECT_TRUE(q1.lock() == p2);
  EXPECT_TRUE(q2.lock() == p2);
}

TEST(shared_ptr_testing, weak_ptr_assignment_operator_from_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p1(new test_object(42));
  weak_ptr<test_object> q1 = p1;
  weak_ptr<test_object> q2;

  q1 = q2;

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST(shared_ptr_testing, weak_ptr_assignment_operator_to_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q1;
  weak_ptr<test_object> q2 = p;

  q1 = q2;

  EXPECT_TRUE(q1.lock() == p);
  EXPECT_TRUE(q2.lock() == p);
}

TEST(shared_ptr_testing, weak_ptr_assignment_operator_nullptr) {
  weak_ptr<test_object> q1;
  weak_ptr<test_object> q2;

  q1 = q2;

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST(shared_ptr_testing, weak_ptr_assignment_operator_self) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;

  q = q;

  EXPECT_TRUE(q.lock() == p);
}

TEST(shared_ptr_testing, weak_ptr_assignment_operator_self_nullptr) {
  test_object::no_new_instances_guard g;
  weak_ptr<test_object> q;

  q = q;

  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST(shared_ptr_testing, weak_ptr_move_assignment_operator) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p1(new test_object(42));
  weak_ptr<test_object> q1 = p1;
  shared_ptr<test_object> p2(new test_object(43));
  weak_ptr<test_object> q2 = p2;

  q1 = std::move(q2);

  EXPECT_TRUE(q1.lock() == p2);
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST(shared_ptr_testing, weak_ptr_move_assignment_operator_from_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p1(new test_object(42));
  weak_ptr<test_object> q1 = p1;
  weak_ptr<test_object> q2;

  q1 = std::move(q2);

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST(shared_ptr_testing, weak_ptr_move_assignment_operator_to_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q1;
  weak_ptr<test_object> q2 = p;

  q1 = std::move(q2);

  EXPECT_TRUE(q1.lock() == p);
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST(shared_ptr_testing, weak_ptr_move_assignment_operator_nullptr) {
  weak_ptr<test_object> q1;
  weak_ptr<test_object> q2;

  q1 = std::move(q2);

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST(shared_ptr_testing, weak_ptr_move_assignment_operator_self) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;

  q = std::move(q);

  EXPECT_TRUE(q.lock() == p);
}

TEST(shared_ptr_testing, weak_ptr_move_assignment_operator_self_nullptr) {
  test_object::no_new_instances_guard g;
  weak_ptr<test_object> q;

  q = std::move(q);

  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST(shared_ptr_testing, custom_deleter) {
  test_object::no_new_instances_guard g;
  bool deleted = false;
  {
    shared_ptr<test_object> p(new test_object(42),
                              custom_deleter<test_object>(&deleted));
  }
  EXPECT_TRUE(deleted);
}

TEST(shared_ptr_testing, custom_deleter_reset) {
  test_object::no_new_instances_guard g;
  bool deleted;
  {
    shared_ptr<test_object> p;
    p.reset(new test_object(42), custom_deleter<test_object>(&deleted));
  }
  EXPECT_TRUE(deleted);
}

TEST(shared_ptr_testing, aliasing_ctor) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  int x;
  shared_ptr<int> q(p, &x);
  EXPECT_EQ(2, p.use_count());
  EXPECT_EQ(2, q.use_count());
}

TEST(shared_ptr_testing, aliasing_ctor_nullptr_non_empty) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<int> q(p, nullptr);
  EXPECT_EQ(2, p.use_count());
  EXPECT_EQ(2, q.use_count());
  EXPECT_TRUE(q.get() == nullptr);
}

TEST(shared_ptr_testing, comparison_with_nullptr) {
  shared_ptr<test_object> p;
  EXPECT_TRUE(p == nullptr);
  EXPECT_FALSE(p != nullptr);
  EXPECT_TRUE(nullptr == p);
  EXPECT_FALSE(nullptr != p);
}

TEST(shared_ptr_testing, conversions_const) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object const> q = p;
  EXPECT_EQ(42, *q);
}

TEST(shared_ptr_testing, conversions_inheritance) {
  struct base {};
  struct derived : base {};

  shared_ptr<derived> d(new derived());
  shared_ptr<base> b = d;
  EXPECT_EQ(d.get(), b.get());
}
