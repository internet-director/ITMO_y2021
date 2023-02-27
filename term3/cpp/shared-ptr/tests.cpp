#include "shared-ptr.h"
#include "tests-extra/test-object.h"
#include <gtest/gtest.h>

TEST(shared_ptr_testing, default_ctor) {
  shared_ptr<test_object> p;
  EXPECT_EQ(nullptr, p.get());
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST(shared_ptr_testing, ptr_ctor) {
  test_object::no_new_instances_guard g;
  test_object* p = new test_object(42);
  shared_ptr<test_object> q(p);
  EXPECT_TRUE(static_cast<bool>(q));
  EXPECT_EQ(p, q.get());
  EXPECT_EQ(42, *q);
}

TEST(shared_ptr_testing, ptr_ctor_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(nullptr);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_EQ(0, p.use_count());
}

TEST(shared_ptr_testing, ptr_ctor_non_empty_nullptr) {
  shared_ptr<test_object> p(static_cast<test_object*>(nullptr));
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_EQ(1, p.use_count());
}

TEST(shared_ptr_testing, copy_ctor) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  EXPECT_EQ(1, p.use_count());
  shared_ptr<test_object> q = p;
  EXPECT_TRUE(static_cast<bool>(p));
  EXPECT_TRUE(static_cast<bool>(q));
  EXPECT_TRUE(p == q);
  EXPECT_EQ(42, *p);
  EXPECT_EQ(42, *q);
  EXPECT_EQ(2, q.use_count());
}

TEST(shared_ptr_testing, copy_ctor_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q = p;
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST(shared_ptr_testing, const_dereferencing) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> const p(new test_object(42));
  EXPECT_EQ(42, *p);
  EXPECT_EQ(42, p->operator int());
}

TEST(shared_ptr_testing, reset) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> q(new test_object(42));
  EXPECT_TRUE(static_cast<bool>(q));
  q.reset();
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST(shared_ptr_testing, reset_nullptr) {
  shared_ptr<test_object> q;
  EXPECT_FALSE(static_cast<bool>(q));
  q.reset();
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST(shared_ptr_testing, reset_ptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> q(new test_object(42));
  EXPECT_TRUE(static_cast<bool>(q));
  q.reset(new test_object(43));
  EXPECT_EQ(43, *q);
}

TEST(shared_ptr_testing, move_ctor) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q = std::move(p);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_TRUE(static_cast<bool>(q));
  EXPECT_EQ(42, *q);
}

TEST(shared_ptr_testing, move_ctor_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q = std::move(p);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST(shared_ptr_testing, assignment_operator) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q(new test_object(43));
  p = q;
  EXPECT_EQ(43, *p);
  EXPECT_TRUE(p == q);
}

TEST(shared_ptr_testing, assignment_operator_from_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q;
  p = q;
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST(shared_ptr_testing, assignment_operator_to_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p;
  shared_ptr<test_object> q(new test_object(43));
  p = q;
  EXPECT_EQ(43, *p);
  EXPECT_TRUE(p == q);
}

TEST(shared_ptr_testing, assignment_operator_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q;
  p = q;
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST(shared_ptr_testing, assignment_operator_const) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> const q(new test_object(43));
  p = q;
  EXPECT_EQ(43, *p);
  EXPECT_TRUE(p == q);
}

TEST(shared_ptr_testing, assignment_operator_self) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  p = p;
  EXPECT_EQ(42, *p);
}

TEST(shared_ptr_testing, assignment_operator_self_nullptr) {
  shared_ptr<test_object> p;
  p = p;
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST(shared_ptr_testing, move_assignment_operator) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q(new test_object(43));
  p = std::move(q);
  EXPECT_EQ(43, *p);
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST(shared_ptr_testing, move_assignment_operator_from_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q;
  p = std::move(q);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST(shared_ptr_testing, move_assignment_operator_to_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p;
  shared_ptr<test_object> q(new test_object(43));
  p = std::move(q);
  EXPECT_EQ(43, *p);
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST(shared_ptr_testing, move_assignment_operator_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q;
  p = std::move(q);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST(shared_ptr_testing, move_assignment_operator_self) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  p = std::move(p);
  EXPECT_EQ(42, *p);
}

TEST(shared_ptr_testing, move_assignment_operator_self_nullptr) {
  shared_ptr<test_object> p;
  p = std::move(p);
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST(shared_ptr_testing, weak_ptr_lock) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;
  shared_ptr<test_object> r = q.lock();
  EXPECT_TRUE(r == p);
  EXPECT_EQ(42, *r);
}

TEST(shared_ptr_testing, weak_ptr_lock_nullptr) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;
  p.reset();
  g.expect_no_instances();
  shared_ptr<test_object> r = q.lock();
  EXPECT_FALSE(static_cast<bool>(r));
}

TEST(shared_ptr_testing, weak_ptr_lock_nullptr_2) {
  weak_ptr<test_object> q;
  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST(shared_ptr_testing, make_shared) {
  test_object::no_new_instances_guard g;
  shared_ptr<test_object> p = make_shared<test_object>(42);
  EXPECT_EQ(42, *p);
}

TEST(shared_ptr_testing, make_shared_weak_ptr) {
  test_object::no_new_instances_guard g;
  weak_ptr<test_object> p;
  {
    shared_ptr<test_object> q = make_shared<test_object>(42);
    p = q;
  }
  g.expect_no_instances();
}

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define DISABLE_ALLOCATION_TESTS 1
#endif
#endif

#ifndef DISABLE_ALLOCATION_TESTS
namespace {
size_t new_calls = 0;
size_t delete_calls = 0;
} // namespace
void* operator new(std::size_t count) {
  new_calls += 1;
  return malloc(count);
}

void operator delete(void* ptr) noexcept {
  delete_calls += 1;
  free(ptr);
}

void operator delete(void* ptr, size_t) noexcept {
  delete_calls += 1;
  free(ptr);
}

TEST(shared_ptr_testing, weak_ptr_allocations) {
  size_t new_calls_before = new_calls;
  size_t delete_calls_before = delete_calls;
  int* i_p = new int(1337);
  weak_ptr<int> w_p;
  {
    shared_ptr<int> s_p(i_p);
    w_p = s_p;
  }
  const auto new_calls_after = new_calls;
  const auto delete_calls_after = delete_calls;
  EXPECT_EQ(new_calls_after - new_calls_before, 2);
  EXPECT_EQ(delete_calls_after - delete_calls_before, 1);
  EXPECT_FALSE(w_p.lock());
}

TEST(shared_ptr_testing, make_shared_weak_ptr_allocations) {
  size_t new_calls_before = new_calls;
  size_t delete_calls_before = delete_calls;
  weak_ptr<int> w_p;
  {
    shared_ptr<int> s_p = make_shared<int>(42);
    w_p = s_p;
  }
  const auto new_calls_after = new_calls;
  const auto delete_calls_after = delete_calls;
  EXPECT_EQ(new_calls_after - new_calls_before, 1);
  EXPECT_EQ(delete_calls_after - delete_calls_before, 0);
  EXPECT_FALSE(w_p.lock());
}

TEST(shared_ptr_testing, allocations) {
  size_t new_calls_before = new_calls;
  size_t delete_calls_before = delete_calls;
  int* i_p = new int(1337);
  {
    shared_ptr<int> p(i_p);
    EXPECT_EQ(*i_p, *p);
  }
  const auto new_calls_after = new_calls;
  const auto delete_calls_after = delete_calls;
  EXPECT_EQ(new_calls_after - new_calls_before, 2);
  EXPECT_EQ(delete_calls_after - delete_calls_before, 2);
}

TEST(shared_ptr_testing, make_shared_allocations) {
  size_t new_calls_before = new_calls;
  size_t delete_calls_before = delete_calls;
  {
    shared_ptr<int> p = make_shared<int>(42);
    EXPECT_EQ(42, *p);
  }
  const auto new_calls_after = new_calls;
  const auto delete_calls_after = delete_calls;
  EXPECT_EQ(new_calls_after - new_calls_before, 1);
  EXPECT_EQ(delete_calls_after - delete_calls_before, 1);
}
#endif
