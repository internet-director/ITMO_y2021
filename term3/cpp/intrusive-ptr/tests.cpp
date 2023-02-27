#include "gtest/gtest.h"

#include "solution.h"

#include <thread>

struct object : intrusive_ref_counter<object> {};
struct derived : object {};

TEST(correctness, default_ref_counter_ctor) {
  object o;
  ASSERT_EQ(o.use_count(), 0);
}

TEST(correctness, default_ptr_ctor) {
  intrusive_ptr<object> ptr;
  ASSERT_EQ(ptr.get(), nullptr);
  ASSERT_EQ(bool(ptr), false);
  ASSERT_EQ(!ptr, true);
}

TEST(correctness, ptr_ctor) {
  auto o = new object();
  intrusive_ptr<object> ptr3(o);
  ASSERT_EQ(o->use_count(), 1);
  { // intrusive_ptr ctor increments ref counter
    intrusive_ptr<object> ptr(o);
    ASSERT_EQ(ptr.get(), o);
    ASSERT_EQ(ptr->use_count(), 2);
    ASSERT_EQ(o->use_count(), 2);
  } // and decrements in destructor
  ASSERT_EQ(o->use_count(), 1);
  {
    intrusive_ptr<object> ptr(o);
    ASSERT_EQ(ptr->use_count(), 2);
    ASSERT_EQ(o->use_count(), 2);
    ptr.detach(); // doesn't decrement counter
  }
  ASSERT_EQ(o->use_count(), 2);
  { // doesn't decrement counter with add_ref = false
    intrusive_ptr<object> ptr(o, false);
    ASSERT_EQ(o->use_count(), 2);
  } // but does increment
  ASSERT_EQ(o->use_count(), 1);
}

TEST(correctness, ptr_copy_move_ctor) {
  auto o = new derived();
  intrusive_ptr<derived> a(o);
  ASSERT_EQ(o->use_count(), 1);
  auto b = a;
  ASSERT_EQ(o->use_count(), 2);
  intrusive_ptr<object> c = b;
  ASSERT_EQ(o->use_count(), 3);

  c = std::move(b);
  ASSERT_EQ(o->use_count(), 2);
}

TEST(correctness, thread_safety_check) {
  /// Num threads to run
  auto N = std::thread::hardware_concurrency();

  /// Number of copy ctor calls in a worker thread
  auto N_ctors = 16 * 1000000 / N;

  ASSERT_GT(N, 1);

  auto o = new derived();
  intrusive_ptr<object> ptr(o);
  std::vector<std::thread> workers;
  for (size_t i = 0; i < N; ++i) {
    workers.emplace_back([o, N_ctors]() {
      for (size_t j = 0; j < N_ctors; j++) {
        intrusive_ptr<object> ptr(o);
      }
    });
  }

  for (auto& W : workers)
    W.join();

  ASSERT_EQ(o->use_count(), 1);
}
