#include "test_object.h"
#include <gtest/gtest.h>

namespace {
int transcode(int data, void const* ptr) {
  return data ^ static_cast<int>(reinterpret_cast<std::ptrdiff_t>(ptr) /
                                 sizeof(test_object));
}
} // namespace

test_object::test_object(int data) : data(transcode(data, this)) {
  auto p = instances.insert(this);
  EXPECT_TRUE(p.second);
}

test_object::test_object(test_object const& other) {
  other.check_this();
  {
    auto p = instances.insert(this);
    EXPECT_TRUE(p.second);
  }
  data = transcode(transcode(other.data, &other), this);
}

test_object::~test_object() {
  size_t n = instances.erase(this);
  if (n != 1)
    ADD_FAILURE() << "destroying non-existing object at " << this;
}

test_object& test_object::operator=(test_object const& c) {
  check_this();
  c.check_this();

  data = transcode(transcode(c.data, &c), this);
  return *this;
}

test_object::operator int() const {
  check_this();
  return transcode(data, this);
}

void test_object::check_this() const {
  if (instances.find(this) == instances.end()) {
    ADD_FAILURE() << "accessing non-existing object at " << this;
    std::abort();
  }
}

std::set<test_object const*> test_object::instances;

test_object::no_new_instances_guard::no_new_instances_guard()
    : old_instances(instances) {}

test_object::no_new_instances_guard::~no_new_instances_guard() {
  EXPECT_EQ(old_instances, instances);
}

void test_object::no_new_instances_guard::expect_no_instances() const {
  EXPECT_EQ(old_instances, instances);
}