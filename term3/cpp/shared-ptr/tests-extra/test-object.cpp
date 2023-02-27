#include "test-object.h"
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
  {
    EXPECT_TRUE(instances.find(&other) != instances.end());
    auto p = instances.insert(this);
    EXPECT_TRUE(p.second);
  }
  data = transcode(transcode(other.data, &other), this);
}

test_object::~test_object() {
  size_t n = instances.erase(this);
  EXPECT_EQ(1u, n);
}

test_object& test_object::operator=(test_object const& c) {
  EXPECT_TRUE(instances.find(this) != instances.end());
  data = transcode(transcode(c.data, &c), this);
  return *this;
}

test_object::operator int() const {
  EXPECT_TRUE(instances.find(this) != instances.end());

  return transcode(data, this);
}

std::set<test_object const*> test_object::instances;

test_object::no_new_instances_guard::no_new_instances_guard()
    : old_instances(instances) {}

test_object::no_new_instances_guard::~no_new_instances_guard() {
  EXPECT_TRUE(old_instances == instances);
}

void test_object::no_new_instances_guard::expect_no_instances() const {
  EXPECT_TRUE(old_instances == instances);
}
