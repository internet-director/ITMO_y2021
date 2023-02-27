#include "test-classes.h"

std::unordered_set<address_checking_object const*> address_checking_object::addresses;
void address_checking_object::add_instance() const {
  auto [it, was_inserted] = addresses.insert(this);
  if (!was_inserted) {
    FAIL() << "New object is created at the address "
           << static_cast<void const*>(this)
           << " while the previous object at this address was not destroyed";
  }
}
void address_checking_object::remove_instance() const {
  size_t erased_count = addresses.erase(this);
  if (erased_count != 1) {
    FAIL() <<"Destroying non-existing object at the address "
           << static_cast<void const*>(this);
  }
}
void address_checking_object::assert_exists() const {
  if (!addresses.contains(this)) {
    FAIL() << "Accessing an non-existing object at address "
           << static_cast<void const*>(this);
  }
}
void address_checking_object::expect_no_instances() {
  if (!addresses.empty()) {
    addresses.clear();
    FAIL() << "Not all instances are destroyed";
  }
}

size_t address_checking_object::copy_throw_countdown = 0;
void address_checking_object::process_copying() {
  if (copy_throw_countdown != 0)
    if (--copy_throw_countdown == 0)
      throw std::runtime_error("address_checking_object copying failed");
}
void address_checking_object::set_copy_throw_countdown(size_t new_countdown) {
  copy_throw_countdown = new_countdown;
}

address_checking_object::operator int() const {
  assert_exists();
  return value;
}

address_checking_object::address_checking_object() {
  add_instance();
}
address_checking_object::address_checking_object(int value) : value(value) {
  add_instance();
}
address_checking_object::address_checking_object(
    const address_checking_object& other)
    : value(other.value) {
  process_copying();
  add_instance();
}
address_checking_object&
address_checking_object::operator=(const address_checking_object& other) {
  assert_exists();
  other.assert_exists();
  process_copying();
  value = other.value;
  return *this;
}
address_checking_object::~address_checking_object() {
  remove_instance();
}
