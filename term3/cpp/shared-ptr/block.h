#pragma once

#include <memory>

namespace shared_ptr_details {

class control_block {
  size_t shar_ref = 0;
  size_t weak_ref = 0;

public:
  virtual void delete_obj() = 0;
  virtual ~control_block() = default;

  size_t shar() {
    return shar_ref;
  }
  size_t weak() {
    return weak_ref;
  }

  void shar_inc() {
    shar_ref++;
  }
  void weak_inc() {
    weak_ref++;
  }
  void shar_dec() {
    shar_ref--;
    if (shar_ref == 0) {
      delete_obj();
      if (weak_ref == 0) {
        delete this;
      }
    }
  }
  void weak_dec() {
    weak_ref--;
    if (weak_ref == 0 && shar_ref == 0) {
      delete this;
    }
  }
};

template <typename T, typename D>
class ptr_block : public control_block {
  T* ptr;
  D del;

public:
  explicit ptr_block(T* ptr, D del) : ptr(ptr), del(std::move(del)) {}
  T* get() {
    return ptr;
  }
  void delete_obj() {
    if (ptr)
      del(ptr);
    ptr = nullptr;
  }
};

template <typename T>
struct object_block : public control_block {
  std::aligned_storage_t<sizeof(T), alignof(T)> Type;

  template <typename... Args>
  explicit object_block(Args&&... args) {
    new (&Type) T(args...);
  }
  T* get() {
    return reinterpret_cast<T*>(&Type);
  }
  void delete_obj() {
    get()->~T();
  }
};

} // namespace shared_ptr_details