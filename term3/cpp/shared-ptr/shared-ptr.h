#pragma once

#include "block.h"

template <typename T>
class weak_ptr;

template <typename T>
class shared_ptr {
public:
  shared_ptr() noexcept : cb(nullptr), ptr(nullptr) {}
  shared_ptr(std::nullptr_t) noexcept : shared_ptr() {}
  template <typename T1>
  shared_ptr(T1* ptr) : shared_ptr(ptr, std::default_delete<T1>()) {}
  template <typename T1, typename D>
  shared_ptr(T1* ptr, D del) : ptr(ptr) {
    try {
      cb = new shared_ptr_details::ptr_block<T1, D>(ptr, std::move(del));
    } catch (...) {
      del(ptr);
      throw;
    }
    cb->shar_inc();
  }

  shared_ptr(const shared_ptr& other) noexcept : cb(other.cb), ptr(other.ptr) {
    if (cb != nullptr) {
      cb->shar_inc();
    }
  }
  template <typename T1>
  shared_ptr(const shared_ptr<T1>& other) noexcept
      : cb(other.cb), ptr(other.ptr) {
    if (cb != nullptr) {
      cb->shar_inc();
    }
  }
  shared_ptr(const weak_ptr<T>& other) noexcept : cb(other.cb), ptr(other.ptr) {
    if (cb != nullptr) {
      cb->shar_inc();
    }
  }

  shared_ptr(const shared_ptr& other, T* ptr) noexcept
      : cb(other.cb), ptr(ptr) {
    if (cb != nullptr) {
      cb->shar_inc();
    }
  }
  template <typename T1>
  shared_ptr(const shared_ptr<T1>& other, T* ptr) noexcept
      : cb(other.cb), ptr(ptr) {
    if (cb != nullptr) {
      cb->shar_inc();
    }
  }
  shared_ptr(shared_ptr&& other) noexcept : shared_ptr() {
    swap(other);
  }
  ~shared_ptr() {
    clear();
  }

  friend bool operator==(const shared_ptr& l, const shared_ptr& r) noexcept {
    return l.ptr == r.ptr;
  }
  friend bool operator!=(const shared_ptr& l, const shared_ptr& r) noexcept {
    return l.ptr != r.ptr;
  }

  shared_ptr& operator=(const shared_ptr& other) noexcept {
    if (this == &other) {
      return *this;
    }
    shared_ptr<T> shared(other);
    swap(shared);
    return *this;
  }
  template <typename T1>
  shared_ptr& operator=(const shared_ptr<T1>& other) noexcept {
    if (this == &other) {
      return *this;
    }
    shared_ptr<T> shared(other);
    swap(shared);
    return *this;
  }
  shared_ptr& operator=(shared_ptr&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    shared_ptr<T> shared(std::move(other));
    swap(shared);
    return *this;
  }
  template <typename T1>
  shared_ptr& operator=(shared_ptr<T1>&& other) noexcept {
    shared_ptr<T> shared(std::move(other));
    swap(shared);
    return *this;
  }

  T* get() const noexcept {
    return ptr;
  }
  operator bool() const noexcept {
    return this->get() != nullptr;
  }
  T& operator*() const noexcept {
    return *this->get();
  }
  T* operator->() const noexcept {
    return this->get();
  }

  size_t use_count() const noexcept {
    if (cb == nullptr)
      return 0;
    return cb->shar();
  }
  void reset() noexcept {
    clear();
  }
  template <typename T1>
  void reset(T1* new_ptr) {
    reset(new_ptr, std::default_delete<T1>());
  }
  template <typename T1, typename D>
  void reset(T1* new_ptr, D del) {
    shared_ptr shar(new_ptr, del);
    swap(shar);
  }

private:
  T* ptr;
  shared_ptr_details::control_block* cb;

  template <typename T1>
  friend class weak_ptr;
  template <typename T1>
  friend class shared_ptr;
  template <typename T1, typename... Args>
  friend shared_ptr<T1> make_shared(Args&&... args);

  explicit shared_ptr(shared_ptr_details::object_block<T>* ptr) noexcept
      : cb(ptr), ptr(ptr->get()) {
    if (cb != nullptr) {
      cb->shar_inc();
    }
  }

  void swap(shared_ptr& other) {
    std::swap(cb, other.cb);
    std::swap(ptr, other.ptr);
  }

  void clear() {
    ptr = nullptr;
    if (cb == nullptr)
      return;
    cb->shar_dec();
    cb = nullptr;
  }
};

template <typename T>
class weak_ptr {
public:
  weak_ptr() noexcept : cb(nullptr), ptr(nullptr) {}
  weak_ptr(const shared_ptr<T>& other) noexcept : cb(other.cb), ptr(other.ptr) {
    if (cb != nullptr) {
      cb->weak_inc();
    }
  }
  weak_ptr(const weak_ptr<T>& other) noexcept : cb(other.cb), ptr(other.ptr) {
    if (cb != nullptr) {
      cb->weak_inc();
    }
  }
  weak_ptr(shared_ptr<T>&& other) noexcept : weak_ptr() {
    swap(other);
  }
  weak_ptr(weak_ptr<T>&& other) noexcept : weak_ptr() {
    swap(other);
  }
  ~weak_ptr() {
    clear();
  }
  weak_ptr& operator=(const shared_ptr<T>& other) noexcept {
    weak_ptr weak(other);
    swap(weak);
    return *this;
  }
  weak_ptr& operator=(const weak_ptr<T>& other) noexcept {
    if (this == &other) {
      return *this;
    }
    weak_ptr weak(other);
    swap(weak);
    return *this;
  }
  weak_ptr& operator=(shared_ptr<T>&& other) noexcept {
    weak_ptr weak(std::move(other));
    swap(weak);
    return *this;
  }
  weak_ptr& operator=(weak_ptr<T>&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    weak_ptr weak(std::move(other));
    swap(weak);
    return *this;
  }

  shared_ptr<T> lock() const noexcept {
    if (cb != nullptr && cb->shar() != 0) {
      return shared_ptr<T>(*this);
    }
    return shared_ptr<T>();
  }

private:
  T* ptr;
  shared_ptr_details::control_block* cb;
  friend class shared_ptr<T>;

  void swap(weak_ptr& other) {
    std::swap(cb, other.cb);
    std::swap(ptr, other.ptr);
  }

  void clear() {
    ptr = nullptr;
    if (cb == nullptr)
      return;
    cb->weak_dec();
    cb = nullptr;
  }
};

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args&&... args) {
  return shared_ptr<T>(new shared_ptr_details::object_block<T>(args...));
}
