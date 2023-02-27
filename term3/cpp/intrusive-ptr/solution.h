#include <atomic>
#include <iostream>

template <typename T>
struct intrusive_ref_counter {
  intrusive_ref_counter() noexcept = default;
  intrusive_ref_counter(const intrusive_ref_counter& v) noexcept {}

  intrusive_ref_counter& operator=(const intrusive_ref_counter& v) noexcept {
    return *this;
  }

  unsigned int use_count() const noexcept {
    return counter.load(std::memory_order_relaxed);
  }

  template <class Derived>
  friend void
  intrusive_ptr_add_ref(const intrusive_ref_counter<Derived>* p) noexcept;
  template <class Derived>
  friend void
  intrusive_ptr_release(const intrusive_ref_counter<Derived>* p) noexcept;

protected:
  ~intrusive_ref_counter() = default;

private:
  mutable std::atomic<unsigned int> counter = 0;
};

template <class Derived>
void intrusive_ptr_add_ref(const intrusive_ref_counter<Derived>* p) noexcept {
  p->counter.fetch_add(1, std::memory_order_relaxed);
}

template <class Derived>
void intrusive_ptr_release(const intrusive_ref_counter<Derived>* p) noexcept {
  if (p->counter.fetch_sub(1, std::memory_order_acq_rel) == 1)
    delete static_cast<const Derived*>(p);
}

template <typename T>
struct intrusive_ptr {
  using element_type = T;

  intrusive_ptr() noexcept = default;
  intrusive_ptr(T* p, bool add_ref = true) : ptr(p) {
    if (ptr != nullptr && add_ref) {
      intrusive_ptr_add_ref(ptr);
    }
  }

  intrusive_ptr(intrusive_ptr const& r) : ptr(r.get()) {
    if (ptr != nullptr) {
      intrusive_ptr_add_ref(ptr);
    }
  }
  template <class Y>
  intrusive_ptr(intrusive_ptr<Y> const& r) requires std::derived_from<Y, T>
      : ptr(static_cast<T*>(r.get())) {
    if (ptr != nullptr) {
      intrusive_ptr_add_ref(ptr);
    }
  }

  intrusive_ptr(intrusive_ptr&& r) : ptr(r.get()) {
    r.ptr = nullptr;
  }
  template <class Y>
  intrusive_ptr(intrusive_ptr<Y>&& r) requires std::derived_from<Y, T>
      : ptr(static_cast<T*>(r.get())) {
    r.ptr = nullptr;
  }

  ~intrusive_ptr() {
    if (ptr != nullptr) {
      intrusive_ptr_release(ptr);
    }
  }

  intrusive_ptr& operator=(intrusive_ptr const& r) {
    if (&r != this) {
      intrusive_ptr tmp(r);
      tmp.swap(*this);
    }
    return *this;
  }
  template <class Y>
  intrusive_ptr&
  operator=(intrusive_ptr<Y> const& r) requires std::derived_from<Y, T> {
    intrusive_ptr tmp(r);
    tmp.swap(*this);
    return *this;
  }
  intrusive_ptr& operator=(T* r) {
    if (&r != this) {
      intrusive_ptr tmp(r);
      tmp.swap(*this);
    }
    return *this;
  }

  intrusive_ptr& operator=(intrusive_ptr&& r) {
    if (&r != this) {
      intrusive_ptr tmp(std::move(r));
      tmp.swap(*this);
    }
    return *this;
  }
  template <class Y>
  intrusive_ptr&
  operator=(intrusive_ptr<Y>&& r) requires std::derived_from<Y, T> {
    intrusive_ptr tmp(std::move(r));
    tmp.swap(*this);
    return *this;
  }

  void reset() {
    intrusive_ptr tmp;
    tmp.swap(*this);
  }
  void reset(T* r) {
    reset(r, true);
  }
  void reset(T* r, bool add_ref) {
    intrusive_ptr tmp(r, add_ref);
    tmp.swap(*this);
  }

  T& operator*() const noexcept {
    return *ptr;
  }
  T* operator->() const noexcept {
    return ptr;
  }
  T* get() const noexcept {
    return ptr;
  }
  T* detach() noexcept {
    auto* p = ptr;
    ptr = nullptr;
    return p;
  }

  explicit operator bool() const noexcept {
    return ptr != nullptr;
  }

  void swap(intrusive_ptr& b) noexcept {
    std::swap(this->ptr, b.ptr);
  }

private:
  T* ptr = nullptr;

  template <typename Y>
  friend struct intrusive_ptr;
};

template <class T, class U>
bool operator==(intrusive_ptr<T> const& a, intrusive_ptr<U> const& b) noexcept {
  return a.get() == b.get();
}

template <class T, class U>
bool operator!=(intrusive_ptr<T> const& a, intrusive_ptr<U> const& b) noexcept {
  return a.get() != b.get();
}

template <class T, class U>
bool operator==(intrusive_ptr<T> const& a, U* b) noexcept {
  return a.get() == b;
}

template <class T, class U>
bool operator!=(intrusive_ptr<T> const& a, U* b) noexcept {
  return a.get() != b;
}

template <class T, class U>
bool operator==(T* a, intrusive_ptr<U> const& b) noexcept {
  return a == b.get();
}

template <class T, class U>
bool operator!=(T* a, intrusive_ptr<U> const& b) noexcept {
  return a != b.get();
}

template <class T>
bool operator<(intrusive_ptr<T> const& a, intrusive_ptr<T> const& b) noexcept {
  return a.get() != b.get();
}

template <class T>
void swap(intrusive_ptr<T>& a, intrusive_ptr<T>& b) noexcept {
  a.swap(b);
}
