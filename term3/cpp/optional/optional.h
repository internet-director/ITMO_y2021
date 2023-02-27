#pragma once
#include "details.h"
#include <memory>
#include <new>
#include <utility>
#include <vector>

struct nullopt_t {
  enum class Construct { Token };

  explicit constexpr nullopt_t(Construct) {}
};
inline constexpr nullopt_t nullopt{nullopt_t::Construct::Token};

struct in_place_t {
  enum class Construct { Token };

  explicit constexpr in_place_t(Construct) {}
};

inline constexpr in_place_t in_place{in_place_t::Construct::Token};

template <typename T, bool is = std::is_trivially_destructible_v<T>>
struct optional_base {
  constexpr optional_base() : is_present(false), dummy(0) {}
  constexpr optional_base(nullopt_t) : is_present(false), dummy(0) {}
  constexpr optional_base(const T& x) : is_present(true), data(x) {}
  constexpr optional_base(T&& x) : is_present(true), data(std::move(x)) {}
  template <typename... Args>
  constexpr explicit optional_base(in_place_t, Args&&... args)
      : is_present(true), data(std::forward<Args>(args)...) {}

  ~optional_base() {
    reset();
  }

  constexpr void reset() {
    if (is_present) {
      data.~T();
      is_present = false;
    }
  }

protected:
  bool is_present;
  union {
    char dummy;
    T data;
  };
};

template <typename T>
struct optional_base<T, true> {
  constexpr optional_base() : is_present(false), dummy(0) {}
  constexpr optional_base(nullopt_t) : is_present(false), dummy(0) {}
  constexpr optional_base(const T& x) : is_present(true), data(x) {}
  constexpr optional_base(T&& x) : is_present(true), data(std::move(x)) {}
  template <typename... Args>
  constexpr explicit optional_base(in_place_t, Args&&... args)
      : is_present(true), data(std::forward<Args>(args)...) {}

  constexpr void reset() {
    is_present = false;
  }

protected:
  bool is_present;
  union {
    char dummy;
    T data;
  };
};

template <typename T, bool is = std::is_trivially_copyable_v<T>>
struct copy_move_oper : optional_base<T> {
  using optional_base<T>::optional_base;

  constexpr copy_move_oper() = default;
  constexpr copy_move_oper(const copy_move_oper& other) {
    if (other.is_present) {
      new (&this->data) T(other.data);
      this->is_present = true;
    }
  }
  constexpr copy_move_oper(copy_move_oper&& other) {
    if (other.is_present) {
      new (&this->data) T(std::move(other.data));
      this->is_present = true;
    }
  }
  constexpr copy_move_oper& operator=(const copy_move_oper& other) {
    if (this == &other) {
      return *this;
    }
    if (this->is_present) {
      this->reset();
    }
    if (other.is_present) {
      new (&this->data) T(other.data);
      this->is_present = true;
    }
    return *this;
  }
  constexpr copy_move_oper& operator=(copy_move_oper&& other) {
    if (this == &other) {
      return *this;
    }
    if (this->is_present) {
      this->reset();
    }
    if (other.is_present) {
      new (&this->data) T(std::move(other.data));
      this->is_present = true;
    }
    return *this;
  }
};

template <typename T>
struct copy_move_oper<T, true> : optional_base<T> {
  using optional_base<T>::optional_base;
  constexpr copy_move_oper() = default;
  constexpr copy_move_oper(const copy_move_oper&) = default;
  constexpr copy_move_oper(copy_move_oper&&) = default;
  constexpr copy_move_oper& operator=(const copy_move_oper&) = default;
  constexpr copy_move_oper& operator=(copy_move_oper&&) = default;
};
template <typename T>
struct optional : copy_move_oper<T>,
                  copy_constr_base<std::is_copy_constructible_v<T>>,
                  move_constr_base<std::is_move_constructible_v<T>>,
                  copy_assign_base<std::is_copy_constructible_v<T> &&
                                   std::is_copy_assignable_v<T>>,
                  move_assign_base<std::is_move_assignable_v<T>> {
  using copy_move_oper<T>::copy_move_oper;

public:
  constexpr optional& operator=(nullopt_t) noexcept {
    if (this->is_present) {
      this->reset();
    }
    return *this;
  }

  constexpr explicit operator bool() const noexcept {
    return this->is_present;
  }

  constexpr T& operator*() noexcept {
    return this->data;
  }
  constexpr T const& operator*() const noexcept {
    return this->data;
  }

  constexpr T* operator->() noexcept {
    return &this->data;
  }
  constexpr T const* operator->() const noexcept {
    return &this->data;
  }

  template <typename... Args>
  void emplace(Args&&... args) {
    this->reset();
    new (&this->data) T(std::forward<Args>(args)...);
    this->is_present = true;
  }
};

template <typename T>
constexpr bool operator==(optional<T> const& a, optional<T> const& b) {
  if (!static_cast<bool>(a) && !static_cast<bool>(b)) {
    return true;
  }
  if (!static_cast<bool>(a) || !static_cast<bool>(b)) {
    return false;
  }
  return *a == *b;
}

template <typename T>
constexpr bool operator!=(optional<T> const& a, optional<T> const& b) {
  return !(a == b);
}

template <typename T>
constexpr bool operator<(optional<T> const& a, optional<T> const& b) {
  if (!static_cast<bool>(a) && !static_cast<bool>(b)) {
    return false;
  }
  if (!static_cast<bool>(a)) {
    return true;
  }
  if (!static_cast<bool>(b)) {
    return false;
  }
  return *a < *b;
}

template <typename T>
constexpr bool operator<=(optional<T> const& a, optional<T> const& b) {
  return !(a > b);
}

template <typename T>
constexpr bool operator>(optional<T> const& a, optional<T> const& b) {
  if (!static_cast<bool>(a) && !static_cast<bool>(b)) {
    return false;
  }
  if (!static_cast<bool>(a)) {
    return false;
  }
  if (!static_cast<bool>(b)) {
    return true;
  }
  return *a > *b;
}

template <typename T>
constexpr bool operator>=(optional<T> const& a, optional<T> const& b) {
  return !(a < b);
}
