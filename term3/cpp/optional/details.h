#pragma once

template <bool isCopiedConstr>
struct copy_constr_base {
  constexpr copy_constr_base() = default;
  constexpr copy_constr_base(const copy_constr_base&) = delete;
  constexpr copy_constr_base(copy_constr_base&&) = default;
  constexpr copy_constr_base& operator=(const copy_constr_base&) = default;
  constexpr copy_constr_base& operator=(copy_constr_base&&) = default;
};

template <>
struct copy_constr_base<true> {
  constexpr copy_constr_base() = default;
  constexpr copy_constr_base(const copy_constr_base&) = default;
  constexpr copy_constr_base(copy_constr_base&&) = default;
  constexpr copy_constr_base& operator=(const copy_constr_base&) = default;
  constexpr copy_constr_base& operator=(copy_constr_base&&) = default;
};

template <bool isMovedConstr>
struct move_constr_base {
  constexpr move_constr_base() = default;
  constexpr move_constr_base(const move_constr_base&) = default;
  constexpr move_constr_base(move_constr_base&&) = delete;
  constexpr move_constr_base& operator=(const move_constr_base&) = default;
  constexpr move_constr_base& operator=(move_constr_base&&) = default;
};

template <>
struct move_constr_base<true> {
  constexpr move_constr_base() = default;
  constexpr move_constr_base(const move_constr_base&) = default;
  constexpr move_constr_base(move_constr_base&&) = default;
  constexpr move_constr_base& operator=(const move_constr_base&) = default;
  constexpr move_constr_base& operator=(move_constr_base&&) = default;
};

template <bool isCopiedAssign>
struct copy_assign_base {
  constexpr copy_assign_base() = default;
  constexpr copy_assign_base(const copy_assign_base&) = default;
  constexpr copy_assign_base(copy_assign_base&&) = default;
  constexpr copy_assign_base& operator=(const copy_assign_base&) = delete;
  constexpr copy_assign_base& operator=(copy_assign_base&&) = default;
};

template <>
struct copy_assign_base<true> {
  constexpr copy_assign_base() = default;
  constexpr copy_assign_base(const copy_assign_base&) = default;
  constexpr copy_assign_base(copy_assign_base&&) = default;
  constexpr copy_assign_base& operator=(const copy_assign_base&) = default;
  constexpr copy_assign_base& operator=(copy_assign_base&&) = default;
};

template <bool isMovedAssign>
struct move_assign_base {
  constexpr move_assign_base() = default;
  constexpr move_assign_base(const move_assign_base&) = default;
  constexpr move_assign_base(move_assign_base&&) = default;
  constexpr move_assign_base& operator=(const move_assign_base&) = default;
  constexpr move_assign_base& operator=(move_assign_base&&) = delete;
};

template <>
struct move_assign_base<true> {
  constexpr move_assign_base() = default;
  constexpr move_assign_base(const move_assign_base&) = default;
  constexpr move_assign_base(move_assign_base&&) = default;
  constexpr move_assign_base& operator=(const move_assign_base&) = default;
  constexpr move_assign_base& operator=(move_assign_base&&) = default;
};
