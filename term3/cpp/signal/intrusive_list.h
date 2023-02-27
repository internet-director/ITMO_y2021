#pragma once
#include <iostream>

namespace intrusive {
struct default_tag;

struct list_element_base {
  list_element_base *l, *r;
  list_element_base() : l(this), r(this) {}
};

template <typename Tag = default_tag>
struct list_element : list_element_base {
  list_element() = default;
  list_element(const list_element& l) = delete;
  list_element& operator=(const list_element& l) = delete;
  void unlink() {
    l->r = r;
    r->l = l;
    l = r = this;
  }
};

template <typename T, typename Tag = default_tag>
struct iterator_base {
  using iterator_category = std::bidirectional_iterator_tag;
  using value_type = T;
  using difference_type = std::ptrdiff_t;
  using pointer = T*;
  using reference = T&;
  list_element_base* node;

  explicit iterator_base(list_element_base* node = nullptr) : node(node) {}
  template <typename T1>
  iterator_base(const iterator_base<T1, Tag>& base) : node(base.node) {}
  template <typename T1>
  bool operator==(const iterator_base<T1, Tag>& it) const noexcept {
    return it.node == node;
  }
  template <typename T1>
  bool operator!=(const iterator_base<T1, Tag>& it) const noexcept {
    return it.node != node;
  }
  T* operator->() const noexcept {
    return static_cast<T*>(static_cast<list_element<Tag>*>(node));
  }
  T& operator*() const noexcept {
    return *static_cast<T*>(static_cast<list_element<Tag>*>(node));
  }
  template <typename T1>
  iterator_base& operator=(const iterator_base<T1, Tag>& it) noexcept {
    this->node = it.node;
    return *this;
  }

  iterator_base& operator++() noexcept {
    node = node->r;
    return *this;
  }
  iterator_base& operator--() noexcept {
    node = node->l;
    return *this;
  }
  iterator_base operator++(int) noexcept {
    iterator_base it = *this;
    node = node->r;
    return it;
  }
  iterator_base operator--(int) noexcept {
    iterator_base it = *this;
    node = node->l;
    return it;
  }
};

template <typename T, typename Tag = default_tag>
struct list {
  using Node = list_element<Tag>*;
  using iterator = iterator_base<T, Tag>;
  using const_iterator = iterator_base<const T, Tag>;

  list() = default;
  list(const list& l) = delete;
  list(list&& l) {
    splice(end(), l, l.begin(), l.end());
  }
  void push_back(T& node) noexcept {
    insert(end(), node);
  }
  void push_front(T& node) noexcept {
    insert(begin(), node);
  }
  void pop_back() noexcept {
    erase(--end());
  }
  void pop_front() noexcept {
    erase(begin());
  }
  list<T, Tag>& operator=(const list& l) = delete;
  list<T, Tag>& operator=(list&& l) noexcept {
    if (this == &l) {
      return *this;
    }
    clear();
    splice(end(), l, l.begin(), l.end());
    return *this;
  }
  T& back() noexcept {
    return *--end();
  }
  T& front() noexcept {
    return *begin();
  }
  const T& back() const noexcept {
    return *--end();
  }
  const T& front() const noexcept {
    return *begin();
  }
  iterator end() noexcept {
    return iterator(&elem);
  }
  iterator begin() noexcept {
    return iterator(elem.r);
  }
  const_iterator end() const noexcept {
    return const_iterator(const_cast<list_element<Tag>*>(&elem));
  }
  const_iterator begin() const noexcept {
    return const_iterator(elem.r);
  }
  bool empty() const {
    return elem.l == &elem && elem.r == &elem;
  }

  iterator insert(iterator it, T& node) noexcept {
    Node new_node = static_cast<Node>(&node);
    iterator ret(new_node);
    if (new_node == it.node) {
      return ret;
    }
    new_node->unlink();
    new_node->l = it.node->l;
    new_node->r = it.node;
    new_node->l->r = new_node;
    new_node->r->l = new_node;
    return ret;
  }

  iterator erase(iterator it) noexcept {
    iterator ret(it.node->r);
    static_cast<list_element<Tag>*>(it.node)->unlink();
    return ret;
  }

  void clear() noexcept {
    while (!empty()) {
      pop_back();
    }
  }

  void splice(const_iterator pos, list&, const_iterator first, const_iterator end) noexcept {
    if (pos == first || first == end) {
      return;
    }

    auto* last = end.node->l;
    first.node->l->r = last->r;
    last->r->l = first.node->l;
    pos.node->l->r = first.node;
    first.node->l = pos.node->l;
    pos.node->l = last;
    last->r = pos.node;
  }

private:
  list_element<Tag> elem;
};
} // namespace intrusive
