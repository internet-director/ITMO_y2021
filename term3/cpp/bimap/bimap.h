#pragma once

#include "tree.h"
#include <iostream>

template <typename Left, typename Right, typename CompareLeft = std::less<Left>,
          typename CompareRight = std::less<Right>>
class bimap {
  using node_base = bimap_details::node_base;
  using node_l = bimap_details::node_base_value<Left, bimap_details::left_t>;
  using node_r = bimap_details::node_base_value<Right, bimap_details::right_t>;
  using map_l = bimap_details::map<Left, CompareLeft, bimap_details::left_t>;
  using map_r = bimap_details::map<Right, CompareRight, bimap_details::right_t>;

  map_l l_map;
  map_r r_map;
  std::size_t _size = 0;

  template <typename NodeType1,
            typename NodeType2 = typename std::conditional_t<
                (std::is_same_v<node_l, NodeType1>), node_r, node_l>>
  class template_iterator {
    node_base* node;
    friend class bimap;
    using it_type =
        typename std::conditional_t<(std::is_same_v<node_l, NodeType1>), Left,
                                    Right>;
    using node_v =
        typename std::conditional_t<(std::is_same_v<node_l, NodeType1>), node_l,
                                    node_r>;
    using flip_iterator = template_iterator<NodeType2>;

    explicit template_iterator(node_base* node) : node(node) {}

  public:
    // Элемент на который сейчас ссылается итератор.
    // Разыменование итератора end_left() неопределено.
    // Разыменование невалидного итератора неопределено.
    it_type const& operator*() const noexcept {
      return static_cast<node_v*>(node)->val1;
    }
    it_type const* operator->() const noexcept {
      return &static_cast<node_v*>(node)->val1;
    }

    // Переход к следующему по величине left'у.
    // Инкремент итератора end_left() неопределен.
    // Инкремент невалидного итератора неопределен.
    template_iterator& operator++() noexcept {
      node = node->next();
      return *this;
    }
    template_iterator operator++(int) noexcept {
      auto old_it = *this;
      ++(*this);
      return old_it;
    }

    // Переход к предыдущему по величине left'у.
    // Декремент итератора begin_left() неопределен.
    // Декремент невалидного итератора неопределен.
    template_iterator& operator--() noexcept {
      node = node->prev();
      return *this;
    }

    template_iterator operator--(int) noexcept {
      auto old_it = *this;
      --(*this);
      return old_it;
    }

    // left_iterator ссылается на левый элемент некоторой пары.
    // Эта функция возвращает итератор на правый элемент той же пары.
    // end_left().flip() возращает end_right().
    // end_right().flip() возвращает end_left().
    // flip() невалидного итератора неопределен.
    flip_iterator flip() const noexcept {
      if (node->p == nullptr) {
        return flip_iterator(node->r);
      }
      return flip_iterator(static_cast<NodeType2*>(
          static_cast<node_t*>(static_cast<NodeType1*>(node))));
    }

    friend bool operator==(const template_iterator& a,
                           const template_iterator& b) {
      return a.node == b.node;
    }
    friend bool operator!=(const template_iterator& a,
                           const template_iterator& b) {
      return a.node != b.node;
    }
  };

public:
  using left_t = Left;
  using right_t = Right;
  using left_iterator = template_iterator<node_l>;
  using right_iterator = template_iterator<node_r>;
  using node_t = bimap_details::node<left_t, right_t>;

  // Создает bimap не содержащий ни одной пары.
  bimap(CompareLeft compare_left = CompareLeft(),
        CompareRight compare_right = CompareRight())
      : l_map(std::move(compare_left)), r_map(std::move(compare_right)) {
    l_map.end_node.r = &r_map.end_node;
    r_map.end_node.r = &l_map.end_node;
  }

  // Конструкторы от других и присваивания
  bimap(bimap const& other)
      : l_map(other.l_map.getComparator()), r_map(other.r_map.getComparator()) {
    l_map.end_node.r = &r_map.end_node;
    r_map.end_node.r = &l_map.end_node;
    try {
      for (auto i = other.begin_left(); i != other.end_left(); i++) {
        insert(*i, *i.flip());
      }
    } catch (...) {
      erase_left(begin_left(), end_left());
      throw;
    }
  }
  bimap(bimap&& other) noexcept {
    l_map.end_node.r = &r_map.end_node;
    r_map.end_node.r = &l_map.end_node;
    swap(other);
  }

  bimap& operator=(bimap const& other) {
    if (this == &other) {
      return *this;
    }
    bimap map(other);
    swap(map);
    return *this;
  }
  bimap& operator=(bimap&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    bimap map(std::move(other));
    swap(map);
    return *this;
  }

  // Деструктор. Вызывается при удалении объектов bimap.
  // Инвалидирует все итераторы ссылающиеся на элементы этого bimap
  // (включая итераторы ссылающиеся на элементы следующие за последними).
  ~bimap() {
    erase_left(begin_left(), end_left());
  }

  // Вставка пары (left, right), возвращает итератор на left.
  // Если такой left или такой right уже присутствуют в bimap, вставка не
  // производится и возвращается end_left().
  left_iterator insert(left_t const& left, right_t const& right) {
    return map_insert(left, right);
  }
  left_iterator insert(left_t const& left, right_t&& right) {
    return map_insert(left, std::move(right));
  }
  left_iterator insert(left_t&& left, right_t const& right) {
    return map_insert(std::move(left), right);
  }
  left_iterator insert(left_t&& left, right_t&& right) {
    return map_insert(std::move(left), std::move(right));
  }

  // Удаляет элемент и соответствующий ему парный.
  // erase невалидного итератора неопределен.
  // erase(end_left()) и erase(end_right()) неопределены.
  // Пусть it ссылается на некоторый элемент e.
  // erase инвалидирует все итераторы ссылающиеся на e и на элемент парный к e.
  left_iterator erase_left(left_iterator it) {
    return map_erase(it);
  }
  // Аналогично erase, но по ключу, удаляет элемент если он присутствует, иначе
  // не делает ничего Возвращает была ли пара удалена
  bool erase_left(left_t const& left) {
    left_iterator it = find_left(left);
    if (it == end_left())
      return false;
    erase_left(it);
    return true;
  }

  right_iterator erase_right(right_iterator it) {
    return map_erase(it);
  }
  bool erase_right(right_t const& right) {
    right_iterator it = find_right(right);
    if (it == end_right())
      return false;
    erase_right(it);
    return true;
  }

  // erase от ренжа, удаляет [first, last), возвращает итератор на последний
  // элемент за удаленной последовательностью
  left_iterator erase_left(left_iterator first, left_iterator last) {
    return map_erase(first, last);
  }
  right_iterator erase_right(right_iterator first, right_iterator last) {
    return map_erase(first, last);
  }

  // Возвращает итератор по элементу. Если не найден - соответствующий
  // rightmost()
  left_iterator find_left(left_t const& left) const noexcept {
    return left_iterator(l_map.find(left));
  }
  right_iterator find_right(right_t const& right) const noexcept {
    return right_iterator(r_map.find(right));
  }

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует -- бросает std::out_of_range
  right_t const& at_left(left_t const& key) const {
    right_iterator node = find_left(key).flip();
    if (node == end_right()) {
      throw std::out_of_range("Out of range");
    }
    return *node;
  }
  left_t const& at_right(right_t const& key) const {
    left_iterator node = find_right(key).flip();
    if (node == end_left()) {
      throw std::out_of_range("Out of range");
    }
    return *node;
  }

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует, добавляет его в bimap и на противоположную
  // сторону кладет дефолтный элемент, ссылку на который и возвращает
  // Если дефолтный элемент уже лежит в противоположной паре - должен поменять
  // соответствующий ему элемент на запрашиваемый (смотри тесты)
  template <typename = std::enable_if<std::is_default_constructible_v<right_t>>>
  right_t const& at_left_or_default(left_t const& key) {
    right_iterator node = find_left(key).flip();
    if (node != end_right()) {
      return *node;
    }

    right_t right = right_t();
    erase_right(right);
    return *insert(key, std::move(right)).flip();
  }

  template <typename = std::enable_if<std::is_default_constructible_v<left_t>>>
  left_t const& at_right_or_default(right_t const& key) {
    left_iterator node = find_right(key).flip();
    if (node != end_left()) {
      return *node;
    }

    left_t left = left_t();
    erase_left(left);
    return *insert(std::move(left), key);
  }

  // lower и upper bound'ы по каждой стороне
  // Возвращают итераторы на соответствующие элементы
  // Смотри std::lower_bound, std::upper_bound.
  left_iterator lower_bound_left(const left_t& left) const {
    return left_iterator(l_map.lower(left));
  }
  left_iterator upper_bound_left(const left_t& left) const {
    return left_iterator(l_map.lower(left));
  }

  right_iterator lower_bound_right(const right_t& left) const {
    return right_iterator(r_map.lower(left));
  }
  right_iterator upper_bound_right(const right_t& left) const {
    return right_iterator(r_map.lower(left));
  }

  // Возващает итератор на минимальный по порядку left.
  left_iterator begin_left() const noexcept {
    return left_iterator(l_map.begin());
  }
  // Возващает итератор на следующий за последним по порядку left.
  left_iterator end_left() const noexcept {
    return left_iterator(l_map.end());
  }

  // Возващает итератор на минимальный по порядку right.
  right_iterator begin_right() const noexcept {
    return right_iterator(r_map.begin());
  }
  // Возващает итератор на следующий за последним по порядку right.
  right_iterator end_right() const noexcept {
    return right_iterator(r_map.end());
  }

  // Проверка на пустоту
  bool empty() const {
    return size() == 0;
  }

  // Возвращает размер бимапы (кол-во пар)
  std::size_t size() const {
    return _size;
  }

  // операторы сравнения
  friend bool operator==(bimap const& a, bimap const& b) {
    if (a.size() != b.size()) {
      return false;
    }

    auto ait = a.begin_left();
    auto bit = b.begin_left();

    for (; ait != a.end_left(); ait++, bit++) {
      if (!a.l_map.equal(*ait, *bit) ||
          !b.r_map.equal(*ait.flip(), *bit.flip())) {
        return false;
      }
    }
    return true;
  }

  friend bool operator!=(bimap const& a, bimap const& b) {
    return !(a == b);
  }

  void swap(bimap& map) {
    l_map.swap(map.l_map);
    r_map.swap(map.r_map);
    std::swap(_size, map._size);
  }

private:
  template <typename T1, typename T2>
  left_iterator map_insert(T1&& left, T2&& right) {
    if (find_left(left) == end_left() && find_right(right) == end_right()) {
      node_t* new_node =
          new node_t(std::forward<T1>(left), std::forward<T2>(right));
      l_map.insert(static_cast<node_l*>(new_node));
      r_map.insert(static_cast<node_r*>(new_node));
      _size++;
      return left_iterator(static_cast<node_l*>(new_node));
    }
    return end_left();
  }

  template <typename T>
  T map_erase(T it) {
    T ret(it.node);
    ret++;
    node_t* node;
    if constexpr (std::is_same_v<T, left_iterator>) {
      node = static_cast<node_t*>(static_cast<node_l*>(it.node));
    } else {
      node = static_cast<node_t*>(static_cast<node_r*>(it.node));
    }
    l_map.erase(static_cast<node_l*>(node));
    r_map.erase(static_cast<node_r*>(node));
    _size--;
    delete node;
    return ret;
  }

  template <typename T>
  T map_erase(T left, T right) {
    for (; left != right;) {
      left = map_erase(left);
    }
    return left;
  }
};
