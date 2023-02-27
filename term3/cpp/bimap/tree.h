#pragma once

#include <cstddef>
#include <cstring>
#include <memory>

template <typename Left, typename Right, typename CompareLeft,
          typename CompareRight>
class bimap;

namespace bimap_details {
struct left_t;
struct right_t;

struct node_base {
  node_base* leftmost() noexcept {
    node_base* root = this;
    while (root->l != nullptr)
      root = root->l;
    return root;
  }
  node_base* rightmost() noexcept {
    node_base* root = this;
    while (root->r != nullptr)
      root = root->r;
    return root;
  }

  node_base* next() noexcept {
    node_base* root = this;
    if (root->r) {
      root = root->r;
      root = root->leftmost();
    } else if (root->is_left()) {
      root = root->p;
    } else if (root->is_right()) {
      while (root->is_right()) {
        root = root->p;
      }
      root = root->p;
    }
    return root;
  }

  node_base* prev() noexcept {
    node_base* root = this;
    if (root->l) {
      root = root->l;
      root = root->rightmost();
    } else if (root->is_right()) {
      root = root->p;
    } else {
      while (root->is_left()) {
        root = root->p;
      }
      root = root->p;
    }
    return root;
  }

  bool is_left() const noexcept {
    if (p == nullptr) {
      return false;
    }
    return p->l == this;
  }
  bool is_right() const noexcept {
    if (p == nullptr) {
      return false;
    }
    return p->r == this;
  }

  node_base* l = nullptr;
  node_base* r = nullptr;
  node_base* p = nullptr;
};

template <typename T, typename Tag>
struct node_base_value : node_base {
  explicit node_base_value(const T& val1) : val1(val1) {}
  explicit node_base_value(T&& val1) : val1(std::move(val1)) {}

  T val1;
};

template <typename T1, typename T2>
struct node : node_base_value<T1, left_t>, node_base_value<T2, right_t> {
  template <typename _T1, typename _T2>
  explicit node(_T1&& val1, _T2&& val2)
      : node_base_value<T1, left_t>(std::forward<_T1>(val1)),
        node_base_value<T2, right_t>(std::forward<_T2>(val2)) {}
};

template <typename Type, typename Compare, typename Tag>
class map : public Compare {
  template <typename Left, typename Right, typename CompareLeft,
            typename CompareRight>
  friend class ::bimap;
  using Node = node_base;

  // так как мы облегчили итератор и выкинули из него указатель на бимап, то
  // доступа ко всему дереву мы не имеем так что перенесем в него код некст/прев
  // ноды. А чтобы была возможность уходить из end-а, добавим пустую вершину,
  // которая всегда будет самой правой нодой дерева. Ну и чтобы это получилось,
  // перенесем value из нод в отдельную структуру дабы не вызывать лишний раз
  // конструкторы (которых может и не быть)

  // ну и так как выкинули root, переместим корень дерева на end_node.l,
  // а указатель на end_node в соседнем дереве на end_node.r
  Node end_node;

public:
  map() = default;
  map(const Compare& cmp) : Compare(cmp) {}
  map(Compare&& cmp) : Compare(std::move(cmp)) {}

  Node* begin() const noexcept {
    return const_cast<Node*>(&end_node)->leftmost();
  }
  Node* end() const noexcept {
    return const_cast<Node*>(&end_node);
  }
  Node* insert(Node* new_node) {
    if (end_node.l == nullptr) {
      end_node.l = new_node;
      linkEndNode();
      return end_node.l;
    }
    Node* par = end_node.l;
    new_node->l = new_node->r = new_node->p = nullptr;

    while (true) {
      int cmp = compare(par, new_node);
      if (cmp == 0) {
        break;
      }
      if (cmp < 0) {
        if (par->l == nullptr) {
          par->l = new_node;
          new_node->p = par;
          par = par->l;
          break;
        } else {
          par = par->l;
        }
      } else {
        if (par->r == nullptr) {
          par->r = new_node;
          new_node->p = par;
          par = par->r;
          break;
        } else {
          par = par->r;
        }
      }
    }
    return splay(par);
  }
  void erase(Node* del_node) noexcept {
    if (splay(del_node) == nullptr) {
      return;
    }
    // чтобы дерево не поломалось, будем отвязывать parent корня от end_node
    unlinkEndNode();
    Node* par = del_node->l;
    if (par == nullptr) {
      end_node.l = del_node->r;
    } else {
      par = par->rightmost();
      if (del_node->r != nullptr) {
        par->r = del_node->r;
        del_node->r->p = par;
      }
      end_node.l = del_node->l;
    }
    if (end_node.l != nullptr) {
      end_node.l->p = &end_node;
    }
  }
  Node* find(const Type& val) const {
    return exists(end_node.l, val);
  }
  Node* lower(const Type& val) const {
    return lower(end_node.l, val);
  }
  Node* upper(const Type& val) const {
    return upper(end_node.l, val);
  }
  bool equal(const Type& val1, const Type& val2) const {
    return compare(val1, val2) == 0;
  }
  void swap(map& other) {
    std::swap(end_node.l, other.end_node.l);
    if (end_node.l != nullptr) {
      end_node.l->p = &end_node;
    }
    if (other.end_node.l != nullptr) {
      other.end_node.l->p = &other.end_node;
    }
  }

private:
  Compare& getComparator() noexcept {
    return static_cast<Compare&>(*this);
  }
  Compare const& getComparator() const noexcept {
    return static_cast<Compare const&>(*this);
  }
  int compare(const Type& l, const Type& r) const {
    if (getComparator()(l, r))
      return 1;
    if (!getComparator()(r, l))
      return 0;
    return -1;
  }
  int compare(Node* l, Node* r) const {
    return compare(cast(l)->val1, cast(r)->val1);
  }
  bool larger(const Type& l, const Type& r) const {
    return getComparator()(l, r);
  }
  bool larger(Node* l, Node* r) const {
    return larger(cast(l)->val1, cast(r)->val1);
  }
  bool less(const Type& l, const Type& r) const {
    return getComparator()(r, l);
  }
  bool less(Node* l, Node* r) const {
    return less(cast(l)->val1, cast(r)->val1);
  }
  void Rturn(Node* p) noexcept {
    Node* R = p->l;
    Node* L = R->r;
    Node* par = p->p;

    if (par != nullptr) {
      (par->r != p) ? par->l = R : par->r = R;
    }
    if (L != nullptr) {
      L->p = p;
    }

    R->p = par;
    R->r = p;
    p->p = R;
    p->l = L;
  }
  void Lturn(Node* p) noexcept {
    Node* R = p->r;
    Node* L = R->l;
    Node* par = p->p;

    if (par != nullptr) {
      (par->r != p) ? par->l = R : par->r = R;
    }
    if (L != nullptr) {
      L->p = p;
    }

    R->p = par;
    R->l = p;
    p->p = R;
    p->r = L;
  }
  Node* splay(Node* n) noexcept {
    unlinkEndNode();
    if (n == nullptr) {
      linkEndNode();
      return nullptr;
    }
    while (true) {
      Node* orig_par = n->p;
      if (orig_par == nullptr)
        break;
      Node* par = orig_par->p;
      if (par == nullptr) {
        (orig_par->l != n) ? Lturn(orig_par) : Rturn(orig_par);
        break;
      }
      if (par->l != orig_par) {
        if (orig_par->l != n) {
          Lturn(par);
          Lturn(orig_par);
        } else {
          Rturn(orig_par);
          Lturn(par);
        }
      } else {
        if (orig_par->l != n) {
          Lturn(orig_par);
          Rturn(par);
        } else {
          Rturn(par);
          Rturn(orig_par);
        }
      }
    }
    end_node.l = n;
    end_node.l->p = &end_node;
    return end_node.l;
  }
  Node* exists(Node* p, const Type& x) const {
    if (p == nullptr || p == end())
      return end();
    if (less(cast(p)->val1, x)) {
      return exists(p->l, x);
    } else if (larger(cast(p)->val1, x)) {
      return exists(p->r, x);
    }
    return p;
  }
  Node* lower(Node* p, const Type& val) const {
    if (p == nullptr || p == end())
      return end();
    if (larger(cast(p)->val1, val)) {
      return lower(p->r, val);
    }
    Node* n = lower(p->l, val);
    if (n != end())
      return n;
    return p;
  }
  Node* upper(Node* p, const Type& val) const {
    if (p == nullptr || p == end())
      return end();
    if (less(cast(p)->val1, val)) {
      return lower(p->l, val);
    }
    Node* n = lower(p->r, val);
    if (n != end())
      return n;
    return p;
  }

  // непосредственно конвертация ноды, дабы иметь доступ к данным
  constexpr node_base_value<Type, Tag>* cast(Node* node) const noexcept {
    return static_cast<node_base_value<Type, Tag>*>(node);
  }

  void linkEndNode() noexcept {
    if (end_node.l == nullptr) {
      return;
    }
    end_node.l->p = &end_node;
  }
  void unlinkEndNode() noexcept {
    if (end_node.l == nullptr) {
      return;
    }
    end_node.l->p = nullptr;
  }
};
} // namespace bimap_details
