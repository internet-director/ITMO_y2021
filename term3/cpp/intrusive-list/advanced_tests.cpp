#include "intrusive_list.h"
#include "test_utils.h"

TEST(advanced_intrusive_list_testing, iterators_01) {
  intrusive::list<node> list;
  auto it1 = list.begin();
  auto it2 = list.end();

  node a(1);
  list.push_back(a);

  auto it3 = list.begin();
  auto it4 = list.end();
  EXPECT_TRUE(it1 == it4);
  EXPECT_TRUE(it2 == it4);
  EXPECT_TRUE(it3 != it4);

  --it1;

  EXPECT_TRUE(it1 == it3);
}

TEST(advanced_intrusive_list_testing, iterators_02) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);

  list.push_back(b);
  list.push_back(c);
  list.push_front(a);

  auto i = list.begin();
  EXPECT_TRUE(i != list.end());
  EXPECT_EQ(1, i->value);
  ++i;
  EXPECT_TRUE(i != list.end());
  EXPECT_EQ(2, i->value);
  ++i;
  EXPECT_TRUE(i != list.end());
  EXPECT_EQ(3, i->value);
  ++i;
  EXPECT_TRUE(i == list.end());
}

TEST(advanced_intrusive_list_testing, iterators_03) {
  intrusive::list<node> list;
  node a(1);

  list.push_back(a);

  auto it1 = list.end();
  auto it2 = --it1;
  EXPECT_EQ(1, it2->value);
}

TEST(advanced_intrusive_list_testing, iterators_04) {
  intrusive::list<node> list;
  node a(1), b(2);

  list.push_back(a);
  list.push_back(b);

  auto it1 = list.begin();
  auto it2 = ++it1;
  EXPECT_EQ(2, it2->value);
}

TEST(advanced_intrusive_list_testing, iterators_05) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);

  list.push_back(b);
  list.push_back(c);
  list.push_front(a);

  auto i = list.end();
  EXPECT_TRUE(i != list.begin());
  --i;
  EXPECT_EQ(3, i->value);
  EXPECT_TRUE(i != list.begin());
  --i;
  EXPECT_EQ(2, i->value);
  EXPECT_TRUE(i != list.begin());
  --i;
  EXPECT_EQ(1, i->value);
  EXPECT_TRUE(i == list.begin());
}

TEST(advanced_intrusive_list_testing, iterators_06) {
  intrusive::list<node> list;
  node a(1), b(2);

  list.push_back(a);
  list.push_back(b);

  auto it1 = list.begin();
  auto it2 = it1++;
  EXPECT_EQ(2, it1->value);
  EXPECT_EQ(1, it2->value);
}

TEST(advanced_intrusive_list_testing, iterators_07) {
  intrusive::list<node> list;
  node a(1), b(2);

  list.push_back(a);
  list.push_back(b);

  auto it1 = list.end();
  --it1;
  auto it2 = it1--;
  EXPECT_EQ(1, it1->value);
  EXPECT_EQ(2, it2->value);
}

TEST(advanced_intrusive_list_testing, iterators_08) {
  intrusive::list<node> list;
  node a(1);

  list.push_back(a);

  intrusive::list<node>::iterator it = list.begin();
  intrusive::list<node>::const_iterator it2 = it;

  EXPECT_EQ(1, it2->value);
}

TEST(intrusive_list_testing, iterators_09) {
  intrusive::list<node> list;
  EXPECT_EQ(list.begin(), std::as_const(list).begin());
}

TEST(advanced_intrusive_list_testing, ends_01) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);
  list.push_back(a);
  list.push_back(b);
  list.push_back(c);
  list.push_back(d);

  EXPECT_EQ(1, list.front().value);
  EXPECT_EQ(4, list.back().value);
  list.pop_front();
  EXPECT_EQ(2, list.front().value);
  EXPECT_EQ(4, list.back().value);
  list.pop_back();
  EXPECT_EQ(2, list.front().value);
  EXPECT_EQ(3, list.back().value);
  list.pop_front();
  EXPECT_EQ(3, list.front().value);
  EXPECT_EQ(3, list.back().value);
}

TEST(advanced_intrusive_list_testing, ends_02) {
  node a(1);
  {
    intrusive::list<node> list;
    list.push_back(a);
    EXPECT_EQ(1, list.back().value);
    EXPECT_EQ(1, list.front().value);
    EXPECT_EQ(1, std::as_const(list).back().value);
    EXPECT_EQ(1, std::as_const(list).front().value);
    EXPECT_FALSE(list.empty());
    list.pop_back();
    EXPECT_TRUE(list.empty());
  }
}

TEST(advanced_intrusive_list_testing, ends_03) {
  node a(1);
  {
    intrusive::list<node> list;
    list.push_front(a);
    EXPECT_EQ(1, list.back().value);
    EXPECT_EQ(1, list.front().value);
    EXPECT_EQ(1, std::as_const(list).back().value);
    EXPECT_EQ(1, std::as_const(list).front().value);
    EXPECT_FALSE(list.empty());
    list.pop_front();
    EXPECT_TRUE(list.empty());
  }
}

TEST(advanced_intrusive_list_testing, insert_01) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);

  list.push_back(b);
  list.push_back(c);
  list.push_back(d);

  auto it1 = list.begin();
  auto it2 = list.insert(it1, a);
  EXPECT_EQ(1, it2->value);
  EXPECT_EQ(2, it1->value);
  EXPECT_FALSE(it1 == it2);
  EXPECT_TRUE(it2 == list.begin());
  ++it2;
  EXPECT_TRUE(it1 == it2);
  --it1;
  EXPECT_EQ(1, it1->value);
}

TEST(advanced_intrusive_list_testing, insert_02) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);

  list.push_back(a);
  list.push_back(b);
  list.push_back(d);

  auto it1 = list.begin();
  ++it1;
  ++it1;
  EXPECT_EQ(4, it1->value);
  auto it2 = list.insert(it1, c);
  EXPECT_EQ(3, it2->value);
  --it2;
  EXPECT_EQ(2, it2->value);
  ++it2;
  EXPECT_EQ(3, it2->value);
  ++it2;
  EXPECT_EQ(4, it2->value);
  EXPECT_TRUE(it1 == it2);
  --it1;
  EXPECT_EQ(3, it1->value);
}

TEST(advanced_intrusive_list_testing, insert_03) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);

  list.push_back(a);
  list.push_back(b);
  list.push_back(c);

  auto it1 = list.end();
  auto it2 = list.insert(it1, d);
  EXPECT_EQ(4, it2->value);
  --it2;
  EXPECT_EQ(3, it2->value);
  ++it2;
  EXPECT_EQ(4, it2->value);
  ++it2;
  EXPECT_TRUE(it1 == it2);
  --it1;
  EXPECT_EQ(4, it1->value);
}

TEST(advanced_intrusive_list_testing, insert_04) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);

  list.push_back(b);
  list.push_back(c);

  auto it1 = list.insert(list.begin(), a);
  EXPECT_EQ(1, it1->value);
  auto it2 = it1;
  ++it2;
  EXPECT_EQ(2, it2->value);
  list.erase(it1);
  EXPECT_TRUE(it2 == list.begin());
}

TEST(advanced_intrusive_list_testing, insert_05) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);

  list.push_back(b);
  list.push_back(c);

  auto it1 = list.insert(list.begin(), a);
  EXPECT_EQ(1, it1->value);
  auto it2 = it1;
  ++it2;
  EXPECT_EQ(2, it2->value);
  list.erase(it2);
  EXPECT_TRUE(it1 == list.begin());
  ++it1;
  EXPECT_EQ(3, it1->value);
}

TEST(intrusive_list_testing, insert_06) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  list.push_back(a);
  list.push_back(b);
  list.push_back(c);

  auto b_iter = std::next(list.begin());
  list.insert(b_iter, b);
  expect_eq(list, {1, 2, 3});

  list.insert(b_iter, a);
  expect_eq(list, {1, 2, 3});

  list.insert(b_iter, c);
  expect_eq(list, {1, 3, 2});
}

TEST(advanced_intrusive_list_testing, erase_01) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);

  list.push_back(a);
  list.push_back(b);
  list.push_back(c);

  auto it1 = list.begin();
  ++it1;
  auto it2 = list.erase(it1);
  EXPECT_EQ(3, it2->value);
  --it2;
  EXPECT_EQ(1, it2->value);
  EXPECT_TRUE(it2 == list.begin());
}

TEST(advanced_intrusive_list_testing, erase_02) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);

  list.push_back(a);
  list.push_back(b);
  list.push_back(c);

  auto it1 = list.end();
  --it1;
  auto it2 = list.erase(it1);
  EXPECT_TRUE(it2 == list.end());
  --it2;
  EXPECT_EQ(2, it2->value);
}

void magic(node& n) {
  n.value = 42;
}
void magic(node const&) {}

TEST(advanced_intrusive_list_testing, back_front_ncref) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, a, b, c, d, e);
  magic(std::as_const(list).front());
  magic(std::as_const(list).back());

  expect_eq(list, {1, 2, 3, 4, 5});
}

TEST(advanced_intrusive_list_testing, back_front_ref) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, a, b, c, d, e);
  list.front().value = 6;
  list.back().value = 7;
  expect_eq(list, {6, 2, 3, 4, 7});
}

TEST(advanced_intrusive_list_testing, move_ctor) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);
  intrusive::list<node> list2 = std::move(list1);

  EXPECT_TRUE(list1.empty());
  expect_eq(list2, {1, 2, 3});
}

TEST(advanced_intrusive_list_testing, move_operator_from_empty) {
  intrusive::list<node> list1, list2;
  node a(1), b(2), c(3);
  mass_push_back(list2, a, b, c);
  list1 = std::move(list2);
  expect_eq(list1, {1, 2, 3});
  EXPECT_TRUE(list2.empty());
}

TEST(advanced_intrusive_list_testing, move_operator) {
  intrusive::list<node> list1, list2;
  node a(1), b(2), c(3);
  node d(4), e(5), f(6);
  mass_push_back(list1, a, b, c);
  mass_push_back(list2, d, e, f);
  list1 = std::move(list2);
  expect_eq(list1, {4, 5, 6});
  EXPECT_TRUE(list2.empty());
}

TEST(advanced_intrusive_list_testing, iterator_deref_1) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);
  intrusive::list<node>::iterator i = std::next(list.begin());
  EXPECT_EQ(2, i->value);
  magic(*i);
  expect_eq(list, {1, 42, 3});

  intrusive::list<node>::const_iterator j = std::next(list.begin(), 2);
  EXPECT_EQ(3, j->value);
  magic(*j);
  expect_eq(list, {1, 42, 3});
}

TEST(advanced_intrusive_list_testing, iterator_deref_1c) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);
  intrusive::list<node>::iterator const i = std::next(list.begin());
  EXPECT_EQ(2, i->value);
  magic(*i);
  expect_eq(list, {1, 42, 3});

  intrusive::list<node>::const_iterator const j = std::next(list.begin(), 2);
  EXPECT_EQ(3, j->value);
  magic(*j);
  expect_eq(list, {1, 42, 3});
}

TEST(advanced_intrusive_list_testing, iterator_deref_2) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);
  intrusive::list<node>::iterator i = std::next(list.begin());
  EXPECT_EQ(2, i->value);
  magic(*i.operator->());
  expect_eq(list, {1, 42, 3});

  intrusive::list<node>::const_iterator j = std::next(list.begin(), 2);
  EXPECT_EQ(3, j->value);
  magic(*j.operator->());
  expect_eq(list, {1, 42, 3});
}

TEST(advanced_intrusive_list_testing, iterator_deref_2c) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);
  intrusive::list<node>::iterator const i = std::next(list.begin());
  EXPECT_EQ(2, i->value);
  magic(*i.operator->());
  expect_eq(list, {1, 42, 3});

  intrusive::list<node>::const_iterator const j = std::next(list.begin(), 2);
  EXPECT_EQ(3, j->value);
  magic(*j.operator->());
  expect_eq(list, {1, 42, 3});
}

TEST(advanced_intrusive_list_testing, multiple_tags) {
  intrusive::list<multi_node, tag_a> list_a;
  intrusive::list<multi_node, tag_b> list_b;
  multi_node x(1), y(2), z(3);

  mass_push_back(list_a, x, y, z);
  mass_push_back(list_b, z, y, x);

  expect_eq(list_a, {1, 2, 3});
  expect_eq(list_b, {3, 2, 1});
}

TEST(advanced_intrusive_list_testing, splice_begin_begin) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.begin(), c2, c2.begin(), std::next(c2.begin(), 2));
  expect_eq(c1, {5, 6, 1, 2, 3, 4});
  expect_eq(c2, {7, 8});
}

TEST(advanced_intrusive_list_testing, splice_begin_middle) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.begin(), c2, std::next(c2.begin()), std::next(c2.begin(), 2));
  expect_eq(c1, {6, 1, 2, 3, 4});
  expect_eq(c2, {5, 7, 8});
}

TEST(advanced_intrusive_list_testing, splice_begin_end) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.begin(), c2, std::next(c2.begin(), 2), c2.end());
  expect_eq(c1, {7, 8, 1, 2, 3, 4});
  expect_eq(c2, {5, 6});
}

TEST(advanced_intrusive_list_testing, splice_begin_whole) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.begin(), c2, c2.begin(), c2.end());
  expect_eq(c1, {5, 6, 7, 8, 1, 2, 3, 4});
  EXPECT_TRUE(c2.empty());
}

TEST(advanced_intrusive_list_testing, splice_begin_empty) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.begin(), c2, std::next(c2.begin(), 2), std::next(c2.begin(), 2));
  expect_eq(c1, {1, 2, 3, 4});
  expect_eq(c2, {5, 6, 7, 8});
}

TEST(advanced_intrusive_list_testing, splice_middle_begin) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(std::next(c1.begin(), 2), c2, c2.begin(), std::next(c2.begin(), 2));
  expect_eq(c1, {1, 2, 5, 6, 3, 4});
  expect_eq(c2, {7, 8});
}

TEST(advanced_intrusive_list_testing, splice_middle_middle) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(std::next(c1.begin(), 2), c2, std::next(c2.begin()),
            std::next(c2.begin(), 3));
  expect_eq(c1, {1, 2, 6, 7, 3, 4});
  expect_eq(c2, {5, 8});
}

TEST(advanced_intrusive_list_testing, splice_middle_end) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(std::next(c1.begin(), 2), c2, std::next(c2.begin(), 2), c2.end());
  expect_eq(c1, {1, 2, 7, 8, 3, 4});
  expect_eq(c2, {5, 6});
}

TEST(advanced_intrusive_list_testing, splice_middle_whole) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(std::next(c1.begin(), 2), c2, c2.begin(), c2.end());
  expect_eq(c1, {1, 2, 5, 6, 7, 8, 3, 4});
  EXPECT_TRUE(c2.empty());
}

TEST(advanced_intrusive_list_testing, splice_middle_empty) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(std::next(c1.begin(), 2), c2, std::next(c2.begin(), 2),
            std::next(c2.begin(), 2));
  expect_eq(c1, {1, 2, 3, 4});
  expect_eq(c2, {5, 6, 7, 8});
}

TEST(advanced_intrusive_list_testing, splice_end_begin) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, c2.begin(), std::next(c2.begin(), 2));
  expect_eq(c1, {1, 2, 3, 4, 5, 6});
  expect_eq(c2, {7, 8});
}

TEST(advanced_intrusive_list_testing, splice_end_middle) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, std::next(c2.begin()), std::next(c2.begin(), 3));
  expect_eq(c1, {1, 2, 3, 4, 6, 7});
  expect_eq(c2, {5, 8});
}

TEST(advanced_intrusive_list_testing, splice_end_end) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, std::next(c2.begin(), 2), c2.end());
  expect_eq(c1, {1, 2, 3, 4, 7, 8});
  expect_eq(c2, {5, 6});
}

TEST(advanced_intrusive_list_testing, splice_end_whole) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, c2.begin(), c2.end());
  expect_eq(c1, {1, 2, 3, 4, 5, 6, 7, 8});
  EXPECT_TRUE(c2.empty());
}

TEST(advanced_intrusive_list_testing, splice_end_empty) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, std::next(c2.begin(), 2), std::next(c2.begin(), 2));
  expect_eq(c1, {1, 2, 3, 4});
  expect_eq(c2, {5, 6, 7, 8});
}

TEST(advanced_intrusive_list_testing, splice_empty_begin) {
  intrusive::list<node> c1, c2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, c2.begin(), std::next(c2.begin(), 2));
  expect_eq(c1, {5, 6});
  expect_eq(c2, {7, 8});
}

TEST(advanced_intrusive_list_testing, splice_empty_middle) {
  intrusive::list<node> c1, c2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, std::next(c2.begin(), 1), std::next(c2.begin(), 3));
  expect_eq(c1, {6, 7});
  expect_eq(c2, {5, 8});
}

TEST(advanced_intrusive_list_testing, splice_empty_end) {
  intrusive::list<node> c1, c2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, std::next(c2.begin(), 2), c2.end());
  expect_eq(c1, {7, 8});
  expect_eq(c2, {5, 6});
}

TEST(advanced_intrusive_list_testing, splice_empty_whole) {
  intrusive::list<node> c1, c2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(c2, e, f, g, h);
  c1.splice(c1.end(), c2, c2.begin(), c2.end());
  expect_eq(c1, {5, 6, 7, 8});
  EXPECT_TRUE(c2.empty());
}

TEST(advanced_intrusive_list_testing, splice_self) {
  intrusive::list<node> c1;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(c1, a, b, c, d, e);
  c1.splice(std::next(c1.begin()), c1, std::next(c1.begin(), 2),
            std::prev(c1.end()));
  expect_eq(c1, {1, 3, 4, 2, 5});
}

TEST(advanced_intrusive_list_testing, splice_iterators) {
  intrusive::list<node> c1, c2;
  node a(1), b(2), c(3), d(4);
  node e(5), f(6), g(7), h(8);
  mass_push_back(c1, a, b, c, d);
  mass_push_back(c2, e, f, g, h);
  intrusive::list<node>::const_iterator i = std::next(c1.begin(), 2);
  intrusive::list<node>::const_iterator j = std::next(c2.begin());
  intrusive::list<node>::const_iterator k = std::prev(c2.end());
  c1.splice(i, c2, j, k);
  expect_eq(c1, {1, 2, 6, 7, 3, 4});
  expect_eq(c2, {5, 8});

  EXPECT_EQ(3, i->value);
  EXPECT_EQ(6, j->value);
  EXPECT_EQ(8, k->value);

  EXPECT_EQ(7, std::prev(i)->value);
  EXPECT_EQ(2, std::prev(j)->value);
  EXPECT_EQ(5, std::prev(k)->value);
}
