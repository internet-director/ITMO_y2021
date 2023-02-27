#include "intrusive_list.h"
#include "test_utils.h"

#include <utility>

TEST(intrusive_list_testing, default_ctor) {
  intrusive::list<node> list;
}

TEST(intrusive_list_testing, ends_01) {
  intrusive::list<node> list;
  node a(1);
  list.push_back(a);
}

TEST(intrusive_list_testing, ends_02) {
  node a(1);
  {
    intrusive::list<node> list;
    list.push_back(a);
  }
}

TEST(intrusive_list_testing, ends_03) {
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

TEST(intrusive_list_testing, ends_04) {
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

TEST(intrusive_list_testing, ends_05) {
  intrusive::list<node> list;
  node a(1);
  list.push_front(a);
}

TEST(intrusive_list_testing, ends_06) {
  node a(1);
  {
    intrusive::list<node> list;
    list.push_front(a);
  }
}

TEST(intrusive_list_testing, ends_07) {
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

TEST(intrusive_list_testing, back_front) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);
  EXPECT_EQ(1, list.front().value);
  EXPECT_EQ(1, std::as_const(list).front().value);
  EXPECT_EQ(3, list.back().value);
  EXPECT_EQ(3, std::as_const(list).back().value);
}

TEST(intrusive_list_testing, back_front_ref) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, a, b, c, d, e);
  list.front().value = 6;
  list.back().value = 7;
}

TEST(intrusive_list_testing, back_front_cref) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, a, b, c, d, e);
  EXPECT_TRUE(&list.front() == &std::as_const(list).front());
  EXPECT_TRUE(&list.back() == &std::as_const(list).back());
}

TEST(intrusive_list_testing, move_ctor_empty) {
  intrusive::list<node> list1;
  intrusive::list<node> list2 = std::move(list1);

  EXPECT_TRUE(list2.empty());
}

TEST(intrusive_list_testing, move_ctor) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);
  intrusive::list<node> list2 = std::move(list1);

  EXPECT_TRUE(list1.empty());
  EXPECT_EQ(list2.back().value, 3);
  EXPECT_EQ(list2.front().value, 1);
}

TEST(intrusive_list_testing, move_operator_empty) {
  intrusive::list<node> list1, list2;
  list1 = std::move(list2);
  EXPECT_TRUE(list1.empty());
  EXPECT_TRUE(list2.empty());
}

TEST(intrusive_list_testing, move_operator_from_empty) {
  intrusive::list<node> list1, list2;
  node a(1), b(2), c(3);
  mass_push_back(list2, a, b, c);
  list1 = std::move(list2);
  EXPECT_EQ(list1.back().value, 3);
  EXPECT_EQ(list1.front().value, 1);
  EXPECT_TRUE(list2.empty());
}

TEST(intrusive_list_testing, move_operator_to_empty) {
  intrusive::list<node> list1, list2;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);
  list1 = std::move(list2);
  EXPECT_TRUE(list1.empty());
  EXPECT_TRUE(list2.empty());
}

TEST(intrusive_list_testing, move_operator) {
  intrusive::list<node> list1, list2;
  node a(1), b(2), c(3);
  node d(4), e(5), f(6);
  mass_push_back(list1, a, b, c);
  mass_push_back(list2, d, e, f);
  list1 = std::move(list2);
  EXPECT_EQ(list1.back().value, 6);
  EXPECT_EQ(list1.front().value, 4);
  EXPECT_TRUE(list2.empty());
}

TEST(intrusive_list_testing, iterators_01) {
  intrusive::list<node> list;
  EXPECT_TRUE(list.begin() == list.end());
  EXPECT_FALSE(list.begin() != list.end());

  intrusive::list<node> const& clist = list;
  EXPECT_TRUE(clist.begin() == clist.end());
  EXPECT_FALSE(clist.begin() != clist.end());
}

TEST(intrusive_list_testing, iterators_02) {
  intrusive::list<node> list;
  intrusive::list<node>::iterator it;
  it = list.begin();
}

TEST(intrusive_list_testing, iterators_03) {
  intrusive::list<node> list;
  node a(1);

  list.push_back(a);

  auto it1 = list.begin();
  node& ra1 = *it1;
  EXPECT_EQ(&a, &ra1);
  auto const it2 = it1;
  node& ra2 = *it2;
  EXPECT_EQ(&a, &ra2);
}

TEST(intrusive_list_testing, insert_01) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);

  list.push_back(b);
  list.push_back(c);
  list.push_back(d);

  auto it1 = list.begin();
  auto it2 = list.insert(it1, a);
  EXPECT_EQ(1, (*it2).value);
  EXPECT_EQ(2, (*it1).value);
  EXPECT_FALSE(it1 == it2);
  EXPECT_TRUE(it2 == list.begin());
}

TEST(intrusive_list_testing, insert_02) {
  intrusive::list<node> list1, list2;
  node a(1);
  list1.push_back(a);
  list2.push_back(a);
  EXPECT_TRUE(list1.empty());
  EXPECT_FALSE(list2.empty());
}

TEST(intrusive_list_testing, erase_01) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);

  list.push_back(a);
  list.push_back(b);
  list.push_back(c);

  auto it1 = list.erase(list.begin());
  EXPECT_EQ(2, (*it1).value);
  EXPECT_TRUE(it1 == list.begin());
}

TEST(intrusive_list_testing, multiple_tags) {
  intrusive::list<multi_node, tag_a> list_a;
  intrusive::list<multi_node, tag_b> list_b;
  multi_node x(1), y(2), z(3);

  mass_push_back(list_a, x, y, z);
  mass_push_back(list_b, z, y, x);

  EXPECT_EQ(list_a.front().value, 1);
  EXPECT_EQ(list_b.front().value, 3);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
