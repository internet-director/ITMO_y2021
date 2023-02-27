#include "signals.h"
#include "gtest/gtest.h"

TEST(signal_testing, trivial) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  auto conn2 = sig.connect([&] { ++got2; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);

  sig();

  EXPECT_EQ(2, got1);
  EXPECT_EQ(2, got2);
}

TEST(signal_testing, arguments) {
  signals::signal<void(int, int, int)> sig;
  auto conn = sig.connect([](int a, int b, int c) {
    EXPECT_EQ(5, a);
    EXPECT_EQ(6, b);
    EXPECT_EQ(7, c);
  });

  sig(5, 6, 7);
}

TEST(signal_testing, empty_connection_move) {
  signals::signal<void()>::connection a;
  signals::signal<void()>::connection b = std::move(a);
  b = std::move(b);
}

TEST(signal_testing, disconnect) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  auto conn2 = sig.connect([&] { ++got2; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);

  conn1.disconnect();
  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(2, got2);
}

TEST(signal_testing, connection_move_ctor) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1_old = sig.connect([&] { ++got1; });
  auto conn1_new = std::move(conn1_old);

  sig();

  EXPECT_EQ(1, got1);
}

TEST(signal_testing, connection_destructor) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1 = std::make_unique<signals::signal<void()>::connection>(sig.connect([&] { ++got1; }));
  uint32_t got2 = 0;
  auto conn2 = sig.connect([&] { ++got2; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);

  conn1.reset();
  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(2, got2);
}

TEST(signal_testing, disconnect_in_emit) {
  using connection = signals::signal<void()>::connection;
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1 = std::make_unique<connection>(sig.connect([&] { ++got1; }));
  uint32_t got2 = 0;
  std::unique_ptr<connection> conn2;
  conn2.reset(new connection(sig.connect([&] {
    ++got2;
    conn2.reset();
  })));
  uint32_t got3 = 0;
  auto conn3 = std::make_unique<connection>(sig.connect([&] { ++got3; }));

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);
  EXPECT_EQ(1, got3);

  sig();

  EXPECT_EQ(2, got1);
  EXPECT_EQ(1, got2);
  EXPECT_EQ(2, got3);
}

TEST(signal_testing, destroy_signal_before_connection_01) {
  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  auto conn1 = sig->connect([&] { ++got1; });

  sig.reset();
}

TEST(signal_testing, destroy_signal_before_connection_02) {
  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  auto conn1_old = sig->connect([&] { ++got1; });

  sig.reset();

  auto conn1_new = std::move(conn1_old);
}

TEST(signal_testing, destroy_signal_in_emit) {
  using connection = signals::signal<void()>::connection;

  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  connection conn1(sig->connect([&] { ++got1; }));
  uint32_t got2 = 0;
  connection conn2(sig->connect([&] {
    ++got2;
    sig.reset();
  }));
  uint32_t got3 = 0;
  connection conn3(sig->connect([&] { ++got3; }));

  (*sig)();

  EXPECT_EQ(1, got2);
}

TEST(signal_testing, recursive_emit) {
  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  auto conn1 = sig->connect([&] { ++got1; });
  uint32_t got2 = 0;
  auto conn2 = sig->connect([&] {
    ++got2;
    if (got2 == 1)
      (*sig)();
    else if (got2 == 2)
      sig.reset();
    else
      assert(false);
  });
  uint32_t got3 = 0;
  auto conn3 = sig->connect([&] { ++got3; });

  (*sig)();

  EXPECT_EQ(2, got2);
}

TEST(signal_testing, exception_in_emit) {
  struct test_exception : std::exception {};

  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  auto conn1 = sig->connect([&] { ++got1; });
  uint32_t got2 = 0;
  auto conn2 = sig->connect([&] {
    ++got2;
    if (got2 == 1)
      (*sig)();
    else if (got2 == 2)
      throw test_exception();
  });
  uint32_t got3 = 0;
  auto conn3 = sig->connect([&] { ++got3; });

  EXPECT_THROW((*sig)(), test_exception);
  EXPECT_EQ(2, got2);

  got1 = 0;
  got3 = 0;

  (*sig)();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(3, got2);
  EXPECT_EQ(1, got3);
}

TEST(signal_testing, move_in_emit_01) {
  using connection = signals::signal<void()>::connection;

  signals::signal<void()> sig;
  uint32_t got1 = 0;
  connection conn1_old;
  std::unique_ptr<connection> conn1_new;

  conn1_old = sig.connect([&] {
    ++got1;
    if (got1 == 1) {
      auto& ref_copy = conn1_new;
      ref_copy = std::make_unique<connection>(std::move(conn1_old));
    }
  });

  sig();
  EXPECT_EQ(1, got1);

  sig();
  EXPECT_EQ(2, got1);
}

TEST(signal_testing, move_in_emit_02) {
  using connection = signals::signal<void()>::connection;

  signals::signal<void()> sig;
  uint32_t got1 = 0;
  uint32_t got2 = 0;
  uint32_t got3 = 0;
  std::unique_ptr<connection> conn1_old;
  std::unique_ptr<connection> conn1_new;
  std::unique_ptr<connection> conn2;
  std::unique_ptr<connection> conn3_old;
  std::unique_ptr<connection> conn3_new;

  conn1_old = std::make_unique<connection>(sig.connect([&] { ++got1; }));

  conn2 = std::make_unique<connection>(sig.connect([&] {
    ++got2;
    conn1_new = std::make_unique<connection>(std::move(*conn1_old));
    conn3_new = std::make_unique<connection>(std::move(*conn3_old));
    conn1_old.reset();
    conn3_old.reset();
  }));

  conn3_old = std::make_unique<connection>(sig.connect([&] { ++got3; }));

  sig();
  EXPECT_EQ(1, got1);
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
