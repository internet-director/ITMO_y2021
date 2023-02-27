#pragma once
#include "intrusive_list.h"
#include <functional>

// Чтобы не было коллизий с UNIX-сигналами реализация вынесена в неймспейс, по
// той же причине изменено и название файла
namespace signals {

template <typename T>
struct signal;

struct connection_tag {};

template <typename... Args>
struct signal<void(Args...)> {
  using slot_t = std::function<void(Args...)>;
  class connection : public intrusive::list_element<connection_tag> {
    friend struct signal;
    signal* sig = nullptr;
    slot_t func;

    connection(signal* sig_, slot_t func_) : sig(sig_), func(std::move(func_)) {
      if (sig != nullptr) {
        sig->connections.push_back(*this);
      }
    }

    void operator()(Args... args) const {
      func(std::forward<Args>(args)...);
    }

  public:
    connection() = default;

    connection(const connection& other) = delete;
    connection& operator=(const connection& other) = delete;
    connection(connection&& other) : sig(other.sig), func(std::move(other.func)) {
      if (sig != nullptr) {
        connection_iterator it(&other);
        sig->connections.insert(++it, *this);
        other.disconnect();
      }
    }
    connection& operator=(connection&& other) {
      if (this == &other) {
        return *this;
      }
      sig = other.sig;
      func = std::move(other.func);
      if (sig != nullptr) {
        connection_iterator it(&other);
        sig->connections.insert(++it, *this);
        other.disconnect();
      }
      return *this;
    }

    void disconnect() {
      if (sig != nullptr) {
        for (iterator_holder* it = sig->tail; it != nullptr; it = it->prev) {
          if (&*it->current == this) {
            it->current++;
          }
        }
        this->unlink();
        sig = nullptr;
      }
    }

    ~connection() {
      disconnect();
    }
  };

  signal() = default;
  signal(signal const&) = delete;
  signal& operator=(signal const&) = delete;
  ~signal() {
    for (auto it = connections.begin(); it != connections.end(); it++) {
      it->sig = nullptr;
      it->func = nullptr;
    }
    for (iterator_holder* it = this->tail; it != nullptr; it = it->prev) {
      it->sig = nullptr;
    }
    connections.clear();
  }

  connection connect(std::function<void(Args...)> slot) noexcept {
    return connection(this, std::move(slot));
  }

  void operator()(Args... args) const {
    iterator_holder holder(this);
    while (holder.current != connections.end()) {
      auto copy = holder.current;
      holder.current++;
      (*copy)(std::forward<Args>(args)...);
      if (holder.sig == nullptr) {
        return;
      }
    }
  }

private:
  using connections_list = intrusive::list<connection, connection_tag>;
  using connection_iterator = typename connections_list::iterator;

  struct iterator_holder {
    explicit iterator_holder(const signal* sig_) : sig(sig_), current(sig->connections.begin()), prev(sig->tail) {
      sig->tail = this;
    }

    ~iterator_holder() {
      if (sig != nullptr) {
        sig->tail = sig->tail->prev;
      }
    }

    const signal* sig;
    typename connections_list::const_iterator current;
    iterator_holder* prev;
  };

  connections_list connections;
  mutable iterator_holder* tail = nullptr;
};

} // namespace signals
