#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <bitset>
#include <ctime>
#include <queue>
#include <algorithm>
#include <set>
#include <deque>
#include <algorithm>

namespace std {
    template<typename T1, typename T2, typename T3>
    struct triple {
        triple() = default;
        triple(const T1& t1, const T2& t2, const T3& t3) :a(t1), b(t2), c(t3) {}
        triple(T1&& t1, T2&& t2, T3&& t3) :a(std::move(t1)), b(std::move(t2)), c(std::move(t3)) {}

        T1 a;
        T2 b;
        T3 c;
    };
}

namespace std {
    template<typename T1, typename T2, typename T3, typename T4>
    struct quadruple {
        quadruple() = default;
        quadruple(const T1& t1, const T2& t2, const T3& t3, const T4& t4) :a(t1), b(t2), c(t3), d(t4) {}
        quadruple(T1&& t1, T2&& t2, T3&& t3, T4&& t4) :a(std::move(t1)), b(std::move(t2)), c(std::move(t3)), d(std::move(t4)) {}

        T1 a;
        T2 b;
        T3 c;
        T4 d;

        bool operator<(const quadruple& y) const { return b < y.b; }
        bool operator==(const quadruple& y) const { return b == y.b; }
    };
}


using triple = std::triple<std::uint64_t, bool, bool>;
using quadruple = std::quadruple<std::uint64_t, std::uint64_t, bool, bool>;

struct cmp {
    bool operator() (const quadruple& a, const quadruple& b) {
        return a.a > b.a;
    }
};

struct cmp2 {
    bool operator() (const std::pair<int, int>& a, const std::pair<int, int>& b) const {
        return a.first > b.first;
    }
};


std::uint64_t del(std::uint64_t n, std::uint64_t index) {
    return (n & ~(1 << index));
}

std::uint64_t add(std::uint64_t n, std::uint64_t index) {
    return (n | (1 << index));
}

bool checkb(std::uint64_t n, int index) {
    return (n & (1 << index));
}

void rec(std::set<std::uint64_t>& data, std::vector<bool>& vec, std::uint64_t k, std::uint64_t dop, std::uint64_t sz) {
    if (dop == sz) {
        //data.insert(k);
        vec[k] = true;
    }
    else {
        rec(data, vec, k, dop + 1, sz);
        if (!((k >> dop) & 1)) rec(data, vec, k + add(0, dop), dop + 1, sz);
    }
}

int main() {
    bool first = false;
    std::ios_base::sync_with_stdio(false);
    std::uint64_t n, m, s = 0, c = 0;
    std::ifstream in("cycles.in");
    std::ofstream out("cycles.out");

    in >> n >> m;

    std::set<std::pair<std::uint64_t, std::uint64_t>, cmp2> data;
    std::set<std::uint64_t> red;
    std::set<quadruple> vec, new_vec;
    std::vector<std::pair<std::uint64_t, std::uint64_t >> dat(n);
    std::vector<bool> map(1 << (n + 1), 0);

    for (int i = 0; i < n; i++) {
        std::uint64_t k;
        in >> k;
        data.insert({ k, i });
        dat[i] = { k, i };
    }

    std::sort(dat.begin(), dat.end(), [](const std::pair<std::uint64_t, std::uint64_t>& a, const std::pair<std::uint64_t, std::uint64_t>& b) {
        return a.first > b.first;
        });

    for (int i = 0; i < m; i++) {
        std::uint64_t k, bit;
        std::uint64_t sum = 0;
        in >> k;
        std::bitset<10> b;
        for (int j = 0; j < k; j++) {
            in >> bit;
            b[bit - 1] = true;
            sum += dat[bit - 1].first;
        }
        std::uint64_t num = b.to_ullong();
        map[num] = true;
        vec.insert({ sum, num, true, true });
    }

    for (auto& it : vec) {
        rec(red, map, it.b, 0, n);
    }

    for (auto& it : dat) {
        if (!map[(add(c, it.second))]) {
            s += it.first;
            c = add(c, it.second);
        }
    }

    out << s;

    return 0;
}