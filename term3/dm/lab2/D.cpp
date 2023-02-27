#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <bitset>
#include <ctime>
#include <queue>
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
    };
}


using triple = std::triple<int, bool, bool>;
using quadruple = std::quadruple<int, int, bool, bool>;

struct cmp {
    bool operator() (const quadruple& a, const quadruple& b) {
        return a.a > b.a;
    }
};

std::uint32_t del(std::uint32_t n, std::uint32_t index) {
    return (n & ~(1 << index));
}

std::uint32_t add(std::uint32_t n, std::uint32_t index) {
    return (n | (1 << index));
}

bool checkb(std::uint32_t n, int index){
    return (n & (1 << index));
}



int main() {
    bool first = false;
    std::int64_t n, m, s,c = 0;
    std::ifstream in("check.in");
    std::ofstream out("check.out");
    
    in >> n >> m;

    std::set<quadruple> vec, new_vec;

    for (int i = 0; i < m; i++) {
        int k, bit;
        in >> k;
        std::bitset < 10> b;
        for (int j = 0; j < k; j++) {
            in >> bit;
            b[bit - 1] = true;
        }
        int num = b.to_ulong();
        vec.insert({ k, num, true, true });
    }

    for (auto& it : vec) {
        if (!it.a) {
            first = true;
            break;
        }
    }

    if (!first) {
        out << "NO";
        return 0;
    }

    for (auto& it : vec) {
        bool check = false;
        for (int i = 0; i < n; i++) {
            auto dop = vec.find(quadruple(0, del(it.b, i), 0, 0));
            if (dop == vec.end()) {
                check = true;
                break;
            }
        }
        if(check) new_vec.insert(quadruple(it.a, it.b, it.c, 0));
        else new_vec.insert(quadruple(it.a, it.b, it.c, 1));
    }

    for (auto it = vec.begin(), it2 = new_vec.begin(); it != vec.end(); it++, it2++) {
        if (it->c != it2->d) {
            out << "NO";
            return 0;
        }
    }

    for (auto it = vec.begin(); it != vec.end(); it++) {
        for (auto it2 = vec.begin(); it2 != vec.end(); it2++) {
            if (it->a > it2->a) {
                bool check = false;
                for (int i = 0; i < n; i++) {
                    if (checkb(it->b, i) && !checkb(it2->b, i)) {
                        auto dop = vec.find(quadruple(0, add(it2->b, i), 0, 0));
                        if (dop != vec.end()) {
                            check = true;
                            break;
                        }
                    }
                }

                if (!check) {
                    out << "NO";
                    return 0;
                }
            }
        }
    }

    out << "YES";
 	return 0;
}