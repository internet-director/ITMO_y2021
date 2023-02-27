#include <iostream>
#include <fstream>
#include <vector>
#include <string>
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
    };
}

using quadruple = std::quadruple<int, int, std::uint64_t, int>;
constexpr int INF = 100000;


int init(std::vector<int>& index, int v) {
    if (index[v] == v) return v;
    return (index[v] = init(index, index[v]));
}

int main() {
    std::int64_t n, m, s,c = 0;
    std::ifstream in("destroy.in");
    std::ofstream out("destroy.out");
    
    in >> n >> m >> s;

    std::set<int> result, res;
    std::vector<int> index(n);
    std::vector<quadruple> matrix(m), deleted;

    for (int i = 0; i < n; i++) {
        index[i] = i;
    }

    for (int i = 0; i < m; i++) {
        in >> matrix[i].a >> matrix[i].b >> matrix[i].c;
        matrix[i].d = i;
        matrix[i].a--;
        matrix[i].b--;
    }

    std::sort(matrix.begin(), matrix.end(), [](const quadruple& a, const quadruple& b) {
        return a.c > b.c;
        });

    for (int i = 0; i < m; i++) {
        int v = matrix[i].a;
        int u = matrix[i].b;

        if (init(index, v) != init(index, u)) {
            res.insert(matrix[i].d);
            int v_id = init(index, v);
            int u_id = init(index, u);
            if (std::clock() % 2) std::swap(v_id, u_id);
            if (v_id != u_id) index[v_id] = u_id;
        }
    }

    for (int i = 0; i < m; i++) {
        if (res.find(matrix[i].d) == res.end()) deleted.push_back({quadruple(0, 0, matrix[i].c, matrix[i].d)});
    }

    std::sort(deleted.begin(), deleted.end(), [](const quadruple& a, const quadruple& b) {
        return a.c < b.c;
        });


    for (auto& i : deleted) {
        c += i.c;
        if (c > s) break;
        result.insert(i.d);
    }
    out << result.size() << "\n";
    for (auto i : result) {
        out << i + 1 << " ";
    }

 	return 0;
}