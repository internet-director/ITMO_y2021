#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <bitset>
#include <ctime>
#include <queue>
#include <algorithm>
#include <set>
#include <unordered_set>
#include <map>
#include <deque>
#include <cmath>
#include <algorithm>

namespace std {
    template<typename T1, typename T2, typename T3>
    struct triple {
        triple() = default;
        triple(const T1& t1, const T2& t2, const T3& t3) : a(t1), b(t2), c(t3) {}
        triple(T1&& t1, T2&& t2, T3&& t3) : a(std::move(t1)), b(std::move(t2)), c(std::move(t3)) {}

        T1 a;
        T2 b;
        T3 c;
    };
}

namespace std {
    template<typename T1, typename T2, typename T3, typename T4>
    struct quadruple {
        quadruple() = default;
        quadruple(const T1& t1, const T2& t2, const T3& t3, const T4& t4) : a(t1), b(t2), c(t3), d(t4) {}
        quadruple(T1&& t1, T2&& t2, T3&& t3, T4&& t4) : a(std::move(t1)), b(std::move(t2)), c(std::move(t3)),
            d(std::move(t4)) {}

        T1 a;
        T2 b;
        T3 c;
        T4 d;

        bool operator<(const quadruple& y) const { return b < y.b; }
        bool operator==(const quadruple& y) const { return b == y.b; }
    };
}

using triple = std::triple<std::uint64_t, bool, bool>;
using quadruple = std::quadruple<std::uint64_t, std::uint64_t, int, int>;

struct cmp {
    bool operator()(const quadruple& a, const quadruple& b) {
        return a.a > b.a;
    }
};

struct cmp2 {
    bool operator()(const std::pair<int, int>& a, const std::pair<int, int>& b) const {
        return a.first > b.first;
    }
};

#define finded(X, Y) (X.find(Y) != X.end())
#define fill_vec(vec) for (int i = 0; i < vec.size(); i++) vec[i] = i;

int init(std::vector<int>& index, int v) {
    if (index[v] == v) return v;
    return (index[v] = init(index, index[v]));
}

std::vector<int> rel;

void kryskal(std::vector<quadruple>& matrix, std::vector<int>& index, std::unordered_set<int>& vset, std::unordered_set<int>& result, int skip) {
    std::fill(rel.begin(), rel.end(), 0);
    for (int i = 0; i < matrix.size(); i++) {
        if (i == skip) continue;
        int v = matrix[i].a;
        int u = matrix[i].b;
        int c = matrix[i].c;
        if (finded(vset, i)) {
            result.insert(c);
            int v_id = init(index, v);
            int u_id = init(index, u);
            if (std::clock() % 2) std::swap(v_id, u_id);
            if (v_id != u_id) {
                index[u_id] = v_id;
                if (rel[v_id] == rel[u_id]) rel[v_id]++;
            }
        }
    }
}

void gen_p(std::vector<std::unordered_set<int>>& matrix, std::vector<int>& p, std::vector<int>& w, int e) {
    std::vector<int> dop;
    for (int i : matrix[e]) {
        if (w[e] >= w[i] - 1) continue;
        p[i] = e;
        w[i] = w[e] + 1;
        dop.push_back(i);
    }
    for (int i : dop) gen_p(matrix, p, w, i);
}

int main() {
    std::uint64_t n, m;
    std::ifstream in("rainbow.in");
    std::ofstream out("rainbow.out");

    in >> n >> m;
    rel.resize(n);
    std::unordered_set<int> set;
    std::vector<quadruple> matrix(m);
    std::vector<int> vec(n), weig(m), p_e(m);
    std::vector<std::vector<int>> matrix_c(m);

    for (int i = 0; i < m; i++) {
        in >> matrix[i].a >> matrix[i].b >> matrix[i].c;
        matrix[i].a--;
        matrix[i].b--;
        matrix[i].d = i;
    }

    for (;;) {
        std::int32_t dop = INT32_MAX / 4;
        std::vector<int> dop_arr;
        std::unordered_set<int> kostil, col;

        for (auto& it : matrix_c) it.clear();
        std::fill(weig.begin(), weig.end(), dop);
        std::fill(p_e.begin(), p_e.end(), -1);

        for (int i = 0; i < m; i++) {
            if (finded(set, i)) {
                std::unordered_set<int> cols;
                fill_vec(vec);
                kryskal(matrix, vec, set, cols, i);

                for (int j = 0; j < m; j++) {
                    int v = matrix[j].a;
                    int u = matrix[j].b;
                    if (!finded(set, j)) {
                        if (init(vec, u) != init(vec, v)) matrix_c[i].push_back(j);
                        if (!finded(cols, matrix[j].c)) matrix_c[j].push_back(i);
                    }
                }
            }
        }

        fill_vec(vec);
        kryskal(matrix, vec, set, col, -1);

        for (int i = 0; i < m; i++) {
            if (!finded(set, i)) {
                int v = matrix[i].a;
                int u = matrix[i].b;
                if (!finded(col, matrix[i].c)) kostil.insert(i);

                if (init(vec, u) != init(vec, v)) {
                    weig[i] = 0;
                    dop_arr.push_back(i);
                }
            }
        }

        for (;;) {
            if (!dop_arr.size()) break;
            int e = dop_arr.back();
            dop_arr.pop_back();
            for (int i : matrix_c[e]) {
                if (weig[e] >= weig[i] - 1) continue;
                p_e[i] = e;
                weig[i] = weig[e] + 1;
                dop_arr.push_back(i);
            }
        }

        int index = -1;
        for (int e : kostil) {
            if (weig[e] < dop) index = e;
            dop = std::min((int)dop, weig[e]);
        }

        if (index == -1) break;
        while (index != -1) {
            if (finded(set, index)) set.erase(index);
            else set.insert(index);
            index = p_e[index];
        }
    }

    out << set.size() << "\n";
    for (int i : set) {
        out << i + 1 << " ";
    }
    return 0;
}
