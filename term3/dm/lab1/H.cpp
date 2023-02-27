#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <stack>
#include <set>


int get_s(int n, int k) {
    if (n == 0 && k == 0)
        return 1;
    if (k == 0 && n > 0)
        return 0;
    if (n == 0 && k > 0)
        return 0;
    return get_s(n - 1, k - 1) - (n - 1) * get_s(n - 1, k);
}

struct meta {
    std::vector<std::vector<bool>> vec;
    int64_t m = 0;
    int64_t n = 0;

    meta(int64_t n, int64_t m) : n(n), m(m) {
        vec.resize(n + 1);
        for (int i = 1; i < n + 1; i++) {
            vec[i].resize(i + 1);
        }
    }
    meta(const meta& m) : n(m.n), m(m.m), vec(m.vec) { }

    bool is_full() {
        return (n * (n - 1) / 2) == m;
    }

    bool is_empty() {
        return m == 0;
    }
};

int check(meta& matrix, int i, int j) {
    if (i < j) std::swap(i, j);
    return matrix.vec[i][j];
}

void erase(meta& matrix, int i, int j) {
    if (i < j) std::swap(i, j);

}

void init(meta& matrix, int i, int j, bool set = true) {
    if (i < j) std::swap(i, j);
    matrix.vec[i][j] = set;
}

void calc(meta& matrix, std::vector<int>& res) {
    if (matrix.is_empty()) {
        res[matrix.n]++;
        return;
    }
    else if (matrix.is_full()) {
        for (int i = 0; i <= matrix.n; i++) {
            res[i] += get_s(matrix.n, i);
        }
        return;
    }


    meta mat1(matrix), mat2(matrix);

    int i1 = -1, j1 = -1;

    for (int i = 1; i < matrix.vec.size(); i++) {
        for (int j = 1; j < matrix.vec[i].size() - 1; j++) {
            if (!check(matrix, i, j)) {
                i1 = i;
                j1 = j;
                goto skip;
            }
        }
    }
skip:;

    init(mat1, i1, j1);
    mat1.m++;

    //std::cout << mat2.n << " " << mat2.vec.size() << "\n";

    for (int i = 1; i <= mat2.n; i++) {
        if (check(mat2, i, j1)) {
            if (check(mat2, i1, i)) mat2.m--;
            else init(mat2, i1, i);
        }
        if (mat2.vec[i].size() > j1) mat2.vec[i].erase(mat2.vec[i].begin() + j1);
    }
    mat2.vec.erase(mat2.vec.begin() + j1);
    mat2.n--;

    calc(mat1, res);
    calc(mat2, res);
}

int main() {
    int n, m;
    std::cin >> n >> m;

    meta mat(n, m);

    for (int i = 0; i < m; i++) {
        int a, b;
        std::cin >> a >> b;
        init(mat, a, b);
    }

    std::vector<int> result(n + 1);

    calc(mat, result);
    std::reverse(result.begin(), result.end());
    int count = 0;
    for (; count < n + 1; count++) {
        if (result[count] != 0) {
            break;
        }
    }

    std::cout << n - count << "\n";

    for (int i = count; i < result.size(); i++) {
        std::cout << result[i] << " ";
    }


    return 0;
}