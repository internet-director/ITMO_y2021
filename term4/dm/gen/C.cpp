#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

#define MOD 998244353

int main() {
    int k;
    std::cin >> k;
    std::vector<int> q(k + 1), a(k);
    q[0] = 1;
    for (int i = 0; i < k; i++) {
        std::cin >> a[i];
    }
    for (int i = 0; i < k; i++) {
        std::cin >> q[i + 1];
        q[i + 1] *= -1;
    }

    while (!q[q.size() - 1]) q.pop_back();

    //P
    {
        std::vector<int> p(q.size() - 1);
        for (int i = 0; i < q.size() - 1; i++) {
            for (int j = 0; j <= i; j++) {
                p[i] += a[j] * q[i - j];
            }
        }
        while (!p[p.size() - 1]) p.pop_back();
        std::cout << p.size() - 1 << "\n";
        for (int i: p) {
            std::cout << i << " ";
        }
    }

    //Q
    {
        std::cout << "\n" << q.size() - 1 << "\n";
        for (int i: q) {
            std::cout << " " << i;
        }
    }

    return 0;
}
