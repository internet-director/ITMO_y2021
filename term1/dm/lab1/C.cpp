#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <bitset>
#include <cmath>

typedef std::vector<int> vec;

struct meta {
    int znach;
    vec i;
    vec p;

    meta(vec a, vec b) : p(a), i(b) {}
    meta() {}
};

void i2b(vec& bin, int n) {
    /*if (n == 0) {
        bin = vec(n, 0);
        return;
    }*/
    int i = n;
    do {
        i--;
        bin[i] = bin[i] ^ 1;
    } while (bin[i] != 1);
}

int b2i(std::string bin) {
    return std::bitset<32>(bin).to_ulong();
}

int main() {
    int n, col = 0;
    std::cin >> n;
    std::vector<meta> ist_per;
    std::vector<int> dep(n, 0);

    for (int i = 0; i < n; i++) {
        int k;
        std::cin >> k;
        if (k) {
            std::vector<int> a(k);
            std::vector<int> b(1 << k);

            for (int j = 0; j < k; j++) {
                std::cin >> a[j];
                a[j]--;
            }
            for (int j = 0; j < (1 << k); j++) {
                std::cin >> b[j];
            }
            ist_per.push_back(meta(a, b));
        }
        else {
            col++;
            ist_per.push_back(meta());
        }
    }

    for (int i = 0; i < n; i++) {
        if (!ist_per[i].p.size()) {
            continue;
        }
        int size = dep[i];
        for (int j = 0; j < ist_per[i].p.size(); j++) {
            size = std::max(size, dep[ist_per[i].p[j]]);
        }
        dep[i] = size + 1;
    }

    std::cout << dep[n - 1] << "\n";

    vec bin(col, 0);
    std::vector<int> result(1 << col);
    std::vector<int> kostyl(n);

    for (int i = 0; i < 1 << col; i++) {
        if (i) i2b(bin, col);
        int dop = 0;
        for (int j = 0; j < n; j++) {
            if (!ist_per[j].p.size()) {
                kostyl[j] = bin[dop];
                dop++;
                continue;
            }
            int p = 1;
            int s = 0;
            for (int m = ist_per[j].p.size() - 1; m >= 0; m--) {
                s += (p * kostyl[ist_per[j].p[m]]);
                p <<= 1;
            }
            kostyl[j] = ist_per[j].i[s];
        }
        result[i] = kostyl[n - 1];
    }

    for (int i = 0; i < 1 << col; i++) {
        std::cout << result[i];
    }
    return 0;
}