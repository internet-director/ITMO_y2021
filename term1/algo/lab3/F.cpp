#include <iostream>
#include <vector>

int max(std::vector<int>& vec) {
    int max = vec[0];
    for (int i = 1; i < vec.size(); i++)
        if (max < vec[i]) max = vec[i];
    return max;
}

int main() {
    int n, m;
    std::cin >> n;
    std::vector<int> data(n);
    for (int i = 0; i < n; i++) std::cin >> data[i];

    m = max(data);
    std::vector<int> prime, mp(m + 1, -1);
    std::vector<bool> check(m + 1, false);
    for (int i = 2; i <= m; i++) {
        if (mp[i] == -1) {
            prime.push_back(i);
            check[i] = true;
            mp[i] = i;
        }
        for (int p : prime) {
            if (p > mp[i] || i * p > m) break;
            mp[i * p] = p;
        }
    }
    for (int i = 0; i < n; i++) {
        if (check[data[i]]) std::cout << "YES";
        else std::cout << "NO";
        std::cout << '\n';
    }

    return 0;
}