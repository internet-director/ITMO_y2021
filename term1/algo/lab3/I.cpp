#include <iostream>
#include <algorithm>
#include <vector>
#include <string>

typedef unsigned long long ull;

ull max(std::vector<ull>& vec) {
    ull max = vec[0];
    for (ull i = 1; i < vec.size(); i++)
        if (max < vec[i]) max = vec[i];
    return max;
}

std::string get(std::vector<ull>& vec, ull n) {
    std::string result;
    std::vector<ull> data;
    ull index = 0;

    while (n > 1) {
        data.push_back(vec[n]);
        n /= vec[n];
    }
    std::sort(data.begin(), data.end());
    for(ull p: data) result += std::to_string(p) + " ";
    return result;
}

int main() {
    ull n, m;
    std::cin >> n;
    std::vector<ull> data(n);
    for (ull i = 0; i < n; i++) std::cin >> data[i];

    m = max(data);
    std::vector<ull> prime(m + 1);
    for (ull i = 0; i < m + 1; i++) prime[i] = i;

    for (ull i = 2; i <= m; i++) {
        if (prime[i] == i) {
            for (ull j = i * i; j <= m; j += i) {
                prime[j] = i;
            }
        }
    }

    for (ull num : data) {
        std::cout << get(prime, num) << '\n';
    }

    return 0;
}