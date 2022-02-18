#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
std::string print(std::vector<int>& vec) {
    std::string res;
    for (int p : vec) {
        res += std::to_string(p) + " ";
    }
    return res;
}
 
std::string next(std::vector<int> vec, int n, int k) {
    std::vector<int> b(k + 1);
    for (int i = 0; i < k; i++) b[i] = vec[i];
    b[k] = n + 1;
    int i = k - 1;
    while (i >= 0 && (b[i + 1] - b[i] < 2)) i--;
    if (i >= 0) {
        b[i]++;
        for (int j = i + 1; j < k; j++) b[j] = b[j - 1] + 1;
        for (int j = 0; j < k; j++) vec[j] = b[j];
        return print(vec);
    }
    else {
        std::cout << -1;
        exit(0);
    }
}
 
int main() {
    int n, k;
    std::cin >> n >> k;
    std::vector<int> data(k);
    for (int i = 0; i < k; i++)std::cin >> data[i];
    std::cout << next(data, n, k);
    return 0;
}