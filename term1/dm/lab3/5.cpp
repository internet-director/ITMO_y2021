#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
int main() {
    int n, k;
    std::cin >> n >> k;
    std::vector<std::string> data(k);
    for (int i = 0; i < k; i++) data[i] = '0' + i;
    for (int i = 1; i < n; i++) {
        std::vector<std::string> rev = data, result;
        std::reverse(rev.begin(), rev.end());
        for (int j = 0; j < k; j++) {
            if (j % 2 == 0) result.insert(result.end(), data.begin(), data.end());
            else result.insert(result.end(), rev.begin(), rev.end());
        }
        data = result;
        for (int j = 0; j < k; j++)
            for (int l = j * rev.size(); l < rev.size() * (j + 1); l++) data[l].insert(data[l].begin(), '0' + j);
    }
    for (std::string p : data) std::cout << p << "\n";
    return 0;
}