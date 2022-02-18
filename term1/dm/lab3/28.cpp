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
 
std::string next(std::vector<int> vec) {
    int i = vec.size() - 2;
    while (i >= 0 && vec[i] >= vec[i + 1]) i--;
    if (i >= 0) {
        int j = i + 1;
        while (j < vec.size() - 1 && vec[j + 1] > vec[i]) j++;
        std::swap(vec[i], vec[j]);
        std::reverse(vec.begin() + i + 1, vec.end());
        return print(vec);
    }
    else {
        return print((vec = std::vector<int>(vec.size(), 0)));
    }
}
 
int main() {
    int n;
    std::cin >> n;
    std::vector<int> data(n);
    for (int i = 0; i < n; i++)std::cin >> data[i];
    std::cout << next(data);
    return 0;
}