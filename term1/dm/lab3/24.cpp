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
 
std::string last(std::vector<int> vec) {
    for (int i = vec.size() - 2; i >= 0; i--) {
        if (vec[i] > vec[i + 1]) {
            int max = i + 1;
            for (int j = i + 1; j < vec.size(); j++)
                if (vec[j] > vec[max] && vec[j] < vec[i]) max = j;
            std::swap(vec[i], vec[max]);
            std::reverse(vec.begin() + i + 1, vec.end());
            return print(vec);
        }
    }
    std::string res;
    for (int i = 0; i < vec.size(); i++) res += "0 ";
    return res;
}
std::string next(std::vector<int> vec) {
    for (int i = vec.size() - 2; i >= 0; i--) {
        if (vec[i] < vec[i + 1]) {
            int max = i + 1;
            for (int j = i + 1; j < vec.size(); j++)
                if (vec[j] < vec[max] && vec[j] > vec[i]) max = j;
            std::swap(vec[i], vec[max]);
            std::reverse(vec.begin() + i + 1, vec.end());
            return print(vec);
        }
    }
    std::string res;
    for (int i = 0; i < vec.size(); i++) res += "0 ";
    return res;
}
 
 
int main() {
    int n;
    std::cin >> n;
    std::vector<int> data(n);
    for (int i = 0; i < n; i++) std::cin >> data[i];
    std::cout << last(data) << "\n";
    std::cout << next(data);
    return 0;
}