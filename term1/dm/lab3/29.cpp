#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
int n;
 
std::string print(std::vector<int>& vec) {
    std::string res;
    for (int p = 0; p < vec.size() - 1; p++) {
        res += std::to_string(vec[p]) + "+";
    }
    return res + std::to_string(vec[vec.size() - 1]);
}
 
std::string next(std::vector<int> vec) {
    vec[vec.size() - 1]--;
    vec[vec.size() - 2]++;
    if (vec[vec.size() - 2] > vec[vec.size() - 1]) {
        vec[vec.size() - 2] += vec[vec.size() - 1];
        vec.erase(vec.end() - 1);
    } 
    else {
        while (vec[vec.size() - 2] * 2 <= vec[vec.size() - 1]) {
            vec.push_back(vec[vec.size() - 1] - vec[vec.size() - 2]);
            vec[vec.size() - 2] = vec[vec.size() - 3];
        }
    }
    return print(vec);
}
 
int main() {
    std::string input, dop;
    std::cin >> input;
    std::vector<int> numbers;
    for (int i = 0; i < input.size(); i++) {
        if (input[i] == '=' || input[i] == '+') {
            numbers.push_back(std::stoi(dop));
            dop = "";
        }
        else dop += input[i];
    }
    if (dop.size()) numbers.push_back(std::stoi(dop));
    n = numbers[0];
    numbers.erase(numbers.begin());
    if (numbers.size() == 1) std::cout << "No solution";
    else std::cout << n << "=" << next(numbers);
    return 0;
}