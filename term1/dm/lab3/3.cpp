#include <iostream>
#include <fstream>
#include <string>
#include <vector>
 
std::string toTern(long long i, int n) {
    std::string s;
    int k = 0;
    while (i != 0 || k < n) {
        s += '0' + i % 3;
        i /= 3;
        k++;
    }
    std::reverse(s.begin(), s.end());
    return s;
}
 
void update(std::string& str) {
    for (int i = 0; i < str.size(); i++) {
        str[i] = '0' + (str[i] - '0' + 1) % 3;
    }
}
 
int main() {
    int n;
    std::cin >> n;
 
    std::vector<std::string> result;
 
    for (long long i = 0; i < pow(3, n - 1); i++) {
        result.push_back(toTern(i, n - 1));
    }
 
    for (long long j = 0; j < result.size(); j++) result[j] += "0";
 
    std::vector<std::string> d;
    for (long long j = 0; j < result.size(); j++) {
        std::string str = result[j];
        d.push_back(str);
        update(str);
        d.push_back(str);
        update(str);
        d.push_back(str);
    }
    result = d;
 
    for (int i = 0; i < result.size(); i++) {
        std::string d = result[i];
        std::reverse(d.begin(), d.end());
        std::cout << d << "\n";
    }
    return 0;
}