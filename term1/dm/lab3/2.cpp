#include <iostream>
#include <fstream>
#include <string>
#include <vector>
 
void toBin(long long i, int n) {
    std::string s;
    int k = 0;
    while (i != 0 || k < n) {
        s += '0' + i % 2;
        i /= 2;
        k++;
    }
    std::reverse(s.begin(), s.end());
    std::cout << s << "\n";
}
 
int main() {
    int n;
    std::cin >> n;
 
    std::vector<std::string> result;
    result.push_back("0");
    result.push_back("1");
     
    for (long long i = 1; i < n; i++) {
        std::vector<std::string> dop = result;
        std::reverse(dop.begin(), dop.end());
 
        result.insert(result.end(), dop.begin(), dop.end());
        for (long long j = 0; j < result.size() / 2; j++) result[j] += "0";
        for (long long j = result.size() / 2; j < result.size(); j++) result[j] += "1";
    }
 
    for (int i = 0; i < result.size(); i++) {
        std::string d = result[i];
        std::reverse(d.begin(), d.end());
        std::cout << d << "\n";
    }
    return 0;
}