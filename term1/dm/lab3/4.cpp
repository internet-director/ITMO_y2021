#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
int main() {
    int n;
    std::cin >> n;
    std::string current(n, '0');
    std::set<std::string> result;
 
    for(;;) {
        std::string prefix = current.substr(1, current.size());
        std::string d1 = prefix + "0";
        std::string d2 = prefix + "1";
 
        if (result.find(d2) == result.end())  current = d2;
        else if (result.find(d1) == result.end())  current = d1;
        else break;
        result.insert(current);
        std::cout << current << "\n";
    }
    return 0;
}