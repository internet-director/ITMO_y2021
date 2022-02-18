#include <iostream>
#include <cmath>
 
std::string next(std::string& vec) {
    for (int i = vec.size() - 1; i >= 0; i--) {
        if (vec[i] == '0') {
            vec[i] = '1';
            return vec;
        }
        else vec[i] = '0';
    }
    return (vec = "-");
}
 
int main() {
    int n;
    std::cin >> n;
    std::string str(n, '0');
    for (long i = 0; i < pow(2, n); i++) {
        std::cout << str << "\n";
        next(str);
    }
    return 0;
}