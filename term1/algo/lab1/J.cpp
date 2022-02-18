#include <iostream>
#include <algorithm>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>
#include <map>

typedef std::vector<std::string> v;

struct meta {
    int start = 0;
    int end = 0;
};

meta findsl(std::string& str) {
    meta d;
    for (int i = 0; i < str.size(); i++) {
        if (str[i] != ' ') {
            d.start = i;
            break;
        }
    }
    for (int i = 0; i < str.size(); i++) {
        if (str[i] == '/') {
            d.end = i;
            break;
        }
    }
    return d;
}

void rec() {

}

int main() {
    long long n;
    std::cin >> n;
    v vec(n);
    v result(n);

    for (int i = 0; i < n; i++) std::cin >> vec[i];
    std::sort(vec.begin(), vec.end());

    for (int i = 0; i < vec.size(); i++) {
        //int num = findsl(vec, result);
        rec();
        std::string last;
        long long c = 0;
        bool checker = true;
        for (int j = vec[i].size() - 1; j >= 0; j--) {
            if (vec[i][j] != '/' && checker) last = vec[i][j] + last;
            else checker = false;
            if (vec[i][j] == '/') c++;
        }

        std::string dop;
        for (int j = 0; j < c; j++) dop += "  ";
        std::cout << dop << last << "\n";
    }
    return 0;
}