#include <iostream>
#include <fstream>
#include <string>
#include <vector>

typedef unsigned long long ull;

int main() {
    ull n;
    std::ifstream input("input.txt");
    std::ofstream output("output.txt");
    input >> n;
    std::vector<ull> mass(n);
    mass[0] = 1;    //1 el
    mass[1] = 1;    //2 el
    for (int i = 2; i < n; i++) {
        mass[i] = mass[i - 1] + mass[i - 2];
    }
    output << mass[n - 1];
    input.close();
    output.close();
    return 0;
}