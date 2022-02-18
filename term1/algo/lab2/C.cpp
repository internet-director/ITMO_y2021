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
    std::vector<ull> a(n + 1);
    a[0] = 1;
    a[1] = 2;

    for (int i = 2; i <= n; i++) {
        a[i] = a[i - 1] + a[i - 2];
    }
    output << a[n];
    input.close();
    output.close();
    return 0;
}