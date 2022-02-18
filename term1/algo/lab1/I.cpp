#include <iostream>
#include <algorithm>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>

typedef std::vector<int> vec;

void fixIt(vec& a, vec& b, vec& c) {
    b.push_back(a[a.size() - 1]);
    a.erase(a.end() - 1);
    c.push_back(1);
    c.push_back(1);
}

int main() {
    int n;
    std::cin >> n;
    vec input(n), stack, result;
    for (int i = 0; i < n; i++) std::cin >> input[i];
    std::reverse(input.begin(), input.end());

    for (int i = 1; i <= n; i++) {
        if (std::find(input.begin(), input.end(), i) != input.end()) {
            while (input[input.size() - 1] != i) {
                //if (input[input.size() - 1] == i) dop = false;
                fixIt(input, stack, result);
            }
            fixIt(input, stack, result);
        }
        if (stack[stack.size() - 1] != i) {
            std::cout << 0;
            return 0;
        }
        else {
            stack.erase(stack.end() - 1);
            result.push_back(2);
            result.push_back(1);
        }
    }
    for (int i = 0; i < result.size(); i += 2) std::cout << result[i] << " " << result[i + 1] << "\n";

    return 0;
}