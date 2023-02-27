#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <stack>


int main() {
    int n;
    std::cin >> n;
    std::deque<int> arr(1);

    for (int i = 1; i < n; i++) {
        int l = -1, r = arr.size();
        while (r - l > 1) {
            int m = (l + r) / 2;
            std::cout << "1 " << arr[m] + 1 << " " << i + 1 << "\n";
            std::cout.flush();
            std::string str;
            std::cin >> str;
            if (str == "YES") {
                l = m;
            }
            else {
                r = m;
            }
        }
        arr.insert(arr.begin() + r, i);
    }

    std::cout << "0 ";
    for (auto i : arr) {
        std::cout << i + 1 << " ";
    }
    return 0;
}