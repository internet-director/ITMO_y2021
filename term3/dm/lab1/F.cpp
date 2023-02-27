#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <stack>
#include <set>

int main() {
    int n;
    std::cin >> n;
    std::vector<std::vector<int>> vec(n + 1);
    std::vector<int> pruf(n - 2), deg(n + 1, 1);
    std::set<int> set;

    for (int i = 0; i < n - 2; i++) {
        std::cin >> pruf[i];
        deg[pruf[i]]++;
    }
        
    for (int i = 1; i <= n; i++) {
        if (deg[i] == 1) set.insert(i);
    }

    for (int i = 0; i < n - 2; ++i) {
        int l = *set.begin();
        set.erase(l);

        int dop = pruf[i];
        vec[l].push_back(dop);
        if (deg[dop] == 2) {
            set.insert(dop);
        }
        deg[dop]--;
    }
    vec[*set.begin()].push_back(*--set.end());

    for (int i = 0; i < vec.size(); i++) {
        if (vec[i].size()) {
            for (auto j : vec[i]) {
                std::cout << i << " " << j << "\n";
            }
        }
    }

    return 0;
}