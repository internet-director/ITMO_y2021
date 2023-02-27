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

    for (int i = 0; i < n - 1; i++) {
        int a, b;
        std::cin >> a >> b;
        vec[a].push_back(b);
        vec[b].push_back(a);
    }

    std::set<int> vert;
    std::vector<int> deg(n), res(n - 2);
    std::vector<bool> used(n);

    for (int i = 0; i < n; i++) {
        deg[i] = vec[i].size();
        if (deg[i] == 1) {
            vert.insert(i);
        }
    }


    for (int i = 0; i < n - 2; i++) {
        int l = *vert.begin();
        vert.erase(vert.begin());
        used[l] = true;

        int v;
        for (int j = 0; j < vec[l].size(); j++) {
            if (!used[vec[l][j]]) {
                v = vec[l][j];
            }
        }
        res[i] = v;
        if (deg[v] == 2) {
            vert.insert(v);
        }
        deg[v]--;
    }

    for (auto i : res) {
        std::cout << i << " ";
    }

    return 0;
}