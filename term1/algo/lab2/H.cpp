#include <algorithm>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>
#include <map>

int main() {
    std::string s1, s2;
    std::cin >> s1 >> s2;

    std::vector<std::vector<int>> dist(s1.size() + 1, std::vector<int>(s2.size() + 1));

    for (int i = 0; i <= s1.size(); i++) dist[i][0] = i;
    for (int i = 0; i <= s2.size(); i++) dist[0][i] = i;

    for (int i = 1; i <= s1.size(); i++) {
        for (int j = 1; j <= s2.size(); j++) {
            int dop = 0;
            if (s1[i - 1] != s2[j - 1]) dop++;
            dist[i][j] = std::min(std::min(dist[i - 1][j] + 1, dist[i][j - 1] + 1), dist[i - 1][j - 1] + dop);
        }
    }
    std::cout << dist[s1.size()][s2.size()];
    return 0;
}