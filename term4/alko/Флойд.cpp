#include <iostream>
#include <fstream>
#include <memory>
#include <cstring>
#include <string_view>
#include <vector>
#include <string>
#include <queue>
#include <stack>
#include <set>
#include <variant>
#include <map>



int main(int argc, char** argv) {
    std::uint64_t n;
    std::cin >> n;
    std::vector<std::vector<std::int64_t>> matrix(n, std::vector<std::int64_t>(n)), d;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            std::cin >> matrix[i][j];
        }
    }
    
    d = matrix;

    for (int i = 0; i < n; i++) {
        for (int u = 0; u < n; u++) {
            for (int v = 0; v < n; v++) {
                matrix[u][v] = std::min(matrix[u][i] + matrix[i][v], matrix[u][v]);
                //if (matrix[u][i] + matrix[i][v] < matrix[u][v]) d[u][v] = d[u][i];
            }
        }
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            std::cout << matrix[i][j] << " ";
        }
        std::cout << "\n";
    }

    return 0;
}