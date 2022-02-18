#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
std::string print(std::vector<int>& vec) {
    std::string res;
    for (int p : vec) {
        res += std::to_string(vec[p]) + " ";
    }
    return res;
}
 
std::vector<int> parse(std::string input) {
    std::string dop;
    std::vector<int> numbers;
    for (int i = 0; i < input.size(); i++) {
        if (input[i] == ' ' && dop.size()) {
            numbers.push_back(std::stoi(dop));
            dop = "";
        }
        else dop += input[i];
    }
    if (dop.size()) numbers.push_back(std::stoi(dop));
    return numbers;
}
 
int max(std::vector<int>& vec) {
    int max = vec[0];
    for (int i : vec)
        if (i > max) max = i;
    return max;
}
 
int min(std::vector<int>& vec, int num) {
    int m = max(vec);
    int index = 0;
    for (int i = 0; i < vec.size(); i++) {
        if ((vec[i] <= m) && (vec[i] > num)) {
            m = vec[i];
            index = i;
        }
    }
    return index;
}
 
std::vector<std::vector<int>> next(std::vector<std::vector<int>>& vec) {
    std::vector<int> dop;
    bool fl = false;
 
    for (int i = vec.size() - 1; i >= 0; i--) {
        if (dop.size() != 0 && (max(dop) > vec[i][vec[i].size() - 1])) {
            int pos = min(dop, vec[i][vec[i].size() - 1]);
            int m = dop[pos];
            vec[i].push_back(m);
            dop.erase(dop.begin() + pos);
            break;
        }
        for (int j = vec[i].size() - 1; j >= 0; j--) {
            if ((dop.size() != 0) && (j != 0) && (max(dop) > vec[i][j])) {
                int pos = min(dop, vec[i][j]);
                int m = dop[pos];
                int old = vec[i][j];
                vec[i][j] = m;
                dop.erase(dop.begin() + pos);
                dop.push_back(old);
                fl = true;
                break;
            }
            else {
                dop.push_back(vec[i][vec[i].size() - 1]);
                vec[i].erase(vec[i].end() - 1);
                if (vec[i].size() == 0) vec.erase(vec.end() - 1);
            }
        }
        if (fl) break;
    }
    std::sort(dop.begin(), dop.end());
    for (int i = 0; i < dop.size(); i++)
        vec.push_back(std::vector<int>(1, dop[i]));
    return vec;
}
 
struct meta {
    int n, k;
    std::vector<std::vector<int>> data;
    meta(std::vector<std::vector<int>>& vec, int n, int k) : data(vec), n(n), k(k){}
};
 
int main() {
    std::vector<meta> result;
    for (;;) {
        std::vector<std::vector<int>> data;
        int n, k;
        std::string vec;
        std::cin >> n >> k;
        std::cin.ignore();
 
        if (n == 0 && k == 0) break;
        for (int i = 0; i < k; i++) {
            std::getline(std::cin, vec);
            //std::cout << vec << "\n";
            data.push_back(parse(vec));
        }
        next(data);
        result.push_back(meta(data, n, k));
    }
 
    for (meta r : result) {
        std::cout << r.n << " " << r.data.size() << "\n";
        for (std::vector<int> i : r.data) {
            for (int p : i) std::cout << p << " ";
            std::cout << "\n";
        }
        std::cout << "\n";
    }
 
    return 0;
}