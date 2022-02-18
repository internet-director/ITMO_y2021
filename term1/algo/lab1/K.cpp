#include <algorithm>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>
#include <map>

struct meta {
    int num;
    int min;
    meta(int a, int b) : num(a), min(b) {}
};

int minim(int a, int b) {
    if (a < b) return a;
    return b;
}

class stack {
    std::vector<meta> data;

public:
    void add(int num) {
        if (data.size())  data.push_back(meta(num, minim(num, data[data.size() - 1].min)));
        else data.push_back(meta(num, num));
    }
    int pop() {
        int num = data[data.size() - 1].num;
        data.erase(data.end() - 1);
        return num;
    }
    int min() {
        if (data.size()) return data[data.size() - 1].min;
        return -1;
    }
    int size() {
        return data.size();
    }
};

class queue {
    stack p1, p2;

public:
    void add(int num) {
        p1.add(num);
    }
    void pop() {
        if (!p2.size()) {
            while (p1.size()) {
                p2.add(p1.pop());
            }
        }
        p2.pop();
    }
    int min() {
        int m1 = p1.min();
        int m2 = p2.min();
        if (m1 == -1) return m2;
        else if (m2 == -1) return m1;
        return minim(m1, m2);
    }
};

int main() {
    int n, k;
    std::cin >> n;
    queue data;
    char c;
    std::vector<int> result;

    for (int i = 0; i < n; i++) {
        std::cin >> c;
        if (c == '+') {
            std::cin >> k;
            data.add(k);
        }
        else data.pop();
        result.push_back(data.min());
    }

    for (int p : result) std::cout << p << "\n";

    return 0;
}