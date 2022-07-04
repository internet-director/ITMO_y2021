#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <stack>
#include <queue>
#include <set>
#include <map>

struct meta {
    int start;
    int end;
    char c;
    meta() {}
    meta(int s, int e, char c) : start(s), end(e), c(c) {}
    bool operator ==(const meta& a) const {
        if (a.c != this->c) return false;
        return true;
    }
    bool operator !=(const meta& a) const {
        return !this->operator==(a);
    }
    bool operator <(const meta& a) const {
        if (a.c < this->c) return true;
        return false;
    }
};
typedef std::vector<meta> vec;
typedef std::pair<int, int> pint;
typedef std::vector<std::vector<uint64_t>> mint;

std::istream& operator>>(std::istream& in, meta& ph)
{
    return in >> ph.start >> ph.end >> ph.c;
}

std::ostream& operator<<(std::ostream& os, const meta& ph)
{
    return os << ph.start << " " << ph.end << " " << ph.c;
}

void init(int& n, int& m, int& k, int& l, std::vector<bool>& cond, mint& data, std::ifstream& input) {
    input >> n >> m >> k >> l;
    cond = std::vector<bool>(n + 1, false);
    data = mint(n, std::vector<uint64_t>(n, 0));
    for (int i = 0; i < k; i++) {
        int pos;
        input >> pos;
        cond[pos] = true;
    }
    for (int i = 0; i < m; i++) {
        int a, b;
        char c;
        input >> a >> b >> c;
        data[a - 1][b - 1]++;
    }
}

void fast(mint& data, mint& kostil) {
    mint dop = mint(data.size(), std::vector<uint64_t>(data.size(), 0));

    for (int i = 0; i < dop.size(); i++) {
        for (int j = 0; j < dop.size(); j++) {
            for (int k = 0; k < dop.size(); k++) {
                dop[i][j] += (data[i][k] * kostil[k][j] % 1000000007);
                dop[i][j] = (dop[i][j] % 1000000007);
            }
        }
    }

    data = dop;
}

void pow(mint& data, int step) {
    mint dop = mint(data.size(), std::vector<uint64_t>(data.size(), 0));
    for (int i = 0; i < dop.size(); i++) dop[i][i] = 1;

    while (step) {
        if (step % 2 == 1)
        {
            fast(dop, data);
            step--;
        }
        else {
            fast(data, data);
            step /= 2;
        }
    }
    data = dop;
}

int main() {
    std::ifstream input("problem4.in");
    std::ofstream output("problem4.out");
    //input = std::ifstream(stdin);
    //output = std::ofstream(stdout);

    std::vector<bool> cond;
    mint data;
    int n, m, k, l;
    init(n, m, k, l, cond, data, input);
    pow(data, l);
    uint64_t res = 0;
    for (int i = 0; i < n; i++) {
        if (cond[i + 1]) {
            res += data[0][i];
            res = (res % 1000000007);
        }
    }
    output << (res) % 1000000007;
    return 0;
}