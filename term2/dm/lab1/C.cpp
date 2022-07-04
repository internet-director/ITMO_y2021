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

#define F(X) X.first
#define S(X) X.second

std::istream& operator>>(std::istream& in, meta& ph)
{
    return in >> ph.start >> ph.end >> ph.c;
}

std::ostream& operator<<(std::ostream& os, const meta& ph)
{
    return os << ph.start << " " << ph.end << " " << ph.c;
}

void init_check(int k, std::vector<pint>& check, mint& data) {
    check[k].first = 1;
    int l;
    for (int i = 0; i < 26; i++) {
        if ((l = data[k][i]) != 0) {
            if (!check[l].first) init_check(l, check, data);
        }
    }
}

void init(int& n, int& m, int& k, int& l, std::vector<bool>& cond, mint& data, mint& kostil, std::vector<pint>& check, std::ifstream& input) {
    input >> n >> m >> k;
    cond = std::vector<bool>(n + 1, false);
    data = mint(n + 1, std::vector<uint64_t>(26, 0));
    kostil = mint(n + 1, std::vector<uint64_t>(26, 0));
    check = std::vector<pint>(n + 1, pint(0, 0));
    for (int i = 0; i < k; i++) {
        int pos;
        input >> pos;
        cond[pos] = true;
    }
    for (int i = 0; i < m; i++) {
        int a, b;
        char c;
        input >> a >> b >> c;
        data[a][c - 'a'] = b;
        kostil[b][c - 'a'] = a;
    }
    for (int i = 0; i < n; i++) 
        if(cond[i + 1]) init_check(i + 1, check, kostil);
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

bool isInf(int k, std::vector<pint>& check, mint& data) {
    S(check[k]) = INT32_MAX;
    int l;

    for (int i = 0; i < 26; i++) {
        if ((l = data[k][i]) != 0) {
            if (!S(check[l])) if (isInf(l, check, data)) return true;
            if (F(check[l]) && S(check[l]) == INT32_MAX) return true;
        }
    }
    S(check[k]) = INT32_MIN;
    return false;
}

uint64_t size(int k, std::vector<pint>& check, std::vector<bool>& cond, mint& data) {
    if (!F(check[k])) return 0;
    uint64_t res = 0;
    if (cond[k]) res++;
    int l;
    for (int i = 0; i < 26; i++) {
        if ((l = data[k][i]) != 0 ) {
            res += size(l, check, cond, data);
            res = (res % 1000000007);
        }
    }
    return (res % 1000000007);
}

int main() {
    std::ifstream input("problem3.in");
    std::ofstream output("problem3.out");
    //input = std::ifstream(stdin);
    //output = std::ofstream(stdout);

    std::vector<bool> cond;
    std::vector<pint>check;
    mint data, kostil;
    int n, m, k, l;
    init(n, m, k, l, cond, data, kostil, check, input);

    if (isInf(1, check, data)) {
        output << -1;
        return 0;
    }

    output << size(1, check, cond, data);

    return 0;

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