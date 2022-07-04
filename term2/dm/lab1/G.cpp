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
typedef std::vector<std::vector<int>> mint;

std::istream& operator>>(std::istream& in, meta& ph)
{
    return in >> ph.start >> ph.end >> ph.c;
}

std::ostream& operator<<(std::ostream& os, const meta& ph)
{
    return os << ph.start << " " << ph.end << " " << ph.c;
}

void init(int& n, int& m, int& k, std::vector<bool>& cond, mint& data, std::ifstream& input) {
    input >> n >> m >> k;
    cond = std::vector<bool>(n + 1, false);
    data = mint(n + 1, std::vector<int>(26, 0));
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
    }
}

int main() {
    std::ifstream input("equivalence.in");
    std::ofstream output("equivalence.out");
    //input = std::ifstream(stdin);
    //output = std::ofstream(stdout);

    std::vector<bool> cond;
    mint data;
    int n, m, k;
    init(n, m, k, cond, data, input);

    std::vector<bool> cond2;
    mint data2;
    int n2, m2, k2;
    init(n2, m2, k2, cond2, data2, input);

    bool res = true;

    std::queue<pint> st;
    std::vector<std::vector<bool>> check(n + 1, std::vector<bool>(n2 + 1, false));
    //std::cerr << "\n";

    st.push({ 1, 1 });
    while (!st.empty()) {
        pint p = st.front();
        st.pop();
        if (cond[p.first] != cond2[p.second]) {
            res = false;
            break;
        }
        check[p.first][p.second] = true;

        for (int i = 0; i < 26; i++) {
            int k1 = data[p.first][i];
            int l1 = data2[p.second][i];

            if (!check[k1][l1]) {
                st.push({ k1, l1 });
            }
        }
    }
    output << (res ? "YES" : "NO");
    return 0;
}