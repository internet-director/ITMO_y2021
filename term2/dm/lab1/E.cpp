#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <stack>
#include <queue>
#include <set>
#include <map>

#define MAX_SZ 100

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
typedef std::pair<int, int> pint;
typedef std::vector<std::vector<std::set<int>>> vec;
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

struct kostil {
    std::set<int> start;
    std::set<int> end;
    char c;
    kostil(std::set<int> s, std::set<int> b, char c) : start(s), end(b), c(c) {}
};

void convert(vec& tran, mint& tran2, std::vector<bool>& cond, std::vector<bool>& cond2, int& n, int m) {
    std::queue<std::set<int>> p;
    std::vector<kostil> t_dop;
    std::vector<std::set<int>> q;
    p.push({ 1 });
    q.push_back(p.front());

    while (!p.empty()) {
        std::set<int> dop = p.front();
        p.pop();

        for (int i = 0; i < 26; i++) {
            std::set<int> k;
            for (int l : dop) {
                for (int o : tran[l][i])
                    k.insert(o);
            }

            if (!k.empty()) {
                t_dop.push_back(kostil(dop, k, i));
                if (std::find(q.begin(), q.end(), k) == q.end()) {
                    p.push(k);
                    q.push_back(k);
                }
            }
        }
    }
    n = q.size();
    tran2 = mint(n, std::vector<uint64_t>(n, 0));
    cond2 = std::vector<bool>(n, false);
    for (kostil h : t_dop) {
        int k = std::find(q.begin(), q.end(), h.start) - q.begin();
        int l = std::find(q.begin(), q.end(), h.end) - q.begin();
        tran2[k][l]++;
        for (int i = 1; i < cond.size(); i++) {
            if (cond[i]) {
                if (std::find(h.start.begin(), h.start.end(), i) != h.start.end()) cond2[k] = true;
                if (std::find(h.end.begin(), h.end.end(), i) != h.end.end()) cond2[l] = true;
            }
        }
    }
}

int main() {
    std::ifstream input("problem5.in");
    std::ofstream output("problem5.out");
    //input = std::ifstream(stdin);
    //output = std::ofstream(stdout);

    int n, m, k, l;
    input >> n >> m >> k >> l;
    std::vector<bool> cond(n + 1, false);
    vec tran(n + 1, std::vector<std::set<int>>(26));

    for (int i = 0; i < k; i++) {
        int pos;
        input >> pos;
        cond[pos] = true;
    }
    for (int i = 0; i < m; i++) {
        int a, b;
        char c;

        input >> a >> b >> c;
        tran[a][c - 'a'].insert(b);
    }

    std::vector<bool> cond2;
    mint data2;

    convert(tran, data2, cond, cond2, n, m);

    //for (int i = 0; i < data2.size(); i++) {
    //    for (int j = 0; j < data2.size(); j++) {
    //        std::cout << data2[i][j] << " ";
    //    }
    //    std::cout << "\n";
    //}
    //std::cout << "\n";
    //for (int i = 0; i < cond2.size(); i++) {
    //    std::cout << cond2[i] << " ";
    //}
    //std::cout << "\n\n";

    pow(data2, l);

    /*for (int i = 0; i < data2.size(); i++) {
        for (int j = 0; j < data2.size(); j++) {
            std::cout << data2[i][j] << " ";
        }
        std::cout << "\n";
    }
    std::cout << "\n";*/

    uint64_t res = 0;
    for (int i = 0; i < cond2.size(); i++) {
        if (cond2[i]) {
            res += data2[0][i];
            res = (res % 1000000007);
        }
    }
    output << (res) % 1000000007;
    return 0;
}