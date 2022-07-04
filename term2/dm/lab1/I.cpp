#include <algorithm>
#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
#include <set>
#include <map>

struct meta {
    std::map<int, std::vector<int>> rdata;
    std::map<int, int> data;
    int term = 0;
};

typedef std::vector<meta> vec;
typedef std::pair<int, int> pint;
typedef std::pair<int, std::vector<int>> vpp;
typedef std::vector<std::vector<uint64_t>> mint;
typedef std::pair<pint, int> pp;

#define F(X) X.first
#define S(X) X.second
#define LAST(X) X[X.size() - 1]
#define SZ(X) X.size()

void init_check(int k, std::vector<meta>& data, std::vector<int>& check) {
    check[k] = 1;
    for (vpp pair : data[k].rdata) {
        for (int j : pair.second) {
            if (!check[j]) init_check(j, data, check);
        }
    }
}
void init(int& n, int& m, int& k, std::vector<meta>& data, std::vector<meta>& data_rev, std::vector<int>& check, std::ifstream& input) {
    input >> n >> m >> k;
    data = std::vector<meta>(n + 1);
    data_rev = std::vector<meta>(n + 1);
    check = std::vector<int>(n + 1);
    for (int i = 0; i < k; i++) {
        int pos;
        input >> pos;
        data[pos].term = 1;
    }
    for (int i = 0; i < m; i++) {
        int a, b;
        char c;
        input >> a >> b >> c;
        data[a].data[c - 'a'] = b;
        data_rev[b].rdata[c - 'a'].push_back(a);
    }

    for (int i = 0; i < n; i++) {
        if (data[i + 1].term && !check[i + 1]) init_check(i + 1, data_rev, check);
    }
}
std::vector<int> convert(int& n, std::vector<int>& check, std::vector<meta>& data,
    std::vector<meta>& data_rev) {
    std::vector<int> Class(n + 1);
    std::vector<std::set<int>> P;
    std::queue<pint> queue;
    std::set<int> F, Q;

    for (int i = 0; i < n; i++) {
        if (!check[i + 1]) continue;
        if (data[i + 1].term) {
            F.insert(i + 1);
            Class[i + 1] = 0;
        }
        else {
            Q.insert(i + 1);
            Class[i + 1] = 1;
        }
    }
    P.push_back(F);
    P.push_back(Q);

    for (int i = 0; i < 26; i++) {
        queue.push(pint(0, i));
        queue.push(pint(1, i));
    }

    while (!queue.empty()) {
        pint dop = queue.front();
        queue.pop();

        std::map<int, std::vector<int>> involved;

        for (int q : P[dop.first]) {
            for (int r : data_rev[q].rdata[dop.second]) {
                int pos = Class[r];
                if (involved.find(pos) == involved.end()) involved[pos] = std::vector<int>();
                involved[pos].push_back(r);
            }
        }

        for (vpp dat : involved) {
            if (!dat.second.size()) continue;
            if (SZ(involved[dat.first]) < SZ(P[dat.first])) {
                P.push_back(std::set<int>());
                for (int k : involved[dat.first]) {
                    P[dat.first].erase(k);
                    LAST(P).insert(k);
                }
                if (SZ(LAST(P)) > SZ(P[dat.first])) std::swap(LAST(P), P[dat.first]);
                for (int r : LAST(P)) Class[r] = SZ(P) - 1;
                for (int i = 0; i < 26; i++) queue.push(pint(SZ(P) - 1, i));
            }
        }
    }
    n = P.size();
    return Class;
}
void kostil_fix(int k, std::vector<meta>& data_tran, std::vector<meta>& res,
    std::map<int, int>& data, std::vector<int>& check) {
    int pos = data.size();
    res[pos].term = data_tran[k].term;
    check[k] = INT_MIN;
    data[k] = pos;

    for (pint dat : data_tran[k].data) {
        if (check[dat.second] != INT_MIN) kostil_fix(dat.second, data_tran, res, data, check);
        res[pos].data[dat.first] = data[dat.second] + 1;
    }
}

int main() {
    std::ifstream input("fastminimization.in");
    std::ofstream output("fastminimization.out");
    //input = std::ifstream(stdin);
    //output = std::ofstream(stdout);

    std::vector<int> check;
    std::vector<meta> data, data_rev, kostil3;
    int n, m, k, nn;

    init(n, m, k, data, data_rev, check, input);
    nn = n;
    std::vector<int> Class = convert(nn, check, data, data_rev);
    kostil3.resize(nn);

    for (int i = 0; i < n; i++) {
        if (check[i + 1]) {
            for (pint j : data[i + 1].data) {
                if (check[j.second]) kostil3[Class[i + 1]].data[j.first] = Class[j.second];
            }
            if (!kostil3[Class[i + 1]].term && !data[i + 1].term) kostil3[Class[i + 1]].term = 0;
            else kostil3[Class[i + 1]].term = 1;
        }
    }

    std::vector<meta> tran_res(nn);
    std::map<int, int> tran_map;

    kostil_fix(Class[1], kostil3, tran_res, tran_map, check);
    nn = tran_map.size();

    int t = 0, tran = 0;
    for (int i = 0; i < nn; i++) {
        if (tran_res[i].term) t++;
        tran += tran_res[i].data.size();
    }

    output << nn << " " << tran << " " << t << "\n";

    for (int i = 0; i < nn; i++) {
        if (tran_res[i].term) output << i + 1 << " ";
    }
    output << "\n";

    for (int i = 0; i < nn; i++) {
        for (pint dat : tran_res[i].data) {
            output << i + 1 << " " << dat.second << " " << (char)(dat.first + 'a') << "\n";
        }
    }
    return 0;
}