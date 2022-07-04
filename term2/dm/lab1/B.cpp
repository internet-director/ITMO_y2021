#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <set>

struct meta {
    int start;
    int end;
    char c;
    meta() {}
    meta(int s, int e, char c) : start(s), end(e), c(c) {}
};
struct prev {
    int pos;
    int f;
    int s;
    prev(int pos, int f, int s) : pos(pos), f(f), s(s) { }
};
typedef std::vector<meta> vec;
typedef std::vector<prev> prv;

std::istream& operator>>(std::istream& in, meta& ph)
{
    return in >> ph.start >> ph.end >> ph.c;
}

std::ostream& operator<<(std::ostream& os, const meta& ph)
{
    return os << ph.start << " " << ph.end << " " << ph.c;
}

bool comp(meta& a, meta& b) {
    return a.start < b.start;
}
bool cmp(int a, int b) {
    return a < b;
}

int find(const vec& v, int p, int start = 0) {
    for (int i = start; i < v.size(); i++) {
        if (v[i].start >= p) return (v[i].start > p ? -1 : i);
    }
    return -1;
}
bool find(const std::vector<int>& v, int p) {
    for (int i = 0; i < v.size(); i++) {
        if (v[i] >= p) return v[i] == p;
    }
    return false;
}

int main() {
    std::ifstream input("problem2.in");
    std::ofstream output("problem2.out");
    //input = std::ifstream(stdin);
    //output = std::ofstream(stdout);

    std::string word;
    std::vector<int> cond;
    vec tran;
    int n, m, k;
    input >> word >> n >> m >> k;
    cond.resize(k);
    tran.resize(m);
    for (int i = 0; i < k; i++) input >> cond[i];
    for (int i = 0; i < m; i++) input >> tran[i];
    std::sort(tran.begin(), tran.end(), comp);
    std::sort(cond.begin(), cond.end(), cmp);

    std::vector<std::set<int>> data(word.size());
    bool res = true;
    for (int i = 0; i < word.size(); i++) {
        for (int j = 0; j < tran.size(); j++) {
            if (!i) {
                if (tran[j].start == 1 && tran[j].c == word[i]) {
                    data[i].insert(tran[j].end);
                }
            }
            else {
                if (data[i - 1].find(tran[j].start) != data[i - 1].end() && tran[j].c == word[i]) {
                    data[i].insert(tran[j].end);
                }
            }
        }
        if (!data[i].size()) {
            res = false;
            break;
        }
    }
    int c = 0;
    for (int i = 0; i < cond.size(); i++) {
        if (data[data.size() - 1].end() != data[data.size() - 1].find(cond[i])) {
            c = 1;
            break;
        }
    }

    output << ((res && c) ? "Accepts" : "Rejects");
    return 0;
}