#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <ctime>
#include <map>

typedef long long ll;
struct node {
	ll data = 0, num = 0;
	ll p = 0;
	node* l = 0, * r = 0, * par = 0;
	node(ll data, ll p, ll num) : data(data), l(0), r(0), p(p), num(num) {}
	node(ll data, ll p, ll num, node* par) : data(data), l(0), r(0), p(p), num(num), par(par) {}
};

template <typename T1, typename T2, typename T3>
class triple {
public:
	// why not?
	constexpr explicit triple() {

	}
	constexpr explicit triple(const T1& a, const T2& b, const T3& c) {
		this->a = a;
		this->b = b;
		this->c = c;
	}

	T1 a;
	T2 b;
	T3 c;
};

typedef node* pnode;
typedef std::pair<int, int> pint;
typedef std::pair<pnode, pnode> pps;
typedef std::vector<triple<ll, ll, ll>> vec;

pnode morg(pnode l, pnode r) {
	if (!l || !r) return l ? l : r;
	if (l->p >= r->p) {
		r->l = morg(l, r->l);
		return r;
	}
	l->r = morg(l->r, r);
	return l;
}
pps sp(pnode p, ll k) {
	if (!p) return pps(0, 0);
	pps ret;
	if (p->data <= k) {
		pps d = sp(p->r, k);
		p->r = d.first;
		ret = pps(p, d.second);
	}
	else {
		pps d = sp(p->l, k);
		p->l = d.second;
		ret = pps(d.first, p);
	}
	return ret;
}
pnode add(pnode p, ll k, ll s, ll n)
{
	pps a = sp(p, k);
	pps b = sp(a.second, 1);
	if (!b.first) b.first = new node(k, s, n);
	return morg(a.first, morg(b.first, b.second));
}
pnode fast(vec data) {
	pnode start = new node(data[0].a, data[0].b, data[0].c, 0);
	for (int i = 1; i < data.size(); i++) {
		if (data[i].b > start->p) {
			start->r = new node(data[i].a, data[i].b, data[i].c, start);
			start = start->r;
		}
		else {
			pnode dop = start;
			while (dop->par && dop->p > data[i].b) dop = dop->par;
			if (dop->p < data[i].b) {
				start = new node(data[i].a, data[i].b, data[i].c, dop);
				start->l = dop->r;
				dop->r->par = start;
				dop->r = start;
			}
			else {
				start = new node(data[i].a, data[i].b, data[i].c, 0);
				start->l = dop;
				dop->par = start;
			}
		}
	}
	while (start->par) start = start->par;
	return start;
}
void print(pnode p, pnode kostil, std::vector<std::string>& res) {
	if (!p) return;

	print(p->l, p, res);
	print(p->r, p, res);
	std::string a, b, c;
	kostil ? a = std::to_string(kostil->num) : a = "0";
	p->l ? b = std::to_string(p->l->num) : b = "0";
	p->r ? c = std::to_string(p->r->num) : c = "0";
	res[p->num - 1] = a + " " + b + " " + c;
}
bool cmp(const triple<ll, ll, ll>& a, const triple<ll, ll, ll>& b) {
	return a.a < b.a;
}
int main() {
	std::ios_base::sync_with_stdio(0);
	ll n, c, k, y = 0;
	std::cin >> n;
	pnode p = nullptr;
	std::vector<std::string> res(n);
	std::vector<triple<ll, ll, ll>> data;

	for (int i = 0; i < n; i++) {
		std::cin >> k >> y;
		data.push_back(triple<ll, ll, ll>(k, y, i + 1));
	}
	std::sort(data.begin(), data.end(), cmp);
	p = fast(data);

	std::cout << "YES\n";
	print(p, 0, res);

	for (int i = 0; i < n; i++) {
		std::cout << res[i] << "\n";
	}
	return 0;
}