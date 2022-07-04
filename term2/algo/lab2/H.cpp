#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <ctime>

typedef long long ll;
struct node {
	ll data, sz = 1;
	uint32_t p = rand();
	node* l, * r;
	node (ll data): data(data), l(0), r(0) {}
	ll size() { return sz; }
	void update() {
		sz = 1;
		if (l) sz += l->size();
		if (r) sz += r->size();
	}
};
typedef node* pnode;
typedef std::pair<pnode, pnode> pps;

ll sz(pnode p) {
	if (p) return p->sz;
	return 0;
}
pnode morg(pnode l, pnode r) {
	if (!l || !r) return l ? l : r;
	if (l->p >= r->p) {
		r->l = morg(l, r->l);
		r->update();
		return r;
	}
	l->r = morg(l->r, r);
	l->update();
	return l;
}
pps sp(pnode p, ll k) {
	if (!p) return pps(0, 0);
	if (sz(p->l) >= k) {
		pps d = sp(p->l, k);
		p->l = d.second;
		p->update();
		return pps(d.first, p);
	}
	pps d = sp(p->r, k - sz(p->l) - 1);
	p->r = d.first;
	p->update();
	return pps(p, d.second);
}
pnode add(pnode p, ll pos, ll k)
{
	pps dop = sp(p, pos);
	pnode n = new node(k);
	return morg(morg(dop.first, n), dop.second);
}
pnode del(pnode p, ll pos) {
	pps a = sp(p, pos);
	pps b = sp(a.second, 1);
	p = morg(a.first, b.second);
	delete b.first;
	return p;
}
void print(pnode p) {
	if (!p) return;
	print(p->l);
	std::cout << p->data << " ";
	print(p->r);
}

int main() {
	std::ios_base::sync_with_stdio(0);
	srand(time(0));
	pnode p = nullptr;
	ll n, m, k, l;
	std::cin >> n >> m;
	for (ll i = 0; i < n; i++) {
		std::cin >> k;
		p = add(p, i, k);
	}
	std::string s;

	for (ll i = 0; i < m; i++) {
		std::cin >> s >> k;
		if (s == "add") {
			std::cin >> l;
			p = add(p, k, l);
		}
		else p = del(p, k - 1);
	}
	std::cout << sz(p) << "\n";
	print(p);
	std::cout << "\n";
	return 0;
}