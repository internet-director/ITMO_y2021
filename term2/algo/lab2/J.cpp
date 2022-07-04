#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <ctime>

typedef long long ll;
struct node {
	ll data, sz = 1;
	bool rev = false;
	uint32_t p = rand();
	node* l = 0, * r = 0;
	node (ll data): data(data) {}
	ll size() { return sz; }
	void update() {
		sz = 1;
		if (l) sz += l->size();
		if (r) sz += r->size();
	}
	void push() {
		if (rev) {
			rev = false;
			if (l) l->rev ^= true;
			if (r) r->rev ^= true;
			std::swap(l, r);
		}
	}
};
typedef node* pnode;
typedef std::pair<pnode, pnode> pps;

ll sz(pnode p) {
	if (p) return p->sz;
	return 0;
}
void push(pnode p) {
	if (p) p->push();
}
pnode morg(pnode l, pnode r) {
	push(l);
	push(r);
	if (!l || !r) return l ? l : r;
	if (l->p > r->p) {
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
	push(p);
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
pnode rev(pnode p, ll l, ll r) {
	if (l == r) return p;
	pps a = sp(p, l);
	pps b = sp(a.second, r - l + 1);
	b.first->rev ^= true;
	p = morg(a.first, b.first);
	p = morg(p, b.second);
	return p;
}
void print(pnode p) {
	if (!p) return;
	push(p);
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
		p = add(p, i, i + 1);
	}
	std::string s;

	for (ll i = 0; i < m; i++) {
		std::cin >> l >> k;
		p = rev(p, l - 1, k - 1);
	}
	print(p);
	return 0;
}