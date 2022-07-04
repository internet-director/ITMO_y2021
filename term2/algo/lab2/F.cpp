#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <ctime>

typedef long long ll;
struct node {
	ll data, sz = 1, sum;
	uint32_t p = rand();
	node* l = 0, * r = 0;
	node(ll data) : data(data), l(0), r(0), sum(data) {}
	ll size() { return sz; }
	void update() {
		sz = 1;
		sum = data;
		if (l) sz += l->size(), sum += l->sum;
		if (r) sz += r->size(), sum += r->sum;
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
	if (l->p <= r->p) {
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
	pps ret;
	if (p->data < k) {
		pps d = sp(p->r, k);
		p->r = d.first;
		ret = pps(p, d.second);
	}
	else {
		pps d = sp(p->l, k);
		p->l = d.second;
		ret = pps(d.first, p);
	}
	ret.first ? ret.first->update() : void();
	ret.second ? ret.second->update() : void();
	return ret;
}
pnode add(pnode p, ll k)
{
	pps a = sp(p, k);
	pps b = sp(a.second, k + 1);
	if (!b.first) b.first = new node(k);
	return morg(a.first, morg(b.first, b.second));
}
pnode del(pnode p, ll pos) {
	pps a = sp(p, pos);
	pps b = sp(a.second, 1);
	p = morg(a.first, b.second);
	delete b.first;
	return p;
}
ll sum(pnode& p, ll l, ll r) {
	pps a = sp(p, l);
	pps b = sp(a.second, r + 1);
	ll res = b.first ? b.first->sum : 0;
	p = morg(a.first, morg(b.first, b.second));
	return res;
}
void print(pnode p) {
	if (!p) return;
	print(p->l);
	std::cout << p->data << " ";
	print(p->r);
}

int main() {
	std::ios_base::sync_with_stdio(0);
	ll n, c, k, y = 0;
	std::cin >> n;
	pnode p = nullptr;
	bool type = false;
	std::string s;
	for (int i = 0; i < n; i++) {
		std::cin >> s >> k;
		if (s == "?") {
			std::cin >> c;
			y = sum(p, k, c);
			std::cout << y << "\n";
			type = true;
		}
		else {
			k = type ? (k + y) % 1000000000 : k % 1000000000;
			type = false;
			p = add(p, k);
		}
	}
	//free(p);
	return 0;
}