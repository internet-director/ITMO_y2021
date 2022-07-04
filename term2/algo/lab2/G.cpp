#include <iostream>
#include <algorithm>
#include <string>
#include <vector>

typedef long long ll;

struct node {
	ll data, h, count;
	node* l = nullptr, * r = nullptr;
	node(ll data) : data(data), h(1), count(1) {}
	node& operator!=(const node& b) {
		this->data = b.data;
		this->h = b.h;
		this->l = b.l;
		this->r = b.r;
		return *this;
	}
};
typedef node* pnode;

ll heig(pnode p) {
	return p ? p->h : 0;
}
ll bal(pnode p) {
	return p ? heig(p->l) - heig(p->r) : 0;
}
pnode min(pnode p) {
	pnode t = p;
	while (t->l) t = t->l;
	return t;
}
size_t fixC(pnode p) {
	if (!p) return 0;
	return fixC(p->l) + fixC(p->r) + 1;
}
pnode R(pnode p) {
	pnode l = p->l, r = l->r;
	l->r = p;
	p->l = r;
	p->h = std::max(heig(p->l), heig(p->r)) + 1;
	l->h = std::max(heig(l->l), heig(l->r)) + 1;
	ll a = fixC(l->r), b = fixC(p->r);
	l->count = a ? a + 1 : 1;
	p->count = b ? b + 1: 1;
	return l;
}
pnode L(pnode p) {
	pnode l = p->r, r = l->l;
	l->l = p;
	p->r = r;
	p->h = std::max(heig(p->l), heig(p->r)) + 1;
	l->h = std::max(heig(l->l), heig(l->r)) + 1;
	ll a = fixC(l->r), b = fixC(p->r);
	l->count = a ? a + 1 : 1;
	p->count = b ? b + 1 : 1;
	return l;
}
pnode minim(pnode p) {
	if (!p->l) return p;
	return minim(p->l);
}
pnode insert(pnode p, ll x) {
	if (!p) return new node(x);
	if (x < p->data) p->l = insert(p->l, x);
	else if (x > p->data) {
		p->r = insert(p->r, x);
		p->count++;
	}
	else return p;
	p->h = std::max(heig(p->l), heig(p->r)) + 1;
	ll b = bal(p);
	if (b > 1 && x < p->l->data) return R(p);
	if (b < -1 && x > p->r->data) return L(p);
	if (b > 1 && x > p->l->data) {
		p->l = L(p->l);
		return R(p);
	}
	if (b < -1 && x < p->r->data) {
		p->r = R(p->r);
		return L(p);
	}
	return p;
}
pnode del(pnode p, ll x) {
	if (!p) return p;
	if (x < p->data) p->l = del(p->l, x);
	else if (x > p->data) {
		p->r = del(p->r, x);
		p->count--;
	}
	else {
		p->count--;
		if (!p->l || !p->r) {
			pnode t = p->l ? p->l : p->r;
			if (t) *p = *t;
			else {
				t = p;
				p = nullptr;
			}
			delete t;
		}
		else {
			pnode t = min(p->r);
			p->data = t->data;
			p->r = del(p->r, t->data);
		}
	}
	if (!p) return p;
	p->h = std::max(heig(p->l), heig(p->r)) + 1;
	ll b = bal(p), bl = bal(p->l), br = bal(p->r);
	if (b > 1 && bl >= 0) return R(p);
	if (b < -1 && br <= 0) return L(p);
	if (b > 1 && bl < 0) {
		p->l = L(p->l);
		return R(p);
	}
	if (b < -1 && br > 0) {
		p->r = R(p->r);
		return L(p);
	}
	return p;
}
void free(pnode p) {
	if (p->l) free(p->l);
	if (p->r) free(p->r);
	if (p) delete p;
}
ll kmax(pnode p, ll k) {
	if (p->count == k) return p->data;
	if (k <= p->count) {
		if (!p->r) return p->data;
		return kmax(p->r, k);
	}
	if (!p->l) return p->data;
	return kmax(p->l, k - p->count);
}
int main() {
	std::ios_base::sync_with_stdio(0);
	ll n, c, k;
	std::cin >> n;
	pnode p = nullptr;
	/*n = 200;
	for (int i = n; i >= 0; i--) {
		p = insert(p, i);
	}
	for (int i = 0; i < n; i++) {
		std::cout << kmax(p, i + 1) << " ";
	}
	return 0;*/


	for (int i = 0; i < n; i++) {
		std::cin >> c >> k;
		if (c == 1) p = insert(p, k);
		else if (!c) std::cout << kmax(p, k) << "\n";
		else p = del(p, k);
	}
	//free(p);
	return 0;
}