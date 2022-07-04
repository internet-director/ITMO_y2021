#include <iostream>
#include <algorithm>
#include <string>
#include <vector>

typedef long long ll;

struct node {
	ll data, h;
	node* l = nullptr, * r = nullptr;
	node(ll data) : data(data), h(1) {}
	node& operator!=(const node& b) {
		this->data = b.data;
		this->h = b.h;
		this->l = b.l;
		this->r = b.r;
		return *this;
	}
};
typedef node* pnode;

struct meta {
	std::string f;
	int key = 0;
};

meta sp(std::string& str) {
	meta d;
	int i = 0;
	while (!std::isspace(str[i++])) d.f += str[i - 1];
	d.key = std::stoi(str.substr(i));
	return d;
}

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
pnode R(pnode p) {
	pnode l = p->l, r = l->r;
	l->r = p;
	p->l = r;
	p->h = std::max(heig(p->l), heig(p->r)) + 1;
	l->h = std::max(heig(l->l), heig(l->r)) + 1;
	return l;
}
pnode L(pnode p) {
	pnode l = p->r, r = l->l;
	l->l = p;
	p->r = r;
	p->h = std::max(heig(p->l), heig(p->r)) + 1;
	l->h = std::max(heig(l->l), heig(l->r)) + 1;
	return l;
}
pnode minim(pnode p) {
	if (!p->l) return p;
	return minim(p->l);
}
pnode insert(pnode p, ll x) {
	if (!p) return new node(x);
	if (x < p->data) p->l = insert(p->l, x);
	else if (x > p->data) p->r = insert(p->r, x);
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
	else if (x > p->data) p->r = del(p->r, x);
	else {
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
pnode exists(pnode p, ll x) {
	if (!p || x == p->data) return p;
	if (x < p->data) return exists(p->l, x);
	return exists(p->r, x);
}
pnode next(pnode p, ll x) {
	pnode t = p, s = nullptr;
	while (t) {
		if (t->data > x) {
			s = t;
			t = t->l;
		}
		else {
			t = t->r;
		}
	}
	return s;
}
pnode prev(pnode p, ll x) {
	pnode t = p, s = nullptr;
	while (t) {
		if (t->data < x) {
			s = t;
			t = t->r;
		}
		else {
			t = t->l;
		}
	}
	return s;
}

void free(pnode p) {
	if (p->l) free(p->l);
	if (p->r) free(p->r);
	if (p) delete p;
}

int main() {
	pnode p = nullptr;
	std::string str;
	while (std::getline(std::cin, str)) {
		meta d = sp(str);
		if (d.f == "insert") {
			p = insert(p, d.key);
		}
		else if (d.f == "delete") {
			p = del(p, d.key);
		}
		else if (d.f == "exists") {
			std::cout << (exists(p, d.key) ? "true" : "false") << "\n";
		}
		else if (d.f == "next") {
			pnode t = next(p, d.key);
			if (t) std::cout << t->data;
			else std::cout << "none";
			std::cout << "\n";
		}
		else {
			pnode t = prev(p, d.key);
			if (t) std::cout << t->data;
			else std::cout << "none";
			std::cout << "\n";
		}
	}
	free(p);
	return 0;
}