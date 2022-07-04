#include <iostream>
#include <algorithm>
#include <string>
#include <vector>

typedef long long ll;

struct node {
	ll data;
	node* l = nullptr, * r = nullptr;
	node(ll data): data(data) {}
};
typedef node* pnode;

struct meta {
	std::string f;
	int key;
};

meta sp(std::string& str) {
	meta d;
	int i = 0;
	while (!std::isspace(str[i++])) d.f += str[i - 1];
	d.key = std::stoi(str.substr(i));
	return d;
}

pnode minim(pnode p) {
	if (!p->l) return p;
	return minim(p->l);
}
pnode insert(pnode p, ll x) {
	if (!p) return new node(x);
	if (x < p->data) p->l = insert(p->l, x);
	else if (x > p->data) p->r = insert(p->r, x);
	return p;
}
pnode del(pnode p, ll x) {
	if (!p) return p;
	if (x < p->data) p->l = del(p->l, x);
	else if (x > p->data) p->r = del(p->r, x);
	else if (p->l && p->r) {
		p->data = minim(p->r)->data;
		p->r = del(p->r, p->data);
	}
	else {
		if (p->l) p = p->l;
		else if (p->r) p = p->r;
		else p = nullptr;
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