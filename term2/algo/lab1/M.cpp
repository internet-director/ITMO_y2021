#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>
#include <string>

template <typename T1, typename T2, typename T3>
class triple {
public:
	T1 a;
	T2 b;
	T3 c;
	triple(): a(0), b(0), c(0) {}
	triple(T1 a, T2 b, T3 c): a(a), b(b), c(c) {}
};

typedef long long ll;
typedef triple<ll, ll, ll> pint;

int main() {
	int n, m;
	std::cin >> n >> m;
	std::vector<ll> l(n), r(n), result(m);
	std::vector<pint> p(m);
	for (int i = 0; i < n; i++) {
		std::cin >> l[i] >> r[i];
		if (l[i] > r[i]) std::swap(l[i], r[i]);
	}
	for (int i = 0; i < m; i++) std::cin >> p[i].a;
	for (int i = 0; i < m; i++) p[i].b = i;

	std::sort(l.begin(), l.end());
	std::sort(r.begin(), r.end());
	std::sort(p.begin(), p.end(), [](const pint l, const pint r) { return l.a < r.a; });

	l.push_back(LLONG_MAX);
	r.push_back(LLONG_MAX);
	p.push_back(triple<ll, ll, ll>(LLONG_MAX, LLONG_MAX, LLONG_MAX));

	int i = 0, j = 0, k = 0, res = 0;
	for (;;) {
		if (i >= n && j >= n && k >= m) break;
		if (i < n && l[i] <= r[j] && l[i] <= p[k].a) {
			res++;
			i++;
		}
		else if (k < m && p[k].a <= l[i] && p[k].a <= r[j]) {
			p[k].c = res;
			k++;
		}
		else if (j < n && r[j] <= p[k].a && r[j] <= l[i]) {
			res--;
			j++;
		}
		else break;
	}

	std::sort(p.begin(), p.end(), [](const pint l, const pint r) { return l.b < r.b; });

	for (int i = 0; i < p.size() - 1; i++ ) std::cout << p[i].c << " ";
	return 0;
}