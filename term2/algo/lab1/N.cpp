#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>
#include <string>

namespace std {
	template <typename T1, typename T2, typename T3>
	class triple {
	public:
		T1 a;
		T2 b;
		T3 c;
		triple() : a(0), b(0), c(0) {}
		triple(T1 a, T2 b, T3 c) : a(a), b(b), c(c) {}
	};
}

typedef long long ll;
typedef std::pair<ll, ll> pint;
typedef std::triple<ll, ll, ll> tint;

int main() {
	ll n, dop = 0, k = 0, res = 0;
	std::cin >> n;
	
	std::vector<std::pair<tint, tint>> data(n);
	std::vector<pint> l(n), r(n);
	for (int i = 0; i < n; i++) std::cin >> data[i].first.a >> data[i].first.b >> data[i].first.c
		>> data[i].second.a >> data[i].second.b >> data[i].second.c;

	for (int i = 0; i < n; i++) {
		l[i - k] = pint(data[i].first.a * 3600 + data[i].first.b * 60 + data[i].first.c, 1);
		r[i - k] = pint(data[i].second.a * 3600 + data[i].second.b * 60 + data[i].second.c, 0);

		if (l[i - k].first >= r[i - k].first) dop++;
		if (l[i - k].first == r[i - k].first) {
			l.pop_back();
			r.pop_back();
			k++;
		}
	}
	data.~vector();

	std::vector<pint> d = l;
	d.insert(d.end(), r.begin(), r.end());
	l.~vector();
	r.~vector();
	d.push_back(pint(24 * 60 * 60, 0));

	std::sort(d.begin(), d.end(), [](const pint l, const pint r) { return l.first < r.first; });

	for (int i = 0; i < d.size(); i++) {
		if (d[i].second) dop++;
		else if (dop-- == n) res += d[i].first - (i ? d[i - 1].first : 0);
	}
	std::cout << res;
	return 0;
}