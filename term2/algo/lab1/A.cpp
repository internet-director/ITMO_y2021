#include <iostream>

#define min(x, y) ((x) < (y) ? (x) : (y))
#define max(x, y) ((x) > (y) ? (x) : (y))

typedef unsigned long long ll;

class set {
public:
	set(ll size, ll* arr) {
		this->size = size;
		this->arr = arr;
		arr_set = new data[size * 4];
		
		build(1, 0, size - 1);
	}

	ll sum(ll l, ll r) {
		return sum(1, 0, size - 1, l, r - 1);
	}

	void update(ll index, ll value) {
		arr[index] = value;
		update(1, 0, size - 1, index, value);
	}

private:
	struct data;
	ll*	arr, size;
	data* arr_set;

	void build(ll v, ll l, ll r) {
		if (l == r) {
			arr_set[v].sum = arr[l];
		}
		else {
			ll tmp = (l + r) / 2;
			build(v * 2, l, tmp);
			build(v * 2 + 1, tmp + 1, r);
			arr_set[v].sum = arr_set[v * 2].sum + arr_set[v * 2 + 1].sum;
		}
	}

	ll sum(ll v, ll l1, ll r1, ll l, ll r) {
		if (l > r) {
			return 0;
		}
		if (l == l1 && r == r1) {
			return arr_set[v].sum;
		}
		ll tmp = (l1 + r1) / 2;
		return sum(v * 2, l1, tmp, l, min(r, tmp))
			+ sum(v * 2 + 1, tmp + 1, r1, max(l, tmp + 1), r);
	}

	void update(ll v, ll l, ll r, ll index, ll value) {
		if (l == r) {
			arr_set[v].sum = value;
		}
		else {
			ll tmp = (l + r) / 2;
			if (index <= tmp)
				update(v * 2, l, tmp, index, value);
			else
				update(v * 2 + 1, tmp + 1, r, index, value);
			arr_set[v].sum = arr_set[v * 2].sum + arr_set[v * 2 + 1].sum;
		}
	}

	struct data {
		ll sum = 0;
	};
};

int main() {
	ll n, m, *arr, k1, k2;
	std::cin >> n >> m;
	arr = new ll[n];

	for (ll i = 0; i < n; i++) {
		std::cin >> arr[i];
	}

	set s(n, arr);

	for (ll i = 0; i < m; i++) {
		std::cin >> n >> k1 >> k2;

		if (n == 2) {
			std::cout << s.sum(k1, k2) << std::endl;
		}
		else {
			s.update(k1, k2);
		}
	}
	return 0;
}