#include <iostream>

typedef long long ll;

ll min(ll x, ll y) {
	return ((x) < (y) ? (x) : (y));
} 
ll max(ll x, ll y) {
	return ((x) > (y) ? (x) : (y));
}



struct data {
	ll sum = 0;
	ll min;
	ll count = 0;

	data() {}
	data(ll min, ll count): min(min), count(count) {}
};

class set {
public:
	set(ll size, ll* arr) {
		this->size = size;
		this->arr = arr;
		arr_set = new data[size * 4];

		build(1, 0, size - 1);
	}

	~set() {
		delete[] arr;
		delete[] arr_set;
	}

	ll sum(ll l, ll r) {
		return sum(1, 0, size - 1, l, r - 1);
	}

	data minim(ll l, ll r) {
		return minim(1, 0, size - 1, l, r - 1);
	}

	void update(ll index, ll value) {
		arr[index] = value;
		update(1, 0, size - 1, index);
	}


private:
	ll* arr, size;
	data* arr_set;

	void build(ll v, ll l, ll r) {
		if (l == r) {
			arr_set[v].sum = arr[l];
			arr_set[v].min = arr[l];
			arr_set[v].count = 1;
		}
		else {
			ll tmp = (l + r) / 2;
			build(v * 2, l, tmp);
			build(v * 2 + 1, tmp + 1, r);
			combine(v);
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

	data minim(ll v, ll l1, ll r1, ll l, ll r) {
		if (l > r) {
			return data(LLONG_MAX, 0);
		}
		if (l == l1 && r == r1) {
			return arr_set[v];
		}
		ll tmp = (l1 + r1) / 2;
		data lf = minim(v * 2, l1, tmp, l, min(r, tmp));
		data rf = minim(v * 2 + 1, tmp + 1, r1, max(l, tmp + 1), r);

		if (lf.min > rf.min) return rf;
		if (lf.min < rf.min) return lf;
		return data(lf.min, lf.count + rf.count);
	}

	void update(ll v, ll l, ll r, ll index) {
		if (l == r) {
			arr_set[v].sum = arr[l];
			arr_set[v].min = arr[l];
			arr_set[v].count = 1;
		}
		else {
			ll tmp = (l + r) / 2;
			if (index <= tmp)
				update(v * 2, l, tmp, index);
			else
				update(v * 2 + 1, tmp + 1, r, index);
			combine(v);
		}
	}

	void combine(ll v) {
		arr_set[v].sum = arr_set[v * 2].sum + arr_set[v * 2 + 1].sum;
		if (arr_set[v * 2].min > arr_set[v * 2 + 1].min) {
			arr_set[v].min = arr_set[v * 2 + 1].min;
			arr_set[v].count = arr_set[v * 2 + 1].count;
		}
		else if (arr_set[v * 2].min < arr_set[v * 2 + 1].min) {
			arr_set[v].min = arr_set[v * 2].min;
			arr_set[v].count = arr_set[v * 2].count;
		}
		else {
			arr_set[v].min = arr_set[v * 2].min;
			arr_set[v].count = arr_set[v * 2].count + arr_set[v * 2 + 1].count;
		}
	}
};

int main() {
	ll n, m, * arr, k1, k2;
	std::cin >> n >> m;
	arr = new ll[n];

	for (ll i = 0; i < n; i++) {
		std::cin >> arr[i];
	}

	set s(n, arr);

	for (ll i = 0; i < m; i++) {
		std::cin >> n >> k1 >> k2;

		if (n == 2) {
			data d = s.minim(k1, k2);
			std::cout << d.min << " " << d.count << std::endl;
			//std::cout << s.sum(k1, k2) << std::endl;
		}
		else {
			s.update(k1, k2);
		}
	}
	return 0;
}