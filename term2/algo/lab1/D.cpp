#include <iostream>

typedef long long ll;

ll min(ll x, ll y) {
	return ((x) < (y) ? (x) : (y));
} 
ll max(ll x, ll y) {
	return ((x) > (y) ? (x) : (y));
}

struct data {
	ll sum		= 0;
	ll pref		= 0;
	ll suff		= 0;
	ll res		= 0;

	ll min		= 0;
	ll count	= 0;
	
	ll one		= 0;

	data() {}
	data(ll sum, ll min, ll count): sum(sum), min(min), count(count) {
		pref = suff = res = max(0, sum);
		if (sum == 1) one = 1;
	}
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

	data maxsub() {
		return maxsub(1, 0, size - 1, 0, size - 1);
	}

	ll getOne(ll k) {
		return getOne(1, 0, size - 1, k);
	}

	ll Onesum(ll l, ll r) {
		return Onesum(1, 0, size - 1, l, r - 1);
	}

	void print() {
		for (ll i = 0; i < size; i++) {
			std::cout << arr[i] << " ";
		}
		std::cout << std::endl;
	}

	void invert(ll i) {
		arr[i] = arr[i] ? 0 : 1;
		update(1, 0, size - 1, i);
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
			arr_set[v] = data(arr[l], arr[l], 1);
		}
		else {
			ll tmp = (l + r) / 2;
			build(v * 2, l, tmp);
			build(v * 2 + 1, tmp + 1, r);
			arr_set[v] = combine(v);
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
			return data(0, LLONG_MAX, 0);
		}
		if (l == l1 && r == r1) {
			return arr_set[v];
		}
		ll tmp = (l1 + r1) / 2;
		data lf = minim(v * 2, l1, tmp, l, tmp);
		data rf = minim(v * 2 + 1, tmp + 1, r1, tmp + 1, r);

		if (lf.min > rf.min) return rf;
		if (lf.min < rf.min) return lf;
		return data(0, lf.min, lf.count + rf.count);
	}

	data maxsub(ll v, ll l1, ll r1, ll l, ll r) {
		if (l == l1 && r == r1) return arr_set[v];

		ll tmp = (l1 + r1) / 2;
		if (r <= tmp) return maxsub(v * 2, l1, tmp, l, r);
		if (l > tmp) return maxsub(v * 2 + 1, tmp + 1, r1, l, r);

		data lf = maxsub(v * 2, l1, tmp, l, tmp);
		data rf = maxsub(v * 2 + 1, tmp + 1, r1, tmp + 1, r);
		return combine(lf, rf);
	}

	ll getOne(ll v, ll l, ll r, ll k) {
		if (k > arr_set[v].one) return 0;
		if (l == r) return l;

		ll tmp = (l + r) / 2;
		
		return (arr_set[v * 2].one > k) ? getOne(v * 2, l, tmp, k) : getOne(v * 2 + 1, tmp + 1, r, k - arr_set[v * 2].one);
	}

	ll Onesum(ll v, ll l1, ll r1, ll l, ll r) {
		if (l > r) {
			return 0;
		}
		if (l == l1 && r == r1) {
			return arr_set[v].one;
		}
		ll tmp = (l1 + r1) / 2;
		return Onesum(v * 2, l1, tmp, l, min(r, tmp))
			+ Onesum(v * 2 + 1, tmp + 1, r1, max(l, tmp + 1), r);
	}

	void update(ll v, ll l, ll r, ll index) {
		if (l == r) {
			arr_set[v] = data(arr[l], arr[l], 1);
		}
		else {
			ll tmp = (l + r) / 2;
			if (index <= tmp)
				update(v * 2, l, tmp, index);
			else
				update(v * 2 + 1, tmp + 1, r, index);
			arr_set[v] = combine(v);
		}
	}

	data combine(ll v) {
		return combine(arr_set[v * 2], arr_set[v * 2 + 1]);
	}

	data combine(data& a, data& b) {
		data tmp;
		tmp.sum = a.sum + b.sum;
		tmp.one = a.one + b.one;
		tmp.pref = max(a.pref, a.sum + b.pref);
		tmp.suff = max(b.suff, b.sum + a.suff);
		tmp.res = max(max(a.res, b.res), a.suff + b.pref);

		if (a.min > b.min) {
			tmp.min = b.min;
			tmp.count = b.count;
		}
		else if (a.min < b.min) {
			tmp.min = a.min;
			tmp.count = a.count;
		}
		else {
			tmp.min = a.min;
			tmp.count = a.count + b.count;
		}
		return tmp;
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
		std::cin >> n >> k1;
		if (n == 2) {
			std::cout << s.getOne(k1) << std::endl;
		}
		else {
			s.invert(k1);
		}
	}
	return 0;
}