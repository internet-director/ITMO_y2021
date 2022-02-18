    #include <vector>
    #include <iostream>
    #include <string>
     
    typedef long long ll;
     
    int main() {
    	ll n, m;
    	std::cin >> n >> m;
    	ll* mass = new ll[n];
    	ll* dop = new ll[m * 2];
    	ll* dat = new ll[n + 1];
    	dat[0] = 0;
     
    	for (ll i = 1; i <= n; i++) {
    		std::cin >> mass[i - 1];
    		dat[i] = dat[i - 1] + mass[i - 1];
    	}
     
    	for (ll i = 0; i < m * 2; i++) {
    		std::cin >> dop[i];
    	}
     
    	for (ll i = 0; i < m * 2; i += 2) {
    		std::cout << dat[dop[i + 1]] - dat[dop[i] - 1] << '\n';
    	}
    	return 0;
    }