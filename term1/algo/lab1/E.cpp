    #include <vector>
    #include <iostream>
    #include <string>
     
    typedef long long ll;
     
    int main() {
    	int n;
    	std::cin >> n;
    	int* mass = new int[n + 1];
    	mass[0] = 0;
     
    	for (int i = 0; i < n; i++) {
    		std::cin >> mass[i + 1];
    	}
     
    	bool check = true;
    	for (int i = 1; i <= n / 2; i++) {
    		if ((2 * i <= n) && (mass[i] > mass[i * 2])) {
    			check = false;
    			break;
    		}
    		else if ((2 * i + 1 <= n) && (mass[i] > mass[2 * i + 1])) {
    			check = false;
    			break;
    		}
    	}
    	if (check) std::cout << "YES";
    	else std::cout << "NO";
    }