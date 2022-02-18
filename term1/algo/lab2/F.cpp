    #include <iostream>
    #include <fstream>
    #include <string>
    #include <vector>
    typedef long long ull;
     
    int main() {
    	int n, m;
    	std::ifstream input("knight.in");
    	std::ofstream output("knight.out");
    	input >> n >> m;
    	//std::cin >> n >> m;
    	n++;
    	m++;
    	std::vector<std::vector<ull> > mass(n, std::vector<ull>(m, 0));
    	mass[1][1] = 1;
     
    	for (int i = 2; i < n; i++) {
    		for (int j = 2; j < m; j++) {
    			mass[i][j] = mass[i - 1][j - 2] + mass[i - 2][j - 1];
    		}
    	}
    	output << mass[n - 1][m - 1];
    	input.close();
    	output.close();
    	return 0;
    }