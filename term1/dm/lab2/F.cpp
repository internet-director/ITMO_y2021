#include <iostream>
#include <vector>
#include <string>
#include <map>
 
int main() {
    int n;
    std::cin >> n;
 
    std::string result;
    std::map <std::string, int> alph;
    std::map <int, std::string> dec;
    std::vector <int> input(n);
 
    for (int i = 0; i < n; i++) {
        std::cin >> input[i];
    }
 
    std::string dop;
    for (int i = 0; i < 26; i++) {
        dop = 'a' + i;
        alph[dop] = i;
        dec[i] = dop;
    }
 
    std::string l1 = dec[input[0]], l2;
 
    for (int i = 1; i < n; i++) {
        result += l1;
        int pos = i + 25;
        if (dec.find(input[i]) != dec.end()) {
            l2 = dec[input[i]];
            l1.push_back(l2[0]);
 
            alph[l1] = pos;
            dec[pos] = l1;
        }
        else {
            l1.push_back(l1[0]);
            alph[l1] = pos;
            dec[pos] = l1;
 
            l2 = dec[input[i]];
        }
        l1 = l2;
    }
    result += l1;
    std::cout << result;
    return 0;
}