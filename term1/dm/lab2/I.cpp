#include <iostream>
#include <string>
#include <vector>
#include <bitset>
#include <WIndows.h>

bool isStep(int num)
{
    return (num & (num - 1)) == 0;
}

int encode(std::string& str, std::string& orig) {
    int num = 0;
    for (int i = 0; i < str.size(); i++) {
        num += (orig[i] - '0') * (str[i] - '0');
        //num %= 2;
    }
    return (num % 2);
}

int main() {
    int code, dop = 1;
    std::string str, result;
    std::cin >> code >> str;


    if (code == 1)
    {
        for (int i = 1; i < str.size() + dop; i++)
        {
            if (isStep(i)) {
                dop++;
                result += '0';
            }
            else result += str[i - dop];
        }
        std::vector<std::string> bin(result.size());
        for (int i = 1; i <= result.size(); i++) {
            std::bitset<20> b(i);
            bin[i - 1] = b.to_string().substr(20 - dop, 20);
            //std::cout << bin[i - 1] << "\n";
        }
        std::vector<std::string> enc(dop);
        for (int i = 0; i < enc.size(); i++) {
            for (int j = 0; j < bin.size(); j++) {
                enc[i] += bin[j][i];
            }
        }
        std::string res = result;
        for (int i = 1; i <= result.size(); i++) {
            if (isStep(i)) {
                result[i - 1] = '0' + encode(enc[--dop], res);
            }
        }
        std::cout << result << "\n";
    }
    else
    {
        for (int i = 0; i < str.size(); i++)
        {
            if (isStep(i + 1)) dop++;
        }
        std::vector<std::string> bin(str.size());
        for (int i = 1; i <= str.size(); i++) {
            std::bitset<20> b(i);
            bin[i - 1] = b.to_string().substr(20 - dop, 20);
        }
        std::vector<std::string> enc(dop);
        for (int i = 0; i < enc.size(); i++) {
            for (int j = 0; j < bin.size(); j++) {
                enc[i] += bin[j][i];
            }
        }
        std::string res;
        int count = 0;
        dop = 0;
        for (int i = 0; i <= str.size(); i++) {
            if (isStep(i)) {
                //std::cout << enc[dop] << "\n";
                res += '0' + encode(enc[dop++], str);
            }
        }
        std::bitset<20> b(res);
        int p = b.to_ulong() - 1;

        if (p >= 0) {
            if (str[p] == '0') str[p] = '1';
            else str[p] = '0';
        }
        for (int i = 0; i < str.size(); i++)
        {
            if (!isStep(i + 1)) result += str[i];
            else dop++;
        }
        std::cout << result << "\n";
    }
}