#include <cmath>
#include <vector>
#include <string>
#include <Windows.h>
#include <fstream>
#include <iostream>


//дорогой дневник, мне не описать ту боль, которую я испытал
int thisMonom(int* mass, int size) {
    int check = 0;
    int pos = -1;
    for (int i = 0; i < size; i++) {
        if (mass[i] >= 0) {
            check++;
            pos = i;
            if (check > 1) {
                return -1;
            }
        }
    }
    return pos;
}

int main() {
    int n, k;
    std::cin >> n >> k;
    int** ist = new int* [k];
    for (int i = 0; i < k; i++) {
        ist[i] = new int[n];
    }

    for (int i = 0; i < k; i++) {
        for (int j = 0; j < n; j++) {
            std::cin >> ist[i][j];
        }
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < k; j++) {
            int position = thisMonom(ist[j], n);
            if (position != -1) {
                for (int l = 0; l < k; l++) {
                    if ((l != j) && (ist[l][position] == ist[j][position])) {
                        for (int d = 0; d < n; d++) {
                            ist[l][d] = -1;
                        }
                    }
                }

                for (int l = 0; l < k; l++) {
                    int dopP = thisMonom(ist[l], n);
                    if ((dopP == position) && (ist[j][position] != ist[l][dopP])) {
                        std::cout << "YES";
                        return 0;
                    }
                    /*else if ((dopP == position) && (ist[j][position] == ist[l][dopP]) && (l != j)) {
                        ist[l][dopP] = -1;
                    }*/
                }

                for (int l = 0; l < k; l++) {
                    ist[l][position] = -1;
                }
            }
        }
    }

    std::cout << "NO";

    return 0;
}