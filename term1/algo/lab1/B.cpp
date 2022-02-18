#include <iostream>
#include <vector>
#include <string>

int main() 
{
	int n;
	std::cin >> n;
	int* pred = new int[101];
	memset(pred, 0, 101 * sizeof(int));
	int k;

	for (int i = 0; i < n; i++)
	{
		std::cin >> k;
		pred[k]++;
	}

	for (int i = 0; i < 101; i++)
	{
		for (int j = 0; j < pred[i]; j++)
		{
			std::cout << i << " ";
		}
	}
}