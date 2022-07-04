#define _CRT_SECURE_NO_WARNINGS
#include "return_codes.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

typedef union {
	float f;
	unsigned int a;
	struct {
		unsigned int m : 23;
		unsigned int e : 8;
		unsigned int s : 1;
	};
} Float;

Float cNull;

void initFloat() {
	float f = 0.0f;
	memcpy(&cNull, &f, 4);
}

void swap(float** mass, size_t l, size_t r);
size_t readData(FILE* file, float** mass, size_t size);
void freeMemory(float** mass, float* result, size_t N, FILE* in, FILE* out);
size_t fixMatrix(float** mass, size_t num, size_t N);
int compare(float a);
int compare2(float a, float b);

int main(int argc, const char* argv[])
{
	initFloat();
	size_t N = 0;
	FILE* input = NULL, * output = NULL;
	float** arr = NULL, * result = NULL;

	if (argc != 3)
	{
		printf("Invalid arguments\nExpected: lab1 <input file> <output file>\n");
		return ERROR_INVALID_PARAMETER;
	}

	input = fopen(argv[1], "r");

	if (!input)
	{
		printf("Error read input file!\n");
		freeMemory(arr, result, 0, input, output);
		return ERROR_FILE_NOT_FOUND;
	}

	output = fopen(argv[2], "w");

	if (!output)
	{
		printf("Error create output file!\n");
		freeMemory(arr, result, 0, input, output);
		return ERROR_FILE_NOT_FOUND;
	}

	if (!fscanf(input, "%zi", &N))
	{
		printf("Invalid input data!\n");
		freeMemory(arr, result, 0, input, output);
		return ERROR_INVALID_DATA;
	}

	arr = malloc(N * sizeof(float*));
	result = malloc(N * sizeof(float));

	if (!arr || !result)
	{
		printf("Error init memory!\n");
		freeMemory(arr, result, 0, input, output);
		return ERROR_MEMORY;
	}

	for (size_t i = 0; i < N; i++)
	{
		arr[i] = malloc(sizeof(float) * (N + 1));
		if (!arr[i])
		{
			printf("Error init memory!\n");
			freeMemory(arr, result, i, input, output);
			return ERROR_MEMORY;
		}
	}

	if (readData(input, arr, N) == ERROR_INVALID_DATA)
	{
		printf("Invalid input data!\n");
		freeMemory(arr, result, N, input, output);
		return ERROR_INVALID_DATA;
	}

	for (size_t i = 0; i < N - 1; i++)
	{
		if (arr[i][i] == 0.0f)
		{
			if (!fixMatrix(arr, i, N))
			{
				continue;
			}
		}
		for (size_t j = i + 1; j < N; j++)
		{
			if (arr[j][i] == 0.0f)
			{
				continue;
			}
			float tmp = arr[i][i] / arr[j][i];
			for (size_t k = i; k <= N; k++)
			{
				arr[j][k] *= tmp;
				arr[j][k] -= arr[i][k];
			}
		}
	}

	for (size_t index = 0; index < N; index++)
	{
		size_t i = N - index - 1;
		float tmp = 0;
		for (size_t j = i + 1; j < N; j++)
		{
			//if (compare(arr[i][j]) && compare(result[j]))
			if (arr[i][j] != 0.0f && result[j] != 0.0f)
			{
				tmp += arr[i][j] * result[j];
			}
		}
		tmp = arr[i][N] - tmp;
		if (!compare(arr[i][i]))
		{
			if (!compare(tmp))
			{
				fprintf(output, "%s", "many solutions\n");
			}
			else
			{
				fprintf(output, "%s", "no solution\n");
			}
			freeMemory(arr, result, N, input, output);
			return ERROR_SUCCESS;
		}
		result[i] = tmp / arr[i][i];
	}

	for (size_t i = 0; i < N; i++)
	{
		fprintf(output, "%g\n", result[i]);
	}

	freeMemory(arr, result, N, input, output);
	return ERROR_SUCCESS;
}

void freeMemory(float** mass, float* result, size_t N, FILE* in, FILE* out)
{
	if (mass)
	{
		for (size_t i = 0; i < N; i++)
		{
			free(mass[i]);
		}
		free(mass);
	}
	if (result)
	{
		free(result);
	}
	if (in)
	{
		fclose(in);
	}
	if (out)
	{
		fclose(out);
	}
}

size_t readData(FILE* file, float** mass, size_t size)
{
	float f;
	for (size_t i = 0; i < size; i++)
	{
		for (size_t j = 0; j <= size; j++)
		{
			fscanf(file, "%f", &mass[i][j]);
		}
	}

	return 0;
}

void swap(float** mass, size_t l, size_t r)
{
	float* dop = mass[l];
	mass[l] = mass[r];
	mass[r] = dop;
}

size_t fixMatrix(float** mass, size_t num, size_t N)
{
	size_t p = -1;
	float max = 0;
	for (size_t i = num + 1; i < N; i++)
	{
		if (fabsf(mass[i][num]) > fabsf(max))
		{
			max = mass[i][num];
			p = i;
		}
	}

	if (compare(max))
	{
		swap(mass, num, p);
		return 1;
	}

	return 0;
}


// 0 if a == 0
// 1 if a != 0
int compare(float a)
{
	Float f;
	memcpy(&f, &a, 4);
	if ((f.e == cNull.e) && (abs(f.m - cNull.m) < 100)) {
		return 0;
	}
	return 1;
}

int compare2(float a, float b)
{
	Float f, f2;
	memcpy(&f, &a, 4);
	memcpy(&f2, &b, 4);
	if (f.e == f2.e && (abs(f.m - f2.m) < 20)) {
		return 0;
	}
	return 1;
}