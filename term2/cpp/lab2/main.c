#define _CRT_SECURE_NO_WARNINGS
#define ZLIB

typedef unsigned char byte;
typedef unsigned char* pbyte;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

#ifdef ZLIB
#include <zlib.h>
#elif defined(LIBDEFLATE)
#include <libdeflate.h>
#elif defined(ISAL)
#include <include/igzip_lib.h>
#else
#error "Not found library"
#endif

#include "return_codes.h"
#include <math.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void invertNumber(uint32_t* num)
{
	uint32_t ret = 0;
	for (int i = 0; i < 4; i++)
	{
		ret += ((pbyte)num)[i] << (8 * (3 - i));
	}
	*num = ret;
}

typedef struct
{
	uint32_t size;
	uint32_t type;
	uint32_t crc;
	uint32_t w;
	uint32_t h;
	byte depth;
	byte ctype;

	pbyte data;
	uint64_t d_sz;
	pbyte err;
} hdrImage, * phdrImage;

int checkInvalidType(phdrImage p) {
	if (((pbyte)&p->type)[0] >= 'A' && ((pbyte)&p->type)[0] <= 'Z')
	{
		if (memcmp(&p->type, "IDAT", 4) && memcmp(&p->type, "IHDR", 4) && memcmp(&p->type, "IEND", 4) && memcmp(&p->type, "PLTE", 4))
		{
			p->err = "Invalid chunk type!\n";
			return ERROR_INVALID_DATA;
		}
	}
	return 0;
}

int checkType(phdrImage p, char* type)
{
	return !memcmp(&p->type, type, 4);
}

byte predictor(byte a, byte b, byte c)
{
	int p = a + b - c;
	int pa = abs(p - a);
	int pb = abs(p - b);
	int pc = abs(p - c);

	if (pa <= pb && pa <= pc)
	{
		return a;
	}
	if (pb <= pc)
	{
		return b;
	}
	return c;
}

int getHeader(FILE* file, phdrImage p)
{
	if (4 != fread(&p->size, 1, 4, file) || 4 != fread(&p->type, 1, 4, file))
	{
		p->err = "Unexpected end of file!\n";
		return ERROR_INVALID_DATA;
	}
	invertNumber(&p->size);
	return 0;
}

int hdrInit(FILE* file, phdrImage image)
{
	int code = 0;
	byte sign[3];
	memset(image, 0, sizeof(hdrImage));
	code += fread(&image->size, 1, 4, file);
	code += fread(&image->type, 1, 4, file);
	code += fread(&image->w, 1, 4, file);
	code += fread(&image->h, 1, 4, file);
	code += fread(&image->depth, 1, 1, file);
	code += fread(&image->ctype, 1, 1, file);
	code += fread(sign, 1, 3, file);
	code += fread(&image->crc, 1, 4, file);

	if (code != 25)
	{
		image->err = "Unexpected end of file!\n";
		return ERROR_INVALID_DATA;
	}

	invertNumber(&image->size);
	invertNumber(&image->w);
	invertNumber(&image->h);

	if (image->size != 0x0d || (!image->w || !image->h) || image->depth != 8 || (image->ctype != 0 && image->ctype != 2) || memcmp(sign, "\0\0\0", 3))
	{
		image->err = "Invalid hdr header!\n";
		return ERROR_INVALID_DATA;
	}

	if (memcmp(&image->type, "IHDR", 4))
	{
		image->err = "Expected IHDR chunk\n";
		return ERROR_INVALID_DATA;
	}

	return 0;
}

int datChunk(FILE* file, phdrImage image, int skip)
{
	if (skip) {
		if (fseek(file, image->size + 4, SEEK_CUR)) {
			image->err = "Unexpected end of file!\n";
			return ERROR_INVALID_DATA;
		}
		return 0;
	}

	if (image->size != fread(image->data + image->d_sz, 1, image->size, file) || 4 != fread(&image->crc, 1, 4, file))
	{
		image->err = "Unexpected end of file!\n";
		return ERROR_INVALID_DATA;
	}
	image->d_sz += image->size;
	return 0;
}

int decode(phdrImage image)
{
	int code = -1;
	size_t color = image->ctype + 1;
	size_t step = color * image->w + 1;
	byte* ret = malloc(step * image->h);
	size_t szlib = step * image->h;

	if (!ret)
	{
		free(image->data);
		return ERROR_MEMORY;
	}

#ifdef ZLIB
	code = uncompress(ret, &szlib, image->data, image->d_sz);
#elif defined(LIBDEFLATE)
	struct libdeflate_decompressor* dec = libdeflate_alloc_decompressor();
	if (!dec)
	{
		free(ret);
		free(image->data);
		return ERROR_MEMORY;
	}
	code = libdeflate_zlib_decompress(dec, image->data, image->d_sz, ret, step * image->h, NULL);
	libdeflate_free_decompressor(dec);
#elif defined(ISAL)
	struct inflate_state stream;
	isal_inflate_init(&stream);
	stream.next_in = image->data;
	stream.avail_in = image->d_sz;
	stream.next_out = ret;
	stream.avail_out = step * image->h;
	stream.crc_flag = IGZIP_ZLIB;

	code = isal_inflate_stateless(&stream);
	szlib = stream.total_out;
#endif

	if (code || (szlib != step * image->h))
	{
		image->err = "Error decode data!\n";
		free(ret);
		free(image->data);
		return ERROR_INVALID_DATA;
	}

	free(image->data);
	image->data = malloc(color * image->w * image->h);

	if (!image->data)
	{
		free(ret);
		return ERROR_MEMORY;
	}
	image->d_sz = color * image->w * image->h;

	for (size_t i = 0; i < image->h; i++)
	{
		switch (ret[i * step])
		{
		case 0:
			break;
		case 1:
		{
			for (uint32_t j = 1 + color; j < step; j++)
			{
				ret[i * step + j] += ret[i * step + j - color];
			}
			break;
		}
		case 2:
		{
			if (!i)
			{
				break;
			}
			for (uint32_t j = 1; j < step; j++)
			{
				ret[i * step + j] += ret[(i - 1) * step + j];
			}
			break;
		}
		case 3:
		{
			for (uint32_t j = 1; j < step; j++)
			{
				byte a;
				if (!i && j <= color)
				{
					a = 0;
				}
				else if (!i)
				{
					a = floor(ret[i * step + j - color] / 2);
				}
				else if (j <= color)
				{
					a = floor(ret[(i - 1) * step + j] / 2);
				}
				else
				{
					a = floor((ret[(i - 1) * step + j] + ret[i * step + j - color]) / 2);
				}

				ret[i * step + j] += a;
			}
			break;
		}
		case 4:
		{
			for (uint32_t j = 1; j < step; j++)
			{
				byte a, b, c;
				if (!i && j <= color)
				{
					a = b = c = 0;
				}
				else if (!i)
				{
					a = ret[i * step + j - color];
					b = 0;
					c = 0;
				}
				else if (j <= color)
				{
					a = 0;
					b = ret[(i - 1) * step + j];
					c = 0;
				}
				else
				{
					a = ret[i * step + j - color];
					b = ret[(i - 1) * step + j];
					c = ret[(i - 1) * step + j - color];
				}
				ret[i * step + j] += predictor(a, b, c);
			}
			break;
		}
		default:
			free(ret);
			free(image->data);
			image->err = "Invalid filter type!\n";
			return ERROR_INVALID_DATA;
		}
		memcpy(image->data + (step - 1) * i, ret + i * step + 1, step - 1);
	}

	free(ret);
	return 0;
}

int checkPng(FILE* file, phdrImage image) {
	int code;
	byte end_signature[4] = { 0xae, 0x42, 0x60, 0x82 };

	if (!checkType(image, "IDAT")) {
		image->err = "Expected IDAT chunk!\n";
		return ERROR_INVALID_DATA;
	}

	do
	{
		image->d_sz += image->size;
		if ((code = datChunk(file, image, 1)) || (code = getHeader(file, image)) || (code = checkInvalidType(image)))
		{
			return code;
		}
	} while (checkType(image, "IDAT"));

	if (!image->d_sz) {
		image->err = "Unexpected end of file!\n";
		return ERROR_INVALID_DATA;
	}

	while (!checkType(image, "IEND"))
	{
		if (checkType(image, "IHDR") || checkType(image, "IDAT") || checkType(image, "PLTE"))
		{
			image->err = "Expected IEND\n";
			return ERROR_INVALID_DATA;
		}
		if ((code = datChunk(file, image, 1)) || (code = getHeader(file, image)) || (code = checkInvalidType(image)))
		{
			return code;
		}
	}

	if (image->size || memcmp(&image->type, "IEND", 4))
	{
		image->err = "Not found IEND section!\n";
		return ERROR_INVALID_DATA;
	}

	if (fread(&image->size, 1, 4, file) != 4 || memcmp(&image->size, end_signature, 4))
	{
		image->err = "Invalid IEND signature\n";
		return ERROR_INVALID_DATA;
	}

	return 0;
}

int getPng(FILE* file, phdrImage image)
{
	int code;
	byte signature[8] = { 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a }, fSing[8];

	if (fread(fSing, 1, 8, file) != 8)
	{
		image->err = "Unexpected end of file!\n";
		return ERROR_INVALID_DATA;
	}

	if (memcmp(signature, fSing, 8))
	{
		image->err = "Invalid start signature!\n";
		return ERROR_INVALID_DATA;
	}

	if ((code = hdrInit(file, image)) || (code = getHeader(file, image)) || (code = checkInvalidType(image)))
	{
		return code;
	}

	while (!checkType(image, "IDAT") && !checkType(image, "PLTE"))
	{
		if (checkType(image, "IHDR") || checkType(image, "IEND"))
		{
			image->err = "Unexpected chynk type!\n";
			return ERROR_INVALID_DATA;
		}
		if ((code = datChunk(file, image, 1)) || (code = getHeader(file, image)) || (code = checkInvalidType(image)))
		{
			return code;
		}
	}

	if (checkType(image, "PLTE")) {
		if (!image->ctype)
		{
			image->err = "PLTE in color type = 0 is not supported!\n";
			return ERROR_INVALID_DATA;
		}
		if ((image->size % 3 != 0) || (image->size > 3 * 256) || !image->size)
		{
			image->err = "Invalid plte data!\n";
			return ERROR_INVALID_DATA;
		}
		if ((code = datChunk(file, image, 1)) || (code = getHeader(file, image)) || (code = checkInvalidType(image)))
		{
			return code;
		}
	}

	uint32_t position = ftell(file), sz = image->size;
	if (code = checkPng(file, image)) {
		return code;
	}
	image->data = malloc(image->d_sz);
	image->size = sz;
	image->d_sz = 0;
	if (!image->data) {
		return ERROR_MEMORY;
	}

	if (fseek(file, position, SEEK_SET)) {
		image->err = "Unexpected end of file!\n";
		return ERROR_INVALID_DATA;
	}

	do
	{
		if ((code = datChunk(file, image, 0)) || (code = getHeader(file, image)))
		{
			free(image->data);
			return code;
		}
	} while (checkType(image, "IDAT"));

	return decode(image);
}

int main(int argc, char* argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "Invalid arguments\nExpected: lab1 <input png file> <output pnm file>\n");
		return ERROR_INVALID_PARAMETER;
	}
	hdrImage im;

	FILE* in = fopen(argv[1], "rb");
	if (!in)
	{
		fprintf(stderr, "Error open input file!\n");
		return ERROR_FILE_NOT_FOUND;
	}

	int code = getPng(in, &im);
	fclose(in);

	switch (code)
	{
	case ERROR_INVALID_DATA:
	{
		fprintf(stderr, im.err);
		return code;
	}
	case ERROR_MEMORY:
	{
		fprintf(stderr, "Memory error!\n");
		return code;
	}
	}

	FILE* out = fopen(argv[2], "wb");
	if (!out)
	{
		fprintf(stderr, "Error open output file!\n");
		return ERROR_FILE_NOT_FOUND;
	}

	if (!fprintf(out, "P%i\n%lu %lu\n255\n", (im.ctype == 2) ? 6 : 5, im.w, im.h) || im.d_sz != fwrite(im.data, 1, im.d_sz, out))
	{
		fprintf(stderr, "Error write in out file!\n");
		code = ERROR_FILE_NOT_FOUND;
	}

	fclose(out);
	free(im.data);
	return code;
}