#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <stdio.h>
#include <zlib.h>

typedef unsigned char byte;
typedef unsigned char* pbyte;

size_t s = 58756156156;

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
	size_t d_sz;
	pbyte err;
} hdrImage, * phdrImage;

void genData(phdrImage p, byte type) {
	size_t sz = (p->ctype + 1) * p->w + 1;
	pbyte data = (pbyte)malloc(sz * p->h);

	for (int j = 0; j < p->h; j++) {
		data[j * sz] = type;
		for (size_t i = 1; i < sz; i++) {
			s += rand() * 88857891 + 18975648972318;
			data[j * sz + i] = s % 256;
		}
	}

	p->size = compressBound(sz * p->h);
	p->data = (pbyte)malloc(p->size);
	int code = compress(p->data, (uLongf*)&p->size, data, sz * p->h);
	if (code) {
		std::cout << "FUUUUUUUUUUU\n";
		exit(0);
	}
	free(data);
}

void gen(phdrImage p, size_t sz) {
	p->data = (pbyte)malloc(sz);
	p->size = sz;
	srand(clock());
	for (size_t i = 0; i < sz; i++) {
		p->data[i] = rand() % 256;
	}
}
void sign(FILE* file, phdrImage image) {
	byte signature[8] = { 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a };
	pbyte d = (pbyte)malloc(21);
	invertNumber(&image->size);
	invertNumber(&image->w);
	invertNumber(&image->h);

	memcpy(d, &image->size, 4);
	memcpy(d + 4, &image->type, 4);
	memcpy(d + 8, &image->w, 4);
	memcpy(d + 12, &image->h, 4);
	memcpy(d + 16, &image->depth, 1);
	memcpy(d + 17, &image->ctype, 1);
	memcpy(d + 18, "\0\0\0", 3);

	fwrite(signature, 1, 8, file);
	fwrite(d, 1, 21, file);
	uLong crc = crc32(0, d, 21);
	invertNumber((uint32_t*)&crc);
	fwrite(&crc, 1, 4, file);
	invertNumber(&image->size);
	invertNumber(&image->w);
	invertNumber(&image->h);
}
void end(FILE* file) {
	byte end_signature[8] = { 'I', 'E', 'N', 'D', 0xae, 0x42, 0x60, 0x82 };
	fwrite("\0\0\0\0", 1, 4, file);
	fwrite(end_signature, 1, 8, file);
}
void write(FILE* file, phdrImage image, const char* type) {
	invertNumber(&image->size);
	fwrite(&image->size, 1, 4, file);
	fwrite(type, 1, 4, file);
	invertNumber(&image->size);
	fwrite(image->data, 1, image->size, file);
	uLong crc = crc32(0, image->data, image->size);
	invertNumber((uint32_t*)&crc);
	fwrite(&crc, 1, 4, file);
	free(image->data);
}
void write_big(FILE* file, size_t sz) {
	fwrite(&sz, 1, 4, file);
	for (size_t i = 0; i < sz; i++) {
		fwrite("\x69", 1, 1, file);
	}
}


int main() {
	s = time(0);
	FILE* file = fopen("F:\\Foton\\source\\C\\lab\\x64\\Release\\test\\t1.png", "wb");
	hdrImage p;
	memset(&p, 0, sizeof(hdrImage));
	p.ctype = 2;
	p.depth = 8;
	p.w = 100;
	p.h = 21;
	p.size = 13;
	memcpy(&p.type, "IHDR", 4);
	sign(file, &p);

	genData(&p, 1);
	write(file, &p, "IDAT");
	genData(&p, 1);
	write(file, &p, "TFUK");
	genData(&p, 1);

	end(file);
	fwrite("\36\65\f\a\0", 1, 5, file);
	fclose(file);
	return 0;
}