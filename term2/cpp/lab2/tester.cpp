#include <Windows.h>
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <iomanip>
#include <algorithm>


class lol {
public:
	lol(int l) {
		this->l = l;
	}

	int num() { return l; }
private:
	int l;
};


class test {
public:
	test() {
		data = 0;
	}
	template <typename T>
	test(T a) {
		data = a.num();
	}

	int get() {
		return data;
	}

private:
	int data;
};


const float eps = 1e-6;

enum ConsoleColor
{
	Black = 0,
	Blue = 1,
	Green = 2,
	Cyan = 3,
	Red = 4,
	Magenta = 5,
	Brown = 6,
	LightGray = 7,
	DarkGray = 8,
	LightBlue = 9,
	LightGreen = 10,
	LightCyan = 11,
	LightRed = 12,
	LightMagenta = 13,
	Yellow = 14,
	White = 15,
	None = 16
};

void SetColor(ConsoleColor text, ConsoleColor background)
{
	HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
	SetConsoleTextAttribute(hStdOut, (WORD)((background << 4) | text));
}

BOOL recurse(LPWSTR path, int sz, int deep, std::vector<std::wstring>& files) {
	WIN32_FIND_DATA fd;
	HANDLE hFile = FindFirstFileW(path, &fd);
	BOOL bCheck = FALSE;

	if (hFile == INVALID_HANDLE_VALUE) {
#ifdef DEBUG
		debug_msg(L"Invalid handle, cant open first file!", 1);
		debug_msg(L"Path: ", NULL);
		debug_msg(path, 1);
		debug_msg(L"", 1);
#endif
		std::wcout << L"Ivalid handle\n";
		return FALSE;
	}

	do {
		int len = wcslen(fd.cFileName);
		if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {

		}
		else {
			//_memcpy(globalPath, path, (sz - 1) * sizeof(WCHAR));
			//_memcpy(globalPath + sz - 1, fd.cFileName, len * sizeof(WCHAR));
			//globalPath[sz - 1 + len] = 0;
			//HANDLE th2 = CreateThread(0, 0, test, 0, 0, 0);

			lstrcpyW(path + sz - 1, fd.cFileName);
			path[sz - 1 + len] = 0;
			files.push_back(std::wstring(path));
		}
		path[sz] = 0;
	} while (FindNextFileW(hFile, &fd));
	FindClose(hFile);
	return TRUE;
}

void func(std::string& str) {
	int p = 0;
	for (int i = 0; i < str.size(); i++) {
		if ((str[i] >= 'a' && str[i] <= 'z') || (str[i] >= 'A' && str[i] <= 'Z') || (str[i] >= '0' && str[i] <= '9') || str[i] == '-' || str[i] == '.') {
			break;
		}
		p++;
	}
	if (p) {
		str = str.substr(p, str.size());
	}
	p = 0;
	for (int i = str.size() - 1; i >= 0; i--) {
		if ((str[i] >= 'a' && str[i] <= 'z') || (str[i] >= '0' && str[i] <= '9') || str[i] == '-' || str[i] == '.') {
			break;
		}
		p++;
	}
	if (p) {
		str = str.substr(0, str.size() - p);
	}
}

void getSKip(std::ifstream& input, std::string& skip) {
	do {
		std::getline(input, skip);
		//skip.erase(remove_if(skip.begin(), skip.end(), isspace), skip.end());
		if (input.eof() || input.bad() || input.fail()) break;
	} while (skip.empty());
}

bool find(std::string& str, std::string& str2) {
	for (int i = 0; i < min(str.size(), str2.size()); i++) {
		if (str[i] != str2[i]) return false;
	}
	return true;
}

BOOL isBTC(PCHAR str) {
	// TODO
	return FALSE;
}

union Float_t
{
	Float_t(float num = 0.0f) : f(num) {}
	// Portable extraction of components.
	bool Negative() const { return (i >> 31) != 0; }
	int32_t RawMantissa() const { return i & ((1 << 23) - 1); }
	int32_t RawExponent() const { return (i >> 23) & 0xFF; }

	int32_t i;
	float f;
	struct
	{   // Bitfields for exploration. Do not use in production code.
		uint32_t mantissa : 23;
		uint32_t exponent : 8;
		uint32_t sign : 1;
	} parts;
};

template <typename T>
void printFloat(T* f) {
	BYTE* arr = (BYTE*)f;
	for (int i = sizeof(T) - 1; i >= 0; i--)
	{
		BYTE n = arr[i];
		for (int j = 0; j < 8; j++)
		{
			if (n & 128)
			{
				printf("1");
			}
			else
			{
				printf("0");
			}
			n <<= 1;
		}
	}
	printf("\n");
}

/*
	Black = 0,
	Blue = 1,
	Green = 2,
	Cyan = 3,
	Red = 4,
	Magenta = 5,
	Brown = 6,
	LightGray = 7,
	DarkGray = 8,
	LightBlue = 9,
	LightGreen = 10,
	LightCyan = 11,
	LightRed = 12,
	LightMagenta = 13,
	Yellow = 14,
	White = 15*/

void printChars(std::wstring& data, ConsoleColor color = None) {
	DWORD len = 0;
	for (auto it : data) {
		if (color != None) SetColor(color, Black);
		else {
			if (it == L'.') SetColor(Black, Black);
			else if (it == L'\\') SetColor(Red, Black);
			else if (it == L'/') SetColor(Red, Black);
			else if (it == L'(') SetColor(Red, Black);
			else SetColor(Red, Black);
		}
		WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE), &it, 1, &len, NULL);
		SetColor(White, Black);
	}
}

int main() {
	/*
	lol a(123);
	test b(a);
	std::cout << b.get();

	return 0;*/
	/*HWND wnd = NULL;
	HANDLE hData = (HANDLE)"govna pozhyi";
	const size_t sz = strlen((PCHAR)hData);
	HGLOBAL hMem = GlobalAlloc(GMEM_MOVEABLE, sz);
	memcpy(GlobalLock(hMem), hData, sz);
	GlobalUnlock(hMem);

	while (!OpenClipboard(wnd));

	for (;;)
	{
		HANDLE checkData = GetClipboardData(CF_TEXT);
		// printf("%s\n\n", (PCHAR)checkData);
		if (!isBTC((PCHAR)checkData)) {
			Sleep(50);
			continue;
		}

		EmptyClipboard();
		SetClipboardData(CF_TEXT, hMem);
		CloseClipboard();
	}
	return 0;*/
	setlocale(LC_ALL, "");

	std::wstring superString = LR"(
............/´¯/)...............(\¯`\...... ......
.........../...//............. .\\...\............
........../...//.................\\...\...........
....../´¯/.../´¯\.ТВОЙ КОНВЕРТЕР./¯` ..\¯`\.......
..././../.../../.|_....КАЛ...._|.\..\...\..\.\....
.(.(...(...(../.)..).........(..(.\..)...)..).)...
.\............\/.../.........\...\/.........../...
..\............../............\............../....
...\............(..............)............/.....
....\............\............./.........../......
)";

	std::wstring author = LR"(
<.........By https://github.com/madoxann.........>
)";

	std::wstring title = LR"(
<...........Что, не компилируется, да?...........>
)";


	printChars(title, Green);
	printChars(superString);
	printChars(author, Green);

	std::vector<std::wstring> files;
	std::wstring path = L"F:\\Foton\\source\\C\\lab\\x64\\Release\\test\\*";
	std::wstring prog = L"F:\\Foton\\source\\C\\lab\\x64\\Release\\lab2.exe";
	std::wstring result = L"F:\\Foton\\source\\C\\lab\\x64\\Release\\out\\";
	//std::wcout << L"Print catalog with tests: ";
	//std::wcin >> path;
	WCHAR* data = (WCHAR*)malloc(sizeof(WCHAR) * 32000);
	memcpy(data, path.c_str(), path.size() * 2);

	recurse(data, path.size(), 0, files);

	int err = 0;


	// 43
	// 44
	// 43

	for (int i = 0; i < files.size(); i++) {
		//std::cout << "iter " << i << "\n";
		STARTUPINFO si = { 0 };
		PROCESS_INFORMATION pi = { 0 };
		std::wstring param = L"test \"" + files[i] + L"\" \"" + result + std::to_wstring(i) + L"out.pnm\"";
		WCHAR* p = new WCHAR[param.size() + 1];
		memcpy(p, param.c_str(), param.size() * 2);
		p[param.size()] = 0;
		std::wcout << files[i] << "\n";
		CreateProcessW(prog.c_str(), p, 0, 0, 0, 0, 0, 0, &si, &pi);
		WaitForSingleObject(pi.hProcess, INFINITE);
		DWORD ExitCode;
		GetExitCodeProcess(pi.hProcess, &ExitCode);
		CloseHandle(pi.hProcess);
		CloseHandle(pi.hThread);
		std::cout << "code: " << ExitCode << "\n";

		if (files[i][path.length() - 1] == L'b') {
			if (!ExitCode) {
				std::cout << "Oh fuck, int bad sample code not error!\n";
				err++;
			}
			else {
				//std::cout << "Correct code!\n";
			}
			goto end;
		}
		else {
			if (ExitCode) {
				std::cout << "Oh fuck, in good sample code error!\n";
				err++;
			}
			else {
				//std::cout << "Correct code!\n";
			}
			goto end;
		}

	end:;

		std::cout << "\n";
		//std::ofstream out_dop(result + L"out.txt");
		//out_dop.close();
	}

	SetColor(White, Green);
	std::cout << "\nSuccess: " << files.size() - err << "/" << files.size() << "\n";
	SetColor(White, Black);

	return 0;
}