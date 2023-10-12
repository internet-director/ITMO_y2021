#pragma once
#include <sstream>
#include <strstream>
#include <string>
#include <vector>
#include <iostream>
#include <map>
#include <unordered_map>
#include "Expression.h"
#include "Parser.h"

using upair = std::pair<size_t, size_t>;
using tpair = std::pair<Expr, int>;
using tvec = std::vector<std::pair<std::vector<Expr>, Expr>>;
using ttvec = std::vector<std::pair<bool, Expr>>;
using tppair = std::pair<size_t, ttvec>;


#define AXIOM 0x111
#define MOP 0x222
#define HYPH 0x333
