//
// Created by interceo on 9/27/23.
//

#ifndef MATLOG_PARSER_H
#define MATLOG_PARSER_H
#include "Expression.h"

class Parser {
    const char* str = nullptr;

public:
    Parser() = default;
    Parser(const char* str) {
        init(str);
    }

    void init(const char* str) {
        this->str = str;
    }

    Expr expr() {
        Expr left = dij();
        if (left && skip("->")) return new implication(left, expr());
        return left;
    }

    std::vector<Expr> start_expr() {
        std::vector<Expr> lfts;
        while (1) {
            auto* e = expr();
            if (e != nullptr) lfts.push_back(e);
            if (skip(",")) continue;
            if (skip("|-")) return lfts;
        }
        return lfts;
    }

private:
    Expr dij() {
        Expr left = con();
        while (left && skip("|")) {
            if (*str == '-') {
                str--;
                return left;
            }
            left = new disjunction(left, con());
        }
        return left;
    }

    Expr con() {
        Expr left = nt();
        while (left && skip("&")) {
            left = new conjunction(left, nt());
        }
        return left;
    }

    Expr nt() {
        if (skip("(")) {
            Expr left = expr();
            skip(")");
            return left;
        }
        if (skip("!")) {
            return new negation(nt());
        }
        skip();
        std::string name = getVar();
        if (name.empty()) return nullptr;
        return new variable(name);
    }

    std::string getVar() {
        std::string name;
        while (isVar(*str)) name.push_back(*str), str++;
        return name;
    }

    bool isVar(char c) {
        if (c >= 'A' && c <= 'Z') return true;
        if (c >= '0' && c <= '9') return true;
        if (c == 39) return true;
        return false;
    }

    bool skip(const std::string sk) {
        skip();
        size_t index{ 0 };
        for (char c : sk) {
            char ch = *str;
            if (ch != c) {
                str -= index;
                return false;
            }
            index++;
            str++;
        }
        return true;
    }

    void skip() {
        while (std::isspace(*str)) str++;
    }
};

#endif //MATLOG_PARSER_H
