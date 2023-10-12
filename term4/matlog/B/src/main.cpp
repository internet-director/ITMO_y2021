#include <sstream>
#include <strstream>
#include <string>
#include <vector>
#include <iostream>
#include <map>
#include <unordered_map>


/*<------------------------------------------------------------------------------------------------------------>*/
class expression {
public:
    virtual bool equals(const expression*, bool = true) const = 0;
    virtual std::string prefix_form() const = 0;
    virtual ~expression() = default;
};


class variable : public expression {
    std::string name;
public:
    variable(const std::string& name) : name{ name } {}
    virtual bool equals(const expression* expr, bool kostil = true) const;
    virtual std::string prefix_form() const;
};

class implication : public expression {
public:
    expression* left, *right;
    implication(expression* left, expression* right) : left{ left }, right{ right } {}
    virtual bool equals(const expression* expr, bool kostil = true) const;
    virtual std::string prefix_form() const;
};

class disjunction : public expression {
public:
    expression* left, *right;
    disjunction(expression* left, expression* right) : left{ left }, right{ right } {}
    virtual bool equals(const expression* expr, bool kostil) const;
    virtual std::string prefix_form() const;
};

class conjunction : public expression {
public:
    expression* left, *right;
    conjunction(expression* left, expression* right) : left{ left }, right{ right } {}
    virtual bool equals(const expression* expr, bool kostil) const;
    virtual std::string prefix_form() const;
};

class negation : public expression {
    expression* expr;
public:
    negation(expression* expr) : expr{ expr } {}
    virtual bool equals(const expression* expr, bool kostil) const;
    virtual std::string prefix_form() const;
};

using Expr = expression*;
using Var = variable*;
using Impl = implication*;
using Dis = disjunction*;
using Con = conjunction*;
using Neg = negation*;

static std::unordered_map<std::string, const expression*> ntm;

std::string bin_prefix(const expression* left, const expression* right, const std::string& code) {
    return "(" + code + "," + left->prefix_form() + "," + right->prefix_form() + ")";
}

bool variable::equals(const expression* expr, bool kostil) const {
    const variable* dop = dynamic_cast<const variable*>(expr);
    if (kostil) {
        if (dop == nullptr || dop->name != name) {
            return false;
        }
        goto skip;
    }
    if (ntm.count(name)) {
        const variable* tmp = dynamic_cast<const variable*>(ntm[name]);
        if (tmp != nullptr) {
            return dop != nullptr && dop->name == tmp->name;
        }
        return ntm[name]->equals(expr, !kostil);
    }
skip:;
    ntm[name] = expr;
    return true;
}

bool implication::equals(const expression* expr, bool kostil) const {
    const implication* tmp = dynamic_cast<const implication*>(expr);
    if (tmp == nullptr) return false;
    return left->equals(tmp->left, kostil) && right->equals(tmp->right, kostil);
}

bool disjunction::equals(const expression* expr, bool kostil) const {
    const disjunction* tmp = dynamic_cast<const disjunction*>(expr);
    if (tmp == nullptr) return false;
    return left->equals(tmp->left, kostil) && right->equals(tmp->right, kostil);
}

bool conjunction::equals(const expression* expr, bool kostil) const {
    const conjunction* tmp = dynamic_cast<const conjunction*>(expr);
    if (tmp == nullptr) return false;
    return left->equals(tmp->left, kostil) && right->equals(tmp->right, kostil);
}

bool negation::equals(const expression* e, bool kostil) const {
    const negation* tmp = dynamic_cast<const negation*>(e);
    if (tmp == nullptr) return false;
    return expr->equals(tmp->expr, kostil);
}

std::string variable::prefix_form() const {
    return name;
}

std::string implication::prefix_form() const {
    return bin_prefix(left, right, "->");
}

std::string disjunction::prefix_form() const {
    return bin_prefix(left, right, "|");
}

std::string conjunction::prefix_form() const {
    return bin_prefix(left, right, "&");
}

std::string negation::prefix_form() const {
    return "(!" + expr->prefix_form() + ")";
}


/*<------------------------------------------------------------------------------------------------------------>*/

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
        char c = *str;
        while (std::isspace(*str)) str++;
    }
};

/*<------------------------------------------------------------------------------------------------------------>*/


std::string A = "a", B = "b", C = "c";
Var a = new variable(A), b = new variable(B), c = new variable(C);
const std::vector<Expr> axioms {
    new implication(a, new implication(b, a)),
        new implication(new implication(a, b), new implication(new implication(a, new implication(b, c)), new implication(a, c))),
        new implication(a, new implication(b, new conjunction(a, b))),
        new implication(new conjunction(a, b), a), new implication(new conjunction(a, b), b),
        new implication(a, new disjunction(a, b)), new implication(b, new disjunction(a, b)),
        new implication(new implication(a, c), new implication(new implication(b, c), new implication(new disjunction(a, b), c))),
        new implication(new implication(a, b), new implication(new implication(a, new negation(b)), new negation(a))),
        new implication(new negation(new negation(a)), a)
};
static std::vector<std::pair<std::vector<Expr>, Expr>> vAn, vnytriDeda;

size_t isAxyp(const std::vector<Expr>& lft, const Expr expr, bool flag) {
    for (size_t i = 0; i < lft.size(); i++) {
        ntm.clear();
        if (lft[i]->equals(expr, flag)) return i + 1;
    }
    return { 0 };
}

size_t isAx(const Expr expr) {
    return isAxyp(axioms, expr, false);
}

size_t isHyp(const std::vector<Expr>& lft, Expr expr) {
    return isAxyp(lft, expr, true);
}

std::pair<size_t, size_t> isAnus() {
    auto t1 = vAn.back().first;
    auto t2 = vAn.back().second;
    size_t index{ 0 };

    for (auto& it : vAn) {
        index++;
        if (index >= vAn.size()) break;
        Impl dop = dynamic_cast<Impl>(it.second);

        if (!dop || !dop->right->equals(t2)) goto skip;

        if (t1.size() == it.first.size()) {
            std::vector<bool> ded_umer(it.first.size());
            std::fill(ded_umer.begin(), ded_umer.end(), false);

            for (Expr h : t1) {
                size_t k{ 0 };
                for (; k < it.first.size(); ++k) {
                    if (k < it.first.size() &&
                        !ded_umer[k] && h->equals(it.first[k])) {
                        ded_umer[k] = true;
                        break;
                    }
                }
                if (k == it.first.size()) {
                    goto skip;
                }
            }

            size_t index2{ 0 };
            for (auto& it2 : vAn) {
                index2++;
                if (index != index2 && index2 < vAn.size() &&
                    dop->left->equals(it2.second) && t1.size() == it2.first.size()) {
                    ded_umer.resize(it2.first.size());

                    std::fill(ded_umer.begin(), ded_umer.end(), false);

                    for (Expr h : t1) {
                        size_t l{ 0 };
                        for (; l < it2.first.size(); ++l) {
                            if (l < it2.first.size() &&
                                !ded_umer[l] && h->equals(it2.first[l])) {
                                ded_umer[l] = true;
                                break;
                            }
                        }
                        if (l == it.first.size()) {
                            goto skip1;
                        }
                    }
                    return { index2, index };
                }
            skip1:;
            }
        }

    skip:;
    }
    return { 0, 0 };
}

size_t isDed() {
    auto t1 = vnytriDeda.back().first;
    auto t2 = vnytriDeda.back().second;
    size_t index{ 0 };

    for (auto& it : vnytriDeda) {
        index++;
        if (index < vnytriDeda.size() &&
            t2->equals(it.second) &&
            t1.size() == it.first.size()) {
            std::vector<bool> ded_umer(it.first.size());
            std::fill(ded_umer.begin(), ded_umer.end(), false);

            for (Expr h : t1) {
                size_t i = 0;
                for (; i < it.first.size(); i++) {
                    if (i < it.first.size() &&
                        !ded_umer[i] && h->equals(it.first[i])) {
                        ded_umer[i] = true;
                        break;
                    }
                }
                if (i == it.first.size()) goto skip;
            }
            return index;
        skip:;
        }
    }
    return 0;
}

std::string parse(const Expr e, const std::vector<Expr>& lft) {
	size_t axNumber = isAx(e);
	if (axNumber > 0) {
		return "Ax. sch. " + std::to_string(axNumber);
	}
	size_t dedNumber = isDed();
	if (dedNumber > 0) {
		return "Ded. " + std::to_string(dedNumber);
	}
	std::pair<size_t, size_t> anusNumber = isAnus();
	if (anusNumber.first > 0) {
		return "M.P. " + std::to_string(anusNumber.first) + ", " + std::to_string(anusNumber.second);
	}
    size_t hypNumber = isHyp(lft, e);
    if (hypNumber > 0) {
        return "Hyp. " + std::to_string(hypNumber);
    }
	return "Incorrect";
}
void reinit(std::vector<Expr> lft, Expr e) {
    vAn.push_back({ lft, e });

    Impl ptr = dynamic_cast<Impl>(e);
    while (ptr != nullptr) {
        lft.push_back(ptr->left);
        e = ptr->right;
        ptr = dynamic_cast<Impl>(e);
    }

    vnytriDeda.push_back({ lft, e });
}

int main() {
	size_t index{ 1 };
	std::string str;
	std::stringstream result;

	while (std::getline(std::cin, str) && !str.empty()) {
		std::cout << "[" << index++ << "] " << str;
        Parser parser;
        parser.init(str.c_str());
        std::vector<Expr> lft = parser.start_expr();
        Expr e = parser.expr();

        reinit(lft, e);
		std::cout << " [" << parse(e, lft) << "]\n";
	}

	//std::cout << result.str();

	return 0;
}