#include "main.h"


Var a = new variable("a"), b = new variable("b"), c = new variable("c");
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


size_t isAxyp(const std::vector<Expr>& lft, const Expr expr, bool flag) {
    for (size_t i = 0; i < lft.size(); i++) {
        ntmCLear();
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

Expr reinit(std::vector<Expr>& lft, Expr e, bool insert = true) {
    //vAn.push_back({ lft, e });

    Impl ptr = dynamic_cast<Impl>(e);
    while (ptr != nullptr) {
        if (insert) {
            lft.push_back(ptr->left);
        }
        e = ptr->right;
        ptr = dynamic_cast<Impl>(e);
    }
    return e;

    //vnytriDeda.push_back({ lft, e });
}

bool cc(const std::vector<Expr>& sm, const std::vector<Expr>& bg) {
    std::vector<bool> dop(bg.size());
    if (sm.size() <= bg.size()) {
        std::fill(dop.begin(), dop.end(), false);
        for (Expr sh : sm) {
            size_t j{ 0 };
            for (; j <= bg.size(); j++) {
                if (!dop[j] && j != bg.size() && sh->equals(bg[j])) {
                    dop[j] = true;
                    break;
                }
            }
            if (j == bg.size() + 1) return false;
        }
        return true;
    }
    return false;
}

upair mp(const tvec& vAn, size_t i) {
    for (size_t j = i; j < std::string::npos; j--) {
        Impl dop = dynamic_cast<Impl>(vAn[j].second);
        if (dop && dop->right->equals(vAn[i].second) && cc(vAn[j].first, vAn[i].first)) {
            for (size_t k = i; k < std::string::npos; k--) {
                if (k != j && dop->left->equals(vAn[k].second)) {
                    if (cc(vAn[k].first, vAn[i].first)) {
                        return { j, k };
                    }
                }
            }
        }
    }
    return { -1, -1 };
}

void rec_degradation(const tvec& vAn, const std::vector<tpair>& globalAns, 
    std::vector<tpair>& gloablTmo, Expr e, size_t num) {
    for (size_t i = num + 1; i < globalAns.size(); i++) {
        if (globalAns[i].first->equals(e)) { 
            rec_degradation(vAn, globalAns, gloablTmo, globalAns[i].first, i);
            goto skip;
        }
    }
    for (Expr h : vAn.back().first) {
        if (e->equals(h)) {
            gloablTmo.push_back({ h, HYPH });
            goto skip;
        }
    }
    if (isAx(e) == 0) {
        for (size_t i = num + 1; i < globalAns.size(); i++) {
            Impl dop = dynamic_cast<Impl>(globalAns[i].first); 
            if (dop && dop->right->equals(e)) {
                for (size_t j = num + 1; j < globalAns.size(); j++) {
                    if (i != j && dop->left->equals(globalAns[j].first)) { 
                        gloablTmo.push_back({ e, MOP });
                        rec_degradation(vAn, globalAns, gloablTmo, globalAns[j].first, j);
                        rec_degradation(vAn, globalAns, gloablTmo, globalAns[i].first, i);
                        goto skip;
                    }
                }
            }
        }
    }
    gloablTmo.push_back({ e, AXIOM });
    skip:;
}

#define REPEAT(E, V) Impl vv = dynamic_cast<Impl> (E); \
                        if (!vv) break;                \
                        E = vv->right;

tppair dd(const tvec& vAn, size_t num) {
    std::vector<Expr> dop = vAn[num].first;
    Expr expr = reinit(dop, vAn[num].second);

    for (size_t i = num; i--;) {
        std::vector<Expr> vec(vAn[i].first);

        if (expr->equals(reinit(vec, vAn[i].second)) && cc(vec, dop)) {
            ttvec dop2;
            Expr tmp = vAn[i].second;
            for (;;) {
                expr = vAn[num].second;
                std::vector<Expr> kostil;
                for(;;) {
                    if (expr->equals(tmp)) {
                        for (size_t j = 0; j < kostil.size(); j++) {
                            dop2.push_back({ true, kostil[kostil.size() - j - 1] });
                        }
                        return { i + 1, dop2 };
                    }

                    REPEAT(expr, kostil)
                    kostil.push_back(vv->left);
                }

                REPEAT(tmp, dop2)
                dop2.emplace_back(false, vv->left);
            }
            break;
        }
    }
    return { 0, {} };
}

void GetFirstType(const std::pair<bool, Expr>& it, std::vector<tpair>& tmp) {
    Impl t1 = new implication(it.second, it.second);
    Impl t2 = new implication(it.second, t1);
    Impl t3 = new implication(it.second, new implication(t1, it.second));
    Impl t4 = new implication(t3, t1);

    {
        tmp.push_back({ t2, AXIOM });
        tmp.push_back({ new implication(t2, t4), AXIOM });
        tmp.push_back({ t4, MOP });
        tmp.push_back({ t3, AXIOM });
        tmp.push_back({ t1, MOP });
    }
}
void GetSecondType(const std::pair<bool, Expr>& it, std::vector<tpair>& tmp, Expr expr) {
    Impl t1 = new implication(it.second, expr);

    {
        tmp.push_back({ new implication(expr, t1), AXIOM });
        tmp.push_back({ expr, HYPH });
        tmp.push_back({ t1, MOP });
    }
}
void GetThirdType(const std::pair<bool, Expr>& it, std::vector<tpair>& tmp, Expr expr, 
    size_t num, const std::vector<tpair>& globalAns) {
    size_t cntr{ 0 }, dop{ 0 };
    for (size_t i = num + 1; i < globalAns.size() && !cntr; ++i) {
        Impl ab = dynamic_cast<Impl>(globalAns[i].first);
        if (ab && ab->right->equals(globalAns[num].first)) {
            for (size_t j = num + 1; j < globalAns.size(); ++j) {
                if (i != j && ab->left->equals(globalAns[j].first)) {
                    cntr = j + 1;
                    dop = i + 1;
                    break;
                }
            }
        }
    }
    Impl t1 = new implication(it.second, expr);
    Impl t2 = new implication(new implication(it.second, globalAns[dop - 1].first), t1);
    tmp.push_back({ new implication(new implication(it.second, globalAns[cntr - 1].first), t2), AXIOM });
    tmp.push_back({ t2, MOP });
    tmp.push_back({ t1, MOP });
}

void prove(const tvec& vAn, std::vector<tpair>& globalAns, size_t num) {
    Expr expr = vAn[num].second;
    for (Expr it : vAn[num].first) { 
        if (it->equals(expr)) { 
            globalAns.push_back({ expr, HYPH });
            return; 
        } 
    }
    if (isAx(expr) > 0) { 
        globalAns.push_back({ expr, AXIOM });
        return; 
    }
    upair mod = mp(vAn, num);
    if (mod.first != -1 && mod.second != -1) {
        globalAns.push_back({ expr, MOP });
        prove(vAn, globalAns, mod.first);
        prove(vAn, globalAns, mod.second);
    }
    else {
        auto dDop = dd(vAn, num);
        size_t sz = globalAns.size();
        
        if (dDop.first != 0) {
            Expr exprLst = vAn[dDop.first - 1].second;
            prove(vAn, globalAns, dDop.first - 1);

            for (const std::pair<bool, Expr>& it : dDop.second) {
                std::vector<tpair> tmp;
                if (!it.first) {
                    Impl imp = dynamic_cast<Impl>(exprLst);
                    exprLst = imp->right;
                    for (size_t j = sz; j < globalAns.size(); ++j) {
                        tmp.push_back(globalAns[j]);
                    }
                    globalAns.resize(sz);
                    globalAns.push_back({ exprLst, MOP });
                    globalAns.push_back({ it.second, HYPH });
                    for (const tpair& v : tmp) {
                        globalAns.push_back(v);
                    }
                }
                else {
                    exprLst = new implication(it.second, exprLst);
                    for (size_t i = globalAns.size(); i != 0 && (sz <= 0 || i != sz - 1);) {
                        i--;
                        Expr exprSEc = globalAns[i].first;

                        if (exprSEc->equals(it.second)) {
                            GetFirstType(it, tmp);
                        }
                        else if (globalAns[i].second == AXIOM || 
                            globalAns[i].second == HYPH) {
                            GetSecondType(it, tmp, exprSEc);
                        }
                        else {
                            GetThirdType(it, tmp, exprSEc, i, globalAns);
                        }
                    }
                    globalAns.resize(sz);
                    for (size_t i = 0; i < tmp.size(); i++) {
                        globalAns.push_back(tmp[tmp.size() - i - 1]);
                    }
                }
            }
        }
    }
}

std::string ans(const tvec& vAn) {
    std::stringstream result;
    std::vector<tpair> globalAns, gloablTmo;

    if (!vAn.empty()) {
        size_t count{ 0 };
        for (const Expr v : vAn.back().first) {
            if (count) result << ",";
            result << v->toString();
            count++;
        }

        result << "|-" << vAn.back().second->toString() << "\n";
        prove(vAn, globalAns, vAn.size() - 1);

        if (!globalAns.empty()) {
            rec_degradation(vAn, globalAns, gloablTmo, globalAns.front().first, 0);
            for (size_t i = 0; i < gloablTmo.size(); i++) {
                result << gloablTmo[gloablTmo.size() - i - 1].first->toString() << "\n";
            }
        }
    }
    return result.str();
}

int main() {
    std::string str;
    tvec vAn;
    while (std::getline(std::cin, str) && !str.empty()) {
        Parser parser;
        parser.init(str.c_str());
        std::vector<Expr> lft = parser.start_expr();
        Expr e = parser.expr();
        reinit(lft, e, false);
        vAn.push_back({ lft, e });
    }

    std::cout << ans(vAn);
    return 0;
}