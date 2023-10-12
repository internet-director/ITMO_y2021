//
// Created by interceo on 9/27/23.
//

#include "Expression.h"

static std::unordered_map<std::string, const expression*> ntm;

void ntmCLear() {
    ntm.clear();
}

std::string bin_prefix(const expression* left, const expression* right, const std::string& code) {
    return "(" + code + "," + left->prefix_form() + "," + right->prefix_form() + ")";
}

std::string bin_normal(const expression* left, const expression* right, const std::string& code) {
    return "("+ left->toString() + code + right->toString() + ")";
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


std::string variable::toString() const {
    return this->prefix_form();
}

std::string implication::toString() const {
    return bin_normal(left, right, "->");
}

std::string disjunction::toString() const {
    return bin_normal(left, right, "|");
}

std::string conjunction::toString() const {
    return bin_normal(left, right, "&");
}

std::string negation::toString() const {
    return "(!" + expr->toString() + ")";
}