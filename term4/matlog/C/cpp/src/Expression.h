//
// Created by interceo on 9/27/23.
//

#ifndef MATLOG_EXPRESSION_H
#define MATLOG_EXPRESSION_H

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>

void ntmCLear();


class expression {
public:
    virtual bool equals(const expression*, bool = true) const = 0;
    virtual std::string prefix_form() const = 0;
    virtual std::string toString() const = 0;
    virtual ~expression() = default;
};


class variable : public expression {
    std::string name;
public:
    variable(const std::string& name) : name{ name } {}
    virtual bool equals(const expression* expr, bool kostil = true) const;
    virtual std::string prefix_form() const;
    virtual std::string toString() const;
};

class implication : public expression {
public:
    expression* left, *right;
    implication(expression* left, expression* right) : left{ left }, right{ right } {}
    virtual bool equals(const expression* expr, bool kostil = true) const;
    virtual std::string prefix_form() const;
    virtual std::string toString() const;
};

class disjunction : public expression {
public:
    expression* left, *right;
    disjunction(expression* left, expression* right) : left{ left }, right{ right } {}
    virtual bool equals(const expression* expr, bool kostil) const;
    virtual std::string prefix_form() const;
    virtual std::string toString() const;
};

class conjunction : public expression {
public:
    expression* left, *right;
    conjunction(expression* left, expression* right) : left{ left }, right{ right } {}
    virtual bool equals(const expression* expr, bool kostil) const;
    virtual std::string prefix_form() const;
    virtual std::string toString() const;
};

class negation : public expression {
    expression* expr;
public:
    negation(expression* expr) : expr{ expr } {}
    virtual bool equals(const expression* expr, bool kostil) const;
    virtual std::string prefix_form() const;
    virtual std::string toString() const;
};

using Expr = expression*;
using Var = variable*;
using Impl = implication*;
using Dis = disjunction*;
using Con = conjunction*;
using Neg = negation*;


#endif //MATLOG_EXPRESSION_H
