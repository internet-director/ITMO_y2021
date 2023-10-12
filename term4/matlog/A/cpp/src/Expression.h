#pragma once
#include <string>

class expression {
public:
    virtual std::string prefix_form() = 0;

    virtual ~expression() {};
};

class variable : public expression {
private:
    std::string _name;

public:
    variable(std::string name) : _name(name) {
    }

    virtual std::string prefix_form() {
        return _name;
    }
};

class implication : public expression {
private:
    expression* _left;
    expression* _right;

public:
    implication(expression* left, expression* right) :
        _left(left),
        _right(right) {
    }

    virtual std::string prefix_form() {
        return "(->," + _left->prefix_form() + "," + _right->prefix_form() + ")";
    }
};

class disjunction : public expression {
private:
    expression* _left;
    expression* _right;

public:
    disjunction(expression* left, expression* right) :
        _left(left),
        _right(right) {
    }

    virtual std::string prefix_form() {
        return "(|," + _left->prefix_form() + "," + _right->prefix_form() + ")";
    }
};

class conjunction : public expression {
private:
    expression* _left;
    expression* _right;

public:
    conjunction(expression* left, expression* right) :
        _left(left),
        _right(right) {
    }

    virtual std::string prefix_form() {
        return "(&," + _left->prefix_form() + "," + _right->prefix_form() + ")";
    }
};

class negation : public expression {
private:
    expression* _expr;

public:
    negation(expression* expr) :
        _expr(expr) {
    }

    virtual std::string prefix_form() {
        return "(!" + _expr->prefix_form() + ")";
    }
};