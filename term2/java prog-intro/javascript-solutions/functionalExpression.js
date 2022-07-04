"use strict";

const data = {
    "x" : 0,
    "y" : 1,
    "z" : 2
};

const operation = oper => (...args) => (...dat) => oper(args[0](...dat), args[1](...dat));
const unaryOperation = oper => (...args) => (...dat) => oper(args[0](...dat));
const cnst = value => () => value;
const variable = name => (...args) => args[data[name]];
const add = operation((x, y) => x + y);
const subtract = operation((x, y) => x - y);
const multiply = operation((x, y) => x * y);
const divide = operation((x, y) => x / y);
const negate = unaryOperation((x) => -x);
const pi = cnst(Math.PI);
const e =  cnst(Math.E);

