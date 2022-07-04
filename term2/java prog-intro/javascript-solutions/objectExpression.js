"use strict";

const data = {
    "x" : 0,
    "y" : 1,
    "z" : 2,
};

function Const(val) {
    this.val = val;
	this.toString = () => this.val.toString();
	this.prefix = () => this.val.toString();
	this.evaluate = (...args) => this.val;
	this.diff = (args) => new Const(0);
	this.simplify = () => new Const(this.val);
}

function Variable(name) {
    this.name = name;
	this.toString = () => this.name;
	this.prefix = () => this.name;
	this.evaluate = (...args) => args[data[this.name]];
	this.diff = (args) => {
		if (args === this.name) {
			return new Const(1);
		}
		return new Const(0);
	};
	this.simplify = () => new Variable(this.name);
}

function Operation(...operands) {
	this.arg = operands;
	this.get = () => this.arg;
	this.toString = () => this.arg.map(str => str.toString()).join(" ") + " " + this.type;
	this.prefix = () => "(" + this.type + " " + this.arg.map(str => str.prefix()).join(" ") + ")";
	this.evaluate = (...args) => this.oper(...this.get().map(operand => operand.evaluate.apply(operand, args)));
}

function BinaryOperation(operation, type) {
    return function (...operands) {
		Operation.apply(this, operands);
		this.oper = operation;
		this.type = type;
		this.simplify = () => {
			let a = this.arg[0].simplify();
			let b = this.arg[1].simplify();
			
			if (this.type === "+" || this.type === "-") {
				let t = 1;
				if (this.type === "-") {
					t = -1;
				}
				
				if (a instanceof Const && b instanceof Const) {
					return new Const(a.val + b.val * t);
				}
				if (a instanceof Const && a.val === 0) {
					if (this.type === "-") {
						return new Negate(b);
					}
					return b;
				}
				if (b instanceof Const && b.val === 0) {
					return a;
				}
				if (this.type === "+") {
					return new Add(a, b);
				}
				return new Subtract(a, b);
			} else if (this.type === "*") {
				if (a instanceof Const && b instanceof Const) {
					return new Const(a.val * b.val);
				}
				if (a instanceof Const && a.val === 0) {
					return new Const(0);
				}
				if (b instanceof Const && b.val === 0) {
					return new Const(0);
				}
				if (a instanceof Const && a.val === 1) {
					return b;
				}
				if (b instanceof Const && b.val === 1) {
					return a;
				}
				return new Multiply(a, b);
			}
			if (a instanceof Const && b instanceof Const) {
				return new Const(a.val / b.val);
			}
			if (a instanceof Const && a.val === 0) {
				return new Const(0);
			}
	
			if (b instanceof Multiply) {
				if (a.toString() === b.arg[0].toString()) {
					return new Divide(new Const(1), b.arg[1]);
				}
				if (a.toString() === b.arg[1].toString()) {
					return new Divide(new Const(1), b.arg[0]);
				}
			}
			
			return new Divide(a, b);
		}
		this.diff = (args) => {
			let a = this.arg[0].diff(args);
			let b = this.arg[1].diff(args);
			
			if (this.type === "+") {
				return new Add(a, b);
			} else if (this.type === "-") {
				return new Subtract(a, b);
			} else if (this.type === "*") {
				return new Add(new Multiply(this.arg[0], b), new Multiply(a, this.arg[1]));
			}
			return new Divide(new Subtract(new Multiply(a, this.arg[1]), new Multiply(this.arg[0], b)), new Multiply(this.arg[1], this.arg[1]));
		}
    }; 	
}

function UnaryOperation(operation, type) {
    return function (...args) {
        Operation.apply(this, args);
		this.oper = operation;
		this.type = type;
		this.diff = (args) => {
			let a = this.arg[0].diff(args);
			let r;
			if (this.type === "negate") {
				return new Negate(a);
			} else if (this.type === "sinh") {
				r = new Cosh(this.arg[0]);
			} else {
				r = new Sinh(this.arg[0]);
			}			
			return new Multiply(r, a); 	
		}
		this.simplify = () => {
			let a = this.arg[0].simplify();
			if (a instanceof Const) {
				return new Const(-a.val);
			}
			return new Negate(a);
		}
    }; 	
}

function ManyOperation(operation, type) {
	 return function (...operands) {
		Operation.apply(this, operands);
		this.oper = operation;
		this.type = type;
	 }
}

const Add = BinaryOperation((a, b) => a + b, "+");
const Subtract = BinaryOperation((a, b) => a - b, "-");
const Multiply = BinaryOperation((a, b) => a * b, "*");
const Divide = BinaryOperation((a, b) => a / b, "/");
const Negate = UnaryOperation(a => -a, "negate");
const Sinh = UnaryOperation( a => Math.sinh(a), "sinh");
const Cosh = UnaryOperation( a => Math.cosh(a), "cosh");
const Min3 = ManyOperation((...arr) => Math.min.apply(null, arr), "min3");
const Max5 = ManyOperation((...arr) => Math.max.apply(null, arr), "max5");

const operMap = {
    "+" : Add,
    "-" : Subtract,
    "*" : Multiply,
    "/" : Divide,
    "negate" : Negate,
	"min3" : Min3,
	"max5" : Max5,
	"sinh" : Sinh,
	"cosh" : Cosh
};

const sizeMap = {
    "negate" : 1,
    "+" : 2,
    "-" : 2,
    "*" : 2,
    "/" : 2,
	"min3" : 3,
	"max5" : 5,
	"sinh" : 1,
	"cosh" : 1
};

let parse = function(expression) {
    let expr = expression.split(" ").filter(fil => fil !== "");
    let stack = [];
	
	for (let i = 0; i < expr.length; i++) {
		if (expr[i] in operMap) {
			let sz = stack.length - sizeMap[expr[i]];
            let arg = stack.slice(sz);
            stack = stack.slice(0, sz);
            stack.push(new operMap[expr[i]](...arg));
        } else {
			if (expr[i] in data) {
				stack.push(new Variable(expr[i]));
			} else {
				stack.push(new Const(parseInt(expr[i])));
			}
        }
	}
	
    return stack.pop();
};

let SKIP = ["(", ")", " "];

function BaseParser(source) {
    this.position = 0;
    this.next = () => source[this.position++];
	this.hasNext = () => this.position + 1 <= source.length;
	this.skip = () => {
		while (this.position < source.length && source[this.position] === " ") {
			this.position++;
		}
	}
	this.check = (str) => {
		this.skip();
		for (let i = 0; i < str.length; i++) {
			if ((str[i] !== source[this.position + i])) {
				return false;
			}
		}
		return true;
	}
	this.getOper = () => {
		let oper = "";
		this.skip();
		while (this.hasNext()) {
			if (SKIP.indexOf(source[this.position]) !== -1) {
				return (oper.length > 0) ? oper : this.next();
			}
			oper += this.next();
		}
		return oper;
	}
}

function parseExpression(expression, type) {
	let expr = expression.trim();
	if (expr.length === 0) {
		throw new Error("Empty string");
	}
    let parser = new BaseParser(expr);
	
    function parseMain(oper) {
        let result;
        if (oper === "(") {
			result = parseExpression();
			oper = parser.getOper();
            if (oper !== ")") {
				throw new Error("not found close breakets");
            }
        } else if (oper in data) {
            result = new Variable(oper);
        } else if (!isNaN(new Number(oper))) {
            result = new Const(parseInt(oper));
        } else {
			throw new Error("invalid symbols");
        }
        return result;
    }

    function parseArgs() {
        let operands = [];
		let arg = "";
        while (parser.hasNext() && !parser.check(")")) {
			if ((arg = parser.getOper()) in operMap) {
				parser.position -= arg.length;
				break;
			}
            operands.push(parseMain(arg))
        }
        return operands;
    }

    function parseExpression() {
        let operands, oper;
        if (type) {
            oper = parser.getOper();
            operands = parseArgs();
        } else {
            operands = parseArgs();
            oper = parser.getOper();
        }
		
		if (!(oper in operMap)) {
			throw new Error("Not found operation");
        }
		
        if (operands.length !== sizeMap[oper]) {
			throw new Error("Not found args");
		}
        return new operMap[oper](...operands);
    }

    function parse() {
        let result = parseMain(parser.getOper());
        if (parser.hasNext()) {
			throw new Error("Expected end");
        }
        return result;
    }

    return parse();
}

let parsePrefix = (expr) => parseExpression(expr, true);
let parsePostfix = (expr) => parseExpression(expr, false);