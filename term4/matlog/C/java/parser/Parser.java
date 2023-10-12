package parser;

import expression.*;

import java.util.Vector;

public class Parser {
    String str;
    int pos = 0;

    public Parser(String expr) {
        str = expr;
    }

    public Expression expr() {
        Expression left = dij();
        if (left != null && skip("->")) return new Implication(left, expr());
        return left;
    }

    public Vector<Expression> start_expr() {
        Vector<Expression> lfts = new Vector<>();
        while (true) {
            Expression e = expr();
            if (e != null) lfts.add(e);
            if (skip(",")) continue;
            if (skip("|-")) return lfts;
        }
    }

    private Expression dij() {
        Expression left = con();
        while (left != null && skip("|")) {
            if (str.charAt(pos) == '-') {
                pos--;
                return left;
            }
            left = new Disjunction(left, con());
        }
        return left;
    }

    private Expression con() {
        Expression left = nt();
        while (left != null && skip("&")) {
            left = new Conjunction(left, nt());
        }
        return left;
    }
    private Expression nt() {
        if (skip("(")) {
            Expression left = expr();
            skip(")");
            return left;
        }
        if (skip("!")) {
            return new Negation(nt());
        }
        skip();
        String name = getVar();
        if (name.length() == 0) return null;
        return new Variable(name);
    }

    private String getVar() {
        StringBuilder name = new StringBuilder();
        while (isNEnd() && isVar(str.charAt(pos))) {
            name.append(str.charAt(pos));
            pos++;
        }
        return name.toString();
    }

    private boolean isVar(char c) {
        if (c >= 'A' && c <= 'Z') return true;
        if (c >= '0' && c <= '9') return true;
        return c == 39;
    }

    private boolean skip(String sk) {
        skip();
        int sz = 0;
        for (int i = 0; i < sk.length(); i++) {
            if (!isNEnd() || str.charAt(pos) != sk.charAt(i)) {
                pos -= sz;
                return false;
            }
            sz++;
            pos++;
        }
        return true;
    }

    private void skip() {
        while(isNEnd() && Character.isWhitespace(str.charAt(pos))) pos++;
    }

    private boolean isNEnd() {
        return pos < str.length();
    }
}
