import expression.*;
import expression.Variable;
import parser.Parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class Main {

    static final Integer AXIOM = 0x111;
    static final Integer MOP = 0x222;
    static final Integer HYPH = 0x333;

    public static class Pair<F, S> {
        public F first; //first member of pair
        public S second; //second member of pair

        public Pair() {
            this.first = null;
            this.second = null;
        }

        public Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }
    }

    static Expression[] axioms = new Expression[10];
    private static Vector<Pair<Vector<Expression>, Expression>> van = new Vector<>();
    private static Vector<Pair<Expression, Integer>> globalAns = new Vector<>();
    private static Vector<Pair<Expression, Integer>> gloablTmo = new Vector<>();

    private static final Variable a = new Variable("a");
    private static final Variable b = new Variable("b");
    private static final Variable c = new Variable("c");

    private static void axInit() {
        axioms[0] = new Implication(a, new Implication(b, a));
        axioms[1] = new Implication(new Implication(a, b), new Implication(new Implication(a, new Implication(b, c)), new Implication(a, c)));
        axioms[2] = new Implication(a, new Implication(b, new Conjunction(a, b)));
        axioms[3] = new Implication(new Conjunction(a, b), a);
        axioms[4] = new Implication(new Conjunction(a, b), b);
        axioms[5] = new Implication(a, new Disjunction(a, b));
        axioms[6] = new Implication(b, new Disjunction(a, b));
        axioms[7] = new Implication(new Implication(a, c), new Implication(new Implication(b, c), new Implication(new Disjunction(a, b), c)));
        axioms[8] = new Implication(new Implication(a, b), new Implication(new Implication(a, new Negation(b)), new Negation(a)));
        axioms[9] = new Implication(new Negation(new Negation(a)), a);
    }

    private static Map<String, Expression> ntm = new HashMap<>();

    private static Expression reinit(Vector<Expression> lft, Expression e, boolean insert) {
        Expression tmp = e;
        while (tmp instanceof Implication) {
            Implication ptr = (Implication) tmp;
            if (insert) {
                lft.add(ptr.left);
            }
            tmp = ptr.right;
        }
        return tmp;
    }

    private static Expression reinit(Vector<Expression> lft, Expression e) {
        return reinit(lft, e, true);
    }

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String str;

        while ((str = in.readLine()) != null && str.length() > 2) {
            Parser parser = new Parser(str);
            van.add(new Pair<>(parser.start_expr(), parser.expr()));
        }

        axInit();
        answer();

    }

    private static int isAx(Expression expr) {
        for (int i = 0; i < 10; i++) {
            ntm.clear();
            if (axioms[i].eq(expr, ntm, false)) return i + 1;
        }
        return 0;
    }

    private static boolean cCheck(Vector<Expression> lft, Vector<Expression> rft) {
        Boolean[] dop = new Boolean[rft.size()];
        if (lft.size() <= rft.size()) {
            Arrays.fill(dop, false);
            for (Expression expression : lft) {
                int j = 0;
                for (; j <= rft.size(); j++) {
                    if (j < rft.size() && !dop[j] && j != rft.size() && expression.eq(rft.get(j), ntm)) {
                        dop[j] = true;
                        break;
                    }
                }
                if (j == rft.size() + 1) return false;
            }
            return true;
        }
        return false;
    }

    private static Pair<Integer, Integer> mCheck(int i) {
        for (int j = i; j >= 0; j--) {
            if (van.get(j).second instanceof Implication) {
                Implication dop = (Implication) van.get(j).second;
                if (dop.right.eq(van.get(i).second, ntm) && cCheck(van.get(j).first, van.get(i).first)) {
                    for (int k = i; k >= 0; k--) {
                        if (k != j && dop.left.eq(van.get(k).second, ntm)) {
                            if (cCheck(van.get(k).first, van.get(i).first)) {
                                return new Pair<>(j, k);
                            }
                        }
                    }
                }
            }
        }
        return new Pair<>(-1, -1);
    }

    private static void answer() {
        if (!van.isEmpty()) {
            for (int i = 0; i < van.get(van.size() - 1).first.size(); i++) {
                if (i != 0) System.out.print(",");
                System.out.print(van.get(van.size() - 1).first.get(i).toString());
            }

            System.out.print("|-" + van.get(van.size() - 1).second.toString() + "\n");


            prove(van.size() - 1);


            if (!globalAns.isEmpty()) {
                rec_degradation(globalAns.get(0).first, 0);
                for (int i = 0; i < gloablTmo.size(); i++) {
                    System.out.print(gloablTmo.get(gloablTmo.size() - i - 1).first.toString() + "\n");
                }
            }
        }
    }

    private static void rec_degradation(Expression e, int num) {
        for (int i = num + 1; i < globalAns.size(); i++) {
            if (globalAns.get(i).first.eq(e, ntm)) {
                rec_degradation(globalAns.get(i).first, i);
                return;
            }
        }

        for (int i = 0; i < van.lastElement().first.size(); i++) {
            if (e.eq(van.lastElement().first.get(i), ntm)) {
                gloablTmo.add(new Pair<>(van.lastElement().first.get(i), HYPH));
                return;
            }
        }
        if (isAx(e) == 0) {
            for (int i = num + 1; i < globalAns.size(); i++) {
                if (globalAns.get(i).first instanceof Implication) {
                    Implication dop = (Implication) globalAns.get(i).first;
                    if (dop.right.eq(e, ntm)) {
                        for (int j = num + 1; j < globalAns.size(); j++) {
                            if (i != j && dop.left.eq(globalAns.get(j).first, ntm)) {
                                gloablTmo.add(new Pair<>(e, MOP));
                                rec_degradation(globalAns.get(j).first, j);
                                rec_degradation(globalAns.get(i).first, i);
                                return;
                            }
                        }
                    }
                }
            }
        }
        gloablTmo.add(new Pair<>(e, AXIOM));
    }

    private static Pair<Integer, Vector<Pair<Boolean, Expression>>> dCheck(int num) {
        Vector<Expression> dop = van.get(num).first;
        Expression expr = reinit(dop, van.get(num).second);
        Vector<Expression> vec;
        Vector<Expression> kostil = new Vector<>();
        Vector<Pair<Boolean, Expression>> dop2 = new Vector<>();

        for (int i = num; i != 0; ) {
            i--;
            vec = van.get(i).first;

            if (expr.eq(reinit(vec, van.get(i).second), ntm) && cCheck(vec, dop)) {
                dop.clear();
                Expression tmp = van.get(i).second;
                for (; ; ) {
                    expr = van.get(num).second;
                    kostil.clear();
                    for (; ; ) {
                        if (expr.eq(tmp, ntm)) {
                            for (int j = 0; j < kostil.size(); j++) {
                                dop2.add(new Pair<>(true, kostil.get(kostil.size() - j - 1)));
                            }
                            return new Pair<>(i + 1, dop2);
                        }
                        if (expr instanceof Implication) {
                            Implication ptr = (Implication) expr;
                            expr = ptr.right;
                            kostil.add(ptr.left);
                        } else break;
                    }
                    if (tmp instanceof Implication) {
                        Implication ptr = (Implication) tmp;
                        tmp = ptr.right;
                        dop2.add(new Pair<>(false, ptr.left));
                    } else break;
                }
                break;
            }
        }

        return new Pair<>(0, new Vector<>());
    }

    private static void GetFirstType(Pair<Boolean, Expression> it, Vector<Pair<Expression, Integer>> tmp) {
        Implication t1 = new Implication(it.second, it.second);
        Implication t2 = new Implication(it.second, t1);
        Implication t3 = new Implication(it.second, new Implication(t1, it.second));
        Implication t4 = new Implication(t3, t1);

        {
            tmp.add(new Pair<>(t2, AXIOM));
            tmp.add(new Pair<>(new Implication(t2, t4), AXIOM));
            tmp.add(new Pair<>(t4, MOP));
            tmp.add(new Pair<>(t3, AXIOM));
            tmp.add(new Pair<>(t1, MOP));
        }
    }

    private static void GetSecondType(Pair<Boolean, Expression> it,
                                      Vector<Pair<Expression, Integer>> tmp, Expression expr) {
        Implication t1 = new Implication(it.second, expr);

        {
            tmp.add(new Pair<>(new Implication(expr, t1), AXIOM));
            tmp.add(new Pair<>(expr, HYPH));
            tmp.add(new Pair<>(t1, MOP));
        }
    }

    private static void GetThirdType(Pair<Boolean, Expression> it,
                                     Vector<Pair<Expression, Integer>> tmp, Expression expr,
                                     int num) {
        int cntr = 0, dop = 0;
        for (int i = num + 1; i < globalAns.size(); i++) {
            if (cntr > 0) break;
            if (globalAns.get(i).first instanceof Implication) {
                if (((Implication) globalAns.get(i).first).right.eq(globalAns.get(num).first, ntm)) {
                    for (int j = num + 1; j < globalAns.size(); j++) {
                        if (i != j && ((Implication) globalAns.get(i).first).left.eq(globalAns.get(j).first, ntm)) {
                            cntr = j + 1;
                            dop = i + 1;
                            break;
                        }
                    }
                }
            }
        }
        Implication t1 = new Implication(it.second, expr);
        Implication t2 = new Implication(new Implication(it.second, globalAns.get(dop - 1).first), t1);
        tmp.add(new Pair<>(new Implication(new Implication(it.second, globalAns.get(cntr - 1).first), t2), AXIOM));
        tmp.add(new Pair<>(t2, MOP));
        tmp.add(new Pair<>(t1, MOP));
    }

    private static void prove(int num) {
        Pair<Integer, Integer> mod = null;
        Pair<Integer, Vector<Pair<Boolean, Expression>>> dDop;
        int sz = 0;

        for (Expression expr : van.get(num).first) {
            if (expr.eq(van.get(num).second, ntm)) {
                globalAns.add(new Pair<>(van.get(num).second, HYPH));
                return;
            }
        }
        if (isAx(van.get(num).second) > 0) {
            globalAns.add(new Pair<>(van.get(num).second, AXIOM));
            return;
        }

            mod = mCheck(num);

        if (mod.first != -1 && mod.second != -1 && (mod.first != num && mod.second != num)) {
            globalAns.add(new Pair<>(van.get(num).second, MOP));
            prove(mod.first);
            prove(mod.second);
            return;
        }


        dDop = dCheck(num);
        sz = globalAns.size();

        if (dDop.first != 0 && dDop.first != num + 1) {
            prove(dDop.first - 1);
            Expression exprLst = van.get(dDop.first - 1).second;
            Vector<Pair<Expression, Integer>> tmp = new Vector<>();

            for (Pair<Boolean, Expression> it : dDop.second) {
                if (!it.first) {
                    exprLst = ((Implication) exprLst).right;
                    for (int j = sz; j < globalAns.size(); ++j) {
                        tmp.add(globalAns.get(j));
                    }
                    globalAns.setSize(sz);
                    globalAns.add(new Pair<>(exprLst, MOP));
                    globalAns.add(new Pair<>(it.second, HYPH));
                    globalAns.addAll(tmp);
                } else {
                    exprLst = new Implication(it.second, exprLst);

                    for (int i = globalAns.size(); i != 0 && (sz <= 0 || i != sz - 1); ) {
                        i--;

                        if (globalAns.get(i).first.eq(it.second, ntm)) {
                            GetFirstType(it, tmp);
                        } else if (globalAns.get(i).second == AXIOM ||
                                globalAns.get(i).second == HYPH) {
                            GetSecondType(it, tmp, globalAns.get(i).first);
                        } else {
                            GetThirdType(it, tmp, globalAns.get(i).first, i);
                        }
                    }
                    globalAns.setSize(sz);
                    for (int i = 0; i < tmp.size(); i++) {
                        globalAns.add(tmp.get(tmp.size() - i - 1));
                    }
                }
                tmp.clear();
            }
        }
    }
}
