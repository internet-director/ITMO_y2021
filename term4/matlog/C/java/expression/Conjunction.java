package expression;

import java.util.Map;
import java.util.Objects;

public class Conjunction implements Expression {

    public Expression left; //Conjunction
    public Expression right; //Negation

    public Conjunction(Expression left, Expression right) {
        this.left = left;
        this.right = right;
    }


    public boolean eq(Expression expr, Map<String, Expression> ntm) {
        return eq(expr, ntm, true);
    }
    public boolean eq(Expression expr, Map<String, Expression> ntm, boolean kostil) {
        if (!(expr instanceof Conjunction)) {
            return false;
        }
        Conjunction tmp = (Conjunction) expr;
        return left.eq(tmp.left, ntm, kostil) && right.eq(tmp.right, ntm, kostil);
    }

    @Override
    public String toTree() {
        return "(&," + left.toTree() + "," + right.toTree() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Conjunction that = (Conjunction) o;
        return Objects.equals(left, that.left) &&
                Objects.equals(right, that.right);
    }

    @Override
    public int hashCode() {
        return Objects.hash(left, right);
    }

    @Override
    public String toString() {
        return "(" + left.toString() + "&" + right.toString() + ")";
    }
}
