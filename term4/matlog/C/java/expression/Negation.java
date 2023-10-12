package expression;

import java.util.Map;
import java.util.Objects;

public class Negation implements Expression{

    public Expression negated;

    public Negation(Expression negated) {
        this.negated = negated;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Negation negation = (Negation) o;
        return Objects.equals(negated, negation.negated);
    }

    public boolean eq(Expression expr, Map<String, Expression> ntm) {
        return eq(expr, ntm, true);
    }

    public boolean eq(Expression expr, Map<String, Expression> ntm, boolean kostil) {
        if (!(expr instanceof Negation)) {
            return false;
        }
        Negation tmp = (Negation) expr;
        return negated.eq(tmp.negated, ntm, kostil);
    }

    @Override
    public int hashCode() {
        return Objects.hash(negated);
    }

    @Override
    public String toTree() {
        return "(!" + negated.toTree() + ")";
    }

    @Override
    public String toString() {
        return "(!" + negated.toString() + ")";
    }
}
