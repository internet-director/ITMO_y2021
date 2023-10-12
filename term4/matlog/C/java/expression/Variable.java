package expression;

import java.util.Map;
import java.util.Objects;

public class Variable implements Expression {

    private String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public String toTree() {
        return name;
    }

    public String getName() {
        return name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Variable variable = (Variable) o;
        return Objects.equals(name, variable.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }

    public boolean eq(Expression expr, Map<String, Expression> ntm) {
        return eq(expr, ntm, true);
    }

    public boolean eq(Expression expr, Map<String, Expression> ntm, boolean kostil) {
        if (kostil) {
            if (!(expr instanceof Variable) || !Objects.equals(((Variable) expr).name, name)) {
                return false;
            }
            ntm.put(name, expr);
            return true;
        }
        if (ntm.containsKey(name)) {
            Expression ex = ntm.get(name);
            if (ex instanceof Variable) {
                return (expr instanceof Variable) && Objects.equals(((Variable) expr).name, ((Variable) ex).name);
            }
            Expression e = ntm.get(name);
            return e.eq(expr, ntm, true);
        }
        ntm.put(name, expr);
        return true;
    }
}
