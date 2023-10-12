package expression;

import java.util.Map;

public interface Expression {
    String toTree();
    boolean eq(Expression expr, Map<String, Expression> ntm);
    boolean eq(Expression expr, Map<String, Expression> ntm, boolean kostil);
}
