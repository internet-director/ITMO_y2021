package markup;

import java.util.List;

public class Strong extends Element {
    public Strong(final List<Container> lc) {
        super(lc);
    }

    public void toMarkdown(StringBuilder sb) {
		super.toMarkdown(sb, "__", "__");
    }
	public void toBBCode(StringBuilder sb) {
		super.toBBCode(sb, "[b]", "[/b]");
    }
}