package markup;

import java.util.List;

public class Emphasis extends Element {
    public Emphasis(final List<Container> lc) {
        super(lc);
    }

    public void toMarkdown(StringBuilder sb) {
		super.toMarkdown(sb, "*", "*");
    }
	public void toBBCode(StringBuilder sb) {
		super.toBBCode(sb, "[i]", "[/i]");
    }
}