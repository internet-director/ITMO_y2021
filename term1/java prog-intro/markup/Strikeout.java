package markup;

import java.util.List;

public class Strikeout extends Element {
    public Strikeout(final List<Container> lc) {
        super(lc);
    }

    public void toMarkdown(StringBuilder sb) {
		super.toMarkdown(sb, "~", "~");
    }
	public void toBBCode(StringBuilder sb) {
		super.toBBCode(sb, "[s]", "[/s]");
    }
}