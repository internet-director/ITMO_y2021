package markup;

import java.util.List;
import java.lang.StringBuilder;

public abstract class Element implements Container {
    protected final List<Container> list;

    public Element(final List<Container> l) {
        this.list = l;
    }
	
	protected void toMarkdown(StringBuilder sb, String start, String end) {
		sb.append(start);
		for (int i = 0; i < list.size(); i++) {
			list.get(i).toMarkdown(sb);
		}
		sb.append(end);
	}
	
	protected void toBBCode(StringBuilder sb, String start, String end) {
		sb.append(start);
		for (int i = 0; i < list.size(); i++) {
			list.get(i).toBBCode(sb);
		}
		sb.append(end);
	}
}